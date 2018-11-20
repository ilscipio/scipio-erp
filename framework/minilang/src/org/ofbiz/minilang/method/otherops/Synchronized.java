/*******************************************************************************
 * Licensed to the Apache Software Foundation (ASF) under one
 * or more contributor license agreements.  See the NOTICE file
 * distributed with this work for additional information
 * regarding copyright ownership.  The ASF licenses this file
 * to you under the Apache License, Version 2.0 (the
 * "License"); you may not use this file except in compliance
 * with the License.  You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 * KIND, either express or implied.  See the License for the
 * specific language governing permissions and limitations
 * under the License.
 *******************************************************************************/
package org.ofbiz.minilang.method.otherops;

import java.util.Collections;
import java.util.List;

import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.collections.FlexibleMapAccessor;
import org.ofbiz.minilang.MiniLangException;
import org.ofbiz.minilang.SimpleMethod;
import org.ofbiz.minilang.artifact.ArtifactInfoContext;
import org.ofbiz.minilang.method.MethodContext;
import org.ofbiz.minilang.method.MethodOperation;
import org.w3c.dom.Element;

/**
 * SCIPIO: Implements the &lt;synchronized&gt; element.
 * Added 2018-11-20.
 */
public final class Synchronized extends MethodOperation {

    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());
    
    private final FlexibleMapAccessor<Object> fieldFma;
    private final List<MethodOperation> subOps;

    public Synchronized(Element element, SimpleMethod simpleMethod) throws MiniLangException {
        super(element, simpleMethod);
        this.fieldFma = FlexibleMapAccessor.getInstance(element.getAttribute("field"));
        this.subOps = Collections.unmodifiableList(SimpleMethod.readOperations(element, simpleMethod));
    }

    @Override
    public boolean exec(MethodContext methodContext) throws MiniLangException {
        Object fieldVal = fieldFma.get(methodContext.getEnvMap());
        if (fieldVal != null) {
            synchronized (fieldVal) {
                return SimpleMethod.runSubOps(subOps, methodContext);
            }
        } else {
            // FIXME: this was copy-pasted from Log.java
            StringBuilder buf = new StringBuilder("[");
            String methodLocation = this.simpleMethod.getFromLocation();
            int pos = methodLocation.lastIndexOf('/');
            if (pos != -1) {
                methodLocation = methodLocation.substring(pos + 1);
            }
            buf.append(methodLocation);
            buf.append("#");
            buf.append(this.simpleMethod.getMethodName());
            buf.append(" line ");
            buf.append(getLineNumber());
            buf.append("] ");
            buf.append("Cannot synchronize on null field (expr: " + fieldFma.getOriginalName() + ")");
            Debug.logWarning(buf.toString(), module);
            return SimpleMethod.runSubOps(subOps, methodContext);
        }
    }

    @Override
    public void gatherArtifactInfo(ArtifactInfoContext aic) {
        for (MethodOperation method : this.subOps) {
            method.gatherArtifactInfo(aic);
        }
    }

    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder("<if-not-empty ");
        sb.append("field=\"").append(this.fieldFma).append("\"/>");
        return sb.toString();
    }

    /**
     * A &lt;synchronized&gt; element factory.
     */
    public static final class SynchronizedFactory implements Factory<Synchronized> {
        @Override
        public Synchronized createMethodOperation(Element element, SimpleMethod simpleMethod) throws MiniLangException {
            return new Synchronized(element, simpleMethod);
        }

        @Override
        public String getName() {
            return "synchronized";
        }
    }
}
