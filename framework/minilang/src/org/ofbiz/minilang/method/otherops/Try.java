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

import org.ofbiz.base.util.UtilXml;
import org.ofbiz.minilang.MiniLangException;
import org.ofbiz.minilang.SimpleMethod;
import org.ofbiz.minilang.artifact.ArtifactInfoContext;
import org.ofbiz.minilang.method.MethodContext;
import org.ofbiz.minilang.method.MethodOperation;
import org.w3c.dom.Element;

/**
 * SCIPIO: Implements the &lt;try&gt; element.
 * <p>
 * TODO: Does not yet support "catch", only "finally".
 * <p>
 * Added 2018-11-29.
 */
public final class Try extends MethodOperation {

    private final List<MethodOperation> finallySubOps;
    //private final FlexibleMapAccessor<Object> fieldFma;
    private final List<MethodOperation> subOps;

    public Try(Element element, SimpleMethod simpleMethod) throws MiniLangException {
        super(element, simpleMethod);
        //this.fieldFma = FlexibleMapAccessor.getInstance(element.getAttribute("field"));
        this.subOps = Collections.unmodifiableList(SimpleMethod.readOperations(element, simpleMethod));
        Element finallyElement = UtilXml.firstChildElement(element, "finally");
        if (finallyElement != null) {
            this.finallySubOps = Collections.unmodifiableList(SimpleMethod.readOperations(finallyElement, simpleMethod));
        } else {
            this.finallySubOps = null;
        }
    }

    @Override
    public boolean exec(MethodContext methodContext) throws MiniLangException {
        try {
            return SimpleMethod.runSubOps(subOps, methodContext);
        } finally {
            if (finallySubOps != null) {
                SimpleMethod.runSubOps(finallySubOps, methodContext);
            }
        }
    }

    @Override
    public void gatherArtifactInfo(ArtifactInfoContext aic) {
        for (MethodOperation method : this.subOps) {
            method.gatherArtifactInfo(aic);
        }
        if (this.finallySubOps != null) {
            for (MethodOperation method : this.finallySubOps) {
                method.gatherArtifactInfo(aic);
            }
        }
    }

    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder("<try ");
        sb.append("\"/>");
        return sb.toString();
    }

    /**
     * A &lt;try&gt; element factory.
     */
    public static final class TryFactory implements Factory<Try> {
        @Override
        public Try createMethodOperation(Element element, SimpleMethod simpleMethod) throws MiniLangException {
            return new Try(element, simpleMethod);
        }

        @Override
        public String getName() {
            return "try";
        }
    }
}
