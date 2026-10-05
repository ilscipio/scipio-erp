/*
 * Scipio Commerce
 * Copyright (C) Ilscipio GmbH
 *
 * This file is part of Scipio Commerce. Scipio Commerce is free software: you
 * can redistribute it and modify it under the terms of the GNU Affero General
 * Public License, version 3, as published by the Free Software Foundation.
 * Scipio Commerce is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 * FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License
 * for more details. You should have received a copy of the license with this
 * work (file LICENSE). If not, see <https://www.gnu.org/licenses/agpl-3.0.html>.
 * A commercial license is available from Ilscipio GmbH.
 *
 * SPDX-License-Identifier: AGPL-3.0-only
 */
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
        StringBuilder sb = new StringBuilder("<synchronized ");
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
