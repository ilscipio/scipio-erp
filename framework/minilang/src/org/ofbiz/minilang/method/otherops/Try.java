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
