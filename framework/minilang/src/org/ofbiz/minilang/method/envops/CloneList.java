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
package org.ofbiz.minilang.method.envops;

import org.ofbiz.base.util.collections.FlexibleMapAccessor;
import org.ofbiz.minilang.MiniLangException;
import org.ofbiz.minilang.MiniLangValidate;
import org.ofbiz.minilang.SimpleMethod;
import org.ofbiz.minilang.method.MethodContext;
import org.ofbiz.minilang.method.MethodOperation;
import org.w3c.dom.Element;

import java.util.ArrayList;
import java.util.List;

/**
 * Implements the &lt;clone-list&gt; element (SCIPIO).
 */
public final class CloneList extends MethodOperation {

    private final FlexibleMapAccessor<List<Object>> listFma;
    private final FlexibleMapAccessor<List<Object>> toListFma;

    public CloneList(Element element, SimpleMethod simpleMethod) throws MiniLangException {
        super(element, simpleMethod);
        if (MiniLangValidate.validationOn()) {
            MiniLangValidate.attributeNames(simpleMethod, element, "to-list", "list");
            MiniLangValidate.requiredAttributes(simpleMethod, element, "list");
            MiniLangValidate.expressionAttributes(simpleMethod, element, "to-list", "list");
            MiniLangValidate.noChildElements(simpleMethod, element);
        }
        FlexibleMapAccessor<List<Object>> toListFma = FlexibleMapAccessor.getInstance(element.getAttribute("to-list"));
        this.toListFma = toListFma.isEmpty() ? null : toListFma;
        this.listFma = FlexibleMapAccessor.getInstance(element.getAttribute("list"));
    }

    @Override
    public boolean exec(MethodContext methodContext) throws MiniLangException {
        List<Object> fromList = listFma.get(methodContext.getEnvMap());
        if (fromList != null) {
            List<Object> toList = new ArrayList<>(fromList);
            if (toListFma != null) {
                toListFma.put(methodContext.getEnvMap(), toList);
            } else {
                listFma.put(methodContext.getEnvMap(), toList);
            }
        }
        return true;
    }

    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder("<clone-list ");
        if (toListFma!= null) {
            sb.append("to-list=\"").append(this.toListFma).append("\" ");
        }
        sb.append("list=\"").append(this.listFma).append("\" />");
        return sb.toString();
    }

    /**
     * A factory for the &lt;clone-list&gt; element.
     */
    public static final class CloneListFactory implements Factory<CloneList> {
        @Override
        public CloneList createMethodOperation(Element element, SimpleMethod simpleMethod) throws MiniLangException {
            return new CloneList(element, simpleMethod);
        }

        @Override
        public String getName() {
            return "clone-list";
        }
    }
}
