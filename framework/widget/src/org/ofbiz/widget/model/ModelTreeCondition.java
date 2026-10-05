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
package org.ofbiz.widget.model;

import java.io.Serializable;

import org.w3c.dom.Element;

/**
 * Models the &lt;condition&gt; element.
 *
 * @see <code>widget-tree.xsd</code>
 */
@SuppressWarnings("serial")
public class ModelTreeCondition implements Serializable {
    //private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());
    private final ModelCondition condition;

    public ModelTreeCondition(ModelTree modelTree, Element conditionElement) {
        this.condition = AbstractModelCondition.DEFAULT_CONDITION_FACTORY.newInstance(modelTree, conditionElement);
    }

    public ModelCondition getCondition() {
        return condition;
    }
}
