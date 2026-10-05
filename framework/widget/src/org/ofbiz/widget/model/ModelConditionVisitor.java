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

import org.ofbiz.widget.model.AbstractModelCondition.And;
import org.ofbiz.widget.model.AbstractModelCondition.IfCompare;
import org.ofbiz.widget.model.AbstractModelCondition.IfCompareField;
import org.ofbiz.widget.model.AbstractModelCondition.IfComponent;
import org.ofbiz.widget.model.AbstractModelCondition.IfEmpty;
import org.ofbiz.widget.model.AbstractModelCondition.IfEntity;
import org.ofbiz.widget.model.AbstractModelCondition.IfEntityPermission;
import org.ofbiz.widget.model.AbstractModelCondition.IfHasPermission;
import org.ofbiz.widget.model.AbstractModelCondition.IfRegexp;
import org.ofbiz.widget.model.AbstractModelCondition.IfService;
import org.ofbiz.widget.model.AbstractModelCondition.IfServicePermission;
import org.ofbiz.widget.model.AbstractModelCondition.IfValidateMethod;
import org.ofbiz.widget.model.AbstractModelCondition.Not;
import org.ofbiz.widget.model.AbstractModelCondition.Or;
import org.ofbiz.widget.model.AbstractModelCondition.Xor;
import org.ofbiz.widget.model.ModelScreenCondition.IfEmptySection;

/**
 *  A <code>ModelCondition</code> visitor.
 */
public interface ModelConditionVisitor {

    void visit(And and) throws Exception;

    void visit(IfCompare ifCompare) throws Exception;

    void visit(IfCompareField ifCompareField) throws Exception;

    void visit(IfEmpty ifEmpty) throws Exception;

    void visit(IfEntityPermission ifEntityPermission) throws Exception;

    void visit(IfHasPermission ifHasPermission) throws Exception;

    void visit(IfRegexp ifRegexp) throws Exception;

    void visit(IfServicePermission ifServicePermission) throws Exception;

    void visit(IfValidateMethod ifValidateMethod) throws Exception;

    void visit(Not not) throws Exception;

    void visit(Or or) throws Exception;

    void visit(Xor xor) throws Exception;

    void visit(ModelMenuCondition modelMenuCondition) throws Exception;

    void visit(ModelTreeCondition modelTreeCondition) throws Exception;

    void visit(IfEmptySection ifEmptySection) throws Exception;

    void visit(IfComponent ifComponent) throws Exception; // SCIPIO

    void visit(IfEntity ifEntity) throws Exception; // SCIPIO

    void visit(IfService ifService) throws Exception; // SCIPIO
}
