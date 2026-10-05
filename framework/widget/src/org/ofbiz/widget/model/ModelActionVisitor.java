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

import org.ofbiz.widget.model.AbstractModelAction.ClearField;

/**
 *  A <code>ModelAction</code> visitor.
 */
public interface ModelActionVisitor {

    void visit(ModelFormAction.CallParentActions callParentActions) throws Exception;

    void visit(AbstractModelAction.EntityAnd entityAnd) throws Exception;

    void visit(AbstractModelAction.EntityCondition entityCondition) throws Exception;

    void visit(AbstractModelAction.EntityOne entityOne) throws Exception;

    void visit(AbstractModelAction.GetRelated getRelated) throws Exception;

    void visit(AbstractModelAction.GetRelatedOne getRelatedOne) throws Exception;

    void visit(AbstractModelAction.PropertyMap propertyMap) throws Exception;

    void visit(AbstractModelAction.PropertyToField propertyToField) throws Exception;

    void visit(AbstractModelAction.Script script) throws Exception;

    void visit(AbstractModelAction.Service service) throws Exception;

    void visit(AbstractModelAction.SetField setField) throws Exception;

    void visit(ModelFormAction.Service service) throws Exception;

    void visit(@SuppressWarnings("deprecation") ModelMenuAction.SetField setField) throws Exception; // SCIPIO: Deprecated

    void visit(ModelTreeAction.Script script) throws Exception;

    void visit(ModelTreeAction.Service service) throws Exception;

    void visit(ModelTreeAction.EntityAnd entityAnd) throws Exception;

    void visit(ModelTreeAction.EntityCondition entityCondition) throws Exception;

    void visit(ClearField clearField) throws Exception; // SCIPIO: Added 2019-02-04
}
