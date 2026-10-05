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
import org.ofbiz.widget.model.AbstractModelAction.EntityAnd;
import org.ofbiz.widget.model.AbstractModelAction.EntityCondition;
import org.ofbiz.widget.model.AbstractModelAction.EntityOne;
import org.ofbiz.widget.model.AbstractModelAction.GetRelated;
import org.ofbiz.widget.model.AbstractModelAction.GetRelatedOne;
import org.ofbiz.widget.model.AbstractModelAction.PropertyMap;
import org.ofbiz.widget.model.AbstractModelAction.PropertyToField;
import org.ofbiz.widget.model.AbstractModelAction.Script;
import org.ofbiz.widget.model.AbstractModelAction.Service;
import org.ofbiz.widget.model.AbstractModelAction.SetField;
import org.ofbiz.widget.model.ModelFormAction.CallParentActions;

/**
 * An object that generates XML from widget models.
 * The generated XML is unformatted - if you want to
 * "pretty print" the XML, then use a transformer.
 *
 */
public class XmlWidgetActionVisitor extends XmlAbstractWidgetVisitor implements ModelActionVisitor {

    //private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());

    public XmlWidgetActionVisitor(Appendable writer) {
        super(writer);
    }

    @Override
    public void visit(CallParentActions callParentActions) throws Exception {
        writer.append("<call-parent-actions/>");
    }

    @Override
    public void visit(EntityAnd entityAnd) throws Exception {
        writer.append("<entity-and/>");
        // TODO: Create ByAndFinder visitor
    }

    @Override
    public void visit(org.ofbiz.widget.model.ModelTreeAction.EntityAnd entityAnd) throws Exception {
        writer.append("<entity-and/>");
        // TODO: Create ByAndFinder visitor
    }

    @Override
    public void visit(EntityCondition entityCondition) throws Exception {
        writer.append("<entity-condition/>");
        // TODO: Create ByConditionFinder visitor
    }

    @Override
    public void visit(org.ofbiz.widget.model.ModelTreeAction.EntityCondition entityCondition) throws Exception {
        writer.append("<entity-condition/>");
        // TODO: Create ByConditionFinder visitor
    }

    @Override
    public void visit(EntityOne entityOne) throws Exception {
        writer.append("<entity-one/>");
        // TODO: Create PrimaryKeyFinder visitor
    }

    @Override
    public void visit(GetRelated getRelated) throws Exception {
        writer.append("<get-related");
        visitAttribute("value-field", getRelated.getValueNameAcsr());
        visitAttribute("list", getRelated.getListNameAcsr());
        visitAttribute("relation-name", getRelated.getRelationName());
        visitAttribute("map", getRelated.getMapAcsr());
        visitAttribute("order-by-list", getRelated.getOrderByListAcsr());
        visitAttribute("use-cache", getRelated.getUseCache());
        writer.append("/>");
    }

    @Override
    public void visit(GetRelatedOne getRelatedOne) throws Exception {
        writer.append("<get-related-one");
        visitAttribute("value-field", getRelatedOne.getValueNameAcsr());
        visitAttribute("to-value-field", getRelatedOne.getToValueNameAcsr());
        visitAttribute("relation-name", getRelatedOne.getRelationName());
        visitAttribute("use-cache", getRelatedOne.getUseCache());
        writer.append("/>");
    }

    @Override
    public void visit(PropertyMap propertyMap) throws Exception {
        writer.append("<property-map");
        visitAttribute("resource", propertyMap.getResourceExdr());
        visitAttribute("map-name", propertyMap.getMapNameAcsr());
        visitAttribute("global", propertyMap.getGlobalExdr());
        writer.append("/>");
    }

    @Override
    public void visit(PropertyToField propertyToField) throws Exception {
        writer.append("<property-map");
        visitAttribute("resource", propertyToField.getResourceExdr());
        visitAttribute("property", propertyToField.getPropertyExdr());
        visitAttribute("field", propertyToField.getFieldAcsr());
        visitAttribute("default", propertyToField.getDefaultExdr());
        visitAttribute("no-locale", propertyToField.getNoLocale());
        visitAttribute("arg-list-name", propertyToField.getArgListAcsr());
        visitAttribute("global", propertyToField.getGlobalExdr());
        writer.append("/>");
    }

    @Override
    public void visit(Script script) throws Exception {
        writer.append("<script");
        visitAttribute("location", script.getLocation());
        visitAttribute("method", script.getMethod());
        writer.append("/>");
    }

    @Override
    public void visit(org.ofbiz.widget.model.ModelTreeAction.Script script) throws Exception {
        writer.append("<script");
        visitAttribute("location", script.getLocation());
        visitAttribute("method", script.getMethod());
        writer.append("/>");
    }

    @Override
    public void visit(Service service) throws Exception {
        writer.append("<service");
        visitAttribute("service-name", service.getServiceNameExdr());
        visitAttribute("result-map", service.getResultMapNameAcsr());
        visitAttribute("auto-field-map", service.getAutoFieldMapExdr());
        writer.append(">");
        // TODO: Create Field Map visitor
        writer.append("<field-map/>");
        writer.append("</service>");
    }

    @Override
    public void visit(org.ofbiz.widget.model.ModelFormAction.Service service) throws Exception {
        writer.append("<service");
        visitAttribute("service-name", service.getServiceNameExdr());
        visitAttribute("result-map", service.getResultMapNameAcsr());
        visitAttribute("auto-field-map", service.getAutoFieldMapExdr());
        visitAttribute("result-map-list", service.getResultMapListNameExdr());
        visitAttribute("ignore-error", service.getIgnoreError());
        writer.append(">");
        // TODO: Create Field Map visitor
        writer.append("<field-map/>");
        writer.append("</service>");
    }

    @Override
    public void visit(org.ofbiz.widget.model.ModelTreeAction.Service service) throws Exception {
        writer.append("<service");
        visitAttribute("service-name", service.getServiceNameExdr());
        visitAttribute("result-map", service.getResultMapNameAcsr());
        visitAttribute("auto-field-map", service.getAutoFieldMapExdr());
        visitAttribute("result-map-list", service.getResultMapListNameExdr());
        visitAttribute("result-map-value", service.getResultMapValueNameExdr());
        visitAttribute("value", service.getValueNameExdr());
        writer.append(">");
        // TODO: Create Field Map visitor
        writer.append("<field-map/>");
        writer.append("</service>");
    }

    @Override
    public void visit(SetField setField) throws Exception {
        writer.append("<set");
        visitAttribute("field", setField.getField());
        visitAttribute("from-field", setField.getFromField());
        visitAttribute("value", setField.getValueExdr());
        visitAttribute("default-value", setField.getDefaultExdr());
        visitAttribute("global", setField.getGlobalExdr());
        visitAttribute("type", setField.getType());
        visitAttribute("to-scope", setField.getToScope());
        visitAttribute("from-scope", setField.getFromScope());
        writer.append("/>");
    }

    @SuppressWarnings("deprecation") // SCIPIO: Deprecated
    @Override
    public void visit(org.ofbiz.widget.model.ModelMenuAction.SetField setField) throws Exception {
        writer.append("<set");
        visitAttribute("field", setField.getField());
        visitAttribute("from-field", setField.getFromField());
        visitAttribute("value", setField.getValueExdr());
        visitAttribute("default-value", setField.getDefaultExdr());
        visitAttribute("global", setField.getGlobalExdr());
        visitAttribute("type", setField.getType());
        visitAttribute("to-scope", setField.getToScope());
        visitAttribute("from-scope", setField.getFromScope());
        writer.append("/>");
    }

    @Override
    public void visit(ClearField clearField) throws Exception { // SCIPIO: Added 2019-02-04
        writer.append("<clear-field");
        visitAttribute("field", clearField.getField());
        writer.append("/>");
    }
}
