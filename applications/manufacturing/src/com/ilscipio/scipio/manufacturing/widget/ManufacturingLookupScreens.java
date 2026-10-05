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
package com.ilscipio.scipio.manufacturing.widget;

import com.ilscipio.scipio.widget.def.screen.*;
import com.ilscipio.scipio.widget.def.condition.Condition;
import com.ilscipio.scipio.widget.def.condition.ConditionNode;
import com.ilscipio.scipio.widget.def.condition.NestedCondition;
import com.ilscipio.scipio.widget.def.condition.NestedCondition2;
import com.ilscipio.scipio.widget.def.condition.impl.*;

/**
 * Auto-generated annotation-based widget definitions.
 *
 * <p>Generated from XML by convertXmlToAnnotation Gradle task.</p>
 *
 * <p>SCIPIO: 4.0.0: Auto-generated.</p>
 */
public class ManufacturingLookupScreens {

    @Screen(name = "LookupRouting", location = "component://manufacturing/widget/manufacturing/LookupScreens.xml", condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {@Condition(type = HasPermission.class, params = {"MANUFACTURING", "_VIEW"})}))
    @Action(type = ActionType.PROPERTY_MAP, resource = "ManufacturingUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.SET, field = "title", value = "${uiLabelMap.PageTitleLookupRouting}")
    @Action(type = ActionType.SET, field = "entityName", value = "WorkEffort")
    @Action(type = ActionType.SET, field = "requestParameters.workEffortTypeId", defaultValue = "ROUTING")
    @Action(type = ActionType.SET, field = "requestParameters.fixedAssetId")
    @Action(type = ActionType.SET, field = "queryString", fromField = "result.queryString")
    @Action(type = ActionType.SET, field = "searchFields", value = "[workEffortId]")
    @Action(type = ActionType.SET, field = "viewIndex", fromField = "parameters.VIEW_INDEX", valueType = "Integer")
    @Action(type = ActionType.PROPERTY_TO_FIELD, field = "viewSizeDefaultValue", resource = "widget", property = "widget.form.defaultViewSize")
    @Action(type = ActionType.SET, field = "viewSize", fromField = "parameters.VIEW_SIZE", valueType = "Integer", defaultValue = "${viewSizeDefaultValue}")
    @DecoratorScreen(
        name = "LookupDecorator",
        location = "component://common/widget/CommonScreens.xml",
        sections = {
            @DecoratorSection(name = "search-options", value = {
                @Widget(type = WidgetType.INCLUDE_FORM, name = "lookupRouting", location = "component://manufacturing/widget/manufacturing/FieldLookupForms.xml"
            )}),
            @DecoratorSection(name = "search-results", value = {
                @Widget(type = WidgetType.INCLUDE_FORM, name = "listLookupRouting", location = "component://manufacturing/widget/manufacturing/FieldLookupForms.xml"
            )})
        }
    )
    public interface LookupRouting {}

    @Screen(name = "LookupRoutingTask", location = "component://manufacturing/widget/manufacturing/LookupScreens.xml", condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {@Condition(type = HasPermission.class, params = {"MANUFACTURING", "_VIEW"})}))
    @Action(type = ActionType.PROPERTY_MAP, resource = "ManufacturingUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.SET, field = "title", value = "${uiLabelMap.PageTitleLookupRoutingTask}")
    @Action(type = ActionType.SET, field = "entityName", value = "WorkEffort")
    @Action(type = ActionType.SET, field = "requestParameters.workEffortTypeId", defaultValue = "ROU_TASK")
    @Action(type = ActionType.SET, field = "requestParameters.fixedAssetId")
    @Action(type = ActionType.SET, field = "queryString", fromField = "result.queryString")
    @Action(type = ActionType.SET, field = "searchFields", value = "[workEffortId, workEffortName]")
    @Action(type = ActionType.SET, field = "viewIndex", fromField = "parameters.VIEW_INDEX", valueType = "Integer")
    @Action(type = ActionType.PROPERTY_TO_FIELD, field = "viewSizeDefaultValue", resource = "widget", property = "widget.form.defaultViewSize")
    @Action(type = ActionType.SET, field = "viewSize", fromField = "parameters.VIEW_SIZE", valueType = "Integer", defaultValue = "${viewSizeDefaultValue}")
    @DecoratorScreen(
        name = "LookupDecorator",
        location = "component://common/widget/CommonScreens.xml",
        sections = {
            @DecoratorSection(name = "search-options", value = {
                @Widget(type = WidgetType.INCLUDE_FORM, name = "lookupRoutingTask", location = "component://manufacturing/widget/manufacturing/FieldLookupForms.xml"
            )}),
            @DecoratorSection(name = "search-results", value = {
                @Widget(type = WidgetType.INCLUDE_FORM, name = "listLookupRoutingTask", location = "component://manufacturing/widget/manufacturing/FieldLookupForms.xml"
            )})
        }
    )
    public interface LookupRoutingTask {}

}
