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
public class ManufacturingCostScreens {

    @Screen(name = "EditCostCalcs", location = "component://manufacturing/widget/manufacturing/CostScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleEditCostCalcs")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "costs")
    @Action(type = ActionType.SET, field = "viewIndex", fromField = "parameters.VIEW_INDEX", valueType = "Integer")
    @Action(type = ActionType.PROPERTY_TO_FIELD, field = "viewSizeDefaultValue", resource = "widget", property = "widget.form.defaultViewSize")
    @Action(type = ActionType.SET, field = "viewSize", fromField = "parameters.VIEW_SIZE", valueType = "Integer", defaultValue = "${viewSizeDefaultValue}")
    @Action(type = ActionType.PROPERTY_TO_FIELD, field = "defaultCurrencyUomId", resource = "general", property = "currency.uom.id.default", defaultValue = "USD")
    @Action(type = ActionType.ENTITY_CONDITION, entityName = "CostComponentCalc", list = "allCostComponentCalcs")
    @Action(type = ActionType.ENTITY_ONE, entityName = "CostComponentCalc", valueField = "costComponentCalc")
    @DecoratorScreen(
        name = "CommonManufacturingAppDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.INCLUDE_FORM, name = "ListCostComponentCalc", location = "component://manufacturing/widget/manufacturing/CostForms.xml"
            )}, screenlets = {
                @Screenlet(title = "${uiLabelMap.ManufacturingEditCostComponentCalc}", name = "ManufacturingCostComponentCalcPanel", collapsible = true, includeForms = {
                    @IncludeForm(name = "EditCostComponentCalc", location = "component://manufacturing/widget/manufacturing/CostForms.xml"
                )}, position = 0)})
        }
    )
    public interface EditCostCalcs {}

}
