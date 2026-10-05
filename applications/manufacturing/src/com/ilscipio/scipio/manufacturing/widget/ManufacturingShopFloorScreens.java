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
 * Screen definitions for the manufacturing shop floor task declaration feature.
 *
 * <p>SCIPIO: 4.0.0: Added for the shop floor screen.</p>
 */
public class ManufacturingShopFloorScreens {

    @Screen(name = "ShopFloor", location = "component://manufacturing/widget/manufacturing/ShopFloorScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "ManufacturingShopFloor")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "ShopFloor")
    @Action(type = ActionType.ENTITY_CONDITION, entityName = "Enumeration", list = "rejectReasons", conditions = {@ConditionExpr(fieldName = "enumTypeId", value = "PRUN_REJECT_REASON")}, orderBy = {"sequenceId"})
    @Action(type = ActionType.SERVICE, serviceName = "getShopFloorTasks", fieldMaps = {
        @FieldMap(fieldName = "fixedAssetId", fromField = "parameters.fixedAssetId"),
        @FieldMap(fieldName = "facilityId", fromField = "parameters.facilityId"),
        @FieldMap(fieldName = "includeCompleted", fromField = "parameters.includeCompleted")
    })
    @DecoratorScreen(
        name = "CommonManufacturingAppDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", screenlets = {
                @Screenlet(includeForms = {
                    @IncludeForm(name = "ShopFloorFilter", location = "component://manufacturing/widget/manufacturing/ShopFloorForms.xml"
                )})}, sections = {
                    @InlineSection(condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {
                        @Condition(type = Always.class
                    )}), widgets = @InlineWidgets(value = {
                        @Widget(type = WidgetType.HTML_TEMPLATE, location = "component://manufacturing/webapp/manufacturing/shopfloor/ShopFloor.ftl"
                    )}))})
        }
    )
    public interface ShopFloor {}

    @Screen(name = "CreateProductionRunFromOrder", location = "component://manufacturing/widget/manufacturing/ShopFloorScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "ManufacturingCreateProductionRunFromOrder")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "jobshop")
    @DecoratorScreen(
        name = "CommonManufacturingAppDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", screenlets = {
                @Screenlet(title = "${uiLabelMap.ManufacturingCreateProductionRunFromOrder}", includeForms = {
                    @IncludeForm(name = "CreateProductionRunFromOrder", location = "component://manufacturing/widget/manufacturing/ShopFloorForms.xml"
                )})})
        }
    )
    public interface CreateProductionRunFromOrder {}

}
