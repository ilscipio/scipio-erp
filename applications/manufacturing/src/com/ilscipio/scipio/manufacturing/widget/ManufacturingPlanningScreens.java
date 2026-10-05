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
 * MRP planning screens: MrpRuns list/detail, MrpProposals (approve/reject proposed
 * requirements), and the product-cost / where-used screens on top of EditProductBom.
 *
 * <p>SCIPIO: 4.0.0: Added.</p>
 */
public class ManufacturingPlanningScreens {

    @Screen(name = "MrpRuns", location = "component://manufacturing/widget/manufacturing/PlanningScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleMrpRuns")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "MrpRuns")
    @Action(type = ActionType.SET, field = "viewIndex", fromField = "parameters.VIEW_INDEX", valueType = "Integer")
    @Action(type = ActionType.PROPERTY_TO_FIELD, field = "viewSizeDefaultValue", resource = "widget", property = "widget.form.defaultViewSize")
    @Action(type = ActionType.SET, field = "viewSize", fromField = "parameters.VIEW_SIZE", valueType = "Integer", defaultValue = "${viewSizeDefaultValue}")
    @Action(type = ActionType.ENTITY_CONDITION, entityName = "MrpRun", list = "mrpRuns", orderBy = {"-startDate"})
    @DecoratorScreen(
        name = "CommonMrpDecorator",
        location = "component://manufacturing/widget/manufacturing/MrpScreens.xml",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.LINK, text = "${uiLabelMap.ManufacturingRunMrp}", style = "${styles.link_run_sys} ${styles.action_add}", target = "RunMrp"),
                @Widget(type = WidgetType.LINK, text = "${uiLabelMap.PageTitleFindInventoryEventPlan}", style = "${styles.link_nav} ${styles.action_view}", target = "FindInventoryEventPlan"),
                @Widget(type = WidgetType.INCLUDE_FORM, name = "ListMrpRuns", location = "component://manufacturing/widget/manufacturing/PlanningForms.xml"
            )})
        }
    )
    public interface MrpRuns {}

    @Screen(name = "MrpRunDetail", location = "component://manufacturing/widget/manufacturing/PlanningScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleMrpRunDetail")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "MrpRuns")
    @Action(type = ActionType.SET, field = "mrpId", fromField = "parameters.mrpId")
    @Action(type = ActionType.ENTITY_ONE, entityName = "MrpRun", valueField = "mrpRun")
    @Action(type = ActionType.ENTITY_CONDITION, entityName = "MrpEventView", list = "mrpEvents", conditions = {@ConditionExpr(fieldName = "mrpId", fromField = "mrpId")}, orderBy = {"productId", "eventDate"})
    @Action(type = ActionType.ENTITY_CONDITION, entityName = "MrpEventView", list = "mrpErrorEvents", conditions = {@ConditionExpr(fieldName = "mrpId", fromField = "mrpId"), @ConditionExpr(fieldName = "mrpEventTypeId", value = "ERROR")}, orderBy = {"productId", "eventDate"})
    @DecoratorScreen(
        name = "CommonMrpDecorator",
        location = "component://manufacturing/widget/manufacturing/MrpScreens.xml",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.HTML_TEMPLATE, location = "component://manufacturing/webapp/manufacturing/mrp/MrpRunDetail.ftl"
            )}, screenlets = {
                @Screenlet(includeForms = {
                    @IncludeForm(name = "ListMrpRunEvents", location = "component://manufacturing/widget/manufacturing/PlanningForms.xml"
                )})}, sections = {
                    @InlineSection(condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {
                        @Condition(type = NotEmpty.class, params = {"mrpErrorEvents"})}), widgets = @InlineWidgets(screenlets = {
                            @Screenlet(title = "${uiLabelMap.ManufacturingErrorEvents}", includeForms = {
                                @IncludeForm(name = "ListMrpRunErrorEvents", location = "component://manufacturing/widget/manufacturing/PlanningForms.xml"
                            )})}))})
        }
    )
    public interface MrpRunDetail {}

    @Screen(name = "MrpProposals", location = "component://manufacturing/widget/manufacturing/PlanningScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleMrpProposals")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "MrpProposals")
    @Action(type = ActionType.SET, field = "facilityId", fromField = "parameters.facilityId")
    @Action(type = ActionType.SET, field = "requirementTypeId", fromField = "parameters.requirementTypeId")
    @Action(type = ActionType.SET, field = "viewIndex", fromField = "parameters.VIEW_INDEX", valueType = "Integer")
    @Action(type = ActionType.PROPERTY_TO_FIELD, field = "viewSizeDefaultValue", resource = "widget", property = "widget.form.defaultViewSize")
    @Action(type = ActionType.SET, field = "viewSize", fromField = "parameters.VIEW_SIZE", valueType = "Integer", defaultValue = "${viewSizeDefaultValue}")
    @Action(type = ActionType.ENTITY_CONDITION, entityName = "Requirement", list = "proposals", conditions = {
        @ConditionExpr(fieldName = "statusId", value = "REQ_PROPOSED"),
        @ConditionExpr(fieldName = "facilityId", fromField = "facilityId", ignoreIfEmpty = true),
        @ConditionExpr(fieldName = "requirementTypeId", fromField = "requirementTypeId", ignoreIfEmpty = true)
    }, orderBy = {"requirementStartDate"})
    @Action(type = ActionType.SCRIPT, location = "component://manufacturing/webapp/manufacturing/WEB-INF/actions/mrp/MrpProposals.groovy")
    @DecoratorScreen(
        name = "CommonMrpDecorator",
        location = "component://manufacturing/widget/manufacturing/MrpScreens.xml",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.LABEL, text = "${uiLabelMap.ManufacturingMrpProposalsInfo}", style = "common-msg-result"),
                @Widget(type = WidgetType.LINK, text = "${uiLabelMap.ManufacturingCreatePurchaseOrders}", style = "${styles.link_nav} ${styles.action_view}", target = "/ordermgr/control/ApprovedProductRequirementsByVendor"),
                @Widget(type = WidgetType.INCLUDE_FORM, name = "FilterMrpProposals", location = "component://manufacturing/widget/manufacturing/PlanningForms.xml"),
                @Widget(type = WidgetType.INCLUDE_FORM, name = "ListMrpProposals", location = "component://manufacturing/widget/manufacturing/PlanningForms.xml"
            )})
        }
    )
    public interface MrpProposals {}

    @Screen(name = "ProductCost", location = "component://manufacturing/widget/manufacturing/PlanningScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleProductCost")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "productCost")
    @Action(type = ActionType.SET, field = "productId", fromField = "parameters.productId")
    @Action(type = ActionType.ENTITY_ONE, entityName = "Product", valueField = "product")
    @Action(type = ActionType.SET, field = "recalculate", value = "${parameters.recalculate == 'Y'}", valueType = "Boolean", defaultValue = "false")
    @Action(type = ActionType.SCRIPT, location = "component://manufacturing/webapp/manufacturing/WEB-INF/actions/bom/ProductCost.groovy")
    @DecoratorScreen(
        name = "CommonBomDecorator",
        location = "component://manufacturing/widget/manufacturing/BomScreens.xml",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.HTML_TEMPLATE, location = "component://manufacturing/webapp/manufacturing/bom/ProductCost.ftl"
            )})
        }
    )
    public interface ProductCost {}

    @Screen(name = "ProductWhereUsed", location = "component://manufacturing/widget/manufacturing/PlanningScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleProductWhereUsed")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "whereUsed")
    @Action(type = ActionType.SET, field = "productId", fromField = "parameters.productId")
    @Action(type = ActionType.ENTITY_ONE, entityName = "Product", valueField = "product")
    @Action(type = ActionType.SCRIPT, location = "component://manufacturing/webapp/manufacturing/WEB-INF/actions/bom/ProductWhereUsed.groovy")
    @DecoratorScreen(
        name = "CommonBomDecorator",
        location = "component://manufacturing/widget/manufacturing/BomScreens.xml",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.HTML_TEMPLATE, location = "component://manufacturing/webapp/manufacturing/bom/ProductWhereUsed.ftl"
            )})
        }
    )
    public interface ProductWhereUsed {}

}
