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
public class ManufacturingMrpScreens {

    @Screen(name = "CommonMrpDecorator", location = "component://manufacturing/widget/manufacturing/MrpScreens.xml")
    @Action(type = ActionType.SET, field = "activeSubMenu", fromField = "activeSubMenu", defaultValue = "Mrp")
    @DecoratorScreen(
        name = "CommonManufacturingAppDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {@Widget(type = WidgetType.DECORATOR_SECTION_INCLUDE, name = "body")})
        }
    )
    public interface CommonMrpDecorator {}

    @Screen(name = "MrpExecution", location = "component://manufacturing/widget/manufacturing/MrpScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "ManufacturingRunMrp")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "RunMrp")
    @DecoratorScreen(
        name = "CommonMrpDecorator",
        location = "component://manufacturing/widget/manufacturing/MrpScreens.xml",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.HTML_TEMPLATE, location = "component://manufacturing/webapp/manufacturing/mrp/RunMrpInfo.ftl"
            )}, screenlets = {
                @Screenlet(includeForms = {
                    @IncludeForm(name = "RunMrp", location = "component://manufacturing/widget/manufacturing/MrpForms.xml"
                )})})
        }
    )
    public interface MrpExecution {}

    @Screen(name = "FindMrpPlannedEvents", location = "component://manufacturing/widget/manufacturing/MrpScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleFindInventoryEventPlan")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "findInventoryEventPlan")
    @Action(type = ActionType.SET, field = "viewIndex", fromField = "parameters.VIEW_INDEX", valueType = "Integer")
    @Action(type = ActionType.PROPERTY_TO_FIELD, field = "viewSizeDefaultValue", resource = "widget", property = "widget.form.defaultViewSize")
    @Action(type = ActionType.SET, field = "viewSize", fromField = "parameters.VIEW_SIZE", valueType = "Integer", defaultValue = "${viewSizeDefaultValue}")
    @Action(type = ActionType.ENTITY_CONDITION, entityName = "JobSandbox", list = "mrpActiveJobs", conditions = {@ConditionExpr(fieldName = "serviceName", value = "executeMrp")}, orderBy = {"-createdStamp"})
    @Action(type = ActionType.ENTITY_CONDITION, entityName = "JobSandbox", list = "mrpRunningJobs", conditions = {@ConditionExpr(fieldName = "serviceName", value = "executeMrp"), @ConditionExpr(fieldName = "statusId", value = "SERVICE_RUNNING")}, orderBy = {"-createdStamp"})
    @Action(type = ActionType.ENTITY_CONDITION, entityName = "JobSandbox", list = "lastFinishedJobs", conditions = {@ConditionExpr(fieldName = "serviceName", value = "executeMrp")}, orderBy = {"-finishDateTime"})
    @Action(type = ActionType.SCRIPT, location = "component://manufacturing/webapp/manufacturing/WEB-INF/actions/mrp/FindInventoryEventPlan.groovy")
    @DecoratorScreen(
        name = "CommonMrpDecorator",
        location = "component://manufacturing/widget/manufacturing/MrpScreens.xml",
        sections = {
            @DecoratorSection(name = "body", screenlets = {
                @Screenlet(title = "${uiLabelMap.ManufacturingMrpJobLastExecuted}", includeForms = {
                    @IncludeForm(name = "ListFinishedMrpJobs", location = "component://manufacturing/widget/manufacturing/MrpForms.xml"
                )}),
                @Screenlet(title = "${uiLabelMap.ManufacturingMrpJobScheduledOrRunning}", includeForms = {
                    @IncludeForm(name = "ListRunningMrpJobs", location = "component://manufacturing/widget/manufacturing/MrpForms.xml"
                )})}, sections = {
                    @InlineSection(condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {
                        @Condition(type = Empty.class, params = {"mrpRunningJobs"})}), widgets = @InlineWidgets(value = {
                            @Widget(type = WidgetType.HTML_TEMPLATE, location = "component://manufacturing/webapp/manufacturing/mrp/findInventoryEventPlan.ftl"
                        )}), failWidgets = @InlineWidgets(value = {
                            @Widget(type = WidgetType.LABEL, text = "${uiLabelMap.ManufacturingMrpJobIsRunning}", style = "common-msg-result"
                        )}))})
        }
    )
    public interface FindMrpPlannedEvents {}

}
