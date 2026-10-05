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
package com.ilscipio.scipio.humanres.widget;

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
public class EmplPositionScreens {

    @Screen(name = "FindEmplPositions", location = "component://humanres/widget/EmplPositionScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "HumanResFindEmplPosition")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "FindEmplPositions")
    @DecoratorScreen(
        name = "CommonEmplPositionDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", decorators = {
                @DecoratorScreenNested(name = "FindScreenDecorator", location = "component://common/widget/CommonScreens.xml", sections = {
                    @DecoratorSectionNested(name = "menu-bar", widgets = @WidgetsForContainer4(containers = {
                        @Container4(style = "button-bar", widgets = {
                            @Widget(type = WidgetType.LINK, text = "${uiLabelMap.HumanResNewEmplPosition}", style = "${styles.link_nav} ${styles.action_add}", target = "EditEmplPosition"
                        )})})),
                        @DecoratorSectionNested(name = "search-options", widgets = @WidgetsForContainer4(value = {
                            @Widget(type = WidgetType.INCLUDE_FORM, name = "FindEmplPositions", location = "component://humanres/widget/forms/EmplPositionForms.xml"
                        )})),
                        @DecoratorSectionNested(name = "search-results", widgets = @WidgetsForContainer4(value = {
                            @Widget(type = WidgetType.INCLUDE_FORM, name = "ListEmplPositions", location = "component://humanres/widget/forms/EmplPositionForms.xml"
                        )}))})})
        }
    )
    public interface FindEmplPositions {}

    @Screen(name = "ListEmplPositionsParty", location = "component://humanres/widget/EmplPositionScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "HumanResFindEmplPosition")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "ListEmplPositions")
    @DecoratorScreen(
        name = "EmployeeDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", screenlets = {
                @Screenlet(title = "${uiLabelMap.HumanResEmployeePosition}", includeForms = {
                    @IncludeForm(name = "ListEmplPositionsParty", location = "component://humanres/widget/forms/EmplPositionForms.xml"
                )})})
        }
    )
    public interface ListEmplPositionsParty {}

    @Screen(name = "EditEmplPosition", location = "component://humanres/widget/EmplPositionScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "HumanResEditEmplPosition")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "EditEmplPosition")
    @Action(type = ActionType.SET, field = "emplPositionId", fromField = "parameters.emplPositionId")
    @Action(type = ActionType.ENTITY_ONE, entityName = "EmplPosition", valueField = "emplPosition")
    @DecoratorScreen(
        name = "CommonEmplPositionDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", sections = {
                @InlineSection(condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {
                    @Condition(type = Empty.class, params = {"emplPosition.emplPositionId"
                })}), widgets = @InlineWidgets(screenlets = {
                    @Screenlet(title = "${uiLabelMap.CommonAdd} ${uiLabelMap.FormFieldTitle_position}", includeForms = {
                        @IncludeForm(name = "EditEmplPosition", location = "component://humanres/widget/forms/EmplPositionForms.xml"
                    )})}), failWidgets = @InlineWidgets(screenlets = {
                        @Screenlet(includeForms = {
                            @IncludeForm(name = "EditEmplPosition", location = "component://humanres/widget/forms/EmplPositionForms.xml"
                        )})}))})
        }
    )
    public interface EditEmplPosition {}

    @Screen(name = "EditEmplPositionFulfillments", location = "component://humanres/widget/EmplPositionScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleListEmplPositionFulfillments")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "EditEmplPositionFulfillments")
    @Action(type = ActionType.SET, field = "emplPositionId", fromField = "parameters.emplPositionId")
    @DecoratorScreen(
        name = "CommonEmplPositionDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.INCLUDE_FORM, name = "ListEmplPositionFulfillments", location = "component://humanres/widget/forms/EmplPositionForms.xml"
            )}, screenlets = {
                @Screenlet(title = "${uiLabelMap.HumanResAddEmplPositionFulfillment}", name = "AddEmplPositionFulfillmentPanel", collapsible = true, includeForms = {
                    @IncludeForm(name = "AddEmplPositionFulfillment", location = "component://humanres/widget/forms/EmplPositionForms.xml"
                )}, position = 0)})
        }
    )
    public interface EditEmplPositionFulfillments {}

    @Screen(name = "EditEmplPositionResponsibilities", location = "component://humanres/widget/EmplPositionScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleListEmplPositionResponsibilities")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "EditEmplPositionResponsibilities")
    @Action(type = ActionType.SET, field = "emplPositionId", fromField = "parameters.emplPositionId")
    @DecoratorScreen(
        name = "CommonEmplPositionDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.INCLUDE_FORM, name = "ListEmplPositionResponsibilities", location = "component://humanres/widget/forms/EmplPositionForms.xml"
            )}, screenlets = {
                @Screenlet(title = "${uiLabelMap.HumanResAddEmplPositionResponsibility}", name = "AddEmplPositionResponsibilityPanel", collapsible = true, includeForms = {
                    @IncludeForm(name = "AddEmplPositionResponsibility", location = "component://humanres/widget/forms/EmplPositionForms.xml"
                )}, position = 0)})
        }
    )
    public interface EditEmplPositionResponsibilities {}

    @Screen(name = "EditEmplPositionReportingStructs", location = "component://humanres/widget/EmplPositionScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleListEmplPositionReportingStructs")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "EditEmplPositionReportingStructs")
    @Action(type = ActionType.SET, field = "emplPositionId", fromField = "parameters.emplPositionId")
    @DecoratorScreen(
        name = "CommonEmplPositionDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.LABEL, text = "${uiLabelMap.HumanResEditEmplPositionReportingStruct} ${uiLabelMap.CommonFor}: [${parameters.emplPositionId}]", style = "heading"
            ),
            @Widget(type = WidgetType.INCLUDE_FORM, name = "ListReportsToEmplPositionReportingStructs", location = "component://humanres/widget/forms/EmplPositionForms.xml"
            ),
            @Widget(type = WidgetType.INCLUDE_FORM, name = "ListReportedToEmplPositionReportingStructs", location = "component://humanres/widget/forms/EmplPositionForms.xml"
            )}, screenlets = {
                @Screenlet(title = "${uiLabelMap.HumanResAddReportsToEmplPositionReportingStruct}", name = "AddReportsToEmplPositionReportingStructPanel", collapsible = true, includeForms = {
                    @IncludeForm(name = "AddReportsToEmplPositionReportingStruct", location = "component://humanres/widget/forms/EmplPositionForms.xml"
                )}, position = 1),
                @Screenlet(title = "${uiLabelMap.HumanResAddReportedToEmplPositionReportingStruct}", name = "AddReportedToEmplPositionReportingStructPanel", collapsible = true, includeForms = {
                    @IncludeForm(name = "AddReportedToEmplPositionReportingStruct", location = "component://humanres/widget/forms/EmplPositionForms.xml"
                )}, position = 3)})
        }
    )
    public interface EditEmplPositionReportingStructs {}

    @Screen(name = "ListValidResponsibilities", location = "component://humanres/widget/EmplPositionScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleListValidResponsibilities")
    @DecoratorScreen(
        name = "CommonHumanResAppDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", screenlets = {
                @Screenlet(title = "${uiLabelMap.HumanResListValidResponsibility}", includeForms = {
                    @IncludeForm(name = "ListValidResponsibilities", location = "component://humanres/widget/forms/EmplPositionForms.xml", position = 1
                )}, containers = {
                    @Container(widgets = {
                        @Widget(type = WidgetType.LINK, text = "${uiLabelMap.HumanResAddValidResponsibility}", style = "${styles.link_nav} ${styles.action_add}", target = "EditValidResponsibility"
                    )}, position = 0)})})
        }
    )
    public interface ListValidResponsibilities {}

    @Screen(name = "EditValidResponsibility", location = "component://humanres/widget/EmplPositionScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleEditValidResponsibility")
    @DecoratorScreen(
        name = "CommonHumanResAppDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", screenlets = {
                @Screenlet(title = "${uiLabelMap.HumanResAddValidResponsibility}", includeForms = {
                    @IncludeForm(name = "AddValidResponsibility", location = "component://humanres/widget/forms/EmplPositionForms.xml", position = 1
                )}, containers = {
                    @Container(widgets = {
                        @Widget(type = WidgetType.LINK, text = "${uiLabelMap.HumanResAddValidResponsibility}", style = "${styles.link_nav} ${styles.action_add}", target = "EditValidResponsibility"
                    )}, position = 0)})})
        }
    )
    public interface EditValidResponsibility {}

    @Screen(name = "EmplPositionView", location = "component://humanres/widget/EmplPositionScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "HumanResEmplPositionSummary")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "EmplPositionView")
    @Action(type = ActionType.SET, field = "emplPositionId", fromField = "parameters.emplPositionId")
    @Action(type = ActionType.ENTITY_ONE, entityName = "EmplPosition", valueField = "emplPosition")
    @DecoratorScreen(
        name = "CommonEmplPositionDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.INCLUDE_MENU, name = "EmplPositionViewSubTarBar", location = "component://humanres/widget/HumanresMenus.xml"
            )}, containers = {
                @Container(style = "${styles.grid_row}", containers = {
                    @Container2(style = "${styles.grid_large}6 ${styles.grid_cell}", includeScreens = {
                        @IncludeScreen(name = "EmplPositionFulfilmentView", location = "component://humanres/widget/EmplPositionScreens.xml", position = 1
                    ),
                    @IncludeScreen(name = "EmplPositionResponsibilityView", location = "component://humanres/widget/EmplPositionScreens.xml", position = 2
                )}, screenlets = {
                    @ScreenletNested(includeForms = {
                    @IncludeForm(name = "EmplPositionInfo", location = "component://humanres/widget/forms/EmplPositionForms.xml"
                
                )}, position = 0)}),
                @Container2(style = "${styles.grid_large}6 ${styles.grid_cell}", includeScreens = {
                    @IncludeScreen(name = "EmplPositionReportsToView", location = "component://humanres/widget/EmplPositionScreens.xml"
                ),
                @IncludeScreen(name = "EmplPositionReportedToView", location = "component://humanres/widget/EmplPositionScreens.xml"
            )})})})
        }
    )
    public interface EmplPositionView {}

    @Screen(name = "EmplPositionFulfilmentView", location = "component://humanres/widget/EmplPositionScreens.xml")
    @Action(type = ActionType.ENTITY_CONDITION, entityName = "EmplPositionFulfillment", list = "emplPositionFulfillments", conditions = {@ConditionExpr(fieldName = "emplPositionId", operator = "equals", fromField = "parameters.emplPositionId")}, orderBy = {"fromDate"})
    @Section(widgets = @Widgets(screenlets = {@Screenlet(title = "${uiLabelMap.HumanResPositionFulfillments}", includeForms = {@IncludeForm(name = "ListEmplPositionFulfilmentInfo", location = "component://humanres/widget/forms/EmplPositionForms.xml")})}))
    public interface EmplPositionFulfilmentView {}

    @Screen(name = "EmplPositionResponsibilityView", location = "component://humanres/widget/EmplPositionScreens.xml")
    @Action(type = ActionType.ENTITY_CONDITION, entityName = "EmplPositionResponsibility", list = "emplPositionResponsibilities", conditions = {@ConditionExpr(fieldName = "emplPositionId", operator = "equals", fromField = "parameters.emplPositionId")}, orderBy = {"fromDate"})
    @Section(widgets = @Widgets(screenlets = {@Screenlet(title = "${uiLabelMap.HumanResEmplPositionResponsibilities}", includeForms = {@IncludeForm(name = "ListEmplPositionResponsibilityInfo", location = "component://humanres/widget/forms/EmplPositionForms.xml")})}))
    public interface EmplPositionResponsibilityView {}

    @Screen(name = "EmplPositionReportsToView", location = "component://humanres/widget/EmplPositionScreens.xml")
    @Action(type = ActionType.ENTITY_CONDITION, entityName = "EmplPositionReportingStruct", list = "emplPositionReportingStructs", conditions = {@ConditionExpr(fieldName = "emplPositionIdManagedBy", operator = "equals", fromField = "parameters.emplPositionId")}, orderBy = {"fromDate"})
    @Section(widgets = @Widgets(screenlets = {@Screenlet(title = "${uiLabelMap.HumanResEmplPositionReportingStruct}: ${uiLabelMap.HumanResReportsTo}", includeForms = {@IncludeForm(name = "ListEmplPositionReportsToInfo", location = "component://humanres/widget/forms/EmplPositionForms.xml")})}))
    public interface EmplPositionReportsToView {}

    @Screen(name = "EmplPositionReportedToView", location = "component://humanres/widget/EmplPositionScreens.xml")
    @Action(type = ActionType.ENTITY_CONDITION, entityName = "EmplPositionReportingStruct", list = "emplPositionReportingStructs", conditions = {@ConditionExpr(fieldName = "emplPositionIdReportingTo", operator = "equals", fromField = "parameters.emplPositionId")}, orderBy = {"fromDate"})
    @Section(widgets = @Widgets(screenlets = {@Screenlet(title = "${uiLabelMap.HumanResEmplPositionReportingStruct}: ${uiLabelMap.HumanResReportedTo}", includeForms = {@IncludeForm(name = "ListEmplPositionReportedToInfo", location = "component://humanres/widget/forms/EmplPositionForms.xml")})}))
    public interface EmplPositionReportedToView {}

    @Screen(name = "EditInternalOrgFtl", location = "component://humanres/widget/EmplPositionScreens.xml")
    @Section(widgets = @Widgets(value = {@Widget(type = WidgetType.HTML_TEMPLATE, location = "component://humanres/webapp/humanres/humanres/internalorg/editinternalorg.ftl")}))
    public interface EditInternalOrgFtl {}

    @Screen(name = "EditInternalOrgOnlyForm", location = "component://humanres/widget/EmplPositionScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleEditInternalOrg")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "EditInternalOrg")
    @Action(type = ActionType.PROPERTY_MAP, resource = "CommonUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.ENTITY_ONE, entityName = "PartyRole", valueField = "partyRole")
    @Section(widgets = @Widgets(value = {@Widget(type = WidgetType.INCLUDE_FORM, name = "ListInternalOrg", location = "component://humanres/widget/forms/EmplPositionForms.xml")}))
    public interface EditInternalOrgOnlyForm {}

    @Screen(name = "RemoveInternalOrgFtl", location = "component://humanres/widget/EmplPositionScreens.xml")
    @Section(widgets = @Widgets(value = {@Widget(type = WidgetType.HTML_TEMPLATE, location = "component://humanres/webapp/humanres/humanres/internalorg/removeinternalorg.ftl")}))
    public interface RemoveInternalOrgFtl {}

    @Screen(name = "ScipioOpenPositions", location = "component://humanres/widget/EmplPositionScreens.xml")
    @Action(type = ActionType.ENTITY_CONDITION, entityName = "JobRequisitionAndEmplPosition", list = "positionList", conditions = {@ConditionExpr(fieldName = "statusId", value = "EMPL_POS_ACTIVE")}, orderBy = {"-estimatedFromDate"})
    @Section(widgets = @Widgets(value = {@Widget(type = WidgetType.HTML_TEMPLATE, location = "component://humanres/webapp/humanres/humanres/emplposition/ScipioPositionsSummary.ftl")}))
    public interface ScipioOpenPositions {}

}
