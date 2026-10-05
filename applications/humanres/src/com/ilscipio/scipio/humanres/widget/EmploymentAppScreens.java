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
public class EmploymentAppScreens {

    @Screen(name = "FindEmploymentApps", location = "component://humanres/widget/EmploymentAppScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "HumanResFindEmploymentApp")
    @Action(type = ActionType.SET, field = "employmentAppCtx", fromField = "parameters")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "FindEmploymentApps")
    @DecoratorScreen(
        name = "CommonEmploymentAppDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", decorators = {
                @DecoratorScreenNested(name = "FindScreenDecorator", location = "component://common/widget/CommonScreens.xml", sections = {
                    @DecoratorSectionNested(name = "menu-bar", widgets = @WidgetsForContainer4(value = {
                        @Widget(type = WidgetType.INCLUDE_MENU, name = "EmploymentAppGeneralSubTabBar", location = "component://humanres/widget/HumanresMenus.xml"
                    )})),
                    @DecoratorSectionNested(name = "search-options", widgets = @WidgetsForContainer4(value = {
                        @Widget(type = WidgetType.INCLUDE_FORM, name = "FindEmploymentApps", location = "component://humanres/widget/forms/EmploymentAppForms.xml"
                    )})),
                    @DecoratorSectionNested(name = "search-results", widgets = @WidgetsForContainer4(value = {
                        @Widget(type = WidgetType.INCLUDE_FORM, name = "ListEmploymentApps", location = "component://humanres/widget/forms/EmploymentAppForms.xml"
                    )}))})})
        }
    )
    public interface FindEmploymentApps {}

    @Screen(name = "EditEmploymentApps", location = "component://humanres/widget/EmploymentAppScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleViewEmploymentApp")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "EditEmploymentApps")
    @Action(type = ActionType.SET, field = "referredByPartyId", fromField = "parameters.partyId")
    @Action(type = ActionType.SET, field = "employmentAppCtx.applicationId", fromField = "parameters.applicationId")
    @Action(type = ActionType.SET, field = "parameters.insideEmployee", value = "true")
    @DecoratorScreen(
        name = "CommonPartyDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.INCLUDE_FORM, name = "ListEmploymentApps", location = "component://humanres/widget/forms/EmploymentAppForms.xml"
            )}, screenlets = {
                @Screenlet(title = "${uiLabelMap.CommonAdd} ${uiLabelMap.HumanResEmploymentApp}", name = "AddEmploymentAppPanel", collapsible = true, includeForms = {
                    @IncludeForm(name = "AddEmploymentApp", location = "component://humanres/widget/forms/EmploymentAppForms.xml"
                )})})
        }
    )
    public interface EditEmploymentApps {}

    @Screen(name = "NewEmploymentApp", location = "component://humanres/widget/EmploymentAppScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "HumanResNewEmploymentApp")
    @Action(type = ActionType.SET, field = "applicationId", fromField = "parameters.applicationId")
    @Action(type = ActionType.ENTITY_ONE, entityName = "EmploymentApp", valueField = "employmentApp")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "NewEmploymentApp")
    @DecoratorScreen(
        name = "CommonEmploymentAppDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", screenlets = {
                @Screenlet(includeForms = {
                    @IncludeForm(name = "AddEmploymentApp", location = "component://humanres/widget/forms/EmploymentAppForms.xml"
                )})})
        }
    )
    public interface NewEmploymentApp {}

    @Screen(name = "EditEmploymentApp", location = "component://humanres/widget/EmploymentAppScreens.xml")
    @Action(type = ActionType.SET, field = "applicationId", fromField = "parameters.applicationId")
    @Action(type = ActionType.ENTITY_ONE, entityName = "EmploymentApp", valueField = "employmentApp")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "EditEmploymentApp")
    @Section(condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {@Condition(type = Empty.class, params = {"employmentApp.applicationId"})}), widgets = @Widgets(sections = {@SectionNested(actions = @Actions(value = {@Action(type = ActionType.SET, field = "titleProperty", value = "HumanResNewEmploymentApp")}), widgets = @WidgetsForContainer(decorator = @DecoratorScreenNested(name = "CommonEmploymentAppDecorator", location = "${parameters.mainDecoratorLocation}", sections = {@DecoratorSectionNested(name = "body", widgets = @WidgetsForContainer4(screenlets = {@ScreenletNested(navigationMenuName = "EmploymentAppGeneralSubTabBar", includeForms = {
                    @IncludeForm(name = "AddEmploymentApp", location = "component://humanres/widget/forms/EmploymentAppForms.xml", position = 1
                )}, includeScreens = {
                    @IncludeScreen(name = "EditEmploymentAppInfoBox", location = "component://humanres/widget/EmploymentAppScreens.xml", position = 2
                )}, includeMenus = {
                    @IncludeMenu(name = "EmploymentAppGeneralSubTabBar", location = "component://humanres/widget/HumanresMenus.xml", position = 0
                )})}))})))}), failWidgets = @Widgets(sections = {@SectionNested(actions = @Actions(value = {@Action(type = ActionType.SET, field = "titleProperty", value = "HumanResEditEmploymentApp")}), widgets = @WidgetsForContainer(decorator = @DecoratorScreenNested(name = "CommonEmploymentAppDecorator", location = "${parameters.mainDecoratorLocation}", sections = {@DecoratorSectionNested(name = "body", widgets = @WidgetsForContainer4(screenlets = {@ScreenletNested(navigationMenuName = "EmploymentAppEditSubTabBar", includeForms = {
                    @IncludeForm(name = "EditEmploymentApp", location = "component://humanres/widget/forms/EmploymentAppForms.xml", position = 1
                )}, includeScreens = {
                    @IncludeScreen(name = "EditEmploymentAppInfoBox", location = "component://humanres/widget/EmploymentAppScreens.xml", position = 2
                ),
                @IncludeScreen(name = "EmploymentAppResumeContent", location = "component://humanres/widget/EmploymentAppScreens.xml", position = 3
                )}, includeMenus = {
                    @IncludeMenu(name = "EmploymentAppEditSubTabBar", location = "component://humanres/widget/HumanresMenus.xml", position = 0
                )})}))})))}))
    public interface EditEmploymentApp {}

    @Screen(name = "EditEmploymentAppInfoBox", location = "component://humanres/widget/EmploymentAppScreens.xml")
    @Section(widgets = @Widgets(value = {@Widget(type = WidgetType.HTML_TEMPLATE, content = "<#-- NOTE: this code works around escaping in a safe manner. TODO: use a utility later -->\n                    <#assign posLink><a href=\"<@pageUrl uri=\"FindEmplPositions\"/>\">${getLabel('HumanResEmployeePosition')}</a></#assign>\n                    <#assign jobReqLink><a href=\"<@pageUrl uri=\"FindJobRequisitions\"/>\">${getLabel('HumanResJobRequisition')}</a></#assign>\n                    <@alert type=\"info\">** ${getLabel('HumanResEitherPositionOrRequisitionMustBeSpecifiedSubst','',{\"position\":\"POS\",\"jobReq\":\"JOBREQ\"})?replace(\"POS\", posLink)?replace(\"JOBREQ\", jobReqLink)}</@alert>")}))
    public interface EditEmploymentAppInfoBox {}

    @Screen(name = "ViewEmploymentApp", location = "component://humanres/widget/EmploymentAppScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "HumanResEmploymentApplication")
    @Action(type = ActionType.SET, field = "applicationId", fromField = "parameters.applicationId")
    @Action(type = ActionType.ENTITY_ONE, entityName = "EmploymentApp", valueField = "employmentApp")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "ViewEmploymentApp")
    @Action(type = ActionType.SET, field = "resumeReadOnly", value = "true", valueType = "Boolean")
    @DecoratorScreen(
        name = "CommonEmploymentAppDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", screenlets = {
                @Screenlet(includeForms = {
                    @IncludeForm(name = "ViewEmploymentApp", location = "component://humanres/widget/forms/EmploymentAppForms.xml", position = 1
                )}, includeScreens = {
                    @IncludeScreen(name = "EmploymentAppResumeContent", location = "component://humanres/widget/EmploymentAppScreens.xml", position = 2
                )}, includeMenus = {
                    @IncludeMenu(name = "EmploymentAppEditSubTabBar", location = "component://humanres/widget/HumanresMenus.xml", position = 0
                )})})
        }
    )
    public interface ViewEmploymentApp {}

    @Screen(name = "EmploymentAppResumeContent", location = "component://humanres/widget/EmploymentAppScreens.xml")
    @Action(type = ActionType.SET, field = "resumePartyId", fromField = "resumePartyId", defaultValue = "${employmentApp.applyingPartyId}")
    @Action(type = ActionType.SET, field = "partyId", fromField = "resumePartyId")
    @Action(type = ActionType.SET, field = "resumeReadOnly", fromField = "resumeReadOnly", valueType = "Boolean", defaultValue = "false")
    @Action(type = ActionType.SET, field = "pcntExtraParams", valueType = "Object")
    @Action(type = ActionType.SET, field = "pcntExtraParams.resumeReadOnly", fromField = "resumeReadOnly")
    @Action(type = ActionType.SET, field = "pcntExtraParams.applicationId", fromField = "applicationId")
    @Action(type = ActionType.SET, field = "pcntTitle", value = "${uiLabelMap.HumanResPartyResume}: ${partyId}")
    @Action(type = ActionType.SET, field = "pcntAttachTitle", value = "${uiLabelMap.CommonUpload}")
    @Action(type = ActionType.SET, field = "pcntNoAttach", fromField = "resumeReadOnly", valueType = "Boolean", defaultValue = "false")
    @Action(type = ActionType.SET, field = "pcntUploadUri", value = "uploadEmplAppResume")
    @Action(type = ActionType.SET, field = "pcntCntListLoc", value = "component://humanres/widget/EmploymentAppScreens.xml#EmploymentAppResumeList")
    @Action(type = ActionType.SET, field = "pcntPartyContentTypeId", value = "RESUME")
    @Action(type = ActionType.SET, field = "pcntAllowPublic", value = "false", valueType = "Boolean")
    @Section(widgets = @Widgets(value = {@Widget(type = WidgetType.INCLUDE_SCREEN, name = "Content", location = "component://party/widget/partymgr/ProfileScreens.xml")}))
    public interface EmploymentAppResumeContent {}

    @Screen(name = "EmploymentAppResumeList", location = "component://humanres/widget/EmploymentAppScreens.xml")
    @Action(type = ActionType.SET, field = "resumeReadOnly", fromField = "resumeReadOnly", valueType = "Boolean", defaultValue = "${groovy: parameters.resumeReadOnly ?: 'true'}")
    @Action(type = ActionType.SET, field = "pcntListReadOnly", fromField = "resumeReadOnly", valueType = "Boolean", defaultValue = "false")
    @Action(type = ActionType.SET, field = "applicationId", fromField = "applicationId", defaultValue = "${parameters.applicationId}")
    @Action(type = ActionType.SET, field = "pcntListRemoveDonePage", fromField = "pcntListRemoveDonePage", defaultValue = "EditEmploymentApp")
    @Action(type = ActionType.SET, field = "pcntListRemoveExtraParams", valueType = "Object")
    @Action(type = ActionType.SET, field = "pcntListRemoveExtraParams.applicationId", fromField = "applicationId")
    @Action(type = ActionType.SET, field = "pcntListShowStatus", value = "false", valueType = "Boolean")
    @Action(type = ActionType.SET, field = "pcntPartyContentTypeId", value = "RESUME")
    @Action(type = ActionType.SET, field = "partyId", fromField = "partyId", defaultValue = "${groovy: (parameters.partyId ?: context.userLogin?.partyId)}")
    @Action(type = ActionType.PROPERTY_MAP, resource = "CommonUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "ContentUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "PartyUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.ENTITY_AND, entityName = "PartyContent", list = "partyContent", fieldMaps = {@FieldMap(fieldName = "partyId", fromField = "partyId"), @FieldMap(fieldName = "partyContentTypeId", value = "RESUME")})
    @Section(condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {@Condition(type = ServicePermission.class, params = {"partyIdPermissionCheck", "VIEW"})}), widgets = @Widgets(value = {@Widget(type = WidgetType.HTML_TEMPLATE, location = "component://party/webapp/partymgr/party/profileblocks/ContentList.ftl", position = 1)}, sections = {@SectionNested(condition = @com.ilscipio.scipio.widget.def.screen.Condition(not = true, functionalConditions = {@Condition(type = True.class, params = {"resumeReadOnly"})}), widgets = @WidgetsForContainer(value = {@Widget(type = WidgetType.INCLUDE_MENU, name = "ResumeListSubTabBar", location = "component://humanres/widget/HumanresMenus.xml")}), position = 0)}), failWidgets = @Widgets(value = {@Widget(type = WidgetType.LABEL, text = "${uiLabelMap.CommonPermissionError}", style = "common-msg-error-perm")}))
    public interface EmploymentAppResumeList {}

    @Screen(name = "ScipioNewApplications", location = "component://humanres/widget/EmploymentAppScreens.xml")
    @Action(type = ActionType.ENTITY_CONDITION, entityName = "EmploymentApp", list = "emplAppList", conditions = {@ConditionExpr(fieldName = "statusId", value = "EMPL_POS_ACTIVE"), @ConditionExpr(fieldName = "statusId", value = "IJP_APPLIED")}, orderBy = {"-applicationDate"})
    @Section(widgets = @Widgets(value = {@Widget(type = WidgetType.HTML_TEMPLATE, location = "component://humanres/webapp/humanres/humanres/emplapp/ScipioApplicationsSummary.ftl")}))
    public interface ScipioNewApplications {}

}
