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
public class EmployeeScreens {

    @Screen(name = "FindEmployee", location = "component://humanres/widget/EmployeeScreens.xml")
    @Action(type = ActionType.PROPERTY_MAP, resource = "HumanResUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "CommonUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.SET, field = "title", value = "${uiLabelMap.CommonFind} ${uiLabelMap.HumanResEmployeeApplicant}")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "Employees")
    @DecoratorScreen(
        name = "CommonHumanResAppDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", sections = {
                @InlineSection(actions = @Actions(value = {
                    @Action(type = ActionType.SERVICE, serviceName = "findParty"),
                    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "Employee"
                ),
                @Action(type = ActionType.SET, field = "findEmplQueryRan", value = "${groovy: (parameters.doFindQuery == 'Y' || (parameters.doFindQuery != 'N' && context.requestMethod == 'POST'))}", valueType = "Boolean"
            )}), widgets = @InlineWidgets(value = {
                @Widget(type = WidgetType.HTML_TEMPLATE, location = "component://humanres/webapp/humanres/humanres/findEmployee.ftl"
            )}))})
        }
    )
    public interface FindEmployee {}

    @Screen(name = "NewEmployee", location = "component://humanres/widget/EmployeeScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "HumanResNewEmployeeApplicant")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "Employees")
    @Action(type = ActionType.PROPERTY_TO_FIELD, field = "defaultCountryGeoId", resource = "general", property = "country.geo.id.default", defaultValue = "USA")
    @DecoratorScreen(
        name = "CommonHumanResAppDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", screenlets = {
                @Screenlet(actions = @Actions(value = {
                    @Action(type = ActionType.SET, field = "dependentForm", value = "NewEmployee"
                ),
                @Action(type = ActionType.SET, field = "paramKey", value = "countryGeoId"
            ),
            @Action(type = ActionType.SET, field = "mainId", value = "countryGeoId"
            ),
            @Action(type = ActionType.SET, field = "dependentId", value = "stateProvinceGeoId"
            ),
            @Action(type = ActionType.SET, field = "requestName", value = "getAssociatedStateList"
            ),
            @Action(type = ActionType.SET, field = "responseName", value = "stateList"
            ),
            @Action(type = ActionType.SET, field = "dependentKeyName", value = "geoId"
            ),
            @Action(type = ActionType.SET, field = "descName", value = "geoName"
            ),
            @Action(type = ActionType.SET, field = "selectedDependentOption", value = "_none_"
            )}), includeForms = {
                @IncludeForm(name = "NewEmployee", location = "component://humanres/widget/forms/EmployeeForms.xml", position = 1
            )}, htmlTemplates = {
                @HtmlTemplate(location = "component://common/webcommon/includes/setDependentDropdownValuesJs.ftl", position = 0
            )})})
        }
    )
    public interface NewEmployee {}

    @Screen(name = "EmployeeProfile", location = "component://humanres/widget/EmployeeScreens.xml")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "EmployeeProfile")
    @Action(type = ActionType.SET, field = "labelTitleProperty", value = "PartyTaxAuthInfos")
    @Action(type = ActionType.SET, field = "partyId", fromField = "parameters.partyId")
    @Action(type = ActionType.SET, field = "layoutSettings.javaScripts[]", value = "/partymgr/static/PartyProfileContent.js", global = true)
    @Action(type = ActionType.SCRIPT, location = "component://party/webapp/partymgr/WEB-INF/actions/party/ViewProfile.groovy")
    @Action(type = ActionType.SCRIPT, location = "component://party/webapp/partymgr/WEB-INF/actions/party/GetUserLoginPrimaryEmail.groovy")
    @Action(type = ActionType.SET, field = "titleProperty", value = "HumanResEmployeeApplicantProfile")
    @DecoratorScreen(
        name = "EmployeeDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", sections = {
                @InlineSection(condition = @com.ilscipio.scipio.widget.def.screen.Condition(not = true, functionalConditions = {
                    @Condition(type = Empty.class, params = {"party"})}), widgets = @InlineWidgets(value = {
                        @Widget(type = WidgetType.INCLUDE_MENU, name = "EmployeeProfileSubTarBar", location = "component://humanres/widget/HumanresMenus.xml"
                    )}, containers = {
                        @Container(style = "profile-left", includeScreens = {
                            @IncludeScreen(name = "Party", location = "component://party/widget/partymgr/ProfileScreens.xml"
                        ),
                        @IncludeScreen(name = "Contact", location = "component://party/widget/partymgr/ProfileScreens.xml"
                    ),
                    @IncludeScreen(name = "contactsAndAccounts", location = "component://party/widget/partymgr/ProfileScreens.xml"
                ),
                @IncludeScreen(name = "trainingsList", location = "component://humanres/widget/EmployeeScreens.xml"
            )}),
            @Container(style = "profile-right", includeScreens = {
                @IncludeScreen(name = "CurrentEmploymentData", location = "component://humanres/widget/EmployeeScreens.xml"
            ),
            @IncludeScreen(name = "UserLogin", location = "component://party/widget/partymgr/ProfileScreens.xml"
            ),
            @IncludeScreen(name = "EmployeeContent", location = "component://humanres/widget/EmployeeScreens.xml"
            ),
            @IncludeScreen(name = "Notes", location = "component://party/widget/partymgr/ProfileScreens.xml"
            ),
            @IncludeScreen(name = "Attributes", location = "component://party/widget/partymgr/ProfileScreens.xml"
            )})}), failWidgets = @InlineWidgets(containers = {
                @Container(labels = {
                    @Label(text = "${uiLabelMap.PartyNoPartyFoundWithPartyId}: ${parameters.partyId}", style = "common-msg-error"
                )})}))})
        }
    )
    public interface EmployeeProfile {}

    @Screen(name = "EditEmployeeSkills", location = "component://humanres/widget/EmployeeScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleViewPartySkill")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "EditEmployeeSkills")
    @Action(type = ActionType.SET, field = "partyId", fromField = "parameters.partyId")
    @Action(type = ActionType.SET, field = "skillTypeId", fromField = "parameters.skillTypeId")
    @Action(type = ActionType.SET, field = "partySkillsCtx.partyId", fromField = "parameters.partyId")
    @Action(type = ActionType.SET, field = "parameters.insideEmployee", value = "true")
    @DecoratorScreen(
        name = "EmployeeDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.INCLUDE_FORM, name = "ListEmployeeSkills", location = "component://humanres/widget/forms/EmployeeForms.xml"
            )}, screenlets = {
                @Screenlet(title = "${uiLabelMap.HumanResAddPartySkill}", name = "AddPartySkillPanel", collapsible = true, includeForms = {
                    @IncludeForm(name = "AddEmployeeSkills", location = "component://humanres/widget/forms/EmployeeForms.xml"
                )})})
        }
    )
    public interface EditEmployeeSkills {}

    @Screen(name = "EditEmployeeQuals", location = "component://humanres/widget/EmployeeScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "HumanResEditPartyQual")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "EditEmployeeQuals")
    @Action(type = ActionType.SET, field = "partyId", fromField = "parameters.partyId")
    @Action(type = ActionType.SET, field = "partyQualCtx.partyId", fromField = "parameters.partyId")
    @Action(type = ActionType.SET, field = "parameters.insideEmployee", value = "true")
    @DecoratorScreen(
        name = "EmployeeDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.INCLUDE_FORM, name = "ListEmployeeQualification", location = "component://humanres/widget/forms/EmployeeForms.xml"
            )}, screenlets = {
                @Screenlet(title = "${uiLabelMap.HumanResAddPartyQual}", name = "AddPartyQualPanel", collapsible = true, includeForms = {
                    @IncludeForm(name = "AddEmployeeQualification", location = "component://humanres/widget/forms/EmployeeForms.xml"
                )})})
        }
    )
    public interface EditEmployeeQuals {}

    @Screen(name = "EditEmployeeTrainings", location = "component://humanres/widget/EmployeeScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "HumanResTraining")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "EditEmployeeTrainings")
    @DecoratorScreen(
        name = "EmployeeDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", screenlets = {
                @Screenlet(title = "${uiLabelMap.HumanResTrainingStatus}", name = "TrainingStatus", includeForms = {
                    @IncludeForm(name = "ListTrainingStatus", location = "component://humanres/widget/forms/PersonTrainingForms.xml"
                )})})
        }
    )
    public interface EditEmployeeTrainings {}

    @Screen(name = "EditEmployeeEmploymentApps", location = "component://humanres/widget/EmployeeScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleViewEmploymentApp")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "EditEmployeeEmploymentApps")
    @Action(type = ActionType.SET, field = "referredByPartyId", fromField = "parameters.partyId")
    @Action(type = ActionType.SET, field = "employmentAppCtx.applicationId", fromField = "parameters.applicationId")
    @Action(type = ActionType.SET, field = "parameters.insideEmployee", value = "true")
    @DecoratorScreen(
        name = "EmployeeDecorator",
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
    public interface EditEmployeeEmploymentApps {}

    @Screen(name = "EditEmployeeResumes", location = "component://humanres/widget/EmployeeScreens.xml")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "EditEmployeeResumes")
    @Action(type = ActionType.SET, field = "resumeId", fromField = "parameters.resumeId")
    @Action(type = ActionType.SET, field = "partyId", fromField = "parameters.partyId")
    @Action(type = ActionType.ENTITY_ONE, entityName = "PartyResume", valueField = "partyResume")
    @DecoratorScreen(
        name = "EmployeeDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.INCLUDE_FORM, name = "ListPartyResumes", location = "component://humanres/widget/forms/PartyResumeForms.xml"
            )}, screenlets = {
                @Screenlet(title = "${uiLabelMap.CommonAdd} ${uiLabelMap.HumanResEditPartyResume}", name = "AddEmploymentAppPanel", collapsible = true, includeForms = {
                    @IncludeForm(name = "EditPartyResume", location = "component://humanres/widget/forms/PartyResumeForms.xml"
                )})})
        }
    )
    public interface EditEmployeeResumes {}

    @Screen(name = "EditEmployeePerformanceNotes", location = "component://humanres/widget/EmployeeScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "HumanResPerfNote")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "EditEmployeePerformanceNotes")
    @Action(type = ActionType.SET, field = "partyId", fromField = "parameters.partyId")
    @DecoratorScreen(
        name = "EmployeeDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.INCLUDE_FORM, name = "ListPerformanceNotes", location = "component://humanres/widget/forms/EmploymentForms.xml"
            )}, screenlets = {
                @Screenlet(title = "${uiLabelMap.HumanResAddPerfNote}", name = "AddPerformanceNotePanel", collapsible = true, includeForms = {
                    @IncludeForm(name = "AddPerformanceNote", location = "component://humanres/widget/forms/EmploymentForms.xml"
                )})})
        }
    )
    public interface EditEmployeePerformanceNotes {}

    @Screen(name = "EditEmployeeLeaves", location = "component://humanres/widget/EmployeeScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "HumanResEditEmplLeave")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "EditEmployeeLeaves")
    @Action(type = ActionType.SET, field = "partyId", fromField = "parameters.partyId")
    @Action(type = ActionType.ENTITY_ONE, entityName = "EmplLeave", valueField = "leaveApp")
    @DecoratorScreen(
        name = "EmployeeDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.INCLUDE_FORM, name = "ListEmplLeaves", location = "component://humanres/widget/forms/EmployeeForms.xml"
            )}, screenlets = {
                @Screenlet(title = "${uiLabelMap.HumanResAddEmplLeave}", name = "AddEmplLeavePanel", collapsible = true, includeForms = {
                    @IncludeForm(name = "AddEmplLeave", location = "component://humanres/widget/forms/EmployeeForms.xml"
                )})})
        }
    )
    public interface EditEmployeeLeaves {}

    @Screen(name = "CurrentEmploymentData", location = "component://humanres/widget/EmployeeScreens.xml")
    @Action(type = ActionType.SERVICE, serviceName = "getCurrentPartyEmploymentData", resultMapName = "employmentData")
    @Section(widgets = @Widgets(screenlets = {@Screenlet(title = "${uiLabelMap.HumanResCurrentEmploymentData}", includeForms = {@IncludeForm(name = "CurrentEmploymentData", location = "component://humanres/widget/forms/EmployeeForms.xml")})}))
    public interface CurrentEmploymentData {}

    @Screen(name = "trainingsList", location = "component://humanres/widget/EmployeeScreens.xml")
    @Section(widgets = @Widgets(screenlets = {@Screenlet(title = "${uiLabelMap.HumanResTrainings}", includeForms = {@IncludeForm(name = "simpleListTrainingStatus", location = "component://humanres/widget/forms/PersonTrainingForms.xml")})}))
    public interface trainingsList {}

    @Screen(name = "PayrollHistory", location = "component://humanres/widget/EmployeeScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "HumanResPayRollHistory")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "PayrollHistory")
    @Action(type = ActionType.SET, field = "partyId", fromField = "parameters.partyId")
    @Action(type = ActionType.SET, field = "parameters.sortField", fromField = "parameters.sortField", defaultValue = "invoiceDate DESC")
    @Action(type = ActionType.ENTITY_CONDITION, entityName = "InvoiceAndType", list = "payroll", conditions = {@ConditionExpr(fieldName = "partyIdFrom", operator = "equals", fromField = "partyId"), @ConditionExpr(fieldName = "invoiceTypeId", operator = "equals", value = "PAYROL_INVOICE")}, orderBy = {"${parameters.sortField}"})
    @DecoratorScreen(
        name = "EmployeeDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.INCLUDE_FORM, name = "PayrollHistoryList", location = "component://humanres/widget/forms/EmployeeForms.xml"
            )})
        }
    )
    public interface PayrollHistory {}

    @Screen(name = "MyLeaveList", location = "component://humanres/widget/EmployeeScreens.xml")
    @Action(type = ActionType.SET, field = "partyId", fromField = "userLogin.partyId")
    @Section(widgets = @Widgets(screenlets = {@Screenlet(title = "${uiLabelMap.HumanResMyLeaves}", includeForms = {@IncludeForm(name = "ListEmplLeaves", location = "component://humanres/widget/forms/EmployeeForms.xml")})}))
    public interface MyLeaveList {}

    @Screen(name = "MyTrainings", location = "component://humanres/widget/EmployeeScreens.xml")
    @Action(type = ActionType.SET, field = "partyId", fromField = "userLogin.partyId")
    @Section(widgets = @Widgets(screenlets = {@Screenlet(title = "${uiLabelMap.HumanResMyTrainings}", includeForms = {@IncludeForm(name = "ListEmplTrainings", location = "component://humanres/widget/forms/PersonTrainingForms.xml")})}))
    public interface MyTrainings {}

    @Screen(name = "EditPartyContents", location = "component://humanres/widget/EmployeeScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "HumanResPartyContentAndResumes")
    @Action(type = ActionType.SET, field = "activeSubMenu", value = "EmployeeProfile")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "EditEmployeeContent")
    @Section(widgets = @Widgets(value = {@Widget(type = WidgetType.INCLUDE_SCREEN, name = "EditPartyContents", location = "component://party/widget/partymgr/PartyScreens.xml")}))
    public interface EditPartyContents {}

    @Screen(name = "EmployeeContent", location = "component://humanres/widget/EmployeeScreens.xml")
    @Action(type = ActionType.SET, field = "pcntTitle", fromField = "pcntTitle", defaultValue = "${uiLabelMap.HumanResPartyContentAndResumes}")
    @Action(type = ActionType.SET, field = "pcntCntListLoc", fromField = "pcntCntListLoc", defaultValue = "component://humanres/widget/EmployeeScreens.xml#EmployeeContentList")
    @Action(type = ActionType.SET, field = "pcntUploadUri", value = "uploadEmployeeContent")
    @Section(widgets = @Widgets(value = {@Widget(type = WidgetType.INCLUDE_SCREEN, name = "Content", location = "component://party/widget/partymgr/ProfileScreens.xml")}))
    public interface EmployeeContent {}

    @Screen(name = "EmployeeContentList", location = "component://humanres/widget/EmployeeScreens.xml")
    @Action(type = ActionType.SET, field = "pcntListRemoveDonePage", fromField = "pcntListRemoveDonePage", defaultValue = "EmployeeProfile")
    @Section(widgets = @Widgets(value = {@Widget(type = WidgetType.INCLUDE_SCREEN, name = "ContentList", location = "component://party/widget/partymgr/ProfileScreens.xml")}))
    public interface EmployeeContentList {}

}
