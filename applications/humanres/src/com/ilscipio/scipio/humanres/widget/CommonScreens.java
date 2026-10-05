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
public class CommonScreens {

    @Screen(name = "webapp-common-actions", location = "component://humanres/widget/CommonScreens.xml")
    @Action(type = ActionType.INCLUDE_SCREEN_ACTIONS, name = "MainSideBarMenu")
    @Action(type = ActionType.SET, field = "mainSideBarMenuCfg", fromField = "menuCfg")
    @Action(type = ActionType.SET, field = "mainComplexMenuCfg", fromField = "menuCfg")
    @Action(type = ActionType.SET, field = "menuCfg")
    public interface webapp_common_actions {}

    @Screen(name = "main-decorator", location = "component://humanres/widget/CommonScreens.xml")
    @Action(type = ActionType.PROPERTY_MAP, resource = "HumanResUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "CommonUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "PartyUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "AccountingUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "OrderUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "WorkEffortUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "MarketingUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "ContentUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "SecurityUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "ProductUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.SET, field = "applicationMenuName", value = "HumanResAppBar", global = true)
    @Action(type = ActionType.SET, field = "applicationMenuLocation", value = "component://humanres/widget/HumanresMenus.xml", global = true)
    @Action(type = ActionType.SET, field = "applicationTitle", value = "${uiLabelMap.HumanResManager}", global = true)
    @Action(type = ActionType.SET, field = "menuCfg", fromField = "mainComplexMenuCfg")
    @Action(type = ActionType.INCLUDE_SCREEN_ACTIONS, name = "DeriveComplexSideBarMenuItems", location = "component://common/widget/CommonScreens.xml")
    @Action(type = ActionType.SET, field = "layoutSettings.companyName", fromField = "uiLabelMap.HumanResCompanyName", global = true)
    @Action(type = ActionType.SET, field = "layoutSettings.companySubtitle", fromField = "uiLabelMap.HumanResCompanySubtitle", global = true)
    @Action(type = ActionType.SET, field = "activeApp", value = "humanres", global = true)
    @Action(type = ActionType.SET, field = "layoutSettings.javaScripts[]", value = "/partymgr/static/partymgr.js", global = true)
    @Action(type = ActionType.SET, field = "layoutSettings.styleSheets[]", value = "/partymgr/static/partymgr.css", global = true)
    @Action(type = ActionType.SET, field = "layoutSettings.styleSheets[]", value = "/images/humanres/humanres.css", global = true)
    @Action(type = ActionType.SET, field = "layoutSettings.javaScripts[+0]", value = "/base-theme/bower_components/jquery.cookie/jquery.cookie.js", global = true)
    @Action(type = ActionType.SET, field = "layoutSettings.javaScripts[+0]", value = "/base-theme/bower_components/jstree/dist/jstree.min.js", global = true)
    @DecoratorScreen(
        name = "ApplicationDecorator",
        location = "component://commonext/widget/CommonScreens.xml",
        sections = {
            @DecoratorSection(name = "left-column", useWhen = "${context.widePage != true}", sections = {
                @InlineSection(condition = @com.ilscipio.scipio.widget.def.screen.Condition(not = true, functionalConditions = {
                    @Condition(type = EmptySection.class, params = {"left-column"
                })}), widgets = @InlineWidgets(value = {
                    @Widget(type = WidgetType.DECORATOR_SECTION_INCLUDE, name = "left-column"
                )}), failWidgets = @InlineWidgets(value = {
                    @Widget(type = WidgetType.INCLUDE_SCREEN, name = "DefMainSideBarMenu", location = "${parameters.mainDecoratorLocation}"
                )}))}),
            @DecoratorSection(name = "body", value = {@Widget(type = WidgetType.DECORATOR_SECTION_INCLUDE, name = "body")})
        }
    )
    public interface main_decorator {}

    @Screen(name = "CommonHumanResAppDecorator", location = "component://humanres/widget/CommonScreens.xml")
    @Action(type = ActionType.CONDITION_TO_FIELD, field = "commonHumanResAppBasePermCond", valueType = "Boolean", onlyIfField = "empty", condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {@Condition(type = HasPermission.class, params = {"HUMANRES", "_VIEW"})}))
    @DecoratorScreen(
        name = "main-decorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "left-column", useWhen = "${context.widePage != true}", overrideByAutoInclude = true, value = {
                @Widget(type = WidgetType.INCLUDE_SCREEN, name = "CommonHumanResAppSideBarMenu", location = "component://humanres/widget/CommonScreens.xml"
            )}),
            @DecoratorSection(name = "body", sections = {
                @InlineSection(condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {
                    @Condition(type = True.class, params = {"commonHumanResAppBasePermCond"
                })}), widgets = @InlineWidgets(value = {
                    @Widget(type = WidgetType.DECORATOR_SECTION_INCLUDE, name = "body"
                )}), failWidgets = @InlineWidgets(value = {
                    @Widget(type = WidgetType.LABEL, text = "${uiLabelMap.HumanResViewPermissionError}", style = "common-msg-error-perm"
                )}))})
        }
    )
    public interface CommonHumanResAppDecorator {}

    @Screen(name = "main", location = "component://humanres/widget/CommonScreens.xml")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "main")
    @Action(type = ActionType.SET, field = "titleProperty", value = "HumanResManager")
    @Action(type = ActionType.SET, field = "employmentAppCtx", valueType = "NewMap")
    @DecoratorScreen(
        name = "CommonHumanResAppDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", screenlets = {
                @Screenlet(title = "${uiLabelMap.AccountingApplications}", containers = {
                    @Container(style = "${styles.grid_row}", containers = {
                        @Container2(style = "${styles.grid_large}6 ${styles.grid_cell}", screenlets = {
                            @ScreenletNested(title = "${uiLabelMap.HumanResNewApplicants}", includeScreens = {
                    @IncludeScreen(name = "ScipioNewApplications", location = "component://humanres/widget/EmploymentAppScreens.xml"
                
                        )})}),
                        @Container2(style = "${styles.grid_large}6 ${styles.grid_cell}", screenlets = {
                            @ScreenletNested(title = "${uiLabelMap.HumanResCurrentOpenings}", includeScreens = {
                    @IncludeScreen(name = "ScipioOpenPositions", location = "component://humanres/widget/EmplPositionScreens.xml"
                
                        )})})})})})
        }
    )
    public interface main {}

    @Screen(name = "OrgTree", location = "component://humanres/widget/CommonScreens.xml")
    @Action(type = ActionType.SET, field = "partyId", fromField = "defaultOrganizationPartyId")
    @Action(type = ActionType.SCRIPT, location = "component://humanres/webapp/humanres/WEB-INF/actions/category/CategoryTree.groovy")
    @Section(widgets = @Widgets(screenlets = {@Screenlet(title = "${uiLabelMap.FormFieldTitle_company}", htmlTemplates = {@HtmlTemplate(location = "component://humanres/webapp/humanres/humanres/category/CategoryTree.ftl")})}))
    public interface OrgTree {}

    @Screen(name = "PartyGroupTreeLine", location = "component://humanres/widget/CommonScreens.xml")
    @Section(widgets = @Widgets(value = {@Widget(type = WidgetType.LABEL, text = "${partyAndGroup.groupName}")}))
    public interface PartyGroupTreeLine {}

    @Screen(name = "PartyPersonTreeLine", location = "component://humanres/widget/CommonScreens.xml")
    @Section(widgets = @Widgets(value = {@Widget(type = WidgetType.LABEL, text = "${partyAndPerson.firstName} ${partyAndPerson.lastName}")}))
    public interface PartyPersonTreeLine {}

    @Screen(name = "CommonEmplPositionDecorator", location = "component://humanres/widget/CommonScreens.xml")
    @Action(type = ActionType.SET, field = "activeSubMenu", fromField = "activeSubMenu", defaultValue = "component://humanres/widget/HumanresMenus.xml#EmplPosition")
    @Action(type = ActionType.SET, field = "emplPositionId", fromField = "parameters.emplPositionId")
    @Action(type = ActionType.ENTITY_ONE, entityName = "EmplPosition", valueField = "emplPosition")
    @Action(type = ActionType.SET, field = "commonSideBarMenu.condList[]", value = "${not empty context.emplPosition}", valueType = "Boolean")
    @DecoratorScreen(
        name = "CommonHumanResAppDecorator",
        location = "${parameters.mainDecoratorLocation}"
    )
    public interface CommonEmplPositionDecorator {}

    @Screen(name = "CommonEmploymentDecorator", location = "component://humanres/widget/CommonScreens.xml")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", fromField = "activeSubMenuItem", defaultValue = "Employment")
    @Action(type = ActionType.SET, field = "roleTypeIdFrom", fromField = "parameters.roleTypeIdFrom")
    @Action(type = ActionType.SET, field = "roleTypeIdTo", fromField = "parameters.roleTypeIdTo")
    @Action(type = ActionType.SET, field = "partyIdFrom", fromField = "parameters.partyIdFrom")
    @Action(type = ActionType.SET, field = "partyIdTo", fromField = "parameters.partyIdTo")
    @Action(type = ActionType.SET, field = "fromDate", fromField = "parameters.fromDate", valueType = "Timestamp")
    @Action(type = ActionType.ENTITY_ONE, entityName = "Employment", valueField = "employment")
    @DecoratorScreen(
        name = "CommonHumanResAppDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.DECORATOR_SECTION_INCLUDE, name = "body"
            )}, sections = {
                @InlineSection(condition = @com.ilscipio.scipio.widget.def.screen.Condition(not = true, functionalConditions = {
                    @Condition(type = Empty.class, params = {"parameters.partyIdFrom"
                })}), actions = @Actions(value = {
                    @Action(type = ActionType.ENTITY_ONE, entityName = "PartyNameView", valueField = "emplName", fieldMaps = {
                        @FieldMap(fieldName = "partyId", fromField = "parameters.partyIdTo"
                    )}),
                    @Action(type = ActionType.ENTITY_ONE, entityName = "PartyNameView", valueField = "orgName", fieldMaps = {
                        @FieldMap(fieldName = "partyId", fromField = "parameters.partyIdFrom"
                    )})}), widgets = @InlineWidgets(sections = {
                        @SectionNested(condition = @com.ilscipio.scipio.widget.def.screen.Condition(not = true, functionalConditions = {
                            @Condition(type = Empty.class, params = {"parameters.fromDate"
                        })}), widgets = @WidgetsForContainer(value = {
                            @Widget(type = WidgetType.INCLUDE_MENU, name = "EmploymentBar", location = "component://humanres/widget/HumanresMenus.xml", position = 0
                        ),
                        @Widget(type = WidgetType.LABEL, text = "${emplName.lastName},${emplName.firstName} ${emplName.middleName} [${emplName.partyId}] ${uiLabelMap.CommonFor}", style = "heading", position = 2
                    ),
                    @Widget(type = WidgetType.LABEL, text = "${orgName.groupName} [${orgName.partyId}]", style = "heading", position = 3
                )}, containers = {
                    @Container2(style = "button-bar", widgets = {
                        @Widget(type = WidgetType.LINK, text = "${uiLabelMap.HumanResNewEmployment}", style = "${styles.link_nav} ${styles.action_add}", target = "EditEmployment"
                    )}, position = 1)}))}), position = 0)})
        }
    )
    public interface CommonEmploymentDecorator {}

    @Screen(name = "CommonEmploymentAppDecorator", location = "component://humanres/widget/CommonScreens.xml")
    @Action(type = ActionType.SET, field = "activeSubMenu", fromField = "activeSubMenu", defaultValue = "TOP")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "EmploymentApp")
    @DecoratorScreen(
        name = "CommonEmploymentDecorator",
        location = "component://humanres/widget/CommonScreens.xml"
    )
    public interface CommonEmploymentAppDecorator {}

    @Screen(name = "CommonPerfReviewDecorator", location = "component://humanres/widget/CommonScreens.xml")
    @Action(type = ActionType.SET, field = "perfReviewId", fromField = "parameters.perfReviewId")
    @Action(type = ActionType.ENTITY_ONE, entityName = "PerfReview", valueField = "perfReview")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", fromField = "activeSubMenuItem", defaultValue = "PerfReview")
    @DecoratorScreen(
        name = "CommonHumanResAppDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.DECORATOR_SECTION_INCLUDE, name = "body"
            )}, sections = {
                @InlineSection(condition = @com.ilscipio.scipio.widget.def.screen.Condition(not = true, functionalConditions = {
                    @Condition(type = Empty.class, params = {"perfReview"})}), actions = @Actions(value = {
                        @Action(type = ActionType.ENTITY_ONE, entityName = "PartyNameView", valueField = "partyNameView", fieldMaps = {
                            @FieldMap(fieldName = "partyId", fromField = "perfReview.employeePartyId"
                        )})}), widgets = @InlineWidgets(value = {
                            @Widget(type = WidgetType.INCLUDE_MENU, name = "PerfReviewBar", location = "component://humanres/widget/HumanresMenus.xml"
                        ),
                        @Widget(type = WidgetType.LABEL, text = "${uiLabelMap.HumanResPerfReview} [${perfReview.perfReviewId}] ${partyNameView.lastName} ${partyNameView.firstName} ${partyNameView.middleName}", style = "heading"
                    )}), position = 0)})
        }
    )
    public interface CommonPerfReviewDecorator {}

    @Screen(name = "EmployeeDecorator", location = "component://humanres/widget/CommonScreens.xml")
    @Action(type = ActionType.SET, field = "activeSubMenu", fromField = "activeSubMenu", defaultValue = "component://humanres/widget/HumanresMenus.xml#EmployeeProfile")
    @Action(type = ActionType.SET, field = "partyId", fromField = "parameters.partyId")
    @Action(type = ActionType.ENTITY_ONE, entityName = "Party", valueField = "party")
    @Action(type = ActionType.ENTITY_ONE, entityName = "Person", valueField = "lookupPerson")
    @Action(type = ActionType.SET, field = "titleFormat", fromField = "titleFormat", defaultValue = "\\${finalTitle}${groovy: context.partyId ? (': ' + context.partyId) : ''}")
    @Action(type = ActionType.SET, field = "commonSideBarMenu.condList[]", value = "${not empty context.partyId}", valueType = "Boolean")
    @DecoratorScreen(
        name = "CommonHumanResAppDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {@Widget(type = WidgetType.DECORATOR_SECTION_INCLUDE, name = "body")})
        }
    )
    public interface EmployeeDecorator {}

    @Screen(name = "CommonPartyDecorator", location = "component://humanres/widget/CommonScreens.xml")
    @Action(type = ActionType.SET, field = "activeSubMenu", fromField = "activeSubMenu", defaultValue = "component://humanres/widget/HumanresMenus.xml#EmployeeProfile")
    @Action(type = ActionType.SET, field = "partyId", fromField = "parameters.partyId")
    @Action(type = ActionType.ENTITY_ONE, entityName = "Party", valueField = "party")
    @Action(type = ActionType.ENTITY_ONE, entityName = "Person", valueField = "lookupPerson")
    @Action(type = ActionType.ENTITY_ONE, entityName = "PartyGroup", valueField = "lookupGroup")
    @Action(type = ActionType.SET, field = "commonSideBarMenu.condList[]", value = "${not empty context.partyId}", valueType = "Boolean")
    @Action(type = ActionType.CONDITION_TO_FIELD, field = "commonSideBarMenu.condList[]", valueType = "Boolean", condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {@Condition(type = ServicePermission.class, params = {"partyBasePermissionCheck", "VIEW"})}))
    @DecoratorScreen(
        name = "CommonHumanResAppDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", sections = {
                @InlineSection(condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {
                    @Condition(type = ServicePermission.class, params = {"partyBasePermissionCheck", "VIEW"
                })}), widgets = @InlineWidgets(value = {
                    @Widget(type = WidgetType.DECORATOR_SECTION_INCLUDE, name = "body", position = 1
                )}, sections = {
                    @SectionNested(condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {
                        @Condition(type = NotEmpty.class, params = {"partyId"}),
                        @Condition(type = And.class, tree = {
                            @ConditionNode(not = true, type = True.class, params = {"skipProfileHeader"
                        })})}), widgets = @WidgetsForContainer(containers = {
                            @Container2(style = "heading", sections = {
                                @SectionNested2(condition = @com.ilscipio.scipio.widget.def.screen.Condition(or = {
                                    @OrCondition(ifNotEmpty = {"lookupPerson", "lookupGroup"})}), widgets = @WidgetsForContainer2(value = {
                                        @Widget(type = WidgetType.LABEL, text = "${uiLabelMap.PartyTheProfileOf} ${lookupPerson.personalTitle} ${lookupPerson.firstName} ${lookupPerson.middleName} ${lookupPerson.lastName} ${lookupPerson.suffix} ${lookupGroup.groupName} [${partyId}]"
                                    )}), failWidgets = @WidgetsForContainer2(sections = {
                                        @SectionNested3(condition = @com.ilscipio.scipio.widget.def.screen.Condition(not = true, functionalConditions = {
                                            @Condition(type = Empty.class, params = {"party"})}), widgets = @WidgetsForContainer3(value = {
                                                @Widget(type = WidgetType.LABEL, text = "${uiLabelMap.PartyTheProfileOf} ${partyId}"
                                            )}), failWidgets = @WidgetsForContainer3(value = {
                                                @Widget(type = WidgetType.LABEL, text = "${uiLabelMap.PartyNewUser}", style = "heading"
                                            )}))}))})}), position = 0)}), failWidgets = @InlineWidgets(value = {
                                                @Widget(type = WidgetType.LABEL, text = "${uiLabelMap.PartyMgrViewPermissionError}", style = "common-msg-error-perm"
                                            )}))})
        }
    )
    public interface CommonPartyDecorator {}

    @Screen(name = "GlobalHRSettingsDecorator", location = "component://humanres/widget/CommonScreens.xml")
    @Action(type = ActionType.SET, field = "activeSubMenu", fromField = "activeSubMenu", defaultValue = "component://humanres/widget/HumanresMenus.xml#GlobalHRSetting")
    @DecoratorScreen(
        name = "CommonHumanResAppDecorator",
        location = "${parameters.mainDecoratorLocation}"
    )
    public interface GlobalHRSettingsDecorator {}

    @Screen(name = "CommonRecruitmentDecorator", location = "component://humanres/widget/CommonScreens.xml")
    @Action(type = ActionType.SET, field = "activeSubMenu", fromField = "activeSubMenu", defaultValue = "component://humanres/widget/HumanresMenus.xml#RecruitmentType")
    @Action(type = ActionType.PROPERTY_MAP, resource = "ContentUiLabels", mapName = "uiLabelMap", global = true)
    @DecoratorScreen(
        name = "CommonHumanResAppDecorator",
        location = "${parameters.mainDecoratorLocation}"
    )
    public interface CommonRecruitmentDecorator {}

    @Screen(name = "CommonInternalJobPostingDecorator", location = "component://humanres/widget/CommonScreens.xml")
    @Action(type = ActionType.SET, field = "activeSubMenu", fromField = "activeSubMenu", defaultValue = "component://humanres/widget/HumanresMenus.xml#InternalJobPosting")
    @DecoratorScreen(
        name = "CommonRecruitmentDecorator",
        location = "${parameters.mainDecoratorLocation}"
    )
    public interface CommonInternalJobPostingDecorator {}

    @Screen(name = "CommonTrainingDecorator", location = "component://humanres/widget/CommonScreens.xml")
    @Action(type = ActionType.SET, field = "activeSubMenu", fromField = "activeSubMenu", defaultValue = "component://humanres/widget/HumanresMenus.xml#TrainingType")
    @DecoratorScreen(
        name = "CommonHumanResAppDecorator",
        location = "${parameters.mainDecoratorLocation}"
    )
    public interface CommonTrainingDecorator {}

    @Screen(name = "CommonMyCommunicationEventDecorator", location = "component://humanres/widget/CommonScreens.xml")
    @Action(type = ActionType.SET, field = "activeSubMenu", fromField = "activeSubMenu", defaultValue = "component://humanres/widget/HumanresMenus.xml#EmployeeProfile")
    @Action(type = ActionType.SCRIPT, location = "component://party/webapp/partymgr/WEB-INF/actions/communication/GetMyCommunicationEventRole.groovy")
    @DecoratorScreen(
        name = "CommonHumanResAppDecorator",
        location = "${parameters.mainDecoratorLocation}"
    )
    public interface CommonMyCommunicationEventDecorator {}

    @Screen(name = "MainSideBarMenu", location = "component://humanres/widget/CommonScreens.xml")
    @Action(type = ActionType.SET, field = "menuCfg.location", value = "component://humanres/widget/HumanresMenus.xml")
    @Action(type = ActionType.SET, field = "menuCfg.name", value = "HumanResAppSideBar")
    @Action(type = ActionType.SET, field = "menuCfg.defLocation", value = "component://humanres/widget/HumanresMenus.xml")
    @Section(widgets = @Widgets(value = {@Widget(type = WidgetType.INCLUDE_SCREEN, name = "ComplexSideBarMenu", location = "component://common/widget/CommonScreens.xml")}))
    public interface MainSideBarMenu {}

    @Screen(name = "DefMainSideBarMenu", location = "component://humanres/widget/CommonScreens.xml")
    @Action(type = ActionType.SCRIPT, location = "component://common/webcommon/WEB-INF/actions/includes/scipio/PrepareDefComplexSideBarMenu.groovy")
    @Section(widgets = @Widgets(value = {@Widget(type = WidgetType.INCLUDE_SCREEN, name = "MainSideBarMenu")}))
    public interface DefMainSideBarMenu {}

    @Screen(name = "CommonHumanResAppSideBarMenu", location = "component://humanres/widget/CommonScreens.xml")
    @Action(type = ActionType.CONDITION_TO_FIELD, field = "commonHumanResAppBasePermCond", valueType = "Boolean", onlyIfField = "empty", condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {@Condition(type = HasPermission.class, params = {"HUMANRES", "_VIEW"})}))
    @Action(type = ActionType.SET, field = "commonSideBarMenu.cond", fromField = "commonSideBarMenu.cond", valueType = "Boolean", defaultValue = "${commonHumanResAppBasePermCond}")
    @Section(widgets = @Widgets(value = {@Widget(type = WidgetType.INCLUDE_SCREEN, name = "CommonSideBarMenu", location = "component://common/widget/CommonScreens.xml")}))
    public interface CommonHumanResAppSideBarMenu {}

}
