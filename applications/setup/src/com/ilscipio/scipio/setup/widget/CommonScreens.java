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
package com.ilscipio.scipio.setup.widget;

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

    @Screen(name = "webapp-common-actions", location = "component://setup/widget/CommonScreens.xml")
    @Action(type = ActionType.INCLUDE_SCREEN_ACTIONS, name = "MainSideBarMenu")
    @Action(type = ActionType.SET, field = "mainSideBarMenuCfg", fromField = "menuCfg")
    @Action(type = ActionType.SET, field = "mainComplexMenuCfg", fromField = "menuCfg")
    @Action(type = ActionType.SET, field = "menuCfg")
    public interface webapp_common_actions {}

    @Screen(name = "main-decorator", location = "component://setup/widget/CommonScreens.xml")
    @Action(type = ActionType.PROPERTY_MAP, resource = "ScipioSetupErrorUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "ScipioSetupUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "SetupUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "WebtoolsUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "ContentUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "MarketingUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "PartyUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "OrderUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "ProductUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "AccountingUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "CommonErrorUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "CommonUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.SET, field = "MainColumnStyle", value = "nocolumns", global = true)
    @Action(type = ActionType.SET, field = "layoutSettings.companyName", fromField = "uiLabelMap.SetupCompanyName", global = true)
    @Action(type = ActionType.SET, field = "layoutSettings.companySubtitle", fromField = "uiLabelMap.SetupCompanySubtitle", global = true)
    @Action(type = ActionType.SET, field = "activeApp", value = "setup", global = true)
    @Action(type = ActionType.SET, field = "applicationMenuName", value = "SetupAppBar", global = true)
    @Action(type = ActionType.SET, field = "applicationMenuLocation", value = "component://setup/widget/Menus.xml", global = true)
    @Action(type = ActionType.SET, field = "applicationTitle", value = "${uiLabelMap.SetupApp}", global = true)
    @Action(type = ActionType.SET, field = "layoutSettings.javaScripts[+0]", value = "/base-theme/bower_components/jquery.cookie/jquery.cookie.js", global = true)
    @Action(type = ActionType.SET, field = "layoutSettings.styleSheets[+0]", value = "/base-theme/bower_components/jstree/dist/themes/default/style.css", global = true)
    @Action(type = ActionType.SET, field = "layoutSettings.javaScripts[+0]", value = "/base-theme/bower_components/jstree/dist/jstree.min.js", global = true)
    @Action(type = ActionType.SET, field = "menuCfg", fromField = "mainComplexMenuCfg")
    @Action(type = ActionType.INCLUDE_SCREEN_ACTIONS, name = "DeriveComplexSideBarMenuItems", location = "component://common/widget/CommonScreens.xml")
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
                )}))})
        }
    )
    public interface main_decorator {}

    @Screen(name = "CommonSetupAppDecorator", location = "component://setup/widget/CommonScreens.xml")
    @Action(type = ActionType.SET, field = "layoutSettings.javaScriptsFooter[]", value = "/ordermgr-js/geoAutoCompleter.js", global = true)
    @Action(type = ActionType.SET, field = "commonSetupAppBasePermCond", fromField = "commonSetupAppBasePermCond", valueType = "Boolean", defaultValue = "true")
    @DecoratorScreen(
        name = "main-decorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "left-column", useWhen = "${context.widePage != true}", overrideByAutoInclude = true, value = {
                @Widget(type = WidgetType.INCLUDE_SCREEN, name = "CommonSetupAppSideBarMenu", location = "component://setup/widget/CommonScreens.xml"
            )}),
            @DecoratorSection(name = "body", sections = {
                @InlineSection(condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {
                    @Condition(type = True.class, params = {"commonSetupAppBasePermCond"
                })}), widgets = @InlineWidgets(value = {
                    @Widget(type = WidgetType.DECORATOR_SECTION_INCLUDE, name = "body"
                )}), failWidgets = @InlineWidgets(value = {
                    @Widget(type = WidgetType.LABEL, text = "${uiLabelMap.CommonPermissionError}", style = "common-msg-error-perm"
                )}))})
        }
    )
    public interface CommonSetupAppDecorator {}

    @Screen(name = "CommonPartyDecorator", location = "component://setup/widget/CommonScreens.xml")
    @Section(condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {@Condition(type = True.class, params = {"useSetupWizardDec"})}), widgets = @Widgets(decorator = @DecoratorScreen(
        name = "CommonSetupWizardDecorator",
        location = "component://setup/widget/CommonScreens.xml"
    )), failWidgets = @Widgets(decorator = @DecoratorScreen(
        name = "CommonSetupDecorator",
        location = "component://setup/widget/CommonScreens.xml"
    )))
    public interface CommonPartyDecorator {}

    @Screen(name = "CommonSetupDecorator", location = "component://setup/widget/CommonScreens.xml")
    @Action(type = ActionType.SET, field = "activeSubMenu", fromField = "activeSubMenu", defaultValue = "component://setup/widget/Menus.xml#Setup")
    @Action(type = ActionType.ENTITY_CONDITION, entityName = "PartyRole", list = "parties", conditions = {@ConditionExpr(fieldName = "roleTypeId", operator = "equals", value = "INTERNAL_ORGANIZATIO")})
    @Action(type = ActionType.SET, field = "partyId", fromField = "parties[0].partyId")
    @Action(type = ActionType.ENTITY_ONE, entityName = "Party", valueField = "party")
    @Action(type = ActionType.ENTITY_ONE, entityName = "PartyGroup", valueField = "lookupGroup")
    @Action(type = ActionType.CONDITION_TO_FIELD, field = "commonSideBarMenu.condList[]", valueType = "Boolean", condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {@Condition(type = ServicePermission.class, params = {"partyBasePermissionCheck", "VIEW"}), @Condition(type = NotEmpty.class, params = {"taxAuthority"})}))
    @DecoratorScreen(
        name = "CommonSetupAppDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", sections = {
                @InlineSection(condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {
                    @Condition(type = ServicePermission.class, params = {"partyBasePermissionCheck", "VIEW"
                })}), widgets = @InlineWidgets(value = {
                    @Widget(type = WidgetType.DECORATOR_SECTION_INCLUDE, name = "body", position = 1
                )}, sections = {
                    @SectionNested(condition = @com.ilscipio.scipio.widget.def.screen.Condition(not = true, functionalConditions = {
                        @Condition(type = Empty.class, params = {"partyId"})}), widgets = @WidgetsForContainer(containers = {
                            @Container2(style = "clear"),
                            @Container2(style = "h2", sections = {
                                @SectionNested2(condition = @com.ilscipio.scipio.widget.def.screen.Condition(not = true, functionalConditions = {
                                    @Condition(type = Empty.class, params = {"lookupGroup"})}), widgets = @WidgetsForContainer2(value = {
                                        @Widget(type = WidgetType.LABEL, text = "${uiLabelMap.PartyTheProfileOf} ${lookupPerson.personalTitle} ${lookupPerson.firstName} ${lookupPerson.middleName} ${lookupPerson.lastName} ${lookupPerson.suffix} ${lookupGroup.groupName} [${partyId}]"
                                    ),
                                    @Widget(type = WidgetType.HORIZONTAL_SEPARATOR)}), failWidgets = @WidgetsForContainer2(value = {
                                        @Widget(type = WidgetType.LABEL, text = "${uiLabelMap.PartyNewUser}"
                                    )}))})}), position = 0)}), failWidgets = @InlineWidgets(value = {
                                        @Widget(type = WidgetType.LABEL, text = "${uiLabelMap.PartyMgrViewPermissionError}", style = "common-msg-error-perm"
                                    )}))})
        }
    )
    public interface CommonSetupDecorator {}

    @Screen(name = "CommonSetupWizardDecorator", location = "component://setup/widget/CommonScreens.xml")
    @Action(type = ActionType.SCRIPT, location = "component://setup/webapp/setup/WEB-INF/actions/SetupWizardCommonActions.groovy")
    @Action(type = ActionType.SCRIPT, location = "component://setup/webapp/setup/WEB-INF/actions/generated/CommonSetupWizardDecorator_script1.groovy")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", fromField = "activeSubMenuItem", defaultValue = "${setupStep}")
    @Action(type = ActionType.SET, field = "activeSubMenu", fromField = "activeSubMenu", defaultValue = "component://setup/widget/Menus.xml#SetupSteps")
    @Action(type = ActionType.SET, field = "noTitle", value = "true")
    @Action(type = ActionType.SET, field = "settingUpLabelProp", fromField = "settingUpLabelProp", defaultValue = "SetupSettingUp")
    @DecoratorScreen(
        name = "CommonSetupAppDecorator",
        location = "component://setup/widget/CommonScreens.xml",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.INCLUDE_SCREEN, name = "SetupProgress", location = "${parameters.mainDecoratorLocation}"
            )}, sections = {
                @InlineSection(condition = @com.ilscipio.scipio.widget.def.screen.Condition(not = true, functionalConditions = {
                    @Condition(type = Empty.class, params = {"orgPartyGroup"})}), widgets = @InlineWidgets(value = {
                        @Widget(type = WidgetType.HTML_TEMPLATE, content = "<@heading>${uiLabelMap[raw(settingUpLabelProp)]}: ${orgPartyGroup.groupName} [${uiLabelMap.CommonPartyId}: ${orgPartyGroup.partyId}]<#t/>\n                                            <#t/><#if productStore?has_content> - ${productStore.storeName!productStore.productStoreId}</#if></@heading>"
                    )})),
                    @InlineSection(condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {
                        @Condition(type = Compare.class, params = {"setupStepAllowed", "equals", "true", "Boolean"
                    })}), widgets = @InlineWidgets(value = {
                        @Widget(type = WidgetType.DECORATOR_SECTION_INCLUDE, name = "setup-pre-body", position = 0
                    ),
                    @Widget(type = WidgetType.DECORATOR_SECTION_INCLUDE, name = "body", position = 1
                ),
                @Widget(type = WidgetType.DECORATOR_SECTION_INCLUDE, name = "extra-body", position = 3
            )}, sections = {
                @SectionNested(condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {
                    @Condition(type = Compare.class, params = {"useDefaultSetupSubmitBar", "not-equals", "false", "Boolean"
                })}), widgets = @WidgetsForContainer(value = {
                    @Widget(type = WidgetType.HTML_TEMPLATE, content = "<#include \"component://setup/webapp/setup/common/common.ftl\">\n                                                  <@setupSubmitBar submitFormId=(submitFormId!)><#-- can't rely on this: isCreate=(isCreate!false)-->\n                                                    <@render type=\"section\" name=\"menu-functions\"/>\n                                                  </@setupSubmitBar>"
                )}), position = 2)}), failWidgets = @InlineWidgets(sections = {
                    @SectionNested(actions = @Actions(value = {
                        @Action(type = ActionType.SET, field = "setupErrorMsg", value = "${uiLabelMap.SetupErrorStepNotAccessible}"
                    )}), widgets = @WidgetsForContainer(value = {
                        @Widget(type = WidgetType.INCLUDE_SCREEN, name = "SetupErrorMsg", location = "component://setup/widget/CommonScreens.xml"
                    )}))}))})
        }
    )
    public interface CommonSetupWizardDecorator {}

    @Screen(name = "SetupErrorMsg", location = "component://setup/widget/CommonScreens.xml")
    @Action(type = ActionType.SCRIPT, location = "component://setup/webapp/setup/WEB-INF/actions/generated/SetupErrorMsg_script1.groovy")
    @Section(widgets = @Widgets(value = {@Widget(type = WidgetType.HTML_TEMPLATE, location = "component://setup/webapp/setup/wizard/setuperrormsg.ftl")}))
    public interface SetupErrorMsg {}

    @Screen(name = "SetupProgress", location = "component://setup/widget/CommonScreens.xml")
    @Section(widgets = @Widgets(value = {@Widget(type = WidgetType.HTML_TEMPLATE, location = "component://setup/webapp/setup/wizard/setupprogress.ftl")}))
    public interface SetupProgress {}

    @Screen(name = "EntityExportAll", location = "component://setup/widget/CommonScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleEntityExportAll")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "entityExportAll")
    @Action(type = ActionType.SET, field = "results", fromField = "parameters.results")
    @DecoratorScreen(
        name = "CommonSetupAppDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", screenlets = {
                @Screenlet(htmlTemplates = {
                    @HtmlTemplate(location = "", content = "<@alert type=\"info\"><a href=\"<@serverUrl uri=\"/admin/control/xmldsdump\" extLoginKey=true/>\"><#rt/>\n                                    <#lt/>${uiLabelMap.SetupExportAdvancedInfo}</a></@alert>"
                ),
                @HtmlTemplate(location = "component://webtools/webapp/webtools/entity/EntityExportAll.ftl"
            )})})
        }
    )
    public interface EntityExportAll {}

    @Screen(name = "MainSideBarMenu", location = "component://setup/widget/CommonScreens.xml")
    @Action(type = ActionType.SET, field = "menuCfg.location", value = "component://setup/widget/Menus.xml")
    @Action(type = ActionType.SET, field = "menuCfg.name", value = "SetupAppSideBar")
    @Action(type = ActionType.SET, field = "menuCfg.defLocation", value = "component://setup/widget/Menus.xml")
    @Section(widgets = @Widgets(value = {@Widget(type = WidgetType.INCLUDE_SCREEN, name = "ComplexSideBarMenu", location = "component://common/widget/CommonScreens.xml")}))
    public interface MainSideBarMenu {}

    @Screen(name = "DefMainSideBarMenu", location = "component://setup/widget/CommonScreens.xml")
    @Action(type = ActionType.SCRIPT, location = "component://common/webcommon/WEB-INF/actions/includes/scipio/PrepareDefComplexSideBarMenu.groovy")
    @Section(widgets = @Widgets(value = {@Widget(type = WidgetType.INCLUDE_SCREEN, name = "MainSideBarMenu")}))
    public interface DefMainSideBarMenu {}

    @Screen(name = "CommonSetupAppSideBarMenu", location = "component://setup/widget/CommonScreens.xml")
    @Action(type = ActionType.SET, field = "commonSetupAppBasePermCond", fromField = "commonSetupAppBasePermCond", valueType = "Boolean", defaultValue = "true")
    @Action(type = ActionType.SET, field = "commonSideBarMenu.cond", fromField = "commonSideBarMenu.cond", valueType = "Boolean", defaultValue = "${commonWebtoolsAppBasePermCond}")
    @Section(widgets = @Widgets(value = {@Widget(type = WidgetType.INCLUDE_SCREEN, name = "CommonSideBarMenu", location = "component://common/widget/CommonScreens.xml")}))
    public interface CommonSetupAppSideBarMenu {}

    @Screen(name = "CommonSetupAccountingTabsAction", location = "component://setup/widget/CommonScreens.xml")
    @Action(type = ActionType.SET, field = "setupStep", value = "accounting")
    @Action(type = ActionType.SCRIPT, location = "component://setup/webapp/setup/WEB-INF/actions/SetupWizardCommonActions.groovy")
    @Action(type = ActionType.SET, field = "accountingData", fromField = "setupStepStates[setupStep].stepData")
    @Action(type = ActionType.SCRIPT, location = "component://setup/webapp/setup/WEB-INF/actions/accounting/SetupAccounting.groovy")
    @Action(type = ActionType.SCRIPT, location = "component://setup/webapp/setup/WEB-INF/actions/accounting/EditGLAccountTree.groovy")
    @Action(type = ActionType.PROPERTY_MAP, resource = "ScipioSetupErrorUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "ScipioSetupUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "SetupUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "WebtoolsUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "ContentUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "MarketingUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "PartyUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "OrderUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "ProductUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "AccountingUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "CommonErrorUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "CommonUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.SET, field = "commonSetupAppBasePermCond", fromField = "commonSetupAppBasePermCond", valueType = "Boolean", defaultValue = "true")
    public interface CommonSetupAccountingTabsAction {}

}
