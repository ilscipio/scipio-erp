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
package com.ilscipio.scipio.commonext.widget;

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
public class OfbizsetupCommonScreens {

    @Screen(name = "webapp-common-actions", location = "component://commonext/widget/ofbizsetup/CommonScreens.xml")
    @Action(type = ActionType.INCLUDE_SCREEN_ACTIONS, name = "MainSideBarMenu")
    @Action(type = ActionType.SET, field = "mainSideBarMenuCfg", fromField = "menuCfg")
    @Action(type = ActionType.SET, field = "mainComplexMenuCfg", fromField = "menuCfg")
    @Action(type = ActionType.SET, field = "menuCfg")
    public interface webapp_common_actions {}

    @Screen(name = "main-decorator", location = "component://commonext/widget/ofbizsetup/CommonScreens.xml")
    @Action(type = ActionType.PROPERTY_MAP, resource = "SetupUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "WebtoolsUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "CommonUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "ContentUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "PartyUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "ProductUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "AccountingUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.SET, field = "MainColumnStyle", value = "nocolumns", global = true)
    @Action(type = ActionType.SET, field = "layoutSettings.companyName", fromField = "uiLabelMap.SetupCompanyName", global = true)
    @Action(type = ActionType.SET, field = "layoutSettings.companySubtitle", fromField = "uiLabelMap.SetupCompanySubtitle", global = true)
    @Action(type = ActionType.SET, field = "activeApp", value = "ofbizsetup", global = true)
    @Action(type = ActionType.SET, field = "applicationMenuName", value = "SetupAppBar", global = true)
    @Action(type = ActionType.SET, field = "applicationMenuLocation", value = "component://commonext/widget/ofbizsetup/Menus.xml", global = true)
    @Action(type = ActionType.SET, field = "applicationTitle", value = "${uiLabelMap.SetupApp}", global = true)
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

    @Screen(name = "CommonSetupAppDecorator", location = "component://commonext/widget/ofbizsetup/CommonScreens.xml")
    @Action(type = ActionType.SET, field = "commonSetupAppBasePermCond", fromField = "commonSetupAppBasePermCond", valueType = "Boolean", defaultValue = "true")
    @DecoratorScreen(
        name = "main-decorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "left-column", useWhen = "${context.widePage != true}", overrideByAutoInclude = true, value = {
                @Widget(type = WidgetType.INCLUDE_SCREEN, name = "CommonSetupAppSideBarMenu", location = "component://commonext/widget/ofbizsetup/CommonScreens.xml"
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

    @Screen(name = "CommonPartyDecorator", location = "component://commonext/widget/ofbizsetup/CommonScreens.xml")
    @DecoratorScreen(
        name = "CommonSetupDecorator",
        location = "component://commonext/widget/ofbizsetup/CommonScreens.xml"
    )
    public interface CommonPartyDecorator {}

    @Screen(name = "CommonSetupDecorator", location = "component://commonext/widget/ofbizsetup/CommonScreens.xml")
    @Action(type = ActionType.SET, field = "activeSubMenu", fromField = "activeSubMenu", defaultValue = "component://commonext/widget/ofbizsetup/Menus.xml#Setup")
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

    @Screen(name = "EntityExportAll", location = "component://commonext/widget/ofbizsetup/CommonScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleEntityExportAll")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "entityExportAll")
    @Action(type = ActionType.SET, field = "results", fromField = "parameters.results")
    @DecoratorScreen(
        name = "CommonSetupAppDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", screenlets = {
                @Screenlet(htmlTemplates = {
                    @HtmlTemplate(location = "component://webtools/webapp/webtools/entity/EntityExportAll.ftl"
                )})})
        }
    )
    public interface EntityExportAll {}

    @Screen(name = "MainSideBarMenu", location = "component://commonext/widget/ofbizsetup/CommonScreens.xml")
    @Action(type = ActionType.SET, field = "menuCfg.location", value = "component://commonext/widget/ofbizsetup/Menus.xml")
    @Action(type = ActionType.SET, field = "menuCfg.name", value = "SetupAppSideBar")
    @Action(type = ActionType.SET, field = "menuCfg.defLocation", value = "component://commonext/widget/ofbizsetup/Menus.xml")
    @Section(widgets = @Widgets(value = {@Widget(type = WidgetType.INCLUDE_SCREEN, name = "ComplexSideBarMenu", location = "component://common/widget/CommonScreens.xml")}))
    public interface MainSideBarMenu {}

    @Screen(name = "DefMainSideBarMenu", location = "component://commonext/widget/ofbizsetup/CommonScreens.xml")
    @Action(type = ActionType.SCRIPT, location = "component://common/webcommon/WEB-INF/actions/includes/scipio/PrepareDefComplexSideBarMenu.groovy")
    @Section(widgets = @Widgets(value = {@Widget(type = WidgetType.INCLUDE_SCREEN, name = "MainSideBarMenu")}))
    public interface DefMainSideBarMenu {}

    @Screen(name = "CommonSetupAppSideBarMenu", location = "component://commonext/widget/ofbizsetup/CommonScreens.xml")
    @Action(type = ActionType.SET, field = "commonSetupAppBasePermCond", fromField = "commonSetupAppBasePermCond", valueType = "Boolean", defaultValue = "true")
    @Action(type = ActionType.SET, field = "commonSideBarMenu.cond", fromField = "commonSideBarMenu.cond", valueType = "Boolean", defaultValue = "${commonWebtoolsAppBasePermCond}")
    @Section(widgets = @Widgets(value = {@Widget(type = WidgetType.INCLUDE_SCREEN, name = "CommonSideBarMenu", location = "component://common/widget/CommonScreens.xml")}))
    public interface CommonSetupAppSideBarMenu {}

}
