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
package com.ilscipio.scipio.marketing.widget;

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

    @Screen(name = "webapp-common-actions", location = "component://marketing/widget/CommonScreens.xml")
    @Action(type = ActionType.INCLUDE_SCREEN_ACTIONS, name = "MainSideBarMenu")
    @Action(type = ActionType.SET, field = "mainSideBarMenuCfg", fromField = "menuCfg")
    @Action(type = ActionType.SET, field = "mainComplexMenuCfg", fromField = "menuCfg")
    @Action(type = ActionType.SET, field = "menuCfg")
    public interface webapp_common_actions {}

    @Screen(name = "main-decorator", location = "component://marketing/widget/CommonScreens.xml")
    @Action(type = ActionType.PROPERTY_MAP, resource = "MarketingUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "PartyUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "ProductUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "CommonUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.SET, field = "layoutSettings.companyName", fromField = "uiLabelMap.MarketingCompanyName", global = true)
    @Action(type = ActionType.SET, field = "layoutSettings.companySubtitle", fromField = "uiLabelMap.MarketingCompanySubtitle", global = true)
    @Action(type = ActionType.SET, field = "activeApp", value = "marketing", global = true)
    @Action(type = ActionType.SET, field = "applicationTitle", fromField = "uiLabelMap.MarketingManagerApplication", global = true)
    @Action(type = ActionType.SET, field = "applicationMenuName", value = "MarketingAppBar", global = true)
    @Action(type = ActionType.SET, field = "applicationMenuLocation", value = "component://marketing/widget/MarketingMenus.xml", global = true)
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
                )}))}),
            @DecoratorSection(name = "body", value = {@Widget(type = WidgetType.DECORATOR_SECTION_INCLUDE, name = "body")})
        }
    )
    public interface main_decorator {}

    @Screen(name = "CommonMarketingAppDecorator", location = "component://marketing/widget/CommonScreens.xml")
    @Action(type = ActionType.CONDITION_TO_FIELD, field = "commonMarketingAppBasePermCond", valueType = "Boolean", onlyIfField = "empty", condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {@Condition(type = HasPermission.class, params = {"MARKETING", "_VIEW"})}))
    @DecoratorScreen(
        name = "main-decorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "left-column", useWhen = "${context.widePage != true}", overrideByAutoInclude = true, value = {
                @Widget(type = WidgetType.INCLUDE_SCREEN, name = "CommonMarketingAppSideBarMenu", location = "component://marketing/widget/CommonScreens.xml"
            )}),
            @DecoratorSection(name = "body", sections = {
                @InlineSection(condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {
                    @Condition(type = True.class, params = {"commonMarketingAppBasePermCond"
                })}), widgets = @InlineWidgets(value = {
                    @Widget(type = WidgetType.DECORATOR_SECTION_INCLUDE, name = "body"
                )}), failWidgets = @InlineWidgets(value = {
                    @Widget(type = WidgetType.LABEL, text = "${uiLabelMap.MarketingViewPermissionError}", style = "common-msg-error-perm"
                )}))})
        }
    )
    public interface CommonMarketingAppDecorator {}

    @Screen(name = "CommonContactListDecorator", location = "component://marketing/widget/CommonScreens.xml")
    @Action(type = ActionType.SET, field = "activeSubMenu", fromField = "activeSubMenu", defaultValue = "component://marketing/widget/ContactListMenus.xml#ContactList")
    @Action(type = ActionType.PROPERTY_MAP, resource = "MarketingUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.SET, field = "activeSubMenuItem", fromField = "activeSubMenuItem", defaultValue = "ContactList")
    @Action(type = ActionType.SET, field = "contactListId", fromField = "parameters.contactListId")
    @Action(type = ActionType.ENTITY_ONE, entityName = "ContactList", valueField = "contactList")
    @Action(type = ActionType.SET, field = "commonSideBarMenu.condList[]", value = "${not empty context.contactListId}", valueType = "Boolean")
    @DecoratorScreen(
        name = "CommonMarketingAppDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.DECORATOR_SECTION_INCLUDE, name = "body"
            )}, sections = {
                @InlineSection(condition = @com.ilscipio.scipio.widget.def.screen.Condition(not = true, functionalConditions = {
                    @Condition(type = Empty.class, params = {"contactListId"})}), widgets = @InlineWidgets(value = {
                        @Widget(type = WidgetType.LABEL, text = "${uiLabelMap.MarketingContactList} ${contactList.contactListName} [${contactListId}]", style = "heading"
                    )}), position = 0)})
        }
    )
    public interface CommonContactListDecorator {}

    @Screen(name = "main", location = "component://marketing/widget/CommonScreens.xml")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "main")
    @Action(type = ActionType.SET, field = "title", value = "${uiLabelMap.MarketingManagerApplication}")
    @Action(type = ActionType.SET, field = "titleProperty", value = "Marketing")
    @DecoratorScreen(
        name = "CommonMarketingAppDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", containers = {
                @Container(style = "${styles.grid_row}", containers = {
                    @Container2(style = "${styles.grid_large}4 ${styles.grid_cell}", includeScreens = {
                        @IncludeScreen(name = "DashboardStatsOrderTotalDay", location = "component://order/widget/ordermgr/CommonWidgets.xml"
                    )}),
                    @Container2(style = "${styles.grid_large}4 ${styles.grid_cell}", includeScreens = {
                        @IncludeScreen(name = "BestSellingProducts", location = "component://product/widget/catalog/ProductScreens.xml"
                    )}),
                    @Container2(style = "${styles.grid_large}4 ${styles.grid_cell}", includeScreens = {
                        @IncludeScreen(name = "ScipioNewRegistrations", location = "component://party/widget/partymgr/CommonScreens.xml"
                    )})}),
                    @Container(style = "${styles.grid_row}", containers = {
                        @Container2(style = "${styles.grid_large}6 ${styles.grid_cell}", includeScreens = {
                            @IncludeScreen(name = "ScipioLastCommunication", location = "component://party/widget/partymgr/CommonScreens.xml"
                        )}),
                        @Container2(style = "${styles.grid_large}6 ${styles.grid_cell}", includeScreens = {
                            @IncludeScreen(name = "ScipioMarketingCampaigns", location = "component://marketing/widget/CommonScreens.xml"
                        )})})})
        }
    )
    public interface main {}

    @Screen(name = "ScipioMarketingCampaigns", location = "component://marketing/widget/CommonScreens.xml")
    @Action(type = ActionType.SET, field = "marketingCampaignId", fromField = "parameters.marketingCampaignId")
    @Action(type = ActionType.SET, field = "entityName", value = "MarketingCampaign")
    @Action(type = ActionType.SET, field = "showActionButtons", value = "N")
    @Section(condition = @com.ilscipio.scipio.widget.def.screen.Condition(not = true, functionalConditions = {@Condition(type = Empty.class, params = {"entityName"})}), widgets = @Widgets(screenlets = {@Screenlet(title = "${uiLabelMap.PageTitleListMarketingCampaign}", includeScreens = {@IncludeScreen(name = "MarketingCampaignSearchResults", location = "component://marketing/widget/MarketingCampaignScreens.xml")})}))
    public interface ScipioMarketingCampaigns {}

    @Screen(name = "MainSideBarMenu", location = "component://marketing/widget/CommonScreens.xml")
    @Action(type = ActionType.SET, field = "menuCfg.location", value = "component://marketing/widget/MarketingMenus.xml")
    @Action(type = ActionType.SET, field = "menuCfg.name", value = "MarketingAppSideBar")
    @Action(type = ActionType.SET, field = "menuCfg.defLocation", value = "component://marketing/widget/MarketingMenus.xml")
    @Section(widgets = @Widgets(value = {@Widget(type = WidgetType.INCLUDE_SCREEN, name = "ComplexSideBarMenu", location = "component://common/widget/CommonScreens.xml")}))
    public interface MainSideBarMenu {}

    @Screen(name = "DefMainSideBarMenu", location = "component://marketing/widget/CommonScreens.xml")
    @Action(type = ActionType.SCRIPT, location = "component://common/webcommon/WEB-INF/actions/includes/scipio/PrepareDefComplexSideBarMenu.groovy")
    @Section(widgets = @Widgets(value = {@Widget(type = WidgetType.INCLUDE_SCREEN, name = "MainSideBarMenu")}))
    public interface DefMainSideBarMenu {}

    @Screen(name = "CommonMarketingAppSideBarMenu", location = "component://marketing/widget/CommonScreens.xml")
    @Action(type = ActionType.CONDITION_TO_FIELD, field = "commonMarketingAppBasePermCond", valueType = "Boolean", onlyIfField = "empty", condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {@Condition(type = HasPermission.class, params = {"MARKETING", "_VIEW"})}))
    @Action(type = ActionType.SET, field = "commonSideBarMenu.cond", fromField = "commonSideBarMenu.cond", valueType = "Boolean", defaultValue = "${commonMarketingAppBasePermCond}")
    @Section(widgets = @Widgets(value = {@Widget(type = WidgetType.INCLUDE_SCREEN, name = "CommonSideBarMenu", location = "component://common/widget/CommonScreens.xml")}))
    public interface CommonMarketingAppSideBarMenu {}

}
