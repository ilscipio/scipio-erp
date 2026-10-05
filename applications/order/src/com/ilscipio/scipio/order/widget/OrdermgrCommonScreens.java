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
package com.ilscipio.scipio.order.widget;

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
public class OrdermgrCommonScreens {

    @Screen(name = "webapp-common-actions", location = "component://order/widget/ordermgr/CommonScreens.xml")
    @Action(type = ActionType.INCLUDE_SCREEN_ACTIONS, name = "MainSideBarMenu")
    @Action(type = ActionType.SET, field = "mainSideBarMenuCfg", fromField = "menuCfg")
    @Action(type = ActionType.SET, field = "mainComplexMenuCfg", fromField = "menuCfg")
    @Action(type = ActionType.SET, field = "menuCfg")
    public interface webapp_common_actions {}

    @Screen(name = "main-decorator", location = "component://order/widget/ordermgr/CommonScreens.xml")
    @Action(type = ActionType.SET, field = "activeApp", value = "ordermgr", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "EcommerceUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "OrderUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "OrderErrorUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "AccountingUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "HumanResUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "ProductUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "ManufacturingUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "WorkEffortUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "PartyUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "ContentUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "CommonUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.SET, field = "layoutSettings.companyName", fromField = "uiLabelMap.OrderCompanyName", global = true)
    @Action(type = ActionType.SET, field = "layoutSettings.companySubtitle", fromField = "uiLabelMap.OrderCompanySubtitle", global = true)
    @Action(type = ActionType.SET, field = "applicationMenuName", value = "OrderAppBar", global = true)
    @Action(type = ActionType.SET, field = "applicationMenuLocation", value = "component://order/widget/ordermgr/OrderMenus.xml", global = true)
    @Action(type = ActionType.SET, field = "applicationTitle", value = "${uiLabelMap.Order}", global = true)
    @Action(type = ActionType.SET, field = "customerDetailLink", value = "/partymgr/control/viewprofile?partyId=", global = true)
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

    @Screen(name = "CommonOrderAppDecorator", location = "component://order/widget/ordermgr/CommonScreens.xml")
    @Action(type = ActionType.CONDITION_TO_FIELD, field = "commonOrderAppBasePermCond", valueType = "Boolean", onlyIfField = "empty", condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {@Condition(type = HasPermission.class, params = {"ORDERMGR", "_VIEW"})}))
    @DecoratorScreen(
        name = "main-decorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "left-column", useWhen = "${context.widePage != true}", overrideByAutoInclude = true, value = {
                @Widget(type = WidgetType.INCLUDE_SCREEN, name = "CommonOrderAppSideBarMenu", location = "component://order/widget/ordermgr/CommonScreens.xml"
            )}),
            @DecoratorSection(name = "body", sections = {
                @InlineSection(condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {
                    @Condition(type = True.class, params = {"commonOrderAppBasePermCond"
                })}), widgets = @InlineWidgets(value = {
                    @Widget(type = WidgetType.DECORATOR_SECTION_INCLUDE, name = "body"
                )}), failWidgets = @InlineWidgets(value = {
                    @Widget(type = WidgetType.LABEL, text = "${uiLabelMap.OrderViewPermissionError}", style = "common-msg-error-perm"
                )}))})
        }
    )
    public interface CommonOrderAppDecorator {}

    @Screen(name = "CommonQuoteDecorator", location = "component://order/widget/ordermgr/CommonScreens.xml")
    @Action(type = ActionType.SET, field = "activeSubMenu", fromField = "activeSubMenu", defaultValue = "component://order/widget/ordermgr/OrderMenus.xml#Quote")
    @Action(type = ActionType.SET, field = "quoteId", fromField = "parameters.quoteId")
    @Action(type = ActionType.SET, field = "commonSideBarMenu.condList[]", value = "${not empty context.quoteId}", valueType = "Boolean")
    @DecoratorScreen(
        name = "CommonOrderAppDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.DECORATOR_SECTION_INCLUDE, name = "body"
            )}, sections = {
                @InlineSection(condition = @com.ilscipio.scipio.widget.def.screen.Condition(not = true, functionalConditions = {
                    @Condition(type = Empty.class, params = {"quoteId"})}), widgets = @InlineWidgets(value = {
                        @Widget(type = WidgetType.INCLUDE_MENU, name = "QuoteSubTabBar", location = "component://order/widget/ordermgr/OrderMenus.xml"
                    )}), position = 0)})
        }
    )
    public interface CommonQuoteDecorator {}

    @Screen(name = "CommonOrderDecorator", location = "component://order/widget/ordermgr/CommonScreens.xml")
    @Action(type = ActionType.SET, field = "activeSubMenu", fromField = "activeSubMenu", defaultValue = "component://order/widget/ordermgr/OrderMenus.xml#Order")
    @Action(type = ActionType.SET, field = "commonSideBarMenu.condList[]", value = "${not empty context.orderId}", valueType = "Boolean")
    @DecoratorScreen(
        name = "CommonOrderAppDecorator",
        location = "${parameters.mainDecoratorLocation}"
    )
    public interface CommonOrderDecorator {}

    @Screen(name = "CommonOrderCheckoutDecorator", location = "component://order/widget/ordermgr/CommonScreens.xml")
    @Action(type = ActionType.SET, field = "activeSubMenu", fromField = "activeSubMenu")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", fromField = "activeSubMenuItem", defaultValue = "orderentry")
    @Action(type = ActionType.SET, field = "checkoutType", fromField = "checkoutType", defaultValue = "${parameters.checkoutType}")
    @Action(type = ActionType.SCRIPT, location = "component://order/webapp/ordermgr/WEB-INF/actions/entry/SetShoppingCart.groovy")
    @Action(type = ActionType.SCRIPT, location = "component://order/webapp/ordermgr/WEB-INF/actions/entry/SetCheckOutTabBar.groovy")
    @Action(type = ActionType.SET, field = "commonSideBarMenu.condList[]", value = "${not empty context.orderId}", valueType = "Boolean")
    @DecoratorScreen(
        name = "CommonOrderAppDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.HTML_TEMPLATE, location = "component://order/webapp/ordermgr/entry/OrderEntryCheckOutTabBar.ftl"
            ),
            @Widget(type = WidgetType.DECORATOR_SECTION_INCLUDE, name = "body"
            )})
        }
    )
    public interface CommonOrderCheckoutDecorator {}

    @Screen(name = "CommonPartyDecorator", location = "component://order/widget/ordermgr/CommonScreens.xml")
    @Action(type = ActionType.SET, field = "activeSubMenu", fromField = "activeSubMenu")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", fromField = "activeSubMenuItem")
    @DecoratorScreen(
        name = "CommonOrderAppDecorator",
        location = "${parameters.mainDecoratorLocation}"
    )
    public interface CommonPartyDecorator {}

    @Screen(name = "CommonPopUpDecorator", location = "component://order/widget/ordermgr/CommonScreens.xml")
    @Action(type = ActionType.PROPERTY_MAP, resource = "OrderUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "OrderErrorUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "AccountingUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "ProductUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "ManufacturingUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "WorkEffortUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "PartyUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "ContentUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.SET, field = "layoutSettings.companyName", fromField = "uiLabelMap.OrderCompanyName", global = true)
    @Action(type = ActionType.SET, field = "layoutSettings.companySubtitle", fromField = "uiLabelMap.OrderCompanySubtitle", global = true)
    @DecoratorScreen(
        name = "LookupDecorator",
        location = "component://common/widget/CommonScreens.xml"
    )
    public interface CommonPopUpDecorator {}

    @Screen(name = "CommonReportDecorator", location = "component://order/widget/ordermgr/CommonScreens.xml")
    @Action(type = ActionType.SET, field = "activeSubMenu", fromField = "activeSubMenu")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", fromField = "activeSubMenuItem", defaultValue = "reports")
    @DecoratorScreen(
        name = "CommonOrderAppDecorator",
        location = "${parameters.mainDecoratorLocation}"
    )
    public interface CommonReportDecorator {}

    @Screen(name = "CommonRequestDecorator", location = "component://order/widget/ordermgr/CommonScreens.xml")
    @Action(type = ActionType.SET, field = "activeSubMenu", fromField = "activeSubMenu", defaultValue = "component://order/widget/ordermgr/OrderMenus.xml#Request")
    @Action(type = ActionType.SET, field = "custRequestId", fromField = "parameters.custRequestId")
    @Action(type = ActionType.ENTITY_ONE, entityName = "CustRequest", valueField = "custRequest")
    @Action(type = ActionType.SET, field = "commonSideBarMenu.condList[]", value = "${not empty context.custRequest}", valueType = "Boolean")
    @DecoratorScreen(
        name = "CommonOrderAppDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.INCLUDE_MENU, name = "RequestSubTabBar", location = "component://order/widget/ordermgr/OrderMenus.xml"
            ),
            @Widget(type = WidgetType.DECORATOR_SECTION_INCLUDE, name = "body"
            )}, containers = {
                @Container(sections = {
                    @SectionNested(condition = @com.ilscipio.scipio.widget.def.screen.Condition(not = true, functionalConditions = {
                        @Condition(type = Empty.class, params = {"custRequest"})}), widgets = @WidgetsForContainer(value = {
                            @Widget(type = WidgetType.LABEL, text = "${custRequest.custRequestName} [${custRequest.custRequestId}] ", style = "heading"
                        )}))}, position = 1)})
        }
    )
    public interface CommonRequestDecorator {}

    @Screen(name = "CommonRequirementDecorator", location = "component://order/widget/ordermgr/CommonScreens.xml")
    @Action(type = ActionType.SET, field = "activeSubMenu", fromField = "activeSubMenu", defaultValue = "component://order/widget/ordermgr/OrderMenus.xml#Requirement")
    @DecoratorScreen(
        name = "CommonOrderAppDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "left-column", useWhen = "${context.widePage != true}", overrideByAutoInclude = true, sections = {
                @InlineSection(condition = @com.ilscipio.scipio.widget.def.screen.Condition(not = true, functionalConditions = {
                    @Condition(type = Empty.class, params = {"requirementId"})}), widgets = @InlineWidgets(value = {
                        @Widget(type = WidgetType.INCLUDE_SCREEN, name = "CommonOrderAppSideBarMenu"
                    )}), failWidgets = @InlineWidgets(sections = {
                        @SectionNested(actions = @Actions(value = {
                            @Action(type = ActionType.SET, field = "activeSubMenu", value = "component://order/widget/ordermgr/OrderMenus.xml#Requirements"
                        ),
                        @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "FindRequirements"
                    )}), widgets = @WidgetsForContainer(value = {
                        @Widget(type = WidgetType.INCLUDE_SCREEN, name = "CommonOrderAppSideBarMenu"
                    )}))}))}),
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.DECORATOR_SECTION_INCLUDE, name = "body"
            )}, sections = {
                @InlineSection(condition = @com.ilscipio.scipio.widget.def.screen.Condition(not = true, functionalConditions = {
                    @Condition(type = Empty.class, params = {"requirement"})}), widgets = @InlineWidgets(value = {
                        @Widget(type = WidgetType.LABEL, text = "${uiLabelMap.OrderRequirement} [${requirementId}]", style = "heading"
                    )}), position = 0)})
        }
    )
    public interface CommonRequirementDecorator {}

    @Screen(name = "CommonRequirementsDecorator", location = "component://order/widget/ordermgr/CommonScreens.xml")
    @Action(type = ActionType.SET, field = "activeSubMenu", fromField = "activeSubMenu", defaultValue = "Requirements")
    @DecoratorScreen(
        name = "CommonOrderAppDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.DECORATOR_SECTION_INCLUDE, name = "body"
            )}, containers = {
                @Container(widgets = {
                    @Widget(type = WidgetType.LINK, text = "${uiLabelMap.OrderNewRequirement}", style = "${styles.link_nav} ${styles.action_add}", target = "EditRequirement"
                )}, position = 0)})
        }
    )
    public interface CommonRequirementsDecorator {}

    @Screen(name = "CommonReturnDecorator", location = "component://order/widget/ordermgr/CommonScreens.xml")
    @Action(type = ActionType.SET, field = "activeSubMenu", fromField = "activeSubMenu", defaultValue = "component://order/widget/ordermgr/OrderMenus.xml#Return")
    @Action(type = ActionType.SET, field = "commonSideBarMenu.condList[]", value = "${not empty context.returnId}", valueType = "Boolean")
    @DecoratorScreen(
        name = "CommonOrderAppDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.HTML_TEMPLATE, location = "component://order/webapp/ordermgr/return/returnLinks.ftl"
            ),
            @Widget(type = WidgetType.DECORATOR_SECTION_INCLUDE, name = "body"
            )})
        }
    )
    public interface CommonReturnDecorator {}

    @Screen(name = "MainSideBarMenu", location = "component://order/widget/ordermgr/CommonScreens.xml")
    @Action(type = ActionType.SET, field = "menuCfg.location", value = "component://order/widget/ordermgr/OrderMenus.xml")
    @Action(type = ActionType.SET, field = "menuCfg.name", value = "OrderAppSideBar")
    @Action(type = ActionType.SET, field = "menuCfg.defLocation", value = "component://order/widget/ordermgr/OrderMenus.xml")
    @Section(widgets = @Widgets(value = {@Widget(type = WidgetType.INCLUDE_SCREEN, name = "ComplexSideBarMenu", location = "component://common/widget/CommonScreens.xml")}))
    public interface MainSideBarMenu {}

    @Screen(name = "DefMainSideBarMenu", location = "component://order/widget/ordermgr/CommonScreens.xml")
    @Action(type = ActionType.SCRIPT, location = "component://common/webcommon/WEB-INF/actions/includes/scipio/PrepareDefComplexSideBarMenu.groovy")
    @Section(widgets = @Widgets(value = {@Widget(type = WidgetType.INCLUDE_SCREEN, name = "MainSideBarMenu")}))
    public interface DefMainSideBarMenu {}

    @Screen(name = "CommonOrderAppSideBarMenu", location = "component://order/widget/ordermgr/CommonScreens.xml")
    @Action(type = ActionType.CONDITION_TO_FIELD, field = "commonOrderAppBasePermCond", valueType = "Boolean", onlyIfField = "empty", condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {@Condition(type = HasPermission.class, params = {"ORDERMGR", "_VIEW"})}))
    @Action(type = ActionType.SET, field = "commonSideBarMenu.cond", fromField = "commonSideBarMenu.cond", valueType = "Boolean", defaultValue = "${commonOrderAppBasePermCond}")
    @Section(widgets = @Widgets(value = {@Widget(type = WidgetType.INCLUDE_SCREEN, name = "CommonSideBarMenu", location = "component://common/widget/CommonScreens.xml")}))
    public interface CommonOrderAppSideBarMenu {}

}
