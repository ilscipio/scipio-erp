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

import com.ilscipio.scipio.widget.def.menu.*;
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
public class OrdermgrOrderMenus {

    @Menu(
        name = "OrderAppBar",
        location = "component://order/widget/ordermgr/OrderMenus.xml",
        title = "${uiLabelMap.OrderManager}",
        extendsMenu = "CommonAppBarMenu",
        extendsResource = "component://common/widget/CommonMenus.xml",
        items = {
            @MenuItem(name = "main", title = "${uiLabelMap.CommonDashboard}", widgetStyle = "+${styles.menu_sidebar_itemdashboard}", overrideMode = "replace", sortMode = "off", condition = @MenuItemCondition(mode = "omit", or = {@com.ilscipio.scipio.widget.def.screen.OrCondition(ifHasPermission = {@com.ilscipio.scipio.widget.def.screen.IfHasPermission(permission = "ORDERMGR", action = "_VIEW"), @com.ilscipio.scipio.widget.def.screen.IfHasPermission(permission = "ORDERMGR", action = "_PURCHASE_VIEW")})}), link = @MenuLink(target = "main")),
            @MenuItem(name = "findorders", title = "${uiLabelMap.OrderOrders}", sortMode = "off", condition = @MenuItemCondition(mode = "omit", conditions = {@Condition(type = HasPermission.class, params = {"ORDERMGR", "_VIEW"})}), link = @MenuLink(target = "findorders")),
            @MenuItem(name = "request", title = "${uiLabelMap.OrderRequests}", condition = @MenuItemCondition(mode = "omit", or = {@com.ilscipio.scipio.widget.def.screen.OrCondition(ifHasPermission = {@com.ilscipio.scipio.widget.def.screen.IfHasPermission(permission = "ORDERMGR", action = "_VIEW"), @com.ilscipio.scipio.widget.def.screen.IfHasPermission(permission = "ORDERMGR", action = "_PURCHASE_VIEW")})}), link = @MenuLink(target = "FindRequest")),
            @MenuItem(name = "quote", title = "${uiLabelMap.OrderOrderQuotes}", condition = @MenuItemCondition(mode = "omit", or = {@com.ilscipio.scipio.widget.def.screen.OrCondition(ifHasPermission = {@com.ilscipio.scipio.widget.def.screen.IfHasPermission(permission = "ORDERMGR", action = "_VIEW"), @com.ilscipio.scipio.widget.def.screen.IfHasPermission(permission = "ORDERMGR", action = "_PURCHASE_VIEW")})}), link = @MenuLink(target = "FindQuote")),
            @MenuItem(name = "orderlist", title = "${uiLabelMap.OrderOrderList} / ${uiLabelMap.CommonFilter}", condition = @MenuItemCondition(mode = "omit", conditions = {@Condition(type = HasPermission.class, params = {"ORDERMGR", "_VIEW"})}), link = @MenuLink(target = "orderlist")),
            @MenuItem(name = "orderentry", title = "${uiLabelMap.OrderOrderEntry}", condition = @MenuItemCondition(mode = "omit", or = {@com.ilscipio.scipio.widget.def.screen.OrCondition(ifHasPermission = {@com.ilscipio.scipio.widget.def.screen.IfHasPermission(permission = "ORDERMGR", action = "_CREATE"), @com.ilscipio.scipio.widget.def.screen.IfHasPermission(permission = "ORDERMGR", action = "_PURCHASE_CREATE")})}), link = @MenuLink(target = "orderentry", linkType = LinkType.ANCHOR)),
            @MenuItem(name = "return", title = "${uiLabelMap.OrderOrderReturns}", condition = @MenuItemCondition(mode = "omit", conditions = {@Condition(type = HasPermission.class, params = {"ORDERMGR", "_RETURN"})}), link = @MenuLink(target = "findreturn")),
            @MenuItem(name = "requirement", title = "${uiLabelMap.OrderRequirements}", condition = @MenuItemCondition(mode = "omit", or = {@com.ilscipio.scipio.widget.def.screen.OrCondition(ifHasPermission = {@com.ilscipio.scipio.widget.def.screen.IfHasPermission(permission = "ORDERMGR", action = "_VIEW"), @com.ilscipio.scipio.widget.def.screen.IfHasPermission(permission = "ORDERMGR_ROLE", action = "_VIEW")})}), link = @MenuLink(target = "FindRequirements")),
            @MenuItem(name = "stats", title = "${uiLabelMap.CommonStats}", link = @MenuLink(target = "orderstats"))
        }
    )
    public interface OrderAppBar {}

    @Menu(
        name = "OrderAppSideBar",
        location = "component://order/widget/ordermgr/OrderMenus.xml",
        title = "${uiLabelMap.OrderManager}",
        extendsMenu = "CommonAppSideBarMenu",
        extendsResource = "component://common/widget/CommonMenus.xml",
        includeElements = {
            @IncludeElements(menuName = "OrderAppBar")
        },
        alwaysExpandSelectedOrAncestor = "true",
        items = {
            @MenuItem(name = "findorders", subMenus = {@SubMenu(name = "Order", include = "component://order/widget/ordermgr/OrderMenus.xml#OrderSideBar")}),
            @MenuItem(name = "request", subMenus = {@SubMenu(name = "Request", include = "component://order/widget/ordermgr/OrderMenus.xml#RequestSideBar")}),
            @MenuItem(name = "quote", subMenus = {@SubMenu(name = "Quote", include = "component://order/widget/ordermgr/OrderMenus.xml#QuoteSideBar")}),
            @MenuItem(name = "return", subMenus = {@SubMenu(name = "Return", include = "component://order/widget/ordermgr/OrderMenus.xml#ReturnSideBar")}),
            @MenuItem(name = "requirement", subMenus = {@SubMenu(name = "Requirements", include = "component://order/widget/ordermgr/OrderMenus.xml#RequirementsSideBar")})
        }
    )
    public interface OrderAppSideBar {}

    @Menu(
        name = "OrderTabBar",
        location = "component://order/widget/ordermgr/OrderMenus.xml",
        extendsMenu = "CommonTabBarMenu",
        extendsResource = "component://common/widget/CommonMenus.xml",
        actions = @MenuActions(entityOne = {@EntityOneAction(entityName = "OrderHeader", valueField = "OrderTabBar_orderHeader")}, entityCondition = {@EntityConditionAction(entityName = "OrderItemShipGroup", list = "OrderTabBar_shipGroups")}),
        items = {
            @MenuItem(name = "Summary", title = "${uiLabelMap.CommonSummary}", condition = @MenuItemCondition(not = true, conditions = {@Condition(type = Empty.class, params = {"orderId"})}), link = @MenuLink(target = "orderview", parameters = {@MenuParameter(paramName = "orderId", fromField = "orderId")})),
            @MenuItem(name = "OrderShipping", title = "${uiLabelMap.OrderShipmentInformation}", widgetStyle = "+${styles.action_nav}", condition = @MenuItemCondition(not = true, conditions = {@Condition(type = Empty.class, params = {"OrderTabBar_shipGroups"})}), link = @MenuLink(target = "orderShipping", parameters = {@MenuParameter(paramName = "orderId", fromField = "orderId")})),
            @MenuItem(name = "OrderDeliveryScheduleInfo", title = "${uiLabelMap.OrderViewEditDeliveryScheduleInfo}", condition = @MenuItemCondition(conditions = {@Condition(type = NotEmpty.class, params = {"OrderTabBar_orderHeader"}), @Condition(type = Compare.class, params = {"OrderTabBar_orderHeader.statusId", "not-equals", "ORDER_COMPLETED"}), @Condition(type = Compare.class, params = {"OrderTabBar_orderHeader.statusId", "not-equals", "ORDER_CANCELLED"})}), link = @MenuLink(target = "OrderDeliveryScheduleInfo", parameters = {@MenuParameter(paramName = "orderId", fromField = "orderId")})),
            @MenuItem(name = "OrderHistory", title = "${uiLabelMap.OrderViewOrderHistory}", condition = @MenuItemCondition(not = true, conditions = {@Condition(type = Empty.class, params = {"orderId"})}), link = @MenuLink(target = "OrderHistory", parameters = {@MenuParameter(paramName = "orderId", fromField = "orderId")}))
        }
    )
    public interface OrderTabBar {}

    @Menu(
        name = "OrderSideBar",
        location = "component://order/widget/ordermgr/OrderMenus.xml",
        menuContainerStyle = "+scipio-nav-actions-menu",
        extendsMenu = "CommonSideBarMenu",
        extendsResource = "component://common/widget/CommonMenus.xml",
        includeElements = {
            @IncludeElements(menuName = "OrderTabBar", recursive = RecursiveMode.INCLUDES_ONLY)
        }
    )
    public interface OrderSideBar {}

    @Menu(
        name = "OrderShippingSubTabBar",
        location = "component://order/widget/ordermgr/OrderMenus.xml",
        extendsMenu = "CommonSubTabBarMenu",
        extendsResource = "component://common/widget/CommonMenus.xml",
        items = {
            @MenuItem(name = "QuickShipOrder", title = "${uiLabelMap.OrderQuickShipEntireOrder}", widgetStyle = "+${styles.action_run_sys} ${styles.action_complete}", condition = @MenuItemCondition(mode = "omit", conditions = {@Condition(type = HasPermission.class, params = {"FACILITY", "_CREATE"}), @Condition(type = Compare.class, params = {"orderHeader.orderTypeId", "equals", "SALES_ORDER"}), @Condition(type = Compare.class, params = {"orderHeader.statusId", "equals", "ORDER_APPROVED"}), @Condition(type = False.class, params = {"allOrderItemsShipped"}), @Condition(type = False.class, params = {"allShipGroupsNoShipping"})}), link = @MenuLink(target = "quickShipOrder", parameters = {@MenuParameter(paramName = "orderId", fromField = "orderId"), @MenuParameter(paramName = "setItemStatus", value = "Y"), @MenuParameter(paramName = "statusId", value = "ORDER_COMPLETED")})),
            @MenuItem(name = "CreateShipGroup", title = "${uiLabelMap.OrderCreateShipGroup}", widgetStyle = "+${styles.action_run_sys} ${styles.action_add}", condition = @MenuItemCondition(conditions = {@Condition(type = Compare.class, params = {"orderHeader.statusId", "not-equals", "ORDER_COMPLETED"}), @Condition(type = Compare.class, params = {"orderHeader.statusId", "not-equals", "ORDER_CANCELLED"}), @Condition(type = Compare.class, params = {"orderHeader.statusId", "not-equals", "ORDER_SENT"}), @Condition(type = False.class, params = {"allOrderItemsShipped"}), @Condition(type = False.class, params = {"singleOrderItem"}), @Condition(type = False.class, params = {"allShipGroupsNoShipping"}), @Condition(type = False.class, params = {"orderContainsOnlyDigitalProducts"}), @Condition(type = True.class, params = {"maySplit"}), @Condition(type = False.class, params = {"maxShipGroups"})}), link = @MenuLink(target = "createOrderItemShipGroup", parameters = {@MenuParameter(paramName = "orderId", fromField = "orderId")})),
            @MenuItem(name = "CompleteShipGroup", title = "${uiLabelMap.OrderCompleteOrder}", widgetStyle = "+${styles.action_run_sys} ${styles.action_add}", condition = @MenuItemCondition(conditions = {@Condition(type = Compare.class, params = {"orderHeader.statusId", "equals", "ORDER_APPROVED"}), @Condition(type = True.class, params = {"setOrderCompleteOption"}), @Condition(type = Empty.class, params = {"allShipments"}), @Condition(type = False.class, params = {"allOrderItemsShipped"})}), itemActions = @MenuActions(script = {@ScriptAction(location = "component://order/webapp/order/WEB-INF/actions/generated/OrderShippingSubTabBar_CompleteShipGroup_script1.groovy")}), link = @MenuLink(target = "completeSalesOrder", parameters = {@MenuParameter(paramName = "orderId", fromField = "orderId"), @MenuParameter(paramName = "setItemStatus", value = "Y"), @MenuParameter(paramName = "statusId", value = "ORDER_COMPLETED")}))
        }
    )
    public interface OrderShippingSubTabBar {}

    @Menu(
        name = "RequirementsTabBar",
        location = "component://order/widget/ordermgr/OrderMenus.xml",
        extendsMenu = "CommonTabBarMenu",
        extendsResource = "component://common/widget/CommonMenus.xml",
        items = {
            @MenuItem(name = "FindRequirements", title = "${uiLabelMap.OrderRequirements}", link = @MenuLink(target = "FindRequirements")),
            @MenuItem(name = "ApproveRequirements", title = "${uiLabelMap.OrderApproveRequirements}", link = @MenuLink(target = "ApproveRequirements")),
            @MenuItem(name = "ApprovedProductRequirementsByVendor", title = "${uiLabelMap.PageTitleFindApprovedRequirementsBySupplier}", link = @MenuLink(target = "ApprovedProductRequirementsByVendor")),
            @MenuItem(name = "ApprovedProductRequirements", title = "${uiLabelMap.OrderApprovedProductRequirements}", link = @MenuLink(target = "ApprovedProductRequirements"))
        }
    )
    public interface RequirementsTabBar {}

    @Menu(
        name = "RequirementsSideBar",
        location = "component://order/widget/ordermgr/OrderMenus.xml",
        extendsMenu = "CommonSideBarMenu",
        extendsResource = "component://common/widget/CommonMenus.xml",
        includeElements = {
            @IncludeElements(menuName = "RequirementsTabBar", recursive = RecursiveMode.INCLUDES_ONLY)
        },
        items = {
            @MenuItem(name = "FindRequirements", subMenus = {@SubMenu(name = "Requirement", include = "component://order/widget/ordermgr/OrderMenus.xml#RequirementSideBar")})
        }
    )
    public interface RequirementsSideBar {}

    @Menu(
        name = "RequirementTabBar",
        location = "component://order/widget/ordermgr/OrderMenus.xml",
        extendsMenu = "CommonTabBarMenu",
        extendsResource = "component://common/widget/CommonMenus.xml",
        items = {
            @MenuItem(name = "EditRequirement", title = "${uiLabelMap.OrderRequirement}", link = @MenuLink(target = "EditRequirement", parameters = {@MenuParameter(paramName = "requirementId", fromField = "requirement.requirementId")})),
            @MenuItem(name = "ListRequirementCustRequests", title = "${uiLabelMap.OrderRequests}", link = @MenuLink(target = "ListRequirementCustRequests", parameters = {@MenuParameter(paramName = "requirementId", fromField = "requirement.requirementId")})),
            @MenuItem(name = "ListRequirementOrdersTab", title = "${uiLabelMap.OrderOrders}", link = @MenuLink(target = "ListRequirementOrders", parameters = {@MenuParameter(paramName = "requirementId", fromField = "requirement.requirementId")})),
            @MenuItem(name = "ListRequirementRolesTab", title = "${uiLabelMap.PartyRoles}", link = @MenuLink(target = "ListRequirementRoles", parameters = {@MenuParameter(paramName = "requirementId", fromField = "requirement.requirementId")}))
        }
    )
    public interface RequirementTabBar {}

    @Menu(
        name = "RequirementSideBar",
        location = "component://order/widget/ordermgr/OrderMenus.xml",
        menuContainerStyle = "+scipio-nav-actions-menu",
        extendsMenu = "CommonSideBarMenu",
        extendsResource = "component://common/widget/CommonMenus.xml",
        includeElements = {
            @IncludeElements(menuName = "RequirementTabBar", recursive = RecursiveMode.INCLUDES_ONLY)
        }
    )
    public interface RequirementSideBar {}

    @Menu(
        name = "QuoteTabBar",
        location = "component://order/widget/ordermgr/OrderMenus.xml",
        extendsMenu = "CommonButtonBarMenu",
        extendsResource = "component://common/widget/CommonMenus.xml",
        items = {
            @MenuItem(name = "ViewQuote", title = "${uiLabelMap.CommonSummary}", condition = @MenuItemCondition(not = true, conditions = {@Condition(type = Empty.class, params = {"quoteId"})}), link = @MenuLink(target = "ViewQuote", parameters = {@MenuParameter(paramName = "quoteId", fromField = "quote.quoteId")})),
            @MenuItem(name = "EditQuote", title = "${uiLabelMap.OrderQuote}", condition = @MenuItemCondition(not = true, conditions = {@Condition(type = Empty.class, params = {"quoteId"})}), link = @MenuLink(target = "EditQuote", parameters = {@MenuParameter(paramName = "quoteId", fromField = "quote.quoteId")})),
            @MenuItem(name = "ListQuoteAttributes", title = "${uiLabelMap.CommonAttributes}", condition = @MenuItemCondition(not = true, conditions = {@Condition(type = Empty.class, params = {"quoteId"})}), link = @MenuLink(target = "ListQuoteAttributes", parameters = {@MenuParameter(paramName = "quoteId", fromField = "quoteId")})),
            @MenuItem(name = "ListQuoteItems", title = "${uiLabelMap.CommonItems}", condition = @MenuItemCondition(not = true, conditions = {@Condition(type = Empty.class, params = {"quoteId"})}), link = @MenuLink(target = "ListQuoteItems", parameters = {@MenuParameter(paramName = "quoteId", fromField = "quote.quoteId")})),
            @MenuItem(name = "ListQuoteNotes", title = "${uiLabelMap.CommonNotes}", condition = @MenuItemCondition(not = true, conditions = {@Condition(type = Empty.class, params = {"quoteId"})}), link = @MenuLink(target = "ListQuoteNotes", parameters = {@MenuParameter(paramName = "quoteId", fromField = "quote.quoteId")})),
            @MenuItem(name = "ListQuoteRoles", title = "${uiLabelMap.PartyParties}", condition = @MenuItemCondition(not = true, conditions = {@Condition(type = Empty.class, params = {"quoteId"})}), link = @MenuLink(target = "ListQuoteRoles", parameters = {@MenuParameter(paramName = "quoteId", fromField = "quote.quoteId")})),
            @MenuItem(name = "ListQuoteCoefficients", title = "${uiLabelMap.OrderOrderQuoteCoefficients}", condition = @MenuItemCondition(mode = "omit", conditions = {@Condition(type = HasPermission.class, params = {"ORDERMGR", "_QUOTE_PRICE"}), @Condition(type = NotEmpty.class, params = {"quoteId"})}), link = @MenuLink(target = "ListQuoteCoefficients", parameters = {@MenuParameter(paramName = "quoteId", fromField = "quote.quoteId")})),
            @MenuItem(name = "ManageQuotePrices", title = "${uiLabelMap.ProductPrices}", condition = @MenuItemCondition(mode = "omit", conditions = {@Condition(type = HasPermission.class, params = {"ORDERMGR", "_QUOTE_PRICE"}), @Condition(type = NotEmpty.class, params = {"quoteId"})}), link = @MenuLink(target = "ManageQuotePrices", parameters = {@MenuParameter(paramName = "quoteId", fromField = "quote.quoteId")})),
            @MenuItem(name = "ListQuoteAdjustments", title = "${uiLabelMap.OrderOrderQuoteAdjustments}", condition = @MenuItemCondition(mode = "omit", conditions = {@Condition(type = HasPermission.class, params = {"ORDERMGR", "_QUOTE_PRICE"}), @Condition(type = NotEmpty.class, params = {"quoteId"})}), link = @MenuLink(target = "ListQuoteAdjustments", parameters = {@MenuParameter(paramName = "quoteId", fromField = "quote.quoteId")})),
            @MenuItem(name = "ViewQuoteProfit", title = "${uiLabelMap.OrderViewQuoteProfit}", condition = @MenuItemCondition(mode = "omit", conditions = {@Condition(type = HasPermission.class, params = {"ORDERMGR", "_QUOTE_PRICE"}), @Condition(type = NotEmpty.class, params = {"quoteId"})}), link = @MenuLink(target = "ViewQuoteProfit", parameters = {@MenuParameter(paramName = "quoteId", fromField = "quote.quoteId")})),
            @MenuItem(name = "QuoteWorkEfforts", title = "${uiLabelMap.WorkEfforts}", condition = @MenuItemCondition(not = true, conditions = {@Condition(type = Empty.class, params = {"quoteId"})}), link = @MenuLink(target = "ListQuoteWorkEfforts", parameters = {@MenuParameter(paramName = "quoteId", fromField = "quote.quoteId")})),
            @MenuItem(name = "QuoteTerms", title = "${uiLabelMap.CommonTerms}", condition = @MenuItemCondition(not = true, conditions = {@Condition(type = Empty.class, params = {"quoteId"})}), link = @MenuLink(target = "ListQuoteTerms", parameters = {@MenuParameter(paramName = "quoteId", fromField = "parameters.quoteId")}))
        }
    )
    public interface QuoteTabBar {}

    @Menu(
        name = "QuoteSideBar",
        location = "component://order/widget/ordermgr/OrderMenus.xml",
        menuContainerStyle = "+scipio-nav-actions-menu",
        extendsMenu = "CommonSideBarMenu",
        extendsResource = "component://common/widget/CommonMenus.xml",
        includeElements = {
            @IncludeElements(menuName = "QuoteTabBar", recursive = RecursiveMode.INCLUDES_ONLY)
        }
    )
    public interface QuoteSideBar {}

    @Menu(
        name = "QuoteSubTabBar",
        location = "component://order/widget/ordermgr/OrderMenus.xml",
        menuContainerStyle = "+${styles.menu_buttonstyle_alt2}",
        extendsMenu = "CommonButtonBarMenu",
        extendsResource = "component://common/widget/CommonMenus.xml",
        items = {
            @MenuItem(name = "NewQuote", title = "${uiLabelMap.OrderNewQuote}", widgetStyle = "+${styles.action_nav} ${styles.action_add}", link = @MenuLink(target = "EditQuote")),
            @MenuItem(name = "EditQuote", title = "${uiLabelMap.CommonEdit}", condition = @MenuItemCondition(conditions = {@Condition(type = Compare.class, params = {"quote.statusId", "equals", "QUO_CREATED"})}), link = @MenuLink(target = "EditQuote", parameters = {@MenuParameter(paramName = "quoteId", fromField = "quote.quoteId")})),
            @MenuItem(name = "EditQuoteItem", title = "${uiLabelMap.OrderCreateOrderQuoteItem}", condition = @MenuItemCondition(conditions = {@Condition(type = Compare.class, params = {"quote.statusId", "not-equals", "QUO_APPROVED"}), @Condition(type = Compare.class, params = {"quote.statusId", "not-equals", "QUO_REJECTED"}), @Condition(type = Compare.class, params = {"quote.statusId", "not-equals", "QUO_ORDERED"})}), link = @MenuLink(target = "EditQuoteItem", text = "${uiLabelMap.OrderCreateOrderQuoteItem}", parameters = {@MenuParameter(paramName = "quoteId", fromField = "quote.quoteId")})),
            @MenuItem(name = "EditQuoteTerm", title = "${uiLabelMap.OrderCreateOrderQuoteTerm}", condition = @MenuItemCondition(conditions = {@Condition(type = NotEmpty.class, params = {"quote.quoteId"}), @Condition(type = Empty.class, params = {"parameters.quoteItemSeqId"}), @Condition(type = Compare.class, params = {"quote.statusId", "not-equals", "QUO_APPROVED"}), @Condition(type = Compare.class, params = {"quote.statusId", "not-equals", "QUO_REJECTED"}), @Condition(type = Compare.class, params = {"quote.statusId", "not-equals", "QUO_ORDERED"})}), link = @MenuLink(target = "EditQuoteTerm", parameters = {@MenuParameter(paramName = "quoteId", fromField = "parameters.quoteId")})),
            @MenuItem(name = "EditQuoteTermItem", title = "${uiLabelMap.OrderCreateOrderQuoteTerm}", condition = @MenuItemCondition(conditions = {@Condition(type = NotEmpty.class, params = {"quote.quoteId"}), @Condition(type = NotEmpty.class, params = {"parameters.quoteItemSeqId"}), @Condition(type = Compare.class, params = {"quote.statusId", "not-equals", "QUO_APPROVED"}), @Condition(type = Compare.class, params = {"quote.statusId", "not-equals", "QUO_REJECTED"}), @Condition(type = Compare.class, params = {"quote.statusId", "not-equals", "QUO_ORDERED"})}), link = @MenuLink(target = "EditQuoteTermItem", parameters = {@MenuParameter(paramName = "quoteId", fromField = "parameters.quoteId"), @MenuParameter(paramName = "quoteItemSeqId", fromField = "parameters.quoteItemSeqId")})),
            @MenuItem(name = "NewWorkEffort", title = "${uiLabelMap.OrderCreateQuoteWorkEffort}", widgetStyle = "+${styles.action_nav} ${styles.action_add}", condition = @MenuItemCondition(conditions = {@Condition(type = NotEmpty.class, params = {"quote.quoteId"}), @Condition(type = Empty.class, params = {"parameters.quoteItemSeqId"}), @Condition(type = Compare.class, params = {"quote.statusId", "not-equals", "QUO_APPROVED"}), @Condition(type = Compare.class, params = {"quote.statusId", "not-equals", "QUO_REJECTED"}), @Condition(type = Compare.class, params = {"quote.statusId", "not-equals", "QUO_ORDERED"})}), link = @MenuLink(target = "AddQuoteWorkEffort", parameters = {@MenuParameter(paramName = "quoteId", fromField = "quoteId")})),
            @MenuItem(name = "NewRole", title = "${uiLabelMap.OrderCreateOrderQuoteRole}", widgetStyle = "+${styles.action_nav} ${styles.action_add}", condition = @MenuItemCondition(conditions = {@Condition(type = Compare.class, params = {"quote.statusId", "not-equals", "QUO_APPROVED"}), @Condition(type = Compare.class, params = {"quote.statusId", "not-equals", "QUO_REJECTED"}), @Condition(type = Compare.class, params = {"quote.statusId", "not-equals", "QUO_ORDERED"})}), link = @MenuLink(target = "EditQuoteRole", parameters = {@MenuParameter(paramName = "quoteId", fromField = "quote.quoteId")})),
            @MenuItem(name = "NewNote", title = "${uiLabelMap.OrderCreateOrderQuoteNote}", widgetStyle = "+${styles.action_nav} ${styles.action_add}", link = @MenuLink(target = "createnewquotenote", parameters = {@MenuParameter(paramName = "quoteId", fromField = "quote.quoteId")})),
            @MenuItem(name = "createOrder", title = "${uiLabelMap.OrderCreateOrder}", condition = @MenuItemCondition(disabledStyle = "disabled", conditions = {@Condition(type = NotEmpty.class, params = {"quote"}), @Condition(type = Compare.class, params = {"quote.statusId", "equals", "QUO_APPROVED"})}), link = @MenuLink(target = "loadCartFromQuote", parameters = {@MenuParameter(paramName = "quoteId", fromField = "quote.quoteId"), @MenuParameter(paramName = "finalizeMode", value = "init")})),
            @MenuItem(name = "quoteReport", title = "${uiLabelMap.CommonPdf}", widgetStyle = "+${styles.action_run_sys} ${styles.action_export}", link = @MenuLink(target = "QuoteReport", targetWindow = "new", parameters = {@MenuParameter(paramName = "quoteId", fromField = "quote.quoteId")})),
            @MenuItem(name = "editQuoteReportMail", title = "${uiLabelMap.CommonPdf}: ${uiLabelMap.CommonSendReportByMail}", widgetStyle = "+${styles.action_run_sys} ${styles.action_send}", link = @MenuLink(target = "EditQuoteReportMail", parameters = {@MenuParameter(paramName = "quoteId", fromField = "quote.quoteId"), @MenuParameter(paramName = "emailType", value = "PRDS_QUO_CONFIRM")}))
        }
    )
    public interface QuoteSubTabBar {}

    @Menu(
        name = "QuoteItemsSubTabBar",
        location = "component://order/widget/ordermgr/OrderMenus.xml",
        menuContainerStyle = "+${styles.menu_buttonstyle_alt2}",
        extendsMenu = "CommonButtonBarMenu",
        extendsResource = "component://common/widget/CommonMenus.xml"
    )
    public interface QuoteItemsSubTabBar {}

    @Menu(
        name = "RequestTabBar",
        location = "component://order/widget/ordermgr/OrderMenus.xml",
        extendsMenu = "CommonButtonBarMenu",
        extendsResource = "component://common/widget/CommonMenus.xml",
        items = {
            @MenuItem(name = "ViewRequest", title = "${uiLabelMap.OrderRequestOverview}", link = @MenuLink(target = "ViewRequest", parameters = {@MenuParameter(paramName = "custRequestId", fromField = "custRequest.custRequestId")})),
            @MenuItem(name = "editRequest", title = "${uiLabelMap.OrderRequest}", condition = @MenuItemCondition(conditions = {@Condition(type = NotEmpty.class, params = {"custRequest"}), @Condition(type = Compare.class, params = {"custRequest.statusId", "not-equals", "CRQ_CANCELLED"}), @Condition(type = Compare.class, params = {"custRequest.statusId", "not-equals", "CRQ_COMPLETED"})}), link = @MenuLink(target = "request", parameters = {@MenuParameter(paramName = "custRequestId", fromField = "custRequest.custRequestId")})),
            @MenuItem(name = "requestroles", title = "${uiLabelMap.OrderRequestRoles}", condition = @MenuItemCondition(conditions = {@Condition(type = NotEmpty.class, params = {"custRequest"}), @Condition(type = Compare.class, params = {"custRequest.statusId", "not-equals", "CRQ_CANCELLED"}), @Condition(type = Compare.class, params = {"custRequest.statusId", "not-equals", "CRQ_COMPLETED"})}), link = @MenuLink(target = "requestroles", parameters = {@MenuParameter(paramName = "custRequestId", fromField = "custRequest.custRequestId")})),
            @MenuItem(name = "requestitems", title = "${uiLabelMap.OrderRequestItems}", condition = @MenuItemCondition(conditions = {@Condition(type = NotEmpty.class, params = {"custRequest"}), @Condition(type = Compare.class, params = {"custRequest.statusId", "not-equals", "CRQ_CANCELLED"}), @Condition(type = Compare.class, params = {"custRequest.statusId", "not-equals", "CRQ_COMPLETED"})}), link = @MenuLink(target = "requestitems", parameters = {@MenuParameter(paramName = "custRequestId", fromField = "custRequest.custRequestId")})),
            @MenuItem(name = "custRequestContent", title = "${uiLabelMap.OrderRequestContent}", condition = @MenuItemCondition(conditions = {@Condition(type = NotEmpty.class, params = {"custRequest"}), @Condition(type = Compare.class, params = {"custRequest.statusId", "not-equals", "CRQ_CANCELLED"}), @Condition(type = Compare.class, params = {"custRequest.statusId", "not-equals", "CRQ_COMPLETED"})}), link = @MenuLink(target = "EditCustRequestContent", parameters = {@MenuParameter(paramName = "custRequestId", fromField = "custRequest.custRequestId")}))
        }
    )
    public interface RequestTabBar {}

    @Menu(
        name = "RequestSideBar",
        location = "component://order/widget/ordermgr/OrderMenus.xml",
        menuContainerStyle = "+scipio-nav-actions-menu",
        extendsMenu = "CommonSideBarMenu",
        extendsResource = "component://common/widget/CommonMenus.xml",
        includeElements = {
            @IncludeElements(menuName = "RequestTabBar", recursive = RecursiveMode.INCLUDES_ONLY)
        },
        items = {
            @MenuItem(name = "requestitems", subMenus = {@SubMenu(name = "RequestItem", include = "component://order/widget/ordermgr/OrderMenus.xml#RequestItemSideBar")})
        }
    )
    public interface RequestSideBar {}

    @Menu(
        name = "RequestSubTabBar",
        location = "component://order/widget/ordermgr/OrderMenus.xml",
        extendsMenu = "CommonButtonBarMenu",
        extendsResource = "component://common/widget/CommonMenus.xml",
        items = {
            @MenuItem(name = "newRequest", title = "${uiLabelMap.OrderNewRequest}", widgetStyle = "+${styles.action_nav} ${styles.action_add}", condition = @MenuItemCondition(or = {@com.ilscipio.scipio.widget.def.screen.OrCondition(ifNotEmpty = {"custRequest"}, ifCompare = {@com.ilscipio.scipio.widget.def.screen.IfCompare(field = "activeSubMenuItem", operator = "not-equals", value = "editRequest")})}), link = @MenuLink(target = "EditRequest")),
            @MenuItem(name = "createQuoteFromRequest", title = "${uiLabelMap.OrderCreateQuoteFromRequest}", condition = @MenuItemCondition(conditions = {@Condition(type = NotEmpty.class, params = {"custRequest"}), @Condition(type = Compare.class, params = {"custRequest.custRequestTypeId", "equals", "RF_QUOTE"})}), link = @MenuLink(target = "createQuoteFromCustRequest", parameters = {@MenuParameter(paramName = "custRequestId", fromField = "custRequest.custRequestId")}))
        }
    )
    public interface RequestSubTabBar {}

    @Menu(
        name = "RequestScreenletMenu",
        location = "component://order/widget/ordermgr/OrderMenus.xml",
        menuContainerStyle = "+${styles.menu_buttonstyle_alt2} ${styles.menu_noclear}",
        extendsMenu = "CommonButtonBarMenu",
        extendsResource = "component://common/widget/CommonMenus.xml",
        items = {
            @MenuItem(name = "newRequest", title = "${uiLabelMap.OrderNewRequest}", widgetStyle = "+${styles.action_nav} ${styles.action_add}", link = @MenuLink(target = "EditRequest"))
        }
    )
    public interface RequestScreenletMenu {}

    @Menu(
        name = "ReturnTabBar",
        location = "component://order/widget/ordermgr/OrderMenus.xml",
        extendsMenu = "CommonButtonBarMenu",
        extendsResource = "component://common/widget/CommonMenus.xml",
        items = {
            @MenuItem(name = "OrderReturnHeader", title = "${uiLabelMap.CommonSummary}", link = @MenuLink(target = "returnMain", parameters = {@MenuParameter(paramName = "returnId", fromField = "returnId")})),
            @MenuItem(name = "OrderReturnItems", title = "${uiLabelMap.OrderReturnItems}", link = @MenuLink(target = "returnItems", parameters = {@MenuParameter(paramName = "returnId", fromField = "returnId")})),
            @MenuItem(name = "OrderReturnHistory", title = "${uiLabelMap.OrderReturnHistory}", link = @MenuLink(target = "ReturnHistory", parameters = {@MenuParameter(paramName = "returnId", fromField = "returnId")}))
        }
    )
    public interface ReturnTabBar {}

    @Menu(
        name = "ReturnSideBar",
        location = "component://order/widget/ordermgr/OrderMenus.xml",
        menuContainerStyle = "+scipio-nav-actions-menu",
        extendsMenu = "CommonSideBarMenu",
        extendsResource = "component://common/widget/CommonMenus.xml",
        includeElements = {
            @IncludeElements(menuName = "ReturnTabBar", recursive = RecursiveMode.INCLUDES_ONLY)
        }
    )
    public interface ReturnSideBar {}

    @Menu(
        name = "editrequestitemmenu",
        location = "component://order/widget/ordermgr/OrderMenus.xml",
        extendsMenu = "CommonButtonBarMenu",
        extendsResource = "component://common/widget/CommonMenus.xml",
        items = {
            @MenuItem(name = "requestitem", title = "${uiLabelMap.OrderRequestItem}", link = @MenuLink(target = "requestitem", id = "requestitem", parameters = {@MenuParameter(paramName = "custRequestId", fromField = "custRequest.custRequestId"), @MenuParameter(paramName = "custRequestItemSeqId", fromField = "custRequestItem.custRequestItemSeqId")})),
            @MenuItem(name = "requestitemnotes", title = "${uiLabelMap.OrderRequestItemNotes}", link = @MenuLink(target = "requestitemnotes", id = "requestitemnotes", parameters = {@MenuParameter(paramName = "custRequestId", fromField = "custRequest.custRequestId"), @MenuParameter(paramName = "custRequestItemSeqId", fromField = "custRequestItem.custRequestItemSeqId")})),
            @MenuItem(name = "requestitemquotes", title = "${uiLabelMap.OrderRequestItemQuotes}", link = @MenuLink(target = "RequestItemQuotes", id = "requestitemquotes", parameters = {@MenuParameter(paramName = "custRequestId", fromField = "custRequest.custRequestId"), @MenuParameter(paramName = "custRequestItemSeqId", fromField = "custRequestItem.custRequestItemSeqId")})),
            @MenuItem(name = "workeffortrequirements", title = "${uiLabelMap.OrderRequestItemWorkEffort}", link = @MenuLink(target = "requestitemrequirements", id = "workeffortrequirements", parameters = {@MenuParameter(paramName = "custRequestId", fromField = "custRequest.custRequestId"), @MenuParameter(paramName = "custRequestItemSeqId", fromField = "custRequestItem.custRequestItemSeqId")}))
        }
    )
    public interface editrequestitemmenu {}

    @Menu(
        name = "RequestItemSideBar",
        location = "component://order/widget/ordermgr/OrderMenus.xml",
        menuContainerStyle = "+scipio-nav-actions-menu",
        extendsMenu = "CommonSideBarMenu",
        extendsResource = "component://common/widget/CommonMenus.xml",
        includeElements = {
            @IncludeElements(menuName = "editrequestitemmenu", recursive = RecursiveMode.INCLUDES_ONLY)
        }
    )
    public interface RequestItemSideBar {}

    @Menu(
        name = "quoteTermSubTabBar",
        location = "component://order/widget/ordermgr/OrderMenus.xml",
        menuContainerStyle = "+${styles.menu_buttonstyle_alt2}",
        extendsMenu = "CommonButtonBarMenu",
        extendsResource = "component://common/widget/CommonMenus.xml",
        items = {
            @MenuItem(name = "EditQuoteTerm", title = "${uiLabelMap.OrderCreateOrderQuoteTerm}", condition = @MenuItemCondition(conditions = {@Condition(type = Empty.class, params = {"parameters.quoteItemSeqId"}), @Condition(type = NotEmpty.class, params = {"quote.quoteId"})}), link = @MenuLink(target = "EditQuoteTerm", parameters = {@MenuParameter(paramName = "quoteId", fromField = "parameters.quoteId")})),
            @MenuItem(name = "EditQuoteTermItem", title = "${uiLabelMap.OrderCreateOrderQuoteTerm}", condition = @MenuItemCondition(conditions = {@Condition(type = NotEmpty.class, params = {"parameters.quoteItemSeqId"}), @Condition(type = NotEmpty.class, params = {"quote.quoteId"})}), link = @MenuLink(target = "EditQuoteTermItem", parameters = {@MenuParameter(paramName = "quoteId", fromField = "parameters.quoteId"), @MenuParameter(paramName = "quoteItemSeqId", fromField = "parameters.quoteItemSeqId")}))
        }
    )
    public interface quoteTermSubTabBar {}

}
