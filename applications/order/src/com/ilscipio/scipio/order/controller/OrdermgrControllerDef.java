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
package com.ilscipio.scipio.order.controller;

import com.ilscipio.scipio.ce.webapp.control.def.View;
import com.ilscipio.scipio.ce.webapp.control.def.Request;
import com.ilscipio.scipio.ce.webapp.control.def.Response;
import com.ilscipio.scipio.ce.webapp.control.def.Responses;
import com.ilscipio.scipio.ce.webapp.control.def.Event;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import org.ofbiz.order.OrderManagerEvents;
import org.ofbiz.order.shoppingcart.ShoppingCartEvents;
import org.ofbiz.order.order.OrderEvents;
import org.ofbiz.order.shoppinglist.ShoppingListEvents;
import org.ofbiz.product.product.ProductSearchSession;
import org.ofbiz.content.survey.SurveyEvents;
import org.ofbiz.order.shoppingcart.CheckOutEvents;
import org.ofbiz.order.shoppingcart.shipping.ShippingEvents;
import org.ofbiz.product.product.ProductEvents;

/**
 * Auto-generated annotation-based controller definitions.
 *
 * <p>Generated from controller.xml by convertXmlToAnnotation Gradle task.</p>
 *
 * <p>SCIPIO: 4.0.0: Auto-generated.</p>
 */
public class OrdermgrControllerDef {
    @com.ilscipio.scipio.ce.webapp.control.def.View(
        name = "LookupProductCategory",
        type = "screen",
        page = "component://product/widget/catalog/LookupScreens.xml#LookupProductCategory",
        controller = "ordermgr"
    )
    public static final String VIEW_LOOKUPPRODUCTCATEGORY = "LookupProductCategory";

    @com.ilscipio.scipio.ce.webapp.control.def.View(
        name = "main",
        type = "screen",
        page = "component://order/widget/ordermgr/OrderViewScreens.xml#Main",
        controller = "ordermgr"
    )
    public static final String VIEW_MAIN = "main";

    @com.ilscipio.scipio.ce.webapp.control.def.View(
        name = "orderstats",
        type = "screen",
        page = "component://order/widget/ordermgr/OrderViewScreens.xml#OrderStats",
        controller = "ordermgr"
    )
    public static final String VIEW_ORDERSTATS = "orderstats";

    @com.ilscipio.scipio.ce.webapp.control.def.View(
        name = "findorders",
        type = "screen",
        page = "component://order/widget/ordermgr/OrderViewScreens.xml#OrderFindOrder",
        controller = "ordermgr"
    )
    public static final String VIEW_FINDORDERS = "findorders";

    @com.ilscipio.scipio.ce.webapp.control.def.View(
        name = "OrderDeliveryScheduleInfo",
        type = "screen",
        page = "component://order/widget/ordermgr/OrderViewScreens.xml#OrderDeliveryScheduleInfo",
        controller = "ordermgr"
    )
    public static final String VIEW_ORDERDELIVERYSCHEDULEINFO = "OrderDeliveryScheduleInfo";

    @com.ilscipio.scipio.ce.webapp.control.def.View(
        name = "orderview",
        type = "screen",
        page = "component://order/widget/ordermgr/OrderViewScreens.xml#OrderHeaderView",
        controller = "ordermgr"
    )
    public static final String VIEW_ORDERVIEW = "orderview";

    @com.ilscipio.scipio.ce.webapp.control.def.View(
        name = "orderShipping",
        type = "screen",
        page = "component://order/widget/ordermgr/OrderViewScreens.xml#OrderShipping",
        controller = "ordermgr"
    )
    public static final String VIEW_ORDERSHIPPING = "orderShipping";

    @com.ilscipio.scipio.ce.webapp.control.def.View(
        name = "OrderHistory",
        type = "screen",
        page = "component://order/widget/ordermgr/OrderViewScreens.xml#OrderHistory",
        controller = "ordermgr"
    )
    public static final String VIEW_ORDERHISTORY = "OrderHistory";

    @com.ilscipio.scipio.ce.webapp.control.def.View(
        name = "orderlist",
        type = "screen",
        page = "component://order/widget/ordermgr/OrderViewScreens.xml#OrderHeaderListView",
        controller = "ordermgr"
    )
    public static final String VIEW_ORDERLIST = "orderlist";

    @com.ilscipio.scipio.ce.webapp.control.def.View(
        name = "editorderitems",
        type = "screen",
        page = "component://order/widget/ordermgr/OrderViewScreens.xml#OrderItemEdit",
        controller = "ordermgr"
    )
    public static final String VIEW_EDITORDERITEMS = "editorderitems";

    @com.ilscipio.scipio.ce.webapp.control.def.View(
        name = "createnewnote",
        type = "screen",
        page = "component://order/widget/ordermgr/OrderViewScreens.xml#OrderNewNote",
        controller = "ordermgr"
    )
    public static final String VIEW_CREATENEWNOTE = "createnewnote";

    @com.ilscipio.scipio.ce.webapp.control.def.View(
        name = "receivepayment",
        type = "screen",
        page = "component://order/widget/ordermgr/OrderViewScreens.xml#OrderReceivePayment",
        controller = "ordermgr"
    )
    public static final String VIEW_RECEIVEPAYMENT = "receivepayment";

    @com.ilscipio.scipio.ce.webapp.control.def.View(
        name = "viewimage",
        type = "screen",
        page = "component://order/widget/ordermgr/OrderViewScreens.xml#ViewImage",
        controller = "ordermgr"
    )
    public static final String VIEW_VIEWIMAGE = "viewimage";

    @com.ilscipio.scipio.ce.webapp.control.def.View(
        name = "ListOrderTerms",
        type = "screen",
        page = "component://order/widget/ordermgr/OrderViewScreens.xml#ListOrderTerms",
        controller = "ordermgr"
    )
    public static final String VIEW_LISTORDERTERMS = "ListOrderTerms";

    @com.ilscipio.scipio.ce.webapp.control.def.View(
        name = "survey",
        type = "screen",
        page = "component://order/widget/ordermgr/OrderEntryCartScreens.xml#survey",
        controller = "ordermgr"
    )
    public static final String VIEW_SURVEY = "survey";

    @com.ilscipio.scipio.ce.webapp.control.def.View(
        name = "showcart",
        type = "screen",
        page = "component://order/widget/ordermgr/OrderEntryCartScreens.xml#ShowCart",
        controller = "ordermgr"
    )
    public static final String VIEW_SHOWCART = "showcart";

    @com.ilscipio.scipio.ce.webapp.control.def.View(
        name = "checkinits",
        type = "screen",
        page = "component://order/widget/ordermgr/OrderEntryScreens.xml#CheckInits",
        controller = "ordermgr"
    )
    public static final String VIEW_CHECKINITS = "checkinits";

    @com.ilscipio.scipio.ce.webapp.control.def.View(
        name = "orderagreements",
        type = "screen",
        page = "component://order/widget/ordermgr/OrderEntryScreens.xml#OrderAgreements",
        controller = "ordermgr"
    )
    public static final String VIEW_ORDERAGREEMENTS = "orderagreements";

    @com.ilscipio.scipio.ce.webapp.control.def.View(
        name = "viewshoppinglists",
        type = "screen",
        page = "component://order/widget/ordermgr/OrderEntryScreens.xml#ViewShoppingLists",
        controller = "ordermgr"
    )
    public static final String VIEW_VIEWSHOPPINGLISTS = "viewshoppinglists";

    @com.ilscipio.scipio.ce.webapp.control.def.View(
        name = "addfromshoppinglist",
        type = "screen",
        page = "component://order/widget/ordermgr/OrderEntryScreens.xml#AddFromShoppingList",
        controller = "ordermgr"
    )
    public static final String VIEW_ADDFROMSHOPPINGLIST = "addfromshoppinglist";


    // Auto-generated split (Part 2)
    public static class Part2 {
        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "keywordsearch",
            type = "screen",
            page = "component://order/widget/ordermgr/OrderEntryCatalogScreens.xml#keywordsearch",
            controller = "ordermgr"
        )
        public static final String VIEW_KEYWORDSEARCH = "keywordsearch";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "advancedsearch",
            type = "screen",
            page = "component://order/widget/ordermgr/OrderEntryCatalogScreens.xml#advancedsearch",
            controller = "ordermgr"
        )
        public static final String VIEW_ADVANCEDSEARCH = "advancedsearch";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "quickcheckout",
            type = "screen",
            page = "component://order/widget/ordermgr/OrderEntryOrderScreens.xml#quickFinalizeOrder",
            controller = "ordermgr"
        )
        public static final String VIEW_QUICKCHECKOUT = "quickcheckout";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "checkoutshippingaddress",
            type = "screen",
            page = "component://order/widget/ordermgr/OrderEntryOrderScreens.xml#checkoutshippingaddress",
            controller = "ordermgr"
        )
        public static final String VIEW_CHECKOUTSHIPPINGADDRESS = "checkoutshippingaddress";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "editcontactmech",
            type = "screen",
            page = "component://party/widget/partymgr/PartyScreens.xml#editcontactmech",
            controller = "ordermgr"
        )
        public static final String VIEW_EDITCONTACTMECH = "editcontactmech";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "editcreditcard",
            type = "screen",
            page = "component://party/widget/partymgr/PaymentMethodScreens.xml#editcreditcard",
            controller = "ordermgr"
        )
        public static final String VIEW_EDITCREDITCARD = "editcreditcard";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "editeftaccount",
            type = "screen",
            page = "component://party/widget/partymgr/PaymentMethodScreens.xml#editeftaccount",
            controller = "ordermgr"
        )
        public static final String VIEW_EDITEFTACCOUNT = "editeftaccount";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "checkoutpayment",
            type = "screen",
            page = "component://order/widget/ordermgr/OrderEntryOrderScreens.xml#checkoutpayment",
            controller = "ordermgr"
        )
        public static final String VIEW_CHECKOUTPAYMENT = "checkoutpayment";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "category",
            type = "screen",
            page = "component://order/widget/ordermgr/OrderEntryCatalogScreens.xml#category",
            controller = "ordermgr"
        )
        public static final String VIEW_CATEGORY = "category";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "product",
            type = "screen",
            page = "component://order/widget/ordermgr/OrderEntryCatalogScreens.xml#product",
            controller = "ordermgr"
        )
        public static final String VIEW_PRODUCT = "product";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "compareProducts",
            type = "screen",
            page = "component://order/widget/ordermgr/OrderEntryCatalogScreens.xml#compareProducts",
            controller = "ordermgr"
        )
        public static final String VIEW_COMPAREPRODUCTS = "compareProducts";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "quickadd",
            type = "screen",
            page = "component://order/widget/ordermgr/OrderEntryCatalogScreens.xml#quickadd",
            controller = "ordermgr"
        )
        public static final String VIEW_QUICKADD = "quickadd";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "AddGiftCertificate",
            type = "screen",
            page = "component://order/widget/ordermgr/OrderEntryCartScreens.xml#AddGiftCertificate",
            controller = "ordermgr"
        )
        public static final String VIEW_ADDGIFTCERTIFICATE = "AddGiftCertificate";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "custsetting",
            type = "screen",
            page = "component://order/widget/ordermgr/OrderEntryOrderScreens.xml#CustSettings",
            controller = "ordermgr"
        )
        public static final String VIEW_CUSTSETTING = "custsetting";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "shipsetting",
            type = "screen",
            page = "component://order/widget/ordermgr/OrderEntryOrderScreens.xml#ShipSettings",
            controller = "ordermgr"
        )
        public static final String VIEW_SHIPSETTING = "shipsetting";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "EditShipAddress",
            type = "screen",
            page = "component://order/widget/ordermgr/OrderEntryOrderScreens.xml#EditShipAddress",
            controller = "ordermgr"
        )
        public static final String VIEW_EDITSHIPADDRESS = "EditShipAddress";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "SetItemShipGroups",
            type = "screen",
            page = "component://order/widget/ordermgr/OrderEntryOrderScreens.xml#SetItemShipGroups",
            controller = "ordermgr"
        )
        public static final String VIEW_SETITEMSHIPGROUPS = "SetItemShipGroups";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "optionsetting",
            type = "screen",
            page = "component://order/widget/ordermgr/OrderEntryOrderScreens.xml#OptionSettings",
            controller = "ordermgr"
        )
        public static final String VIEW_OPTIONSETTING = "optionsetting";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "billsetting",
            type = "screen",
            page = "component://order/widget/ordermgr/OrderEntryOrderScreens.xml#checkoutpayment",
            controller = "ordermgr"
        )
        public static final String VIEW_BILLSETTING = "billsetting";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "confirm",
            type = "screen",
            page = "component://order/widget/ordermgr/OrderEntryOrderScreens.xml#ConfirmOrder",
            controller = "ordermgr"
        )
        public static final String VIEW_CONFIRM = "confirm";

    }

    // Auto-generated split (Part 3)
    public static class Part3 {
        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "ordercomplete",
            type = "screen",
            page = "component://order/widget/ordermgr/OrderViewScreens.xml#OrderHeaderView",
            controller = "ordermgr"
        )
        public static final String VIEW_ORDERCOMPLETE = "ordercomplete";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "orderTerm",
            type = "screen",
            page = "component://order/widget/ordermgr/OrderEntryOrderScreens.xml#OrderTerms",
            controller = "ordermgr"
        )
        public static final String VIEW_ORDERTERM = "orderTerm";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "setAdditionalParty",
            type = "screen",
            page = "component://order/widget/ordermgr/OrderEntryOrderScreens.xml#SetAdditionalParty",
            controller = "ordermgr"
        )
        public static final String VIEW_SETADDITIONALPARTY = "setAdditionalParty";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "showAllPromotions",
            type = "screen",
            page = "component://order/widget/ordermgr/OrderEntryCartScreens.xml#showAllPromotions",
            controller = "ordermgr"
        )
        public static final String VIEW_SHOWALLPROMOTIONS = "showAllPromotions";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "showPromotionDetails",
            type = "screen",
            page = "component://order/widget/ordermgr/OrderEntryCartScreens.xml#showPromotionDetails",
            controller = "ordermgr"
        )
        public static final String VIEW_SHOWPROMOTIONDETAILS = "showPromotionDetails";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "findreturn",
            type = "screen",
            page = "component://order/widget/ordermgr/OrderReturnScreens.xml#OrderFindReturn",
            controller = "ordermgr"
        )
        public static final String VIEW_FINDRETURN = "findreturn";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "returnlist",
            type = "screen",
            page = "component://order/widget/ordermgr/OrderReturnScreens.xml#OrderReturnList",
            controller = "ordermgr"
        )
        public static final String VIEW_RETURNLIST = "returnlist";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "returnhead",
            type = "screen",
            page = "component://order/widget/ordermgr/OrderReturnScreens.xml#OrderReturnHeader",
            controller = "ordermgr"
        )
        public static final String VIEW_RETURNHEAD = "returnhead";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "returnitems",
            type = "screen",
            page = "component://order/widget/ordermgr/OrderReturnScreens.xml#OrderReturnItems",
            controller = "ordermgr"
        )
        public static final String VIEW_RETURNITEMS = "returnitems";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "quickReturn",
            type = "screen",
            page = "component://order/widget/ordermgr/OrderReturnScreens.xml#OrderQuickReturn",
            controller = "ordermgr"
        )
        public static final String VIEW_QUICKRETURN = "quickReturn";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "paysetup",
            type = "screen",
            page = "component://order/widget/ordermgr/OrderSetupScreens.xml#OrderPaymentSetup",
            controller = "ordermgr"
        )
        public static final String VIEW_PAYSETUP = "paysetup";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "OrderPurchaseReportOptions",
            type = "screen",
            page = "component://order/widget/ordermgr/ReportScreens.xml#OrderPurchaseReportOptions",
            controller = "ordermgr"
        )
        public static final String VIEW_ORDERPURCHASEREPORTOPTIONS = "OrderPurchaseReportOptions";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "OrderPurchaseReportPayment",
            type = "screenfop",
            page = "component://order/widget/ordermgr/ReportScreens.xml#OrderPurchaseReportPayment",
            contentType = "application/pdf",
            encoding = "none",
            controller = "ordermgr"
        )
        public static final String VIEW_ORDERPURCHASEREPORTPAYMENT = "OrderPurchaseReportPayment";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "OrderPurchaseReportProduct",
            type = "screenfop",
            page = "component://order/widget/ordermgr/ReportScreens.xml#OrderPurchaseReportProduct",
            contentType = "application/pdf",
            encoding = "none",
            controller = "ordermgr"
        )
        public static final String VIEW_ORDERPURCHASEREPORTPRODUCT = "OrderPurchaseReportProduct";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "SalesByStoreReport",
            type = "screenfop",
            page = "component://order/widget/ordermgr/ReportScreens.xml#SalesByStoreReport",
            contentType = "application/pdf",
            encoding = "none",
            controller = "ordermgr"
        )
        public static final String VIEW_SALESBYSTOREREPORT = "SalesByStoreReport";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "OpenOrderItemsReport",
            type = "screen",
            page = "component://order/widget/ordermgr/ReportScreens.xml#OpenOrderItemsReport",
            controller = "ordermgr"
        )
        public static final String VIEW_OPENORDERITEMSREPORT = "OpenOrderItemsReport";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "PurchasesByOrganizationReport",
            type = "screenfop",
            page = "component://order/widget/ordermgr/ReportScreens.xml#PurchasesByOrganizationReport",
            contentType = "application/pdf",
            encoding = "none",
            controller = "ordermgr"
        )
        public static final String VIEW_PURCHASESBYORGANIZATIONREPORT = "PurchasesByOrganizationReport";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "FindRequirements",
            type = "screen",
            page = "component://order/widget/ordermgr/RequirementScreens.xml#FindRequirements",
            controller = "ordermgr"
        )
        public static final String VIEW_FINDREQUIREMENTS = "FindRequirements";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "EditRequirement",
            type = "screen",
            page = "component://order/widget/ordermgr/RequirementScreens.xml#EditRequirement",
            controller = "ordermgr"
        )
        public static final String VIEW_EDITREQUIREMENT = "EditRequirement";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "ListRequirementCustRequests",
            type = "screen",
            page = "component://order/widget/ordermgr/RequirementScreens.xml#ListRequirementCustRequests",
            controller = "ordermgr"
        )
        public static final String VIEW_LISTREQUIREMENTCUSTREQUESTS = "ListRequirementCustRequests";

    }

    // Auto-generated split (Part 4)
    public static class Part4 {
        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "ListRequirementOrders",
            type = "screen",
            page = "component://order/widget/ordermgr/RequirementScreens.xml#ListRequirementOrders",
            controller = "ordermgr"
        )
        public static final String VIEW_LISTREQUIREMENTORDERS = "ListRequirementOrders";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "ListRequirementRoles",
            type = "screen",
            page = "component://order/widget/ordermgr/RequirementScreens.xml#ListRequirementRoles",
            controller = "ordermgr"
        )
        public static final String VIEW_LISTREQUIREMENTROLES = "ListRequirementRoles";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "EditRequirementRole",
            type = "screen",
            page = "component://order/widget/ordermgr/RequirementScreens.xml#EditRequirementRole",
            controller = "ordermgr"
        )
        public static final String VIEW_EDITREQUIREMENTROLE = "EditRequirementRole";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "ApproveRequirements",
            type = "screen",
            page = "component://order/widget/ordermgr/RequirementScreens.xml#ApproveRequirements",
            controller = "ordermgr"
        )
        public static final String VIEW_APPROVEREQUIREMENTS = "ApproveRequirements";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "ApprovedProductRequirements",
            type = "screen",
            page = "component://order/widget/ordermgr/RequirementScreens.xml#ApprovedProductRequirements",
            controller = "ordermgr"
        )
        public static final String VIEW_APPROVEDPRODUCTREQUIREMENTS = "ApprovedProductRequirements";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "ApprovedProductRequirementsReport",
            type = "screenfop",
            page = "component://order/widget/ordermgr/RequirementScreens.xml#ApprovedProductRequirementsReport",
            contentType = "application/pdf",
            encoding = "none",
            controller = "ordermgr"
        )
        public static final String VIEW_APPROVEDPRODUCTREQUIREMENTSREPORT = "ApprovedProductRequirementsReport";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "ApprovedProductRequirementsByVendor",
            type = "screen",
            page = "component://order/widget/ordermgr/RequirementScreens.xml#ApprovedProductRequirementsByVendor",
            controller = "ordermgr"
        )
        public static final String VIEW_APPROVEDPRODUCTREQUIREMENTSBYVENDOR = "ApprovedProductRequirementsByVendor";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "ApprovedProductRequirementsByVendorReport",
            type = "screenfop",
            page = "component://order/widget/ordermgr/RequirementScreens.xml#ApprovedProductRequirementsByVendorReport",
            contentType = "application/pdf",
            encoding = "none",
            controller = "ordermgr"
        )
        public static final String VIEW_APPROVEDPRODUCTREQUIREMENTSBYVENDORREPORT = "ApprovedProductRequirementsByVendorReport";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "RequirementsForSupplier",
            type = "screen",
            page = "component://order/widget/ordermgr/OrderEntryScreens.xml#RequirementsForSupplier",
            controller = "ordermgr"
        )
        public static final String VIEW_REQUIREMENTSFORSUPPLIER = "RequirementsForSupplier";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "FindQuoteForCart",
            type = "screen",
            page = "component://order/widget/ordermgr/OrderEntryScreens.xml#FindQuoteForCart",
            controller = "ordermgr"
        )
        public static final String VIEW_FINDQUOTEFORCART = "FindQuoteForCart";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "FindQuote",
            type = "screen",
            page = "component://order/widget/ordermgr/QuoteScreens.xml#FindQuote",
            controller = "ordermgr"
        )
        public static final String VIEW_FINDQUOTE = "FindQuote";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "ViewQuote",
            type = "screen",
            page = "component://order/widget/ordermgr/QuoteScreens.xml#ViewQuote",
            controller = "ordermgr"
        )
        public static final String VIEW_VIEWQUOTE = "ViewQuote";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "QuoteReport",
            type = "screenfop",
            page = "component://order/widget/ordermgr/QuoteScreens.xml#QuoteReport",
            contentType = "application/pdf",
            encoding = "none",
            controller = "ordermgr"
        )
        public static final String VIEW_QUOTEREPORT = "QuoteReport";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "EditQuote",
            type = "screen",
            page = "component://order/widget/ordermgr/QuoteScreens.xml#EditQuote",
            controller = "ordermgr"
        )
        public static final String VIEW_EDITQUOTE = "EditQuote";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "ListQuoteRoles",
            type = "screen",
            page = "component://order/widget/ordermgr/QuoteScreens.xml#ListQuoteRoles",
            controller = "ordermgr"
        )
        public static final String VIEW_LISTQUOTEROLES = "ListQuoteRoles";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "EditQuoteRole",
            type = "screen",
            page = "component://order/widget/ordermgr/QuoteScreens.xml#EditQuoteRole",
            controller = "ordermgr"
        )
        public static final String VIEW_EDITQUOTEROLE = "EditQuoteRole";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "ListQuoteItems",
            type = "screen",
            page = "component://order/widget/ordermgr/QuoteScreens.xml#ListQuoteItems",
            controller = "ordermgr"
        )
        public static final String VIEW_LISTQUOTEITEMS = "ListQuoteItems";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "EditQuoteItem",
            type = "screen",
            page = "component://order/widget/ordermgr/QuoteScreens.xml#EditQuoteItem",
            controller = "ordermgr"
        )
        public static final String VIEW_EDITQUOTEITEM = "EditQuoteItem";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "ListQuoteAttributes",
            type = "screen",
            page = "component://order/widget/ordermgr/QuoteScreens.xml#ListQuoteAttributes",
            controller = "ordermgr"
        )
        public static final String VIEW_LISTQUOTEATTRIBUTES = "ListQuoteAttributes";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "EditQuoteAttribute",
            type = "screen",
            page = "component://order/widget/ordermgr/QuoteScreens.xml#EditQuoteAttribute",
            controller = "ordermgr"
        )
        public static final String VIEW_EDITQUOTEATTRIBUTE = "EditQuoteAttribute";

    }

    // Auto-generated split (Part 5)
    public static class Part5 {
        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "ListQuoteCoefficients",
            type = "screen",
            page = "component://order/widget/ordermgr/QuoteScreens.xml#ListQuoteCoefficients",
            controller = "ordermgr"
        )
        public static final String VIEW_LISTQUOTECOEFFICIENTS = "ListQuoteCoefficients";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "EditQuoteCoefficient",
            type = "screen",
            page = "component://order/widget/ordermgr/QuoteScreens.xml#EditQuoteCoefficient",
            controller = "ordermgr"
        )
        public static final String VIEW_EDITQUOTECOEFFICIENT = "EditQuoteCoefficient";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "ManageQuotePrices",
            type = "screen",
            page = "component://order/widget/ordermgr/QuoteScreens.xml#ManageQuotePrices",
            controller = "ordermgr"
        )
        public static final String VIEW_MANAGEQUOTEPRICES = "ManageQuotePrices";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "ListQuoteAdjustments",
            type = "screen",
            page = "component://order/widget/ordermgr/QuoteScreens.xml#ListQuoteAdjustments",
            controller = "ordermgr"
        )
        public static final String VIEW_LISTQUOTEADJUSTMENTS = "ListQuoteAdjustments";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "EditQuoteAdjustment",
            type = "screen",
            page = "component://order/widget/ordermgr/QuoteScreens.xml#EditQuoteAdjustment",
            controller = "ordermgr"
        )
        public static final String VIEW_EDITQUOTEADJUSTMENT = "EditQuoteAdjustment";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "ViewQuoteProfit",
            type = "screen",
            page = "component://order/widget/ordermgr/QuoteScreens.xml#ViewQuoteProfit",
            controller = "ordermgr"
        )
        public static final String VIEW_VIEWQUOTEPROFIT = "ViewQuoteProfit";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "EditQuoteReportMail",
            type = "screen",
            page = "component://order/widget/ordermgr/QuoteScreens.xml#EditQuoteReportMail",
            controller = "ordermgr"
        )
        public static final String VIEW_EDITQUOTEREPORTMAIL = "EditQuoteReportMail";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "createnewquotenote",
            type = "screen",
            page = "component://order/widget/ordermgr/QuoteScreens.xml#QuoteNewNote",
            controller = "ordermgr"
        )
        public static final String VIEW_CREATENEWQUOTENOTE = "createnewquotenote";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "ListQuoteNotes",
            type = "screen",
            page = "component://order/widget/ordermgr/QuoteScreens.xml#ListQuoteNotes",
            controller = "ordermgr"
        )
        public static final String VIEW_LISTQUOTENOTES = "ListQuoteNotes";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "EditQuoteNote",
            type = "screen",
            page = "component://order/widget/ordermgr/QuoteScreens.xml#EditQuoteNote",
            controller = "ordermgr"
        )
        public static final String VIEW_EDITQUOTENOTE = "EditQuoteNote";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "FindRequest",
            type = "screen",
            page = "component://order/widget/ordermgr/CustRequestScreens.xml#FindRequest",
            controller = "ordermgr"
        )
        public static final String VIEW_FINDREQUEST = "FindRequest";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "ViewRequest",
            type = "screen",
            page = "component://order/widget/ordermgr/CustRequestScreens.xml#ViewRequest",
            controller = "ordermgr"
        )
        public static final String VIEW_VIEWREQUEST = "ViewRequest";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "EditRequest",
            type = "screen",
            page = "component://order/widget/ordermgr/CustRequestScreens.xml#EditRequest",
            controller = "ordermgr"
        )
        public static final String VIEW_EDITREQUEST = "EditRequest";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "EditRequestCustomer",
            type = "screen",
            page = "component://order/widget/ordermgr/CustRequestScreens.xml#EditRequestCustomer",
            controller = "ordermgr"
        )
        public static final String VIEW_EDITREQUESTCUSTOMER = "EditRequestCustomer";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "EditRequestItem",
            type = "screen",
            page = "component://order/widget/ordermgr/CustRequestScreens.xml#EditRequestItem",
            controller = "ordermgr"
        )
        public static final String VIEW_EDITREQUESTITEM = "EditRequestItem";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "RequestItems",
            type = "screen",
            page = "component://order/widget/ordermgr/CustRequestScreens.xml#RequestItems",
            controller = "ordermgr"
        )
        public static final String VIEW_REQUESTITEMS = "RequestItems";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "RequestRoles",
            type = "screen",
            page = "component://order/widget/ordermgr/CustRequestScreens.xml#RequestRoles",
            controller = "ordermgr"
        )
        public static final String VIEW_REQUESTROLES = "RequestRoles";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "RequestItemNotes",
            type = "screen",
            page = "component://order/widget/ordermgr/CustRequestScreens.xml#RequestItemNotes",
            controller = "ordermgr"
        )
        public static final String VIEW_REQUESTITEMNOTES = "RequestItemNotes";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "RequestItemQuotes",
            type = "screen",
            page = "component://order/widget/ordermgr/CustRequestScreens.xml#RequestItemQuotes",
            controller = "ordermgr"
        )
        public static final String VIEW_REQUESTITEMQUOTES = "RequestItemQuotes";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "RequestItemRequirements",
            type = "screen",
            page = "component://order/widget/ordermgr/CustRequestScreens.xml#RequestItemRequirements",
            controller = "ordermgr"
        )
        public static final String VIEW_REQUESTITEMREQUIREMENTS = "RequestItemRequirements";

    }

    // Auto-generated split (Part 6)
    public static class Part6 {
        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "EditRequestItemWorkEfforts",
            type = "screen",
            page = "component://order/widget/ordermgr/CustRequestScreens.xml#EditRequestItemWorkEfforts",
            controller = "ordermgr"
        )
        public static final String VIEW_EDITREQUESTITEMWORKEFFORTS = "EditRequestItemWorkEfforts";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "CreateQuoteAndQuoteItemForRequest",
            type = "screen",
            page = "component://order/widget/ordermgr/CustRequestScreens.xml#CreateQuoteAndQuoteItemForRequest",
            controller = "ordermgr"
        )
        public static final String VIEW_CREATEQUOTEANDQUOTEITEMFORREQUEST = "CreateQuoteAndQuoteItemForRequest";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "EditQuoteItemForRequest",
            type = "screen",
            page = "component://order/widget/ordermgr/CustRequestScreens.xml#EditQuoteItemForRequest",
            controller = "ordermgr"
        )
        public static final String VIEW_EDITQUOTEITEMFORREQUEST = "EditQuoteItemForRequest";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "EditCustRequestContent",
            type = "screen",
            page = "component://order/widget/ordermgr/CustRequestScreens.xml#EditCustRequestContent",
            controller = "ordermgr"
        )
        public static final String VIEW_EDITCUSTREQUESTCONTENT = "EditCustRequestContent";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "AddQuoteWorkEffort",
            type = "screen",
            page = "component://order/widget/ordermgr/QuoteWorkEffortScreens.xml#AddQuoteWorkEffort",
            controller = "ordermgr"
        )
        public static final String VIEW_ADDQUOTEWORKEFFORT = "AddQuoteWorkEffort";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "EditQuoteWorkEffort",
            type = "screen",
            page = "component://order/widget/ordermgr/QuoteWorkEffortScreens.xml#EditQuoteWorkEffort",
            controller = "ordermgr"
        )
        public static final String VIEW_EDITQUOTEWORKEFFORT = "EditQuoteWorkEffort";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "ListQuoteWorkEfforts",
            type = "screen",
            page = "component://order/widget/ordermgr/QuoteWorkEffortScreens.xml#ListQuoteWorkEfforts",
            controller = "ordermgr"
        )
        public static final String VIEW_LISTQUOTEWORKEFFORTS = "ListQuoteWorkEfforts";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "EditOrderHeader",
            type = "screen",
            page = "component://order/widget/ordermgr/OrderHeaderScreens.xml#EditOrderHeader",
            controller = "ordermgr"
        )
        public static final String VIEW_EDITORDERHEADER = "EditOrderHeader";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "ListOrderHeaders",
            type = "screen",
            page = "component://order/widget/ordermgr/OrderHeaderScreens.xml#ListOrderHeaders",
            controller = "ordermgr"
        )
        public static final String VIEW_LISTORDERHEADERS = "ListOrderHeaders";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "LookupPerson",
            type = "screen",
            page = "component://party/widget/partymgr/LookupScreens.xml#LookupPerson",
            controller = "ordermgr"
        )
        public static final String VIEW_LOOKUPPERSON = "LookupPerson";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "LookupPartyGroup",
            type = "screen",
            page = "component://party/widget/partymgr/LookupScreens.xml#LookupPartyGroup",
            controller = "ordermgr"
        )
        public static final String VIEW_LOOKUPPARTYGROUP = "LookupPartyGroup";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "LookupPartyName",
            type = "screen",
            page = "component://party/widget/partymgr/LookupScreens.xml#LookupPartyName",
            controller = "ordermgr"
        )
        public static final String VIEW_LOOKUPPARTYNAME = "LookupPartyName";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "LookupCustomerName",
            type = "screen",
            page = "component://party/widget/partymgr/LookupScreens.xml#LookupCustomerName",
            controller = "ordermgr"
        )
        public static final String VIEW_LOOKUPCUSTOMERNAME = "LookupCustomerName";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "LookupProduct",
            type = "screen",
            page = "component://product/widget/catalog/LookupScreens.xml#LookupProduct",
            controller = "ordermgr"
        )
        public static final String VIEW_LOOKUPPRODUCT = "LookupProduct";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "LookupSupplierProduct",
            type = "screen",
            page = "component://product/widget/catalog/LookupScreens.xml#LookupSupplierProduct",
            controller = "ordermgr"
        )
        public static final String VIEW_LOOKUPSUPPLIERPRODUCT = "LookupSupplierProduct";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "LookupBulkAddSupplierProductsInApprovedOrder",
            type = "screen",
            page = "component://order/widget/ordermgr/OrderEntryOrderScreens.xml#LookupBulkAddSupplierProductsInApprovedOrder",
            controller = "ordermgr"
        )
        public static final String VIEW_LOOKUPBULKADDSUPPLIERPRODUCTSINAPPROVEDORDER = "LookupBulkAddSupplierProductsInApprovedOrder";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "LookupProductAndPrice",
            type = "screen",
            page = "component://product/widget/catalog/LookupScreens.xml#LookupProductAndPrice",
            controller = "ordermgr"
        )
        public static final String VIEW_LOOKUPPRODUCTANDPRICE = "LookupProductAndPrice";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "LookupProductFeature",
            type = "screen",
            page = "component://product/widget/catalog/LookupScreens.xml#LookupProductFeature",
            controller = "ordermgr"
        )
        public static final String VIEW_LOOKUPPRODUCTFEATURE = "LookupProductFeature";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "LookupUserLoginAndPartyDetails",
            type = "screen",
            page = "component://party/widget/partymgr/LookupScreens.xml#LookupUserLoginAndPartyDetails",
            controller = "ordermgr"
        )
        public static final String VIEW_LOOKUPUSERLOGINANDPARTYDETAILS = "LookupUserLoginAndPartyDetails";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "LookupPreferredContactMech",
            type = "screen",
            page = "component://marketing/widget/ContactListScreens.xml#LookupPreferredContactMech",
            controller = "ordermgr"
        )
        public static final String VIEW_LOOKUPPREFERREDCONTACTMECH = "LookupPreferredContactMech";

    }

    // Auto-generated split (Part 7)
    public static class Part7 {
        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "LookupVariantProduct",
            type = "screen",
            page = "component://product/widget/catalog/LookupScreens.xml#LookupVariantProduct",
            controller = "ordermgr"
        )
        public static final String VIEW_LOOKUPVARIANTPRODUCT = "LookupVariantProduct";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "LookupFacility",
            type = "screen",
            page = "component://product/widget/facility/LookupScreens.xml#LookupFacility",
            controller = "ordermgr"
        )
        public static final String VIEW_LOOKUPFACILITY = "LookupFacility";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "LookupFixedAsset",
            type = "screen",
            page = "component://accounting/widget/LookupScreens.xml#LookupFixedAsset",
            controller = "ordermgr"
        )
        public static final String VIEW_LOOKUPFIXEDASSET = "LookupFixedAsset";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "LookupShoppingList",
            type = "screen",
            page = "component://order/widget/ordermgr/LookupScreens.xml#LookupShoppingList",
            controller = "ordermgr"
        )
        public static final String VIEW_LOOKUPSHOPPINGLIST = "LookupShoppingList";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "LookupCustRequest",
            type = "screen",
            page = "component://order/widget/ordermgr/LookupScreens.xml#LookupCustRequest",
            controller = "ordermgr"
        )
        public static final String VIEW_LOOKUPCUSTREQUEST = "LookupCustRequest";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "LookupCustRequestItem",
            type = "screen",
            page = "component://order/widget/ordermgr/LookupScreens.xml#LookupCustRequestItem",
            controller = "ordermgr"
        )
        public static final String VIEW_LOOKUPCUSTREQUESTITEM = "LookupCustRequestItem";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "LookupRequirement",
            type = "screen",
            page = "component://order/widget/ordermgr/LookupScreens.xml#LookupRequirement",
            controller = "ordermgr"
        )
        public static final String VIEW_LOOKUPREQUIREMENT = "LookupRequirement";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "LookupQuote",
            type = "screen",
            page = "component://order/widget/ordermgr/LookupScreens.xml#LookupQuote",
            controller = "ordermgr"
        )
        public static final String VIEW_LOOKUPQUOTE = "LookupQuote";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "LookupQuoteItem",
            type = "screen",
            page = "component://order/widget/ordermgr/LookupScreens.xml#LookupQuoteItem",
            controller = "ordermgr"
        )
        public static final String VIEW_LOOKUPQUOTEITEM = "LookupQuoteItem";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "LookupOrderHeader",
            type = "screen",
            page = "component://order/widget/ordermgr/LookupScreens.xml#LookupOrderHeader",
            controller = "ordermgr"
        )
        public static final String VIEW_LOOKUPORDERHEADER = "LookupOrderHeader";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "LookupWorkEffort",
            type = "screen",
            page = "component://workeffort/widget/LookupScreens.xml#LookupWorkEffort",
            controller = "ordermgr"
        )
        public static final String VIEW_LOOKUPWORKEFFORT = "LookupWorkEffort";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "LookupContent",
            type = "screen",
            page = "component://content/widget/content/ContentScreens.xml#LookupContent",
            controller = "ordermgr"
        )
        public static final String VIEW_LOOKUPCONTENT = "LookupContent";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "LookupAssociatedProducts",
            type = "screen",
            page = "component://order/widget/ordermgr/OrderEntryCartScreens.xml#LookupAssociatedProducts",
            controller = "ordermgr"
        )
        public static final String VIEW_LOOKUPASSOCIATEDPRODUCTS = "LookupAssociatedProducts";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "OrderPDF",
            type = "screenfop",
            page = "component://order/widget/ordermgr/OrderPrintScreens.xml#OrderPDF",
            contentType = "application/pdf",
            encoding = "none",
            controller = "ordermgr"
        )
        public static final String VIEW_ORDERPDF = "OrderPDF";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "ReturnPDF",
            type = "screenfop",
            page = "component://order/widget/ordermgr/OrderPrintScreens.xml#ReturnPDF",
            contentType = "application/pdf",
            encoding = "none",
            controller = "ordermgr"
        )
        public static final String VIEW_RETURNPDF = "ReturnPDF";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "ShipGroupsPDF",
            type = "screenfop",
            page = "component://order/widget/ordermgr/OrderPrintScreens.xml#ShipGroupsPDF",
            contentType = "application/pdf",
            encoding = "none",
            controller = "ordermgr"
        )
        public static final String VIEW_SHIPGROUPSPDF = "ShipGroupsPDF";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "OrderPickSheetPDF",
            type = "screenfop",
            page = "component://product/widget/facility/FacilityScreens.xml#PrintPickSheets.fo",
            contentType = "application/pdf",
            encoding = "none",
            controller = "ordermgr"
        )
        public static final String VIEW_ORDERPICKSHEETPDF = "OrderPickSheetPDF";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "SendConfirmationMail",
            type = "screen",
            page = "component://order/widget/ordermgr/OrderViewScreens.xml#SendOrderConfirmation",
            controller = "ordermgr"
        )
        public static final String VIEW_SENDCONFIRMATIONMAIL = "SendConfirmationMail";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "SendCompletionMail",
            type = "screen",
            page = "component://order/widget/ordermgr/OrderViewScreens.xml#SendOrderCompletion",
            controller = "ordermgr"
        )
        public static final String VIEW_SENDCOMPLETIONMAIL = "SendCompletionMail";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "ReturnHistory",
            type = "screen",
            page = "component://order/widget/ordermgr/OrderReturnScreens.xml#OrderReturnHistory",
            controller = "ordermgr"
        )
        public static final String VIEW_RETURNHISTORY = "ReturnHistory";

    }

    // Auto-generated split (Part 8)
    public static class Part8 {
        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "ProductUomDropDownOnly",
            type = "screen",
            page = "component://order/widget/ordermgr/OrderEntryCatalogScreens.xml#ProductUomDropDownOnly",
            controller = "ordermgr"
        )
        public static final String VIEW_PRODUCTUOMDROPDOWNONLY = "ProductUomDropDownOnly";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "EditQuoteTerm",
            type = "screen",
            page = "component://order/widget/ordermgr/QuoteScreens.xml#EditQuoteTerm",
            controller = "ordermgr"
        )
        public static final String VIEW_EDITQUOTETERM = "EditQuoteTerm";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "EditQuoteTermItem",
            type = "screen",
            page = "component://order/widget/ordermgr/QuoteScreens.xml#EditQuoteTermItem",
            controller = "ordermgr"
        )
        public static final String VIEW_EDITQUOTETERMITEM = "EditQuoteTermItem";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "ListQuoteTerms",
            type = "screen",
            page = "component://order/widget/ordermgr/QuoteScreens.xml#ListQuoteTerms",
            controller = "ordermgr"
        )
        public static final String VIEW_LISTQUOTETERMS = "ListQuoteTerms";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "splitship",
            type = "screen",
            page = "component://order/widget/ordermgr/OrderEntryOrderScreens.xml#splitship",
            controller = "ordermgr"
        )
        public static final String VIEW_SPLITSHIP = "splitship";

        @Request(
            uri = "view",
            controller = "ordermgr",
            secure = "true"
        )
        @Response(name = "success", type = "request", value = "main")
        public interface ViewDef {}

        @Request(
            uri = "main",
            controller = "ordermgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "main")
        public interface Main {}

        @Request(
            uri = "orderstats",
            controller = "ordermgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "orderstats")
        public interface Orderstats {}

        @Request(
            uri = "orderview",
            controller = "ordermgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "orderview")
        public interface Orderview {}

        @Request(
            uri = "orderShipping",
            controller = "ordermgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "orderShipping")
        public interface OrderShipping {}

        @Request(
            uri = "findorders",
            controller = "ordermgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "findorders")
        public interface Findorders {}

        @Request(
            uri = "searchorders",
            controller = "ordermgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "findorders")
        @Response(name = "error", type = "view", value = "findorders")
        public interface Searchorders {}

        @Request(
            uri = "orderlist",
            controller = "ordermgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "orderlist")
        public interface Orderlist {}

        @Request(
            uri = "confirmationmailedit",
            controller = "ordermgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "SendConfirmationMail")
        public interface Confirmationmailedit {}

        @Request(
            uri = "completionmailedit",
            controller = "ordermgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "SendCompletionMail")
        public interface Completionmailedit {}

        @Request(
            uri = "sendconfirmationmail",
            controller = "ordermgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "orderview")
        @Response(name = "error", type = "view", value = "SendConfirmationMail")
        @Event(type = "service", invoke = "sendMail")
        public static String sendconfirmationmail(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "OrderHistory",
            controller = "ordermgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "OrderHistory")
        public interface OrderHistory {}

        @Request(
            uri = "massApproveOrders",
            controller = "ordermgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "findorders")
        @Response(name = "error", type = "view", value = "findorders")
        @Event(type = "service", invoke = "massChangeOrderApproved")
        public static String massApproveOrders(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "massProcessOrders",
            controller = "ordermgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "findorders")
        @Response(name = "error", type = "view", value = "findorders")
        @Event(type = "service", invoke = "massProcessOrders")
        public static String massProcessOrders(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "massHoldOrders",
            controller = "ordermgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "findorders")
        @Response(name = "error", type = "view", value = "findorders")
        @Event(type = "service", invoke = "massHoldOrders")
        public static String massHoldOrders(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

    }

    // Auto-generated split (Part 9)
    public static class Part9 {
        @Request(
            uri = "massCancelOrders",
            controller = "ordermgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "findorders")
        @Response(name = "error", type = "view", value = "findorders")
        @Event(type = "service", invoke = "massCancelOrders")
        public static String massCancelOrders(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "massCancelRemainingPurchaseOrderItems",
            controller = "ordermgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "findorders")
        @Response(name = "error", type = "view", value = "findorders")
        @Event(type = "service", invoke = "massCancelRemainingPurchaseOrderItems")
        public static String massCancelRemainingPurchaseOrderItems(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "massRejectOrders",
            controller = "ordermgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "findorders")
        @Response(name = "error", type = "view", value = "findorders")
        @Event(type = "service", invoke = "massRejectOrders")
        public static String massRejectOrders(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "massQuickShipOrders",
            controller = "ordermgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "findorders")
        @Response(name = "error", type = "view", value = "findorders")
        @Event(type = "service", invoke = "massQuickShipOrders")
        public static String massQuickShipOrders(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "massPickOrders",
            controller = "ordermgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "findorders")
        @Response(name = "error", type = "view", value = "findorders")
        @Event(type = "service", invoke = "massPickOrders")
        public static String massPickOrders(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "massPrintOrders",
            controller = "ordermgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "findorders")
        @Response(name = "error", type = "view", value = "findorders")
        @Event(type = "service", invoke = "massPrintOrders")
        public static String massPrintOrders(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "massCreateFileForOrders",
            controller = "ordermgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "findorders")
        @Response(name = "error", type = "view", value = "findorders")
        @Event(type = "service", invoke = "massCreateFileForOrders")
        public static String massCreateFileForOrders(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "OrderDeliveryScheduleInfo",
            controller = "ordermgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "OrderDeliveryScheduleInfo")
        public interface OrderDeliveryScheduleInfo {}

        @Request(
            uri = "createOrderDeliverySchedule",
            controller = "ordermgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "orderview")
        @Response(name = "error", type = "view", value = "orderview")
        @Event(type = "service", invoke = "createOrderDeliverySchedule")
        public static String createOrderDeliverySchedule(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "updateOrderDeliverySchedule",
            controller = "ordermgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "orderview")
        @Response(name = "error", type = "view", value = "orderview")
        @Event(type = "service", invoke = "updateOrderDeliverySchedule")
        public static String updateOrderDeliverySchedule(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "changeOrderStatus",
            controller = "ordermgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "orderview")
        @Response(name = "error", type = "view", value = "orderview")
        @Event(type = "service", invoke = "changeOrderStatus")
        public static String changeOrderStatus(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "changeOrderItemStatus",
            controller = "ordermgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "orderview")
        @Response(name = "error", type = "view", value = "orderview")
        @Event(type = "service", invoke = "changeOrderItemStatus")
        public static String changeOrderItemStatus(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "cancelOrderItem",
            controller = "ordermgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "editorderitems")
        @Response(name = "error", type = "view", value = "editorderitems")
        @Event(type = "service", invoke = "cancelOrderItem")
        public static String cancelOrderItem(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "cancelSelectedOrderItems",
            controller = "ordermgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "editorderitems")
        @Response(name = "error", type = "view", value = "editorderitems")
        public static String cancelSelectedOrderItems(HttpServletRequest request, HttpServletResponse response) throws Exception {
            // Delegates to: org.ofbiz.order.order.OrderEvents.cancelSelectedOrderItems
            return OrderEvents.cancelSelectedOrderItems(request, response);
        }

        @Request(
            uri = "createOrderAdjustment",
            controller = "ordermgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "request", value = "recalcTax")
        @Response(name = "error", type = "view", value = "editorderitems")
        @Event(type = "service", invoke = "createOrderAdjustment")
        public static String createOrderAdjustment(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "updateOrderAdjustment",
            controller = "ordermgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "request", value = "recalcTax")
        @Response(name = "error", type = "view", value = "editorderitems")
        @Event(type = "service", invoke = "updateOrderAdjustment")
        public static String updateOrderAdjustment(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "deleteOrderAdjustment",
            controller = "ordermgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "request", value = "recalcTax")
        @Response(name = "error", type = "view", value = "editorderitems")
        @Event(type = "service", invoke = "deleteOrderAdjustment")
        public static String deleteOrderAdjustment(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "recalcTax",
            controller = "ordermgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "editorderitems")
        @Response(name = "error", type = "view", value = "editorderitems")
        @Event(type = "service", invoke = "recalcTaxTotal")
        public static String recalcTax(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "addpromocode",
            controller = "ordermgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "showcart")
        @Response(name = "error", type = "view", value = "showcart")
        public static String addpromocode(HttpServletRequest request, HttpServletResponse response) throws Exception {
            // Delegates to: org.ofbiz.order.shoppingcart.ShoppingCartEvents.addProductPromoCode
            return ShoppingCartEvents.addProductPromoCode(request, response);
        }

        @Request(
            uri = "getConfigDetailsEvent",
            controller = "ordermgr",
            secure = "true"
        )
        @Response(name = "success", type = "request", value = "json")
        @Response(name = "error", type = "request", value = "json")
        public static String getConfigDetailsEvent(HttpServletRequest request, HttpServletResponse response) throws Exception {
            // Delegates to: org.ofbiz.order.shoppingcart.ShoppingCartEvents.getConfigDetailsEvent
            return ShoppingCartEvents.getConfigDetailsEvent(request, response);
        }

    }

    // Auto-generated split (Part 10)
    public static class Part10 {
        @Request(
            uri = "updateTrackingNumber",
            controller = "ordermgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "orderview")
        @Response(name = "error", type = "view", value = "orderview")
        @Event(type = "service", invoke = "updateTrackingNumber")
        public static String updateTrackingNumber(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "receivepayment",
            controller = "ordermgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "receivepayment")
        public interface Receivepayment {}

        @Request(
            uri = "receiveOfflinePayments",
            controller = "ordermgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "orderview")
        @Response(name = "error", type = "view", value = "receivepayment")
        public static String receiveOfflinePayments(HttpServletRequest request, HttpServletResponse response) throws Exception {
            // Delegates to: org.ofbiz.order.OrderManagerEvents.receiveOfflinePayment
            return OrderManagerEvents.receiveOfflinePayment(request, response);
        }

        @Request(
            uri = "allowordersplit",
            controller = "ordermgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "orderview")
        @Response(name = "error", type = "view", value = "orderview")
        @Event(type = "service", invoke = "setAllowOrderSplit")
        public static String allowordersplit(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "quickShipOrder",
            controller = "ordermgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "request-redirect", value = "orderview")
        @Response(name = "error", type = "view", value = "orderview")
        @Event(type = "service", invoke = "quickShipEntireOrder")
        public static String quickShipOrder(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "orderSendShip",
            controller = "ordermgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "request-redirect", value = "orderview")
        @Response(name = "error", type = "view", value = "orderview")
        @Event(type = "service", invoke = "orderSendShip")
        public static String orderSendShip(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "orderCompleteShip",
            controller = "ordermgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "request-redirect", value = "orderview")
        @Response(name = "error", type = "view", value = "orderview")
        @Event(type = "service", invoke = "orderCompleteShip")
        public static String orderCompleteShip(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "createQuoteTerm",
            controller = "ordermgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "request-redirect", value = "ListQuoteTerms")
        @Response(name = "error", type = "request-redirect", value = "EditQuoteTerm")
        @Event(type = "service", invoke = "createQuoteTerm")
        public static String createQuoteTerm(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "createQuoteTermFromItem",
            controller = "ordermgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "request-redirect", value = "EditQuoteItem")
        @Response(name = "error", type = "request-redirect", value = "EditQuoteTermItem")
        @Event(type = "service", invoke = "createQuoteTerm")
        public static String createQuoteTermFromItem(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "updateQuoteTermFromItem",
            controller = "ordermgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "request-redirect", value = "EditQuoteItem")
        @Response(name = "error", type = "request-redirect", value = "EditQuoteTermItem")
        @Event(type = "service", invoke = "updateQuoteTerm")
        public static String updateQuoteTermFromItem(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "updateQuoteTerm",
            controller = "ordermgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "request-redirect", value = "ListQuoteTerms")
        @Response(name = "error", type = "request-redirect", value = "EditQuoteTerm")
        @Event(type = "service", invoke = "updateQuoteTerm")
        public static String updateQuoteTerm(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "deleteQuoteTermFromItem",
            controller = "ordermgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "request-redirect", value = "EditQuoteItem")
        @Response(name = "error", type = "view", value = "EditQuoteItem")
        @Event(type = "service", invoke = "deleteQuoteTerm")
        public static String deleteQuoteTermFromItem(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "deleteQuoteTerm",
            controller = "ordermgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "request-redirect", value = "ListQuoteTerms")
        @Response(name = "error", type = "view", value = "ListQuoteTerms")
        @Event(type = "service", invoke = "deleteQuoteTerm")
        public static String deleteQuoteTerm(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "quickDropShipOrder",
            controller = "ordermgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "request-redirect", value = "orderview")
        @Response(name = "error", type = "view", value = "orderview")
        @Event(type = "service", invoke = "quickDropShipOrder")
        public static String quickDropShipOrder(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "completePurchaseOrder",
            controller = "ordermgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "orderview")
        @Response(name = "error", type = "view", value = "orderview")
        @Event(type = "service", invoke = "completePurchaseOrder")
        public static String completePurchaseOrder(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "completeSalesOrder",
            controller = "ordermgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "orderview")
        @Response(name = "error", type = "view", value = "orderview")
        @Event(type = "service", invoke = "orderCompleteShip")
        public static String completeSalesOrder(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "balanceInventoryItems",
            controller = "ordermgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "orderview")
        @Response(name = "error", type = "view", value = "orderview")
        @Event(type = "service", invoke = "balanceInventoryItems")
        public static String balanceInventoryItems(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "editOrderItems",
            controller = "ordermgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "editorderitems")
        public interface EditOrderItems {}

        @Request(
            uri = "updateOrderItems",
            controller = "ordermgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "editorderitems")
        @Response(name = "error", type = "view", value = "editorderitems")
        @Event(type = "service", invoke = "updateOrderItems")
        public static String updateOrderItems(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "appendItemToOrder",
            controller = "ordermgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "orderview")
        @Response(name = "error", type = "view", value = "editorderitems")
        @Event(type = "service", invoke = "appendOrderItem")
        public static String appendItemToOrder(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

    }

    // Auto-generated split (Part 11)
    public static class Part11 {
        @Request(
            uri = "viewimage",
            controller = "ordermgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "viewimage")
        public interface Viewimage {}

        @Request(
            uri = "setShippingInstructions",
            controller = "ordermgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "orderview")
        @Response(name = "error", type = "view", value = "orderview")
        @Event(type = "service", invoke = "setShippingInstructions")
        public static String setShippingInstructions(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "setGiftMessage",
            controller = "ordermgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "orderview")
        @Response(name = "error", type = "view", value = "orderview")
        @Event(type = "service", invoke = "setGiftMessage")
        public static String setGiftMessage(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "createnewnote",
            controller = "ordermgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "createnewnote")
        public interface Createnewnote {}

        @Request(
            uri = "createordernote",
            controller = "ordermgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "orderview")
        @Response(name = "error", type = "view", value = "createnewnote")
        @Event(type = "service", invoke = "createOrderNote")
        public static String createordernote(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "ListOrderTerms",
            controller = "ordermgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ListOrderTerms")
        public interface ListOrderTerms {}

        @Request(
            uri = "createOrderTerm",
            controller = "ordermgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ListOrderTerms")
        @Response(name = "error", type = "view", value = "ListOrderTerms")
        @Event(type = "service", invoke = "createOrderTerm")
        public static String createOrderTerm(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "updateOrderTerm",
            controller = "ordermgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ListOrderTerms")
        @Response(name = "error", type = "view", value = "ListOrderTerms")
        @Event(type = "service", invoke = "updateOrderTerm")
        public static String updateOrderTerm(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "removeOrderTerm",
            controller = "ordermgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ListOrderTerms")
        @Response(name = "error", type = "view", value = "ListOrderTerms")
        @Event(type = "service", invoke = "removeOrderTerm")
        public static String removeOrderTerm(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "updateOrderNote",
            controller = "ordermgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "request-redirect", value = "orderview")
        @Response(name = "error", type = "view", value = "orderview")
        @Event(type = "service", invoke = "updateOrderNote")
        public static String updateOrderNote(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "orderentry",
            controller = "ordermgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "init", type = "view", value = "checkinits")
        @Response(name = "agreements", type = "view", value = "orderagreements")
        @Response(name = "cart", type = "view", value = "showcart")
        @Response(name = "error", type = "view", value = "checkinits")
        public static String orderentry(HttpServletRequest request, HttpServletResponse response) throws Exception {
            // Delegates to: org.ofbiz.order.shoppingcart.ShoppingCartEvents.routeOrderEntry
            return ShoppingCartEvents.routeOrderEntry(request, response);
        }

        @Request(
            uri = "initorderentry",
            controller = "ordermgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "request", value = "orderentry")
        @Response(name = "error", type = "view", value = "checkinits")
        public static String initorderentry(HttpServletRequest request, HttpServletResponse response) throws Exception {
            // Delegates to: org.ofbiz.order.shoppingcart.ShoppingCartEvents.initializeOrderEntry
            return ShoppingCartEvents.initializeOrderEntry(request, response);
        }

        @Request(
            uri = "checkinits",
            controller = "ordermgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "checkinits")
        public interface Checkinits {}

        @Request(
            uri = "orderagreements",
            controller = "ordermgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "orderagreements")
        public interface Orderagreements {}

        @Request(
            uri = "setOrderCurrencyAgreementShipDates",
            controller = "ordermgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "request", value = "orderentry")
        @Response(name = "error", type = "request", value = "orderagreements")
        public static String setOrderCurrencyAgreementShipDates(HttpServletRequest request, HttpServletResponse response) throws Exception {
            // Delegates to: org.ofbiz.order.shoppingcart.ShoppingCartEvents.setOrderCurrencyAgreementShipDatesForOrderEntry
            return ShoppingCartEvents.setOrderCurrencyAgreementShipDatesForOrderEntry(request, response);
        }

        @Request(
            uri = "setOrderAgreement",
            controller = "ordermgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "request", value = "orderentry")
        @Response(name = "error", type = "request", value = "orderagreements")
        public static String setOrderAgreement(HttpServletRequest request, HttpServletResponse response) throws Exception {
            // Delegates to: org.ofbiz.order.shoppingcart.ShoppingCartEvents.selectAgreement
            return ShoppingCartEvents.selectAgreement(request, response);
        }

        @Request(
            uri = "setOrderCurrency",
            controller = "ordermgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "request", value = "orderentry")
        @Response(name = "error", type = "request", value = "orderagreements")
        public static String setOrderCurrency(HttpServletRequest request, HttpServletResponse response) throws Exception {
            // Delegates to: org.ofbiz.order.shoppingcart.ShoppingCartEvents.setCurrency
            return ShoppingCartEvents.setCurrency(request, response);
        }

        @Request(
            uri = "setOrderName",
            controller = "ordermgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "request", value = "orderentry")
        @Response(name = "error", type = "request", value = "orderentry")
        public static String setOrderName(HttpServletRequest request, HttpServletResponse response) throws Exception {
            // Delegates to: org.ofbiz.order.shoppingcart.ShoppingCartEvents.setOrderName
            return ShoppingCartEvents.setOrderName(request, response);
        }

        @Request(
            uri = "setPoNumber",
            controller = "ordermgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "request", value = "orderentry")
        @Response(name = "error", type = "request", value = "orderentry")
        public static String setPoNumber(HttpServletRequest request, HttpServletResponse response) throws Exception {
            // Delegates to: org.ofbiz.order.shoppingcart.ShoppingCartEvents.setPoNumber
            return ShoppingCartEvents.setPoNumber(request, response);
        }

        @Request(
            uri = "additem",
            controller = "ordermgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "request", value = "orderentry")
        @Response(name = "survey", type = "view", value = "survey", allowViewSave = "false")
        @Response(name = "product", type = "view", value = "product")
        @Response(name = "viewcart", type = "request-redirect", value = "orderentry")
        @Response(name = "error", type = "request", value = "orderentry")
        public static String additem(HttpServletRequest request, HttpServletResponse response) throws Exception {
            // Delegates to: org.ofbiz.order.shoppingcart.ShoppingCartEvents.addToCart
            return ShoppingCartEvents.addToCart(request, response);
        }

    }

    // Auto-generated split (Part 12)
    public static class Part12 {
        @Request(
            uri = "additemsurvey",
            controller = "ordermgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "request", value = "additem")
        @Response(name = "error", type = "request", value = "orderentry")
        public static String additemsurvey(HttpServletRequest request, HttpServletResponse response) throws Exception {
            // Delegates to: org.ofbiz.content.survey.SurveyEvents.createSurveyResponseAndRestoreParameters
            return SurveyEvents.createSurveyResponseAndRestoreParameters(request, response);
        }

        @Request(
            uri = "addRequirementsToCart",
            controller = "ordermgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "request", value = "orderentry")
        @Response(name = "error", type = "request", value = "orderentry")
        public static String addRequirementsToCart(HttpServletRequest request, HttpServletResponse response) throws Exception {
            // Delegates to: org.ofbiz.order.shoppingcart.ShoppingCartEvents.addToCartBulkRequirements
            return ShoppingCartEvents.addToCartBulkRequirements(request, response);
        }

        @Request(
            uri = "quickAddRequirementsToCart",
            controller = "ordermgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "request", value = "quickCheckoutOrderWithDefaultOptions")
        @Response(name = "error", type = "request", value = "orderentry")
        public static String quickAddRequirementsToCart(HttpServletRequest request, HttpServletResponse response) throws Exception {
            // Delegates to: org.ofbiz.order.shoppingcart.ShoppingCartEvents.addToCartBulkRequirements
            return ShoppingCartEvents.addToCartBulkRequirements(request, response);
        }

        @Request(
            uri = "FindQuoteForCart",
            controller = "ordermgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "FindQuoteForCart")
        public interface FindQuoteForCart {}

        @Request(
            uri = "createQuoteFromCart",
            controller = "ordermgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "request", value = "ViewQuote")
        @Response(name = "error", type = "request", value = "orderentry")
        public static String createQuoteFromCart(HttpServletRequest request, HttpServletResponse response) throws Exception {
            // Delegates to: org.ofbiz.order.shoppingcart.ShoppingCartEvents.createQuoteFromCart
            return ShoppingCartEvents.createQuoteFromCart(request, response);
        }

        @Request(
            uri = "createCustRequestFromCart",
            controller = "ordermgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "request", value = "ViewRequest")
        @Response(name = "error", type = "request", value = "orderentry")
        public static String createCustRequestFromCart(HttpServletRequest request, HttpServletResponse response) throws Exception {
            // Delegates to: org.ofbiz.order.shoppingcart.ShoppingCartEvents.createCustRequestFromCart
            return ShoppingCartEvents.createCustRequestFromCart(request, response);
        }

        @Request(
            uri = "createQuoteFromShoppingList",
            controller = "ordermgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "request", value = "ViewQuote")
        @Response(name = "error", type = "request", value = "orderentry")
        @Event(type = "service", invoke = "createQuoteFromShoppingList")
        public static String createQuoteFromShoppingList(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "createQuoteFromCustRequest",
            controller = "ordermgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "request", value = "ViewQuote")
        @Response(name = "error", type = "request", value = "request")
        @Event(type = "service", invoke = "createQuoteFromCustRequest")
        public static String createQuoteFromCustRequest(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "createCustRequestFromShoppingList",
            controller = "ordermgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "request", value = "ViewRequest")
        @Response(name = "error", type = "request", value = "orderentry")
        @Event(type = "service", invoke = "createCustRequestFromShoppingList")
        public static String createCustRequestFromShoppingList(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "viewPartyShoppingLists",
            controller = "ordermgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "viewshoppinglists")
        public interface ViewPartyShoppingLists {}

        @Request(
            uri = "addFromShoppingList",
            controller = "ordermgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "addfromshoppinglist")
        public interface AddFromShoppingList {}

        @Request(
            uri = "addAllFromShoppingList",
            controller = "ordermgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "request", value = "orderentry")
        @Response(name = "error", type = "view", value = "checkinits")
        public static String addAllFromShoppingList(HttpServletRequest request, HttpServletResponse response) throws Exception {
            // Delegates to: org.ofbiz.order.shoppinglist.ShoppingListEvents.addListToCart
            return ShoppingListEvents.addListToCart(request, response);
        }

        @Request(
            uri = "addBulkToShoppingList",
            controller = "ordermgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "request", value = "addFromShoppingList")
        @Response(name = "error", type = "view", value = "checkinits")
        public static String addBulkToShoppingList(HttpServletRequest request, HttpServletResponse response) throws Exception {
            // Delegates to: org.ofbiz.order.shoppinglist.ShoppingListEvents.addBulkFromCart
            return ShoppingListEvents.addBulkFromCart(request, response);
        }

        @Request(
            uri = "addItemToShoppingList",
            controller = "ordermgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "viewshoppinglists")
        @Response(name = "error", type = "view", value = "viewshoppinglists")
        @Event(type = "service", invoke = "createShoppingListItem")
        public static String addItemToShoppingList(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "loadCartFromShoppingList",
            controller = "ordermgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "showcart")
        @Response(name = "error", type = "view", value = "checkinits")
        public static String loadCartFromShoppingList(HttpServletRequest request, HttpServletResponse response) throws Exception {
            // Delegates to: org.ofbiz.order.shoppingcart.ShoppingCartEvents.loadCartFromShoppingList
            return ShoppingCartEvents.loadCartFromShoppingList(request, response);
        }

        @Request(
            uri = "loadCartFromOrder",
            controller = "ordermgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "request", value = "finalizeOrder")
        @Response(name = "error", type = "view", value = "orderview")
        public static String loadCartFromOrder(HttpServletRequest request, HttpServletResponse response) throws Exception {
            // Delegates to: org.ofbiz.order.shoppingcart.ShoppingCartEvents.loadCartFromOrder
            return ShoppingCartEvents.loadCartFromOrder(request, response);
        }

        @Request(
            uri = "getProductInventoryAvailable",
            controller = "ordermgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "showcart")
        @Response(name = "error", type = "view", value = "showcart")
        @Event(type = "service", invoke = "getInventoryAvailableByFacility")
        public static String getProductInventoryAvailable(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "AddGiftCertificate",
            controller = "ordermgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "AddGiftCertificate")
        public interface AddGiftCertificate {}

        @Request(
            uri = "addGiftCertificateSurvey",
            controller = "ordermgr",
            secure = "true"
        )
        @Response(name = "success", type = "request", value = "additem")
        @Response(name = "error", type = "view", value = "AddGiftCertificate")
        public static String addGiftCertificateSurvey(HttpServletRequest request, HttpServletResponse response) throws Exception {
            // Delegates to: org.ofbiz.content.survey.SurveyEvents.createSurveyResponseAndRestoreParameters
            return SurveyEvents.createSurveyResponseAndRestoreParameters(request, response);
        }

        @Request(
            uri = "loadCartForReplacementOrder",
            controller = "ordermgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "request", value = "createReplacementOrder")
        @Response(name = "error", type = "view", value = "orderview")
        public static String loadCartForReplacementOrder(HttpServletRequest request, HttpServletResponse response) throws Exception {
            // Delegates to: org.ofbiz.order.shoppingcart.ShoppingCartEvents.loadCartFromOrder
            return ShoppingCartEvents.loadCartFromOrder(request, response);
        }

    }

    // Auto-generated split (Part 13)
    public static class Part13 {
        @Request(
            uri = "createReplacementOrder",
            controller = "ordermgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "request", value = "clearCartForReplacementOrder")
        @Response(name = "error", type = "view", value = "orderview")
        public static String createReplacementOrder(HttpServletRequest request, HttpServletResponse response) throws Exception {
            // Delegates to: org.ofbiz.order.shoppingcart.CheckOutEvents.createReplacementOrder
            return CheckOutEvents.createReplacementOrder(request, response);
        }

        @Request(
            uri = "clearCartForReplacementOrder",
            controller = "ordermgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "orderview")
        @Response(name = "error", type = "view", value = "orderview")
        public static String clearCartForReplacementOrder(HttpServletRequest request, HttpServletResponse response) throws Exception {
            // Delegates to: org.ofbiz.order.shoppingcart.ShoppingCartEvents.destroyCart
            return ShoppingCartEvents.destroyCart(request, response);
        }

        @Request(
            uri = "addseperator",
            controller = "ordermgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "request", value = "orderentry")
        @Response(name = "error", type = "request", value = "orderentry")
        @Event(type = "java", path = "org.ofbiz.order.shoppingcart.ShoppingCartEvents", invoke = "addSeparator")
        public static String addseperator(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "modifycart",
            controller = "ordermgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "request", value = "orderentry")
        @Response(name = "error", type = "request", value = "orderentry")
        public static String modifycart(HttpServletRequest request, HttpServletResponse response) throws Exception {
            // Delegates to: org.ofbiz.order.shoppingcart.ShoppingCartEvents.modifyCart
            return ShoppingCartEvents.modifyCart(request, response);
        }

        @Request(
            uri = "emptycart",
            controller = "ordermgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "request", value = "orderentry")
        @Response(name = "error", type = "request", value = "orderentry")
        public static String emptycart(HttpServletRequest request, HttpServletResponse response) throws Exception {
            // Delegates to: org.ofbiz.order.shoppingcart.ShoppingCartEvents.destroyCart
            return ShoppingCartEvents.destroyCart(request, response);
        }

        @Request(
            uri = "doManualPromotions",
            controller = "ordermgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "showcart")
        @Response(name = "error", type = "view", value = "showcart")
        public static String doManualPromotions(HttpServletRequest request, HttpServletResponse response) throws Exception {
            // Delegates to: org.ofbiz.order.shoppingcart.ShoppingCartEvents.doManualPromotions
            return ShoppingCartEvents.doManualPromotions(request, response);
        }

        @Request(
            uri = "setDesiredAlternateGwpProductId",
            controller = "ordermgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "showcart")
        @Response(name = "error", type = "view", value = "showcart")
        public static String setDesiredAlternateGwpProductId(HttpServletRequest request, HttpServletResponse response) throws Exception {
            // Delegates to: org.ofbiz.order.shoppingcart.ShoppingCartEvents.setDesiredAlternateGwpProductId
            return ShoppingCartEvents.setDesiredAlternateGwpProductId(request, response);
        }

        @Request(
            uri = "showAllPromotions",
            controller = "ordermgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "showAllPromotions")
        public interface ShowAllPromotions {}

        @Request(
            uri = "showPromotionDetails",
            controller = "ordermgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "showPromotionDetails")
        public interface ShowPromotionDetails {}

        @Request(
            uri = "removePromotion",
            controller = "ordermgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "request", value = "orderentry")
        @Response(name = "error", type = "request", value = "orderentry")
        public static String removePromotion(HttpServletRequest request, HttpServletResponse response) throws Exception {
            // Delegates to: org.ofbiz.order.shoppingcart.ShoppingCartEvents.removePromotion
            return ShoppingCartEvents.removePromotion(request, response);
        }

        @Request(
            uri = "quickadd",
            controller = "ordermgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "quickadd")
        public interface Quickadd {}

        @Request(
            uri = "advancedsearch",
            controller = "ordermgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "advancedsearch")
        public interface Advancedsearch {}

        @Request(
            uri = "search",
            controller = "ordermgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "keywordsearch")
        @Response(name = "none", type = "none")
        public static String search(HttpServletRequest request, HttpServletResponse response) throws Exception {
            // Delegates to: org.ofbiz.product.product.ProductSearchSession.checkDoKeywordOverride
            return ProductSearchSession.checkDoKeywordOverride(request, response);
        }

        @Request(
            uri = "keywordsearch",
            controller = "ordermgr",
            secure = "true"
        )
        @Response(name = "success", type = "request", value = "search")
        public interface Keywordsearch {}

        @Request(
            uri = "choosecatalog",
            controller = "ordermgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "request", value = "orderentry")
        public interface Choosecatalog {}

        @Request(
            uri = "addtocartbulk",
            controller = "ordermgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "keywordsearch")
        @Response(name = "error", type = "view", value = "keywordsearch")
        public static String addtocartbulk(HttpServletRequest request, HttpServletResponse response) throws Exception {
            // Delegates to: org.ofbiz.order.shoppingcart.ShoppingCartEvents.addToCartBulk
            return ShoppingCartEvents.addToCartBulk(request, response);
        }

        @Request(
            uri = "addCategoryDefaults",
            controller = "ordermgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "showcart")
        @Response(name = "error", type = "view", value = "showcart")
        public static String addCategoryDefaults(HttpServletRequest request, HttpServletResponse response) throws Exception {
            // Delegates to: org.ofbiz.order.shoppingcart.ShoppingCartEvents.addCategoryDefaults
            return ShoppingCartEvents.addCategoryDefaults(request, response);
        }

        @Request(
            uri = "BulkAddProducts",
            controller = "ordermgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "request", value = "orderentry")
        @Response(name = "error", type = "request", value = "orderentry")
        public static String bulkAddProducts(HttpServletRequest request, HttpServletResponse response) throws Exception {
            // Delegates to: org.ofbiz.order.shoppingcart.ShoppingCartEvents.bulkAddProducts
            return ShoppingCartEvents.bulkAddProducts(request, response);
        }

        @Request(
            uri = "bulkAddProductsInApprovedOrder",
            controller = "ordermgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "request-redirect", value = "orderview")
        @Response(name = "error", type = "request", value = "orderview")
        public static String bulkAddProductsInApprovedOrder(HttpServletRequest request, HttpServletResponse response) throws Exception {
            // Delegates to: org.ofbiz.order.shoppingcart.ShoppingCartEvents.bulkAddProductsInApprovedOrder
            return ShoppingCartEvents.bulkAddProductsInApprovedOrder(request, response);
        }

        @Request(
            uri = "category",
            controller = "ordermgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "category")
        public interface Category {}

    }

    // Auto-generated split (Part 14)
    public static class Part14 {
        @Request(
            uri = "product",
            controller = "ordermgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "product")
        public interface Product {}

        @Request(
            uri = "addToCompare",
            controller = "ordermgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view-last")
        public static String addToCompare(HttpServletRequest request, HttpServletResponse response) throws Exception {
            // Delegates to: org.ofbiz.product.product.ProductEvents.addProductToComparisonList
            return ProductEvents.addProductToComparisonList(request, response);
        }

        @Request(
            uri = "removeFromCompare",
            controller = "ordermgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view-last")
        public static String removeFromCompare(HttpServletRequest request, HttpServletResponse response) throws Exception {
            // Delegates to: org.ofbiz.product.product.ProductEvents.removeProductFromComparisonList
            return ProductEvents.removeProductFromComparisonList(request, response);
        }

        @Request(
            uri = "clearCompareList",
            controller = "ordermgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view-last")
        public static String clearCompareList(HttpServletRequest request, HttpServletResponse response) throws Exception {
            // Delegates to: org.ofbiz.product.product.ProductEvents.clearProductComparisonList
            return ProductEvents.clearProductComparisonList(request, response);
        }

        @Request(
            uri = "compareProducts",
            controller = "ordermgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "compareProducts", saveLastView = "true")
        public interface CompareProducts {}

        @Request(
            uri = "finalizeOrder",
            controller = "ordermgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "addparty", type = "view", value = "setAdditionalParty")
        @Response(name = "customer", type = "view", value = "custsetting")
        @Response(name = "shipping", type = "view", value = "shipsetting")
        @Response(name = "shippingAddress", type = "view", value = "EditShipAddress")
        @Response(name = "options", type = "view", value = "optionsetting")
        @Response(name = "payment", type = "request", value = "calcShippingBeforePayment")
        @Response(name = "paymentError", type = "request", value = "calcShippingBeforePayment")
        @Response(name = "term", type = "view", value = "orderTerm")
        @Response(name = "shipGroups", type = "view", value = "SetItemShipGroups")
        @Response(name = "sales", type = "request", value = "calcShipping")
        @Response(name = "po", type = "request", value = "calcTax")
        @Response(name = "error", type = "request", value = "orderentry")
        public static String finalizeOrder(HttpServletRequest request, HttpServletResponse response) throws Exception {
            // Delegates to: org.ofbiz.order.shoppingcart.CheckOutEvents.finalizeOrderEntry
            return CheckOutEvents.finalizeOrderEntry(request, response);
        }

        @Request(
            uri = "calcShippingBeforePayment",
            controller = "ordermgr",
            directRequest = "false"
        )
        @Response(name = "success", type = "request", value = "calcTaxBeforePayment")
        @Response(name = "error", type = "request", value = "orderentry")
        public static String calcShippingBeforePayment(HttpServletRequest request, HttpServletResponse response) throws Exception {
            // Delegates to: org.ofbiz.order.shoppingcart.shipping.ShippingEvents.getShipEstimate
            return ShippingEvents.getShipEstimate(request, response);
        }

        @Request(
            uri = "calcTaxBeforePayment",
            controller = "ordermgr",
            directRequest = "false"
        )
        @Response(name = "success", type = "request", value = "validatePaymentMethodsBeforePayment")
        @Response(name = "error", type = "request", value = "orderentry")
        public static String calcTaxBeforePayment(HttpServletRequest request, HttpServletResponse response) throws Exception {
            // Delegates to: org.ofbiz.order.shoppingcart.CheckOutEvents.calcTax
            return CheckOutEvents.calcTax(request, response);
        }

        @Request(
            uri = "validatePaymentMethodsBeforePayment",
            controller = "ordermgr",
            directRequest = "false"
        )
        @Response(name = "success", type = "view", value = "billsetting")
        @Response(name = "error", type = "request", value = "orderentry")
        public static String validatePaymentMethodsBeforePayment(HttpServletRequest request, HttpServletResponse response) throws Exception {
            // Delegates to: org.ofbiz.order.shoppingcart.CheckOutEvents.checkPaymentMethods
            return CheckOutEvents.checkPaymentMethods(request, response);
        }

        @Request(
            uri = "quickcheckout",
            controller = "ordermgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "quickcheckout", saveHomeView = "true")
        public interface Quickcheckout {}

        @Request(
            uri = "updateCheckoutOptions",
            controller = "ordermgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "checkoutshippingaddress")
        @Response(name = "error", type = "view", value = "showcart")
        public static String updateCheckoutOptions(HttpServletRequest request, HttpServletResponse response) throws Exception {
            // Delegates to: org.ofbiz.order.shoppingcart.CheckOutEvents.setPartialCheckOutOptions
            return CheckOutEvents.setPartialCheckOutOptions(request, response);
        }

        @Request(
            uri = "cartUpdateShipToCustomerParty",
            controller = "ordermgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "quickcheckout")
        @Response(name = "error", type = "view", value = "showcart")
        public static String cartUpdateShipToCustomerParty(HttpServletRequest request, HttpServletResponse response) throws Exception {
            // Delegates to: org.ofbiz.order.shoppingcart.CheckOutEvents.setCartShipToCustomerParty
            return CheckOutEvents.setCartShipToCustomerParty(request, response);
        }

        @Request(
            uri = "checkout",
            controller = "ordermgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "request", value = "calcShipping")
        @Response(name = "error", type = "view-last")
        public static String checkout(HttpServletRequest request, HttpServletResponse response) throws Exception {
            // Delegates to: org.ofbiz.order.shoppingcart.CheckOutEvents.setQuickCheckOutOptions
            return CheckOutEvents.setQuickCheckOutOptions(request, response);
        }

        @Request(
            uri = "createPostalAddressAndPurpose",
            controller = "ordermgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "editcontactmech")
        @Response(name = "error", type = "view", value = "editcontactmech")
        @Event(type = "service", invoke = "createPartyPostalAddress")
        public static String createPostalAddressAndPurpose(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "createPartyContactMechPurpose",
            controller = "ordermgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "editcontactmech")
        @Response(name = "error", type = "view", value = "editcontactmech")
        @Event(type = "service", invoke = "createPartyContactMechPurpose")
        public static String createPartyContactMechPurpose(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "deletePartyContactMechPurpose",
            controller = "ordermgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "editcontactmech")
        @Response(name = "error", type = "view", value = "editcontactmech")
        @Event(type = "service", invoke = "deletePartyContactMechPurpose")
        public static String deletePartyContactMechPurpose(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "checkoutoptions",
            controller = "ordermgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "request", value = "calcShipping")
        public interface Checkoutoptions {}

        @Request(
            uri = "updatePostalAddress",
            controller = "ordermgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "editcontactmech")
        @Response(name = "error", type = "view", value = "editcontactmech")
        @Event(type = "service", invoke = "updatePartyPostalAddress")
        public static String updatePostalAddress(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "createCreditCard",
            controller = "ordermgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "editcreditcard")
        @Response(name = "address", type = "view", value = "editcontactmech")
        @Response(name = "error", type = "view", value = "editcreditcard")
        @Event(type = "simple", path = "component://accounting/script/org/ofbiz/accounting/payment/PaymentMethodEvents.xml", invoke = "createCreditCard")
        public static String createCreditCard(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "createPartyForOrder",
            controller = "ordermgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "custsetting")
        public interface CreatePartyForOrder {}

    }

    // Auto-generated split (Part 15)
    public static class Part15 {
        @Request(
            uri = "updateOrderContactMech",
            controller = "ordermgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "orderview")
        @Response(name = "error", type = "view", value = "orderview")
        @Event(type = "service", invoke = "updateOrderContactMech")
        public static String updateOrderContactMech(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "setAdditionalParty",
            controller = "ordermgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "setAdditionalParty")
        public interface SetAdditionalParty {}

        @Request(
            uri = "addAdditionalParty",
            controller = "ordermgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "setAdditionalParty")
        @Response(name = "error", type = "view", value = "setAdditionalParty")
        public static String addAdditionalParty(HttpServletRequest request, HttpServletResponse response) throws Exception {
            // Delegates to: org.ofbiz.order.shoppingcart.ShoppingCartEvents.addAdditionalParty
            return ShoppingCartEvents.addAdditionalParty(request, response);
        }

        @Request(
            uri = "removeAdditionalParty",
            controller = "ordermgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "setAdditionalParty")
        @Response(name = "error", type = "view", value = "setAdditionalParty")
        public static String removeAdditionalParty(HttpServletRequest request, HttpServletResponse response) throws Exception {
            // Delegates to: org.ofbiz.order.shoppingcart.ShoppingCartEvents.removeAdditionalParty
            return ShoppingCartEvents.removeAdditionalParty(request, response);
        }

        @Request(
            uri = "calcShipping",
            controller = "ordermgr",
            directRequest = "false"
        )
        @Response(name = "success", type = "request", value = "calcTax")
        @Response(name = "error", type = "request", value = "orderentry")
        public static String calcShipping(HttpServletRequest request, HttpServletResponse response) throws Exception {
            // Delegates to: org.ofbiz.order.shoppingcart.shipping.ShippingEvents.getShipEstimate
            return ShippingEvents.getShipEstimate(request, response);
        }

        @Request(
            uri = "calcTax",
            controller = "ordermgr",
            directRequest = "false"
        )
        @Response(name = "success", type = "view", value = "confirm")
        @Response(name = "error", type = "request", value = "orderentry")
        public static String calcTax(HttpServletRequest request, HttpServletResponse response) throws Exception {
            // Delegates to: org.ofbiz.order.shoppingcart.CheckOutEvents.calcTax
            return CheckOutEvents.calcTax(request, response);
        }

        @Request(
            uri = "setCustomer",
            controller = "ordermgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "custsetting")
        public interface SetCustomer {}

        @Request(
            uri = "createCustomer",
            controller = "ordermgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "request", value = "finalizeOrder")
        @Response(name = "error", type = "view", value = "custsetting")
        @Event(type = "simple", path = "component://order/script/org/ofbiz/order/customer/CustomerEvents.xml", invoke = "createCustomer")
        public static String createCustomer(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "confirmOrder",
            controller = "ordermgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "confirm")
        public interface ConfirmOrder {}

        @Request(
            uri = "setShipping",
            controller = "ordermgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "shipsetting")
        public interface SetShipping {}

        @Request(
            uri = "EditShipAddress",
            controller = "ordermgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditShipAddress")
        public interface EditShipAddress {}

        @Request(
            uri = "createPostalAddress",
            controller = "ordermgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "request", value = "finalizeOrder")
        @Response(name = "error", type = "view", value = "EditShipAddress")
        @Event(type = "service", invoke = "createPartyPostalAddress")
        public static String createPostalAddress(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "updatePostalAddressOrderEntry",
            controller = "ordermgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "request", value = "finalizeOrder")
        @Response(name = "error", type = "view", value = "shipsetting")
        @Event(type = "service", invoke = "updatePartyPostalAddress")
        public static String updatePostalAddressOrderEntry(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "SetItemShipGroups",
            controller = "ordermgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "SetItemShipGroups")
        public interface SetItemShipGroups {}

        @Request(
            uri = "assignItemToShipGroups",
            controller = "ordermgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "SetItemShipGroups")
        @Response(name = "error", type = "view", value = "SetItemShipGroups")
        @Event(type = "service-multi", invoke = "assignItemShipGroup")
        public static String assignItemToShipGroups(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "setOptions",
            controller = "ordermgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "optionsetting")
        public interface SetOptions {}

        @Request(
            uri = "setBilling",
            controller = "ordermgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "billsetting")
        public interface SetBilling {}

        @Request(
            uri = "createCreditCardAndPostalAddress",
            controller = "ordermgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "request", value = "finalizeOrder")
        @Response(name = "error", type = "view", value = "billsetting")
        @Event(type = "service", invoke = "createCreditCardAndAddress")
        public static String createCreditCardAndPostalAddress(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "createCreditCardOrderEntry",
            controller = "ordermgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "request", value = "finalizeOrder")
        @Response(name = "error", type = "view", value = "billsetting")
        @Event(type = "service", invoke = "createCreditCard")
        public static String createCreditCardOrderEntry(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "updateCreditCard",
            controller = "ordermgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "editcreditcard")
        @Response(name = "address", type = "view", value = "editcreditcard")
        @Response(name = "error", type = "view", value = "editcreditcard")
        @Event(type = "simple", path = "component://accounting/script/org/ofbiz/accounting/payment/PaymentMethodEvents.xml", invoke = "updateCreditCard")
        public static String updateCreditCard(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

    }

    // Auto-generated split (Part 16)
    public static class Part16 {
        @Request(
            uri = "updateCreditCardAndPostalAddress",
            controller = "ordermgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "request", value = "finalizeOrder")
        @Response(name = "error", type = "view", value = "billsetting")
        @Event(type = "service", invoke = "updateCreditCardAndAddress")
        public static String updateCreditCardAndPostalAddress(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "createEftAndPostalAddress",
            controller = "ordermgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "request", value = "finalizeOrder")
        @Response(name = "error", type = "view", value = "billsetting")
        @Event(type = "service", invoke = "createEftAccountAndAddress")
        public static String createEftAndPostalAddress(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "createEftAccount",
            controller = "ordermgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "request", value = "finalizeOrder")
        @Response(name = "error", type = "view", value = "billsetting")
        @Event(type = "service", invoke = "createEftAccount")
        public static String createEftAccount(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "updateEftAndPostalAddress",
            controller = "ordermgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "request", value = "finalizeOrder")
        @Response(name = "error", type = "view", value = "billsetting")
        @Event(type = "service", invoke = "updateEftAccountAndAddress")
        public static String updateEftAndPostalAddress(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "processorder",
            controller = "ordermgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "sales_order", type = "request", value = "checkBlackList")
        @Response(name = "work_order", type = "request", value = "checkBlackList")
        @Response(name = "purchase_order", type = "request", value = "clearpocart")
        @Response(name = "error", type = "view", value = "confirm")
        public static String processorder(HttpServletRequest request, HttpServletResponse response) throws Exception {
            // Delegates to: org.ofbiz.order.shoppingcart.CheckOutEvents.createOrder
            return CheckOutEvents.createOrder(request, response);
        }

        @Request(
            uri = "checkBlackList",
            controller = "ordermgr",
            directRequest = "false"
        )
        @Response(name = "success", type = "request", value = "processpayment")
        @Response(name = "failed", type = "request", value = "failedBlacklist")
        @Response(name = "error", type = "view", value = "confirm")
        public static String checkBlackList(HttpServletRequest request, HttpServletResponse response) throws Exception {
            // Delegates to: org.ofbiz.order.shoppingcart.CheckOutEvents.checkOrderBlacklist
            return CheckOutEvents.checkOrderBlacklist(request, response);
        }

        @Request(
            uri = "failedBlacklist",
            controller = "ordermgr",
            directRequest = "false"
        )
        @Response(name = "success", type = "view", value = "main")
        @Response(name = "error", type = "view", value = "main")
        public static String failedBlacklist(HttpServletRequest request, HttpServletResponse response) throws Exception {
            // Delegates to: org.ofbiz.order.shoppingcart.CheckOutEvents.failedBlacklistCheck
            return CheckOutEvents.failedBlacklistCheck(request, response);
        }

        @Request(
            uri = "processpayment",
            controller = "ordermgr",
            directRequest = "false"
        )
        @Response(name = "success", type = "request", value = "clearcart")
        @Response(name = "fail", type = "view", value = "confirm")
        @Response(name = "error", type = "view", value = "confirm")
        public static String processpayment(HttpServletRequest request, HttpServletResponse response) throws Exception {
            // Delegates to: org.ofbiz.order.shoppingcart.CheckOutEvents.processPayment
            return CheckOutEvents.processPayment(request, response);
        }

        @Request(
            uri = "clearcart",
            controller = "ordermgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "request", value = "emailorder")
        @Response(name = "error", type = "view", value = "confirm")
        public static String clearcart(HttpServletRequest request, HttpServletResponse response) throws Exception {
            // Delegates to: org.ofbiz.order.shoppingcart.ShoppingCartEvents.destroyCart
            return ShoppingCartEvents.destroyCart(request, response);
        }

        @Request(
            uri = "clearpocart",
            controller = "ordermgr",
            secure = "true",
            directRequest = "false"
        )
        @Response(name = "success", type = "view", value = "ordercomplete")
        @Response(name = "error", type = "view", value = "confirm")
        public static String clearpocart(HttpServletRequest request, HttpServletResponse response) throws Exception {
            // Delegates to: org.ofbiz.order.shoppingcart.ShoppingCartEvents.destroyCart
            return ShoppingCartEvents.destroyCart(request, response);
        }

        @Request(
            uri = "emailorder",
            controller = "ordermgr",
            secure = "true",
            directRequest = "false"
        )
        @Response(name = "success", type = "view", value = "ordercomplete")
        @Response(name = "error", type = "view", value = "ordercomplete")
        @Event(type = "service", path = "async", invoke = "sendOrderConfirmation")
        public static String emailorder(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "setOrderTerm",
            controller = "ordermgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "orderTerm")
        public interface SetOrderTerm {}

        @Request(
            uri = "addOrderTerm",
            controller = "ordermgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "orderTerm")
        @Response(name = "error", type = "view", value = "orderTerm")
        public static String addOrderTerm(HttpServletRequest request, HttpServletResponse response) throws Exception {
            // Delegates to: org.ofbiz.order.shoppingcart.ShoppingCartEvents.addOrderTerm
            return ShoppingCartEvents.addOrderTerm(request, response);
        }

        @Request(
            uri = "removeCartOrderTerm",
            controller = "ordermgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "orderTerm")
        @Response(name = "error", type = "view", value = "orderTerm")
        public static String removeCartOrderTerm(HttpServletRequest request, HttpServletResponse response) throws Exception {
            // Delegates to: org.ofbiz.order.shoppingcart.ShoppingCartEvents.removeOrderTerm
            return ShoppingCartEvents.removeOrderTerm(request, response);
        }

        @Request(
            uri = "findreturn",
            controller = "ordermgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "findreturn")
        public interface Findreturn {}

        @Request(
            uri = "returnlist",
            controller = "ordermgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "returnlist")
        public interface Returnlist {}

        @Request(
            uri = "quickreturn",
            controller = "ordermgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "quickReturn")
        public interface Quickreturn {}

        @Request(
            uri = "makeQuickReturn",
            controller = "ordermgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "request-redirect", value = "returnItems")
        @Response(name = "error", type = "view", value = "orderview")
        @Event(type = "service-multi", invoke = "createReturnAndItemOrAdjustment")
        public static String makeQuickReturn(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "quickRefundOrder",
            controller = "ordermgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "orderview")
        @Response(name = "error", type = "view", value = "orderview")
        @Event(type = "service", invoke = "quickReturnOrder")
        public static String quickRefundOrder(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "returnMain",
            controller = "ordermgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "returnhead")
        public interface ReturnMain {}

    }

    // Auto-generated split (Part 17)
    public static class Part17 {
        @Request(
            uri = "returnItems",
            controller = "ordermgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "returnitems")
        public interface ReturnItems {}

        @Request(
            uri = "createReturn",
            controller = "ordermgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "returnhead")
        @Event(type = "service", invoke = "createReturnHeader")
        public static String createReturn(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "updateReturn",
            controller = "ordermgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "returnhead")
        @Response(name = "error", type = "view", value = "returnhead")
        @Event(type = "service", invoke = "updateReturnHeader")
        public static String updateReturn(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "createReturnItems",
            controller = "ordermgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "returnitems")
        @Event(type = "service-multi", invoke = "createReturnItemOrAdjustment")
        public static String createReturnItems(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "updateReturnItems",
            controller = "ordermgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "returnitems")
        @Event(type = "service-multi", invoke = "updateReturnItemOrAdjustment")
        public static String updateReturnItems(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "removeReturnItem",
            controller = "ordermgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "returnitems")
        @Event(type = "service", invoke = "removeReturnItem")
        public static String removeReturnItem(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "removeReturnAdjustment",
            controller = "ordermgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "returnitems")
        @Event(type = "service", invoke = "removeReturnAdjustment")
        public static String removeReturnAdjustment(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "getStatusItemsForReturn",
            controller = "ordermgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "request", value = "json")
        @Response(name = "error", type = "request", value = "json")
        @Event(type = "service", invoke = "getStatusItemsForReturn")
        public static String getStatusItemsForReturn(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "paysetup",
            controller = "ordermgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "paysetup")
        public interface Paysetup {}

        @Request(
            uri = "createWebSitePaymentSetting",
            controller = "ordermgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "paysetup")
        public interface CreateWebSitePaymentSetting {}

        @Request(
            uri = "updateWebSitePaymentSetting",
            controller = "ordermgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "paysetup")
        public interface UpdateWebSitePaymentSetting {}

        @Request(
            uri = "removeWebSitePaymentSetting",
            controller = "ordermgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "paysetup")
        public interface RemoveWebSitePaymentSetting {}

        @Request(
            uri = "OrderPurchaseReportOptions",
            controller = "ordermgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "OrderPurchaseReportOptions")
        public interface OrderPurchaseReportOptions {}

        @Request(
            uri = "OrderPurchaseReportPayment.pdf",
            controller = "ordermgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "OrderPurchaseReportPayment")
        public interface OrderPurchaseReportPaymentPdf {}

        @Request(
            uri = "OrderPurchaseReportProduct.pdf",
            controller = "ordermgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "OrderPurchaseReportProduct")
        public interface OrderPurchaseReportProductPdf {}

        @Request(
            uri = "SalesByStoreReport.pdf",
            controller = "ordermgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "SalesByStoreReport")
        public interface SalesByStoreReportPdf {}

        @Request(
            uri = "OpenOrderItemsReport",
            controller = "ordermgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "OpenOrderItemsReport")
        public interface OpenOrderItemsReport {}

        @Request(
            uri = "PurchasesByOrganizationReport.pdf",
            controller = "ordermgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "PurchasesByOrganizationReport")
        public interface PurchasesByOrganizationReportPdf {}

        @Request(
            uri = "FindRequirements",
            controller = "ordermgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "FindRequirements")
        public interface FindRequirements {}

        @Request(
            uri = "EditRequirement",
            controller = "ordermgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditRequirement")
        public interface EditRequirement {}

    }

    // Auto-generated split (Part 18)
    public static class Part18 {
        @Request(
            uri = "ListRequirementCustRequests",
            controller = "ordermgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ListRequirementCustRequests")
        public interface ListRequirementCustRequests {}

        @Request(
            uri = "ListRequirementOrders",
            controller = "ordermgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ListRequirementOrders")
        public interface ListRequirementOrders {}

        @Request(
            uri = "ListRequirementRoles",
            controller = "ordermgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ListRequirementRoles")
        public interface ListRequirementRoles {}

        @Request(
            uri = "EditRequirementRole",
            controller = "ordermgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditRequirementRole")
        public interface EditRequirementRole {}

        @Request(
            uri = "createRequirement",
            controller = "ordermgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditRequirement")
        @Event(type = "service", invoke = "createRequirement")
        public static String createRequirement(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "updateRequirement",
            controller = "ordermgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditRequirement")
        @Event(type = "service", invoke = "updateRequirement")
        public static String updateRequirement(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "deleteRequirement",
            controller = "ordermgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "FindRequirements")
        @Event(type = "service", invoke = "deleteRequirement")
        public static String deleteRequirement(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "removeRequirementRole",
            controller = "ordermgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ListRequirementRoles")
        @Event(type = "service", invoke = "removeRequirementRole")
        public static String removeRequirementRole(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "createRequirementRole",
            controller = "ordermgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ListRequirementRoles")
        @Event(type = "service", invoke = "createRequirementRole")
        public static String createRequirementRole(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "updateRequirementRole",
            controller = "ordermgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ListRequirementRoles")
        @Event(type = "service", invoke = "updateRequirementRole")
        public static String updateRequirementRole(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "autoAssignRequirementToSupplier",
            controller = "ordermgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ListRequirementRoles")
        @Event(type = "service", invoke = "autoAssignRequirementToSupplier")
        public static String autoAssignRequirementToSupplier(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "ApproveRequirements",
            controller = "ordermgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ApproveRequirements")
        public interface ApproveRequirements {}

        @Request(
            uri = "approveRequirements",
            controller = "ordermgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ApproveRequirements")
        @Response(name = "error", type = "view", value = "ApproveRequirements")
        @Event(type = "service-multi", invoke = "approveRequirement")
        public static String approveRequirements_2(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "createTransfersFromRequirements",
            controller = "ordermgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ApproveRequirements")
        @Response(name = "error", type = "view", value = "ApproveRequirements")
        @Event(type = "service-multi", invoke = "createTransferFromRequirement")
        public static String createTransfersFromRequirements(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "ApprovedProductRequirements",
            controller = "ordermgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ApprovedProductRequirements")
        public interface ApprovedProductRequirements {}

        @Request(
            uri = "ApprovedProductRequirementsReport",
            controller = "ordermgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ApprovedProductRequirementsReport")
        public interface ApprovedProductRequirementsReport {}

        @Request(
            uri = "ApprovedProductRequirementsByVendor",
            controller = "ordermgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ApprovedProductRequirementsByVendor")
        public interface ApprovedProductRequirementsByVendor {}

        @Request(
            uri = "ApprovedProductRequirementsByVendorReport",
            controller = "ordermgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ApprovedProductRequirementsByVendorReport")
        public interface ApprovedProductRequirementsByVendorReport {}

        @Request(
            uri = "quickPurchaseOrderEntry",
            controller = "ordermgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "request", value = "quickAddRequirementsToCart")
        @Response(name = "error", type = "view", value = "ApprovedProductRequirements")
        public static String quickPurchaseOrderEntry(HttpServletRequest request, HttpServletResponse response) throws Exception {
            // Delegates to: org.ofbiz.order.shoppingcart.ShoppingCartEvents.quickInitPurchaseOrder
            return ShoppingCartEvents.quickInitPurchaseOrder(request, response);
        }

        @Request(
            uri = "quickCheckoutOrderWithDefaultOptions",
            controller = "ordermgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "request", value = "calcTax")
        @Response(name = "error", type = "request", value = "orderentry")
        public static String quickCheckoutOrderWithDefaultOptions(HttpServletRequest request, HttpServletResponse response) throws Exception {
            // Delegates to: org.ofbiz.order.shoppingcart.ShoppingCartEvents.quickCheckoutOrderWithDefaultOptions
            return ShoppingCartEvents.quickCheckoutOrderWithDefaultOptions(request, response);
        }

    }

    // Auto-generated split (Part 19)
    public static class Part19 {
        @Request(
            uri = "RequirementsForSupplier",
            controller = "ordermgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "RequirementsForSupplier")
        public interface RequirementsForSupplier {}

        @Request(
            uri = "FindRequest",
            controller = "ordermgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "FindRequest")
        public interface FindRequest {}

        @Request(
            uri = "ViewRequest",
            controller = "ordermgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ViewRequest")
        public interface ViewRequest {}

        @Request(
            uri = "EditRequest",
            controller = "ordermgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditRequest")
        public interface EditRequest {}

        @Request(
            uri = "EditRequestCustomer",
            controller = "ordermgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditRequestCustomer")
        public interface EditRequestCustomer {}

        @Request(
            uri = "EditCustRequestContent",
            controller = "ordermgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditCustRequestContent")
        public interface EditCustRequestContent {}

        @Request(
            uri = "createCustRequestContent",
            controller = "ordermgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditCustRequestContent")
        @Response(name = "error", type = "view", value = "EditCustRequestContent")
        @Event(type = "service", invoke = "CustRequestUploadContentFile")
        public static String createCustRequestContent(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "deleteCustRequestContent",
            controller = "ordermgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "request-redirect", value = "EditCustRequestContent")
        @Response(name = "error", type = "view", value = "EditCustRequestContent")
        @Event(type = "service", invoke = "deleteCustRequestContent")
        public static String deleteCustRequestContent(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "request",
            controller = "ordermgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditRequest")
        public interface RequestDef {}

        @Request(
            uri = "createrequest",
            controller = "ordermgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditRequest")
        @Response(name = "error", type = "view", value = "EditRequest")
        @Event(type = "service", invoke = "createCustRequest")
        public static String createrequest(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "updaterequest",
            controller = "ordermgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditRequest")
        @Response(name = "error", type = "view", value = "EditRequest")
        @Event(type = "service", invoke = "updateCustRequest")
        public static String updaterequest(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "setCustRequestStatus",
            controller = "ordermgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view-home", value = "FindRequest")
        @Response(name = "error", type = "view", value = "EditRequest")
        @Event(type = "service", invoke = "setCustRequestStatus")
        public static String setCustRequestStatus(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "requestroles",
            controller = "ordermgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "RequestRoles")
        public interface Requestroles {}

        @Request(
            uri = "createCustRequestParty",
            controller = "ordermgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "RequestRoles")
        @Response(name = "error", type = "view", value = "RequestRoles")
        @Event(type = "service", invoke = "createCustRequestParty")
        public static String createCustRequestParty(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "updateCustRequestParty",
            controller = "ordermgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "RequestRoles")
        @Response(name = "error", type = "view", value = "RequestRoles")
        @Event(type = "service", invoke = "updateCustRequestParty")
        public static String updateCustRequestParty(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "deleteCustRequestParty",
            controller = "ordermgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "RequestRoles")
        @Response(name = "error", type = "view", value = "RequestRoles")
        @Event(type = "service", invoke = "deleteCustRequestParty")
        public static String deleteCustRequestParty(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "expireCustRequestParty",
            controller = "ordermgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "RequestRoles")
        @Response(name = "error", type = "view", value = "RequestRoles")
        @Event(type = "service", invoke = "expireCustRequestParty")
        public static String expireCustRequestParty(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "requestitems",
            controller = "ordermgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "RequestItems")
        public interface Requestitems {}

        @Request(
            uri = "EditRequestItem",
            controller = "ordermgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditRequestItem")
        public interface EditRequestItem {}

        @Request(
            uri = "requestitem",
            controller = "ordermgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditRequestItem")
        public interface Requestitem {}

    }

    // Auto-generated split (Part 20)
    public static class Part20 {
        @Request(
            uri = "createrequestitem",
            controller = "ordermgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "RequestItems")
        @Response(name = "error", type = "view", value = "RequestItems")
        @Event(type = "service", invoke = "createCustRequestItem")
        public static String createrequestitem(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "updaterequestitem",
            controller = "ordermgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "RequestItems")
        @Response(name = "error", type = "view", value = "RequestItems")
        @Event(type = "service", invoke = "updateCustRequestItem")
        public static String updaterequestitem(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "copyCustRequestItem",
            controller = "ordermgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditRequestItem")
        @Response(name = "error", type = "view", value = "EditRequestItem")
        @Event(type = "service", invoke = "copyCustRequestItem")
        public static String copyCustRequestItem(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "removerequestitem",
            controller = "ordermgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "RequestItems")
        @Response(name = "error", type = "view", value = "RequestItems")
        @Event(type = "service", invoke = "removeCustRequestItem")
        public static String removerequestitem(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "requestitemnotes",
            controller = "ordermgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "RequestItemNotes")
        public interface Requestitemnotes {}

        @Request(
            uri = "createrequestitemnote",
            controller = "ordermgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "RequestItemNotes")
        @Response(name = "error", type = "view", value = "RequestItemNotes")
        @Event(type = "service", invoke = "createCustRequestItemNote")
        public static String createrequestitemnote(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "requestitemrequirements",
            controller = "ordermgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "RequestItemRequirements")
        public interface Requestitemrequirements {}

        @Request(
            uri = "EditRequestItemWorkEfforts",
            controller = "ordermgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditRequestItemWorkEfforts")
        public interface EditRequestItemWorkEfforts {}

        @Request(
            uri = "createCustRequestItemWorkEffort",
            controller = "ordermgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditRequestItemWorkEfforts")
        @Response(name = "error", type = "view", value = "EditRequestItemWorkEfforts")
        @Event(type = "service", invoke = "createWorkEffortRequestItem")
        public static String createCustRequestItemWorkEffort(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "deleteCustRequestItemWorkEffort",
            controller = "ordermgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditRequestItemWorkEfforts")
        @Response(name = "error", type = "view", value = "EditRequestItemWorkEfforts")
        @Event(type = "service", invoke = "deleteWorkEffortRequestItem")
        public static String deleteCustRequestItemWorkEffort(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "RequestItemQuotes",
            controller = "ordermgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "RequestItemQuotes")
        public interface RequestItemQuotes {}

        @Request(
            uri = "CreateQuoteAndQuoteItemForRequest",
            controller = "ordermgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "CreateQuoteAndQuoteItemForRequest")
        public interface CreateQuoteAndQuoteItemForRequest {}

        @Request(
            uri = "createQuoteAndQuoteItemForRequest",
            controller = "ordermgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "RequestItemQuotes")
        @Response(name = "error", type = "view", value = "RequestItemQuotes")
        @Event(type = "service", invoke = "createQuoteAndQuoteItemForRequest")
        public static String createQuoteAndQuoteItemForRequest_2(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "EditQuoteItemForRequest",
            controller = "ordermgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditQuoteItemForRequest")
        public interface EditQuoteItemForRequest {}

        @Request(
            uri = "createQuoteItemForRequest",
            controller = "ordermgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "RequestItemQuotes")
        @Response(name = "error", type = "view", value = "RequestItemQuotes")
        @Event(type = "service", invoke = "createQuoteItem")
        public static String createQuoteItemForRequest(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "updateQuoteItemForRequest",
            controller = "ordermgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "RequestItemQuotes")
        @Response(name = "error", type = "view", value = "RequestItemQuotes")
        @Event(type = "service", invoke = "updateQuoteItem")
        public static String updateQuoteItemForRequest(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "FindQuote",
            controller = "ordermgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "FindQuote")
        public interface FindQuote {}

        @Request(
            uri = "ViewQuote",
            controller = "ordermgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ViewQuote")
        public interface ViewQuote {}

        @Request(
            uri = "QuoteReport",
            controller = "ordermgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "QuoteReport")
        public interface QuoteReport {}

        @Request(
            uri = "ViewQuoteProfit",
            controller = "ordermgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ViewQuoteProfit")
        public interface ViewQuoteProfit {}

    }

    // Auto-generated split (Part 21)
    public static class Part21 {
        @Request(
            uri = "EditQuoteReportMail",
            controller = "ordermgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditQuoteReportMail")
        public interface EditQuoteReportMail {}

        @Request(
            uri = "sendQuoteReportMail",
            controller = "ordermgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ViewQuote")
        @Response(name = "error", type = "view", value = "EditQuoteReportMail")
        @Event(type = "service", invoke = "sendQuoteReportMail")
        public static String sendQuoteReportMail(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "EditQuote",
            controller = "ordermgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditQuote")
        public interface EditQuote {}

        @Request(
            uri = "createQuote",
            controller = "ordermgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ViewQuote")
        @Response(name = "error", type = "view", value = "EditQuote")
        @Event(type = "service", invoke = "createQuote")
        public static String createQuote(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "updateQuote",
            controller = "ordermgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ViewQuote")
        @Response(name = "error", type = "view", value = "EditQuote")
        @Event(type = "service", invoke = "updateQuote")
        public static String updateQuote(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "copyQuote",
            controller = "ordermgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditQuote")
        @Response(name = "error", type = "view", value = "EditQuote")
        @Event(type = "service", invoke = "copyQuote")
        public static String copyQuote(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "ListQuoteRoles",
            controller = "ordermgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ListQuoteRoles")
        public interface ListQuoteRoles {}

        @Request(
            uri = "EditQuoteRole",
            controller = "ordermgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditQuoteRole")
        public interface EditQuoteRole {}

        @Request(
            uri = "createQuoteRole",
            controller = "ordermgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditQuoteRole")
        @Response(name = "error", type = "view", value = "EditQuoteRole")
        @Event(type = "service", invoke = "createQuoteRole")
        public static String createQuoteRole(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "removeQuoteRole",
            controller = "ordermgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ListQuoteRoles")
        @Response(name = "error", type = "view", value = "ListQuoteRoles")
        @Event(type = "service", invoke = "removeQuoteRole")
        public static String removeQuoteRole(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "ListQuoteItems",
            controller = "ordermgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ListQuoteItems")
        public interface ListQuoteItems {}

        @Request(
            uri = "EditQuoteItem",
            controller = "ordermgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditQuoteItem")
        public interface EditQuoteItem {}

        @Request(
            uri = "createQuoteItem",
            controller = "ordermgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "request-redirect", value = "ListQuoteItems")
        @Event(type = "service", invoke = "createQuoteItem")
        public static String createQuoteItem(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "updateQuoteItem",
            controller = "ordermgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ListQuoteItems")
        @Response(name = "error", type = "view", value = "EditQuoteItem")
        @Event(type = "service", invoke = "updateQuoteItem")
        public static String updateQuoteItem(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "removeQuoteItem",
            controller = "ordermgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ListQuoteItems")
        @Event(type = "service", invoke = "removeQuoteItem")
        public static String removeQuoteItem(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "ListQuoteAttributes",
            controller = "ordermgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ListQuoteAttributes")
        public interface ListQuoteAttributes {}

        @Request(
            uri = "EditQuoteAttribute",
            controller = "ordermgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditQuoteAttribute")
        public interface EditQuoteAttribute {}

        @Request(
            uri = "createQuoteAttribute",
            controller = "ordermgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditQuoteAttribute")
        @Event(type = "service", invoke = "createQuoteAttribute")
        public static String createQuoteAttribute(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "updateQuoteAttribute",
            controller = "ordermgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditQuoteAttribute")
        @Event(type = "service", invoke = "updateQuoteAttribute")
        public static String updateQuoteAttribute(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "removeQuoteAttribute",
            controller = "ordermgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ListQuoteAttributes")
        @Event(type = "service", invoke = "removeQuoteAttribute")
        public static String removeQuoteAttribute(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

    }

    // Auto-generated split (Part 22)
    public static class Part22 {
        @Request(
            uri = "ListQuoteCoefficients",
            controller = "ordermgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ListQuoteCoefficients")
        public interface ListQuoteCoefficients {}

        @Request(
            uri = "EditQuoteCoefficient",
            controller = "ordermgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditQuoteCoefficient")
        public interface EditQuoteCoefficient {}

        @Request(
            uri = "createQuoteCoefficient",
            controller = "ordermgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditQuoteCoefficient")
        @Event(type = "service", invoke = "createQuoteCoefficient")
        public static String createQuoteCoefficient(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "updateQuoteCoefficient",
            controller = "ordermgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditQuoteCoefficient")
        @Event(type = "service", invoke = "updateQuoteCoefficient")
        public static String updateQuoteCoefficient(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "removeQuoteCoefficient",
            controller = "ordermgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ListQuoteCoefficients")
        @Event(type = "service", invoke = "removeQuoteCoefficient")
        public static String removeQuoteCoefficient(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "ManageQuotePrices",
            controller = "ordermgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ManageQuotePrices")
        public interface ManageQuotePrices {}

        @Request(
            uri = "ListQuoteAdjustments",
            controller = "ordermgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ListQuoteAdjustments")
        public interface ListQuoteAdjustments {}

        @Request(
            uri = "EditQuoteAdjustment",
            controller = "ordermgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditQuoteAdjustment")
        public interface EditQuoteAdjustment {}

        @Request(
            uri = "autoUpdateQuotePrices",
            controller = "ordermgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ManageQuotePrices")
        @Response(name = "error", type = "view", value = "ManageQuotePrices")
        @Event(type = "service-multi", invoke = "autoUpdateQuotePrice")
        public static String autoUpdateQuotePrices(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "autoCreateQuoteAdjustments",
            controller = "ordermgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ListQuoteAdjustments")
        @Event(type = "service", invoke = "autoCreateQuoteAdjustments")
        public static String autoCreateQuoteAdjustments(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "loadCartFromQuote",
            controller = "ordermgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "request", value = "finalizeOrder")
        @Response(name = "error", type = "view", value = "EditQuote")
        public static String loadCartFromQuote(HttpServletRequest request, HttpServletResponse response) throws Exception {
            // Delegates to: org.ofbiz.order.shoppingcart.ShoppingCartEvents.loadCartFromQuote
            return ShoppingCartEvents.loadCartFromQuote(request, response);
        }

        @Request(
            uri = "createQuoteAdjustment",
            controller = "ordermgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditQuoteAdjustment")
        @Event(type = "service", invoke = "createQuoteAdjustment")
        public static String createQuoteAdjustment(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "updateQuoteAdjustment",
            controller = "ordermgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditQuoteAdjustment")
        @Event(type = "service", invoke = "updateQuoteAdjustment")
        public static String updateQuoteAdjustment(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "removeQuoteAdjustment",
            controller = "ordermgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ListQuoteAdjustments")
        @Event(type = "service", invoke = "removeQuoteAdjustment")
        public static String removeQuoteAdjustment(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "createnewquotenote",
            controller = "ordermgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "createnewquotenote")
        public interface Createnewquotenote {}

        @Request(
            uri = "createquotenote",
            controller = "ordermgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ListQuoteNotes")
        @Response(name = "error", type = "view", value = "createnewquotenote")
        @Event(type = "service", invoke = "createQuoteNote")
        public static String createquotenote(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "updateQuoteNote",
            controller = "ordermgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "request-redirect", value = "ListQuoteNotes")
        @Response(name = "error", type = "view", value = "ListQuoteNotes")
        @Event(type = "service", invoke = "updateNote")
        public static String updateQuoteNote(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "ListQuoteNotes",
            controller = "ordermgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ListQuoteNotes")
        public interface ListQuoteNotes {}

        @Request(
            uri = "EditQuoteNote",
            controller = "ordermgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditQuoteNote")
        public interface EditQuoteNote {}

        @Request(
            uri = "ListQuoteWorkEfforts",
            controller = "ordermgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ListQuoteWorkEfforts")
        public interface ListQuoteWorkEfforts {}

    }

    // Auto-generated split (Part 23)
    public static class Part23 {
        @Request(
            uri = "AddQuoteWorkEffort",
            controller = "ordermgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "AddQuoteWorkEffort")
        public interface AddQuoteWorkEffort {}

        @Request(
            uri = "EditQuoteWorkEffort",
            controller = "ordermgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditQuoteWorkEffort")
        public interface EditQuoteWorkEffort {}

        @Request(
            uri = "createQuoteWorkEffort",
            controller = "ordermgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditQuoteWorkEffort")
        @Response(name = "error", type = "view", value = "AddQuoteWorkEffort")
        @Event(type = "service", invoke = "createQuoteWorkEffort")
        public static String createQuoteWorkEffort(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "updateQuoteWorkEffort",
            controller = "ordermgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditQuoteWorkEffort")
        @Response(name = "error", type = "view", value = "EditQuoteWorkEffort")
        @Event(type = "service", invoke = "updateWorkEffort")
        public static String updateQuoteWorkEffort(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "deleteQuoteWorkEffort",
            controller = "ordermgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ListQuoteWorkEfforts")
        @Response(name = "error", type = "view", value = "ListQuoteWorkEfforts")
        @Event(type = "service", invoke = "deleteQuoteWorkEffort")
        public static String deleteQuoteWorkEffort(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "ListOrderHeaders",
            controller = "ordermgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ListOrderHeaders")
        public interface ListOrderHeaders {}

        @Request(
            uri = "AddOrderHeader",
            controller = "ordermgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditOrderHeader")
        public interface AddOrderHeader {}

        @Request(
            uri = "EditOrderHeader",
            controller = "ordermgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditOrderHeader")
        public interface EditOrderHeader {}

        @Request(
            uri = "createOrderHeader",
            controller = "ordermgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditOrderHeader")
        @Response(name = "error", type = "view", value = "EditOrderHeader")
        @Event(type = "service", invoke = "createOrderHeader")
        public static String createOrderHeader(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "updateOrderHeader",
            controller = "ordermgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditOrderHeader")
        @Response(name = "error", type = "view", value = "EditOrderHeader")
        @Event(type = "service", invoke = "updateOrderHeader")
        public static String updateOrderHeader(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "deleteOrderHeader",
            controller = "ordermgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ListOrderHeaders")
        @Response(name = "error", type = "view", value = "ListOrderHeaders")
        public interface DeleteOrderHeader {}

        @Request(
            uri = "createOrderItemShipGroup",
            controller = "ordermgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "orderShipping")
        @Response(name = "error", type = "view", value = "orderview")
        @Event(type = "service", invoke = "createOrderItemShipGroup")
        public static String createOrderItemShipGroup(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "updateOrderItemShipGroup",
            controller = "ordermgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "orderview")
        @Response(name = "error", type = "view", value = "orderview")
        @Event(type = "service", invoke = "updateOrderItemShipGroup")
        public static String updateOrderItemShipGroup(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "addPaymentMethodToOrder",
            controller = "ordermgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "request", value = "authOrderPayment")
        @Event(type = "service", invoke = "addPaymentMethodToOrder")
        public static String addPaymentMethodToOrder(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "authOrderPayment",
            controller = "ordermgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "request-redirect", value = "orderview")
        @Event(type = "service", invoke = "authOrderPaymentPreference")
        public static String authOrderPayment(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "updateOrderPaymentPreference",
            controller = "ordermgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "request-redirect", value = "orderview")
        @Response(name = "error", type = "view", value = "orderview")
        @Event(type = "service", invoke = "updateOrderPaymentPreference")
        public static String updateOrderPaymentPreference(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "setOrderReservationPriority",
            controller = "ordermgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "orderview")
        @Response(name = "error", type = "view", value = "orderview")
        @Event(type = "service", invoke = "setOrderReservationPriority")
        public static String setOrderReservationPriority(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "markOrderViewed",
            controller = "ordermgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "request", value = "json")
        @Response(name = "error", type = "request", value = "json")
        @Event(type = "service", invoke = "updateOrderHeader")
        public static String markOrderViewed(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "setInvoicePerShipment",
            controller = "ordermgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "orderview")
        @Response(name = "error", type = "view", value = "orderview")
        @Event(type = "service", invoke = "updateOrderHeader")
        public static String setInvoicePerShipment(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "addShippingAddress",
            controller = "ordermgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "request", value = "updateShipGroupShipInfo")
        @Response(name = "error", type = "view", value = "orderview")
        @Event(type = "service", invoke = "createUpdateShippingAddress")
        public static String addShippingAddress(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

    }

    // Auto-generated split (Part 24)
    public static class Part24 {
        @Request(
            uri = "upsEmailReturnLabelOrder",
            controller = "ordermgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "orderview")
        @Response(name = "error", type = "view", value = "orderview")
        @Event(type = "service", invoke = "upsEmailReturnLabel")
        public static String upsEmailReturnLabelOrder(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "upsEmailReturnLabelReturn",
            controller = "ordermgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "returnhead")
        @Response(name = "error", type = "view", value = "returnhead")
        @Event(type = "service", invoke = "upsEmailReturnLabel")
        public static String upsEmailReturnLabelReturn(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "updateShippingMethodAndCharges",
            controller = "ordermgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "orderview")
        @Response(name = "error", type = "view", value = "orderview")
        @Event(type = "service", invoke = "updateShippingMethodAndCharges")
        public static String updateShippingMethodAndCharges(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "crosssell",
            controller = "ordermgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "product")
        public interface Crosssell {}

        @Request(
            uri = "AddOrderItemShipGroup",
            controller = "ordermgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "orderview")
        @Response(name = "error", type = "view", value = "orderview")
        @Event(type = "service", invoke = "addOrderItemShipGroup")
        public static String addOrderItemShipGroup(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "DeleteOrderItemShipGroup",
            controller = "ordermgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "orderview")
        @Response(name = "error", type = "view", value = "orderview")
        @Event(type = "service", invoke = "deleteOrderItemShipGroup")
        public static String deleteOrderItemShipGroup(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "AddOrderItemShipGroupAssoc",
            controller = "ordermgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view-last", value = "orderview")
        @Response(name = "error", type = "view-last", value = "orderview")
        @Event(type = "service", invoke = "addOrderItemShipGroupAssoc")
        public static String addOrderItemShipGroupAssoc(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "UpdateOrderItemShipGroupAssoc",
            controller = "ordermgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "orderview")
        @Response(name = "error", type = "view", value = "orderview")
        @Event(type = "service-multi", invoke = "updateOrderItemShipGroupAssoc")
        public static String updateOrderItemShipGroupAssoc(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "DeleteOrderItemShipGroupAssoc",
            controller = "ordermgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "orderview")
        @Response(name = "error", type = "view", value = "orderview")
        @Event(type = "service", invoke = "deleteOrderItemShipGroupAssoc")
        public static String deleteOrderItemShipGroupAssoc(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "LookupPerson",
            controller = "ordermgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "LookupPerson")
        public interface LookupPerson {}

        @Request(
            uri = "LookupPartyGroup",
            controller = "ordermgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "LookupPartyGroup")
        public interface LookupPartyGroup {}

        @Request(
            uri = "LookupPartyName",
            controller = "ordermgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "LookupPartyName")
        public interface LookupPartyName {}

        @Request(
            uri = "LookupCustomerName",
            controller = "ordermgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "LookupCustomerName")
        public interface LookupCustomerName {}

        @Request(
            uri = "LookupProduct",
            controller = "ordermgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "LookupProduct")
        public interface LookupProduct {}

        @Request(
            uri = "LookupSupplierProduct",
            controller = "ordermgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "LookupSupplierProduct")
        public interface LookupSupplierProduct {}

        @Request(
            uri = "LookupBulkAddSupplierProductsInApprovedOrder",
            controller = "ordermgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "LookupBulkAddSupplierProductsInApprovedOrder")
        public interface LookupBulkAddSupplierProductsInApprovedOrder {}

        @Request(
            uri = "LookupProductAndPrice",
            controller = "ordermgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "LookupProductAndPrice")
        public interface LookupProductAndPrice {}

        @Request(
            uri = "LookupProductFeature",
            controller = "ordermgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "LookupProductFeature")
        public interface LookupProductFeature {}

        @Request(
            uri = "LookupUserLoginAndPartyDetails",
            controller = "ordermgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "LookupUserLoginAndPartyDetails")
        public interface LookupUserLoginAndPartyDetails {}

        @Request(
            uri = "LookupPreferredContactMech",
            controller = "ordermgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "LookupPreferredContactMech")
        public interface LookupPreferredContactMech {}

    }

    // Auto-generated split (Part 25)
    public static class Part25 {
        @Request(
            uri = "LookupVariantProduct",
            controller = "ordermgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "LookupVariantProduct")
        public interface LookupVariantProduct {}

        @Request(
            uri = "LookupFacility",
            controller = "ordermgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "LookupFacility")
        public interface LookupFacility {}

        @Request(
            uri = "LookupFixedAsset",
            controller = "ordermgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "LookupFixedAsset")
        public interface LookupFixedAsset {}

        @Request(
            uri = "LookupShoppingList",
            controller = "ordermgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "LookupShoppingList")
        public interface LookupShoppingList {}

        @Request(
            uri = "LookupCustRequest",
            controller = "ordermgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "LookupCustRequest")
        public interface LookupCustRequest {}

        @Request(
            uri = "LookupCustRequestItem",
            controller = "ordermgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "LookupCustRequestItem")
        public interface LookupCustRequestItem {}

        @Request(
            uri = "LookupRequirement",
            controller = "ordermgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "LookupRequirement")
        public interface LookupRequirement {}

        @Request(
            uri = "LookupQuote",
            controller = "ordermgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "LookupQuote")
        public interface LookupQuote {}

        @Request(
            uri = "LookupQuoteItem",
            controller = "ordermgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "LookupQuoteItem")
        public interface LookupQuoteItem {}

        @Request(
            uri = "LookupOrderHeader",
            controller = "ordermgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "LookupOrderHeader")
        public interface LookupOrderHeader {}

        @Request(
            uri = "LookupWorkEffort",
            controller = "ordermgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "LookupWorkEffort")
        public interface LookupWorkEffort {}

        @Request(
            uri = "LookupAssociatedProducts",
            controller = "ordermgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "LookupAssociatedProducts")
        public interface LookupAssociatedProducts {}

        @Request(
            uri = "order.pdf",
            controller = "ordermgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "OrderPDF")
        public interface OrderPdf {}

        @Request(
            uri = "return.pdf",
            controller = "ordermgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ReturnPDF")
        public interface ReturnPdf {}

        @Request(
            uri = "shipGroups.pdf",
            controller = "ordermgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ShipGroupsPDF")
        public interface ShipGroupsPdf {}

        @Request(
            uri = "orderPickSheet.pdf",
            controller = "ordermgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "OrderPickSheetPDF")
        @Response(name = "error", type = "view", value = "orderview")
        @Event(type = "service", invoke = "printPickSheets")
        public static String orderPickSheetPdf(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "LookupProductCategory",
            controller = "ordermgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "LookupProductCategory")
        public interface LookupProductCategory {}

        @Request(
            uri = "ReturnHistory",
            controller = "ordermgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ReturnHistory")
        public interface ReturnHistory {}

        @Request(
            uri = "LookupContent",
            controller = "ordermgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "LookupContent")
        public interface LookupContent {}

        @Request(
            uri = "productAvailabalityByFacility",
            controller = "ordermgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "showcart")
        @Response(name = "error", type = "view", value = "showcart")
        @Event(type = "service", invoke = "productAvailabalityByFacility")
        public static String productAvailabalityByFacility(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

    }

    // Auto-generated split (Part 26)
    public static class Part26 {
        @Request(
            uri = "clearSearchOptionsHistoryList",
            controller = "ordermgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "advancedsearch")
        @Response(name = "error", type = "view", value = "advancedsearch")
        public static String clearSearchOptionsHistoryList(HttpServletRequest request, HttpServletResponse response) throws Exception {
            // Delegates to: org.ofbiz.product.product.ProductSearchSession.clearSearchOptionsHistoryList
            return ProductSearchSession.clearSearchOptionsHistoryList(request, response);
        }

        @Request(
            uri = "setCurrentSearchFromHistory",
            controller = "ordermgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "advancedsearch")
        @Response(name = "error", type = "view", value = "advancedsearch")
        public static String setCurrentSearchFromHistory(HttpServletRequest request, HttpServletResponse response) throws Exception {
            // Delegates to: org.ofbiz.product.product.ProductSearchSession.setCurrentSearchFromHistory
            return ProductSearchSession.setCurrentSearchFromHistory(request, response);
        }

        @Request(
            uri = "setCurrentSearchFromHistoryAndSearch",
            controller = "ordermgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "request", value = "keywordsearch")
        @Response(name = "error", type = "view", value = "advancedsearch")
        public static String setCurrentSearchFromHistoryAndSearch(HttpServletRequest request, HttpServletResponse response) throws Exception {
            // Delegates to: org.ofbiz.product.product.ProductSearchSession.setCurrentSearchFromHistory
            return ProductSearchSession.setCurrentSearchFromHistory(request, response);
        }

        @Request(
            uri = "ProductUomDropDownOnly",
            controller = "ordermgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ProductUomDropDownOnly", saveLastView = "true")
        public interface ProductUomDropDownOnly {}

        @Request(
            uri = "ListQuoteTerms",
            controller = "ordermgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ListQuoteTerms", saveLastView = "true")
        public interface ListQuoteTerms {}

        @Request(
            uri = "EditQuoteTerm",
            controller = "ordermgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditQuoteTerm", saveLastView = "true")
        public interface EditQuoteTerm {}

        @Request(
            uri = "EditQuoteTermItem",
            controller = "ordermgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditQuoteTermItem", saveLastView = "true")
        public interface EditQuoteTermItem {}

        @Request(
            uri = "splitship",
            controller = "ordermgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "splitship")
        public interface Splitship {}

        @Request(
            uri = "updatesplit",
            controller = "ordermgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "splitship")
        @Response(name = "error", type = "view", value = "splitship")
        @Event(type = "service", invoke = "assignItemShipGroup")
        public static String updatesplit(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "updateShippingAddress",
            controller = "ordermgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "splitship")
        @Response(name = "error", type = "request", value = "splitship")
        @Event(type = "service", invoke = "setCartShippingAddress")
        public static String updateShippingAddress(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "updateShippingOptions",
            controller = "ordermgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "splitship")
        @Response(name = "error", type = "request", value = "splitship")
        @Event(type = "service", invoke = "setCartShippingOptions")
        public static String updateShippingOptions(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "updateShipGroupShipInfo",
            controller = "ordermgr",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "orderview")
        @Response(name = "error", type = "view", value = "orderview")
        @Event(type = "service", invoke = "updateShipGroupShipInfo")
        public static String updateShipGroupShipInfo(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }


    }
}
