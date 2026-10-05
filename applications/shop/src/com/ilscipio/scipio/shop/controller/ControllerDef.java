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
package com.ilscipio.scipio.shop.controller;

import com.ilscipio.scipio.ce.webapp.control.def.View;
import com.ilscipio.scipio.ce.webapp.control.def.Request;
import com.ilscipio.scipio.ce.webapp.control.def.Response;
import com.ilscipio.scipio.ce.webapp.control.def.Responses;
import com.ilscipio.scipio.ce.webapp.control.def.Event;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import org.ofbiz.order.order.OrderReturnEvents;
import com.ilscipio.scipio.shop.janrain.JanrainHelper;
import org.ofbiz.content.data.DataEvents;
import org.ofbiz.content.survey.SurveyEvents;
import org.ofbiz.order.shoppingcart.shipping.ShippingEvents;
import org.ofbiz.product.product.ProductEvents;
import org.ofbiz.order.shoppingcart.product.ProductStoreCartAwareEvents;
import com.ilscipio.scipio.shop.misc.ThirdPartyEvents;
// NOTE: PayPalEvents, WorldPayEvents, IdealEvents are excluded from compilation (thirdparty)
// import org.ofbiz.accounting.thirdparty.paypal.PayPalEvents;
// import org.ofbiz.accounting.thirdparty.worldpay.WorldPayEvents;
// import org.ofbiz.accounting.thirdparty.ideal.IdealEvents;
import org.ofbiz.common.CommonEvents;
import org.ofbiz.order.shoppingcart.ShoppingCartEvents;
import org.ofbiz.order.shoppinglist.ShoppingListEvents;
import org.ofbiz.order.order.OrderEvents;
import org.ofbiz.order.thirdparty.paypal.ExpressCheckoutEvents;
import org.ofbiz.product.product.ProductSearchSession;
import org.ofbiz.order.shoppingcart.CheckOutEvents;

/**
 * Auto-generated annotation-based controller definitions.
 *
 * <p>Generated from controller.xml by convertXmlToAnnotation Gradle task.</p>
 *
 * <p>SCIPIO: 4.0.0: Auto-generated.</p>
 */
public class ControllerDef {
    @com.ilscipio.scipio.ce.webapp.control.def.View(
        name = "main",
        type = "screen",
        page = "component://shop/widget/CommonScreens.xml#main",
        controller = "shop"
    )
    public static final String VIEW_MAIN = "main";

    // SCIPIO: 4.0.0: home page template of the storefront theme (VT_SHOP_HOME)
    @com.ilscipio.scipio.ce.webapp.control.def.View(
        name = "themeHome",
        type = "screen",
        page = "component://shop/widget/CommonScreens.xml#themeHome",
        controller = "shop"
    )
    public static final String VIEW_THEME_HOME = "themeHome";

    @com.ilscipio.scipio.ce.webapp.control.def.View(
        name = "policies",
        type = "screen",
        page = "component://shop/widget/CommonScreens.xml#policies",
        controller = "shop"
    )
    public static final String VIEW_POLICIES = "policies";

    @com.ilscipio.scipio.ce.webapp.control.def.View(
        name = "license",
        type = "screen",
        page = "component://shop/widget/CommonScreens.xml#license",
        controller = "shop"
    )
    public static final String VIEW_LICENSE = "license";

    // SCIPIO: 4.0.0: store legal texts (compliance component): legal?doc=<slug>
    @com.ilscipio.scipio.ce.webapp.control.def.View(
        name = "legal",
        type = "screen",
        page = "component://shop/widget/CommonScreens.xml#legal",
        controller = "shop"
    )
    public static final String VIEW_LEGAL = "legal";

    // SCIPIO: 4.0.0: Your Privacy Choices (US opt-out, GPC; compliance component)
    @com.ilscipio.scipio.ce.webapp.control.def.View(
        name = "privacyChoices",
        type = "screen",
        page = "component://shop/widget/CommonScreens.xml#privacyChoices",
        controller = "shop"
    )
    public static final String VIEW_PRIVACY_CHOICES = "privacyChoices";

    // SCIPIO: 4.0.0: EU withdrawal function "Withdraw from contract here" (compliance component)
    @com.ilscipio.scipio.ce.webapp.control.def.View(
        name = "withdraw",
        type = "screen",
        page = "component://shop/widget/CommonScreens.xml#withdraw",
        controller = "shop"
    )
    public static final String VIEW_WITHDRAW = "withdraw";

    // SCIPIO: 4.0.0: marketplace seller page with trader data (compliance component)
    @com.ilscipio.scipio.ce.webapp.control.def.View(
        name = "seller",
        type = "screen",
        page = "component://shop/widget/CommonScreens.xml#seller",
        controller = "shop"
    )
    public static final String VIEW_SELLER = "seller";

    // SCIPIO: 4.0.0: privacy center and privacy requests (compliance component)
    @com.ilscipio.scipio.ce.webapp.control.def.View(
        name = "privacyCenter",
        type = "screen",
        page = "component://shop/widget/CommonScreens.xml#privacyCenter",
        controller = "shop"
    )
    public static final String VIEW_PRIVACY_CENTER = "privacyCenter";

    @com.ilscipio.scipio.ce.webapp.control.def.View(
        name = "privacyRequest",
        type = "screen",
        page = "component://shop/widget/CommonScreens.xml#privacyRequest",
        controller = "shop"
    )
    public static final String VIEW_PRIVACY_REQUEST = "privacyRequest";

    @com.ilscipio.scipio.ce.webapp.control.def.View(
        name = "privacyDeleted",
        type = "screen",
        page = "component://shop/widget/CommonScreens.xml#privacyDeleted",
        controller = "shop"
    )
    public static final String VIEW_PRIVACY_DELETED = "privacyDeleted";

    @com.ilscipio.scipio.ce.webapp.control.def.View(
        name = "login",
        type = "screen",
        page = "component://shop/widget/CommonScreens.xml#login",
        controller = "shop"
    )
    public static final String VIEW_LOGIN = "login";

    @com.ilscipio.scipio.ce.webapp.control.def.View(
        name = "requirePasswordChange",
        type = "screen",
        page = "component://shop/widget/CommonScreens.xml#requirePasswordChange",
        controller = "shop"
    )
    public static final String VIEW_REQUIREPASSWORDCHANGE = "requirePasswordChange";

    @com.ilscipio.scipio.ce.webapp.control.def.View(
        name = "editShoppingList",
        type = "screen",
        page = "component://shop/widget/ShoppingListScreens.xml#editShoppingList",
        controller = "shop"
    )
    public static final String VIEW_EDITSHOPPINGLIST = "editShoppingList";

    @com.ilscipio.scipio.ce.webapp.control.def.View(
        name = "showcart",
        type = "screen",
        page = "component://shop/widget/CartScreens.xml#showcart",
        controller = "shop"
    )
    public static final String VIEW_SHOWCART = "showcart";

    @com.ilscipio.scipio.ce.webapp.control.def.View(
        name = "showAllPromotions",
        type = "screen",
        page = "component://shop/widget/CartScreens.xml#showAllPromotions",
        controller = "shop"
    )
    public static final String VIEW_SHOWALLPROMOTIONS = "showAllPromotions";

    @com.ilscipio.scipio.ce.webapp.control.def.View(
        name = "showPromotionDetails",
        type = "screen",
        page = "component://shop/widget/CartScreens.xml#showPromotionDetails",
        controller = "shop"
    )
    public static final String VIEW_SHOWPROMOTIONDETAILS = "showPromotionDetails";

    @com.ilscipio.scipio.ce.webapp.control.def.View(
        name = "UpdateCart",
        type = "screen",
        page = "component://shop/widget/CartScreens.xml#UpdateCart",
        controller = "shop"
    )
    public static final String VIEW_UPDATECART = "UpdateCart";

    @com.ilscipio.scipio.ce.webapp.control.def.View(
        name = "quickadd",
        type = "screen",
        page = "component://shop/widget/CatalogScreens.xml#quickadd",
        controller = "shop"
    )
    public static final String VIEW_QUICKADD = "quickadd";

    @com.ilscipio.scipio.ce.webapp.control.def.View(
        name = "category",
        type = "screen",
        page = "component://shop/widget/CatalogScreens.xml#category",
        controller = "shop"
    )
    public static final String VIEW_CATEGORY = "category";

    @com.ilscipio.scipio.ce.webapp.control.def.View(
        name = "product",
        type = "screen",
        page = "component://shop/widget/CatalogScreens.xml#product",
        controller = "shop"
    )
    public static final String VIEW_PRODUCT = "product";

    @com.ilscipio.scipio.ce.webapp.control.def.View(
        name = "lastviewedproducts",
        type = "screen",
        page = "component://shop/widget/CatalogScreens.xml#lastviewedproducts",
        controller = "shop"
    )
    public static final String VIEW_LASTVIEWEDPRODUCTS = "lastviewedproducts";

    @com.ilscipio.scipio.ce.webapp.control.def.View(
        name = "productReview",
        type = "screen",
        page = "component://shop/widget/CatalogScreens.xml#productreview",
        controller = "shop"
    )
    public static final String VIEW_PRODUCTREVIEW = "productReview";

    @com.ilscipio.scipio.ce.webapp.control.def.View(
        name = "keywordsearch",
        type = "screen",
        page = "component://shop/widget/CatalogScreens.xml#keywordsearch",
        controller = "shop"
    )
    public static final String VIEW_KEYWORDSEARCH = "keywordsearch";

    @com.ilscipio.scipio.ce.webapp.control.def.View(
        name = "advancedsearch",
        type = "screen",
        page = "component://shop/widget/CatalogScreens.xml#advancedsearch",
        controller = "shop"
    )
    public static final String VIEW_ADVANCEDSEARCH = "advancedsearch";

    @com.ilscipio.scipio.ce.webapp.control.def.View(
        name = "tellafriend",
        type = "screen",
        page = "component://shop/widget/CatalogScreens.xml#tellafriend",
        controller = "shop"
    )
    public static final String VIEW_TELLAFRIEND = "tellafriend";

    @com.ilscipio.scipio.ce.webapp.control.def.View(
        name = "custsetting",
        type = "screen",
        page = "component://shop/widget/OrderScreens.xml#custsettings",
        controller = "shop"
    )
    public static final String VIEW_CUSTSETTING = "custsetting";

    @com.ilscipio.scipio.ce.webapp.control.def.View(
        name = "shipsetting",
        type = "screen",
        page = "component://shop/widget/OrderScreens.xml#shipsettings",
        controller = "shop"
    )
    public static final String VIEW_SHIPSETTING = "shipsetting";


    // Auto-generated split (Part 2)
    public static class Part2 {
        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "optionsetting",
            type = "screen",
            page = "component://shop/widget/OrderScreens.xml#optionsettings",
            controller = "shop"
        )
        public static final String VIEW_OPTIONSETTING = "optionsetting";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "paymentoptions",
            type = "screen",
            page = "component://shop/widget/OrderScreens.xml#paymentoptions",
            controller = "shop"
        )
        public static final String VIEW_PAYMENTOPTIONS = "paymentoptions";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "paymentinformation",
            type = "screen",
            page = "component://shop/widget/OrderScreens.xml#paymentinformation",
            controller = "shop"
        )
        public static final String VIEW_PAYMENTINFORMATION = "paymentinformation";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "checkoutshippingaddress",
            type = "screen",
            page = "component://shop/widget/OrderScreens.xml#checkoutshippingaddress",
            controller = "shop"
        )
        public static final String VIEW_CHECKOUTSHIPPINGADDRESS = "checkoutshippingaddress";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "checkoutshippingoptions",
            type = "screen",
            page = "component://shop/widget/OrderScreens.xml#checkoutshippingoptions",
            controller = "shop"
        )
        public static final String VIEW_CHECKOUTSHIPPINGOPTIONS = "checkoutshippingoptions";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "checkoutpayment",
            type = "screen",
            page = "component://shop/widget/OrderScreens.xml#checkoutpayment",
            controller = "shop"
        )
        public static final String VIEW_CHECKOUTPAYMENT = "checkoutpayment";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "splitship",
            type = "screen",
            page = "component://shop/widget/OrderScreens.xml#splitship",
            controller = "shop"
        )
        public static final String VIEW_SPLITSHIP = "splitship";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "checkoutreview",
            type = "screen",
            page = "component://shop/widget/OrderScreens.xml#checkoutreview",
            controller = "shop"
        )
        public static final String VIEW_CHECKOUTREVIEW = "checkoutreview";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "orderreview",
            type = "screen",
            page = "component://shop/widget/OrderScreens.xml#orderreview",
            controller = "shop"
        )
        public static final String VIEW_ORDERREVIEW = "orderreview";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "billsetting",
            type = "screen",
            page = "component://shop/widget/OrderScreens.xml#billsettings",
            controller = "shop"
        )
        public static final String VIEW_BILLSETTING = "billsetting";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "ordercomplete",
            type = "screen",
            page = "component://shop/widget/OrderScreens.xml#ordercomplete",
            controller = "shop"
        )
        public static final String VIEW_ORDERCOMPLETE = "ordercomplete";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "orderviewonly",
            type = "screen",
            page = "component://shop/widget/OrderScreens.xml#orderviewonly",
            controller = "shop"
        )
        public static final String VIEW_ORDERVIEWONLY = "orderviewonly";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "orderprint",
            type = "screen",
            page = "component://shop/widget/OrderScreens.xml#orderprint",
            controller = "shop"
        )
        public static final String VIEW_ORDERPRINT = "orderprint";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "orderhistory",
            type = "screen",
            page = "component://shop/widget/OrderScreens.xml#orderhistory",
            controller = "shop"
        )
        public static final String VIEW_ORDERHISTORY = "orderhistory";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "orderdownloads",
            type = "screen",
            page = "component://shop/widget/OrderScreens.xml#orderdownloads",
            controller = "shop"
        )
        public static final String VIEW_ORDERDOWNLOADS = "orderdownloads";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "orderstatus",
            type = "screen",
            page = "component://shop/widget/OrderScreens.xml#orderstatus",
            controller = "shop"
        )
        public static final String VIEW_ORDERSTATUS = "orderstatus";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "requestreturn",
            type = "screen",
            page = "component://shop/widget/OrderScreens.xml#requestreturn",
            controller = "shop"
        )
        public static final String VIEW_REQUESTRETURN = "requestreturn";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "quickAnonCustSetting",
            type = "screen",
            page = "component://shop/widget/OrderScreens.xml#quickAnonCustSettings",
            controller = "shop"
        )
        public static final String VIEW_QUICKANONCUSTSETTING = "quickAnonCustSetting";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "quickAnonOptionSetting",
            type = "screen",
            page = "component://shop/widget/OrderScreens.xml#quickAnonOptionSettings",
            controller = "shop"
        )
        public static final String VIEW_QUICKANONOPTIONSETTING = "quickAnonOptionSetting";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "quickAnonOrderReview",
            type = "screen",
            page = "component://shop/widget/OrderScreens.xml#quickAnonOrderReview",
            controller = "shop"
        )
        public static final String VIEW_QUICKANONORDERREVIEW = "quickAnonOrderReview";

    }

    // Auto-generated split (Part 3)
    public static class Part3 {
        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "quickAnonOrderItems",
            type = "screen",
            page = "component://shop/widget/OrderScreens.xml#quickAnonOrderItems",
            controller = "shop"
        )
        public static final String VIEW_QUICKANONORDERITEMS = "quickAnonOrderItems";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "quickAnonCcInfo",
            type = "screen",
            page = "component://shop/widget/OrderScreens.xml#quickAnonCcInfo",
            controller = "shop"
        )
        public static final String VIEW_QUICKANONCCINFO = "quickAnonCcInfo";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "quickAnonGcInfo",
            type = "screen",
            page = "component://shop/widget/OrderScreens.xml#quickAnonGcInfo",
            controller = "shop"
        )
        public static final String VIEW_QUICKANONGCINFO = "quickAnonGcInfo";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "quickAnonEftInfo",
            type = "screen",
            page = "component://shop/widget/OrderScreens.xml#quickAnonEftInfo",
            controller = "shop"
        )
        public static final String VIEW_QUICKANONEFTINFO = "quickAnonEftInfo";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "survey",
            type = "screen",
            page = "component://shop/widget/CustomerScreens.xml#customersurvey",
            controller = "shop"
        )
        public static final String VIEW_SURVEY = "survey";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "newcustomer",
            type = "screen",
            page = "component://shop/widget/CustomerScreens.xml#newcustomer",
            controller = "shop"
        )
        public static final String VIEW_NEWCUSTOMER = "newcustomer";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "viewprofile",
            type = "screen",
            page = "component://shop/widget/CustomerScreens.xml#viewprofile",
            controller = "shop"
        )
        public static final String VIEW_VIEWPROFILE = "viewprofile";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "editcontactmech",
            type = "screen",
            page = "component://shop/widget/CustomerScreens.xml#editcontactmech",
            controller = "shop"
        )
        public static final String VIEW_EDITCONTACTMECH = "editcontactmech";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "editcreditcard",
            type = "screen",
            page = "component://shop/widget/CustomerScreens.xml#editcreditcard",
            controller = "shop"
        )
        public static final String VIEW_EDITCREDITCARD = "editcreditcard";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "editeftaccount",
            type = "screen",
            page = "component://shop/widget/CustomerScreens.xml#editeftaccount",
            controller = "shop"
        )
        public static final String VIEW_EDITEFTACCOUNT = "editeftaccount";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "editgiftcard",
            type = "screen",
            page = "component://shop/widget/CustomerScreens.xml#editgiftcard",
            controller = "shop"
        )
        public static final String VIEW_EDITGIFTCARD = "editgiftcard";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "changepassword",
            type = "screen",
            page = "component://shop/widget/CustomerScreens.xml#changepassword",
            controller = "shop"
        )
        public static final String VIEW_CHANGEPASSWORD = "changepassword";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "editperson",
            type = "screen",
            page = "component://shop/widget/CustomerScreens.xml#editperson",
            controller = "shop"
        )
        public static final String VIEW_EDITPERSON = "editperson";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "giftcardbalance",
            type = "screen",
            page = "component://shop/widget/CustomerScreens.xml#giftcardbalance",
            controller = "shop"
        )
        public static final String VIEW_GIFTCARDBALANCE = "giftcardbalance";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "giftcardlink",
            type = "screen",
            page = "component://shop/widget/CustomerScreens.xml#giftcardlink",
            controller = "shop"
        )
        public static final String VIEW_GIFTCARDLINK = "giftcardlink";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "profilesurvey",
            type = "screen",
            page = "component://shop/widget/CustomerScreens.xml#customersurvey",
            controller = "shop"
        )
        public static final String VIEW_PROFILESURVEY = "profilesurvey";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "digitalproductlist",
            type = "screen",
            page = "component://shop/widget/CustomerScreens.xml#digitalproductlist",
            controller = "shop"
        )
        public static final String VIEW_DIGITALPRODUCTLIST = "digitalproductlist";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "digitalproductedit",
            type = "screen",
            page = "component://shop/widget/CustomerScreens.xml#digitalproductedit",
            controller = "shop"
        )
        public static final String VIEW_DIGITALPRODUCTEDIT = "digitalproductedit";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "contactus",
            type = "screen",
            page = "component://shop/widget/CustomerScreens.xml#contactus",
            controller = "shop"
        )
        public static final String VIEW_CONTACTUS = "contactus";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "AnonContactus",
            type = "screen",
            page = "component://shop/widget/CustomerScreens.xml#AnonContactus",
            controller = "shop"
        )
        public static final String VIEW_ANONCONTACTUS = "AnonContactus";

    }

    // Auto-generated split (Part 4)
    public static class Part4 {
        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "messagelist",
            type = "screen",
            page = "component://shop/widget/CustomerScreens.xml#messagelist",
            controller = "shop"
        )
        public static final String VIEW_MESSAGELIST = "messagelist";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "messagedetail",
            type = "screen",
            page = "component://shop/widget/CustomerScreens.xml#messagedetail",
            controller = "shop"
        )
        public static final String VIEW_MESSAGEDETAIL = "messagedetail";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "messagecreate",
            type = "screen",
            page = "component://shop/widget/CustomerScreens.xml#messagecreate",
            controller = "shop"
        )
        public static final String VIEW_MESSAGECREATE = "messagecreate";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "ManageAddress",
            type = "screen",
            page = "component://shop/widget/CustomerScreens.xml#viewprofile",
            controller = "shop"
        )
        public static final String VIEW_MANAGEADDRESS = "ManageAddress";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "EditProfile",
            type = "screen",
            page = "component://shop/widget/CustomerScreens.xml#EditProfile",
            controller = "shop"
        )
        public static final String VIEW_EDITPROFILE = "EditProfile";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "ListQuotes",
            type = "screen",
            page = "component://shop/widget/QuoteScreens.xml#ListQuotes",
            controller = "shop"
        )
        public static final String VIEW_LISTQUOTES = "ListQuotes";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "ViewQuote",
            type = "screen",
            page = "component://shop/widget/QuoteScreens.xml#ViewQuote",
            controller = "shop"
        )
        public static final String VIEW_VIEWQUOTE = "ViewQuote";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "ListRequests",
            type = "screen",
            page = "component://shop/widget/CustRequestScreens.xml#ListRequests",
            controller = "shop"
        )
        public static final String VIEW_LISTREQUESTS = "ListRequests";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "ViewRequest",
            type = "screen",
            page = "component://shop/widget/CustRequestScreens.xml#ViewRequest",
            controller = "shop"
        )
        public static final String VIEW_VIEWREQUEST = "ViewRequest";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "NewRequest",
            type = "screen",
            page = "component://shop/widget/CustRequestScreens.xml#NewRequest",
            controller = "shop"
        )
        public static final String VIEW_NEWREQUEST = "NewRequest";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "OrderPDF",
            type = "screenfop",
            page = "component://shop/widget/OrderPrintScreens.xml#OrderPDF",
            contentType = "application/pdf",
            encoding = "none",
            controller = "shop"
        )
        public static final String VIEW_ORDERPDF = "OrderPDF";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "InvoicePDF",
            type = "screenfop",
            page = "component://shop/widget/OrderPrintScreens.xml#InvoicePDF",
            contentType = "application/pdf",
            encoding = "none",
            controller = "shop"
        )
        public static final String VIEW_INVOICEPDF = "InvoicePDF";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "OnePageCheckout",
            type = "screen",
            page = "component://shop/widget/OrderScreens.xml#OnePageCheckout",
            controller = "shop"
        )
        public static final String VIEW_ONEPAGECHECKOUT = "OnePageCheckout";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "compareProducts",
            type = "screen",
            page = "component://shop/widget/CatalogScreens.xml#compareProducts",
            controller = "shop"
        )
        public static final String VIEW_COMPAREPRODUCTS = "compareProducts";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "ProductUomDropDownOnly",
            type = "screen",
            page = "component://shop/widget/CatalogScreens.xml#ProductUomDropDownOnly",
            controller = "shop"
        )
        public static final String VIEW_PRODUCTUOMDROPDOWNONLY = "ProductUomDropDownOnly";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "ContactListOptOut",
            type = "screen",
            page = "component://shop/widget/ContactListScreens.xml#OptOutResponse",
            controller = "shop"
        )
        public static final String VIEW_CONTACTLISTOPTOUT = "ContactListOptOut";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "productCategoryList",
            type = "screen",
            page = "component://shop/widget/CatalogScreens.xml#productCategoryList",
            controller = "shop"
        )
        public static final String VIEW_PRODUCTCATEGORYLIST = "productCategoryList";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "showShoppingList",
            type = "screen",
            page = "component://shop/widget/ShoppingListScreens.xml#showShoppingList",
            controller = "shop"
        )
        public static final String VIEW_SHOWSHOPPINGLIST = "showShoppingList";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "ListLocales",
            type = "screen",
            page = "component://shop/widget/CommonScreens.xml#ListLocalesCompact",
            controller = "shop"
        )
        public static final String VIEW_LISTLOCALES = "ListLocales";

        @Request(
            uri = "main",
            controller = "shop",
            secure = "true"
        )
        @Response(name = "success", type = "view", value = "main", saveCurrentView = "true")
        @Response(name = "themeHome", type = "view", value = "themeHome", saveCurrentView = "true")
        public static String main(HttpServletRequest request, HttpServletResponse response) {
            // SCIPIO: 4.0.0: a storefront theme may give the home page its own template (VisualThemeResource VT_SHOP_HOME)
            try {
                org.ofbiz.entity.GenericValue store = org.ofbiz.product.store.ProductStoreWorker.getProductStore(request);
                String themeId = store != null ? store.getString("visualThemeId") : null;
                if (themeId != null) {
                    org.ofbiz.entity.GenericValue home = org.ofbiz.entity.util.EntityQuery.use((org.ofbiz.entity.Delegator) request.getAttribute("delegator"))
                            .from("VisualThemeResource").where("visualThemeId", themeId, "resourceTypeEnumId", "VT_SHOP_HOME").cache().queryFirst();
                    if (home != null) {
                        request.setAttribute("shopThemeHomeLocation", home.getString("resourceValue"));
                        return "themeHome";
                    }
                }
            } catch (Exception e) {
                org.ofbiz.base.util.Debug.logWarning("Could not check the theme home page: " + e.getMessage(), "ControllerDef");
            }
            return "success";
        }

    }

    // Auto-generated split (Part 5)
    public static class Part5 {
        @Request(
            uri = "policies",
            controller = "shop",
            secure = "true"
        )
        @Response(name = "success", type = "view", value = "policies")
        public interface Policies {}

        @Request(
            uri = "license",
            controller = "shop",
            secure = "true"
        )
        @Response(name = "success", type = "view", value = "license")
        public interface License {}

        @Request(
            uri = "legal",
            controller = "shop",
            secure = "true"
        )
        @Response(name = "success", type = "view", value = "legal")
        public interface Legal {}

        @Request(
            uri = "privacyChoices",
            controller = "shop",
            secure = "true"
        )
        @Response(name = "success", type = "view", value = "privacyChoices")
        public interface PrivacyChoices {}

        @Request(
            uri = "withdraw",
            controller = "shop",
            secure = "true"
        )
        @Response(name = "success", type = "view", value = "withdraw")
        public interface Withdraw {}

        @Request(
            uri = "seller",
            controller = "shop",
            secure = "true"
        )
        @Response(name = "success", type = "view", value = "seller")
        public interface Seller {}

        @Request(
            uri = "privacyCenter",
            controller = "shop",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "privacyCenter")
        public interface PrivacyCenter {}

        @Request(
            uri = "privacyRequest",
            controller = "shop",
            secure = "true"
        )
        @Response(name = "success", type = "view", value = "privacyRequest")
        public interface PrivacyRequest {}

        @Request(
            uri = "privacyDeleted",
            controller = "shop",
            secure = "true"
        )
        @Response(name = "success", type = "view", value = "privacyDeleted")
        public interface PrivacyDeleted {}

        @Request(
            uri = "privacyExport",
            controller = "shop",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "none")
        @Response(name = "error", type = "view", value = "privacyCenter")
        public static String privacyExport(HttpServletRequest request, HttpServletResponse response) {
            // Delegates to: com.ilscipio.scipio.compliance.web.PrivacyEvents.privacyExport
            try { return (String) Class.forName("com.ilscipio.scipio.compliance.web.PrivacyEvents").getMethod("privacyExport", HttpServletRequest.class, HttpServletResponse.class).invoke(null, request, response); } catch (Exception e) { org.ofbiz.base.util.Debug.logError(e, "Error invoking PrivacyEvents.privacyExport", "ControllerDef"); return "error"; }
        }

        @Request(
            uri = "privacyDeleteAccount",
            controller = "shop",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "request-redirect-noparam", value = "privacyDeleted")
        @Response(name = "error", type = "view", value = "privacyCenter")
        public static String privacyDeleteAccount(HttpServletRequest request, HttpServletResponse response) {
            // Delegates to: com.ilscipio.scipio.compliance.web.PrivacyEvents.privacyDeleteAccount
            try { return (String) Class.forName("com.ilscipio.scipio.compliance.web.PrivacyEvents").getMethod("privacyDeleteAccount", HttpServletRequest.class, HttpServletResponse.class).invoke(null, request, response); } catch (Exception e) { org.ofbiz.base.util.Debug.logError(e, "Error invoking PrivacyEvents.privacyDeleteAccount", "ControllerDef"); return "error"; }
        }

        @Request(
            uri = "privacyRequestSubmit",
            controller = "shop",
            secure = "true"
        )
        @Response(name = "success", type = "view", value = "privacyRequest")
        @Response(name = "error", type = "view", value = "privacyRequest")
        public static String privacyRequestSubmit(HttpServletRequest request, HttpServletResponse response) {
            // Delegates to: com.ilscipio.scipio.compliance.web.PrivacyEvents.privacyRequestSubmit
            try { return (String) Class.forName("com.ilscipio.scipio.compliance.web.PrivacyEvents").getMethod("privacyRequestSubmit", HttpServletRequest.class, HttpServletResponse.class).invoke(null, request, response); } catch (Exception e) { org.ofbiz.base.util.Debug.logError(e, "Error invoking PrivacyEvents.privacyRequestSubmit", "ControllerDef"); return "error"; }
        }

        @Request(
            uri = "privacyVerify",
            controller = "shop",
            secure = "true"
        )
        @Response(name = "success", type = "view", value = "privacyRequest")
        @Response(name = "error", type = "view", value = "privacyRequest")
        public static String privacyVerify(HttpServletRequest request, HttpServletResponse response) {
            // Delegates to: com.ilscipio.scipio.compliance.web.PrivacyEvents.privacyVerify
            try { return (String) Class.forName("com.ilscipio.scipio.compliance.web.PrivacyEvents").getMethod("privacyVerify", HttpServletRequest.class, HttpServletResponse.class).invoke(null, request, response); } catch (Exception e) { org.ofbiz.base.util.Debug.logError(e, "Error invoking PrivacyEvents.privacyVerify", "ControllerDef"); return "error"; }
        }

        @Request(
            uri = "withdrawCheck",
            controller = "shop",
            secure = "true"
        )
        @Response(name = "success", type = "view", value = "withdraw")
        @Response(name = "error", type = "view", value = "withdraw")
        public static String withdrawCheck(HttpServletRequest request, HttpServletResponse response) {
            // Delegates to: com.ilscipio.scipio.compliance.web.WithdrawalEvents.withdrawCheck
            try { return (String) Class.forName("com.ilscipio.scipio.compliance.web.WithdrawalEvents").getMethod("withdrawCheck", HttpServletRequest.class, HttpServletResponse.class).invoke(null, request, response); } catch (Exception e) { org.ofbiz.base.util.Debug.logError(e, "Error invoking WithdrawalEvents.withdrawCheck", "ControllerDef"); return "error"; }
        }

        @Request(
            uri = "withdrawSubmit",
            controller = "shop",
            secure = "true"
        )
        @Response(name = "success", type = "view", value = "withdraw")
        @Response(name = "error", type = "view", value = "withdraw")
        public static String withdrawSubmit(HttpServletRequest request, HttpServletResponse response) {
            // Delegates to: com.ilscipio.scipio.compliance.web.WithdrawalEvents.withdrawSubmit
            try { return (String) Class.forName("com.ilscipio.scipio.compliance.web.WithdrawalEvents").getMethod("withdrawSubmit", HttpServletRequest.class, HttpServletResponse.class).invoke(null, request, response); } catch (Exception e) { org.ofbiz.base.util.Debug.logError(e, "Error invoking WithdrawalEvents.withdrawSubmit", "ControllerDef"); return "error"; }
        }

        // SCIPIO: 4.0.0: full EU GARAN label of a product (SVG with brand, model and years filled in)
        @Request(
            uri = "garanLabel",
            controller = "shop",
            secure = "true"
        )
        @Response(name = "success", type = "none")
        public static String garanLabel(HttpServletRequest request, HttpServletResponse response) {
            // Delegates to: com.ilscipio.scipio.compliance.web.WithdrawalEvents.garanLabel
            try { return (String) Class.forName("com.ilscipio.scipio.compliance.web.WithdrawalEvents").getMethod("garanLabel", HttpServletRequest.class, HttpServletResponse.class).invoke(null, request, response); } catch (Exception e) { org.ofbiz.base.util.Debug.logError(e, "Error invoking WithdrawalEvents.garanLabel", "ControllerDef"); return "error"; }
        }

        // SCIPIO: 4.0.0: logs a consent choice (cookie dialog, GPC, Your Privacy Choices) and writes the consent cookie
        @Request(
            uri = "recordConsent",
            controller = "shop",
            secure = "true"
        )
        @Response(name = "success", type = "none")
        public static String recordConsent(HttpServletRequest request, HttpServletResponse response) {
            // Delegates to: com.ilscipio.scipio.compliance.web.ConsentEvents.recordConsent
            try { return (String) Class.forName("com.ilscipio.scipio.compliance.web.ConsentEvents").getMethod("recordConsent", HttpServletRequest.class, HttpServletResponse.class).invoke(null, request, response); } catch (Exception e) { org.ofbiz.base.util.Debug.logError(e, "Error invoking ConsentEvents.recordConsent", "ControllerDef"); return "error"; }
        }

        @Request(
            uri = "ListLocales",
            controller = "shop",
            secure = "true"
        )
        @Response(name = "success", type = "view", value = "ListLocales", saveLastView = "true")
        public interface ListLocales {}

        @Request(
            uri = "setSessionLocale",
            controller = "shop",
            secure = "true"
        )
        @Response(name = "success", type = "request-redirect", value = "fromSetSessionLocale")
        @Response(name = "error", type = "view", value = "main")
        public static String setSessionLocale(HttpServletRequest request, HttpServletResponse response) {
            // Delegates to: org.ofbiz.common.CommonEvents.setSessionLocale
            return CommonEvents.setSessionLocale(request, response);
        }

        @Request(
            uri = "setSessionLocaleProfile",
            controller = "shop",
            secure = "true"
        )
        @Response(name = "success", type = "request-redirect-noparam", value = "viewprofile")
        @Response(name = "error", type = "view", value = "viewprofile")
        public static String setSessionLocaleProfile(HttpServletRequest request, HttpServletResponse response) {
            // Delegates to: org.ofbiz.common.CommonEvents.setSessionLocale
            return CommonEvents.setSessionLocale(request, response);
        }

        @Request(
            uri = "setSessionCurrencyUom",
            controller = "shop",
            secure = "true"
        )
        @Response(name = "success", type = "view-last", value = "main")
        @Response(name = "error", type = "view", value = "main")
        public static String setSessionCurrencyUom(HttpServletRequest request, HttpServletResponse response) {
            // Delegates to: org.ofbiz.common.CommonEvents.setSessionCurrencyUom
            return CommonEvents.setSessionCurrencyUom(request, response);
        }

        @Request(
            uri = "setSessionProductStore",
            controller = "shop",
            secure = "true"
        )
        @Response(name = "success", type = "view-last", value = "main")
        @Response(name = "error", type = "view", value = "main")
        public static String setSessionProductStore(HttpServletRequest request, HttpServletResponse response) {
            // Delegates to: org.ofbiz.order.shoppingcart.product.ProductStoreCartAwareEvents.setSessionProductStore
            return ProductStoreCartAwareEvents.setSessionProductStore(request, response);
        }

        @Request(
            uri = "setdistributor",
            controller = "shop",
            secure = "true"
        )
        @Response(name = "success", type = "view", value = "main")
        @Response(name = "error", type = "view", value = "main")
        public static String setdistributor(HttpServletRequest request, HttpServletResponse response) {
            // Delegates to: com.ilscipio.scipio.shop.misc.ThirdPartyEvents.setAssociationId
            return ThirdPartyEvents.setAssociationId(request, response);
        }

        @Request(
            uri = "editShoppingList",
            controller = "shop",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "editShoppingList")
        public interface EditShoppingList {}

        @Request(
            uri = "createEmptyShoppingList",
            controller = "shop",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "editShoppingList")
        @Response(name = "error", type = "view", value = "editShoppingList")
        public static String createEmptyShoppingList(HttpServletRequest request, HttpServletResponse response) {
            // Delegates to: org.ofbiz.order.shoppinglist.ShoppingListEvents.createEmptyShoppingList
            try { return ShoppingListEvents.createEmptyShoppingList(request, response); } catch (Exception e) { org.ofbiz.base.util.Debug.logError(e, "Error in ShoppingListEvents.createEmptyShoppingList", "ControllerDef"); return "error"; }
        }

        @Request(
            uri = "createShoppingListFromOrder",
            controller = "shop",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "editShoppingList")
        @Response(name = "error", type = "view", value = "orderstatus")
        public static String createShoppingListFromOrder(HttpServletRequest request, HttpServletResponse response) {
            // Delegates to: org.ofbiz.order.shoppinglist.ShoppingListEvents.createShoppingListFromOrder
            try { return ShoppingListEvents.createShoppingListFromOrder(request, response); } catch (Exception e) { org.ofbiz.base.util.Debug.logError(e, "Error in ShoppingListEvents.createShoppingListFromOrder", "ControllerDef"); return "error"; }
        }

        @Request(
            uri = "updateShoppingList",
            controller = "shop",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "editShoppingList")
        @Response(name = "error", type = "view", value = "editShoppingList")
        public static String updateShoppingList(HttpServletRequest request, HttpServletResponse response) {
            // Delegates to: org.ofbiz.order.shoppinglist.ShoppingListEvents.updateShoppingList
            try { return ShoppingListEvents.updateShoppingList(request, response); } catch (Exception e) { org.ofbiz.base.util.Debug.logError(e, "Error in ShoppingListEvents.updateShoppingList", "ControllerDef"); return "error"; }
        }

        @Request(
            uri = "addItemToShoppingList",
            controller = "shop",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "editShoppingList")
        @Response(name = "error", type = "view", value = "editShoppingList")
        public static String addItemToShoppingList(HttpServletRequest request, HttpServletResponse response) {
            // Delegates to: org.ofbiz.order.shoppinglist.ShoppingListEvents.addItemToShoppingList
            try { return ShoppingListEvents.addItemToShoppingList(request, response); } catch (Exception e) { org.ofbiz.base.util.Debug.logError(e, "Error in ShoppingListEvents.addItemToShoppingList", "ControllerDef"); return "error"; }
        }

        @Request(
            uri = "addItemToDefaultShoppingList",
            controller = "shop",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "editShoppingList")
        @Response(name = "error", type = "view", value = "editShoppingList")
        public static String addItemToDefaultShoppingList(HttpServletRequest request, HttpServletResponse response) {
            // Delegates to: org.ofbiz.order.shoppinglist.ShoppingListEvents.addItemToDefaultShoppingList
            try { return ShoppingListEvents.addItemToDefaultShoppingList(request, response); } catch (Exception e) { org.ofbiz.base.util.Debug.logError(e, "Error in ShoppingListEvents.addItemToDefaultShoppingList", "ControllerDef"); return "error"; }
        }

        @Request(
            uri = "addBulkToShoppingList",
            controller = "shop",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "editShoppingList")
        @Response(name = "error", type = "view", value = "showcart")
        public static String addBulkToShoppingList(HttpServletRequest request, HttpServletResponse response) {
            // Delegates to: org.ofbiz.order.shoppinglist.ShoppingListEvents.addBulkFromCart
            return ShoppingListEvents.addBulkFromCart(request, response);
        }

        @Request(
            uri = "addListToCart",
            controller = "shop",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "showcart")
        @Response(name = "error", type = "view", value = "editShoppingList")
        public static String addListToCart(HttpServletRequest request, HttpServletResponse response) {
            // Delegates to: org.ofbiz.order.shoppinglist.ShoppingListEvents.addListToCart
            return ShoppingListEvents.addListToCart(request, response);
        }

        @Request(
            uri = "updateShoppingListItem",
            controller = "shop",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "editShoppingList")
        @Response(name = "error", type = "view", value = "editShoppingList")
        public static String updateShoppingListItem(HttpServletRequest request, HttpServletResponse response) {
            // Delegates to: org.ofbiz.order.shoppinglist.ShoppingListEvents.updateShoppingListItem
            try { return ShoppingListEvents.updateShoppingListItem(request, response); } catch (Exception e) { org.ofbiz.base.util.Debug.logError(e, "Error in ShoppingListEvents.updateShoppingListItem", "ControllerDef"); return "error"; }
        }

        @Request(
            uri = "removeFromShoppingList",
            controller = "shop",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "editShoppingList")
        @Response(name = "error", type = "view", value = "editShoppingList")
        public static String removeFromShoppingList(HttpServletRequest request, HttpServletResponse response) {
            // Delegates to: org.ofbiz.order.shoppinglist.ShoppingListEvents.removeFromShoppingList
            try { return ShoppingListEvents.removeFromShoppingList(request, response); } catch (Exception e) { org.ofbiz.base.util.Debug.logError(e, "Error in ShoppingListEvents.removeFromShoppingList", "ControllerDef"); return "error"; }
        }

        @Request(
            uri = "removeFromDefaultShoppingList",
            controller = "shop",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "editShoppingList")
        @Response(name = "error", type = "view", value = "editShoppingList")
        public static String removeFromDefaultShoppingList(HttpServletRequest request, HttpServletResponse response) {
            // Delegates to: org.ofbiz.order.shoppinglist.ShoppingListEvents.removeFromDefaultShoppingList
            try { return ShoppingListEvents.removeFromDefaultShoppingList(request, response); } catch (Exception e) { org.ofbiz.base.util.Debug.logError(e, "Error in ShoppingListEvents.removeFromDefaultShoppingList", "ControllerDef"); return "error"; }
        }

        @Request(
            uri = "replaceShoppingListItem",
            controller = "shop",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "editShoppingList")
        @Response(name = "error", type = "view", value = "editShoppingList")
        public static String replaceShoppingListItem(HttpServletRequest request, HttpServletResponse response) {
            // Delegates to: org.ofbiz.order.shoppinglist.ShoppingListEvents.replaceShoppingListItem
            return ShoppingListEvents.replaceShoppingListItem(request, response);
        }

    }

    // Auto-generated split (Part 6)
    public static class Part6 {
        @Request(
            uri = "addpromocode",
            controller = "shop",
            secure = "true"
        )
        @Response(name = "success", type = "view", value = "showcart")
        @Response(name = "error", type = "view", value = "showcart")
        public static String addpromocode(HttpServletRequest request, HttpServletResponse response) {
            // Delegates to: org.ofbiz.order.shoppingcart.ShoppingCartEvents.addProductPromoCode
            return ShoppingCartEvents.addProductPromoCode(request, response);
        }

        @Request(
            uri = "additem",
            controller = "shop",
            secure = "true"
        )
        @Response(name = "success", type = "view-last", value = "showcart", saveLastView = "true")
        @Response(name = "survey", type = "view", value = "survey", allowViewSave = "false")
        @Response(name = "product", type = "view", value = "product")
        @Response(name = "viewcart", type = "request-redirect", value = "showcart")
        @Response(name = "error", type = "view-last", value = "showcart")
        public static String additem(HttpServletRequest request, HttpServletResponse response) {
            // Delegates to: org.ofbiz.order.shoppingcart.ShoppingCartEvents.addToCart
            return ShoppingCartEvents.addToCart(request, response);
        }

        @Request(
            uri = "additemsurvey",
            controller = "shop",
            secure = "true"
        )
        @Response(name = "success", type = "request", value = "additem")
        @Response(name = "error", type = "view", value = "survey", allowViewSave = "false")
        public static String additemsurvey(HttpServletRequest request, HttpServletResponse response) {
            // Delegates to: org.ofbiz.content.survey.SurveyEvents.createSurveyResponseAndRestoreParameters
            return SurveyEvents.createSurveyResponseAndRestoreParameters(request, response);
        }

        @Request(
            uri = "addordertocart",
            controller = "shop",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "showcart")
        @Response(name = "error", type = "view", value = "orderstatus")
        public static String addordertocart(HttpServletRequest request, HttpServletResponse response) {
            // Delegates to: org.ofbiz.order.shoppingcart.ShoppingCartEvents.addToCartFromOrder
            return ShoppingCartEvents.addToCartFromOrder(request, response);
        }

        @Request(
            uri = "addtocartbulk",
            controller = "shop",
            secure = "true"
        )
        @Response(name = "success", type = "view", value = "quickadd")
        @Response(name = "error", type = "view", value = "quickadd")
        public static String addtocartbulk(HttpServletRequest request, HttpServletResponse response) {
            // Delegates to: org.ofbiz.order.shoppingcart.ShoppingCartEvents.addToCartBulk
            return ShoppingCartEvents.addToCartBulk(request, response);
        }

        @Request(
            uri = "addCategoryDefaults",
            controller = "shop",
            secure = "true"
        )
        @Response(name = "success", type = "view", value = "showcart")
        @Response(name = "error", type = "view", value = "showcart")
        public static String addCategoryDefaults(HttpServletRequest request, HttpServletResponse response) {
            // Delegates to: org.ofbiz.order.shoppingcart.ShoppingCartEvents.addCategoryDefaults
            return ShoppingCartEvents.addCategoryDefaults(request, response);
        }

        @Request(
            uri = "addseperator",
            controller = "shop",
            secure = "true"
        )
        @Response(name = "success", type = "view", value = "showcart")
        @Response(name = "error", type = "view", value = "showcart")
        public static String addseperator(HttpServletRequest request, HttpServletResponse response) {
            // Delegates to: org.ofbiz.order.shoppingcart.ShoppingCartEvents.addSeparator
            // NOTE: Method does not exist in ShoppingCartEvents - pre-existing issue in controller.xml
            return "success";
        }

        @Request(
            uri = "showcart",
            controller = "shop",
            secure = "true"
        )
        @Response(name = "success", type = "view", value = "showcart")
        public interface Showcart {}

        @Request(
            uri = "modifycart",
            controller = "shop",
            secure = "true"
        )
        @Response(name = "success", type = "view", value = "showcart")
        @Response(name = "error", type = "view", value = "showcart")
        public static String modifycart(HttpServletRequest request, HttpServletResponse response) {
            // Delegates to: org.ofbiz.order.shoppingcart.ShoppingCartEvents.modifyCart
            return ShoppingCartEvents.modifyCart(request, response);
        }

        @Request(
            uri = "emptycart",
            controller = "shop",
            secure = "true"
        )
        @Response(name = "success", type = "view", value = "showcart")
        @Response(name = "error", type = "view", value = "showcart")
        public static String emptycart(HttpServletRequest request, HttpServletResponse response) {
            // Delegates to: org.ofbiz.order.shoppingcart.ShoppingCartEvents.clearEnsureCart
            return ShoppingCartEvents.clearEnsureCart(request, response);
        }

        @Request(
            uri = "UpdateCart",
            controller = "shop",
            secure = "true"
        )
        @Response(name = "success", type = "view", value = "UpdateCart")
        public interface UpdateCart {}

        @Request(
            uri = "loadCartFromAbandonedCart",
            controller = "shop",
            secure = "true"
        )
        @Response(name = "error", type = "view", value = "showcart")
        @Response(name = "success", type = "view", value = "showcart")
        public static String loadCartFromAbandonedCart(HttpServletRequest request, HttpServletResponse response) {
            // Delegates to: org.ofbiz.order.shoppingcart.ShoppingCartEvents.loadCartFromAbandonedCart
            return ShoppingCartEvents.loadCartFromAbandonedCart(request, response);
        }

        @Request(
            uri = "setDesiredAlternateGwpProductId",
            controller = "shop",
            secure = "true"
        )
        @Response(name = "success", type = "view", value = "showcart")
        @Response(name = "error", type = "view", value = "showcart")
        public static String setDesiredAlternateGwpProductId(HttpServletRequest request, HttpServletResponse response) {
            // Delegates to: org.ofbiz.order.shoppingcart.ShoppingCartEvents.setDesiredAlternateGwpProductId
            return ShoppingCartEvents.setDesiredAlternateGwpProductId(request, response);
        }

        @Request(
            uri = "showAllPromotions",
            controller = "shop",
            secure = "true"
        )
        @Response(name = "success", type = "view", value = "showAllPromotions")
        public interface ShowAllPromotions {}

        @Request(
            uri = "showPromotionDetails",
            controller = "shop",
            secure = "true"
        )
        @Response(name = "success", type = "view", value = "showPromotionDetails")
        public interface ShowPromotionDetails {}

        @Request(
            uri = "removePromotion",
            controller = "shop",
            secure = "true"
        )
        @Response(name = "success", type = "view", value = "showcart")
        @Response(name = "error", type = "view", value = "showcart")
        public static String removePromotion(HttpServletRequest request, HttpServletResponse response) {
            // Delegates to: org.ofbiz.order.shoppingcart.ShoppingCartEvents.removePromotion
            return ShoppingCartEvents.removePromotion(request, response);
        }

        @Request(
            uri = "setCustomer",
            controller = "shop",
            secure = "true"
        )
        @Response(name = "success", type = "view", value = "custsetting")
        public interface SetCustomer {}

        @Request(
            uri = "processCustomerSettings",
            controller = "shop",
            secure = "true"
        )
        @Response(name = "success", type = "request", value = "anonCheckShipmentNeeded")
        @Response(name = "error", type = "view", value = "custsetting")
        @Event(type = "simple", path = "component://shop/script/com/ilscipio/scipio/shop/customer/CustomerEvents.xml", invoke = "processCustomerSettings")
        public static String processCustomerSettings(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "anonCheckShipmentNeeded",
            controller = "shop",
            directRequest = "false"
        )
        @Response(name = "shipmentNeeded", type = "request", value = "setShipping")
        @Response(name = "shipmentNotNeeded", type = "request", value = "setPaymentOption")
        @Response(name = "error", type = "view", value = "custsetting")
        public static String anonCheckShipmentNeeded(HttpServletRequest request, HttpServletResponse response) {
            // Delegates to: org.ofbiz.order.shoppingcart.CheckOutEvents.checkShipmentNeeded
            return CheckOutEvents.checkShipmentNeeded(request, response);
        }

        @Request(
            uri = "setShipping",
            controller = "shop",
            secure = "true"
        )
        @Response(name = "success", type = "view", value = "shipsetting")
        public interface SetShipping {}

    }

    // Auto-generated split (Part 7)
    public static class Part7 {
        @Request(
            uri = "processShipSettings",
            controller = "shop",
            secure = "true"
        )
        @Response(name = "success", type = "request", value = "setShipOptions")
        @Response(name = "error", type = "view", value = "shipsetting")
        @Event(type = "simple", path = "component://shop/script/com/ilscipio/scipio/shop/customer/CustomerEvents.xml", invoke = "processShipSettings")
        public static String processShipSettings(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "setShipOptions",
            controller = "shop",
            secure = "true"
        )
        @Response(name = "success", type = "view", value = "optionsetting")
        public interface SetShipOptions {}

        @Request(
            uri = "processShipOptions",
            controller = "shop",
            secure = "true"
        )
        @Response(name = "success", type = "request", value = "setShippingBeforePayment")
        @Response(name = "error", type = "view", value = "optionsetting")
        @Event(type = "simple", path = "component://shop/script/com/ilscipio/scipio/shop/customer/CustomerEvents.xml", invoke = "processShipOptions")
        public static String processShipOptions(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "setShippingBeforePayment",
            controller = "shop",
            directRequest = "false"
        )
        @Response(name = "success", type = "request", value = "setTaxBeforePayment")
        @Response(name = "error", type = "view", value = "optionsetting")
        public static String setShippingBeforePayment(HttpServletRequest request, HttpServletResponse response) {
            // Delegates to: org.ofbiz.order.shoppingcart.shipping.ShippingEvents.getShipEstimate
            return ShippingEvents.getShipEstimate(request, response);
        }

        @Request(
            uri = "setTaxBeforePayment",
            controller = "shop",
            directRequest = "false"
        )
        @Response(name = "success", type = "request", value = "setPaymentOption")
        @Response(name = "error", type = "view", value = "optionsetting")
        public static String setTaxBeforePayment(HttpServletRequest request, HttpServletResponse response) {
            // Delegates to: org.ofbiz.order.shoppingcart.CheckOutEvents.calcTax
            return CheckOutEvents.calcTax(request, response);
        }

        @Request(
            uri = "setPaymentOption",
            controller = "shop",
            secure = "true"
        )
        @Response(name = "success", type = "view", value = "paymentoptions")
        public interface SetPaymentOption {}

        @Request(
            uri = "setPaymentInformation",
            controller = "shop",
            secure = "true"
        )
        @Response(name = "success", type = "view", value = "paymentinformation")
        @Response(name = "paypal", type = "request", value = "setPayPalCheckout")
        public static String setPaymentInformation(HttpServletRequest request, HttpServletResponse response) {
            // Delegates to: org.ofbiz.order.shoppingcart.CheckOutEvents.checkExternalCheckout
            return CheckOutEvents.checkExternalCheckout(request, response);
        }

        @Request(
            uri = "enterCreditCardAndBillingAddress",
            controller = "shop",
            secure = "true"
        )
        @Response(name = "success", type = "request", value = "processPaymentSettings")
        @Response(name = "error", type = "view", value = "paymentinformation")
        @Event(type = "service", invoke = "createCreditCardAndAddress")
        public static String enterCreditCardAndBillingAddress(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "enterCreditCard",
            controller = "shop",
            secure = "true"
        )
        @Response(name = "success", type = "request", value = "processPaymentSettings")
        @Response(name = "error", type = "view", value = "paymentinformation")
        @Event(type = "service", invoke = "createCreditCard")
        public static String enterCreditCard(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "changeCreditCardAndBillingAddress",
            controller = "shop",
            secure = "true"
        )
        @Response(name = "success", type = "request", value = "processPaymentSettings")
        @Response(name = "error", type = "view", value = "paymentinformation")
        @Event(type = "service", invoke = "updateCreditCardAndAddress")
        public static String changeCreditCardAndBillingAddress(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "enterEftAccountAndBillingAddress",
            controller = "shop",
            secure = "true"
        )
        @Response(name = "success", type = "request", value = "processPaymentSettings")
        @Response(name = "error", type = "view", value = "paymentinformation")
        @Event(type = "service", invoke = "createEftAccountAndAddress")
        public static String enterEftAccountAndBillingAddress(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "enterEftAccount",
            controller = "shop",
            secure = "true"
        )
        @Response(name = "success", type = "request", value = "processPaymentSettings")
        @Response(name = "error", type = "view", value = "paymentinformation")
        @Event(type = "service", invoke = "createEftAccount")
        public static String enterEftAccount(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "changeEftAccountAndBillingAddress",
            controller = "shop",
            secure = "true"
        )
        @Response(name = "success", type = "request", value = "processPaymentSettings")
        @Response(name = "error", type = "view", value = "paymentinformation")
        @Event(type = "service", invoke = "updateEftAccountAndAddress")
        public static String changeEftAccountAndBillingAddress(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "processPaymentSettings",
            controller = "shop",
            secure = "true"
        )
        @Response(name = "success", type = "request", value = "reviewOrder")
        @Response(name = "error", type = "view", value = "paymentinformation")
        @Event(type = "groovy", path = "component://shop/script/com/ilscipio/scipio/shop/order/ProcessPaymentSettings.groovy")
        public static String processPaymentSettings(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "reviewOrder",
            controller = "shop",
            secure = "true"
        )
        @Response(name = "success", type = "view", value = "orderreview")
        public interface ReviewOrder {}

        @Request(
            uri = "createOrder",
            controller = "shop",
            secure = "true"
        )
        @Response(name = "error", type = "view", value = "checkoutreview")
        @Response(name = "success", type = "view", value = "checkoutreview")
        public static String createOrder(HttpServletRequest request, HttpServletResponse response) {
            // Delegates to: org.ofbiz.order.shoppingcart.CheckOutEvents.createOrder
            return CheckOutEvents.createOrder(request, response);
        }

        @Request(
            uri = "quickAnonCheckout",
            controller = "shop",
            secure = "true"
        )
        @Response(name = "success", type = "request", value = "quickAnonSetCustomer")
        @Response(name = "error", type = "view", value = "main")
        public static String quickAnonCheckout(HttpServletRequest request, HttpServletResponse response) {
            // Delegates to: org.ofbiz.order.shoppingcart.CheckOutEvents.cartNotEmpty
            return CheckOutEvents.cartNotEmpty(request, response);
        }

        @Request(
            uri = "quickAnonSetCustomer",
            controller = "shop",
            secure = "true"
        )
        @Response(name = "success", type = "view", value = "quickAnonCustSetting")
        public interface QuickAnonSetCustomer {}

        @Request(
            uri = "quickAnonProcessCustomerSettings",
            controller = "shop",
            secure = "true"
        )
        @Response(name = "success", type = "request", value = "quickAnonOrderReview")
        @Response(name = "error", type = "view", value = "quickAnonCustSetting")
        @Event(type = "simple", path = "component://shop/script/com/ilscipio/scipio/shop/customer/QuickAnonCustomerEvents.xml", invoke = "createUpdateCustomer")
        public static String quickAnonProcessCustomerSettings(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "quickAnonSetShipOptions",
            controller = "shop",
            secure = "true"
        )
        @Response(name = "success", type = "view", value = "quickAnonOptionSetting")
        public interface QuickAnonSetShipOptions {}

    }

    // Auto-generated split (Part 8)
    public static class Part8 {
        @Request(
            uri = "quickAnonProcessShipOptions",
            controller = "shop",
            secure = "true"
        )
        @Response(name = "success", type = "view", value = "quickAnonOptionSetting")
        @Response(name = "error", type = "view", value = "quickAnonOptionSetting")
        @Event(type = "simple", path = "component://shop/script/com/ilscipio/scipio/shop/customer/QuickAnonCustomerEvents.xml", invoke = "processShipOptions")
        public static String quickAnonProcessShipOptions(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "quickAnonProcessShipOptionsUpdateOrderItems",
            controller = "shop",
            secure = "true"
        )
        @Response(name = "success", type = "request", value = "quickAnonUpdateShippingChargeOrderItems")
        @Response(name = "error", type = "view", value = "quickAnonOrderItems")
        @Event(type = "simple", path = "component://shop/script/com/ilscipio/scipio/shop/customer/QuickAnonCustomerEvents.xml", invoke = "processShipOptions")
        public static String quickAnonProcessShipOptionsUpdateOrderItems(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "quickAnonUpdateShippingChargeOrderItems",
            controller = "shop",
            secure = "true"
        )
        @Response(name = "success", type = "view", value = "quickAnonOrderItems")
        @Response(name = "error", type = "view", value = "quickAnonOrderItems")
        public static String quickAnonUpdateShippingChargeOrderItems(HttpServletRequest request, HttpServletResponse response) {
            // Delegates to: org.ofbiz.order.shoppingcart.shipping.ShippingEvents.getShipEstimate
            return ShippingEvents.getShipEstimate(request, response);
        }

        @Request(
            uri = "quickAnonSetShippingBeforePayment",
            controller = "shop",
            directRequest = "false"
        )
        @Response(name = "success", type = "request", value = "quickAnonSetTaxBeforePayment")
        @Response(name = "error", type = "view", value = "quickAnonOptionSetting")
        public static String quickAnonSetShippingBeforePayment(HttpServletRequest request, HttpServletResponse response) {
            // Delegates to: org.ofbiz.order.shoppingcart.shipping.ShippingEvents.getShipEstimate
            return ShippingEvents.getShipEstimate(request, response);
        }

        @Request(
            uri = "quickAnonSetTaxBeforePayment",
            controller = "shop",
            directRequest = "false"
        )
        @Response(name = "success", type = "view", value = "quickAnonOrderReview")
        @Response(name = "error", type = "view", value = "quickAnonCustSetting")
        public static String quickAnonSetTaxBeforePayment(HttpServletRequest request, HttpServletResponse response) {
            // Delegates to: org.ofbiz.order.shoppingcart.CheckOutEvents.calcTax
            return CheckOutEvents.calcTax(request, response);
        }

        @Request(
            uri = "quickAnonEnterCreditCard",
            controller = "shop",
            secure = "true"
        )
        @Response(name = "success", type = "request", value = "quickAnonAddCreditCardToCart")
        @Response(name = "error", type = "view", value = "quickAnonCcInfo")
        @Event(type = "service", invoke = "createCreditCard")
        public static String quickAnonEnterCreditCard(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "quickAnonAddCreditCardToCart",
            controller = "shop",
            secure = "true"
        )
        @Response(name = "success", type = "view", value = "quickAnonCcInfo")
        @Response(name = "error", type = "view", value = "quickAnonCcInfo")
        @Event(type = "groovy", path = "component://shop/script/com/ilscipio/scipio/shop/order/ProcessPaymentSettings.groovy")
        public static String quickAnonAddCreditCardToCart(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "quickAnonEnterEftAccount",
            controller = "shop",
            secure = "true"
        )
        @Response(name = "success", type = "request", value = "quickAnonAddEftAccountToCart")
        @Response(name = "error", type = "view", value = "quickAnonEftInfo")
        @Event(type = "service", invoke = "createEftAccount")
        public static String quickAnonEnterEftAccount(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "quickAnonAddEftAccountToCart",
            controller = "shop",
            secure = "true"
        )
        @Response(name = "success", type = "view", value = "quickAnonEftInfo")
        @Response(name = "error", type = "view", value = "quickAnonEftInfo")
        @Event(type = "groovy", path = "component://shop/script/com/ilscipio/scipio/shop/order/ProcessPaymentSettings.groovy")
        public static String quickAnonAddEftAccountToCart(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "quickAnonEnterExtOffline",
            controller = "shop",
            secure = "true"
        )
        @Response(name = "success", type = "view", value = "quickAnonOrderReview")
        @Response(name = "error", type = "view", value = "quickAnonOrderReview")
        @Event(type = "groovy", path = "component://shop/script/com/ilscipio/scipio/shop/order/ProcessPaymentSettings.groovy")
        public static String quickAnonEnterExtOffline(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "quickAnonAddGiftCardToCart",
            controller = "shop",
            secure = "true"
        )
        @Response(name = "success", type = "request", value = "quickAnonGcInfo")
        @Response(name = "error", type = "view", value = "quickAnonGcInfo")
        @Event(type = "groovy", path = "component://shop/script/com/ilscipio/scipio/shop/order/ProcessPaymentSettings.groovy")
        public static String quickAnonAddGiftCardToCart(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "quickAnonOrderReview",
            controller = "shop",
            secure = "true"
        )
        @Response(name = "success", type = "request", value = "quickAnonSetTaxBeforePayment")
        public interface QuickAnonOrderReview {}

        @Request(
            uri = "quickAnonCcInfo",
            controller = "shop",
            secure = "true"
        )
        @Response(name = "success", type = "view", value = "quickAnonCcInfo")
        public interface QuickAnonCcInfo {}

        @Request(
            uri = "quickAnonEftInfo",
            controller = "shop",
            secure = "true"
        )
        @Response(name = "success", type = "view", value = "quickAnonEftInfo")
        public interface QuickAnonEftInfo {}

        @Request(
            uri = "quickAnonGcInfo",
            controller = "shop",
            secure = "true"
        )
        @Response(name = "success", type = "view", value = "quickAnonGcInfo")
        public interface QuickAnonGcInfo {}

        @Request(
            uri = "quickAnonProcessOrder",
            controller = "shop",
            secure = "true"
        )
        @Response(name = "success", type = "view", value = "quickAnonGcInfo")
        public interface QuickAnonProcessOrder {}

        @Request(
            uri = "checkoutreview",
            controller = "shop",
            secure = "true"
        )
        @Response(name = "success", type = "request", value = "calcShipping")
        public interface Checkoutreview {}

        @Request(
            uri = "checkoutpayment",
            controller = "shop",
            secure = "true"
        )
        @Response(name = "success", type = "request", value = "calcShippingBeforePayment")
        public interface Checkoutpayment {}

        @Request(
            uri = "checkoutshippingaddress",
            controller = "shop",
            secure = "true"
        )
        @Response(name = "success", type = "view", value = "checkoutshippingaddress")
        public interface Checkoutshippingaddress {}

        @Request(
            uri = "checkoutshippingoptions",
            controller = "shop",
            secure = "true"
        )
        @Response(name = "success", type = "view", value = "checkoutshippingoptions")
        public interface Checkoutshippingoptions {}

    }

    // Auto-generated split (Part 9)
    public static class Part9 {
        @Request(
            uri = "checkoutoptionslogin",
            controller = "shop",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "request", value = "checkoutoptions")
        public interface Checkoutoptionslogin {}

        @Request(
            uri = "anoncheckoutoptions",
            controller = "shop",
            secure = "true"
        )
        @Response(name = "success", type = "request", value = "checkoutoptions")
        public interface Anoncheckoutoptions {}

        @Request(
            uri = "checkoutoptions",
            controller = "shop",
            secure = "true"
        )
        @Response(name = "success", type = "request", value = "checkoutoptionscore")
        @Response(name = "error", type = "request", value = "checkouterror")
        @Event(type = "simple", path = "component://shop/script/com/ilscipio/scipio/shop/customer/CustomerEvents.xml", invoke = "checkCreateUpdateAnonUser")
        public static String checkoutoptions(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "checkoutoptionscore",
            controller = "shop",
            directRequest = "false"
        )
        @Response(name = "shippingaddress", type = "view", value = "checkoutshippingaddress", saveCurrentView = "true")
        @Response(name = "shippingoptions", type = "request", value = "setOrderCurrencyAgreementShipDates")
        @Response(name = "payment", type = "request", value = "setPoNumber")
        @Response(name = "confirm", type = "request", value = "calcShipping")
        @Response(name = "success", type = "view", value = "checkoutshippingaddress")
        @Response(name = "error", type = "request", value = "checkouterror")
        public static String checkoutoptionscore(HttpServletRequest request, HttpServletResponse response) {
            // Delegates to: org.ofbiz.order.shoppingcart.CheckOutEvents.setCheckOutPages
            return CheckOutEvents.setCheckOutPages(request, response);
        }

        @Request(
            uri = "setOrderCurrencyAgreementShipDates",
            controller = "shop",
            secure = "true"
        )
        @Response(name = "success", type = "view", value = "checkoutshippingoptions")
        @Response(name = "error", type = "request", value = "checkouterror")
        public static String setOrderCurrencyAgreementShipDates(HttpServletRequest request, HttpServletResponse response) {
            // Delegates to: org.ofbiz.order.shoppingcart.ShoppingCartEvents.setOrderCurrencyAgreementShipDates
            return ShoppingCartEvents.setOrderCurrencyAgreementShipDates(request, response);
        }

        @Request(
            uri = "setPoNumber",
            controller = "shop",
            directRequest = "false"
        )
        @Response(name = "success", type = "request", value = "calcShippingBeforePayment")
        public static String setPoNumber(HttpServletRequest request, HttpServletResponse response) {
            // Delegates to: org.ofbiz.order.shoppingcart.ShoppingCartEvents.setPoNumber
            return ShoppingCartEvents.setPoNumber(request, response);
        }

        @Request(
            uri = "checkouterror",
            controller = "shop",
            secure = "true"
        )
        @Response(name = "shippingaddress", type = "view", value = "checkoutshippingaddress")
        @Response(name = "shippingoptions", type = "view", value = "checkoutshippingoptions")
        @Response(name = "payment", type = "view", value = "checkoutpayment")
        @Response(name = "confirm", type = "request", value = "calcShipping")
        @Response(name = "quick", type = "view", value = "checkoutshippingaddress")
        @Response(name = "error", type = "view", value = "checkoutshippingaddress")
        @Response(name = "success", type = "view", value = "checkoutshippingaddress")
        public static String checkouterror(HttpServletRequest request, HttpServletResponse response) {
            // Delegates to: org.ofbiz.order.shoppingcart.CheckOutEvents.setCheckOutError
            return CheckOutEvents.setCheckOutError(request, response);
        }

        @Request(
            uri = "checkoutpaymenterror",
            controller = "shop",
            secure = "true"
        )
        @Response(name = "success", type = "view", value = "checkoutpayment")
        @Response(name = "error", type = "view", value = "checkoutpayment")
        public interface Checkoutpaymenterror {}

        @Request(
            uri = "splitship",
            controller = "shop",
            secure = "true"
        )
        @Response(name = "success", type = "view", value = "splitship")
        public interface Splitship {}

        @Request(
            uri = "updatesplit",
            controller = "shop",
            secure = "true"
        )
        @Response(name = "success", type = "view", value = "splitship")
        @Response(name = "error", type = "view", value = "splitship")
        @Event(type = "service", invoke = "assignItemShipGroup")
        public static String updatesplit(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "checkout",
            controller = "shop",
            secure = "true"
        )
        @Response(name = "success", type = "request", value = "setOptions")
        @Response(name = "error", type = "view", value = "showcart")
        public static String checkout(HttpServletRequest request, HttpServletResponse response) {
            // Delegates to: org.ofbiz.order.shoppingcart.CheckOutEvents.cartNotEmpty
            return CheckOutEvents.cartNotEmpty(request, response);
        }

        @Request(
            uri = "updateCheckoutOptions",
            controller = "shop",
            secure = "true"
        )
        @Response(name = "success", type = "view", value = "checkoutshippingaddress")
        @Response(name = "error", type = "request", value = "checkouterror")
        public static String updateCheckoutOptions(HttpServletRequest request, HttpServletResponse response) {
            // Delegates to: org.ofbiz.order.shoppingcart.CheckOutEvents.setPartialCheckOutOptions
            return CheckOutEvents.setPartialCheckOutOptions(request, response);
        }

        @Request(
            uri = "setOptions",
            controller = "shop",
            secure = "true",
            directRequest = "false"
        )
        @Response(name = "success", type = "request", value = "calcShipping")
        @Response(name = "error", type = "request", value = "checkouterror")
        public static String setOptions(HttpServletRequest request, HttpServletResponse response) {
            // Delegates to: org.ofbiz.order.shoppingcart.CheckOutEvents.setCheckOutOptions
            return CheckOutEvents.setCheckOutOptions(request, response);
        }

        @Request(
            uri = "updateShippingAddress",
            controller = "shop",
            secure = "true"
        )
        @Response(name = "success", type = "view", value = "splitship")
        @Response(name = "error", type = "request", value = "splitship")
        @Event(type = "service", invoke = "setCartShippingAddress")
        public static String updateShippingAddress(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "updateShippingOptions",
            controller = "shop",
            secure = "true"
        )
        @Response(name = "success", type = "view", value = "splitship")
        @Response(name = "error", type = "request", value = "splitship")
        @Event(type = "service", invoke = "setCartShippingOptions")
        public static String updateShippingOptions(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "calcShipping",
            controller = "shop",
            directRequest = "false"
        )
        @Response(name = "success", type = "request", value = "calcTax")
        @Response(name = "error", type = "request", value = "checkouterror")
        public static String calcShipping(HttpServletRequest request, HttpServletResponse response) {
            // Delegates to: org.ofbiz.order.shoppingcart.shipping.ShippingEvents.getShipEstimate
            return ShippingEvents.getShipEstimate(request, response);
        }

        @Request(
            uri = "calcTax",
            controller = "shop",
            directRequest = "false"
        )
        @Response(name = "success", type = "request", value = "validatePaymentMethods")
        @Response(name = "error", type = "request", value = "checkouterror")
        public static String calcTax(HttpServletRequest request, HttpServletResponse response) {
            // Delegates to: org.ofbiz.order.shoppingcart.CheckOutEvents.calcTax
            return CheckOutEvents.calcTax(request, response);
        }

        @Request(
            uri = "validatePaymentMethods",
            controller = "shop",
            directRequest = "false"
        )
        @Response(name = "success", type = "view", value = "checkoutreview")
        @Response(name = "error", type = "request", value = "checkouterror")
        public static String validatePaymentMethods(HttpServletRequest request, HttpServletResponse response) {
            // Delegates to: org.ofbiz.order.shoppingcart.CheckOutEvents.checkPaymentMethods
            return CheckOutEvents.checkPaymentMethods(request, response);
        }

        @Request(
            uri = "calcShippingBeforePayment",
            controller = "shop",
            directRequest = "false"
        )
        @Response(name = "success", type = "request", value = "calcTaxBeforePayment")
        @Response(name = "error", type = "request", value = "checkouterror")
        public static String calcShippingBeforePayment(HttpServletRequest request, HttpServletResponse response) {
            // Delegates to: org.ofbiz.order.shoppingcart.shipping.ShippingEvents.getShipEstimate
            return ShippingEvents.getShipEstimate(request, response);
        }

        @Request(
            uri = "calcTaxBeforePayment",
            controller = "shop",
            directRequest = "false"
        )
        @Response(name = "success", type = "request", value = "validatePaymentMethodsBeforePayment")
        @Response(name = "error", type = "request", value = "checkouterror")
        public static String calcTaxBeforePayment(HttpServletRequest request, HttpServletResponse response) {
            // Delegates to: org.ofbiz.order.shoppingcart.CheckOutEvents.calcTax
            return CheckOutEvents.calcTax(request, response);
        }

    }

    // Auto-generated split (Part 10)
    public static class Part10 {
        @Request(
            uri = "validatePaymentMethodsBeforePayment",
            controller = "shop",
            directRequest = "false"
        )
        @Response(name = "success", type = "view", value = "checkoutpayment")
        @Response(name = "payment", type = "request", value = "checkoutpaymenterror")
        public static String validatePaymentMethodsBeforePayment(HttpServletRequest request, HttpServletResponse response) {
            // Delegates to: org.ofbiz.order.shoppingcart.CheckOutEvents.checkPaymentMethodsBeforePayment
            return CheckOutEvents.checkPaymentMethodsBeforePayment(request, response);
        }

        @Request(
            uri = "checkBlacklist",
            controller = "shop",
            directRequest = "false"
        )
        @Response(name = "success", type = "request", value = "processpayment")
        @Response(name = "failed", type = "request", value = "failedBlacklist")
        @Response(name = "error", type = "view", value = "checkoutreview")
        public static String checkBlacklist(HttpServletRequest request, HttpServletResponse response) {
            // Delegates to: org.ofbiz.order.shoppingcart.CheckOutEvents.checkOrderBlacklist
            return CheckOutEvents.checkOrderBlacklist(request, response);
        }

        @Request(
            uri = "failedBlacklist",
            controller = "shop",
            directRequest = "false"
        )
        @Response(name = "success", type = "view", value = "main")
        @Response(name = "error", type = "view", value = "error")
        public static String failedBlacklist(HttpServletRequest request, HttpServletResponse response) {
            // Delegates to: org.ofbiz.order.shoppingcart.CheckOutEvents.failedBlacklistCheck
            return CheckOutEvents.failedBlacklistCheck(request, response);
        }

        @Request(
            uri = "processorder",
            controller = "shop",
            secure = "true"
        )
        @Response(name = "success", type = "request", value = "processordervalidatepayment")
        @Response(name = "error", type = "request-redirect-noparam", value = "showcart")
        public static String processorder(HttpServletRequest request, HttpServletResponse response) {
            // SCIPIO: 4.0.0: the terms must be accepted first (compliance component)
            try {
                Class<?> c = Class.forName("com.ilscipio.scipio.compliance.web.CheckoutComplianceEvents");
                if ("error".equals(c.getMethod("checkTerms", HttpServletRequest.class, HttpServletResponse.class).invoke(null, request, response))) {
                    return "error";
                }
            } catch (ClassNotFoundException e) {
                // compliance component not installed
            } catch (Exception e) {
                org.ofbiz.base.util.Debug.logError(e, "Error invoking CheckoutComplianceEvents.checkTerms", "ControllerDef");
            }
            // Delegates to: org.ofbiz.order.shoppingcart.CheckOutEvents.cartNotEmpty
            return CheckOutEvents.cartNotEmpty(request, response);
        }

        @Request(
            uri = "processordervalidatepayment",
            controller = "shop",
            directRequest = "false"
        )
        @Response(name = "success", type = "request", value = "processordercreate")
        @Response(name = "payment", type = "request", value = "checkoutpaymenterror")
        public static String processordervalidatepayment(HttpServletRequest request, HttpServletResponse response) {
            // Delegates to: org.ofbiz.order.shoppingcart.CheckOutEvents.checkPaymentMethods
            return CheckOutEvents.checkPaymentMethods(request, response);
        }

        @Request(
            uri = "processordercreate",
            controller = "shop",
            directRequest = "false"
        )
        @Response(name = "sales_order", type = "request", value = "checkBlacklist")
        @Response(name = "work_order", type = "request", value = "checkBlacklist")
        @Response(name = "error", type = "view", value = "checkoutreview")
        public static String processordercreate(HttpServletRequest request, HttpServletResponse response) {
            // Delegates to: org.ofbiz.order.shoppingcart.CheckOutEvents.createOrder
            String result = CheckOutEvents.createOrder(request, response);
            if ("sales_order".equals(result) || "work_order".equals(result)) {
                // SCIPIO: 4.0.0: store the accepted legal text versions with the order (compliance component)
                try {
                    Class.forName("com.ilscipio.scipio.compliance.web.CheckoutComplianceEvents").getMethod("recordOrderConsents", HttpServletRequest.class, HttpServletResponse.class).invoke(null, request, response);
                } catch (ClassNotFoundException e) {
                    // compliance component not installed
                } catch (Exception e) {
                    org.ofbiz.base.util.Debug.logError(e, "Error invoking CheckoutComplianceEvents.recordOrderConsents", "ControllerDef");
                }
            }
            return result;
        }

        @Request(
            uri = "processpayment",
            controller = "shop",
            secure = "true",
            directRequest = "false"
        )
        @Response(name = "success", type = "request", value = "clearcartfororder")
        @Response(name = "fail", type = "request", value = "checkouterror")
        @Response(name = "error", type = "view", value = "checkoutreview")
        public static String processpayment(HttpServletRequest request, HttpServletResponse response) {
            // Delegates to: org.ofbiz.order.shoppingcart.CheckOutEvents.processPayment
            return CheckOutEvents.processPayment(request, response);
        }

        @Request(
            uri = "clearcartfororder",
            controller = "shop",
            directRequest = "false"
        )
        @Response(name = "success", type = "request", value = "checkExternalPayment")
        @Response(name = "error", type = "view", value = "checkoutreview")
        public static String clearcartfororder(HttpServletRequest request, HttpServletResponse response) {
            // Delegates to: org.ofbiz.order.shoppingcart.ShoppingCartEvents.clearEnsureCart
            return ShoppingCartEvents.clearEnsureCart(request, response);
        }

        @Request(
            uri = "checkExternalPayment",
            controller = "shop",
            secure = "true",
            directRequest = "false"
        )
        @Response(name = "none", type = "request", value = "emailorder")
        @Response(name = "offline", type = "request", value = "emailorder")
        @Response(name = "worldpay", type = "request", value = "callWorldPay")
        @Response(name = "paypal", type = "request", value = "callPayPal")
        @Response(name = "billact", type = "request", value = "emailorder")
        @Response(name = "cod", type = "request", value = "emailorder")
        @Response(name = "ideal", type = "request", value = "callIdeal")
        @Response(name = "stripe_hub", type = "request", value = "emailorder")
        @Response(name = "error", type = "view", value = "checkoutreview")
        public static String checkExternalPayment(HttpServletRequest request, HttpServletResponse response) {
            // Delegates to: org.ofbiz.order.shoppingcart.CheckOutEvents.checkExternalPayment
            return CheckOutEvents.checkExternalPayment(request, response);
        }

        @Request(
            uri = "emailorder",
            controller = "shop",
            secure = "true",
            directRequest = "false"
        )
        @Response(name = "success", type = "view", value = "ordercomplete")
        @Response(name = "error", type = "view", value = "ordercomplete")
        @Event(type = "service", path = "async", invoke = "sendOrderConfirmation")
        public static String emailorder(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "ordercomplete",
            controller = "shop",
            secure = "true"
        )
        @Response(name = "success", type = "view", value = "ordercomplete")
        public interface Ordercomplete {}

        @Request(
            uri = "orderviewonly",
            controller = "shop",
            secure = "true"
        )
        @Response(name = "success", type = "view", value = "orderviewonly")
        public interface Orderviewonly {}

        @Request(
            uri = "orderprint",
            controller = "shop",
            secure = "true"
        )
        @Response(name = "success", type = "view", value = "orderprint")
        public interface Orderprint {}

        @Request(
            uri = "callWorldPay",
            controller = "shop",
            secure = "true",
            directRequest = "false"
        )
        @Response(name = "success", type = "none")
        @Response(name = "error", type = "view", value = "checkoutreview")
        public static String callWorldPay(HttpServletRequest request, HttpServletResponse response) {
            // Delegates to: org.ofbiz.accounting.thirdparty.worldpay.WorldPayEvents.worldPayRequest
            try { return (String) Class.forName("org.ofbiz.accounting.thirdparty.worldpay.WorldPayEvents").getMethod("worldPayRequest", HttpServletRequest.class, HttpServletResponse.class).invoke(null, request, response); } catch (Exception e) { org.ofbiz.base.util.Debug.logError(e, "Error invoking WorldPayEvents.worldPayRequest", "ControllerDef"); return "error"; }
        }

        @Request(
            uri = "worldPayNotify",
            controller = "shop",
            secure = "true"
        )
        @Response(name = "success", type = "none")
        @Response(name = "error", type = "view", value = "checkoutreview")
        public static String worldPayNotify(HttpServletRequest request, HttpServletResponse response) {
            // Delegates to: org.ofbiz.accounting.thirdparty.worldpay.WorldPayEvents.worldPayNotify
            try { return (String) Class.forName("org.ofbiz.accounting.thirdparty.worldpay.WorldPayEvents").getMethod("worldPayNotify", HttpServletRequest.class, HttpServletResponse.class).invoke(null, request, response); } catch (Exception e) { org.ofbiz.base.util.Debug.logError(e, "Error invoking WorldPayEvents.worldPayNotify", "ControllerDef"); return "error"; }
        }

        @Request(
            uri = "callPayPal",
            controller = "shop",
            secure = "true",
            directRequest = "false"
        )
        @Response(name = "success", type = "none")
        @Response(name = "error", type = "view", value = "checkoutreview")
        public static String callPayPal(HttpServletRequest request, HttpServletResponse response) {
            // Delegates to: org.ofbiz.accounting.thirdparty.paypal.PayPalEvents.callPayPal
            try { return (String) Class.forName("org.ofbiz.accounting.thirdparty.paypal.PayPalEvents").getMethod("callPayPal", HttpServletRequest.class, HttpServletResponse.class).invoke(null, request, response); } catch (Exception e) { org.ofbiz.base.util.Debug.logError(e, "Error invoking PayPalEvents.callPayPal", "ControllerDef"); return "error"; }
        }

        @Request(
            uri = "payPalNotify",
            controller = "shop",
            secure = "true"
        )
        @Response(name = "success", type = "none")
        @Response(name = "error", type = "none")
        public static String payPalNotify(HttpServletRequest request, HttpServletResponse response) {
            // Delegates to: org.ofbiz.accounting.thirdparty.paypal.PayPalEvents.payPalIPN
            try { return (String) Class.forName("org.ofbiz.accounting.thirdparty.paypal.PayPalEvents").getMethod("payPalIPN", HttpServletRequest.class, HttpServletResponse.class).invoke(null, request, response); } catch (Exception e) { org.ofbiz.base.util.Debug.logError(e, "Error invoking PayPalEvents.payPalIPN", "ControllerDef"); return "error"; }
        }

        @Request(
            uri = "payPalCancel",
            controller = "shop",
            secure = "true"
        )
        @Response(name = "success", type = "view", value = "main")
        @Response(name = "error", type = "view", value = "main")
        public static String payPalCancel(HttpServletRequest request, HttpServletResponse response) {
            // Delegates to: org.ofbiz.accounting.thirdparty.paypal.PayPalEvents.cancelPayPalOrder
            try { return (String) Class.forName("org.ofbiz.accounting.thirdparty.paypal.PayPalEvents").getMethod("cancelPayPalOrder", HttpServletRequest.class, HttpServletResponse.class).invoke(null, request, response); } catch (Exception e) { org.ofbiz.base.util.Debug.logError(e, "Error invoking PayPalEvents.cancelPayPalOrder", "ControllerDef"); return "error"; }
        }

        @Request(
            uri = "callIdeal",
            controller = "shop",
            secure = "true",
            directRequest = "false"
        )
        @Response(name = "success", type = "none")
        @Response(name = "error", type = "view", value = "checkoutreview")
        public static String callIdeal(HttpServletRequest request, HttpServletResponse response) {
            // Delegates to: org.ofbiz.accounting.thirdparty.ideal.IdealEvents.callIdeal
            try { return (String) Class.forName("org.ofbiz.accounting.thirdparty.ideal.IdealEvents").getMethod("callIdeal", HttpServletRequest.class, HttpServletResponse.class).invoke(null, request, response); } catch (Exception e) { org.ofbiz.base.util.Debug.logError(e, "Error invoking IdealEvents.callIdeal", "ControllerDef"); return "error"; }
        }

        @Request(
            uri = "idealNotify",
            controller = "shop",
            secure = "true"
        )
        @Response(name = "success", type = "view", value = "ordercomplete")
        @Response(name = "error", type = "view", value = "checkoutreview")
        public static String idealNotify(HttpServletRequest request, HttpServletResponse response) {
            // Delegates to: org.ofbiz.accounting.thirdparty.ideal.IdealEvents.idealNotify
            try { return (String) Class.forName("org.ofbiz.accounting.thirdparty.ideal.IdealEvents").getMethod("idealNotify", HttpServletRequest.class, HttpServletResponse.class).invoke(null, request, response); } catch (Exception e) { org.ofbiz.base.util.Debug.logError(e, "Error invoking IdealEvents.idealNotify", "ControllerDef"); return "error"; }
        }

    }

    // Auto-generated split (Part 11)
    public static class Part11 {
        @Request(
            uri = "setPayPalCheckout",
            controller = "shop",
            secure = "true"
        )
        @Response(name = "success", type = "request", value = "payPalCheckoutRedirect")
        @Response(name = "error", type = "view-last")
        public static String setPayPalCheckout(HttpServletRequest request, HttpServletResponse response) {
            // Delegates to: org.ofbiz.order.thirdparty.paypal.ExpressCheckoutEvents.setExpressCheckout
            return ExpressCheckoutEvents.setExpressCheckout(request, response);
        }

        @Request(
            uri = "payPalCheckoutRedirect",
            controller = "shop",
            secure = "true"
        )
        @Response(name = "success", type = "none")
        @Response(name = "error", type = "view-last")
        public static String payPalCheckoutRedirect(HttpServletRequest request, HttpServletResponse response) {
            // Delegates to: org.ofbiz.order.thirdparty.paypal.ExpressCheckoutEvents.expressCheckoutRedirect
            return ExpressCheckoutEvents.expressCheckoutRedirect(request, response);
        }

        @Request(
            uri = "payPalCheckoutReturn",
            controller = "shop",
            secure = "true"
        )
        @Response(name = "success", type = "request", value = "reviewOrder")
        @Response(name = "error", type = "view-last", value = "main")
        public static String payPalCheckoutReturn(HttpServletRequest request, HttpServletResponse response) {
            // Delegates to: org.ofbiz.order.thirdparty.paypal.ExpressCheckoutEvents.getExpressCheckoutDetails
            return ExpressCheckoutEvents.getExpressCheckoutDetails(request, response);
        }

        @Request(
            uri = "payPalCheckoutCancel",
            controller = "shop",
            secure = "true"
        )
        @Response(name = "success", type = "view-last")
        public static String payPalCheckoutCancel(HttpServletRequest request, HttpServletResponse response) {
            // Delegates to: org.ofbiz.order.thirdparty.paypal.ExpressCheckoutEvents.expressCheckoutCancel
            return ExpressCheckoutEvents.expressCheckoutCancel(request, response);
        }

        @Request(
            uri = "payPalCheckoutUpdate",
            controller = "shop",
            secure = "true"
        )
        @Response(name = "success", type = "none")
        public static String payPalCheckoutUpdate(HttpServletRequest request, HttpServletResponse response) {
            // Delegates to: org.ofbiz.order.thirdparty.paypal.ExpressCheckoutEvents.expressCheckoutUpdate
            return ExpressCheckoutEvents.expressCheckoutUpdate(request, response);
        }

        @Request(
            uri = "quickadd",
            controller = "shop",
            secure = "true"
        )
        @Response(name = "success", type = "view", value = "quickadd")
        public interface Quickadd {}

        @Request(
            uri = "category",
            controller = "shop",
            secure = "true"
        )
        @Response(name = "success", type = "view", value = "category", saveCurrentView = "true")
        public interface Category {}

        @Request(
            uri = "product",
            controller = "shop",
            secure = "true"
        )
        @Response(name = "success", type = "view", value = "product", saveCurrentView = "true")
        public interface Product {}

        @Request(
            uri = "crosssell",
            controller = "shop",
            secure = "true"
        )
        @Response(name = "success", type = "view", value = "product")
        public interface Crosssell {}

        @Request(
            uri = "upsell",
            controller = "shop",
            secure = "true"
        )
        @Response(name = "success", type = "view", value = "product")
        public interface Upsell {}

        @Request(
            uri = "clearLastViewed",
            controller = "shop",
            secure = "true"
        )
        @Response(name = "success", type = "view", value = "main")
        @Response(name = "error", type = "view", value = "main")
        public static String clearLastViewed(HttpServletRequest request, HttpServletResponse response) {
            // Delegates to: org.ofbiz.product.product.ProductEvents.clearAllLastViewed
            return ProductEvents.clearAllLastViewed(request, response);
        }

        @Request(
            uri = "lastviewedproducts",
            controller = "shop",
            secure = "true"
        )
        @Response(name = "success", type = "view", value = "lastviewedproducts")
        public interface Lastviewedproducts {}

        @Request(
            uri = "reviewProduct",
            controller = "shop",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "productReview")
        public interface ReviewProduct {}

        @Request(
            uri = "createProductReview",
            controller = "shop",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view-last", value = "product")
        @Response(name = "error", type = "view", value = "productReview")
        @Event(type = "service", invoke = "createProductReview")
        public static String createProductReview(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "advancedsearch",
            controller = "shop",
            secure = "true"
        )
        @Response(name = "success", type = "view", value = "advancedsearch")
        public interface Advancedsearch {}

        @Request(
            uri = "search",
            controller = "shop",
            secure = "true"
        )
        @Response(name = "success", type = "view", value = "keywordsearch")
        @Response(name = "none", type = "none")
        public static String search(HttpServletRequest request, HttpServletResponse response) {
            // Delegates to: com.ilscipio.scipio.shop.product.ProductEvents.checkDoKeywordOverride
            return com.ilscipio.scipio.shop.product.ProductEvents.checkDoKeywordOverride(request, response);
        }

        @Request(
            uri = "keywordsearch",
            controller = "shop",
            secure = "true"
        )
        @Response(name = "success", type = "request", value = "search")
        public interface Keywordsearch {}

        @Request(
            uri = "clearSearchOptionsHistoryList",
            controller = "shop",
            secure = "true"
        )
        @Response(name = "success", type = "view", value = "advancedsearch")
        @Response(name = "error", type = "view", value = "advancedsearch")
        public static String clearSearchOptionsHistoryList(HttpServletRequest request, HttpServletResponse response) {
            // Delegates to: org.ofbiz.product.product.ProductSearchSession.clearSearchOptionsHistoryList
            return ProductSearchSession.clearSearchOptionsHistoryList(request, response);
        }

        @Request(
            uri = "setCurrentSearchFromHistory",
            controller = "shop",
            secure = "true"
        )
        @Response(name = "success", type = "view", value = "advancedsearch")
        @Response(name = "error", type = "view", value = "advancedsearch")
        public static String setCurrentSearchFromHistory(HttpServletRequest request, HttpServletResponse response) {
            // Delegates to: org.ofbiz.product.product.ProductSearchSession.setCurrentSearchFromHistory
            return ProductSearchSession.setCurrentSearchFromHistory(request, response);
        }

        @Request(
            uri = "setCurrentSearchFromHistoryAndSearch",
            controller = "shop",
            secure = "true"
        )
        @Response(name = "success", type = "request", value = "search")
        @Response(name = "error", type = "view", value = "advancedsearch")
        public static String setCurrentSearchFromHistoryAndSearch(HttpServletRequest request, HttpServletResponse response) {
            // Delegates to: org.ofbiz.product.product.ProductSearchSession.setCurrentSearchFromHistory
            return ProductSearchSession.setCurrentSearchFromHistory(request, response);
        }

    }

    // Auto-generated split (Part 12)
    public static class Part12 {
        @Request(
            uri = "orderhistory",
            controller = "shop",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "orderhistory")
        public interface Orderhistory {}

        @Request(
            uri = "orderdownloads",
            controller = "shop",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "orderdownloads")
        public interface Orderdownloads {}

        @Request(
            uri = "orderstatus",
            controller = "shop",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "orderstatus")
        public interface Orderstatus {}

        @Request(
            uri = "allowordersplit",
            controller = "shop",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "orderstatus")
        @Response(name = "error", type = "view", value = "orderstatus")
        @Event(type = "service", invoke = "setAllowOrderSplit")
        public static String allowordersplit(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "cancelOrderItem",
            controller = "shop",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "orderstatus")
        @Response(name = "error", type = "view", value = "orderstatus")
        @Event(type = "service", invoke = "cancelOrderItem")
        public static String cancelOrderItem(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "downloadDigitalProduct",
            controller = "shop",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "none")
        @Response(name = "error", type = "view", value = "orderhistory")
        public static String downloadDigitalProduct(HttpServletRequest request, HttpServletResponse response) {
            // Delegates to: org.ofbiz.order.order.OrderEvents.downloadDigitalProduct
            return OrderEvents.downloadDigitalProduct(request, response);
        }

        @Request(
            uri = "makeReturn",
            controller = "shop",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "requestreturn")
        public interface MakeReturn {}

        @Request(
            uri = "requestReturn",
            controller = "shop",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "request", value = "orderstatus")
        @Response(name = "error", type = "view", value = "requestreturn")
        public static String requestReturn(HttpServletRequest request, HttpServletResponse response) {
            // Delegates to: org.ofbiz.order.order.OrderReturnEvents.createCustomerReturn
            return OrderReturnEvents.createCustomerReturn(request, response);
        }

        @Request(
            uri = "newcustomer",
            controller = "shop",
            secure = "true"
        )
        @Response(name = "success", type = "view", value = "newcustomer")
        public interface Newcustomer {}

        @Request(
            uri = "createcustomer",
            controller = "shop",
            method = "POST",
            secure = "true"
        )
        @Response(name = "success", type = "request", value = "createcustomersuccess")
        @Response(name = "error", type = "view", value = "newcustomer")
        @Event(type = "simple", path = "component://shop/script/com/ilscipio/scipio/shop/customer/CustomerEvents.xml", invoke = "createCustomer")
        public static String createcustomer(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "createcustomersuccess",
            controller = "shop",
            directRequest = "false"
        )
        @Response(name = "success", type = "request-redirect-noparam", value = "main")
        @Response(name = "error", type = "view", value = "newcustomer")
        @Event(type = "simple", path = "component://shop/script/com/ilscipio/scipio/shop/customer/CustomerEvents.xml", invoke = "createCustomerSuccess")
        public static String createcustomersuccess(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "viewprofile",
            controller = "shop",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "viewprofile")
        public interface Viewprofile {}

        @Request(
            uri = "handleProfileTargetPage",
            controller = "shop",
            directRequest = "false"
        )
        @Response(name = "redirect", type = "request-redirect-noparam", value = "${requestAttributes.targetPage}")
        @Response(name = "forward", type = "request", value = "${requestAttributes.targetPage}")
        @Response(name = "success", type = "view", value = "viewprofile")
        @Response(name = "error", type = "view", value = "viewprofile")
        public static String handleProfileTargetPage(HttpServletRequest request, HttpServletResponse response) {
            // Delegates to: org.ofbiz.common.CommonEvents.processTargetPage
            return CommonEvents.processTargetPage(request, response);
        }

        @Request(
            uri = "editcontactmech",
            controller = "shop",
            secure = "true"
        )
        @Response(name = "success", type = "view", value = "editcontactmech")
        public interface Editcontactmech {}

        @Request(
            uri = "editcontactmechnosave",
            controller = "shop",
            secure = "true"
        )
        @Response(name = "success", type = "view", value = "editcontactmech")
        public interface Editcontactmechnosave {}

        @Request(
            uri = "createContactMech",
            controller = "shop",
            secure = "true"
        )
        @Response(name = "success", type = "request", value = "handleProfileTargetPage")
        @Response(name = "error", type = "view", value = "editcontactmech")
        @Event(type = "service", invoke = "createPartyContactMech")
        public static String createContactMech(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "updateContactMech",
            controller = "shop",
            secure = "true"
        )
        @Response(name = "success", type = "request", value = "handleProfileTargetPage")
        @Response(name = "error", type = "view", value = "editcontactmech")
        @Event(type = "service", invoke = "updatePartyContactMech")
        public static String updateContactMech(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "deleteContactMech",
            controller = "shop",
            secure = "true"
        )
        @Response(name = "success", type = "view", value = "viewprofile")
        @Response(name = "error", type = "view", value = "editcontactmech")
        @Event(type = "service", invoke = "deletePartyContactMech")
        public static String deleteContactMech(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "createPostalAddressAndPurpose",
            controller = "shop",
            secure = "true"
        )
        @Response(name = "success", type = "request", value = "handleProfileTargetPage")
        @Response(name = "error", type = "view", value = "editcontactmech")
        @Event(type = "service", invoke = "createPartyPostalAddress")
        public static String createPostalAddressAndPurpose(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "createPostalAddress",
            controller = "shop",
            secure = "true"
        )
        @Response(name = "success", type = "request", value = "handleProfileTargetPage")
        @Response(name = "error", type = "view", value = "editcontactmech")
        @Event(type = "service", invoke = "createPartyPostalAddress")
        public static String createPostalAddress(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

    }

    // Auto-generated split (Part 13)
    public static class Part13 {
        @Request(
            uri = "updatePostalAddress",
            controller = "shop",
            secure = "true"
        )
        @Response(name = "success", type = "request", value = "handleProfileTargetPage")
        @Response(name = "error", type = "view", value = "editcontactmech")
        @Event(type = "service", invoke = "updatePartyPostalAddress")
        public static String updatePostalAddress(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "createTelecomNumber",
            controller = "shop",
            secure = "true"
        )
        @Response(name = "success", type = "request", value = "handleProfileTargetPage")
        @Response(name = "error", type = "view", value = "editcontactmech")
        @Event(type = "service", invoke = "createPartyTelecomNumber")
        public static String createTelecomNumber(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "updateTelecomNumber",
            controller = "shop",
            secure = "true"
        )
        @Response(name = "success", type = "request", value = "handleProfileTargetPage")
        @Response(name = "error", type = "view", value = "editcontactmech")
        @Event(type = "service", invoke = "updatePartyTelecomNumber")
        public static String updateTelecomNumber(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "createEmailAddress",
            controller = "shop",
            secure = "true"
        )
        @Response(name = "success", type = "request", value = "handleProfileTargetPage")
        @Response(name = "error", type = "view", value = "editcontactmech")
        @Event(type = "service", invoke = "createPartyEmailAddress")
        public static String createEmailAddress(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "updateEmailAddress",
            controller = "shop",
            secure = "true"
        )
        @Response(name = "success", type = "request", value = "handleProfileTargetPage")
        @Response(name = "error", type = "view", value = "editcontactmech")
        @Event(type = "service", invoke = "updatePartyEmailAddress")
        public static String updateEmailAddress(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "createPartyContactMechPurpose",
            controller = "shop",
            secure = "true"
        )
        @Response(name = "success", type = "request", value = "handleProfileTargetPage")
        @Response(name = "error", type = "view", value = "editcontactmech")
        @Event(type = "service", invoke = "createPartyContactMechPurpose")
        public static String createPartyContactMechPurpose(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "deletePartyContactMechPurpose",
            controller = "shop",
            secure = "true"
        )
        @Response(name = "success", type = "view", value = "editcontactmech")
        @Response(name = "error", type = "view", value = "editcontactmech")
        @Event(type = "service", invoke = "deletePartyContactMechPurpose")
        public static String deletePartyContactMechPurpose(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "editcreditcard",
            controller = "shop",
            secure = "true"
        )
        @Response(name = "success", type = "view", value = "editcreditcard")
        public interface Editcreditcard {}

        @Request(
            uri = "createCreditCard",
            controller = "shop",
            secure = "true"
        )
        @Response(name = "success", type = "request", value = "handleProfileTargetPage")
        @Response(name = "address", type = "view", value = "editcontactmech")
        @Response(name = "error", type = "view", value = "editcreditcard")
        @Event(type = "simple", path = "component://accounting/script/org/ofbiz/accounting/payment/PaymentMethodEvents.xml", invoke = "createCreditCard")
        public static String createCreditCard(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "updateCreditCard",
            controller = "shop",
            secure = "true"
        )
        @Response(name = "success", type = "request", value = "handleProfileTargetPage")
        @Response(name = "address", type = "view", value = "editcontactmech")
        @Response(name = "error", type = "view", value = "editcreditcard")
        @Event(type = "simple", path = "component://accounting/script/org/ofbiz/accounting/payment/PaymentMethodEvents.xml", invoke = "updateCreditCard")
        public static String updateCreditCard(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "editgiftcard",
            controller = "shop",
            secure = "true"
        )
        @Response(name = "success", type = "view", value = "editgiftcard")
        public interface Editgiftcard {}

        @Request(
            uri = "createGiftCard",
            controller = "shop",
            secure = "true"
        )
        @Response(name = "success", type = "request", value = "handleProfileTargetPage")
        @Response(name = "error", type = "view", value = "editgiftcard")
        @Event(type = "groovy")
        public static String createGiftCard(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "updateGiftCard",
            controller = "shop",
            secure = "true"
        )
        @Response(name = "success", type = "request", value = "handleProfileTargetPage")
        @Response(name = "error", type = "view", value = "editgiftcard")
        @Event(type = "groovy")
        public static String updateGiftCard(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "editeftaccount",
            controller = "shop",
            secure = "true"
        )
        @Response(name = "success", type = "view", value = "editeftaccount")
        public interface Editeftaccount {}

        @Request(
            uri = "createEftAccount",
            controller = "shop",
            secure = "true"
        )
        @Response(name = "success", type = "request", value = "handleProfileTargetPage")
        @Response(name = "address", type = "view", value = "editcontactmech")
        @Response(name = "error", type = "view", value = "editeftaccount")
        @Event(type = "simple", path = "component://accounting/script/org/ofbiz/accounting/payment/PaymentMethodEvents.xml", invoke = "createEftAccount")
        public static String createEftAccount(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "updateEftAccount",
            controller = "shop",
            secure = "true"
        )
        @Response(name = "success", type = "request", value = "handleProfileTargetPage")
        @Response(name = "error", type = "view", value = "editeftaccount")
        @Event(type = "simple", path = "component://accounting/script/org/ofbiz/accounting/payment/PaymentMethodEvents.xml", invoke = "updateEftAccount")
        public static String updateEftAccount(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "deletePaymentMethod",
            controller = "shop",
            secure = "true"
        )
        @Response(name = "success", type = "view", value = "viewprofile")
        @Response(name = "error", type = "view", value = "viewprofile")
        @Event(type = "simple", path = "component://accounting/script/org/ofbiz/accounting/payment/PaymentMethodEvents.xml", invoke = "deletePaymentMethod")
        public static String deletePaymentMethod(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "editperson",
            controller = "shop",
            secure = "true"
        )
        @Response(name = "success", type = "view", value = "editperson")
        public interface Editperson {}

        @Request(
            uri = "createPerson",
            controller = "shop",
            secure = "true"
        )
        @Response(name = "success", type = "request", value = "handleProfileTargetPage")
        @Response(name = "error", type = "view", value = "editperson")
        @Event(type = "service", invoke = "createPerson")
        public static String createPerson(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "updatePerson",
            controller = "shop",
            secure = "true"
        )
        @Response(name = "success", type = "request", value = "handleProfileTargetPage")
        @Response(name = "error", type = "view", value = "editperson")
        @Event(type = "service", invoke = "updatePerson")
        public static String updatePerson(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

    }

    // Auto-generated split (Part 14)
    public static class Part14 {
        @Request(
            uri = "setprofiledefault",
            controller = "shop",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "viewprofile")
        @Response(name = "error", type = "view", value = "viewprofile")
        @Event(type = "service", invoke = "setPartyProfileDefaults")
        public static String setprofiledefault(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "tellafriend",
            controller = "shop",
            secure = "true"
        )
        @Response(name = "success", type = "view", value = "tellafriend")
        public interface Tellafriend {}

        @Request(
            uri = "emailFriend",
            controller = "shop",
            secure = "true"
        )
        @Response(name = "success", type = "view", value = "tellafriend")
        @Response(name = "error", type = "view", value = "tellafriend")
        public static String emailFriend(HttpServletRequest request, HttpServletResponse response) {
            // Delegates to: org.ofbiz.product.product.ProductEvents.tellAFriend
            return ProductEvents.tellAFriend(request, response);
        }

        @Request(
            uri = "giftcardbalance",
            controller = "shop",
            secure = "true"
        )
        @Response(name = "success", type = "view", value = "giftcardbalance")
        public interface Giftcardbalance {}

        @Request(
            uri = "querygcbalance",
            controller = "shop",
            method = "post",
            secure = "true"
        )
        @Response(name = "success", type = "view", value = "giftcardbalance")
        @Response(name = "error", type = "view", value = "giftcardbalance")
        @Event(type = "groovy")
        public static String querygcbalance(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "giftcardlink",
            controller = "shop",
            secure = "true"
        )
        @Response(name = "success", type = "view", value = "giftcardlink")
        public interface Giftcardlink {}

        @Request(
            uri = "linkgiftcard",
            controller = "shop",
            method = "post",
            secure = "true"
        )
        @Response(name = "success", type = "view", value = "giftcardlink")
        @Response(name = "error", type = "view", value = "giftcardlink")
        @Event(type = "groovy")
        public static String linkgiftcard(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "digitalproductlist",
            controller = "shop",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "digitalproductlist")
        public interface Digitalproductlist {}

        @Request(
            uri = "digitalproductedit",
            controller = "shop",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "digitalproductedit")
        public interface Digitalproductedit {}

        @Request(
            uri = "createCustomerDigitalDownloadProduct",
            controller = "shop",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "digitalproductedit")
        @Response(name = "error", type = "view", value = "digitalproductedit")
        @Event(type = "service", invoke = "createCustomerDigitalDownloadProduct")
        public static String createCustomerDigitalDownloadProduct(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "updateCustomerDigitalDownloadProduct",
            controller = "shop",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "digitalproductedit")
        @Response(name = "error", type = "view", value = "digitalproductedit")
        @Event(type = "service", invoke = "updateCustomerDigitalDownloadProduct")
        public static String updateCustomerDigitalDownloadProduct(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "deleteCustomerDigitalDownloadProduct",
            controller = "shop",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "digitalproductlist")
        @Response(name = "error", type = "view", value = "digitalproductlist")
        @Event(type = "service", invoke = "deleteCustomerDigitalDownloadProduct")
        public static String deleteCustomerDigitalDownloadProduct(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "addCustomerDigitalDownloadProductFile",
            controller = "shop",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "digitalproductedit")
        @Response(name = "error", type = "view", value = "digitalproductedit")
        @Event(type = "service", invoke = "addCustomerDigitalDownloadProductFile")
        public static String addCustomerDigitalDownloadProductFile(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "removeCustomerDigitalDownloadProductFile",
            controller = "shop",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "digitalproductedit")
        @Response(name = "error", type = "view", value = "digitalproductedit")
        @Event(type = "service", invoke = "removeCustomerDigitalDownloadProductFile")
        public static String removeCustomerDigitalDownloadProductFile(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "takesurvey",
            controller = "shop",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "profilesurvey")
        public interface Takesurvey {}

        @Request(
            uri = "profilesurvey",
            controller = "shop",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "profilesurvey")
        @Response(name = "error", type = "view", value = "profilesurvey")
        public static String profilesurvey(HttpServletRequest request, HttpServletResponse response) {
            // Delegates to: org.ofbiz.content.survey.SurveyEvents.createSurveyResponseAndRestoreParameters
            return SurveyEvents.createSurveyResponseAndRestoreParameters(request, response);
        }

        @Request(
            uri = "minipoll",
            controller = "shop",
            secure = "true"
        )
        @Response(name = "success", type = "view", value = "main")
        @Response(name = "error", type = "view", value = "main")
        public static String minipoll(HttpServletRequest request, HttpServletResponse response) {
            // Delegates to: org.ofbiz.content.survey.SurveyEvents.createSurveyResponseAndRestoreParameters
            return SurveyEvents.createSurveyResponseAndRestoreParameters(request, response);
        }

        @Request(
            uri = "messagelist",
            controller = "shop",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "messagelist")
        public interface Messagelist {}

        @Request(
            uri = "readmessage",
            controller = "shop",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "messagedetail")
        @Response(name = "error", type = "view", value = "messagedetail")
        @Event(type = "service", invoke = "setCommEventRoleToRead")
        public static String readmessage(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "newmessage",
            controller = "shop",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "messagecreate")
        public interface Newmessage {}

    }

    // Auto-generated split (Part 15)
    public static class Part15 {
        @Request(
            uri = "sendmessage",
            controller = "shop",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "messagelist")
        @Response(name = "error", type = "view", value = "messagecreate")
        @Event(type = "service", invoke = "createCommunicationEventWithoutPermission")
        public static String sendmessage(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "contactus",
            controller = "shop",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "contactus")
        public interface Contactus {}

        @Request(
            uri = "AnonContactus",
            controller = "shop",
            secure = "true"
        )
        @Response(name = "success", type = "view", value = "AnonContactus")
        public interface AnonContactus {}

        @Request(
            uri = "contactsubmit",
            controller = "shop",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "contactus")
        @Response(name = "error", type = "view", value = "contactus")
        @Event(type = "service", invoke = "createCommunicationEventWithoutPermission")
        public static String contactsubmit(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "submitAnonContact",
            controller = "shop",
            secure = "true"
        )
        @Response(name = "success", type = "request-redirect", value = "main")
        @Response(name = "error", type = "request", value = "AnonContactus")
        @Event(type = "simple", path = "component://shop/script/com/ilscipio/scipio/shop/customer/CustomerEvents.xml", invoke = "createAnonContact")
        public static String submitAnonContact(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "signUpForContactList",
            controller = "shop",
            secure = "true"
        )
        @Response(name = "success", type = "view-last")
        @Response(name = "error", type = "view-last", value = "main")
        @Event(type = "service", invoke = "signUpForContactList")
        public static String signUpForContactList(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "unsubscribeContactListParty",
            controller = "shop",
            secure = "true"
        )
        @Response(name = "success", type = "view-last")
        @Response(name = "error", type = "view-last", value = "main")
        @Event(type = "service", invoke = "unsubscribeContactListParty")
        public static String unsubscribeContactListParty(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "unsubscribeContactListPartyContachMech",
            controller = "shop",
            secure = "true"
        )
        @Response(name = "success", type = "view-last")
        @Response(name = "error", type = "view-last", value = "main")
        @Event(type = "service", invoke = "unsubscribeContactListPartyContachMech")
        public static String unsubscribeContactListPartyContachMech(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "contactListOptOut",
            controller = "shop",
            secure = "true"
        )
        @Response(name = "success", type = "view", value = "ContactListOptOut")
        @Event(type = "service", invoke = "updateContactListPartyNoUserLogin")
        public static String contactListOptOut(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "uploadPartyContent",
            controller = "shop",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "viewprofile")
        @Response(name = "error", type = "view", value = "viewprofile")
        @Event(type = "service", invoke = "uploadPartyContentFile")
        public static String uploadPartyContent(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "removePartyAsset",
            controller = "shop",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "viewprofile")
        @Response(name = "error", type = "view", value = "viewprofile")
        @Event(type = "service", invoke = "deactivateAllContentRoles")
        public static String removePartyAsset(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "createContactListParty",
            controller = "shop",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "viewprofile")
        @Response(name = "error", type = "view", value = "viewprofile")
        @Event(type = "service", invoke = "createContactListParty")
        public static String createContactListParty(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "updateContactListParty",
            controller = "shop",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "viewprofile")
        @Response(name = "error", type = "view", value = "viewprofile")
        @Event(type = "service", invoke = "updateContactListParty")
        public static String updateContactListParty(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "updateContactListPartyNoUserLogin",
            controller = "shop",
            secure = "true"
        )
        @Response(name = "success", type = "view", value = "main")
        @Response(name = "error", type = "view", value = "main")
        @Event(type = "service", invoke = "updateContactListPartyNoUserLogin")
        public static String updateContactListPartyNoUserLogin(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "choosecatalog",
            controller = "shop",
            secure = "true"
        )
        @Response(name = "success", type = "view", value = "main")
        public interface Choosecatalog {}

        @Request(
            uri = "ListQuotes",
            controller = "shop",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ListQuotes")
        public interface ListQuotes {}

        @Request(
            uri = "ViewQuote",
            controller = "shop",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ViewQuote")
        public interface ViewQuote {}

        @Request(
            uri = "loadCartFromQuote",
            controller = "shop",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "request", value = "finalizeOrder")
        @Response(name = "error", type = "view", value = "ViewQuote")
        public static String loadCartFromQuote(HttpServletRequest request, HttpServletResponse response) {
            // Delegates to: org.ofbiz.order.shoppingcart.ShoppingCartEvents.loadCartFromQuote
            return ShoppingCartEvents.loadCartFromQuote(request, response);
        }

        @Request(
            uri = "finalizeOrder",
            controller = "shop",
            secure = "true"
        )
        @Response(name = "customer", type = "view", value = "custsetting")
        @Response(name = "shipping", type = "view", value = "shipsetting")
        @Response(name = "options", type = "view", value = "optionsetting")
        @Response(name = "payment", type = "view", value = "paymentoptions")
        @Response(name = "term", type = "view", value = "paymentoptions")
        @Response(name = "addparty", type = "request", value = "calcShipping")
        @Response(name = "paysplit", type = "view", value = "checkoutpayment")
        @Response(name = "sales", type = "request", value = "calcShipping")
        @Response(name = "paymentError", type = "request", value = "calcShippingBeforePayment")
        @Response(name = "shipGroups", type = "request", value = "finalizeOrderError")
        @Response(name = "po", type = "request", value = "calcTax")
        @Response(name = "error", type = "request", value = "finalizeOrderError")
        public static String finalizeOrder(HttpServletRequest request, HttpServletResponse response) {
            // Delegates to: org.ofbiz.order.shoppingcart.CheckOutEvents.finalizeOrderEntry
            return CheckOutEvents.finalizeOrderEntry(request, response);
        }

        @Request(
            uri = "finalizeOrderError",
            controller = "shop",
            directRequest = "false"
        )
        @Response(name = "customer", type = "view", value = "custsetting")
        @Response(name = "shipping", type = "view", value = "shipsetting")
        @Response(name = "options", type = "view", value = "optionsetting")
        @Response(name = "payment", type = "view", value = "paymentoptions")
        @Response(name = "paysplit", type = "view", value = "checkoutpayment")
        @Response(name = "sales", type = "request", value = "calcShipping")
        @Response(name = "error", type = "view", value = "showcart")
        public static String finalizeOrderError(HttpServletRequest request, HttpServletResponse response) {
            // Delegates to: org.ofbiz.order.shoppingcart.CheckOutEvents.finalizeOrderEntryError
            return CheckOutEvents.finalizeOrderEntryError(request, response);
        }

    }

    // Auto-generated split (Part 16)
    public static class Part16 {
        @Request(
            uri = "setBilling",
            controller = "shop",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "billsetting")
        public interface SetBilling {}

        @Request(
            uri = "ListRequests",
            controller = "shop",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ListRequests", saveLastView = "true")
        public interface ListRequests {}

        @Request(
            uri = "ViewRequest",
            controller = "shop",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ViewRequest")
        public interface ViewRequest {}

        @Request(
            uri = "NewCustRequest",
            controller = "shop",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "NewRequest")
        public interface NewCustRequest {}

        @Request(
            uri = "createCustRequest",
            controller = "shop",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ListRequests")
        @Response(name = "error", type = "view", value = "ListRequests")
        public static String createCustRequest(HttpServletRequest request, HttpServletResponse response) {
            // Delegates to: org.ofbiz.order.shoppingcart.ShoppingCartEvents.createCustRequest
            return ShoppingCartEvents.createCustRequest(request, response);
        }

        @Request(
            uri = "createCustRequestFromCart",
            controller = "shop",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view-last")
        @Response(name = "error", type = "request", value = "showcart")
        public static String createCustRequestFromCart(HttpServletRequest request, HttpServletResponse response) {
            // Delegates to: org.ofbiz.order.shoppingcart.ShoppingCartEvents.createCustRequestFromCart
            return ShoppingCartEvents.createCustRequestFromCart(request, response);
        }

        @Request(
            uri = "createQuoteFromCart",
            controller = "shop",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "request", value = "showcart")
        @Response(name = "error", type = "request", value = "showcart")
        public static String createQuoteFromCart(HttpServletRequest request, HttpServletResponse response) {
            // Delegates to: org.ofbiz.order.shoppingcart.ShoppingCartEvents.createQuoteFromCart
            return ShoppingCartEvents.createQuoteFromCart(request, response);
        }

        @Request(
            uri = "createCustRequestFromShoppingList",
            controller = "shop",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "request", value = "editShoppingList")
        @Response(name = "error", type = "request", value = "editShoppingList")
        @Event(type = "service", invoke = "createCustRequestFromShoppingList")
        public static String createCustRequestFromShoppingList(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "createQuoteFromShoppingList",
            controller = "shop",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "request", value = "editShoppingList")
        @Response(name = "error", type = "request", value = "editShoppingList")
        @Event(type = "service", invoke = "createQuoteFromShoppingList")
        public static String createQuoteFromShoppingList(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "order.pdf",
            controller = "shop",
            secure = "true"
        )
        @Response(name = "success", type = "view", value = "OrderPDF")
        public interface OrderPdf {}

        @Request(
            uri = "invoice.pdf",
            controller = "shop",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "InvoicePDF")
        public interface InvoicePdf {}

        @Request(
            uri = "onePageCheckout",
            controller = "shop",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "OnePageCheckout")
        @Response(name = "error", type = "view", value = "main")
        public static String onePageCheckout(HttpServletRequest request, HttpServletResponse response) {
            // Delegates to: org.ofbiz.order.shoppingcart.CheckOutEvents.cartNotEmpty
            return CheckOutEvents.cartNotEmpty(request, response);
        }

        @Request(
            uri = "anonOnePageCheckout",
            controller = "shop",
            secure = "true"
        )
        @Response(name = "success", type = "view", value = "OnePageCheckout")
        @Response(name = "error", type = "view", value = "main")
        public static String anonOnePageCheckout(HttpServletRequest request, HttpServletResponse response) {
            // Delegates to: org.ofbiz.order.shoppingcart.CheckOutEvents.cartNotEmpty
            return CheckOutEvents.cartNotEmpty(request, response);
        }

        @Request(
            uri = "getCountryList",
            controller = "shop",
            secure = "true"
        )
        @Response(name = "success", type = "request", value = "json")
        @Response(name = "error", type = "request", value = "json")
        @Event(type = "service", invoke = "getCountryList")
        public static String getCountryList(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "getAssociatedStateList",
            controller = "shop",
            secure = "true"
        )
        @Response(name = "success", type = "request", value = "json")
        @Response(name = "error", type = "request", value = "json")
        @Event(type = "service", invoke = "getAssociatedStateList")
        public static String getAssociatedStateList(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "createUpdateShippingAddress",
            controller = "shop",
            secure = "true"
        )
        @Response(name = "success", type = "request", value = "json")
        @Response(name = "error", type = "request", value = "json")
        @Event(type = "service", invoke = "createUpdateCustomerAndShippingAddress")
        public static String createUpdateShippingAddress(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "getShipOptions",
            controller = "shop",
            secure = "true"
        )
        @Response(name = "success", type = "request", value = "json")
        @Response(name = "error", type = "request", value = "json")
        @Event(type = "simple", path = "component://shop/script/com/ilscipio/scipio/shop/customer/CustomerEvents.xml", invoke = "getShipOptions")
        public static String getShipOptions(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "setShippingOption",
            controller = "shop",
            secure = "true"
        )
        @Response(name = "success", type = "request", value = "json")
        @Response(name = "error", type = "request", value = "json")
        @Event(type = "simple", path = "component://shop/script/com/ilscipio/scipio/shop/customer/CustomerEvents.xml", invoke = "setShippingOption")
        public static String setShippingOption(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "createUpdateBillingAndPayment",
            controller = "shop",
            secure = "true"
        )
        @Response(name = "success", type = "request", value = "json")
        @Response(name = "error", type = "request", value = "json")
        @Event(type = "service", invoke = "createUpdateBillingAddressAndPaymentMethod")
        public static String createUpdateBillingAndPayment(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "cartItemQtyUpdate",
            controller = "shop",
            secure = "true"
        )
        @Response(name = "success", type = "request", value = "json")
        @Response(name = "error", type = "request", value = "json")
        public static String cartItemQtyUpdate(HttpServletRequest request, HttpServletResponse response) {
            // Delegates to: org.ofbiz.order.shoppingcart.ShoppingCartEvents.modifyCartAndGetCartData
            return ShoppingCartEvents.modifyCartAndGetCartData(request, response);
        }

    }

    // Auto-generated split (Part 17)
    public static class Part17 {
        @Request(
            uri = "silentAddPromoCode",
            controller = "shop",
            secure = "true"
        )
        @Response(name = "success", type = "request", value = "json")
        @Response(name = "error", type = "request", value = "json")
        public static String silentAddPromoCode(HttpServletRequest request, HttpServletResponse response) {
            // Delegates to: org.ofbiz.order.shoppingcart.ShoppingCartEvents.addProductPromoCode
            return ShoppingCartEvents.addProductPromoCode(request, response);
        }

        @Request(
            uri = "getCartData",
            controller = "shop",
            secure = "true"
        )
        @Response(name = "success", type = "request", value = "json")
        @Response(name = "error", type = "request", value = "json")
        @Event(type = "service", invoke = "getShoppingCartData")
        public static String getCartData(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "getShoppingCartItemIndex",
            controller = "shop",
            secure = "true"
        )
        @Response(name = "success", type = "request", value = "json")
        @Response(name = "error", type = "request", value = "json")
        @Event(type = "service", invoke = "getShoppingCartItemIndex")
        public static String getShoppingCartItemIndex(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "onePageProcessOrder",
            controller = "shop",
            secure = "true"
        )
        @Response(name = "sales_order", type = "request", value = "onePageCheckBlacklist")
        @Response(name = "work_order", type = "request", value = "onePageCheckBlacklist")
        @Response(name = "error", type = "view", value = "OnePageCheckout")
        @Response(name = "errorJson", type = "request", value = "json")
        public static String onePageProcessOrder(HttpServletRequest request, HttpServletResponse response) {
            // Delegates to: org.ofbiz.order.shoppingcart.CheckOutEvents.createOrder
            return CheckOutEvents.createOrder(request, response);
        }

        @Request(
            uri = "onePageCheckBlacklist",
            controller = "shop",
            directRequest = "false"
        )
        @Response(name = "success", type = "request", value = "onePageProcessPayment")
        @Response(name = "failed", type = "request", value = "failedBlacklist")
        @Response(name = "error", type = "view", value = "OnePageCheckout")
        @Response(name = "errorJson", type = "request", value = "json")
        public static String onePageCheckBlacklist(HttpServletRequest request, HttpServletResponse response) {
            // Delegates to: org.ofbiz.order.shoppingcart.CheckOutEvents.checkOrderBlacklist
            return CheckOutEvents.checkOrderBlacklist(request, response);
        }

        @Request(
            uri = "onePageProcessPayment",
            controller = "shop",
            secure = "true",
            directRequest = "false"
        )
        @Response(name = "success", type = "request", value = "onePageClearCartForOrder")
        @Response(name = "fail", type = "request", value = "checkouterror")
        @Response(name = "failJson", type = "request", value = "json")
        @Response(name = "error", type = "view", value = "OnePageCheckout")
        @Response(name = "errorJson", type = "request", value = "json")
        public static String onePageProcessPayment(HttpServletRequest request, HttpServletResponse response) {
            // Delegates to: org.ofbiz.order.shoppingcart.CheckOutEvents.processPayment
            return CheckOutEvents.processPayment(request, response);
        }

        @Request(
            uri = "onePageClearCartForOrder",
            controller = "shop",
            directRequest = "false"
        )
        @Response(name = "success", type = "request", value = "onePageCheckExternalPayment")
        @Response(name = "error", type = "view", value = "OnePageCheckout")
        @Response(name = "errorJson", type = "request", value = "json")
        public static String onePageClearCartForOrder(HttpServletRequest request, HttpServletResponse response) {
            // Delegates to: org.ofbiz.order.shoppingcart.ShoppingCartEvents.clearEnsureCart
            return ShoppingCartEvents.clearEnsureCart(request, response);
        }

        @Request(
            uri = "onePageCheckExternalPayment",
            controller = "shop",
            secure = "true",
            directRequest = "false"
        )
        @Response(name = "none", type = "request", value = "emailorder")
        @Response(name = "stripe_hub", type = "request", value = "emailorder")
        @Response(name = "error", type = "view", value = "OnePageCheckout")
        @Response(name = "errorJson", type = "request", value = "json")
        public static String onePageCheckExternalPayment(HttpServletRequest request, HttpServletResponse response) {
            // Delegates to: org.ofbiz.order.shoppingcart.CheckOutEvents.checkExternalPayment
            return CheckOutEvents.checkExternalPayment(request, response);
        }

        @Request(
            uri = "editProfile",
            controller = "shop",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditProfile")
        public interface EditProfile {}

        @Request(
            uri = "manageAddress",
            controller = "shop",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ManageAddress")
        public interface ManageAddress {}

        @Request(
            uri = "createCustomerProfile",
            controller = "shop",
            secure = "true"
        )
        @Response(name = "success", type = "view", value = "main")
        @Response(name = "error", type = "view", value = "newcustomer")
        @Event(type = "simple", path = "component://shop/script/com/ilscipio/scipio/shop/customer/CustomerEvents.xml", invoke = "createCustomerProfile")
        public static String createCustomerProfile(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "updateCustomerProfile",
            controller = "shop",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "viewprofile")
        @Response(name = "error", type = "view", value = "EditProfile")
        @Event(type = "simple", path = "component://shop/script/com/ilscipio/scipio/shop/customer/CustomerEvents.xml", invoke = "updateCustomerProfile")
        public static String updateCustomerProfile(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "createPartyPostalAddress",
            controller = "shop",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "request", value = "json")
        @Response(name = "error", type = "request", value = "json")
        @Event(type = "service", invoke = "createPostalAddressAndPurposes")
        public static String createPartyPostalAddress(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "updatePartyPostalAddress",
            controller = "shop",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "request", value = "json")
        @Response(name = "error", type = "request", value = "json")
        @Event(type = "service", invoke = "updateContactMechAndPurposes")
        public static String updatePartyPostalAddress(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "deletePostalAddress",
            controller = "shop",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ManageAddress")
        @Response(name = "error", type = "view", value = "ManageAddress")
        @Event(type = "service", invoke = "deletePartyContactMech")
        public static String deletePostalAddress(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "updatePartyEmailAddress",
            controller = "shop",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "request", value = "json")
        @Response(name = "error", type = "request", value = "json")
        @Event(type = "service", invoke = "createUpdatePartyEmailAddress")
        public static String updatePartyEmailAddress(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "getConfigDetailsEvent",
            controller = "shop",
            secure = "true"
        )
        @Response(name = "success", type = "request", value = "json")
        @Response(name = "error", type = "request", value = "json")
        public static String getConfigDetailsEvent(HttpServletRequest request, HttpServletResponse response) {
            // Delegates to: org.ofbiz.order.shoppingcart.ShoppingCartEvents.getConfigDetailsEvent
            return ShoppingCartEvents.getConfigDetailsEvent(request, response);
        }

        @Request(
            uri = "addToCompare",
            controller = "shop",
            secure = "true"
        )
        @Response(name = "success", type = "view-last", value = "main")
        public static String addToCompare(HttpServletRequest request, HttpServletResponse response) {
            // Delegates to: org.ofbiz.product.product.ProductEvents.addProductToComparisonList
            return ProductEvents.addProductToComparisonList(request, response);
        }

        @Request(
            uri = "removeFromCompare",
            controller = "shop",
            secure = "true"
        )
        @Response(name = "success", type = "view-last")
        public static String removeFromCompare(HttpServletRequest request, HttpServletResponse response) {
            // Delegates to: org.ofbiz.product.product.ProductEvents.removeProductFromComparisonList
            return ProductEvents.removeProductFromComparisonList(request, response);
        }

        @Request(
            uri = "clearCompareList",
            controller = "shop",
            secure = "true"
        )
        @Response(name = "success", type = "view-last")
        public static String clearCompareList(HttpServletRequest request, HttpServletResponse response) {
            // Delegates to: org.ofbiz.product.product.ProductEvents.clearProductComparisonList
            return ProductEvents.clearProductComparisonList(request, response);
        }

    }

    // Auto-generated split (Part 18)
    public static class Part18 {
        @Request(
            uri = "compareProducts",
            controller = "shop",
            secure = "true"
        )
        @Response(name = "success", type = "view", value = "compareProducts", saveLastView = "true")
        public interface CompareProducts {}

        @Request(
            uri = "ProductUomDropDownOnly",
            controller = "shop",
            secure = "true"
        )
        @Response(name = "success", type = "view", value = "ProductUomDropDownOnly")
        public interface ProductUomDropDownOnly {}

        @Request(
            uri = "captcha.jpg",
            controller = "shop",
            secure = "true"
        )
        @Response(name = "success", type = "none")
        public static String captchaJpg(HttpServletRequest request, HttpServletResponse response) {
            // Delegates to: org.ofbiz.common.CommonEvents.getCaptcha
            return CommonEvents.getCaptcha(request, response);
        }

        @Request(
            uri = "productCategoryList",
            controller = "shop",
            secure = "true"
        )
        @Response(name = "success", type = "view", value = "productCategoryList", saveCurrentView = "true")
        public interface ProductCategoryList {}

        @Request(
            uri = "productCategoryListSecure",
            controller = "shop",
            secure = "true"
        )
        @Response(name = "success", type = "view", value = "productCategoryList", saveCurrentView = "true")
        public interface ProductCategoryListSecure {}

        @Request(
            uri = "categoryAjaxFired",
            controller = "shop",
            secure = "true"
        )
        @Response(name = "success", type = "view", value = "productCategoryList", saveCurrentView = "true")
        public interface CategoryAjaxFired {}

        @Request(
            uri = "categoryAjaxFiredSecure",
            controller = "shop",
            secure = "true"
        )
        @Response(name = "success", type = "view", value = "productCategoryList", saveCurrentView = "true")
        public interface CategoryAjaxFiredSecure {}

        @Request(
            uri = "fromSetSessionLocale",
            controller = "shop",
            secure = "true"
        )
        @Response(name = "success", type = "view-last", value = "main")
        @Response(name = "error", type = "view", value = "main")
        @Event(type = "simple", path = "component://shop/script/com/ilscipio/scipio/shop/customer/CustomerEvents.xml", invoke = "fromSetSessionLocale")
        public static String fromSetSessionLocale(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "stream",
            controller = "shop",
            secure = "true"
        )
        @Response(name = "success", type = "none")
        @Response(name = "error", type = "view", value = "error")
        public static String stream(HttpServletRequest request, HttpServletResponse response) {
            // Delegates to: org.ofbiz.content.data.DataEvents.serveObjectData
            return DataEvents.serveObjectData(request, response);
        }

        @Request(
            uri = "showShoppingList",
            controller = "shop",
            secure = "true"
        )
        @Response(name = "success", type = "view", value = "showShoppingList", saveCurrentView = "true")
        public interface ShowShoppingList {}

        @Request(
            uri = "showShoppingListSecure",
            controller = "shop",
            secure = "true"
        )
        @Response(name = "success", type = "view", value = "showShoppingList", saveCurrentView = "true")
        public interface ShowShoppingListSecure {}

        @Request(
            uri = "showShoppingListAjaxFired",
            controller = "shop",
            secure = "true"
        )
        @Response(name = "success", type = "view", value = "showShoppingList", saveCurrentView = "true")
        public interface ShowShoppingListAjaxFired {}

        @Request(
            uri = "showShoppingListAjaxFiredSecure",
            controller = "shop",
            secure = "true"
        )
        @Response(name = "success", type = "view", value = "showShoppingList", saveCurrentView = "true")
        public interface ShowShoppingListAjaxFiredSecure {}

        @Request(
            uri = "janrainCheckLogin",
            controller = "shop",
            secure = "true"
        )
        @Response(name = "success", type = "view", value = "main")
        @Response(name = "error", type = "view", value = "login")
        @Response(name = "userLoginMissing", type = "request", value = "newcustomer")
        public static String janrainCheckLogin(HttpServletRequest request, HttpServletResponse response) {
            // Delegates to: com.ilscipio.scipio.shop.janrain.JanrainHelper.janrainCheckLogin
            return JanrainHelper.janrainCheckLogin(request, response);
        }


    }
}
