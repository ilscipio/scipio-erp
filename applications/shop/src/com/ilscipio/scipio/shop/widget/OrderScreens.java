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
package com.ilscipio.scipio.shop.widget;

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
public class OrderScreens {

    @Screen(name = "anonymoustrail", location = "component://shop/widget/OrderScreens.xml")
    @Action(type = ActionType.PROPERTY_MAP, resource = "ShopUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "EcommerceUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.SCRIPT, location = "component://shop/webapp/shop/WEB-INF/actions/order/AnonymousTrail.groovy")
    @Section(widgets = @Widgets(value = {@Widget(type = WidgetType.HTML_TEMPLATE, location = "component://shop/webapp/shop/order/anonymoustrail.ftl")}))
    public interface anonymoustrail {}

    @Screen(name = "genericaddress", location = "component://shop/widget/OrderScreens.xml")
    @Section(widgets = @Widgets(value = {@Widget(type = WidgetType.HTML_TEMPLATE, location = "component://shop/webapp/shop/order/genericaddress.ftl")}))
    public interface genericaddress {}

    @Screen(name = "orderheader", location = "component://shop/widget/OrderScreens.xml")
    @Section(widgets = @Widgets(value = {@Widget(type = WidgetType.HTML_TEMPLATE, location = "component://shop/webapp/shop/order/orderheader.ftl")}))
    public interface orderheader {}

    @Screen(name = "orderitems", location = "component://shop/widget/OrderScreens.xml")
    @Action(type = ActionType.SCRIPT, location = "component://shop/webapp/shop/WEB-INF/actions/order/OrderItems.groovy")
    @Section(widgets = @Widgets(value = {@Widget(type = WidgetType.HTML_TEMPLATE, location = "component://shop/webapp/shop/order/orderitems.ftl")}))
    public interface orderitems {}

    @Screen(name = "customertaxinfo", location = "component://shop/widget/OrderScreens.xml")
    @Action(type = ActionType.SET, field = "customerTaxInfoTemplateLocation", value = "component://shop/webapp/shop/order/customertaxinfo.ftl")
    @Section(widgets = @Widgets(value = {@Widget(type = WidgetType.INCLUDE_SCREEN, name = "customertaxinfoimpl", location = "component://order/widget/ordermgr/OrderEntryOrderScreens.xml")}))
    public interface customertaxinfo {}

    @Screen(name = "CheckoutProgressFull", location = "component://shop/widget/OrderScreens.xml")
    @Action(type = ActionType.SET, field = "checkoutMode", fromField = "checkoutMode", defaultValue = "primary")
    @Action(type = ActionType.SET, field = "activeStep", fromField = "activeStep", valueType = "String", defaultValue = "cart", global = true)
    @Section(widgets = @Widgets(value = {@Widget(type = WidgetType.HTML_TEMPLATE, location = "component://shop/webapp/shop/order/checkoutprogressfull.ftl")}))
    public interface CheckoutProgressFull {}

    @Screen(name = "CommonCheckoutDecorator", location = "component://shop/widget/OrderScreens.xml")
    @Action(type = ActionType.SCRIPT, location = "component://shop/webapp/shop/WEB-INF/actions/order/CheckoutCommon.groovy")
    @Action(type = ActionType.SET, field = "checkoutAllowAnonUnknown", fromField = "checkoutAllowAnonUnknown", valueType = "Boolean", defaultValue = "false")
    @Action(type = ActionType.SET, field = "checkoutAllowEmptyCart", fromField = "checkoutAllowEmptyCart", valueType = "Boolean", defaultValue = "false")
    @Action(type = ActionType.SCRIPT, location = "component://shop/webapp/shop/WEB-INF/actions/common/CommonUserChecks.groovy")
    @Section(condition = @com.ilscipio.scipio.widget.def.screen.Condition(or = {@OrCondition(ifTrue = {"checkoutAllowAnonUnknown", "userIsKnown"})}), widgets = @Widgets(sections = {@SectionNested(condition = @com.ilscipio.scipio.widget.def.screen.Condition(or = {@OrCondition(ifTrue = {"checkoutAllowEmptyCart"}, ifCompare = {@IfCompare(field = "cartSize", operator = "greater", value = "0", type = "Integer")})}), widgets = @WidgetsForContainer(decorator = @DecoratorScreenNested(name = "CommonShopAppDecorator", location = "${parameters.mainDecoratorLocation}", sections = {@DecoratorSectionNested(name = "pre-body", useWhen = "${context.checkoutType == 'full'}", overrideByAutoInclude = true, widgets = @WidgetsForContainer4(sections = {@SectionLeaf(condition = @Condition(type = Compare.class, params = {"checkoutType", "equals", "full"}), widgets = @WidgetsLeaf(includeScreens = {@IncludeScreen(name = "CheckoutProgressFull", location = "component://shop/widget/OrderScreens.xml")}))})), @DecoratorSectionNested(name = "body", widgets = @WidgetsForContainer4(value = {@Widget(type = WidgetType.DECORATOR_SECTION_INCLUDE, name = "body")}))})), failWidgets = @WidgetsForContainer(decorator = @DecoratorScreenNested(name = "CommonShopAppDecorator", location = "${parameters.mainDecoratorLocation}", sections = {@DecoratorSectionNested(name = "body", widgets = @WidgetsForContainer4(value = {@Widget(type = WidgetType.LABEL, text = "${uiLabelMap.EcommerceYourShoppingCartEmpty}.", style = "common-msg-error")}, containers = {@Container4(widgets = {@Widget(type = WidgetType.LINK, text = "${uiLabelMap.EcommerceContinueShopping}", style = "${styles.link_nav_cancel}", target = "main")})}))})))}), failWidgets = @Widgets(decorator = @DecoratorScreen(
        name = "CommonShopAppDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.LABEL, text = "${uiLabelMap.ShopViewPermissionError}", style = "common-msg-error-perm"
            )})
        }
    )))
    public interface CommonCheckoutDecorator {}

    @Screen(name = "custsettings", location = "component://shop/widget/OrderScreens.xml")
    @Action(type = ActionType.SET, field = "rightbarScreenName", value = "rightbar")
    @Action(type = ActionType.SET, field = "MainColumnStyle", value = "rightonly")
    @Action(type = ActionType.SET, field = "parameters.formNameValue", value = "custsetupform", global = true)
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleShippingInformation")
    @Action(type = ActionType.SCRIPT, location = "component://shop/webapp/shop/WEB-INF/actions/order/CustSettings.groovy")
    @Action(type = ActionType.SET, field = "checkoutType", value = "full")
    @Action(type = ActionType.SET, field = "checkoutMode", value = "legacy-anon")
    @Action(type = ActionType.SET, field = "activeStep", value = "customer")
    @Action(type = ActionType.SET, field = "checkoutAllowAnonUnknown", value = "true", valueType = "Boolean")
    @DecoratorScreen(
        name = "CommonCheckoutDecorator",
        location = "component://shop/widget/OrderScreens.xml",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.HTML_TEMPLATE, location = "component://shop/webapp/shop/order/custsettings.ftl"
            )})
        }
    )
    public interface custsettings {}

    @Screen(name = "shipsettings", location = "component://shop/widget/OrderScreens.xml")
    @Action(type = ActionType.SET, field = "rightbarScreenName", value = "rightbar")
    @Action(type = ActionType.SET, field = "MainColumnStyle", value = "rightonly")
    @Action(type = ActionType.SET, field = "parameters.formNameValue", value = "shipsetupform", global = true)
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleShippingInformation")
    @Action(type = ActionType.SCRIPT, location = "component://shop/webapp/shop/WEB-INF/actions/order/ShipSettings.groovy")
    @Action(type = ActionType.SET, field = "checkoutType", value = "full")
    @Action(type = ActionType.SET, field = "checkoutMode", value = "legacy-anon")
    @Action(type = ActionType.SET, field = "activeStep", value = "shippingAddress")
    @DecoratorScreen(
        name = "CommonCheckoutDecorator",
        location = "component://shop/widget/OrderScreens.xml",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.HTML_TEMPLATE, location = "component://shop/webapp/shop/order/shipsettings.ftl"
            )})
        }
    )
    public interface shipsettings {}

    @Screen(name = "optionsettings", location = "component://shop/widget/OrderScreens.xml")
    @Action(type = ActionType.SET, field = "rightbarScreenName", value = "rightbar")
    @Action(type = ActionType.SET, field = "MainColumnStyle", value = "rightonly")
    @Action(type = ActionType.SET, field = "parameters.formNameValue", value = "optsetupform", global = true)
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleShippingOptions")
    @Action(type = ActionType.SCRIPT, location = "component://shop/webapp/shop/WEB-INF/actions/order/OptionSettings.groovy")
    @Action(type = ActionType.SET, field = "checkoutType", value = "full")
    @Action(type = ActionType.SET, field = "checkoutMode", value = "legacy-anon")
    @Action(type = ActionType.SET, field = "activeStep", value = "shippingOptions")
    @DecoratorScreen(
        name = "CommonCheckoutDecorator",
        location = "component://shop/widget/OrderScreens.xml",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.HTML_TEMPLATE, location = "component://shop/webapp/shop/order/optionsettings.ftl"
            )})
        }
    )
    public interface optionsettings {}

    @Screen(name = "paymentoptions", location = "component://shop/widget/OrderScreens.xml")
    @Action(type = ActionType.SET, field = "rightbarScreenName", value = "rightbar")
    @Action(type = ActionType.SET, field = "MainColumnStyle", value = "rightonly")
    @Action(type = ActionType.SET, field = "parameters.formNameValue", value = "paymentoptions", global = true)
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleBillingInformation")
    @Action(type = ActionType.SCRIPT, location = "component://shop/webapp/shop/WEB-INF/actions/order/PaymentOptions.groovy")
    @Action(type = ActionType.SCRIPT, location = "component://order/webapp/ordermgr/WEB-INF/actions/entry/StorePaymentOptions.groovy")
    @Action(type = ActionType.SET, field = "checkoutType", value = "full")
    @Action(type = ActionType.SET, field = "checkoutMode", value = "legacy-anon")
    @Action(type = ActionType.SET, field = "activeStep", value = "billing")
    @DecoratorScreen(
        name = "CommonCheckoutDecorator",
        location = "component://shop/widget/OrderScreens.xml",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.HTML_TEMPLATE, location = "component://shop/webapp/shop/order/paymentoptions.ftl"
            )})
        }
    )
    public interface paymentoptions {}

    @Screen(name = "paymentinformation", location = "component://shop/widget/OrderScreens.xml")
    @Action(type = ActionType.SET, field = "rightbarScreenName", value = "rightbar")
    @Action(type = ActionType.SET, field = "MainColumnStyle", value = "rightonly")
    @Action(type = ActionType.SET, field = "parameters.formNameValue", value = "billsetupform")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleBillingInformation")
    @Action(type = ActionType.SCRIPT, location = "component://shop/webapp/shop/WEB-INF/actions/order/PaymentInformation.groovy")
    @Action(type = ActionType.SCRIPT, location = "component://order/webapp/ordermgr/WEB-INF/actions/entry/StorePaymentOptions.groovy")
    @Action(type = ActionType.SET, field = "checkoutType", value = "full")
    @Action(type = ActionType.SET, field = "checkoutMode", value = "legacy-anon")
    @Action(type = ActionType.SET, field = "activeStep", value = "billingInfo")
    @DecoratorScreen(
        name = "CommonCheckoutDecorator",
        location = "component://shop/widget/OrderScreens.xml",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.HTML_TEMPLATE, location = "component://shop/webapp/shop/order/paymentinformation.ftl"
            )})
        }
    )
    public interface paymentinformation {}

    @Screen(name = "orderreview", location = "component://shop/widget/OrderScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleCheckoutReview")
    @Action(type = ActionType.SET, field = "parameters.formNameValue", value = "orderreview", global = true)
    @Action(type = ActionType.SCRIPT, location = "component://shop/webapp/shop/WEB-INF/actions/order/CheckoutReview.groovy")
    @Action(type = ActionType.SET, field = "checkoutType", value = "full")
    @Action(type = ActionType.SET, field = "checkoutMode", value = "legacy-anon")
    @Action(type = ActionType.SET, field = "activeStep", value = "orderReview")
    @DecoratorScreen(
        name = "CommonCheckoutDecorator",
        location = "component://shop/widget/OrderScreens.xml",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.HTML_TEMPLATE, location = "component://shop/webapp/shop/order/checkoutreview.ftl"
            )})
        }
    )
    public interface orderreview {}

    @Screen(name = "billsettings", location = "component://shop/widget/OrderScreens.xml")
    @Action(type = ActionType.SET, field = "rightbarScreenName", value = "rightbar")
    @Action(type = ActionType.SET, field = "MainColumnStyle", value = "rightonly")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleBillingInformation")
    @Action(type = ActionType.SET, field = "anonymoustrailScreen", value = "component://shop/widget/OrderScreens.xml#anonymoustrail")
    @Action(type = ActionType.SCRIPT, location = "component://shop/webapp/shop/WEB-INF/actions/order/BillSettings.groovy")
    @Action(type = ActionType.SCRIPT, location = "component://order/webapp/ordermgr/WEB-INF/actions/entry/StorePaymentOptions.groovy")
    @Action(type = ActionType.SET, field = "checkoutType", value = "full")
    @Action(type = ActionType.SET, field = "checkoutMode", value = "primary")
    @Action(type = ActionType.SET, field = "activeStep", value = "billing")
    @DecoratorScreen(
        name = "CommonCheckoutDecorator",
        location = "component://shop/widget/OrderScreens.xml",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.HTML_TEMPLATE, location = "component://shop/webapp/shop/order/billsettings.ftl"
            )})
        }
    )
    public interface billsettings {}

    @Screen(name = "checkoutoptions", location = "component://shop/widget/OrderScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleCheckoutOptions")
    @Action(type = ActionType.SCRIPT, location = "component://order/webapp/ordermgr/WEB-INF/actions/entry/CheckoutPayment.groovy")
    @Action(type = ActionType.SCRIPT, location = "component://order/webapp/ordermgr/WEB-INF/actions/entry/CheckoutOptions.groovy")
    @Action(type = ActionType.SCRIPT, location = "component://order/webapp/ordermgr/WEB-INF/actions/entry/StorePaymentOptions.groovy")
    @Action(type = ActionType.SET, field = "checkoutType", value = "quick")
    @Action(type = ActionType.SET, field = "checkoutMode", value = "primary")
    @Action(type = ActionType.SET, field = "checkoutAllowAnonUnknown", value = "true", valueType = "Boolean")
    @DecoratorScreen(
        name = "CommonCheckoutDecorator",
        location = "component://shop/widget/OrderScreens.xml",
        sections = {
            @DecoratorSection(name = "body")
        }
    )
    public interface checkoutoptions {}

    @Screen(name = "checkoutshippingaddress", location = "component://shop/widget/OrderScreens.xml")
    @Action(type = ActionType.SET, field = "rightbarScreenName", value = "rightbar")
    @Action(type = ActionType.SET, field = "MainColumnStyle", value = "rightonly")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleCheckoutOptions")
    @Action(type = ActionType.SCRIPT, location = "component://shop/webapp/shop/WEB-INF/actions/order/CustSettings.groovy")
    @Action(type = ActionType.SCRIPT, location = "component://order/webapp/ordermgr/WEB-INF/actions/entry/CheckoutShippingAddress.groovy")
    @Action(type = ActionType.SET, field = "checkoutType", value = "full")
    @Action(type = ActionType.SET, field = "checkoutMode", value = "primary")
    @Action(type = ActionType.SET, field = "activeStep", value = "shippingAddress")
    @Action(type = ActionType.SET, field = "checkoutAllowAnonUnknown", value = "true", valueType = "Boolean")
    @DecoratorScreen(
        name = "CommonCheckoutDecorator",
        location = "component://shop/widget/OrderScreens.xml",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.HTML_TEMPLATE, location = "component://shop/webapp/shop/order/checkoutshippingaddress.ftl"
            )})
        }
    )
    public interface checkoutshippingaddress {}

    @Screen(name = "checkoutshippingoptions", location = "component://shop/widget/OrderScreens.xml")
    @Action(type = ActionType.SET, field = "rightbarScreenName", value = "rightbar")
    @Action(type = ActionType.SET, field = "MainColumnStyle", value = "rightonly")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleCheckoutOptions")
    @Action(type = ActionType.SCRIPT, location = "component://shop/webapp/shop/WEB-INF/actions/order/CheckoutShippingOptions.groovy")
    @Action(type = ActionType.SET, field = "checkoutType", value = "full")
    @Action(type = ActionType.SET, field = "checkoutMode", value = "primary")
    @Action(type = ActionType.SET, field = "activeStep", value = "shippingOptions")
    @DecoratorScreen(
        name = "CommonCheckoutDecorator",
        location = "component://shop/widget/OrderScreens.xml",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.HTML_TEMPLATE, location = "component://shop/webapp/shop/order/checkoutshippingoptions.ftl"
            )})
        }
    )
    public interface checkoutshippingoptions {}

    @Screen(name = "splitship", location = "component://shop/widget/OrderScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleSplitItemsForShipping")
    @Action(type = ActionType.SCRIPT, location = "component://order/webapp/ordermgr/WEB-INF/actions/entry/SplitShip.groovy")
    @Action(type = ActionType.SET, field = "checkoutType", value = "full")
    @Action(type = ActionType.SET, field = "checkoutMode", value = "primary")
    @Action(type = ActionType.SET, field = "activeStep", value = "shippingAddress")
    @DecoratorScreen(
        name = "CommonCheckoutDecorator",
        location = "component://shop/widget/OrderScreens.xml",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.HTML_TEMPLATE, location = "component://shop/webapp/shop/order/splitship.ftl"
            )})
        }
    )
    public interface splitship {}

    @Screen(name = "checkoutpayment", location = "component://shop/widget/OrderScreens.xml")
    @Action(type = ActionType.SET, field = "rightbarScreenName", value = "rightbar")
    @Action(type = ActionType.SET, field = "MainColumnStyle", value = "rightonly")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleCheckoutOptions")
    @Action(type = ActionType.SCRIPT, location = "component://order/webapp/ordermgr/WEB-INF/actions/entry/CheckoutPayment.groovy")
    @Action(type = ActionType.SCRIPT, location = "component://order/webapp/ordermgr/WEB-INF/actions/entry/StorePaymentOptions.groovy")
    @Action(type = ActionType.SET, field = "checkoutType", value = "full")
    @Action(type = ActionType.SET, field = "checkoutMode", value = "primary")
    @Action(type = ActionType.SET, field = "activeStep", value = "billing")
    @DecoratorScreen(
        name = "CommonCheckoutDecorator",
        location = "component://shop/widget/OrderScreens.xml",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.HTML_TEMPLATE, location = "component://shop/webapp/shop/order/checkoutpayment.ftl"
            )})
        }
    )
    public interface checkoutpayment {}

    @Screen(name = "checkoutreview", location = "component://shop/widget/OrderScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleCheckoutReview")
    @Action(type = ActionType.SET, field = "parameters.formNameValue", value = "orderreview", global = true)
    @Action(type = ActionType.SCRIPT, location = "component://shop/webapp/shop/WEB-INF/actions/order/CheckoutReview.groovy")
    @Action(type = ActionType.SET, field = "checkoutType", value = "full")
    @Action(type = ActionType.SET, field = "checkoutMode", value = "primary")
    @Action(type = ActionType.SET, field = "activeStep", value = "orderReview")
    @DecoratorScreen(
        name = "CommonCheckoutDecorator",
        location = "component://shop/widget/OrderScreens.xml",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.HTML_TEMPLATE, location = "component://shop/webapp/shop/order/checkoutreview.ftl"
            )})
        }
    )
    public interface checkoutreview {}

    @Screen(name = "ordercomplete", location = "component://shop/widget/OrderScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "EcommerceOrderConfirmation")
    @Action(type = ActionType.SCRIPT, location = "component://shop/webapp/shop/WEB-INF/actions/order/OrderStatus.groovy")
    @DecoratorScreen(
        name = "CommonShopAppDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            // W1-17d: the signed pay link of the order mail (hubPayLinkValid, OrderStatus.groovy) opens the page without a login
            @DecoratorSection(name = "body", sections = {
                @InlineSection(condition = @com.ilscipio.scipio.widget.def.screen.Condition(or = {
                    @OrCondition(ifTrue = {"hubPayLinkValid", "userIsKnown"})}), widgets = @InlineWidgets(value = {
                        @Widget(type = WidgetType.HTML_TEMPLATE, location = "component://shop/webapp/shop/order/ordercomplete.ftl"
                    )}), failWidgets = @InlineWidgets(value = {
                        @Widget(type = WidgetType.LABEL, text = "${uiLabelMap.ShopViewPermissionError}", style = "common-msg-error-perm"
                    )}))})
        }
    )
    public interface ordercomplete {}

    @Screen(name = "orderhistory", location = "component://shop/widget/OrderScreens.xml")
    @Action(type = ActionType.SET, field = "rightbarScreenName", value = "rightbar")
    @Action(type = ActionType.SET, field = "MainColumnStyle", value = "rightonly")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleOrderHistory")
    @Action(type = ActionType.SCRIPT, location = "component://shop/webapp/shop/WEB-INF/actions/order/OrderHistory.groovy")
    @DecoratorScreen(
        name = "CommonShopAppDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", sections = {
                @InlineSection(condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {
                    @Condition(type = True.class, params = {"userHasAccount"})}), widgets = @InlineWidgets(value = {
                        @Widget(type = WidgetType.HTML_TEMPLATE, location = "component://shop/webapp/shop/order/orderhistory.ftl"
                    )}), failWidgets = @InlineWidgets(value = {
                        @Widget(type = WidgetType.LABEL, text = "${uiLabelMap.ShopViewPermissionError}", style = "common-msg-error-perm"
                    )}))})
        }
    )
    public interface orderhistory {}

    @Screen(name = "orderdownloads", location = "component://shop/widget/OrderScreens.xml")
    @Action(type = ActionType.SET, field = "rightbarScreenName", value = "rightbar")
    @Action(type = ActionType.SET, field = "MainColumnStyle", value = "rightonly")
    @Action(type = ActionType.SET, field = "titleProperty", value = "EcommerceDownloadsAvailableTitle")
    @Action(type = ActionType.SCRIPT, location = "component://shop/webapp/shop/WEB-INF/actions/order/OrderHistory.groovy")
    @DecoratorScreen(
        name = "CommonShopAppDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", sections = {
                @InlineSection(condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {
                    @Condition(type = True.class, params = {"userIsKnown"})}), widgets = @InlineWidgets(screenlets = {
                        @Screenlet(title = "${uiLabelMap.EcommerceDownloadsAvailableTitle}", includeScreens = {
                            @IncludeScreen(name = "orderdownloadscontent", location = "component://shop/widget/OrderScreens.xml"
                        )})}), failWidgets = @InlineWidgets(value = {
                            @Widget(type = WidgetType.LABEL, text = "${uiLabelMap.ShopViewPermissionError}", style = "common-msg-error-perm"
                        )}))})
        }
    )
    public interface orderdownloads {}

    @Screen(name = "orderdownloadscontent", location = "component://shop/widget/OrderScreens.xml")
    @Section(widgets = @Widgets(value = {@Widget(type = WidgetType.HTML_TEMPLATE, location = "component://shop/webapp/shop/order/orderdownloads.ftl")}))
    public interface orderdownloadscontent {}

    @Screen(name = "orderstatus", location = "component://shop/widget/OrderScreens.xml")
    @Action(type = ActionType.SET, field = "rightbarScreenName", value = "rightbar")
    @Action(type = ActionType.SET, field = "MainColumnStyle", value = "rightonly")
    @Action(type = ActionType.SET, field = "maySelectItems", value = "Y")
    @Action(type = ActionType.SCRIPT, location = "component://shop/webapp/shop/WEB-INF/actions/order/OrderStatus.groovy")
    @DecoratorScreen(
        name = "CommonShopAppDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", sections = {
                @InlineSection(condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {
                    @Condition(type = True.class, params = {"userIsKnown"})}), widgets = @InlineWidgets(value = {
                        @Widget(type = WidgetType.HTML_TEMPLATE, location = "component://shop/webapp/shop/order/orderstatus.ftl"
                    )}), failWidgets = @InlineWidgets(value = {
                        @Widget(type = WidgetType.LABEL, text = "${uiLabelMap.ShopViewPermissionError}", style = "common-msg-error-perm"
                    )}))})
        }
    )
    public interface orderstatus {}

    @Screen(name = "orderviewonly", location = "component://shop/widget/OrderScreens.xml")
    @Section(widgets = @Widgets(value = {@Widget(type = WidgetType.INCLUDE_SCREEN, name = "ordercomplete")}))
    public interface orderviewonly {}

    @Screen(name = "orderprint", location = "component://shop/widget/OrderScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "EcommerceOrderConfirmation")
    @Action(type = ActionType.SET, field = "printable", value = "true", valueType = "Boolean")
    @Action(type = ActionType.SCRIPT, location = "component://shop/webapp/shop/WEB-INF/actions/order/OrderStatus.groovy")
    @DecoratorScreen(
        name = "CommonEmptyDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", sections = {
                @InlineSection(condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {
                    @Condition(type = True.class, params = {"userIsKnown"})}), widgets = @InlineWidgets(value = {
                        @Widget(type = WidgetType.HTML_TEMPLATE, location = "component://shop/webapp/shop/order/ordercomplete.ftl"
                    )}), failWidgets = @InlineWidgets(value = {
                        @Widget(type = WidgetType.LABEL, text = "${uiLabelMap.ShopViewPermissionError}", style = "p"
                    )}))})
        }
    )
    public interface orderprint {}

    @Screen(name = "requestreturn", location = "component://shop/widget/OrderScreens.xml")
    @Action(type = ActionType.SET, field = "rightbarScreenName", value = "rightbar")
    @Action(type = ActionType.SET, field = "MainColumnStyle", value = "rightonly")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleRequestReturn")
    @Action(type = ActionType.SCRIPT, location = "component://shop/webapp/shop/WEB-INF/actions/order/RequestReturn.groovy")
    @DecoratorScreen(
        name = "CommonShopAppDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", sections = {
                @InlineSection(condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {
                    @Condition(type = True.class, params = {"userHasAccount"}),
                    @Condition(type = True.class, params = {"hasReturnPermission"
                })}), widgets = @InlineWidgets(value = {
                    @Widget(type = WidgetType.HTML_TEMPLATE, location = "component://shop/webapp/shop/order/requestreturn.ftl"
                )}), failWidgets = @InlineWidgets(value = {
                    @Widget(type = WidgetType.LABEL, text = "${uiLabelMap.ShopViewPermissionError}", style = "common-msg-error-perm"
                )}))})
        }
    )
    public interface requestreturn {}

    @Screen(name = "quickAnonOrderHeader", location = "component://shop/widget/OrderScreens.xml")
    @Section(widgets = @Widgets(value = {@Widget(type = WidgetType.HTML_TEMPLATE, location = "component://shop/webapp/shop/order/quickAnonOrderHeader.ftl")}))
    public interface quickAnonOrderHeader {}

    @Screen(name = "quickAnonCheckoutLinks", location = "component://shop/widget/OrderScreens.xml")
    @Action(type = ActionType.SCRIPT, location = "component://shop/webapp/shop/WEB-INF/actions/order/QuickAnonCheckoutLinks.groovy")
    @Section(widgets = @Widgets(value = {@Widget(type = WidgetType.HTML_TEMPLATE, location = "component://shop/webapp/shop/order/quickAnonCheckoutLinks.ftl")}))
    public interface quickAnonCheckoutLinks {}

    @Screen(name = "quickAnonCheckoutDecorator", location = "component://shop/widget/OrderScreens.xml")
    @Action(type = ActionType.SCRIPT, location = "component://shop/webapp/shop/WEB-INF/actions/order/QuickAnonCheckoutLinks.groovy")
    @Action(type = ActionType.SET, field = "checkoutType", value = "quick")
    @Action(type = ActionType.SET, field = "checkoutMode", value = "legacy-anon")
    @Action(type = ActionType.SET, field = "checkoutAllowAnonUnknown", value = "true", valueType = "Boolean")
    @DecoratorScreen(
        name = "CommonCheckoutDecorator",
        location = "component://shop/widget/OrderScreens.xml",
        sections = {
            @DecoratorSection(name = "pre-body", value = {
                @Widget(type = WidgetType.INCLUDE_SCREEN, name = "quickAnonCheckoutLinks", location = "component://shop/widget/OrderScreens.xml"
            )}),
            @DecoratorSection(name = "body", value = {@Widget(type = WidgetType.DECORATOR_SECTION_INCLUDE, name = "body")})
        }
    )
    public interface quickAnonCheckoutDecorator {}

    @Screen(name = "quickAnonCustSettings", location = "component://shop/widget/OrderScreens.xml")
    @Action(type = ActionType.SET, field = "rightbarScreenName", value = "rightbar")
    @Action(type = ActionType.SET, field = "MainColumnStyle", value = "rightonly")
    @Action(type = ActionType.SET, field = "parameters.formNameValue", value = "quickAnonCustSetupForm", global = true)
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleShippingInformation")
    @Action(type = ActionType.SET, field = "layoutSettings.javaScripts[+0]", value = "/shop/images/quickAnonCustSettings.js", global = true)
    @Action(type = ActionType.SCRIPT, location = "component://shop/webapp/shop/WEB-INF/actions/order/QuickAnonCustSettings.groovy")
    @DecoratorScreen(
        name = "quickAnonCheckoutDecorator",
        location = "component://shop/widget/OrderScreens.xml",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.HTML_TEMPLATE, location = "component://shop/webapp/shop/order/quickAnonCustSettings.ftl"
            )})
        }
    )
    public interface quickAnonCustSettings {}

    @Screen(name = "quickAnonOptionSettings", location = "component://shop/widget/OrderScreens.xml")
    @Action(type = ActionType.PROPERTY_MAP, resource = "ShopUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "EcommerceUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "ProductUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "AccountingUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "PartyUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "OrderUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "ContentUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "CommonUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleShippingOptions")
    @Action(type = ActionType.SCRIPT, location = "component://shop/webapp/shop/WEB-INF/actions/order/QuickAnonOptionSettings.groovy")
    @Section(widgets = @Widgets(value = {@Widget(type = WidgetType.HTML_TEMPLATE, location = "component://shop/webapp/shop/order/quickAnonOptionSettings.ftl")}))
    public interface quickAnonOptionSettings {}

    @Screen(name = "quickAnonPaymentInformation", location = "component://shop/widget/OrderScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleBillingInformation")
    @Action(type = ActionType.SCRIPT, location = "component://shop/webapp/shop/WEB-INF/actions/order/QuickAnonPaymentInformation.groovy")
    @Action(type = ActionType.SCRIPT, location = "component://order/webapp/ordermgr/WEB-INF/actions/entry/StorePaymentOptions.groovy")
    @Section(widgets = @Widgets(value = {@Widget(type = WidgetType.HTML_TEMPLATE, location = "component://shop/webapp/shop/order/quickAnonPaymentInformation.ftl")}))
    public interface quickAnonPaymentInformation {}

    @Screen(name = "quickAnonOrderReview", location = "component://shop/widget/OrderScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleCheckoutReview")
    @Action(type = ActionType.SET, field = "parameters.formNameValue", value = "quickAnonOrderReview", global = true)
    @Action(type = ActionType.SCRIPT, location = "component://shop/webapp/shop/WEB-INF/actions/order/CheckoutReview.groovy")
    @DecoratorScreen(
        name = "quickAnonCheckoutDecorator",
        location = "component://shop/widget/OrderScreens.xml",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.HTML_TEMPLATE, location = "component://shop/webapp/shop/order/quickAnonCheckoutReview.ftl"
            )})
        }
    )
    public interface quickAnonOrderReview {}

    @Screen(name = "quickAnonCcInfo", location = "component://shop/widget/OrderScreens.xml")
    @Action(type = ActionType.PROPERTY_MAP, resource = "ShopUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "EcommerceUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "ProductUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "AccountingUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "PartyUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "OrderUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "ContentUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "CommonUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.SCRIPT, location = "component://shop/webapp/shop/WEB-INF/actions/order/QuickAnonPaymentInformation.groovy")
    @Section(widgets = @Widgets(value = {@Widget(type = WidgetType.HTML_TEMPLATE, location = "component://common/webcommon/includes/messages.ftl"), @Widget(type = WidgetType.HTML_TEMPLATE, location = "component://shop/webapp/shop/order/ccinfo.ftl")}))
    public interface quickAnonCcInfo {}

    @Screen(name = "quickAnonEftInfo", location = "component://shop/widget/OrderScreens.xml")
    @Action(type = ActionType.PROPERTY_MAP, resource = "ShopUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "EcommerceUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "ProductUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "AccountingUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "PartyUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "OrderUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "ContentUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "CommonUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.SCRIPT, location = "component://shop/webapp/shop/WEB-INF/actions/order/QuickAnonPaymentInformation.groovy")
    @Section(widgets = @Widgets(value = {@Widget(type = WidgetType.HTML_TEMPLATE, location = "component://common/webcommon/includes/messages.ftl"), @Widget(type = WidgetType.HTML_TEMPLATE, location = "component://shop/webapp/shop/order/eftinfo.ftl")}))
    public interface quickAnonEftInfo {}

    @Screen(name = "quickAnonGcInfo", location = "component://shop/widget/OrderScreens.xml")
    @Action(type = ActionType.PROPERTY_MAP, resource = "ShopUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "EcommerceUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "ProductUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "AccountingUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "PartyUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "OrderUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "ContentUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "CommonUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.SCRIPT, location = "component://shop/webapp/shop/WEB-INF/actions/order/QuickAnonPaymentInformation.groovy")
    @Section(widgets = @Widgets(value = {@Widget(type = WidgetType.HTML_TEMPLATE, location = "component://common/webcommon/includes/messages.ftl"), @Widget(type = WidgetType.HTML_TEMPLATE, location = "component://shop/webapp/shop/order/gcinfo.ftl")}))
    public interface quickAnonGcInfo {}

    @Screen(name = "quickAnonOrderItems", location = "component://shop/widget/OrderScreens.xml")
    @Action(type = ActionType.PROPERTY_MAP, resource = "ShopUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "EcommerceUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "ProductUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "AccountingUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "PartyUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "OrderUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "ContentUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "CommonUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.SCRIPT, location = "component://shop/webapp/shop/WEB-INF/actions/order/CheckoutReview.groovy")
    @Action(type = ActionType.SCRIPT, location = "component://shop/webapp/shop/WEB-INF/actions/order/QuickAnonPaymentInformation.groovy")
    @Action(type = ActionType.SCRIPT, location = "component://shop/webapp/shop/WEB-INF/actions/order/OrderItems.groovy")
    @Section(widgets = @Widgets(value = {@Widget(type = WidgetType.HTML_TEMPLATE, location = "component://shop/webapp/shop/order/orderitems.ftl")}))
    public interface quickAnonOrderItems {}

    @Screen(name = "OnePageCheckout", location = "component://shop/widget/OrderScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "EcommerceOnePageCheckout")
    @Action(type = ActionType.SET, field = "layoutSettings.javaScriptsFooter[]", value = "/shop/images/checkoutProcess.js", global = true)
    @Action(type = ActionType.SET, field = "layoutSettings.javaScriptsFooter[]", value = "/ordermgr-js/geoAutoCompleter.js", global = true)
    @Action(type = ActionType.SCRIPT, location = "component://shop/webapp/shop/WEB-INF/actions/customer/EditShippingAddress.groovy")
    @Action(type = ActionType.SCRIPT, location = "component://shop/webapp/shop/WEB-INF/actions/customer/EditBillingAddress.groovy")
    @Action(type = ActionType.SCRIPT, location = "component://shop/webapp/shop/WEB-INF/actions/customer/EditEmailAndTelecomNumber.groovy")
    @Action(type = ActionType.SCRIPT, location = "component://shop/webapp/shop/WEB-INF/actions/customer/OnePageCheckoutOptions.groovy")
    @Action(type = ActionType.SET, field = "checkoutType", value = "onepage")
    @Action(type = ActionType.SET, field = "checkoutAllowAnonUnknown", value = "true", valueType = "Boolean")
    @DecoratorScreen(
        name = "CommonCheckoutDecorator",
        location = "component://shop/widget/OrderScreens.xml",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.HTML_TEMPLATE, location = "component://shop/webapp/shop/order/OnePageCheckoutProcess.ftl"
            )})
        }
    )
    public interface OnePageCheckout {}

}
