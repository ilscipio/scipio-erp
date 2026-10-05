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
public class OrdermgrOrderEntryOrderScreens {

    @Screen(name = "quickFinalizeOrder", location = "component://order/widget/ordermgr/OrderEntryOrderScreens.xml")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "orderentry")
    @Action(type = ActionType.SET, field = "stepLabelId", value = "PageTitleCheckoutOptions")
    @Action(type = ActionType.SET, field = "stepTitleId", value = "PageTitleCheckoutOptions")
    @Action(type = ActionType.SET, field = "checkoutType", value = "quick")
    @Action(type = ActionType.SET, field = "layoutSettings.javaScriptsFooter[]", value = "/ordermgr-js/geoAutoCompleter.js", global = true)
    @Action(type = ActionType.SCRIPT, location = "component://order/webapp/ordermgr/WEB-INF/actions/entry/CheckoutPayment.groovy")
    @Action(type = ActionType.SCRIPT, location = "component://order/webapp/ordermgr/WEB-INF/actions/entry/CheckoutOptions.groovy")
    @Action(type = ActionType.SCRIPT, location = "component://order/webapp/ordermgr/WEB-INF/actions/entry/StorePaymentOptions.groovy")
    @Action(type = ActionType.PROPERTY_TO_FIELD, field = "giftEnable", resource = "order", property = "orderPreference.giftEnable", defaultValue = "Y")
    @DecoratorScreen(
        name = "CommonOrderCheckoutDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.HTML_TEMPLATE, location = "component://order/webapp/ordermgr/entry/checkoutoptions.ftl"
            )})
        }
    )
    public interface quickFinalizeOrder {}

    @Screen(name = "CustSettings", location = "component://order/widget/ordermgr/OrderEntryOrderScreens.xml")
    @Action(type = ActionType.SET, field = "stepLabelId", value = "PartyParties")
    @Action(type = ActionType.SCRIPT, location = "component://order/webapp/ordermgr/WEB-INF/actions/entry/ShipSettings.groovy")
    @DecoratorScreen(
        name = "CommonOrderCheckoutDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.HTML_TEMPLATE, location = "component://order/webapp/ordermgr/entry/custsettings.ftl"
            )})
        }
    )
    public interface CustSettings {}

    @Screen(name = "ShipSettings", location = "component://order/widget/ordermgr/OrderEntryOrderScreens.xml")
    @Action(type = ActionType.SET, field = "stepTitleId", value = "OrderOrderEntryShipToSettings")
    @Action(type = ActionType.SET, field = "stepLabelId", value = "FacilityShipping")
    @Action(type = ActionType.SCRIPT, location = "component://order/webapp/ordermgr/WEB-INF/actions/entry/ShipSettings.groovy")
    @DecoratorScreen(
        name = "CommonOrderCheckoutDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.HTML_TEMPLATE, location = "component://order/webapp/ordermgr/entry/shipsettings.ftl"
            )})
        }
    )
    public interface ShipSettings {}

    @Screen(name = "EditShipAddress", location = "component://order/widget/ordermgr/OrderEntryOrderScreens.xml")
    @Action(type = ActionType.SET, field = "stepTitleId", value = "OrderOrderEntryShipToSettings")
    @Action(type = ActionType.SET, field = "stepLabelId", value = "FacilityShipping")
    @Action(type = ActionType.SCRIPT, location = "component://order/webapp/ordermgr/WEB-INF/actions/entry/ShipSettings.groovy")
    @Action(type = ActionType.SET, field = "dependentForm", value = "checkoutsetupform")
    @Action(type = ActionType.SET, field = "paramKey", value = "countryGeoId")
    @Action(type = ActionType.SET, field = "mainId", value = "countryGeoId")
    @Action(type = ActionType.SET, field = "dependentId", value = "stateProvinceGeoId")
    @Action(type = ActionType.SET, field = "requestName", value = "getAssociatedStateList")
    @Action(type = ActionType.SET, field = "responseName", value = "stateList")
    @Action(type = ActionType.SET, field = "dependentKeyName", value = "geoId")
    @Action(type = ActionType.SET, field = "descName", value = "geoName")
    @Action(type = ActionType.SET, field = "selectedDependentOption", fromField = "mechMap.postalAddress.stateProvinceGeoId", defaultValue = "_none_")
    @DecoratorScreen(
        name = "CommonOrderCheckoutDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.HTML_TEMPLATE, location = "component://common/webcommon/includes/setDependentDropdownValuesJs.ftl"
            ),
            @Widget(type = WidgetType.HTML_TEMPLATE, location = "component://order/webapp/ordermgr/entry/editShipAddress.ftl"
            )})
        }
    )
    public interface EditShipAddress {}

    @Screen(name = "SetItemShipGroups", location = "component://order/widget/ordermgr/OrderEntryOrderScreens.xml")
    @Action(type = ActionType.SET, field = "stepTitleId", value = "SetItemShipGroups")
    @Action(type = ActionType.SET, field = "stepLabelId", value = "OrderShipGroups")
    @Action(type = ActionType.SCRIPT, location = "component://order/webapp/ordermgr/WEB-INF/actions/entry/SetShoppingCart.groovy")
    @DecoratorScreen(
        name = "CommonOrderCheckoutDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.HTML_TEMPLATE, location = "component://order/webapp/ordermgr/entry/SetItemShipGroups.ftl"
            )})
        }
    )
    public interface SetItemShipGroups {}

    @Screen(name = "OptionSettings", location = "component://order/widget/ordermgr/OrderEntryOrderScreens.xml")
    @Action(type = ActionType.SET, field = "stepTitleId", value = "OrderShippingOptions")
    @Action(type = ActionType.SET, field = "stepLabelId", value = "OrderShippingOptions")
    @Action(type = ActionType.SCRIPT, location = "component://order/webapp/ordermgr/WEB-INF/actions/entry/OptionSettings.groovy")
    @Action(type = ActionType.PROPERTY_TO_FIELD, field = "giftEnable", resource = "order", property = "orderPreference.giftEnable", defaultValue = "Y")
    @DecoratorScreen(
        name = "CommonOrderCheckoutDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.HTML_TEMPLATE, location = "component://order/webapp/ordermgr/entry/optionsettings.ftl"
            )})
        }
    )
    public interface OptionSettings {}

    @Screen(name = "BillSettings", location = "component://order/widget/ordermgr/OrderEntryOrderScreens.xml")
    @Action(type = ActionType.SET, field = "stepTitleId", value = "OrderOrderEntryPaymentSettings")
    @Action(type = ActionType.SET, field = "stepLabelId", value = "AccountingPayment")
    @Action(type = ActionType.SCRIPT, location = "component://order/webapp/ordermgr/WEB-INF/actions/entry/BillSettings.groovy")
    @DecoratorScreen(
        name = "CommonOrderCheckoutDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.HTML_TEMPLATE, location = "component://order/webapp/ordermgr/entry/billsettings.ftl"
            )})
        }
    )
    public interface BillSettings {}

    @Screen(name = "SetAdditionalParty", location = "component://order/widget/ordermgr/OrderEntryOrderScreens.xml")
    @Action(type = ActionType.SET, field = "stepTitleId", value = "PartyAdditionalPartyEntry")
    @Action(type = ActionType.SET, field = "stepLabelId", value = "PartyParties")
    @Action(type = ActionType.SCRIPT, location = "component://order/webapp/ordermgr/WEB-INF/actions/entry/SetAdditionalParty.groovy")
    @Action(type = ActionType.SCRIPT, location = "component://order/webapp/ordermgr/WEB-INF/actions/entry/AdditionalPartyListing.groovy")
    @DecoratorScreen(
        name = "CommonOrderCheckoutDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.HTML_TEMPLATE, location = "component://common/webcommon/includes/setMultipleSelectJs.ftl"
            ),
            @Widget(type = WidgetType.HTML_TEMPLATE, location = "component://order/webapp/ordermgr/entry/setAdditionalParty.ftl"
            ),
            @Widget(type = WidgetType.HTML_TEMPLATE, location = "component://order/webapp/ordermgr/entry/additionalPartyListing.ftl"
            )})
        }
    )
    public interface SetAdditionalParty {}

    @Screen(name = "OrderTerms", location = "component://order/widget/ordermgr/OrderEntryOrderScreens.xml")
    @Action(type = ActionType.SET, field = "stepTitleId", value = "OrderOrderEntryOrderTerms")
    @Action(type = ActionType.SET, field = "stepLabelId", value = "OrderOrderTerms")
    @Action(type = ActionType.SCRIPT, location = "component://order/webapp/ordermgr/WEB-INF/actions/entry/OrderTerms.groovy")
    @DecoratorScreen(
        name = "CommonOrderCheckoutDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.HTML_TEMPLATE, location = "component://order/webapp/ordermgr/entry/orderterms.ftl"
            )})
        }
    )
    public interface OrderTerms {}

    @Screen(name = "ConfirmOrder", location = "component://order/widget/ordermgr/OrderEntryOrderScreens.xml")
    @Action(type = ActionType.SET, field = "stepTitleId", value = "OrderOrderConfirmation")
    @Action(type = ActionType.SET, field = "stepLabelId", value = "OrderReviewOrder")
    @Action(type = ActionType.PROPERTY_TO_FIELD, field = "giftEnable", resource = "order", property = "orderPreference.giftEnable", defaultValue = "Y")
    @Action(type = ActionType.SCRIPT, location = "component://order/webapp/ordermgr/WEB-INF/actions/entry/CheckoutReview.groovy")
    @DecoratorScreen(
        name = "CommonOrderCheckoutDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", containers = {
                @Container(style = "${styles.grid_row}", containers = {
                    @Container2(style = "${styles.grid_large}6 ${styles.grid_cell}", htmlTemplates = {
                        @HtmlTemplate(location = "component://order/webapp/ordermgr/entry/order/orderheaderinfo.ftl"
                    )}),
                    @Container2(style = "${styles.grid_large}6 ${styles.grid_cell}", htmlTemplates = {
                        @HtmlTemplate(location = "component://order/webapp/ordermgr/entry/order/orderpaymentinfo.ftl"
                    )})}),
                    @Container(style = "${styles.grid_row}", containers = {
                        @Container2(style = "${styles.grid_large}12 ${styles.grid_cell}", htmlTemplates = {
                            @HtmlTemplate(location = "component://order/webapp/ordermgr/entry/order/shipGroupConfirmSummary.ftl"
                        ),
                        @HtmlTemplate(location = "component://order/webapp/ordermgr/entry/order/orderitems.ftl"
                    )})})})
        }
    )
    public interface ConfirmOrder {}

    @Screen(name = "checkoutshippingaddress", location = "component://order/widget/ordermgr/OrderEntryOrderScreens.xml")
    @Action(type = ActionType.SET, field = "rightbarScreenName", value = "rightbar")
    @Action(type = ActionType.SET, field = "MainColumnStyle", value = "rightonly")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleCheckoutOptions")
    @Action(type = ActionType.SCRIPT, location = "component://order/webapp/ordermgr/WEB-INF/actions/entry/CheckoutShippingAddress.groovy")
    @DecoratorScreen(
        name = "CommonOrderAppDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.HTML_TEMPLATE, location = "component://order/webapp/ordermgr/entry/checkoutshippingaddress.ftl"
            )})
        }
    )
    public interface checkoutshippingaddress {}

    @Screen(name = "checkoutpayment", location = "component://order/widget/ordermgr/OrderEntryOrderScreens.xml")
    @Action(type = ActionType.SET, field = "rightbarScreenName", value = "rightbar")
    @Action(type = ActionType.SET, field = "MainColumnStyle", value = "rightonly")
    @Action(type = ActionType.SET, field = "stepLabelId", value = "AccountingPayment")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleCheckoutOptions")
    @Action(type = ActionType.SET, field = "layoutSettings.javaScriptsFooter[]", value = "/ordermgr-js/geoAutoCompleter.js", global = true)
    @Action(type = ActionType.SCRIPT, location = "component://order/webapp/ordermgr/WEB-INF/actions/entry/CheckoutPayment.groovy")
    @Action(type = ActionType.SCRIPT, location = "component://order/webapp/ordermgr/WEB-INF/actions/entry/StorePaymentOptions.groovy")
    @DecoratorScreen(
        name = "CommonOrderCheckoutDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.HTML_TEMPLATE, location = "component://order/webapp/ordermgr/entry/checkoutpayment.ftl"
            )})
        }
    )
    public interface checkoutpayment {}

    @Screen(name = "customertaxinfo", location = "component://order/widget/ordermgr/OrderEntryOrderScreens.xml")
    @Action(type = ActionType.SET, field = "customerTaxInfoTemplateLocation", value = "component://order/webapp/ordermgr/entry/customertaxinfo.ftl")
    @Section(widgets = @Widgets(value = {@Widget(type = WidgetType.INCLUDE_SCREEN, name = "customertaxinfoimpl")}))
    public interface customertaxinfo {}

    @Screen(name = "customertaxinfoimpl", location = "component://order/widget/ordermgr/OrderEntryOrderScreens.xml")
    @Action(type = ActionType.SET, field = "partyId", fromField = "userLogin.partyId")
    @Action(type = ActionType.ENTITY_CONDITION, entityName = "PartyTaxAuthInfoAndDetail", list = "partyTaxAuthInfoAndDetailList", conditions = {@ConditionExpr(fieldName = "partyId", fromField = "partyId")}, orderBy = {"geoCode", "groupName"})
    @Action(type = ActionType.ENTITY_CONDITION, entityName = "TaxAuthorityAndDetail", list = "taxAuthorityAndDetailList", orderBy = {"geoCode", "groupName"})
    @Section(widgets = @Widgets(value = {@Widget(type = WidgetType.HTML_TEMPLATE, location = "${customerTaxInfoTemplateLocation}")}))
    public interface customertaxinfoimpl {}

    @Screen(name = "LookupBulkAddSupplierProductsInApprovedOrder", location = "component://order/widget/ordermgr/OrderEntryOrderScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleLookupBulkAddSupplierProduct")
    @Action(type = ActionType.SCRIPT, location = "component://order/webapp/ordermgr/WEB-INF/actions/entry/cart/LookupBulkAddSupplierProducts.groovy")
    @DecoratorScreen(
        name = "CommonOrderAppDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", containers = {
                @Container(style = "screenlet", htmlTemplates = {
                    @HtmlTemplate(location = "component://order/webapp/ordermgr/entry/OrderEntryCatalogTabBar.ftl"
                )}, containers = {
                    @Container2(style = "screenlet-body", includeForms = {
                        @IncludeForm(name = "LookupBulkAddSupplierProductsInApprovedOrder", location = "component://order/widget/ordermgr/OrderForms.xml"
                    )})})})
        }
    )
    public interface LookupBulkAddSupplierProductsInApprovedOrder {}

    @Screen(name = "splitship", location = "component://order/widget/ordermgr/OrderEntryOrderScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleSplitItemsForShipping")
    @Action(type = ActionType.SCRIPT, location = "component://order/webapp/ordermgr/WEB-INF/actions/entry/SplitShip.groovy")
    @DecoratorScreen(
        name = "CommonOrderAppDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.HTML_TEMPLATE, location = "component://order/webapp/ordermgr/entry/SplitShip.ftl"
            )})
        }
    )
    public interface splitship {}

}
