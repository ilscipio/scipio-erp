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
package com.ilscipio.scipio.order.service;

import com.ilscipio.scipio.service.def.*;
import com.ilscipio.scipio.service.def.Service.GroupInvoke;

/**
 * Auto-generated annotation-based service definitions.
 *
 * <p>Generated from services.xml by convertXmlToAnnotation Gradle task.</p>
 *
 * <p>SCIPIO: 4.0.0: Auto-generated.</p>
 */
public class CartServices {

    /**
     * Assign a ShoppingCartItem -> Quantity to a ship group
     */
    @Service(
        name = "assignItemShipGroup",
        engine = "java",
        location = "org.ofbiz.order.shoppingcart.ShoppingCartServices",
        invoke = "assignItemShipGroup",
        description = "Assign a ShoppingCartItem -> Quantity to a ship group",
        attributes = {
            @Attribute(name = "shoppingCart", type = "org.ofbiz.order.shoppingcart.ShoppingCart", mode = "IN"),
            @Attribute(name = "fromGroupIndex", type = "Integer", mode = "IN"),
            @Attribute(name = "toGroupIndex", type = "Integer", mode = "IN"),
            @Attribute(name = "itemIndex", type = "Integer", mode = "IN"),
            @Attribute(name = "quantity", type = "BigDecimal", mode = "IN"),
            @Attribute(name = "clearEmptyGroups", type = "Boolean", mode = "IN", optional = "true")
        }
    )
    public interface AssignItemShipGroup {}

    /**
     * Sets The ShoppingCart Shipping Options
     */
    @Service(
        name = "setCartShippingOptions",
        engine = "java",
        location = "org.ofbiz.order.shoppingcart.ShoppingCartServices",
        invoke = "setShippingOptions",
        description = "Sets The ShoppingCart Shipping Options",
        attributes = {
            @Attribute(name = "shoppingCart", type = "org.ofbiz.order.shoppingcart.ShoppingCart", mode = "IN"),
            @Attribute(name = "groupIndex", type = "Integer", mode = "IN"),
            @Attribute(name = "shippingContactMechId", type = "String", mode = "IN"),
            @Attribute(name = "shipmentMethodString", type = "String", mode = "IN"),
            @Attribute(name = "shippingInstructions", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "maySplit", type = "Boolean", mode = "IN"),
            @Attribute(name = "isGift", type = "Boolean", mode = "IN"),
            @Attribute(name = "giftMessage", type = "String", mode = "IN", optional = "true")
        }
    )
    public interface SetCartShippingOptions {}

    /**
     * Sets The ShoppingCart Shipping Options
     */
    @Service(
        name = "setCartShippingAddress",
        engine = "java",
        location = "org.ofbiz.order.shoppingcart.ShoppingCartServices",
        invoke = "setShippingOptions",
        description = "Sets The ShoppingCart Shipping Options",
        attributes = {
            @Attribute(name = "shoppingCart", type = "org.ofbiz.order.shoppingcart.ShoppingCart", mode = "IN"),
            @Attribute(name = "groupIndex", type = "Integer", mode = "IN"),
            @Attribute(name = "shippingContactMechId", type = "String", mode = "IN")
        }
    )
    public interface SetCartShippingAddress {}

    /**
     * Sets the ShoppingCart Payment Options
     */
    @Service(
        name = "setCartPaymentOptions",
        engine = "java",
        location = "org.ofbiz.order.shoppingcart.ShoppingCartServices",
        invoke = "setPaymentOptions",
        description = "Sets the ShoppingCart Payment Options",
        attributes = {
            @Attribute(name = "shoppingCart", type = "org.ofbiz.order.shoppingcart.ShoppingCart", mode = "IN"),
            @Attribute(name = "paymentInfoId", type = "String", mode = "IN"),
            @Attribute(name = "amount", type = "BigDecimal", mode = "IN", optional = "true"),
            @Attribute(name = "refNum", type = "String", mode = "IN", optional = "true")
        }
    )
    public interface SetCartPaymentOptions {}

    /**
     * Sets the ShoppingCart Other Options (besided payment and shipping)
     */
    @Service(
        name = "setCartOtherOptions",
        engine = "java",
        location = "org.ofbiz.order.shoppingcart.ShoppingCartServices",
        invoke = "setOtherOptions",
        description = "Sets the ShoppingCart Other Options (besided payment and shipping)",
        attributes = {
            @Attribute(name = "shoppingCart", type = "org.ofbiz.order.shoppingcart.ShoppingCart", mode = "IN"),
            @Attribute(name = "orderAdditionalEmails", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "correspondingPoId", type = "String", mode = "IN", optional = "true")
        }
    )
    public interface SetCartOtherOptions {}

    /**
     * Create a ShoppingCart Object based on an existing order
     */
    @Service(
        name = "loadCartFromOrder",
        engine = "java",
        location = "org.ofbiz.order.shoppingcart.ShoppingCartServices",
        invoke = "loadCartFromOrder",
        description = "Create a ShoppingCart Object based on an existing order",
        auth = "true",
        attributes = {
            @Attribute(name = "orderId", type = "String", mode = "IN"),
            @Attribute(name = "createAsNewOrder", type = "String", mode = "IN", defaultValue = "N"),
            @Attribute(name = "skipInventoryChecks", type = "Boolean", mode = "IN", optional = "true"),
            @Attribute(name = "skipProductChecks", type = "Boolean", mode = "IN", optional = "true"),
            @Attribute(name = "includePromoItems", type = "Boolean", mode = "IN", optional = "true"),
            @Attribute(name = "shoppingCart", type = "org.ofbiz.order.shoppingcart.ShoppingCart", mode = "OUT")
        }
    )
    public interface LoadCartFromOrder {}

    /**
     * Create a ShoppingCart Object based on an existing quote. If applyQuoteAdjustments is set to false then standard cart adjustments are generated.
     */
    @Service(
        name = "loadCartFromQuote",
        engine = "java",
        location = "org.ofbiz.order.shoppingcart.ShoppingCartServices",
        invoke = "loadCartFromQuote",
        description = "Create a ShoppingCart Object based on an existing quote. If applyQuoteAdjustments is set to false then standard cart adjustments are generated.",
        auth = "true",
        attributes = {
            @Attribute(name = "quoteId", type = "String", mode = "IN"),
            @Attribute(name = "applyQuoteAdjustments", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "shoppingCart", type = "org.ofbiz.order.shoppingcart.ShoppingCart", mode = "OUT")
        }
    )
    public interface LoadCartFromQuote {}

    /**
     * Create a ShoppingCart Object based on an existing shopping list.
     */
    @Service(
        name = "loadCartFromShoppingList",
        engine = "java",
        location = "org.ofbiz.order.shoppingcart.ShoppingCartServices",
        invoke = "loadCartFromShoppingList",
        description = "Create a ShoppingCart Object based on an existing shopping list.",
        auth = "true",
        attributes = {
            @Attribute(name = "shoppingListId", type = "String", mode = "IN"),
            @Attribute(name = "applyStorePromotions", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "orderPartyId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "shoppingCart", type = "org.ofbiz.order.shoppingcart.ShoppingCart", mode = "OUT")
        }
    )
    public interface LoadCartFromShoppingList {}

    /**
     * Get the ShoppingCart data
     */
    @Service(
        name = "getShoppingCartData",
        engine = "java",
        location = "org.ofbiz.order.shoppingcart.ShoppingCartServices",
        invoke = "getShoppingCartData",
        description = "Get the ShoppingCart data",
        attributes = {
            @Attribute(name = "shoppingCart", type = "org.ofbiz.order.shoppingcart.ShoppingCart", mode = "IN"),
            @Attribute(name = "totalQuantity", type = "BigDecimal", mode = "OUT", optional = "true"),
            @Attribute(name = "currencyIsoCode", type = "String", mode = "OUT", optional = "true"),
            @Attribute(name = "subTotal", type = "BigDecimal", mode = "OUT", optional = "true"),
            @Attribute(name = "subTotalCurrencyFormatted", type = "String", mode = "OUT", optional = "true"),
            @Attribute(name = "totalShipping", type = "BigDecimal", mode = "OUT", optional = "true"),
            @Attribute(name = "totalShippingCurrencyFormatted", type = "String", mode = "OUT", optional = "true"),
            @Attribute(name = "totalSalesTax", type = "BigDecimal", mode = "OUT", optional = "true"),
            @Attribute(name = "totalSalesTaxCurrencyFormatted", type = "String", mode = "OUT", optional = "true"),
            @Attribute(name = "displayGrandTotal", type = "BigDecimal", mode = "OUT", optional = "true"),
            @Attribute(name = "displayGrandTotalCurrencyFormatted", type = "String", mode = "OUT", optional = "true"),
            @Attribute(name = "cartItemData", type = "Map", mode = "OUT"),
            @Attribute(name = "displayOrderAdjustmentsTotalCurrencyFormatted", type = "String", mode = "OUT", optional = "true")
        }
    )
    public interface GetShoppingCartData {}

    /**
     * Get the ShoppingCart info from the productId
     */
    @Service(
        name = "getShoppingCartItemIndex",
        engine = "java",
        location = "org.ofbiz.order.shoppingcart.ShoppingCartServices",
        invoke = "getShoppingCartItemIndex",
        description = "Get the ShoppingCart info from the productId",
        attributes = {
            @Attribute(name = "shoppingCart", type = "org.ofbiz.order.shoppingcart.ShoppingCart", mode = "IN"),
            @Attribute(name = "productId", type = "String", mode = "IN"),
            @Attribute(name = "itemIndex", type = "String", mode = "OUT", optional = "true")
        }
    )
    public interface GetShoppingCartItemIndex {}

    /**
     * Reset the ship Groups in the cart and put the items in default group
     */
    @Service(
        name = "resetShipGroupItems",
        engine = "java",
        location = "org.ofbiz.order.shoppingcart.ShoppingCartServices",
        invoke = "resetShipGroupItems",
        description = "Reset the ship Groups in the cart and put the items in default group",
        attributes = {
            @Attribute(name = "shoppingCart", type = "org.ofbiz.order.shoppingcart.ShoppingCart", mode = "IN")
        }
    )
    public interface ResetShipGroupItems {}

    /**
     * Split the default shipgroup to individual shipgroups that are unique to a vendor
     */
    @Service(
        name = "prepareVendorShipGroups",
        engine = "java",
        location = "org.ofbiz.order.shoppingcart.ShoppingCartServices",
        invoke = "prepareVendorShipGroups",
        description = "Split the default shipgroup to individual shipgroups that are unique to a vendor",
        attributes = {
            @Attribute(name = "shoppingCart", type = "org.ofbiz.order.shoppingcart.ShoppingCart", mode = "IN")
        }
    )
    public interface PrepareVendorShipGroups {}

    /**
     * Create CartAbandonedLine record
     */
    @Service(
        name = "createCartAbandonedLine",
        engine = "entity-auto",
        invoke = "create",
        description = "Create CartAbandonedLine record",
        defaultEntityName = "CartAbandonedLine",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface CreateCartAbandonedLine {}

    /**
     * Update CartAbandonedLine record
     */
    @Service(
        name = "updateCartAbandonedLine",
        engine = "entity-auto",
        invoke = "update",
        description = "Update CartAbandonedLine record",
        defaultEntityName = "CartAbandonedLine",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface UpdateCartAbandonedLine {}

    /**
     * Delete CartAbandonedLine record
     */
    @Service(
        name = "deleteCartAbandonedLine",
        engine = "entity-auto",
        invoke = "delete",
        description = "Delete CartAbandonedLine record",
        defaultEntityName = "CartAbandonedLine",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        }
    )
    public interface DeleteCartAbandonedLine {}

    /**
     * Finds abandoned carts
     */
    @Service(
        name = "findAbandonedCarts",
        engine = "java",
        location = "org.ofbiz.order.shoppingcart.ShoppingCartServices",
        invoke = "findAbandonedCarts",
        description = "Finds abandoned carts",
        auth = "true",
        attributes = {
            @Attribute(name = "productStoreId", type = "String", mode = "IN"),
            @Attribute(name = "fromDate", type = "Timestamp", mode = "IN", optional = "true"),
            @Attribute(name = "daysOffset", type = "Long", mode = "IN", optional = "true"),
            @Attribute(name = "abandonedCarts", type = "java.util.List", mode = "OUT")
        }
    )
    public interface FindAbandonedCarts {}

    /**
     * Send abandoned cart email reminder
     */
    @Service(
        name = "sendAbandonedCartEmailReminder",
        engine = "java",
        location = "org.ofbiz.order.shoppingcart.ShoppingCartServices",
        invoke = "sendAbandonedCartEmailReminder",
        description = "Send abandoned cart email reminder",
        auth = "true",
        attributes = {
            @Attribute(name = "productStoreId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "abandonedCarts", type = "java.util.List", mode = "IN")
        }
    )
    public interface SendAbandonedCartEmailReminder {}

    /**
     * Create a ShoppingCart Object based on an existing abandoned cart.
     */
    @Service(
        name = "loadCartFromAbandonedCart",
        engine = "java",
        location = "org.ofbiz.order.shoppingcart.ShoppingCartServices",
        invoke = "loadCartFromAbandonedCart",
        description = "Create a ShoppingCart Object based on an existing abandoned cart.",
        auth = "true",
        attributes = {
            @Attribute(name = "visitId", type = "String", mode = "IN"),
            @Attribute(name = "abandonedCart", type = "org.ofbiz.entity.GenericValue", mode = "IN", optional = "true"),
            @Attribute(name = "abandonedCartStatus", type = "org.ofbiz.entity.GenericValue", mode = "IN", optional = "true"),
            @Attribute(name = "abandonedCartLines", type = "java.util.List", mode = "IN", optional = "true"),
            @Attribute(name = "isUserInSession", type = "Boolean", mode = "IN", optional = "true", defaultValue = "false"),
            @Attribute(name = "applyStorePromotions", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "cartPartyId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "shoppingCart", type = "org.ofbiz.order.shoppingcart.ShoppingCart", mode = "OUT")
        }
    )
    public interface LoadCartFromAbandonedCart {}

    @Service(
        name = "findAbandonedCartsAndSendReminderEmails",
        engine = "group",
        transactionTimeout = "36000",
        invokes = {@GroupInvoke(name = "findAbandonedCarts", resultToContext = "true"), @GroupInvoke(name = "sendAbandonedCartEmailReminder", resultToContext = "false")}
    )
    public interface FindAbandonedCartsAndSendReminderEmails {}

}
