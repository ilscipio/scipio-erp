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

/**
 * Auto-generated annotation-based service definitions.
 *
 * <p>Generated from services.xml by convertXmlToAnnotation Gradle task.</p>
 *
 * <p>SCIPIO: 4.0.0: Auto-generated.</p>
 */
public class ShoppinglistServices {

    /**
     * Shopping List Interface
     */
    @Service(
        name = "shoppingListInterface",
        engine = "interface",
        description = "Shopping List Interface",
        entityAttributes = {
            @EntityAttributes(entityName = "ShoppingList", mode = "IN", include = "nonpk", optional = "true")
        },
        attributes = {
            @Attribute(name = "shippingMethodString", type = "String", mode = "IN", optional = "true")
        }
    )
    public interface ShoppingListInterface {}

    /**
     * Create a shopping list entity
     */
    @Service(
        name = "createShoppingList",
        engine = "simple",
        location = "component://order/script/org/ofbiz/order/shoppinglist/ShoppingListServices.xml",
        invoke = "createShoppingList",
        description = "Create a shopping list entity",
        implemented = {@Implements(service = "createShoppingListRecurrence"), @Implements(service = "shoppingListInterface")},
        attributes = {
            @Attribute(name = "shoppingListId", type = "String", mode = "OUT"),
            @Attribute(name = "shoppingListAuthToken", type = "String", mode = "OUT", optional = "true")
        }
    )
    public interface CreateShoppingList {}

    /**
     * Update a shopping list entity
     */
    @Service(
        name = "updateShoppingList",
        engine = "simple",
        location = "component://order/script/org/ofbiz/order/shoppinglist/ShoppingListServices.xml",
        invoke = "updateShoppingList",
        description = "Update a shopping list entity",
        implemented = {@Implements(service = "createShoppingListRecurrence"), @Implements(service = "shoppingListInterface")},
        attributes = {
            @Attribute(name = "shoppingListId", type = "String", mode = "IN"),
            @Attribute(name = "shoppingListAuthToken", type = "String", mode = "IN", optional = "true")
        }
    )
    public interface UpdateShoppingList {}

    /**
     * Remove a shopping list entity
     */
    @Service(
        name = "removeShoppingList",
        engine = "simple",
        location = "component://order/script/org/ofbiz/order/shoppinglist/ShoppingListServices.xml",
        invoke = "removeShoppingList",
        description = "Remove a shopping list entity",
        attributes = {
            @Attribute(name = "shoppingListId", type = "String", mode = "IN"),
            @Attribute(name = "shoppingListAuthToken", type = "String", mode = "IN", optional = "true")
        }
    )
    public interface RemoveShoppingList {}

    /**
     * Remove a shopping list entity
     */
    @Service(
        name = "calculateShoppingListDeepTotalPrice",
        engine = "simple",
        location = "component://order/script/org/ofbiz/order/shoppinglist/ShoppingListServices.xml",
        invoke = "calculateShoppingListDeepTotalPrice",
        description = "Remove a shopping list entity",
        attributes = {
            @Attribute(name = "shoppingListId", type = "String", mode = "IN"),
            @Attribute(name = "prodCatalogId", type = "String", mode = "IN"),
            @Attribute(name = "webSiteId", type = "String", mode = "IN"),
            @Attribute(name = "partyId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "productStoreId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "productStoreGroupId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "quantity", type = "BigDecimal", mode = "IN", optional = "true"),
            @Attribute(name = "currencyUomId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "autoUserLogin", type = "org.ofbiz.entity.GenericValue", mode = "IN", optional = "true"),
            @Attribute(name = "shoppingListAuthToken", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "totalPrice", type = "BigDecimal", mode = "OUT")
        }
    )
    public interface CalculateShoppingListDeepTotalPrice {}

    /**
     * A service designed to be automatically run by job scheduler to create orders from auto-order shopping lists.             This is done by looking for all auto-order shopping lists which are active             comparing the lastOrderedDate and the defined recurrenceInfo with the time when the service is run.
     */
    @Service(
        name = "runShoppingListAutoReorder",
        engine = "java",
        location = "org.ofbiz.order.shoppinglist.ShoppingListServices",
        invoke = "createListReorders",
        description = "A service designed to be automatically run by job scheduler to create orders from auto-order shopping lists.\n            This is done by looking for all auto-order shopping lists which are active\n            comparing the lastOrderedDate and the defined recurrenceInfo with the time when the service is run.",
        auth = "true",
        useTransaction = "false"
    )
    public interface RunShoppingListAutoReorder {}

    /**
     * Creates Recurrence Info For Auto-Reorder Lists
     */
    @Service(
        name = "createShoppingListRecurrence",
        engine = "java",
        location = "org.ofbiz.order.shoppinglist.ShoppingListServices",
        invoke = "setShoppingListRecurrence",
        description = "Creates Recurrence Info For Auto-Reorder Lists",
        auth = "true",
        attributes = {
            @Attribute(name = "startDateTime", type = "Timestamp", mode = "IN", optional = "true"),
            @Attribute(name = "endDateTime", type = "Timestamp", mode = "IN", optional = "true"),
            @Attribute(name = "frequency", type = "Integer", mode = "IN", optional = "true"),
            @Attribute(name = "intervalNumber", type = "Integer", mode = "IN", optional = "true"),
            @Attribute(name = "recurrenceInfoId", type = "String", mode = "OUT", optional = "true")
        }
    )
    public interface CreateShoppingListRecurrence {}

    /**
     * Splits the shipping method string
     */
    @Service(
        name = "splitShipmentMethodString",
        engine = "java",
        location = "org.ofbiz.order.shoppinglist.ShoppingListServices",
        invoke = "splitShipmentMethodString",
        description = "Splits the shipping method string",
        attributes = {
            @Attribute(name = "shippingMethodString", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "shipmentMethodTypeId", type = "String", mode = "OUT", optional = "true"),
            @Attribute(name = "carrierPartyId", type = "String", mode = "OUT", optional = "true")
        }
    )
    public interface SplitShipmentMethodString {}

    /**
     * Create/Update a shopping list from an order
     */
    @Service(
        name = "makeShoppingListFromOrder",
        engine = "java",
        location = "org.ofbiz.order.shoppinglist.ShoppingListServices",
        invoke = "makeListFromOrder",
        description = "Create/Update a shopping list from an order",
        auth = "true",
        implemented = {@Implements(service = "createShoppingListRecurrence")},
        attributes = {
            @Attribute(name = "shoppingListTypeId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "shoppingListId", type = "String", mode = "INOUT", optional = "true"),
            @Attribute(name = "shoppingListAuthToken", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "orderId", type = "String", mode = "IN"),
            @Attribute(name = "partyId", type = "String", mode = "IN", optional = "true")
        }
    )
    public interface MakeShoppingListFromOrder {}

    /**
     * Interface of shopping list items
     */
    @Service(
        name = "shoppingListItemInterface",
        engine = "interface",
        description = "Interface of shopping list items",
        attributes = {
            @Attribute(name = "shoppingListId", type = "String", mode = "IN"),
            @Attribute(name = "productId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "quantity", type = "BigDecimal", mode = "IN", optional = "true"),
            @Attribute(name = "modifiedPrice", type = "BigDecimal", mode = "IN", optional = "true"),
            @Attribute(name = "reservStart", type = "Timestamp", mode = "IN", optional = "true"),
            @Attribute(name = "reservLength", type = "BigDecimal", mode = "IN", optional = "true"),
            @Attribute(name = "reservPersons", type = "BigDecimal", mode = "IN", optional = "true"),
            @Attribute(name = "quantityPurchased", type = "BigDecimal", mode = "IN", optional = "true"),
            @Attribute(name = "configId", type = "String", mode = "IN", optional = "true")
        }
    )
    public interface ShoppingListItemInterface {}

    /**
     * Create a shopping list item
     */
    @Service(
        name = "createShoppingListItem",
        engine = "simple",
        location = "component://order/script/org/ofbiz/order/shoppinglist/ShoppingListServices.xml",
        invoke = "createShoppingListItem",
        description = "Create a shopping list item",
        implemented = {@Implements(service = "shoppingListItemInterface")},
        attributes = {
            @Attribute(name = "shoppingListId", type = "String", mode = "INOUT", optional = "true"),
            @Attribute(name = "shoppingListAuthToken", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "productStoreId", type = "String", mode = "INOUT", optional = "true"),
            @Attribute(name = "productId", type = "String", mode = "IN"),
            @Attribute(name = "shoppingListItemSeqId", type = "String", mode = "OUT")
        }
    )
    public interface CreateShoppingListItem {}

    /**
     * Update a shopping list item
     */
    @Service(
        name = "updateShoppingListItem",
        engine = "simple",
        location = "component://order/script/org/ofbiz/order/shoppinglist/ShoppingListServices.xml",
        invoke = "updateShoppingListItem",
        description = "Update a shopping list item",
        implemented = {@Implements(service = "shoppingListItemInterface")},
        attributes = {
            @Attribute(name = "shoppingListItemSeqId", type = "String", mode = "IN"),
            @Attribute(name = "shoppingListAuthToken", type = "String", mode = "IN", optional = "true")
        }
    )
    public interface UpdateShoppingListItem {}

    /**
     * Remove a shopping list item
     */
    @Service(
        name = "removeShoppingListItem",
        engine = "simple",
        location = "component://order/script/org/ofbiz/order/shoppinglist/ShoppingListServices.xml",
        invoke = "removeShoppingListItem",
        description = "Remove a shopping list item",
        attributes = {
            @Attribute(name = "shoppingListId", type = "String", mode = "IN"),
            @Attribute(name = "shoppingListItemSeqId", type = "String", mode = "IN"),
            @Attribute(name = "shoppingListAuthToken", type = "String", mode = "IN", optional = "true")
        }
    )
    public interface RemoveShoppingListItem {}

    /**
     * Add suggestions to a shopping list
     */
    @Service(
        name = "addSuggestionsToShoppingList",
        engine = "simple",
        location = "component://order/script/org/ofbiz/order/shoppinglist/ShoppingListServices.xml",
        invoke = "addSuggestionsToShoppingList",
        description = "Add suggestions to a shopping list",
        auth = "true",
        attributes = {
            @Attribute(name = "orderId", type = "String", mode = "IN")
        }
    )
    public interface AddSuggestionsToShoppingList {}

    /**
     * Adds a shopping list item if one with the same productId does not exist
     */
    @Service(
        name = "addDistinctShoppingListItem",
        engine = "simple",
        location = "component://order/script/org/ofbiz/order/shoppinglist/ShoppingListServices.xml",
        invoke = "addDistinctShoppingListItem",
        description = "Adds a shopping list item if one with the same productId does not exist",
        implemented = {@Implements(service = "shoppingListItemInterface")},
        attributes = {
            @Attribute(name = "shoppingListId", type = "String", mode = "IN"),
            @Attribute(name = "shoppingListAuthToken", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "productStoreId", type = "String", mode = "INOUT", optional = "true"),
            @Attribute(name = "productId", type = "String", mode = "IN"),
            @Attribute(name = "shoppingListItemSeqId", type = "String", mode = "OUT")
        }
    )
    public interface AddDistinctShoppingListItem {}

    /**
     *              Automatic delete auto save shopping list for anonymous users that are not updated in last 30 days.             Default to 30 days unless no configuration is specified.             SCIPIO: This service now also deletes expired regular anonymous wishlists by default.         
     */
    @Service(
        name = "autoDeleteAutoSaveShoppingListAlways",
        engine = "java",
        location = "org.ofbiz.order.shoppinglist.ShoppingListServices",
        invoke = "autoDeleteAutoSaveShoppingList",
        description = "\n            Automatic delete auto save shopping list for anonymous users that are not updated in last 30 days.\n            Default to 30 days unless no configuration is specified.\n            SCIPIO: This service now also deletes expired regular anonymous wishlists by default.\n        ",
        auth = "true",
        useTransaction = "false",
        attributes = {
            @Attribute(name = "deleteAutoSaveList", type = "Boolean", mode = "IN", optional = "true", defaultValue = "true"),
            @Attribute(name = "deleteAnonWishList", type = "Boolean", mode = "IN", optional = "true", defaultValue = "true"),
            @Attribute(name = "sepTransBatch", type = "Integer", mode = "IN", optional = "true", defaultValue = "100", description = "The number of shopping lists to include in a deletion transaction (0 disables separate transactions)")
        }
    )
    public interface AutoDeleteAutoSaveShoppingListAlways {}

    /**
     *              Automatic delete auto save shopping list for anonymous users that are not updated in last 30 days.             Default to 30 days unless no configuration is specified.             SCIPIO: This service now also deletes expired regular anonymous wishlists by default, and this             version uses a semaphore.         
     */
    @Service(
        name = "autoDeleteAutoSaveShoppingList",
        engine = "java",
        location = "org.ofbiz.order.shoppinglist.ShoppingListServices",
        invoke = "autoDeleteAutoSaveShoppingList",
        description = "\n            Automatic delete auto save shopping list for anonymous users that are not updated in last 30 days.\n            Default to 30 days unless no configuration is specified.\n            SCIPIO: This service now also deletes expired regular anonymous wishlists by default, and this\n            version uses a semaphore.\n        ",
        auth = "true",
        useTransaction = "false",
        semaphore = "fail",
        implemented = {@Implements(service = "autoDeleteAutoSaveShoppingListAlways")}
    )
    public interface AutoDeleteAutoSaveShoppingList {}

    /**
     * Create a ShoppingListItemSurvey Record
     */
    @Service(
        name = "createShoppingListItemSurvey",
        engine = "entity-auto",
        invoke = "create",
        description = "Create a ShoppingListItemSurvey Record",
        defaultEntityName = "ShoppingListItemSurvey",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface CreateShoppingListItemSurvey {}

    /**
     * Delete a ShoppingListItemSurvey Record
     */
    @Service(
        name = "deleteShoppingListItemSurvey",
        engine = "entity-auto",
        invoke = "delete",
        description = "Delete a ShoppingListItemSurvey Record",
        defaultEntityName = "ShoppingListItemSurvey",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        }
    )
    public interface DeleteShoppingListItemSurvey {}

    /**
     * Create a ShoppingListType
     */
    @Service(
        name = "createShoppingListType",
        engine = "entity-auto",
        invoke = "create",
        description = "Create a ShoppingListType",
        defaultEntityName = "ShoppingListType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface CreateShoppingListType {}

    /**
     * Update a ShoppingListType
     */
    @Service(
        name = "updateShoppingListType",
        engine = "entity-auto",
        invoke = "update",
        description = "Update a ShoppingListType",
        defaultEntityName = "ShoppingListType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface UpdateShoppingListType {}

    /**
     * Delete a ShoppingListType
     */
    @Service(
        name = "deleteShoppingListType",
        engine = "entity-auto",
        invoke = "delete",
        description = "Delete a ShoppingListType",
        defaultEntityName = "ShoppingListType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        }
    )
    public interface DeleteShoppingListType {}

    /**
     * Converts an anonymous/guest ShoppingList to a registered one (auth token switched with partyId) (SCIPIO)
     */
    @Service(
        name = "convertAnonShoppingListToRegistered",
        engine = "java",
        location = "org.ofbiz.order.shoppinglist.ShoppingListServices",
        invoke = "convertAnonShoppingListToRegistered",
        description = "Converts an anonymous/guest ShoppingList to a registered one (auth token switched with partyId) (SCIPIO)",
        auth = "true",
        attributes = {
            @Attribute(name = "shoppingListId", type = "String", mode = "IN"),
            @Attribute(name = "shoppingListAuthToken", type = "String", mode = "IN", optional = "true", description = "NOTE: Required if userLogin is non-admin"),
            @Attribute(name = "targetUserLogin", type = "GenericValue", mode = "IN", optional = "true")
        }
    )
    public interface ConvertAnonShoppingListToRegistered {}

}
