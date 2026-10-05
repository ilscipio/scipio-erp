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
public class OrderServices {

    /**
     * Create an OrderAdjustmentAttribute record
     */
    @Service(
        name = "createOrderAdjustmentAttribute",
        engine = "entity-auto",
        invoke = "create",
        description = "Create an OrderAdjustmentAttribute record",
        defaultEntityName = "OrderAdjustmentAttribute",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface CreateOrderAdjustmentAttribute {}

    /**
     * Update an OrderAdjustmentAttribute record
     */
    @Service(
        name = "updateOrderAdjustmentAttribute",
        engine = "entity-auto",
        invoke = "update",
        description = "Update an OrderAdjustmentAttribute record",
        defaultEntityName = "OrderAdjustmentAttribute",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface UpdateOrderAdjustmentAttribute {}

    /**
     * Delete an OrderAdjustmentAttribute record
     */
    @Service(
        name = "deleteOrderAdjustmentAttribute",
        engine = "entity-auto",
        invoke = "delete",
        description = "Delete an OrderAdjustmentAttribute record",
        defaultEntityName = "OrderAdjustmentAttribute",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        }
    )
    public interface DeleteOrderAdjustmentAttribute {}

    /**
     * Create an OrderAdjustmentType record
     */
    @Service(
        name = "createOrderAdjustmentType",
        engine = "entity-auto",
        invoke = "create",
        description = "Create an OrderAdjustmentType record",
        defaultEntityName = "OrderAdjustmentType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "INOUT", include = "pk", optional = "true"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface CreateOrderAdjustmentType {}

    /**
     * Update an OrderAdjustmentType record
     */
    @Service(
        name = "updateOrderAdjustmentType",
        engine = "entity-auto",
        invoke = "update",
        description = "Update an OrderAdjustmentType record",
        defaultEntityName = "OrderAdjustmentType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface UpdateOrderAdjustmentType {}

    /**
     * Delete an OrderAdjustmentType record
     */
    @Service(
        name = "deleteOrderAdjustmentType",
        engine = "entity-auto",
        invoke = "delete",
        description = "Delete an OrderAdjustmentType record",
        defaultEntityName = "OrderAdjustmentType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        }
    )
    public interface DeleteOrderAdjustmentType {}

    /**
     * Create an OrderAttribute record
     */
    @Service(
        name = "createOrderAttribute",
        engine = "entity-auto",
        invoke = "create",
        description = "Create an OrderAttribute record",
        defaultEntityName = "OrderAttribute",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface CreateOrderAttribute {}

    /**
     * Update an OrderAttribute record
     */
    @Service(
        name = "updateOrderAttribute",
        engine = "entity-auto",
        invoke = "update",
        description = "Update an OrderAttribute record",
        defaultEntityName = "OrderAttribute",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface UpdateOrderAttribute {}

    /**
     * Delete an OrderAttribute record
     */
    @Service(
        name = "deleteOrderAttribute",
        engine = "entity-auto",
        invoke = "delete",
        description = "Delete an OrderAttribute record",
        defaultEntityName = "OrderAttribute",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        }
    )
    public interface DeleteOrderAttribute {}

    /**
     * Create an OrderBlacklist record
     */
    @Service(
        name = "createOrderBlacklist",
        engine = "entity-auto",
        invoke = "create",
        description = "Create an OrderBlacklist record",
        defaultEntityName = "OrderBlacklist",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface CreateOrderBlacklist {}

    /**
     * Delete an OrderBlacklist record
     */
    @Service(
        name = "deleteOrderBlacklist",
        engine = "entity-auto",
        invoke = "delete",
        description = "Delete an OrderBlacklist record",
        defaultEntityName = "OrderBlacklist",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        }
    )
    public interface DeleteOrderBlacklist {}

    /**
     * Create an OrderBlacklistType record
     */
    @Service(
        name = "createOrderBlacklistType",
        engine = "entity-auto",
        invoke = "create",
        description = "Create an OrderBlacklistType record",
        defaultEntityName = "OrderBlacklistType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "INOUT", include = "pk", optional = "true"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface CreateOrderBlacklistType {}

    /**
     * Update an OrderBlacklistType record
     */
    @Service(
        name = "updateOrderBlacklistType",
        engine = "entity-auto",
        invoke = "update",
        description = "Update an OrderBlacklistType record",
        defaultEntityName = "OrderBlacklistType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface UpdateOrderBlacklistType {}

    /**
     * Delete an OrderBlacklistType record
     */
    @Service(
        name = "deleteOrderBlacklistType",
        engine = "entity-auto",
        invoke = "delete",
        description = "Delete an OrderBlacklistType record",
        defaultEntityName = "OrderBlacklistType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        }
    )
    public interface DeleteOrderBlacklistType {}

    /**
     * Create an OrderItemAssoc record
     */
    @Service(
        name = "createOrderItemAssoc",
        engine = "entity-auto",
        invoke = "create",
        description = "Create an OrderItemAssoc record",
        defaultEntityName = "OrderItemAssoc",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface CreateOrderItemAssoc {}

    /**
     * Update an OrderItemAssoc record
     */
    @Service(
        name = "updateOrderItemAssoc",
        engine = "entity-auto",
        invoke = "update",
        description = "Update an OrderItemAssoc record",
        defaultEntityName = "OrderItemAssoc",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface UpdateOrderItemAssoc {}

    /**
     * Delete an OrderItemAssoc record
     */
    @Service(
        name = "deleteOrderItemAssoc",
        engine = "entity-auto",
        invoke = "delete",
        description = "Delete an OrderItemAssoc record",
        defaultEntityName = "OrderItemAssoc",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        }
    )
    public interface DeleteOrderItemAssoc {}

    /**
     * Create an OrderItemAssocType record
     */
    @Service(
        name = "createOrderItemAssocType",
        engine = "entity-auto",
        invoke = "create",
        description = "Create an OrderItemAssocType record",
        defaultEntityName = "OrderItemAssocType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "INOUT", include = "pk", optional = "true"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface CreateOrderItemAssocType {}

    /**
     * Update an OrderItemAssocType record
     */
    @Service(
        name = "updateOrderItemAssocType",
        engine = "entity-auto",
        invoke = "update",
        description = "Update an OrderItemAssocType record",
        defaultEntityName = "OrderItemAssocType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface UpdateOrderItemAssocType {}

    /**
     * Delete an OrderItemAssocType record
     */
    @Service(
        name = "deleteOrderItemAssocType",
        engine = "entity-auto",
        invoke = "delete",
        description = "Delete an OrderItemAssocType record",
        defaultEntityName = "OrderItemAssocType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        }
    )
    public interface DeleteOrderItemAssocType {}

    /**
     * Create an OrderItemContactMech record
     */
    @Service(
        name = "createOrderItemContactMech",
        engine = "entity-auto",
        invoke = "create",
        description = "Create an OrderItemContactMech record",
        defaultEntityName = "OrderItemContactMech",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface CreateOrderItemContactMech {}

    /**
     * Update an OrderItemContactMech record
     */
    @Service(
        name = "updateOrderItemContactMech",
        engine = "entity-auto",
        invoke = "update",
        description = "Update an OrderItemContactMech record",
        defaultEntityName = "OrderItemContactMech",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface UpdateOrderItemContactMech {}

    /**
     * Delete an OrderItemContactMech record
     */
    @Service(
        name = "deleteOrderItemContactMech",
        engine = "entity-auto",
        invoke = "delete",
        description = "Delete an OrderItemContactMech record",
        defaultEntityName = "OrderItemContactMech",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        }
    )
    public interface DeleteOrderItemContactMech {}

    /**
     * Create an OrderItemGroup record
     */
    @Service(
        name = "createOrderItemGroup",
        engine = "entity-auto",
        invoke = "create",
        description = "Create an OrderItemGroup record",
        defaultEntityName = "OrderItemGroup",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface CreateOrderItemGroup {}

    /**
     * Update an OrderItemGroup record
     */
    @Service(
        name = "updateOrderItemGroup",
        engine = "entity-auto",
        invoke = "update",
        description = "Update an OrderItemGroup record",
        defaultEntityName = "OrderItemGroup",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface UpdateOrderItemGroup {}

    /**
     * Delete an OrderItemGroup record
     */
    @Service(
        name = "deleteOrderItemGroup",
        engine = "entity-auto",
        invoke = "delete",
        description = "Delete an OrderItemGroup record",
        defaultEntityName = "OrderItemGroup",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        }
    )
    public interface DeleteOrderItemGroup {}

    /**
     * Create an OrderItemPriceInfo record
     */
    @Service(
        name = "createOrderItemPriceInfo",
        engine = "entity-auto",
        invoke = "create",
        description = "Create an OrderItemPriceInfo record",
        defaultEntityName = "OrderItemPriceInfo",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "INOUT", include = "pk", optional = "true"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface CreateOrderItemPriceInfo {}

    /**
     * Update an OrderItemPriceInfo record
     */
    @Service(
        name = "updateOrderItemPriceInfo",
        engine = "entity-auto",
        invoke = "update",
        description = "Update an OrderItemPriceInfo record",
        defaultEntityName = "OrderItemPriceInfo",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface UpdateOrderItemPriceInfo {}

    /**
     * Delete an OrderItemPriceInfo record
     */
    @Service(
        name = "deleteOrderItemPriceInfo",
        engine = "entity-auto",
        invoke = "delete",
        description = "Delete an OrderItemPriceInfo record",
        defaultEntityName = "OrderItemPriceInfo",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        }
    )
    public interface DeleteOrderItemPriceInfo {}

    /**
     * Create an OrderItemPriceInfo record
     */
    @Service(
        name = "createOrderItemRole",
        engine = "entity-auto",
        invoke = "create",
        description = "Create an OrderItemPriceInfo record",
        defaultEntityName = "OrderItemRole",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface CreateOrderItemRole {}

    /**
     * Delete an OrderItemPriceInfo record
     */
    @Service(
        name = "deleteOrderItemRole",
        engine = "entity-auto",
        invoke = "delete",
        description = "Delete an OrderItemPriceInfo record",
        defaultEntityName = "OrderItemRole",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        }
    )
    public interface DeleteOrderItemRole {}

    /**
     * Create an OrderItemShipGrpInvRes record
     */
    @Service(
        name = "createOrderItemShipGrpInvRes",
        engine = "entity-auto",
        invoke = "create",
        description = "Create an OrderItemShipGrpInvRes record",
        defaultEntityName = "OrderItemShipGrpInvRes",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface CreateOrderItemShipGrpInvRes {}

    /**
     * Update an OrderItemShipGrpInvRes record
     */
    @Service(
        name = "updateOrderItemShipGrpInvRes",
        engine = "entity-auto",
        invoke = "update",
        description = "Update an OrderItemShipGrpInvRes record",
        defaultEntityName = "OrderItemShipGrpInvRes",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface UpdateOrderItemShipGrpInvRes {}

    /**
     * Delete an OrderItemShipGrpInvRes record
     */
    @Service(
        name = "deleteOrderItemShipGrpInvRes",
        engine = "entity-auto",
        invoke = "delete",
        description = "Delete an OrderItemShipGrpInvRes record",
        defaultEntityName = "OrderItemShipGrpInvRes",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        }
    )
    public interface DeleteOrderItemShipGrpInvRes {}

    /**
     * Create an OrderItemType record
     */
    @Service(
        name = "createOrderItemType",
        engine = "entity-auto",
        invoke = "create",
        description = "Create an OrderItemType record",
        defaultEntityName = "OrderItemType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "INOUT", include = "pk", optional = "true"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface CreateOrderItemType {}

    /**
     * Update an OrderItemType record
     */
    @Service(
        name = "updateOrderItemType",
        engine = "entity-auto",
        invoke = "update",
        description = "Update an OrderItemType record",
        defaultEntityName = "OrderItemType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface UpdateOrderItemType {}

    /**
     * Delete an OrderItemType record
     */
    @Service(
        name = "deleteOrderItemType",
        engine = "entity-auto",
        invoke = "delete",
        description = "Delete an OrderItemType record",
        defaultEntityName = "OrderItemType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        }
    )
    public interface DeleteOrderItemType {}

    /**
     * Create an OrderItemTypeAttr record
     */
    @Service(
        name = "createOrderItemTypeAttr",
        engine = "entity-auto",
        invoke = "create",
        description = "Create an OrderItemTypeAttr record",
        defaultEntityName = "OrderItemTypeAttr",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface CreateOrderItemTypeAttr {}

    /**
     * Update an OrderItemTypeAttr record
     */
    @Service(
        name = "updateOrderItemTypeAttr",
        engine = "entity-auto",
        invoke = "update",
        description = "Update an OrderItemTypeAttr record",
        defaultEntityName = "OrderItemTypeAttr",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface UpdateOrderItemTypeAttr {}

    /**
     * Delete an OrderItemTypeAttr record
     */
    @Service(
        name = "deleteOrderItemTypeAttr",
        engine = "entity-auto",
        invoke = "delete",
        description = "Delete an OrderItemTypeAttr record",
        defaultEntityName = "OrderItemTypeAttr",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        }
    )
    public interface DeleteOrderItemTypeAttr {}

    /**
     * Update an OrderNotification record
     */
    @Service(
        name = "updateOrderNotification",
        engine = "entity-auto",
        invoke = "update",
        description = "Update an OrderNotification record",
        defaultEntityName = "OrderNotification",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface UpdateOrderNotification {}

    /**
     * Delete an OrderNotification record
     */
    @Service(
        name = "deleteOrderNotification",
        engine = "entity-auto",
        invoke = "delete",
        description = "Delete an OrderNotification record",
        defaultEntityName = "OrderNotification",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        }
    )
    public interface DeleteOrderNotification {}

    /**
     * Create an OrderProductPromoCode record
     */
    @Service(
        name = "createOrderProductPromoCode",
        engine = "entity-auto",
        invoke = "create",
        description = "Create an OrderProductPromoCode record",
        defaultEntityName = "OrderProductPromoCode",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface CreateOrderProductPromoCode {}

    /**
     * Delete an OrderProductPromoCode record
     */
    @Service(
        name = "deleteOrderProductPromoCode",
        engine = "entity-auto",
        invoke = "delete",
        description = "Delete an OrderProductPromoCode record",
        defaultEntityName = "OrderProductPromoCode",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        }
    )
    public interface DeleteOrderProductPromoCode {}

    /**
     * Update an OrderRequirementCommitment record
     */
    @Service(
        name = "updateOrderRequirementCommitment",
        engine = "entity-auto",
        invoke = "update",
        description = "Update an OrderRequirementCommitment record",
        defaultEntityName = "OrderRequirementCommitment",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface UpdateOrderRequirementCommitment {}

    /**
     * Delete an OrderRequirementCommitment record
     */
    @Service(
        name = "deleteOrderRequirementCommitment",
        engine = "entity-auto",
        invoke = "delete",
        description = "Delete an OrderRequirementCommitment record",
        defaultEntityName = "OrderRequirementCommitment",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        }
    )
    public interface DeleteOrderRequirementCommitment {}

    /**
     * Create an OrderSummaryEntry record
     */
    @Service(
        name = "createOrderSummaryEntry",
        engine = "entity-auto",
        invoke = "create",
        description = "Create an OrderSummaryEntry record",
        defaultEntityName = "OrderSummaryEntry",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface CreateOrderSummaryEntry {}

    /**
     * Update an OrderSummaryEntry record
     */
    @Service(
        name = "updateOrderSummaryEntry",
        engine = "entity-auto",
        invoke = "update",
        description = "Update an OrderSummaryEntry record",
        defaultEntityName = "OrderSummaryEntry",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface UpdateOrderSummaryEntry {}

    /**
     * Delete an OrderSummaryEntry record
     */
    @Service(
        name = "deleteOrderSummaryEntry",
        engine = "entity-auto",
        invoke = "delete",
        description = "Delete an OrderSummaryEntry record",
        defaultEntityName = "OrderSummaryEntry",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        }
    )
    public interface DeleteOrderSummaryEntry {}

    /**
     * Create an OrderTermAttribute record
     */
    @Service(
        name = "createOrderTermAttribute",
        engine = "entity-auto",
        invoke = "create",
        description = "Create an OrderTermAttribute record",
        defaultEntityName = "OrderTermAttribute",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface CreateOrderTermAttribute {}

    /**
     * Update an OrderTermAttribute record
     */
    @Service(
        name = "updateOrderTermAttribute",
        engine = "entity-auto",
        invoke = "update",
        description = "Update an OrderTermAttribute record",
        defaultEntityName = "OrderTermAttribute",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface UpdateOrderTermAttribute {}

    /**
     * Delete an OrderTermAttribute record
     */
    @Service(
        name = "deleteOrderTermAttribute",
        engine = "entity-auto",
        invoke = "delete",
        description = "Delete an OrderTermAttribute record",
        defaultEntityName = "OrderTermAttribute",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        }
    )
    public interface DeleteOrderTermAttribute {}

    /**
     * Create an OrderType record
     */
    @Service(
        name = "createOrderType",
        engine = "entity-auto",
        invoke = "create",
        description = "Create an OrderType record",
        defaultEntityName = "OrderType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "INOUT", include = "pk", optional = "true"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface CreateOrderType {}

    /**
     * Update an OrderType record
     */
    @Service(
        name = "updateOrderType",
        engine = "entity-auto",
        invoke = "update",
        description = "Update an OrderType record",
        defaultEntityName = "OrderType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface UpdateOrderType {}

    /**
     * Delete an OrderType record
     */
    @Service(
        name = "deleteOrderType",
        engine = "entity-auto",
        invoke = "delete",
        description = "Delete an OrderType record",
        defaultEntityName = "OrderType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        }
    )
    public interface DeleteOrderType {}

    /**
     * Create an OrderTypeAttr record
     */
    @Service(
        name = "createOrderTypeAttr",
        engine = "entity-auto",
        invoke = "create",
        description = "Create an OrderTypeAttr record",
        defaultEntityName = "OrderTypeAttr",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface CreateOrderTypeAttr {}

    /**
     * Update an OrderTypeAttr record
     */
    @Service(
        name = "updateOrderTypeAttr",
        engine = "entity-auto",
        invoke = "update",
        description = "Update an OrderTypeAttr record",
        defaultEntityName = "OrderTypeAttr",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface UpdateOrderTypeAttr {}

    /**
     * Delete an OrderTypeAttr record
     */
    @Service(
        name = "deleteOrderTypeAttr",
        engine = "entity-auto",
        invoke = "delete",
        description = "Delete an OrderTypeAttr record",
        defaultEntityName = "OrderTypeAttr",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        }
    )
    public interface DeleteOrderTypeAttr {}

    /**
     * Create a OrderContentType
     */
    @Service(
        name = "createOrderContentType",
        engine = "entity-auto",
        invoke = "create",
        description = "Create a OrderContentType",
        defaultEntityName = "OrderContentType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true"),
            @EntityAttributes(mode = "INOUT", include = "pk", optional = "true")
        }
    )
    public interface CreateOrderContentType {}

    /**
     * Update a OrderContentType
     */
    @Service(
        name = "updateOrderContentType",
        engine = "entity-auto",
        invoke = "update",
        description = "Update a OrderContentType",
        defaultEntityName = "OrderContentType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface UpdateOrderContentType {}

    /**
     * Delete a OrderContentType
     */
    @Service(
        name = "deleteOrderContentType",
        engine = "entity-auto",
        invoke = "delete",
        description = "Delete a OrderContentType",
        defaultEntityName = "OrderContentType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        }
    )
    public interface DeleteOrderContentType {}

    /**
     * Sends order data to listening websockets on channel. Requires interval to be set (HOURLY, DAILY, MONTHLY, YEARHLY) prevents miscalculated columns)
     */
    @Service(
        name = "sendOrderLiveData",
        engine = "java",
        location = "com.ilscipio.scipio.order.web.OrderWebServices",
        invoke = "sendOrderLiveData",
        description = "Sends order data to listening websockets on channel. Requires interval to be set (HOURLY, DAILY, MONTHLY, YEARHLY) prevents miscalculated columns)",
        requireNewTransaction = "true",
        maxRetry = "3",
        attributes = {
            @Attribute(name = "orderId", type = "String", mode = "IN"),
            @Attribute(name = "channel", type = "String", mode = "IN"),
            @Attribute(name = "interval", type = "String", mode = "IN"),
            @Attribute(name = "productStoreId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "webSiteId", type = "String", mode = "IN", optional = "true")
        }
    )
    public interface SendOrderLiveData {}

    /**
     * Sends order data to listening websockets on channel.
     */
    @Service(
        name = "wsSendOrder",
        engine = "java",
        location = "com.ilscipio.scipio.order.web.OrderWebServices",
        invoke = "sendOrder",
        description = "Sends order data to listening websockets on channel.",
        requireNewTransaction = "true",
        maxRetry = "1",
        attributes = {
            @Attribute(name = "orderId", type = "String", mode = "IN"),
            @Attribute(name = "channel", type = "String", mode = "IN")
        }
    )
    public interface WsSendOrder {}

    /**
     * Sends order item data to listening websockets on channel.
     */
    @Service(
        name = "wsSendOrderItem",
        engine = "java",
        location = "com.ilscipio.scipio.order.web.OrderWebServices",
        invoke = "sendOrderItem",
        description = "Sends order item data to listening websockets on channel.",
        requireNewTransaction = "true",
        maxRetry = "1",
        attributes = {
            @Attribute(name = "orderId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "orderItemSeqId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "channel", type = "String", mode = "IN")
        }
    )
    public interface WsSendOrderItem {}

}
