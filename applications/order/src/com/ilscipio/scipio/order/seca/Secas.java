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
package com.ilscipio.scipio.order.seca;

import com.ilscipio.scipio.service.def.seca.*;

/**
 * Auto-generated annotation-based service ECA definitions.
 *
 * <p>Generated from secas.xml by convertXmlToAnnotation Gradle task.</p>
 *
 * <p>SCIPIO: 4.0.0: Auto-generated.</p>
 */
public class Secas {

    /**
     * SECA for service storeOrder on event return.
     */
    @Seca(
        service = "storeOrder",
        event = "return",
        actions = {
            @SecaAction(
                service = "resetGrandTotal",
                mode = "sync"
            ),
            @SecaAction(
                service = "addSuggestionsToShoppingList",
                mode = "async",
                persist = "true"
            )
        }
    )
    public interface StoreOrderreturnSeca1 {}

    /**
     * SECA for service storeOrder on event return.
     */
    @Seca(
        service = "storeOrder",
        event = "return",
        condition = "orderTypeId == 'SALES_ORDER'",
        actions = {
            @SecaAction(
                service = "checkCreateDropShipPurchaseOrders",
                mode = "sync",
                runAsUser = "system"
            )
        }
    )
    public interface StoreOrderreturnSeca2 {}

    /**
     * SECA for service storeOrder on event return.
     */
    @Seca(
        service = "storeOrder",
        event = "return",
        actions = {
            @SecaAction(
                service = "balanceOrderItemsWithNegativeReservations",
                mode = "sync"
            )
        }
    )
    public interface StoreOrderreturnSeca3 {}

    /**
     * SECA for service storeOrder on event return.
     */
    @Seca(
        service = "storeOrder",
        event = "return",
        condition = "orderTypeId == 'PURCHASE_ORDER'",
        actions = {
            @SecaAction(
                service = "setUnitPriceAsLastPrice",
                mode = "sync"
            )
        }
    )
    public interface StoreOrderreturnSeca4 {}

    /**
     * SECA for service storeOrder on event return.
     */
    @Seca(
        service = "storeOrder",
        event = "return",
        actions = {
            @SecaAction(
                service = "setOrderReservationPriority",
                mode = "sync"
            )
        }
    )
    public interface StoreOrderreturnSeca5 {}

    /**
     * SECA for service updateOrderItems on event commit.
     */
    @Seca(
        service = "updateOrderItems",
        event = "commit",
        condition = "orderTypeId == 'PURCHASE_ORDER'",
        actions = {
            @SecaAction(
                service = "setUnitPriceAsLastPrice",
                mode = "sync"
            )
        }
    )
    public interface UpdateOrderItemscommitSeca6 {}

    /**
     * SECA for service receiveInventoryProduct on event commit.
     */
    @Seca(
        service = "receiveInventoryProduct",
        event = "commit",
        condition = "!empty(facilityId)",
        actions = {
            @SecaAction(
                service = "addProductsBackToCategory",
                mode = "sync"
            ),
            @SecaAction(
                service = "setUnitPriceAsLastPrice",
                mode = "sync"
            )
        }
    )
    public interface ReceiveInventoryProductcommitSeca7 {}

    /**
     * SECA for service receiveInventoryProduct on event commit.
     */
    @Seca(
        service = "receiveInventoryProduct",
        event = "commit",
        condition = "!empty(facilityId) && !empty(orderId)",
        actions = {
            @SecaAction(
                service = "addProductsBackToCategory",
                mode = "sync"
            )
        }
    )
    public interface ReceiveInventoryProductcommitSeca8 {}

    /**
     * SECA for service changeOrderItemStatus on event commit.
     */
    @Seca(
        service = "changeOrderItemStatus",
        event = "commit",
        condition = "statusId == 'ITEM_CANCELLED'",
        actions = {
            @SecaAction(
                service = "cancelOrderInventoryReservation",
                mode = "sync"
            ),
            @SecaAction(
                service = "cancleOrderItemGroupOrder",
                mode = "sync"
            ),
            @SecaAction(
                service = "recalcShippingTotal",
                mode = "sync"
            ),
            @SecaAction(
                service = "recalcTaxTotal",
                mode = "sync"
            ),
            @SecaAction(
                service = "resetGrandTotal",
                mode = "sync"
            ),
            @SecaAction(
                service = "checkOrderItemStatus",
                mode = "sync"
            )
        }
    )
    public interface ChangeOrderItemStatuscommitSeca9 {}

    /**
     * SECA for service changeOrderItemStatus on event commit.
     */
    @Seca(
        service = "changeOrderItemStatus",
        event = "commit",
        condition = "statusId == 'ITEM_COMPLETED'",
        actions = {
            @SecaAction(
                service = "checkOrderItemStatus",
                mode = "sync"
            )
        }
    )
    public interface ChangeOrderItemStatuscommitSeca10 {}

    /**
     * SECA for service changeOrderItemStatus on event commit.
     */
    @Seca(
        service = "changeOrderItemStatus",
        event = "commit",
        condition = "statusId == 'ITEM_SENT'",
        actions = {
            @SecaAction(
                service = "checkOrderItemStatus",
                mode = "sync"
            )
        }
    )
    public interface ChangeOrderItemStatuscommitSeca11 {}

    /**
     * SECA for service changeOrderItemStatus on event commit.
     */
    @Seca(
        service = "changeOrderItemStatus",
        event = "commit",
        condition = "statusId == 'ITEM_APPROVED'",
        actions = {
            @SecaAction(
                service = "checkOrderItemStatus",
                mode = "sync"
            )
        }
    )
    public interface ChangeOrderItemStatuscommitSeca12 {}

    /**
     * SECA for service changeOrderItemStatus on event commit.
     */
    @Seca(
        service = "changeOrderItemStatus",
        event = "commit",
        condition = "statusId == 'ITEM_APPROVED'",
        actions = {
            @SecaAction(
                service = "checkDigitalItemFulfillment",
                mode = "sync"
            )
        }
    )
    public interface ChangeOrderItemStatuscommitSeca13 {}

    /**
     * SECA for service changeOrderItemStatus on event commit.
     */
    @Seca(
        service = "changeOrderItemStatus",
        event = "commit",
        condition = "statusId == 'ITEM_APPROVED'",
        actions = {
            @SecaAction(
                service = "invoiceServiceItems",
                mode = "sync"
            )
        }
    )
    public interface ChangeOrderItemStatuscommitSeca14 {}

    /**
     * SECA for service changeOrderStatus on event commit.
     */
    @Seca(
        service = "changeOrderStatus",
        event = "commit",
        condition = "statusId == 'ORDER_CANCELLED'",
        actions = {
            @SecaAction(
                service = "releaseOrderPayments",
                mode = "sync"
            ),
            @SecaAction(
                service = "processRefundReturnForReplacement",
                mode = "sync"
            )
        }
    )
    public interface ChangeOrderStatuscommitSeca15 {}

    /**
     * SECA for service changeOrderStatus on event global-commit-post-run.
     */
    @Seca(
        service = "changeOrderStatus",
        event = "global-commit-post-run",
        condition = "statusId == 'ORDER_COMPLETED' && statusId != oldStatusId",
        actions = {
            @SecaAction(
                service = "createInvoiceFromOrder",
                mode = "sync"
            ),
            @SecaAction(
                service = "resetGrandTotal",
                mode = "sync"
            ),
            @SecaAction(
                service = "sendOrderCompleteNotification",
                mode = "async",
                persist = "true"
            ),
            @SecaAction(
                service = "createReturnItemForRental",
                mode = "sync"
            )
        }
    )
    public interface ChangeOrderStatusglobalcommitpostrunSeca16 {}

    /**
     * SECA for service changeOrderStatus on event commit.
     */
    @Seca(
        service = "changeOrderStatus",
        event = "commit",
        condition = "orderTypeId == 'PURCHASE_ORDER' && statusId == 'ORDER_APPROVED' && statusId != oldStatusId",
        actions = {
            @SecaAction(
                service = "createPaymentFromOrder",
                mode = "sync",
                persist = "true"
            )
        }
    )
    public interface ChangeOrderStatuscommitSeca17 {}

    /**
     * SECA for service changeOrderStatus on event commit.
     */
    @Seca(
        service = "changeOrderStatus",
        event = "commit",
        condition = "orderTypeId == 'SALES_ORDER' && statusId == 'ORDER_COMPLETED' && statusId != oldStatusId",
        actions = {
            @SecaAction(
                service = "createPaymentFromOrder",
                mode = "sync",
                persist = "true"
            )
        }
    )
    public interface ChangeOrderStatuscommitSeca18 {}

    /**
     * SECA for service changeOrderStatus on event commit.
     */
    @Seca(
        service = "changeOrderStatus",
        event = "commit",
        condition = "statusId == 'ORDER_APPROVED' && orderTypeId == 'SALES_ORDER' && statusId != oldStatusId",
        actions = {
            @SecaAction(
                service = "updateContentSubscriptionByOrder",
                mode = "sync"
            ),
            @SecaAction(
                service = "processExtendSubscriptionByOrder",
                mode = "sync"
            )
        }
    )
    public interface ChangeOrderStatuscommitSeca19 {}

    /**
     * SECA for service createOrderAdjustment on event commit.
     */
    @Seca(
        service = "createOrderAdjustment",
        event = "commit",
        actions = {
            @SecaAction(
                service = "resetGrandTotal",
                mode = "sync"
            )
        }
    )
    public interface CreateOrderAdjustmentcommitSeca20 {}

    /**
     * SECA for service updateOrderAdjustment on event commit.
     */
    @Seca(
        service = "updateOrderAdjustment",
        event = "commit",
        actions = {
            @SecaAction(
                service = "resetGrandTotal",
                mode = "sync"
            )
        }
    )
    public interface UpdateOrderAdjustmentcommitSeca21 {}

    /**
     * SECA for service deleteOrderAdjustment on event commit.
     */
    @Seca(
        service = "deleteOrderAdjustment",
        event = "commit",
        actions = {
            @SecaAction(
                service = "resetGrandTotal",
                mode = "sync"
            )
        }
    )
    public interface DeleteOrderAdjustmentcommitSeca22 {}

    /**
     * SECA for service updateOrderItems on event commit.
     */
    @Seca(
        service = "updateOrderItems",
        event = "commit",
        actions = {
            @SecaAction(
                service = "resetGrandTotal",
                mode = "sync"
            ),
            @SecaAction(
                service = "sendOrderChangeNotification",
                mode = "async",
                persist = "true"
            )
        }
    )
    public interface UpdateOrderItemscommitSeca23 {}

    /**
     * SECA for service appendOrderItem on event commit.
     */
    @Seca(
        service = "appendOrderItem",
        event = "commit",
        actions = {
            @SecaAction(
                service = "resetGrandTotal",
                mode = "sync"
            )
        }
    )
    public interface AppendOrderItemcommitSeca24 {}

    /**
     * SECA for service cancelOrderItem on event global-commit-post-run.
     */
    @Seca(
        service = "cancelOrderItem",
        event = "global-commit-post-run",
        actions = {
            @SecaAction(
                service = "recreateOrderAdjustments",
                mode = "sync",
                runAsUser = "system"
            ),
            @SecaAction(
                service = "resetGrandTotal",
                mode = "sync"
            ),
            @SecaAction(
                service = "sendOrderChangeNotification",
                mode = "async",
                persist = "true"
            )
        }
    )
    public interface CancelOrderItemglobalcommitpostrunSeca25 {}

    /**
     * SECA for service updateOrderItems on event return.
     */
    @Seca(
        service = "updateOrderItems",
        event = "return",
        actions = {
            @SecaAction(
                service = "processOrderPayments",
                mode = "sync"
            )
        }
    )
    public interface UpdateOrderItemsreturnSeca26 {}

    /**
     * SECA for service appendOrderItem on event return.
     */
    @Seca(
        service = "appendOrderItem",
        event = "return",
        actions = {
            @SecaAction(
                service = "processOrderPayments",
                mode = "sync"
            )
        }
    )
    public interface AppendOrderItemreturnSeca27 {}

    /**
     * SECA for service sendOrderConfirmation on event commit.
     */
    @Seca(
        service = "sendOrderConfirmation",
        event = "commit",
        actions = {
            @SecaAction(
                service = "createOrderNotificationLog",
                mode = "sync"
            )
        }
    )
    public interface SendOrderConfirmationcommitSeca28 {}

    /**
     * SECA for service sendOrderChangeNotification on event commit.
     */
    @Seca(
        service = "sendOrderChangeNotification",
        event = "commit",
        actions = {
            @SecaAction(
                service = "createOrderNotificationLog",
                mode = "sync"
            )
        }
    )
    public interface SendOrderChangeNotificationcommitSeca29 {}

    /**
     * SECA for service sendOrderCompleteNotification on event commit.
     */
    @Seca(
        service = "sendOrderCompleteNotification",
        event = "commit",
        actions = {
            @SecaAction(
                service = "createOrderNotificationLog",
                mode = "sync"
            )
        }
    )
    public interface SendOrderCompleteNotificationcommitSeca30 {}

    /**
     * SECA for service sendOrderBackorderNotification on event commit.
     */
    @Seca(
        service = "sendOrderBackorderNotification",
        event = "commit",
        actions = {
            @SecaAction(
                service = "createOrderNotificationLog",
                mode = "sync"
            )
        }
    )
    public interface SendOrderBackorderNotificationcommitSeca31 {}

    /**
     * SECA for service sendOrderPayRetryNotification on event commit.
     */
    @Seca(
        service = "sendOrderPayRetryNotification",
        event = "commit",
        actions = {
            @SecaAction(
                service = "createOrderNotificationLog",
                mode = "sync"
            )
        }
    )
    public interface SendOrderPayRetryNotificationcommitSeca32 {}

    /**
     * SECA for service createOrderDeliverySchedule on event commit.
     */
    @Seca(
        service = "createOrderDeliverySchedule",
        event = "commit",
        actions = {
            @SecaAction(
                service = "sendOrderDeliveryScheduleNotification",
                mode = "async",
                persist = "true"
            )
        }
    )
    public interface CreateOrderDeliverySchedulecommitSeca33 {}

    /**
     * SECA for service updateOrderDeliverySchedule on event commit.
     */
    @Seca(
        service = "updateOrderDeliverySchedule",
        event = "commit",
        actions = {
            @SecaAction(
                service = "sendOrderDeliveryScheduleNotification",
                mode = "async",
                persist = "true"
            )
        }
    )
    public interface UpdateOrderDeliverySchedulecommitSeca34 {}

    /**
     * SECA for service createReturnHeader on event commit.
     */
    @Seca(
        service = "createReturnHeader",
        event = "commit",
        actions = {
            @SecaAction(
                service = "createReturnStatus",
                mode = "sync"
            )
        }
    )
    public interface CreateReturnHeadercommitSeca35 {}

    /**
     * SECA for service updateReturnHeader on event commit.
     */
    @Seca(
        service = "updateReturnHeader",
        event = "commit",
        actions = {
            @SecaAction(
                service = "checkReturnComplete",
                mode = "sync"
            )
        }
    )
    public interface UpdateReturnHeadercommitSeca36 {}

    /**
     * SECA for service updateReturnHeader on event return.
     */
    @Seca(
        service = "updateReturnHeader",
        event = "return",
        condition = "statusId == 'RETURN_ACCEPTED'",
        actions = {
            @SecaAction(
                service = "quickReceiveReturn",
                mode = "sync"
            )
        }
    )
    public interface UpdateReturnHeaderreturnSeca37 {}

    /**
     * SECA for service updateReturnHeader on event commit.
     */
    @Seca(
        service = "updateReturnHeader",
        event = "commit",
        condition = "statusId == 'RETURN_ACCEPTED' && oldStatusId != 'RETURN_ACCEPTED'",
        actions = {
            @SecaAction(
                service = "processWaitReplacementReservedReturn",
                mode = "sync"
            ),
            @SecaAction(
                service = "processReplaceImmediatelyReturn",
                mode = "sync"
            ),
            @SecaAction(
                service = "createShipmentAndItemsForReturn",
                mode = "sync"
            ),
            @SecaAction(
                service = "processCrossShipReplacementReturn",
                mode = "sync"
            ),
            @SecaAction(
                service = "createTrackingCodeOrderReturns",
                mode = "sync",
                runAsUser = "system"
            ),
            @SecaAction(
                service = "sendReturnAcceptNotification",
                mode = "async",
                persist = "true"
            ),
            @SecaAction(
                service = "processRefundImmediatelyReturn",
                mode = "sync"
            ),
            @SecaAction(
                service = "createReturnStatus",
                mode = "sync"
            )
        }
    )
    public interface UpdateReturnHeadercommitSeca38 {}

    /**
     * SECA for service updateReturnHeader on event commit.
     */
    @Seca(
        service = "updateReturnHeader",
        event = "commit",
        condition = "statusId == 'RETURN_RECEIVED' && oldStatusId != 'RETURN_RECEIVED'",
        actions = {
            @SecaAction(
                service = "addProductsBackToCategory",
                mode = "sync"
            ),
            @SecaAction(
                service = "processWaitReplacementReturn",
                mode = "sync"
            ),
            @SecaAction(
                service = "processWaitReplacementReservedReturn",
                mode = "sync"
            ),
            @SecaAction(
                service = "processRepairReplacementReturn",
                mode = "sync"
            ),
            @SecaAction(
                service = "processCreditReturn",
                mode = "sync"
            ),
            @SecaAction(
                service = "processRefundOnlyReturn",
                mode = "sync"
            )
        }
    )
    public interface UpdateReturnHeadercommitSeca39 {}

    /**
     * SECA for service updateReturnStatusFromReceipt on event global-commit-post-run.
     */
    @Seca(
        service = "updateReturnStatusFromReceipt",
        event = "global-commit-post-run",
        condition = "returnHeaderStatus == 'RETURN_RECEIVED'",
        actions = {
            @SecaAction(
                service = "addProductsBackToCategory",
                mode = "sync"
            ),
            @SecaAction(
                service = "processWaitReplacementReturn",
                mode = "sync"
            ),
            @SecaAction(
                service = "processRepairReplacementReturn",
                mode = "sync"
            ),
            @SecaAction(
                service = "processCreditReturn",
                mode = "sync"
            ),
            @SecaAction(
                service = "processRefundOnlyReturn",
                mode = "sync"
            )
        }
    )
    public interface UpdateReturnStatusFromReceiptglobalcommitpostrunSeca40 {}

    /**
     * SECA for service updateReturnHeader on event commit.
     */
    @Seca(
        service = "updateReturnHeader",
        event = "commit",
        condition = "statusId == 'RETURN_COMPLETED' && oldStatusId != 'RETURN_COMPLETED'",
        actions = {
            @SecaAction(
                service = "sendReturnCompleteNotification",
                mode = "async",
                persist = "true"
            ),
            @SecaAction(
                service = "processSubscriptionReturn",
                mode = "sync"
            ),
            @SecaAction(
                service = "createReturnStatus",
                mode = "sync"
            ),
            @SecaAction(
                service = "createInvoiceFromReturn",
                mode = "sync"
            )
        }
    )
    public interface UpdateReturnHeadercommitSeca41 {}

    /**
     * SECA for service updateReturnHeader on event commit.
     */
    @Seca(
        service = "updateReturnHeader",
        event = "commit",
        condition = "statusId == 'RETURN_CANCELLED' && oldStatusId != 'RETURN_CANCELLED'",
        actions = {
            @SecaAction(
                service = "cancelReturnItems",
                mode = "sync"
            ),
            @SecaAction(
                service = "createReturnStatus",
                mode = "sync"
            ),
            @SecaAction(
                service = "sendReturnCancelNotification",
                mode = "async",
                persist = "true"
            )
        }
    )
    public interface UpdateReturnHeadercommitSeca42 {}

    /**
     * SECA for service updateReturnHeader on event commit.
     */
    @Seca(
        service = "updateReturnHeader",
        event = "commit",
        condition = "statusId == 'SUP_RETURN_SHIPPED' && oldStatusId != 'SUP_RETURN_SHIPPED'",
        actions = {
            @SecaAction(
                service = "createReturnStatus",
                mode = "sync"
            ),
            @SecaAction(
                service = "processWaitReplacementReturn",
                mode = "sync"
            )
        }
    )
    public interface UpdateReturnHeadercommitSeca43 {}

    /**
     * SECA for service updateReturnHeader on event commit.
     */
    @Seca(
        service = "updateReturnHeader",
        event = "commit",
        condition = "statusId == 'RETURN_ACCEPTED' && oldStatusId != 'RETURN_ACCEPTED'",
        actions = {
            @SecaAction(
                service = "updateReturnItemsStatus",
                mode = "sync"
            )
        }
    )
    public interface UpdateReturnHeadercommitSeca44 {}

    /**
     * SECA for service updateReturnItem on event commit.
     */
    @Seca(
        service = "updateReturnItem",
        event = "commit",
        condition = "statusId == 'RETURN_CANCELLED' && oldStatusId != 'RETURN_CANCELLED'",
        actions = {
            @SecaAction(
                service = "cancelReplacementOrderItems",
                mode = "sync"
            )
        }
    )
    public interface UpdateReturnItemcommitSeca45 {}

    /**
     * SECA for service processReplacementReturn on event commit.
     */
    @Seca(
        service = "processReplacementReturn",
        event = "commit",
        actions = {
            @SecaAction(
                service = "checkReturnComplete",
                mode = "sync"
            )
        }
    )
    public interface ProcessReplacementReturncommitSeca46 {}

    /**
     * SECA for service processCreditReturn on event commit.
     */
    @Seca(
        service = "processCreditReturn",
        event = "commit",
        actions = {
            @SecaAction(
                service = "checkReturnComplete",
                mode = "sync"
            )
        }
    )
    public interface ProcessCreditReturncommitSeca47 {}

    /**
     * SECA for service processRefundReturn on event commit.
     */
    @Seca(
        service = "processRefundReturn",
        event = "commit",
        actions = {
            @SecaAction(
                service = "checkReturnComplete",
                mode = "sync"
            )
        }
    )
    public interface ProcessRefundReturncommitSeca48 {}

    /**
     * SECA for service storeOrder on event commit.
     */
    @Seca(
        service = "storeOrder",
        event = "commit",
        condition = "!empty(originOrderId)",
        actions = {
            @SecaAction(
                service = "createExchangeOrderAssoc",
                mode = "sync"
            )
        }
    )
    public interface StoreOrdercommitSeca49 {}

    /**
     * SECA for service createShoppingList on event in-validate.
     */
    @Seca(
        service = "createShoppingList",
        event = "in-validate",
        condition = "!empty(shippingMethodString)",
        actions = {
            @SecaAction(
                service = "splitShipmentMethodString",
                mode = "sync"
            )
        }
    )
    public interface CreateShoppingListinvalidateSeca50 {}

    /**
     * SECA for service updateShoppingList on event in-validate.
     */
    @Seca(
        service = "updateShoppingList",
        event = "in-validate",
        condition = "!empty(shippingMethodString)",
        actions = {
            @SecaAction(
                service = "splitShipmentMethodString",
                mode = "sync"
            )
        }
    )
    public interface UpdateShoppingListinvalidateSeca51 {}

    /**
     * SECA for service createShoppingList on event in-validate.
     */
    @Seca(
        service = "createShoppingList",
        event = "in-validate",
        condition = "!empty(frequency)",
        actions = {
            @SecaAction(
                service = "createShoppingListRecurrence",
                mode = "sync"
            )
        }
    )
    public interface CreateShoppingListinvalidateSeca52 {}

    /**
     * SECA for service updateShoppingList on event in-validate.
     */
    @Seca(
        service = "updateShoppingList",
        event = "in-validate",
        condition = "!empty(frequency)",
        actions = {
            @SecaAction(
                service = "createShoppingListRecurrence",
                mode = "sync"
            )
        }
    )
    public interface UpdateShoppingListinvalidateSeca53 {}

    /**
     * SECA for service createShoppingListItem on event in-validate.
     */
    @Seca(
        service = "createShoppingListItem",
        event = "in-validate",
        condition = "empty(shoppingListId)",
        actions = {
            @SecaAction(
                service = "createShoppingList",
                mode = "sync"
            )
        }
    )
    public interface CreateShoppingListIteminvalidateSeca54 {}

    /**
     * SECA for service storeOrder on event commit.
     */
    @Seca(
        service = "storeOrder",
        event = "commit",
        actions = {
            @SecaAction(
                service = "updateShoppingListQuantitiesFromOrder",
                mode = "sync"
            )
        }
    )
    public interface StoreOrdercommitSeca55 {}

    /**
     * SECA for service createCustRequest on event commit.
     */
    @Seca(
        service = "createCustRequest",
        event = "commit",
        condition = "!empty(communicationEventId) && !empty(custRequestId)",
        actions = {
            @SecaAction(
                service = "updateCommunicationEvent",
                mode = "sync"
            )
        }
    )
    public interface CreateCustRequestcommitSeca56 {}

    /**
     * SECA for service updateCustRequest on event commit.
     */
    @Seca(
        service = "updateCustRequest",
        event = "commit",
        condition = "!empty(communicationEventId) && !empty(custRequestId)",
        actions = {
            @SecaAction(
                service = "updateCommunicationEvent",
                mode = "sync"
            )
        }
    )
    public interface UpdateCustRequestcommitSeca57 {}

    /**
     * SECA for service createCustRequestItemNote on event commit.
     */
    @Seca(
        service = "createCustRequestItemNote",
        event = "commit",
        assignments = {
            @SecaSet(fieldName = "noteParty", envName = "partyId"),
            @SecaSet(fieldName = "noteInfo", value = "A note has been added to customer request"),
            @SecaSet(fieldName = "moreInfoItemName", value = "custRequestId"),
            @SecaSet(fieldName = "moreInfoItemId", envName = "custRequestId"),
            @SecaSet(fieldName = "moreInfoUrl", value = "/ordermgr/control/ViewRequest")
        },
        actions = {
            @SecaAction(
                service = "createSystemInfoNote",
                mode = "sync"
            )
        }
    )
    public interface CreateCustRequestItemNotecommitSeca58 {}

    /**
     * SECA for service createRequirement on event commit.
     */
    @Seca(
        service = "createRequirement",
        event = "commit",
        condition = "!empty(custRequestId) && !empty(custRequestItemSeqId)",
        actions = {
            @SecaAction(
                service = "associatedRequirementWithRequestItem",
                mode = "sync"
            )
        }
    )
    public interface CreateRequirementcommitSeca59 {}

    /**
     * SECA for service createRequirement on event commit.
     */
    @Seca(
        service = "createRequirement",
        event = "commit",
        condition = "!empty(productId) && requirementTypeId == 'PRODUCT_REQUIREMENT'",
        actions = {
            @SecaAction(
                service = "autoAssignRequirementToSupplier",
                mode = "sync"
            )
        }
    )
    public interface CreateRequirementcommitSeca60 {}

    /**
     * SECA for service createItemIssuance on event invoke.
     */
    @Seca(
        service = "createItemIssuance",
        event = "invoke",
        condition = "quantity > 0",
        actions = {
            @SecaAction(
                service = "checkCreateStockRequirementQoh",
                mode = "sync",
                runAsUser = "system"
            )
        }
    )
    public interface CreateItemIssuanceinvokeSeca61 {}

    /**
     * SECA for service updateItemIssuance on event invoke.
     */
    @Seca(
        service = "updateItemIssuance",
        event = "invoke",
        condition = "quantity > 0",
        actions = {
            @SecaAction(
                service = "checkCreateStockRequirementQoh",
                mode = "sync",
                runAsUser = "system"
            )
        }
    )
    public interface UpdateItemIssuanceinvokeSeca62 {}

    /**
     * SECA for service reserveOrderItemInventory on event commit.
     */
    @Seca(
        service = "reserveOrderItemInventory",
        event = "commit",
        condition = "quantity > 0",
        actions = {
            @SecaAction(
                service = "checkCreateStockRequirementAtp",
                mode = "sync",
                runAsUser = "system"
            )
        }
    )
    public interface ReserveOrderItemInventorycommitSeca63 {}

    /**
     * SECA for service changeOrderStatus on event commit.
     */
    @Seca(
        service = "changeOrderStatus",
        event = "commit",
        condition = "oldStatusId == 'ORDER_CREATED' && statusId == 'ORDER_APPROVED' && orderTypeId == 'SALES_ORDER'",
        actions = {
            @SecaAction(
                service = "createAutoRequirementsForOrder",
                mode = "sync"
            ),
            @SecaAction(
                service = "createATPRequirementsForOrder",
                mode = "sync"
            )
        }
    )
    public interface ChangeOrderStatuscommitSeca64 {}

    /**
     * SECA for service changeOrderStatus on event commit.
     */
    @Seca(
        service = "changeOrderStatus",
        event = "commit",
        condition = "oldStatusId == 'ORDER_CREATED' && statusId == 'ORDER_APPROVED' && orderTypeId == 'PURCHASE_ORDER'",
        actions = {
            @SecaAction(
                service = "updateRequirementsToOrdered",
                mode = "sync"
            )
        }
    )
    public interface ChangeOrderStatuscommitSeca65 {}

    /**
     * SECA for service createQuoteRole on event invoke.
     */
    @Seca(
        service = "createQuoteRole",
        event = "invoke",
        actions = {
            @SecaAction(
                service = "ensurePartyRole",
                mode = "sync"
            )
        }
    )
    public interface CreateQuoteRoleinvokeSeca66 {}

    /**
     * SECA for service createQuoteWorkEffort on event in-validate.
     */
    @Seca(
        service = "createQuoteWorkEffort",
        event = "in-validate",
        condition = "empty(workEffortId)",
        actions = {
            @SecaAction(
                service = "createWorkEffort",
                mode = "sync"
            )
        }
    )
    public interface CreateQuoteWorkEffortinvalidateSeca67 {}

    /**
     * SECA for service setCustRequestStatus on event commit.
     */
    @Seca(
        service = "setCustRequestStatus",
        event = "commit",
        condition = "oldStatusId != 'CRQ_ACCEPTED' && oldStatusId != 'CRQ_PENDING' && statusId == 'CRQ_ACCEPTED'",
        assignments = {
            @SecaSet(fieldName = "bodyParameters.custRequestId", envName = "custRequestId"),
            @SecaSet(fieldName = "bodyParameters.custRequestName", envName = "custRequestName"),
            @SecaSet(fieldName = "partyIdTo", envName = "fromPartyId"),
            @SecaSet(fieldName = "emailTemplateSettingId", value = "CUST_REQ_ACCEPTED")
        },
        actions = {
            @SecaAction(
                service = "sendMailFromTemplateSetting",
                mode = "sync"
            )
        }
    )
    public interface SetCustRequestStatuscommitSeca68 {}

    /**
     * SECA for service setCustRequestStatus on event commit.
     */
    @Seca(
        service = "setCustRequestStatus",
        event = "commit",
        condition = "oldStatusId != 'CRQ_COMPLETED' && statusId == 'CRQ_COMPLETED'",
        assignments = {
            @SecaSet(fieldName = "bodyParameters.custRequestId", envName = "custRequestId"),
            @SecaSet(fieldName = "partyIdTo", envName = "fromPartyId"),
            @SecaSet(fieldName = "emailTemplateSettingId", value = "CUST_REQ_COMPLETED")
        },
        actions = {
            @SecaAction(
                service = "sendMailFromTemplateSetting",
                mode = "sync"
            )
        }
    )
    public interface SetCustRequestStatuscommitSeca69 {}

    /**
     * SECA for service createCustRequestItemNote on event commit.
     */
    @Seca(
        service = "createCustRequestItemNote",
        event = "commit",
        condition = "partyIdTo != 'notePartyId'",
        assignments = {
            @SecaSet(fieldName = "bodyParameters.custRequestId", envName = "custRequestId"),
            @SecaSet(fieldName = "bodyParameters.custRequestItemSeqId", envName = "custRequestItemSeqId"),
            @SecaSet(fieldName = "bodyParameters.noteId", envName = "noteId"),
            @SecaSet(fieldName = "partyIdTo", envName = "fromPartyId"),
            @SecaSet(fieldName = "emailTemplateSettingId", value = "CUST_REQ_NOTE_ADDED")
        },
        actions = {
            @SecaAction(
                service = "sendMailFromTemplateSetting",
                mode = "sync"
            )
        }
    )
    public interface CreateCustRequestItemNotecommitSeca70 {}

    /**
     * SECA for service createSalesOpportunity on event commit.
     */
    @Seca(
        service = "createSalesOpportunity",
        event = "commit",
        condition = "!empty(accountPartyId)",
        actions = {
            @SecaAction(
                service = "createSalesOpportunityAccountRole",
                mode = "sync"
            )
        }
    )
    public interface CreateSalesOpportunitycommitSeca71 {}

    /**
     * SECA for service updateSalesOpportunity on event commit.
     */
    @Seca(
        service = "updateSalesOpportunity",
        event = "commit",
        condition = "!empty(accountPartyId)",
        actions = {
            @SecaAction(
                service = "createSalesOpportunityAccountRole",
                mode = "sync"
            )
        }
    )
    public interface UpdateSalesOpportunitycommitSeca72 {}

    /**
     * SECA for service createSalesOpportunity on event commit.
     */
    @Seca(
        service = "createSalesOpportunity",
        event = "commit",
        condition = "!empty(leadPartyId)",
        actions = {
            @SecaAction(
                service = "createSalesOpportunityLeadRole",
                mode = "sync"
            )
        }
    )
    public interface CreateSalesOpportunitycommitSeca73 {}

    /**
     * SECA for service updateSalesOpportunity on event commit.
     */
    @Seca(
        service = "updateSalesOpportunity",
        event = "commit",
        condition = "!empty(leadPartyId)",
        actions = {
            @SecaAction(
                service = "createSalesOpportunityLeadRole",
                mode = "sync"
            )
        }
    )
    public interface UpdateSalesOpportunitycommitSeca74 {}

    /**
     * SECA for service createReturnItem on event commit.
     */
    @Seca(
        service = "createReturnItem",
        event = "commit",
        actions = {
            @SecaAction(
                service = "createReturnStatus",
                mode = "sync"
            )
        }
    )
    public interface CreateReturnItemcommitSeca75 {}

    /**
     * SECA for service updateReturnItem on event commit.
     */
    @Seca(
        service = "updateReturnItem",
        event = "commit",
        condition = "!empty(statusId) && statusId != oldStatusId",
        actions = {
            @SecaAction(
                service = "createReturnStatus",
                mode = "sync"
            )
        }
    )
    public interface UpdateReturnItemcommitSeca76 {}

    /**
     * SECA for service createUpdateCustomerAndShippingAddress on event invoke.
     */
    @Seca(
        service = "createUpdateCustomerAndShippingAddress",
        event = "invoke",
        actions = {
            @SecaAction(
                service = "setAnonUserLogin",
                mode = "sync"
            )
        }
    )
    public interface CreateUpdateCustomerAndShippingAddressinvokeSeca77 {}

    /**
     * SECA for service createPaymentFromPreference on event commit.
     */
    @Seca(
        service = "createPaymentFromPreference",
        event = "commit",
        condition = "!empty(paymentId)",
        actions = {
            @SecaAction(
                service = "createOrderPaymentApplication",
                mode = "sync"
            )
        }
    )
    public interface CreatePaymentFromPreferencecommitSeca78 {}

    /**
     * SECA for service storeOrder on event commit.
     */
    @Seca(
        service = "storeOrder",
        event = "commit",
        condition = "orderTypeId == 'SALES_ORDER'",
        actions = {
            @SecaAction(
                service = "checkOrderItemForProductGroupOrder",
                mode = "sync"
            )
        }
    )
    public interface StoreOrdercommitSeca79 {}

    /**
     * SECA for service updateShipGroupShipInfo on event commit.
     */
    @Seca(
        service = "updateShipGroupShipInfo",
        event = "commit",
        actions = {
            @SecaAction(
                service = "resetGrandTotal",
                mode = "sync"
            )
        }
    )
    public interface UpdateShipGroupShipInfocommitSeca80 {}

    /**
     * SECA for service changeOrderStatus on event commit.
     */
    @Seca(
        service = "changeOrderStatus",
        event = "commit",
        condition = "statusId == 'ORDER_CANCELLED'",
        assignments = {
            @SecaSet(fieldName = "noteParty", value = "admin"),
            @SecaSet(fieldName = "noteInfo", value = "An order has been cancelled"),
            @SecaSet(fieldName = "moreInfoItemName", value = "orderId"),
            @SecaSet(fieldName = "moreInfoItemId", envName = "orderId"),
            @SecaSet(fieldName = "moreInfoUrl", value = "/ordermgr/control/orderview")
        },
        actions = {
            @SecaAction(
                service = "createSystemInfoNote",
                mode = "sync"
            )
        }
    )
    public interface ChangeOrderStatuscommitSeca81 {}

    /**
     * SECA for service updateReturnHeader on event commit.
     */
    @Seca(
        service = "updateReturnHeader",
        event = "commit",
        condition = "statusId == 'RETURN_REQUESTED'",
        assignments = {
            @SecaSet(fieldName = "noteParty", value = "admin"),
            @SecaSet(fieldName = "noteInfo", value = "A return has been requested"),
            @SecaSet(fieldName = "moreInfoItemName", value = "returnId"),
            @SecaSet(fieldName = "moreInfoItemId", envName = "returnId"),
            @SecaSet(fieldName = "moreInfoUrl", value = "/ordermgr/control/returnMain")
        },
        actions = {
            @SecaAction(
                service = "createSystemInfoNote",
                mode = "sync"
            )
        }
    )
    public interface UpdateReturnHeadercommitSeca82 {}

    /**
     * SECA for service storeOrder on event global-commit-post-run.
     */
    @Seca(
        service = "storeOrder",
        event = "global-commit-post-run",
        condition = "orderTypeId == 'SALES_ORDER'",
        assignments = {
            @SecaSet(fieldName = "channel", value = "orderdatalive"),
            @SecaSet(fieldName = "interval", value = "HOUR")
        },
        actions = {
            @SecaAction(
                service = "sendOrderLiveData",
                mode = "sync"
            )
        }
    )
    public interface StoreOrderglobalcommitpostrunSeca83 {}

    /**
     * SECA for service storeOrder on event global-commit-post-run.
     */
    @Seca(
        service = "storeOrder",
        event = "global-commit-post-run",
        condition = "orderTypeId == 'SALES_ORDER'",
        assignments = {
            @SecaSet(fieldName = "channel", value = "orderdata")
        },
        actions = {
            @SecaAction(
                service = "wsSendOrder",
                mode = "sync"
            )
        }
    )
    public interface StoreOrderglobalcommitpostrunSeca84 {}

    /**
     * SECA for service changeOrderStatus on event global-commit-post-run.
     */
    @Seca(
        service = "changeOrderStatus",
        event = "global-commit-post-run",
        condition = "orderTypeId == 'SALES_ORDER'",
        assignments = {
            @SecaSet(fieldName = "channel", value = "orderdata")
        },
        actions = {
            @SecaAction(
                service = "wsSendOrder",
                mode = "sync",
                priority = "10"
            )
        }
    )
    public interface ChangeOrderStatusglobalcommitpostrunSeca85 {}

    /**
     * SECA for service changeOrderItemStatus on event global-commit-post-run.
     */
    @Seca(
        service = "changeOrderItemStatus",
        event = "global-commit-post-run",
        condition = "(statusId != 'ITEM_CREATED' && statusId != 'ITEM_APPROVED')",
        assignments = {
            @SecaSet(fieldName = "channel", value = "orderitemdata")
        },
        actions = {
            @SecaAction(
                service = "wsSendOrderItem",
                mode = "sync",
                priority = "10"
            )
        }
    )
    public interface ChangeOrderItemStatusglobalcommitpostrunSeca86 {}

}
