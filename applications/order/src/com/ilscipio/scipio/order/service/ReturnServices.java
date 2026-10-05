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
public class ReturnServices {

    /**
     * Quick Return Order
     */
    @Service(
        name = "quickReturnOrder",
        engine = "simple",
        location = "component://order/script/org/ofbiz/order/order/OrderReturnServices.xml",
        invoke = "quickReturnFromOrder",
        description = "Quick Return Order",
        attributes = {
            @Attribute(name = "orderId", type = "String", mode = "IN"),
            @Attribute(name = "returnReasonId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "returnTypeId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "returnHeaderTypeId", type = "String", mode = "IN"),
            @Attribute(name = "receiveReturn", type = "Boolean", mode = "IN", optional = "true"),
            @Attribute(name = "returnId", type = "String", mode = "OUT")
        }
    )
    public interface QuickReturnOrder {}

    /**
     * Create a new ReturnHeader
     */
    @Service(
        name = "createReturnHeader",
        engine = "simple",
        location = "component://order/script/org/ofbiz/order/order/OrderReturnServices.xml",
        invoke = "createReturnHeader",
        description = "Create a new ReturnHeader",
        defaultEntityName = "ReturnHeader",
        entityAttributes = {
            @EntityAttributes(mode = "OUT", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        overrideAttributes = {
            @OverrideAttribute(name = "returnHeaderTypeId", optional = "false")
        }
    )
    public interface CreateReturnHeader {}

    /**
     * Update a ReturnHeader
     */
    @Service(
        name = "updateReturnHeader",
        engine = "simple",
        location = "component://order/script/org/ofbiz/order/order/OrderReturnServices.xml",
        invoke = "updateReturnHeader",
        description = "Update a ReturnHeader",
        defaultEntityName = "ReturnHeader",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        attributes = {
            @Attribute(name = "oldStatusId", type = "String", mode = "OUT")
        },
        permissionService = @PermissionService(service = "returnPermissionCheck", mainAction = "UPDATE")
    )
    public interface UpdateReturnHeader {}

    /**
     * Create a new return item billing record
     */
    @Service(
        name = "createReturnItemBilling",
        engine = "simple",
        location = "component://order/script/org/ofbiz/order/order/OrderReturnServices.xml",
        invoke = "createReturnItemBilling",
        description = "Create a new return item billing record",
        defaultEntityName = "ReturnItemBilling",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface CreateReturnItemBilling {}

    /**
     * Create a new ReturnItem in the RETURN_REQUESTED status, based on returnableQuantity and returnablePrice from the                      getReturnableQuantity service.  This can be called by the customer to request a return for himself or by a user with                      ORDERMGR_CREATE, but, if the former, the returnPrice will be overriden by the returnablePrice from getReturnableQuantity.
     */
    @Service(
        name = "createReturnItem",
        engine = "simple",
        location = "component://order/script/org/ofbiz/order/order/OrderReturnServices.xml",
        invoke = "createReturnItem",
        description = "Create a new ReturnItem in the RETURN_REQUESTED status, based on returnableQuantity and returnablePrice from the\n                     getReturnableQuantity service.  This can be called by the customer to request a return for himself or by a user with\n                     ORDERMGR_CREATE, but, if the former, the returnPrice will be overriden by the returnablePrice from getReturnableQuantity.",
        entityAttributes = {
            @EntityAttributes(entityName = "ReturnItem", mode = "IN", optional = "true", excludeFields = {"returnItemSeqId"})
        },
        attributes = {
            @Attribute(name = "includeAdjustments", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "returnItemSeqId", type = "String", mode = "OUT")
        },
        overrideAttributes = {
            @OverrideAttribute(name = "returnId", optional = "false"),
            @OverrideAttribute(name = "returnTypeId", optional = "false"),
            @OverrideAttribute(name = "returnItemTypeId", optional = "false"),
            @OverrideAttribute(name = "orderId", optional = "false"),
            @OverrideAttribute(name = "returnQuantity", optional = "false")
        }
    )
    public interface CreateReturnItem {}

    /**
     * Update a ReturnItem and related adjustments
     */
    @Service(
        name = "updateReturnItem",
        engine = "simple",
        location = "component://order/script/org/ofbiz/order/order/OrderReturnServices.xml",
        invoke = "updateReturnItem",
        description = "Update a ReturnItem and related adjustments",
        defaultEntityName = "ReturnItem",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        attributes = {
            @Attribute(name = "oldStatusId", type = "String", mode = "OUT")
        },
        permissionService = @PermissionService(service = "returnPermissionCheck", mainAction = "UPDATE")
    )
    public interface UpdateReturnItem {}

    /**
     * Update ReturnItem(s) Status
     */
    @Service(
        name = "updateReturnItemsStatus",
        engine = "simple",
        location = "component://order/script/org/ofbiz/order/order/OrderReturnServices.xml",
        invoke = "updateReturnItemsStatus",
        description = "Update ReturnItem(s) Status",
        attributes = {
            @Attribute(name = "returnId", type = "String", mode = "IN"),
            @Attribute(name = "statusId", type = "String", mode = "IN")
        },
        permissionService = @PermissionService(service = "returnPermissionCheck", mainAction = "UPDATE")
    )
    public interface UpdateReturnItemsStatus {}

    /**
     * Remove a ReturnItem and related adjustments
     */
    @Service(
        name = "removeReturnItem",
        engine = "simple",
        location = "component://order/script/org/ofbiz/order/order/OrderReturnServices.xml",
        invoke = "removeReturnItem",
        description = "Remove a ReturnItem and related adjustments",
        entityAttributes = {
            @EntityAttributes(entityName = "ReturnItem", mode = "IN", include = "pk")
        },
        permissionService = @PermissionService(service = "returnPermissionCheck", mainAction = "DELETE")
    )
    public interface RemoveReturnItem {}

    /**
     * Creates a ReturnItemResponse record.
     */
    @Service(
        name = "createReturnItemResponse",
        engine = "simple",
        location = "component://order/script/org/ofbiz/order/order/OrderReturnServices.xml",
        invoke = "createReturnItemResponse",
        description = "Creates a ReturnItemResponse record.",
        entityAttributes = {
            @EntityAttributes(entityName = "ReturnItemResponse", mode = "IN", include = "nonpk", optional = "true")
        },
        attributes = {
            @Attribute(name = "returnItemResponseId", type = "String", mode = "OUT")
        }
    )
    public interface CreateReturnItemResponse {}

    /**
     * Creates PaymentApplications for each return item billing related to the return response until                 the responseAmount is reached or all items are paid.
     */
    @Service(
        name = "createPaymentApplicationsFromReturnItemResponse",
        engine = "java",
        location = "org.ofbiz.order.order.OrderReturnServices",
        invoke = "createPaymentApplicationsFromReturnItemResponse",
        description = "Creates PaymentApplications for each return item billing related to the return response until\n                the responseAmount is reached or all items are paid.",
        attributes = {
            @Attribute(name = "returnItemResponseId", type = "String", mode = "IN")
        }
    )
    public interface CreatePaymentApplicationsFromReturnItemResponse {}

    /**
     * Cancel ReturnItems and set their status to "RETURN_CANCELLED"
     */
    @Service(
        name = "cancelReturnItems",
        engine = "simple",
        location = "component://order/script/org/ofbiz/order/order/OrderReturnServices.xml",
        invoke = "cancelReturnItems",
        description = "Cancel ReturnItems and set their status to \"RETURN_CANCELLED\"",
        attributes = {
            @Attribute(name = "returnId", type = "String", mode = "IN")
        },
        permissionService = @PermissionService(service = "returnPermissionCheck", mainAction = "UPDATE")
    )
    public interface CancelReturnItems {}

    /**
     * Cancel the associated OrderItems of the replacement order, if any.
     */
    @Service(
        name = "cancelReplacementOrderItems",
        engine = "simple",
        location = "component://order/script/org/ofbiz/order/order/OrderReturnServices.xml",
        invoke = "cancelReplacementOrderItems",
        description = "Cancel the associated OrderItems of the replacement order, if any.",
        entityAttributes = {
            @EntityAttributes(entityName = "ReturnItem", mode = "IN", include = "pk")
        },
        permissionService = @PermissionService(service = "returnPermissionCheck", mainAction = "UPDATE")
    )
    public interface CancelReplacementOrderItems {}

    /**
     * Return Adjustment Interface
     */
    @Service(
        name = "returnAdjustmentInterface",
        engine = "interface",
        description = "Return Adjustment Interface",
        entityAttributes = {
            @EntityAttributes(entityName = "ReturnAdjustment", mode = "IN", optional = "true")
        }
    )
    public interface ReturnAdjustmentInterface {}

    /**
     * Simple create service
     */
    @Service(
        name = "createReturnAdjustment",
        engine = "java",
        location = "org.ofbiz.order.order.OrderReturnServices",
        invoke = "createReturnAdjustment",
        description = "Simple create service",
        implemented = {@Implements(service = "returnAdjustmentInterface")},
        overrideAttributes = {
            @OverrideAttribute(name = "returnAdjustmentId", mode = "OUT", optional = "false")
        }
    )
    public interface CreateReturnAdjustment {}

    @Service(
        name = "updateReturnAdjustment",
        engine = "java",
        location = "org.ofbiz.order.order.OrderReturnServices",
        invoke = "updateReturnAdjustment",
        implemented = {@Implements(service = "returnAdjustmentInterface")},
        attributes = {
            @Attribute(name = "originalReturnPrice", type = "BigDecimal", mode = "IN", optional = "true"),
            @Attribute(name = "originalReturnQuantity", type = "BigDecimal", mode = "IN", optional = "true")
        },
        overrideAttributes = {
            @OverrideAttribute(name = "returnAdjustmentId", optional = "false")
        }
    )
    public interface UpdateReturnAdjustment {}

    /**
     * Simple remove service
     */
    @Service(
        name = "removeReturnAdjustment",
        engine = "simple",
        location = "component://order/script/org/ofbiz/order/order/OrderReturnServices.xml",
        invoke = "removeReturnAdjustment",
        description = "Simple remove service",
        entityAttributes = {
            @EntityAttributes(entityName = "ReturnAdjustment", mode = "IN", include = "pk")
        },
        permissionService = @PermissionService(service = "returnPermissionCheck", mainAction = "DELETE")
    )
    public interface RemoveReturnAdjustment {}

    /**
     * If returnId is null, create a return; then create Return Item or Adjustment based on the parameters passed in
     */
    @Service(
        name = "createReturnAndItemOrAdjustment",
        engine = "simple",
        location = "component://order/script/org/ofbiz/order/order/OrderReturnServices.xml",
        invoke = "createReturnAndItemOrAdjustment",
        description = "If returnId is null, create a return; then create Return Item or Adjustment based on the parameters passed in",
        entityAttributes = {
            @EntityAttributes(entityName = "ReturnHeader", mode = "IN", include = "nonpk", optional = "true"),
            @EntityAttributes(entityName = "ReturnAdjustment", mode = "IN", optional = "true"),
            @EntityAttributes(entityName = "ReturnItem", mode = "IN", optional = "true")
        },
        attributes = {
            @Attribute(name = "returnAdjustmentId", type = "String", mode = "OUT", optional = "true"),
            @Attribute(name = "returnItemSeqId", type = "String", mode = "OUT", optional = "true"),
            @Attribute(name = "returnId", type = "String", mode = "OUT", optional = "true")
        }
    )
    public interface CreateReturnAndItemOrAdjustment {}

    /**
     * create Return Item or Adjustment based on the parameters passed in
     */
    @Service(
        name = "createReturnItemOrAdjustment",
        engine = "java",
        location = "org.ofbiz.order.order.OrderReturnServices",
        invoke = "createReturnItemOrAdjustment",
        description = "create Return Item or Adjustment based on the parameters passed in",
        entityAttributes = {
            @EntityAttributes(entityName = "ReturnAdjustment", mode = "IN", optional = "true"),
            @EntityAttributes(entityName = "ReturnItem", mode = "IN", optional = "true")
        },
        attributes = {
            @Attribute(name = "returnAdjustmentId", type = "String", mode = "OUT", optional = "true"),
            @Attribute(name = "returnItemSeqId", type = "String", mode = "OUT", optional = "true")
        }
    )
    public interface CreateReturnItemOrAdjustment {}

    /**
     * update Return Item or Adjustment based on the parameters passed in
     */
    @Service(
        name = "updateReturnItemOrAdjustment",
        engine = "java",
        location = "org.ofbiz.order.order.OrderReturnServices",
        invoke = "updateReturnItemOrAdjustment",
        description = "update Return Item or Adjustment based on the parameters passed in",
        entityAttributes = {
            @EntityAttributes(entityName = "ReturnAdjustment", mode = "IN", optional = "true"),
            @EntityAttributes(entityName = "ReturnItem", mode = "IN", optional = "true")
        },
        attributes = {
            @Attribute(name = "oldStatusId", type = "String", mode = "OUT", optional = "true")
        }
    )
    public interface UpdateReturnItemOrAdjustment {}

    /**
     * Finds the refunded or credited payment amounts for each order on a return
     */
    @Service(
        name = "getReturnAmountByOrder",
        engine = "java",
        location = "org.ofbiz.order.order.OrderReturnServices",
        invoke = "getReturnAmountByOrder",
        description = "Finds the refunded or credited payment amounts for each order on a return",
        attributes = {
            @Attribute(name = "returnId", type = "String", mode = "IN"),
            @Attribute(name = "orderReturnAmountMap", type = "Map", mode = "OUT")
        }
    )
    public interface GetReturnAmountByOrder {}

    /**
     * Makes sure the return is not over-refunding/crediting any order, or return an error
     */
    @Service(
        name = "checkPaymentAmountForRefund",
        engine = "java",
        location = "org.ofbiz.order.order.OrderReturnServices",
        invoke = "checkPaymentAmountForRefund",
        description = "Makes sure the return is not over-refunding/crediting any order, or return an error",
        attributes = {
            @Attribute(name = "returnId", type = "String", mode = "IN")
        }
    )
    public interface CheckPaymentAmountForRefund {}

    /**
     * Gets the item's initial cost based on the inventory item record associated with the order item or 0.00 if none found.
     */
    @Service(
        name = "getReturnItemInitialCost",
        engine = "java",
        location = "org.ofbiz.order.order.OrderReturnServices",
        invoke = "getReturnItemInitialCost",
        description = "Gets the item's initial cost based on the inventory item record associated with the order item or 0.00 if none found.",
        attributes = {
            @Attribute(name = "returnId", type = "String", mode = "IN"),
            @Attribute(name = "returnItemSeqId", type = "String", mode = "IN"),
            @Attribute(name = "initialItemCost", type = "BigDecimal", mode = "OUT")
        }
    )
    public interface GetReturnItemInitialCost {}

    /**
     * Checks if all items on a return are complete/cancelled and updates the header status
     */
    @Service(
        name = "checkReturnComplete",
        engine = "java",
        location = "org.ofbiz.order.order.OrderReturnServices",
        invoke = "checkReturnComplete",
        description = "Checks if all items on a return are complete/cancelled and updates the header status",
        attributes = {
            @Attribute(name = "returnId", type = "String", mode = "IN"),
            @Attribute(name = "statusId", type = "String", mode = "OUT", optional = "true")
        }
    )
    public interface CheckReturnComplete {}

    /**
     * Send a notification that a return has been accepted
     */
    @Service(
        name = "sendReturnAcceptNotification",
        engine = "java",
        location = "org.ofbiz.order.order.OrderReturnServices",
        invoke = "sendReturnAcceptNotification",
        description = "Send a notification that a return has been accepted",
        maxRetry = "3",
        attributes = {
            @Attribute(name = "returnId", type = "String", mode = "IN"),
            @Attribute(name = "communicationEventId", type = "String", mode = "OUT", optional = "true")
        }
    )
    public interface SendReturnAcceptNotification {}

    /**
     * Send a notification that a return has been completed
     */
    @Service(
        name = "sendReturnCompleteNotification",
        engine = "java",
        location = "org.ofbiz.order.order.OrderReturnServices",
        invoke = "sendReturnCompleteNotification",
        description = "Send a notification that a return has been completed",
        maxRetry = "3",
        attributes = {
            @Attribute(name = "returnId", type = "String", mode = "IN"),
            @Attribute(name = "communicationEventId", type = "String", mode = "OUT", optional = "true")
        }
    )
    public interface SendReturnCompleteNotification {}

    /**
     * Send a notification that a return has been cancelled
     */
    @Service(
        name = "sendReturnCancelNotification",
        engine = "java",
        location = "org.ofbiz.order.order.OrderReturnServices",
        invoke = "sendReturnCancelNotification",
        description = "Send a notification that a return has been cancelled",
        maxRetry = "3",
        attributes = {
            @Attribute(name = "returnId", type = "String", mode = "IN"),
            @Attribute(name = "communicationEventId", type = "String", mode = "OUT", optional = "true")
        }
    )
    public interface SendReturnCancelNotification {}

    /**
     * Automatic cancellation of replacement order if return is not received within 30 days
     */
    @Service(
        name = "autoCancelReplacementOrders",
        engine = "java",
        location = "org.ofbiz.order.order.OrderReturnServices",
        invoke = "autoCancelReplacementOrders",
        description = "Automatic cancellation of replacement order if return is not received within 30 days",
        transactionTimeout = "36000",
        maxRetry = "3"
    )
    public interface AutoCancelReplacementOrders {}

    /**
     * Process the credits in a return
     */
    @Service(
        name = "processCreditReturn",
        engine = "java",
        location = "org.ofbiz.order.order.OrderReturnServices",
        invoke = "processCreditReturn",
        description = "Process the credits in a return",
        auth = "true",
        attributes = {
            @Attribute(name = "returnId", type = "String", mode = "IN")
        }
    )
    public interface ProcessCreditReturn {}

    /**
     * Process the refunds in a return
     */
    @Service(
        name = "processRefundReturn",
        engine = "java",
        location = "org.ofbiz.order.order.OrderReturnServices",
        invoke = "processRefundReturn",
        description = "Process the refunds in a return",
        auth = "true",
        attributes = {
            @Attribute(name = "returnId", type = "String", mode = "IN"),
            @Attribute(name = "returnTypeId", type = "String", mode = "IN")
        }
    )
    public interface ProcessRefundReturn {}

    /**
     * Process the replacements in a return
     */
    @Service(
        name = "processReplacementReturn",
        engine = "java",
        location = "org.ofbiz.order.order.OrderReturnServices",
        invoke = "processReplacementReturn",
        description = "Process the replacements in a return",
        auth = "true",
        attributes = {
            @Attribute(name = "returnId", type = "String", mode = "IN"),
            @Attribute(name = "returnTypeId", type = "String", mode = "IN")
        }
    )
    public interface ProcessReplacementReturn {}

    /**
     * Process the replacements in a wait return
     */
    @Service(
        name = "processWaitReplacementReturn",
        engine = "simple",
        location = "component://order/script/org/ofbiz/order/order/OrderReturnServices.xml",
        invoke = "processWaitReplacementReturn",
        description = "Process the replacements in a wait return",
        auth = "true",
        attributes = {
            @Attribute(name = "returnId", type = "String", mode = "IN")
        }
    )
    public interface ProcessWaitReplacementReturn {}

    /**
     * Process the replacements in a wait reserved return when the return is accepted and then received
     */
    @Service(
        name = "processWaitReplacementReservedReturn",
        engine = "simple",
        location = "component://order/script/org/ofbiz/order/order/OrderReturnServices.xml",
        invoke = "processWaitReplacementReservedReturn",
        description = "Process the replacements in a wait reserved return when the return is accepted and then received",
        auth = "true",
        attributes = {
            @Attribute(name = "returnId", type = "String", mode = "IN")
        }
    )
    public interface ProcessWaitReplacementReservedReturn {}

    /**
     * Process the replacements in a cross-ship return
     */
    @Service(
        name = "processCrossShipReplacementReturn",
        engine = "simple",
        location = "component://order/script/org/ofbiz/order/order/OrderReturnServices.xml",
        invoke = "processCrossShipReplacementReturn",
        description = "Process the replacements in a cross-ship return",
        auth = "true",
        attributes = {
            @Attribute(name = "returnId", type = "String", mode = "IN")
        }
    )
    public interface ProcessCrossShipReplacementReturn {}

    /**
     * Process the replacements in a repair return
     */
    @Service(
        name = "processRepairReplacementReturn",
        engine = "simple",
        location = "component://order/script/org/ofbiz/order/order/OrderReturnServices.xml",
        invoke = "processRepairReplacementReturn",
        description = "Process the replacements in a repair return",
        auth = "true",
        attributes = {
            @Attribute(name = "returnId", type = "String", mode = "IN")
        }
    )
    public interface ProcessRepairReplacementReturn {}

    /**
     * Process the replacements in a Immediate Return
     */
    @Service(
        name = "processReplaceImmediatelyReturn",
        engine = "simple",
        location = "component://order/script/org/ofbiz/order/order/OrderReturnServices.xml",
        invoke = "processReplaceImmediatelyReturn",
        description = "Process the replacements in a Immediate Return",
        auth = "true",
        attributes = {
            @Attribute(name = "returnId", type = "String", mode = "IN")
        }
    )
    public interface ProcessReplaceImmediatelyReturn {}

    /**
     * Process the Refund in a return
     */
    @Service(
        name = "processRefundOnlyReturn",
        engine = "simple",
        location = "component://order/script/org/ofbiz/order/order/OrderReturnServices.xml",
        invoke = "processRefundOnlyReturn",
        description = "Process the Refund in a return",
        auth = "true",
        attributes = {
            @Attribute(name = "returnId", type = "String", mode = "IN")
        }
    )
    public interface ProcessRefundOnlyReturn {}

    /**
     * Process the Immediate Refund in a return
     */
    @Service(
        name = "processRefundImmediatelyReturn",
        engine = "simple",
        location = "component://order/script/org/ofbiz/order/order/OrderReturnServices.xml",
        invoke = "processRefundImmediatelyReturn",
        description = "Process the Immediate Refund in a return",
        auth = "true",
        attributes = {
            @Attribute(name = "returnId", type = "String", mode = "IN")
        }
    )
    public interface ProcessRefundImmediatelyReturn {}

    /**
     * Process subscription changes from a return
     */
    @Service(
        name = "processSubscriptionReturn",
        engine = "java",
        location = "org.ofbiz.order.order.OrderReturnServices",
        invoke = "processSubscriptionReturn",
        description = "Process subscription changes from a return",
        auth = "true",
        attributes = {
            @Attribute(name = "returnId", type = "String", mode = "IN")
        }
    )
    public interface ProcessSubscriptionReturn {}

    /**
     * Process the refund return for replacement order
     */
    @Service(
        name = "processRefundReturnForReplacement",
        engine = "java",
        location = "org.ofbiz.order.order.OrderReturnServices",
        invoke = "processRefundReturnForReplacement",
        description = "Process the refund return for replacement order",
        auth = "true",
        attributes = {
            @Attribute(name = "orderId", type = "String", mode = "IN")
        }
    )
    public interface ProcessRefundReturnForReplacement {}

    /**
     * Update return/item status when items have been received
     */
    @Service(
        name = "updateReturnStatusFromReceipt",
        engine = "simple",
        location = "component://order/script/org/ofbiz/order/order/OrderReturnServices.xml",
        invoke = "updateReturnStatusFromReceipt",
        description = "Update return/item status when items have been received",
        attributes = {
            @Attribute(name = "returnId", type = "String", mode = "IN"),
            @Attribute(name = "returnHeaderStatus", type = "String", mode = "OUT")
        },
        permissionService = @PermissionService(service = "returnPermissionCheck", mainAction = "UPDATE")
    )
    public interface UpdateReturnStatusFromReceipt {}

    /**
     * Get the quantity allowed for an item to be returned
     */
    @Service(
        name = "getReturnableQuantity",
        engine = "java",
        location = "org.ofbiz.order.order.OrderReturnServices",
        invoke = "getReturnableQuantity",
        description = "Get the quantity allowed for an item to be returned",
        attributes = {
            @Attribute(name = "orderItem", type = "org.ofbiz.entity.GenericValue", mode = "IN"),
            @Attribute(name = "returnableQuantity", type = "BigDecimal", mode = "OUT"),
            @Attribute(name = "returnablePrice", type = "BigDecimal", mode = "OUT", optional = "true")
        }
    )
    public interface GetReturnableQuantity {}

    /**
     * Get a map of returnable items orderItem => quantity available to return
     */
    @Service(
        name = "getReturnableItems",
        engine = "java",
        location = "org.ofbiz.order.order.OrderReturnServices",
        invoke = "getReturnableItems",
        description = "Get a map of returnable items orderItem => quantity available to return",
        attributes = {
            @Attribute(name = "orderId", type = "String", mode = "IN"),
            @Attribute(name = "returnableItems", type = "Map", mode = "OUT")
        }
    )
    public interface GetReturnableItems {}

    /**
     * Get the total amount of all returns for an order: orderTotal, returnTotal - totals so far.  availableReturnTotal = orderTotal - returnTotal - adjustment.  Used for checking if the return total has gone over the order total.  If countNewReturnItems is set to Boolean.TRUE then return items in the CREATED state will be counted.  This should only be the case during quickRefundEntireOrder.
     */
    @Service(
        name = "getOrderAvailableReturnedTotal",
        engine = "java",
        location = "org.ofbiz.order.order.OrderReturnServices",
        invoke = "getOrderAvailableReturnedTotal",
        description = "Get the total amount of all returns for an order: orderTotal, returnTotal - totals so far.  availableReturnTotal = orderTotal - returnTotal - adjustment.  Used for checking if the return total has gone over the order total.  If countNewReturnItems is set to Boolean.TRUE then return items in the CREATED state will be counted.  This should only be the case during quickRefundEntireOrder.",
        attributes = {
            @Attribute(name = "orderId", type = "String", mode = "IN"),
            @Attribute(name = "adjustment", type = "BigDecimal", mode = "IN", optional = "true"),
            @Attribute(name = "countNewReturnItems", type = "Boolean", mode = "IN", optional = "true"),
            @Attribute(name = "orderTotal", type = "BigDecimal", mode = "OUT"),
            @Attribute(name = "returnTotal", type = "BigDecimal", mode = "OUT"),
            @Attribute(name = "availableReturnTotal", type = "BigDecimal", mode = "OUT")
        }
    )
    public interface GetOrderAvailableReturnedTotal {}

    /**
     * Refunds A Billing Account Payment
     */
    @Service(
        name = "refundBillingAccountPayment",
        engine = "java",
        location = "org.ofbiz.order.order.OrderReturnServices",
        invoke = "refundBillingAccountPayment",
        description = "Refunds A Billing Account Payment",
        auth = "true",
        attributes = {
            @Attribute(name = "orderPaymentPreference", type = "org.ofbiz.entity.GenericValue", mode = "IN"),
            @Attribute(name = "refundAmount", type = "BigDecimal", mode = "IN"),
            @Attribute(name = "paymentId", type = "String", mode = "OUT")
        }
    )
    public interface RefundBillingAccountPayment {}

    /**
     * Create a new ReturnItemShipment
     */
    @Service(
        name = "createReturnItemShipment",
        engine = "simple",
        location = "component://order/script/org/ofbiz/order/order/OrderReturnServices.xml",
        invoke = "createReturnItemShipment",
        description = "Create a new ReturnItemShipment",
        entityAttributes = {
            @EntityAttributes(entityName = "ReturnItemShipment", mode = "IN")
        },
        permissionService = @PermissionService(service = "returnPermissionCheck", mainAction = "CREATE")
    )
    public interface CreateReturnItemShipment {}

    /**
     * Get the return status associated with customer/vendor return
     */
    @Service(
        name = "getStatusItemsForReturn",
        engine = "simple",
        location = "component://order/script/org/ofbiz/order/order/OrderReturnServices.xml",
        invoke = "getStatusItemsForReturn",
        description = "Get the return status associated with customer/vendor return",
        attributes = {
            @Attribute(name = "returnHeaderTypeId", type = "String", mode = "IN"),
            @Attribute(name = "statusItems", type = "List", mode = "OUT")
        }
    )
    public interface GetStatusItemsForReturn {}

    /**
     * Associate exchange order with original order in OrderItemAssoc entity
     */
    @Service(
        name = "createExchangeOrderAssoc",
        engine = "simple",
        location = "component://order/script/org/ofbiz/order/order/OrderReturnServices.xml",
        invoke = "createExchangeOrderAssoc",
        description = "Associate exchange order with original order in OrderItemAssoc entity",
        attributes = {
            @Attribute(name = "orderId", type = "String", mode = "IN"),
            @Attribute(name = "originOrderId", type = "String", mode = "IN")
        }
    )
    public interface CreateExchangeOrderAssoc {}

    /**
     * Add product(s) back to category if it has no active category
     */
    @Service(
        name = "addProductsBackToCategory",
        engine = "simple",
        location = "component://order/script/org/ofbiz/order/order/OrderReturnServices.xml",
        invoke = "addProductsBackToCategory",
        description = "Add product(s) back to category if it has no active category",
        attributes = {
            @Attribute(name = "returnId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "inventoryItemId", type = "String", mode = "IN", optional = "true")
        }
    )
    public interface AddProductsBackToCategory {}

    /**
     * Create Return Status
     */
    @Service(
        name = "createReturnStatus",
        engine = "simple",
        location = "component://order/script/org/ofbiz/order/order/OrderReturnServices.xml",
        invoke = "createReturnStatus",
        description = "Create Return Status",
        attributes = {
            @Attribute(name = "returnId", type = "String", mode = "IN"),
            @Attribute(name = "returnItemSeqId", type = "String", mode = "IN", optional = "true")
        }
    )
    public interface CreateReturnStatus {}

    /**
     * Create a ReturnContactMech
     */
    @Service(
        name = "createReturnContactMech",
        engine = "entity-auto",
        invoke = "create",
        description = "Create a ReturnContactMech",
        defaultEntityName = "ReturnContactMech",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface CreateReturnContactMech {}

    /**
     * Update Return Contact Mech
     */
    @Service(
        name = "updateReturnContactMech",
        engine = "simple",
        location = "component://order/script/org/ofbiz/order/order/OrderReturnServices.xml",
        invoke = "updateReturnContactMech",
        description = "Update Return Contact Mech",
        defaultEntityName = "ReturnContactMech",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        },
        attributes = {
            @Attribute(name = "oldContactMechId", type = "String", mode = "IN", optional = "true")
        },
        permissionService = @PermissionService(service = "returnPermissionCheck", mainAction = "UPDATE")
    )
    public interface UpdateReturnContactMech {}

    /**
     * Delete a ReturnContactMech
     */
    @Service(
        name = "deleteReturnContactMech",
        engine = "entity-auto",
        invoke = "delete",
        description = "Delete a ReturnContactMech",
        defaultEntityName = "ReturnContactMech",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        }
    )
    public interface DeleteReturnContactMech {}

    /**
     * Create the return item for rental (which items has product type is ASSET_USAGE_OUT_IN)
     */
    @Service(
        name = "createReturnItemForRental",
        engine = "simple",
        location = "component://order/script/org/ofbiz/order/order/OrderReturnServices.xml",
        invoke = "createReturnItemForRental",
        description = "Create the return item for rental (which items has product type is ASSET_USAGE_OUT_IN)",
        attributes = {
            @Attribute(name = "orderId", type = "String", mode = "IN"),
            @Attribute(name = "returnId", type = "String", mode = "OUT")
        }
    )
    public interface CreateReturnItemForRental {}

    /**
     * Create a ReturnAdjustmentType record
     */
    @Service(
        name = "createReturnAdjustmentType",
        engine = "entity-auto",
        invoke = "create",
        description = "Create a ReturnAdjustmentType record",
        defaultEntityName = "ReturnAdjustmentType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "INOUT", include = "pk", optional = "true"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface CreateReturnAdjustmentType {}

    /**
     * Update a ReturnAdjustmentType record
     */
    @Service(
        name = "updateReturnAdjustmentType",
        engine = "entity-auto",
        invoke = "update",
        description = "Update a ReturnAdjustmentType record",
        defaultEntityName = "ReturnAdjustmentType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface UpdateReturnAdjustmentType {}

    /**
     * Delete a ReturnAdjustmentType record
     */
    @Service(
        name = "deleteReturnAdjustmentType",
        engine = "entity-auto",
        invoke = "delete",
        description = "Delete a ReturnAdjustmentType record",
        defaultEntityName = "ReturnAdjustmentType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        }
    )
    public interface DeleteReturnAdjustmentType {}

    /**
     * Create a ReturnHeaderType record
     */
    @Service(
        name = "createReturnHeaderType",
        engine = "entity-auto",
        invoke = "create",
        description = "Create a ReturnHeaderType record",
        defaultEntityName = "ReturnHeaderType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "INOUT", include = "pk", optional = "true"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface CreateReturnHeaderType {}

    /**
     * Update a ReturnHeaderType record
     */
    @Service(
        name = "updateReturnHeaderType",
        engine = "entity-auto",
        invoke = "update",
        description = "Update a ReturnHeaderType record",
        defaultEntityName = "ReturnHeaderType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface UpdateReturnHeaderType {}

    /**
     * Delete a ReturnHeaderType record
     */
    @Service(
        name = "deleteReturnHeaderType",
        engine = "entity-auto",
        invoke = "delete",
        description = "Delete a ReturnHeaderType record",
        defaultEntityName = "ReturnHeaderType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        }
    )
    public interface DeleteReturnHeaderType {}

    /**
     * Create a ReturnItemType record
     */
    @Service(
        name = "createReturnItemType",
        engine = "entity-auto",
        invoke = "create",
        description = "Create a ReturnItemType record",
        defaultEntityName = "ReturnItemType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "INOUT", include = "pk", optional = "true"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface CreateReturnItemType {}

    /**
     * Update a ReturnItemType record
     */
    @Service(
        name = "updateReturnItemType",
        engine = "entity-auto",
        invoke = "update",
        description = "Update a ReturnItemType record",
        defaultEntityName = "ReturnItemType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface UpdateReturnItemType {}

    /**
     * Delete a ReturnItemType record
     */
    @Service(
        name = "deleteReturnItemType",
        engine = "entity-auto",
        invoke = "delete",
        description = "Delete a ReturnItemType record",
        defaultEntityName = "ReturnItemType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        }
    )
    public interface DeleteReturnItemType {}

    /**
     * Create a ReturnItemTypeMap record
     */
    @Service(
        name = "createReturnItemTypeMap",
        engine = "entity-auto",
        invoke = "create",
        description = "Create a ReturnItemTypeMap record",
        defaultEntityName = "ReturnItemTypeMap",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface CreateReturnItemTypeMap {}

    /**
     * Update a ReturnItemTypeMap record
     */
    @Service(
        name = "updateReturnItemTypeMap",
        engine = "entity-auto",
        invoke = "update",
        description = "Update a ReturnItemTypeMap record",
        defaultEntityName = "ReturnItemTypeMap",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface UpdateReturnItemTypeMap {}

    /**
     * Delete a ReturnItemTypeMap record
     */
    @Service(
        name = "deleteReturnItemTypeMap",
        engine = "entity-auto",
        invoke = "delete",
        description = "Delete a ReturnItemTypeMap record",
        defaultEntityName = "ReturnItemTypeMap",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        }
    )
    public interface DeleteReturnItemTypeMap {}

    /**
     * Create a ReturnReason record
     */
    @Service(
        name = "createReturnReason",
        engine = "entity-auto",
        invoke = "create",
        description = "Create a ReturnReason record",
        defaultEntityName = "ReturnReason",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "INOUT", include = "pk", optional = "true"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface CreateReturnReason {}

    /**
     * Update a ReturnReason record
     */
    @Service(
        name = "updateReturnReason",
        engine = "entity-auto",
        invoke = "update",
        description = "Update a ReturnReason record",
        defaultEntityName = "ReturnReason",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface UpdateReturnReason {}

    /**
     * Delete a ReturnReason record
     */
    @Service(
        name = "deleteReturnReason",
        engine = "entity-auto",
        invoke = "delete",
        description = "Delete a ReturnReason record",
        defaultEntityName = "ReturnReason",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        }
    )
    public interface DeleteReturnReason {}

    /**
     * Create a ReturnType record
     */
    @Service(
        name = "createReturnType",
        engine = "entity-auto",
        invoke = "create",
        description = "Create a ReturnType record",
        defaultEntityName = "ReturnType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "INOUT", include = "pk", optional = "true"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface CreateReturnType {}

    /**
     * Update a ReturnType record
     */
    @Service(
        name = "updateReturnType",
        engine = "entity-auto",
        invoke = "update",
        description = "Update a ReturnType record",
        defaultEntityName = "ReturnType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface UpdateReturnType {}

    /**
     * Delete a ReturnType record
     */
    @Service(
        name = "deleteReturnType",
        engine = "entity-auto",
        invoke = "delete",
        description = "Delete a ReturnType record",
        defaultEntityName = "ReturnType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        }
    )
    public interface DeleteReturnType {}

}
