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
package com.ilscipio.scipio.product.seca;

import com.ilscipio.scipio.service.def.seca.*;

/**
 * Auto-generated annotation-based service ECA definitions.
 *
 * <p>Generated from secas.xml by convertXmlToAnnotation Gradle task.</p>
 *
 * <p>SCIPIO: 4.0.0: Auto-generated.</p>
 */
public class ShipmentSecas {

    /**
     * SECA for service updateShipment on event commit.
     */
    @Seca(
        service = "updateShipment",
        event = "commit",
        condition = "statusId != oldStatusId && shipmentTypeId == 'SALES_SHIPMENT' && statusId == 'SHIPMENT_CANCELLED'",
        actions = {
            @SecaAction(
                service = "checkCancelItemIssuanceAndOrderShipmentFromShipment",
                mode = "sync"
            )
        }
    )
    public interface UpdateShipmentcommitSeca1 {}

    /**
     * SECA for service updateShipment on event commit.
     */
    @Seca(
        service = "updateShipment",
        event = "commit",
        condition = "statusId != oldStatusId && statusId == 'SHIPMENT_PICKED' && shipmentTypeId == 'SALES_SHIPMENT'",
        actions = {
            @SecaAction(
                service = "createInvoicesFromShipment",
                mode = "sync",
                runAsUser = "system"
            )
        }
    )
    public interface UpdateShipmentcommitSeca2 {}

    /**
     * SECA for service updateShipment on event commit.
     */
    @Seca(
        service = "updateShipment",
        event = "commit",
        condition = "statusId != oldStatusId && statusId == 'SHIPMENT_PACKED' && shipmentTypeId == 'SALES_SHIPMENT'",
        actions = {
            @SecaAction(
                service = "createInvoicesFromShipment",
                mode = "sync",
                runAsUser = "system"
            ),
            @SecaAction(
                service = "setInvoicesToReadyFromShipment",
                mode = "sync",
                runAsUser = "system"
            )
        }
    )
    public interface UpdateShipmentcommitSeca3 {}

    /**
     * SECA for service updateShipment on event commit.
     */
    @Seca(
        service = "updateShipment",
        event = "commit",
        condition = "statusId != oldStatusId && (statusId == 'SHIPMENT_SHIPPED' || statusId == 'SHIPMENT_DELIVERED') && shipmentTypeId == 'SALES_SHIPMENT'",
        actions = {
            @SecaAction(
                service = "sendShipmentCompleteNotification",
                mode = "async",
                persist = "true"
            )
        }
    )
    public interface UpdateShipmentcommitSeca4 {}

    /**
     * SECA for service updateShipment on event commit.
     */
    @Seca(
        service = "updateShipment",
        event = "commit",
        condition = "statusId != oldStatusId && statusId == 'PURCH_SHIP_RECEIVED' && shipmentTypeId == 'PURCHASE_SHIPMENT'",
        actions = {
            @SecaAction(
                service = "balanceItemIssuancesForShipment",
                mode = "sync"
            ),
            @SecaAction(
                service = "createInvoicesFromShipment",
                mode = "sync",
                runAsUser = "system"
            )
        }
    )
    public interface UpdateShipmentcommitSeca5 {}

    /**
     * SECA for service updateShipment on event commit.
     */
    @Seca(
        service = "updateShipment",
        event = "commit",
        condition = "statusId != oldStatusId && statusId == 'PURCH_SHIP_SHIPPED' && shipmentTypeId == 'DROP_SHIPMENT'",
        actions = {
            @SecaAction(
                service = "createInvoicesFromShipment",
                mode = "sync",
                runAsUser = "system"
            )
        }
    )
    public interface UpdateShipmentcommitSeca6 {}

    /**
     * SECA for service updateShipment on event commit.
     */
    @Seca(
        service = "updateShipment",
        event = "commit",
        condition = "statusId != oldStatusId && statusId == 'PURCH_SHIP_RECEIVED' && shipmentTypeId == 'DROP_SHIPMENT'",
        actions = {
            @SecaAction(
                service = "createSalesInvoicesFromDropShipment",
                mode = "sync",
                runAsUser = "system"
            )
        }
    )
    public interface UpdateShipmentcommitSeca7 {}

    /**
     * SECA for service updateShipment on event commit.
     */
    @Seca(
        service = "updateShipment",
        event = "commit",
        condition = "statusId != oldStatusId && statusId == 'PURCH_SHIP_RECEIVED' && shipmentTypeId == 'SALES_RETURN'",
        actions = {
            @SecaAction(
                service = "createInvoicesFromReturnShipment",
                mode = "sync",
                runAsUser = "system"
            )
        }
    )
    public interface UpdateShipmentcommitSeca8 {}

    /**
     * SECA for service updateShipment on event commit.
     */
    @Seca(
        service = "updateShipment",
        event = "commit",
        condition = "statusId != oldStatusId && statusId == 'SHIPMENT_SHIPPED' && shipmentTypeId == 'PURCHASE_RETURN'",
        actions = {
            @SecaAction(
                service = "createInvoicesFromReturnShipment",
                mode = "sync",
                runAsUser = "system"
            )
        }
    )
    public interface UpdateShipmentcommitSeca9 {}

    /**
     * SECA for service createShipment on event commit.
     */
    @Seca(
        service = "createShipment",
        event = "commit",
        condition = "statusId == 'SHIPMENT_SCHEDULED'",
        actions = {
            @SecaAction(
                service = "sendShipmentScheduledNotification",
                mode = "async"
            )
        }
    )
    public interface CreateShipmentcommitSeca10 {}

    /**
     * SECA for service updateShipment on event commit.
     */
    @Seca(
        service = "updateShipment",
        event = "commit",
        condition = "statusId != oldStatusId && statusId == 'SHIPMENT_SCHEDULED'",
        actions = {
            @SecaAction(
                service = "sendShipmentScheduledNotification",
                mode = "async"
            )
        }
    )
    public interface UpdateShipmentcommitSeca11 {}

    /**
     * SECA for service createShipment on event commit.
     */
    @Seca(
        service = "createShipment",
        event = "commit",
        condition = "!empty(originFacilityId)",
        actions = {
            @SecaAction(
                service = "setShipmentSettingsFromFacilities",
                mode = "sync"
            )
        }
    )
    public interface CreateShipmentcommitSeca12 {}

    /**
     * SECA for service createShipment on event commit.
     */
    @Seca(
        service = "createShipment",
        event = "commit",
        condition = "!empty(destinationFacilityId)",
        actions = {
            @SecaAction(
                service = "setShipmentSettingsFromFacilities",
                mode = "sync"
            )
        }
    )
    public interface CreateShipmentcommitSeca13 {}

    /**
     * SECA for service updateShipment on event commit.
     */
    @Seca(
        service = "updateShipment",
        event = "commit",
        condition = "originFacilityId != oldOriginFacilityId && !empty(originFacilityId)",
        actions = {
            @SecaAction(
                service = "setShipmentSettingsFromFacilities",
                mode = "sync"
            )
        }
    )
    public interface UpdateShipmentcommitSeca14 {}

    /**
     * SECA for service updateShipment on event commit.
     */
    @Seca(
        service = "updateShipment",
        event = "commit",
        condition = "destinationFacilityId != oldDestinationFacilityId && !empty(destinationFacilityId)",
        actions = {
            @SecaAction(
                service = "setShipmentSettingsFromFacilities",
                mode = "sync"
            )
        }
    )
    public interface UpdateShipmentcommitSeca15 {}

    /**
     * SECA for service createShipment on event commit.
     */
    @Seca(
        service = "createShipment",
        event = "commit",
        condition = "!empty(primaryOrderId)",
        actions = {
            @SecaAction(
                service = "setShipmentSettingsFromPrimaryOrder",
                mode = "sync"
            )
        }
    )
    public interface CreateShipmentcommitSeca16 {}

    /**
     * SECA for service updateShipment on event commit.
     */
    @Seca(
        service = "updateShipment",
        event = "commit",
        condition = "primaryOrderId != oldPrimaryOrderId && !empty(primaryOrderId)",
        actions = {
            @SecaAction(
                service = "setShipmentSettingsFromPrimaryOrder",
                mode = "sync"
            )
        }
    )
    public interface UpdateShipmentcommitSeca17 {}

    /**
     * SECA for service createShipmentReceipt on event commit.
     */
    @Seca(
        service = "createShipmentReceipt",
        event = "commit",
        condition = "!empty(returnId)",
        actions = {
            @SecaAction(
                service = "checkDecomposeInventoryItem",
                mode = "sync"
            ),
            @SecaAction(
                service = "updateReturnStatusFromReceipt",
                mode = "sync",
                runAsUser = "system"
            )
        }
    )
    public interface CreateShipmentReceiptcommitSeca18 {}

    /**
     * SECA for service createShipmentReceipt on event commit.
     */
    @Seca(
        service = "createShipmentReceipt",
        event = "commit",
        condition = "!empty(orderId)",
        actions = {
            @SecaAction(
                service = "updateOrderStatusFromReceipt",
                mode = "sync",
                runAsUser = "system"
            )
        }
    )
    public interface CreateShipmentReceiptcommitSeca19 {}

    /**
     * SECA for service createShipmentReceipt on event commit.
     */
    @Seca(
        service = "createShipmentReceipt",
        event = "commit",
        condition = "!empty(shipmentId)",
        actions = {
            @SecaAction(
                service = "checkDecomposeInventoryItem",
                mode = "sync"
            ),
            @SecaAction(
                service = "updatePurchaseShipmentFromReceipt",
                mode = "sync"
            )
        }
    )
    public interface CreateShipmentReceiptcommitSeca20 {}

    /**
     * SECA for service createShipmentPackageContent on event in-validate.
     */
    @Seca(
        service = "createShipmentPackageContent",
        event = "in-validate",
        condition = "shipmentPackageSeqId == 'New'",
        actions = {
            @SecaAction(
                service = "createShipmentPackage",
                mode = "sync"
            )
        }
    )
    public interface CreateShipmentPackageContentinvalidateSeca21 {}

    /**
     * SECA for service updatePicklistItem on event commit.
     */
    @Seca(
        service = "updatePicklistItem",
        event = "commit",
        condition = "itemStatusId != oldItemStatusId",
        actions = {
            @SecaAction(
                service = "checkPicklistBinItemStatuses",
                mode = "sync"
            )
        }
    )
    public interface UpdatePicklistItemcommitSeca22 {}

    /**
     * SECA for service packBulkItems on event commit.
     */
    @Seca(
        service = "packBulkItems",
        event = "commit",
        condition = "nextPackageSeq == '0'",
        actions = {
            @SecaAction(
                service = "setNextPackageSeq",
                mode = "sync"
            )
        }
    )
    public interface PackBulkItemscommitSeca23 {}

}
