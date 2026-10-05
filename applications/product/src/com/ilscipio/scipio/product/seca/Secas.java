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
public class Secas {

    /**
     * SECA for service updateInventoryItem on event commit.
     */
    @Seca(
        service = "updateInventoryItem",
        event = "commit",
        condition = "!empty(statusId) && oldStatusId != statusId",
        actions = {
            @SecaAction(
                service = "createInventoryItemStatus",
                mode = "sync"
            )
        }
    )
    public interface UpdateInventoryItemcommitSeca1 {}

    /**
     * SECA for service updateInventoryItem on event commit.
     */
    @Seca(
        service = "updateInventoryItem",
        event = "commit",
        condition = "!empty(statusId) && !empty(productId) && oldProductId != productId",
        actions = {
            @SecaAction(
                service = "createInventoryItemStatus",
                mode = "sync"
            )
        }
    )
    public interface UpdateInventoryItemcommitSeca2 {}

    /**
     * SECA for service updateInventoryItem on event commit.
     */
    @Seca(
        service = "updateInventoryItem",
        event = "commit",
        condition = "!empty(statusId) && !empty(ownerPartyId) && oldOwnerPartyId != ownerPartyId",
        actions = {
            @SecaAction(
                service = "createInventoryItemStatus",
                mode = "sync"
            )
        }
    )
    public interface UpdateInventoryItemcommitSeca3 {}

    /**
     * SECA for service createItemIssuance on event return.
     */
    @Seca(
        service = "createItemIssuance",
        event = "return",
        condition = "!empty(orderId) && !empty(inventoryItemId)",
        actions = {
            @SecaAction(
                service = "changeOwnerUponIssuance",
                mode = "sync"
            )
        }
    )
    public interface CreateItemIssuancereturnSeca4 {}

    /**
     * SECA for service createInventoryTransfer on event invoke.
     */
    @Seca(
        service = "createInventoryTransfer",
        event = "invoke",
        condition = "statusId != 'IXF_CANCELLED'",
        actions = {
            @SecaAction(
                service = "prepareInventoryTransfer",
                mode = "sync"
            )
        }
    )
    public interface CreateInventoryTransferinvokeSeca5 {}

    /**
     * SECA for service createInventoryTransfer on event commit.
     */
    @Seca(
        service = "createInventoryTransfer",
        event = "commit",
        condition = "statusId == 'IXF_COMPLETE'",
        actions = {
            @SecaAction(
                service = "completeInventoryTransfer",
                mode = "sync"
            ),
            @SecaAction(
                service = "balanceInventoryItems",
                mode = "sync"
            )
        }
    )
    public interface CreateInventoryTransfercommitSeca6 {}

    /**
     * SECA for service updateInventoryTransfer on event invoke.
     */
    @Seca(
        service = "updateInventoryTransfer",
        event = "invoke",
        condition = "statusId == 'IXF_CANCELLED'",
        actions = {
            @SecaAction(
                service = "cancelInventoryTransfer",
                mode = "sync"
            )
        }
    )
    public interface UpdateInventoryTransferinvokeSeca7 {}

    /**
     * SECA for service updateInventoryTransfer on event commit.
     */
    @Seca(
        service = "updateInventoryTransfer",
        event = "commit",
        condition = "statusId == 'IXF_COMPLETE'",
        actions = {
            @SecaAction(
                service = "completeInventoryTransfer",
                mode = "sync"
            ),
            @SecaAction(
                service = "balanceInventoryItems",
                mode = "sync"
            )
        }
    )
    public interface UpdateInventoryTransfercommitSeca8 {}

    /**
     * SECA for service createPhysicalInventoryAndVariance on event commit.
     */
    @Seca(
        service = "createPhysicalInventoryAndVariance",
        event = "commit",
        actions = {
            @SecaAction(
                service = "balanceInventoryItems",
                mode = "sync"
            )
        }
    )
    public interface CreatePhysicalInventoryAndVariancecommitSeca9 {}

    /**
     * SECA for service balanceInventoryItems on event commit.
     */
    @Seca(
        service = "balanceInventoryItems",
        event = "commit",
        actions = {
            @SecaAction(
                service = "updateProductIfAvailableFromShipment",
                mode = "sync"
            )
        }
    )
    public interface BalanceInventoryItemscommitSeca10 {}

    /**
     * SECA for service createProductPrice on event commit.
     */
    @Seca(
        service = "createProductPrice",
        event = "commit",
        actions = {
            @SecaAction(
                service = "saveProductPriceChange",
                mode = "sync"
            )
        }
    )
    public interface CreateProductPricecommitSeca11 {}

    /**
     * SECA for service updateProductPrice on event commit.
     */
    @Seca(
        service = "updateProductPrice",
        event = "commit",
        condition = "price != oldPrice",
        actions = {
            @SecaAction(
                service = "saveProductPriceChange",
                mode = "sync"
            )
        }
    )
    public interface UpdateProductPricecommitSeca12 {}

    /**
     * SECA for service deleteProductPrice on event commit.
     */
    @Seca(
        service = "deleteProductPrice",
        event = "commit",
        actions = {
            @SecaAction(
                service = "saveProductPriceChange",
                mode = "sync"
            )
        }
    )
    public interface DeleteProductPricecommitSeca13 {}

    /**
     * SECA for service addPartyToCategory on event invoke.
     */
    @Seca(
        service = "addPartyToCategory",
        event = "invoke",
        condition = "roleTypeId == '_NA_'",
        actions = {
            @SecaAction(
                service = "ensureNaPartyRole",
                mode = "sync"
            )
        }
    )
    public interface AddPartyToCategoryinvokeSeca14 {}

    /**
     * SECA for service addPartyToFacility on event invoke.
     */
    @Seca(
        service = "addPartyToFacility",
        event = "invoke",
        condition = "roleTypeId == '_NA_'",
        actions = {
            @SecaAction(
                service = "ensureNaPartyRole",
                mode = "sync"
            )
        }
    )
    public interface AddPartyToFacilityinvokeSeca15 {}

    /**
     * SECA for service addPartyToFacilityGroup on event invoke.
     */
    @Seca(
        service = "addPartyToFacilityGroup",
        event = "invoke",
        condition = "roleTypeId == '_NA_'",
        actions = {
            @SecaAction(
                service = "ensureNaPartyRole",
                mode = "sync"
            )
        }
    )
    public interface AddPartyToFacilityGroupinvokeSeca16 {}

    /**
     * SECA for service addProdCatalogToParty on event invoke.
     */
    @Seca(
        service = "addProdCatalogToParty",
        event = "invoke",
        condition = "roleTypeId == '_NA_'",
        actions = {
            @SecaAction(
                service = "ensureNaPartyRole",
                mode = "sync"
            )
        }
    )
    public interface AddProdCatalogToPartyinvokeSeca17 {}

    /**
     * SECA for service createProductContent on event in-validate.
     */
    @Seca(
        service = "createProductContent",
        event = "in-validate",
        condition = "empty(contentId)",
        actions = {
            @SecaAction(
                service = "createContent",
                mode = "sync"
            )
        }
    )
    public interface CreateProductContentinvalidateSeca18 {}

    /**
     * SECA for service createPicklistFromOrders on event in-validate.
     */
    @Seca(
        service = "createPicklistFromOrders",
        event = "in-validate",
        condition = "empty(orderHeaderList) && !empty(orderIdList)",
        actions = {
            @SecaAction(
                service = "convertPickOrderIdListToHeaders",
                mode = "sync"
            )
        }
    )
    public interface CreatePicklistFromOrdersinvalidateSeca19 {}

    /**
     * SECA for service receiveInventoryProduct on event commit.
     */
    @Seca(
        service = "receiveInventoryProduct",
        event = "commit",
        condition = "!empty(facilityId) && !empty(orderId)",
        actions = {
            @SecaAction(
                service = "updateIssuanceShipmentAndPoOnReceiveInventory",
                mode = "sync"
            )
        }
    )
    public interface ReceiveInventoryProductcommitSeca20 {}

    /**
     * SECA for service receiveInventoryProduct on event commit.
     */
    @Seca(
        service = "receiveInventoryProduct",
        event = "commit",
        condition = "!empty(inventoryItemId)",
        actions = {
            @SecaAction(
                service = "updateProductAverageCostOnReceiveInventory",
                mode = "sync"
            )
        }
    )
    public interface ReceiveInventoryProductcommitSeca21 {}

    /**
     * SECA for service deletePartyRole on event commit.
     */
    @Seca(
        service = "deletePartyRole",
        event = "commit",
        condition = "roleTypeId == 'IMAGEAPPROVER'",
        actions = {
            @SecaAction(
                service = "removeImageContentApproval",
                mode = "sync"
            )
        }
    )
    public interface DeletePartyRolecommitSeca22 {}

    /**
     * SECA for service updateProductStoreGroup on event commit.
     */
    @Seca(
        service = "updateProductStoreGroup",
        event = "commit",
        condition = "primaryParentGroupId != null",
        actions = {
            @SecaAction(
                service = "checkProductStoreGroupRollup",
                mode = "sync"
            )
        }
    )
    public interface UpdateProductStoreGroupcommitSeca23 {}

    /**
     * SECA for service createProductStoreGroupRollup on event commit.
     */
    @Seca(
        service = "createProductStoreGroupRollup",
        event = "commit",
        actions = {
            @SecaAction(
                service = "checkProductStoreGroupRollup",
                mode = "sync"
            )
        }
    )
    public interface CreateProductStoreGroupRollupcommitSeca24 {}

}
