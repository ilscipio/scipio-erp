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
package com.ilscipio.scipio.product.eeca;

import com.ilscipio.scipio.service.def.eeca.*;

/**
 * Auto-generated annotation-based entity ECA definitions.
 *
 * <p>Generated from eecas.xml by convertXmlToAnnotation Gradle task.</p>
 *
 * <p>SCIPIO: 4.0.0: Auto-generated.</p>
 */
public class Eecas {

    /**
     * EECA for entity Product on create/return.
     */
    @Eeca(
        entity = "Product",
        operation = "create",
        event = "return",
        condition = "autoCreateKeywords != 'N'",
        actions = {
            @EecaAction(
                service = "indexProductKeywords",
                mode = "sync",
                valueAttr = "productInstance"
            )
        }
    )
    public interface ProductCreateReturnEeca1 {}

    /**
     * EECA for entity Product on store/return.
     */
    @Eeca(
        entity = "Product",
        operation = "store",
        event = "return",
        condition = "autoCreateKeywords != 'N'",
        actions = {
            @EecaAction(
                service = "indexProductKeywords",
                mode = "sync"
            )
        }
    )
    public interface ProductStoreReturnEeca2 {}

    /**
     * EECA for entity GoodIdentification on create-store/return.
     */
    @Eeca(
        entity = "GoodIdentification",
        operation = "create-store",
        event = "return",
        actions = {
            @EecaAction(
                service = "indexProductKeywords",
                mode = "sync"
            )
        }
    )
    public interface GoodIdentificationCreateStoreReturnEeca3 {}

    /**
     * EECA for entity ProductAttribute on create-store/return.
     */
    @Eeca(
        entity = "ProductAttribute",
        operation = "create-store",
        event = "return",
        actions = {
            @EecaAction(
                service = "indexProductKeywords",
                mode = "sync"
            )
        }
    )
    public interface ProductAttributeCreateStoreReturnEeca4 {}

    /**
     * EECA for entity ProductFeatureAppl on create-store/return.
     */
    @Eeca(
        entity = "ProductFeatureAppl",
        operation = "create-store",
        event = "return",
        actions = {
            @EecaAction(
                service = "indexProductKeywords",
                mode = "sync"
            )
        }
    )
    public interface ProductFeatureApplCreateStoreReturnEeca5 {}

    /**
     * EECA for entity ProductContent on create-store/return.
     */
    @Eeca(
        entity = "ProductContent",
        operation = "create-store",
        event = "return",
        actions = {
            @EecaAction(
                service = "indexProductKeywords",
                mode = "sync"
            )
        }
    )
    public interface ProductContentCreateStoreReturnEeca6 {}

    /**
     * EECA for entity InventoryItem on create-store/return.
     */
    @Eeca(
        entity = "InventoryItem",
        operation = "create-store",
        event = "return",
        condition = "!empty(productId) && !empty(availableToPromiseTotal) && availableToPromiseTotal <= 0",
        actions = {
            @EecaAction(
                service = "checkProductInventoryDiscontinuation",
                mode = "async"
            )
        }
    )
    public interface InventoryItemCreateStoreReturnEeca7 {}

    /**
     * EECA for entity InventoryItem on create-store/return.
     */
    @Eeca(
        entity = "InventoryItem",
        operation = "create-store",
        event = "return",
        actions = {
            @EecaAction(
                service = "updateSerializedInventoryTotals",
                mode = "sync"
            )
        }
    )
    public interface InventoryItemCreateStoreReturnEeca8 {}

    /**
     * EECA for entity InventoryItem on create/return.
     */
    @Eeca(
        entity = "InventoryItem",
        operation = "create",
        event = "return",
        actions = {
            @EecaAction(
                service = "createInventoryItemCheckSetAtpQoh",
                mode = "sync"
            )
        }
    )
    public interface InventoryItemCreateReturnEeca9 {}

    /**
     * EECA for entity InventoryItem on create/return.
     */
    @Eeca(
        entity = "InventoryItem",
        operation = "create",
        event = "return",
        condition = "!empty(statusId)",
        actions = {
            @EecaAction(
                service = "createInventoryItemStatus",
                mode = "sync"
            )
        }
    )
    public interface InventoryItemCreateReturnEeca10 {}

    /**
     * EECA for entity InventoryItemDetail on create-store-remove/return.
     */
    @Eeca(
        entity = "InventoryItemDetail",
        operation = "create-store-remove",
        event = "return",
        actions = {
            @EecaAction(
                service = "updateInventoryItemFromDetail",
                mode = "sync"
            )
        }
    )
    public interface InventoryItemDetailCreateStoreRemoveReturnEeca11 {}

    /**
     * EECA for entity InventoryItemDetail on create-store-remove/return.
     */
    @Eeca(
        entity = "InventoryItemDetail",
        operation = "create-store-remove",
        event = "return",
        condition = "(availableToPromiseDiff != '0' || quantityOnHandDiff != '0')",
        actions = {
            @EecaAction(
                service = "setLastInventoryCount",
                mode = "sync",
                valueAttr = "inventoryItemDetail"
            )
        }
    )
    public interface InventoryItemDetailCreateStoreRemoveReturnEeca12 {}

    /**
     * EECA for entity Picklist on create-store/return.
     */
    @Eeca(
        entity = "Picklist",
        operation = "create-store",
        event = "return",
        condition = "statusId == 'PICKLIST_CANCELLED'",
        actions = {
            @EecaAction(
                service = "cancelPicklistAndItems",
                mode = "async"
            )
        }
    )
    public interface PicklistCreateStoreReturnEeca13 {}

    /**
     * EECA for entity ProductGroupOrder on create/return.
     */
    @Eeca(
        entity = "ProductGroupOrder",
        operation = "create",
        event = "return",
        actions = {
            @EecaAction(
                service = "createJobForProductGroupOrder",
                mode = "sync"
            )
        }
    )
    public interface ProductGroupOrderCreateReturnEeca14 {}

    /**
     * EECA for entity ProductImageOpRequest on create/return.
     */
    @Eeca(
        entity = "ProductImageOpRequest",
        operation = "create",
        event = "return",
        assignments = {
            @EecaSet(fieldName = "deleteReq", value = "true")
        },
        actions = {
            @EecaAction(
                service = "productImageOpRequest",
                mode = "sync",
                valueAttr = "opReq"
            )
        }
    )
    public interface ProductImageOpRequestCreateReturnEeca15 {}

}
