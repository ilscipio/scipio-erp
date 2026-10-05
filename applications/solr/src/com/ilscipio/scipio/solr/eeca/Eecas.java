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
package com.ilscipio.scipio.solr.eeca;

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
     * EECA for entity Product on create-store/return.
     */
    @Eeca(
        entity = "Product",
        operation = "create-store",
        event = "return",
        assignments = {
            @EecaSet(fieldName = "updateVariants", value = "true")
        },
        actions = {
            @EecaAction(
                service = "scheduleProductIndexing",
                mode = "sync",
                valueAttr = "instance"
            )
        }
    )
    public interface ProductCreateStoreReturnEeca1 {}

    /**
     * EECA for entity Product on remove/return.
     */
    @Eeca(
        entity = "Product",
        operation = "remove",
        event = "return",
        actions = {
            @EecaAction(
                service = "scheduleProductIndexing",
                mode = "sync",
                valueAttr = "instance"
            )
        }
    )
    public interface ProductRemoveReturnEeca2 {}

    /**
     * EECA for entity ProductCategoryMember on create-store-remove/return.
     */
    @Eeca(
        entity = "ProductCategoryMember",
        operation = "create-store-remove",
        event = "return",
        assignments = {
            @EecaSet(fieldName = "updateVariants", value = "true")
        },
        actions = {
            @EecaAction(
                service = "scheduleProductIndexing",
                mode = "sync",
                valueAttr = "instance"
            )
        }
    )
    public interface ProductCategoryMemberCreateStoreRemoveReturnEeca3 {}

    /**
     * EECA for entity ProductPrice on create-store-remove/return.
     */
    @Eeca(
        entity = "ProductPrice",
        operation = "create-store-remove",
        event = "return",
        assignments = {
            @EecaSet(fieldName = "updateVariants", value = "true")
        },
        actions = {
            @EecaAction(
                service = "scheduleProductIndexing",
                mode = "sync",
                valueAttr = "instance"
            )
        }
    )
    public interface ProductPriceCreateStoreRemoveReturnEeca4 {}

    /**
     * EECA for entity ProductKeyword on create-store-remove/return.
     */
    @Eeca(
        entity = "ProductKeyword",
        operation = "create-store-remove",
        event = "return",
        condition = "!empty(statusId)",
        assignments = {
            @EecaSet(fieldName = "updateVariants", value = "true")
        },
        actions = {
            @EecaAction(
                service = "scheduleProductIndexing",
                mode = "sync",
                valueAttr = "instance"
            )
        }
    )
    public interface ProductKeywordCreateStoreRemoveReturnEeca5 {}

    /**
     * EECA for entity ProductContent on create-store-remove/return.
     */
    @Eeca(
        entity = "ProductContent",
        operation = "create-store-remove",
        event = "return",
        assignments = {
            @EecaSet(fieldName = "updateVariants", value = "true")
        },
        actions = {
            @EecaAction(
                service = "scheduleProductIndexing",
                mode = "sync",
                valueAttr = "instance"
            )
        }
    )
    public interface ProductContentCreateStoreRemoveReturnEeca6 {}

    /**
     * EECA for entity InventoryItem on create-store-remove/return.
     */
    @Eeca(
        entity = "InventoryItem",
        operation = "create-store-remove",
        event = "return",
        condition = "!empty(productId)",
        assignments = {
            @EecaSet(fieldName = "updateVirtual", value = "true")
        },
        actions = {
            @EecaAction(
                service = "scheduleProductIndexing",
                mode = "sync",
                valueAttr = "instance"
            )
        }
    )
    public interface InventoryItemCreateStoreRemoveReturnEeca7 {}

    /**
     * EECA for entity ProductFacility on create-store-remove/return.
     */
    @Eeca(
        entity = "ProductFacility",
        operation = "create-store-remove",
        event = "return",
        assignments = {
            @EecaSet(fieldName = "updateVirtual", value = "true")
        },
        actions = {
            @EecaAction(
                service = "scheduleProductIndexing",
                mode = "sync",
                valueAttr = "instance"
            )
        }
    )
    public interface ProductFacilityCreateStoreRemoveReturnEeca8 {}

}
