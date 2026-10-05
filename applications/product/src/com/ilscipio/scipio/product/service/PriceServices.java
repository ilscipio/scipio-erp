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
package com.ilscipio.scipio.product.service;

import com.ilscipio.scipio.service.def.*;

/**
 * Auto-generated annotation-based service definitions.
 *
 * <p>Generated from services.xml by convertXmlToAnnotation Gradle task.</p>
 *
 * <p>SCIPIO: 4.0.0: Auto-generated.</p>
 */
public class PriceServices {

    /**
     * Create a QuantityBreakType record
     */
    @Service(
        name = "createQuantityBreakType",
        engine = "entity-auto",
        invoke = "create",
        description = "Create a QuantityBreakType record",
        defaultEntityName = "QuantityBreakType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "INOUT", include = "pk", optional = "true"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface CreateQuantityBreakType {}

    /**
     * Update a QuantityBreakType record
     */
    @Service(
        name = "updateQuantityBreakType",
        engine = "entity-auto",
        invoke = "update",
        description = "Update a QuantityBreakType record",
        defaultEntityName = "QuantityBreakType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface UpdateQuantityBreakType {}

    /**
     * Delete a QuantityBreakType record
     */
    @Service(
        name = "deleteQuantityBreakType",
        engine = "entity-auto",
        invoke = "delete",
        description = "Delete a QuantityBreakType record",
        defaultEntityName = "QuantityBreakType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        }
    )
    public interface DeleteQuantityBreakType {}

    /**
     * Create a SaleType record
     */
    @Service(
        name = "createSaleType",
        engine = "entity-auto",
        invoke = "create",
        description = "Create a SaleType record",
        defaultEntityName = "SaleType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "INOUT", include = "pk", optional = "true"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface CreateSaleType {}

    /**
     * Update a SaleType record
     */
    @Service(
        name = "updateSaleType",
        engine = "entity-auto",
        invoke = "update",
        description = "Update a SaleType record",
        defaultEntityName = "SaleType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface UpdateSaleType {}

    /**
     * Delete a SaleType record
     */
    @Service(
        name = "deleteSaleType",
        engine = "entity-auto",
        invoke = "delete",
        description = "Delete a SaleType record",
        defaultEntityName = "SaleType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        }
    )
    public interface DeleteSaleType {}

    /**
     * Create a ProductPricePurpose
     */
    @Service(
        name = "createProductPricePurpose",
        engine = "entity-auto",
        invoke = "create",
        description = "Create a ProductPricePurpose",
        defaultEntityName = "ProductPricePurpose",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "INOUT", include = "pk", optional = "true"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface CreateProductPricePurpose {}

    /**
     * Update a ProductPricePurpose
     */
    @Service(
        name = "updateProductPricePurpose",
        engine = "entity-auto",
        invoke = "update",
        description = "Update a ProductPricePurpose",
        defaultEntityName = "ProductPricePurpose",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface UpdateProductPricePurpose {}

    /**
     * Delete a ProductPricePurpose
     */
    @Service(
        name = "deleteProductPricePurpose",
        engine = "entity-auto",
        invoke = "delete",
        description = "Delete a ProductPricePurpose",
        defaultEntityName = "ProductPricePurpose",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        }
    )
    public interface DeleteProductPricePurpose {}

    /**
     * Create a ProductPriceType
     */
    @Service(
        name = "createProductPriceType",
        engine = "entity-auto",
        invoke = "create",
        description = "Create a ProductPriceType",
        defaultEntityName = "ProductPriceType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "INOUT", include = "pk", optional = "true"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface CreateProductPriceType {}

    /**
     * Update a ProductPriceType
     */
    @Service(
        name = "updateProductPriceType",
        engine = "entity-auto",
        invoke = "update",
        description = "Update a ProductPriceType",
        defaultEntityName = "ProductPriceType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface UpdateProductPriceType {}

    /**
     * Delete a ProductPriceType
     */
    @Service(
        name = "deleteProductPriceType",
        engine = "entity-auto",
        invoke = "delete",
        description = "Delete a ProductPriceType",
        defaultEntityName = "ProductPriceType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        }
    )
    public interface DeleteProductPriceType {}

}
