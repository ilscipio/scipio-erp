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
public class SupplierServices {

    /**
     * Create a ReorderGuideline record
     */
    @Service(
        name = "createReorderGuideline",
        engine = "entity-auto",
        invoke = "create",
        description = "Create a ReorderGuideline record",
        defaultEntityName = "ReorderGuideline",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "INOUT", include = "pk", optional = "true"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface CreateReorderGuideline {}

    /**
     * Update a ReorderGuideline record
     */
    @Service(
        name = "updateReorderGuideline",
        engine = "entity-auto",
        invoke = "update",
        description = "Update a ReorderGuideline record",
        defaultEntityName = "ReorderGuideline",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface UpdateReorderGuideline {}

    /**
     * Delete a ReorderGuideline record
     */
    @Service(
        name = "deleteReorderGuideline",
        engine = "entity-auto",
        invoke = "delete",
        description = "Delete a ReorderGuideline record",
        defaultEntityName = "ReorderGuideline",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        }
    )
    public interface DeleteReorderGuideline {}

    /**
     * Create a SupplierRatingType
     */
    @Service(
        name = "createSupplierRatingType",
        engine = "entity-auto",
        invoke = "create",
        description = "Create a SupplierRatingType",
        defaultEntityName = "SupplierRatingType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true"),
            @EntityAttributes(mode = "INOUT", include = "pk", optional = "true")
        }
    )
    public interface CreateSupplierRatingType {}

    /**
     * Update a SupplierRatingType
     */
    @Service(
        name = "updateSupplierRatingType",
        engine = "entity-auto",
        invoke = "update",
        description = "Update a SupplierRatingType",
        defaultEntityName = "SupplierRatingType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface UpdateSupplierRatingType {}

    /**
     * Delete a SupplierRatingType
     */
    @Service(
        name = "deleteSupplierRatingType",
        engine = "entity-auto",
        invoke = "delete",
        description = "Delete a SupplierRatingType",
        defaultEntityName = "SupplierRatingType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        }
    )
    public interface DeleteSupplierRatingType {}

}
