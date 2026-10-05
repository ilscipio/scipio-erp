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
public class RentalServices {

    /**
     * Create an FixedAsset and link to an existing product to ease rental products creation
     */
    @Service(
        name = "createFixedAssetAndLinkToProduct",
        engine = "simple",
        location = "component://product/script/org/ofbiz/product/rental/RentalServices.xml",
        invoke = "createFixedAssetAndLinkToProduct",
        description = "Create an FixedAsset and link to an existing product to ease rental products creation",
        defaultEntityName = "FixedAsset",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "INOUT", include = "pk", optional = "true"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        attributes = {
            @Attribute(name = "productId", type = "String", mode = "IN")
        }
    )
    public interface CreateFixedAssetAndLinkToProduct {}

    /**
     * Most rental products are associated with one fixed asset only, this service will return the first genericValue fixedAsset
     */
    @Service(
        name = "getProductFirstRelatedFixedAsset",
        engine = "simple",
        location = "component://product/script/org/ofbiz/product/rental/RentalServices.xml",
        invoke = "getProductFirstRelatedFixedAsset",
        description = "Most rental products are associated with one fixed asset only, this service will return the first genericValue fixedAsset",
        auth = "true",
        attributes = {
            @Attribute(name = "productId", type = "String", mode = "INOUT"),
            @Attribute(name = "fixedAssetId", type = "String", mode = "OUT", optional = "true")
        }
    )
    public interface GetProductFirstRelatedFixedAsset {}

}
