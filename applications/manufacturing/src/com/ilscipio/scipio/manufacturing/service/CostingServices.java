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
package com.ilscipio.scipio.manufacturing.service;

import com.ilscipio.scipio.service.def.*;

/**
 * Manufacturing product standard costing and BOM where-used lookup service definitions.
 *
 * <p>SCIPIO: 4.0.0: Added.</p>
 */
public class CostingServices {

    /**
     * Gets a product's standard cost breakdown from CostComponent entries, optionally recalculating first.
     */
    @Service(
        name = "getProductStandardCost",
        engine = "java",
        location = "com.ilscipio.scipio.manufacturing.costing.CostingServices",
        invoke = "getProductStandardCost",
        description = "Gets a product's standard cost breakdown from CostComponent entries, optionally recalculating first",
        auth = "true",
        permissions = {@Permissions(joinType = "AND", permissions = {@Permission(permission = "MANUFACTURING", action = "_VIEW")})},
        attributes = {
            @Attribute(name = "productId", type = "String", mode = "IN"),
            @Attribute(name = "currencyUomId", type = "String", mode = "IN", optional = "true", defaultValue = "USD"),
            @Attribute(name = "costComponentTypePrefix", type = "String", mode = "IN", optional = "true", defaultValue = "EST_STD"),
            @Attribute(name = "recalculate", type = "Boolean", mode = "IN", optional = "true", defaultValue = "false"),
            @Attribute(name = "costComponents", type = "List", mode = "OUT"),
            @Attribute(name = "totalCost", type = "BigDecimal", mode = "OUT"),
            @Attribute(name = "materialCost", type = "BigDecimal", mode = "OUT"),
            @Attribute(name = "laborCost", type = "BigDecimal", mode = "OUT"),
            @Attribute(name = "overheadCost", type = "BigDecimal", mode = "OUT"),
            @Attribute(name = "routingCost", type = "BigDecimal", mode = "OUT"),
            @Attribute(name = "components", type = "List", mode = "OUT"),
            @Attribute(name = "lastCalculatedDate", type = "Timestamp", mode = "OUT", optional = "true")
        }
    )
    public interface GetProductStandardCost {}

    /**
     * Gets the list of products that (directly or indirectly) use the given product as a bill-of-materials component.
     */
    @Service(
        name = "getProductWhereUsed",
        engine = "java",
        location = "com.ilscipio.scipio.manufacturing.costing.CostingServices",
        invoke = "getProductWhereUsed",
        description = "Gets the list of products that (directly or indirectly) use the given product as a bill-of-materials component",
        auth = "true",
        permissions = {@Permissions(joinType = "AND", permissions = {@Permission(permission = "MANUFACTURING", action = "_VIEW")})},
        attributes = {
            @Attribute(name = "productId", type = "String", mode = "IN"),
            @Attribute(name = "bomType", type = "String", mode = "IN", optional = "true", defaultValue = "MANUF_COMPONENT"),
            @Attribute(name = "inDate", type = "Timestamp", mode = "IN", optional = "true"),
            @Attribute(name = "whereUsed", type = "List", mode = "OUT"),
            @Attribute(name = "rootProductId", type = "String", mode = "OUT"),
            @Attribute(name = "count", type = "Integer", mode = "OUT")
        }
    )
    public interface GetProductWhereUsed {}

}
