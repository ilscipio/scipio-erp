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
package com.ilscipio.scipio.mcp.catalog;

import static org.junit.jupiter.api.Assertions.assertEquals;

import java.util.Map;

import org.junit.jupiter.api.Test;

public class ServiceSchemaBuilderTest {

    @Test
    public void mapsJavaTypesToJsonSchema() {
        assertEquals("string", ServiceSchemaBuilder.typeSchema("String").get("type"));
        assertEquals("integer", ServiceSchemaBuilder.typeSchema("java.lang.Long").get("type"));
        assertEquals("number", ServiceSchemaBuilder.typeSchema("Double").get("type"));
        assertEquals("boolean", ServiceSchemaBuilder.typeSchema("Boolean").get("type"));
        Map<String, Object> ts = ServiceSchemaBuilder.typeSchema("java.sql.Timestamp");
        assertEquals("string", ts.get("type"));
        assertEquals("date-time", ts.get("format"));
        Map<String, Object> bd = ServiceSchemaBuilder.typeSchema("java.math.BigDecimal");
        assertEquals(java.util.Arrays.asList("number", "string"), bd.get("type"));
        assertEquals("array", ServiceSchemaBuilder.typeSchema("java.util.List").get("type"));
        assertEquals("object", ServiceSchemaBuilder.typeSchema("org.ofbiz.entity.GenericValue").get("type"));
        Map<String, Object> other = ServiceSchemaBuilder.typeSchema("java.nio.ByteBuffer");
        assertEquals("string", other.get("type"));
        assertEquals("java.nio.ByteBuffer", other.get("x-javaType"));
    }

    @Test
    public void convertsServiceNamesToSnakeCase() {
        assertEquals("create_order", ServiceSchemaBuilder.toSnakeCase("createOrder"));
        assertEquals("get_product_price", ServiceSchemaBuilder.toSnakeCase("getProductPrice"));
        assertEquals("update_party_group", ServiceSchemaBuilder.toSnakeCase("updatePartyGroup"));
        assertEquals("send_order_confirmation", ServiceSchemaBuilder.toSnakeCase("sendOrderConfirmation"));
    }
}
