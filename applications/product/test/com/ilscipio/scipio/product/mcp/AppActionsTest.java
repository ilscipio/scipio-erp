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
package com.ilscipio.scipio.product.mcp;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.Mockito.*;

import java.sql.Timestamp;
import java.util.Map;

import org.junit.jupiter.api.Test;
import org.ofbiz.security.Security;
import org.ofbiz.service.DispatchContext;
import org.ofbiz.service.ServiceUtil;

import com.ilscipio.scipio.mcp.def.McpServer;
import com.ilscipio.scipio.mcp.def.McpServiceTool;
import com.ilscipio.scipio.mcp.def.McpTool;
import com.ilscipio.scipio.product.service.ProductReviewServices;

/** W1-08c: catalog/product review_mark, facility/shipment pack and label_void (service and wiring level). */
public class AppActionsTest {

    private static McpServiceTool tool(Class<?> c, String topic, String name) {
        for (McpServiceTool t : c.getAnnotation(McpServer.class).serviceTools()) {
            if (topic.equals(t.topic()) && name.equals(t.name())) return t;
        }
        return null;
    }

    @Test
    void reviewMarkIsWiredAndFlagsAreYN() throws Exception {
        McpServiceTool t = tool(CatalogMcp.class, "product", "review_mark");
        assertNotNull(t);
        assertEquals("markProductReviewed", t.service());
        assertEquals("Y", ProductReviewServices.flag(true));
        assertEquals("N", ProductReviewServices.flag(false));
        String n = ProductReviewServices.note(true, "admin", new Timestamp(0));
        assertTrue(n.startsWith("reviewed by admin at "));
        assertNotNull(ProductReviewServices.class.getMethod("markProductReviewed", DispatchContext.class, Map.class));
    }

    @Test
    void reviewMarkNeedsPermission() {
        DispatchContext dctx = mock(DispatchContext.class);
        Security sec = mock(Security.class);
        when(dctx.getSecurity()).thenReturn(sec);
        Map<String, Object> r = ProductReviewServices.markProductReviewed(dctx, Map.of("productId", "P1", "reviewed", Boolean.TRUE));
        assertTrue(ServiceUtil.isError(r));
        assertTrue(ServiceUtil.getErrorMessage(r).contains("CATALOG_UPDATE"));
    }

    @Test
    void packSetsShipmentPackedThroughUpdateShipment() {
        McpServiceTool t = tool(FacilityMcp.class, "shipment", "pack");
        assertNotNull(t);
        assertEquals("updateShipment", t.service());
        assertArrayEquals(new String[] {"statusId=SHIPMENT_PACKED"}, t.fixed());
    }

    @Test
    void labelVoidIsAnActionWithUpsOnly() throws Exception {
        McpTool found = null;
        for (java.lang.reflect.Method m : FacilityMcp.class.getDeclaredMethods()) {
            McpTool a = m.getAnnotation(McpTool.class);
            if (a != null && "shipment".equals(a.topic()) && "label_void".equals(a.name())) found = a;
        }
        assertNotNull(found);
        assertFalse(found.readOnly());
        assertEquals("UPS", FacilityMcp.VOID_CARRIER);
    }
}
