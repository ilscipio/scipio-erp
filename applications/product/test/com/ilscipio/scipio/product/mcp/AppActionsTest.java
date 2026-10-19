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

    /** R4-4: facility/shipment tracking_set is a write action with the shipment and tracking number as required arguments. */
    @Test
    void trackingSetIsAWriteActionOfTheShipmentTool() throws Exception {
        java.lang.reflect.Method found = null;
        for (java.lang.reflect.Method m : FacilityMcp.class.getDeclaredMethods()) {
            McpTool a = m.getAnnotation(McpTool.class);
            if (a != null && "shipment".equals(a.topic()) && "tracking_set".equals(a.name())) {
                found = m;
                assertFalse(a.readOnly());
            }
        }
        assertNotNull(found);
        java.util.Set<String> required = new java.util.HashSet<>();
        for (java.lang.reflect.Parameter p : found.getParameters()) {
            com.ilscipio.scipio.mcp.def.McpParam mp = p.getAnnotation(com.ilscipio.scipio.mcp.def.McpParam.class);
            if (mp != null && mp.required()) required.add(mp.name());
        }
        assertEquals(java.util.Set.of("shipmentId", "trackingIdNumber"), required);
    }

    @Test
    void trackingSetNeedsThePermission() {
        org.ofbiz.entity.GenericValue userLogin = mock(org.ofbiz.entity.GenericValue.class);
        when(userLogin.getString("userLoginId")).thenReturn("agent");
        Security sec = mock(Security.class);
        when(sec.hasPermission(anyString(), any(org.ofbiz.entity.GenericValue.class))).thenReturn(false);
        when(sec.hasEntityPermission(anyString(), anyString(), any(org.ofbiz.entity.GenericValue.class))).thenReturn(false);
        javax.servlet.http.HttpServletRequest http = mock(javax.servlet.http.HttpServletRequest.class);
        com.ilscipio.scipio.mcp.web.McpRequest req = new com.ilscipio.scipio.mcp.web.McpRequest(http, null, null, null, null, sec, "req-1");
        org.ofbiz.entity.GenericValue token = mock(org.ofbiz.entity.GenericValue.class);
        when(token.getString("tokenId")).thenReturn("tok");
        req.setPrincipal(new com.ilscipio.scipio.mcp.security.McpPrincipal(token, userLogin));
        com.ilscipio.scipio.mcp.registry.McpCallContext ctx = new com.ilscipio.scipio.mcp.registry.McpCallContext(req);
        com.ilscipio.scipio.mcp.registry.McpToolException e = assertThrows(com.ilscipio.scipio.mcp.registry.McpToolException.class,
                () -> FacilityMcp.setTracking(ctx, "10050", "9400 1111", "USPS", null));
        assertTrue(e.isDenied());
        assertTrue(e.getMessage().contains("FACILITY_UPDATE"));
    }

    @Test
    void trackingNumberIsCleanedAndBounded() {
        assertEquals("940011112222", FacilityMcp.cleanTracking("  9400 1111 2222 "));
        assertNull(FacilityMcp.cleanTracking("   "));
        assertNull(FacilityMcp.cleanTracking(null));
        assertEquals(60, FacilityMcp.MAX_TRACKING_LENGTH); // ShipmentRouteSegment.trackingIdNumber is a short-varchar (60)
        assertNull(FacilityMcp.cleanTracking("1".repeat(FacilityMcp.MAX_TRACKING_LENGTH + 1)));
        assertNotNull(FacilityMcp.cleanTracking("1".repeat(FacilityMcp.MAX_TRACKING_LENGTH)));
    }

    /** R4-4 finding 8: the carrier id is cleaned, and a new carrier party gets a readable name. */
    @Test
    void carrierIdIsCleanedAndNamed() {
        assertEquals("ROYAL_MAIL", FacilityMcp.cleanCarrier(" royal_mail "));
        assertNull(FacilityMcp.cleanCarrier("Royal Mail"));
        assertNull(FacilityMcp.cleanCarrier(""));
        assertNull(FacilityMcp.cleanCarrier(null));
        assertNull(FacilityMcp.cleanCarrier("A".repeat(21)));
        assertEquals("Royal Mail", FacilityMcp.carrierName("ROYAL_MAIL"));
        assertEquals("Evri", FacilityMcp.carrierName("EVRI"));
        assertEquals("XPRESS", FacilityMcp.carrierName("XPRESS"));
    }

    /** R4-4 finding 9: a new route segment needs FACILITY_CREATE; an existing segment does not. */
    @Test
    void aNewSegmentNeedsTheCreatePermission() throws Exception {
        org.ofbiz.entity.GenericValue userLogin = mock(org.ofbiz.entity.GenericValue.class);
        when(userLogin.getString("userLoginId")).thenReturn("agent");
        Security sec = mock(Security.class);
        when(sec.hasPermission(anyString(), any(org.ofbiz.entity.GenericValue.class))).thenReturn(false);
        when(sec.hasEntityPermission(anyString(), anyString(), any(org.ofbiz.entity.GenericValue.class))).thenReturn(false);
        javax.servlet.http.HttpServletRequest http = mock(javax.servlet.http.HttpServletRequest.class);
        com.ilscipio.scipio.mcp.web.McpRequest req = new com.ilscipio.scipio.mcp.web.McpRequest(http, null, null, null, null, sec, "req-2");
        org.ofbiz.entity.GenericValue token = mock(org.ofbiz.entity.GenericValue.class);
        when(token.getString("tokenId")).thenReturn("tok");
        req.setPrincipal(new com.ilscipio.scipio.mcp.security.McpPrincipal(token, userLogin));
        com.ilscipio.scipio.mcp.registry.McpCallContext ctx = new com.ilscipio.scipio.mcp.registry.McpCallContext(req);
        com.ilscipio.scipio.mcp.registry.McpToolException e = assertThrows(com.ilscipio.scipio.mcp.registry.McpToolException.class,
                () -> FacilityMcp.requireCreateForNewSegment(ctx, false));
        assertTrue(e.isDenied());
        assertTrue(e.getMessage().contains("FACILITY_CREATE"));
        FacilityMcp.requireCreateForNewSegment(ctx, true); // an existing segment needs FACILITY_UPDATE only: no check here
    }
}
