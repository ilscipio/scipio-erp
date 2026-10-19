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

import java.math.BigDecimal;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.junit.jupiter.api.Test;
import org.ofbiz.security.Security;

import com.ilscipio.scipio.mcp.def.McpParam;
import com.ilscipio.scipio.mcp.def.McpTool;
import com.ilscipio.scipio.mcp.registry.McpCallContext;
import com.ilscipio.scipio.mcp.registry.McpToolException;

/** R4-6: facility/shipping_rate list, set and remove (wiring, input rules, permission and the fold of the estimates). */
public class ShippingRateToolsTest {

    private static McpTool tool(String name) {
        for (java.lang.reflect.Method m : FacilityMcp.class.getDeclaredMethods()) {
            McpTool a = m.getAnnotation(McpTool.class);
            if (a != null && "shipping_rate".equals(a.topic()) && name.equals(a.name())) return a;
        }
        return null;
    }

    private static Set<String> required(String name) {
        Set<String> required = new java.util.HashSet<>();
        for (java.lang.reflect.Method m : FacilityMcp.class.getDeclaredMethods()) {
            McpTool a = m.getAnnotation(McpTool.class);
            if (a != null && "shipping_rate".equals(a.topic()) && name.equals(a.name())) {
                for (java.lang.reflect.Parameter p : m.getParameters()) {
                    McpParam mp = p.getAnnotation(McpParam.class);
                    if (mp != null && mp.required()) required.add(mp.name());
                }
            }
        }
        return required;
    }

    @Test
    void listIsReadOnlyAndSetAndRemoveWrite() {
        assertNotNull(tool("list"));
        assertTrue(tool("list").readOnly());
        assertNotNull(tool("set"));
        assertFalse(tool("set").readOnly());
        assertNotNull(tool("remove"));
        assertFalse(tool("remove").readOnly());
        assertEquals(Set.of("country", "amount"), required("set"));
        assertEquals(Set.of("country"), required("remove"));
    }

    @Test
    void countryAndAmountAreCleaned() {
        assertEquals("GB", FacilityMcp.cleanCountry(" gb "));
        assertNull(FacilityMcp.cleanCountry("GBR"));
        assertNull(FacilityMcp.cleanCountry(""));
        assertNull(FacilityMcp.cleanCountry(null));
        assertEquals(new BigDecimal("3.95"), FacilityMcp.cleanRateAmount(new BigDecimal("3.949")));
        assertEquals(new BigDecimal("0.00"), FacilityMcp.cleanRateAmount(BigDecimal.ZERO));
        assertNull(FacilityMcp.cleanRateAmount(new BigDecimal("-0.01")));
        assertNull(FacilityMcp.cleanRateAmount(new BigDecimal("100000.01")));
        assertNull(FacilityMcp.cleanRateAmount(null));
        assertEquals(List.of("STANDARD", "EXPRESS"), FacilityMcp.RATE_METHODS);
    }

    @Test
    void theFoldGivesOneRowForEachCountryAndMethod() {
        Map<String, String> geo = Map.of("GBR", "GB", "DEU", "DE", "FRA", "FR");
        Map<String, String> names = Map.of("STANDARD", "Standard", "EXPRESS", "Faster delivery");
        List<Map<String, Object>> rows = FacilityMcp.foldRates(List.of(
                new FacilityMcp.RateEstimate("GBR", "EXPRESS", new BigDecimal("7.50"), null),
                new FacilityMcp.RateEstimate("GBR", "STANDARD", new BigDecimal("3.95"), null),
                new FacilityMcp.RateEstimate("GBR", "STANDARD", BigDecimal.ZERO, new BigDecimal("40.00")),
                new FacilityMcp.RateEstimate("DEU", "STANDARD", new BigDecimal("4.90"), null),
                new FacilityMcp.RateEstimate("XXX", "STANDARD", new BigDecimal("1.00"), null),       // a geo that is no country
                new FacilityMcp.RateEstimate("FRA", "STANDARD", BigDecimal.ZERO, new BigDecimal("9"))), // no base estimate
                geo, names);
        assertEquals(3, rows.size());
        assertEquals("DE", rows.get(0).get("country"));
        assertEquals("GB", rows.get(1).get("country"));
        assertEquals("STANDARD", rows.get(1).get("shipmentMethodTypeId"));
        assertEquals(new BigDecimal("3.95"), rows.get(1).get("amount"));
        assertEquals(new BigDecimal("40.00"), rows.get(1).get("freeFrom"));
        assertNull(rows.get(0).get("freeFrom"));
        assertEquals("EXPRESS", rows.get(2).get("shipmentMethodTypeId"));
        assertEquals("Faster delivery", rows.get(2).get("name"));
    }

    private static org.ofbiz.entity.GenericValue estimate(String id, String breakId) {
        org.ofbiz.entity.GenericValue e = mock(org.ofbiz.entity.GenericValue.class);
        when(e.getString("shipmentCostEstimateId")).thenReturn(id);
        when(e.getString("priceBreakId")).thenReturn(breakId);
        return e;
    }

    @Test
    void duplicateEstimatesAreSplitSoThatSetKeepsOneOfEachKind() {
        java.util.List<org.ofbiz.entity.GenericValue> bases = new java.util.ArrayList<>();
        java.util.List<org.ofbiz.entity.GenericValue> frees = new java.util.ArrayList<>();
        FacilityMcp.splitEstimates(List.of(estimate("1", null), estimate("2", ""), estimate("3", "B1"), estimate("4", "B2")), bases, frees);
        assertEquals(2, bases.size(), "set updates the first base estimate and removes the second");
        assertEquals("1", bases.get(0).getString("shipmentCostEstimateId"));
        assertEquals(2, frees.size(), "set updates the first free-from estimate and removes the second");
        assertEquals("3", frees.get(0).getString("shipmentCostEstimateId"));
    }

    @Test
    void aCreatedBreakIsFoundByItsNewId() {
        assertEquals("12", FacilityMcp.newId(Set.of("10", "11"), Set.of("10", "11", "12")));
        assertNull(FacilityMcp.newId(Set.of("10"), Set.of("10")));
    }

    @Test
    void bothToolsGiveTheCurrencyOfTheStore() {
        assertEquals("EUR", FacilityMcp.currencyOf("EUR", "USD"));
        assertEquals("USD", FacilityMcp.currencyOf(null, "USD"));
        assertEquals("USD", FacilityMcp.currencyOf("", "USD"));
    }

    @Test
    void theRateRowsGoThroughServicesNotTheDelegator() throws Exception {
        java.nio.file.Path src = java.nio.file.Paths.get("applications/product/src/com/ilscipio/scipio/product/mcp/FacilityMcp.java");
        if (!java.nio.file.Files.exists(src)) return;
        String text = new String(java.nio.file.Files.readAllBytes(src), java.nio.charset.StandardCharsets.UTF_8);
        for (String entity : new String[] {"QuantityBreak", "ShipmentMethodType", "CarrierShipmentMethod"}) {
            assertFalse(text.contains("makeValue(\"" + entity + "\")"), entity + " is written by a service");
        }
    }

    private static McpCallContext deniedContext() {
        org.ofbiz.entity.GenericValue userLogin = mock(org.ofbiz.entity.GenericValue.class);
        when(userLogin.getString("userLoginId")).thenReturn("agent");
        Security sec = mock(Security.class);
        when(sec.hasPermission(anyString(), any(org.ofbiz.entity.GenericValue.class))).thenReturn(false);
        when(sec.hasEntityPermission(anyString(), anyString(), any(org.ofbiz.entity.GenericValue.class))).thenReturn(false);
        javax.servlet.http.HttpServletRequest http = mock(javax.servlet.http.HttpServletRequest.class);
        com.ilscipio.scipio.mcp.web.McpRequest req = new com.ilscipio.scipio.mcp.web.McpRequest(http, null, null, null, null, sec, "req-rate");
        org.ofbiz.entity.GenericValue token = mock(org.ofbiz.entity.GenericValue.class);
        when(token.getString("tokenId")).thenReturn("tok");
        req.setPrincipal(new com.ilscipio.scipio.mcp.security.McpPrincipal(token, userLogin));
        return new McpCallContext(req);
    }

    @Test
    void setAndRemoveNeedThePermission() {
        McpCallContext ctx = deniedContext();
        McpToolException set = assertThrows(McpToolException.class,
                () -> FacilityMcp.setShippingRate(ctx, "GB", new BigDecimal("3.95"), null, null, null));
        assertTrue(set.isDenied());
        assertTrue(set.getMessage().contains("FACILITY_UPDATE"));
        McpToolException remove = assertThrows(McpToolException.class, () -> FacilityMcp.removeShippingRate(ctx, "GB", "EXPRESS", null));
        assertTrue(remove.isDenied());
    }
}
