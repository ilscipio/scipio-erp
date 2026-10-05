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
package com.ilscipio.scipio.manufacturing.test;

import java.math.BigDecimal;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.ofbiz.base.util.UtilGenerics;
import org.ofbiz.entity.GenericValue;
import org.ofbiz.entity.util.EntityQuery;
import org.ofbiz.service.ServiceUtil;
import org.ofbiz.service.testtools.OFBizTestCase;

/**
 * Tests for the getProductStandardCost and getProductWhereUsed manufacturing costing services.
 */
public class CostingServicesTest extends OFBizTestCase {

    protected GenericValue userLogin = null;

    public CostingServicesTest(String name) {
        super(name);
    }

    @Override
    protected void setUp() throws Exception {
        userLogin = EntityQuery.use(delegator).from("UserLogin").where("userLoginId", "system").queryOne();
    }

    @Override
    protected void tearDown() throws Exception {
    }

    public void testStandardCostProdManuf() throws Exception {
        Map<String, Object> ctx = new HashMap<>();
        ctx.put("productId", "PROD_MANUF");
        ctx.put("recalculate", Boolean.TRUE);
        ctx.put("userLogin", userLogin);
        Map<String, Object> result = dispatcher.runSync("getProductStandardCost", ctx);
        assertFalse(ServiceUtil.getErrorMessage(result), ServiceUtil.isError(result));

        BigDecimal totalCost = (BigDecimal) result.get("totalCost");
        assertNotNull("totalCost was not returned", totalCost);
        assertTrue("totalCost should be greater than zero", totalCost.signum() > 0);

        BigDecimal materialCost = (BigDecimal) result.get("materialCost");
        assertNotNull("materialCost was not returned", materialCost);
        assertTrue("materialCost should be greater than zero", materialCost.signum() > 0);

        List<Map<String, Object>> components = UtilGenerics.cast(result.get("components"));
        assertNotNull("components was not returned", components);
        assertEquals("PROD_MANUF should have 2 direct BOM components", 2, components.size());

        Map<String, Object> matA = findComponent(components, "MAT_A_COST");
        assertNotNull("MAT_A_COST component not found", matA);
        assertEquals(0, new BigDecimal("2").compareTo((BigDecimal) matA.get("quantity")));

        Map<String, Object> matB = findComponent(components, "MAT_B_COST");
        assertNotNull("MAT_B_COST component not found", matB);
        assertEquals(0, new BigDecimal("3").compareTo((BigDecimal) matB.get("quantity")));
    }

    public void testWhereUsed() throws Exception {
        Map<String, Object> matACtx = new HashMap<>();
        matACtx.put("productId", "MAT_A_COST");
        matACtx.put("userLogin", userLogin);
        Map<String, Object> matAResult = dispatcher.runSync("getProductWhereUsed", matACtx);
        assertFalse(ServiceUtil.getErrorMessage(matAResult), ServiceUtil.isError(matAResult));

        List<Map<String, Object>> whereUsed = UtilGenerics.cast(matAResult.get("whereUsed"));
        assertNotNull("whereUsed was not returned", whereUsed);

        Map<String, Object> prodManufEntry = findComponent(whereUsed, "PROD_MANUF");
        assertNotNull("PROD_MANUF not found in MAT_A_COST's where-used list", prodManufEntry);
        assertEquals(1, ((Integer) prodManufEntry.get("depth")).intValue());
        assertEquals(0, new BigDecimal("2").compareTo((BigDecimal) prodManufEntry.get("quantity")));

        Map<String, Object> prodManufCtx = new HashMap<>();
        prodManufCtx.put("productId", "PROD_MANUF");
        prodManufCtx.put("userLogin", userLogin);
        Map<String, Object> prodManufResult = dispatcher.runSync("getProductWhereUsed", prodManufCtx);
        assertFalse(ServiceUtil.getErrorMessage(prodManufResult), ServiceUtil.isError(prodManufResult));
        assertEquals("unexpected where-used rows: " + prodManufResult.get("whereUsed"), 0, ((Integer) prodManufResult.get("count")).intValue());
    }

    private static Map<String, Object> findComponent(List<Map<String, Object>> rows, String productId) {
        for (Map<String, Object> row : rows) {
            if (productId.equals(row.get("productId"))) {
                return row;
            }
        }
        return null;
    }

}
