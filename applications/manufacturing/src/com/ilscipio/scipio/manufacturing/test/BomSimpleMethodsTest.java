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
import java.net.URL;
import java.util.HashMap;
import java.util.Locale;
import java.util.Map;

import org.ofbiz.base.location.FlexibleLocation;
import org.ofbiz.base.util.UtilMisc;
import org.ofbiz.entity.GenericValue;
import org.ofbiz.entity.util.EntityQuery;
import org.ofbiz.entity.util.EntitySaxReader;
import org.ofbiz.service.ServiceUtil;
import org.ofbiz.service.testtools.OFBizTestCase;

import com.ilscipio.scipio.manufacturing.event.BomSimpleMethods;

/**
 * Tests for the hand-written services and event that replaced BomSimpleMethods.xml, BomMapProcs.xml,
 * BomFormulas.xml and TaskFormulae.xml.
 */
public class BomSimpleMethodsTest extends OFBizTestCase {

    protected GenericValue userLogin = null;

    public BomSimpleMethodsTest(String name) {
        super(name);
    }

    @Override
    protected void setUp() throws Exception {
        userLogin = EntityQuery.use(delegator).from("UserLogin").where("userLoginId", "system").queryOne();
        // SCIPIO: self-contained fixture load; not registered in productionruntests.xml/ManufacturingTestsData.xml
        URL dataUrl = FlexibleLocation.resolveLocation("component://manufacturing/testdef/data/BomTestData.xml");
        new EntitySaxReader(delegator).parse(dataUrl);
        // SCIPIO: rows left by a previous run would trip the BOM loop check
        delegator.removeByCondition("ProductAssoc", org.ofbiz.entity.condition.EntityCondition.makeCondition(org.ofbiz.base.util.UtilMisc.toList(
                org.ofbiz.entity.condition.EntityCondition.makeCondition("productId", org.ofbiz.entity.condition.EntityOperator.LIKE, "MFT_PROD_%"),
                org.ofbiz.entity.condition.EntityCondition.makeCondition("productIdTo", org.ofbiz.entity.condition.EntityOperator.LIKE, "MFT_PROD_%")), org.ofbiz.entity.condition.EntityOperator.OR));
        delegator.removeByCondition("ProductManufacturingRule", org.ofbiz.entity.condition.EntityCondition.makeCondition("productId", org.ofbiz.entity.condition.EntityOperator.LIKE, "MFT_PROD_%"));
    }

    @Override
    protected void tearDown() throws Exception {
    }

    public void testCreateBOMAssoc() throws Exception {
        Map<String, Object> ctx = new HashMap<>();
        ctx.put("productId", "MFT_PROD_A");
        ctx.put("productIdTo", "MFT_PROD_B");
        ctx.put("productAssocTypeId", "MANUF_COMPONENT");
        ctx.put("quantity", new BigDecimal("1"));
        ctx.put("userLogin", userLogin);
        Map<String, Object> result = dispatcher.runSync("createBOMAssoc", ctx);
        assertFalse(ServiceUtil.getErrorMessage(result), ServiceUtil.isError(result));

        GenericValue assoc = EntityQuery.use(delegator).from("ProductAssoc")
                .where("productId", "MFT_PROD_A", "productIdTo", "MFT_PROD_B", "productAssocTypeId", "MANUF_COMPONENT")
                .queryFirst();
        assertNotNull("ProductAssoc MFT_PROD_A -> MFT_PROD_B was not created", assoc);
    }

    public void testCopyBOMAssocs() throws Exception {
        Map<String, Object> createCtx = new HashMap<>();
        createCtx.put("productId", "MFT_PROD_A");
        createCtx.put("productIdTo", "MFT_PROD_D");
        createCtx.put("productAssocTypeId", "MANUF_COMPONENT");
        createCtx.put("quantity", new BigDecimal("2"));
        createCtx.put("userLogin", userLogin);
        Map<String, Object> createResult = dispatcher.runSync("createBOMAssoc", createCtx);
        assertFalse(ServiceUtil.getErrorMessage(createResult), ServiceUtil.isError(createResult));

        Map<String, Object> copyCtx = new HashMap<>();
        copyCtx.put("productId", "MFT_PROD_A");
        copyCtx.put("copyToProductId", "MFT_PROD_C");
        copyCtx.put("productAssocTypeId", "MANUF_COMPONENT");
        copyCtx.put("userLogin", userLogin);
        Map<String, Object> copyResult = dispatcher.runSync("copyBOMAssocs", copyCtx);
        assertFalse(ServiceUtil.getErrorMessage(copyResult), ServiceUtil.isError(copyResult));

        GenericValue copiedAssoc = EntityQuery.use(delegator).from("ProductAssoc")
                .where("productId", "MFT_PROD_C", "productIdTo", "MFT_PROD_D", "productAssocTypeId", "MANUF_COMPONENT")
                .queryFirst();
        assertNotNull("ProductAssoc was not copied onto MFT_PROD_C", copiedAssoc);
    }

    public void testAddProductManufacturingRule() throws Exception {
        Map<String, Object> ctx = new HashMap<>();
        ctx.put("ruleId", "MFT_RULE_ADD");
        ctx.put("productId", "MFT_PROD_A");
        ctx.put("productIdIn", "MFT_PROD_D");
        ctx.put("quantity", 1.0);
        ctx.put("userLogin", userLogin);
        Map<String, Object> result = dispatcher.runSync("addProductManufacturingRule", ctx);
        assertFalse(ServiceUtil.getErrorMessage(result), ServiceUtil.isError(result));

        GenericValue rule = EntityQuery.use(delegator).from("ProductManufacturingRule").where("ruleId", "MFT_RULE_ADD").queryOne();
        assertNotNull("ProductManufacturingRule was not created", rule);
        assertEquals("MFT_PROD_D", rule.getString("productIdIn"));
    }

    public void testUpdateProductManufacturingRule() throws Exception {
        Map<String, Object> addCtx = new HashMap<>();
        addCtx.put("ruleId", "MFT_RULE_UPD");
        addCtx.put("productId", "MFT_PROD_A");
        addCtx.put("productIdIn", "MFT_PROD_D");
        addCtx.put("quantity", 1.0);
        addCtx.put("userLogin", userLogin);
        Map<String, Object> addResult = dispatcher.runSync("addProductManufacturingRule", addCtx);
        assertFalse(ServiceUtil.getErrorMessage(addResult), ServiceUtil.isError(addResult));

        Map<String, Object> updateCtx = new HashMap<>();
        updateCtx.put("ruleId", "MFT_RULE_UPD");
        updateCtx.put("productIdIn", "MFT_PROD_B");
        updateCtx.put("userLogin", userLogin);
        Map<String, Object> result = dispatcher.runSync("updateProductManufacturingRule", updateCtx);
        assertFalse(ServiceUtil.getErrorMessage(result), ServiceUtil.isError(result));

        GenericValue rule = EntityQuery.use(delegator).from("ProductManufacturingRule").where("ruleId", "MFT_RULE_UPD").queryOne();
        assertNotNull(rule);
        assertEquals("MFT_PROD_B", rule.getString("productIdIn"));
    }

    public void testDeleteProductManufacturingRule() throws Exception {
        Map<String, Object> addCtx = new HashMap<>();
        addCtx.put("ruleId", "MFT_RULE_DEL");
        addCtx.put("productId", "MFT_PROD_A");
        addCtx.put("productIdIn", "MFT_PROD_D");
        addCtx.put("quantity", 1.0);
        addCtx.put("userLogin", userLogin);
        Map<String, Object> addResult = dispatcher.runSync("addProductManufacturingRule", addCtx);
        assertFalse(ServiceUtil.getErrorMessage(addResult), ServiceUtil.isError(addResult));

        Map<String, Object> deleteCtx = new HashMap<>();
        deleteCtx.put("ruleId", "MFT_RULE_DEL");
        deleteCtx.put("userLogin", userLogin);
        Map<String, Object> result = dispatcher.runSync("deleteProductManufacturingRule", deleteCtx);
        assertFalse(ServiceUtil.getErrorMessage(result), ServiceUtil.isError(result));

        GenericValue rule = EntityQuery.use(delegator).from("ProductManufacturingRule").where("ruleId", "MFT_RULE_DEL").queryOne();
        assertNull("ProductManufacturingRule was not deleted", rule);
    }

    public void testExampleComponentFormula() throws Exception {
        Map<String, Object> arguments = UtilMisc.toMap("neededQuantity", new BigDecimal("5"));
        Map<String, Object> ctx = UtilMisc.toMap("arguments", arguments, "userLogin", userLogin);
        Map<String, Object> result = dispatcher.runSync("exampleComponentFormula", ctx);
        assertFalse(ServiceUtil.getErrorMessage(result), ServiceUtil.isError(result));
        assertEquals(0, new BigDecimal("50.00").compareTo((BigDecimal) result.get("quantity")));
    }

    public void testLinearComponentFormula() throws Exception {
        // neededQuantity(10) * amount(1) = 10; 10 / width(3) = 3.33 -> rounds up to the next whole piece: 4
        Map<String, Object> arguments = UtilMisc.toMap(
                "neededQuantity", new BigDecimal("10"), "amount", new BigDecimal("1"), "width", new BigDecimal("3"));
        Map<String, Object> ctx = UtilMisc.toMap("arguments", arguments, "userLogin", userLogin);
        Map<String, Object> result = dispatcher.runSync("linearComponentFormula", ctx);
        assertFalse(ServiceUtil.getErrorMessage(result), ServiceUtil.isError(result));
        assertEquals(0, new BigDecimal("4.00").compareTo((BigDecimal) result.get("quantity")));
    }

    public void testExampleTaskFormula() throws Exception {
        GenericValue task = delegator.makeValue("WorkEffort", "estimatedMilliSeconds", 1000.0);
        Map<String, Object> arguments = UtilMisc.<String, Object>toMap("workEffort", task, "quantity", new BigDecimal("5"));
        Map<String, Object> ctx = UtilMisc.toMap("arguments", arguments, "userLogin", userLogin);
        Map<String, Object> result = dispatcher.runSync("exampleTaskFormula", ctx);
        assertFalse(ServiceUtil.getErrorMessage(result), ServiceUtil.isError(result));
        // totalTime = estimatedMilliSeconds(1000) * quantity(5) * 10 = 50000
        assertEquals(0, new BigDecimal("50000.00").compareTo((BigDecimal) result.get("totalTime")));
    }

    public void testEditBom() throws Exception {
        Map<String, Object> params = new HashMap<>();
        params.put("UPDATE_MODE", "CREATE");
        params.put("productId", "MFT_PROD_B");
        params.put("productIdTo", "MFT_PROD_C");
        params.put("productAssocTypeId", "MANUF_COMPONENT");
        params.put("quantity", "3");

        Map<String, Object> result = BomSimpleMethods.editBom(delegator, dispatcher, userLogin, params, Locale.getDefault());
        assertFalse(ServiceUtil.getErrorMessage(result), ServiceUtil.isError(result));

        GenericValue assoc = EntityQuery.use(delegator).from("ProductAssoc")
                .where("productId", "MFT_PROD_B", "productIdTo", "MFT_PROD_C", "productAssocTypeId", "MANUF_COMPONENT")
                .queryFirst();
        assertNotNull("editBom(UPDATE_MODE=CREATE) did not create the ProductAssoc", assoc);
    }

}
