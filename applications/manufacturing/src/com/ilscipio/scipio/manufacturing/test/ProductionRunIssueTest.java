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
import java.sql.Timestamp;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;

import org.ofbiz.entity.GenericValue;
import org.ofbiz.entity.util.EntityQuery;
import org.ofbiz.service.ServiceUtil;
import org.ofbiz.service.testtools.OFBizTestCase;

import com.ilscipio.scipio.manufacturing.event.ProductionRunSimpleEvents;

/**
 * SCIPIO: Tests for the hand-written ProductionRunSimpleServices (issueProductionRunTask,
 * issueProductionRunTaskComponent, issueInventoryItemToWorkEffort, createProductionRunPartyAssign,
 * createProductionRunAssoc) and ProductionRunSimpleEvents logic methods.
 *
 * <p>Uses the demo product PROD_MANUF (BOM: MAT_A_COST x2, MAT_B_COST x3, routing ROUTING_COST,
 * facility ScipioShopWarehouse; see applications/shop/data/DemoStandardCostingData.xml), plus
 * dedicated on-hand inventory loaded from testdef/data/ProductionRunIssueTestData.xml
 * (MFT_INV_A / MFT_INV_B, 20 units each).</p>
 */
public class ProductionRunIssueTest extends OFBizTestCase {

    private static final String FACILITY_ID = "ScipioShopWarehouse";
    private static final String PRODUCT_ID = "PROD_MANUF";
    private static final String MAT_A = "MAT_A_COST";
    private static final String MAT_B = "MAT_B_COST";

    protected GenericValue userLogin = null;

    public ProductionRunIssueTest(String name) {
        super(name);
    }

    @Override
    protected void setUp() throws Exception {
        userLogin = EntityQuery.use(delegator).from("UserLogin").where("userLoginId", "system").queryOne();
    }

    @Override
    protected void tearDown() throws Exception {
    }

    // ==================== Helpers ====================

    private String createProductionRun(BigDecimal quantity) throws Exception {
        Map<String, Object> ctx = new HashMap<>();
        ctx.put("userLogin", userLogin);
        ctx.put("productId", PRODUCT_ID);
        ctx.put("pRQuantity", quantity);
        ctx.put("startDate", new Timestamp(System.currentTimeMillis() + 24L * 60 * 60 * 1000));
        ctx.put("facilityId", FACILITY_ID);
        Map<String, Object> result = dispatcher.runSync("createProductionRun", ctx);
        assertFalse("createProductionRun failed: " + ServiceUtil.getErrorMessage(result), ServiceUtil.isError(result));
        String productionRunId = (String) result.get("productionRunId");
        assertNotNull(productionRunId);
        return productionRunId;
    }

    private GenericValue getProductionRunTask(String productionRunId) throws Exception {
        GenericValue task = EntityQuery.use(delegator).from("WorkEffort")
                .where("workEffortParentId", productionRunId, "workEffortTypeId", "PROD_ORDER_TASK")
                .queryFirst();
        assertNotNull("No production run task found for " + productionRunId, task);
        return task;
    }

    private BigDecimal sumInventoryOnHand(String productId) throws Exception {
        List<GenericValue> items = EntityQuery.use(delegator).from("InventoryItem")
                .where("productId", productId, "facilityId", FACILITY_ID, "inventoryItemTypeId", "NON_SERIAL_INV_ITEM")
                .queryList();
        BigDecimal sum = BigDecimal.ZERO;
        for (GenericValue item : items) {
            BigDecimal qoh = item.getBigDecimal("quantityOnHandTotal");
            if (qoh != null) {
                sum = sum.add(qoh);
            }
        }
        return sum;
    }

    private BigDecimal sumWorkEffortAssignedQuantity(String workEffortId, String productId) throws Exception {
        List<GenericValue> assigns = EntityQuery.use(delegator).from("WorkEffortAndInventoryAssign")
                .where("workEffortId", workEffortId, "productId", productId)
                .queryList();
        BigDecimal sum = BigDecimal.ZERO;
        for (GenericValue assign : assigns) {
            BigDecimal quantity = assign.getBigDecimal("quantity");
            if (quantity != null) {
                sum = sum.add(quantity);
            }
        }
        return sum;
    }

    // ==================== issueProductionRunTask / issueProductionRunTaskComponent / assignInventoryToWorkEffort ====================

    /** issueProductionRunTask issues every BOM component in full and inventory falls accordingly. */
    public void testIssueProductionRunTaskFullIssuance() throws Exception {
        String productionRunId = createProductionRun(new BigDecimal("2"));
        GenericValue task = getProductionRunTask(productionRunId);

        BigDecimal onHandABefore = sumInventoryOnHand(MAT_A);
        BigDecimal onHandBBefore = sumInventoryOnHand(MAT_B);

        Map<String, Object> ctx = new HashMap<>();
        ctx.put("userLogin", userLogin);
        ctx.put("workEffortId", task.getString("workEffortId"));
        Map<String, Object> result = dispatcher.runSync("issueProductionRunTask", ctx);
        assertFalse("issueProductionRunTask failed: " + ServiceUtil.getErrorMessage(result), ServiceUtil.isError(result));

        // BOM: MAT_A_COST x2, MAT_B_COST x3; production run quantity = 2 -> needed 4 and 6
        BigDecimal issuedA = sumWorkEffortAssignedQuantity(task.getString("workEffortId"), MAT_A);
        BigDecimal issuedB = sumWorkEffortAssignedQuantity(task.getString("workEffortId"), MAT_B);
        assertEquals(0, new BigDecimal("4").compareTo(issuedA));
        assertEquals(0, new BigDecimal("6").compareTo(issuedB));

        BigDecimal onHandAAfter = sumInventoryOnHand(MAT_A);
        BigDecimal onHandBAfter = sumInventoryOnHand(MAT_B);
        assertEquals(0, new BigDecimal("4").compareTo(onHandABefore.subtract(onHandAAfter)));
        assertEquals(0, new BigDecimal("6").compareTo(onHandBBefore.subtract(onHandBAfter)));
    }

    /** issueProductionRunTaskComponent issues only the requested partial quantity, not the full BOM need. */
    public void testIssueProductionRunTaskComponentPartial() throws Exception {
        String productionRunId = createProductionRun(new BigDecimal("1"));
        GenericValue task = getProductionRunTask(productionRunId);

        // full need for MAT_A_COST at quantity=1 would be 2; issue only 1
        Map<String, Object> ctx = new HashMap<>();
        ctx.put("userLogin", userLogin);
        ctx.put("workEffortId", task.getString("workEffortId"));
        ctx.put("productId", MAT_A);
        ctx.put("quantity", new BigDecimal("1"));
        Map<String, Object> result = dispatcher.runSync("issueProductionRunTaskComponent", ctx);
        assertFalse("issueProductionRunTaskComponent failed: " + ServiceUtil.getErrorMessage(result), ServiceUtil.isError(result));

        BigDecimal issuedA = sumWorkEffortAssignedQuantity(task.getString("workEffortId"), MAT_A);
        assertEquals(0, new BigDecimal("1").compareTo(issuedA));
    }

    /** issueInventoryItemToWorkEffort issues a specific InventoryItem directly, without going through the BOM. */
    public void testIssueInventoryItemToWorkEffortDirect() throws Exception {
        String productionRunId = createProductionRun(new BigDecimal("1"));
        GenericValue task = getProductionRunTask(productionRunId);

        GenericValue inventoryItem = null;
        for (GenericValue candidate : EntityQuery.use(delegator).from("InventoryItem")
                .where("productId", MAT_A, "facilityId", FACILITY_ID, "inventoryItemTypeId", "NON_SERIAL_INV_ITEM")
                .queryList()) {
            BigDecimal atp = candidate.getBigDecimal("availableToPromiseTotal");
            if (atp != null && atp.compareTo(BigDecimal.ZERO) > 0) {
                inventoryItem = candidate;
                break;
            }
        }
        assertNotNull("No InventoryItem with available stock found for " + MAT_A, inventoryItem);
        BigDecimal atpBefore = inventoryItem.getBigDecimal("availableToPromiseTotal");

        Map<String, Object> ctx = new HashMap<>();
        ctx.put("userLogin", userLogin);
        ctx.put("workEffortId", task.getString("workEffortId"));
        ctx.put("inventoryItem", inventoryItem);
        ctx.put("quantity", new BigDecimal("1"));
        Map<String, Object> result = dispatcher.runSync("issueInventoryItemToWorkEffort", ctx);
        assertFalse("issueInventoryItemToWorkEffort failed: " + ServiceUtil.getErrorMessage(result), ServiceUtil.isError(result));
        assertEquals(0, BigDecimal.ONE.compareTo((BigDecimal) result.get("quantityIssued")));
        assertEquals(MAT_A, result.get("finishedProductId"));

        GenericValue refreshed = EntityQuery.use(delegator).from("InventoryItem")
                .where("inventoryItemId", inventoryItem.getString("inventoryItemId")).queryOne();
        assertEquals(0, atpBefore.subtract(BigDecimal.ONE).compareTo(refreshed.getBigDecimal("availableToPromiseTotal")));

        BigDecimal issuedA = sumWorkEffortAssignedQuantity(task.getString("workEffortId"), MAT_A);
        assertEquals(0, BigDecimal.ONE.compareTo(issuedA));
    }

    /** issueProductionRunTask fails with failIfItemsAreNotAvailable when the required quantity exceeds stock on hand. */
    public void testIssueProductionRunTaskFailsWhenNotAvailable() throws Exception {
        String productionRunId = createProductionRun(new BigDecimal("1000000"));
        GenericValue task = getProductionRunTask(productionRunId);

        Map<String, Object> ctx = new HashMap<>();
        ctx.put("userLogin", userLogin);
        ctx.put("workEffortId", task.getString("workEffortId"));
        ctx.put("failIfItemsAreNotAvailable", "Y");
        Map<String, Object> result = dispatcher.runSync("issueProductionRunTask", ctx);
        assertTrue("issueProductionRunTask should have failed due to insufficient stock", ServiceUtil.isError(result));
    }

    // ==================== createProductionRunPartyAssign / createProductionRunAssoc ====================

    public void testCreateProductionRunPartyAssign() throws Exception {
        String productionRunId = createProductionRun(new BigDecimal("1"));

        Map<String, Object> ctx = new HashMap<>();
        ctx.put("userLogin", userLogin);
        ctx.put("productionRunId", productionRunId);
        ctx.put("partyId", "TestManufAdmin");
        ctx.put("roleTypeId", "CAL_ATTENDEE");
        Map<String, Object> result = dispatcher.runSync("createProductionRunPartyAssign", ctx);
        assertFalse("createProductionRunPartyAssign failed: " + ServiceUtil.getErrorMessage(result), ServiceUtil.isError(result));
        assertEquals(productionRunId, result.get("productionRunId"));

        GenericValue assignment = EntityQuery.use(delegator).from("WorkEffortPartyAssignment")
                .where("workEffortId", productionRunId, "partyId", "TestManufAdmin", "roleTypeId", "CAL_ATTENDEE")
                .queryFirst();
        assertNotNull(assignment);
        assertEquals("PRTYASGN_ASSIGNED", assignment.getString("statusId"));
    }

    public void testCreateProductionRunAssoc() throws Exception {
        String productionRunId1 = createProductionRun(new BigDecimal("1"));
        String productionRunId2 = createProductionRun(new BigDecimal("1"));

        Map<String, Object> ctx = new HashMap<>();
        ctx.put("userLogin", userLogin);
        ctx.put("productionRunId", productionRunId1);
        ctx.put("productionRunIdTo", productionRunId2);
        ctx.put("workFlowSequenceTypeId", "WF_SUCCESSOR");
        Map<String, Object> result = dispatcher.runSync("createProductionRunAssoc", ctx);
        assertFalse("createProductionRunAssoc failed: " + ServiceUtil.getErrorMessage(result), ServiceUtil.isError(result));

        GenericValue assoc = EntityQuery.use(delegator).from("WorkEffortAssoc")
                .where("workEffortIdFrom", productionRunId1, "workEffortIdTo", productionRunId2,
                        "workEffortAssocTypeId", "WORK_EFF_PRECEDENCY")
                .queryFirst();
        assertNotNull(assoc);
    }

    // ==================== ProductionRunSimpleEvents logic methods ====================

    public void testCreateProductionRunEventLogic() {
        Locale locale = Locale.US;
        Map<String, Object> params = new HashMap<>();
        params.put("productId", PRODUCT_ID);
        params.put("quantity", "3");
        Map<String, Object> result = ProductionRunSimpleEvents.createProductionRun(delegator, dispatcher, userLogin, params, locale);
        assertEquals("createProductionRunSingle", result.get("responseCode"));
        assertEquals(0, new BigDecimal("3").compareTo((BigDecimal) result.get("pRQuantity")));

        Map<String, Object> params2 = new HashMap<>();
        params2.put("productId", PRODUCT_ID);
        params2.put("quantity", "3");
        params2.put("createDependentProductionRuns", "Y");
        Map<String, Object> result2 = ProductionRunSimpleEvents.createProductionRun(delegator, dispatcher, userLogin, params2, locale);
        assertEquals("createProductionRunsForProductBom", result2.get("responseCode"));
    }

    @SuppressWarnings("unchecked")
    public void testAddProductionRunRoutingTaskEventLogicValidation() {
        Locale locale = Locale.US;
        Map<String, Object> params = new HashMap<>();
        // routingTaskId and priority intentionally omitted -> validation errors
        Map<String, Object> result = ProductionRunSimpleEvents.addProductionRunRoutingTask(delegator, dispatcher, userLogin, params, locale);
        assertEquals("error", result.get("responseCode"));
        List<String> errorMessages = (List<String>) result.get("errorMessageList");
        assertNotNull(errorMessages);
        assertFalse(errorMessages.isEmpty());
    }

    public void testEditProductionRunRoutingTaskEventLogic() throws Exception {
        String productionRunId = createProductionRun(new BigDecimal("1"));
        GenericValue task = getProductionRunTask(productionRunId);
        Locale locale = Locale.US;

        Map<String, Object> params = new HashMap<>();
        params.put("productionRunId", productionRunId);
        params.put("routingTaskId", task.getString("workEffortId"));
        params.put("priority", "5");
        params.put("estimatedStartDate", task.get("estimatedStartDate"));
        // checkUpdatePrunRoutingTask requires these (not optional in its service definition)
        params.put("estimatedSetupMillis", task.get("estimatedSetupMillis"));
        params.put("estimatedMilliSeconds", task.get("estimatedMilliSeconds"));
        Map<String, Object> result = ProductionRunSimpleEvents.editProductionRunRoutingTask(delegator, dispatcher, userLogin, params, locale);
        assertEquals("success: " + result.get("errorMessage") + " " + result.get("errorMessageList"), "success", result.get("responseCode"));

        // missing required estimatedStartDate -> validation error
        Map<String, Object> badParams = new HashMap<>();
        badParams.put("productionRunId", productionRunId);
        badParams.put("routingTaskId", task.getString("workEffortId"));
        badParams.put("priority", "5");
        Map<String, Object> badResult = ProductionRunSimpleEvents.editProductionRunRoutingTask(delegator, dispatcher, userLogin, badParams, locale);
        assertEquals("error", badResult.get("responseCode"));
    }
}
