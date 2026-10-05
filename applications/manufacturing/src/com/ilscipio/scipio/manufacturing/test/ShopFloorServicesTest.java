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

import org.ofbiz.base.util.UtilDateTime;
import org.ofbiz.base.util.UtilGenerics;
import org.ofbiz.entity.GenericValue;
import org.ofbiz.entity.util.EntityQuery;
import org.ofbiz.service.ServiceUtil;
import org.ofbiz.service.testtools.OFBizTestCase;

/**
 * Shop floor services tests: production run task reject declaration and shop floor task listing.
 *
 * <p>SCIPIO: 4.0.0: New.</p>
 */
public class ShopFloorServicesTest extends OFBizTestCase {

    protected GenericValue userLogin = null;

    public ShopFloorServicesTest(String name) {
        super(name);
    }

    @Override
    protected void setUp() throws Exception {
        userLogin = EntityQuery.use(delegator).from("UserLogin").where("userLoginId", "system").queryOne();
    }

    @Override
    protected void tearDown() throws Exception {
    }

    public void testDeclareRejectAndShopFloorTasks() throws Exception {
        // Create a production run for the demo manufactured product
        Map<String, Object> createCtx = new HashMap<>();
        createCtx.put("userLogin", userLogin);
        createCtx.put("productId", "PROD_MANUF");
        createCtx.put("pRQuantity", new BigDecimal("3"));
        createCtx.put("startDate", UtilDateTime.nowTimestamp());
        createCtx.put("facilityId", "ScipioShopWarehouse");
        createCtx.put("routingId", "ROUTING_COST");
        Map<String, Object> createResult = dispatcher.runSync("createProductionRun", createCtx);
        assertFalse(ServiceUtil.getErrorMessage(createResult), ServiceUtil.isError(createResult));
        String productionRunId = (String) createResult.get("productionRunId");
        assertNotNull(productionRunId);

        // Move the run to PRUN_DOC_PRINTED
        Map<String, Object> docPrintedCtx = new HashMap<>();
        docPrintedCtx.put("userLogin", userLogin);
        docPrintedCtx.put("productionRunId", productionRunId);
        docPrintedCtx.put("statusId", "PRUN_DOC_PRINTED");
        Map<String, Object> docPrintedResult = dispatcher.runSync("changeProductionRunStatus", docPrintedCtx);
        assertFalse(ServiceUtil.getErrorMessage(docPrintedResult), ServiceUtil.isError(docPrintedResult));

        // Find the first routing task by priority
        List<GenericValue> runTasks = EntityQuery.use(delegator).from("WorkEffort")
                .where("workEffortParentId", productionRunId, "workEffortTypeId", "PROD_ORDER_TASK")
                .orderBy("priority")
                .queryList();
        assertFalse(runTasks.isEmpty());
        String taskId = runTasks.get(0).getString("workEffortId");

        // changeProductionRunStatus alone does not start the run's tasks; start the first task explicitly.
        // This also moves the run header from PRUN_DOC_PRINTED to PRUN_RUNNING.
        Map<String, Object> startCtx = new HashMap<>();
        startCtx.put("userLogin", userLogin);
        startCtx.put("productionRunId", productionRunId);
        startCtx.put("workEffortId", taskId);
        startCtx.put("statusId", "PRUN_RUNNING");
        Map<String, Object> startResult = dispatcher.runSync("changeProductionRunTaskStatus", startCtx);
        assertFalse(ServiceUtil.getErrorMessage(startResult), ServiceUtil.isError(startResult));

        // getShopFloorTasks should list the running task with canDeclare true
        Map<String, Object> shopFloorCtx = new HashMap<>();
        shopFloorCtx.put("userLogin", userLogin);
        shopFloorCtx.put("facilityId", "ScipioShopWarehouse");
        Map<String, Object> shopFloorResult = dispatcher.runSync("getShopFloorTasks", shopFloorCtx);
        assertFalse(ServiceUtil.getErrorMessage(shopFloorResult), ServiceUtil.isError(shopFloorResult));
        List<Map<String, Object>> shopFloorTasks = UtilGenerics.cast(shopFloorResult.get("tasks"));
        Map<String, Object> shopFloorTask = null;
        for (Map<String, Object> row : shopFloorTasks) {
            if (taskId.equals(row.get("workEffortId"))) {
                shopFloorTask = row;
                break;
            }
        }
        assertNotNull(shopFloorTask);
        assertEquals(Boolean.TRUE, shopFloorTask.get("canDeclare"));

        // Declare a reject of 1 with a machine fault reason
        Map<String, Object> rejectCtx = new HashMap<>();
        rejectCtx.put("userLogin", userLogin);
        rejectCtx.put("productionRunId", productionRunId);
        rejectCtx.put("workEffortId", taskId);
        rejectCtx.put("quantity", new BigDecimal("1"));
        rejectCtx.put("reasonEnumId", "PRUN_REJ_MACHINE");
        Map<String, Object> rejectResult = dispatcher.runSync("declareProductionRunTaskReject", rejectCtx);
        assertFalse(ServiceUtil.getErrorMessage(rejectResult), ServiceUtil.isError(rejectResult));
        String rejectSeqId = (String) rejectResult.get("rejectSeqId");
        assertNotNull(rejectSeqId);

        GenericValue rejectEntity = EntityQuery.use(delegator).from("ProductionRunReject")
                .where("workEffortId", taskId, "rejectSeqId", rejectSeqId).queryOne();
        assertNotNull(rejectEntity);
        assertEquals(0, new BigDecimal("1").compareTo(rejectEntity.getBigDecimal("quantity")));

        GenericValue updatedTask = EntityQuery.use(delegator).from("WorkEffort").where("workEffortId", taskId).queryOne();
        assertEquals(0, new BigDecimal("1").compareTo(updatedTask.getBigDecimal("quantityRejected")));

        // getProductionRunRejects should return the row with a reason description
        Map<String, Object> rejectsCtx = new HashMap<>();
        rejectsCtx.put("userLogin", userLogin);
        rejectsCtx.put("productionRunId", productionRunId);
        Map<String, Object> rejectsResult = dispatcher.runSync("getProductionRunRejects", rejectsCtx);
        assertFalse(ServiceUtil.getErrorMessage(rejectsResult), ServiceUtil.isError(rejectsResult));
        List<Map<String, Object>> rejects = UtilGenerics.cast(rejectsResult.get("rejects"));
        Map<String, Object> foundReject = null;
        for (Map<String, Object> row : rejects) {
            if (rejectSeqId.equals(row.get("rejectSeqId"))) {
                foundReject = row;
                break;
            }
        }
        assertNotNull(foundReject);
        assertEquals("Machine fault", foundReject.get("reasonDescription"));

        // Error path: quantity must be positive
        Map<String, Object> badQtyCtx = new HashMap<>();
        badQtyCtx.put("userLogin", userLogin);
        badQtyCtx.put("productionRunId", productionRunId);
        badQtyCtx.put("workEffortId", taskId);
        badQtyCtx.put("quantity", BigDecimal.ZERO);
        badQtyCtx.put("reasonEnumId", "PRUN_REJ_MACHINE");
        Map<String, Object> badQtyResult = dispatcher.runSync("declareProductionRunTaskReject", badQtyCtx);
        assertTrue(ServiceUtil.isError(badQtyResult));

        // Error path: unknown reason
        Map<String, Object> badReasonCtx = new HashMap<>();
        badReasonCtx.put("userLogin", userLogin);
        badReasonCtx.put("productionRunId", productionRunId);
        badReasonCtx.put("workEffortId", taskId);
        badReasonCtx.put("quantity", BigDecimal.ONE);
        badReasonCtx.put("reasonEnumId", "PRUN_REJ_UNKNOWN");
        Map<String, Object> badReasonResult = dispatcher.runSync("declareProductionRunTaskReject", badReasonCtx);
        assertTrue(ServiceUtil.isError(badReasonResult));
    }
}
