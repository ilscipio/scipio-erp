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
import java.util.Map;

import org.ofbiz.base.util.UtilDateTime;
import org.ofbiz.base.util.UtilGenerics;
import org.ofbiz.entity.GenericValue;
import org.ofbiz.entity.util.EntityQuery;
import org.ofbiz.service.ServiceUtil;
import org.ofbiz.service.testtools.OFBizTestCase;

/**
 * SCIPIO: Tests for the getWorkCenterLoad and getManufacturingDashboard planning services.
 */
public class PlanningServicesTest extends OFBizTestCase {

    private GenericValue userLogin;

    public PlanningServicesTest(String name) {
        super(name);
    }

    @Override
    protected void setUp() throws Exception {
        userLogin = EntityQuery.use(delegator).from("UserLogin").where("userLoginId", "system").queryOne();
    }

    public void testWorkCenterLoadDemoCalendar() throws Exception {
        Timestamp fromDate = Timestamp.valueOf("2026-03-02 00:00:00.0");
        Timestamp saturday = Timestamp.valueOf("2026-03-07 00:00:00.0");

        // Create a demo production run against WORKCENTER_COST, whose FixedAsset has no calendarId,
        // so it falls back to the DEFAULT calendar (Mon-Fri 08:30, 8 hours = 480 minutes/day).
        Map<String, Object> cprCtx = new HashMap<>();
        cprCtx.put("productId", "PROD_MANUF");
        cprCtx.put("pRQuantity", new BigDecimal("2"));
        cprCtx.put("startDate", fromDate);
        cprCtx.put("facilityId", "ScipioShopWarehouse");
        cprCtx.put("routingId", "ROUTING_COST");
        cprCtx.put("userLogin", userLogin);
        Map<String, Object> cprResult = dispatcher.runSync("createProductionRun", cprCtx);
        assertFalse(ServiceUtil.getErrorMessage(cprResult), ServiceUtil.isError(cprResult));
        String productionRunId = (String) cprResult.get("productionRunId");
        assertNotNull("productionRunId returned", productionRunId);

        Map<String, Object> wclCtx = new HashMap<>();
        wclCtx.put("fixedAssetId", "WORKCENTER_COST");
        wclCtx.put("fromDate", fromDate);
        wclCtx.put("thruDate", UtilDateTime.addDaysToTimestamp(fromDate, 14));
        wclCtx.put("userLogin", userLogin);
        Map<String, Object> wclResult = dispatcher.runSync("getWorkCenterLoad", wclCtx);
        assertFalse(ServiceUtil.getErrorMessage(wclResult), ServiceUtil.isError(wclResult));

        List<Map<String, Object>> loadRows = UtilGenerics.cast(wclResult.get("loadRows"));
        assertNotNull("loadRows returned", loadRows);
        boolean foundMonday = false;
        boolean foundSaturday = false;
        for (Map<String, Object> row : loadRows) {
            Timestamp day = (Timestamp) row.get("day");
            Double capacityMinutes = (Double) row.get("capacityMinutes");
            if (day.equals(fromDate)) {
                assertEquals("Monday capacity minutes", Double.valueOf(480.0), capacityMinutes);
                foundMonday = true;
            }
            if (day.equals(saturday)) {
                assertEquals("Saturday capacity minutes", Double.valueOf(0.0), capacityMinutes);
                foundSaturday = true;
            }
        }
        assertTrue("Monday load row found", foundMonday);
        assertTrue("Saturday load row found", foundSaturday);

        List<Map<String, Object>> tasks = UtilGenerics.cast(wclResult.get("tasks"));
        assertNotNull("tasks returned", tasks);
        Map<String, Object> createdTask = null;
        for (Map<String, Object> task : tasks) {
            if (productionRunId.equals(task.get("productionRunId"))) {
                createdTask = task;
                break;
            }
        }
        assertNotNull("created run task found in tasks", createdTask);
        Double taskLoadMinutes = (Double) createdTask.get("loadMinutes");
        assertNotNull(taskLoadMinutes);
        assertTrue("task loadMinutes > 0", taskLoadMinutes > 0.0);

        Timestamp taskStartDate = (Timestamp) createdTask.get("estimatedStartDate");
        assertNotNull("task estimatedStartDate set", taskStartDate);
        Timestamp taskStartDay = UtilDateTime.getDayStart(taskStartDate);
        Double taskDayLoadMinutes = null;
        for (Map<String, Object> row : loadRows) {
            if ("WORKCENTER_COST".equals(row.get("fixedAssetId")) && taskStartDay.equals(row.get("day"))) {
                taskDayLoadMinutes = (Double) row.get("loadMinutes");
                break;
            }
        }
        assertNotNull("load row for task start day found", taskDayLoadMinutes);
        assertTrue("load row for task start day has loadMinutes > 0", taskDayLoadMinutes > 0.0);
    }

    public void testDashboard() throws Exception {
        Map<String, Object> cprCtx = new HashMap<>();
        cprCtx.put("productId", "PROD_MANUF");
        cprCtx.put("pRQuantity", new BigDecimal("2"));
        cprCtx.put("startDate", UtilDateTime.addDaysToTimestamp(UtilDateTime.nowTimestamp(), 1));
        cprCtx.put("facilityId", "ScipioShopWarehouse");
        cprCtx.put("routingId", "ROUTING_COST");
        cprCtx.put("userLogin", userLogin);
        Map<String, Object> cprResult = dispatcher.runSync("createProductionRun", cprCtx);
        assertFalse(ServiceUtil.getErrorMessage(cprResult), ServiceUtil.isError(cprResult));
        String productionRunId = (String) cprResult.get("productionRunId");
        assertNotNull("productionRunId returned", productionRunId);

        Map<String, Object> ctx = new HashMap<>();
        ctx.put("userLogin", userLogin);
        Map<String, Object> result = dispatcher.runSync("getManufacturingDashboard", ctx);
        assertFalse(ServiceUtil.getErrorMessage(result), ServiceUtil.isError(result));

        Map<String, Object> runCounts = UtilGenerics.cast(result.get("runCounts"));
        assertNotNull("runCounts returned", runCounts);
        Long total = (Long) runCounts.get("total");
        assertNotNull("runCounts.total present", total);
        assertTrue("runCounts.total >= 1", total >= 1);

        List<Map<String, Object>> lateRuns = UtilGenerics.cast(result.get("lateRuns"));
        List<Map<String, Object>> upcomingRuns = UtilGenerics.cast(result.get("upcomingRuns"));
        assertTrue("created run appears in upcomingRuns or lateRuns",
                containsRun(lateRuns, productionRunId) || containsRun(upcomingRuns, productionRunId));

        Object shortages = result.get("shortages");
        assertTrue("shortages is a list", shortages instanceof List);

        Map<String, Object> mrpProposals = UtilGenerics.cast(result.get("mrpProposals"));
        assertNotNull("mrpProposals returned", mrpProposals);
        assertTrue("mrpProposals has productionRuns key", mrpProposals.containsKey("productionRuns"));
        assertTrue("mrpProposals has purchases key", mrpProposals.containsKey("purchases"));
    }

    private static boolean containsRun(List<Map<String, Object>> runs, String workEffortId) {
        if (runs == null) {
            return false;
        }
        for (Map<String, Object> run : runs) {
            if (workEffortId.equals(run.get("workEffortId"))) {
                return true;
            }
        }
        return false;
    }
}
