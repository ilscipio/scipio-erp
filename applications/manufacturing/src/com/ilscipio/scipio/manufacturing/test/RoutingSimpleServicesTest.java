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
/*
 * SCIPIO: Test coverage for the hand-written RoutingSimpleServices / RoutingServices / RoutingSimpleEvents
 * replacements of the former RoutingSimpleServices.xml / RoutingServices.xml / RoutingSimpleEvents.xml.
 */
package com.ilscipio.scipio.manufacturing.test;

import java.net.URL;
import java.sql.Timestamp;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;

import org.ofbiz.base.location.FlexibleLocation;
import org.ofbiz.base.util.UtilGenerics;
import org.ofbiz.entity.GenericValue;
import org.ofbiz.entity.util.EntityQuery;
import org.ofbiz.entity.util.EntitySaxReader;
import org.ofbiz.service.ServiceUtil;
import org.ofbiz.service.testtools.OFBizTestCase;

import com.ilscipio.scipio.manufacturing.event.RoutingSimpleEvents;

/**
 * Tests for the manufacturing routing calendar services, routing lookup services and routing task assoc events.
 */
public class RoutingSimpleServicesTest extends OFBizTestCase {

    protected GenericValue userLogin = null;
    protected Locale locale = Locale.getDefault();

    public RoutingSimpleServicesTest(String name) {
        super(name);
    }

    @Override
    protected void setUp() throws Exception {
        userLogin = EntityQuery.use(delegator).from("UserLogin").where("userLoginId", "system").queryOne();
        URL dataUrl = FlexibleLocation.resolveLocation("component://manufacturing/testdef/data/RoutingTestData.xml");
        new EntitySaxReader(delegator).parse(dataUrl);
    }

    @Override
    protected void tearDown() throws Exception {
        // SCIPIO: best-effort cleanup of the rows created by the create-oriented tests below, so the fixture
        // (which is re-loaded, create-update, on every setUp) stays reusable across repeated test runs.
        safeRemoveByAnd("TechDataCalendar", "calendarId", "MFT_CAL_NEW");
        safeRemoveByAnd("TechDataCalendarWeek", "calendarWeekId", "MFT_CAL_WEEK_NEW");
        safeRemoveByAnd("TechDataCalendarExcDay", "calendarId", "MFT_CAL_EXC", "exceptionDateStartTime", Timestamp.valueOf("2030-06-01 08:00:00.0"));
        safeRemoveByAnd("TechDataCalendarExcWeek", "calendarId", "MFT_CAL_EXC", "exceptionDateStart", java.sql.Date.valueOf("2030-07-01"));
        safeRemoveByAnd("WorkEffortAssoc", "workEffortIdFrom", "MFT_ROUTING_01", "workEffortIdTo", "MFT_TASK_03");
    }

    private void safeRemoveByAnd(String entityName, Object... fields) {
        try {
            delegator.removeByAnd(entityName, fields);
        } catch (Exception e) {
            // ignore: best-effort cleanup only
        }
    }

    public void testCalendarCrud() throws Exception {
        Map<String, Object> createCtx = new HashMap<>();
        createCtx.put("calendarId", "MFT_CAL_NEW");
        createCtx.put("calendarWeekId", "MFT_CAL_WEEK_01");
        createCtx.put("description", "MFT Test Calendar (created)");
        createCtx.put("userLogin", userLogin);
        createCtx.put("locale", locale);
        Map<String, Object> createResult = dispatcher.runSync("createCalendar", createCtx);
        assertFalse("createCalendar should succeed: " + ServiceUtil.getErrorMessage(createResult), ServiceUtil.isError(createResult));

        Map<String, Object> updateCtx = new HashMap<>();
        updateCtx.put("calendarId", "MFT_CAL_UPD");
        updateCtx.put("description", "MFT Test Calendar (updated)");
        updateCtx.put("userLogin", userLogin);
        updateCtx.put("locale", locale);
        Map<String, Object> updateResult = dispatcher.runSync("updateCalendar", updateCtx);
        assertFalse("updateCalendar should succeed: " + ServiceUtil.getErrorMessage(updateResult), ServiceUtil.isError(updateResult));
        GenericValue updated = EntityQuery.use(delegator).from("TechDataCalendar").where("calendarId", "MFT_CAL_UPD").queryOne();
        assertEquals("MFT Test Calendar (updated)", updated.getString("description"));

        Map<String, Object> removeCtx = new HashMap<>();
        removeCtx.put("calendarId", "MFT_CAL_DEL");
        removeCtx.put("userLogin", userLogin);
        removeCtx.put("locale", locale);
        Map<String, Object> removeResult = dispatcher.runSync("removeCalendar", removeCtx);
        assertFalse("removeCalendar should succeed: " + ServiceUtil.getErrorMessage(removeResult), ServiceUtil.isError(removeResult));
        GenericValue removed = EntityQuery.use(delegator).from("TechDataCalendar").where("calendarId", "MFT_CAL_DEL").queryOne();
        assertNull(removed);
    }

    public void testCalendarWeekCrud() throws Exception {
        Map<String, Object> createCtx = new HashMap<>();
        createCtx.put("calendarWeekId", "MFT_CAL_WEEK_NEW");
        createCtx.put("description", "MFT Test Calendar Week (created)");
        createCtx.put("userLogin", userLogin);
        createCtx.put("locale", locale);
        Map<String, Object> createResult = dispatcher.runSync("createCalendarWeek", createCtx);
        assertFalse("createCalendarWeek should succeed: " + ServiceUtil.getErrorMessage(createResult), ServiceUtil.isError(createResult));

        Map<String, Object> updateCtx = new HashMap<>();
        updateCtx.put("calendarWeekId", "MFT_CAL_WEEK_UPD");
        updateCtx.put("description", "MFT Test Calendar Week (updated)");
        updateCtx.put("userLogin", userLogin);
        updateCtx.put("locale", locale);
        Map<String, Object> updateResult = dispatcher.runSync("updateCalendarWeek", updateCtx);
        assertFalse("updateCalendarWeek should succeed: " + ServiceUtil.getErrorMessage(updateResult), ServiceUtil.isError(updateResult));
        GenericValue updated = EntityQuery.use(delegator).from("TechDataCalendarWeek").where("calendarWeekId", "MFT_CAL_WEEK_UPD").queryOne();
        assertEquals("MFT Test Calendar Week (updated)", updated.getString("description"));

        Map<String, Object> removeCtx = new HashMap<>();
        removeCtx.put("calendarWeekId", "MFT_CAL_WEEK_DEL");
        removeCtx.put("userLogin", userLogin);
        removeCtx.put("locale", locale);
        Map<String, Object> removeResult = dispatcher.runSync("removeCalendarWeek", removeCtx);
        assertFalse("removeCalendarWeek should succeed: " + ServiceUtil.getErrorMessage(removeResult), ServiceUtil.isError(removeResult));
        GenericValue removed = EntityQuery.use(delegator).from("TechDataCalendarWeek").where("calendarWeekId", "MFT_CAL_WEEK_DEL").queryOne();
        assertNull(removed);
    }

    public void testCalendarExceptionDayCrud() throws Exception {
        Timestamp createStart = Timestamp.valueOf("2030-06-01 08:00:00.0");
        Map<String, Object> createCtx = new HashMap<>();
        createCtx.put("calendarId", "MFT_CAL_EXC");
        createCtx.put("exceptionDateStartTime", createStart);
        createCtx.put("description", "MFT Test Exception Day (created)");
        createCtx.put("userLogin", userLogin);
        createCtx.put("locale", locale);
        Map<String, Object> createResult = dispatcher.runSync("createCalendarExceptionDay", createCtx);
        assertFalse("createCalendarExceptionDay should succeed: " + ServiceUtil.getErrorMessage(createResult), ServiceUtil.isError(createResult));

        Map<String, Object> updateCtx = new HashMap<>();
        updateCtx.put("calendarId", "MFT_CAL_EXC");
        updateCtx.put("exceptionDateStartTime", Timestamp.valueOf("2030-06-02 08:00:00.0"));
        updateCtx.put("description", "MFT Test Exception Day (updated)");
        updateCtx.put("userLogin", userLogin);
        updateCtx.put("locale", locale);
        Map<String, Object> updateResult = dispatcher.runSync("updateCalendarExceptionDay", updateCtx);
        assertFalse("updateCalendarExceptionDay should succeed: " + ServiceUtil.getErrorMessage(updateResult), ServiceUtil.isError(updateResult));

        Map<String, Object> removeCtx = new HashMap<>();
        removeCtx.put("calendarId", "MFT_CAL_EXC");
        removeCtx.put("exceptionDateStartTime", Timestamp.valueOf("2030-06-03 08:00:00.0"));
        removeCtx.put("userLogin", userLogin);
        removeCtx.put("locale", locale);
        Map<String, Object> removeResult = dispatcher.runSync("removeCalendarExceptionDay", removeCtx);
        assertFalse("removeCalendarExceptionDay should succeed: " + ServiceUtil.getErrorMessage(removeResult), ServiceUtil.isError(removeResult));
    }

    public void testCalendarExceptionWeekCrud() throws Exception {
        Map<String, Object> createCtx = new HashMap<>();
        createCtx.put("calendarId", "MFT_CAL_EXC");
        createCtx.put("exceptionDateStart", java.sql.Date.valueOf("2030-07-01"));
        createCtx.put("calendarWeekId", "MFT_CAL_WEEK_01");
        createCtx.put("description", "MFT Test Exception Week (created)");
        createCtx.put("userLogin", userLogin);
        createCtx.put("locale", locale);
        Map<String, Object> createResult = dispatcher.runSync("createCalendarExceptionWeek", createCtx);
        assertFalse("createCalendarExceptionWeek should succeed: " + ServiceUtil.getErrorMessage(createResult), ServiceUtil.isError(createResult));

        Map<String, Object> updateCtx = new HashMap<>();
        updateCtx.put("calendarId", "MFT_CAL_EXC");
        updateCtx.put("exceptionDateStart", java.sql.Date.valueOf("2030-07-02"));
        updateCtx.put("description", "MFT Test Exception Week (updated)");
        updateCtx.put("userLogin", userLogin);
        updateCtx.put("locale", locale);
        Map<String, Object> updateResult = dispatcher.runSync("updateCalendarExceptionWeek", updateCtx);
        assertFalse("updateCalendarExceptionWeek should succeed: " + ServiceUtil.getErrorMessage(updateResult), ServiceUtil.isError(updateResult));

        Map<String, Object> removeCtx = new HashMap<>();
        removeCtx.put("calendarId", "MFT_CAL_EXC");
        removeCtx.put("exceptionDateStart", java.sql.Date.valueOf("2030-07-03"));
        removeCtx.put("userLogin", userLogin);
        removeCtx.put("locale", locale);
        Map<String, Object> removeResult = dispatcher.runSync("removeCalendarExceptionWeek", removeCtx);
        assertFalse("removeCalendarExceptionWeek should succeed: " + ServiceUtil.getErrorMessage(removeResult), ServiceUtil.isError(removeResult));
    }

    public void testGetProductRoutingExplicit() throws Exception {
        Map<String, Object> ctx = new HashMap<>();
        ctx.put("productId", "MFT_PROD_ROUTED");
        ctx.put("workEffortId", "MFT_ROUTING_01");
        ctx.put("userLogin", userLogin);
        ctx.put("locale", locale);
        Map<String, Object> result = dispatcher.runSync("getProductRouting", ctx);
        assertFalse("getProductRouting should succeed: " + ServiceUtil.getErrorMessage(result), ServiceUtil.isError(result));
        GenericValue routing = (GenericValue) result.get("routing");
        assertNotNull("Explicit routing should be found", routing);
        assertEquals("MFT_ROUTING_01", routing.getString("workEffortId"));
        List<GenericValue> tasks = UtilGenerics.cast(result.get("tasks"));
        assertNotNull(tasks);
        assertEquals(2, tasks.size());
    }

    public void testGetProductRoutingDefaultFallback() throws Exception {
        Map<String, Object> ctx = new HashMap<>();
        ctx.put("productId", "MFT_PROD_DEFAULT");
        ctx.put("userLogin", userLogin);
        ctx.put("locale", locale);
        Map<String, Object> result = dispatcher.runSync("getProductRouting", ctx);
        assertFalse("getProductRouting should succeed: " + ServiceUtil.getErrorMessage(result), ServiceUtil.isError(result));
        GenericValue routing = (GenericValue) result.get("routing");
        assertNotNull("DEFAULT_ROUTING fallback should be found", routing);
        assertEquals("DEFAULT_ROUTING", routing.getString("workEffortId"));
    }

    public void testGetRoutingTaskAssocs() throws Exception {
        Map<String, Object> ctx = new HashMap<>();
        ctx.put("workEffortId", "MFT_ROUTING_01");
        ctx.put("userLogin", userLogin);
        ctx.put("locale", locale);
        Map<String, Object> result = dispatcher.runSync("getRoutingTaskAssocs", ctx);
        assertFalse("getRoutingTaskAssocs should succeed: " + ServiceUtil.getErrorMessage(result), ServiceUtil.isError(result));
        List<GenericValue> assocs = UtilGenerics.cast(result.get("routingTaskAssocs"));
        assertNotNull(assocs);
        // SCIPIO: at least the two seeded assocs (seq 10, 20); testAddRoutingTaskAssoc may add a third (seq 30)
        assertTrue("Expected at least 2 routing task assocs, got " + assocs.size(), assocs.size() >= 2);
    }

    public void testAddRoutingTaskAssoc() {
        Map<String, Object> params = new HashMap<>();
        params.put("workEffortId", "MFT_ROUTING_01");
        params.put("workEffortIdTo", "MFT_TASK_03");
        params.put("workEffortAssocTypeId", "ROUTING_COMPONENT");
        params.put("sequenceNum", "30");
        params.put("copyTask", "N");

        Map<String, Object> result = RoutingSimpleEvents.addRoutingTaskAssoc(delegator, dispatcher, userLogin, params, locale);
        assertFalse("addRoutingTaskAssoc should succeed: " + ServiceUtil.getErrorMessage(result), ServiceUtil.isError(result));
    }

    public void testAddRoutingTaskAssocDuplicateSequenceNumFails() {
        // SCIPIO: sequenceNum 10 on MFT_ROUTING_01 is already used (by MFT_TASK_01, no thruDate) so adding
        // another assoc at the same sequenceNum with no thruDate must fail with the "same seq id" error.
        Map<String, Object> params = new HashMap<>();
        params.put("workEffortId", "MFT_ROUTING_01");
        params.put("workEffortIdTo", "MFT_TASK_02");
        params.put("workEffortAssocTypeId", "ROUTING_COMPONENT");
        params.put("sequenceNum", "10");
        params.put("copyTask", "N");

        Map<String, Object> result = RoutingSimpleEvents.addRoutingTaskAssoc(delegator, dispatcher, userLogin, params, locale);
        assertTrue("addRoutingTaskAssoc should fail: the task is already in the routing at this sequenceNum", ServiceUtil.isError(result));
    }

    public void testUpdateRoutingTaskAssoc() {
        // SCIPIO: pass an explicit fromDate (rather than relying on the callee's now-timestamp default) so the
        // update below can address the exact same WorkEffortAssoc primary key.
        Timestamp fromDate = Timestamp.valueOf("2029-01-01 00:00:00.0");
        Map<String, Object> addParams = new HashMap<>();
        addParams.put("workEffortId", "MFT_ROUTING_01");
        addParams.put("workEffortIdTo", "MFT_TASK_03");
        addParams.put("workEffortAssocTypeId", "ROUTING_COMPONENT");
        addParams.put("sequenceNum", "30");
        addParams.put("fromDate", fromDate);
        addParams.put("copyTask", "N");
        Map<String, Object> addResult = RoutingSimpleEvents.addRoutingTaskAssoc(delegator, dispatcher, userLogin, addParams, locale);
        assertFalse("addRoutingTaskAssoc should succeed: " + ServiceUtil.getErrorMessage(addResult), ServiceUtil.isError(addResult));

        Map<String, Object> updateParams = new HashMap<>();
        updateParams.put("workEffortId", "MFT_ROUTING_01");
        updateParams.put("workEffortIdTo", "MFT_TASK_03");
        updateParams.put("workEffortAssocTypeId", "ROUTING_COMPONENT");
        updateParams.put("sequenceNum", "30");
        updateParams.put("fromDate", fromDate);

        Map<String, Object> updateResult = RoutingSimpleEvents.updateRoutingTaskAssoc(delegator, dispatcher, userLogin, updateParams, locale);
        assertFalse("updateRoutingTaskAssoc should succeed: " + ServiceUtil.getErrorMessage(updateResult), ServiceUtil.isError(updateResult));
    }
}
