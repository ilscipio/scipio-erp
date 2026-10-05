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

import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.ofbiz.entity.GenericValue;
import org.ofbiz.entity.util.EntityQuery;
import org.ofbiz.service.ServiceUtil;
import org.ofbiz.service.testtools.OFBizTestCase;

/**
 * SCIPIO: Tests that executeMrp writes an MrpRun header with counts.
 */
public class MrpRunTest extends OFBizTestCase {

    private GenericValue userLogin;

    public MrpRunTest(String name) {
        super(name);
    }

    @Override
    protected void setUp() throws Exception {
        userLogin = EntityQuery.use(delegator).from("UserLogin").where("userLoginId", "system").queryOne();
    }

    public void testExecuteMrpCreatesRunHeader() throws Exception {
        Map<String, Object> ctx = new HashMap<>();
        ctx.put("mrpName", "MFT run");
        ctx.put("facilityId", "ScipioShopWarehouse");
        ctx.put("defaultYearsOffset", 1);
        ctx.put("userLogin", userLogin);
        Map<String, Object> result = dispatcher.runSync("executeMrp", ctx);
        assertFalse(ServiceUtil.getErrorMessage(result), ServiceUtil.isError(result));
        String mrpId = (String) result.get("mrpId");
        assertNotNull("mrpId returned", mrpId);
        GenericValue run = EntityQuery.use(delegator).from("MrpRun").where("mrpId", mrpId).queryOne();
        assertNotNull("MrpRun row", run);
        assertEquals("MRP_FINISHED", run.getString("statusId"));
        assertEquals("MFT run", run.getString("mrpName"));
        assertEquals("ScipioShopWarehouse", run.getString("facilityId"));
        assertNotNull(run.getTimestamp("startDate"));
        assertNotNull(run.getTimestamp("finishDate"));
        assertEquals("system", run.getString("runByUserLoginId"));
        long events = EntityQuery.use(delegator).from("MrpEvent").where("mrpId", mrpId).queryCount();
        assertEquals(Long.valueOf(events), run.getLong("eventCount"));
        long errors = EntityQuery.use(delegator).from("MrpEvent").where("mrpId", mrpId, "mrpEventTypeId", "ERROR").queryCount();
        assertEquals(Long.valueOf(errors), run.getLong("errorCount"));
        List<GenericValue> related = run.getRelated("MrpEvent", null, null, false);
        assertEquals(events, related.size());
    }

    public void testExecuteMrpWithoutFacilityRecordsFailure() throws Exception {
        long before = EntityQuery.use(delegator).from("MrpRun").where("statusId", "MRP_FAILED").queryCount();
        Map<String, Object> ctx = new HashMap<>();
        ctx.put("mrpName", "MFT fail");
        ctx.put("userLogin", userLogin);
        Map<String, Object> result = dispatcher.runSync("executeMrp", ctx, 60, true);
        assertTrue(ServiceUtil.isError(result));
        long after = EntityQuery.use(delegator).from("MrpRun").where("statusId", "MRP_FAILED").queryCount();
        assertEquals(before + 1, after);
    }
}
