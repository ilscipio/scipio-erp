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

import java.sql.Timestamp;
import java.util.Map;

import org.ofbiz.base.util.UtilMisc;
import org.ofbiz.entity.GenericValue;
import org.ofbiz.entity.util.EntityQuery;
import org.ofbiz.manufacturing.techdata.TechDataServices;
import org.ofbiz.service.testtools.OFBizTestCase;

/**
 * SCIPIO: Tests the calendar capacity functions with exception days and exception weeks.
 */
public class TechDataCalendarTest extends OFBizTestCase {

    private static final String CAL = "MFT_CAL";
    private static final String WEEK = "MFT_WEEK";
    private static final String WEEK_HALF = "MFT_WEEK_HALF";
    private static final long HOUR = 3600000L;

    public TechDataCalendarTest(String name) {
        super(name);
    }

    @Override
    protected void setUp() throws Exception {
        cleanup();
        // Mon-Fri 08:00, 8 h
        delegator.create("TechDataCalendarWeek", UtilMisc.toMap("calendarWeekId", WEEK, "description", "test 8h",
                "mondayStartTime", java.sql.Time.valueOf("08:00:00"), "mondayCapacity", 8.0 * HOUR,
                "tuesdayStartTime", java.sql.Time.valueOf("08:00:00"), "tuesdayCapacity", 8.0 * HOUR,
                "wednesdayStartTime", java.sql.Time.valueOf("08:00:00"), "wednesdayCapacity", 8.0 * HOUR,
                "thursdayStartTime", java.sql.Time.valueOf("08:00:00"), "thursdayCapacity", 8.0 * HOUR,
                "fridayStartTime", java.sql.Time.valueOf("08:00:00"), "fridayCapacity", 8.0 * HOUR));
        // Mon-Fri 08:00, 4 h
        delegator.create("TechDataCalendarWeek", UtilMisc.toMap("calendarWeekId", WEEK_HALF, "description", "test 4h",
                "mondayStartTime", java.sql.Time.valueOf("08:00:00"), "mondayCapacity", 4.0 * HOUR,
                "tuesdayStartTime", java.sql.Time.valueOf("08:00:00"), "tuesdayCapacity", 4.0 * HOUR,
                "wednesdayStartTime", java.sql.Time.valueOf("08:00:00"), "wednesdayCapacity", 4.0 * HOUR,
                "thursdayStartTime", java.sql.Time.valueOf("08:00:00"), "thursdayCapacity", 4.0 * HOUR,
                "fridayStartTime", java.sql.Time.valueOf("08:00:00"), "fridayCapacity", 4.0 * HOUR));
        delegator.create("TechDataCalendar", UtilMisc.toMap("calendarId", CAL, "description", "test", "calendarWeekId", WEEK));
        // Wednesday 2026-03-04 closed
        delegator.create("TechDataCalendarExcDay", UtilMisc.toMap("calendarId", CAL,
                "exceptionDateStartTime", Timestamp.valueOf("2026-03-04 08:00:00"), "exceptionCapacity", java.math.BigDecimal.ZERO, "description", "closed"));
        // week of 2026-03-09 runs on the 4 h pattern
        delegator.create("TechDataCalendarExcWeek", UtilMisc.toMap("calendarId", CAL,
                "exceptionDateStart", java.sql.Date.valueOf("2026-03-09"), "calendarWeekId", WEEK_HALF, "description", "half"));
        delegator.clearAllCaches();
    }

    @Override
    protected void tearDown() throws Exception {
        cleanup();
    }

    private void cleanup() throws Exception {
        delegator.removeByAnd("TechDataCalendarExcDay", UtilMisc.toMap("calendarId", CAL));
        delegator.removeByAnd("TechDataCalendarExcWeek", UtilMisc.toMap("calendarId", CAL));
        delegator.removeByAnd("TechDataCalendar", UtilMisc.toMap("calendarId", CAL));
        delegator.removeByAnd("TechDataCalendarWeek", UtilMisc.toMap("calendarWeekId", WEEK));
        delegator.removeByAnd("TechDataCalendarWeek", UtilMisc.toMap("calendarWeekId", WEEK_HALF));
        delegator.clearAllCaches();
    }

    private GenericValue calendar() throws Exception {
        return EntityQuery.use(delegator).from("TechDataCalendar").where("calendarId", CAL).queryOne();
    }

    public void testCapacityRemainingOnNormalDay() throws Exception {
        long remaining = TechDataServices.capacityRemaining(calendar(), Timestamp.valueOf("2026-03-03 10:00:00"));
        assertEquals(6 * HOUR, remaining);
    }

    public void testExceptionDayHasNoCapacity() throws Exception {
        long remaining = TechDataServices.capacityRemaining(calendar(), Timestamp.valueOf("2026-03-04 10:00:00"));
        assertEquals(0L, remaining);
    }

    public void testStartNextDaySkipsExceptionDay() throws Exception {
        Map<String, Object> next = TechDataServices.startNextDay(calendar(), Timestamp.valueOf("2026-03-03 17:00:00"));
        assertEquals(Timestamp.valueOf("2026-03-05 08:00:00"), next.get("dateTo"));
        assertEquals(8.0 * HOUR, ((Double) next.get("nextCapacity")).doubleValue(), 0.1);
    }

    public void testAddForwardSkipsExceptionDayAndWeekend() throws Exception {
        // Tue 14:00 + 4 h: 2 h left on Tuesday, Wednesday closed, 2 h on Thursday
        Timestamp to = TechDataServices.addForward(calendar(), Timestamp.valueOf("2026-03-03 14:00:00"), 4 * HOUR);
        assertEquals(Timestamp.valueOf("2026-03-05 10:00:00"), to);
        // Fri 15:00 + 2 h: 1 h left on Friday, weekend skipped, exception week Monday 1 h
        to = TechDataServices.addForward(calendar(), Timestamp.valueOf("2026-03-06 15:00:00"), 2 * HOUR);
        assertEquals(Timestamp.valueOf("2026-03-09 09:00:00"), to);
    }

    public void testAddBackwardSkipsExceptionDay() throws Exception {
        Timestamp to = TechDataServices.addBackward(calendar(), Timestamp.valueOf("2026-03-05 10:00:00"), 4 * HOUR);
        assertEquals(Timestamp.valueOf("2026-03-03 14:00:00"), to);
    }

    public void testExceptionWeekUsesOtherPattern() throws Exception {
        long remaining = TechDataServices.capacityRemaining(calendar(), Timestamp.valueOf("2026-03-10 09:00:00"));
        assertEquals(3 * HOUR, remaining);
        // the week after the exception week is back to 8 h
        remaining = TechDataServices.capacityRemaining(calendar(), Timestamp.valueOf("2026-03-17 09:00:00"));
        assertEquals(7 * HOUR, remaining);
    }

    public void testGetDayCapacityFields() throws Exception {
        GenericValue cal = calendar();
        GenericValue week = cal.getRelatedOne("TechDataCalendarWeek", true);
        Map<String, Object> day = TechDataServices.getDayCapacity(cal, week, Timestamp.valueOf("2026-03-03 12:00:00"));
        assertEquals(Timestamp.valueOf("2026-03-03 08:00:00"), day.get("periodStart"));
        assertEquals(Timestamp.valueOf("2026-03-03 16:00:00"), day.get("periodEnd"));
        Map<String, Object> saturday = TechDataServices.getDayCapacity(cal, week, Timestamp.valueOf("2026-03-07 12:00:00"));
        assertEquals(0.0, ((Double) saturday.get("capacity")).doubleValue(), 0.1);
    }
}
