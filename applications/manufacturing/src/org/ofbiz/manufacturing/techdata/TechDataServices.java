/*
 * Licensed to the Apache Software Foundation (ASF) under one
 * or more contributor license agreements.  See the NOTICE file
 * distributed with this work for additional information
 * regarding copyright ownership.  The ASF licenses this file
 * to you under the Apache License, Version 2.0 (the
 * "License"); you may not use this file except in compliance
 * with the License.  You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 * KIND, either express or implied.  See the License for the
 * specific language governing permissions and limitations
 * under the License.
 */
/*
 * Changes to this file: Copyright (C) Ilscipio GmbH. The changes are licensed
 * under the GNU Affero General Public License, version 3, or a commercial
 * license from Ilscipio GmbH (file LICENSE). The original code stays under
 * the Apache License, version 2.0, as stated above.
 */
package org.ofbiz.manufacturing.techdata;

import java.sql.Time;
import java.sql.Timestamp;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Locale;
import java.util.Map;

import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.UtilDateTime;
import org.ofbiz.base.util.UtilMisc;
import org.ofbiz.base.util.UtilProperties;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.GenericEntityException;
import org.ofbiz.entity.GenericValue;
import org.ofbiz.entity.condition.EntityCondition;
import org.ofbiz.entity.condition.EntityExpr;
import org.ofbiz.entity.condition.EntityOperator;
import org.ofbiz.entity.util.EntityQuery;
import org.ofbiz.entity.util.EntityUtil;
import org.ofbiz.service.DispatchContext;
import org.ofbiz.service.ServiceUtil;

import com.ibm.icu.util.Calendar;

/**
 * TechDataServices - TechData related Services
 *
 */
public class TechDataServices {

    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());
    public static final String resource = "ManufacturingUiLabels";

    /**
     *
     * Used to retrieve some RoutingTasks (WorkEffort) selected by Name or MachineGroup ordered by Name
     *
     * @param ctx the dispatch context
     * @param context a map containing workEffortName (routingTaskName) and fixedAssetId (MachineGroup or ANY)
     * @return result a map containing lookupResult (list of RoutingTask &lt;=&gt; workEffortId with currentStatusId = "ROU_ACTIVE" and workEffortTypeId = "ROU_TASK"
     */
    public static Map<String, Object> lookupRoutingTask(DispatchContext ctx, Map<String, ? extends Object> context) {
        Delegator delegator = ctx.getDelegator();
        Map<String, Object> result = new HashMap<String, Object>();
        Locale locale = (Locale) context.get("locale");
        String workEffortName = (String) context.get("workEffortName");
        String fixedAssetId = (String) context.get("fixedAssetId");

        List<GenericValue> listRoutingTask = null;
        List<EntityExpr> constraints = new LinkedList<EntityExpr>();

        if (UtilValidate.isNotEmpty(workEffortName)) {
            constraints.add(EntityCondition.makeCondition("workEffortName", EntityOperator.GREATER_THAN_EQUAL_TO, workEffortName));
        }
        if (UtilValidate.isNotEmpty(fixedAssetId) && ! "ANY".equals(fixedAssetId)) {
            constraints.add(EntityCondition.makeCondition("fixedAssetId", EntityOperator.EQUALS, fixedAssetId));
        }
        constraints.add(EntityCondition.makeCondition("currentStatusId", EntityOperator.EQUALS, "ROU_ACTIVE"));
        constraints.add(EntityCondition.makeCondition("workEffortTypeId", EntityOperator.EQUALS, "ROU_TASK"));

        try {
            listRoutingTask = EntityQuery.use(delegator).from("WorkEffort")
                    .where(constraints)
                    .orderBy("workEffortName")
                    .queryList();
        } catch (GenericEntityException e) {
            Debug.logWarning(e, module);
            return ServiceUtil.returnError(UtilProperties.getMessage(resource, "ManufacturingTechDataWorkEffortNotExist", UtilMisc.toMap("errorString", e.toString()), locale));
        }
        if (listRoutingTask == null) {
            listRoutingTask = new LinkedList<GenericValue>();
        }
        if (listRoutingTask.size() == 0) {
            //FIXME is it correct ?
            // listRoutingTask.add(UtilMisc.toMap("label","no Match","value","NO_MATCH"));
        }
        result.put("lookupResult", listRoutingTask);
        return result;
    }

    /**
     *
     * Used to check if there is not two routing task with the same SeqId valid at the same period
     *
     * @param ctx            The DispatchContext that this service is operating in.
     * @param context    a map containing workEffortIdFrom (routing) and SeqId, fromDate thruDate
     * @return result      a map containing sequenceNumNotOk which is equal to "Y" if it's not Ok
     */
    public static Map<String, Object> checkRoutingTaskAssoc(DispatchContext ctx, Map<String, ? extends Object> context) {
        Delegator delegator = ctx.getDelegator();
        Map<String, Object> result = new HashMap<String, Object>();
        String sequenceNumNotOk = "N";
        Locale locale = (Locale) context.get("locale");
        String workEffortIdFrom = (String) context.get("workEffortIdFrom");
        String workEffortIdTo = (String) context.get("workEffortIdTo");
        String workEffortAssocTypeId = (String) context.get("workEffortAssocTypeId");
        Long sequenceNum =  (Long) context.get("sequenceNum");
        Timestamp fromDate = (Timestamp) context.get("fromDate");
        Timestamp thruDate = (Timestamp) context.get("thruDate");
        String create = (String) context.get("create");

        boolean createProcess = (create !=null && "Y".equals(create)) ? true : false;
        List<GenericValue> listRoutingTaskAssoc = null;

        try {
            listRoutingTaskAssoc = EntityQuery.use(delegator).from("WorkEffortAssoc")
                    .where("workEffortIdFrom", workEffortIdFrom,"sequenceNum",sequenceNum)
                    .orderBy("fromDate")
                    .queryList();
        } catch (GenericEntityException e) {
            Debug.logWarning(e, module);
            return ServiceUtil.returnError(UtilProperties.getMessage(resource, "ManufacturingTechDataWorkEffortAssocNotExist", UtilMisc.toMap("errorString", e.toString()), locale));
        }

        if (listRoutingTaskAssoc != null) {
            for (GenericValue routingTaskAssoc : listRoutingTaskAssoc) {
                if (! workEffortIdFrom.equals(routingTaskAssoc.getString("workEffortIdFrom")) ||
                ! workEffortIdTo.equals(routingTaskAssoc.getString("workEffortIdTo")) ||
                ! workEffortAssocTypeId.equals(routingTaskAssoc.getString("workEffortAssocTypeId")) ||
                ! sequenceNum.equals(routingTaskAssoc.getLong("sequenceNum"))
               ) {
                    if (routingTaskAssoc.getTimestamp("thruDate") == null && routingTaskAssoc.getTimestamp("fromDate") == null) sequenceNumNotOk = "Y";
                    else if (routingTaskAssoc.getTimestamp("thruDate") == null) {
                        if (thruDate == null) sequenceNumNotOk = "Y";
                        else if (thruDate.after(routingTaskAssoc.getTimestamp("fromDate"))) sequenceNumNotOk = "Y";
                    }
                    else  if (routingTaskAssoc.getTimestamp("fromDate") == null) {
                        if (fromDate == null) sequenceNumNotOk = "Y";
                        else if (fromDate.before(routingTaskAssoc.getTimestamp("thruDate"))) sequenceNumNotOk = "Y";
                    }
                    else if (fromDate == null && thruDate == null) sequenceNumNotOk = "Y";
                    else if (thruDate == null) {
                        if (fromDate.before(routingTaskAssoc.getTimestamp("thruDate"))) sequenceNumNotOk = "Y";
                    }
                    else if (fromDate == null) {
                        if (thruDate.after(routingTaskAssoc.getTimestamp("fromDate"))) sequenceNumNotOk = "Y";
                    }
                    else if (routingTaskAssoc.getTimestamp("fromDate").before(thruDate) && fromDate.before(routingTaskAssoc.getTimestamp("thruDate"))) sequenceNumNotOk = "Y";
                } else if (createProcess) sequenceNumNotOk = "Y";
            }
        }
        result.put("sequenceNumNotOk", sequenceNumNotOk);
        return result;
    }

    /**
     * Used to get the techDataCalendar for a routingTask, if there is a entity exception
     * or routingTask associated with no MachineGroup the DEFAULT TechDataCalendar is return.
     *
     * @param routingTask    the routingTask for which we are looking for
     * @return the techDataCalendar associated
     */
    public static GenericValue getTechDataCalendar(GenericValue routingTask) {
        GenericValue machineGroup = null, techDataCalendar = null;
        try {
            machineGroup = routingTask.getRelatedOne("FixedAsset", true);
        } catch (GenericEntityException e) {
            Debug.logError("Pb reading FixedAsset associated with routingTask"+e.getMessage(), module);
        }
        if (machineGroup != null) {
            if (machineGroup.getString("calendarId") != null) {
                try {
                    techDataCalendar = machineGroup.getRelatedOne("TechDataCalendar", true);
                } catch (GenericEntityException e) {
                    Debug.logError("Pb reading TechDataCalendar associated with machineGroup"+e.getMessage(), module);
                }
            } else {
                try {
                    List<GenericValue> machines = machineGroup.getRelated("ChildFixedAsset", null, null, true);
                    if (machines != null && machines.size()>0) {
                        GenericValue machine = EntityUtil.getFirst(machines);
                        techDataCalendar = machine.getRelatedOne("TechDataCalendar", true);
                    }
                } catch (GenericEntityException e) {
                    Debug.logError("Pb reading machine child from machineGroup"+e.getMessage(), module);
                }
            }
        }
        if (techDataCalendar == null) {
            try {
                Delegator delegator = routingTask.getDelegator();
                techDataCalendar = EntityQuery.use(delegator).from("TechDataCalendar").where("calendarId", "DEFAULT").queryOne();
            } catch (GenericEntityException e) {
                Debug.logError("Pb reading TechDataCalendar DEFAULT"+e.getMessage(), module);
            }
        }
        return techDataCalendar;
    }

    /** Used to find the fisrt day in the TechDataCalendarWeek where capacity != 0, beginning at dayStart, dayStart included.
     *
     * @param techDataCalendarWeek        The TechDataCalendarWeek cover
     * @param dayStart
     * @return a map with the  capacity (Double) available and moveDay (int): the number of day it's necessary to move to have capacity available
     */
    public static Map<String, Object> dayStartCapacityAvailable(GenericValue techDataCalendarWeek,  int  dayStart) {
        Map<String, Object> result = new HashMap<String, Object>();
        int moveDay = 0;
        Double capacity = null;
        Time startTime = null;
        while (capacity == null || capacity ==0) {
            switch (dayStart) {
                case Calendar.MONDAY:
                    capacity =  techDataCalendarWeek.getDouble("mondayCapacity");
                    startTime =  techDataCalendarWeek.getTime("mondayStartTime");
                    break;
                case Calendar.TUESDAY:
                    capacity =  techDataCalendarWeek.getDouble("tuesdayCapacity");
                    startTime =  techDataCalendarWeek.getTime("tuesdayStartTime");
                    break;
                case Calendar.WEDNESDAY:
                    capacity =  techDataCalendarWeek.getDouble("wednesdayCapacity");
                    startTime =  techDataCalendarWeek.getTime("wednesdayStartTime");
                    break;
                case Calendar.THURSDAY:
                    capacity =  techDataCalendarWeek.getDouble("thursdayCapacity");
                    startTime =  techDataCalendarWeek.getTime("thursdayStartTime");
                    break;
                case Calendar.FRIDAY:
                    capacity =  techDataCalendarWeek.getDouble("fridayCapacity");
                    startTime =  techDataCalendarWeek.getTime("fridayStartTime");
                    break;
                case Calendar.SATURDAY:
                    capacity =  techDataCalendarWeek.getDouble("saturdayCapacity");
                    startTime =  techDataCalendarWeek.getTime("saturdayStartTime");
                    break;
                case Calendar.SUNDAY:
                    capacity =  techDataCalendarWeek.getDouble("sundayCapacity");
                    startTime =  techDataCalendarWeek.getTime("sundayStartTime");
                    break;
            }
            if (capacity == null || capacity == 0) {
                moveDay +=1;
                dayStart = (dayStart==7) ? 1 : dayStart +1;
            }
        }
        result.put("capacity",capacity);
        result.put("startTime",startTime);
        result.put("moveDay", moveDay);
        return result;
    }
    /** SCIPIO: maximum number of days scanned for an available period before the calendar is declared empty. */
    private static final int MAX_CALENDAR_SCAN_DAYS = 400;

    /** SCIPIO: Returns the calendar week pattern of a calendar, or null when it cannot be read. */
    private static GenericValue getCalendarWeek(GenericValue techDataCalendar) {
        try {
            return techDataCalendar.getRelatedOne("TechDataCalendarWeek", true);
        } catch (GenericEntityException e) {
            Debug.logError("Pb reading Calendar Week associated with calendar" + e.getMessage(), module);
            return null;
        }
    }

    /** SCIPIO: Returns capacity (ms) and start time of one weekday from a calendar week pattern. */
    private static Map<String, Object> weekDayValues(GenericValue techDataCalendarWeek, int dayOfWeek) {
        String prefix;
        switch (dayOfWeek) {
            case Calendar.MONDAY: prefix = "monday"; break;
            case Calendar.TUESDAY: prefix = "tuesday"; break;
            case Calendar.WEDNESDAY: prefix = "wednesday"; break;
            case Calendar.THURSDAY: prefix = "thursday"; break;
            case Calendar.FRIDAY: prefix = "friday"; break;
            case Calendar.SATURDAY: prefix = "saturday"; break;
            default: prefix = "sunday"; break;
        }
        Map<String, Object> result = new HashMap<>();
        result.put("capacity", techDataCalendarWeek.getDouble(prefix + "Capacity"));
        result.put("startTime", techDataCalendarWeek.getTime(prefix + "StartTime"));
        return result;
    }

    /**
     * SCIPIO: Returns the working period of one calendar day: "capacity" (Double, milliseconds),
     * "periodStart" and "periodEnd" (Timestamp). An exception day (TechDataCalendarExcDay) wins,
     * then an exception week (TechDataCalendarExcWeek) that starts within the 7 days before the day,
     * then the calendar week pattern. A day without capacity returns capacity 0.
     */
    public static Map<String, Object> getDayCapacity(GenericValue techDataCalendar, GenericValue techDataCalendarWeek, Timestamp day) {
        Map<String, Object> result = new HashMap<>();
        Timestamp dayStart = UtilDateTime.getDayStart(day);
        Timestamp dayEnd = UtilDateTime.getDayEnd(day);
        Delegator delegator = techDataCalendar.getDelegator();
        String calendarId = techDataCalendar.getString("calendarId");
        Double capacity = null;
        Timestamp periodStart = null;
        try {
            GenericValue excDay = EntityQuery.use(delegator).from("TechDataCalendarExcDay")
                    .where(EntityCondition.makeCondition("calendarId", calendarId),
                            EntityCondition.makeCondition("exceptionDateStartTime", EntityOperator.GREATER_THAN_EQUAL_TO, dayStart),
                            EntityCondition.makeCondition("exceptionDateStartTime", EntityOperator.LESS_THAN_EQUAL_TO, dayEnd))
                    .orderBy("exceptionDateStartTime").cache(true).queryFirst();
            if (excDay != null) {
                capacity = excDay.getDouble("exceptionCapacity");
                periodStart = excDay.getTimestamp("exceptionDateStartTime");
            } else {
                GenericValue week = techDataCalendarWeek;
                Timestamp weekWindowStart = UtilDateTime.getDayStart(dayStart, -6);
                GenericValue excWeek = EntityQuery.use(delegator).from("TechDataCalendarExcWeek")
                        .where(EntityCondition.makeCondition("calendarId", calendarId),
                                EntityCondition.makeCondition("exceptionDateStart", EntityOperator.GREATER_THAN_EQUAL_TO, new java.sql.Date(weekWindowStart.getTime())),
                                EntityCondition.makeCondition("exceptionDateStart", EntityOperator.LESS_THAN_EQUAL_TO, new java.sql.Date(dayStart.getTime())))
                        .orderBy("-exceptionDateStart").cache(true).queryFirst();
                if (excWeek != null) {
                    GenericValue excWeekPattern = excWeek.getRelatedOne("TechDataCalendarWeek", true);
                    if (excWeekPattern != null) {
                        week = excWeekPattern;
                    }
                }
                if (week != null) {
                    Calendar cal = Calendar.getInstance();
                    cal.setTime(dayStart);
                    Map<String, Object> values = weekDayValues(week, cal.get(Calendar.DAY_OF_WEEK));
                    capacity = (Double) values.get("capacity");
                    Time startTime = (Time) values.get("startTime");
                    if (startTime != null) {
                        Calendar st = Calendar.getInstance();
                        st.setTime(startTime);
                        cal.set(Calendar.HOUR_OF_DAY, st.get(Calendar.HOUR_OF_DAY));
                        cal.set(Calendar.MINUTE, st.get(Calendar.MINUTE));
                        cal.set(Calendar.SECOND, st.get(Calendar.SECOND));
                        cal.set(Calendar.MILLISECOND, 0);
                        periodStart = new Timestamp(cal.getTimeInMillis());
                    }
                }
            }
        } catch (GenericEntityException e) {
            Debug.logError(e, "Problem reading calendar exceptions for calendar " + calendarId, module);
        }
        if (capacity == null) {
            capacity = 0.0;
        }
        if (periodStart == null) {
            periodStart = dayStart;
        }
        result.put("capacity", capacity);
        result.put("periodStart", periodStart);
        result.put("periodEnd", new Timestamp(periodStart.getTime() + capacity.longValue()));
        return result;
    }

    /** Used to to request the remain capacity available for dateFrom in a TechDataCalenda,
     * If the dateFrom (param in) is not  in an available TechDataCalendar period, the return value is zero.
     * SCIPIO: honors exception days and exception weeks.
     *
     * @param techDataCalendar        The TechDataCalendar cover
     * @param dateFrom                        the date
     * @return  long capacityRemaining
     */
    public static long capacityRemaining(GenericValue techDataCalendar,  Timestamp  dateFrom) {
        GenericValue techDataCalendarWeek = getCalendarWeek(techDataCalendar);
        if (techDataCalendarWeek == null) return 0;
        Map<String, Object> dayInfo = getDayCapacity(techDataCalendar, techDataCalendarWeek, dateFrom);
        Double capacity = (Double) dayInfo.get("capacity");
        if (capacity == null || capacity == 0) return 0;
        Timestamp periodStart = (Timestamp) dayInfo.get("periodStart");
        Timestamp periodEnd = (Timestamp) dayInfo.get("periodEnd");
        if (dateFrom.before(periodStart) || dateFrom.after(periodEnd)) return 0;
        return periodEnd.getTime() - dateFrom.getTime();
    }

    /** Used to move in a TechDataCalenda, produce the Timestamp for the begining of the next day available and its associated capacity.
     * If the dateFrom (param in) is not  in an available TechDataCalendar period, the return value is the next day available
     * SCIPIO: honors exception days and exception weeks.
     *
     * @param techDataCalendar        The TechDataCalendar cover
     * @param dateFrom                        the date
     * @return a map with Timestamp dateTo, Double nextCapacity
     */
    public static Map<String, Object> startNextDay(GenericValue techDataCalendar, Timestamp  dateFrom) {
        Map<String, Object> result = new HashMap<>();
        GenericValue techDataCalendarWeek = getCalendarWeek(techDataCalendar);
        if (techDataCalendarWeek == null) {
            return ServiceUtil.returnError("Pb reading Calendar Week associated with calendar");
        }
        Timestamp day = dateFrom;
        for (int i = 0; i < MAX_CALENDAR_SCAN_DAYS; i++) {
            Map<String, Object> dayInfo = getDayCapacity(techDataCalendar, techDataCalendarWeek, day);
            Double capacity = (Double) dayInfo.get("capacity");
            Timestamp periodStart = (Timestamp) dayInfo.get("periodStart");
            if (capacity != null && capacity > 0 && dateFrom.before(periodStart)) {
                result.put("dateTo", periodStart);
                result.put("nextCapacity", capacity);
                return result;
            }
            day = UtilDateTime.getNextDayStart(day);
        }
        throw new IllegalStateException("Calendar " + techDataCalendar.getString("calendarId") + " has no available day within "
                + MAX_CALENDAR_SCAN_DAYS + " days after " + dateFrom);
    }

    /** Used to move forward in a TechDataCalenda, start from the dateFrom and move forward only on available period.
     * If the dateFrom (param in) is not  a available TechDataCalendar period, the startDate is the begining of the next  day available
     *
     * @param techDataCalendar        The TechDataCalendar cover
     * @param dateFrom                        the start date
     * @param amount                           the amount of millisecond to move forward
     * @return the dateTo
     */
    public static Timestamp addForward(GenericValue techDataCalendar,  Timestamp  dateFrom, long amount) {
        Timestamp dateTo = (Timestamp) dateFrom.clone();
        long nextCapacity = capacityRemaining(techDataCalendar, dateFrom);
        if (amount <= nextCapacity) {
            dateTo.setTime(dateTo.getTime()+amount);
            amount = 0;
        } else amount -= nextCapacity;

        Map<String, Object> result = new HashMap<String, Object>();
        while (amount > 0)  {
            result = startNextDay(techDataCalendar, dateTo);
            dateTo = (Timestamp) result.get("dateTo");
            nextCapacity = ((Double) result.get("nextCapacity")).longValue();
            if (amount <= nextCapacity) {
                dateTo.setTime(dateTo.getTime()+amount);
                amount = 0;
            } else amount -= nextCapacity;
        }
        return dateTo;
    }

    ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    /** Used to find the last day in the TechDataCalendarWeek where capacity != 0, ending at dayEnd, dayEnd included.
     *
     * @param techDataCalendarWeek        The TechDataCalendarWeek cover
     * @param dayEnd
     * @return a map with the  capacity (Double) available, the startTime and  moveDay (int): the number of day it's necessary to move to have capacity available
     */
    public static Map<String, Object> dayEndCapacityAvailable(GenericValue techDataCalendarWeek, int dayEnd) {
        Map<String, Object> result = new HashMap<String, Object>();
        int moveDay = 0;
        Double capacity = null;
        Time startTime = null;
        while (capacity == null || capacity == 0) {
            switch (dayEnd) {
                case Calendar.MONDAY:
                    capacity =  techDataCalendarWeek.getDouble("mondayCapacity");
                    startTime =  techDataCalendarWeek.getTime("mondayStartTime");
                    break;
                case Calendar.TUESDAY:
                    capacity =  techDataCalendarWeek.getDouble("tuesdayCapacity");
                    startTime =  techDataCalendarWeek.getTime("tuesdayStartTime");
                    break;
                case Calendar.WEDNESDAY:
                    capacity =  techDataCalendarWeek.getDouble("wednesdayCapacity");
                    startTime =  techDataCalendarWeek.getTime("wednesdayStartTime");
                    break;
                case Calendar.THURSDAY:
                    capacity =  techDataCalendarWeek.getDouble("thursdayCapacity");
                    startTime =  techDataCalendarWeek.getTime("thursdayStartTime");
                    break;
                case Calendar.FRIDAY:
                    capacity =  techDataCalendarWeek.getDouble("fridayCapacity");
                    startTime =  techDataCalendarWeek.getTime("fridayStartTime");
                    break;
                case Calendar.SATURDAY:
                    capacity =  techDataCalendarWeek.getDouble("saturdayCapacity");
                    startTime =  techDataCalendarWeek.getTime("saturdayStartTime");
                    break;
                case Calendar.SUNDAY:
                    capacity =  techDataCalendarWeek.getDouble("sundayCapacity");
                    startTime =  techDataCalendarWeek.getTime("sundayStartTime");
                    break;
            }
            if (capacity == null || capacity == 0) {
                moveDay -=1;
                dayEnd = (dayEnd==1) ? 7 : dayEnd - 1;
            }
        }
        result.put("capacity",capacity);
        result.put("startTime",startTime);
        result.put("moveDay", moveDay);
        return result;
    }
    /** Used to request the remaining capacity available for dateFrom in a TechDataCalenda,
     * If the dateFrom (param in) is not  in an available TechDataCalendar period, the return value is zero.
     * SCIPIO: honors exception days and exception weeks.
     *
     * @param techDataCalendar        The TechDataCalendar cover
     * @param dateFrom                        the date
     * @return  long capacityRemaining
     */
    public static long capacityRemainingBackward(GenericValue techDataCalendar,  Timestamp  dateFrom) {
        GenericValue techDataCalendarWeek = getCalendarWeek(techDataCalendar);
        if (techDataCalendarWeek == null) return 0;
        Map<String, Object> dayInfo = getDayCapacity(techDataCalendar, techDataCalendarWeek, dateFrom);
        Double capacity = (Double) dayInfo.get("capacity");
        if (capacity == null || capacity == 0) return 0;
        Timestamp periodStart = (Timestamp) dayInfo.get("periodStart");
        Timestamp periodEnd = (Timestamp) dayInfo.get("periodEnd");
        if (dateFrom.before(periodStart) || dateFrom.after(periodEnd)) return 0;
        return dateFrom.getTime() - periodStart.getTime();
    }

    /** Used to move in a TechDataCalenda, produce the Timestamp for the end of the previous day available and its associated capacity.
     * If the dateFrom (param in) is not  in an available TechDataCalendar period, the return value is the previous day available
     * SCIPIO: honors exception days and exception weeks.
     *
     * @param techDataCalendar        The TechDataCalendar cover
     * @param dateFrom                        the date
     * @return a map with Timestamp dateTo, Double previousCapacity
     */
    public static Map<String, Object> endPreviousDay(GenericValue techDataCalendar,  Timestamp  dateFrom) {
        Map<String, Object> result = new HashMap<>();
        GenericValue techDataCalendarWeek = getCalendarWeek(techDataCalendar);
        if (techDataCalendarWeek == null) {
            return ServiceUtil.returnError("Pb reading Calendar Week associated with calendar");
        }
        Timestamp day = dateFrom;
        for (int i = 0; i < MAX_CALENDAR_SCAN_DAYS; i++) {
            Map<String, Object> dayInfo = getDayCapacity(techDataCalendar, techDataCalendarWeek, day);
            Double capacity = (Double) dayInfo.get("capacity");
            Timestamp periodEnd = (Timestamp) dayInfo.get("periodEnd");
            if (capacity != null && capacity > 0 && periodEnd.before(dateFrom)) {
                result.put("dateTo", periodEnd);
                result.put("previousCapacity", capacity);
                return result;
            }
            day = UtilDateTime.getDayStart(day, -1);
        }
        throw new IllegalStateException("Calendar " + techDataCalendar.getString("calendarId") + " has no available day within "
                + MAX_CALENDAR_SCAN_DAYS + " days before " + dateFrom);
    }

    /** Used to move backward in a TechDataCalendar, start from the dateFrom and move backward only on available period.
     * If the dateFrom (param in) is not  a available TechDataCalendar period, the startDate is the end of the previous day available
     *
     * @param techDataCalendar        The TechDataCalendar cover
     * @param dateFrom                        the start date
     * @param amount                           the amount of millisecond to move backward
     * @return the dateTo
     */
    public static Timestamp addBackward(GenericValue techDataCalendar, Timestamp  dateFrom, long amount) {
        Timestamp dateTo = (Timestamp) dateFrom.clone();
        long previousCapacity = capacityRemainingBackward(techDataCalendar, dateFrom);
        if (amount <= previousCapacity) {
            dateTo.setTime(dateTo.getTime()-amount);
            amount = 0;
        } else amount -= previousCapacity;

        Map<String, Object> result = new HashMap<String, Object>();
        while (amount > 0)  {
            result = endPreviousDay(techDataCalendar, dateTo);
            dateTo = (Timestamp) result.get("dateTo");
            previousCapacity = ((Double) result.get("previousCapacity")).longValue();
            if (amount <= previousCapacity) {
                dateTo.setTime(dateTo.getTime()-amount);
                amount = 0;
            } else amount -= previousCapacity;
        }
        return dateTo;
    }
}
