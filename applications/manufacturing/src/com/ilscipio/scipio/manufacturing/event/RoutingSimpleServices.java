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
 * SCIPIO: Hand-written replacement for the RoutingSimpleServices.xml simple-methods.
 */
package com.ilscipio.scipio.manufacturing.event;

import java.util.List;
import java.util.Locale;
import java.util.Map;

import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.UtilProperties;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.GenericEntityException;
import org.ofbiz.entity.GenericValue;
import org.ofbiz.entity.util.EntityQuery;
import org.ofbiz.entity.util.EntityUtil;
import org.ofbiz.security.Security;
import org.ofbiz.service.DispatchContext;
import org.ofbiz.service.ServiceUtil;

/**
 * Manufacturing routing calendar (TechDataCalendar / TechDataCalendarWeek / TechDataCalendarExcDay /
 * TechDataCalendarExcWeek) create/update/remove services.
 *
 * <p>SCIPIO: Hand-written replacement for RoutingSimpleServices.xml.</p>
 */
public class RoutingSimpleServices {

    private static final String MODULE = RoutingSimpleServices.class.getName();

    /** Create a TechDataCalendar, verifying the referenced calendar week already exists. */
    public static Map<String, Object> createCalendar(DispatchContext dctx, Map<String, ? extends Object> context) {
        Delegator delegator = dctx.getDelegator();
        Security security = dctx.getSecurity();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        if (!security.hasEntityPermission("MANUFACTURING", "_CREATE", userLogin)) {
            return ServiceUtil.returnError(UtilProperties.getMessage("ManufacturingUiLabels", "ManufacturingCalendarPermissionError", locale));
        }
        try {
            GenericValue lookupPKMap = delegator.makeValue("TechDataCalendar");
            lookupPKMap.setPKFields(context);
            GenericValue newEntity = EntityQuery.use(delegator).from("TechDataCalendar").where(lookupPKMap).queryOne();
            if (UtilValidate.isNotEmpty(newEntity)) {
                return ServiceUtil.returnError(UtilProperties.getMessage("ManufacturingUiLabels", "ManufacturingCalendarIdAlreadyExist", locale));
            }
            GenericValue lookupWeekMap = delegator.makeValue("TechDataCalendarWeek");
            lookupWeekMap.setPKFields(context);
            GenericValue weekEntity = EntityQuery.use(delegator).from("TechDataCalendarWeek").where(lookupWeekMap).queryOne();
            if (UtilValidate.isEmpty(weekEntity)) {
                return ServiceUtil.returnError(UtilProperties.getMessage("ManufacturingUiLabels", "ManufacturingCalendarWeekIdNotExisting", locale));
            }
            newEntity = delegator.makeValue("TechDataCalendar");
            newEntity.setPKFields(context);
            newEntity.setNonPKFields(context);
            delegator.create(newEntity);
            return ServiceUtil.returnSuccess();
        } catch (GenericEntityException e) {
            Debug.logError(e, "Error creating TechDataCalendar: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
    }

    /** Update a TechDataCalendar. */
    public static Map<String, Object> updateCalendar(DispatchContext dctx, Map<String, ? extends Object> context) {
        Delegator delegator = dctx.getDelegator();
        Security security = dctx.getSecurity();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        if (!security.hasEntityPermission("MANUFACTURING", "_UPDATE", userLogin)) {
            return ServiceUtil.returnError(UtilProperties.getMessage("ManufacturingUiLabels", "ManufacturingCalendarPermissionError", locale));
        }
        try {
            GenericValue lookupPKMap = delegator.makeValue("TechDataCalendar");
            lookupPKMap.setPKFields(context);
            GenericValue lookedUpValue = EntityQuery.use(delegator).from("TechDataCalendar").where(lookupPKMap).queryOne();
            lookedUpValue.setNonPKFields(context);
            delegator.store(lookedUpValue);
            return ServiceUtil.returnSuccess();
        } catch (GenericEntityException e) {
            Debug.logError(e, "Error updating TechDataCalendar: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
    }

    /** Remove a TechDataCalendar, refusing when a calendar exception day or week still references it. */
    public static Map<String, Object> removeCalendar(DispatchContext dctx, Map<String, ? extends Object> context) {
        Delegator delegator = dctx.getDelegator();
        Security security = dctx.getSecurity();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        if (!security.hasEntityPermission("MANUFACTURING", "_DELETE", userLogin)) {
            return ServiceUtil.returnError(UtilProperties.getMessage("ManufacturingUiLabels", "ManufacturingCalendarPermissionError", locale));
        }
        try {
            GenericValue lookupPKMap = delegator.makeValue("TechDataCalendar");
            lookupPKMap.setPKFields(context);
            List<GenericValue> excDays = lookupPKMap.getRelated("TechDataCalendarExcDay", null, null, false);
            if (UtilValidate.isNotEmpty(EntityUtil.getFirst(excDays))) {
                return ServiceUtil.returnError(UtilProperties.getMessage("ManufacturingUiLabels", "ManufacturingCalendarExceptionDayUseCalendar", locale));
            }
            List<GenericValue> excWeeks = lookupPKMap.getRelated("TechDataCalendarExcWeek", null, null, false);
            if (UtilValidate.isNotEmpty(EntityUtil.getFirst(excWeeks))) {
                return ServiceUtil.returnError(UtilProperties.getMessage("ManufacturingUiLabels", "ManufacturingCalendarExceptionWeekUseCalendar", locale));
            }
            GenericValue lookedUpValue = EntityQuery.use(delegator).from("TechDataCalendar").where(lookupPKMap).queryOne();
            if (UtilValidate.isNotEmpty(lookedUpValue) && UtilValidate.isNotEmpty(lookedUpValue.get("calendarWeekId"))) {
                delegator.removeValue(lookedUpValue);
            }
            return ServiceUtil.returnSuccess();
        } catch (GenericEntityException e) {
            Debug.logError(e, "Error removing TechDataCalendar: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
    }

    /** Create a TechDataCalendarWeek. */
    public static Map<String, Object> createCalendarWeek(DispatchContext dctx, Map<String, ? extends Object> context) {
        Delegator delegator = dctx.getDelegator();
        Security security = dctx.getSecurity();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        if (!security.hasEntityPermission("MANUFACTURING", "_CREATE", userLogin)) {
            return ServiceUtil.returnError(UtilProperties.getMessage("ManufacturingUiLabels", "ManufacturingCalendarPermissionError", locale));
        }
        try {
            GenericValue lookupPKMap = delegator.makeValue("TechDataCalendarWeek");
            lookupPKMap.setPKFields(context);
            GenericValue newEntity = EntityQuery.use(delegator).from("TechDataCalendarWeek").where(lookupPKMap).queryOne();
            if (UtilValidate.isNotEmpty(newEntity)) {
                return ServiceUtil.returnError(UtilProperties.getMessage("ManufacturingUiLabels", "ManufacturingCalendarWeekIdAlreadyExist", locale));
            }
            newEntity = delegator.makeValue("TechDataCalendarWeek");
            newEntity.setPKFields(context);
            newEntity.setNonPKFields(context);
            delegator.create(newEntity);
            return ServiceUtil.returnSuccess();
        } catch (GenericEntityException e) {
            Debug.logError(e, "Error creating TechDataCalendarWeek: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
    }

    /** Update a TechDataCalendarWeek. */
    public static Map<String, Object> updateCalendarWeek(DispatchContext dctx, Map<String, ? extends Object> context) {
        Delegator delegator = dctx.getDelegator();
        Security security = dctx.getSecurity();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        if (!security.hasEntityPermission("MANUFACTURING", "_UPDATE", userLogin)) {
            return ServiceUtil.returnError(UtilProperties.getMessage("ManufacturingUiLabels", "ManufacturingCalendarPermissionError", locale));
        }
        try {
            GenericValue lookupPKMap = delegator.makeValue("TechDataCalendarWeek");
            lookupPKMap.setPKFields(context);
            GenericValue lookedUpValue = EntityQuery.use(delegator).from("TechDataCalendarWeek").where(lookupPKMap).queryOne();
            lookedUpValue.setNonPKFields(context);
            delegator.store(lookedUpValue);
            return ServiceUtil.returnSuccess();
        } catch (GenericEntityException e) {
            Debug.logError(e, "Error updating TechDataCalendarWeek: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
    }

    /** Remove a TechDataCalendarWeek, refusing when a calendar or calendar exception week still references it. */
    public static Map<String, Object> removeCalendarWeek(DispatchContext dctx, Map<String, ? extends Object> context) {
        Delegator delegator = dctx.getDelegator();
        Security security = dctx.getSecurity();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        if (!security.hasEntityPermission("MANUFACTURING", "_DELETE", userLogin)) {
            return ServiceUtil.returnError(UtilProperties.getMessage("ManufacturingUiLabels", "ManufacturingCalendarPermissionError", locale));
        }
        try {
            GenericValue lookupPKMap = delegator.makeValue("TechDataCalendarWeek");
            lookupPKMap.setPKFields(context);
            List<GenericValue> calendars = lookupPKMap.getRelated("TechDataCalendar", null, null, false);
            if (UtilValidate.isNotEmpty(EntityUtil.getFirst(calendars))) {
                return ServiceUtil.returnError(UtilProperties.getMessage("ManufacturingUiLabels", "ManufacturingCalendarUseCalendarWeek", locale));
            }
            List<GenericValue> excWeeks = lookupPKMap.getRelated("TechDataCalendarExcWeek", null, null, false);
            if (UtilValidate.isNotEmpty(EntityUtil.getFirst(excWeeks))) {
                return ServiceUtil.returnError(UtilProperties.getMessage("ManufacturingUiLabels", "ManufacturingCalendarWeekExceptionUseCalendarWeek", locale));
            }
            GenericValue lookedUpValue = EntityQuery.use(delegator).from("TechDataCalendarWeek").where(lookupPKMap).queryOne();
            if (UtilValidate.isNotEmpty(lookedUpValue) && UtilValidate.isNotEmpty(lookedUpValue.get("calendarWeekId"))) {
                delegator.removeValue(lookedUpValue);
            }
            return ServiceUtil.returnSuccess();
        } catch (GenericEntityException e) {
            Debug.logError(e, "Error removing TechDataCalendarWeek: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
    }

    /** Create a TechDataCalendarExcDay. */
    public static Map<String, Object> createCalendarExceptionDay(DispatchContext dctx, Map<String, ? extends Object> context) {
        Delegator delegator = dctx.getDelegator();
        Security security = dctx.getSecurity();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        if (!security.hasEntityPermission("MANUFACTURING", "_CREATE", userLogin)) {
            return ServiceUtil.returnError(UtilProperties.getMessage("ManufacturingUiLabels", "ManufacturingCalendarPermissionError", locale));
        }
        try {
            GenericValue lookupPKMap = delegator.makeValue("TechDataCalendarExcDay");
            lookupPKMap.setPKFields(context);
            GenericValue newEntity = EntityQuery.use(delegator).from("TechDataCalendarExcDay").where(lookupPKMap).queryOne();
            if (UtilValidate.isNotEmpty(newEntity)) {
                return ServiceUtil.returnError(UtilProperties.getMessage("ManufacturingUiLabels", "ManufacturingCalendarExceptionDayIdAlreadyExist", locale));
            }
            newEntity = delegator.makeValue("TechDataCalendarExcDay");
            newEntity.setPKFields(context);
            newEntity.setNonPKFields(context);
            delegator.create(newEntity);
            return ServiceUtil.returnSuccess();
        } catch (GenericEntityException e) {
            Debug.logError(e, "Error creating TechDataCalendarExcDay: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
    }

    /** Update a TechDataCalendarExcDay. */
    public static Map<String, Object> updateCalendarExceptionDay(DispatchContext dctx, Map<String, ? extends Object> context) {
        Delegator delegator = dctx.getDelegator();
        Security security = dctx.getSecurity();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        if (!security.hasEntityPermission("MANUFACTURING", "_UPDATE", userLogin)) {
            return ServiceUtil.returnError(UtilProperties.getMessage("ManufacturingUiLabels", "ManufacturingCalendarPermissionError", locale));
        }
        try {
            GenericValue lookupPKMap = delegator.makeValue("TechDataCalendarExcDay");
            lookupPKMap.setPKFields(context);
            GenericValue lookedUpValue = EntityQuery.use(delegator).from("TechDataCalendarExcDay").where(lookupPKMap).queryOne();
            lookedUpValue.setNonPKFields(context);
            delegator.store(lookedUpValue);
            return ServiceUtil.returnSuccess();
        } catch (GenericEntityException e) {
            Debug.logError(e, "Error updating TechDataCalendarExcDay: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
    }

    /** Remove a TechDataCalendarExcDay. */
    public static Map<String, Object> removeCalendarExceptionDay(DispatchContext dctx, Map<String, ? extends Object> context) {
        Delegator delegator = dctx.getDelegator();
        Security security = dctx.getSecurity();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        if (!security.hasEntityPermission("MANUFACTURING", "_DELETE", userLogin)) {
            return ServiceUtil.returnError(UtilProperties.getMessage("ManufacturingUiLabels", "ManufacturingCalendarPermissionError", locale));
        }
        try {
            GenericValue lookupPKMap = delegator.makeValue("TechDataCalendarExcDay");
            lookupPKMap.setPKFields(context);
            GenericValue lookedUpValue = EntityQuery.use(delegator).from("TechDataCalendarExcDay").where(lookupPKMap).queryOne();
            if (UtilValidate.isNotEmpty(lookedUpValue) && UtilValidate.isNotEmpty(lookedUpValue.get("calendarId"))) {
                delegator.removeValue(lookedUpValue);
            }
            return ServiceUtil.returnSuccess();
        } catch (GenericEntityException e) {
            Debug.logError(e, "Error removing TechDataCalendarExcDay: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
    }

    /** Create a TechDataCalendarExcWeek. */
    public static Map<String, Object> createCalendarExceptionWeek(DispatchContext dctx, Map<String, ? extends Object> context) {
        Delegator delegator = dctx.getDelegator();
        Security security = dctx.getSecurity();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        if (!security.hasEntityPermission("MANUFACTURING", "_CREATE", userLogin)) {
            return ServiceUtil.returnError(UtilProperties.getMessage("ManufacturingUiLabels", "ManufacturingCalendarPermissionError", locale));
        }
        try {
            GenericValue lookupPKMap = delegator.makeValue("TechDataCalendarExcWeek");
            lookupPKMap.setPKFields(context);
            GenericValue newEntity = EntityQuery.use(delegator).from("TechDataCalendarExcWeek").where(lookupPKMap).queryOne();
            if (UtilValidate.isNotEmpty(newEntity)) {
                return ServiceUtil.returnError(UtilProperties.getMessage("ManufacturingUiLabels", "ManufacturingCalendarExceptionWeekIdAlreadyExist", locale));
            }
            newEntity = delegator.makeValue("TechDataCalendarExcWeek");
            newEntity.setPKFields(context);
            newEntity.setNonPKFields(context);
            delegator.create(newEntity);
            return ServiceUtil.returnSuccess();
        } catch (GenericEntityException e) {
            Debug.logError(e, "Error creating TechDataCalendarExcWeek: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
    }

    /** Update a TechDataCalendarExcWeek. */
    public static Map<String, Object> updateCalendarExceptionWeek(DispatchContext dctx, Map<String, ? extends Object> context) {
        Delegator delegator = dctx.getDelegator();
        Security security = dctx.getSecurity();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        if (!security.hasEntityPermission("MANUFACTURING", "_UPDATE", userLogin)) {
            return ServiceUtil.returnError(UtilProperties.getMessage("ManufacturingUiLabels", "ManufacturingCalendarPermissionError", locale));
        }
        try {
            GenericValue lookupPKMap = delegator.makeValue("TechDataCalendarExcWeek");
            lookupPKMap.setPKFields(context);
            GenericValue lookedUpValue = EntityQuery.use(delegator).from("TechDataCalendarExcWeek").where(lookupPKMap).queryOne();
            lookedUpValue.setNonPKFields(context);
            delegator.store(lookedUpValue);
            return ServiceUtil.returnSuccess();
        } catch (GenericEntityException e) {
            Debug.logError(e, "Error updating TechDataCalendarExcWeek: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
    }

    /** Remove a TechDataCalendarExcWeek. */
    public static Map<String, Object> removeCalendarExceptionWeek(DispatchContext dctx, Map<String, ? extends Object> context) {
        Delegator delegator = dctx.getDelegator();
        Security security = dctx.getSecurity();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        if (!security.hasEntityPermission("MANUFACTURING", "_DELETE", userLogin)) {
            return ServiceUtil.returnError(UtilProperties.getMessage("ManufacturingUiLabels", "ManufacturingCalendarPermissionError", locale));
        }
        try {
            GenericValue lookupPKMap = delegator.makeValue("TechDataCalendarExcWeek");
            lookupPKMap.setPKFields(context);
            GenericValue lookedUpValue = EntityQuery.use(delegator).from("TechDataCalendarExcWeek").where(lookupPKMap).queryOne();
            if (UtilValidate.isNotEmpty(lookedUpValue) && UtilValidate.isNotEmpty(lookedUpValue.get("calendarId"))) {
                delegator.removeValue(lookedUpValue);
            }
            return ServiceUtil.returnSuccess();
        } catch (GenericEntityException e) {
            Debug.logError(e, "Error removing TechDataCalendarExcWeek: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
    }

    private RoutingSimpleServices() {}
}
