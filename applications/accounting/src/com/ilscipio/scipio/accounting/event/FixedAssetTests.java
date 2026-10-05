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
package com.ilscipio.scipio.accounting.event;

import java.math.BigDecimal;
import java.sql.Timestamp;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.UtilHttp;
import org.ofbiz.base.util.UtilMisc;
import org.ofbiz.base.util.UtilProperties;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.GenericValue;
import org.ofbiz.entity.util.EntityQuery;
import org.ofbiz.entity.util.EntityUtil;
import org.ofbiz.service.LocalDispatcher;
import org.ofbiz.service.ServiceUtil;

/**
 * Auto-generated event class from Simple Method XML.
 *
 * <p>Generated from: component://accounting/script/org/ofbiz/accounting/test/FixedAssetTests.xml</p>
 *
 * <p>SCIPIO: 4.0.0: Auto-generated.</p>
 */
public class FixedAssetTests {

    private static final String MODULE = FixedAssetTests.class.getName();


    /**
     * Test case for service createFixedAssetRegistration
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String testCreateFixedAssetRegistration(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> serviceCtx = new HashMap<>();
        serviceCtx.put("fixedAssetId", "DEMO_VEHICLE_01");
        serviceCtx.put("licenseNumber", "123456");
        serviceCtx.put("registrationNumber", "123456");
        serviceCtx.put("registrationDate", Timestamp.valueOf("2009-12-24 12:33:23.703"));
        serviceCtx.put("fromDate", Timestamp.valueOf("2009-12-24 12:33:08.247"));
        serviceCtx.put("thruDate", Timestamp.valueOf("2010-12-25 12:33:18.365"));
        try {
            userLogin = EntityQuery.use(delegator)
                    .from("UserLogin")
                    .where(UtilMisc.toMap("userLoginId", "system"))
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying UserLogin: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        serviceCtx.put("userLogin", userLogin);
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("createFixedAssetRegistration", serviceCtx);
            if (ServiceUtil.isError(serviceResult)) {
                String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                request.setAttribute("_ERROR_MESSAGE_", errMsg);
                return "error";
            }
        } catch (Exception e) {
            Debug.logError(e, "Error calling createFixedAssetRegistration: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        GenericValue fixedAssetRegistration = null;
        try {
            fixedAssetRegistration = EntityQuery.use(delegator)
                    .from("FixedAssetRegistration")
                    .where(UtilMisc.toMap("fixedAssetId", ((Map<String, Object>) serviceCtx).get("fixedAssetId"), "fromDate", ((Map<String, Object>) serviceCtx).get("fromDate")))
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying FixedAssetRegistration: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        assert !(UtilValidate.isEmpty(fixedAssetRegistration)) : "Assertion failed: not if-empty";
        if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
            return "error";
        }

        return "success";
    }


    /**
     * Test case for service updateFixedAssetRegistration
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String testUpdateFixedAssetRegistration(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Object fixedAssetId = "DEMO_VEHICLE_01";
        Timestamp registrationDate = Timestamp.valueOf("2010-12-24 12:33:23.703");
        Timestamp fromDate = Timestamp.valueOf("2009-12-24 12:33:08.247");
        Timestamp thruDate = Timestamp.valueOf("2033-12-25 12:33:18.365");
        try {
            userLogin = EntityQuery.use(delegator)
                    .from("UserLogin")
                    .where(UtilMisc.toMap("userLoginId", "system"))
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying UserLogin: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        Map<String, Object> serviceCtx = new HashMap<>();
        serviceCtx.put("userLogin", userLogin);
        serviceCtx.put("fixedAssetId", fixedAssetId);
        serviceCtx.put("registrationDate", registrationDate);
        serviceCtx.put("fromDate", fromDate);
        serviceCtx.put("thruDate", thruDate);
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("updateFixedAssetRegistration", serviceCtx);
            if (ServiceUtil.isError(serviceResult)) {
                String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                request.setAttribute("_ERROR_MESSAGE_", errMsg);
                return "error";
            }
        } catch (Exception e) {
            Debug.logError(e, "Error calling updateFixedAssetRegistration: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        GenericValue fixedAssetRegistration = null;
        try {
            fixedAssetRegistration = EntityQuery.use(delegator)
                    .from("FixedAssetRegistration")
                    .where(UtilMisc.toMap())
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying FixedAssetRegistration: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        assert !(UtilValidate.isEmpty(fixedAssetRegistration)) : "Assertion failed: not if-empty";
        assert java.util.Objects.equals(((Map<String, Object>) fixedAssetRegistration).get("thruDate"), thruDate) : "Assertion failed: if-compare-field";
        assert java.util.Objects.equals(((Map<String, Object>) fixedAssetRegistration).get("registrationDate"), registrationDate) : "Assertion failed: if-compare-field";
        if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
            return "error";
        }

        return "success";
    }


    /**
     * Test case for service deleteFixedAssetRegistration
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String testDeleteFixedAssetRegistration(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Object fixedAssetId = "DEMO_VEHICLE_01";
        Timestamp fromDate = Timestamp.valueOf("2009-12-24 12:33:08.247");
        try {
            userLogin = EntityQuery.use(delegator)
                    .from("UserLogin")
                    .where(UtilMisc.toMap("userLoginId", "system"))
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying UserLogin: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        Map<String, Object> serviceCtx = new HashMap<>();
        serviceCtx.put("userLogin", userLogin);
        serviceCtx.put("fixedAssetId", fixedAssetId);
        serviceCtx.put("fromDate", fromDate);
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("deleteFixedAssetRegistration", serviceCtx);
            if (ServiceUtil.isError(serviceResult)) {
                String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                request.setAttribute("_ERROR_MESSAGE_", errMsg);
                return "error";
            }
        } catch (Exception e) {
            Debug.logError(e, "Error calling deleteFixedAssetRegistration: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        GenericValue fixedAssetRegistration = null;
        try {
            fixedAssetRegistration = EntityQuery.use(delegator)
                    .from("FixedAssetRegistration")
                    .where(UtilMisc.toMap())
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying FixedAssetRegistration: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        assert UtilValidate.isEmpty(fixedAssetRegistration) : "Assertion failed: if-empty";
        if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
            return "error";
        }

        return "success";
    }


    /**
     * Test case for service createFixedAssetMeter
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String testCreateFixedAssetMeter(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Object fixedAssetId = "DEMO_VEHICLE_01";
        Object productMeterTypeId = "ODOMETER";
        Timestamp readingDate = Timestamp.valueOf("2009-12-24 00:00:00.000");
        BigDecimal meterValue = new BigDecimal("65");
        try {
            userLogin = EntityQuery.use(delegator)
                    .from("UserLogin")
                    .where(UtilMisc.toMap("userLoginId", "system"))
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying UserLogin: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        Map<String, Object> serviceCtx = new HashMap<>();
        serviceCtx.put("userLogin", userLogin);
        serviceCtx.put("fixedAssetId", fixedAssetId);
        serviceCtx.put("productMeterTypeId", productMeterTypeId);
        serviceCtx.put("readingDate", readingDate);
        serviceCtx.put("meterValue", meterValue);
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("createFixedAssetMeter", serviceCtx);
            if (ServiceUtil.isError(serviceResult)) {
                String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                request.setAttribute("_ERROR_MESSAGE_", errMsg);
                return "error";
            }
        } catch (Exception e) {
            Debug.logError(e, "Error calling createFixedAssetMeter: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        GenericValue fixedAssetMeter = null;
        try {
            fixedAssetMeter = EntityQuery.use(delegator)
                    .from("FixedAssetMeter")
                    .where(UtilMisc.toMap())
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying FixedAssetMeter: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        assert !(UtilValidate.isEmpty(fixedAssetMeter)) : "Assertion failed: not if-empty";
        if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
            return "error";
        }

        return "success";
    }


    /**
     * Test case for service updateFixedAssetMeter
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String testUpdateFixedAssetMeter(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Object fixedAssetId = "DEMO_VEHICLE_01";
        Object productMeterTypeId = "ODOMETER";
        Timestamp readingDate = Timestamp.valueOf("2009-12-24 00:00:00.000");
        BigDecimal meterValue = new BigDecimal("85");
        try {
            userLogin = EntityQuery.use(delegator)
                    .from("UserLogin")
                    .where(UtilMisc.toMap("userLoginId", "system"))
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying UserLogin: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        Map<String, Object> serviceCtx = new HashMap<>();
        serviceCtx.put("userLogin", userLogin);
        serviceCtx.put("fixedAssetId", fixedAssetId);
        serviceCtx.put("productMeterTypeId", productMeterTypeId);
        serviceCtx.put("readingDate", readingDate);
        serviceCtx.put("meterValue", meterValue);
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("updateFixedAssetMeter", serviceCtx);
            if (ServiceUtil.isError(serviceResult)) {
                String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                request.setAttribute("_ERROR_MESSAGE_", errMsg);
                return "error";
            }
        } catch (Exception e) {
            Debug.logError(e, "Error calling updateFixedAssetMeter: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        GenericValue fixedAssetMeter = null;
        try {
            fixedAssetMeter = EntityQuery.use(delegator)
                    .from("FixedAssetMeter")
                    .where(UtilMisc.toMap())
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying FixedAssetMeter: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        assert !(UtilValidate.isEmpty(fixedAssetMeter)) : "Assertion failed: not if-empty";
        assert java.util.Objects.equals(((Map<String, Object>) fixedAssetMeter).get("meterValue"), meterValue) : "Assertion failed: if-compare-field";
        if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
            return "error";
        }

        return "success";
    }


    /**
     * Test case for service deleteFixedAssetMeter
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String testDeleteFixedAssetMeter(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Object fixedAssetId = "DEMO_VEHICLE_01";
        Object productMeterTypeId = "ODOMETER";
        Timestamp readingDate = Timestamp.valueOf("2009-12-24 00:00:00.000");
        try {
            userLogin = EntityQuery.use(delegator)
                    .from("UserLogin")
                    .where(UtilMisc.toMap("userLoginId", "system"))
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying UserLogin: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        Map<String, Object> serviceCtx = new HashMap<>();
        serviceCtx.put("userLogin", userLogin);
        serviceCtx.put("fixedAssetId", fixedAssetId);
        serviceCtx.put("productMeterTypeId", productMeterTypeId);
        serviceCtx.put("readingDate", readingDate);
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("deleteFixedAssetMeter", serviceCtx);
            if (ServiceUtil.isError(serviceResult)) {
                String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                request.setAttribute("_ERROR_MESSAGE_", errMsg);
                return "error";
            }
        } catch (Exception e) {
            Debug.logError(e, "Error calling deleteFixedAssetMeter: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        GenericValue fixedAssetMeter = null;
        try {
            fixedAssetMeter = EntityQuery.use(delegator)
                    .from("FixedAssetMeter")
                    .where(UtilMisc.toMap())
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying FixedAssetMeter: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        assert UtilValidate.isEmpty(fixedAssetMeter) : "Assertion failed: if-empty";
        if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
            return "error";
        }

        return "success";
    }


    /**
     * Test case for service createFixedAssetGeoPoint
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String testCreateFixedAssetGeoPoint(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Object geoPointId = "9000";
        Object fixedAssetId = "DEMO_VEHICLE_01";
        try {
            userLogin = EntityQuery.use(delegator)
                    .from("UserLogin")
                    .where(UtilMisc.toMap("userLoginId", "system"))
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying UserLogin: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        Map<String, Object> serviceCtx = new HashMap<>();
        serviceCtx.put("userLogin", userLogin);
        serviceCtx.put("geoPointId", geoPointId);
        serviceCtx.put("fixedAssetId", fixedAssetId);
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("createFixedAssetGeoPoint", serviceCtx);
            if (ServiceUtil.isError(serviceResult)) {
                String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                request.setAttribute("_ERROR_MESSAGE_", errMsg);
                return "error";
            }
        } catch (Exception e) {
            Debug.logError(e, "Error calling createFixedAssetGeoPoint: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        List<GenericValue> fixedAssetGeoPoints = null;
        try {
            fixedAssetGeoPoints = EntityQuery.use(delegator)
                    .from("FixedAssetGeoPoint")
                    .where(UtilMisc.toMap("geoPointId", geoPointId, "fixedAssetId", fixedAssetId))
                    .filterByDate()
                    .queryList();
        } catch (Exception e) {
            Debug.logError(e, "Error querying FixedAssetGeoPoint: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        GenericValue fixedAssetGeoPoint = EntityUtil.getFirst((List<GenericValue>) fixedAssetGeoPoints);
        assert !(UtilValidate.isEmpty(fixedAssetGeoPoint)) : "Assertion failed: not if-empty";
        assert java.util.Objects.equals(((Map<String, Object>) fixedAssetGeoPoint).get("geoPointId"), geoPointId) : "Assertion failed: if-compare-field";
        assert java.util.Objects.equals(((Map<String, Object>) fixedAssetGeoPoint).get("fixedAssetId"), fixedAssetId) : "Assertion failed: if-compare-field";
        if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
            return "error";
        }

        return "success";
    }

}
