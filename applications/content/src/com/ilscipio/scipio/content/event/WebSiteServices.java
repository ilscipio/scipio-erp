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
package com.ilscipio.scipio.content.event;

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
import org.ofbiz.service.LocalDispatcher;
import org.ofbiz.service.ServiceUtil;

/**
 * Auto-generated event class from Simple Method XML.
 *
 * <p>Generated from: component://content/script/org/ofbiz/content/website/WebSiteServices.xml</p>
 *
 * <p>SCIPIO: 4.0.0: Auto-generated.</p>
 */
public class WebSiteServices {

    private static final String MODULE = WebSiteServices.class.getName();


    /**
     * Create a WebSite
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String createWebSite(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        GenericValue newEntity = delegator.makeValue("WebSite");
        newEntity.setNonPKFields((Map<String, Object>) context);
        newEntity.setPKFields((Map<String, Object>) context);
        try {
            delegator.create(newEntity);
        } catch (Exception e) {
            Debug.logError(e, "Error creating value: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }

        return "success";
    }


    /**
     * Update a WebSite
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String updateWebSite(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        GenericValue webSite = null;
        try {
            webSite = EntityQuery.use(delegator)
                    .from("WebSite")
                    .where(UtilMisc.toMap())
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying WebSite: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        webSite.setNonPKFields((Map<String, Object>) context);
        try {
            delegator.store(webSite);
        } catch (Exception e) {
            Debug.logError(e, "Error storing value: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }

        return "success";
    }


    /**
     * Remove a WebSite
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String removeWebSite(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        GenericValue lookupKeyValue = delegator.makeValue("WebSite");
        lookupKeyValue.setPKFields((Map<String, Object>) context);
        GenericValue lookedUpValue = null;
        try {
            lookedUpValue = EntityQuery.use(delegator)
                    .from("WebSite")
                    .where(lookupKeyValue)
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error finding by primary key WebSite: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        lookedUpValue.setNonPKFields((Map<String, Object>) context);
        try {
            delegator.store(lookedUpValue);
        } catch (Exception e) {
            Debug.logError(e, "Error storing value: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }

        return "success";
    }


    /**
     * Create WebSite Content
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String createWebSiteContent(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        GenericValue newEntity = null;
        Timestamp nowTimestamp = null;
        newEntity = delegator.makeValue("WebSiteContent");
        newEntity.setPKFields((Map<String, Object>) context);
        newEntity.setNonPKFields((Map<String, Object>) context);
        if (UtilValidate.isEmpty(((Map<String, Object>) newEntity).get("fromDate"))) {
            nowTimestamp = new Timestamp(System.currentTimeMillis());
            newEntity.put("fromDate", nowTimestamp);
        }
        try {
            delegator.create(newEntity);
        } catch (Exception e) {
            Debug.logError(e, "Error creating value: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }

        return "success";
    }


    /**
     * Update WebSite Content
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String updateWebSiteContent(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        GenericValue lookupPKMap = delegator.makeValue("WebSiteContent");
        lookupPKMap.setPKFields((Map<String, Object>) context);
        GenericValue lookedUpValue = null;
        try {
            lookedUpValue = EntityQuery.use(delegator)
                    .from("WebSiteContent")
                    .where(lookupPKMap)
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error finding by primary key WebSiteContent: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        lookedUpValue.setNonPKFields((Map<String, Object>) context);
        try {
            delegator.store(lookedUpValue);
        } catch (Exception e) {
            Debug.logError(e, "Error storing value: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }

        return "success";
    }


    /**
     * Remove WebSite Content
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String removeWebSiteContent(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        GenericValue lookupPKMap = delegator.makeValue("WebSiteContent");
        lookupPKMap.setPKFields((Map<String, Object>) context);
        GenericValue lookedUpValue = null;
        try {
            lookedUpValue = EntityQuery.use(delegator)
                    .from("WebSiteContent")
                    .where(lookupPKMap)
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error finding by primary key WebSiteContent: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        try {
            delegator.removeValue(lookedUpValue);
        } catch (Exception e) {
            Debug.logError(e, "Error removing value: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }

        return "success";
    }


    /**
     * Create WebSite Content Type
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String createWebSiteContentType(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        GenericValue newEntity = null;
        String webSiteContentTypeId = null;
        newEntity = delegator.makeValue("WebSiteContentType");
        newEntity.setPKFields((Map<String, Object>) context);
        newEntity.setNonPKFields((Map<String, Object>) context);
        if (UtilValidate.isEmpty(((Map<String, Object>) newEntity).get("webSiteContentTypeId"))) {
            webSiteContentTypeId = delegator.getNextSeqId("WebSiteContentTypeId");
            newEntity.put("webSiteContentTypeId", webSiteContentTypeId);
        }
        try {
            delegator.create(newEntity);
        } catch (Exception e) {
            Debug.logError(e, "Error creating value: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }

        return "success";
    }


    /**
     * Update WebSite Content Type
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String updateWebSiteContentType(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        GenericValue lookupPKMap = delegator.makeValue("WebSiteContentType");
        lookupPKMap.setPKFields((Map<String, Object>) context);
        GenericValue lookedUpValue = null;
        try {
            lookedUpValue = EntityQuery.use(delegator)
                    .from("WebSiteContentType")
                    .where(lookupPKMap)
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error finding by primary key WebSiteContentType: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        lookedUpValue.setNonPKFields((Map<String, Object>) context);
        try {
            delegator.store(lookedUpValue);
        } catch (Exception e) {
            Debug.logError(e, "Error storing value: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }

        return "success";
    }


    /**
     * Remove WebSite Content Type
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String removeWebSiteContentType(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        GenericValue lookupPKMap = delegator.makeValue("WebSiteContentType");
        lookupPKMap.setPKFields((Map<String, Object>) context);
        GenericValue lookedUpValue = null;
        try {
            lookedUpValue = EntityQuery.use(delegator)
                    .from("WebSiteContentType")
                    .where(lookupPKMap)
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error finding by primary key WebSiteContentType: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        try {
            delegator.removeValue(lookedUpValue);
        } catch (Exception e) {
            Debug.logError(e, "Error removing value: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }

        return "success";
    }


    /**
     * Create WebSite Path Alias
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String createWebSitePathAlias(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        GenericValue newEntity = delegator.makeValue("WebSitePathAlias");
        newEntity.setPKFields((Map<String, Object>) context);
        newEntity.setNonPKFields((Map<String, Object>) context);
        try {
            delegator.create(newEntity);
        } catch (Exception e) {
            Debug.logError(e, "Error creating value: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }

        return "success";
    }


    /**
     * Update WebSite Path Alias
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String updateWebSitePathAlias(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        GenericValue lookupPKMap = delegator.makeValue("WebSitePathAlias");
        lookupPKMap.setPKFields((Map<String, Object>) context);
        GenericValue lookedUpValue = null;
        try {
            lookedUpValue = EntityQuery.use(delegator)
                    .from("WebSitePathAlias")
                    .where(lookupPKMap)
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error finding by primary key WebSitePathAlias: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        lookedUpValue.setNonPKFields((Map<String, Object>) context);
        try {
            delegator.store(lookedUpValue);
        } catch (Exception e) {
            Debug.logError(e, "Error storing value: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }

        return "success";
    }


    /**
     * Remove WebSite Path Alias
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String removeWebSitePathAlias(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        GenericValue lookupPKMap = delegator.makeValue("WebSitePathAlias");
        lookupPKMap.setPKFields((Map<String, Object>) context);
        GenericValue lookedUpValue = null;
        try {
            lookedUpValue = EntityQuery.use(delegator)
                    .from("WebSitePathAlias")
                    .where(lookupPKMap)
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error finding by primary key WebSitePathAlias: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        try {
            delegator.removeValue(lookedUpValue);
        } catch (Exception e) {
            Debug.logError(e, "Error removing value: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }

        return "success";
    }


    /**
     * Returns a WebSite Path Alias
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String getWebSitePathAlias(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> result = new HashMap<>();

        GenericValue lookupPKMap = delegator.makeValue("WebSitePathAlias");
        lookupPKMap.setPKFields((Map<String, Object>) context);
        GenericValue alias = null;
        try {
            alias = EntityQuery.use(delegator)
                    .from("WebSitePathAlias")
                    .where(lookupPKMap)
                    .cache()
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error finding by primary key WebSitePathAlias: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        result.put("pathTo", ((Map<String, Object>) alias).get("pathTo"));

        return "success";
    }


    /**
     * Create WebSite Role
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String createWebSiteRole(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        GenericValue newEntity = null;
        Timestamp nowTimestamp = null;
        newEntity = delegator.makeValue("WebSiteRole");
        newEntity.setPKFields((Map<String, Object>) context);
        newEntity.setNonPKFields((Map<String, Object>) context);
        if (UtilValidate.isEmpty(((Map<String, Object>) newEntity).get("fromDate"))) {
            nowTimestamp = new Timestamp(System.currentTimeMillis());
            newEntity.put("fromDate", nowTimestamp);
        }
        try {
            delegator.create(newEntity);
        } catch (Exception e) {
            Debug.logError(e, "Error creating value: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }

        return "success";
    }


    /**
     * Update WebSite Role
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String updateWebSiteRole(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        GenericValue lookupPKMap = delegator.makeValue("WebSiteRole");
        lookupPKMap.setPKFields((Map<String, Object>) context);
        GenericValue lookedUpValue = null;
        try {
            lookedUpValue = EntityQuery.use(delegator)
                    .from("WebSiteRole")
                    .where(lookupPKMap)
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error finding by primary key WebSiteRole: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        lookedUpValue.setNonPKFields((Map<String, Object>) context);
        try {
            delegator.store(lookedUpValue);
        } catch (Exception e) {
            Debug.logError(e, "Error storing value: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }

        return "success";
    }


    /**
     * Remove WebSite Role
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String removeWebSiteRole(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        GenericValue lookupPKMap = delegator.makeValue("WebSiteRole");
        lookupPKMap.setPKFields((Map<String, Object>) context);
        GenericValue lookedUpValue = null;
        try {
            lookedUpValue = EntityQuery.use(delegator)
                    .from("WebSiteRole")
                    .where(lookupPKMap)
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error finding by primary key WebSiteRole: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        try {
            delegator.removeValue(lookedUpValue);
        } catch (Exception e) {
            Debug.logError(e, "Error removing value: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }

        return "success";
    }


    /**
     * Auto-Create WebSite CMS Content
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String quickCreateWebSiteContent(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> wcc = null;
        Map<String, Object> cnt = null;
        GenericValue wct = null;
        GenericValue webSite = null;
        try {
            webSite = EntityQuery.use(delegator)
                    .from("WebSite")
                    .where(UtilMisc.toMap("webSiteId", context.get("webSiteId")))
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying WebSite: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        Timestamp now = new Timestamp(System.currentTimeMillis());
        // TODO: Convert <if-instance-of> element

        return "success";
    }


    /**
     * Generate Missing Seo URL's for Website
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String generateMissingSeoUrlForWebsite(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Object categoriesUpdated = null;
        Object contentsUpdated = null;
        Object totalContentsNotUpdated = null;
        Object totalCategoriesUpdated = null;
        Object totalProductsUpdated = null;
        List<GenericValue> productStoreCatalogs = null;
        Object productsNotUpdated = null;
        Object totalCategoriesNotUpdated = null;
        Object contentsNotUpdated = null;
        Map<String, Object> createMissingCategoryAltUrlsMap = null;
        Object totalProductsNotUpdated = null;
        Map<String, Object> createMissingProductAltUrlsMap = null;
        Object totalContentsUpdated = null;
        Map<String, Object> createMissingContentAltUrlsMap = null;
        Object categoriesNotUpdated = null;
        Object productsUpdated = null;
        List<Object> successMessageList = null;
        Object contentMessage = null;
        Object categoriesMessage = null;
        Object productMessage = null;
        totalCategoriesNotUpdated = 0;
        totalCategoriesUpdated = 0;
        totalProductsNotUpdated = 0;
        totalProductsUpdated = 0;
        totalContentsNotUpdated = 0;
        totalContentsUpdated = 0;
        GenericValue webSite = null;
        try {
            webSite = EntityQuery.use(delegator)
                    .from("WebSite")
                    .where(UtilMisc.toMap("webSiteId", context.get("webSiteId")))
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying WebSite: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        GenericValue productStoreCatalog = null;
        GenericValue typeGenerate = null;
        if ("all".equals(context.get("prodCatalogId"))) {
            try {
                productStoreCatalogs = EntityQuery.use(delegator)
                        .from("ProductStoreCatalog")
                        .where(UtilMisc.toMap("productStoreId", ((Map<String, Object>) webSite).get("productStoreId")))
                        .filterByDate()
                        .queryList();
            } catch (Exception e) {
                Debug.logError(e, "Error querying ProductStoreCatalog: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
            if (UtilValidate.isNotEmpty(productStoreCatalogs)) {
                if (productStoreCatalogs != null) {
                    for (GenericValue productStoreCatalogEntry : productStoreCatalogs) {
                        if (context.get("typeGenerate") != null) {
                            for (GenericValue typeGenerateEntry : (List<GenericValue>) context.get("typeGenerate")) {
                                if ("category".equals(typeGenerateEntry)) {
                                    createMissingCategoryAltUrlsMap.put("prodCatalogId", ((Map<String, Object>) productStoreCatalogEntry).get("prodCatalogId"));
                                    createMissingCategoryAltUrlsMap.put("category", "category");
                                    try {
                                        Map<String, Object> serviceResult = dispatcher.runSync("createMissingCategoryAndProductAltUrls", createMissingCategoryAltUrlsMap);
                                        if (ServiceUtil.isError(serviceResult)) {
                                            String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                                            request.setAttribute("_ERROR_MESSAGE_", errMsg);
                                            return "error";
                                        }
                                        categoriesNotUpdated = serviceResult.get("categoriesNotUpdated");
                                        categoriesUpdated = serviceResult.get("categoriesUpdated");
                                    } catch (Exception e) {
                                        Debug.logError(e, "Error calling createMissingCategoryAndProductAltUrls: " + e.getMessage(), MODULE);
                                        request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                                        return "error";
                                    }
                                    totalCategoriesNotUpdated = new BigDecimal(categoriesNotUpdated.toString());
                                    totalCategoriesUpdated = new BigDecimal(categoriesUpdated.toString());
                                }
                                if ("product".equals(typeGenerateEntry)) {
                                    createMissingProductAltUrlsMap.put("prodCatalogId", ((Map<String, Object>) productStoreCatalogEntry).get("prodCatalogId"));
                                    createMissingProductAltUrlsMap.put("product", "product");
                                    try {
                                        Map<String, Object> serviceResult = dispatcher.runSync("createMissingCategoryAndProductAltUrls", createMissingProductAltUrlsMap);
                                        if (ServiceUtil.isError(serviceResult)) {
                                            String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                                            request.setAttribute("_ERROR_MESSAGE_", errMsg);
                                            return "error";
                                        }
                                        productsNotUpdated = serviceResult.get("productsNotUpdated");
                                        productsUpdated = serviceResult.get("productsUpdated");
                                    } catch (Exception e) {
                                        Debug.logError(e, "Error calling createMissingCategoryAndProductAltUrls: " + e.getMessage(), MODULE);
                                        request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                                        return "error";
                                    }
                                    totalProductsNotUpdated = new BigDecimal(productsNotUpdated.toString());
                                    totalProductsUpdated = new BigDecimal(productsUpdated.toString());
                                }
                                if ("content".equals(typeGenerateEntry)) {
                                    createMissingContentAltUrlsMap.put("webSiteId", context.get("webSiteId"));
                                    createMissingContentAltUrlsMap.put("prodCatalogId", ((Map<String, Object>) productStoreCatalogEntry).get("prodCatalogId"));
                                    try {
                                        Map<String, Object> serviceResult = dispatcher.runSync("createMissingContentAltUrls", createMissingContentAltUrlsMap);
                                        if (ServiceUtil.isError(serviceResult)) {
                                            String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                                            request.setAttribute("_ERROR_MESSAGE_", errMsg);
                                            return "error";
                                        }
                                        contentsNotUpdated = serviceResult.get("contentsNotUpdated");
                                        contentsUpdated = serviceResult.get("contentsUpdated");
                                    } catch (Exception e) {
                                        Debug.logError(e, "Error calling createMissingContentAltUrls: " + e.getMessage(), MODULE);
                                        request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                                        return "error";
                                    }
                                    totalContentsNotUpdated = new BigDecimal(productsNotUpdated.toString());
                                    totalContentsUpdated = new BigDecimal(contentsUpdated.toString());
                                }
                            }
                        }
                    }
                }
            } else {
                if (context.get("typeGenerate") != null) {
                    for (GenericValue typeGenerateEntry : (List<GenericValue>) context.get("typeGenerate")) {
                        if ("content".equals(typeGenerateEntry)) {
                            createMissingContentAltUrlsMap.put("webSiteId", context.get("webSiteId"));
                            try {
                                Map<String, Object> serviceResult = dispatcher.runSync("createMissingContentAltUrls", createMissingContentAltUrlsMap);
                                if (ServiceUtil.isError(serviceResult)) {
                                    String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                                    request.setAttribute("_ERROR_MESSAGE_", errMsg);
                                    return "error";
                                }
                                contentsNotUpdated = serviceResult.get("contentsNotUpdated");
                                contentsUpdated = serviceResult.get("contentsUpdated");
                            } catch (Exception e) {
                                Debug.logError(e, "Error calling createMissingContentAltUrls: " + e.getMessage(), MODULE);
                                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                                return "error";
                            }
                            totalContentsNotUpdated = new BigDecimal(productsNotUpdated.toString());
                            totalContentsUpdated = new BigDecimal(contentsUpdated.toString());
                        }
                    }
                }
            }
        } else {
            if (context.get("typeGenerate") != null) {
                for (GenericValue typeGenerateEntry : (List<GenericValue>) context.get("typeGenerate")) {
                    if ("category".equals(typeGenerateEntry)) {
                        createMissingCategoryAltUrlsMap.put("prodCatalogId", context.get("prodCatalogId"));
                        createMissingCategoryAltUrlsMap.put("category", "category");
                        try {
                            Map<String, Object> serviceResult = dispatcher.runSync("createMissingCategoryAndProductAltUrls", createMissingCategoryAltUrlsMap);
                            if (ServiceUtil.isError(serviceResult)) {
                                String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                                request.setAttribute("_ERROR_MESSAGE_", errMsg);
                                return "error";
                            }
                            categoriesNotUpdated = serviceResult.get("categoriesNotUpdated");
                            categoriesUpdated = serviceResult.get("categoriesUpdated");
                        } catch (Exception e) {
                            Debug.logError(e, "Error calling createMissingCategoryAndProductAltUrls: " + e.getMessage(), MODULE);
                            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                            return "error";
                        }
                        totalCategoriesNotUpdated = new BigDecimal(categoriesNotUpdated.toString());
                        totalCategoriesUpdated = new BigDecimal(categoriesUpdated.toString());
                    }
                    if ("product".equals(typeGenerateEntry)) {
                        createMissingProductAltUrlsMap.put("prodCatalogId", context.get("prodCatalogId"));
                        createMissingProductAltUrlsMap.put("product", "product");
                        try {
                            Map<String, Object> serviceResult = dispatcher.runSync("createMissingCategoryAndProductAltUrls", createMissingProductAltUrlsMap);
                            if (ServiceUtil.isError(serviceResult)) {
                                String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                                request.setAttribute("_ERROR_MESSAGE_", errMsg);
                                return "error";
                            }
                            productsNotUpdated = serviceResult.get("productsNotUpdated");
                            productsUpdated = serviceResult.get("productsUpdated");
                        } catch (Exception e) {
                            Debug.logError(e, "Error calling createMissingCategoryAndProductAltUrls: " + e.getMessage(), MODULE);
                            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                            return "error";
                        }
                        totalProductsNotUpdated = new BigDecimal(productsNotUpdated.toString());
                        totalProductsUpdated = new BigDecimal(productsUpdated.toString());
                    }
                    if ("content".equals(typeGenerateEntry)) {
                        createMissingContentAltUrlsMap.put("webSiteId", context.get("webSiteId"));
                        createMissingContentAltUrlsMap.put("prodCatalogId", context.get("prodCatalogId"));
                        try {
                            Map<String, Object> serviceResult = dispatcher.runSync("createMissingContentAltUrls", createMissingContentAltUrlsMap);
                            if (ServiceUtil.isError(serviceResult)) {
                                String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                                request.setAttribute("_ERROR_MESSAGE_", errMsg);
                                return "error";
                            }
                            contentsNotUpdated = serviceResult.get("contentsNotUpdated");
                            contentsUpdated = serviceResult.get("contentsUpdated");
                        } catch (Exception e) {
                            Debug.logError(e, "Error calling createMissingContentAltUrls: " + e.getMessage(), MODULE);
                            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                            return "error";
                        }
                        totalContentsNotUpdated = new BigDecimal(contentsNotUpdated.toString());
                        totalContentsUpdated = new BigDecimal(contentsUpdated.toString());
                    }
                }
            }
        }
        Object generateMissingSeoUrlMessage = "Generated missing SEO urls successfully";
        successMessageList.add(generateMissingSeoUrlMessage);
        if (context.get("typeGenerate") != null) {
            for (GenericValue typeGenerateEntry : (List<GenericValue>) context.get("typeGenerate")) {
                if ("category".equals(typeGenerateEntry)) {
                    categoriesMessage = "Categories already having SEO urls: " + totalCategoriesNotUpdated + ", Categories with url added: " + totalCategoriesUpdated;
                    successMessageList.add(categoriesMessage);
                }
                if ("product".equals(typeGenerateEntry)) {
                    productMessage = "Products already having SEO urls: " + totalProductsNotUpdated + ", Products with url added: " + totalProductsUpdated;
                    successMessageList.add(productMessage);
                }
                if ("content".equals(typeGenerateEntry)) {
                    contentMessage = "Contents already having SEO urls: " + totalContentsNotUpdated + ", Contents with url added: " + totalContentsUpdated;
                    successMessageList.add(contentMessage);
                }
            }
        }

        return "success";
    }

}
