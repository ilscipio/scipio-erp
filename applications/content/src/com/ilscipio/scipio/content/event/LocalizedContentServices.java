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
 * <p>Generated from: component://content/script/org/ofbiz/content/content/LocalizedContentServices.xml</p>
 *
 * <p>SCIPIO: 4.0.0: Auto-generated.</p>
 */
public class LocalizedContentServices {

    private static final String MODULE = LocalizedContentServices.class.getName();


    /**
     * Create Simple Text Content For Alternate Locale
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String createSimpleTextContentForAlternateLocale(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> createSimpleTextMap = new HashMap<>();
        // set-service-fields from "parameters" to "createSimpleTextMap" for service "createSimpleTextContent"
        createSimpleTextMap.putAll(UtilMisc.toMap(context));
        Map<String, Object> createContentAssocMap = new HashMap<>();
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("createSimpleTextContent", createSimpleTextMap);
            if (ServiceUtil.isError(serviceResult)) {
                String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                request.setAttribute("_ERROR_MESSAGE_", errMsg);
                return "error";
            }
            createContentAssocMap.put("contentIdTo", serviceResult.get("contentId"));
        } catch (Exception e) {
            Debug.logError(e, "Error calling createSimpleTextContent: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        createContentAssocMap.put("contentId", context.get("mainContentId"));
        createContentAssocMap.put("contentAssocTypeId", "ALTERNATE_LOCALE");
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("createContentAssoc", createContentAssocMap);
            if (ServiceUtil.isError(serviceResult)) {
                String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                request.setAttribute("_ERROR_MESSAGE_", errMsg);
                return "error";
            }
        } catch (Exception e) {
            Debug.logError(e, "Error calling createContentAssoc: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }

        return "success";
    }


    /**
     * Update Simple Text Content For Alternate Locale (SCIPIO)
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String updateSimpleTextContentForAlternateLocale(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        String result = checkSimpleTextContentAssoc(request, response);
        if (!"success".equals(result)) {
            return result;
        }
        if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
            return "error";
        }
        GenericValue content = null;
        try {
            content = EntityQuery.use(delegator)
                    .from("Content")
                    .where(UtilMisc.toMap("contentId", context.get("contentId")))
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying Content: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        content.setNonPKFields((Map<String, Object>) context);
        try {
            delegator.store(content);
        } catch (Exception e) {
            Debug.logError(e, "Error storing value: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        Map<String, Object> updateCtx = new HashMap<>();
        // set-service-fields from "parameters" to "updateCtx" for service "updateSimpleTextContent"
        updateCtx.putAll(UtilMisc.toMap(context));
        updateCtx.put("textDataResourceId", ((Map<String, Object>) content).get("dataResourceId"));
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("updateSimpleTextContent", updateCtx);
            if (ServiceUtil.isError(serviceResult)) {
                String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                request.setAttribute("_ERROR_MESSAGE_", errMsg);
                return "error";
            }
        } catch (Exception e) {
            Debug.logError(e, "Error calling updateSimpleTextContent: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }

        return "success";
    }


    /**
     * Delete Simple Text Content For Alternate Locale (SCIPIO)
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String deleteSimpleTextContentForAlternateLocale(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        String result = checkSimpleTextContentAssoc(request, response);
        if (!"success".equals(result)) {
            return result;
        }
        if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
            return "error";
        }
        Map<String, Object> removeCtx = new HashMap<>();
        // set-service-fields from "parameters" to "removeCtx" for service "removeContentAndRelated"
        removeCtx.putAll(UtilMisc.toMap(context));
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("removeContentAndRelated", removeCtx);
            if (ServiceUtil.isError(serviceResult)) {
                String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                request.setAttribute("_ERROR_MESSAGE_", errMsg);
                return "error";
            }
        } catch (Exception e) {
            Debug.logError(e, "Error calling removeContentAndRelated: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }

        return "success";
    }


    /**
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String checkSimpleTextContentAssoc(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        List<String> error_list = new LinkedList<>();

        Object contentId = null;
        String errMsg = null;
        List<GenericValue> assocList = null;
        try {
            assocList = EntityQuery.use(delegator)
                    .from("ContentAssoc")
                    .where(UtilMisc.toMap("contentId", context.get("mainContentId"), "contentAssocTypeId", "ALTERNATE_LOCALE", "contentIdTo", context.get("contentId")))
                    .filterByDate()
                    .queryList();
        } catch (Exception e) {
            Debug.logError(e, "Error querying ContentAssoc: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        if (UtilValidate.isEmpty(assocList)) {
            contentId = context.get("contentId");
            errMsg = UtilProperties.getMessage("ContentUiLabels", "ContentNoContentFound", locale);
            error_list.add("${errMsg} (mainContentId: ${parameters.mainContentId})");
            request.setAttribute("_ERROR_MESSAGE_", "${errMsg} (mainContentId: ${parameters.mainContentId})");
            if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                    UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
                return "error";
            }
        }

        return "success";
    }


    /**
     * Create/Update Simple Text Content For Alternate Locale (SCIPIO)
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String createUpdateSimpleTextContentForAlternateLocale(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        List<String> error_list = new LinkedList<>();

        List<GenericValue> assocList = null;
        Map<String, Object> createCtx = null;
        GenericValue mainContent = null;
        Object contentId = null;
        Boolean localeFound = null;
        GenericValue content = null;
        Map<String, Object> updateCtx = null;
        GenericValue contentAssoc = null;
        if (UtilValidate.isNotEmpty(context.get("contentId"))) {
            // set-service-fields from "parameters" to "updateCtx" for service "updateSimpleTextContentForAlternateLocale"
            updateCtx.putAll(UtilMisc.toMap(context));
            try {
                Map<String, Object> serviceResult = dispatcher.runSync("updateSimpleTextContentForAlternateLocale", updateCtx);
                if (ServiceUtil.isError(serviceResult)) {
                    String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                    request.setAttribute("_ERROR_MESSAGE_", errMsg);
                    return "error";
                }
            } catch (Exception e) {
                Debug.logError(e, "Error calling updateSimpleTextContentForAlternateLocale: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
        } else {
            try {
                mainContent = EntityQuery.use(delegator)
                        .from("Content")
                        .where(UtilMisc.toMap("contentId", context.get("mainContentId")))
                        .queryOne();
            } catch (Exception e) {
                Debug.logError(e, "Error querying Content: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
            if (UtilValidate.isEmpty(mainContent)) {
                contentId = context.get("mainContentId");
                {
                    String errorMsg = UtilProperties.getMessage("ContentUiLabels", "ContentNoContentFound", locale);
                    error_list.add(errorMsg);
                    request.setAttribute("_ERROR_MESSAGE_", errorMsg);
                }
                if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                        UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
                    return "error";
                }
            }
            if (java.util.Objects.equals(((Map<String, Object>) mainContent).get("localeString"), context.get("localeString"))) {
                updateCtx.put("textDataResourceId", ((Map<String, Object>) mainContent).get("dataResourceId"));
                updateCtx.put("text", context.get("text"));
                try {
                    Map<String, Object> serviceResult = dispatcher.runSync("updateSimpleTextContent", updateCtx);
                    if (ServiceUtil.isError(serviceResult)) {
                        String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                        request.setAttribute("_ERROR_MESSAGE_", errMsg);
                        return "error";
                    }
                } catch (Exception e) {
                    Debug.logError(e, "Error calling updateSimpleTextContent: " + e.getMessage(), MODULE);
                    request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                    return "error";
                }
            } else {
                try {
                    assocList = EntityQuery.use(delegator)
                            .from("ContentAssoc")
                            .where(UtilMisc.toMap("contentId", context.get("mainContentId"), "contentAssocTypeId", "ALTERNATE_LOCALE"))
                            .filterByDate()
                            .queryList();
                } catch (Exception e) {
                    Debug.logError(e, "Error querying ContentAssoc: " + e.getMessage(), MODULE);
                    request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                    return "error";
                }
                localeFound = Boolean.FALSE;
                if (assocList != null) {
                    for (GenericValue contentAssocEntry : assocList) {
                        try {
                            content = contentAssocEntry.getRelatedOne("ToContent", false);
                        } catch (Exception e) {
                            Debug.logError(e, "Error getting related one ToContent: " + e.getMessage(), MODULE);
                            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                            return "error";
                        }
                        if (java.util.Objects.equals(((Map<String, Object>) content).get("localeString"), context.get("localeString"))) {
                            localeFound = Boolean.TRUE;
                            updateCtx.put("textDataResourceId", ((Map<String, Object>) content).get("dataResourceId"));
                            updateCtx.put("text", context.get("text"));
                            try {
                                Map<String, Object> serviceResult = dispatcher.runSync("updateSimpleTextContent", updateCtx);
                                if (ServiceUtil.isError(serviceResult)) {
                                    String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                                    request.setAttribute("_ERROR_MESSAGE_", errMsg);
                                    return "error";
                                }
                            } catch (Exception e) {
                                Debug.logError(e, "Error calling updateSimpleTextContent: " + e.getMessage(), MODULE);
                                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                                return "error";
                            }
                            // TODO: Convert <break> element
                        }
                    }
                }
                if (Boolean.FALSE.equals(localeFound)) {
                    // set-service-fields from "parameters" to "createCtx" for service "createSimpleTextContentForAlternateLocale"
                    createCtx.putAll(UtilMisc.toMap(context));
                    try {
                        Map<String, Object> serviceResult = dispatcher.runSync("createSimpleTextContentForAlternateLocale", createCtx);
                        if (ServiceUtil.isError(serviceResult)) {
                            String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                            request.setAttribute("_ERROR_MESSAGE_", errMsg);
                            return "error";
                        }
                    } catch (Exception e) {
                        Debug.logError(e, "Error calling createSimpleTextContentForAlternateLocale: " + e.getMessage(), MODULE);
                        request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                        return "error";
                    }
                }
            }
        }

        return "success";
    }

}
