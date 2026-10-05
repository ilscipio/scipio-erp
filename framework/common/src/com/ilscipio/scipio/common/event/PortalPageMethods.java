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
package com.ilscipio.scipio.common.event;

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
import org.ofbiz.entity.condition.EntityCondition;
import org.ofbiz.entity.util.EntityQuery;
import org.ofbiz.service.DispatchContext;
import org.ofbiz.service.LocalDispatcher;
import org.ofbiz.service.ServiceUtil;

/**
 * Auto-generated event class from Simple Method XML.
 *
 * <p>Generated from: component://common/script/org/ofbiz/common/PortalPageMethods.xml</p>
 *
 * <p>SCIPIO: 4.0.0: Auto-generated.</p>
 */
public class PortalPageMethods {

    private static final String MODULE = PortalPageMethods.class.getName();


    /**
     * Sets a PortalPortlet attributes
     */
    public static Map<String, Object> setPortalPortletAttributes(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        GenericValue attributeItem = null;
        List<GenericValue> attributeList = null;
        Map<String, Object> attributeEntityMap = null;
        if (UtilValidate.isNotEmpty(context)) {
            for (Map.Entry<String, Object> entry : ((Map<String, Object>) context).entrySet()) {
                String attributeKey = entry.getKey();
                Object attributeValue = entry.getValue();
                Object attributeEntityMap_attrName = null;
                Object attributeEntityMap_attrValue = null;
                if ((!"portalPageId".equals(attributeKey) && !"portalPortletId".equals(attributeKey) && !"portletSeqId".equals(attributeKey))) {
                    Debug.logInfo("===2==processing: " + attributeKey, MODULE);
                    attributeEntityMap = new HashMap<String, Object>();
                    // set-service-fields from "parameters" to "attributeEntityMap" for service "createPortletAttribute"
                    attributeEntityMap.putAll(UtilMisc.toMap(context));
                    attributeEntityMap.put("attrName", attributeKey);
                    attributeEntityMap.put("attrValue", attributeValue);
                    try {
                        attributeItem = EntityQuery.use(delegator)
                                .from("PortletAttribute")
                                .where(UtilMisc.toMap("attrName", ((Map<String, Object>) attributeEntityMap).get("attrName")))
                                .queryOne();
                    } catch (Exception e) {
                        Debug.logError(e, "Error querying PortletAttribute: " + e.getMessage(), MODULE);
                        return ServiceUtil.returnError(e.getMessage());
                    }
                    if (UtilValidate.isNotEmpty(attributeItem)) {
                        try {
                            delegator.removeValue(attributeItem);
                        } catch (Exception e) {
                            Debug.logError(e, "Error removing value: " + e.getMessage(), MODULE);
                            return ServiceUtil.returnError(e.getMessage());
                        }
                    }
                    try {
                        Map<String, Object> serviceResult = dispatcher.runSync("createPortletAttribute", attributeEntityMap);
                        if (ServiceUtil.isError(serviceResult)) {
                            return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
                        }
                    } catch (Exception e) {
                        Debug.logError(e, "Error calling createPortletAttribute: " + e.getMessage(), MODULE);
                        return ServiceUtil.returnError(e.getMessage());
                    }
                    try {
                        attributeList = EntityQuery.use(delegator)
                                .from("PortletAttribute")
                                .queryList();
                    } catch (Exception e) {
                        Debug.logError(e, "Error querying PortletAttribute: " + e.getMessage(), MODULE);
                        return ServiceUtil.returnError(e.getMessage());
                    }
                    if (attributeList != null) {
                        for (GenericValue attribute : attributeList) {
                            if (UtilValidate.isEmpty(((Map<String, Object>) context.get("${attribute")).get("attrName}"))) {
                                try {
                                    delegator.removeValue(attribute);
                                } catch (Exception e) {
                                    Debug.logError(e, "Error removing value: " + e.getMessage(), MODULE);
                                    return ServiceUtil.returnError(e.getMessage());
                                }
                            }
                        }
                    }
                }
            }
        }

        return result;
    }


    /**
     * Check if the page is a system page, then copy before allowing the user to edit it
     */
    public static String copyIfRequiredSystemPage(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Object portalPageId = null;
        Map<String, Object> createPage = null;
        List<GenericValue> getPrivatePages = null;
        Map<String, Object> inlineResult = null;
        List<GenericValue> portletAttributes = null;
        GenericValue portalPageColumn = null;
        List<GenericValue> portalPagePortlets = null;
        GenericValue portletAttribute = null;
        Map<String, Object> delMap = null;
        List<GenericValue> portalPageColumns = null;
        Map<String, Object> addColumnMap = null;
        Map<String, Object> createPortLet = null;
        Boolean first = null;
        GenericValue portalPagePortlet = null;
        GenericValue portalPage = null;
        try {
            portalPage = EntityQuery.use(delegator)
                    .from("PortalPage")
                    .where(context)
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying PortalPage: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        if ("_NA_".equals(portalPage.get("ownerUserLoginId"))) {
            try {
                getPrivatePages = EntityQuery.use(delegator)
                        .from("PortalPage")
                        .where(UtilMisc.toMap("originalPortalPageId", context.get("portalPageId"), "ownerUserLoginId", userLogin.get("userLoginId")))
                        .queryList();
            } catch (Exception e) {
                Debug.logError(e, "Error querying entity: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
            if (UtilValidate.isEmpty(getPrivatePages)) {
                // set-service-fields from "portalPage" to "createPage" for service "createPortalPage"
                createPage.putAll(UtilMisc.toMap(portalPage));
                createPage.remove("portalPageId");
                createPage.put("ownerUserLoginId", userLogin.get("userLoginId"));
                createPage.put("originalPortalPageId", context.get("portalPageId"));
                try {
                    Map<String, Object> serviceResult = dispatcher.runSync("createPortalPage", createPage);
                    if (ServiceUtil.isError(serviceResult)) {
                        String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                        request.setAttribute("_ERROR_MESSAGE_", errMsg);
                        return "error";
                    }
                    portalPageId = serviceResult.get("portalPageId");
                } catch (Exception e) {
                    Debug.logError(e, "Error calling createPortalPage: " + e.getMessage(), MODULE);
                    request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                    return "error";
                }
                request.setAttribute("portalPageId", portalPageId);
                String result = duplicatePortalPageDetails(request, response);
                if (!"success".equals(result)) {
                    return result;
                }
            }
        }

        return "success";
    }


    /**
     * Duplicate content of portalPage, portalPageColumn, portalPagePortlet, portletAttribute
     */
    public static String duplicatePortalPageDetails(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Object portalPageId = context.get("portalPageId");
        List<GenericValue> portletAttributes = null;
        List<GenericValue> portalPagePortlets = null;
        Map<String, Object> delMap = null;
        List<GenericValue> portalPageColumns = null;
        Map<String, Object> addColumnMap = null;
        Map<String, Object> createPortLet = null;
        Boolean first = null;
        Debug.logInfo("duplicate portalPage detail from portalPageId  " + context.get("portalPageId") + " to new portalPageId=" + portalPageId, MODULE);
        if (UtilValidate.isNotEmpty(portalPageId)) {
            delMap.put("portalPageId", portalPageId);
            try {
                portalPageColumns = EntityQuery.use(delegator)
                        .from("PortalPageColumn")
                        .where(UtilMisc.toMap("portalPageId", context.get("portalPageId")))
                        .queryList();
            } catch (Exception e) {
                Debug.logError(e, "Error querying entity: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
            first = Boolean.TRUE;
            if (portalPageColumns != null) {
                for (GenericValue portalPageColumn : portalPageColumns) {
                    // set-service-fields from "portalPageColumn" to "addColumnMap" for service "addPortalPageColumn"
                    addColumnMap.putAll(UtilMisc.toMap(portalPageColumn));
                    addColumnMap.remove("columnSeqId");
                    addColumnMap.put("portalPageId", portalPageId);
                    try {
                        Map<String, Object> serviceResult = dispatcher.runSync("addPortalPageColumn", addColumnMap);
                        if (ServiceUtil.isError(serviceResult)) {
                            String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                            request.setAttribute("_ERROR_MESSAGE_", errMsg);
                            return "error";
                        }
                    } catch (Exception e) {
                        Debug.logError(e, "Error calling addPortalPageColumn: " + e.getMessage(), MODULE);
                        request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                        return "error";
                    }
                }
            }
            try {
                portalPagePortlets = EntityQuery.use(delegator)
                        .from("PortalPagePortlet")
                        .where(UtilMisc.toMap("portalPageId", context.get("portalPageId")))
                        .queryList();
            } catch (Exception e) {
                Debug.logError(e, "Error querying entity: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
            if (portalPagePortlets != null) {
                for (GenericValue portalPagePortlet : portalPagePortlets) {
                    // set-service-fields from "portalPagePortlet" to "createPortLet" for service "createPortalPagePortlet"
                    createPortLet.putAll(UtilMisc.toMap(portalPagePortlet));
                    createPortLet.put("portalPageId", portalPageId);
                    try {
                        Map<String, Object> serviceResult = dispatcher.runSync("createPortalPagePortlet", createPortLet);
                        if (ServiceUtil.isError(serviceResult)) {
                            String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                            request.setAttribute("_ERROR_MESSAGE_", errMsg);
                            return "error";
                        }
                    } catch (Exception e) {
                        Debug.logError(e, "Error calling createPortalPagePortlet: " + e.getMessage(), MODULE);
                        request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                        return "error";
                    }
                    try {
                        portletAttributes = EntityQuery.use(delegator)
                                .from("PortletAttribute")
                                .where(UtilMisc.toMap("portalPageId", context.get("portalPageId"), "portalPortletId", portalPagePortlet.get("portalPortletId"), "portletSeqId", portalPagePortlet.get("portletSeqId")))
                                .queryList();
                    } catch (Exception e) {
                        Debug.logError(e, "Error querying entity: " + e.getMessage(), MODULE);
                        request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                        return "error";
                    }
                    if (portletAttributes != null) {
                        for (GenericValue portletAttribute : portletAttributes) {
                            portletAttribute.put("portalPageId", portalPageId);
                            try {
                                delegator.create(portletAttribute);
                            } catch (Exception e) {
                                Debug.logError(e, "Error creating value: " + e.getMessage(), MODULE);
                                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                                return "error";
                            }
                        }
                    }
                }
            }
        }

        return "success";
    }


    /**
     * Only duplicate a portal page, user should put correct owner and securityGroup
     */
    public static String duplicatePortalPage(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> createPage = null;
        List<GenericValue> portletAttributes = null;
        GenericValue portalPageColumn = null;
        List<GenericValue> portalPagePortlets = null;
        GenericValue portletAttribute = null;
        Map<String, Object> delMap = null;
        List<GenericValue> portalPageColumns = null;
        Map<String, Object> addColumnMap = null;
        Map<String, Object> createPortLet = null;
        Boolean first = null;
        GenericValue portalPagePortlet = null;
        GenericValue portalPage = null;
        try {
            portalPage = EntityQuery.use(delegator)
                    .from("PortalPage")
                    .where(context)
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying PortalPage: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        // set-service-fields from "portalPage" to "createPage" for service "createPortalPage"
        createPage.putAll(UtilMisc.toMap(portalPage));
        createPage.remove("portalPageId");
        if (UtilValidate.isEmpty(((Map<String, Object>) createPage).get("originalPortalPageId"))) {
            createPage.put("originalPortalPageId", context.get("portalPageId"));
        }
        Object portalPageId = null;
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("createPortalPage", createPage);
            if (ServiceUtil.isError(serviceResult)) {
                String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                request.setAttribute("_ERROR_MESSAGE_", errMsg);
                return "error";
            }
            portalPageId = serviceResult.get("portalPageId");
        } catch (Exception e) {
            Debug.logError(e, "Error calling createPortalPage: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        request.setAttribute("portalPageId", portalPageId);
        Debug.logInfo("new protalPageId=" + portalPageId, MODULE);
        String result = duplicatePortalPageDetails(request, response);
        if (!"success".equals(result)) {
            return result;
        }

        return "success";
    }

}
