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

import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.ScriptUtil;
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
 * <p>Generated from: component://accounting/script/com/ilscipio/scipio/accounting/datev/DatevEvents.xml</p>
 *
 * <p>SCIPIO: 4.0.0: Auto-generated.</p>
 */
public class DatevEvents {

    private static final String MODULE = DatevEvents.class.getName();


    /**
     * Import Datev Data Category
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String importDatevDataCategory(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        List<String> error_list = new LinkedList<>();

        Object operationStats = null;
        Map<String, Object> importDatevCtx = null;
        Map<String, Object> scriptContext = new HashMap<>();
        try {
            scriptContext.put("delegator", delegator);
            scriptContext.put("dispatcher", dispatcher);
            scriptContext.put("locale", locale);
            scriptContext.put("userLogin", userLogin);
            scriptContext.put("context", context);
            scriptContext.put("parameters", context);
            scriptContext.put("request", request);
            scriptContext.put("response", response);
            ScriptUtil.executeScript("component://accounting/script/com/ilscipio/scipio/accounting/datev/DatevCSVUpload.groovy", null, scriptContext);
        } catch (Exception e) {
            Debug.logError(e, "Error executing Groovy script: " + e.getMessage(), MODULE);
        }
        // Extract script bindings into local scope
        Object multiPartMap = scriptContext.get("multiPartMap");
        Object dataCategoryId = ((Map<String, Object>) multiPartMap).get("dataCategoryId");
        importDatevCtx = (Map<String, Object>) multiPartMap;
        GenericValue dataCategory = null;
        try {
            dataCategory = EntityQuery.use(delegator)
                    .from("DatevDataCategory")
                    .where(UtilMisc.toMap("dataCategoryId", dataCategoryId))
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying DatevDataCategory: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        if (UtilValidate.isNotEmpty(dataCategory)) {
            importDatevCtx.put("dataCategory", dataCategory);
            if ("BUCHUNGSSTAPEL".equals(dataCategoryId)) {
                try {
                    Map<String, Object> serviceResult = dispatcher.runSync("importDatevTransactionEntries", importDatevCtx);
                    if (ServiceUtil.isError(serviceResult)) {
                        String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                        request.setAttribute("_ERROR_MESSAGE_", errMsg);
                        return "error";
                    }
                    operationStats = serviceResult.get("operationStats");
                } catch (Exception e) {
                    Debug.logError(e, "Error calling importDatevTransactionEntries: " + e.getMessage(), MODULE);
                    request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                    return "error";
                }
            } else {
                if ("DEBITOREN_KREDITOREN_STAMMDATEN".equals(dataCategoryId)) {
                    try {
                        Map<String, Object> serviceResult = dispatcher.runSync("importDatevContacts", importDatevCtx);
                        if (ServiceUtil.isError(serviceResult)) {
                            String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                            request.setAttribute("_ERROR_MESSAGE_", errMsg);
                            return "error";
                        }
                        operationStats = serviceResult.get("operationStats");
                    } catch (Exception e) {
                        Debug.logError(e, "Error calling importDatevContacts: " + e.getMessage(), MODULE);
                        request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                        return "error";
                    }
                } else {
                    error_list.add("Operation not supported yet.");
                    request.setAttribute("_ERROR_MESSAGE_", "Operation not supported yet.");
                }
            }
            if (UtilValidate.isNotEmpty(operationStats)) {
                request.setAttribute("operationStats", operationStats);
            } else {
                error_list.add("Result not found.");
                request.setAttribute("_ERROR_MESSAGE_", "Result not found.");
            }
        } else {
            error_list.add("Invalid DATEV data category.");
            request.setAttribute("_ERROR_MESSAGE_", "Invalid DATEV data category.");
        }
        request.setAttribute("orgPartyId", ((Map<String, Object>) multiPartMap).get("orgPartyId"));
        request.setAttribute("topGlAccountId", ((Map<String, Object>) multiPartMap).get("topGlAccountId"));
        request.setAttribute("dataCategoryId", dataCategoryId);

        return "success";
    }


    /**
     * Export Datev Transaction Entries
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String exportDatevTransactionEntries(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);


        return "success";
    }

}
