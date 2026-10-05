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
import org.ofbiz.service.LocalDispatcher;
import org.ofbiz.service.ServiceUtil;

/**
 * Auto-generated event class from Simple Method XML.
 *
 * <p>Generated from: component://accounting/script/org/ofbiz/accounting/invoice/InvoiceEvents.xml</p>
 *
 * <p>SCIPIO: 4.0.0: Auto-generated.</p>
 */
public class InvoiceEvents {

    private static final String MODULE = InvoiceEvents.class.getName();


    /**
     * Create a new Invoice Item with Payrol Item Type
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String createInvoiceItemPayrol(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Object AddInvoiceItem = null;
        Map<String, Object> createInvoiceItem = null;
        List<GenericValue> PayrolGroup = null;
        try {
            PayrolGroup = EntityQuery.use(delegator)
                    .from("InvoiceItemType")
                    .queryList();
        } catch (Exception e) {
            Debug.logError(e, "Error querying InvoiceItemType: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        List<GenericValue> PayrolList = null;
        try {
            PayrolList = EntityQuery.use(delegator)
                    .from("InvoiceItemType")
                    .queryList();
        } catch (Exception e) {
            Debug.logError(e, "Error querying InvoiceItemType: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        if (PayrolGroup != null) {
            for (GenericValue payrolGroup : PayrolGroup) {
                if (PayrolList != null) {
                    for (GenericValue payrolList : PayrolList) {
                        if ("${payrolGroup.invoiceItemTypeId}".equals(((Map<String, Object>) payrolList).get("parentTypeId"))) {
                            AddInvoiceItem = "N";
                            createInvoiceItem.put("invoiceId", context.get("invoiceId"));
                            createInvoiceItem.put("invoiceItemTypeId", ((Map<String, Object>) payrolList).get("invoiceItemTypeId"));
                            createInvoiceItem.put("description", ((Map<String, Object>) payrolGroup).get("description") + " : " + ((Map<String, Object>) payrolList).get("description"));
                            createInvoiceItem.put("quantity", ((Map<String, Object>) context.get("${payrolList")).get("invoiceItemTypeId}_Quantity"));
                            if (UtilValidate.isNotEmpty(((Map<String, Object>) context.get("${payrolList")).get("invoiceItemTypeId}_Quantity"))) {
                                AddInvoiceItem = "Y";
                            }
                            createInvoiceItem.put("amount", ((Map<String, Object>) context.get("${payrolList")).get("invoiceItemTypeId}_Amount"));
                            if (UtilValidate.isNotEmpty(((Map<String, Object>) context.get("${payrolList")).get("invoiceItemTypeId}_Amount"))) {
                                AddInvoiceItem = "Y";
                            }
                            if ("Y".equals(AddInvoiceItem)) {
                                if (!"PAYROL_EARN_HOURS".equals(((Map<String, Object>) payrolGroup).get("invoiceItemTypeId"))) {
                                    ((Map<String, Object>) createInvoiceItem).put("amount", new BigDecimal(((Map<String, Object>) createInvoiceItem).get("amount").toString()));
                                }
                                try {
                                    Map<String, Object> serviceResult = dispatcher.runSync("createInvoiceItem", createInvoiceItem);
                                    if (ServiceUtil.isError(serviceResult)) {
                                        String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                                        request.setAttribute("_ERROR_MESSAGE_", errMsg);
                                        return "error";
                                    }
                                } catch (Exception e) {
                                    Debug.logError(e, "Error calling createInvoiceItem: " + e.getMessage(), MODULE);
                                    request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                                    return "error";
                                }
                            }
                        }
                    }
                }
            }
        }

        return "success";
    }

}
