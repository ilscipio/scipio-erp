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
import org.ofbiz.entity.util.EntityQuery;
import org.ofbiz.service.LocalDispatcher;
import org.ofbiz.service.ServiceUtil;

/**
 * Auto-generated event class from Simple Method XML.
 *
 * <p>Generated from: component://accounting/script/org/ofbiz/accounting/invoice/SampleCommissionServices.xml</p>
 *
 * <p>SCIPIO: 4.0.0: Auto-generated.</p>
 */
public class SampleCommissionServices {

    private static final String MODULE = SampleCommissionServices.class.getName();


    /**
     * Sample Calculate Affiliate Commission
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String sampleCalculateAffiliateCommission(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Object commissionPartyId = null;
        Object commissionAmount = null;
        GenericValue payment = null;
        try {
            payment = EntityQuery.use(delegator)
                    .from("Payment")
                    .where(UtilMisc.toMap())
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying Payment: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        List<GenericValue> affiliatePartyRelationshipList = null;
        try {
            affiliatePartyRelationshipList = EntityQuery.use(delegator)
                    .from("PartyRelationship")
                    .where(UtilMisc.toMap("partyIdFrom", ((Map<String, Object>) payment).get("partyIdFrom"), "roleTypeIdFrom", "CUSTOMER", "partyRelationshipTypeId", "SALES_AFFILIATE"))
                    .filterByDate()
                    .queryList();
        } catch (Exception e) {
            Debug.logError(e, "Error querying PartyRelationship: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        if (affiliatePartyRelationshipList != null) {
            for (GenericValue affiliatePartyRelationship : affiliatePartyRelationshipList) {
                if ("AFFILIATE".equals(((Map<String, Object>) affiliatePartyRelationship).get("roleTypeIdTo"))) {
                    commissionAmount = new BigDecimal(((Map<String, Object>) payment).get("amount").toString());
                    commissionPartyId = ((Map<String, Object>) affiliatePartyRelationship).get("partyIdTo");
                    createCommissionInvoiceInline(request, response);
                }
            }
        }

        return "success";
    }


    /**
     * Create Commission Invoice Inline
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String createCommissionInvoiceInline(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> createInvoiceMap = new HashMap<>();
        createInvoiceMap.put("invoiceTypeId", "COMMISSION_INVOICE");
        createInvoiceMap.put("statusId", "INVOICE_RECEIVED");
        createInvoiceMap.put("partyIdFrom", context.get("commissionPartyId"));
        createInvoiceMap.put("partyId", ((Map<String, Object>) context.get("payment")).get("partyIdTo"));
        Object createdInvoiceId = null;
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("createInvoice", createInvoiceMap);
            if (ServiceUtil.isError(serviceResult)) {
                String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                request.setAttribute("_ERROR_MESSAGE_", errMsg);
                return "error";
            }
            createdInvoiceId = serviceResult.get("createdInvoiceId");
        } catch (Exception e) {
            Debug.logError(e, "Error calling createInvoice: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        Map<String, Object> createInvoiceItemMap = new HashMap<>();
        createInvoiceItemMap.put("invoiceId", createdInvoiceId);
        createInvoiceItemMap.put("invoiceItemTypeId", "COMM_INV_ITEM");
        createInvoiceItemMap.put("amount", context.get("commissionAmount"));
        createInvoiceItemMap.put("quantity", BigDecimal.ONE);
        createInvoiceItemMap.put("description", "Commission for Received Customer Payment [" + ((Map<String, Object>) context.get("payment")).get("paymentId") + "]");
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("createInvoiceItem", createInvoiceItemMap);
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

        return "success";
    }

}
