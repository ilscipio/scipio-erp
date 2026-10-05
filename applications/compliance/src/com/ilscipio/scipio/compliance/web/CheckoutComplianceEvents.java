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
package com.ilscipio.scipio.compliance.web;

import java.sql.Timestamp;
import java.util.LinkedHashMap;
import java.util.Locale;
import java.util.Map;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.UtilDateTime;
import org.ofbiz.base.util.UtilHttp;
import org.ofbiz.base.util.UtilProperties;
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.GenericValue;
import org.ofbiz.product.store.ProductStoreWorker;

import com.ilscipio.scipio.compliance.LegalDocumentWorker;

/**
 * Checkout hooks: the shopper accepts the terms (and sees the privacy policy) before the order button; the
 * accepted versions are stored with the order (OrderAttribute) and in the consent log.
 *
 * <p>SCIPIO: 4.0.0: Added.</p>
 */
public final class CheckoutComplianceEvents {

    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());
    private static final String SESSION_KEY = "scpAcceptedDocs";

    private CheckoutComplianceEvents() {}

    /** Before the order chain: the terms checkbox must be ticked when the store has a compliance profile. */
    public static String checkTerms(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        String productStoreId = ProductStoreWorker.getProductStoreId(request);
        GenericValue profile = LegalDocumentWorker.getProfile(delegator, productStoreId);
        if (profile == null) {
            return "success";
        }
        if (!"Y".equals(request.getParameter("scpTermsAccepted"))) {
            Locale locale = UtilHttp.getLocale(request);
            request.setAttribute("_ERROR_MESSAGE_", UtilProperties.getMessage("ComplianceUiLabels", "ComplianceAcceptTermsFirst", locale));
            return "error";
        }
        Locale locale = UtilHttp.getLocale(request);
        Map<String, Object> accepted = new LinkedHashMap<>();
        accepted.put("LEGDOC_TERMS", versionOf(delegator, productStoreId, "LEGDOC_TERMS", locale));
        accepted.put("LEGDOC_PRIVACY", versionOf(delegator, productStoreId, "LEGDOC_PRIVACY", locale));
        accepted.put("acceptedAt", UtilDateTime.nowTimestamp());
        request.getSession().setAttribute(SESSION_KEY, accepted);
        return "success";
    }

    /** After the order is created: OrderAttribute rows and consent log entries. Never fails the order. */
    public static String recordOrderConsents(HttpServletRequest request, HttpServletResponse response) {
        try {
            Delegator delegator = (Delegator) request.getAttribute("delegator");
            String orderId = (String) request.getAttribute("orderId");
            @SuppressWarnings("unchecked")
            Map<String, Object> accepted = (Map<String, Object>) request.getSession().getAttribute(SESSION_KEY);
            if (orderId == null || accepted == null) {
                return "success";
            }
            request.getSession().removeAttribute(SESSION_KEY);
            String productStoreId = ProductStoreWorker.getProductStoreId(request);
            GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
            Timestamp at = (Timestamp) accepted.get("acceptedAt");
            for (String docTypeId : new String[] {"LEGDOC_TERMS", "LEGDOC_PRIVACY"}) {
                @SuppressWarnings("unchecked")
                Map<String, Object> v = (Map<String, Object>) accepted.get(docTypeId);
                String text = docTypeId + " v" + v.get("versionNum") + (v.get("legalDocumentId") != null ? " (" + v.get("legalDocumentId") + ")" : " (template)");
                delegator.createOrStore(delegator.makeValue("OrderAttribute", "orderId", orderId,
                        "attrName", "LEGDOC_TERMS".equals(docTypeId) ? "COMPLIANCE_TERMS_VERSION" : "COMPLIANCE_PRIVACY_VERSION", "attrValue", text));
                GenericValue ev = delegator.makeValue("ConsentEvent");
                ev.set("consentEventId", delegator.getNextSeqId("ConsentEvent"));
                ev.set("productStoreId", productStoreId);
                ev.set("partyId", userLogin != null ? userLogin.getString("partyId") : null);
                ev.set("consentTypeId", "LEGDOC_TERMS".equals(docTypeId) ? "CONSENT_TERMS" : "CONSENT_PRIVACY_NOTICE");
                ev.set("granted", "Y");
                ev.set("sourceId", "CONSRC_CHECKOUT");
                ev.set("legalDocumentId", v.get("legalDocumentId"));
                ev.set("documentVersion", v.get("versionNum"));
                ev.set("orderId", orderId);
                ev.set("eventDate", at != null ? at : UtilDateTime.nowTimestamp());
                ev.set("ipHash", ConsentEvents.ipHash(request.getRemoteAddr(), productStoreId));
                ev.create();
            }
            delegator.createOrStore(delegator.makeValue("OrderAttribute", "orderId", orderId, "attrName", "COMPLIANCE_ACCEPTED_AT",
                    "attrValue", String.valueOf(at)));
        } catch (Exception e) {
            Debug.logError(e, "Could not record the accepted legal text versions of the order", module);
        }
        return "success";
    }

    private static Map<String, Object> versionOf(Delegator delegator, String productStoreId, String docTypeId, Locale locale) {
        Map<String, Object> m = new LinkedHashMap<>();
        GenericValue doc = LegalDocumentWorker.getPublished(delegator, productStoreId, docTypeId, locale);
        m.put("legalDocumentId", doc != null ? doc.getString("legalDocumentId") : null);
        m.put("versionNum", doc != null ? doc.getLong("versionNum") : Long.valueOf(0));
        return m;
    }
}
