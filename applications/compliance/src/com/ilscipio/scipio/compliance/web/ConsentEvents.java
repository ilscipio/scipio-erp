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

import java.nio.charset.StandardCharsets;
import java.security.MessageDigest;
import java.sql.Timestamp;
import java.util.LinkedHashMap;
import java.util.Map;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.UtilDateTime;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.GenericEntityException;
import org.ofbiz.entity.GenericValue;
import org.ofbiz.product.store.ProductStoreWorker;
import org.ofbiz.webapp.stats.VisitHandler;
import org.ofbiz.webapp.website.WebSiteWorker;

import com.ilscipio.scipio.compliance.ConsentWorker;
import com.ilscipio.scipio.compliance.LegalDocumentWorker;

/**
 * Storefront events for consent choices.
 *
 * <p>SCIPIO: 4.0.0: Added.</p>
 */
public final class ConsentEvents {

    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());

    /** Request parameter name to ConsentEvent consentTypeId. Parameter values: Y or N. */
    private static final Map<String, String> PARAMS = new LinkedHashMap<>();
    static {
        PARAMS.put("preferences", "CONSENT_PREFERENCES");
        PARAMS.put("statistics", "CONSENT_STATISTICS");
        PARAMS.put("marketing", "CONSENT_MARKETING");
        PARAMS.put("saleShare", "CONSENT_SALE_SHARE");
        PARAMS.put("targetedAds", "CONSENT_TARGETED_ADS");
    }

    private static final Map<String, String> SOURCES = new LinkedHashMap<>();
    static {
        SOURCES.put("banner", "CONSRC_BANNER");
        SOURCES.put("gpc", "CONSRC_GPC");
        SOURCES.put("privacy-choices", "CONSRC_PRIVACY_CHOICES");
        SOURCES.put("account", "CONSRC_ACCOUNT");
    }

    private ConsentEvents() {}

    /**
     * Records the consent choices of the request (POST preferences/statistics/marketing/saleShare/targetedAds = Y|N,
     * source = banner|gpc|privacy-choices|account) as ConsentEvent rows, and answers with a small JSON body.
     */
    public static String recordConsent(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        String productStoreId = ProductStoreWorker.getProductStoreId(request);
        GenericValue profile = LegalDocumentWorker.getProfile(delegator, productStoreId);
        String source = SOURCES.getOrDefault(request.getParameter("source"), "CONSRC_BANNER");
        if (ConsentWorker.isGpc(request) && "CONSRC_BANNER".equals(source) && request.getParameter("gpcApplied") != null) {
            source = "CONSRC_GPC";
        }
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        String partyId = userLogin != null ? userLogin.getString("partyId") : null;
        String visitorId = null;
        try {
            GenericValue visitor = VisitHandler.getVisitor(request, response);
            visitorId = visitor != null ? visitor.getString("visitorId") : null;
        } catch (Exception e) {
            // visitor tracking may be off
        }
        String consentVersion = ConsentWorker.getConsentVersion(delegator, productStoreId, profile);
        Timestamp now = UtilDateTime.nowTimestamp();
        int written = 0;
        try {
            for (Map.Entry<String, String> p : PARAMS.entrySet()) {
                String v = request.getParameter(p.getKey());
                if (!"Y".equals(v) && !"N".equals(v)) {
                    continue;
                }
                GenericValue ev = delegator.makeValue("ConsentEvent");
                ev.set("consentEventId", delegator.getNextSeqId("ConsentEvent"));
                ev.set("productStoreId", productStoreId);
                ev.set("webSiteId", WebSiteWorker.getWebSiteId(request));
                ev.set("visitorId", visitorId);
                ev.set("partyId", partyId);
                ev.set("consentTypeId", p.getValue());
                ev.set("granted", v);
                ev.set("sourceId", source);
                ev.set("consentVersion", parseVersion(consentVersion));
                ev.set("eventDate", now);
                ev.set("ipHash", ipHash(request.getRemoteAddr(), productStoreId));
                ev.create();
                written++;
            }
        } catch (GenericEntityException e) {
            Debug.logError(e, "Could not record consent", module);
            return writeJson(response, 500, "{\"ok\":false}");
        }
        boolean gpc = ConsentWorker.isGpc(request) || "Y".equals(request.getParameter("gpcApplied"));
        boolean marketing = "Y".equals(request.getParameter("marketing")) && !"N".equals(request.getParameter("saleShare")) && !gpc;
        ConsentWorker.writeState(request, response, consentVersion, ConsentWorker.getRegime(request, profile),
                "Y".equals(request.getParameter("preferences")), "Y".equals(request.getParameter("statistics")), marketing);
        if ("privacyChoices".equals(request.getParameter("returnTo"))) {
            try {
                response.sendRedirect(request.getContextPath() + "/control/privacyChoices?saved=Y");
                return "success";
            } catch (Exception e) {
                Debug.logError(e, module);
            }
        }
        return writeJson(response, 200, "{\"ok\":true,\"recorded\":" + written + ",\"consentVersion\":\"" + consentVersion + "\"}");
    }

    private static Long parseVersion(String consentVersion) {
        try {
            return Long.valueOf(consentVersion.split("-")[0]);
        } catch (Exception e) {
            return null;
        }
    }

    /** A hash of IP and store, so the log proves a choice per connection without keeping the IP address. */
    static String ipHash(String ip, String productStoreId) {
        if (UtilValidate.isEmpty(ip)) {
            return null;
        }
        try {
            MessageDigest md = MessageDigest.getInstance("SHA-256");
            byte[] h = md.digest((ip + "|" + productStoreId + "|scipio-consent").getBytes(StandardCharsets.UTF_8));
            StringBuilder sb = new StringBuilder();
            for (int i = 0; i < 12; i++) {
                sb.append(String.format("%02x", h[i]));
            }
            return sb.toString();
        } catch (Exception e) {
            return null;
        }
    }

    private static String writeJson(HttpServletResponse response, int status, String body) {
        try {
            response.setStatus(status);
            response.setContentType("application/json");
            response.setCharacterEncoding("UTF-8");
            response.getWriter().write(body);
        } catch (Exception e) {
            Debug.logError(e, module);
        }
        return "success";
    }
}
