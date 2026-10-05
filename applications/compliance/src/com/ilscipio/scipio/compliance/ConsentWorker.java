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
package com.ilscipio.scipio.compliance;

import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;

import javax.servlet.http.HttpServletRequest;

import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.GenericEntityException;
import org.ofbiz.entity.GenericValue;
import org.ofbiz.entity.util.EntityQuery;
import org.ofbiz.product.store.ProductStoreWorker;
import org.ofbiz.webapp.website.WebSiteWorker;

/**
 * Consent regime and consent UI data of a storefront request.
 *
 * <p>Regime EU: nothing but necessary services runs before the shopper agrees (GDPR, ePrivacy). Regime US:
 * services run, the shopper can opt out of the sale or sharing of personal information; a Global Privacy
 * Control signal ({@code Sec-GPC: 1} or {@code navigator.globalPrivacyControl}) is an opt-out (CCPA and 11 other
 * states). The consent version changes when the store's service list changes, so the dialog asks again.</p>
 *
 * <p>SCIPIO: 4.0.0: Added.</p>
 */
public final class ConsentWorker {

    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());

    public static final String COOKIE_NAME = "scpConsent";
    public static final String[] CATEGORIES = {"NECESSARY", "PREFERENCES", "STATISTICS", "MARKETING"};

    /** Request headers that CDNs and proxies use for the visitor country. */
    private static final String[] COUNTRY_HEADERS = {"CF-IPCountry", "CloudFront-Viewer-Country", "X-Country-Code", "X-Geo-Country"};

    private ConsentWorker() {}

    public static boolean isGpc(HttpServletRequest request) {
        return "1".equals(request.getHeader("Sec-GPC"));
    }

    /** EU or US, from the store profile's consentMode; AUTO uses a country header, else the strictest rules. */
    public static String getRegime(HttpServletRequest request, GenericValue profile) {
        String mode = profile != null ? profile.getString("consentMode") : null;
        if ("EU_OPT_IN".equals(mode)) {
            return "EU";
        }
        if ("US_OPT_OUT".equals(mode)) {
            return "US";
        }
        boolean eu = profile == null || LegalDocumentWorker.hasJurisdiction(profile, "EU");
        boolean us = profile == null || LegalDocumentWorker.hasJurisdiction(profile, "US");
        if (eu && us) {
            for (String h : COUNTRY_HEADERS) {
                String country = request.getHeader(h);
                if (UtilValidate.isNotEmpty(country)) {
                    return "US".equalsIgnoreCase(country.trim()) ? "US" : "EU";
                }
            }
            return "EU";
        }
        return us ? "US" : "EU";
    }

    public static String getConsentVersion(Delegator delegator, String productStoreId, GenericValue profile) {
        Object v = profile != null ? profile.get("consentVersion") : null;
        String base = v != null ? v.toString().replace(".0", "") : "1";
        return base + "-" + ThirdPartyServiceRegistry.getRegistryHash(delegator, productStoreId).substring(0, 6);
    }

    /**
     * Everything the consent dialog, the footer actions and the gated scripts need, as a map for FreeMarker:
     * regime, gpc, consentVersion, cookieName, categories (id, services, required), scripts (category, code),
     * showPrivacyChoices, productStoreId.
     */
    @SuppressWarnings("unchecked")
    public static Map<String, Object> getConsentContext(HttpServletRequest request, Delegator delegator, Locale locale) {
        Object cached = request.getAttribute("scpConsentContext");
        if (cached instanceof Map) {
            return (Map<String, Object>) cached;
        }
        String productStoreId = ProductStoreWorker.getProductStoreId(request);
        GenericValue profile = LegalDocumentWorker.getProfile(delegator, productStoreId);
        Map<String, Object> out = new LinkedHashMap<>();
        String regime = getRegime(request, profile);
        out.put("productStoreId", productStoreId);
        out.put("regime", regime);
        out.put("gpc", isGpc(request));
        out.put("cookieName", COOKIE_NAME);
        out.put("consentVersion", getConsentVersion(delegator, productStoreId, profile));
        out.put("showPrivacyChoices", profile == null || LegalDocumentWorker.hasJurisdiction(profile, "US"));

        List<Map<String, Object>> categories = new ArrayList<>();
        for (String cat : CATEGORIES) {
            List<String> names = new ArrayList<>();
            for (ThirdPartyServiceRegistry.ServiceEntry s : ThirdPartyServiceRegistry.getServices(delegator, productStoreId, cat)) {
                names.add(s.getName());
            }
            Map<String, Object> c = new LinkedHashMap<>();
            c.put("id", cat.toLowerCase(Locale.ROOT));
            c.put("required", "NECESSARY".equals(cat));
            c.put("services", names);
            categories.add(c);
        }
        out.put("categories", categories);
        out.put("scripts", getGatedScripts(delegator, WebSiteWorker.getWebSiteId(request), productStoreId));
        out.put("state", readState(request));
        request.setAttribute("scpConsentContext", out);
        return out;
    }

    /** The consent cookie as a map (v, r, preferences, statistics, marketing, t), or null. */
    @SuppressWarnings("unchecked")
    public static Map<String, Object> readState(HttpServletRequest request) {
        if (request.getCookies() == null) {
            return null;
        }
        for (javax.servlet.http.Cookie cookie : request.getCookies()) {
            if (COOKIE_NAME.equals(cookie.getName())) {
                try {
                    String json = java.net.URLDecoder.decode(cookie.getValue(), "UTF-8");
                    Object parsed = org.ofbiz.base.lang.JSON.from(json).toObject(Map.class);
                    return parsed instanceof Map ? (Map<String, Object>) parsed : null;
                } catch (Exception e) {
                    return null;
                }
            }
        }
        return null;
    }

    /** Writes the consent cookie in the format that consent.js reads and writes (URL-encoded JSON, 12 months). */
    public static void writeState(HttpServletRequest request, javax.servlet.http.HttpServletResponse response, String consentVersion,
                                  String regime, boolean preferences, boolean statistics, boolean marketing) {
        String json = "{\"v\":\"" + consentVersion + "\",\"r\":\"" + regime + "\",\"preferences\":" + preferences
                + ",\"statistics\":" + statistics + ",\"marketing\":" + marketing + ",\"t\":" + System.currentTimeMillis() + "}";
        try {
            javax.servlet.http.Cookie cookie = new javax.servlet.http.Cookie(COOKIE_NAME, java.net.URLEncoder.encode(json, "UTF-8"));
            cookie.setPath("/");
            cookie.setMaxAge(365 * 24 * 3600);
            cookie.setSecure(request.isSecure());
            response.addCookie(cookie);
        } catch (Exception e) {
            Debug.logError(e, module);
        }
    }

    /**
     * The store's WebAnalyticsConfig code blocks with the consent category of their service. The theme does not
     * print them; the consent script runs a block only when its category is allowed.
     */
    public static List<Map<String, String>> getGatedScripts(Delegator delegator, String webSiteId, String productStoreId) {
        List<Map<String, String>> out = new ArrayList<>();
        if (UtilValidate.isEmpty(webSiteId)) {
            return out;
        }
        try {
            for (GenericValue wac : EntityQuery.use(delegator).from("WebAnalyticsConfig").where("webSiteId", webSiteId).cache().queryList()) {
                String type = wac.getString("webAnalyticsTypeId");
                if ("BACKEND_ANALYTICS".equals(type) || UtilValidate.isEmpty(wac.getString("webAnalyticsCode"))) {
                    continue;
                }
                Map<String, String> s = new LinkedHashMap<>();
                s.put("category", categoryOfAnalyticsType(type));
                s.put("type", type);
                s.put("code", wac.getString("webAnalyticsCode"));
                out.add(s);
            }
        } catch (GenericEntityException e) {
            Debug.logError(e, "Could not read WebAnalyticsConfig of web site " + webSiteId, module);
        }
        return out;
    }

    /** GOOGLE_ANALYTICS is statistics; every other or unknown type counts as marketing (the strictest). */
    public static String categoryOfAnalyticsType(String webAnalyticsTypeId) {
        return "GOOGLE_ANALYTICS".equals(webAnalyticsTypeId) ? "statistics" : "marketing";
    }
}
