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

import java.io.BufferedReader;
import java.util.LinkedHashSet;
import java.util.Set;
import java.util.regex.Pattern;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.ofbiz.base.util.Debug;
import org.ofbiz.entity.Delegator;
import org.ofbiz.product.store.ProductStoreWorker;

import com.ilscipio.scipio.compliance.ThirdPartyServiceRegistry;

/**
 * Payment page script control (PCI DSS 4.0.1 req. 6.4.3 and 11.6.1): on checkout and payment pages the shop sends a
 * Content-Security-Policy in report-only mode. It allows this store's own origin plus the script domains of the
 * registered third-party services (the authorized script inventory); the browser reports any other script or data
 * destination to cspReport, which logs it as a possible skimming attempt. Report-only mode never blocks a page.
 *
 * <p>SCIPIO: 4.0.0: Added. Wired by webapp/hooks/shop-controller-post.xml.</p>
 */
public final class SecurityHeaderEvents {

    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());

    /** Requests that show or process payment data. */
    private static final Pattern PAYMENT_PAGES = Pattern.compile(
            "/control/(checkout[^/?]*|quickcheckout[^/?]*|onePageCheckout[^/?]*|showcart|processorder[^/?]*|.*[Pp]ayment[^/?]*|stripe[^/?]*|checkoutreview|ordercomplete)(\\?.*)?$");

    private SecurityHeaderEvents() {}

    public static String addSecurityHeaders(HttpServletRequest request, HttpServletResponse response) {
        try {
            String uri = request.getRequestURI();
            if (uri != null && PAYMENT_PAGES.matcher(uri).find()) {
                Delegator delegator = (Delegator) request.getAttribute("delegator");
                String productStoreId = ProductStoreWorker.getProductStoreId(request);
                Set<String> domains = new LinkedHashSet<>();
                for (ThirdPartyServiceRegistry.ServiceEntry s : ThirdPartyServiceRegistry.getServices(delegator, productStoreId)) {
                    for (String d : s.getScriptDomains()) {
                        domains.add("https://" + d);
                    }
                }
                // SCIPIO: 4.0.0: the card pay step of the order page (Stripe.js and the hub, W1-10d)
                // (resource hubcheckout, key url: see com.ilscipio.scipio.order.payment.HubCheckout; compliance does not depend on order)
                String hubUrl = org.ofbiz.entity.util.EntityUtilProperties.getPropertyValue("hubcheckout", "url", delegator);
                if (hubUrl != null && !hubUrl.trim().isEmpty()) {
                    domains.add("https://js.stripe.com");
                    domains.add("https://api.stripe.com");
                    domains.add("https://hooks.stripe.com");
                    String hubHost = java.net.URI.create(hubUrl.trim()).getHost();
                    if (hubHost != null) {
                        domains.add("https://" + hubHost);
                    }
                }
                String allowed = String.join(" ", domains);
                String report = request.getContextPath() + "/control/cspReport";
                String policy = "default-src 'self'; "
                        + "script-src 'self' 'unsafe-inline' 'unsafe-eval' " + allowed + "; "
                        + "connect-src 'self' " + allowed + "; "
                        + "frame-src 'self' " + allowed + "; "
                        + "img-src 'self' data: " + allowed + "; "
                        + "style-src 'self' 'unsafe-inline' https://fonts.googleapis.com; "
                        + "font-src 'self' data: https://fonts.gstatic.com; "
                        + "form-action 'self' " + allowed + "; "
                        + "report-uri " + report;
                response.setHeader("Content-Security-Policy-Report-Only", policy);
                response.setHeader("X-Content-Type-Options", "nosniff");
                response.setHeader("Referrer-Policy", "strict-origin-when-cross-origin");
            }
        } catch (Exception e) {
            Debug.logWarning("Could not set the payment page security headers: " + e.getMessage(), module);
        }
        return "success";
    }

    /** Receives CSP violation reports and logs them (tamper signal for the payment pages). */
    public static String cspReport(HttpServletRequest request, HttpServletResponse response) {
        try (BufferedReader r = request.getReader()) {
            StringBuilder sb = new StringBuilder();
            char[] buf = new char[2048];
            int n;
            while ((n = r.read(buf)) > 0 && sb.length() < 8192) {
                sb.append(buf, 0, n);
            }
            Debug.logWarning("CSP violation on a payment page (possible script injection): " + sb.toString().replaceAll("[\\r\\n]", " "), module);
        } catch (Exception e) {
            // ignore malformed reports
        }
        response.setStatus(204);
        return "success";
    }
}
