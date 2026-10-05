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

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.UtilHttp;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.GenericValue;
import org.ofbiz.product.store.ProductStoreWorker;
import org.ofbiz.service.LocalDispatcher;
import org.ofbiz.service.ServiceUtil;

import com.ilscipio.scipio.compliance.GuaranteeWorker;

/**
 * Storefront events of the withdrawal function ("Withdraw from contract here") and the GARAN label.
 * Step 1 form (withdraw) -> withdrawCheck (step 2, confirm) -> withdrawSubmit (receipt). The page reads the
 * request attributes scpWithdrawStep, scpWithdrawForm, scpWithdrawItems, scpWithdrawResult, scpWithdrawError.
 *
 * <p>SCIPIO: 4.0.0: Added.</p>
 */
public final class WithdrawalEvents {

    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());

    private WithdrawalEvents() {}

    public static String withdrawCheck(HttpServletRequest request, HttpServletResponse response) {
        return run(request, "checkWithdrawalOrder", "confirm");
    }

    public static String withdrawSubmit(HttpServletRequest request, HttpServletResponse response) {
        return run(request, "createWithdrawal", "receipt");
    }

    private static String run(HttpServletRequest request, String service, String nextStep) {
        Map<String, Object> form = new HashMap<>();
        form.put("orderId", trim(request.getParameter("orderId")));
        form.put("emailAddress", trim(request.getParameter("emailAddress")));
        form.put("customerName", trim(request.getParameter("customerName")));
        String scope = "some".equals(request.getParameter("scope")) ? "some" : "all";
        form.put("scope", scope);
        List<String> chosen = new ArrayList<>();
        String[] seqs = request.getParameterValues("orderItemSeqId");
        if ("some".equals(scope) && seqs != null) {
            chosen.addAll(Arrays.asList(seqs));
        }
        form.put("chosen", chosen);
        request.setAttribute("scpWithdrawForm", form);
        if (!"POST".equalsIgnoreCase(request.getMethod())) {
            return "error";
        }
        if ("some".equals(scope) && chosen.isEmpty() && "createWithdrawal".equals(service)) {
            request.setAttribute("scpWithdrawError", "noItems");
            request.setAttribute("scpWithdrawStep", "form");
            return "error";
        }
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Locale locale = UtilHttp.getLocale(request);
        Map<String, Object> ctx = new HashMap<>();
        ctx.put("orderId", form.get("orderId"));
        ctx.put("emailAddress", form.get("emailAddress"));
        ctx.put("partyId", userLogin != null ? userLogin.getString("partyId") : null);
        ctx.put("productStoreId", ProductStoreWorker.getProductStoreId(request));
        ctx.put("locale", locale);
        if ("createWithdrawal".equals(service)) {
            ctx.put("customerName", form.get("customerName"));
            ctx.put("orderItemSeqIds", chosen.isEmpty() ? null : chosen);
        }
        try {
            Map<String, Object> res = dispatcher.runSync(service, ctx);
            if (!ServiceUtil.isError(res) && !Boolean.TRUE.equals(res.get("matched"))) {
                request.setAttribute("scpWithdrawError", "noMatch");
                request.setAttribute("scpWithdrawStep", "form");
                return "success";
            }
            if (ServiceUtil.isError(res)) {
                request.setAttribute("scpWithdrawError", "failed");
                request.setAttribute("scpWithdrawStep", "createWithdrawal".equals(service) ? "confirm" : "form");
                return "error";
            }
            if ("confirm".equals(nextStep)) {
                @SuppressWarnings("unchecked")
                List<Map<String, Object>> items = (List<Map<String, Object>>) res.get("items");
                List<Map<String, Object>> shown = new ArrayList<>();
                for (Map<String, Object> it : items) {
                    if (chosen.isEmpty() || chosen.contains((String) it.get("orderItemSeqId"))) {
                        shown.add(it);
                    }
                }
                if (shown.isEmpty()) {
                    request.setAttribute("scpWithdrawError", "noItems");
                    request.setAttribute("scpWithdrawStep", "form");
                    return "error";
                }
                request.setAttribute("scpWithdrawItems", shown);
                request.setAttribute("scpWithdrawOrder", res.get("orderHeader"));
            } else {
                // after the commit of createWithdrawal: the confirmation of receipt on a durable medium
                @SuppressWarnings("unchecked")
                boolean sent = com.ilscipio.scipio.compliance.service.WithdrawalServiceImpl.sendConfirmation(dispatcher.getDispatchContext(),
                        (String) form.get("orderId"), (String) form.get("emailAddress"), (String) form.get("customerName"),
                        (String) res.get("returnId"), (java.sql.Timestamp) res.get("receivedDate"),
                        (List<Map<String, Object>>) res.get("withdrawnItems"), (List<Map<String, Object>>) res.get("pendingItems"), locale);
                Map<String, Object> shown = new HashMap<>(res);
                shown.put("emailSent", sent);
                request.setAttribute("scpWithdrawResult", shown);
            }
            request.setAttribute("scpWithdrawStep", nextStep);
            return "success";
        } catch (Exception e) {
            Debug.logError(e, "Withdrawal step failed", module);
            request.setAttribute("scpWithdrawError", "failed");
            request.setAttribute("scpWithdrawStep", "form");
            return "error";
        }
    }

    /** The full EU GARAN label of a product as SVG (fields filled), or 404. */
    public static String garanLabel(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        try {
            GenericValue product = delegator.findOne("Product", false, "productId", request.getParameter("productId"));
            Map<String, String> data = GuaranteeWorker.getGaranData(delegator, product);
            if (data == null) {
                response.sendError(404);
                return "success";
            }
            String svg = GuaranteeWorker.renderGaranSvg("Y".equals(request.getParameter("nested")), data, "scpgf-" + product.getString("productId"));
            response.setContentType("image/svg+xml");
            response.setCharacterEncoding("UTF-8");
            response.setHeader("Cache-Control", "public, max-age=3600");
            response.getWriter().write(svg);
        } catch (Exception e) {
            Debug.logError(e, "Could not render the GARAN label", module);
        }
        return "success";
    }

    private static String trim(String s) {
        return UtilValidate.isNotEmpty(s) ? s.trim() : null;
    }
}
