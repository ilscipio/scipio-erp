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
import java.util.HashMap;
import java.util.Locale;
import java.util.Map;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.UtilDateTime;
import org.ofbiz.base.util.UtilHttp;
import org.ofbiz.base.util.UtilMisc;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.GenericValue;
import org.ofbiz.entity.util.EntityQuery;
import org.ofbiz.product.store.ProductStoreWorker;
import org.ofbiz.service.LocalDispatcher;
import org.ofbiz.service.ServiceUtil;

import com.ilscipio.scipio.compliance.LegalDocumentWorker;

/**
 * Storefront events of the privacy center (account) and the guest privacy request form.
 *
 * <p>SCIPIO: 4.0.0: Added.</p>
 */
public final class PrivacyEvents {

    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());
    private static final String VERIFY_SCREEN = "component://compliance/widget/ComplianceEmailScreens.xml#PrivacyVerifyEmail";

    private PrivacyEvents() {}

    private static GenericValue customer(HttpServletRequest request) {
        GenericValue ul = (GenericValue) request.getSession().getAttribute("userLogin");
        return ul != null && !"anonymous".equals(ul.getString("userLoginId")) && ul.getString("partyId") != null ? ul : null;
    }

    /** Download of all personal data of the logged-in customer as JSON (GDPR Art. 15/20, CCPA right to know). */
    public static String privacyExport(HttpServletRequest request, HttpServletResponse response) {
        GenericValue userLogin = customer(request);
        if (userLogin == null) {
            return "error";
        }
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        try {
            Map<String, Object> res = dispatcher.runSync("exportPartyPersonalData", UtilMisc.toMap("partyId", userLogin.getString("partyId"), "userLogin", userLogin));
            if (ServiceUtil.isError(res)) {
                request.setAttribute("_ERROR_MESSAGE_", ServiceUtil.getErrorMessage(res));
                return "error";
            }
            dispatcher.runSync("createPrivacyRequest", UtilMisc.toMap("userLogin", userLogin, "requestTypeId", "PRVREQ_ACCESS",
                    "productStoreId", ProductStoreWorker.getProductStoreId(request), "note", "Self-service download"));
            byte[] body = ((String) res.get("dataJson")).getBytes(StandardCharsets.UTF_8);
            response.setContentType("application/json");
            response.setCharacterEncoding("UTF-8");
            response.setHeader("Content-Disposition", "attachment; filename=\"my-data-" + UtilDateTime.nowDateString("yyyy-MM-dd") + ".json\"");
            response.setContentLength(body.length);
            response.getOutputStream().write(body);
            response.getOutputStream().flush();
            return "success";
        } catch (Exception e) {
            Debug.logError(e, "Data export failed", module);
            return "error";
        }
    }

    /** Deletes (anonymizes) the account of the logged-in customer after the confirmation box, then logs out. */
    public static String privacyDeleteAccount(HttpServletRequest request, HttpServletResponse response) {
        GenericValue userLogin = customer(request);
        if (userLogin == null || !"POST".equalsIgnoreCase(request.getMethod())) {
            return "error";
        }
        Locale locale = UtilHttp.getLocale(request);
        if (!"Y".equals(request.getParameter("confirmDelete"))) {
            request.setAttribute("_ERROR_MESSAGE_", org.ofbiz.base.util.UtilProperties.getMessage("ComplianceUiLabels", "ComplianceDeleteConfirmFirst", locale));
            return "error";
        }
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        try {
            Map<String, Object> pr = dispatcher.runSync("createPrivacyRequest", UtilMisc.toMap("userLogin", userLogin, "requestTypeId", "PRVREQ_DELETE",
                    "productStoreId", ProductStoreWorker.getProductStoreId(request), "note", "Self-service account deletion"));
            Map<String, Object> res = dispatcher.runSync("anonymizePartyPersonalData", UtilMisc.toMap("partyId", userLogin.getString("partyId"), "userLogin", userLogin));
            if (ServiceUtil.isError(res)) {
                request.setAttribute("_ERROR_MESSAGE_", ServiceUtil.getErrorMessage(res));
                return "error";
            }
            GenericValue req = EntityQuery.use(delegator).from("PrivacyRequest").where("privacyRequestId", pr.get("privacyRequestId")).queryOne();
            if (req != null) {
                req.set("statusId", "PRS_COMPLETED");
                req.set("completedDate", UtilDateTime.nowTimestamp());
                req.set("note", "Self-service account deletion; retained contact records: " + res.get("retainedContactMechs"));
                req.store();
            }
            request.getSession().invalidate();
            return "success";
        } catch (Exception e) {
            Debug.logError(e, "Account deletion failed", module);
            return "error";
        }
    }

    /** Guest request (access, delete, correct, opt-out, limit): stored as UNVERIFIED, e-mail link to confirm. */
    public static String privacyRequestSubmit(HttpServletRequest request, HttpServletResponse response) {
        String email = request.getParameter("emailAddress");
        String type = request.getParameter("requestTypeId");
        if (!"POST".equalsIgnoreCase(request.getMethod()) || UtilValidate.isEmpty(email) || !UtilValidate.isEmail(email.trim())
                || type == null || !type.startsWith("PRVREQ_")) {
            request.setAttribute("scpPrivacyDone", "invalid");
            return "success";
        }
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        Locale locale = UtilHttp.getLocale(request);
        String productStoreId = ProductStoreWorker.getProductStoreId(request);
        try {
            Map<String, Object> ctx = new HashMap<>();
            ctx.put("emailAddress", email.trim());
            ctx.put("requestTypeId", type);
            ctx.put("productStoreId", productStoreId);
            ctx.put("note", request.getParameter("note"));
            GenericValue ul = customer(request);
            if (ul != null) {
                ctx.put("userLogin", ul);
            }
            Map<String, Object> res = dispatcher.runSync("createPrivacyRequest", ctx);
            String token = (String) res.get("verifyToken");
            if (token != null) {
                GenericValue profile = LegalDocumentWorker.getProfile(delegator, productStoreId);
                String from = profile != null ? profile.getString("contactEmail") : null;
                if (from != null) {
                    String url = UtilHttp.getServerRootUrl(request) + request.getContextPath() + "/control/privacyVerify?token=" + token;
                    GenericValue system = EntityQuery.use(delegator).from("UserLogin").where("userLoginId", "system").cache().queryOne();
                    dispatcher.runSync("sendMailFromScreen", UtilMisc.toMap("sendTo", email.trim(), "sendFrom", from,
                            "subject", org.ofbiz.base.util.UtilProperties.getMessage("ComplianceUiLabels", "CompliancePrivacyVerifySubject", locale),
                            "bodyScreenUri", VERIFY_SCREEN, "bodyParameters", UtilMisc.toMap("verifyUrl", url, "locale", locale),
                            "locale", locale, "userLogin", system));
                }
            }
            request.setAttribute("scpPrivacyDone", token != null ? "sent" : "received");
            return "success";
        } catch (Exception e) {
            Debug.logError(e, "Privacy request failed", module);
            request.setAttribute("scpPrivacyDone", "invalid");
            return "success";
        }
    }

    public static String privacyVerify(HttpServletRequest request, HttpServletResponse response) {
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        try {
            Map<String, Object> res = dispatcher.runSync("verifyPrivacyRequest", UtilMisc.toMap("verifyToken", request.getParameter("token")));
            request.setAttribute("scpPrivacyDone", Boolean.TRUE.equals(res.get("verified")) ? "verified" : "invalidToken");
        } catch (Exception e) {
            request.setAttribute("scpPrivacyDone", "invalidToken");
        }
        return "success";
    }
}
