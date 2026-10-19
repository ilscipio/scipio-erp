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
package com.ilscipio.scipio.accounting.einvoice;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.nio.charset.StandardCharsets;
import java.util.Locale;
import java.util.Map;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.mustangproject.ZUGFeRD.ZUGFeRDInvoiceImporter;
import org.mustangproject.ZUGFeRD.ZUGFeRDVisualizer;
import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.UtilHttp;
import org.ofbiz.base.util.UtilMisc;
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.GenericEntityException;
import org.ofbiz.entity.GenericValue;
import org.ofbiz.security.Security;
import org.ofbiz.service.GenericServiceException;
import org.ofbiz.service.LocalDispatcher;
import org.ofbiz.service.ServiceUtil;

/** E-invoice downloads: the generated e-invoice, and the original file of a received e-invoice (raw or as HTML). */
public class EInvoiceEvents {

    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());

    /** Streams the e-invoice of an invoice (parameters invoiceId, formatId). */
    public static String downloadEInvoice(HttpServletRequest request, HttpServletResponse response) {
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        try {
            Map<String, Object> result = dispatcher.runSync("generateEInvoice", UtilMisc.toMap("invoiceId", request.getParameter("invoiceId"),
                    "formatId", request.getParameter("formatId"), "userLogin", userLogin, "locale", UtilHttp.getLocale(request)));
            if (ServiceUtil.isError(result)) {
                request.setAttribute("_ERROR_MESSAGE_", ServiceUtil.getErrorMessage(result));
                return "error";
            }
            write(response, (byte[]) result.get("content"), (String) result.get("contentType"), (String) result.get("fileName"), true);
            return "success";
        } catch (GenericServiceException | IOException e) {
            Debug.logError(e, module);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
    }

    /** Streams the original file of a received e-invoice (parameter invoiceId). */
    public static String downloadOriginal(HttpServletRequest request, HttpServletResponse response) {
        EInvoiceDocument original = getOriginal(request);
        if (original == null) {
            return "error";
        }
        try {
            write(response, original.getContent(), original.getContentType(), original.getFileName(), true);
            return "success";
        } catch (IOException e) {
            Debug.logError(e, module);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
    }

    /**
     * Shows the original file of a received e-invoice as HTML (XRechnung visualization). The HTML is made from a
     * file that someone outside sent, so it is served in a sandbox: it cannot use the session of the user.
     */
    public static String viewOriginal(HttpServletRequest request, HttpServletResponse response) {
        EInvoiceDocument original = getOriginal(request);
        if (original == null) {
            return "error";
        }
        try {
            byte[] xml = original.getContent();
            if ("application/pdf".equals(original.getContentType())) {
                ZUGFeRDInvoiceImporter importer = new ZUGFeRDInvoiceImporter(new ByteArrayInputStream(xml));
                xml = importer.getUTF8().getBytes(StandardCharsets.UTF_8);
            }
            Locale locale = UtilHttp.getLocale(request);
            ZUGFeRDVisualizer.Language language = "de".equals(locale.getLanguage()) ? ZUGFeRDVisualizer.Language.DE
                    : "fr".equals(locale.getLanguage()) ? ZUGFeRDVisualizer.Language.FR : ZUGFeRDVisualizer.Language.EN;
            String html = new ZUGFeRDVisualizer().visualize(new ByteArrayInputStream(xml), language);
            response.setHeader("Content-Security-Policy", "sandbox allow-scripts; default-src 'none'; style-src 'unsafe-inline'; script-src 'unsafe-inline'; img-src data:");
            response.setHeader("X-Content-Type-Options", "nosniff");
            write(response, html.getBytes(StandardCharsets.UTF_8), "text/html; charset=UTF-8", null, false);
            return "success";
        } catch (Exception e) {
            Debug.logWarning(e, "E-invoice: cannot show the original file: " + e.getMessage(), module);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
    }

    private static EInvoiceDocument getOriginal(HttpServletRequest request) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        Security security = (Security) request.getAttribute("security");
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        if (security == null || !security.hasEntityPermission("ACCOUNTING", "_VIEW", userLogin)) {
            request.setAttribute("_ERROR_MESSAGE_", "No permission (ACCOUNTING_VIEW)");
            return null;
        }
        try {
            EInvoiceDocument original = EInvoiceArchive.find(delegator, request.getParameter("invoiceId"), EInvoiceArchive.ORIGINAL, null);
            if (original == null) {
                request.setAttribute("_ERROR_MESSAGE_", EInvoiceServices.label(UtilHttp.getLocale(request), "EInvoiceImportNoFile"));
            }
            return original;
        } catch (GenericEntityException e) {
            Debug.logError(e, module);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return null;
        }
    }

    private static void write(HttpServletResponse response, byte[] content, String contentType, String fileName, boolean attachment) throws IOException {
        response.setContentType(contentType);
        response.setContentLength(content.length);
        if (fileName != null && attachment) {
            UtilHttp.setContentDisposition(response, fileName);
        }
        try (OutputStream out = response.getOutputStream()) {
            out.write(content);
            out.flush();
        }
    }
}
