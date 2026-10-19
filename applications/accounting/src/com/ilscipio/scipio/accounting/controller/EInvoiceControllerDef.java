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
package com.ilscipio.scipio.accounting.controller;

import com.ilscipio.scipio.accounting.einvoice.EInvoiceEvents;
import com.ilscipio.scipio.ce.webapp.control.def.Event;
import com.ilscipio.scipio.ce.webapp.control.def.RedirectParameter;
import com.ilscipio.scipio.ce.webapp.control.def.Request;
import com.ilscipio.scipio.ce.webapp.control.def.Response;
import com.ilscipio.scipio.ce.webapp.control.def.View;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

/**
 * E-invoice requests of the accounting webapp (EN 16931: XRechnung, ZUGFeRD / Factur-X, Peppol BIS).
 */
public class EInvoiceControllerDef {

    @View(
        name = "InvoiceEInvoice",
        type = "screen",
        page = "component://accounting/widget/invoice/EInvoiceScreens.xml#InvoiceEInvoice",
        controller = "accounting"
    )
    public static final String VIEW_INVOICEEINVOICE = "InvoiceEInvoice";

    @View(
        name = "EInvoiceImport",
        type = "screen",
        page = "component://accounting/widget/invoice/EInvoiceScreens.xml#EInvoiceImport",
        controller = "accounting"
    )
    public static final String VIEW_EINVOICEIMPORT = "EInvoiceImport";

    @View(
        name = "EditPartyEInvoicePref",
        type = "screen",
        page = "component://accounting/widget/invoice/EInvoiceScreens.xml#EditPartyEInvoicePref",
        controller = "accounting"
    )
    public static final String VIEW_EDITPARTYEINVOICEPREF = "EditPartyEInvoicePref";

    @Request(uri = "invoiceEInvoice", controller = "accounting", secure = "true", auth = "true")
    @Response(name = "success", type = "view", value = "InvoiceEInvoice")
    public interface InvoiceEInvoice {}

    /** Downloads the e-invoice (invoiceId, formatId). */
    @Request(uri = "einvoice", controller = "accounting", secure = "true", auth = "true")
    @Response(name = "success", type = "none")
    @Response(name = "error", type = "view", value = "InvoiceEInvoice")
    public static String einvoice(HttpServletRequest request, HttpServletResponse response) throws Exception {
        return EInvoiceEvents.downloadEInvoice(request, response);
    }

    /** Downloads the original file of a received e-invoice (invoiceId). */
    @Request(uri = "einvoiceOriginal", controller = "accounting", secure = "true", auth = "true")
    @Response(name = "success", type = "none")
    @Response(name = "error", type = "view", value = "InvoiceEInvoice")
    public static String einvoiceOriginal(HttpServletRequest request, HttpServletResponse response) throws Exception {
        return EInvoiceEvents.downloadOriginal(request, response);
    }

    /** Shows the original file of a received e-invoice as HTML (invoiceId). */
    @Request(uri = "einvoiceOriginalView", controller = "accounting", secure = "true", auth = "true")
    @Response(name = "success", type = "none")
    @Response(name = "error", type = "view", value = "InvoiceEInvoice")
    public static String einvoiceOriginalView(HttpServletRequest request, HttpServletResponse response) throws Exception {
        return EInvoiceEvents.viewOriginal(request, response);
    }

    @Request(uri = "EInvoiceImport", controller = "accounting", secure = "true", auth = "true")
    @Response(name = "success", type = "view", value = "EInvoiceImport")
    public interface EInvoiceImport {}

    @Request(uri = "importEInvoice", controller = "accounting", secure = "true", auth = "true")
    @Response(name = "success", type = "request-redirect", value = "invoiceOverview", redirectParameters = {
        @RedirectParameter(name = "invoiceId")
    })
    @Response(name = "error", type = "view", value = "EInvoiceImport")
    @Event(type = "service", invoke = "importEInvoice")
    public static String importEInvoice(HttpServletRequest request, HttpServletResponse response) throws Exception {
        return "success"; // Event handled by @Event annotation
    }

    @Request(uri = "EditPartyEInvoicePref", controller = "accounting", secure = "true", auth = "true")
    @Response(name = "success", type = "view", value = "EditPartyEInvoicePref")
    public interface EditPartyEInvoicePref {}

    @Request(uri = "updatePartyEInvoicePref", controller = "accounting", secure = "true", auth = "true")
    @Response(name = "success", type = "view", value = "EditPartyEInvoicePref")
    @Response(name = "error", type = "view", value = "EditPartyEInvoicePref")
    @Event(type = "service", invoke = "updatePartyEInvoicePref")
    public static String updatePartyEInvoicePref(HttpServletRequest request, HttpServletResponse response) throws Exception {
        return "success"; // Event handled by @Event annotation
    }
}
