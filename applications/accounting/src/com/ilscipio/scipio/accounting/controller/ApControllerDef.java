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

import com.ilscipio.scipio.ce.webapp.control.def.View;
import com.ilscipio.scipio.ce.webapp.control.def.Request;
import com.ilscipio.scipio.ce.webapp.control.def.Response;
import com.ilscipio.scipio.ce.webapp.control.def.Responses;
import com.ilscipio.scipio.ce.webapp.control.def.Event;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

/**
 * Auto-generated annotation-based controller definitions.
 *
 * <p>Generated from controller.xml by convertXmlToAnnotation Gradle task.</p>
 *
 * <p>SCIPIO: 4.0.0: Auto-generated.</p>
 */
public class ApControllerDef {
    @com.ilscipio.scipio.ce.webapp.control.def.View(
        name = "CommissionReportPdf",
        type = "screenfop",
        page = "component://accounting/widget/AccountingPrintScreens.xml#CommissionReportPdf",
        contentType = "application/pdf",
        encoding = "none",
        controller = "ap"
    )
    public static final String VIEW_COMMISSIONREPORTPDF = "CommissionReportPdf";

    @com.ilscipio.scipio.ce.webapp.control.def.View(
        name = "FindVendors",
        type = "screen",
        page = "component://accounting/widget/settings/SettingScreens.xml#FindVendors",
        controller = "ap"
    )
    public static final String VIEW_FINDVENDORS = "FindVendors";

    @com.ilscipio.scipio.ce.webapp.control.def.View(
        name = "EditVendor",
        type = "screen",
        page = "component://accounting/widget/settings/SettingScreens.xml#EditVendor",
        controller = "ap"
    )
    public static final String VIEW_EDITVENDOR = "EditVendor";

    @com.ilscipio.scipio.ce.webapp.control.def.View(
        name = "LookupInvoice",
        type = "screen",
        page = "component://accounting/widget/LookupScreens.xml#LookupInvoice",
        controller = "ap"
    )
    public static final String VIEW_LOOKUPINVOICE = "LookupInvoice";

    @com.ilscipio.scipio.ce.webapp.control.def.View(
        name = "LookupPayment",
        type = "screen",
        page = "component://accounting/widget/LookupScreens.xml#LookupPayment",
        controller = "ap"
    )
    public static final String VIEW_LOOKUPPAYMENT = "LookupPayment";

    @Request(
        uri = "main",
        controller = "ap",
        secure = "true",
        auth = "true"
    )
    @Response(name = "success", type = "view", value = "main")
    public interface Main {}

    @Request(
        uri = "FindPurchaseInvoices",
        controller = "ap",
        secure = "true",
        auth = "true"
    )
    @Response(name = "success", type = "view", value = "FindApInvoices")
    public interface FindPurchaseInvoices {}

    @Request(
        uri = "FindApInvoices",
        controller = "ap",
        secure = "true",
        auth = "true"
    )
    @Response(name = "success", type = "view", value = "FindApInvoices")
    public interface FindApInvoices {}

    @Request(
        uri = "NewPurchaseInvoice",
        controller = "ap",
        secure = "true",
        auth = "true"
    )
    @Response(name = "success", type = "view", value = "NewPurchaseInvoice")
    public interface NewPurchaseInvoice {}

    @Request(
        uri = "processMassCheckRun",
        controller = "ap",
        secure = "true",
        auth = "true"
    )
    @Response(name = "success", type = "request", value = "PaymentGroupOverview")
    @Response(name = "error", type = "view", value = "FindApInvoices")
    @Event(type = "service", invoke = "createPaymentAndPaymentGroupForInvoices")
    public static String processMassCheckRun(HttpServletRequest request, HttpServletResponse response) {
        return "success"; // Event handled by @Event annotation
    }

    @Request(
        uri = "findInvoices",
        controller = "ap",
        secure = "true",
        auth = "true"
    )
    @Response(name = "success", type = "view", value = "FindApInvoices")
    public interface FindInvoices {}

    @Request(
        uri = "createInvoice",
        controller = "ap",
        secure = "true",
        auth = "true"
    )
    @Response(name = "success", type = "request", value = "editInvoice")
    @Response(name = "error", type = "view", value = "NewPurchaseInvoice")
    @Event(type = "service", invoke = "createInvoice")
    public static String createInvoice(HttpServletRequest request, HttpServletResponse response) {
        return "success"; // Event handled by @Event annotation
    }

    @Request(
        uri = "findPayments",
        controller = "ap",
        secure = "true",
        auth = "true"
    )
    @Response(name = "success", type = "view", value = "FindApPayments")
    public interface FindPayments {}

    @Request(
        uri = "FindApPayments",
        controller = "ap",
        secure = "true",
        auth = "true"
    )
    @Response(name = "success", type = "view", value = "FindApPayments")
    public interface FindApPayments {}

    @Request(
        uri = "FindCommissions",
        controller = "ap",
        secure = "true",
        auth = "true"
    )
    @Response(name = "success", type = "view", value = "CommissionReport")
    public interface FindCommissions {}

    @Request(
        uri = "newInvoice",
        controller = "ap",
        secure = "true",
        auth = "true"
    )
    @Response(name = "success", type = "view", value = "NewPurchaseInvoice")
    public interface NewInvoice {}

    @Request(
        uri = "FindApPaymentGroups",
        controller = "ap",
        secure = "true",
        auth = "true"
    )
    @Response(name = "success", type = "view", value = "FindApPaymentGroups")
    public interface FindApPaymentGroups {}

    @Request(
        uri = "massChangeInvoiceStatus",
        controller = "ap",
        secure = "true",
        auth = "true"
    )
    @Response(name = "success", type = "view", value = "FindApInvoices")
    @Event(type = "service", invoke = "massChangeInvoiceStatus")
    public static String massChangeInvoiceStatus(HttpServletRequest request, HttpServletResponse response) {
        return "success"; // Event handled by @Event annotation
    }

    @Request(
        uri = "cancelCheckRunPayments",
        controller = "ap",
        secure = "true",
        auth = "true"
    )
    @Response(name = "success", type = "view", value = "PaymentGroupOverview")
    @Response(name = "error", type = "view", value = "FindApPaymentGroups")
    @Event(type = "service", invoke = "cancelCheckRunPayments")
    public static String cancelCheckRunPayments(HttpServletRequest request, HttpServletResponse response) {
        return "success"; // Event handled by @Event annotation
    }

    @Request(
        uri = "findVendors",
        controller = "ap",
        secure = "true",
        auth = "true"
    )
    @Response(name = "success", type = "view", value = "FindVendors")
    public interface FindVendors {}


    // Auto-generated split (Part 2)
    public static class Part2 {
        @Request(
            uri = "editVendor",
            controller = "ap",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditVendor")
        public interface EditVendor {}

        @Request(
            uri = "createVendor",
            controller = "ap",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "FindVendors")
        @Response(name = "error", type = "view", value = "FindVendors")
        @Event(type = "service", invoke = "createVendor")
        public static String createVendor(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "updateVendor",
            controller = "ap",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "FindVendors")
        @Response(name = "error", type = "view", value = "FindVendors")
        @Event(type = "service", invoke = "updateVendor")
        public static String updateVendor(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "CommissionReport.pdf",
            controller = "ap",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "CommissionReportPdf")
        public interface CommissionReportPdf {}


    }
}
