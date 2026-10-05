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
public class ArControllerDef {

    @Request(
        uri = "main",
        controller = "ar",
        secure = "true",
        auth = "true"
    )
    @Response(name = "success", type = "view", value = "main")
    public interface Main {}

    @Request(
        uri = "batchPayments",
        controller = "ar",
        secure = "true",
        auth = "true"
    )
    @Response(name = "success", type = "view", value = "BatchPayments")
    public interface BatchPayments {}

    @Request(
        uri = "createPaymentBatch",
        controller = "ar",
        secure = "true",
        auth = "true"
    )
    @Response(name = "success", type = "view", value = "PaymentGroupOverview")
    @Response(name = "error", type = "view", value = "BatchPayments")
    @Event(type = "service", invoke = "depositWithdrawPayments")
    public static String createPaymentBatch(HttpServletRequest request, HttpServletResponse response) {
        return "success"; // Event handled by @Event annotation
    }

    @Request(
        uri = "newPayment",
        controller = "ar",
        secure = "true",
        auth = "true"
    )
    @Response(name = "success", type = "view", value = "NewIncomingPayment")
    public interface NewPayment {}

    @Request(
        uri = "createPayment",
        controller = "ar",
        secure = "true",
        auth = "true"
    )
    @Response(name = "success", type = "view", value = "editPayment")
    @Response(name = "error", type = "view", value = "NewIncomingPayment")
    @Event(type = "service", invoke = "createPaymentAndFinAccountTrans")
    public static String createPayment(HttpServletRequest request, HttpServletResponse response) {
        return "success"; // Event handled by @Event annotation
    }

    @Request(
        uri = "findInvoices",
        controller = "ar",
        secure = "true",
        auth = "true"
    )
    @Response(name = "success", type = "view", value = "FindArInvoices")
    public interface FindInvoices {}

    @Request(
        uri = "newInvoice",
        controller = "ar",
        secure = "true",
        auth = "true"
    )
    @Response(name = "success", type = "view", value = "NewSalesInvoice")
    public interface NewInvoice {}

    @Request(
        uri = "createInvoice",
        controller = "ar",
        secure = "true",
        auth = "true"
    )
    @Response(name = "success", type = "request", value = "editInvoice")
    @Response(name = "error", type = "view", value = "NewSalesInvoice")
    @Event(type = "service", invoke = "createInvoice")
    public static String createInvoice(HttpServletRequest request, HttpServletResponse response) {
        return "success"; // Event handled by @Event annotation
    }

    @Request(
        uri = "FindArInvoices",
        controller = "ar",
        secure = "true",
        auth = "true"
    )
    @Response(name = "success", type = "view", value = "FindArInvoices")
    public interface FindArInvoices {}

    @Request(
        uri = "NewSalesInvoice",
        controller = "ar",
        secure = "true",
        auth = "true"
    )
    @Response(name = "success", type = "view", value = "NewSalesInvoice")
    public interface NewSalesInvoice {}

    @Request(
        uri = "FindArPaymentGroups",
        controller = "ar",
        secure = "true",
        auth = "true"
    )
    @Response(name = "success", type = "view", value = "FindArPaymentGroups")
    public interface FindArPaymentGroups {}

    @Request(
        uri = "massChangePaymentStatus",
        controller = "ar",
        secure = "true",
        auth = "true"
    )
    @Response(name = "success", type = "view", value = "BatchPayments")
    @Response(name = "error", type = "view", value = "BatchPayments")
    @Event(type = "service", invoke = "massChangePaymentStatus")
    public static String massChangePaymentStatus(HttpServletRequest request, HttpServletResponse response) {
        return "success"; // Event handled by @Event annotation
    }

    @Request(
        uri = "massChangeInvoiceStatus",
        controller = "ar",
        secure = "true",
        auth = "true"
    )
    @Response(name = "success", type = "view", value = "FindArInvoices")
    @Response(name = "error", type = "view", value = "FindArInvoices")
    @Event(type = "service", invoke = "massChangeInvoiceStatus")
    public static String massChangeInvoiceStatus(HttpServletRequest request, HttpServletResponse response) {
        return "success"; // Event handled by @Event annotation
    }

    @Request(
        uri = "cancelPaymentGroup",
        controller = "ar",
        secure = "true",
        auth = "true"
    )
    @Response(name = "success", type = "view", value = "PaymentGroupOverview")
    @Response(name = "error", type = "view", value = "FindArPaymentGroups")
    @Event(type = "service", invoke = "cancelPaymentBatch")
    public static String cancelPaymentGroup(HttpServletRequest request, HttpServletResponse response) {
        return "success"; // Event handled by @Event annotation
    }

}
