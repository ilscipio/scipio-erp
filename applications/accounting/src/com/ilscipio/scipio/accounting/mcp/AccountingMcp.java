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
package com.ilscipio.scipio.accounting.mcp;

import java.io.IOException;
import java.io.StringReader;
import java.math.BigDecimal;
import java.sql.Timestamp;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Set;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;

import org.apache.commons.csv.CSVFormat;
import org.apache.commons.csv.CSVParser;
import org.apache.commons.csv.CSVRecord;
import org.ofbiz.accounting.invoice.InvoiceWorker;
import org.ofbiz.accounting.payment.PaymentWorker;
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.GenericEntityException;
import org.ofbiz.entity.GenericValue;
import org.ofbiz.entity.condition.EntityCondition;
import org.ofbiz.entity.condition.EntityOperator;
import org.ofbiz.entity.util.EntityQuery;
import org.ofbiz.party.party.PartyHelper;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.NodeList;
import org.xml.sax.InputSource;

import com.ilscipio.scipio.mcp.catalog.ResultConverter;
import com.ilscipio.scipio.mcp.def.McpParam;
import com.ilscipio.scipio.mcp.def.McpResource;
import com.ilscipio.scipio.mcp.def.McpServer;
import com.ilscipio.scipio.mcp.def.McpServiceTool;
import com.ilscipio.scipio.mcp.def.McpTool;
import com.ilscipio.scipio.mcp.def.McpTopic;
import com.ilscipio.scipio.mcp.protocol.JsonRpc;
import com.ilscipio.scipio.mcp.registry.McpCallContext;
import com.ilscipio.scipio.mcp.registry.McpToolException;
import com.ilscipio.scipio.mcp.tool.ImportDiff;

/**
 * SCIPIO: 4.0.0: MCP server profile for the accounting component: invoices, payments and the GL.
 */
@McpServer(name = "accounting", title = "Scipio Accounting", component = "accounting",
        description = "Accounting: invoices, payments, GL and financial accounts.",
        featuredServices = {"createInvoice", "updateInvoice", "setInvoiceStatus", "createPayment", "updatePayment",
                "createGlAccount", "quickSendPayment", "createPaymentApplication", "createInvoiceForOrderAllItems", "createInvoiceItem",
                "quickCreateAcctgTransAndEntries"},
        entities = {"Invoice", "InvoiceItem", "InvoiceStatus", "Payment", "PaymentApplication", "GlAccount", "AcctgTrans",
                "AcctgTransEntry", "BillingAccount", "FinAccount", "FinAccountTrans", "GlAccountOrganization", "FixedAsset",
                "TaxAuthority", "TaxAuthorityRateProduct", "CustomTimePeriod", "Agreement", "AgreementItem"},
        serviceTools = {
            @McpServiceTool(service = "createInvoiceForOrderAllItems", topic = "invoice", name = "create_from_order",
                    description = "Create a sales invoice for every item of an order.", readOnly = false, requiresConfirmation = true, order = 30),
            @McpServiceTool(service = "setInvoiceStatus", topic = "invoice", name = "set_status",
                    description = "Change the invoice status, e.g. INVOICE_READY, INVOICE_PAID, INVOICE_CANCELLED.", readOnly = false, destructive = "true", requiresConfirmation = true, order = 40),
            @McpServiceTool(service = "createPayment", topic = "payment", name = "create",
                    description = "Record a payment.", readOnly = false, requiresConfirmation = true, order = 60),
            @McpServiceTool(service = "createPaymentApplication", topic = "payment", name = "apply",
                    description = "Apply a payment to an invoice or billing account.", readOnly = false, requiresConfirmation = true, order = 70),

            @McpServiceTool(service = "createInvoice", topic = "invoice", name = "create",
                    description = "Create an invoice header.", readOnly = false, destructive = "false", order = 41),
            @McpServiceTool(service = "createInvoiceItem", topic = "invoice", name = "item_add",
                    description = "Add a line item to an invoice.", readOnly = false, destructive = "false", order = 42),
            @McpServiceTool(service = "updateInvoice", topic = "invoice", name = "update",
                    description = "Update invoice fields.",
                    readOnly = false, destructive = "false", order = 43),
            @McpServiceTool(service = "quickCreateAcctgTransAndEntries", topic = "ledger", name = "transaction_create",
                    description = "Post a manual GL transaction with one debit and one credit entry.", readOnly = false, destructive = "false", order = 44),
            @McpServiceTool(service = "createGlAccount", topic = "ledger", name = "account_create",
                    description = "Create a GL account.",
                    readOnly = false, destructive = "false", order = 45),
            @McpServiceTool(service = "createGlAccountOrganization", topic = "ledger", name = "account_assign_org",
                    description = "Assign a GL account to an organization.",
                    readOnly = false, destructive = "false", order = 46),
            @McpServiceTool(service = "createFinAccount", topic = "ledger", name = "fin_account_create",
                    description = "Create a financial account.",
                    readOnly = false, destructive = "false", order = 47),
            @McpServiceTool(service = "createFinAccountTrans", topic = "ledger", name = "fin_account_transaction_add",
                    description = "Record a financial account transaction.",
                    readOnly = false, destructive = "false", order = 48),
            @McpServiceTool(service = "createBillingAccount", topic = "ledger", name = "billing_account_create",
                    description = "Create a customer billing account.",
                    readOnly = false, destructive = "false", order = 49),
            @McpServiceTool(service = "createFixedAsset", topic = "ledger", name = "fixed_asset_create",
                    description = "Create a fixed asset.",
                    readOnly = false, destructive = "false", order = 51),
            @McpServiceTool(service = "createTaxAuthority", topic = "ledger", name = "tax_authority_create",
                    description = "Create a tax authority.",
                    readOnly = false, destructive = "false", order = 52),
            @McpServiceTool(service = "createTaxAuthorityRateProduct", topic = "ledger", name = "tax_rate_create",
                    description = "Create a tax rate for a tax authority.",
                    readOnly = false, destructive = "false", order = 53),
            @McpServiceTool(service = "createCustomTimePeriod", topic = "ledger", name = "time_period_create",
                    description = "Create a fiscal time period.",
                    readOnly = false, destructive = "false", order = 54),
            @McpServiceTool(service = "createAgreement", topic = "ledger", name = "agreement_create",
                    description = "Create an agreement between two parties.",
                    readOnly = false, destructive = "false", order = 55),
            @McpServiceTool(service = "createAgreementItem", topic = "ledger", name = "agreement_item_add",
                    description = "Add an item to an agreement.",
                    readOnly = false, destructive = "false", order = 56),

            @McpServiceTool(service = "setPaymentStatus", topic = "payment", name = "set_status",
                    description = "Change the payment status, e.g. PMNT_SENT, PMNT_RECEIVED.",
                    readOnly = false, destructive = "true", requiresConfirmation = true, order = 71),
            @McpServiceTool(service = "voidPayment", topic = "payment", name = "void",
                    description = "Void a payment and remove its applications.",
                    readOnly = false, destructive = "true", requiresConfirmation = true, order = 72),
            @McpServiceTool(service = "removePaymentApplication", topic = "payment", name = "unapply",
                    description = "Remove a payment application.",
                    readOnly = false, destructive = "true", requiresConfirmation = true, order = 73),
            @McpServiceTool(service = "postAcctgTrans", topic = "ledger", name = "transaction_post",
                    description = "Post an existing GL transaction.",
                    readOnly = false, destructive = "true", requiresConfirmation = true, order = 74),
            @McpServiceTool(service = "depositWithdrawPayments", topic = "payment", name = "deposit",
                    description = "Deposit or withdraw a batch of payments against a financial account.",
                    readOnly = false, destructive = "true", requiresConfirmation = true, order = 75),
            @McpServiceTool(service = "closeFinancialTimePeriod", topic = "ledger", name = "time_period_close",
                    description = "Close a fiscal time period; no further postings.",
                    readOnly = false, destructive = "true", requiresConfirmation = true, order = 76),
            @McpServiceTool(service = "sendInvoicePerEmail", topic = "invoice", name = "send",
                    description = "Email an invoice as PDF.",
                    readOnly = false, permission = "MCP_MAIL_SEND", requiresConfirmation = true, order = 77)
        },
        topics = {
            @McpTopic(name = "invoice", title = "Invoices", order = 10, featured = true,
                    description = "Invoices: find, read, create, change status, send, open items and statements."),
            @McpTopic(name = "payment", title = "Payments", order = 20, featured = true,
                    description = "Payments: find, record, apply, void, deposit, bank statement import."),
            @McpTopic(name = "ledger", title = "Ledger and master data", order = 30,
                    description = "GL accounts and transactions, financial and billing accounts, fixed assets, agreements, tax, time periods.")
        })
public final class AccountingMcp {

    private AccountingMcp() {}

    private static final List<String> OPEN_INVOICE_STATUSES = Arrays.asList("INVOICE_READY", "INVOICE_SENT", "INVOICE_APPROVED");

    @McpTool(topic = "invoice", name = "find", description = "Find invoices by id, type, status, party or invoice date range.", readOnly = true, order = 10)
    public static Object findInvoices(McpCallContext ctx,
            @McpParam(name = "invoiceId", required = false) String invoiceId,
            @McpParam(name = "invoiceTypeId", description = "e.g. SALES_INVOICE, PURCHASE_INVOICE", required = false) String invoiceTypeId,
            @McpParam(name = "statusId", description = "e.g. INVOICE_IN_PROCESS, INVOICE_READY, INVOICE_PAID", required = false) String statusId,
            @McpParam(name = "partyId", description = "Bill-to party (partyId) or bill-from party (partyIdFrom)", required = false) String partyId,
            @McpParam(name = "fromDate", description = "Invoice date on/after", required = false) Timestamp fromDate,
            @McpParam(name = "thruDate", description = "Invoice date on/before", required = false) Timestamp thruDate,
            @McpParam(name = "limit", required = false) Integer limit) throws McpToolException {
        Delegator delegator = ctx.getDelegator();
        try {
            List<EntityCondition> conds = new ArrayList<>();
            if (invoiceId != null) conds.add(EntityCondition.makeCondition("invoiceId", invoiceId));
            if (invoiceTypeId != null) conds.add(EntityCondition.makeCondition("invoiceTypeId", invoiceTypeId));
            if (statusId != null) conds.add(EntityCondition.makeCondition("statusId", statusId));
            if (partyId != null) {
                conds.add(EntityCondition.makeCondition(EntityOperator.OR,
                        EntityCondition.makeCondition("partyId", partyId), EntityCondition.makeCondition("partyIdFrom", partyId)));
            }
            if (fromDate != null) conds.add(EntityCondition.makeCondition("invoiceDate", EntityOperator.GREATER_THAN_EQUAL_TO, fromDate));
            if (thruDate != null) conds.add(EntityCondition.makeCondition("invoiceDate", EntityOperator.LESS_THAN_EQUAL_TO, thruDate));
            List<GenericValue> invoices = EntityQuery.use(delegator).from("Invoice").where(conds)
                    .orderBy("-invoiceDate").maxRows(ctx.limit(limit)).queryList();
            List<Map<String, Object>> out = new ArrayList<>();
            for (GenericValue inv : invoices) {
                Map<String, Object> row = new LinkedHashMap<>();
                row.put("invoiceId", inv.getString("invoiceId"));
                row.put("invoiceTypeId", inv.getString("invoiceTypeId"));
                row.put("statusId", inv.getString("statusId"));
                row.put("partyIdFrom", inv.getString("partyIdFrom"));
                row.put("partyId", inv.getString("partyId"));
                row.put("invoiceDate", ResultConverter.toJson(inv.getTimestamp("invoiceDate")));
                row.put("dueDate", ResultConverter.toJson(inv.getTimestamp("dueDate")));
                row.put("currencyUomId", inv.getString("currencyUomId"));
                row.put("total", ResultConverter.toJson(InvoiceWorker.getInvoiceTotal(inv)));
                row.put("outstanding", ResultConverter.toJson(InvoiceWorker.getInvoiceNotApplied(inv)));
                out.add(row);
            }
            return out;
        } catch (GenericEntityException e) {
            throw new McpToolException("Invoice search failed: " + e.getMessage());
        }
    }

    @McpTool(topic = "invoice", name = "get", description = "One invoice with items, status history, payments, totals and open amount.", readOnly = true, order = 20)
    public static Object getInvoice(McpCallContext ctx,
            @McpParam(name = "invoiceId", required = true) String invoiceId) throws McpToolException {
        Delegator delegator = ctx.getDelegator();
        try {
            GenericValue inv = EntityQuery.use(delegator).from("Invoice").where("invoiceId", invoiceId).queryOne();
            if (inv == null) throw new McpToolException("Invoice not found: " + invoiceId);
            Map<String, Object> out = new LinkedHashMap<>();
            out.put("invoice", ResultConverter.toJson(inv));
            out.put("items", ResultConverter.toJson(EntityQuery.use(delegator).from("InvoiceItem").where("invoiceId", invoiceId).orderBy("invoiceItemSeqId").queryList()));
            out.put("statusHistory", ResultConverter.toJson(EntityQuery.use(delegator).from("InvoiceStatus").where("invoiceId", invoiceId).orderBy("statusDate").queryList()));
            out.put("paymentApplications", ResultConverter.toJson(EntityQuery.use(delegator).from("PaymentApplication").where("invoiceId", invoiceId).queryList()));
            out.put("total", ResultConverter.toJson(InvoiceWorker.getInvoiceTotal(inv)));
            out.put("applied", ResultConverter.toJson(InvoiceWorker.getInvoiceApplied(inv)));
            out.put("outstanding", ResultConverter.toJson(InvoiceWorker.getInvoiceNotApplied(inv)));
            return out;
        } catch (GenericEntityException e) {
            throw new McpToolException("Failed to load invoice " + invoiceId + ": " + e.getMessage());
        }
    }

    @McpTool(topic = "payment", name = "find", description = "Find payments by id, type, status, party or effective date range.", readOnly = true, order = 50)
    public static Object findPayments(McpCallContext ctx,
            @McpParam(name = "paymentId", required = false) String paymentId,
            @McpParam(name = "paymentTypeId", description = "e.g. CUSTOMER_PAYMENT, VENDOR_PAYMENT", required = false) String paymentTypeId,
            @McpParam(name = "statusId", description = "e.g. PMNT_NOT_PAID, PMNT_RECEIVED, PMNT_SENT", required = false) String statusId,
            @McpParam(name = "partyId", description = "partyIdFrom or partyIdTo", required = false) String partyId,
            @McpParam(name = "fromDate", description = "Effective date on/after", required = false) Timestamp fromDate,
            @McpParam(name = "thruDate", description = "Effective date on/before", required = false) Timestamp thruDate,
            @McpParam(name = "limit", required = false) Integer limit) throws McpToolException {
        Delegator delegator = ctx.getDelegator();
        try {
            List<EntityCondition> conds = new ArrayList<>();
            if (paymentId != null) conds.add(EntityCondition.makeCondition("paymentId", paymentId));
            if (paymentTypeId != null) conds.add(EntityCondition.makeCondition("paymentTypeId", paymentTypeId));
            if (statusId != null) conds.add(EntityCondition.makeCondition("statusId", statusId));
            if (partyId != null) {
                conds.add(EntityCondition.makeCondition(EntityOperator.OR,
                        EntityCondition.makeCondition("partyIdFrom", partyId), EntityCondition.makeCondition("partyIdTo", partyId)));
            }
            if (fromDate != null) conds.add(EntityCondition.makeCondition("effectiveDate", EntityOperator.GREATER_THAN_EQUAL_TO, fromDate));
            if (thruDate != null) conds.add(EntityCondition.makeCondition("effectiveDate", EntityOperator.LESS_THAN_EQUAL_TO, thruDate));
            List<GenericValue> payments = EntityQuery.use(delegator).from("Payment").where(conds)
                    .orderBy("-effectiveDate").maxRows(ctx.limit(limit)).queryList();
            List<Map<String, Object>> out = new ArrayList<>();
            for (GenericValue pay : payments) {
                Map<String, Object> row = new LinkedHashMap<>();
                for (String f : new String[] {"paymentId", "paymentTypeId", "paymentMethodTypeId", "statusId", "partyIdFrom", "partyIdTo",
                        "currencyUomId", "paymentRefNum"}) {
                    row.put(f, pay.getString(f));
                }
                row.put("amount", ResultConverter.toJson(pay.getBigDecimal("amount")));
                row.put("effectiveDate", ResultConverter.toJson(pay.getTimestamp("effectiveDate")));
                out.add(row);
            }
            return out;
        } catch (GenericEntityException e) {
            throw new McpToolException("Payment search failed: " + e.getMessage());
        }
    }

    // ---- open items / statement ----

    @McpTool(topic = "invoice", name = "open_items", description = "List unpaid invoices as of a date, per party and in total.", readOnly = true, order = 11)
    public static Object openItems(McpCallContext ctx,
            @McpParam(name = "partyId", required = false) String partyId,
            @McpParam(name = "invoiceTypeId", enumValues = {"SALES_INVOICE", "PURCHASE_INVOICE"}, required = false) String invoiceTypeId,
            @McpParam(name = "asOfDate", description = "yyyy-MM-dd; defaults to today", required = false) String asOfDate,
            @McpParam(name = "limit", required = false) Integer limit) throws McpToolException {
        return ResultConverter.toJson(computeOpenItems(ctx, partyId, invoiceTypeId, asOfDate, limit));
    }

    @McpTool(topic = "invoice", name = "statement", description = "Build a plain-text customer statement of open invoices and unapplied payments.", readOnly = true, order = 12)
    public static Object createStatement(McpCallContext ctx,
            @McpParam(name = "partyId", required = true) String partyId,
            @McpParam(name = "asOfDate", description = "yyyy-MM-dd; defaults to today", required = false) String asOfDate,
            @McpParam(name = "invoiceTypeId", required = false) String invoiceTypeId) throws McpToolException {
        Delegator delegator = ctx.getDelegator();
        String typeId = (invoiceTypeId != null && !invoiceTypeId.isEmpty()) ? invoiceTypeId : "SALES_INVOICE";
        Map<String, Object> open = computeOpenItems(ctx, partyId, typeId, asOfDate, 1000);
        @SuppressWarnings("unchecked")
        List<Map<String, Object>> invoiceRows = (List<Map<String, Object>>) open.get("invoices");
        String asOfDateStr = (String) open.get("asOfDate");
        Timestamp asOf = parseAsOfDate(asOfDate);
        String partyName = PartyHelper.getPartyName(delegator, partyId, false);
        String companyName = companyName(delegator, internalOrgPartyIds(delegator));

        BigDecimal unappliedPayments = BigDecimal.ZERO;
        try {
            List<GenericValue> payments = EntityQuery.use(delegator).from("Payment")
                    .where(EntityCondition.makeCondition("partyIdFrom", partyId),
                            EntityCondition.makeCondition("statusId", "PMNT_RECEIVED"),
                            EntityCondition.makeCondition("effectiveDate", EntityOperator.LESS_THAN_EQUAL_TO, asOf))
                    .queryList();
            for (GenericValue p : payments) {
                BigDecimal na = PaymentWorker.getPaymentNotApplied(p);
                if (na != null) unappliedPayments = unappliedPayments.add(na);
            }
        } catch (GenericEntityException e) {
            throw new McpToolException("Statement payment lookup failed: " + e.getMessage());
        }

        BigDecimal totalOpen = (BigDecimal) open.get("totalOpen");
        BigDecimal totalOverdue = (BigDecimal) open.get("totalOverdue");

        StringBuilder sb = new StringBuilder();
        sb.append(companyName).append(" - Statement of Account\n");
        sb.append("Party: ").append(partyName).append(" (").append(partyId).append(")\n");
        sb.append("Date: ").append(asOfDateStr).append("\n\n");
        sb.append(String.format(Locale.ROOT, "%-14s %-12s %-12s %14s %14s%n", "Invoice", "Date", "Due", "Total", "Open"));
        for (Map<String, Object> row : invoiceRows) {
            sb.append(String.format(Locale.ROOT, "%-14s %-12s %-12s %14s %14s%n",
                    row.get("invoiceId"), dateOnly((Timestamp) row.get("invoiceDate")), dateOnly((Timestamp) row.get("dueDate")),
                    row.get("total"), row.get("open")));
        }
        sb.append("\nTotal open: ").append(totalOpen.toPlainString()).append("\n");
        sb.append("Total overdue: ").append(totalOverdue.toPlainString()).append("\n");
        if (unappliedPayments.signum() > 0) {
            sb.append("Unapplied payments: ").append(unappliedPayments.toPlainString()).append("\n");
        }

        Map<String, Object> out = new LinkedHashMap<>();
        out.put("partyId", partyId);
        out.put("partyName", partyName);
        out.put("asOfDate", asOfDateStr);
        out.put("lines", invoiceRows);
        out.put("totalOpen", totalOpen);
        out.put("totalOverdue", totalOverdue);
        out.put("text", sb.toString());
        Map<String, Object> jsonOut = ResultConverter.toJsonMap(out);
        jsonOut.put("text", sb.toString());
        jsonOut.put("next", "send it with mail_send_template templateId=MCP_STATEMENT, partyIdTo=" + partyId + ", text=<text>");
        return jsonOut;
    }

    /** Raw (not yet JSON-converted) open-items data, shared by {@code open_items} and {@code statement_create}. */
    private static Map<String, Object> computeOpenItems(McpCallContext ctx, String partyId, String invoiceTypeId, String asOfDate, Integer limit)
            throws McpToolException {
        Delegator delegator = ctx.getDelegator();
        Timestamp asOf = parseAsOfDate(asOfDate);
        try {
            Set<String> orgIds = internalOrgPartyIds(delegator);
            List<EntityCondition> conds = new ArrayList<>();
            conds.add(EntityCondition.makeCondition("statusId", EntityOperator.IN, OPEN_INVOICE_STATUSES));
            if (invoiceTypeId != null && !invoiceTypeId.isEmpty()) conds.add(EntityCondition.makeCondition("invoiceTypeId", invoiceTypeId));
            if (partyId != null) {
                conds.add(EntityCondition.makeCondition(EntityOperator.OR,
                        EntityCondition.makeCondition("partyId", partyId), EntityCondition.makeCondition("partyIdFrom", partyId)));
            }
            conds.add(EntityCondition.makeCondition("invoiceDate", EntityOperator.LESS_THAN_EQUAL_TO, asOf));
            List<GenericValue> invoices = EntityQuery.use(delegator).from("Invoice").where(conds)
                    .orderBy("dueDate").maxRows(ctx.limit(limit)).queryList();

            List<Map<String, Object>> invoiceRows = new ArrayList<>();
            Map<String, Map<String, Object>> partyTotals = new LinkedHashMap<>();
            BigDecimal totalOpen = BigDecimal.ZERO;
            BigDecimal totalOverdue = BigDecimal.ZERO;
            String currencyUomId = null;
            for (GenericValue inv : invoices) {
                BigDecimal open = InvoiceWorker.getInvoiceNotApplied(inv);
                if (open == null || open.signum() == 0) continue;
                BigDecimal total = InvoiceWorker.getInvoiceTotal(inv);
                Timestamp dueDate = inv.getTimestamp("dueDate");
                boolean overdue = dueDate != null && dueDate.before(asOf);
                long daysOverdue = overdue ? (asOf.getTime() - dueDate.getTime()) / (24L * 60 * 60 * 1000) : 0L;
                String invPartyId = inv.getString("partyId");
                String partyIdFrom = inv.getString("partyIdFrom");
                if (currencyUomId == null) currencyUomId = inv.getString("currencyUomId");

                Map<String, Object> row = new LinkedHashMap<>();
                row.put("invoiceId", inv.getString("invoiceId"));
                row.put("invoiceTypeId", inv.getString("invoiceTypeId"));
                row.put("partyId", invPartyId);
                row.put("partyIdFrom", partyIdFrom);
                row.put("invoiceDate", inv.getTimestamp("invoiceDate"));
                row.put("dueDate", dueDate);
                row.put("total", total);
                row.put("open", open);
                row.put("daysOverdue", daysOverdue);
                invoiceRows.add(row);

                totalOpen = totalOpen.add(open);
                if (overdue) totalOverdue = totalOverdue.add(open);

                String groupPartyId = orgIds.contains(invPartyId) ? partyIdFrom : invPartyId;
                if (groupPartyId == null) groupPartyId = invPartyId;
                Map<String, Object> pt = partyTotals.get(groupPartyId);
                if (pt == null) {
                    pt = new LinkedHashMap<>();
                    pt.put("partyId", groupPartyId);
                    pt.put("partyName", PartyHelper.getPartyName(delegator, groupPartyId, false));
                    pt.put("openTotal", BigDecimal.ZERO);
                    pt.put("overdueTotal", BigDecimal.ZERO);
                    pt.put("count", 0);
                    partyTotals.put(groupPartyId, pt);
                }
                pt.put("openTotal", ((BigDecimal) pt.get("openTotal")).add(open));
                if (overdue) pt.put("overdueTotal", ((BigDecimal) pt.get("overdueTotal")).add(open));
                pt.put("count", ((Integer) pt.get("count")) + 1);
            }

            Map<String, Object> out = new LinkedHashMap<>();
            out.put("asOfDate", dateOnly(asOf));
            out.put("currencyUomId", currencyUomId);
            out.put("parties", new ArrayList<Object>(partyTotals.values()));
            out.put("invoices", invoiceRows);
            out.put("totalOpen", totalOpen);
            out.put("totalOverdue", totalOverdue);
            return out;
        } catch (GenericEntityException e) {
            throw new McpToolException("Open items query failed: " + e.getMessage());
        }
    }

    private static Set<String> internalOrgPartyIds(Delegator delegator) {
        Set<String> ids = new LinkedHashSet<>();
        try {
            List<GenericValue> roles = EntityQuery.use(delegator).from("PartyRole").where("roleTypeId", "INTERNAL_ORGANIZATIO").queryList();
            for (GenericValue r : roles) ids.add(r.getString("partyId"));
        } catch (GenericEntityException e) {
            // best-effort; empty set falls back to grouping by the invoice's bill-to party
        }
        return ids;
    }

    private static String companyName(Delegator delegator, Set<String> orgPartyIds) {
        for (String id : orgPartyIds) {
            try {
                GenericValue pg = EntityQuery.use(delegator).from("PartyGroup").where("partyId", id).queryOne();
                if (pg != null && pg.getString("groupName") != null) return pg.getString("groupName");
            } catch (GenericEntityException e) {
                // try next
            }
        }
        return "Company";
    }

    private static Timestamp parseAsOfDate(String asOfDate) {
        if (asOfDate == null || asOfDate.trim().isEmpty()) {
            return new Timestamp(System.currentTimeMillis());
        }
        try {
            return new Timestamp(java.sql.Date.valueOf(asOfDate.trim()).getTime() + (24L * 60 * 60 * 1000 - 1));
        } catch (IllegalArgumentException e) {
            return new Timestamp(System.currentTimeMillis());
        }
    }

    private static String dateOnly(Timestamp ts) {
        return ts != null ? new java.sql.Date(ts.getTime()).toString() : "";
    }

    // ---- bank statement import ----

    @McpTool(topic = "payment", name = "bank_import", description = "Import a bank statement (CSV or CAMT.053) and match credits to open invoices; dry run unless apply is true.", readOnly = false, destructive = "true", requiresConfirmation = true, order = 88)
    public static Object importBankStatement(McpCallContext ctx,
            @McpParam(name = "finAccountId", required = false) String finAccountId,
            @McpParam(name = "format", description = "CSV or CAMT053; guessed from content when omitted",
                    enumValues = {"CSV", "CAMT053"}, required = false) String format,
            @McpParam(name = "content", description = "The statement file text (CSV or CAMT.053 XML)", required = false) String content,
            @McpParam(name = "rows", description = "Pre-parsed lines: date, amount, reference, counterparty",
                    type = "array", required = false) List<Object> rows,
            @McpParam(name = "invoiceTypeId", required = false) String invoiceTypeId,
            @McpParam(name = "apply", required = false) Boolean apply,
            @McpParam(name = "force", required = false) Boolean force) throws McpToolException {
        Delegator delegator = ctx.getDelegator();
        String typeId = (invoiceTypeId != null && !invoiceTypeId.isEmpty()) ? invoiceTypeId : "SALES_INVOICE";

        List<Map<String, Object>> lines;
        if (rows != null && !rows.isEmpty()) {
            lines = ImportDiff.rows(rows);
        } else if (content != null && !content.trim().isEmpty()) {
            String fmt = (format != null && !format.isEmpty()) ? format : (content.trim().startsWith("<") ? "CAMT053" : "CSV");
            lines = "CAMT053".equalsIgnoreCase(fmt) ? parseCamt053Statement(content) : parseCsvStatement(content);
        } else {
            throw new McpToolException("Provide either content or rows");
        }

        ImportDiff diff = new ImportDiff(apply, force);
        List<Map<String, Object>> toWrite = new ArrayList<>();
        try {
            List<GenericValue> candidates = EntityQuery.use(delegator).from("Invoice")
                    .where(EntityCondition.makeCondition("invoiceTypeId", typeId),
                            EntityCondition.makeCondition("statusId", EntityOperator.IN, OPEN_INVOICE_STATUSES))
                    .queryList();

            int i = 0;
            for (Map<String, Object> line : lines) {
                BigDecimal amount = ImportDiff.decimal(line, "amount");
                String reference = ImportDiff.str(line, "reference", "purpose", "verwendungszweck", "description");
                String counterparty = ImportDiff.str(line, "counterparty", "name", "payee");
                String dateStr = ImportDiff.str(line, "date", "bookingDate", "valueDate");

                if (amount == null) {
                    diff.doubt(i, "line has no amount");
                    i++;
                    continue;
                }
                if (amount.signum() <= 0) {
                    diff.doubt(i, "debit line, not matched (type=debit)");
                    i++;
                    continue;
                }

                GenericValue matched = null;
                String weakDoubt = null;
                if (reference != null) {
                    String refUpper = reference.toUpperCase(Locale.ROOT);
                    for (GenericValue inv : candidates) {
                        String invId = inv.getString("invoiceId");
                        if (invId != null && refUpper.contains(invId.toUpperCase(Locale.ROOT))
                                && amountMatches(InvoiceWorker.getInvoiceNotApplied(inv), amount)) {
                            matched = inv;
                            break;
                        }
                    }
                }
                List<GenericValue> amountMatches = new ArrayList<>();
                if (matched == null) {
                    for (GenericValue inv : candidates) {
                        if (amountMatches(InvoiceWorker.getInvoiceNotApplied(inv), amount)) amountMatches.add(inv);
                    }
                    String hay = ((counterparty != null ? counterparty : "") + " " + (reference != null ? reference : "")).toLowerCase(Locale.ROOT);
                    for (GenericValue inv : amountMatches) {
                        String name = PartyHelper.getPartyName(delegator, inv.getString("partyIdFrom"), false);
                        if (name != null && !name.isEmpty() && hay.contains(name.toLowerCase(Locale.ROOT))) {
                            matched = inv;
                            break;
                        }
                    }
                }
                if (matched == null && amountMatches.size() == 1) {
                    matched = amountMatches.get(0);
                    weakDoubt = "matched by amount only";
                }

                if (matched != null) {
                    String invoiceId = matched.getString("invoiceId");
                    Map<String, Object> data = new LinkedHashMap<>();
                    data.put("invoiceId", invoiceId);
                    data.put("amount", amount);
                    data.put("date", dateStr);
                    data.put("reference", reference);
                    diff.create("payment", invoiceId, data);
                    if (weakDoubt != null) {
                        diff.doubt(i, weakDoubt);
                    } else {
                        Map<String, Object> w = new LinkedHashMap<>();
                        w.put("invoiceId", invoiceId);
                        w.put("amount", amount);
                        w.put("dateStr", dateStr);
                        w.put("reference", reference);
                        w.put("partyIdFrom", matched.getString("partyId"));
                        w.put("partyIdTo", matched.getString("partyIdFrom"));
                        w.put("currencyUomId", matched.getString("currencyUomId"));
                        toWrite.add(w);
                    }
                } else {
                    diff.doubt(i, "no matching open invoice found");
                }
                i++;
            }
        } catch (GenericEntityException e) {
            throw new McpToolException("Bank statement matching failed: " + e.getMessage());
        }

        if (diff.canWrite()) {
            String paymentTypeId = "SALES_INVOICE".equals(typeId) ? "CUSTOMER_PAYMENT" : "VENDOR_PAYMENT";
            for (Map<String, Object> w : toWrite) {
                String invoiceId = (String) w.get("invoiceId");
                Map<String, Object> params = new LinkedHashMap<>();
                params.put("paymentTypeId", paymentTypeId);
                params.put("paymentMethodTypeId", "EFT_ACCOUNT");
                params.put("partyIdFrom", w.get("partyIdFrom"));
                params.put("partyIdTo", w.get("partyIdTo"));
                params.put("amount", w.get("amount"));
                params.put("currencyUomId", w.get("currencyUomId"));
                params.put("effectiveDate", parseLineDate((String) w.get("dateStr")));
                params.put("invoiceId", invoiceId);
                params.put("comments", w.get("reference"));
                if (finAccountId != null) params.put("finAccountId", finAccountId);
                params.put("statusId", "PMNT_RECEIVED");
                Map<String, Object> res = ctx.runService("createPaymentAndApplication", params);
                Map<String, Object> ids = new LinkedHashMap<>();
                ids.put("paymentId", res.get("paymentId"));
                ids.put("paymentApplicationId", res.get("paymentApplicationId"));
                diff.written("payment", invoiceId, ids);
            }
        }
        return ResultConverter.toJson(diff.result());
    }

    private static boolean amountMatches(BigDecimal open, BigDecimal amount) {
        return open != null && open.signum() != 0 && open.subtract(amount).abs().compareTo(new BigDecimal("0.01")) <= 0;
    }

    private static Timestamp parseLineDate(String s) {
        if (s == null || s.trim().length() < 10) return new Timestamp(System.currentTimeMillis());
        try {
            return java.sql.Timestamp.valueOf(s.trim().substring(0, 10) + " 00:00:00");
        } catch (IllegalArgumentException e) {
            return new Timestamp(System.currentTimeMillis());
        }
    }

    private static List<Map<String, Object>> parseCsvStatement(String content) throws McpToolException {
        List<Map<String, Object>> out = new ArrayList<>();
        try (CSVParser parser = CSVFormat.DEFAULT.withFirstRecordAsHeader().withIgnoreHeaderCase().withTrim()
                .parse(new StringReader(content))) {
            for (CSVRecord rec : parser) {
                Map<String, Object> row = new LinkedHashMap<>();
                for (String header : parser.getHeaderNames()) {
                    row.put(header, rec.isSet(header) ? rec.get(header) : null);
                }
                out.add(row);
            }
        } catch (IOException e) {
            throw new McpToolException("CSV parse failed: " + e.getMessage());
        }
        return out;
    }

    private static List<Map<String, Object>> parseCamt053Statement(String content) throws McpToolException {
        List<Map<String, Object>> out = new ArrayList<>();
        try {
            DocumentBuilderFactory dbf = DocumentBuilderFactory.newInstance();
            dbf.setFeature("http://apache.org/xml/features/disallow-doctype-decl", true);
            dbf.setFeature("http://xml.org/sax/features/external-general-entities", false);
            dbf.setFeature("http://xml.org/sax/features/external-parameter-entities", false);
            dbf.setXIncludeAware(false);
            dbf.setExpandEntityReferences(false);
            DocumentBuilder db = dbf.newDocumentBuilder();
            Document doc = db.parse(new InputSource(new StringReader(content)));
            NodeList entries = doc.getElementsByTagNameNS("*", "Ntry");
            for (int i = 0; i < entries.getLength(); i++) {
                Element entry = (Element) entries.item(i);
                String amtText = firstDescendantText(entry, "Amt");
                String cdInd = firstDescendantText(entry, "CdtDbtInd");
                String date = firstDescendantText(entry, "BookgDt");
                if (date == null) date = firstDescendantText(entry, "ValDt");
                String reference = firstDescendantText(entry, "Ustrd");
                String counterparty = firstNestedText(entry, "Dbtr", "Nm");
                if (counterparty == null) counterparty = firstNestedText(entry, "Cdtr", "Nm");

                BigDecimal amt = null;
                if (amtText != null) {
                    try { amt = new BigDecimal(amtText.trim()); } catch (NumberFormatException ignore) { /* leave null */ }
                }
                if (amt != null && "DBIT".equalsIgnoreCase(cdInd)) amt = amt.negate();

                Map<String, Object> row = new LinkedHashMap<>();
                row.put("amount", amt);
                row.put("date", date);
                row.put("reference", reference);
                row.put("counterparty", counterparty);
                out.add(row);
            }
        } catch (Exception e) {
            throw new McpToolException("CAMT.053 parse failed: " + e.getMessage());
        }
        return out;
    }

    private static String firstDescendantText(Element root, String localName) {
        NodeList list = root.getElementsByTagNameNS("*", localName);
        return list.getLength() > 0 ? list.item(0).getTextContent() : null;
    }

    private static String firstNestedText(Element root, String outerLocal, String innerLocal) {
        NodeList outers = root.getElementsByTagNameNS("*", outerLocal);
        for (int i = 0; i < outers.getLength(); i++) {
            String text = firstDescendantText((Element) outers.item(i), innerLocal);
            if (text != null) return text;
        }
        return null;
    }

    @McpResource(uri = "scipio://invoice/{invoiceId}", name = "Invoice", description = "One invoice as JSON with items, status history and payment applications.",
            mimeType = "application/json")
    public static String invoiceResource(McpCallContext ctx, Map<String, String> uriParams) throws McpToolException {
        return JsonRpc.writePretty(getInvoice(ctx, uriParams.get("invoiceId")));
    }
}
