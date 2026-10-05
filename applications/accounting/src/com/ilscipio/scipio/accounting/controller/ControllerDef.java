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
import org.ofbiz.accounting.GlEvents;
import com.ilscipio.scipio.accounting.invoice.InvoiceEvents;

/**
 * Auto-generated annotation-based controller definitions.
 *
 * <p>Generated from controller.xml by convertXmlToAnnotation Gradle task.</p>
 *
 * <p>SCIPIO: 4.0.0: Auto-generated.</p>
 */
public class ControllerDef {
    @com.ilscipio.scipio.ce.webapp.control.def.View(
        name = "main",
        type = "screen",
        page = "component://accounting/widget/AccountingScreens.xml#main",
        controller = "accounting"
    )
    public static final String VIEW_MAIN = "main";

    @com.ilscipio.scipio.ce.webapp.control.def.View(
        name = "apmain",
        type = "screen",
        page = "component://accounting/widget/ap/ApScreens.xml#APDashboard",
        controller = "accounting"
    )
    public static final String VIEW_APMAIN = "apmain";

    @com.ilscipio.scipio.ce.webapp.control.def.View(
        name = "ListAPReports",
        type = "screen",
        page = "component://accounting/widget/invoice/InvoiceScreens.xml#ListAPReports",
        controller = "accounting"
    )
    public static final String VIEW_LISTAPREPORTS = "ListAPReports";

    @com.ilscipio.scipio.ce.webapp.control.def.View(
        name = "FindApPaymentGroups",
        type = "screen",
        page = "component://accounting/widget/payments/PaymentScreens.xml#FindApPaymentGroups",
        controller = "accounting"
    )
    public static final String VIEW_FINDAPPAYMENTGROUPS = "FindApPaymentGroups";

    @com.ilscipio.scipio.ce.webapp.control.def.View(
        name = "FindApInvoices",
        type = "screen",
        page = "component://accounting/widget/invoice/InvoiceScreens.xml#FindApInvoices",
        controller = "accounting"
    )
    public static final String VIEW_FINDAPINVOICES = "FindApInvoices";

    @com.ilscipio.scipio.ce.webapp.control.def.View(
        name = "FindApPayments",
        type = "screen",
        page = "component://accounting/widget/payments/PaymentScreens.xml#FindApPayments",
        controller = "accounting"
    )
    public static final String VIEW_FINDAPPAYMENTS = "FindApPayments";

    @com.ilscipio.scipio.ce.webapp.control.def.View(
        name = "NewPurchaseInvoice",
        type = "screen",
        page = "component://accounting/widget/invoice/InvoiceScreens.xml#NewPurchaseInvoice",
        controller = "accounting"
    )
    public static final String VIEW_NEWPURCHASEINVOICE = "NewPurchaseInvoice";

    @com.ilscipio.scipio.ce.webapp.control.def.View(
        name = "armain",
        type = "screen",
        page = "component://accounting/widget/ar/ArScreens.xml#ARDashboard",
        controller = "accounting"
    )
    public static final String VIEW_ARMAIN = "armain";

    @com.ilscipio.scipio.ce.webapp.control.def.View(
        name = "FindArPaymentGroups",
        type = "screen",
        page = "component://accounting/widget/payments/PaymentScreens.xml#FindArPaymentGroups",
        controller = "accounting"
    )
    public static final String VIEW_FINDARPAYMENTGROUPS = "FindArPaymentGroups";

    @com.ilscipio.scipio.ce.webapp.control.def.View(
        name = "NewIncomingPayment",
        type = "screen",
        page = "component://accounting/widget/payments/PaymentScreens.xml#NewIncomingPayment",
        controller = "accounting"
    )
    public static final String VIEW_NEWINCOMINGPAYMENT = "NewIncomingPayment";

    @com.ilscipio.scipio.ce.webapp.control.def.View(
        name = "BatchPayments",
        type = "screen",
        page = "component://accounting/widget/payments/PaymentScreens.xml#BatchPayments",
        controller = "accounting"
    )
    public static final String VIEW_BATCHPAYMENTS = "BatchPayments";

    @com.ilscipio.scipio.ce.webapp.control.def.View(
        name = "NewSalesInvoice",
        type = "screen",
        page = "component://accounting/widget/invoice/InvoiceScreens.xml#NewSalesInvoice",
        controller = "accounting"
    )
    public static final String VIEW_NEWSALESINVOICE = "NewSalesInvoice";

    @com.ilscipio.scipio.ce.webapp.control.def.View(
        name = "ListARReports",
        type = "screen",
        page = "component://accounting/widget/invoice/InvoiceScreens.xml#ListARReports",
        controller = "accounting"
    )
    public static final String VIEW_LISTARREPORTS = "ListARReports";

    @com.ilscipio.scipio.ce.webapp.control.def.View(
        name = "FindArInvoices",
        type = "screen",
        page = "component://accounting/widget/invoice/InvoiceScreens.xml#FindArInvoices",
        controller = "accounting"
    )
    public static final String VIEW_FINDARINVOICES = "FindArInvoices";

    @com.ilscipio.scipio.ce.webapp.control.def.View(
        name = "FindArPayments",
        type = "screen",
        page = "component://accounting/widget/payments/PaymentScreens.xml#FindArPayments",
        controller = "accounting"
    )
    public static final String VIEW_FINDARPAYMENTS = "FindArPayments";

    @com.ilscipio.scipio.ce.webapp.control.def.View(
        name = "FindBillingAccount",
        type = "screen",
        page = "component://accounting/widget/billing/BillingAccountScreens.xml#FindBillingAccount",
        controller = "accounting"
    )
    public static final String VIEW_FINDBILLINGACCOUNT = "FindBillingAccount";

    @com.ilscipio.scipio.ce.webapp.control.def.View(
        name = "EditBillingAccount",
        type = "screen",
        page = "component://accounting/widget/billing/BillingAccountScreens.xml#EditBillingAccount",
        controller = "accounting"
    )
    public static final String VIEW_EDITBILLINGACCOUNT = "EditBillingAccount";

    @com.ilscipio.scipio.ce.webapp.control.def.View(
        name = "EditBillingAccountRoles",
        type = "screen",
        page = "component://accounting/widget/billing/BillingAccountScreens.xml#EditBillingAccountRoles",
        controller = "accounting"
    )
    public static final String VIEW_EDITBILLINGACCOUNTROLES = "EditBillingAccountRoles";

    @com.ilscipio.scipio.ce.webapp.control.def.View(
        name = "EditBillingAccountTerms",
        type = "screen",
        page = "component://accounting/widget/billing/BillingAccountScreens.xml#EditBillingAccountTerms",
        controller = "accounting"
    )
    public static final String VIEW_EDITBILLINGACCOUNTTERMS = "EditBillingAccountTerms";

    @com.ilscipio.scipio.ce.webapp.control.def.View(
        name = "BillingAccountInvoices",
        type = "screen",
        page = "component://accounting/widget/billing/BillingAccountScreens.xml#BillingAccountInvoices",
        controller = "accounting"
    )
    public static final String VIEW_BILLINGACCOUNTINVOICES = "BillingAccountInvoices";


    // Auto-generated split (Part 2)
    public static class Part2 {
        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "BillingAccountPayments",
            type = "screen",
            page = "component://accounting/widget/billing/BillingAccountScreens.xml#BillingAccountPayments",
            controller = "accounting"
        )
        public static final String VIEW_BILLINGACCOUNTPAYMENTS = "BillingAccountPayments";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "BillingAccountOrders",
            type = "screen",
            page = "component://accounting/widget/billing/BillingAccountScreens.xml#BillingAccountOrders",
            controller = "accounting"
        )
        public static final String VIEW_BILLINGACCOUNTORDERS = "BillingAccountOrders";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "AddGlAccount",
            type = "screen",
            page = "component://accounting/widget/ledger/GlobalGlAccountsScreens.xml#AddGlAccount",
            controller = "accounting"
        )
        public static final String VIEW_ADDGLACCOUNT = "AddGlAccount";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "FindGlobalGlAccount",
            type = "screen",
            page = "component://accounting/widget/ledger/GlobalGlAccountsScreens.xml#ListGlAccounts",
            controller = "accounting"
        )
        public static final String VIEW_FINDGLOBALGLACCOUNT = "FindGlobalGlAccount";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "ListGlobalGlAccounts",
            type = "screen",
            page = "component://accounting/widget/ledger/GlobalGlAccountsScreens.xml#ListGlAccounts",
            controller = "accounting"
        )
        public static final String VIEW_LISTGLOBALGLACCOUNTS = "ListGlobalGlAccounts";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "ListGlAccountsReport",
            type = "screenfop",
            page = "component://accounting/widget/ledger/GlobalGlAccountsScreens.xml#ListGlAccountsReport",
            contentType = "application/pdf",
            encoding = "none",
            controller = "accounting"
        )
        public static final String VIEW_LISTGLACCOUNTSREPORT = "ListGlAccountsReport";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "ListGlAccountsExport",
            type = "screenxml",
            page = "component://accounting/widget/ledger/GlobalGlAccountsScreens.xml#ListGlAccountsReport",
            contentType = "text/xml",
            controller = "accounting"
        )
        public static final String VIEW_LISTGLACCOUNTSEXPORT = "ListGlAccountsExport";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "GlAccountNavigate",
            type = "screen",
            page = "component://accounting/widget/ledger/GlobalGlAccountsScreens.xml#GlAccountNavigate",
            controller = "accounting"
        )
        public static final String VIEW_GLACCOUNTNAVIGATE = "GlAccountNavigate";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "AssignGlAccount",
            type = "screen",
            page = "component://accounting/widget/ledger/GlobalGlAccountsScreens.xml#AssignGlAccount",
            controller = "accounting"
        )
        public static final String VIEW_ASSIGNGLACCOUNT = "AssignGlAccount";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "FindGlAccountReconciliation",
            type = "screen",
            page = "component://accounting/widget/ledger/GlScreens.xml#FindGlAccountReconciliation",
            controller = "accounting"
        )
        public static final String VIEW_FINDGLACCOUNTRECONCILIATION = "FindGlAccountReconciliation";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "FindGlAccountReconciliations",
            type = "screen",
            page = "component://accounting/widget/ledger/GlScreens.xml#FindGlAccountReconciliations",
            controller = "accounting"
        )
        public static final String VIEW_FINDGLACCOUNTRECONCILIATIONS = "FindGlAccountReconciliations";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "EditGlReconciliation",
            type = "screen",
            page = "component://accounting/widget/ledger/GlScreens.xml#EditGlReconciliation",
            controller = "accounting"
        )
        public static final String VIEW_EDITGLRECONCILIATION = "EditGlReconciliation";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "EditGlobalGlAccount",
            type = "screen",
            page = "component://accounting/widget/ledger/GlobalGlAccountsScreens.xml#EditGlobalGlAccount",
            controller = "accounting"
        )
        public static final String VIEW_EDITGLOBALGLACCOUNT = "EditGlobalGlAccount";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "ListGlAccountEntries",
            type = "screen",
            page = "component://accounting/widget/ledger/GlobalGlAccountsScreens.xml#ListGlAccountEntries",
            controller = "accounting"
        )
        public static final String VIEW_LISTGLACCOUNTENTRIES = "ListGlAccountEntries";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "ListAcctgTransEntries",
            type = "screen",
            page = "component://accounting/widget/ledger/GlobalGlAccountsScreens.xml#ListAcctgTransEntries",
            controller = "accounting"
        )
        public static final String VIEW_LISTACCTGTRANSENTRIES = "ListAcctgTransEntries";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "AcctgTransEntriesSearchResultsCsv",
            type = "screencsv",
            page = "component://accounting/widget/ledger/GlScreens.xml#AcctgTransEntriesSearchResultsCsv",
            contentType = "text/csv",
            encoding = "none",
            controller = "accounting"
        )
        public static final String VIEW_ACCTGTRANSENTRIESSEARCHRESULTSCSV = "AcctgTransEntriesSearchResultsCsv";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "AcctgTransEntriesSearchResultsPdf",
            type = "screenfop",
            page = "component://accounting/widget/ledger/GlScreens.xml#AcctgTransEntriesSearchResultsPdf",
            contentType = "application/pdf",
            encoding = "none",
            controller = "accounting"
        )
        public static final String VIEW_ACCTGTRANSENTRIESSEARCHRESULTSPDF = "AcctgTransEntriesSearchResultsPdf";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "AcctgTransSearchResultsCsv",
            type = "screencsv",
            page = "component://accounting/widget/ledger/GlScreens.xml#AcctgTransSearchResultsCsv",
            contentType = "text/csv",
            encoding = "none",
            controller = "accounting"
        )
        public static final String VIEW_ACCTGTRANSSEARCHRESULTSCSV = "AcctgTransSearchResultsCsv";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "AcctgTransSearchResultPdf",
            type = "screenfop",
            page = "component://accounting/widget/ledger/GlScreens.xml#AcctgTransSearchResultPdf",
            contentType = "application/pdf",
            encoding = "none",
            controller = "accounting"
        )
        public static final String VIEW_ACCTGTRANSSEARCHRESULTPDF = "AcctgTransSearchResultPdf";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "AcctgTransDetailReportPdf",
            type = "screenfop",
            page = "component://accounting/widget/ledger/GlScreens.xml#AcctgTransDetailReportPdf",
            contentType = "application/pdf",
            encoding = "none",
            controller = "accounting"
        )
        public static final String VIEW_ACCTGTRANSDETAILREPORTPDF = "AcctgTransDetailReportPdf";

    }

    // Auto-generated split (Part 3)
    public static class Part3 {
        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "PartyAccountsSummary",
            type = "screen",
            page = "component://accounting/widget/ledger/GlScreens.xml#PartyAccountsSummary",
            controller = "accounting"
        )
        public static final String VIEW_PARTYACCOUNTSSUMMARY = "PartyAccountsSummary";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "FindAcctgTrans",
            type = "screen",
            page = "component://accounting/widget/ledger/GlScreens.xml#FindAcctgTrans",
            controller = "accounting"
        )
        public static final String VIEW_FINDACCTGTRANS = "FindAcctgTrans";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "CreateAcctgTransAndEntries",
            type = "screen",
            page = "component://accounting/widget/ledger/GlScreens.xml#CreateAcctgTransAndEntries",
            controller = "accounting"
        )
        public static final String VIEW_CREATEACCTGTRANSANDENTRIES = "CreateAcctgTransAndEntries";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "EditAcctgTrans",
            type = "screen",
            page = "component://accounting/widget/ledger/GlScreens.xml#EditAcctgTrans",
            controller = "accounting"
        )
        public static final String VIEW_EDITACCTGTRANS = "EditAcctgTrans";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "FindAcctgTransEntries",
            type = "screen",
            page = "component://accounting/widget/ledger/GlScreens.xml#FindAcctgTransEntries",
            controller = "accounting"
        )
        public static final String VIEW_FINDACCTGTRANSENTRIES = "FindAcctgTransEntries";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "ListUnpostedAcctgTrans",
            type = "screen",
            page = "component://accounting/widget/ledger/GlScreens.xml#ListUnpostedAcctgTrans",
            controller = "accounting"
        )
        public static final String VIEW_LISTUNPOSTEDACCTGTRANS = "ListUnpostedAcctgTrans";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "ListChecksToPrint",
            type = "screen",
            page = "component://accounting/widget/ledger/GlScreens.xml#ListChecksToPrint",
            controller = "accounting"
        )
        public static final String VIEW_LISTCHECKSTOPRINT = "ListChecksToPrint";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "ListChecksToSend",
            type = "screen",
            page = "component://accounting/widget/ledger/GlScreens.xml#ListChecksToSend",
            controller = "accounting"
        )
        public static final String VIEW_LISTCHECKSTOSEND = "ListChecksToSend";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "NewAcctgTrans",
            type = "screen",
            page = "component://accounting/widget/ledger/GlScreens.xml#NewAcctgTrans",
            controller = "accounting"
        )
        public static final String VIEW_NEWACCTGTRANS = "NewAcctgTrans";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "FindAgreement",
            type = "screen",
            page = "component://accounting/widget/contracts/AgreementScreens.xml#FindAgreement",
            controller = "accounting"
        )
        public static final String VIEW_FINDAGREEMENT = "FindAgreement";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "EditAgreement",
            type = "screen",
            page = "component://accounting/widget/contracts/AgreementScreens.xml#EditAgreement",
            controller = "accounting"
        )
        public static final String VIEW_EDITAGREEMENT = "EditAgreement";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "ListAgreementItems",
            type = "screen",
            page = "component://accounting/widget/contracts/AgreementScreens.xml#ListAgreementItems",
            controller = "accounting"
        )
        public static final String VIEW_LISTAGREEMENTITEMS = "ListAgreementItems";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "EditAgreementItem",
            type = "screen",
            page = "component://accounting/widget/contracts/AgreementScreens.xml#EditAgreementItem",
            controller = "accounting"
        )
        public static final String VIEW_EDITAGREEMENTITEM = "EditAgreementItem";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "EditAgreementTerms",
            type = "screen",
            page = "component://accounting/widget/contracts/AgreementScreens.xml#EditAgreementTerms",
            controller = "accounting"
        )
        public static final String VIEW_EDITAGREEMENTTERMS = "EditAgreementTerms";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "ListAgreementPromoAppls",
            type = "screen",
            page = "component://accounting/widget/contracts/AgreementScreens.xml#ListAgreementPromoAppls",
            controller = "accounting"
        )
        public static final String VIEW_LISTAGREEMENTPROMOAPPLS = "ListAgreementPromoAppls";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "EditAgreementPromoAppl",
            type = "screen",
            page = "component://accounting/widget/contracts/AgreementScreens.xml#EditAgreementPromoAppl",
            controller = "accounting"
        )
        public static final String VIEW_EDITAGREEMENTPROMOAPPL = "EditAgreementPromoAppl";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "ListAgreementItemTerms",
            type = "screen",
            page = "component://accounting/widget/contracts/AgreementScreens.xml#ListAgreementItemTerms",
            controller = "accounting"
        )
        public static final String VIEW_LISTAGREEMENTITEMTERMS = "ListAgreementItemTerms";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "EditAgreementItemTerm",
            type = "screen",
            page = "component://accounting/widget/contracts/AgreementScreens.xml#EditAgreementItemTerm",
            controller = "accounting"
        )
        public static final String VIEW_EDITAGREEMENTITEMTERM = "EditAgreementItemTerm";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "EditAgreementRoles",
            type = "screen",
            page = "component://accounting/widget/contracts/AgreementScreens.xml#EditAgreementRoles",
            controller = "accounting"
        )
        public static final String VIEW_EDITAGREEMENTROLES = "EditAgreementRoles";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "ListAgreementItemProducts",
            type = "screen",
            page = "component://accounting/widget/contracts/AgreementScreens.xml#ListAgreementItemProducts",
            controller = "accounting"
        )
        public static final String VIEW_LISTAGREEMENTITEMPRODUCTS = "ListAgreementItemProducts";

    }

    // Auto-generated split (Part 4)
    public static class Part4 {
        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "ListAgreementItemProductsReport",
            type = "screenfop",
            page = "component://accounting/widget/contracts/AgreementScreens.xml#ListAgreementItemProductsReport",
            contentType = "application/pdf",
            encoding = "none",
            controller = "accounting"
        )
        public static final String VIEW_LISTAGREEMENTITEMPRODUCTSREPORT = "ListAgreementItemProductsReport";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "EditAgreementItemProduct",
            type = "screen",
            page = "component://accounting/widget/contracts/AgreementScreens.xml#EditAgreementItemProduct",
            controller = "accounting"
        )
        public static final String VIEW_EDITAGREEMENTITEMPRODUCT = "EditAgreementItemProduct";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "ListAgreementItemSupplierProducts",
            type = "screen",
            page = "component://accounting/widget/contracts/AgreementScreens.xml#ListAgreementItemSupplierProducts",
            controller = "accounting"
        )
        public static final String VIEW_LISTAGREEMENTITEMSUPPLIERPRODUCTS = "ListAgreementItemSupplierProducts";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "ListAgreementItemSupplierProductsReport",
            type = "screenfop",
            page = "component://accounting/widget/contracts/AgreementScreens.xml#ListAgreementItemSupplierProductsReport",
            contentType = "application/pdf",
            encoding = "none",
            controller = "accounting"
        )
        public static final String VIEW_LISTAGREEMENTITEMSUPPLIERPRODUCTSREPORT = "ListAgreementItemSupplierProductsReport";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "EditAgreementItemSupplierProduct",
            type = "screen",
            page = "component://accounting/widget/contracts/AgreementScreens.xml#EditAgreementItemSupplierProduct",
            controller = "accounting"
        )
        public static final String VIEW_EDITAGREEMENTITEMSUPPLIERPRODUCT = "EditAgreementItemSupplierProduct";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "ListAgreementItemParties",
            type = "screen",
            page = "component://accounting/widget/contracts/AgreementScreens.xml#ListAgreementItemParties",
            controller = "accounting"
        )
        public static final String VIEW_LISTAGREEMENTITEMPARTIES = "ListAgreementItemParties";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "EditAgreementItemParty",
            type = "screen",
            page = "component://accounting/widget/contracts/AgreementScreens.xml#EditAgreementItemParty",
            controller = "accounting"
        )
        public static final String VIEW_EDITAGREEMENTITEMPARTY = "EditAgreementItemParty";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "ListAgreementGeographicalApplic",
            type = "screen",
            page = "component://accounting/widget/contracts/AgreementScreens.xml#ListAgreementGeographicalApplic",
            controller = "accounting"
        )
        public static final String VIEW_LISTAGREEMENTGEOGRAPHICALAPPLIC = "ListAgreementGeographicalApplic";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "EditAgreementGeographicalApplic",
            type = "screen",
            page = "component://accounting/widget/contracts/AgreementScreens.xml#EditAgreementGeographicalApplic",
            controller = "accounting"
        )
        public static final String VIEW_EDITAGREEMENTGEOGRAPHICALAPPLIC = "EditAgreementGeographicalApplic";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "EditAgreementWorkEffortApplics",
            type = "screen",
            page = "component://accounting/widget/contracts/AgreementScreens.xml#EditAgreementWorkEffortApplics",
            controller = "accounting"
        )
        public static final String VIEW_EDITAGREEMENTWORKEFFORTAPPLICS = "EditAgreementWorkEffortApplics";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "ListAgreementItemFacilities",
            type = "screen",
            page = "component://accounting/widget/contracts/AgreementScreens.xml#ListAgreementItemFacilities",
            controller = "accounting"
        )
        public static final String VIEW_LISTAGREEMENTITEMFACILITIES = "ListAgreementItemFacilities";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "EditAgreementItemFacility",
            type = "screen",
            page = "component://accounting/widget/contracts/AgreementScreens.xml#EditAgreementItemFacility",
            controller = "accounting"
        )
        public static final String VIEW_EDITAGREEMENTITEMFACILITY = "EditAgreementItemFacility";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "ListAgreementItemFacilitiesReport",
            type = "screenfop",
            page = "component://accounting/widget/contracts/AgreementScreens.xml#ListAgreementItemFacilitiesReport",
            contentType = "application/pdf",
            encoding = "none",
            controller = "accounting"
        )
        public static final String VIEW_LISTAGREEMENTITEMFACILITIESREPORT = "ListAgreementItemFacilitiesReport";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "CostCenters",
            type = "screen",
            page = "component://accounting/widget/controlling/CostScreens.xml#CostCenters",
            controller = "accounting"
        )
        public static final String VIEW_COSTCENTERS = "CostCenters";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "ListBudgets",
            type = "screen",
            page = "component://accounting/widget/controlling/BudgetScreens.xml#ListBudgets",
            controller = "accounting"
        )
        public static final String VIEW_LISTBUDGETS = "ListBudgets";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "BudgetSearchResults",
            type = "screen",
            page = "component://accounting/widget/controlling/BudgetScreens.xml#BudgetSearchResults",
            controller = "accounting"
        )
        public static final String VIEW_BUDGETSEARCHRESULTS = "BudgetSearchResults";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "EditBudget",
            type = "screen",
            page = "component://accounting/widget/controlling/BudgetScreens.xml#EditBudget",
            controller = "accounting"
        )
        public static final String VIEW_EDITBUDGET = "EditBudget";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "BudgetOverview",
            type = "screen",
            page = "component://accounting/widget/controlling/BudgetScreens.xml#BudgetOverview",
            controller = "accounting"
        )
        public static final String VIEW_BUDGETOVERVIEW = "BudgetOverview";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "EditBudgetItems",
            type = "screen",
            page = "component://accounting/widget/controlling/BudgetScreens.xml#EditBudgetItems",
            controller = "accounting"
        )
        public static final String VIEW_EDITBUDGETITEMS = "EditBudgetItems";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "BudgetRoles",
            type = "screen",
            page = "component://accounting/widget/controlling/BudgetScreens.xml#BudgetRoles",
            controller = "accounting"
        )
        public static final String VIEW_BUDGETROLES = "BudgetRoles";

    }

    // Auto-generated split (Part 5)
    public static class Part5 {
        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "BudgetReviews",
            type = "screen",
            page = "component://accounting/widget/controlling/BudgetScreens.xml#BudgetReviews",
            controller = "accounting"
        )
        public static final String VIEW_BUDGETREVIEWS = "BudgetReviews";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "FindFinAccount",
            type = "screen",
            page = "component://accounting/widget/finance/FinAccountScreens.xml#FindFinAccount",
            controller = "accounting"
        )
        public static final String VIEW_FINDFINACCOUNT = "FindFinAccount";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "FinAccountMain",
            type = "screen",
            page = "component://accounting/widget/finance/FinAccountScreens.xml#FinAccountMain",
            controller = "accounting"
        )
        public static final String VIEW_FINACCOUNTMAIN = "FinAccountMain";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "FindDepositSlips",
            type = "screen",
            page = "component://accounting/widget/finance/FinAccountScreens.xml#FindDepositSlips",
            controller = "accounting"
        )
        public static final String VIEW_FINDDEPOSITSLIPS = "FindDepositSlips";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "EditDepositSlipAndMembers",
            type = "screen",
            page = "component://accounting/widget/finance/FinAccountScreens.xml#EditDepositSlipAndMembers",
            controller = "accounting"
        )
        public static final String VIEW_EDITDEPOSITSLIPANDMEMBERS = "EditDepositSlipAndMembers";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "NewDepositPayment",
            type = "screen",
            page = "component://accounting/widget/finance/FinAccountScreens.xml#NewDepositPayment",
            controller = "accounting"
        )
        public static final String VIEW_NEWDEPOSITPAYMENT = "NewDepositPayment";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "NewWithdrawalPayment",
            type = "screen",
            page = "component://accounting/widget/finance/FinAccountScreens.xml#NewWithdrawalPayment",
            controller = "accounting"
        )
        public static final String VIEW_NEWWITHDRAWALPAYMENT = "NewWithdrawalPayment";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "EditFinAccount",
            type = "screen",
            page = "component://accounting/widget/finance/FinAccountScreens.xml#EditFinAccount",
            controller = "accounting"
        )
        public static final String VIEW_EDITFINACCOUNT = "EditFinAccount";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "EditFinAccountRoles",
            type = "screen",
            page = "component://accounting/widget/finance/FinAccountScreens.xml#EditFinAccountRoles",
            controller = "accounting"
        )
        public static final String VIEW_EDITFINACCOUNTROLES = "EditFinAccountRoles";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "EditFinAccountTrans",
            type = "screen",
            page = "component://accounting/widget/finance/FinAccountScreens.xml#EditFinAccountTrans",
            controller = "accounting"
        )
        public static final String VIEW_EDITFINACCOUNTTRANS = "EditFinAccountTrans";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "EditFinAccountAuths",
            type = "screen",
            page = "component://accounting/widget/finance/FinAccountScreens.xml#EditFinAccountAuths",
            controller = "accounting"
        )
        public static final String VIEW_EDITFINACCOUNTAUTHS = "EditFinAccountAuths";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "EditFinAccountTypeGlAccounts",
            type = "screen",
            page = "component://accounting/widget/settings/GlSetupScreens.xml#EditFinAccountTypeGlAccounts",
            controller = "accounting"
        )
        public static final String VIEW_EDITFINACCOUNTTYPEGLACCOUNTS = "EditFinAccountTypeGlAccounts";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "FindFinAccountTrans",
            type = "screen",
            page = "component://accounting/widget/finance/FinAccountScreens.xml#FindFinAccountTrans",
            controller = "accounting"
        )
        public static final String VIEW_FINDFINACCOUNTTRANS = "FindFinAccountTrans";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "BankReconciliation",
            type = "screen",
            page = "component://accounting/widget/finance/FinAccountScreens.xml#BankReconciliation",
            controller = "accounting"
        )
        public static final String VIEW_BANKRECONCILIATION = "BankReconciliation";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "EditFinAccountReconciliations",
            type = "screen",
            page = "component://accounting/widget/finance/FinAccountScreens.xml#EditFinAccountReconciliations",
            controller = "accounting"
        )
        public static final String VIEW_EDITFINACCOUNTRECONCILIATIONS = "EditFinAccountReconciliations";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "ViewGlReconciliationWithTransaction",
            type = "screen",
            page = "component://accounting/widget/finance/FinAccountScreens.xml#ViewGlReconciliationWithTransaction",
            controller = "accounting"
        )
        public static final String VIEW_VIEWGLRECONCILIATIONWITHTRANSACTION = "ViewGlReconciliationWithTransaction";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "FindFinAccountReconciliations",
            type = "screen",
            page = "component://accounting/widget/finance/FinAccountScreens.xml#FindFinAccountReconciliations",
            controller = "accounting"
        )
        public static final String VIEW_FINDFINACCOUNTRECONCILIATIONS = "FindFinAccountReconciliations";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "ListFixedAssets",
            type = "screen",
            page = "component://accounting/widget/assets/FixedAssetScreens.xml#ListFixedAssets",
            controller = "accounting"
        )
        public static final String VIEW_LISTFIXEDASSETS = "ListFixedAssets";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "FixedAssetSearchResults",
            type = "screen",
            page = "component://accounting/widget/assets/FixedAssetScreens.xml#FixedAssetSearchResults",
            controller = "accounting"
        )
        public static final String VIEW_FIXEDASSETSEARCHRESULTS = "FixedAssetSearchResults";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "EditFixedAsset",
            type = "screen",
            page = "component://accounting/widget/assets/FixedAssetScreens.xml#EditFixedAsset",
            controller = "accounting"
        )
        public static final String VIEW_EDITFIXEDASSET = "EditFixedAsset";

    }

    // Auto-generated split (Part 6)
    public static class Part6 {
        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "ListFixedAssetProducts",
            type = "screen",
            page = "component://accounting/widget/assets/FixedAssetScreens.xml#ListFixedAssetProducts",
            controller = "accounting"
        )
        public static final String VIEW_LISTFIXEDASSETPRODUCTS = "ListFixedAssetProducts";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "EditFixedAssetStdCosts",
            type = "screen",
            page = "component://accounting/widget/assets/FixedAssetScreens.xml#EditFixedAssetStdCosts",
            controller = "accounting"
        )
        public static final String VIEW_EDITFIXEDASSETSTDCOSTS = "EditFixedAssetStdCosts";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "FixedAssetChildren",
            type = "screen",
            page = "component://accounting/widget/assets/FixedAssetScreens.xml#FixedAssetChildren",
            controller = "accounting"
        )
        public static final String VIEW_FIXEDASSETCHILDREN = "FixedAssetChildren";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "EditFixedAssetIdents",
            type = "screen",
            page = "component://accounting/widget/assets/FixedAssetScreens.xml#EditFixedAssetIdents",
            controller = "accounting"
        )
        public static final String VIEW_EDITFIXEDASSETIDENTS = "EditFixedAssetIdents";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "EditFixedAssetRegistrations",
            type = "screen",
            page = "component://accounting/widget/assets/FixedAssetScreens.xml#EditFixedAssetRegistrations",
            controller = "accounting"
        )
        public static final String VIEW_EDITFIXEDASSETREGISTRATIONS = "EditFixedAssetRegistrations";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "EditFixedAssetMaint",
            type = "screen",
            page = "component://accounting/widget/assets/FixedAssetScreens.xml#EditFixedAssetMaint",
            controller = "accounting"
        )
        public static final String VIEW_EDITFIXEDASSETMAINT = "EditFixedAssetMaint";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "ListFixedAssetMaints",
            type = "screen",
            page = "component://accounting/widget/assets/FixedAssetScreens.xml#ListFixedAssetMaints",
            controller = "accounting"
        )
        public static final String VIEW_LISTFIXEDASSETMAINTS = "ListFixedAssetMaints";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "EditFixedAssetMeters",
            type = "screen",
            page = "component://accounting/widget/assets/FixedAssetScreens.xml#EditFixedAssetMeters",
            controller = "accounting"
        )
        public static final String VIEW_EDITFIXEDASSETMETERS = "EditFixedAssetMeters";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "EditFixedAssetMaintOrders",
            type = "screen",
            page = "component://accounting/widget/assets/FixedAssetScreens.xml#EditFixedAssetMaintOrders",
            controller = "accounting"
        )
        public static final String VIEW_EDITFIXEDASSETMAINTORDERS = "EditFixedAssetMaintOrders";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "calendar",
            type = "screen",
            page = "component://accounting/widget/assets/FixedAssetScreens.xml#Calendar",
            controller = "accounting"
        )
        public static final String VIEW_CALENDAR = "calendar";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "WorkEffortSummary",
            type = "screen",
            page = "component://accounting/widget/assets/FixedAssetScreens.xml#WorkEffortSummary",
            controller = "accounting"
        )
        public static final String VIEW_WORKEFFORTSUMMARY = "WorkEffortSummary";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "FixedAssetGeoLocation",
            type = "screen",
            page = "component://accounting/widget/assets/FixedAssetScreens.xml#FixedAssetGeoLocation",
            controller = "accounting"
        )
        public static final String VIEW_FIXEDASSETGEOLOCATION = "FixedAssetGeoLocation";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "ShowFixedAssetDepreciation",
            type = "screen",
            page = "component://accounting/widget/assets/FixedAssetScreens.xml#ShowFixedAssetDepreciation",
            controller = "accounting"
        )
        public static final String VIEW_SHOWFIXEDASSETDEPRECIATION = "ShowFixedAssetDepreciation";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "invoiceOverview",
            type = "screen",
            page = "component://accounting/widget/invoice/InvoiceScreens.xml#invoiceOverview",
            controller = "accounting"
        )
        public static final String VIEW_INVOICEOVERVIEW = "invoiceOverview";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "findInvoices",
            type = "screen",
            page = "component://accounting/widget/invoice/InvoiceScreens.xml#FindInvoices",
            controller = "accounting"
        )
        public static final String VIEW_FINDINVOICES = "findInvoices";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "newInvoice",
            type = "screen",
            page = "component://accounting/widget/invoice/InvoiceScreens.xml#NewInvoice",
            controller = "accounting"
        )
        public static final String VIEW_NEWINVOICE = "newInvoice";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "editInvoice",
            type = "screen",
            page = "component://accounting/widget/invoice/InvoiceScreens.xml#EditInvoice",
            controller = "accounting"
        )
        public static final String VIEW_EDITINVOICE = "editInvoice";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "listInvoiceItems",
            type = "screen",
            page = "component://accounting/widget/invoice/InvoiceScreens.xml#EditInvoiceItems",
            controller = "accounting"
        )
        public static final String VIEW_LISTINVOICEITEMS = "listInvoiceItems";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "editInvoiceTimeEntries",
            type = "screen",
            page = "component://accounting/widget/invoice/InvoiceScreens.xml#EditInvoiceTimeEntries",
            controller = "accounting"
        )
        public static final String VIEW_EDITINVOICETIMEENTRIES = "editInvoiceTimeEntries";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "editInvoiceApplications",
            type = "screen",
            page = "component://accounting/widget/invoice/InvoiceScreens.xml#EditInvoiceApplications",
            controller = "accounting"
        )
        public static final String VIEW_EDITINVOICEAPPLICATIONS = "editInvoiceApplications";

    }

    // Auto-generated split (Part 7)
    public static class Part7 {
        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "invoiceRoles",
            type = "screen",
            page = "component://accounting/widget/invoice/InvoiceScreens.xml#InvoiceRoles",
            controller = "accounting"
        )
        public static final String VIEW_INVOICEROLES = "invoiceRoles";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "invoiceTerms",
            type = "screen",
            page = "component://accounting/widget/invoice/InvoiceScreens.xml#InvoiceTerms",
            controller = "accounting"
        )
        public static final String VIEW_INVOICETERMS = "invoiceTerms";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "DownloadInvoices",
            type = "screen",
            page = "component://accounting/widget/invoice/InvoiceScreens.xml#DownloadInvoices",
            controller = "accounting"
        )
        public static final String VIEW_DOWNLOADINVOICES = "DownloadInvoices";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "sendPerEmail",
            type = "screen",
            page = "component://accounting/widget/invoice/InvoiceScreens.xml#SendPerEmail",
            controller = "accounting"
        )
        public static final String VIEW_SENDPEREMAIL = "sendPerEmail";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "EditInvoiceItemType",
            type = "screen",
            page = "component://accounting/widget/settings/InvoiceItemTypeScreens.xml#EditInvoiceItemType",
            controller = "accounting"
        )
        public static final String VIEW_EDITINVOICEITEMTYPE = "EditInvoiceItemType";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "CommissionRun",
            type = "screen",
            page = "component://accounting/widget/invoice/InvoiceScreens.xml#CommissionRun",
            controller = "accounting"
        )
        public static final String VIEW_COMMISSIONRUN = "CommissionRun";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "CommissionReport",
            type = "screen",
            page = "component://accounting/widget/invoice/InvoiceScreens.xml#CommissionReport",
            controller = "accounting"
        )
        public static final String VIEW_COMMISSIONREPORT = "CommissionReport";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "CommissionReportPdf",
            type = "screenfop",
            page = "component://accounting/widget/AccountingPrintScreens.xml#CommissionReportPdf",
            contentType = "application/pdf",
            encoding = "none",
            controller = "accounting"
        )
        public static final String VIEW_COMMISSIONREPORTPDF = "CommissionReportPdf";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "ImportExport",
            type = "screen",
            page = "component://accounting/widget/tools/ImportExportScreens.xml#ImportExportInvoice",
            controller = "accounting"
        )
        public static final String VIEW_IMPORTEXPORT = "ImportExport";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "ImportExportInvoice",
            type = "screen",
            page = "component://accounting/widget/tools/ImportExportScreens.xml#ImportExportInvoice",
            controller = "accounting"
        )
        public static final String VIEW_IMPORTEXPORTINVOICE = "ImportExportInvoice";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "ExportInvoicesCsv",
            type = "screencsv",
            page = "component://accounting/widget/tools/ImportExportScreens.xml#ExportInvoiceCsv",
            contentType = "text/csv",
            encoding = "none",
            controller = "accounting"
        )
        public static final String VIEW_EXPORTINVOICESCSV = "ExportInvoicesCsv";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "ExportTransactionCsv",
            type = "screencsv",
            page = "component://accounting/widget/tools/ImportExportScreens.xml#ExportTransactionCsv",
            contentType = "text/csv",
            encoding = "none",
            controller = "accounting"
        )
        public static final String VIEW_EXPORTTRANSACTIONCSV = "ExportTransactionCsv";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "ExportTransactions",
            type = "screen",
            page = "component://accounting/widget/tools/ImportExportScreens.xml#ExportTransactions",
            controller = "accounting"
        )
        public static final String VIEW_EXPORTTRANSACTIONS = "ExportTransactions";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "Transactions",
            type = "screen",
            page = "component://accounting/widget/journals/JournalScreens.xml#Transactions",
            controller = "accounting"
        )
        public static final String VIEW_TRANSACTIONS = "Transactions";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "journals",
            type = "screen",
            page = "component://accounting/widget/settings/SettingScreens.xml#Journals",
            controller = "accounting"
        )
        public static final String VIEW_JOURNALS = "journals";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "FinancialSummaryReportOptions",
            type = "screen",
            page = "component://accounting/widget/journals/ReportFinancialSummaryScreens.xml#FinancialSummaryReportOptions",
            controller = "accounting"
        )
        public static final String VIEW_FINANCIALSUMMARYREPORTOPTIONS = "FinancialSummaryReportOptions";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "SalesInvoiceByProductCategorySummary",
            type = "screen",
            page = "component://accounting/widget/journals/ReportFinancialSummaryScreens.xml#SalesInvoiceByProductCategorySummary",
            controller = "accounting"
        )
        public static final String VIEW_SALESINVOICEBYPRODUCTCATEGORYSUMMARY = "SalesInvoiceByProductCategorySummary";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "TrialBalance",
            type = "screen",
            page = "component://accounting/widget/journals/ReportFinancialSummaryScreens.xml#TrialBalance",
            controller = "accounting"
        )
        public static final String VIEW_TRIALBALANCE = "TrialBalance";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "TrialBalanceSearchResultsPdf",
            type = "screenfop",
            page = "component://accounting/widget/journals/ReportFinancialSummaryScreens.xml#TrialBalanceSearchResultsPdf",
            contentType = "application/pdf",
            encoding = "none",
            controller = "accounting"
        )
        public static final String VIEW_TRIALBALANCESEARCHRESULTSPDF = "TrialBalanceSearchResultsPdf";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "TrialBalanceSearchResultsCsv",
            type = "screencsv",
            page = "component://accounting/widget/journals/ReportFinancialSummaryScreens.xml#TrialBalanceSearchResultsCsv",
            contentType = "text/csv",
            encoding = "none",
            controller = "accounting"
        )
        public static final String VIEW_TRIALBALANCESEARCHRESULTSCSV = "TrialBalanceSearchResultsCsv";

    }

    // Auto-generated split (Part 8)
    public static class Part8 {
        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "IncomeStatement",
            type = "screen",
            page = "component://accounting/widget/journals/ReportFinancialSummaryScreens.xml#IncomeStatement",
            controller = "accounting"
        )
        public static final String VIEW_INCOMESTATEMENT = "IncomeStatement";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "IncomeStatementListPdf",
            type = "screenfop",
            page = "component://accounting/widget/journals/ReportFinancialSummaryScreens.xml#IncomeStatementListPdf",
            contentType = "application/pdf",
            encoding = "none",
            controller = "accounting"
        )
        public static final String VIEW_INCOMESTATEMENTLISTPDF = "IncomeStatementListPdf";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "IncomeStatementListCsv",
            type = "screencsv",
            page = "component://accounting/widget/journals/ReportFinancialSummaryScreens.xml#IncomeStatementListCsv",
            contentType = "text/csv",
            encoding = "none",
            controller = "accounting"
        )
        public static final String VIEW_INCOMESTATEMENTLISTCSV = "IncomeStatementListCsv";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "ComparativeIncomeStatement",
            type = "screen",
            page = "component://accounting/widget/journals/ReportFinancialSummaryScreens.xml#ComparativeIncomeStatement",
            controller = "accounting"
        )
        public static final String VIEW_COMPARATIVEINCOMESTATEMENT = "ComparativeIncomeStatement";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "ComparativeIncomeStatementsPdf",
            type = "screenfop",
            page = "component://accounting/widget/journals/ReportFinancialSummaryScreens.xml#ComparativeIncomeStatementsPdf",
            contentType = "application/pdf",
            encoding = "none",
            controller = "accounting"
        )
        public static final String VIEW_COMPARATIVEINCOMESTATEMENTSPDF = "ComparativeIncomeStatementsPdf";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "ComparativeIncomeStatementsCsv",
            type = "screencsv",
            page = "component://accounting/widget/journals/ReportFinancialSummaryScreens.xml#ComparativeIncomeStatementsCsv",
            contentType = "text/csv",
            encoding = "none",
            controller = "accounting"
        )
        public static final String VIEW_COMPARATIVEINCOMESTATEMENTSCSV = "ComparativeIncomeStatementsCsv";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "BalanceSheet",
            type = "screen",
            page = "component://accounting/widget/journals/ReportFinancialSummaryScreens.xml#BalanceSheet",
            controller = "accounting"
        )
        public static final String VIEW_BALANCESHEET = "BalanceSheet";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "BalanceSheetPdf",
            type = "screenfop",
            page = "component://accounting/widget/journals/ReportFinancialSummaryScreens.xml#BalanceSheetPdf",
            contentType = "application/pdf",
            encoding = "none",
            controller = "accounting"
        )
        public static final String VIEW_BALANCESHEETPDF = "BalanceSheetPdf";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "BalanceSheetCsv",
            type = "screencsv",
            page = "component://accounting/widget/journals/ReportFinancialSummaryScreens.xml#BalanceSheetCsv",
            contentType = "text/csv",
            encoding = "none",
            controller = "accounting"
        )
        public static final String VIEW_BALANCESHEETCSV = "BalanceSheetCsv";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "ComparativeBalanceSheet",
            type = "screen",
            page = "component://accounting/widget/journals/ReportFinancialSummaryScreens.xml#ComparativeBalanceSheet",
            controller = "accounting"
        )
        public static final String VIEW_COMPARATIVEBALANCESHEET = "ComparativeBalanceSheet";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "ComparativeBalanceSheetPdf",
            type = "screenfop",
            page = "component://accounting/widget/journals/ReportFinancialSummaryScreens.xml#ComparativeBalanceSheetPdf",
            contentType = "application/pdf",
            encoding = "none",
            controller = "accounting"
        )
        public static final String VIEW_COMPARATIVEBALANCESHEETPDF = "ComparativeBalanceSheetPdf";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "ComparativeBalanceSheetCsv",
            type = "screencsv",
            page = "component://accounting/widget/journals/ReportFinancialSummaryScreens.xml#ComparativeBalanceSheetCsv",
            contentType = "text/csv",
            encoding = "none",
            controller = "accounting"
        )
        public static final String VIEW_COMPARATIVEBALANCESHEETCSV = "ComparativeBalanceSheetCsv";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "TransactionTotals",
            type = "screen",
            page = "component://accounting/widget/journals/ReportFinancialSummaryScreens.xml#TransactionTotals",
            controller = "accounting"
        )
        public static final String VIEW_TRANSACTIONTOTALS = "TransactionTotals";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "TransactionTotalsPdf",
            type = "screenfop",
            page = "component://accounting/widget/journals/ReportFinancialSummaryScreens.xml#TransactionTotalsPdf",
            contentType = "application/pdf",
            encoding = "none",
            controller = "accounting"
        )
        public static final String VIEW_TRANSACTIONTOTALSPDF = "TransactionTotalsPdf";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "TransactionTotalsCsv",
            type = "screencsv",
            page = "component://accounting/widget/journals/ReportFinancialSummaryScreens.xml#TransactionTotalsCsv",
            contentType = "text/csv",
            encoding = "none",
            controller = "accounting"
        )
        public static final String VIEW_TRANSACTIONTOTALSCSV = "TransactionTotalsCsv";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "PaymentsDepositWithdraw",
            type = "screen",
            page = "component://accounting/widget/finance/FinAccountScreens.xml#PaymentsDepositWithdraw",
            controller = "accounting"
        )
        public static final String VIEW_PAYMENTSDEPOSITWITHDRAW = "PaymentsDepositWithdraw";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "InventoryValuation",
            type = "screen",
            page = "component://accounting/widget/journals/ReportFinancialSummaryScreens.xml#InventoryValuation",
            controller = "accounting"
        )
        public static final String VIEW_INVENTORYVALUATION = "InventoryValuation";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "InventoryValuationPdf",
            type = "screenfop",
            page = "component://accounting/widget/journals/ReportFinancialSummaryScreens.xml#InventoryValuationPdf",
            contentType = "application/pdf",
            encoding = "none",
            controller = "accounting"
        )
        public static final String VIEW_INVENTORYVALUATIONPDF = "InventoryValuationPdf";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "InventoryValuationCsv",
            type = "screencsv",
            page = "component://accounting/widget/journals/ReportFinancialSummaryScreens.xml#InventoryValuationCsv",
            contentType = "text/csv",
            encoding = "none",
            controller = "accounting"
        )
        public static final String VIEW_INVENTORYVALUATIONCSV = "InventoryValuationCsv";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "CashFlowStatement",
            type = "screen",
            page = "component://accounting/widget/journals/ReportFinancialSummaryScreens.xml#CashFlowStatement",
            controller = "accounting"
        )
        public static final String VIEW_CASHFLOWSTATEMENT = "CashFlowStatement";

    }

    // Auto-generated split (Part 9)
    public static class Part9 {
        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "CashFlowStatementListPdf",
            type = "screenfop",
            page = "component://accounting/widget/journals/ReportFinancialSummaryScreens.xml#CashFlowStatementListPdf",
            contentType = "application/pdf",
            encoding = "none",
            controller = "accounting"
        )
        public static final String VIEW_CASHFLOWSTATEMENTLISTPDF = "CashFlowStatementListPdf";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "CashFlowStatementListCsv",
            type = "screencsv",
            page = "component://accounting/widget/journals/ReportFinancialSummaryScreens.xml#CashFlowStatementListCsv",
            contentType = "text/csv",
            encoding = "none",
            controller = "accounting"
        )
        public static final String VIEW_CASHFLOWSTATEMENTLISTCSV = "CashFlowStatementListCsv";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "ComparativeCashFlowStatement",
            type = "screen",
            page = "component://accounting/widget/journals/ReportFinancialSummaryScreens.xml#ComparativeCashFlowStatement",
            controller = "accounting"
        )
        public static final String VIEW_COMPARATIVECASHFLOWSTATEMENT = "ComparativeCashFlowStatement";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "ComparativeCashFlowStatementPdf",
            type = "screenfop",
            page = "component://accounting/widget/journals/ReportFinancialSummaryScreens.xml#ComparativeCashFlowStatementPdf",
            contentType = "application/pdf",
            encoding = "none",
            controller = "accounting"
        )
        public static final String VIEW_COMPARATIVECASHFLOWSTATEMENTPDF = "ComparativeCashFlowStatementPdf";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "ComparativeCashFlowStatementCsv",
            type = "screencsv",
            page = "component://accounting/widget/journals/ReportFinancialSummaryScreens.xml#ComparativeCashFlowStatementCsv",
            contentType = "text/csv",
            encoding = "none",
            controller = "accounting"
        )
        public static final String VIEW_COMPARATIVECASHFLOWSTATEMENTCSV = "ComparativeCashFlowStatementCsv";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "SalesInvoiceByProductGlAccountSummary",
            type = "screen",
            page = "component://accounting/widget/journals/ReportFinancialSummaryScreens.xml#SalesInvoiceByProductGlAccountSummary",
            controller = "accounting"
        )
        public static final String VIEW_SALESINVOICEBYPRODUCTGLACCOUNTSUMMARY = "SalesInvoiceByProductGlAccountSummary";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "PaymentByMethodSummary",
            type = "screen",
            page = "component://accounting/widget/journals/ReportFinancialSummaryScreens.xml#PaymentByMethodSummary",
            controller = "accounting"
        )
        public static final String VIEW_PAYMENTBYMETHODSUMMARY = "PaymentByMethodSummary";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "InventoryIssueSummary",
            type = "screen",
            page = "component://accounting/widget/journals/ReportFinancialSummaryScreens.xml#InventoryIssueSummary",
            controller = "accounting"
        )
        public static final String VIEW_INVENTORYISSUESUMMARY = "InventoryIssueSummary";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "FinancialAccountSummary",
            type = "screen",
            page = "component://accounting/widget/journals/ReportFinancialSummaryScreens.xml#FinancialAccountSummary",
            controller = "accounting"
        )
        public static final String VIEW_FINANCIALACCOUNTSUMMARY = "FinancialAccountSummary";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "GlAccountTrialBalance",
            type = "screen",
            page = "component://accounting/widget/journals/ReportFinancialSummaryScreens.xml#GlAccountTrialBalance",
            controller = "accounting"
        )
        public static final String VIEW_GLACCOUNTTRIALBALANCE = "GlAccountTrialBalance";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "GlAccountBalanceByCostCenter",
            type = "screen",
            page = "component://accounting/widget/journals/ReportFinancialSummaryScreens.xml#GlAccountBalanceByCostCenter",
            controller = "accounting"
        )
        public static final String VIEW_GLACCOUNTBALANCEBYCOSTCENTER = "GlAccountBalanceByCostCenter";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "GlAccountBalanceByCostCenterPdf",
            type = "screenfop",
            page = "component://accounting/widget/journals/ReportFinancialSummaryScreens.xml#GlAccountBalanceByCostCenterPdf",
            contentType = "application/pdf",
            encoding = "none",
            controller = "accounting"
        )
        public static final String VIEW_GLACCOUNTBALANCEBYCOSTCENTERPDF = "GlAccountBalanceByCostCenterPdf";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "GlAccountTrialBalanceReportPdf",
            type = "screenfop",
            page = "component://accounting/widget/journals/ReportFinancialSummaryScreens.xml#GlAccountTrialBalanceReportPdf",
            contentType = "application/pdf",
            encoding = "none",
            controller = "accounting"
        )
        public static final String VIEW_GLACCOUNTTRIALBALANCEREPORTPDF = "GlAccountTrialBalanceReportPdf";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "GlAccountTrialBalance",
            type = "screen",
            page = "component://accounting/widget/journals/ReportFinancialSummaryScreens.xml#GlAccountTrialBalance",
            controller = "accounting"
        )
        public static final String VIEW_GLACCOUNTTRIALBALANCE_2 = "GlAccountTrialBalance";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "findPayments",
            type = "screen",
            page = "component://accounting/widget/payments/PaymentScreens.xml#FindPayments",
            controller = "accounting"
        )
        public static final String VIEW_FINDPAYMENTS = "findPayments";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "paymentOverview",
            type = "screen",
            page = "component://accounting/widget/payments/PaymentScreens.xml#PaymentOverview",
            controller = "accounting"
        )
        public static final String VIEW_PAYMENTOVERVIEW = "paymentOverview";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "newPayment",
            type = "screen",
            page = "component://accounting/widget/payments/PaymentScreens.xml#NewPayment",
            controller = "accounting"
        )
        public static final String VIEW_NEWPAYMENT = "newPayment";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "editPayment",
            type = "screen",
            page = "component://accounting/widget/payments/PaymentScreens.xml#EditPayment",
            controller = "accounting"
        )
        public static final String VIEW_EDITPAYMENT = "editPayment";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "editPaymentApplications",
            type = "screen",
            page = "component://accounting/widget/payments/PaymentScreens.xml#EditPaymentApplications",
            controller = "accounting"
        )
        public static final String VIEW_EDITPAYMENTAPPLICATIONS = "editPaymentApplications";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "ManualTransaction",
            type = "screen",
            page = "component://accounting/widget/payments/PaymentScreens.xml#ManualTransaction",
            controller = "accounting"
        )
        public static final String VIEW_MANUALTRANSACTION = "ManualTransaction";

    }

    // Auto-generated split (Part 10)
    public static class Part10 {
        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "PrintChecks",
            type = "screenfop",
            page = "component://accounting/widget/payments/PaymentScreens.xml#PrintChecks",
            contentType = "application/pdf",
            encoding = "none",
            controller = "accounting"
        )
        public static final String VIEW_PRINTCHECKS = "PrintChecks";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "FindSalesInvoicesByDueDate",
            type = "screen",
            page = "component://accounting/widget/payments/PaymentScreens.xml#FindSalesInvoicesByDueDate",
            controller = "accounting"
        )
        public static final String VIEW_FINDSALESINVOICESBYDUEDATE = "FindSalesInvoicesByDueDate";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "FindPurchaseInvoicesByDueDate",
            type = "screen",
            page = "component://accounting/widget/payments/PaymentScreens.xml#FindPurchaseInvoicesByDueDate",
            controller = "accounting"
        )
        public static final String VIEW_FINDPURCHASEINVOICESBYDUEDATE = "FindPurchaseInvoicesByDueDate";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "FindGatewayResponses",
            type = "screen",
            page = "component://accounting/widget/payments/PaymentScreens.xml#FindGatewayResponses",
            controller = "accounting"
        )
        public static final String VIEW_FINDGATEWAYRESPONSES = "FindGatewayResponses";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "ViewGatewayResponse",
            type = "screen",
            page = "component://accounting/widget/payments/PaymentScreens.xml#ViewGatewayResponse",
            controller = "accounting"
        )
        public static final String VIEW_VIEWGATEWAYRESPONSE = "ViewGatewayResponse";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "AuthorizeTransaction",
            type = "screen",
            page = "component://accounting/widget/payments/PaymentScreens.xml#AuthorizeTransaction",
            controller = "accounting"
        )
        public static final String VIEW_AUTHORIZETRANSACTION = "AuthorizeTransaction";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "CaptureTransaction",
            type = "screen",
            page = "component://accounting/widget/payments/PaymentScreens.xml#CaptureTransaction",
            controller = "accounting"
        )
        public static final String VIEW_CAPTURETRANSACTION = "CaptureTransaction";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "FindPaymentGroup",
            type = "screen",
            page = "component://accounting/widget/payments/PaymentGroupScreens.xml#FindPaymentGroup",
            controller = "accounting"
        )
        public static final String VIEW_FINDPAYMENTGROUP = "FindPaymentGroup";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "EditPaymentGroup",
            type = "screen",
            page = "component://accounting/widget/payments/PaymentGroupScreens.xml#EditPaymentGroup",
            controller = "accounting"
        )
        public static final String VIEW_EDITPAYMENTGROUP = "EditPaymentGroup";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "EditPaymentGroupMember",
            type = "screen",
            page = "component://accounting/widget/payments/PaymentGroupScreens.xml#EditPaymentGroupMember",
            controller = "accounting"
        )
        public static final String VIEW_EDITPAYMENTGROUPMEMBER = "EditPaymentGroupMember";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "PaymentGroupOverview",
            type = "screen",
            page = "component://accounting/widget/payments/PaymentGroupScreens.xml#PaymentGroupOverview",
            controller = "accounting"
        )
        public static final String VIEW_PAYMENTGROUPOVERVIEW = "PaymentGroupOverview";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "DepositSlipPdf",
            type = "screenfop",
            page = "component://accounting/widget/payments/PaymentGroupScreens.xml#DepositSlipPdf",
            contentType = "application/pdf",
            encoding = "none",
            controller = "accounting"
        )
        public static final String VIEW_DEPOSITSLIPPDF = "DepositSlipPdf";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "ListCompanies",
            type = "screen",
            page = "component://accounting/widget/settings/GlSetupScreens.xml#ListCompanies",
            controller = "accounting"
        )
        public static final String VIEW_LISTCOMPANIES = "ListCompanies";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "AddCompany",
            type = "screen",
            page = "component://accounting/widget/settings/GlSetupScreens.xml#AddCompany",
            controller = "accounting"
        )
        public static final String VIEW_ADDCOMPANY = "AddCompany";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "AddCustomTimePeriod",
            type = "screen",
            page = "component://accounting/widget/settings/SettingScreens.xml#AddCustomTimePeriod",
            controller = "accounting"
        )
        public static final String VIEW_ADDCUSTOMTIMEPERIOD = "AddCustomTimePeriod";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "EditCustomTimePeriod",
            type = "screen",
            page = "component://accounting/widget/settings/SettingScreens.xml#EditCustomTimePeriod",
            controller = "accounting"
        )
        public static final String VIEW_EDITCUSTOMTIMEPERIOD = "EditCustomTimePeriod";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "PartyAcctgPreference",
            type = "screen",
            page = "component://accounting/widget/settings/GlSetupScreens.xml#PartyAcctgPreference",
            controller = "accounting"
        )
        public static final String VIEW_PARTYACCTGPREFERENCE = "PartyAcctgPreference";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "ListGlAccountOrganization",
            type = "screen",
            page = "component://accounting/widget/settings/GlSetupScreens.xml#ListGlAccountOrganization",
            controller = "accounting"
        )
        public static final String VIEW_LISTGLACCOUNTORGANIZATION = "ListGlAccountOrganization";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "SetupGlJournals",
            type = "screen",
            page = "component://accounting/widget/settings/GlSetupScreens.xml#SetupGlJournals",
            controller = "accounting"
        )
        public static final String VIEW_SETUPGLJOURNALS = "SetupGlJournals";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "ViewFXConversions",
            type = "screen",
            page = "component://accounting/widget/settings/GlSetupScreens.xml#ViewFXConversions",
            controller = "accounting"
        )
        public static final String VIEW_VIEWFXCONVERSIONS = "ViewFXConversions";

    }

    // Auto-generated split (Part 11)
    public static class Part11 {
        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "ViewRateAmounts",
            type = "screen",
            page = "component://accounting/widget/settings/GlSetupScreens.xml#ViewRateAmounts",
            controller = "accounting"
        )
        public static final String VIEW_VIEWRATEAMOUNTS = "ViewRateAmounts";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "GlAccountTypeDefaults",
            type = "screen",
            page = "component://accounting/widget/settings/GlSetupScreens.xml#GlAccountTypeDefaults",
            controller = "accounting"
        )
        public static final String VIEW_GLACCOUNTTYPEDEFAULTS = "GlAccountTypeDefaults";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "GlAccountPurInvoice",
            type = "screen",
            page = "component://accounting/widget/settings/GlSetupScreens.xml#GlAccountPurInvoice",
            controller = "accounting"
        )
        public static final String VIEW_GLACCOUNTPURINVOICE = "GlAccountPurInvoice";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "GlAccountSalInvoice",
            type = "screen",
            page = "component://accounting/widget/settings/GlSetupScreens.xml#GlAccountSalInvoice",
            controller = "accounting"
        )
        public static final String VIEW_GLACCOUNTSALINVOICE = "GlAccountSalInvoice";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "GlAccountTypePaymentType",
            type = "screen",
            page = "component://accounting/widget/settings/GlSetupScreens.xml#GlAccountTypePaymentType",
            controller = "accounting"
        )
        public static final String VIEW_GLACCOUNTTYPEPAYMENTTYPE = "GlAccountTypePaymentType";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "GlAccountNrPaymentMethod",
            type = "screen",
            page = "component://accounting/widget/settings/GlSetupScreens.xml#GlAccountNrPaymentMethod",
            controller = "accounting"
        )
        public static final String VIEW_GLACCOUNTNRPAYMENTMETHOD = "GlAccountNrPaymentMethod";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "EditProductGlAccounts",
            type = "screen",
            page = "component://accounting/widget/settings/GlSetupScreens.xml#EditProductGlAccounts",
            controller = "accounting"
        )
        public static final String VIEW_EDITPRODUCTGLACCOUNTS = "EditProductGlAccounts";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "EditProductCategoryGlAccounts",
            type = "screen",
            page = "component://accounting/widget/settings/GlSetupScreens.xml#EditProductCategoryGlAccounts",
            controller = "accounting"
        )
        public static final String VIEW_EDITPRODUCTCATEGORYGLACCOUNTS = "EditProductCategoryGlAccounts";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "EditVarianceReasonGlAccounts",
            type = "screen",
            page = "component://accounting/widget/settings/GlSetupScreens.xml#EditVarianceReasonGlAccounts",
            controller = "accounting"
        )
        public static final String VIEW_EDITVARIANCEREASONGLACCOUNTS = "EditVarianceReasonGlAccounts";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "EditCreditCardTypeGlAccounts",
            type = "screen",
            page = "component://accounting/widget/settings/GlSetupScreens.xml#EditCreditCardTypeGlAccounts",
            controller = "accounting"
        )
        public static final String VIEW_EDITCREDITCARDTYPEGLACCOUNTS = "EditCreditCardTypeGlAccounts";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "FixedAssetTypeGlAccounts",
            type = "screen",
            page = "component://accounting/widget/settings/GlSetupScreens.xml#FixedAssetTypeGlAccounts",
            controller = "accounting"
        )
        public static final String VIEW_FIXEDASSETTYPEGLACCOUNTS = "FixedAssetTypeGlAccounts";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "ListGlAccountOrgPdf",
            type = "screenfop",
            page = "component://accounting/widget/settings/GlSetupScreens.xml#ListGlAccountOrgPdf",
            contentType = "application/pdf",
            encoding = "none",
            controller = "accounting"
        )
        public static final String VIEW_LISTGLACCOUNTORGPDF = "ListGlAccountOrgPdf";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "ListGlAccountOrgCsv",
            type = "screencsv",
            page = "component://accounting/widget/settings/GlSetupScreens.xml#ListGlAccountOrgCsv",
            contentType = "text/csv",
            encoding = "none",
            controller = "accounting"
        )
        public static final String VIEW_LISTGLACCOUNTORGCSV = "ListGlAccountOrgCsv";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "EditPartyGlAccount",
            type = "screen",
            page = "component://accounting/widget/settings/GlSetupScreens.xml#EditPartyGlAccount",
            controller = "accounting"
        )
        public static final String VIEW_EDITPARTYGLACCOUNT = "EditPartyGlAccount";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "EditGlAccountCategory",
            type = "screen",
            page = "component://accounting/widget/settings/GlSetupScreens.xml#EditGlAccountCategory",
            controller = "accounting"
        )
        public static final String VIEW_EDITGLACCOUNTCATEGORY = "EditGlAccountCategory";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "FindGlAccountCategory",
            type = "screen",
            page = "component://accounting/widget/settings/GlSetupScreens.xml#FindGlAccountCategory",
            controller = "accounting"
        )
        public static final String VIEW_FINDGLACCOUNTCATEGORY = "FindGlAccountCategory";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "EditGlAccountCategoryMember",
            type = "screen",
            page = "component://accounting/widget/settings/GlSetupScreens.xml#EditGlAccountCategoryMember",
            controller = "accounting"
        )
        public static final String VIEW_EDITGLACCOUNTCATEGORYMEMBER = "EditGlAccountCategoryMember";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "EditInvoiceItemType",
            type = "screen",
            page = "component://accounting/widget/settings/InvoiceItemTypeScreens.xml#EditInvoiceItemType",
            controller = "accounting"
        )
        public static final String VIEW_EDITINVOICEITEMTYPE_2 = "EditInvoiceItemType";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "EditPaymentMethodType",
            type = "screen",
            page = "component://accounting/widget/settings/PaymentMethodTypeScreens.xml#EditPaymentMethodType",
            controller = "accounting"
        )
        public static final String VIEW_EDITPAYMENTMETHODTYPE = "EditPaymentMethodType";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "FindPaymentGatewayConfig",
            type = "screen",
            page = "component://accounting/widget/settings/PaymentGatewayConfigScreens.xml#FindPaymentGatewayConfig",
            controller = "accounting"
        )
        public static final String VIEW_FINDPAYMENTGATEWAYCONFIG = "FindPaymentGatewayConfig";

    }

    // Auto-generated split (Part 12)
    public static class Part12 {
        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "EditPaymentGatewayConfig",
            type = "screen",
            page = "component://accounting/widget/settings/PaymentGatewayConfigScreens.xml#EditPaymentGatewayConfig",
            controller = "accounting"
        )
        public static final String VIEW_EDITPAYMENTGATEWAYCONFIG = "EditPaymentGatewayConfig";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "FindPaymentGatewayConfigTypes",
            type = "screen",
            page = "component://accounting/widget/settings/PaymentGatewayConfigScreens.xml#FindPaymentGatewayConfigTypes",
            controller = "accounting"
        )
        public static final String VIEW_FINDPAYMENTGATEWAYCONFIGTYPES = "FindPaymentGatewayConfigTypes";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "EditPaymentGatewayConfigType",
            type = "screen",
            page = "component://accounting/widget/settings/PaymentGatewayConfigScreens.xml#EditPaymentGatewayConfigType",
            controller = "accounting"
        )
        public static final String VIEW_EDITPAYMENTGATEWAYCONFIGTYPE = "EditPaymentGatewayConfigType";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "EditOrganizationTaxAuthorityGlAccounts",
            type = "screen",
            page = "component://accounting/widget/settings/GlSetupScreens.xml#EditOrganizationTaxAuthorityGlAccounts",
            controller = "accounting"
        )
        public static final String VIEW_EDITORGANIZATIONTAXAUTHORITYGLACCOUNTS = "EditOrganizationTaxAuthorityGlAccounts";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "FindTaxAuthority",
            type = "screen",
            page = "component://accounting/widget/settings/TaxAuthorityScreens.xml#FindTaxAuthority",
            controller = "accounting"
        )
        public static final String VIEW_FINDTAXAUTHORITY = "FindTaxAuthority";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "EditTaxAuthority",
            type = "screen",
            page = "component://accounting/widget/settings/TaxAuthorityScreens.xml#EditTaxAuthority",
            controller = "accounting"
        )
        public static final String VIEW_EDITTAXAUTHORITY = "EditTaxAuthority";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "EditTaxAuthorityCategories",
            type = "screen",
            page = "component://accounting/widget/settings/TaxAuthorityScreens.xml#EditTaxAuthorityCategories",
            controller = "accounting"
        )
        public static final String VIEW_EDITTAXAUTHORITYCATEGORIES = "EditTaxAuthorityCategories";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "EditTaxAuthorityAssocs",
            type = "screen",
            page = "component://accounting/widget/settings/TaxAuthorityScreens.xml#EditTaxAuthorityAssocs",
            controller = "accounting"
        )
        public static final String VIEW_EDITTAXAUTHORITYASSOCS = "EditTaxAuthorityAssocs";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "EditTaxAuthorityGlAccounts",
            type = "screen",
            page = "component://accounting/widget/settings/TaxAuthorityScreens.xml#EditTaxAuthorityGlAccounts",
            controller = "accounting"
        )
        public static final String VIEW_EDITTAXAUTHORITYGLACCOUNTS = "EditTaxAuthorityGlAccounts";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "EditTaxAuthorityRateProducts",
            type = "screen",
            page = "component://accounting/widget/settings/TaxAuthorityScreens.xml#EditTaxAuthorityRateProducts",
            controller = "accounting"
        )
        public static final String VIEW_EDITTAXAUTHORITYRATEPRODUCTS = "EditTaxAuthorityRateProducts";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "ListTaxAuthorityParties",
            type = "screen",
            page = "component://accounting/widget/settings/TaxAuthorityScreens.xml#ListTaxAuthorityParties",
            controller = "accounting"
        )
        public static final String VIEW_LISTTAXAUTHORITYPARTIES = "ListTaxAuthorityParties";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "EditTaxAuthorityPartyInfo",
            type = "screen",
            page = "component://accounting/widget/settings/TaxAuthorityScreens.xml#EditTaxAuthorityPartyInfo",
            controller = "accounting"
        )
        public static final String VIEW_EDITTAXAUTHORITYPARTYINFO = "EditTaxAuthorityPartyInfo";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "FindVendors",
            type = "screen",
            page = "component://accounting/widget/settings/SettingScreens.xml#FindVendors",
            controller = "accounting"
        )
        public static final String VIEW_FINDVENDORS = "FindVendors";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "EditVendor",
            type = "screen",
            page = "component://accounting/widget/settings/SettingScreens.xml#EditVendor",
            controller = "accounting"
        )
        public static final String VIEW_EDITVENDOR = "EditVendor";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "viewprofile",
            type = "screen",
            page = "component://party/widget/partymgr/PartyScreens.xml#viewprofile",
            controller = "accounting"
        )
        public static final String VIEW_VIEWPROFILE = "viewprofile";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "LookupPerson",
            type = "screen",
            page = "component://party/widget/partymgr/LookupScreens.xml#LookupPerson",
            controller = "accounting"
        )
        public static final String VIEW_LOOKUPPERSON = "LookupPerson";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "LookupPartyGroup",
            type = "screen",
            page = "component://party/widget/partymgr/LookupScreens.xml#LookupPartyGroup",
            controller = "accounting"
        )
        public static final String VIEW_LOOKUPPARTYGROUP = "LookupPartyGroup";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "LookupPartyName",
            type = "screen",
            page = "component://party/widget/partymgr/LookupScreens.xml#LookupPartyName",
            controller = "accounting"
        )
        public static final String VIEW_LOOKUPPARTYNAME = "LookupPartyName";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "LookupInternalOrganization",
            type = "screen",
            page = "component://party/widget/partymgr/LookupScreens.xml#LookupInternalOrganization",
            controller = "accounting"
        )
        public static final String VIEW_LOOKUPINTERNALORGANIZATION = "LookupInternalOrganization";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "LookupProduct",
            type = "screen",
            page = "component://product/widget/catalog/LookupScreens.xml#LookupProduct",
            controller = "accounting"
        )
        public static final String VIEW_LOOKUPPRODUCT = "LookupProduct";

    }

    // Auto-generated split (Part 13)
    public static class Part13 {
        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "LookupProductFeature",
            type = "screen",
            page = "component://product/widget/catalog/LookupScreens.xml#LookupProductFeature",
            controller = "accounting"
        )
        public static final String VIEW_LOOKUPPRODUCTFEATURE = "LookupProductFeature";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "LookupVariantProduct",
            type = "screen",
            page = "component://product/widget/catalog/LookupScreens.xml#LookupVariantProduct",
            controller = "accounting"
        )
        public static final String VIEW_LOOKUPVARIANTPRODUCT = "LookupVariantProduct";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "LookupProductCategory",
            type = "screen",
            page = "component://product/widget/catalog/LookupScreens.xml#LookupProductCategory",
            controller = "accounting"
        )
        public static final String VIEW_LOOKUPPRODUCTCATEGORY = "LookupProductCategory";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "LookupProductStore",
            type = "screen",
            page = "component://product/widget/catalog/LookupScreens.xml#LookupProductStore",
            controller = "accounting"
        )
        public static final String VIEW_LOOKUPPRODUCTSTORE = "LookupProductStore";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "LookupFixedAsset",
            type = "screen",
            page = "component://accounting/widget/LookupScreens.xml#LookupFixedAsset",
            controller = "accounting"
        )
        public static final String VIEW_LOOKUPFIXEDASSET = "LookupFixedAsset";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "LookupGlAccount",
            type = "screen",
            page = "component://accounting/widget/LookupScreens.xml#LookupGlAccount",
            controller = "accounting"
        )
        public static final String VIEW_LOOKUPGLACCOUNT = "LookupGlAccount";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "LookupBillingAccount",
            type = "screen",
            page = "component://accounting/widget/LookupScreens.xml#LookupBillingAccount",
            controller = "accounting"
        )
        public static final String VIEW_LOOKUPBILLINGACCOUNT = "LookupBillingAccount";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "LookupPayment",
            type = "screen",
            page = "component://accounting/widget/LookupScreens.xml#LookupPayment",
            controller = "accounting"
        )
        public static final String VIEW_LOOKUPPAYMENT = "LookupPayment";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "LookupInvoice",
            type = "screen",
            page = "component://accounting/widget/LookupScreens.xml#LookupInvoice",
            controller = "accounting"
        )
        public static final String VIEW_LOOKUPINVOICE = "LookupInvoice";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "LookupCustomTimePeriod",
            type = "screen",
            page = "component://accounting/widget/LookupScreens.xml#LookupCustomTimePeriod",
            controller = "accounting"
        )
        public static final String VIEW_LOOKUPCUSTOMTIMEPERIOD = "LookupCustomTimePeriod";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "LookupWorkEffort",
            type = "screen",
            page = "component://workeffort/widget/LookupScreens.xml#LookupWorkEffort",
            controller = "accounting"
        )
        public static final String VIEW_LOOKUPWORKEFFORT = "LookupWorkEffort";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "LookupOrderHeader",
            type = "screen",
            page = "component://order/widget/ordermgr/LookupScreens.xml#LookupOrderHeader",
            controller = "accounting"
        )
        public static final String VIEW_LOOKUPORDERHEADER = "LookupOrderHeader";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "LookupFacility",
            type = "screen",
            page = "component://product/widget/facility/LookupScreens.xml#LookupFacility",
            controller = "accounting"
        )
        public static final String VIEW_LOOKUPFACILITY = "LookupFacility";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "LookupFacilityLocation",
            type = "screen",
            page = "component://product/widget/facility/LookupScreens.xml#LookupFacilityLocation",
            controller = "accounting"
        )
        public static final String VIEW_LOOKUPFACILITYLOCATION = "LookupFacilityLocation";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "LookupShipment",
            type = "screen",
            page = "component://product/widget/facility/LookupScreens.xml#LookupShipment",
            controller = "accounting"
        )
        public static final String VIEW_LOOKUPSHIPMENT = "LookupShipment";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "LookupAgreement",
            type = "screen",
            page = "component://accounting/widget/LookupScreens.xml#LookupAgreement",
            controller = "accounting"
        )
        public static final String VIEW_LOOKUPAGREEMENT = "LookupAgreement";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "LookupAgreementItem",
            type = "screen",
            page = "component://accounting/widget/LookupScreens.xml#LookupAgreementItem",
            controller = "accounting"
        )
        public static final String VIEW_LOOKUPAGREEMENTITEM = "LookupAgreementItem";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "LookupPaymentGroupMember",
            type = "screen",
            page = "component://accounting/widget/LookupScreens.xml#LookupPaymentGroupMember",
            controller = "accounting"
        )
        public static final String VIEW_LOOKUPPAYMENTGROUPMEMBER = "LookupPaymentGroupMember";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "LookupGlReconciliation",
            type = "screen",
            page = "component://accounting/widget/LookupScreens.xml#LookupGlReconciliation",
            controller = "accounting"
        )
        public static final String VIEW_LOOKUPGLRECONCILIATION = "LookupGlReconciliation";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "LookupTaxAuthorityGeo",
            type = "screen",
            page = "component://accounting/widget/LookupScreens.xml#LookupTaxAuthorityGeo",
            controller = "accounting"
        )
        public static final String VIEW_LOOKUPTAXAUTHORITYGEO = "LookupTaxAuthorityGeo";

    }

    // Auto-generated split (Part 14)
    public static class Part14 {
        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "LookupTaxAuthorityPartyName",
            type = "screen",
            page = "component://accounting/widget/LookupScreens.xml#LookupTaxAuthorityPartyName",
            controller = "accounting"
        )
        public static final String VIEW_LOOKUPTAXAUTHORITYPARTYNAME = "LookupTaxAuthorityPartyName";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "LookupOrderPaymentPreference",
            type = "screen",
            page = "component://accounting/widget/LookupScreens.xml#LookupOrderPaymentPreference",
            controller = "accounting"
        )
        public static final String VIEW_LOOKUPORDERPAYMENTPREFERENCE = "LookupOrderPaymentPreference";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "LookupCustomerName",
            type = "screen",
            page = "component://party/widget/partymgr/LookupScreens.xml#LookupCustomerName",
            controller = "accounting"
        )
        public static final String VIEW_LOOKUPCUSTOMERNAME = "LookupCustomerName";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "NewDepositSlip",
            type = "screen",
            page = "component://accounting/widget/finance/FinAccountScreens.xml#NewDepositSlip",
            controller = "accounting"
        )
        public static final String VIEW_NEWDEPOSITSLIP = "NewDepositSlip";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "InvoicePDF",
            type = "screenfop",
            page = "component://accounting/widget/AccountingPrintScreens.xml#InvoicePDF",
            contentType = "application/pdf",
            encoding = "none",
            controller = "accounting"
        )
        public static final String VIEW_INVOICEPDF = "InvoicePDF";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "PrintCheckPDF",
            type = "screenfop",
            page = "component://accounting/widget/AccountingPrintScreens.xml#PrintCheckPDF",
            contentType = "application/pdf",
            encoding = "none",
            controller = "accounting"
        )
        public static final String VIEW_PRINTCHECKPDF = "PrintCheckPDF";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "PrintInvoices",
            type = "screenfop",
            page = "component://accounting/widget/AccountingPrintScreens.xml#PrintInvoices",
            contentType = "application/pdf",
            encoding = "none",
            controller = "accounting"
        )
        public static final String VIEW_PRINTINVOICES = "PrintInvoices";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "ScpEgltCommon.js",
            type = "screen",
            page = "component://accounting/widget/AccountingScreens.xml#ScpEgltCommon.js",
            contentType = "application/javascript",
            controller = "accounting"
        )
        public static final String VIEW_SCPEGLTCOMMON_JS = "ScpEgltCommon.js";

        @Request(
            uri = "view",
            controller = "accounting",
            secure = "true"
        )
        @Response(name = "success", type = "request", value = "main")
        public interface ViewDef {}

        @Request(
            uri = "main",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "main")
        public interface Main {}

        @Request(
            uri = "apmain",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "apmain")
        public interface Apmain {}

        @Request(
            uri = "FindApPaymentGroups",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "FindApPaymentGroups")
        public interface FindApPaymentGroups {}

        @Request(
            uri = "massChangeInvoiceStatus",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "FindApInvoices")
        @Event(type = "service", invoke = "massChangeInvoiceStatus")
        public static String massChangeInvoiceStatus(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "cancelCheckRunPayments",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "PaymentGroupOverview")
        @Response(name = "error", type = "view", value = "FindApPaymentGroups")
        @Event(type = "service", invoke = "cancelCheckRunPayments")
        public static String cancelCheckRunPayments(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "FindApInvoices",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "FindApInvoices")
        public interface FindApInvoices {}

        @Request(
            uri = "FindApPayments",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "FindApPayments")
        public interface FindApPayments {}

        @Request(
            uri = "listAPReports",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ListAPReports")
        public interface ListAPReports {}

        @Request(
            uri = "newPurchaseInvoice",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "NewPurchaseInvoice")
        public interface NewPurchaseInvoice {}

        @Request(
            uri = "NewPurchaseInvoice",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "NewPurchaseInvoice")
        public interface NewPurchaseInvoice1 {}

        @Request(
            uri = "processMassCheckRun",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "request", value = "PaymentGroupOverview")
        @Response(name = "error", type = "view", value = "FindPurchaseInvoices")
        @Event(type = "service", invoke = "createPaymentAndPaymentGroupForInvoices")
        public static String processMassCheckRun(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

    }

    // Auto-generated split (Part 15)
    public static class Part15 {
        @Request(
            uri = "armain",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "armain")
        public interface Armain {}

        @Request(
            uri = "ListARReports",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ListARReports")
        public interface ListARReports {}

        @Request(
            uri = "FindArPaymentGroups",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "FindArPaymentGroups")
        public interface FindArPaymentGroups {}

        @Request(
            uri = "massChangePaymentStatus",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "BatchPayments")
        @Response(name = "error", type = "view", value = "BatchPayments")
        @Event(type = "service", invoke = "massChangePaymentStatus")
        public static String massChangePaymentStatus(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "massChangeInvoiceStatus",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "FindArInvoices")
        @Response(name = "error", type = "view", value = "FindArInvoices")
        @Event(type = "service", invoke = "massChangeInvoiceStatus")
        public static String massChangeInvoiceStatus_2(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "cancelPaymentGroup",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "PaymentGroupOverview")
        @Response(name = "error", type = "view", value = "FindArPaymentGroups")
        @Event(type = "service", invoke = "cancelPaymentBatch")
        public static String cancelPaymentGroup(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "findArPayments",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "FindArPayments")
        public interface FindArPayments {}

        @Request(
            uri = "findArInvoices",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "FindArInvoices")
        public interface FindArInvoices {}

        @Request(
            uri = "NewSalesInvoice",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "NewSalesInvoice")
        public interface NewSalesInvoice {}

        @Request(
            uri = "FindBillingAccount",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "FindBillingAccount")
        public interface FindBillingAccount {}

        @Request(
            uri = "EditBillingAccount",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditBillingAccount")
        public interface EditBillingAccount {}

        @Request(
            uri = "createBillingAccount",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditBillingAccount")
        @Response(name = "error", type = "view", value = "EditBillingAccount")
        @Event(type = "service", invoke = "createBillingAccount")
        public static String createBillingAccount(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "updateBillingAccount",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditBillingAccount")
        @Response(name = "error", type = "view", value = "EditBillingAccount")
        @Event(type = "service", invoke = "updateBillingAccount")
        public static String updateBillingAccount(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "EditBillingAccountRoles",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditBillingAccountRoles")
        public interface EditBillingAccountRoles {}

        @Request(
            uri = "createBillingAccountRole",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditBillingAccountRoles")
        @Response(name = "error", type = "view", value = "EditBillingAccountRoles")
        @Event(type = "service", invoke = "createBillingAccountRole")
        public static String createBillingAccountRole(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "updateBillingAccountRole",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditBillingAccountRoles")
        @Response(name = "error", type = "view", value = "EditBillingAccountRoles")
        @Event(type = "service-multi", invoke = "updateBillingAccountRole")
        public static String updateBillingAccountRole(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "deleteBillingAccountRole",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditBillingAccountRoles")
        @Response(name = "error", type = "view", value = "EditBillingAccountRoles")
        @Event(type = "service", invoke = "removeBillingAccountRole")
        public static String deleteBillingAccountRole(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "EditBillingAccountTerms",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditBillingAccountTerms")
        public interface EditBillingAccountTerms {}

        @Request(
            uri = "createBillingAccountTerm",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditBillingAccountTerms")
        @Event(type = "service", invoke = "createBillingAccountTerm")
        public static String createBillingAccountTerm(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "updateBillingAccountTerm",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditBillingAccountTerms")
        @Event(type = "service", invoke = "updateBillingAccountTerm")
        public static String updateBillingAccountTerm(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

    }

    // Auto-generated split (Part 16)
    public static class Part16 {
        @Request(
            uri = "removeBillingAccountTerm",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditBillingAccountTerms")
        @Event(type = "service", invoke = "removeBillingAccountTerm")
        public static String removeBillingAccountTerm(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "BillingAccountInvoices",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "BillingAccountInvoices")
        public interface BillingAccountInvoices {}

        @Request(
            uri = "capturePaymentsByInvoice",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "BillingAccountInvoices")
        @Event(type = "service", invoke = "capturePaymentsByInvoice")
        public static String capturePaymentsByInvoice(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "BillingAccountPayments",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "BillingAccountPayments")
        public interface BillingAccountPayments {}

        @Request(
            uri = "BillingAccountOrders",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "BillingAccountOrders")
        public interface BillingAccountOrders {}

        @Request(
            uri = "createPaymentAndAssociateToBillingAccount",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "BillingAccountPayments")
        @Event(type = "service", invoke = "createPaymentAndApplication")
        public static String createPaymentAndAssociateToBillingAccount(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "AssignGlAccount",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "AssignGlAccount")
        public interface AssignGlAccount {}

        @Request(
            uri = "GlAccountNavigate",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "GlAccountNavigate")
        public interface GlAccountNavigate {}

        @Request(
            uri = "getGlAccountExtendedData",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "request", value = "json")
        @Response(name = "error", type = "request", value = "json")
        @Event(type = "service", invoke = "getGlAccountAndAssocs")
        public static String getGlAccountExtendedData(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "FindGlobalGlAccount",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "FindGlobalGlAccount")
        public interface FindGlobalGlAccount {}

        @Request(
            uri = "ListGlAccountsReport",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ListGlAccountsReport")
        public interface ListGlAccountsReport {}

        @Request(
            uri = "ListGlAccountsExport",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ListGlAccountsExport")
        public interface ListGlAccountsExport {}

        @Request(
            uri = "AddGlAccount",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "AddGlAccount")
        public interface AddGlAccount {}

        @Request(
            uri = "EditGlobalGlAccount",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditGlobalGlAccount")
        public interface EditGlobalGlAccount {}

        @Request(
            uri = "createGlAccount",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "GlAccountNavigate")
        @Response(name = "error", type = "view", value = "EditGlobalGlAccount")
        @Event(type = "service", invoke = "createGlAccount")
        public static String createGlAccount(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "updateGlAccount",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "GlAccountNavigate")
        @Response(name = "error", type = "view", value = "EditGlobalGlAccount")
        @Event(type = "service", invoke = "updateGlAccount")
        public static String updateGlAccount(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "ListGlAccountOrganization",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ListGlAccountOrganization")
        public interface ListGlAccountOrganization {}

        @Request(
            uri = "ListGlAccountOrgPdf.pdf",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ListGlAccountOrgPdf")
        public interface ListGlAccountOrgPdfPdf {}

        @Request(
            uri = "ListGlAccountOrgCsv.csv",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ListGlAccountOrgCsv")
        public interface ListGlAccountOrgCsvCsv {}

        @Request(
            uri = "createGlAccountOrganization",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ListGlAccountOrganization")
        @Response(name = "error", type = "view", value = "ListGlAccountOrganization")
        @Event(type = "service", invoke = "createGlAccountOrganization")
        public static String createGlAccountOrganization(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

    }

    // Auto-generated split (Part 17)
    public static class Part17 {
        @Request(
            uri = "updateGlAccountOrganization",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ListGlAccountOrganization")
        @Response(name = "error", type = "view", value = "ListGlAccountOrganization")
        @Event(type = "service", invoke = "updateGlAccount")
        public static String updateGlAccountOrganization(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "editPartyGlAccounts",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditPartyGlAccount")
        public interface EditPartyGlAccounts {}

        @Request(
            uri = "createPartyGlAccount",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditPartyGlAccount")
        @Response(name = "error", type = "view", value = "EditPartyGlAccount")
        @Event(type = "service", invoke = "createPartyGlAccount")
        public static String createPartyGlAccount(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "updatePartyGlAccount",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditPartyGlAccount")
        @Response(name = "error", type = "view", value = "EditPartyGlAccount")
        @Event(type = "service", invoke = "updatePartyGlAccount")
        public static String updatePartyGlAccount(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "deletePartyGlAccount",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditPartyGlAccount")
        @Response(name = "error", type = "view", value = "EditPartyGlAccount")
        @Event(type = "service", invoke = "deletePartyGlAccount")
        public static String deletePartyGlAccount(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "acctgTransDetailReportPdf.pdf",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "AcctgTransDetailReportPdf")
        public interface AcctgTransDetailReportPdfPdf {}

        @Request(
            uri = "GlAccountTrialBalance",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "GlAccountTrialBalance")
        public interface GlAccountTrialBalance {}

        @Request(
            uri = "GlAccountTrialBalanceReportPdf.pdf",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "GlAccountTrialBalanceReportPdf")
        public interface GlAccountTrialBalanceReportPdfPdf {}

        @Request(
            uri = "FindGlAccountCategory",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "FindGlAccountCategory")
        public interface FindGlAccountCategory {}

        @Request(
            uri = "EditGlAccountCategory",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditGlAccountCategory")
        public interface EditGlAccountCategory {}

        @Request(
            uri = "createGlAccountCategory",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditGlAccountCategory")
        @Event(type = "service", invoke = "createGlAccountCategory")
        public static String createGlAccountCategory(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "updateGlAccountCategory",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditGlAccountCategory")
        @Event(type = "service", invoke = "updateGlAccountCategory")
        public static String updateGlAccountCategory(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "EditGlAccountCategoryMember",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditGlAccountCategoryMember")
        public interface EditGlAccountCategoryMember {}

        @Request(
            uri = "updateGlAccountCategoryMember",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "request-redirect", value = "EditGlAccountCategoryMember")
        @Event(type = "service", invoke = "updateGlAccountCategoryMember")
        public static String updateGlAccountCategoryMember(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "createGlAccountCategoryMember",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "request-redirect", value = "EditGlAccountCategoryMember")
        @Event(type = "service", invoke = "createGlAccountCategoryMember")
        public static String createGlAccountCategoryMember(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "deleteGlAccountCategoryMember",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditGlAccountCategoryMember")
        @Event(type = "service", invoke = "deleteGlAccountCategoryMember")
        public static String deleteGlAccountCategoryMember(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "findGlAccountReconciliation",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "FindGlAccountReconciliation")
        @Response(name = "error", type = "view", value = "FindGlAccountReconciliation")
        public interface FindGlAccountReconciliation {}

        @Request(
            uri = "EditGlReconciliation",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditGlReconciliation")
        @Response(name = "error", type = "view", value = "FindGlAccountReconciliation")
        public static String editGlReconciliation(HttpServletRequest request, HttpServletResponse response) throws Exception {
            // Delegates to: org.ofbiz.accounting.GlEvents.createReconcileAccount
            return GlEvents.createReconcileAccount(request, response);
        }

        @Request(
            uri = "EditGlReconciliations",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditGlReconciliation")
        public interface EditGlReconciliations {}

        @Request(
            uri = "updateGlReconciliation",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditGlReconciliation")
        @Event(type = "service", invoke = "updateGlReconciliation")
        public static String updateGlReconciliation(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

    }

    // Auto-generated split (Part 18)
    public static class Part18 {
        @Request(
            uri = "findGlAccountReconciliations",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "FindGlAccountReconciliations")
        @Response(name = "error", type = "view", value = "FindGlAccountReconciliations")
        public interface FindGlAccountReconciliations {}

        @Request(
            uri = "cancelReconciliation",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "FindFinAccountReconciliations")
        @Response(name = "error", type = "view", value = "FindFinAccountReconciliations")
        @Event(type = "service", invoke = "cancelBankReconciliation")
        public static String cancelReconciliation(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "addtax",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "invoiceOverview")
        @Response(name = "error", type = "view", value = "invoiceOverview")
        @Event(type = "service", invoke = "addtax")
        public static String addtax(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "CommissionRun",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "CommissionRun")
        public interface CommissionRun {}

        @Request(
            uri = "processCommissionRun",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "CommissionRun")
        @Event(type = "service", invoke = "createCommissionInvoices")
        public static String processCommissionRun(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "PartyAccountsSummary",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "PartyAccountsSummary")
        public interface PartyAccountsSummary {}

        @Request(
            uri = "quickCreateAcctgTransAndEntries",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditAcctgTrans")
        @Response(name = "error", type = "view", value = "CreateAcctgTransAndEntries")
        @Event(type = "service", invoke = "quickCreateAcctgTransAndEntries")
        public static String quickCreateAcctgTransAndEntries(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "FindAcctgTrans",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "FindAcctgTrans")
        public interface FindAcctgTrans {}

        @Request(
            uri = "CreateAcctgTransAndEntries",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "CreateAcctgTransAndEntries")
        public interface CreateAcctgTransAndEntries {}

        @Request(
            uri = "EditAcctgTrans",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditAcctgTrans", saveCurrentView = "true")
        public interface EditAcctgTrans {}

        @Request(
            uri = "ListUnpostedAcctgTrans",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ListUnpostedAcctgTrans")
        public interface ListUnpostedAcctgTrans {}

        @Request(
            uri = "completeAcctgTransEntries",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditAcctgTrans")
        @Response(name = "error", type = "view", value = "EditAcctgTrans")
        @Event(type = "service", invoke = "completeAcctgTransEntries")
        public static String completeAcctgTransEntries(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "postAcctgTrans",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditAcctgTrans")
        @Response(name = "error", type = "view", value = "EditAcctgTrans")
        @Event(type = "service", invoke = "postAcctgTrans")
        public static String postAcctgTrans(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "updateAcctgTrans",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditAcctgTrans")
        @Response(name = "error", type = "view", value = "EditAcctgTrans")
        @Event(type = "service", invoke = "updateAcctgTrans")
        public static String updateAcctgTrans(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "createAcctgTransEntry",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditAcctgTrans")
        @Response(name = "error", type = "view", value = "EditAcctgTrans")
        @Event(type = "service", invoke = "createAcctgTransEntry")
        public static String createAcctgTransEntry(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "FindAcctgTransEntries",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "FindAcctgTransEntries")
        public interface FindAcctgTransEntries {}

        @Request(
            uri = "updateAcctgTransEntry",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view-last", value = "EditAcctgTrans")
        @Response(name = "error", type = "view", value = "EditAcctgTrans")
        @Event(type = "service", invoke = "updateAcctgTransEntry")
        public static String updateAcctgTransEntry(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "deleteAcctgTransEntry",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditAcctgTrans")
        @Response(name = "error", type = "view", value = "EditAcctgTrans")
        @Event(type = "service", invoke = "deleteAcctgTransEntry")
        public static String deleteAcctgTransEntry(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "newAcctgTrans",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "NewAcctgTrans")
        public interface NewAcctgTrans {}

        @Request(
            uri = "createAcctgTrans",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditAcctgTrans")
        @Response(name = "error", type = "view", value = "NewAcctgTrans")
        @Event(type = "service", invoke = "createAcctgTrans")
        public static String createAcctgTrans(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

    }

    // Auto-generated split (Part 19)
    public static class Part19 {
        @Request(
            uri = "copyAcctgTransAndEntries",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditAcctgTrans")
        @Response(name = "error", type = "view", value = "EditAcctgTrans")
        @Event(type = "service", invoke = "copyAcctgTransAndEntries")
        public static String copyAcctgTransAndEntries(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "CostCenters",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "CostCenters")
        public interface CostCenters {}

        @Request(
            uri = "createUpdateCostCenter",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "CostCenters")
        @Response(name = "error", type = "view", value = "CostCenters")
        @Event(type = "service-multi", invoke = "createUpdateCostCenter")
        public static String createUpdateCostCenter(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "GlAccountBalanceByCostCenter",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "GlAccountBalanceByCostCenter")
        public interface GlAccountBalanceByCostCenter {}

        @Request(
            uri = "GlAccountBalanceByCostCenter.pdf",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "GlAccountBalanceByCostCenterPdf")
        public interface GlAccountBalanceByCostCenterPdf {}

        @Request(
            uri = "ListBudgets",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ListBudgets")
        public interface ListBudgets {}

        @Request(
            uri = "BudgetSearchResults",
            controller = "accounting",
            secure = "true"
        )
        @Response(name = "success", type = "view", value = "BudgetSearchResults")
        public interface BudgetSearchResults {}

        @Request(
            uri = "EditBudget",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditBudget")
        public interface EditBudget {}

        @Request(
            uri = "BudgetOverview",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "BudgetOverview")
        public interface BudgetOverview {}

        @Request(
            uri = "EditBudgetItems",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditBudgetItems")
        public interface EditBudgetItems {}

        @Request(
            uri = "BudgetRoles",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "BudgetRoles")
        public interface BudgetRoles {}

        @Request(
            uri = "BudgetReviews",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "BudgetReviews")
        public interface BudgetReviews {}

        @Request(
            uri = "createBudget",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditBudget")
        @Response(name = "error", type = "view", value = "EditBudget")
        @Event(type = "service", invoke = "createBudget")
        public static String createBudget(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "updateBudget",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditBudget")
        @Response(name = "error", type = "view", value = "EditBudget")
        @Event(type = "service", invoke = "updateBudget")
        public static String updateBudget(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "updateBudgetStatus",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "BudgetOverview")
        @Response(name = "error", type = "view", value = "BudgetOverview")
        @Event(type = "service", invoke = "updateBudgetStatus")
        public static String updateBudgetStatus(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "createBudgetItem",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditBudgetItems")
        @Response(name = "error", type = "view", value = "EditBudgetItems")
        @Event(type = "service", invoke = "createBudgetItem")
        public static String createBudgetItem(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "updateBudgetItem",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "request-redirect", value = "EditBudgetItems")
        @Response(name = "error", type = "view", value = "EditBudgetItems")
        @Event(type = "service-multi", invoke = "updateBudgetItem")
        public static String updateBudgetItem(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "removeBudgetItem",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditBudgetItems")
        @Response(name = "error", type = "view", value = "EditBudgetItems")
        @Event(type = "service", invoke = "removeBudgetItem")
        public static String removeBudgetItem(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "createBudgetRole",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "BudgetRoles")
        @Response(name = "error", type = "view", value = "BudgetRoles")
        @Event(type = "service", invoke = "createBudgetRole")
        public static String createBudgetRole(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "removeBudgetRole",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "BudgetRoles")
        @Response(name = "error", type = "view", value = "BudgetRoles")
        @Event(type = "service", invoke = "removeBudgetRole")
        public static String removeBudgetRole(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

    }

    // Auto-generated split (Part 20)
    public static class Part20 {
        @Request(
            uri = "createBudgetReview",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "BudgetReviews")
        @Response(name = "error", type = "view", value = "BudgetReviews")
        @Event(type = "service", invoke = "createBudgetReview")
        public static String createBudgetReview(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "removeBudgetReview",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "BudgetReviews")
        @Response(name = "error", type = "view", value = "BudgetReviews")
        @Event(type = "service", invoke = "removeBudgetReview")
        public static String removeBudgetReview(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "FindCommissions",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "CommissionReport")
        public interface FindCommissions {}

        @Request(
            uri = "CommissionReport.pdf",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "CommissionReportPdf")
        public interface CommissionReportPdf {}

        @Request(
            uri = "FindAgreement",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "FindAgreement")
        public interface FindAgreement {}

        @Request(
            uri = "cancelAgreement",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "FindAgreement")
        @Response(name = "error", type = "view", value = "FindAgreement")
        @Event(type = "service", invoke = "cancelAgreement")
        public static String cancelAgreement(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "EditAgreement",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditAgreement")
        public interface EditAgreement {}

        @Request(
            uri = "createAgreement",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditAgreement")
        @Response(name = "error", type = "view", value = "EditAgreement")
        @Event(type = "service", invoke = "createAgreement")
        public static String createAgreement(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "updateAgreement",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditAgreement")
        @Response(name = "error", type = "view", value = "EditAgreement")
        @Event(type = "service", invoke = "updateAgreement")
        public static String updateAgreement(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "copyAgreement",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditAgreement")
        @Response(name = "error", type = "view", value = "EditAgreement")
        @Event(type = "service", invoke = "copyAgreement")
        public static String copyAgreement(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "ListAgreementItems",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ListAgreementItems")
        public interface ListAgreementItems {}

        @Request(
            uri = "removeAgreementItem",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ListAgreementItems")
        @Response(name = "error", type = "view", value = "ListAgreementItems")
        @Event(type = "service", invoke = "removeAgreementItem")
        public static String removeAgreementItem(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "EditAgreementItem",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditAgreementItem")
        public interface EditAgreementItem {}

        @Request(
            uri = "createAgreementItem",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditAgreementItem")
        @Response(name = "error", type = "view", value = "EditAgreementItem")
        @Event(type = "service", invoke = "createAgreementItem")
        public static String createAgreementItem(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "updateAgreementItem",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditAgreementItem")
        @Response(name = "error", type = "view", value = "EditAgreementItem")
        @Event(type = "service", invoke = "updateAgreementItem")
        public static String updateAgreementItem(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "EditAgreementTerms",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditAgreementTerms")
        public interface EditAgreementTerms {}

        @Request(
            uri = "createAgreementTerm",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditAgreementTerms")
        @Response(name = "error", type = "view", value = "EditAgreementTerms")
        @Event(type = "service", invoke = "createAgreementTerm")
        public static String createAgreementTerm(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "updateAgreementTerm",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditAgreementTerms")
        @Response(name = "error", type = "view", value = "EditAgreementTerms")
        @Event(type = "service", invoke = "updateAgreementTerm")
        public static String updateAgreementTerm(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "deleteAgreementTerm",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditAgreementTerms")
        @Response(name = "error", type = "view", value = "EditAgreementTerms")
        @Event(type = "service", invoke = "deleteAgreementTerm")
        public static String deleteAgreementTerm(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "ListAgreementPromoAppls",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ListAgreementPromoAppls")
        public interface ListAgreementPromoAppls {}

    }

    // Auto-generated split (Part 21)
    public static class Part21 {
        @Request(
            uri = "removeAgreementPromoAppl",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ListAgreementPromoAppls")
        @Response(name = "error", type = "view", value = "ListAgreementPromoAppls")
        @Event(type = "service", invoke = "removeAgreementPromoAppl")
        public static String removeAgreementPromoAppl(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "EditAgreementPromoAppl",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditAgreementPromoAppl")
        public interface EditAgreementPromoAppl {}

        @Request(
            uri = "createAgreementPromoAppl",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditAgreementPromoAppl")
        @Response(name = "error", type = "view", value = "EditAgreementPromoAppl")
        @Event(type = "service", invoke = "createAgreementPromoAppl")
        public static String createAgreementPromoAppl(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "updateAgreementPromoAppl",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditAgreementPromoAppl")
        @Response(name = "error", type = "view", value = "EditAgreementPromoAppl")
        @Event(type = "service", invoke = "updateAgreementPromoAppl")
        public static String updateAgreementPromoAppl(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "ListAgreementItemTerms",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ListAgreementItemTerms")
        public interface ListAgreementItemTerms {}

        @Request(
            uri = "removeAgreementItemTerm",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ListAgreementItemTerms")
        @Response(name = "error", type = "view", value = "ListAgreementItemTerms")
        @Event(type = "service", invoke = "deleteAgreementTerm")
        public static String removeAgreementItemTerm(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "EditAgreementItemTerm",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditAgreementItemTerm")
        public interface EditAgreementItemTerm {}

        @Request(
            uri = "createAgreementItemTerm",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditAgreementItemTerm")
        @Response(name = "error", type = "view", value = "EditAgreementItemTerm")
        @Event(type = "service", invoke = "createAgreementTerm")
        public static String createAgreementItemTerm(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "updateAgreementItemTerm",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditAgreementItemTerm")
        @Response(name = "error", type = "view", value = "EditAgreementItemTerm")
        @Event(type = "service", invoke = "updateAgreementTerm")
        public static String updateAgreementItemTerm(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "ListAgreementItemProducts",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ListAgreementItemProducts")
        public interface ListAgreementItemProducts {}

        @Request(
            uri = "ListAgreementItemProductsReport",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ListAgreementItemProductsReport")
        public interface ListAgreementItemProductsReport {}

        @Request(
            uri = "removeAgreementItemProduct",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ListAgreementItemProducts")
        @Response(name = "error", type = "view", value = "ListAgreementItemProducts")
        @Event(type = "service", invoke = "removeAgreementProductAppl")
        public static String removeAgreementItemProduct(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "EditAgreementItemProduct",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditAgreementItemProduct")
        public interface EditAgreementItemProduct {}

        @Request(
            uri = "createAgreementItemProduct",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ListAgreementItemProducts")
        @Response(name = "error", type = "view", value = "EditAgreementItemProduct")
        @Event(type = "service", invoke = "createAgreementProductAppl")
        public static String createAgreementItemProduct(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "updateAgreementItemProduct",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ListAgreementItemProducts")
        @Response(name = "error", type = "view", value = "EditAgreementItemProduct")
        @Event(type = "service", invoke = "updateAgreementProductAppl")
        public static String updateAgreementItemProduct(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "ListAgreementItemFacilities",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ListAgreementItemFacilities")
        public interface ListAgreementItemFacilities {}

        @Request(
            uri = "ListAgreementItemFacilitiesReport",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ListAgreementItemFacilitiesReport")
        public interface ListAgreementItemFacilitiesReport {}

        @Request(
            uri = "removeAgreementItemFacility",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ListAgreementItemFacilities")
        @Response(name = "error", type = "view", value = "ListAgreementItemFacilities")
        @Event(type = "service", invoke = "removeAgreementFacilityAppl")
        public static String removeAgreementItemFacility(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "EditAgreementItemFacility",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditAgreementItemFacility")
        public interface EditAgreementItemFacility {}

        @Request(
            uri = "createAgreementItemFacility",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ListAgreementItemFacilities")
        @Response(name = "error", type = "view", value = "EditAgreementItemFacility")
        @Event(type = "service", invoke = "createAgreementFacilityAppl")
        public static String createAgreementItemFacility(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

    }

    // Auto-generated split (Part 22)
    public static class Part22 {
        @Request(
            uri = "updateAgreementItemFacility",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ListAgreementItemFacilities")
        @Response(name = "error", type = "view", value = "EditAgreementItemFacility")
        @Event(type = "service", invoke = "updateAgreementFacilityAppl")
        public static String updateAgreementItemFacility(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "ListAgreementItemSupplierProducts",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ListAgreementItemSupplierProducts")
        public interface ListAgreementItemSupplierProducts {}

        @Request(
            uri = "ListAgreementItemSupplierProductsReport",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ListAgreementItemSupplierProductsReport")
        public interface ListAgreementItemSupplierProductsReport {}

        @Request(
            uri = "removeAgreementItemSupplierProduct",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ListAgreementItemSupplierProducts")
        @Response(name = "error", type = "view", value = "ListAgreementItemSupplierProducts")
        @Event(type = "service", invoke = "removeSupplierProduct")
        public static String removeAgreementItemSupplierProduct(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "EditAgreementItemSupplierProduct",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditAgreementItemSupplierProduct")
        public interface EditAgreementItemSupplierProduct {}

        @Request(
            uri = "createAgreementItemSupplierProduct",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ListAgreementItemSupplierProducts")
        @Response(name = "error", type = "view", value = "EditAgreementItemSupplierProduct")
        @Event(type = "service", invoke = "createSupplierProduct")
        public static String createAgreementItemSupplierProduct(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "updateAgreementItemSupplierProduct",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ListAgreementItemSupplierProducts")
        @Response(name = "error", type = "view", value = "EditAgreementItemSupplierProduct")
        @Event(type = "service", invoke = "updateSupplierProduct")
        public static String updateAgreementItemSupplierProduct(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "ListAgreementItemParties",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ListAgreementItemParties")
        public interface ListAgreementItemParties {}

        @Request(
            uri = "removeAgreementItemParty",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ListAgreementItemParties")
        @Response(name = "error", type = "view", value = "ListAgreementItemParties")
        @Event(type = "service", invoke = "removeAgreementPartyApplic")
        public static String removeAgreementItemParty(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "EditAgreementItemParty",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditAgreementItemParty")
        public interface EditAgreementItemParty {}

        @Request(
            uri = "createAgreementItemParty",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditAgreementItemParty")
        @Response(name = "error", type = "view", value = "EditAgreementItemParty")
        @Event(type = "service", invoke = "createAgreementPartyApplic")
        public static String createAgreementItemParty(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "updateAgreementItemParty",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditAgreementItemParty")
        @Response(name = "error", type = "view", value = "EditAgreementItemParty")
        @Event(type = "service", invoke = "updateAgreementPartyApplic")
        public static String updateAgreementItemParty(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "ListAgreementGeographicalApplic",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ListAgreementGeographicalApplic")
        public interface ListAgreementGeographicalApplic {}

        @Request(
            uri = "removeAgreementGeographicalApplic",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ListAgreementGeographicalApplic")
        @Response(name = "error", type = "view", value = "ListAgreementGeographicalApplic")
        @Event(type = "service", invoke = "removeAgreementGeographicalApplic")
        public static String removeAgreementGeographicalApplic(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "EditAgreementGeographicalApplic",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditAgreementGeographicalApplic")
        public interface EditAgreementGeographicalApplic {}

        @Request(
            uri = "createAgreementGeographicalApplic",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ListAgreementGeographicalApplic")
        @Response(name = "error", type = "view", value = "EditAgreementGeographicalApplic")
        @Event(type = "service", invoke = "createAgreementGeographicalApplic")
        public static String createAgreementGeographicalApplic(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "EditAgreementWorkEffortApplics",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditAgreementWorkEffortApplics")
        public interface EditAgreementWorkEffortApplics {}

        @Request(
            uri = "createAgreementWorkEffortApplic",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditAgreementWorkEffortApplics")
        @Response(name = "error", type = "view", value = "EditAgreementWorkEffortApplics")
        @Event(type = "service", invoke = "createAgreementWorkEffortApplic")
        public static String createAgreementWorkEffortApplic(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "deleteAgreementWorkEffortApplic",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditAgreementWorkEffortApplics")
        @Response(name = "error", type = "view", value = "EditAgreementWorkEffortApplics")
        @Event(type = "service", invoke = "deleteAgreementWorkEffortApplic")
        public static String deleteAgreementWorkEffortApplic(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "EditAgreementRoles",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditAgreementRoles")
        public interface EditAgreementRoles {}

    }

    // Auto-generated split (Part 23)
    public static class Part23 {
        @Request(
            uri = "createAgreementRole",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditAgreementRoles")
        @Response(name = "error", type = "view", value = "EditAgreementRoles")
        @Event(type = "service", invoke = "createAgreementRole")
        public static String createAgreementRole(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "updateAgreementRole",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditAgreementRoles")
        @Response(name = "error", type = "view", value = "EditAgreementRoles")
        @Event(type = "service", invoke = "updateAgreementRole")
        public static String updateAgreementRole(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "deleteAgreementRole",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditAgreementRoles")
        @Response(name = "error", type = "view", value = "EditAgreementRoles")
        @Event(type = "service", invoke = "deleteAgreementRole")
        public static String deleteAgreementRole(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "findInvoices",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "findInvoices")
        public interface FindInvoices {}

        @Request(
            uri = "invoiceOverview",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "invoiceOverview")
        public interface InvoiceOverview {}

        @Request(
            uri = "newInvoice",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "newInvoice")
        public interface NewInvoice {}

        @Request(
            uri = "editInvoice",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "editInvoice")
        public interface EditInvoice {}

        @Request(
            uri = "createInvoice",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "request", value = "editInvoice")
        @Response(name = "error", type = "view", value = "newInvoice")
        @Event(type = "service", invoke = "createInvoice")
        public static String createInvoice(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "copyInvoice",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "invoiceOverview")
        @Response(name = "error", type = "view", value = "invoiceOverview")
        @Event(type = "service", invoke = "copyInvoice")
        public static String copyInvoice(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "updateInvoice",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "editInvoice")
        @Response(name = "error", type = "view", value = "editInvoice")
        @Event(type = "service", invoke = "updateInvoice")
        public static String updateInvoice(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "listInvoiceItems",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "listInvoiceItems")
        public interface ListInvoiceItems {}

        @Request(
            uri = "createInvoiceItem",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "listInvoiceItems")
        @Response(name = "error", type = "view", value = "listInvoiceItems")
        @Event(type = "service", invoke = "createInvoiceItem")
        public static String createInvoiceItem(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "createInvoiceItemPayrol",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "listInvoiceItems")
        @Response(name = "error", type = "view", value = "listInvoiceItems")
        @Event(type = "simple", path = "component://accounting/script/org/ofbiz/accounting/invoice/InvoiceEvents.xml", invoke = "createInvoiceItemPayrol")
        public static String createInvoiceItemPayrol(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "updateInvoiceItem",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "request-redirect", value = "listInvoiceItems")
        @Response(name = "error", type = "view", value = "listInvoiceItems")
        @Event(type = "service-multi", invoke = "updateInvoiceItem")
        public static String updateInvoiceItem(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "removeInvoiceItem",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "listInvoiceItems")
        @Response(name = "error", type = "view", value = "listInvoiceItems")
        @Event(type = "service", invoke = "removeInvoiceItem")
        public static String removeInvoiceItem(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "editInvoiceTimeEntries",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "editInvoiceTimeEntries")
        public interface EditInvoiceTimeEntries {}

        @Request(
            uri = "unlinkInvoiceFromTimeEntry",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "editInvoiceTimeEntries")
        @Response(name = "error", type = "view", value = "editInvoiceTimeEntries")
        @Event(type = "service", invoke = "unlinkInvoiceFromTimeEntry")
        public static String unlinkInvoiceFromTimeEntry(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "editInvoiceApplications",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "editInvoiceApplications")
        public interface EditInvoiceApplications {}

        @Request(
            uri = "updateInvoiceApplication",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "editInvoiceApplications")
        @Response(name = "error", type = "view", value = "editInvoiceApplications")
        @Event(type = "service", invoke = "updatePaymentApplicationDef")
        public static String updateInvoiceApplication(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "removeInvoiceApplication",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "editInvoiceApplications")
        @Response(name = "error", type = "view", value = "editInvoiceApplications")
        @Event(type = "service", invoke = "removePaymentApplication")
        public static String removeInvoiceApplication(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

    }

    // Auto-generated split (Part 24)
    public static class Part24 {
        @Request(
            uri = "invoiceRoles",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "invoiceRoles")
        public interface InvoiceRoles {}

        @Request(
            uri = "createInvoiceRole",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "invoiceRoles")
        @Response(name = "error", type = "view", value = "invoiceRoles")
        @Event(type = "service", invoke = "createInvoiceRole")
        public static String createInvoiceRole(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "removeInvoiceRole",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "invoiceRoles")
        @Response(name = "error", type = "view", value = "invoiceRoles")
        @Event(type = "service", invoke = "removeInvoiceRole")
        public static String removeInvoiceRole(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "setInvoiceStatus",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "invoiceOverview")
        @Response(name = "error", type = "view", value = "invoiceOverview")
        @Event(type = "service", invoke = "setInvoiceStatus")
        public static String setInvoiceStatus(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "invoiceTerms",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "invoiceTerms")
        public interface InvoiceTerms {}

        @Request(
            uri = "createInvoiceTerm",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "invoiceTerms")
        @Response(name = "error", type = "view", value = "invoiceTerms")
        @Event(type = "service", invoke = "createInvoiceTerm")
        public static String createInvoiceTerm(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "sendPerEmail",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "sendPerEmail")
        public interface SendPerEmail {}

        @Request(
            uri = "executeSendPerEmail",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "invoiceOverview")
        @Response(name = "error", type = "view", value = "invoiceOverview")
        @Event(type = "service", invoke = "sendInvoicePerEmail")
        public static String executeSendPerEmail(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "getInvoiceRunningTotal",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "request", value = "json")
        @Response(name = "error", type = "request", value = "json")
        @Event(type = "service", invoke = "getInvoiceRunningTotal")
        public static String getInvoiceRunningTotal(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "editInvoiceItemType",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditInvoiceItemType")
        public interface EditInvoiceItemType {}

        @Request(
            uri = "updateInvoiceItemType",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditInvoiceItemType")
        @Response(name = "error", type = "view", value = "EditInvoiceItemType")
        @Event(type = "service", invoke = "updateInvoiceItemType")
        public static String updateInvoiceItemType(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "ListFixedAssets",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ListFixedAssets")
        public interface ListFixedAssets {}

        @Request(
            uri = "FixedAssetSearchResults",
            controller = "accounting",
            secure = "true"
        )
        @Response(name = "success", type = "view", value = "FixedAssetSearchResults")
        public interface FixedAssetSearchResults {}

        @Request(
            uri = "EditFixedAsset",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditFixedAsset")
        public interface EditFixedAsset {}

        @Request(
            uri = "createFixedAsset",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditFixedAsset")
        @Response(name = "error", type = "view", value = "EditFixedAsset")
        @Event(type = "service", invoke = "createFixedAsset")
        public static String createFixedAsset(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "updateFixedAsset",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditFixedAsset")
        @Response(name = "error", type = "view", value = "EditFixedAsset")
        @Event(type = "service", invoke = "updateFixedAsset")
        public static String updateFixedAsset(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "ListFixedAssetProducts",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ListFixedAssetProducts")
        public interface ListFixedAssetProducts {}

        @Request(
            uri = "addFixedAssetProduct",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ListFixedAssetProducts")
        @Response(name = "error", type = "view", value = "ListFixedAssetProducts")
        @Event(type = "service", invoke = "addFixedAssetProduct")
        public static String addFixedAssetProduct(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "updateFixedAssetProduct",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ListFixedAssetProducts")
        @Response(name = "error", type = "view", value = "ListFixedAssetProducts")
        @Event(type = "service", invoke = "updateFixedAssetProduct")
        public static String updateFixedAssetProduct(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "removeFixedAssetProduct",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ListFixedAssetProducts")
        @Response(name = "error", type = "view", value = "ListFixedAssetProducts")
        @Event(type = "service", invoke = "removeFixedAssetProduct")
        public static String removeFixedAssetProduct(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

    }

    // Auto-generated split (Part 25)
    public static class Part25 {
        @Request(
            uri = "FixedAssetChildren",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "FixedAssetChildren")
        public interface FixedAssetChildren {}

        @Request(
            uri = "EditFixedAssetStdCosts",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditFixedAssetStdCosts")
        public interface EditFixedAssetStdCosts {}

        @Request(
            uri = "createFixedAssetStdCost",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditFixedAssetStdCosts")
        @Event(type = "service", invoke = "createFixedAssetStdCost")
        public static String createFixedAssetStdCost(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "updateFixedAssetStdCost",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditFixedAssetStdCosts")
        @Event(type = "service", invoke = "updateFixedAssetStdCost")
        public static String updateFixedAssetStdCost(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "cancelFixedAssetStdCost",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditFixedAssetStdCosts")
        @Event(type = "service", invoke = "cancelFixedAssetStdCost")
        public static String cancelFixedAssetStdCost(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "EditFixedAssetIdents",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditFixedAssetIdents")
        public interface EditFixedAssetIdents {}

        @Request(
            uri = "createFixedAssetIdent",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditFixedAssetIdents")
        @Response(name = "error", type = "view", value = "EditFixedAssetIdents")
        @Event(type = "service", invoke = "createFixedAssetIdent")
        public static String createFixedAssetIdent(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "updateFixedAssetIdent",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditFixedAssetIdents")
        @Response(name = "error", type = "view", value = "EditFixedAssetIdents")
        @Event(type = "service", invoke = "updateFixedAssetIdent")
        public static String updateFixedAssetIdent(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "removeFixedAssetIdent",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditFixedAssetIdents")
        @Response(name = "error", type = "view", value = "EditFixedAssetIdents")
        @Event(type = "service", invoke = "removeFixedAssetIdent")
        public static String removeFixedAssetIdent(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "EditFixedAssetRegistrations",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditFixedAssetRegistrations")
        public interface EditFixedAssetRegistrations {}

        @Request(
            uri = "createFixedAssetRegistration",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditFixedAssetRegistrations")
        @Response(name = "error", type = "view", value = "EditFixedAssetRegistrations")
        @Event(type = "service", invoke = "createFixedAssetRegistration")
        public static String createFixedAssetRegistration(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "updateFixedAssetRegistration",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditFixedAssetRegistrations")
        @Response(name = "error", type = "view", value = "EditFixedAssetRegistrations")
        @Event(type = "service", invoke = "updateFixedAssetRegistration")
        public static String updateFixedAssetRegistration(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "deleteFixedAssetRegistration",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditFixedAssetRegistrations")
        @Response(name = "error", type = "view", value = "EditFixedAssetRegistrations")
        @Event(type = "service", invoke = "deleteFixedAssetRegistration")
        public static String deleteFixedAssetRegistration(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "EditFixedAssetMeters",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditFixedAssetMeters")
        public interface EditFixedAssetMeters {}

        @Request(
            uri = "createFixedAssetMeter",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditFixedAssetMeters")
        @Response(name = "error", type = "view", value = "EditFixedAssetMeters")
        @Event(type = "service", invoke = "createFixedAssetMeter")
        public static String createFixedAssetMeter(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "updateFixedAssetMeter",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditFixedAssetMeters")
        @Response(name = "error", type = "view", value = "EditFixedAssetMeters")
        @Event(type = "service", invoke = "updateFixedAssetMeter")
        public static String updateFixedAssetMeter(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "deleteFixedAssetMeter",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditFixedAssetMeters")
        @Response(name = "error", type = "view", value = "EditFixedAssetMeters")
        @Event(type = "service", invoke = "deleteFixedAssetMeter")
        public static String deleteFixedAssetMeter(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "ListFixedAssetMaints",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ListFixedAssetMaints")
        public interface ListFixedAssetMaints {}

        @Request(
            uri = "EditFixedAssetMaint",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditFixedAssetMaint")
        @Response(name = "error", type = "view", value = "EditFixedAssetMaint")
        public interface EditFixedAssetMaint {}

        @Request(
            uri = "createFixedAssetMaint",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditFixedAssetMaint")
        @Response(name = "error", type = "view", value = "EditFixedAssetMaint")
        @Event(type = "service", invoke = "createFixedAssetMaint")
        public static String createFixedAssetMaint(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

    }

    // Auto-generated split (Part 26)
    public static class Part26 {
        @Request(
            uri = "updateFixedAssetMaint",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditFixedAssetMaint")
        @Response(name = "error", type = "view", value = "EditFixedAssetMaint")
        @Event(type = "service", invoke = "updateFixedAssetMaint")
        public static String updateFixedAssetMaint(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "deleteFixedAssetMaint",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditFixedAssetMaint")
        @Response(name = "error", type = "view", value = "EditFixedAssetMaint")
        @Event(type = "service", invoke = "deleteFixedAssetMaint")
        public static String deleteFixedAssetMaint(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "LookupWorkEffort",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "LookupWorkEffort")
        public interface LookupWorkEffort {}

        @Request(
            uri = "LookupOrderHeader",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "LookupOrderHeader")
        public interface LookupOrderHeader {}

        @Request(
            uri = "calendar",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "calendar")
        public interface Calendar {}

        @Request(
            uri = "EditWorkEffort",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditFixedAssetMaint")
        public interface EditWorkEffort {}

        @Request(
            uri = "WorkEffortSummary",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "WorkEffortSummary")
        public interface WorkEffortSummary {}

        @Request(
            uri = "EditFixedAssetMaintOrders",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditFixedAssetMaintOrders")
        public interface EditFixedAssetMaintOrders {}

        @Request(
            uri = "createFixedAssetMaintOrder",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditFixedAssetMaintOrders")
        @Response(name = "error", type = "view", value = "EditFixedAssetMaintOrders")
        @Event(type = "service", invoke = "createFixedAssetMaintOrder")
        public static String createFixedAssetMaintOrder(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "deleteFixedAssetMaintOrder",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditFixedAssetMaintOrders")
        @Response(name = "error", type = "view", value = "EditFixedAssetMaintOrders")
        @Event(type = "service", invoke = "deleteFixedAssetMaintOrder")
        public static String deleteFixedAssetMaintOrder(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "showFixedAssetDepreciation",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ShowFixedAssetDepreciation")
        @Response(name = "error", type = "view", value = "ShowFixedAssetDepreciation")
        public interface ShowFixedAssetDepreciation {}

        @Request(
            uri = "createFixedAssetDepMethod",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ShowFixedAssetDepreciation")
        @Response(name = "error", type = "view", value = "ShowFixedAssetDepreciation")
        @Event(type = "service", invoke = "createFixedAssetDepMethod")
        public static String createFixedAssetDepMethod(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "deleteFixedAssetDepMethod",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ShowFixedAssetDepreciation")
        @Response(name = "error", type = "view", value = "ShowFixedAssetDepreciation")
        @Event(type = "service", invoke = "deleteFixedAssetDepMethod")
        public static String deleteFixedAssetDepMethod(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "updateFixedAssetDepMethod",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ShowFixedAssetDepreciation")
        @Response(name = "error", type = "view", value = "ShowFixedAssetDepreciation")
        @Event(type = "service", invoke = "updateFixedAssetDepMethod")
        public static String updateFixedAssetDepMethod(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "createFixedAssetTypeGlAccountForFixedAsset",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ShowFixedAssetDepreciation")
        @Response(name = "error", type = "view", value = "ShowFixedAssetDepreciation")
        @Event(type = "service", invoke = "createFixedAssetTypeGlAccount")
        public static String createFixedAssetTypeGlAccountForFixedAsset(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "deleteFixedAssetTypeGlAccountForFixedAsset",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ShowFixedAssetDepreciation")
        @Response(name = "error", type = "view", value = "ShowFixedAssetDepreciation")
        @Event(type = "service", invoke = "deleteFixedAssetTypeGlAccount")
        public static String deleteFixedAssetTypeGlAccountForFixedAsset(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "FixedAssetTypeGlAccounts",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "FixedAssetTypeGlAccounts")
        @Response(name = "error", type = "view", value = "FixedAssetTypeGlAccounts")
        public interface FixedAssetTypeGlAccounts {}

        @Request(
            uri = "createFixedAssetTypeGlAccount",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "FixedAssetTypeGlAccounts")
        @Response(name = "error", type = "view", value = "FixedAssetTypeGlAccounts")
        @Event(type = "service", invoke = "createFixedAssetTypeGlAccount")
        public static String createFixedAssetTypeGlAccount(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "deleteFixedAssetTypeGlAccount",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "FixedAssetTypeGlAccounts")
        @Response(name = "error", type = "view", value = "FixedAssetTypeGlAccounts")
        @Event(type = "service", invoke = "deleteFixedAssetTypeGlAccount")
        public static String deleteFixedAssetTypeGlAccount(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "FixedAssetGeoLocation",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "FixedAssetGeoLocation")
        @Response(name = "error", type = "view", value = "EditFixedAsset")
        public interface FixedAssetGeoLocation {}

    }

    // Auto-generated split (Part 27)
    public static class Part27 {
        @Request(
            uri = "FindFinAccount",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "FindFinAccount")
        public interface FindFinAccount {}

        @Request(
            uri = "EditFinAccount",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditFinAccount")
        public interface EditFinAccount {}

        @Request(
            uri = "createFinAccount",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditFinAccount")
        @Response(name = "error", type = "view", value = "EditFinAccount")
        @Event(type = "service", invoke = "createFinAccount")
        public static String createFinAccount(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "updateFinAccount",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditFinAccount")
        @Response(name = "error", type = "view", value = "EditFinAccount")
        @Event(type = "service", invoke = "updateFinAccount")
        public static String updateFinAccount(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "deleteFinAccount",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "FindFinAccount")
        @Response(name = "error", type = "view", value = "FindFinAccount")
        @Event(type = "service", invoke = "deleteFinAccount")
        public static String deleteFinAccount(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "FinAccountMain",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "FinAccountMain")
        public interface FinAccountMain {}

        @Request(
            uri = "FindDepositSlips",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "FindDepositSlips")
        public interface FindDepositSlips {}

        @Request(
            uri = "updateDepositSlip",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditDepositSlipAndMembers")
        @Response(name = "error", type = "view", value = "EditDepositSlipAndMembers")
        @Event(type = "service", invoke = "updatePaymentGroup")
        public static String updateDepositSlip(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "addDepositSlipMember",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditDepositSlipAndMembers")
        @Response(name = "error", type = "view", value = "EditDepositSlipAndMembers")
        @Event(type = "service", invoke = "createPaymentGroupMember")
        public static String addDepositSlipMember(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "updateDepositSlipMember",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditDepositSlipAndMembers")
        @Response(name = "error", type = "view", value = "EditDepositSlipAndMembers")
        @Event(type = "service", invoke = "updatePaymentGroupMember")
        public static String updateDepositSlipMember(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "expireDepositSlipMember",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditDepositSlipAndMembers")
        @Response(name = "error", type = "view", value = "EditDepositSlipAndMembers")
        @Event(type = "service", invoke = "expirePaymentGroupMember")
        public static String expireDepositSlipMember(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "EditDepositSlipAndMembers",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditDepositSlipAndMembers")
        public interface EditDepositSlipAndMembers {}

        @Request(
            uri = "deleteDepositSlip",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "FindDepositSlips")
        @Response(name = "error", type = "view", value = "FindDepositSlips")
        @Event(type = "service", invoke = "cancelPaymentBatch")
        public static String deleteDepositSlip(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "NewDepositPayment",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "NewDepositPayment")
        public interface NewDepositPayment {}

        @Request(
            uri = "NewWithdrawalPayment",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "NewWithdrawalPayment")
        public interface NewWithdrawalPayment {}

        @Request(
            uri = "createDepositPayment",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "PaymentsDepositWithdraw")
        @Response(name = "error", type = "view", value = "NewDepositPayment")
        @Event(type = "service", invoke = "createPaymentAndFinAccountTrans")
        public static String createDepositPayment(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "createWithdrawalPayment",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "PaymentsDepositWithdraw")
        @Response(name = "error", type = "view", value = "NewWithdrawalPayment")
        @Event(type = "service", invoke = "createPaymentAndFinAccountTrans")
        public static String createWithdrawalPayment(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "EditFinAccountRoles",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditFinAccountRoles")
        public interface EditFinAccountRoles {}

        @Request(
            uri = "createFinAccountRole",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditFinAccountRoles")
        @Response(name = "error", type = "view", value = "EditFinAccountRoles")
        @Event(type = "service", invoke = "createFinAccountRole")
        public static String createFinAccountRole(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "updateFinAccountRole",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditFinAccountRoles")
        @Response(name = "error", type = "view", value = "EditFinAccountRoles")
        @Event(type = "service", invoke = "updateFinAccountRole")
        public static String updateFinAccountRole(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

    }

    // Auto-generated split (Part 28)
    public static class Part28 {
        @Request(
            uri = "deleteFinAccountRole",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditFinAccountRoles")
        @Response(name = "error", type = "view", value = "EditFinAccountRoles")
        @Event(type = "service", invoke = "deleteFinAccountRole")
        public static String deleteFinAccountRole(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "EditFinAccountTrans",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditFinAccountTrans")
        @Response(name = "error", type = "view", value = "EditFinAccountTrans")
        public interface EditFinAccountTrans {}

        @Request(
            uri = "createFinAccountTrans",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "FindFinAccountTrans")
        @Response(name = "error", type = "view", value = "EditFinAccountTrans")
        @Event(type = "service", invoke = "createFinAccountTrans")
        public static String createFinAccountTrans(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "FindFinAccountTrans",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "FindFinAccountTrans")
        public interface FindFinAccountTrans {}

        @Request(
            uri = "setFinAccountTransStatus",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "FindFinAccountTrans")
        @Response(name = "error", type = "view", value = "FindFinAccountTrans")
        @Event(type = "service", invoke = "setFinAccountTransStatus")
        public static String setFinAccountTransStatus(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "BankReconciliation",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "BankReconciliation")
        public interface BankReconciliation {}

        @Request(
            uri = "getFinAccountTransRunningTotalAndBalances",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "request", value = "json")
        @Response(name = "error", type = "request", value = "json")
        @Event(type = "service-multi", invoke = "getFinAccountTransRunningTotalAndBalances")
        public static String getFinAccountTransRunningTotalAndBalances(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "reconcileFinAccountTrans",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "BankReconciliation")
        @Response(name = "error", type = "view", value = "BankReconciliation")
        @Event(type = "service-multi", invoke = "reconcileFinAccountTrans")
        public static String reconcileFinAccountTrans(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "EditFinAccountReconciliations",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditFinAccountReconciliations")
        public interface EditFinAccountReconciliations {}

        @Request(
            uri = "FindFinAccountReconciliations",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "FindFinAccountReconciliations")
        public interface FindFinAccountReconciliations {}

        @Request(
            uri = "createGlReconciliation",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditFinAccountReconciliations")
        @Response(name = "error", type = "view", value = "EditFinAccountReconciliations")
        @Event(type = "service", invoke = "createGlReconciliation")
        public static String createGlReconciliation(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "updateFinAccountGlReconciliation",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditFinAccountReconciliations")
        @Response(name = "error", type = "view", value = "EditFinAccountReconciliations")
        @Event(type = "service", invoke = "updateGlReconciliation")
        public static String updateFinAccountGlReconciliation(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "ViewGlReconciliationWithTransaction",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ViewGlReconciliationWithTransaction")
        public interface ViewGlReconciliationWithTransaction {}

        @Request(
            uri = "callReconcileFinAccountTrans",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ViewGlReconciliationWithTransaction")
        @Response(name = "error", type = "view", value = "ViewGlReconciliationWithTransaction")
        @Event(type = "service-multi", invoke = "reconcileFinAccountTrans")
        public static String callReconcileFinAccountTrans(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "assignGlRecToFinAccTrans",
            controller = "accounting",
            secure = "true"
        )
        @Response(name = "success", type = "view", value = "FindFinAccountTrans")
        @Response(name = "error", type = "view", value = "FindFinAccountTrans")
        @Event(type = "service-multi", invoke = "assignGlRecToFinAccTrans")
        public static String assignGlRecToFinAccTrans(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "removeFinAccountTransFromReconciliation",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "BankReconciliation")
        @Response(name = "error", type = "view", value = "BankReconciliation")
        @Event(type = "service", invoke = "removeFinAccountTransFromReconciliation")
        public static String removeFinAccountTransFromReconciliation(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "removeFinAccountTransAssociation",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ViewGlReconciliationWithTransaction")
        @Response(name = "error", type = "view", value = "ViewGlReconciliationWithTransaction")
        @Event(type = "service", invoke = "removeFinAccountTransFromReconciliation")
        public static String removeFinAccountTransAssociation(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "cancelBankReconciliation",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ViewGlReconciliationWithTransaction")
        @Response(name = "error", type = "view", value = "ViewGlReconciliationWithTransaction")
        @Event(type = "service", invoke = "cancelBankReconciliation")
        public static String cancelBankReconciliation(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "EditFinAccountAuths",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditFinAccountAuths")
        public interface EditFinAccountAuths {}

        @Request(
            uri = "createFinAccountAuth",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditFinAccountAuths")
        @Response(name = "error", type = "view", value = "EditFinAccountAuths")
        @Event(type = "service", invoke = "createFinAccountAuth")
        public static String createFinAccountAuth(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

    }

    // Auto-generated split (Part 29)
    public static class Part29 {
        @Request(
            uri = "expireFinAccountAuth",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditFinAccountAuths")
        @Response(name = "error", type = "view", value = "EditFinAccountAuths")
        @Event(type = "service", invoke = "expireFinAccountAuth")
        public static String expireFinAccountAuth(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "editFinAccountTypeGlAccounts",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditFinAccountTypeGlAccounts")
        public interface EditFinAccountTypeGlAccounts {}

        @Request(
            uri = "createFinAccountTypeGlAccount",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditFinAccountTypeGlAccounts")
        @Response(name = "error", type = "view", value = "EditFinAccountTypeGlAccounts")
        @Event(type = "service", invoke = "createFinAccountTypeGlAccount")
        public static String createFinAccountTypeGlAccount(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "updateFinAccountTypeGlAccount",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditFinAccountTypeGlAccounts")
        @Response(name = "error", type = "view", value = "EditFinAccountTypeGlAccounts")
        @Event(type = "service", invoke = "updateFinAccountTypeGlAccount")
        public static String updateFinAccountTypeGlAccount(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "deleteFinAccountTypeGlAccount",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditFinAccountTypeGlAccounts")
        @Response(name = "error", type = "view", value = "EditFinAccountTypeGlAccounts")
        @Event(type = "service", invoke = "deleteFinAccountTypeGlAccount")
        public static String deleteFinAccountTypeGlAccount(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "journals",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "journals")
        public interface Journals {}

        @Request(
            uri = "SetupGlJournals",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "SetupGlJournals")
        @Response(name = "error", type = "view", value = "journals")
        public interface SetupGlJournals {}

        @Request(
            uri = "createGlJournal",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "journals")
        @Response(name = "error", type = "view", value = "journals")
        @Event(type = "service", invoke = "createGlJournal")
        public static String createGlJournal(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "updateGlJournal",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "journals")
        @Response(name = "error", type = "view", value = "journals")
        @Event(type = "service", invoke = "updateGlJournal")
        public static String updateGlJournal(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "deleteGlJournal",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "journals")
        @Response(name = "error", type = "view", value = "journals")
        @Event(type = "service", invoke = "deleteGlJournal")
        public static String deleteGlJournal(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "Transactions",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "Transactions")
        public interface Transactions {}

        @Request(
            uri = "TransactionReports",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "TransactionTotals")
        public interface TransactionReports {}

        @Request(
            uri = "FinancialSummaryReportOptions",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "FinancialSummaryReportOptions")
        public interface FinancialSummaryReportOptions {}

        @Request(
            uri = "SalesInvoiceByProductCategorySummary",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "SalesInvoiceByProductCategorySummary")
        public interface SalesInvoiceByProductCategorySummary {}

        @Request(
            uri = "TrialBalance",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "TrialBalance")
        public interface TrialBalance {}

        @Request(
            uri = "IncomeStatement",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "IncomeStatement")
        public interface IncomeStatement {}

        @Request(
            uri = "ComparativeIncomeStatement",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ComparativeIncomeStatement")
        @Response(name = "error", type = "view", value = "ComparativeIncomeStatement")
        public interface ComparativeIncomeStatement {}

        @Request(
            uri = "BalanceSheet",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "BalanceSheet")
        public interface BalanceSheet {}

        @Request(
            uri = "BalanceSheet.pdf",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "BalanceSheetPdf")
        public interface BalanceSheetPdf {}

        @Request(
            uri = "ComparativeBalanceSheet",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ComparativeBalanceSheet")
        public interface ComparativeBalanceSheet {}

    }

    // Auto-generated split (Part 30)
    public static class Part30 {
        @Request(
            uri = "ComparativeBalanceSheet.pdf",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ComparativeBalanceSheetPdf")
        public interface ComparativeBalanceSheetPdf {}

        @Request(
            uri = "ComparativeBalanceSheet.csv",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ComparativeBalanceSheetCsv")
        public interface ComparativeBalanceSheetCsv {}

        @Request(
            uri = "TransactionTotals",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "TransactionTotals")
        public interface TransactionTotals {}

        @Request(
            uri = "FindPaymentsForDepositOrWithdraw",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "PaymentsDepositWithdraw")
        @Response(name = "error", type = "view", value = "PaymentsDepositWithdraw")
        public interface FindPaymentsForDepositOrWithdraw {}

        @Request(
            uri = "getPaymentRunningTotal",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "request", value = "json")
        @Response(name = "error", type = "request", value = "json")
        @Event(type = "service", invoke = "getPaymentRunningTotal")
        public static String getPaymentRunningTotal(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "LookupCustomerName",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "LookupCustomerName")
        public interface LookupCustomerName {}

        @Request(
            uri = "depositWithdrawPayments",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "PaymentsDepositWithdraw")
        @Response(name = "error", type = "view", value = "PaymentsDepositWithdraw")
        @Event(type = "service", invoke = "depositWithdrawPayments")
        public static String depositWithdrawPayments(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "NewDepositSlip",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "NewDepositSlip")
        public interface NewDepositSlip {}

        @Request(
            uri = "createPaymentBatch",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditDepositSlipAndMembers")
        @Response(name = "error", type = "view", value = "NewDepositSlip")
        @Event(type = "service", invoke = "checkAndCreateBatchForValidPayments")
        public static String createPaymentBatch(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "InventoryValuation",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "InventoryValuation")
        public interface InventoryValuation {}

        @Request(
            uri = "InventoryValuation.pdf",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "InventoryValuationPdf")
        @Response(name = "error", type = "view", value = "InventoryValuation")
        public interface InventoryValuationPdf {}

        @Request(
            uri = "InventoryValuation.csv",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "InventoryValuationCsv")
        @Response(name = "error", type = "view", value = "InventoryValuation")
        public interface InventoryValuationCsv {}

        @Request(
            uri = "CashFlowStatement",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "CashFlowStatement")
        public interface CashFlowStatement {}

        @Request(
            uri = "CashFlowStatementListPdf.pdf",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "CashFlowStatementListPdf")
        public interface CashFlowStatementListPdfPdf {}

        @Request(
            uri = "CashFlowStatementListCsv.csv",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "CashFlowStatementListCsv")
        public interface CashFlowStatementListCsvCsv {}

        @Request(
            uri = "ComparativeCashFlowStatement",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ComparativeCashFlowStatement")
        @Response(name = "error", type = "view", value = "ComparativeCashFlowStatement")
        public interface ComparativeCashFlowStatement {}

        @Request(
            uri = "ComparativeCashFlowStatement.csv",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ComparativeCashFlowStatementCsv")
        @Response(name = "error", type = "view", value = "ComparativeCashFlowStatement")
        public interface ComparativeCashFlowStatementCsv {}

        @Request(
            uri = "ComparativeCashFlowStatement.pdf",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ComparativeCashFlowStatementPdf")
        @Response(name = "error", type = "view", value = "ComparativeCashFlowStatement")
        public interface ComparativeCashFlowStatementPdf {}

        @Request(
            uri = "SalesInvoiceByProductGlAccountSummary",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "SalesInvoiceByProductGlAccountSummary")
        public interface SalesInvoiceByProductGlAccountSummary {}

        @Request(
            uri = "PaymentByMethodSummary",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "PaymentByMethodSummary")
        public interface PaymentByMethodSummary {}

    }

    // Auto-generated split (Part 31)
    public static class Part31 {
        @Request(
            uri = "InventoryIssueSummary",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "InventoryIssueSummary")
        public interface InventoryIssueSummary {}

        @Request(
            uri = "FinancialAccountSummary",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "FinancialAccountSummary")
        public interface FinancialAccountSummary {}

        @Request(
            uri = "GlAccountTrialBalance",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "GlAccountTrialBalance")
        public interface GlAccountTrialBalance1 {}

        @Request(
            uri = "ImportExport",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ImportExport")
        public interface ImportExport {}

        @Request(
            uri = "ImportExportInvoice",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ImportExportInvoice")
        public interface ImportExportInvoice {}

        @Request(
            uri = "ExportInvoiceCsv.csv",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ExportInvoicesCsv")
        public interface ExportInvoiceCsvCsv {}

        @Request(
            uri = "ImportInvoice",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "request", value = "ImportExportInvoice")
        @Response(name = "error", type = "view", value = "ImportExportInvoice")
        @Event(type = "service", invoke = "importInvoice")
        public static String importInvoice(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "ExportTransactions",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ExportTransactions")
        public interface ExportTransactions {}

        @Request(
            uri = "ExportTransaction.csv",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ExportTransactionCsv")
        public interface ExportTransactionCsv {}

        @Request(
            uri = "findPayments",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "findPayments")
        public interface FindPayments {}

        @Request(
            uri = "newPayment",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "newPayment")
        public interface NewPayment {}

        @Request(
            uri = "editPayment",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "editPayment")
        public interface EditPayment {}

        @Request(
            uri = "createPayment",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "editPayment")
        @Response(name = "error", type = "view", value = "newPayment")
        @Event(type = "service", invoke = "createPaymentAndFinAccountTrans")
        public static String createPayment(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "updatePayment",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "editPayment")
        @Response(name = "error", type = "view", value = "editPayment")
        @Event(type = "service", invoke = "updatePayment")
        public static String updatePayment(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "paymentOverview",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "paymentOverview")
        public interface PaymentOverview {}

        @Request(
            uri = "editPaymentApplications",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "editPaymentApplications")
        public interface EditPaymentApplications {}

        @Request(
            uri = "createPaymentApplication",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "editPaymentApplications")
        @Event(type = "service", invoke = "createPaymentApplication")
        public static String createPaymentApplication(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "updatePaymentApplication",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "editPaymentApplications")
        @Event(type = "service", invoke = "updatePaymentApplicationDef")
        public static String updatePaymentApplication(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "removePaymentApplication",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "editPaymentApplications")
        @Response(name = "error", type = "view", value = "editPaymentApplications")
        @Event(type = "service", invoke = "removePaymentApplication")
        public static String removePaymentApplication(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "setPaymentStatus",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "paymentOverview")
        @Response(name = "error", type = "view", value = "paymentOverview")
        @Event(type = "service", invoke = "setPaymentStatus")
        public static String setPaymentStatus(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

    }

    // Auto-generated split (Part 32)
    public static class Part32 {
        @Request(
            uri = "printChecks.pdf",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "PrintChecks")
        public interface PrintChecksPdf {}

        @Request(
            uri = "quickSendPayment",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ListChecksToSend")
        @Response(name = "error", type = "view", value = "ListChecksToSend")
        @Event(type = "service-multi", invoke = "quickSendPayment")
        public static String quickSendPayment(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "FindSalesInvoicesByDueDate",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "FindSalesInvoicesByDueDate")
        public interface FindSalesInvoicesByDueDate {}

        @Request(
            uri = "FindPurchaseInvoicesByDueDate",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "FindPurchaseInvoicesByDueDate")
        public interface FindPurchaseInvoicesByDueDate {}

        @Request(
            uri = "FindGatewayResponses",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "FindGatewayResponses")
        public interface FindGatewayResponses {}

        @Request(
            uri = "ViewGatewayResponse",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ViewGatewayResponse")
        public interface ViewGatewayResponse {}

        @Request(
            uri = "AuthorizeTransaction",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "AuthorizeTransaction")
        public interface AuthorizeTransaction {}

        @Request(
            uri = "CaptureTransaction",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "CaptureTransaction")
        public interface CaptureTransaction {}

        @Request(
            uri = "ManualTransaction",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ManualTransaction")
        public interface ManualTransaction {}

        @Request(
            uri = "manualETx",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ManualTransaction")
        public interface ManualETx {}

        @Request(
            uri = "processManualCcTx",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ManualTransaction")
        @Event(type = "service", invoke = "manualForcedCcTransaction")
        public static String processManualCcTx(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "processAuthorizeTransaction",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ViewGatewayResponse")
        @Response(name = "error", type = "view", value = "ManualTransaction")
        @Event(type = "service", invoke = "authOrderPaymentPreference")
        public static String processAuthorizeTransaction(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "processCaptureTransaction",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ViewGatewayResponse")
        @Response(name = "error", type = "view", value = "ManualTransaction")
        @Event(type = "service", invoke = "captureOrderPayments")
        public static String processCaptureTransaction(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "processReleaseTransaction",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ManualTransaction")
        @Response(name = "error", type = "view", value = "ManualTransaction")
        @Event(type = "service", invoke = "releaseOrderPaymentPreference")
        public static String processReleaseTransaction(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "processRefundTransaction",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ManualTransaction")
        @Response(name = "error", type = "view", value = "ManualTransaction")
        @Event(type = "service", invoke = "refundOrderPaymentPreference")
        public static String processRefundTransaction(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "FindPaymentGroup",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "FindPaymentGroup")
        public interface FindPaymentGroup {}

        @Request(
            uri = "EditPaymentGroup",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditPaymentGroup")
        public interface EditPaymentGroup {}

        @Request(
            uri = "createPaymentGroup",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditPaymentGroup")
        @Response(name = "error", type = "view", value = "EditPaymentGroup")
        @Event(type = "service", invoke = "createPaymentGroup")
        public static String createPaymentGroup(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "updatePaymentGroup",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditPaymentGroup")
        @Response(name = "error", type = "view", value = "EditPaymentGroup")
        @Event(type = "service", invoke = "updatePaymentGroup")
        public static String updatePaymentGroup(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "deletePaymentGroup",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "FindPaymentGroup")
        @Response(name = "error", type = "view", value = "FindPaymentGroup")
        @Event(type = "service", invoke = "deletePaymentGroup")
        public static String deletePaymentGroup(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

    }

    // Auto-generated split (Part 33)
    public static class Part33 {
        @Request(
            uri = "EditPaymentGroupMember",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditPaymentGroupMember")
        public interface EditPaymentGroupMember {}

        @Request(
            uri = "createPaymentGroupMember",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditPaymentGroupMember")
        @Response(name = "error", type = "view", value = "EditPaymentGroupMember")
        @Event(type = "service", invoke = "createPaymentGroupMember")
        public static String createPaymentGroupMember(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "updatePaymentGroupMember",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditPaymentGroupMember")
        @Response(name = "error", type = "view", value = "EditPaymentGroupMember")
        @Event(type = "service", invoke = "updatePaymentGroupMember")
        public static String updatePaymentGroupMember(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "expirePaymentGroupMember",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditPaymentGroupMember")
        @Response(name = "error", type = "view", value = "EditPaymentGroupMember")
        @Event(type = "service", invoke = "expirePaymentGroupMember")
        public static String expirePaymentGroupMember(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "PaymentGroupOverview",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "PaymentGroupOverview")
        public interface PaymentGroupOverview {}

        @Request(
            uri = "cancelPaymentGroup",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "PaymentGroupOverview")
        @Response(name = "error", type = "view", value = "FindPaymentGroup")
        @Event(type = "service", invoke = "cancelPaymentBatch")
        public static String cancelPaymentGroup_2(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "cancelCheckRunPayments",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "PaymentGroupOverview")
        @Response(name = "error", type = "view", value = "FindPaymentGroup")
        @Event(type = "service", invoke = "cancelCheckRunPayments")
        public static String cancelCheckRunPayments_2(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "DepositSlip.pdf",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "DepositSlipPdf")
        @Response(name = "error", type = "view", value = "PaymentGroupOverview")
        public interface DepositSlipPdf {}

        @Request(
            uri = "settings",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ListCompanies")
        public interface Settings {}

        @Request(
            uri = "AddCompany",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "AddCompany")
        public interface AddCompany {}

        @Request(
            uri = "AdminMain",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "PartyAcctgPreference")
        public interface AdminMain {}

        @Request(
            uri = "TimePeriods",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditCustomTimePeriod")
        public interface TimePeriods {}

        @Request(
            uri = "createCustomTimePeriod",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditCustomTimePeriod")
        @Response(name = "error", type = "view", value = "EditCustomTimePeriod")
        @Event(type = "service", invoke = "createCustomTimePeriod")
        public static String createCustomTimePeriod(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "closeFinancialTimePeriod",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditCustomTimePeriod")
        @Response(name = "error", type = "view", value = "EditCustomTimePeriod")
        @Event(type = "service", invoke = "closeFinancialTimePeriod")
        public static String closeFinancialTimePeriod(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "PartyAcctgPreference",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "PartyAcctgPreference")
        public interface PartyAcctgPreference {}

        @Request(
            uri = "createPartyAcctgPreference",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "PartyAcctgPreference")
        @Response(name = "error", type = "view", value = "PartyAcctgPreference")
        @Event(type = "service", invoke = "createPartyAcctgPreference")
        public static String createPartyAcctgPreference(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "updatePartyAcctgPreference",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "PartyAcctgPreference")
        @Response(name = "error", type = "view", value = "PartyAcctgPreference")
        @Event(type = "service", invoke = "updatePartyAcctgPreference")
        public static String updatePartyAcctgPreference(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "GlAccountAssignment",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "GlAccountTypeDefaults")
        public interface GlAccountAssignment {}

        @Request(
            uri = "GlAccountTypeDefaults",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "GlAccountTypeDefaults")
        public interface GlAccountTypeDefaults {}

        @Request(
            uri = "GlAccountSalInvoice",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "GlAccountSalInvoice")
        public interface GlAccountSalInvoice {}

    }

    // Auto-generated split (Part 34)
    public static class Part34 {
        @Request(
            uri = "GlAccountPurInvoice",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "GlAccountPurInvoice")
        public interface GlAccountPurInvoice {}

        @Request(
            uri = "GlAccountTypePaymentType",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "GlAccountTypePaymentType")
        public interface GlAccountTypePaymentType {}

        @Request(
            uri = "GlAccountNrPaymentMethod",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "GlAccountNrPaymentMethod")
        public interface GlAccountNrPaymentMethod {}

        @Request(
            uri = "createGlAccountTypeDefault",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "GlAccountTypeDefaults")
        @Response(name = "error", type = "view", value = "GlAccountTypeDefaults")
        @Event(type = "service", invoke = "createGlAccountTypeDefault")
        public static String createGlAccountTypeDefault(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "removeGlAccountTypeDefault",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "GlAccountTypeDefaults")
        @Response(name = "error", type = "view", value = "GlAccountTypeDefaults")
        @Event(type = "service", invoke = "removeGlAccountTypeDefault")
        public static String removeGlAccountTypeDefault(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "addSalInvoiceItemTypeGlAssignment",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "GlAccountSalInvoice")
        @Response(name = "error", type = "view", value = "GlAccountSalInvoice")
        @Event(type = "service", invoke = "addInvoiceItemTypeGlAssignment")
        public static String addSalInvoiceItemTypeGlAssignment(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "removeSalInvoiceItemTypeGlAssignment",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "GlAccountSalInvoice")
        @Response(name = "error", type = "view", value = "GlAccountSalInvoice")
        @Event(type = "service", invoke = "removeInvoiceItemTypeGlAssignment")
        public static String removeSalInvoiceItemTypeGlAssignment(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "addPurInvoiceItemTypeGlAssignment",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "GlAccountPurInvoice")
        @Response(name = "error", type = "view", value = "GlAccountPurInvoice")
        @Event(type = "service", invoke = "addInvoiceItemTypeGlAssignment")
        public static String addPurInvoiceItemTypeGlAssignment(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "removePurInvoiceItemTypeGlAssignment",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "GlAccountPurInvoice")
        @Response(name = "error", type = "view", value = "GlAccountPurInvoice")
        @Event(type = "service", invoke = "removeInvoiceItemTypeGlAssignment")
        public static String removePurInvoiceItemTypeGlAssignment(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "addPaymentTypeGlAssignment",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "GlAccountTypePaymentType")
        @Response(name = "error", type = "view", value = "GlAccountTypePaymentType")
        @Event(type = "service", invoke = "addPaymentTypeGlAssignment")
        public static String addPaymentTypeGlAssignment(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "removePaymentTypeGlAssignment",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "GlAccountTypePaymentType")
        @Response(name = "error", type = "view", value = "GlAccountTypePaymentType")
        @Event(type = "service", invoke = "removePaymentTypeGlAssignment")
        public static String removePaymentTypeGlAssignment(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "addPaymentMethodTypeGlAssignment",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "GlAccountNrPaymentMethod")
        @Response(name = "error", type = "view", value = "GlAccountNrPaymentMethod")
        @Event(type = "service", invoke = "addPaymentMethodTypeGlAssignment")
        public static String addPaymentMethodTypeGlAssignment(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "removePaymentMethodTypeGlAssignment",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "GlAccountNrPaymentMethod")
        @Response(name = "error", type = "view", value = "GlAccountNrPaymentMethod")
        @Event(type = "service", invoke = "removePaymentMethodTypeGlAssignment")
        public static String removePaymentMethodTypeGlAssignment(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "viewFXConversions",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ViewFXConversions")
        public interface ViewFXConversions {}

        @Request(
            uri = "updateFXConversion",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ViewFXConversions")
        @Response(name = "error", type = "view", value = "ViewFXConversions")
        @Event(type = "service", invoke = "updateFXConversion")
        public static String updateFXConversion(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "viewRateAmounts",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ViewRateAmounts")
        public interface ViewRateAmounts {}

        @Request(
            uri = "updateRateAmount",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ViewRateAmounts")
        @Response(name = "error", type = "view", value = "ViewRateAmounts")
        @Event(type = "service", invoke = "updateRateAmount")
        public static String updateRateAmount(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "deleteRateAmount",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ViewRateAmounts")
        @Response(name = "error", type = "view", value = "ViewRateAmounts")
        @Event(type = "service", invoke = "expireRateAmount")
        public static String deleteRateAmount(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "expireRateAmount",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ViewRateAmounts")
        @Response(name = "error", type = "view", value = "ViewRateAmounts")
        @Event(type = "service", invoke = "expireRateAmount")
        public static String expireRateAmount(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "editProductGlAccounts",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditProductGlAccounts")
        public interface EditProductGlAccounts {}

    }

    // Auto-generated split (Part 35)
    public static class Part35 {
        @Request(
            uri = "createProductGlAccount",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditProductGlAccounts")
        @Response(name = "error", type = "view", value = "EditProductGlAccounts")
        @Event(type = "service", invoke = "createProductGlAccount")
        public static String createProductGlAccount(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "updateProductGlAccount",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditProductGlAccounts")
        @Response(name = "error", type = "view", value = "EditProductGlAccounts")
        @Event(type = "service", invoke = "updateProductGlAccount")
        public static String updateProductGlAccount(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "deleteProductGlAccount",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditProductGlAccounts")
        @Response(name = "error", type = "view", value = "EditProductGlAccounts")
        @Event(type = "service", invoke = "deleteProductGlAccount")
        public static String deleteProductGlAccount(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "editInvoiceItemType",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditInvoiceItemType")
        public interface EditInvoiceItemType1 {}

        @Request(
            uri = "updateInvoiceItemType",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditInvoiceItemType")
        @Response(name = "error", type = "view", value = "EditInvoiceItemType")
        @Event(type = "service", invoke = "updateInvoiceItemType")
        public static String updateInvoiceItemType_2(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "editPaymentMethodType",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditPaymentMethodType")
        public interface EditPaymentMethodType {}

        @Request(
            uri = "updatePaymentMethodType",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditPaymentMethodType")
        @Response(name = "error", type = "view", value = "EditPaymentMethodType")
        @Event(type = "service", invoke = "updatePaymentMethodType")
        public static String updatePaymentMethodType(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "voidPayment",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "paymentOverview")
        @Response(name = "error", type = "view", value = "paymentOverview")
        @Event(type = "service", invoke = "voidPayment")
        public static String voidPayment(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "FindPaymentGatewayConfig",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "FindPaymentGatewayConfig")
        public interface FindPaymentGatewayConfig {}

        @Request(
            uri = "EditPaymentGatewayConfig",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditPaymentGatewayConfig")
        public interface EditPaymentGatewayConfig {}

        @Request(
            uri = "UpdatePaymentGatewayConfig",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditPaymentGatewayConfig")
        @Response(name = "error", type = "view", value = "EditPaymentGatewayConfig")
        @Event(type = "service", invoke = "updatePaymentGatewayConfig")
        public static String updatePaymentGatewayConfig(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "UpdatePaymentGatewayConfigSagePay",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditPaymentGatewayConfig")
        @Response(name = "error", type = "view", value = "EditPaymentGatewayConfig")
        @Event(type = "service", invoke = "updatePaymentGatewayConfigSagePay")
        public static String updatePaymentGatewayConfigSagePay(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "UpdatePaymentGatewayConfigAuthorizeNet",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditPaymentGatewayConfig")
        @Response(name = "error", type = "view", value = "EditPaymentGatewayConfig")
        @Event(type = "service", invoke = "updatePaymentGatewayConfigAuthorizeNet")
        public static String updatePaymentGatewayConfigAuthorizeNet(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "UpdatePaymentGatewayConfigClearCommerce",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditPaymentGatewayConfig")
        @Response(name = "error", type = "view", value = "EditPaymentGatewayConfig")
        @Event(type = "service", invoke = "updatePaymentGatewayConfigClearCommerce")
        public static String updatePaymentGatewayConfigClearCommerce(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "UpdatePaymentGatewayConfigCyberSource",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditPaymentGatewayConfig")
        @Response(name = "error", type = "view", value = "EditPaymentGatewayConfig")
        @Event(type = "service", invoke = "updatePaymentGatewayConfigCyberSource")
        public static String updatePaymentGatewayConfigCyberSource(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "UpdatePaymentGatewayConfigPayflowPro",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditPaymentGatewayConfig")
        @Response(name = "error", type = "view", value = "EditPaymentGatewayConfig")
        @Event(type = "service", invoke = "updatePaymentGatewayConfigPayflowPro")
        public static String updatePaymentGatewayConfigPayflowPro(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "UpdatePaymentGatewayConfigPayPal",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditPaymentGatewayConfig")
        @Response(name = "error", type = "view", value = "EditPaymentGatewayConfig")
        @Event(type = "service", invoke = "updatePaymentGatewayConfigPayPal")
        public static String updatePaymentGatewayConfigPayPal(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "UpdatePaymentGatewayConfigWorldPay",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditPaymentGatewayConfig")
        @Response(name = "error", type = "view", value = "EditPaymentGatewayConfig")
        @Event(type = "service", invoke = "updatePaymentGatewayConfigWorldPay")
        public static String updatePaymentGatewayConfigWorldPay(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "UpdatePaymentGatewayConfigEway",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditPaymentGatewayConfig")
        @Response(name = "error", type = "view", value = "EditPaymentGatewayConfig")
        @Event(type = "service", invoke = "updatePaymentGatewayConfigEway")
        public static String updatePaymentGatewayConfigEway(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "FindPaymentGatewayConfigTypes",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "FindPaymentGatewayConfigTypes")
        public interface FindPaymentGatewayConfigTypes {}

    }

    // Auto-generated split (Part 36)
    public static class Part36 {
        @Request(
            uri = "EditPaymentGatewayConfigType",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditPaymentGatewayConfigType")
        public interface EditPaymentGatewayConfigType {}

        @Request(
            uri = "UpdatePaymentGatewayConfigType",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditPaymentGatewayConfigType")
        @Response(name = "error", type = "view", value = "EditPaymentGatewayConfigType")
        @Event(type = "service", invoke = "updatePaymentGatewayConfigType")
        public static String updatePaymentGatewayConfigType(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "UpdatePaymentGatewayConfigSecurePay",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditPaymentGatewayConfig")
        @Response(name = "error", type = "view", value = "EditPaymentGatewayConfig")
        @Event(type = "service", invoke = "updatePaymentGatewayConfigSecurePay")
        public static String updatePaymentGatewayConfigSecurePay(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "UpdatePaymentGatewayConfigiDEAL",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditPaymentGatewayConfig")
        @Response(name = "error", type = "view", value = "EditPaymentGatewayConfig")
        @Event(type = "service", invoke = "updatePaymentGatewayConfigiDEAL")
        public static String updatePaymentGatewayConfigiDEAL(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "AddCustomTimePeriod",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "AddCustomTimePeriod")
        public interface AddCustomTimePeriod {}

        @Request(
            uri = "createCustomTimePeriod",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditCustomTimePeriod")
        @Response(name = "error", type = "view", value = "AddCustomTimePeriod")
        @Event(type = "service", invoke = "createCustomTimePeriod")
        public static String createCustomTimePeriod_2(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "updateCustomTimePeriod",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditCustomTimePeriod")
        @Response(name = "error", type = "view", value = "EditCustomTimePeriod")
        @Event(type = "service-multi", invoke = "updateCustomTimePeriod")
        public static String updateCustomTimePeriod(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "deleteCustomTimePeriod",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditCustomTimePeriod")
        @Response(name = "error", type = "view", value = "EditCustomTimePeriod")
        @Event(type = "service", invoke = "deleteCustomTimePeriod")
        public static String deleteCustomTimePeriod(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "FindTaxAuthority",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "FindTaxAuthority")
        public interface FindTaxAuthority {}

        @Request(
            uri = "EditTaxAuthority",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditTaxAuthority")
        public interface EditTaxAuthority {}

        @Request(
            uri = "createTaxAuthority",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditTaxAuthority")
        @Response(name = "error", type = "view", value = "EditTaxAuthority")
        @Event(type = "service", invoke = "createTaxAuthority")
        public static String createTaxAuthority(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "updateTaxAuthority",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditTaxAuthority")
        @Response(name = "error", type = "view", value = "EditTaxAuthority")
        @Event(type = "service", invoke = "updateTaxAuthority")
        public static String updateTaxAuthority(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "EditTaxAuthorityCategories",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditTaxAuthorityRateProducts")
        public interface EditTaxAuthorityCategories {}

        @Request(
            uri = "createTaxAuthorityCategory",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditTaxAuthorityRateProducts")
        @Response(name = "error", type = "view", value = "EditTaxAuthorityRateProducts")
        @Event(type = "service", invoke = "createTaxAuthorityCategory")
        public static String createTaxAuthorityCategory(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "updateTaxAuthorityCategory",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditTaxAuthorityRateProducts")
        @Response(name = "error", type = "view", value = "EditTaxAuthorityRateProducts")
        @Event(type = "service", invoke = "updateTaxAuthorityCategory")
        public static String updateTaxAuthorityCategory(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "deleteTaxAuthorityCategory",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditTaxAuthorityRateProducts")
        @Response(name = "error", type = "view", value = "EditTaxAuthorityRateProducts")
        @Event(type = "service", invoke = "deleteTaxAuthorityCategory")
        public static String deleteTaxAuthorityCategory(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "EditTaxAuthorityAssocs",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditTaxAuthorityAssocs")
        public interface EditTaxAuthorityAssocs {}

        @Request(
            uri = "createTaxAuthorityAssoc",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditTaxAuthorityAssocs")
        @Response(name = "error", type = "view", value = "EditTaxAuthorityAssocs")
        @Event(type = "service", invoke = "createTaxAuthorityAssoc")
        public static String createTaxAuthorityAssoc(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "updateTaxAuthorityAssoc",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditTaxAuthorityAssocs")
        @Response(name = "error", type = "view", value = "EditTaxAuthorityAssocs")
        @Event(type = "service", invoke = "updateTaxAuthorityAssoc")
        public static String updateTaxAuthorityAssoc(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "deleteTaxAuthorityAssoc",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditTaxAuthorityAssocs")
        @Response(name = "error", type = "view", value = "EditTaxAuthorityAssocs")
        @Event(type = "service", invoke = "deleteTaxAuthorityAssoc")
        public static String deleteTaxAuthorityAssoc(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

    }

    // Auto-generated split (Part 37)
    public static class Part37 {
        @Request(
            uri = "EditTaxAuthorityGlAccounts",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditTaxAuthorityGlAccounts")
        public interface EditTaxAuthorityGlAccounts {}

        @Request(
            uri = "createTaxAuthorityGlAccount",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditTaxAuthorityGlAccounts")
        @Response(name = "error", type = "view", value = "EditTaxAuthorityGlAccounts")
        @Event(type = "service", invoke = "createTaxAuthorityGlAccount")
        public static String createTaxAuthorityGlAccount(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "deleteTaxAuthorityGlAccount",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditTaxAuthorityGlAccounts")
        @Response(name = "error", type = "view", value = "EditTaxAuthorityGlAccounts")
        @Event(type = "service", invoke = "deleteTaxAuthorityGlAccount")
        public static String deleteTaxAuthorityGlAccount(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "EditTaxAuthorityRateProducts",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditTaxAuthorityRateProducts")
        public interface EditTaxAuthorityRateProducts {}

        @Request(
            uri = "createTaxAuthorityRateProduct",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditTaxAuthorityRateProducts")
        @Response(name = "error", type = "view", value = "EditTaxAuthorityRateProducts")
        @Event(type = "service", invoke = "createTaxAuthorityRateProduct")
        public static String createTaxAuthorityRateProduct(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "updateTaxAuthorityRateProduct",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditTaxAuthorityRateProducts")
        @Response(name = "error", type = "view", value = "EditTaxAuthorityRateProducts")
        @Event(type = "service", invoke = "updateTaxAuthorityRateProduct")
        public static String updateTaxAuthorityRateProduct(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "deleteTaxAuthorityRateProduct",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditTaxAuthorityRateProducts")
        @Response(name = "error", type = "view", value = "EditTaxAuthorityRateProducts")
        @Event(type = "service", invoke = "deleteTaxAuthorityRateProduct")
        public static String deleteTaxAuthorityRateProduct(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "ListTaxAuthorityParties",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditTaxAuthority")
        public interface ListTaxAuthorityParties {}

        @Request(
            uri = "deleteTaxAuthorityPartyInfo",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "request-redirect", value = "EditTaxAuthority")
        @Response(name = "error", type = "view", value = "EditTaxAuthority")
        @Event(type = "service", invoke = "deletePartyTaxAuthInfo")
        public static String deleteTaxAuthorityPartyInfo(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "EditTaxAuthorityPartyInfo",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditTaxAuthorityPartyInfo")
        public interface EditTaxAuthorityPartyInfo {}

        @Request(
            uri = "createTaxAuthorityPartyInfo",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditTaxAuthorityPartyInfo")
        @Response(name = "error", type = "view", value = "EditTaxAuthorityPartyInfo")
        @Event(type = "service", invoke = "createPartyTaxAuthInfo")
        public static String createTaxAuthorityPartyInfo(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "updateTaxAuthorityPartyInfo",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditTaxAuthorityPartyInfo")
        @Response(name = "error", type = "view", value = "EditTaxAuthorityPartyInfo")
        @Event(type = "service", invoke = "updatePartyTaxAuthInfo")
        public static String updateTaxAuthorityPartyInfo(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "editOrganizationTaxAuthorityGlAccount",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditOrganizationTaxAuthorityGlAccounts")
        public interface EditOrganizationTaxAuthorityGlAccount {}

        @Request(
            uri = "createOrganizationTaxAuthorityGlAccount",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditOrganizationTaxAuthorityGlAccounts")
        @Response(name = "error", type = "view", value = "EditOrganizationTaxAuthorityGlAccounts")
        @Event(type = "service", invoke = "createTaxAuthorityGlAccount")
        public static String createOrganizationTaxAuthorityGlAccount(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "updateOrganizationTaxAuthorityGlAccount",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditOrganizationTaxAuthorityGlAccounts")
        @Response(name = "error", type = "view", value = "EditOrganizationTaxAuthorityGlAccounts")
        @Event(type = "service", invoke = "updateTaxAuthorityGlAccount")
        public static String updateOrganizationTaxAuthorityGlAccount(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "deleteOrganizationTaxAuthorityGlAccount",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditOrganizationTaxAuthorityGlAccounts")
        @Response(name = "error", type = "view", value = "EditOrganizationTaxAuthorityGlAccounts")
        @Event(type = "service", invoke = "deleteTaxAuthorityGlAccount")
        public static String deleteOrganizationTaxAuthorityGlAccount(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "editProductCategoryGlAccounts",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditProductCategoryGlAccounts")
        public interface EditProductCategoryGlAccounts {}

        @Request(
            uri = "createProductCategoryGlAccount",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditProductCategoryGlAccounts")
        @Response(name = "error", type = "view", value = "EditProductCategoryGlAccounts")
        @Event(type = "service", invoke = "createProductCategoryGlAccount")
        public static String createProductCategoryGlAccount(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "updateProductCategoryGlAccount",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditProductCategoryGlAccounts")
        @Response(name = "error", type = "view", value = "EditProductCategoryGlAccounts")
        @Event(type = "service", invoke = "updateProductCategoryGlAccount")
        public static String updateProductCategoryGlAccount(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "deleteProductCategoryGlAccount",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditProductCategoryGlAccounts")
        @Response(name = "error", type = "view", value = "EditProductCategoryGlAccounts")
        @Event(type = "service", invoke = "deleteProductCategoryGlAccount")
        public static String deleteProductCategoryGlAccount(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

    }

    // Auto-generated split (Part 38)
    public static class Part38 {
        @Request(
            uri = "editVarianceReasonGlAccounts",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditVarianceReasonGlAccounts")
        public interface EditVarianceReasonGlAccounts {}

        @Request(
            uri = "createVarianceReasonGlAccount",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditVarianceReasonGlAccounts")
        @Response(name = "error", type = "view", value = "EditVarianceReasonGlAccounts")
        @Event(type = "service", invoke = "createVarianceReasonGlAccount")
        public static String createVarianceReasonGlAccount(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "updateVarianceReasonGlAccount",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditVarianceReasonGlAccounts")
        @Response(name = "error", type = "view", value = "EditVarianceReasonGlAccounts")
        @Event(type = "service", invoke = "updateVarianceReasonGlAccount")
        public static String updateVarianceReasonGlAccount(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "deleteVarianceReasonGlAccount",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditVarianceReasonGlAccounts")
        @Response(name = "error", type = "view", value = "EditVarianceReasonGlAccounts")
        @Event(type = "service", invoke = "deleteVarianceReasonGlAccount")
        public static String deleteVarianceReasonGlAccount(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "editCreditCardTypeGlAccounts",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditCreditCardTypeGlAccounts")
        public interface EditCreditCardTypeGlAccounts {}

        @Request(
            uri = "createCreditCardTypeGlAccount",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditCreditCardTypeGlAccounts")
        @Response(name = "error", type = "view", value = "EditCreditCardTypeGlAccounts")
        @Event(type = "service", invoke = "createCreditCardTypeGlAccount")
        public static String createCreditCardTypeGlAccount(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "updateCreditCardTypeGlAccount",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditCreditCardTypeGlAccounts")
        @Response(name = "error", type = "view", value = "EditCreditCardTypeGlAccounts")
        @Event(type = "service", invoke = "updateCreditCardTypeGlAccount")
        public static String updateCreditCardTypeGlAccount(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "deleteCreditCardTypeGlAccount",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditCreditCardTypeGlAccounts")
        @Response(name = "error", type = "view", value = "EditCreditCardTypeGlAccounts")
        @Event(type = "service", invoke = "deleteCreditCardTypeGlAccount")
        public static String deleteCreditCardTypeGlAccount(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "findVendors",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "FindVendors")
        public interface FindVendors {}

        @Request(
            uri = "editVendor",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditVendor")
        @Response(name = "error", type = "view", value = "EditVendor")
        public interface EditVendor {}

        @Request(
            uri = "createVendor",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "FindVendors")
        @Response(name = "error", type = "request-redirect", value = "editVendor")
        @Event(type = "service", invoke = "createVendor")
        public static String createVendor(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "updateVendor",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "FindVendors")
        @Response(name = "error", type = "request-redirect", value = "editVendor")
        @Event(type = "service", invoke = "updateVendor")
        public static String updateVendor(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "LookupProduct",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "LookupProduct")
        public interface LookupProduct {}

        @Request(
            uri = "LookupProductFeature",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "LookupProductFeature")
        public interface LookupProductFeature {}

        @Request(
            uri = "LookupVariantProduct",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "LookupVariantProduct")
        public interface LookupVariantProduct {}

        @Request(
            uri = "LookupProductCategory",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "LookupProductCategory")
        public interface LookupProductCategory {}

        @Request(
            uri = "LookupProductStore",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "LookupProductStore")
        public interface LookupProductStore {}

        @Request(
            uri = "LookupPerson",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "LookupPerson")
        public interface LookupPerson {}

        @Request(
            uri = "LookupPartyGroup",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "LookupPartyGroup")
        public interface LookupPartyGroup {}

        @Request(
            uri = "LookupPartyName",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "LookupPartyName")
        public interface LookupPartyName {}

    }

    // Auto-generated split (Part 39)
    public static class Part39 {
        @Request(
            uri = "LookupInternalOrganization",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "LookupInternalOrganization")
        public interface LookupInternalOrganization {}

        @Request(
            uri = "LookupPayment",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "LookupPayment")
        public interface LookupPayment {}

        @Request(
            uri = "LookupInvoice",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "LookupInvoice")
        public interface LookupInvoice {}

        @Request(
            uri = "LookupFixedAsset",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "LookupFixedAsset")
        public interface LookupFixedAsset {}

        @Request(
            uri = "LookupGlAccount",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "LookupGlAccount")
        public interface LookupGlAccount {}

        @Request(
            uri = "LookupBillingAccount",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "LookupBillingAccount")
        public interface LookupBillingAccount {}

        @Request(
            uri = "LookupFacility",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "LookupFacility")
        public interface LookupFacility {}

        @Request(
            uri = "LookupFacilityLocation",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "LookupFacilityLocation")
        public interface LookupFacilityLocation {}

        @Request(
            uri = "LookupShipment",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "LookupShipment")
        public interface LookupShipment {}

        @Request(
            uri = "LookupAgreement",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "LookupAgreement")
        public interface LookupAgreement {}

        @Request(
            uri = "LookupAgreementItem",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "LookupAgreementItem")
        public interface LookupAgreementItem {}

        @Request(
            uri = "LookupPaymentGroupMember",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "LookupPaymentGroupMember")
        public interface LookupPaymentGroupMember {}

        @Request(
            uri = "LookupGlReconciliation",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "LookupGlReconciliation")
        public interface LookupGlReconciliation {}

        @Request(
            uri = "LookupCustomTimePeriod",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "LookupCustomTimePeriod")
        public interface LookupCustomTimePeriod {}

        @Request(
            uri = "LookupTaxAuthorityGeo",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "LookupTaxAuthorityGeo")
        public interface LookupTaxAuthorityGeo {}

        @Request(
            uri = "LookupTaxAuthorityPartyName",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "LookupTaxAuthorityPartyName")
        public interface LookupTaxAuthorityPartyName {}

        @Request(
            uri = "LookupOrderPaymentPreference",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "LookupOrderPaymentPreference")
        public interface LookupOrderPaymentPreference {}

        @Request(
            uri = "viewprofile",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "viewprofile")
        public interface Viewprofile {}

        @Request(
            uri = "invoice.pdf",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "InvoicePDF")
        public interface InvoicePdf {}

        @Request(
            uri = "downloadInvoices",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "DownloadInvoices")
        public interface DownloadInvoices {}

    }

    // Auto-generated split (Part 40)
    public static class Part40 {
        @Request(
            uri = "massDownloadInvoices.pdf",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "error", type = "view", value = "DownloadInvoices")
        @Response(name = "success", type = "none")
        public static String massDownloadInvoicesPdf(HttpServletRequest request, HttpServletResponse response) throws Exception {
            // Delegates to: com.ilscipio.scipio.accounting.invoice.InvoiceEvents.massDownloadInvoices
            return InvoiceEvents.massDownloadInvoices(request, response);
        }

        @Request(
            uri = "printCheck.pdf",
            controller = "accounting"
        )
        @Response(name = "success", type = "view", value = "PrintCheckPDF")
        public interface PrintCheckPdf {}

        @Request(
            uri = "PrintInvoices",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "PrintInvoices")
        public interface PrintInvoices {}

        @Request(
            uri = "listChecksToPrint",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ListChecksToPrint")
        public interface ListChecksToPrint {}

        @Request(
            uri = "listChecksToSend",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ListChecksToSend")
        public interface ListChecksToSend {}

        @Request(
            uri = "printChecks",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "PrintChecks")
        public interface PrintChecks {}

        @Request(
            uri = "quickSendPayment",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ListChecksToSend")
        @Response(name = "error", type = "view", value = "ListChecksToSend")
        @Event(type = "service-multi", invoke = "quickSendPayment")
        public static String quickSendPayment_2(HttpServletRequest request, HttpServletResponse response) throws Exception {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "AcctgTransEntriesSearchResultsCsv.csv",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "AcctgTransEntriesSearchResultsCsv")
        public interface AcctgTransEntriesSearchResultsCsvCsv {}

        @Request(
            uri = "AcctgTransEntriesSearchResultsPdf.pdf",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "AcctgTransEntriesSearchResultsPdf")
        public interface AcctgTransEntriesSearchResultsPdfPdf {}

        @Request(
            uri = "AcctgTransSearchResultsCsv.csv",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "AcctgTransSearchResultsCsv")
        public interface AcctgTransSearchResultsCsvCsv {}

        @Request(
            uri = "AcctgTransSearchResultPdf.pdf",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "AcctgTransSearchResultPdf")
        public interface AcctgTransSearchResultPdfPdf {}

        @Request(
            uri = "TransactionTotalsPdf.pdf",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "TransactionTotalsPdf")
        public interface TransactionTotalsPdfPdf {}

        @Request(
            uri = "TransactionTotalsCsv.csv",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "TransactionTotalsCsv")
        public interface TransactionTotalsCsvCsv {}

        @Request(
            uri = "IncomeStatementListPdf.pdf",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "IncomeStatementListPdf")
        public interface IncomeStatementListPdfPdf {}

        @Request(
            uri = "IncomeStatementListCsv.csv",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "IncomeStatementListCsv")
        public interface IncomeStatementListCsvCsv {}

        @Request(
            uri = "BalanceSheet.csv",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "BalanceSheetCsv")
        public interface BalanceSheetCsv {}

        @Request(
            uri = "TrialBalanceSearchResultsPdf.pdf",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "TrialBalanceSearchResultsPdf")
        public interface TrialBalanceSearchResultsPdfPdf {}

        @Request(
            uri = "TrialBalanceSearchResultsCsv.csv",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "TrialBalanceSearchResultsCsv")
        public interface TrialBalanceSearchResultsCsvCsv {}

        @Request(
            uri = "ComparativeIncomeStatements.csv",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ComparativeIncomeStatementsCsv")
        @Response(name = "error", type = "view", value = "ComparativeIncomeStatement")
        public interface ComparativeIncomeStatementsCsv {}

        @Request(
            uri = "ComparativeIncomeStatements.pdf",
            controller = "accounting",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ComparativeIncomeStatementsPdf")
        @Response(name = "error", type = "view", value = "ComparativeIncomeStatement")
        public interface ComparativeIncomeStatementsPdf {}

    }

    // Auto-generated split (Part 41)
    public static class Part41 {
        @Request(
            uri = "ScpEgltCommon.js",
            controller = "accounting",
            secure = "true"
        )
        @Response(name = "success", type = "view", value = "ScpEgltCommon.js")
        public interface ScpEgltCommonJs {}


    }
}
