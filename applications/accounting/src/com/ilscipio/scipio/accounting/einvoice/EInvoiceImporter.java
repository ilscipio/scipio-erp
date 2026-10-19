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
import java.math.BigDecimal;
import java.math.RoundingMode;
import java.nio.charset.StandardCharsets;
import java.sql.Timestamp;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Set;

import org.mustangproject.Invoice;
import org.mustangproject.TradeParty;
import org.mustangproject.ZUGFeRD.IZUGFeRDAllowanceCharge;
import org.mustangproject.ZUGFeRD.IZUGFeRDExportableItem;
import org.mustangproject.ZUGFeRD.TransactionCalculator;
import org.mustangproject.ZUGFeRD.VATAmount;
import org.mustangproject.ZUGFeRD.ZUGFeRDInvoiceImporter;
import org.ofbiz.accounting.invoice.InvoiceWorker;
import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.UtilDateTime;
import org.ofbiz.base.util.UtilMisc;
import org.ofbiz.base.util.UtilProperties;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.GenericEntityException;
import org.ofbiz.entity.GenericValue;
import org.ofbiz.entity.condition.EntityCondition;
import org.ofbiz.entity.condition.EntityOperator;
import org.ofbiz.entity.util.EntityQuery;
import org.ofbiz.entity.util.EntityUtilProperties;
import org.ofbiz.party.party.PartyHelper;
import org.ofbiz.service.DispatchContext;
import org.ofbiz.service.GenericServiceException;
import org.ofbiz.service.LocalDispatcher;
import org.ofbiz.service.ServiceUtil;

/**
 * Imports a received e-invoice (CII or UBL XML, or a ZUGFeRD / Factur-X PDF) as a purchase invoice in status
 * INVOICE_IN_PROCESS, for review. The original file is kept with the invoice (EINVOICE_ORIGINAL).
 * <p>
 * The supplier is found by VAT ID, then by legal registration ID or tax number, then by electronic address. A
 * credit note (type 381 and similar) becomes a purchase invoice with negative amounts.
 */
public class EInvoiceImporter {

    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());

    private static final Set<String> CREDIT_NOTE_TYPES = new HashSet<>(Arrays.asList("81", "83", "261", "262", "296", "308", "381", "396", "420", "458", "532"));

    /** The parsed file. */
    public static class Parsed {
        private final Invoice invoice;
        private final String syntax;
        private final List<String> warnings;

        Parsed(Invoice invoice, String syntax, List<String> warnings) {
            this.invoice = invoice;
            this.syntax = syntax;
            this.warnings = warnings;
        }

        public Invoice getInvoice() { return invoice; }
        /** CII, UBL or PDF. */
        public String getSyntax() { return syntax; }
        public List<String> getWarnings() { return warnings; }
    }

    private final Delegator delegator;
    private final LocalDispatcher dispatcher;
    private final GenericValue userLogin;
    private final Locale locale;
    private final List<String> warnings = new ArrayList<>();

    public EInvoiceImporter(DispatchContext dctx, GenericValue userLogin, Locale locale) {
        this.delegator = dctx.getDelegator();
        this.dispatcher = dctx.getDispatcher();
        this.userLogin = userLogin;
        this.locale = (locale != null) ? locale : Locale.getDefault();
    }

    public List<String> getWarnings() {
        return warnings;
    }

    /** Reads an e-invoice file. */
    public static Parsed parse(byte[] data, Locale locale) throws EInvoiceException {
        List<String> warnings = new ArrayList<>();
        boolean pdf = data.length > 4 && new String(data, 0, 5, StandardCharsets.ISO_8859_1).startsWith("%PDF");
        String syntax = pdf ? "PDF" : detectXmlSyntax(data);
        if (syntax == null) {
            throw new EInvoiceException(label(locale, "EInvoiceImportUnknownFile"));
        }
        ZUGFeRDInvoiceImporter importer = new ZUGFeRDInvoiceImporter();
        try {
            if (pdf) {
                importer.setInputStream(new ByteArrayInputStream(data));
                if (!importer.canParse()) {
                    throw new EInvoiceException(label(locale, "EInvoiceImportNoXmlInPdf"));
                }
            } else {
                importer.setRawXML(data, true);
            }
            Invoice invoice;
            try {
                invoice = importer.extractInvoice();
            } catch (ArithmeticException e) {
                // the totals in the file do not add up: import it anyway for review, and say so
                warnings.add(label(locale, "EInvoiceImportCalculationError", "message", e.getMessage()));
                importer.doIgnoreCalculationErrors();
                invoice = importer.extractInvoice();
            }
            return new Parsed(invoice, syntax, warnings);
        } catch (EInvoiceException e) {
            throw e;
        } catch (Exception e) {
            Debug.logWarning(e, "E-invoice import: cannot read file: " + e.getMessage(), module);
            throw new EInvoiceException(label(locale, "EInvoiceImportCannotRead", "message", String.valueOf(e.getMessage())), e);
        }
    }

    private static String detectXmlSyntax(byte[] data) {
        String head = new String(data, 0, Math.min(data.length, 4096), StandardCharsets.UTF_8);
        if (head.contains("CrossIndustryInvoice")) {
            return "CII";
        }
        if (head.contains("urn:oasis:names:specification:ubl:schema:xsd:Invoice-2") || head.contains("urn:oasis:names:specification:ubl:schema:xsd:CreditNote-2")) {
            return "UBL";
        }
        return null;
    }

    /**
     * Imports the file as a purchase invoice of the organization and returns the new invoiceId.
     * @param supplierPartyId optional: the supplier the user selected; else found from the file
     */
    public String importInvoice(byte[] data, String fileName, String organizationPartyId, String supplierPartyId) throws EInvoiceException, GenericEntityException {
        Parsed parsed = parse(data, locale);
        warnings.addAll(parsed.getWarnings());
        Invoice einv = parsed.getInvoice();
        TradeParty sender = einv.getSender();
        String number = einv.getNumber();
        if (UtilValidate.isEmpty(number)) {
            throw new EInvoiceException(label("EInvoiceImportNoNumber"));
        }
        if (UtilValidate.isEmpty(supplierPartyId)) {
            supplierPartyId = findSupplier(sender);
            if (supplierPartyId == null) {
                throw new EInvoiceException(label("EInvoiceImportNoSupplier", "name", sender != null ? sender.getName() : "-",
                        "vatId", (sender != null && sender.getVATID() != null) ? sender.getVATID() : "-"));
            }
        }
        GenericValue duplicate = EntityQuery.use(delegator).from("Invoice").where(EntityCondition.makeCondition(UtilMisc.toList(
                EntityCondition.makeCondition("partyIdFrom", supplierPartyId),
                EntityCondition.makeCondition("referenceNumber", number),
                EntityCondition.makeCondition("invoiceTypeId", "PURCHASE_INVOICE"),
                EntityCondition.makeCondition("statusId", EntityOperator.NOT_EQUAL, "INVOICE_CANCELLED")))).queryFirst();
        if (duplicate != null) {
            throw new EInvoiceException(label("EInvoiceImportDuplicate", "number", number, "invoiceId", duplicate.getString("invoiceId")));
        }
        checkRecipient(einv.getRecipient(), organizationPartyId);

        boolean creditNote = CREDIT_NOTE_TYPES.contains(einv.getDocumentCode());
        BigDecimal sign = creditNote ? BigDecimal.ONE.negate() : BigDecimal.ONE;
        TransactionCalculator calc = new TransactionCalculator(einv);

        Map<String, Object> invoiceCtx = new HashMap<>();
        invoiceCtx.put("invoiceTypeId", "PURCHASE_INVOICE");
        invoiceCtx.put("partyIdFrom", supplierPartyId);
        invoiceCtx.put("partyId", organizationPartyId);
        invoiceCtx.put("statusId", "INVOICE_IN_PROCESS");
        invoiceCtx.put("currencyUomId", einv.getCurrency());
        invoiceCtx.put("referenceNumber", number);
        invoiceCtx.put("invoiceDate", einv.getIssueDate() != null ? new Timestamp(einv.getIssueDate().getTime()) : UtilDateTime.nowTimestamp());
        if (einv.getDueDate() != null) {
            invoiceCtx.put("dueDate", new Timestamp(einv.getDueDate().getTime()));
        }
        String supplierName = (sender != null && UtilValidate.isNotEmpty(sender.getName())) ? sender.getName() : PartyHelper.getPartyName(delegator, supplierPartyId, false);
        invoiceCtx.put("description", label(creditNote ? "EInvoiceImportCreditNoteDescription" : "EInvoiceImportDescription", "number", number,
                "name", supplierName));
        StringBuilder message = new StringBuilder();
        if (einv.getNotes() != null) {
            for (String note : einv.getNotes()) {
                if (UtilValidate.isNotEmpty(note)) {
                    message.append(message.length() > 0 ? "\n" : "").append(note);
                }
            }
        }
        if (UtilValidate.isNotEmpty(einv.getPaymentTermDescription())) {
            message.append(message.length() > 0 ? "\n" : "").append(einv.getPaymentTermDescription());
        }
        if (message.length() > 0) {
            invoiceCtx.put("invoiceMessage", message.length() > 255 ? message.substring(0, 255) : message.toString());
        }
        String invoiceId = runSync("createInvoice", invoiceCtx).get("invoiceId").toString();

        // Lines, with line allowances and charges as child items
        for (IZUGFeRDExportableItem item : einv.getZFItems()) {
            BigDecimal quantity = item.getQuantity() != null ? item.getQuantity() : BigDecimal.ONE;
            BigDecimal basis = (item.getBasisQuantity() != null && item.getBasisQuantity().signum() != 0) ? item.getBasisQuantity() : BigDecimal.ONE;
            BigDecimal unitPrice = item.getPrice().divide(basis, 6, RoundingMode.HALF_UP).stripTrailingZeros();
            BigDecimal lineNet = item.getCalculation().getItemTotalNetAmount();
            String productId = findProduct(supplierPartyId, item.getProduct() != null ? item.getProduct().getSellerAssignedID() : null);
            Map<String, Object> itemCtx = UtilMisc.toMap("invoiceId", invoiceId, "quantity", quantity, "amount", unitPrice.multiply(sign),
                    "invoiceItemTypeId", config(productId != null ? "einvoice.import.itemType.product" : "einvoice.import.itemType.other",
                            productId != null ? "PINV_FPROD_ITEM" : "PINV_SUPLPRD_ITEM"),
                    "description", getItemName(item));
            if (productId != null) {
                itemCtx.put("productId", productId);
            }
            String seqId = (String) runSync("createInvoiceItem", itemCtx).get("invoiceItemSeqId");
            BigDecimal adjustment = lineNet.subtract(quantity.multiply(unitPrice).setScale(2, RoundingMode.HALF_UP));
            if (adjustment.signum() != 0) {
                runSync("createInvoiceItem", UtilMisc.toMap("invoiceId", invoiceId, "parentInvoiceId", invoiceId, "parentInvoiceItemSeqId", seqId,
                        "invoiceItemTypeId", adjustment.signum() < 0 ? "PITM_DISCOUNT_ADJ" : "PITM_MISC_CHARGE",
                        "quantity", BigDecimal.ONE, "amount", adjustment.multiply(sign), "description", getReasons(item)));
            }
        }
        // Document level charges and allowances
        if (einv.getZFCharges() != null) {
            for (IZUGFeRDAllowanceCharge charge : einv.getZFCharges()) {
                String typeId = "FC".equals(charge.getReasonCode()) ? "PINV_SHIP_CHARGES" : "PINV_MISC_CHARGE";
                createHeaderItem(invoiceId, typeId, charge.getTotalAmount(calc).multiply(sign), reasonOf(charge, "EInvoiceImportCharge"));
            }
        }
        if (einv.getZFAllowances() != null) {
            for (IZUGFeRDAllowanceCharge allowance : einv.getZFAllowances()) {
                createHeaderItem(invoiceId, "PINV_DISCOUNT_ADJ", allowance.getTotalAmount(calc).negate().multiply(sign), reasonOf(allowance, "EInvoiceImportAllowance"));
            }
        }
        // VAT per rate
        for (VATAmount vat : calc.getTaxDetails()) {
            BigDecimal amount = vat.getCalculated().setScale(2, RoundingMode.HALF_UP);
            if (amount.signum() != 0) {
                createHeaderItem(invoiceId, "PINV_SALES_TAX", amount.multiply(sign),
                        label("EInvoiceImportVat", "percent", vat.getApplicablePercent().stripTrailingZeros().toPlainString(), "category", vat.getCategoryCode()));
            }
        }
        if (einv.getRoundingAmount() != null && einv.getRoundingAmount().signum() != 0) {
            createHeaderItem(invoiceId, "PINV_MISC_CHARGE", einv.getRoundingAmount().multiply(sign), label("EInvoiceImportRounding"));
        }

        // The Scipio total must equal the file total
        GenericValue created = EntityQuery.use(delegator).from("Invoice").where("invoiceId", invoiceId).queryOne();
        BigDecimal expected = calc.getGrandTotal().add(einv.getRoundingAmount() != null ? einv.getRoundingAmount() : BigDecimal.ZERO).multiply(sign).setScale(2, RoundingMode.HALF_UP);
        BigDecimal actual = InvoiceWorker.getInvoiceTotal(created, Boolean.TRUE).setScale(2, RoundingMode.HALF_UP);
        if (expected.compareTo(actual) != 0) {
            warnings.add(label("EInvoiceImportTotalDiffers", "total", actual.toPlainString(), "fileTotal", expected.toPlainString()));
        }
        if (einv.getTotalPrepaidAmount() != null && einv.getTotalPrepaidAmount().signum() != 0) {
            warnings.add(label("EInvoiceImportPrepaid", "amount", einv.getTotalPrepaidAmount().toPlainString()));
        }

        delegator.create("InvoiceAttribute", UtilMisc.toMap("invoiceId", invoiceId, "attrName", "EINV_FORMAT", "attrValue", parsed.getSyntax()));
        String contentType = "PDF".equals(parsed.getSyntax()) ? "application/pdf" : "application/xml";
        EInvoiceArchive.store(delegator, invoiceId, EInvoiceArchive.ORIGINAL,
                new EInvoiceDocument(null, data, UtilValidate.isNotEmpty(fileName) ? fileName : number + ("PDF".equals(parsed.getSyntax()) ? ".pdf" : ".xml"), contentType),
                parsed.getSyntax(), userLogin);
        return invoiceId;
    }

    private void createHeaderItem(String invoiceId, String invoiceItemTypeId, BigDecimal amount, String description) throws EInvoiceException {
        runSync("createInvoiceItem", UtilMisc.toMap("invoiceId", invoiceId, "invoiceItemTypeId", invoiceItemTypeId,
                "quantity", BigDecimal.ONE, "amount", amount, "description", description));
    }

    private Map<String, Object> runSync(String serviceName, Map<String, Object> ctx) throws EInvoiceException {
        ctx.put("userLogin", userLogin);
        ctx.put("locale", locale);
        try {
            Map<String, Object> result = dispatcher.runSync(serviceName, ctx);
            if (ServiceUtil.isError(result)) {
                throw new EInvoiceException(ServiceUtil.getErrorMessage(result));
            }
            return result;
        } catch (GenericServiceException e) {
            throw new EInvoiceException(e.getMessage(), e);
        }
    }

    /** Supplier by VAT ID, legal registration ID / tax number, or electronic address. */
    private String findSupplier(TradeParty sender) throws GenericEntityException {
        if (sender == null) {
            return null;
        }
        String vatId = normalize(sender.getVATID());
        if (vatId != null) {
            for (GenericValue info : EntityQuery.use(delegator).from("PartyTaxAuthInfo").where(EntityCondition.makeCondition("partyTaxId", EntityOperator.NOT_EQUAL, null)).filterByDate().queryList()) {
                if (vatId.equals(withCountryPrefix(info))) {
                    return info.getString("partyId");
                }
            }
        }
        List<String> ids = new ArrayList<>();
        if (sender.getLegalOrganisation() != null && sender.getLegalOrganisation().getSchemedID() != null) {
            ids.add(sender.getLegalOrganisation().getSchemedID().getID());
        }
        if (sender.getTaxID() != null) {
            ids.add(sender.getTaxID());
        }
        for (String id : ids) {
            GenericValue pi = EntityQuery.use(delegator).from("PartyIdentification").where(EntityCondition.makeCondition(UtilMisc.toList(
                    EntityCondition.makeCondition("idValue", id),
                    EntityCondition.makeCondition("partyIdentificationTypeId", EntityOperator.IN, UtilMisc.toList("LEGAL_REG_ID", "TAX_NUMBER"))))).queryFirst();
            if (pi != null) {
                return pi.getString("partyId");
            }
        }
        if (UtilValidate.isNotEmpty(sender.getUriUniversalCommunicationID())) {
            GenericValue pref = EntityQuery.use(delegator).from("PartyEInvoicePref").where("electronicAddress", sender.getUriUniversalCommunicationID()).queryFirst();
            if (pref != null) {
                return pref.getString("partyId");
            }
        }
        return null;
    }

    private void checkRecipient(TradeParty recipient, String organizationPartyId) throws GenericEntityException {
        String vatId = (recipient != null) ? normalize(recipient.getVATID()) : null;
        if (vatId == null) {
            return;
        }
        List<GenericValue> infos = EntityQuery.use(delegator).from("PartyTaxAuthInfo").where("partyId", organizationPartyId).filterByDate().queryList();
        for (GenericValue info : infos) {
            if (vatId.equals(withCountryPrefix(info))) {
                return;
            }
        }
        warnings.add(label("EInvoiceImportRecipientVatDiffers", "vatId", vatId, "partyId", organizationPartyId));
    }

    private String findProduct(String supplierPartyId, String sellerAssignedId) throws GenericEntityException {
        if (UtilValidate.isEmpty(sellerAssignedId)) {
            return null;
        }
        GenericValue supplierProduct = EntityQuery.use(delegator).from("SupplierProduct").where("partyId", supplierPartyId, "supplierProductId", sellerAssignedId).queryFirst();
        return (supplierProduct != null) ? supplierProduct.getString("productId") : null;
    }

    private static String getItemName(IZUGFeRDExportableItem item) {
        if (item.getProduct() == null) {
            return null;
        }
        String name = item.getProduct().getName();
        if (UtilValidate.isNotEmpty(item.getProduct().getSellerAssignedID())) {
            name = item.getProduct().getSellerAssignedID() + " " + (name != null ? name : "");
        }
        return (name != null && name.length() > 255) ? name.substring(0, 255) : name;
    }

    private String getReasons(IZUGFeRDExportableItem item) {
        List<String> reasons = new ArrayList<>();
        for (IZUGFeRDAllowanceCharge[] list : new IZUGFeRDAllowanceCharge[][] {item.getItemAllowances(), item.getItemCharges()}) {
            if (list != null) {
                for (IZUGFeRDAllowanceCharge ac : list) {
                    if (UtilValidate.isNotEmpty(ac.getReason())) {
                        reasons.add(ac.getReason());
                    }
                }
            }
        }
        return reasons.isEmpty() ? label("EInvoiceImportLineAdjustment") : String.join(", ", reasons);
    }

    private String reasonOf(IZUGFeRDAllowanceCharge ac, String defaultKey) {
        return UtilValidate.isNotEmpty(ac.getReason()) ? ac.getReason() : label(defaultKey);
    }

    /**
     * The tax ID of a PartyTaxAuthInfo as a VAT ID with country prefix: as stored if it has a prefix, else with
     * the ISO code of the tax authority country (EL for Greece). Null if it cannot be a VAT ID.
     */
    private String withCountryPrefix(GenericValue info) throws GenericEntityException {
        String taxId = normalize(info.getString("partyTaxId"));
        if (taxId == null || taxId.matches("^[A-Z]{2}.*")) {
            return taxId;
        }
        GenericValue geo = EntityQuery.use(delegator).from("Geo").where("geoId", info.getString("taxAuthGeoId")).cache().queryOne();
        String iso2 = (geo != null) ? geo.getString("geoCode") : null;
        if (iso2 == null || iso2.length() != 2 || "US".equals(iso2)) {
            return null;
        }
        return ("GR".equals(iso2) ? "EL" : iso2) + taxId;
    }

    private static String normalize(String id) {
        return UtilValidate.isEmpty(id) ? null : id.replaceAll("[\\s.-]", "").toUpperCase(Locale.ROOT);
    }

    private String config(String name, String defaultValue) {
        String value = EntityUtilProperties.getPropertyValue(EInvoiceBuilder.CONFIG, name, delegator);
        return UtilValidate.isNotEmpty(value) ? value : defaultValue;
    }

    private String label(String key, Object... args) {
        return label(locale, key, args);
    }

    private static String label(Locale locale, String key, Object... args) {
        return UtilProperties.getMessage(EInvoiceBuilder.RESOURCE, key, UtilMisc.toMap(args), locale);
    }
}
