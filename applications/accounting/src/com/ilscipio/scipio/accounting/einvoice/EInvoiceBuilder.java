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

import java.math.BigDecimal;
import java.math.RoundingMode;
import java.sql.Timestamp;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Set;

import org.mustangproject.Allowance;
import org.mustangproject.BankDetails;
import org.mustangproject.Charge;
import org.mustangproject.Contact;
import org.mustangproject.Invoice;
import org.mustangproject.Item;
import org.mustangproject.LegalOrganisation;
import org.mustangproject.Product;
import org.mustangproject.ReferencedDocument;
import org.mustangproject.SchemedID;
import org.mustangproject.TradeParty;
import org.mustangproject.ZUGFeRD.TransactionCalculator;
import org.ofbiz.accounting.invoice.InvoiceWorker;
import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.UtilDateTime;
import org.ofbiz.base.util.UtilMisc;
import org.ofbiz.base.util.UtilProperties;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.GenericEntityException;
import org.ofbiz.entity.GenericValue;
import org.ofbiz.entity.util.EntityQuery;
import org.ofbiz.entity.util.EntityTypeUtil;
import org.ofbiz.entity.util.EntityUtil;
import org.ofbiz.entity.util.EntityUtilProperties;
import org.ofbiz.party.party.PartyHelper;

/**
 * Builds an EN 16931 invoice (Mustang model) from a Scipio sales invoice or customer return credit note, and
 * collects the problems a user must fix before the e-invoice can be sent.
 * <p>
 * Roles: a sales invoice is sent by partyIdFrom to partyId. A customer return invoice (CUST_RTN_INVOICE) is
 * stored from the customer (partyIdFrom) to the organization (partyId); its e-invoice is a credit note (type 381)
 * that the organization sends to the customer.
 * <p>
 * Amounts: Mustang computes the VAT per rate, as EN 16931 does. Scipio rounds the VAT per line. A difference in
 * the grand total up to the configured tolerance goes to the rounding amount (BT-114), so the amount due stays
 * equal to the Scipio receivable; a larger difference is an error.
 */
public class EInvoiceBuilder {

    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());

    public static final String RESOURCE = "EInvoiceUiLabels";
    public static final String CONFIG = "einvoice";

    private static final String EU_GROUP_GEO_ID = "EU";
    private static final String GTIN_SCHEME = "0160";

    /** The result of a build: the invoice (null if there are errors) and the messages for the user. */
    public static class Result {
        private final Invoice invoice;
        private final List<String> errors;
        private final List<String> warnings;
        private final String invoiceNumber;
        private final boolean creditNote;

        Result(Invoice invoice, List<String> errors, List<String> warnings, String invoiceNumber, boolean creditNote) {
            this.invoice = invoice;
            this.errors = errors;
            this.warnings = warnings;
            this.invoiceNumber = invoiceNumber;
            this.creditNote = creditNote;
        }

        public Invoice getInvoice() { return invoice; }
        public List<String> getErrors() { return errors; }
        public List<String> getWarnings() { return warnings; }
        public String getInvoiceNumber() { return invoiceNumber; }
        public boolean isCreditNote() { return creditNote; }
        public boolean hasErrors() { return !errors.isEmpty(); }
    }

    /** VAT data of one line or charge. */
    private static class Vat {
        BigDecimal percent = BigDecimal.ZERO;
        String category;
        String exemptionCode;
        String exemptionReason;
        /** VAT that is already included in the Scipio amount (gross prices); 0 for net prices. */
        BigDecimal includedTax = BigDecimal.ZERO;
    }

    private final Delegator delegator;
    private final Locale locale;
    /** Language of the texts inside the e-invoice (einvoice.document.locale); messages use {@link #locale}. */
    private final Locale docLocale;
    private final EInvoiceFormat format;
    private final List<String> errors = new ArrayList<>();
    private final List<String> warnings = new ArrayList<>();

    private GenericValue invoice;
    private String invoiceId;
    private boolean creditNote;
    private String sellerPartyId;
    private String buyerPartyId;
    private GenericValue sellerAddress;
    private GenericValue buyerAddress;
    private String sellerVatId;
    private String buyerVatId;
    private String sellerTaxNumber;
    /** The seller is a small business (Germany: Kleinunternehmer, section 19 UStG) and charges no VAT. */
    private boolean sellerSmallBusiness;
    /** A line is exempt (E) with only the generic default text. */
    private boolean genericExemption;
    private Set<String> euCountries;
    /** VAT categories that occur on the invoice. */
    private final Set<String> categories = new HashSet<>();

    protected EInvoiceBuilder(Delegator delegator, EInvoiceFormat format, Locale locale) {
        this.delegator = delegator;
        this.format = format;
        this.locale = (locale != null) ? locale : Locale.getDefault();
        String documentLocale = EntityUtilProperties.getPropertyValue(CONFIG, "einvoice.document.locale", delegator);
        this.docLocale = UtilValidate.isNotEmpty(documentLocale) ? UtilMisc.parseLocale(documentLocale.trim()) : this.locale;
    }

    /**
     * Builds the e-invoice model for the invoice. Never returns null; check {@link Result#hasErrors()}.
     */
    public static Result build(Delegator delegator, String invoiceId, EInvoiceFormat format, Locale locale) throws GenericEntityException {
        return new EInvoiceBuilder(delegator, format, locale).build(invoiceId);
    }

    protected Result build(String invoiceId) throws GenericEntityException {
        this.invoiceId = invoiceId;
        invoice = EntityQuery.use(delegator).from("Invoice").where("invoiceId", invoiceId).queryOne();
        if (invoice == null) {
            error("EInvoiceInvoiceNotFound", "invoiceId", invoiceId);
            return result(null);
        }
        String invoiceTypeId = invoice.getString("invoiceTypeId");
        creditNote = isInvoiceType(invoiceTypeId, "CUST_RTN_INVOICE");
        if (!creditNote && !isInvoiceType(invoiceTypeId, "SALES_INVOICE")) {
            error("EInvoiceNotSalesInvoice", "invoiceTypeId", invoiceTypeId);
            return result(null);
        }
        if ("INVOICE_CANCELLED".equals(invoice.getString("statusId"))) {
            error("EInvoiceInvoiceCancelled");
            return result(null);
        }
        if ("INVOICE_IN_PROCESS".equals(invoice.getString("statusId"))) {
            warning("EInvoiceInvoiceInProcess");
        }
        sellerPartyId = creditNote ? invoice.getString("partyId") : invoice.getString("partyIdFrom");
        buyerPartyId = creditNote ? invoice.getString("partyIdFrom") : invoice.getString("partyId");
        if (UtilValidate.isEmpty(sellerPartyId) || UtilValidate.isEmpty(buyerPartyId)) {
            error("EInvoiceMissingParties");
            return result(null);
        }

        GenericValue sellerPref = getPref(sellerPartyId);
        sellerSmallBusiness = sellerPref != null && "Y".equals(sellerPref.getString("smallBusinessFlag"));
        GenericValue buyerPref = getPref(buyerPartyId);

        Invoice inv = new Invoice();
        inv.setNumber(invoiceId);
        Timestamp invoiceDate = invoice.getTimestamp("invoiceDate");
        inv.setIssueDate(invoiceDate != null ? invoiceDate : UtilDateTime.nowTimestamp());
        inv.setCurrency(invoice.getString("currencyUomId"));
        if (creditNote) {
            inv.setCreditNote();
        }

        sellerAddress = findPartyAddress(sellerPartyId, creditNote ? null : "PAYMENT_LOCATION", "PAYMENT_LOCATION", "BILLING_LOCATION", "GENERAL_LOCATION");
        buyerAddress = findPartyAddress(buyerPartyId, creditNote ? null : "BILLING_LOCATION", "BILLING_LOCATION", "GENERAL_LOCATION");
        sellerVatId = getVatId(sellerPartyId, sellerAddress);
        buyerVatId = getVatId(buyerPartyId, buyerAddress);
        sellerTaxNumber = getPartyIdentification(sellerPartyId, "TAX_NUMBER");

        TradeParty seller = buildParty(sellerPartyId, sellerAddress, sellerPref, true);
        TradeParty buyer = buildParty(buyerPartyId, buyerAddress, buyerPref, false);
        if (seller == null || buyer == null) {
            return result(null);
        }
        addPayment(inv, seller, sellerPref);
        inv.setSender(seller);
        inv.setRecipient(buyer);

        addReferences(inv, buyerPref);
        addLines(inv);
        if (genericExemption) {
            warning("EInvoiceVatExemptGeneric");
        }
        if (!errors.isEmpty()) {
            return result(null);
        }
        addDelivery(inv);
        checkTaxParties();

        BigDecimal applied = InvoiceWorker.getInvoiceApplied(invoice, Boolean.TRUE);
        if (applied != null && applied.signum() > 0) {
            inv.setTotalPrepaidAmount(applied.setScale(2, RoundingMode.HALF_UP));
        }
        reconcileTotals(inv);
        return result(errors.isEmpty() ? inv : null);
    }

    private Result result(Invoice inv) {
        return new Result(inv, errors, warnings, invoiceId, creditNote);
    }

    // ---------------------------------------------------------------------------------------------------------
    // Parties

    private TradeParty buildParty(String partyId, GenericValue address, GenericValue pref, boolean isSeller) throws GenericEntityException {
        String role = label(isSeller ? "EInvoiceSeller" : "EInvoiceBuyer");
        String name = PartyHelper.getPartyName(delegator, partyId, false);
        if (UtilValidate.isEmpty(name)) {
            error("EInvoicePartyNoName", "role", role, "partyId", partyId);
            name = partyId;
        }
        if (address == null) {
            error("EInvoicePartyNoAddress", "role", role, "partyId", partyId);
            return null;
        }
        String country = getIso2(address.getString("countryGeoId"));
        if (country == null) {
            error("EInvoicePartyNoCountry", "role", role, "partyId", partyId);
            return null;
        }
        if (format.isXRechnung() && (UtilValidate.isEmpty(address.getString("city")) || UtilValidate.isEmpty(address.getString("postalCode")))) {
            error("EInvoicePartyNoCity", "role", role, "partyId", partyId);
        }
        TradeParty party = new TradeParty(name, address.getString("address1"), address.getString("postalCode"), address.getString("city"), country);
        if (UtilValidate.isNotEmpty(address.getString("address2"))) {
            party.setAdditionalAddress(address.getString("address2"));
        }
        String vatId = isSeller ? sellerVatId : buyerVatId;
        if (vatId != null) {
            party.setVATID(vatId);
        }
        if (isSeller && sellerTaxNumber != null) {
            party.setTaxID(sellerTaxNumber);
        }
        if (!isSeller) {
            party.setID(partyId); // BT-46
        }
        String legalId = getPartyIdentification(partyId, "LEGAL_REG_ID");
        if (legalId != null) {
            party.setLegalOrganisation(new LegalOrganisation(legalId));
        }

        // Electronic address (BT-34 / BT-49): the configured address, else the email address with scheme EM
        String email = getPartyEmail(partyId, pref, isSeller);
        String electronicAddress = (pref != null) ? pref.getString("electronicAddress") : null;
        if (UtilValidate.isNotEmpty(electronicAddress)) {
            String scheme = pref.getString("electronicAddressScheme");
            party.addUriUniversalCommunicationID(new SchemedID(UtilValidate.isNotEmpty(scheme) ? scheme : "EM", electronicAddress));
        } else if (format.isPeppol()) {
            // Peppol endpoints need a registered participant ID; the scheme EM (email) is not in the Peppol EAS list
            error("EInvoicePartyNoPeppolId", "role", role, "partyId", partyId);
        } else if (email != null) {
            party.setEmail(email);
        } else if (format.isXRechnung()) {
            error("EInvoicePartyNoElectronicAddress", "role", role, "partyId", partyId);
        }

        // Contact (BG-6 / BG-9); XRechnung requires the seller contact with name, phone and email
        String contactName = (pref != null) ? pref.getString("contactName") : null;
        String contactPhone = (pref != null) ? pref.getString("contactPhone") : null;
        String contactEmail = (pref != null) ? pref.getString("contactEmail") : null;
        if (isSeller) {
            if (UtilValidate.isEmpty(contactName)) {
                contactName = name;
            }
            if (UtilValidate.isEmpty(contactPhone)) {
                contactPhone = getPartyPhone(partyId);
            }
            if (UtilValidate.isEmpty(contactEmail)) {
                contactEmail = email;
            }
            if (format.isXRechnung() && (UtilValidate.isEmpty(contactPhone) || UtilValidate.isEmpty(contactEmail))) {
                error("EInvoiceSellerNoContact", "partyId", partyId);
            }
        }
        if (UtilValidate.isNotEmpty(contactName) || UtilValidate.isNotEmpty(contactPhone) || UtilValidate.isNotEmpty(contactEmail)) {
            Contact contact = new Contact();
            if (UtilValidate.isNotEmpty(contactName)) {
                contact.setName(contactName);
            }
            if (UtilValidate.isNotEmpty(contactPhone)) {
                contact.setPhone(contactPhone);
            }
            if (UtilValidate.isNotEmpty(contactEmail)) {
                contact.setEMail(contactEmail);
            }
            party.setContact(contact);
        }
        return party;
    }

    private void addPayment(Invoice inv, TradeParty seller, GenericValue sellerPref) throws GenericEntityException {
        GenericValue eft = getSellerBankAccount(sellerPref);
        String iban = (eft != null) ? getIban(eft) : null;
        if (iban != null) {
            BankDetails bank = new BankDetails(iban);
            String bic = eft.getString("bicCode");
            if (UtilValidate.isNotEmpty(bic)) {
                bank.setBIC(bic.replace(" ", ""));
            }
            String accountName = UtilValidate.isNotEmpty(eft.getString("companyNameOnAccount")) ? eft.getString("companyNameOnAccount") : eft.getString("nameOnAccount");
            if (UtilValidate.isNotEmpty(accountName)) {
                bank.setAccountName(accountName);
            }
            boolean sepa = "EUR".equals(invoice.getString("currencyUomId"));
            bank.setPaymentMeansCode(config(sepa ? "einvoice.paymentMeans.iban" : "einvoice.paymentMeans.other", sepa ? "58" : "30"));
            seller.addBankDetails(bank);
        } else if (!creditNote) {
            if (format.isXRechnung()) {
                error("EInvoiceSellerNoIban", "partyId", sellerPartyId);
            } else {
                warning("EInvoiceSellerNoIban", "partyId", sellerPartyId);
            }
        }

        // Due date (BT-9) and payment terms (BT-20)
        Timestamp dueDate = invoice.getTimestamp("dueDate");
        List<GenericValue> terms = EntityQuery.use(delegator).from("InvoiceTerm").where("invoiceId", invoiceId).queryList();
        StringBuilder termText = new StringBuilder();
        StringBuilder skontoLines = new StringBuilder();
        for (GenericValue term : terms) {
            String termTypeId = term.getString("termTypeId");
            Long days = term.getLong("termDays");
            BigDecimal value = term.getBigDecimal("termValue");
            if ("FIN_PAYMENT_TERM".equals(termTypeId) && days != null) {
                if (dueDate == null && invoice.getTimestamp("invoiceDate") != null) {
                    dueDate = UtilDateTime.addDaysToTimestamp(invoice.getTimestamp("invoiceDate"), days.intValue());
                }
                appendLine(termText, docLabel("EInvoicePaymentTermDays", "days", days));
            } else if ("FIN_PAYMENT_DISC".equals(termTypeId) && days != null && value != null) {
                if (format.isXRechnung()) {
                    // XRechnung cash discount syntax (BT-20, BR-DE-18): one line each, at the end, each ended by a line break
                    skontoLines.append("#SKONTO#TAGE=").append(days).append("#PROZENT=").append(value.setScale(2, RoundingMode.HALF_UP).toPlainString()).append("#\n");
                } else {
                    appendLine(termText, docLabel("EInvoicePaymentTermDiscount", "percent", value.stripTrailingZeros().toPlainString(), "days", days));
                }
            } else if (UtilValidate.isNotEmpty(term.getString("textValue"))) {
                appendLine(termText, term.getString("textValue"));
            }
        }
        if (dueDate != null) {
            inv.setDueDate(dueDate);
        }
        if (termText.length() == 0) {
            termText.append(dueDate != null ? docLabel("EInvoicePaymentTermDueDate", "dueDate", new SimpleDateFormat("yyyy-MM-dd").format(dueDate))
                    : docLabel("EInvoicePaymentTermImmediate"));
        }
        if (skontoLines.length() > 0) {
            termText.append('\n').append(skontoLines);
        }
        inv.setPaymentTermDescription(termText.toString());
    }

    private void addReferences(Invoice inv, GenericValue buyerPref) throws GenericEntityException {
        String orderId = InvoiceWorker.getOrderIdByInvoiceId(delegator, invoiceId);
        if (orderId != null) {
            inv.setSellerOrderReferencedDocument(new ReferencedDocument(orderId)); // BT-14
        }
        // Buyer reference (BT-10), e.g. the Leitweg-ID
        String buyerReference = getInvoiceAttribute("EINV_BUYER_REFERENCE");
        if (buyerReference == null && orderId != null) {
            GenericValue orderAttr = EntityQuery.use(delegator).from("OrderAttribute").where("orderId", orderId, "attrName", "BUYER_REFERENCE").queryOne();
            buyerReference = (orderAttr != null) ? UtilValidate.nullIfEmpty(orderAttr.getString("attrValue")) : null;
        }
        if (buyerReference == null && buyerPref != null) {
            buyerReference = UtilValidate.nullIfEmpty(buyerPref.getString("buyerReference"));
        }
        if (buyerReference == null) {
            buyerReference = buyerPartyId;
            if (format.isXRechnung()) {
                warning("EInvoiceNoBuyerReference", "partyId", buyerPartyId);
            }
        }
        inv.setReferenceNumber(buyerReference);

        // Preceding invoice (BT-25) of a credit note
        if (creditNote) {
            String preceding = getInvoiceAttribute("EINV_PRECEDING_INVOICE");
            if (preceding == null) {
                preceding = findReturnedInvoiceId();
            }
            if (preceding != null) {
                GenericValue precedingInvoice = EntityQuery.use(delegator).from("Invoice").where("invoiceId", preceding).cache().queryOne();
                Timestamp precedingDate = (precedingInvoice != null) ? precedingInvoice.getTimestamp("invoiceDate") : null;
                inv.addInvoiceReferencedDocument(precedingDate != null ? new ReferencedDocument(preceding, precedingDate) : new ReferencedDocument(preceding));
            } else {
                warning("EInvoiceCreditNoteNoPreceding");
            }
        }
        if (UtilValidate.isNotEmpty(invoice.getString("description"))) {
            inv.addNote(invoice.getString("description"));
        }
        if (UtilValidate.isNotEmpty(invoice.getString("invoiceMessage"))) {
            inv.addNote(invoice.getString("invoiceMessage"));
        }
    }

    // ---------------------------------------------------------------------------------------------------------
    // Lines, allowances and charges

    private void addLines(Invoice inv) throws GenericEntityException {
        List<GenericValue> items = EntityQuery.use(delegator).from("InvoiceItem").where("invoiceId", invoiceId).orderBy("invoiceItemSeqId").queryList();
        Set<String> taxTypes = new HashSet<>(InvoiceWorker.getTaxableInvoiceItemTypeIds(delegator));
        Map<String, List<GenericValue>> taxByParent = new HashMap<>();
        Map<String, List<GenericValue>> adjByParent = new HashMap<>();
        List<GenericValue> lines = new ArrayList<>();
        List<GenericValue> docAdjustments = new ArrayList<>();
        for (GenericValue item : items) {
            String typeId = item.getString("invoiceItemTypeId");
            String parentSeqId = getParentSeqId(item);
            if (taxTypes.contains(typeId) || typeId.contains("_VAT_")) {
                if (parentSeqId != null) {
                    taxByParent.computeIfAbsent(parentSeqId, k -> new ArrayList<>()).add(item);
                }
                // tax without a parent: its amount is part of the invoice VAT; Mustang computes the VAT from the rates
            } else if (isAdjustment(typeId)) {
                if (parentSeqId != null) {
                    adjByParent.computeIfAbsent(parentSeqId, k -> new ArrayList<>()).add(item);
                } else {
                    docAdjustments.add(item);
                }
            } else {
                lines.add(item);
            }
        }
        if (lines.isEmpty()) {
            error("EInvoiceNoLines");
            return;
        }

        // Lines (BG-25)
        Map<BigDecimal, BigDecimal> basisByRate = new LinkedHashMap<>();
        Map<BigDecimal, Vat> vatByRate = new HashMap<>();
        for (GenericValue line : lines) {
            String seqId = line.getString("invoiceItemSeqId");
            BigDecimal quantity = line.getBigDecimal("quantity") != null ? line.getBigDecimal("quantity") : BigDecimal.ONE;
            BigDecimal amount = line.getBigDecimal("amount") != null ? line.getBigDecimal("amount") : BigDecimal.ZERO;
            BigDecimal lineTotal = InvoiceWorker.getInvoiceItemTotal(line);
            Vat vat = getVat(taxByParent.get(seqId), line, lineTotal);

            BigDecimal price = amount;
            if (vat.includedTax.signum() != 0 && quantity.signum() != 0) {
                // gross price: EN 16931 needs the net price
                price = lineTotal.subtract(vat.includedTax).divide(quantity, 4, RoundingMode.HALF_UP);
            }
            if (price.signum() < 0) {
                // BR-27: the item price must not be negative; a negative line has a negative quantity
                price = price.negate();
                quantity = quantity.negate();
            }
            Product product = new Product(getLineName(line), null, getUnitCode(line), vat.percent);
            product.setTaxCategoryCode(vat.category);
            if (vat.exemptionCode != null) {
                product.setTaxExemptionReasonCode(vat.exemptionCode);
            }
            if (vat.exemptionReason != null) {
                product.setTaxExemptionReason(vat.exemptionReason);
            }
            String productId = line.getString("productId");
            if (UtilValidate.isNotEmpty(productId)) {
                product.setSellerAssignedID(productId);
                String gtin = getGtin(productId);
                if (gtin != null) {
                    product.addGlobalID(new SchemedID(GTIN_SCHEME, gtin));
                }
            }
            Item item = new Item(product, price, quantity);
            item.setId(seqId);
            BigDecimal lineNet = price.multiply(quantity);
            for (GenericValue adj : adjByParent.getOrDefault(seqId, Collections.emptyList())) {
                BigDecimal adjAmount = InvoiceWorker.getInvoiceItemTotal(adj);
                String reason = getItemDescription(adj);
                if (adjAmount.signum() < 0) {
                    item.addAllowance(new Allowance(adjAmount.negate()).setReason(reason).setReasonCode("95"));
                } else if (adjAmount.signum() > 0) {
                    item.addCharge(new Charge(adjAmount).setReason(reason));
                }
                lineNet = lineNet.add(adjAmount);
            }
            inv.addItem(item);
            BigDecimal key = vat.percent.stripTrailingZeros();
            basisByRate.merge(key, lineNet, BigDecimal::add);
            vatByRate.putIfAbsent(key, vat);
        }

        // Document level allowances (BG-20) and charges (BG-21)
        Vat mainVat = null;
        BigDecimal mainBasis = null;
        for (Map.Entry<BigDecimal, BigDecimal> entry : basisByRate.entrySet()) {
            if (mainBasis == null || entry.getValue().abs().compareTo(mainBasis) > 0) {
                mainBasis = entry.getValue().abs();
                mainVat = vatByRate.get(entry.getKey());
            }
        }
        for (GenericValue adj : docAdjustments) {
            BigDecimal adjAmount = InvoiceWorker.getInvoiceItemTotal(adj);
            if (adjAmount.signum() == 0) {
                continue;
            }
            List<GenericValue> adjTax = taxByParent.get(adj.getString("invoiceItemSeqId"));
            Vat vat = UtilValidate.isNotEmpty(adjTax) ? getVat(adjTax, adj, adjAmount) : mainVat;
            String reason = getItemDescription(adj);
            if (adjAmount.signum() < 0) {
                Allowance allowance = new Allowance(adjAmount.negate()).setReason(reason).setReasonCode("95");
                allowance.setTaxRateApplicablePercent(vat.percent);
                allowance.setTaxCategoryCode(vat.category);
                inv.addAllowance(allowance);
            } else {
                Charge charge = new Charge(adjAmount).setReason(reason);
                if (adj.getString("invoiceItemTypeId").contains("SHIP")) {
                    charge.setReasonCode("FC");
                }
                charge.setTaxRateApplicablePercent(vat.percent);
                charge.setTaxCategoryCode(vat.category);
                inv.addCharge(charge);
            }
        }
    }

    /** VAT of a line or charge: from its tax items, from VAT included in the price, or by the rules for no tax. */
    private Vat getVat(List<GenericValue> taxItems, GenericValue item, BigDecimal amount) throws GenericEntityException {
        Vat vat = new Vat();
        if (UtilValidate.isNotEmpty(taxItems)) {
            BigDecimal percent = BigDecimal.ZERO;
            BigDecimal taxAmount = BigDecimal.ZERO;
            Set<String> rateIds = new HashSet<>();
            boolean ratesKnown = true;
            for (GenericValue taxItem : taxItems) {
                taxAmount = taxAmount.add(InvoiceWorker.getInvoiceItemTotal(taxItem));
                String rateSeqId = taxItem.getString("taxAuthorityRateSeqId");
                GenericValue rate = (rateSeqId != null) ? EntityQuery.use(delegator).from("TaxAuthorityRateProduct").where("taxAuthorityRateSeqId", rateSeqId).cache().queryOne() : null;
                if (rate == null || rate.getBigDecimal("taxPercentage") == null) {
                    ratesKnown = false;
                } else if (rateIds.add(rateSeqId)) {
                    percent = percent.add(rate.getBigDecimal("taxPercentage"));
                    applyRateCodes(vat, rate);
                }
            }
            if (!ratesKnown && amount.signum() != 0) {
                percent = taxAmount.multiply(BigDecimal.valueOf(100)).divide(amount, 2, RoundingMode.HALF_UP);
            }
            vat.percent = percent;
        } else {
            GenericValue vatAdj = getIncludedVatAdjustment(item);
            if (vatAdj != null) {
                BigDecimal included = InvoiceWorker.getInvoiceItemTaxIncluded(item);
                vat.includedTax = (included != null) ? included : BigDecimal.ZERO;
                GenericValue rate = vatAdj.getRelatedOne("TaxAuthorityRateProduct", true);
                BigDecimal percent = vatAdj.getBigDecimal("sourcePercentage");
                if (percent == null && rate != null) {
                    percent = rate.getBigDecimal("taxPercentage");
                }
                vat.percent = (percent != null) ? percent : BigDecimal.ZERO;
                if (rate != null) {
                    applyRateCodes(vat, rate);
                }
            }
        }
        if (vat.category == null) {
            if (vat.percent.signum() > 0) {
                vat.category = "S";
            } else {
                applyNoTaxCategory(vat);
            }
        }
        applyDefaultExemption(vat);
        categories.add(vat.category);
        return vat;
    }

    private void applyRateCodes(Vat vat, GenericValue rate) {
        if (vat.category == null && UtilValidate.isNotEmpty(rate.getString("vatCategoryCode"))) {
            vat.category = rate.getString("vatCategoryCode");
        }
        if (vat.exemptionCode == null && UtilValidate.isNotEmpty(rate.getString("exemptionReasonCode"))) {
            vat.exemptionCode = rate.getString("exemptionReasonCode");
        }
        if (vat.exemptionReason == null && UtilValidate.isNotEmpty(rate.getString("exemptionReason"))) {
            vat.exemptionReason = (String) rate.get("exemptionReason", docLocale);
        }
    }

    /**
     * The exemption code (VATEX) and text of a category without VAT, when the tax rate gives none. EN 16931
     * requires one of them (BR-E-10, BR-AE-10, BR-IC-10, BR-G-10, BR-O-10); the texts are those of German VAT law.
     */
    private void applyDefaultExemption(Vat vat) {
        if (vat.exemptionCode != null || vat.exemptionReason != null) {
            return;
        }
        if ("K".equals(vat.category)) {
            vat.exemptionCode = "VATEX-EU-IC";
            vat.exemptionReason = docLabel("EInvoiceVatIntraCommunity");
        } else if ("G".equals(vat.category)) {
            vat.exemptionCode = "VATEX-EU-G";
            vat.exemptionReason = docLabel("EInvoiceVatExport");
        } else if ("AE".equals(vat.category)) {
            vat.exemptionCode = "VATEX-EU-AE";
            vat.exemptionReason = docLabel("EInvoiceVatReverseCharge");
        } else if ("O".equals(vat.category)) {
            vat.exemptionCode = "VATEX-EU-O";
            vat.exemptionReason = docLabel("EInvoiceVatNotSubject");
        } else if ("E".equals(vat.category)) {
            // the real reason depends on the supply (section 4 UStG, ...): the check asks for it
            vat.exemptionReason = docLabel("EInvoiceVatExempt");
            genericExemption = true;
        }
    }

    /**
     * No VAT on the line: small business (E with its note), intra-community supply (K), export (G), or the
     * configured category. {@link #applyDefaultExemption} adds the code and the text.
     */
    private void applyNoTaxCategory(Vat vat) throws GenericEntityException {
        if (sellerSmallBusiness) {
            vat.category = "E";
            vat.exemptionReason = docLabel("EInvoiceVatSmallBusiness");
            return;
        }
        String sellerCountry = (sellerAddress != null) ? sellerAddress.getString("countryGeoId") : null;
        String buyerCountry = (buyerAddress != null) ? buyerAddress.getString("countryGeoId") : null;
        if (sellerCountry != null && buyerCountry != null && !sellerCountry.equals(buyerCountry) && isEuCountry(sellerCountry)) {
            if (isEuCountry(buyerCountry) && buyerVatId != null) {
                vat.category = "K";
                return;
            } else if (!isEuCountry(buyerCountry)) {
                vat.category = "G";
                return;
            }
        }
        vat.category = config("einvoice.vat.category.noTax", "E");
        String reason = config("einvoice.vat.exemptionReason.noTax", null);
        if (reason != null) {
            vat.exemptionReason = reason;
        }
    }

    private GenericValue getIncludedVatAdjustment(GenericValue item) throws GenericEntityException {
        GenericValue billing = EntityQuery.use(delegator).from("OrderItemBilling")
                .where("invoiceId", invoiceId, "invoiceItemSeqId", item.getString("invoiceItemSeqId")).queryFirst();
        if (billing == null) {
            return null;
        }
        List<GenericValue> adjustments = EntityQuery.use(delegator).from("OrderAdjustment")
                .where("orderId", billing.getString("orderId"), "orderItemSeqId", billing.getString("orderItemSeqId"), "orderAdjustmentTypeId", "VAT_TAX")
                .queryList();
        for (GenericValue adj : adjustments) {
            BigDecimal included = adj.getBigDecimal("amountAlreadyIncluded");
            if (included != null && included.signum() != 0) {
                return adj;
            }
        }
        return null;
    }

    // ---------------------------------------------------------------------------------------------------------
    // Delivery, checks, totals

    private void addDelivery(Invoice inv) throws GenericEntityException {
        // Actual delivery date (BT-72): the last item issuance of the invoiced order items, else the invoice date
        Timestamp deliveryDate = null;
        List<GenericValue> billings = EntityQuery.use(delegator).from("OrderItemBilling").where("invoiceId", invoiceId).queryList();
        for (GenericValue billing : billings) {
            String issuanceId = billing.getString("itemIssuanceId");
            if (issuanceId != null) {
                GenericValue issuance = EntityQuery.use(delegator).from("ItemIssuance").where("itemIssuanceId", issuanceId).queryOne();
                Timestamp issued = (issuance != null) ? issuance.getTimestamp("issuedDateTime") : null;
                if (issued != null && (deliveryDate == null || issued.after(deliveryDate))) {
                    deliveryDate = issued;
                }
            }
        }
        inv.setDeliveryDate(deliveryDate != null ? deliveryDate : (Date) inv.getIssueDate());

        // Deliver-to address (BG-15): needed for intra-community supply (BR-IC-12)
        GenericValue shipTo = creditNote ? null : InvoiceWorker.getShippingAddress(invoice);
        if (shipTo == null && categories.contains("K")) {
            shipTo = buyerAddress;
        }
        if (shipTo != null && "PostalAddress".equals(shipTo.getEntityName())) {
            String country = getIso2(shipTo.getString("countryGeoId"));
            if (country != null) {
                TradeParty deliveryParty = new TradeParty(UtilValidate.isNotEmpty(shipTo.getString("toName")) ? shipTo.getString("toName") : PartyHelper.getPartyName(delegator, buyerPartyId, false),
                        shipTo.getString("address1"), shipTo.getString("postalCode"), shipTo.getString("city"), country);
                inv.setDeliveryAddress(deliveryParty);
            }
        }
    }

    /** The VAT IDs that EN 16931 requires for the VAT categories on the invoice. */
    private void checkTaxParties() {
        boolean needsSellerId = false;
        for (String category : categories) {
            if (!"O".equals(category)) {
                needsSellerId = true;
            }
        }
        if (needsSellerId && sellerVatId == null && sellerTaxNumber == null) {
            error("EInvoiceSellerNoVatId", "partyId", sellerPartyId);
        }
        if ((categories.contains("K") || categories.contains("AE")) && sellerVatId == null) {
            error("EInvoiceSellerNoVatId", "partyId", sellerPartyId);
        }
        if ((categories.contains("K") || categories.contains("AE")) && buyerVatId == null) {
            error("EInvoiceBuyerNoVatId", "partyId", buyerPartyId);
        }
    }

    private void reconcileTotals(Invoice inv) {
        TransactionCalculator calc = new TransactionCalculator(inv);
        BigDecimal eTotal = calc.getGrandTotal().setScale(2, RoundingMode.HALF_UP);
        BigDecimal sTotal = InvoiceWorker.getInvoiceTotal(invoice, Boolean.TRUE).setScale(2, RoundingMode.HALF_UP);
        BigDecimal diff = sTotal.subtract(eTotal);
        if (diff.signum() == 0) {
            return;
        }
        BigDecimal tolerance = new BigDecimal(config("einvoice.rounding.tolerance.min", "0.05"))
                .max(new BigDecimal(config("einvoice.rounding.tolerance.perLine", "0.01")).multiply(BigDecimal.valueOf(inv.getZFItems().length)));
        if (diff.abs().compareTo(tolerance) <= 0) {
            inv.setRoundingAmount(diff); // BT-114
            warning("EInvoiceRoundingDifference", "eTotal", eTotal.toPlainString(), "total", sTotal.toPlainString(), "difference", diff.toPlainString());
        } else {
            error("EInvoiceTotalMismatch", "eTotal", eTotal.toPlainString(), "total", sTotal.toPlainString());
        }
    }

    // ---------------------------------------------------------------------------------------------------------
    // Data helpers

    private boolean isInvoiceType(String invoiceTypeId, String parentTypeId) {
        return EntityTypeUtil.hasParentType(delegator, "InvoiceType", "invoiceTypeId", invoiceTypeId, "parentTypeId", parentTypeId);
    }

    private boolean isAdjustment(String invoiceItemTypeId) {
        return EntityTypeUtil.hasParentType(delegator, "InvoiceItemType", "invoiceItemTypeId", invoiceItemTypeId, "parentTypeId", "INVOICE_ADJ");
    }

    private String getParentSeqId(GenericValue item) {
        String parentSeqId = item.getString("parentInvoiceItemSeqId");
        String parentInvoiceId = item.getString("parentInvoiceId");
        if (parentSeqId == null || (parentInvoiceId != null && !parentInvoiceId.equals(invoiceId))) {
            return null;
        }
        return parentSeqId;
    }

    private GenericValue getPref(String partyId) throws GenericEntityException {
        return EntityQuery.use(delegator).from("PartyEInvoicePref").where("partyId", partyId).queryOne();
    }

    private String getInvoiceAttribute(String attrName) throws GenericEntityException {
        GenericValue attr = EntityQuery.use(delegator).from("InvoiceAttribute").where("invoiceId", invoiceId, "attrName", attrName).queryOne();
        return (attr != null) ? UtilValidate.nullIfEmpty(attr.getString("attrValue")) : null;
    }

    /** The original invoice of a customer return credit note: return item, order item, order item billing. */
    private String findReturnedInvoiceId() throws GenericEntityException {
        List<GenericValue> returnBillings = EntityQuery.use(delegator).from("ReturnItemBilling").where("invoiceId", invoiceId).queryList();
        for (GenericValue returnBilling : returnBillings) {
            GenericValue returnItem = EntityQuery.use(delegator).from("ReturnItem")
                    .where("returnId", returnBilling.getString("returnId"), "returnItemSeqId", returnBilling.getString("returnItemSeqId")).queryOne();
            if (returnItem == null || returnItem.getString("orderId") == null) {
                continue;
            }
            GenericValue orderBilling = EntityQuery.use(delegator).from("OrderItemBilling")
                    .where("orderId", returnItem.getString("orderId"), "orderItemSeqId", returnItem.getString("orderItemSeqId")).queryFirst();
            if (orderBilling != null) {
                return orderBilling.getString("invoiceId");
            }
        }
        return null;
    }

    /**
     * The postal address of a party: the invoice contact mech of the given purpose (if any), else the first
     * current party address with one of the purposes.
     */
    private GenericValue findPartyAddress(String partyId, String invoicePurpose, String... purposes) throws GenericEntityException {
        if (invoicePurpose != null) {
            GenericValue icm = EntityQuery.use(delegator).from("InvoiceContactMech").where("invoiceId", invoiceId, "contactMechPurposeTypeId", invoicePurpose).queryFirst();
            if (icm != null) {
                GenericValue address = EntityQuery.use(delegator).from("PostalAddress").where("contactMechId", icm.getString("contactMechId")).queryOne();
                if (address != null) {
                    return address;
                }
            }
        }
        for (String purpose : purposes) {
            GenericValue pcm = getPartyContact(partyId, purpose, "POSTAL_ADDRESS");
            if (pcm != null) {
                GenericValue address = EntityQuery.use(delegator).from("PostalAddress").where("contactMechId", pcm.getString("contactMechId")).queryOne();
                if (address != null) {
                    return address;
                }
            }
        }
        return null;
    }

    private GenericValue getPartyContact(String partyId, String purpose, String contactMechTypeId) throws GenericEntityException {
        List<GenericValue> contacts = EntityQuery.use(delegator).from("PartyContactWithPurpose")
                .where("partyId", partyId, "contactMechPurposeTypeId", purpose, "contactMechTypeId", contactMechTypeId).queryList();
        Timestamp now = UtilDateTime.nowTimestamp();
        contacts = EntityUtil.filterByDate(contacts, now, "contactFromDate", "contactThruDate", true);
        contacts = EntityUtil.filterByDate(contacts, now, "purposeFromDate", "purposeThruDate", true);
        return EntityUtil.getFirst(contacts);
    }

    private String getPartyEmail(String partyId, GenericValue pref, boolean isSeller) throws GenericEntityException {
        if (!isSeller && pref != null && UtilValidate.isNotEmpty(pref.getString("einvoiceEmail"))) {
            return pref.getString("einvoiceEmail");
        }
        if (!isSeller && !creditNote) {
            GenericValue icm = EntityQuery.use(delegator).from("InvoiceContactMech").where("invoiceId", invoiceId, "contactMechPurposeTypeId", "BILLING_EMAIL").queryFirst();
            if (icm != null) {
                GenericValue cm = EntityQuery.use(delegator).from("ContactMech").where("contactMechId", icm.getString("contactMechId")).queryOne();
                if (cm != null && UtilValidate.isNotEmpty(cm.getString("infoString"))) {
                    return cm.getString("infoString");
                }
            }
        }
        for (String purpose : isSeller ? new String[] {"BILLING_EMAIL", "PAYMENT_EMAIL", "PRIMARY_EMAIL"} : new String[] {"BILLING_EMAIL", "PRIMARY_EMAIL"}) {
            GenericValue pcm = getPartyContact(partyId, purpose, "EMAIL_ADDRESS");
            if (pcm != null && UtilValidate.isNotEmpty(pcm.getString("infoString"))) {
                return pcm.getString("infoString");
            }
        }
        return null;
    }

    private String getPartyPhone(String partyId) throws GenericEntityException {
        for (String purpose : new String[] {"PHONE_BILLING", "PRIMARY_PHONE", "PHONE_WORK"}) {
            GenericValue pcm = getPartyContact(partyId, purpose, "TELECOM_NUMBER");
            if (pcm != null) {
                GenericValue tn = EntityQuery.use(delegator).from("TelecomNumber").where("contactMechId", pcm.getString("contactMechId")).queryOne();
                if (tn != null && UtilValidate.isNotEmpty(tn.getString("contactNumber"))) {
                    StringBuilder sb = new StringBuilder();
                    if (UtilValidate.isNotEmpty(tn.getString("countryCode"))) {
                        sb.append('+').append(tn.getString("countryCode").replace("+", "")).append(' ');
                    }
                    if (UtilValidate.isNotEmpty(tn.getString("areaCode"))) {
                        sb.append(tn.getString("areaCode")).append(' ');
                    }
                    return sb.append(tn.getString("contactNumber")).toString();
                }
            }
        }
        return null;
    }

    private String getPartyIdentification(String partyId, String typeId) throws GenericEntityException {
        GenericValue pi = EntityQuery.use(delegator).from("PartyIdentification").where("partyId", partyId, "partyIdentificationTypeId", typeId).queryFirst();
        return (pi != null) ? UtilValidate.nullIfEmpty(pi.getString("idValue")) : null;
    }

    /**
     * The VAT ID of a party with its country prefix: the PartyTaxAuthInfo of the party's country first, then of
     * any other EU country. US tax IDs are not VAT IDs.
     */
    private String getVatId(String partyId, GenericValue address) throws GenericEntityException {
        List<GenericValue> infos = EntityQuery.use(delegator).from("PartyTaxAuthInfo").where("partyId", partyId).filterByDate().queryList();
        String country = (address != null) ? address.getString("countryGeoId") : null;
        GenericValue best = null;
        for (GenericValue info : infos) {
            String geoId = info.getString("taxAuthGeoId");
            if (UtilValidate.isEmpty(info.getString("partyTaxId")) || "USA".equals(geoId) || "_NA_".equals(geoId)) {
                continue;
            }
            if (geoId.equals(country)) {
                best = info;
                break;
            }
            if (best == null && isEuCountry(geoId)) {
                best = info;
            }
        }
        if (best == null) {
            return null;
        }
        String vatId = best.getString("partyTaxId").replaceAll("[\\s.-]", "").toUpperCase(Locale.ROOT);
        if (!vatId.matches("^[A-Z]{2}.*")) {
            String iso2 = getIso2(best.getString("taxAuthGeoId"));
            if (iso2 != null) {
                vatId = ("GR".equals(iso2) ? "EL" : iso2) + vatId;
            }
        }
        return vatId;
    }

    private GenericValue getSellerBankAccount(GenericValue sellerPref) throws GenericEntityException {
        if (sellerPref != null && UtilValidate.isNotEmpty(sellerPref.getString("paymentMethodId"))) {
            GenericValue eft = EntityQuery.use(delegator).from("EftAccount").where("paymentMethodId", sellerPref.getString("paymentMethodId")).queryOne();
            if (eft != null) {
                return eft;
            }
        }
        List<GenericValue> methods = EntityQuery.use(delegator).from("PaymentMethod")
                .where("partyId", sellerPartyId, "paymentMethodTypeId", "EFT_ACCOUNT").filterByDate().orderBy("paymentMethodId").queryList();
        for (GenericValue method : methods) {
            GenericValue eft = EntityQuery.use(delegator).from("EftAccount").where("paymentMethodId", method.getString("paymentMethodId")).queryOne();
            if (eft != null && getIban(eft) != null) {
                return eft;
            }
        }
        return null;
    }

    /** The IBAN of an EFT account: ibanNumber, else accountNumber if it passes the IBAN check. */
    static String getIban(GenericValue eft) {
        for (String field : new String[] {"ibanNumber", "accountNumber"}) {
            String value = eft.getString(field);
            if (value != null) {
                value = value.replace(" ", "").toUpperCase(Locale.ROOT);
                if (isValidIban(value)) {
                    return value;
                }
            }
        }
        return null;
    }

    static boolean isValidIban(String iban) {
        if (iban == null || !iban.matches("^[A-Z]{2}[0-9]{2}[A-Z0-9]{11,30}$")) {
            return false;
        }
        String rearranged = iban.substring(4) + iban.substring(0, 4);
        int mod = 0;
        for (char c : rearranged.toCharArray()) {
            int value = Character.isDigit(c) ? c - '0' : c - 'A' + 10;
            mod = (value > 9) ? (mod * 100 + value) % 97 : (mod * 10 + value) % 97;
        }
        return mod == 1;
    }

    private String getIso2(String geoId) throws GenericEntityException {
        if (UtilValidate.isEmpty(geoId)) {
            return null;
        }
        GenericValue geo = EntityQuery.use(delegator).from("Geo").where("geoId", geoId).cache().queryOne();
        String code = (geo != null) ? geo.getString("geoCode") : null;
        return (code != null && code.length() == 2) ? code : null;
    }

    private boolean isEuCountry(String geoId) throws GenericEntityException {
        if (euCountries == null) {
            euCountries = new HashSet<>();
            for (GenericValue assoc : EntityQuery.use(delegator).from("GeoAssoc").where("geoIdTo", EU_GROUP_GEO_ID, "geoAssocTypeId", "GROUP_MEMBER").cache().queryList()) {
                euCountries.add(assoc.getString("geoId"));
            }
            for (String excluded : config("einvoice.eu.excludedCountries", "GBR").split(",")) {
                euCountries.remove(excluded.trim());
            }
        }
        return euCountries.contains(geoId);
    }

    private String getLineName(GenericValue item) throws GenericEntityException {
        String name = getItemDescription(item);
        return UtilValidate.isNotEmpty(name) ? name : item.getString("invoiceItemSeqId");
    }

    /** Plain text item name (no HTML): description, product name, item type. */
    private String getItemDescription(GenericValue item) throws GenericEntityException {
        String description = item.getString("description");
        if (UtilValidate.isEmpty(description) && UtilValidate.isNotEmpty(item.getString("productId"))) {
            GenericValue product = EntityQuery.use(delegator).from("Product").where("productId", item.getString("productId")).cache().queryOne();
            if (product != null) {
                description = UtilValidate.isNotEmpty(product.getString("productName")) ? (String) product.get("productName", docLocale) : product.getString("internalName");
            }
        }
        if (UtilValidate.isEmpty(description)) {
            GenericValue type = item.getRelatedOne("InvoiceItemType", true);
            description = (type != null) ? (String) type.get("description", docLocale) : null;
        }
        return description;
    }

    private String getUnitCode(GenericValue item) throws GenericEntityException {
        String uomId = item.getString("uomId");
        if (UtilValidate.isNotEmpty(uomId)) {
            // InvoiceItem.uomId often holds the currency
            GenericValue uom = EntityQuery.use(delegator).from("Uom").where("uomId", uomId).cache().queryOne();
            if (uom == null || "CURRENCY_MEASURE".equals(uom.getString("uomTypeId"))) {
                uomId = null;
            }
        }
        if (UtilValidate.isEmpty(uomId) && UtilValidate.isNotEmpty(item.getString("productId"))) {
            GenericValue product = EntityQuery.use(delegator).from("Product").where("productId", item.getString("productId")).cache().queryOne();
            uomId = (product != null) ? product.getString("quantityUomId") : null;
        }
        if (UtilValidate.isNotEmpty(uomId)) {
            GenericValue uom = EntityQuery.use(delegator).from("Uom").where("uomId", uomId).cache().queryOne();
            if (uom != null && UtilValidate.isNotEmpty(uom.getString("unCefactCode"))) {
                return uom.getString("unCefactCode");
            }
        }
        return config("einvoice.unit.default", "H87");
    }

    private String getGtin(String productId) throws GenericEntityException {
        for (String type : new String[] {"EAN", "UPCA"}) {
            GenericValue gi = EntityQuery.use(delegator).from("GoodIdentification").where("productId", productId, "goodIdentificationTypeId", type).cache().queryOne();
            if (gi != null && UtilValidate.isNotEmpty(gi.getString("idValue")) && gi.getString("idValue").matches("^[0-9]{8,14}$")) {
                return gi.getString("idValue");
            }
        }
        return null;
    }

    // ---------------------------------------------------------------------------------------------------------
    // Messages and configuration

    private String config(String name, String defaultValue) {
        String value = EntityUtilProperties.getPropertyValue(CONFIG, name, delegator);
        return UtilValidate.isNotEmpty(value) ? value : defaultValue;
    }

    private static void appendLine(StringBuilder sb, String line) {
        if (sb.length() > 0) {
            sb.append('\n');
        }
        sb.append(line);
    }

    /** A text that goes into the e-invoice, in the document language. */
    private String docLabel(String key, Object... args) {
        return UtilProperties.getMessage(RESOURCE, key, UtilMisc.toMap(args), docLocale);
    }

    private String label(String key, Object... args) {
        return UtilProperties.getMessage(RESOURCE, key, UtilMisc.toMap(args), locale);
    }

    private void error(String key, Object... args) {
        errors.add(label(key, args));
    }

    private void warning(String key, Object... args) {
        warnings.add(label(key, args));
    }
}
