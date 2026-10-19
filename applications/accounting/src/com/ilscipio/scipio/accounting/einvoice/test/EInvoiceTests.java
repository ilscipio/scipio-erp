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
package com.ilscipio.scipio.accounting.einvoice.test;

import java.math.BigDecimal;
import java.nio.charset.StandardCharsets;
import java.util.Locale;

import org.mustangproject.Invoice;
import org.mustangproject.ZUGFeRD.IZUGFeRDExportableItem;
import org.mustangproject.ZUGFeRD.TransactionCalculator;
import org.ofbiz.base.util.UtilProperties;
import org.ofbiz.entity.GenericValue;
import org.ofbiz.entity.util.EntityQuery;
import org.ofbiz.service.testtools.OFBizTestCase;

import com.ilscipio.scipio.accounting.einvoice.EInvoiceBuilder;
import com.ilscipio.scipio.accounting.einvoice.EInvoiceFormat;
import com.ilscipio.scipio.accounting.einvoice.EInvoiceImporter;
import com.ilscipio.scipio.accounting.einvoice.EInvoiceWriter;

/**
 * E-invoice tests on the demo invoices of EInvoiceDemoData.xml (EINV1001 to EINV1003). Each output is read back
 * with the Mustang importer and must give the same totals. The official validators (KoSIT XRechnung and Peppol
 * BIS configurations, Mustang for ZUGFeRD and PDF/A) run outside the test suite; see the e-invoice docs.
 */
public class EInvoiceTests extends OFBizTestCase {

    private static final Locale LOCALE = Locale.ENGLISH;

    public EInvoiceTests(String name) {
        super(name);
    }

    public void testFormatIds() {
        assertEquals(EInvoiceFormat.XRECHNUNG_UBL, EInvoiceFormat.fromId("xrechnung_ubl"));
        assertNull(EInvoiceFormat.fromId("NONE"));
        assertTrue(EInvoiceFormat.ZUGFERD.isPdf());
        assertEquals("EINV1001_peppol.xml", EInvoiceFormat.PEPPOL_BIS3.getFileName("EINV1001"));
    }

    public void testXRechnungInvoice() throws Exception {
        EInvoiceBuilder.Result result = EInvoiceBuilder.build(delegator, "EINV1001", EInvoiceFormat.XRECHNUNG_UBL, LOCALE);
        assertFalse("errors: " + result.getErrors(), result.hasErrors());
        Invoice inv = result.getInvoice();
        assertEquals("04011000-12345-03", inv.getReferenceNumber());
        assertEquals("DE123456789", inv.getSender().getVATID());
        assertTrue("BT-20 must end with the XRechnung cash discount line and a line break",
                inv.getPaymentTermDescription().endsWith("#SKONTO#TAGE=10#PROZENT=2.00#\n"));
        TransactionCalculator calc = new TransactionCalculator(inv);
        assertEquals(new BigDecimal("2132.03"), calc.getGrandTotal().setScale(2));
        assertNull("no rounding needed", inv.getRoundingAmount());

        for (EInvoiceFormat format : new EInvoiceFormat[] {EInvoiceFormat.XRECHNUNG_UBL, EInvoiceFormat.XRECHNUNG_CII, EInvoiceFormat.EN16931_UBL}) {
            byte[] xml = EInvoiceWriter.write(inv, "EINV1001", format, () -> null).getContent();
            String text = new String(xml, StandardCharsets.UTF_8);
            assertTrue(format + ": specification id", text.contains(format.getCustomizationId()));
            Invoice back = EInvoiceImporter.parse(xml, LOCALE).getInvoice();
            assertEquals(format + ": number", "EINV1001", back.getNumber());
            assertEquals(format + ": total", new BigDecimal("2132.03"), new TransactionCalculator(back).getGrandTotal().setScale(2));
        }
    }

    public void testIntraCommunitySupply() throws Exception {
        EInvoiceBuilder.Result result = EInvoiceBuilder.build(delegator, "EINV1002", EInvoiceFormat.PEPPOL_BIS3, LOCALE);
        assertFalse("errors: " + result.getErrors(), result.hasErrors());
        Invoice inv = result.getInvoice();
        for (IZUGFeRDExportableItem item : inv.getZFItems()) {
            assertEquals("K", item.getProduct().getTaxCategoryCode());
            assertEquals("VATEX-EU-IC", item.getProduct().getTaxExemptionReasonCode());
        }
        assertNotNull("deliver-to country (BR-IC-12)", inv.getDeliveryAddress());
        byte[] ubl = EInvoiceWriter.write(inv, "EINV1002", EInvoiceFormat.PEPPOL_BIS3, () -> null).getContent();
        String text = new String(ubl, StandardCharsets.UTF_8);
        assertTrue(text.contains("<cbc:EndpointID schemeID=\"0009\">30326504500015</cbc:EndpointID>"));
        assertEquals(new BigDecimal("2495.00"), new TransactionCalculator(EInvoiceImporter.parse(ubl, LOCALE).getInvoice()).getGrandTotal().setScale(2));
    }

    public void testCreditNote() throws Exception {
        EInvoiceBuilder.Result result = EInvoiceBuilder.build(delegator, "EINV1003", EInvoiceFormat.XRECHNUNG_UBL, LOCALE);
        assertFalse("errors: " + result.getErrors(), result.hasErrors());
        assertTrue(result.isCreditNote());
        Invoice inv = result.getInvoice();
        // the organization (Invoice.partyId of a customer return) is the seller of the credit note
        assertEquals("DE123456789", inv.getSender().getVATID());
        assertEquals("EINV1001", inv.getInvoiceReferencedDocuments().get(0).getIssuerAssignedID());
        String text = new String(EInvoiceWriter.write(inv, "EINV1003", EInvoiceFormat.XRECHNUNG_UBL, () -> null).getContent(), StandardCharsets.UTF_8);
        assertTrue(text.contains("<CreditNote "));
        assertTrue(text.contains("<cbc:CreditNoteTypeCode>381</cbc:CreditNoteTypeCode>"));
        assertTrue("BT-25 in InvoiceDocumentReference (BR-55)", text.contains("<cac:InvoiceDocumentReference>"));
        assertFalse(text.contains("<cac:CreditNoteDocumentReference>"));
    }

    public void testSmallBusiness() throws Exception {
        // a Kleinunternehmer charges no VAT: every line without VAT is exempt (E) with the section 19 note
        GenericValue pref = EntityQuery.use(delegator).from("PartyEInvoicePref").where("partyId", "EINV_SELLER").queryOne();
        pref.set("smallBusinessFlag", "Y");
        pref.store();
        try {
            EInvoiceBuilder.Result result = EInvoiceBuilder.build(delegator, "EINV1002", EInvoiceFormat.EN16931_UBL, LOCALE);
            assertFalse("errors: " + result.getErrors(), result.hasErrors());
            String note = UtilProperties.getMessage(EInvoiceBuilder.RESOURCE, "EInvoiceVatSmallBusiness", LOCALE);
            for (IZUGFeRDExportableItem item : result.getInvoice().getZFItems()) {
                assertEquals("E", item.getProduct().getTaxCategoryCode());
                assertEquals(note, item.getProduct().getTaxExemptionReason());
            }
        } finally {
            pref.set("smallBusinessFlag", null);
            pref.store();
        }
    }

    public void testPeppolNeedsParticipantId() throws Exception {
        // the public authority has only an email address: Peppol needs a participant ID
        EInvoiceBuilder.Result result = EInvoiceBuilder.build(delegator, "EINV1001", EInvoiceFormat.PEPPOL_BIS3, LOCALE);
        assertTrue(result.hasErrors());
    }

    public void testNotForPurchaseInvoices() throws Exception {
        EInvoiceBuilder.Result result = EInvoiceBuilder.build(delegator, "demo10001", EInvoiceFormat.XRECHNUNG_UBL, LOCALE);
        assertTrue(result.hasErrors());
    }
}
