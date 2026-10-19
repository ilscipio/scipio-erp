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

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;
import java.util.Locale;

import org.mustangproject.Invoice;
import org.mustangproject.ZUGFeRD.IZUGFeRDExporter;
import org.mustangproject.ZUGFeRD.Profiles;
import org.mustangproject.ZUGFeRD.ZUGFeRD2PullProvider;
import org.mustangproject.ZUGFeRD.ZUGFeRDExporterFromPDFA;

import com.helger.cii.d16b.CIID16BCrossIndustryInvoiceTypeMarshaller;
import com.helger.commons.error.IError;
import com.helger.commons.error.list.ErrorList;
import com.helger.en16931.cii2ubl.CIIToUBL21Converter;
import com.helger.ubl21.UBL21Marshaller;

import oasis.names.specification.ubl.schema.xsd.commonaggregatecomponents_21.BillingReferenceType;
import oasis.names.specification.ubl.schema.xsd.creditnote_21.CreditNoteType;
import oasis.names.specification.ubl.schema.xsd.invoice_21.InvoiceType;
import un.unece.uncefact.data.standard.crossindustryinvoice._100.CrossIndustryInvoiceType;

/**
 * Writes the e-invoice file of a format: CII (Mustang), UBL 2.1 (CII converted by the EN 16931 CII to UBL
 * converter), or a hybrid PDF/A-3 with the CII embedded (ZUGFeRD 2 / Factur-X).
 */
public final class EInvoiceWriter {

    private EInvoiceWriter() {}

    /** Supplies the visible PDF/A-3 invoice for a hybrid format. */
    @FunctionalInterface
    public interface PdfSupplier {
        byte[] get() throws EInvoiceException;
    }

    public static EInvoiceDocument write(Invoice invoice, String invoiceNumber, EInvoiceFormat format, PdfSupplier pdfSupplier) throws EInvoiceException {
        byte[] cii = writeCii(invoice, format);
        byte[] content;
        switch (format.getSyntax()) {
        case UBL:
            content = ciiToUbl(cii, format);
            break;
        case PDF:
            content = writeHybridPdf(invoice, format, pdfSupplier.get());
            break;
        default:
            content = cii;
        }
        return new EInvoiceDocument(format, content, format.getFileName(invoiceNumber), format.getContentType());
    }

    public static byte[] writeCii(Invoice invoice, EInvoiceFormat format) throws EInvoiceException {
        try {
            ZUGFeRD2PullProvider provider = new ZUGFeRD2PullProvider();
            provider.setProfile(Profiles.getByName(format.getCiiProfile()));
            provider.generateXML(invoice);
            return provider.getXML();
        } catch (RuntimeException e) {
            throw new EInvoiceException("CII: " + e.getMessage(), e);
        }
    }

    public static byte[] ciiToUbl(byte[] cii, EInvoiceFormat format) throws EInvoiceException {
        CrossIndustryInvoiceType ciiInvoice = new CIID16BCrossIndustryInvoiceTypeMarshaller().read(cii);
        if (ciiInvoice == null) {
            throw new EInvoiceException("UBL: the CII document cannot be read");
        }
        CIIToUBL21Converter converter = new CIIToUBL21Converter();
        converter.setCustomizationID(format.getCustomizationId());
        if (format.getProfileId() != null) {
            converter.setProfileID(format.getProfileId());
        }
        ErrorList errorList = new ErrorList();
        Serializable ubl = converter.convertCIItoUBL(ciiInvoice, errorList);
        if (ubl == null || errorList.containsAtLeastOneError()) {
            List<String> messages = new ArrayList<>();
            for (IError error : errorList) {
                if (error.isError()) {
                    messages.add("UBL: " + error.getAsString(Locale.ENGLISH));
                }
            }
            if (messages.isEmpty()) {
                messages.add("UBL: the CII document cannot be converted");
            }
            throw new EInvoiceException(messages);
        }
        byte[] bytes;
        if (ubl instanceof InvoiceType) {
            bytes = UBL21Marshaller.invoice().setFormattedOutput(true).getAsBytes((InvoiceType) ubl);
        } else {
            CreditNoteType creditNote = (CreditNoteType) ubl;
            // The converter (2.3.1) puts the preceding invoice (BT-25) into CreditNoteDocumentReference;
            // EN 16931 (BR-55, UBL-CR-039) needs BillingReference/InvoiceDocumentReference.
            for (BillingReferenceType billingRef : creditNote.getBillingReference()) {
                if (billingRef.getCreditNoteDocumentReference() != null && billingRef.getInvoiceDocumentReference() == null) {
                    billingRef.setInvoiceDocumentReference(billingRef.getCreditNoteDocumentReference());
                    billingRef.setCreditNoteDocumentReference(null);
                }
            }
            bytes = UBL21Marshaller.creditNote().setFormattedOutput(true).getAsBytes(creditNote);
        }
        if (bytes == null) {
            throw new EInvoiceException("UBL: the UBL document cannot be written");
        }
        return bytes;
    }

    public static byte[] writeHybridPdf(Invoice invoice, EInvoiceFormat format, byte[] pdfA) throws EInvoiceException {
        try (ZUGFeRDExporterFromPDFA exporter = new ZUGFeRDExporterFromPDFA()) {
            IZUGFeRDExporter loaded = exporter.load(pdfA);
            loaded.setProducer(EInvoiceServices.getProducer());
            loaded.setCreator(EInvoiceServices.getProducer());
            loaded.setZUGFeRDVersion(2);
            loaded.setProfile(Profiles.getByName(format.getCiiProfile()));
            exporter.setTransaction(invoice);
            ByteArrayOutputStream out = new ByteArrayOutputStream();
            exporter.export(out);
            return out.toByteArray();
        } catch (IOException | RuntimeException e) {
            throw new EInvoiceException("PDF/A-3: " + e.getMessage(), e);
        }
    }
}
