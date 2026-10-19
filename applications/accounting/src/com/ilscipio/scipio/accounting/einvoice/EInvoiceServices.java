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

import java.nio.ByteBuffer;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Locale;
import java.util.Map;

import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.UtilMisc;
import org.ofbiz.base.util.UtilProperties;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.GenericEntityException;
import org.ofbiz.entity.GenericValue;
import org.ofbiz.entity.util.EntityQuery;
import org.ofbiz.entity.util.EntityTypeUtil;
import org.ofbiz.entity.util.EntityUtilProperties;
import org.ofbiz.service.DispatchContext;
import org.ofbiz.service.ServiceUtil;

/**
 * E-invoice services: check, generate (with archive), email attachments, import.
 * <p>
 * Delivery: the email channel attaches the e-invoice to the invoice email (sendInvoicePerEmail). A Peppol
 * access-point provider would be one more channel (PartyEInvoicePref.deliveryChannelEnumId = EINV_DLV_PEPPOL);
 * none is connected yet.
 */
public class EInvoiceServices {

    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());

    public static final String NO_FORMAT = "NONE";

    public static String getProducer() {
        return UtilProperties.getPropertyValue(EInvoiceBuilder.CONFIG, "einvoice.pdf.producer", "Scipio ERP");
    }

    /**
     * The format for an invoice: the explicit format id, else the format of the customer (PartyEInvoicePref),
     * else, if useDefault, the default of the organization and then of einvoice.properties. Null = no e-invoice.
     */
    public static EInvoiceFormat resolveFormat(Delegator delegator, GenericValue invoice, String formatId, boolean useDefault) throws GenericEntityException {
        if (NO_FORMAT.equalsIgnoreCase(formatId)) {
            return null;
        }
        if (UtilValidate.isNotEmpty(formatId)) {
            return EInvoiceFormat.fromId(formatId);
        }
        boolean creditNote = EntityTypeUtil.hasParentType(delegator, "InvoiceType", "invoiceTypeId", invoice.getString("invoiceTypeId"), "parentTypeId", "CUST_RTN_INVOICE");
        String customerId = creditNote ? invoice.getString("partyIdFrom") : invoice.getString("partyId");
        String organizationId = creditNote ? invoice.getString("partyId") : invoice.getString("partyIdFrom");
        GenericValue customerPref = EntityQuery.use(delegator).from("PartyEInvoicePref").where("partyId", customerId).queryOne();
        if (customerPref != null && UtilValidate.isNotEmpty(customerPref.getString("formatEnumId"))) {
            return EInvoiceFormat.fromId(customerPref.getString("formatEnumId"));
        }
        if (!useDefault) {
            return null;
        }
        GenericValue orgPref = EntityQuery.use(delegator).from("PartyEInvoicePref").where("partyId", organizationId).queryOne();
        if (orgPref != null && UtilValidate.isNotEmpty(orgPref.getString("formatEnumId"))) {
            return EInvoiceFormat.fromId(orgPref.getString("formatEnumId"));
        }
        return EInvoiceFormat.fromId(EntityUtilProperties.getPropertyValue(EInvoiceBuilder.CONFIG, "einvoice.format.default", delegator));
    }

    /**
     * Returns the e-invoice of the format. When the invoice has left INVOICE_IN_PROCESS, the first generated
     * file is kept (archive) and returned again on later calls.
     */
    public static EInvoiceDocument getDocument(DispatchContext dctx, GenericValue invoice, EInvoiceFormat format, boolean archive, GenericValue userLogin,
            Locale locale, List<String> warnings) throws EInvoiceException, GenericEntityException {
        Delegator delegator = dctx.getDelegator();
        String invoiceId = invoice.getString("invoiceId");
        boolean isFinal = !"INVOICE_IN_PROCESS".equals(invoice.getString("statusId"));
        String archiveTypeId = EInvoiceArchive.getSentContentTypeId(format);
        if (isFinal) {
            EInvoiceDocument stored = EInvoiceArchive.find(delegator, invoiceId, archiveTypeId, format.getId());
            if (stored != null) {
                return stored;
            }
        }
        EInvoiceBuilder.Result result = EInvoiceBuilder.build(delegator, invoiceId, format, locale);
        if (result.hasErrors()) {
            throw new EInvoiceException(result.getErrors());
        }
        warnings.addAll(result.getWarnings());
        EInvoiceDocument document = EInvoiceWriter.write(result.getInvoice(), result.getInvoiceNumber(), format, () -> {
            Map<String, Object> params = UtilMisc.toMap("invoiceId", invoiceId, "userLogin", userLogin, "locale", locale);
            String screen = EntityUtilProperties.getPropertyValue(EInvoiceBuilder.CONFIG, "einvoice.pdf.screen",
                    "component://accounting/widget/AccountingPrintScreens.xml#InvoicePDF", delegator);
            return EInvoicePdfRenderer.render(dctx, screen, params, EInvoicePdfRenderer.PDF_A_3B);
        });
        if (isFinal && archive) {
            EInvoiceArchive.store(delegator, invoiceId, archiveTypeId, document, format.getId(), userLogin);
        }
        return document;
    }

    // ---------------------------------------------------------------------------------------------------------
    // Services

    public static Map<String, Object> checkEInvoice(DispatchContext dctx, Map<String, ?> context) {
        Delegator delegator = dctx.getDelegator();
        Locale locale = (Locale) context.get("locale");
        String invoiceId = (String) context.get("invoiceId");
        try {
            GenericValue invoice = EntityQuery.use(delegator).from("Invoice").where("invoiceId", invoiceId).queryOne();
            if (invoice == null) {
                return ServiceUtil.returnError(label(locale, "EInvoiceInvoiceNotFound", "invoiceId", invoiceId));
            }
            EInvoiceFormat format = resolveFormat(delegator, invoice, (String) context.get("formatId"), true);
            if (format == null) {
                format = EInvoiceFormat.EN16931_UBL;
            }
            EInvoiceBuilder.Result result = EInvoiceBuilder.build(delegator, invoiceId, format, locale);
            Map<String, Object> out = ServiceUtil.returnSuccess();
            out.put("formatId", format.getId());
            out.put("valid", !result.hasErrors());
            out.put("errors", result.getErrors());
            out.put("warnings", result.getWarnings());
            return out;
        } catch (GenericEntityException e) {
            Debug.logError(e, module);
            return ServiceUtil.returnError(e.getMessage());
        }
    }

    public static Map<String, Object> generateEInvoice(DispatchContext dctx, Map<String, ?> context) {
        Delegator delegator = dctx.getDelegator();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");
        String invoiceId = (String) context.get("invoiceId");
        try {
            GenericValue invoice = EntityQuery.use(delegator).from("Invoice").where("invoiceId", invoiceId).queryOne();
            if (invoice == null) {
                return ServiceUtil.returnError(label(locale, "EInvoiceInvoiceNotFound", "invoiceId", invoiceId));
            }
            EInvoiceFormat format = resolveFormat(delegator, invoice, (String) context.get("formatId"), true);
            if (format == null) {
                return ServiceUtil.returnError(label(locale, "EInvoiceNoFormat", "formatId", String.valueOf(context.get("formatId"))));
            }
            List<String> warnings = new ArrayList<>();
            EInvoiceDocument document = getDocument(dctx, invoice, format, !Boolean.FALSE.equals(context.get("archive")), userLogin, locale, warnings);
            Map<String, Object> out = ServiceUtil.returnSuccess();
            out.put("content", document.getContent());
            out.put("fileName", document.getFileName());
            out.put("contentType", document.getContentType());
            out.put("formatId", format.getId());
            out.put("warnings", warnings);
            return out;
        } catch (EInvoiceException e) {
            return ServiceUtil.returnError(e.getMessages());
        } catch (GenericEntityException e) {
            Debug.logError(e, module);
            return ServiceUtil.returnError(e.getMessage());
        }
    }

    /**
     * The e-invoice attachment for the invoice email: the format of the customer (or the given format), if the
     * customer receives e-invoices by email. A hybrid PDF replaces the plain invoice PDF (replacesPdf).
     */
    public static Map<String, Object> getEInvoiceEmailAttachments(DispatchContext dctx, Map<String, ?> context) {
        Delegator delegator = dctx.getDelegator();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");
        String invoiceId = (String) context.get("invoiceId");
        String formatId = (String) context.get("formatId");
        Map<String, Object> out = ServiceUtil.returnSuccess();
        out.put("attachments", Collections.emptyList());
        out.put("replacesPdf", Boolean.FALSE);
        try {
            GenericValue invoice = EntityQuery.use(delegator).from("Invoice").where("invoiceId", invoiceId).queryOne();
            if (invoice == null) {
                return out;
            }
            EInvoiceFormat format = resolveFormat(delegator, invoice, formatId, false);
            if (format == null) {
                return out;
            }
            if (UtilValidate.isEmpty(formatId)) {
                String customerId = EntityTypeUtil.hasParentType(delegator, "InvoiceType", "invoiceTypeId", invoice.getString("invoiceTypeId"), "parentTypeId", "CUST_RTN_INVOICE")
                        ? invoice.getString("partyIdFrom") : invoice.getString("partyId");
                GenericValue pref = EntityQuery.use(delegator).from("PartyEInvoicePref").where("partyId", customerId).queryOne();
                String channel = (pref != null) ? pref.getString("deliveryChannelEnumId") : null;
                if (channel != null && !"EINV_DLV_EMAIL".equals(channel)) {
                    return out;
                }
            }
            List<String> warnings = new ArrayList<>();
            EInvoiceDocument document = getDocument(dctx, invoice, format, true, userLogin, locale, warnings);
            out.put("attachments", UtilMisc.toList(UtilMisc.toMap("content", document.getContent(), "type", document.getContentType(), "filename", document.getFileName())));
            out.put("replacesPdf", format.isPdf());
            out.put("warnings", warnings);
            return out;
        } catch (EInvoiceException e) {
            List<String> messages = new ArrayList<>();
            messages.add(label(locale, "EInvoiceEmailNotSent", "invoiceId", invoiceId));
            messages.addAll(e.getMessages());
            return ServiceUtil.returnError(messages);
        } catch (GenericEntityException e) {
            Debug.logError(e, module);
            return ServiceUtil.returnError(e.getMessage());
        }
    }

    public static Map<String, Object> importEInvoice(DispatchContext dctx, Map<String, ?> context) {
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");
        ByteBuffer buffer = (ByteBuffer) context.get("uploadedFile");
        if (buffer == null) {
            return ServiceUtil.returnError(label(locale, "EInvoiceImportNoFile"));
        }
        ByteBuffer copy = buffer.duplicate();
        copy.rewind();
        byte[] data = new byte[copy.remaining()];
        copy.get(data);
        EInvoiceImporter importer = new EInvoiceImporter(dctx, userLogin, locale);
        try {
            String invoiceId = importer.importInvoice(data, (String) context.get("_uploadedFile_fileName"),
                    (String) context.get("organizationPartyId"), (String) context.get("supplierPartyId"));
            Map<String, Object> out = ServiceUtil.returnSuccess(label(locale, "EInvoiceImportDone", "invoiceId", invoiceId));
            out.put("invoiceId", invoiceId);
            out.put("warnings", importer.getWarnings());
            return out;
        } catch (EInvoiceException e) {
            return ServiceUtil.returnError(e.getMessages());
        } catch (GenericEntityException e) {
            Debug.logError(e, module);
            return ServiceUtil.returnError(e.getMessage());
        }
    }

    /** Creates or updates the e-invoice settings of a party; an empty field is stored as null. */
    public static Map<String, Object> updatePartyEInvoicePref(DispatchContext dctx, Map<String, ?> context) {
        Delegator delegator = dctx.getDelegator();
        String partyId = (String) context.get("partyId");
        try {
            GenericValue pref = EntityQuery.use(delegator).from("PartyEInvoicePref").where("partyId", partyId).queryOne();
            boolean create = (pref == null);
            if (create) {
                pref = delegator.makeValue("PartyEInvoicePref", UtilMisc.toMap("partyId", partyId));
            }
            for (String field : pref.getModelEntity().getNoPkFieldNames()) {
                if (pref.getModelEntity().getField(field).getIsAutoCreatedInternal()) {
                    continue;
                }
                if (context.containsKey(field)) {
                    Object value = context.get(field);
                    pref.set(field, (value instanceof String && ((String) value).trim().isEmpty()) ? null : (value instanceof String ? ((String) value).trim() : value));
                }
            }
            String formatId = pref.getString("formatEnumId");
            if (formatId != null && EInvoiceFormat.fromId(formatId) == null) {
                return ServiceUtil.returnError(label((Locale) context.get("locale"), "EInvoiceNoFormat", "formatId", formatId));
            }
            if (create) {
                pref.create();
            } else {
                pref.store();
            }
            return ServiceUtil.returnSuccess();
        } catch (GenericEntityException e) {
            Debug.logError(e, module);
            return ServiceUtil.returnError(e.getMessage());
        }
    }

    static String label(Locale locale, String key, Object... args) {
        return UtilProperties.getMessage(EInvoiceBuilder.RESOURCE, key, UtilMisc.toMap(args), locale);
    }
}
