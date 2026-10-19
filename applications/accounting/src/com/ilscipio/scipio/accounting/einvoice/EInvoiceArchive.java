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

import java.sql.Timestamp;
import java.util.List;

import org.ofbiz.base.util.UtilDateTime;
import org.ofbiz.base.util.UtilMisc;
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.GenericEntityException;
import org.ofbiz.entity.GenericValue;
import org.ofbiz.entity.util.EntityQuery;

/**
 * Keeps e-invoice files with the invoice (InvoiceContent → Content → DataResource → OtherDataResource), unchanged,
 * so that the same document can be delivered again and kept for the legal retention period.
 * <ul>
 * <li>EINVOICE_XML / EINVOICE_PDF: the e-invoice that was sent (Content.description = format id)</li>
 * <li>EINVOICE_ORIGINAL: the file of a received e-invoice</li>
 * </ul>
 */
public final class EInvoiceArchive {

    public static final String SENT_XML = "EINVOICE_XML";
    public static final String SENT_PDF = "EINVOICE_PDF";
    public static final String ORIGINAL = "EINVOICE_ORIGINAL";

    private EInvoiceArchive() {}

    public static String getSentContentTypeId(EInvoiceFormat format) {
        return format.isPdf() ? SENT_PDF : SENT_XML;
    }

    /** Returns the stored file of the format, or null. formatId null = any format. */
    public static EInvoiceDocument find(Delegator delegator, String invoiceId, String invoiceContentTypeId, String formatId) throws GenericEntityException {
        List<GenericValue> invoiceContents = EntityQuery.use(delegator).from("InvoiceContent")
                .where("invoiceId", invoiceId, "invoiceContentTypeId", invoiceContentTypeId).filterByDate().orderBy("-fromDate").queryList();
        for (GenericValue invoiceContent : invoiceContents) {
            GenericValue content = EntityQuery.use(delegator).from("Content").where("contentId", invoiceContent.getString("contentId")).queryOne();
            if (content == null || (formatId != null && !formatId.equals(content.getString("description")))) {
                continue;
            }
            GenericValue dataResource = EntityQuery.use(delegator).from("DataResource").where("dataResourceId", content.getString("dataResourceId")).queryOne();
            GenericValue other = (dataResource != null) ? EntityQuery.use(delegator).from("OtherDataResource").where("dataResourceId", dataResource.getString("dataResourceId")).queryOne() : null;
            if (other == null || other.getBytes("dataResourceContent") == null) {
                continue;
            }
            return new EInvoiceDocument(EInvoiceFormat.fromId(content.getString("description")), other.getBytes("dataResourceContent"),
                    dataResource.getString("dataResourceName"), dataResource.getString("mimeTypeId"));
        }
        return null;
    }

    /** Stores the file with the invoice and returns the contentId. */
    public static String store(Delegator delegator, String invoiceId, String invoiceContentTypeId, EInvoiceDocument document, String formatId, GenericValue userLogin)
            throws GenericEntityException {
        Timestamp now = UtilDateTime.nowTimestamp();
        String userLoginId = (userLogin != null) ? userLogin.getString("userLoginId") : null;
        String dataResourceId = delegator.getNextSeqId("DataResource");
        delegator.create("DataResource", UtilMisc.toMap("dataResourceId", dataResourceId, "dataResourceTypeId", "OTHER_OBJECT",
                "dataResourceName", document.getFileName(), "mimeTypeId", document.getContentType(), "statusId", "CTNT_PUBLISHED",
                "createdDate", now, "createdByUserLogin", userLoginId));
        delegator.create("OtherDataResource", UtilMisc.toMap("dataResourceId", dataResourceId, "dataResourceContent", document.getContent()));
        String contentId = delegator.getNextSeqId("Content");
        delegator.create("Content", UtilMisc.toMap("contentId", contentId, "contentTypeId", "DOCUMENT", "dataResourceId", dataResourceId,
                "contentName", document.getFileName(), "description", formatId, "mimeTypeId", document.getContentType(), "statusId", "CTNT_PUBLISHED",
                "createdDate", now, "createdByUserLogin", userLoginId));
        delegator.create("InvoiceContent", UtilMisc.toMap("invoiceId", invoiceId, "invoiceContentTypeId", invoiceContentTypeId, "contentId", contentId, "fromDate", now));
        return contentId;
    }
}
