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

/** A generated or received e-invoice file. */
public class EInvoiceDocument {

    private final EInvoiceFormat format;
    private final byte[] content;
    private final String fileName;
    private final String contentType;

    public EInvoiceDocument(EInvoiceFormat format, byte[] content, String fileName, String contentType) {
        this.format = format;
        this.content = content;
        this.fileName = fileName;
        this.contentType = contentType;
    }

    public EInvoiceFormat getFormat() { return format; }

    public byte[] getContent() { return content; }

    public String getFileName() { return fileName; }

    public String getContentType() { return contentType; }
}
