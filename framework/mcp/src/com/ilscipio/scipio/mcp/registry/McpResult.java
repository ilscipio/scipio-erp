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
package com.ilscipio.scipio.mcp.registry;

import java.util.Map;

/**
 * SCIPIO: 4.0.0: Result of one tool call. {@code structured} is JSON-safe (already converted) and becomes
 * {@code structuredContent}; {@code text} becomes the text content block. A {@code blob} result adds a binary
 * resource content block (base64) for a rendered document.
 */
public final class McpResult {

    private final String text;
    private final Object structured;
    private final boolean error;
    private final byte[] blob;
    private final String blobMimeType;
    private final String blobName;

    private McpResult(String text, Object structured, boolean error) {
        this(text, structured, error, null, null, null);
    }

    private McpResult(String text, Object structured, boolean error, byte[] blob, String blobMimeType, String blobName) {
        this.text = text;
        this.structured = structured;
        this.error = error;
        this.blob = blob;
        this.blobMimeType = blobMimeType;
        this.blobName = blobName;
    }

    /** Success with a JSON-safe structured value (Map or List of JSON-safe values). */
    public static McpResult ok(Object structured) {
        return new McpResult(null, structured, false);
    }

    public static McpResult text(String text) {
        return new McpResult(text, null, false);
    }

    public static McpResult error(String message) {
        return new McpResult(message, null, true);
    }

    public static McpResult error(String message, Map<String, Object> details) {
        return new McpResult(message, details, true);
    }

    /**
     * Success with a binary document: {@code structured} describes it (name, size, ids) and {@code data} is sent as
     * an embedded resource content block ({@code blob}, base64) with the given MIME type and file name.
     */
    public static McpResult blob(Object structured, byte[] data, String mimeType, String fileName) {
        if (data == null) throw new IllegalArgumentException("blob data required");
        return new McpResult(null, structured, false, data, mimeType != null ? mimeType : "application/octet-stream",
                fileName != null ? fileName : "document");
    }

    public String getText() { return text; }
    public Object getStructured() { return structured; }
    public boolean isError() { return error; }
    public boolean hasBlob() { return blob != null; }
    public byte[] getBlob() { return blob; }
    public String getBlobMimeType() { return blobMimeType; }
    public String getBlobName() { return blobName; }
}
