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
package com.ilscipio.scipio.product.mcp;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.lang.reflect.Method;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;

import javax.servlet.http.HttpServletRequest;

import org.junit.jupiter.api.Test;
import org.ofbiz.entity.GenericValue;
import org.ofbiz.security.Security;

import com.ilscipio.scipio.mcp.def.McpTool;
import com.ilscipio.scipio.mcp.registry.McpCallContext;
import com.ilscipio.scipio.mcp.registry.McpToolException;
import com.ilscipio.scipio.mcp.security.McpPrincipal;
import com.ilscipio.scipio.mcp.web.McpRequest;

/** R4-22: catalog/product document_attach (a document for buyers on a product): wiring, permission, checks, content type. */
public class DocumentAttachTest {

    private static McpCallContext context(boolean allowed) {
        GenericValue userLogin = mock(GenericValue.class);
        when(userLogin.getString("userLoginId")).thenReturn("desk");
        Security s = mock(Security.class);
        when(s.hasPermission(anyString(), any(GenericValue.class))).thenReturn(allowed);
        when(s.hasEntityPermission(anyString(), anyString(), any(GenericValue.class))).thenReturn(allowed);
        HttpServletRequest http = mock(HttpServletRequest.class);
        when(http.getContextPath()).thenReturn("/catalog");
        when(http.getRemoteAddr()).thenReturn("127.0.0.1");
        McpRequest request = new McpRequest(http, null, null, null, null, s, "req-1");
        GenericValue token = mock(GenericValue.class);
        when(token.getString("tokenId")).thenReturn("tok");
        when(token.getString("webapps")).thenReturn("*");
        when(token.getString("readOnly")).thenReturn("N");
        request.setPrincipal(new McpPrincipal(token, userLogin));
        return new McpCallContext(request);
    }

    @Test
    void theActionIsAWriteOfTheProductTopic() throws Exception {
        Method m = CatalogMcp.class.getMethod("attachDocument", McpCallContext.class, String.class, String.class, String.class, String.class);
        McpTool tool = m.getAnnotation(McpTool.class);
        assertEquals("product", tool.topic());
        assertEquals("document_attach", tool.name());
        assertFalse(tool.readOnly());
    }

    @Test
    void theActionNeedsCatalogUpdate() {
        McpToolException e = assertThrows(McpToolException.class,
                () -> CatalogMcp.attachDocument(context(false), "P1", "https://desk.example/v1/media/public/md_1", "care.pdf", "application/pdf"));
        assertTrue(e.isDenied(), e.getMessage());
    }

    @Test
    void theArgumentsAreChecked() {
        McpCallContext ctx = context(true);
        assertTrue(assertThrows(McpToolException.class,
                () -> CatalogMcp.attachDocument(ctx, "P1", "javascript:alert(1)", "care.pdf", "application/pdf")).getMessage().contains("url"));
        assertTrue(assertThrows(McpToolException.class,
                () -> CatalogMcp.attachDocument(ctx, "P1", "https://d/x", " ", "application/pdf")).getMessage().contains("fileName"));
        assertTrue(assertThrows(McpToolException.class,
                () -> CatalogMcp.attachDocument(ctx, "P1", "https://d/x", "a.exe", "application/x-msdownload")).getMessage().contains("contentType"));
    }

    @Test
    void theFileNameIsClean() {
        assertEquals("care.pdf", CatalogMcp.documentName("C:\\tmp\\care.pdf"));
        assertEquals("ab.pdf", CatalogMcp.documentName("a\u202Eb\n.pdf"));
        assertNull(CatalogMcp.documentName("x".repeat(201)));
        assertNull(CatalogMcp.documentName(null));
    }

    @Test
    void theProductContentTypeIsInTheSeedData() throws Exception {
        Path data = Paths.get("data", "ProductTypeData.xml");
        if (!Files.isRegularFile(data)) {
            data = Paths.get("applications", "product", "data", "ProductTypeData.xml");
        }
        String xml = new String(Files.readAllBytes(data), StandardCharsets.UTF_8);
        assertTrue(xml.contains("productContentTypeId=\"" + CatalogMcp.DOCUMENT_CONTENT_TYPE + "\""));
    }
}
