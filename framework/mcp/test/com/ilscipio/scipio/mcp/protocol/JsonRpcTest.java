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
package com.ilscipio.scipio.mcp.protocol;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.util.Map;

import org.junit.jupiter.api.Test;

public class JsonRpcTest {

    @Test
    public void parsesSingleRequest() throws Exception {
        JsonRpc.Parsed p = JsonRpc.parse("{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"ping\"}");
        assertFalse(p.batch);
        assertEquals(1, p.requests.size());
        JsonRpc.Request r = p.requests.get(0);
        assertEquals("ping", r.method);
        assertEquals(1, r.id);
        assertFalse(r.notification);
        assertFalse(r.isInvalid());
    }

    @Test
    public void parsesNotificationAndBatch() throws Exception {
        JsonRpc.Parsed p = JsonRpc.parse("[{\"jsonrpc\":\"2.0\",\"method\":\"notifications/initialized\"},{\"jsonrpc\":\"2.0\",\"id\":\"a\",\"method\":\"tools/list\",\"params\":{\"cursor\":\"x\"}}]");
        assertTrue(p.batch);
        assertTrue(p.requests.get(0).notification);
        assertNull(p.requests.get(0).id);
        assertEquals("x", p.requests.get(1).params.get("cursor"));
    }

    @Test
    public void flagsInvalidRequests() throws Exception {
        JsonRpc.Parsed p = JsonRpc.parse("{\"jsonrpc\":\"1.0\",\"id\":1,\"method\":\"ping\"}");
        assertTrue(p.requests.get(0).isInvalid());
        p = JsonRpc.parse("{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"ping\",\"params\":[1]}");
        assertTrue(p.requests.get(0).isInvalid());
        JsonRpc.JsonRpcException e = assertThrows(JsonRpc.JsonRpcException.class, () -> JsonRpc.parse("[]"));
        assertEquals(JsonRpc.INVALID_REQUEST, e.getCode());
    }

    @Test
    public void parseErrorOnMalformedJson() {
        JsonRpc.JsonRpcException e = assertThrows(JsonRpc.JsonRpcException.class, () -> JsonRpc.parse("{not json"));
        assertEquals(JsonRpc.PARSE_ERROR, e.getCode());
    }

    @Test
    public void writesResultAndError() {
        Map<String, Object> ok = JsonRpc.result(7, Map.of("a", 1));
        String s = JsonRpc.write(ok);
        assertTrue(s.contains("\"jsonrpc\":\"2.0\""));
        assertTrue(s.contains("\"id\":7"));
        Map<String, Object> err = JsonRpc.error(null, JsonRpc.METHOD_NOT_FOUND, "nope", null);
        assertNotNull(err.get("error"));
        assertTrue(JsonRpc.write(err).contains("-32601"));
    }
}
