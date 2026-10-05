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
package com.ilscipio.scipio.mcp.security;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assertions.fail;

import java.util.Arrays;
import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.Map;

import org.junit.jupiter.api.Test;

import com.ilscipio.scipio.mcp.protocol.JsonRpc;

/**
 * Hardening checks: redaction globs, rate limiter sweeps, JSON nesting limit, case-insensitive deny globs.
 */
public class McpHardeningTest {

    @Test
    @SuppressWarnings("unchecked")
    public void redactorMatchesGlobsCaseInsensitively() {
        McpRedactor r = new McpRedactor(Arrays.asList("password"), Arrays.asList("*secret*", "*accesstoken*", "*cardnumber*"));
        assertTrue(r.isSensitive("clientSecret"));
        assertTrue(r.isSensitive("OAUTH_ACCESSTOKEN"));
        assertTrue(r.isSensitive("CardNumber"));
        assertTrue(r.isSensitive("PASSWORD"));
        assertFalse(r.isSensitive("tokenId"));
        assertFalse(r.isSensitive("orderId"));
        Map<String, Object> m = new LinkedHashMap<>();
        m.put("mySecretValue", "x");
        m.put("orderId", "WS1");
        Map<String, Object> out = (Map<String, Object>) r.redact(m);
        assertEquals(McpRedactor.MASK, out.get("mySecretValue"));
        assertEquals("WS1", out.get("orderId"));
    }

    @Test
    public void denyGlobsIgnoreCase() {
        assertTrue(McpConfig.globMatches("*Sql*", "runRawSQL"));
        assertTrue(McpConfig.globMatches("*keystore*", "importKeyStore"));
        assertTrue(McpConfig.globMatches("runService", "RUNSERVICE"));
        assertFalse(McpConfig.globMatches("purge*", "unpurge"));
    }

    @Test
    public void rateLimiterSweepsBothMaps() {
        McpRateLimiter rl = McpRateLimiter.get();
        rl.reset();
        for (int i = 0; i < 50; i++) {
            rl.tryAcquire("ip:" + i, 10);
            assertTrue(rl.tryAcquireSlot("slot:" + i, 2));
            rl.release("slot:" + i);
        }
        assertEquals(100, rl.trackedKeys());
        rl.sweep(System.currentTimeMillis() + 3_600_000L);
        assertEquals(0, rl.trackedKeys());
        // a slot still in use survives the sweep
        assertTrue(rl.tryAcquireSlot("busy", 1));
        rl.sweep(System.currentTimeMillis() + 3_600_000L);
        assertEquals(1, rl.trackedKeys());
        rl.release("busy");
        rl.reset();
    }

    @Test
    public void deeplyNestedJsonIsAParseError() {
        StringBuilder sb = new StringBuilder("{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"ping\",\"params\":");
        int depth = JsonRpc.MAX_NESTING_DEPTH + 10;
        for (int i = 0; i < depth; i++) sb.append("{\"a\":");
        sb.append("1");
        for (int i = 0; i < depth; i++) sb.append("}");
        sb.append("}");
        try {
            JsonRpc.parse(sb.toString());
            fail("expected a parse error");
        } catch (JsonRpc.JsonRpcException e) {
            assertEquals(JsonRpc.PARSE_ERROR, e.getCode());
        }
        // a shallow body still parses
        try {
            assertEquals(1, JsonRpc.parse("{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"ping\",\"params\":{\"a\":{\"b\":1}}}").requests.size());
        } catch (JsonRpc.JsonRpcException e) {
            fail(e.getMessage());
        }
    }

    @Test
    public void redactorWithoutPatternsBehavesAsBefore() {
        McpRedactor r = new McpRedactor(Collections.singletonList("cardNumber"));
        assertTrue(r.isSensitive("cardnumber"));
        assertFalse(r.isSensitive("cardNumberHint"));
    }
}
