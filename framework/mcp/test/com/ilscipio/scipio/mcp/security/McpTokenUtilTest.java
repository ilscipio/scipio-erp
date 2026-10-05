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
import static org.junit.jupiter.api.Assertions.assertNotEquals;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import org.junit.jupiter.api.Test;

public class McpTokenUtilTest {

    @Test
    public void generatedTokenRoundTrips() {
        McpTokenUtil.Generated g = McpTokenUtil.generate();
        assertTrue(g.rawToken.startsWith("scp_"));
        assertEquals(12, g.tokenId.length());
        assertEquals(g.tokenId, McpTokenUtil.parseTokenId(g.rawToken));
        assertTrue(McpTokenUtil.verify(g.rawToken, g.hash));
        assertEquals(64, g.hash.length());
        assertTrue(g.displayPrefix.endsWith("..."));
        assertFalse(g.displayPrefix.contains(g.rawToken.substring(20, 40)));
    }

    @Test
    public void rejectsTamperedTokens() {
        McpTokenUtil.Generated g = McpTokenUtil.generate();
        String tampered = g.rawToken.substring(0, g.rawToken.length() - 8) + "AAAAAAAA";
        assertNull(McpTokenUtil.parseTokenId(tampered));
        String badSecret = g.rawToken.replaceFirst("_([A-Za-z0-9]{43})_", "_" + "x".repeat(43) + "_");
        assertNull(McpTokenUtil.parseTokenId(badSecret));
        assertFalse(McpTokenUtil.verify(g.rawToken + "x", g.hash));
        assertNull(McpTokenUtil.parseTokenId("scp_short"));
        assertNull(McpTokenUtil.parseTokenId(null));
    }

    @Test
    public void tokensAreUnique() {
        assertNotEquals(McpTokenUtil.generate().rawToken, McpTokenUtil.generate().rawToken);
        String s1 = McpTokenUtil.randomSessionId();
        assertNotEquals(s1, McpTokenUtil.randomSessionId());
        assertTrue(s1.matches("[A-Za-z0-9_-]{43}"));
    }
}
