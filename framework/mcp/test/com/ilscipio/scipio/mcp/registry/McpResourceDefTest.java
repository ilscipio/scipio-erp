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

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.util.Map;

import org.junit.jupiter.api.Test;

public class McpResourceDefTest {

    @Test
    public void matchesTemplatesAndConcreteUris() {
        McpResourceDef t = new McpResourceDef("scipio://skills/{name}", "skill", "", "text/markdown", (c, u, p) -> "");
        assertTrue(t.isTemplate());
        Map<String, String> p = t.match("scipio://skills/order-management");
        assertEquals("order-management", p.get("name"));
        assertNull(t.match("scipio://skills/a/b"));
        assertNull(t.match("scipio://other/x"));

        McpResourceDef c = new McpResourceDef("scipio://docs/readme", "readme", "", "text/plain", (cx, u, pp) -> "");
        assertFalse(c.isTemplate());
        assertTrue(c.match("scipio://docs/readme").isEmpty());
        assertNull(c.match("scipio://docs/readme2"));
    }
}
