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
package com.ilscipio.scipio.mcp.catalog;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.math.BigDecimal;
import java.sql.Timestamp;
import java.util.Arrays;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

import org.junit.jupiter.api.Test;

public class ResultConverterTest {

    @Test
    @SuppressWarnings("unchecked")
    public void convertsValuesAndDropsUnknownObjects() {
        Map<String, Object> m = new LinkedHashMap<>();
        m.put("amount", new BigDecimal("12.50"));
        m.put("when", Timestamp.valueOf("2026-01-31 10:00:00"));
        m.put("list", Arrays.asList(1, "a", new Object()));
        m.put("thread", Thread.currentThread());
        m.put("nested", java.util.Collections.singletonMap("k", Boolean.TRUE));
        Map<String, Object> out = ResultConverter.toJsonMap(m);
        assertEquals("12.50", out.get("amount"));
        assertTrue(((String) out.get("when")).startsWith("2026-01-31T"));
        List<Object> list = (List<Object>) out.get("list");
        assertEquals(2, list.size());
        assertFalse(out.containsKey("thread"));
        assertEquals(Boolean.TRUE, ((Map<String, Object>) out.get("nested")).get("k"));
    }
}
