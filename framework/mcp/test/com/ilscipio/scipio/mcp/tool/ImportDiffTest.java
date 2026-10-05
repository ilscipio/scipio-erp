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
package com.ilscipio.scipio.mcp.tool;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.math.BigDecimal;
import java.util.Arrays;
import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

import org.junit.jupiter.api.Test;

import com.ilscipio.scipio.mcp.registry.McpToolException;

/** Diff-then-apply rules and the tolerant row readers of the import tools. */
public class ImportDiffTest {

    @Test
    public void previewNeverWritesAndDoubtsBlockApplyUnlessForced() {
        ImportDiff preview = new ImportDiff(false, false);
        preview.create("product", "SKU-1", Collections.singletonMap("name", "Bolt"));
        preview.doubt(3, "no name");
        assertFalse(preview.canWrite());
        Map<String, Object> r = preview.result();
        assertEquals(false, r.get("apply"));
        assertEquals(1, ((Map<?, ?>) r.get("counts")).get("creates"));
        assertEquals(1, ((Map<?, ?>) r.get("counts")).get("doubts"));

        ImportDiff apply = new ImportDiff(true, false);
        apply.doubt(1, "unknown unit");
        assertFalse(apply.canWrite());
        assertTrue(String.valueOf(apply.result().get("next")).contains("Nothing was written"));

        ImportDiff forced = new ImportDiff(true, true);
        forced.doubt(1, "unknown unit");
        assertTrue(forced.canWrite());

        ImportDiff clean = new ImportDiff(true, false);
        clean.update("supplierProduct", "P1/S1", Collections.singletonMap("lastPrice", "1"), Collections.singletonMap("lastPrice", "2"));
        assertTrue(clean.canWrite());
        assertEquals("Done.", clean.result().get("next"));
    }

    @Test
    public void rowReadersAcceptAliasesCaseAndNumberFormats() throws McpToolException {
        Map<String, Object> row = new LinkedHashMap<>();
        row.put("Part Number", " SKU-9 ");
        row.put("Qty", "1,5");
        row.put("lead_days", "12");
        row.put("price", "");
        assertEquals("SKU-9", ImportDiff.str(row, "sku", "partNumber"));
        assertEquals(new BigDecimal("1.5"), ImportDiff.decimal(row, "quantity", "qty"));
        assertEquals(Integer.valueOf(12), ImportDiff.integer(row, "leadDays"));
        assertNull(ImportDiff.decimal(row, "price"));
        assertNull(ImportDiff.str(row, "missing"));
        assertEquals("MPL_9290_FRAME", ImportDiff.slug("MPL-9290 frame!", 20));
        assertEquals("ABCDEFGHIJ", ImportDiff.slug("abcdefghijklmnop", 10));

        List<Map<String, Object>> rows = ImportDiff.rows(Arrays.asList(row));
        assertEquals(1, rows.size());
        assertThrows(McpToolException.class, () -> ImportDiff.rows(Collections.emptyList()));
        assertThrows(McpToolException.class, () -> ImportDiff.rows(Arrays.asList("not a row")));
        assertThrows(McpToolException.class, () -> ImportDiff.rows("rows"));
    }
}
