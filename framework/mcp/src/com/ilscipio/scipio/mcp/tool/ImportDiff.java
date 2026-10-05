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

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;

import com.ilscipio.scipio.mcp.registry.McpToolException;

/**
 * SCIPIO: 4.0.0: Diff-then-apply record for the import tools ({@code product_import}, {@code bom_import},
 * {@code supplier_import}, ...). A tool collects what it would create, update or leave unchanged plus a list of
 * doubts (rows it cannot map with confidence). Without {@code apply} the tool returns the diff; with {@code apply}
 * it writes, but only when there are no doubts or {@code force} is set.
 */
public final class ImportDiff {

    private final boolean apply;
    private final boolean force;
    private final List<Map<String, Object>> creates = new ArrayList<>();
    private final List<Map<String, Object>> updates = new ArrayList<>();
    private final List<Map<String, Object>> unchanged = new ArrayList<>();
    private final List<Map<String, Object>> doubts = new ArrayList<>();
    private final List<Map<String, Object>> written = new ArrayList<>();

    public ImportDiff(Boolean apply, Boolean force) {
        this.apply = Boolean.TRUE.equals(apply);
        this.force = Boolean.TRUE.equals(force);
    }

    public boolean isApply() { return apply; }
    public boolean isForce() { return force; }
    public boolean hasDoubts() { return !doubts.isEmpty(); }

    /** True when the tool may write now: apply requested and (no doubts or force). */
    public boolean canWrite() { return apply && (doubts.isEmpty() || force); }

    public void create(String kind, String key, Map<String, Object> data) {
        creates.add(entry(kind, key, data, null));
    }

    public void update(String kind, String key, Map<String, Object> before, Map<String, Object> after) {
        Map<String, Object> e = entry(kind, key, after, null);
        e.put("before", before);
        updates.add(e);
    }

    public void unchanged(String kind, String key) {
        unchanged.add(entry(kind, key, null, null));
    }

    public void doubt(int rowIndex, String message) {
        Map<String, Object> e = new LinkedHashMap<>();
        e.put("row", rowIndex);
        e.put("message", message);
        doubts.add(e);
    }

    public void written(String kind, String key, Map<String, Object> ids) {
        written.add(entry(kind, key, ids, null));
    }

    private static Map<String, Object> entry(String kind, String key, Map<String, Object> data, String note) {
        Map<String, Object> e = new LinkedHashMap<>();
        e.put("kind", kind);
        e.put("key", key);
        if (data != null) e.put("data", data);
        if (note != null) e.put("note", note);
        return e;
    }

    /** The tool result: counts, the four lists, and a next-step line an agent can act on. */
    public Map<String, Object> result() {
        Map<String, Object> out = new LinkedHashMap<>();
        out.put("apply", apply);
        Map<String, Object> counts = new LinkedHashMap<>();
        counts.put("creates", creates.size());
        counts.put("updates", updates.size());
        counts.put("unchanged", unchanged.size());
        counts.put("doubts", doubts.size());
        counts.put("written", written.size());
        out.put("counts", counts);
        out.put("creates", creates);
        out.put("updates", updates);
        out.put("unchanged", unchanged);
        out.put("doubts", doubts);
        if (!written.isEmpty()) out.put("written", written);
        if (!apply) {
            out.put("next", doubts.isEmpty() ? "Review the diff with the user, then call again with apply=true."
                    : "Resolve the doubts with the user (fix the rows), then call again with apply=true; or apply=true with force=true to skip the doubtful rows.");
        } else if (!doubts.isEmpty() && !force) {
            out.put("next", "Nothing was written: " + doubts.size() + " doubt(s). Fix the rows or pass force=true.");
        } else {
            out.put("next", "Done.");
        }
        return out;
    }

    // ---- row helpers ----

    /** Validates the {@code rows} argument: a non-empty list of objects. */
    @SuppressWarnings("unchecked")
    public static List<Map<String, Object>> rows(Object rowsArg) throws McpToolException {
        if (!(rowsArg instanceof List) || ((List<?>) rowsArg).isEmpty()) {
            throw new McpToolException("rows must be a non-empty array of objects");
        }
        List<Map<String, Object>> out = new ArrayList<>();
        int i = 0;
        for (Object o : (List<Object>) rowsArg) {
            if (!(o instanceof Map)) throw new McpToolException("rows[" + i + "] is not an object");
            out.add((Map<String, Object>) o);
            i++;
        }
        return out;
    }

    /** First non-empty value among the alias keys, trimmed; null when none. Keys match case-insensitively. */
    public static String str(Map<String, Object> row, String... keys) {
        for (String k : keys) {
            Object v = valueIgnoreCase(row, k);
            if (v != null) {
                String s = String.valueOf(v).trim();
                if (!s.isEmpty()) return s;
            }
        }
        return null;
    }

    public static BigDecimal decimal(Map<String, Object> row, String... keys) {
        String s = str(row, keys);
        if (s == null) return null;
        s = s.replace(',', '.').replaceAll("[^0-9.\\-]", "");
        if (s.isEmpty() || s.equals("-") || s.equals(".")) return null;
        try {
            return new BigDecimal(s);
        } catch (NumberFormatException e) {
            return null;
        }
    }

    public static Integer integer(Map<String, Object> row, String... keys) {
        BigDecimal d = decimal(row, keys);
        return d != null ? d.intValue() : null;
    }

    private static Object valueIgnoreCase(Map<String, Object> row, String key) {
        Object v = row.get(key);
        if (v != null) return v;
        for (Map.Entry<String, Object> e : row.entrySet()) {
            if (e.getKey() != null && e.getKey().replace("_", "").replace(" ", "").equalsIgnoreCase(key.replace("_", ""))) return e.getValue();
        }
        return null;
    }

    /** Entity id from a free text: upper case, non-alphanumerics to underscore, at most {@code max} chars. */
    public static String slug(String s, int max) {
        if (s == null) return null;
        String id = s.trim().toUpperCase(Locale.ROOT).replaceAll("[^A-Z0-9]+", "_").replaceAll("^_+|_+$", "");
        if (id.length() > max) id = id.substring(0, max);
        return id.isEmpty() ? null : id;
    }
}
