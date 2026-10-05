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

import java.util.ArrayList;
import java.util.Collection;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Set;
import java.util.TreeSet;

/**
 * SCIPIO: 4.0.0: Masks sensitive fields in JSON-safe structures, strips control characters and truncates text.
 * A field is sensitive when its name is in {@code mcp.redact.fields} (exact, case-insensitive) or matches one of
 * the {@code mcp.redact.patterns} globs (case-insensitive, {@code *} wildcard).
 */
public final class McpRedactor {

    public static final String MASK = "***";

    private final Set<String> fields;
    private final List<String> patterns;

    public McpRedactor(Collection<String> fieldNames) {
        this(fieldNames, null);
    }

    public McpRedactor(Collection<String> fieldNames, Collection<String> patterns) {
        Set<String> s = new TreeSet<>();
        if (fieldNames != null) {
            for (String f : fieldNames) s.add(f.toLowerCase(Locale.ROOT));
        }
        this.fields = s;
        List<String> p = new ArrayList<>();
        if (patterns != null) {
            for (String g : patterns) {
                if (g != null && !g.trim().isEmpty()) p.add(g.trim().toLowerCase(Locale.ROOT));
            }
        }
        this.patterns = p;
    }

    public static McpRedactor fromConfig() {
        return new McpRedactor(McpConfig.getRedactFields(), McpConfig.getRedactPatterns());
    }

    public boolean isSensitive(String fieldName) {
        if (fieldName == null) return false;
        String lower = fieldName.toLowerCase(Locale.ROOT);
        if (fields.contains(lower)) return true;
        for (String p : patterns) {
            if (McpConfig.globMatches(p, lower)) return true;
        }
        return false;
    }

    /** Deep-copies maps and lists, masking sensitive keys and cleaning strings. Other values pass through. */
    @SuppressWarnings("unchecked")
    public Object redact(Object value) {
        if (value instanceof Map) {
            Map<String, Object> out = new LinkedHashMap<>();
            for (Map.Entry<Object, Object> e : ((Map<Object, Object>) value).entrySet()) {
                String key = String.valueOf(e.getKey());
                Object v = e.getValue();
                out.put(key, isSensitive(key) && v != null ? MASK : redact(v));
            }
            return out;
        }
        if (value instanceof Collection) {
            List<Object> out = new ArrayList<>();
            for (Object o : (Collection<Object>) value) out.add(redact(o));
            return out;
        }
        if (value instanceof String) {
            return stripControl((String) value);
        }
        return value;
    }

    /** Removes C0 control characters except tab, newline and carriage return. */
    public static String stripControl(String s) {
        if (s == null) return null;
        StringBuilder sb = null;
        for (int i = 0; i < s.length(); i++) {
            char c = s.charAt(i);
            boolean bad = (c < 0x20 && c != '\t' && c != '\n' && c != '\r') || c == 0x7f;
            if (bad) {
                if (sb == null) {
                    sb = new StringBuilder(s.length());
                    sb.append(s, 0, i);
                }
            } else if (sb != null) {
                sb.append(c);
            }
        }
        return sb != null ? sb.toString() : s;
    }

    public static String truncate(String s, int maxChars) {
        if (s == null || s.length() <= maxChars) return s;
        return s.substring(0, Math.max(0, maxChars)) + "\n...[truncated " + (s.length() - maxChars) + " chars]";
    }
}
