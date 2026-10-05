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
package com.ilscipio.scipio.channel.core;

import java.util.LinkedHashMap;
import java.util.Map;

/**
 * Writes and reads a flat JSON object with string values, for the small JSON fields of the channel entities
 * (variation axes, errors). Nothing else: no nesting, no numbers.
 *
 * <p>SCIPIO: 4.0.0: Added (W1-08).</p>
 */
public final class FlatJson {
    private FlatJson() {
    }

    public static String write(Map<String, String> map) {
        StringBuilder sb = new StringBuilder("{");
        boolean first = true;
        for (Map.Entry<String, String> e : map.entrySet()) {
            if (!first) {
                sb.append(',');
            }
            first = false;
            sb.append(SyncQueue.jsonString(e.getKey())).append(':').append(SyncQueue.jsonString(e.getValue()));
        }
        return sb.append('}').toString();
    }

    public static Map<String, String> read(String json) {
        Map<String, String> out = new LinkedHashMap<>();
        if (json == null || json.trim().isEmpty()) {
            return out;
        }
        int[] pos = {0};
        skipWs(json, pos);
        expect(json, pos, '{');
        skipWs(json, pos);
        if (peek(json, pos) == '}') {
            return out;
        }
        while (true) {
            skipWs(json, pos);
            String key = readString(json, pos);
            skipWs(json, pos);
            expect(json, pos, ':');
            skipWs(json, pos);
            String value = readString(json, pos);
            out.put(key, value);
            skipWs(json, pos);
            char c = json.charAt(pos[0]++);
            if (c == '}') {
                return out;
            }
            if (c != ',') {
                throw new IllegalArgumentException("Bad JSON at " + (pos[0] - 1));
            }
        }
    }

    private static char peek(String s, int[] pos) {
        return pos[0] < s.length() ? s.charAt(pos[0]) : '\0';
    }

    private static void skipWs(String s, int[] pos) {
        while (pos[0] < s.length() && Character.isWhitespace(s.charAt(pos[0]))) {
            pos[0]++;
        }
    }

    private static void expect(String s, int[] pos, char c) {
        if (peek(s, pos) != c) {
            throw new IllegalArgumentException("Bad JSON at " + pos[0] + ": expected " + c);
        }
        pos[0]++;
    }

    private static String readString(String s, int[] pos) {
        expect(s, pos, '"');
        StringBuilder sb = new StringBuilder();
        while (pos[0] < s.length()) {
            char c = s.charAt(pos[0]++);
            if (c == '"') {
                return sb.toString();
            }
            if (c == '\\' && pos[0] < s.length()) {
                char n = s.charAt(pos[0]++);
                sb.append(n == 'n' ? '\n' : n == 't' ? '\t' : n);
            } else {
                sb.append(c);
            }
        }
        throw new IllegalArgumentException("Bad JSON: unterminated string");
    }
}
