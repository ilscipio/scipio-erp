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

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.Map;

import com.ilscipio.scipio.mcp.security.McpConfig;

/**
 * SCIPIO: 4.0.0: Small JSON-schema subset validator for tool arguments: required, additionalProperties,
 * type, enum, string and array size limits. Returns human-readable problems.
 */
public final class JsonSchemaValidator {

    private JsonSchemaValidator() {}

    @SuppressWarnings("unchecked")
    public static List<String> validate(Map<String, Object> schema, Map<String, Object> args) {
        List<String> problems = new ArrayList<>();
        if (schema == null) return problems;
        Map<String, Object> properties = schema.get("properties") instanceof Map ? (Map<String, Object>) schema.get("properties") : java.util.Collections.emptyMap();
        Object req = schema.get("required");
        if (req instanceof Collection) {
            for (Object r : (Collection<Object>) req) {
                if (args.get(String.valueOf(r)) == null) problems.add("missing required argument: " + r);
            }
        }
        boolean additional = !Boolean.FALSE.equals(schema.get("additionalProperties"));
        int maxString = McpConfig.getStringMaxLength();
        int maxArray = McpConfig.getArrayMaxLength();
        for (Map.Entry<String, Object> e : args.entrySet()) {
            Object propSchema = properties.get(e.getKey());
            if (propSchema == null) {
                if (!additional) problems.add("unknown argument: " + e.getKey());
                checkSize(e.getKey(), e.getValue(), maxString, maxArray, problems);
                continue;
            }
            Object v = e.getValue();
            if (v == null) continue;
            Map<String, Object> ps = (Map<String, Object>) propSchema;
            Object typeSpec = ps.get("type");
            if (typeSpec instanceof String && !matchesType((String) typeSpec, v)) {
                problems.add("argument " + e.getKey() + " must be of type " + typeSpec);
                continue;
            }
            if (typeSpec instanceof Collection) {
                boolean any = false;
                for (Object t : (Collection<Object>) typeSpec) {
                    if (t instanceof String && matchesType((String) t, v)) { any = true; break; }
                }
                if (!any) {
                    problems.add("argument " + e.getKey() + " must be one of the types " + typeSpec);
                    continue;
                }
            }
            Object en = ps.get("enum");
            if (en instanceof Collection && !((Collection<Object>) en).contains(v)) {
                problems.add("argument " + e.getKey() + " must be one of " + en);
            }
            checkSize(e.getKey(), v, maxString, maxArray, problems);
        }
        return problems;
    }

    private static void checkSize(String name, Object v, int maxString, int maxArray, List<String> problems) {
        if (v instanceof String && ((String) v).length() > maxString) {
            problems.add("argument " + name + " exceeds " + maxString + " characters");
        } else if (v instanceof Collection && ((Collection<?>) v).size() > maxArray) {
            problems.add("argument " + name + " exceeds " + maxArray + " items");
        }
    }

    public static boolean matchesType(String type, Object v) {
        switch (type) {
            case "string": return v instanceof String;
            case "integer": return (v instanceof Number && !(v instanceof Double || v instanceof Float)) || (v instanceof Double && ((Double) v) % 1 == 0);
            case "number": return v instanceof Number;
            case "boolean": return v instanceof Boolean;
            case "array": return v instanceof Collection;
            case "object": return v instanceof Map;
            case "null": return v == null;
            default: return true;
        }
    }
}
