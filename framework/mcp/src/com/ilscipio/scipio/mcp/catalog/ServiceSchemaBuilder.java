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
import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Set;

import org.ofbiz.service.ModelParam;
import org.ofbiz.service.ModelService;

/**
 * SCIPIO: 4.0.0: Derives JSON schemas from service parameter definitions.
 */
public final class ServiceSchemaBuilder {

    /** Parameters injected by the server and never exposed to agents. */
    public static final Set<String> HIDDEN_PARAMS = Collections.unmodifiableSet(new java.util.HashSet<>(java.util.Arrays.asList(
            "userLogin", "login.username", "login.password", "locale", "timeZone", "visualTheme", "request", "response", "session")));

    private ServiceSchemaBuilder() {}

    public static Map<String, Object> inputSchema(ModelService service, Set<String> exclude, Set<String> fixed) {
        Map<String, Object> properties = new LinkedHashMap<>();
        List<String> required = new ArrayList<>();
        for (ModelParam p : service.getModelParamList()) {
            if (!p.isIn()) continue;
            if (isHidden(p, exclude, fixed)) continue;
            Map<String, Object> ps = paramSchema(p);
            if (isAutoNowParam(p)) {
                ps.put("description", (ps.get("description") != null ? ps.get("description") + ". " : "") + "Defaults to now when omitted");
                properties.put(p.name, ps);
                continue;
            }
            properties.put(p.name, ps);
            if (!p.optional) required.add(p.name);
        }
        Map<String, Object> schema = new LinkedHashMap<>();
        schema.put("type", "object");
        schema.put("properties", properties);
        if (!required.isEmpty()) schema.put("required", required);
        schema.put("additionalProperties", false);
        return schema;
    }

    public static Map<String, Object> outputSchema(ModelService service) {
        Map<String, Object> properties = new LinkedHashMap<>();
        for (ModelParam p : service.getModelParamList()) {
            if (!p.isOut()) continue;
            if (p.internal || HIDDEN_PARAMS.contains(p.name)) continue;
            if (ModelService.RESPONSE_MESSAGE.equals(p.name) || ModelService.ERROR_MESSAGE.equals(p.name)
                    || ModelService.ERROR_MESSAGE_LIST.equals(p.name) || ModelService.ERROR_MESSAGE_MAP.equals(p.name)
                    || ModelService.SUCCESS_MESSAGE.equals(p.name) || ModelService.SUCCESS_MESSAGE_LIST.equals(p.name)) {
                continue;
            }
            properties.put(p.name, paramSchema(p));
        }
        Map<String, Object> schema = new LinkedHashMap<>();
        schema.put("type", "object");
        schema.put("properties", properties);
        schema.put("additionalProperties", true);
        return schema;
    }

    /**
     * A required {@code fromDate} Timestamp parameter (effective-date primary keys) that the UI fills with the current
     * time; the MCP layer fills it the same way so agents need not send it.
     */
    public static boolean isAutoNowParam(ModelParam p) {
        if (p.optional || !"fromDate".equals(p.name)) return false;
        String t = p.type != null ? p.type : "";
        return t.endsWith("Timestamp");
    }

    /** Fills auto-now parameters that the caller omitted. Returns the same map for chaining. */
    public static Map<String, Object> applyDefaults(ModelService service, Map<String, Object> params) {
        for (ModelParam p : service.getModelParamList()) {
            if (p.isIn() && isAutoNowParam(p) && params.get(p.name) == null) {
                params.put(p.name, org.ofbiz.base.util.UtilDateTime.nowTimestamp());
            }
        }
        return params;
    }

    public static boolean isHidden(ModelParam p, Set<String> exclude, Set<String> fixed) {
        if (p.internal) return true;
        if (HIDDEN_PARAMS.contains(p.name)) return true;
        if (exclude != null && exclude.contains(p.name)) return true;
        if (fixed != null && fixed.contains(p.name)) return true;
        return false;
    }

    public static Map<String, Object> paramSchema(ModelParam p) {
        Map<String, Object> s = typeSchema(p.type);
        if (p.description != null && !p.description.isEmpty()) {
            s.put("description", p.description);
        } else if (p.formLabel != null && !p.formLabel.isEmpty()) {
            s.put("description", p.formLabel);
        }
        if (p.getDefaultValue() != null && !String.valueOf(p.getDefaultValue()).isEmpty()) {
            s.put("default", String.valueOf(p.getDefaultValue()));
        }
        return s;
    }

    /** Maps a Java type name (as written in service definitions) to a JSON schema fragment. */
    public static Map<String, Object> typeSchema(String javaType) {
        Map<String, Object> s = new LinkedHashMap<>();
        String t = javaType != null ? javaType : "String";
        String simple = t.contains(".") ? t.substring(t.lastIndexOf('.') + 1) : t;
        switch (simple) {
            case "String":
                s.put("type", "string");
                break;
            case "Long": case "Integer": case "Short": case "long": case "int":
                s.put("type", "integer");
                break;
            case "BigDecimal":
                s.put("type", java.util.Arrays.asList("number", "string"));
                s.put("description", "Decimal number; pass a string such as \"12.50\" for exact amounts");
                s.put("x-javaType", t);
                break;
            case "Double": case "Float": case "double": case "float": case "BigInteger":
                s.put("type", "number");
                break;
            case "Boolean": case "boolean":
                s.put("type", "boolean");
                break;
            case "Timestamp": case "Date": case "Time": case "LocalDateTime": case "LocalDate": case "Instant":
                s.put("type", "string");
                s.put("format", "date-time");
                s.put("description", "ISO-8601 date-time, e.g. 2026-01-31T10:00:00Z, or yyyy-MM-dd HH:mm:ss");
                break;
            case "List": case "Set": case "Collection": case "ArrayList": case "LinkedList":
                s.put("type", "array");
                s.put("items", new LinkedHashMap<String, Object>());
                break;
            case "Map": case "GenericValue": case "GenericEntity": case "HashMap": case "LinkedHashMap":
                s.put("type", "object");
                s.put("additionalProperties", true);
                break;
            case "Locale": case "TimeZone":
                s.put("type", "string");
                s.put("x-javaType", t);
                break;
            default:
                s.put("type", "string");
                s.put("x-javaType", t);
                break;
        }
        return s;
    }

    /** Converts a service name to snake_case (createOrder -> create_order). */
    public static String toSnakeCase(String name) {
        StringBuilder sb = new StringBuilder(name.length() + 8);
        for (int i = 0; i < name.length(); i++) {
            char c = name.charAt(i);
            if (Character.isUpperCase(c)) {
                if (i > 0 && (Character.isLowerCase(name.charAt(i - 1)) || (i + 1 < name.length() && Character.isLowerCase(name.charAt(i + 1))))) {
                    sb.append('_');
                }
                sb.append(Character.toLowerCase(c));
            } else if (c == '.' || c == '-' || c == ' ') {
                sb.append('_');
            } else {
                sb.append(c);
            }
        }
        return sb.toString().toLowerCase(Locale.ROOT);
    }
}
