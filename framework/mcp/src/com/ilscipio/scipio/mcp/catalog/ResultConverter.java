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

import java.math.BigDecimal;
import java.sql.Timestamp;
import java.time.Instant;
import java.time.ZoneOffset;
import java.time.format.DateTimeFormatter;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Date;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.TimeZone;

import org.ofbiz.entity.GenericValue;
import org.ofbiz.entity.model.ModelEntity;

/**
 * SCIPIO: 4.0.0: Converts service results and entity values into JSON-safe structures.
 *
 * <p>Never serialize raw OFBiz maps with Jackson: they carry Delegator, LocalDispatcher, carts and iterators.
 * Unknown object types are dropped (map keys omitted, list entries replaced with their {@code toString()} only
 * for simple value types).</p>
 */
public final class ResultConverter {

    private static final int MAX_DEPTH = 12;
    private static final DateTimeFormatter ISO = DateTimeFormatter.ISO_OFFSET_DATE_TIME;

    private ResultConverter() {}

    public static Object toJson(Object value) {
        return convert(value, 0);
    }

    @SuppressWarnings("unchecked")
    public static Map<String, Object> toJsonMap(Map<String, ?> value) {
        Object o = convert(value, 0);
        return o instanceof Map ? (Map<String, Object>) o : new LinkedHashMap<>();
    }

    /** True for types that convert to a JSON scalar. */
    public static boolean isScalar(Object v) {
        return v == null || v instanceof String || v instanceof Number || v instanceof Boolean
                || v instanceof Date || v instanceof Enum || v instanceof Character
                || v instanceof Locale || v instanceof TimeZone || v instanceof java.time.temporal.Temporal;
    }

    @SuppressWarnings("unchecked")
    private static Object convert(Object v, int depth) {
        if (v == null) return null;
        if (depth > MAX_DEPTH) return null;
        if (v instanceof String) return v;
        if (v instanceof Boolean) return v;
        if (v instanceof BigDecimal) return ((BigDecimal) v).toPlainString();
        if (v instanceof Number) return v;
        if (v instanceof Timestamp) return ISO.format(((Timestamp) v).toInstant().atOffset(ZoneOffset.UTC));
        if (v instanceof Date) return ISO.format(Instant.ofEpochMilli(((Date) v).getTime()).atOffset(ZoneOffset.UTC));
        if (v instanceof java.time.temporal.Temporal) return v.toString();
        if (v instanceof Enum) return ((Enum<?>) v).name();
        if (v instanceof Character || v instanceof Locale || v instanceof TimeZone) return v.toString();
        if (v instanceof GenericValue) return convertGenericValue((GenericValue) v, depth);
        if (v instanceof Map) {
            Map<String, Object> out = new LinkedHashMap<>();
            for (Map.Entry<Object, Object> e : ((Map<Object, Object>) v).entrySet()) {
                if (e.getKey() == null) continue;
                Object cv = convert(e.getValue(), depth + 1);
                if (cv != null || e.getValue() == null) out.put(String.valueOf(e.getKey()), cv);
            }
            return out;
        }
        if (v instanceof Collection) {
            List<Object> out = new ArrayList<>();
            for (Object o : (Collection<Object>) v) {
                Object cv = convert(o, depth + 1);
                if (cv != null) out.add(cv);
            }
            return out;
        }
        if (v instanceof Object[]) {
            List<Object> out = new ArrayList<>();
            for (Object o : (Object[]) v) {
                Object cv = convert(o, depth + 1);
                if (cv != null) out.add(cv);
            }
            return out;
        }
        if (v instanceof byte[]) return "<binary " + ((byte[]) v).length + " bytes>";
        if (v instanceof java.nio.ByteBuffer) return "<binary " + ((java.nio.ByteBuffer) v).remaining() + " bytes>";
        // Unknown framework objects (Delegator, dispatcher, carts, iterators, ...) are dropped.
        return null;
    }

    private static Map<String, Object> convertGenericValue(GenericValue gv, int depth) {
        Map<String, Object> out = new LinkedHashMap<>();
        ModelEntity model = gv.getModelEntity();
        out.put("_entity", gv.getEntityName());
        for (String field : model.getAllFieldNames()) {
            Object fv = gv.get(field);
            if (fv == null) continue;
            if (model.getField(field) != null && model.getField(field).getEncryptMethod() != null
                    && model.getField(field).getEncryptMethod().isEncrypted()) {
                out.put(field, "***");
                continue;
            }
            out.put(field, convert(fv, depth + 1));
        }
        return out;
    }
}
