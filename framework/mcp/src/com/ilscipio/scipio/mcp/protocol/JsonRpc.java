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
package com.ilscipio.scipio.mcp.protocol;

import java.util.ArrayList;
import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.core.StreamReadConstraints;
import com.fasterxml.jackson.databind.DeserializationFeature;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.SerializationFeature;

/**
 * SCIPIO: 4.0.0: Minimal JSON-RPC 2.0 codec (Jackson). Handles single requests and batches.
 */
public final class JsonRpc {

    public static final int PARSE_ERROR = -32700;
    public static final int INVALID_REQUEST = -32600;
    public static final int METHOD_NOT_FOUND = -32601;
    public static final int INVALID_PARAMS = -32602;
    public static final int INTERNAL_ERROR = -32603;
    /** Server-defined: session or protocol state problem. */
    public static final int SERVER_ERROR = -32000;

    private static final ObjectMapper MAPPER = new ObjectMapper()
            .configure(DeserializationFeature.FAIL_ON_UNKNOWN_PROPERTIES, false)
            .configure(SerializationFeature.FAIL_ON_EMPTY_BEANS, false)
            .configure(SerializationFeature.WRITE_DATES_AS_TIMESTAMPS, false);

    /** Deepest JSON nesting accepted on input; a deeper body is a parse error, never a StackOverflowError. */
    public static final int MAX_NESTING_DEPTH = 64;

    static {
        MAPPER.getFactory().setStreamReadConstraints(StreamReadConstraints.builder().maxNestingDepth(MAX_NESTING_DEPTH).build());
    }

    private JsonRpc() {}

    public static ObjectMapper mapper() {
        return MAPPER;
    }

    public static final class Request {
        public final Object id;
        public final String method;
        public final Map<String, Object> params;
        public final boolean notification;
        public final String invalidReason;

        Request(Object id, String method, Map<String, Object> params, boolean notification, String invalidReason) {
            this.id = id;
            this.method = method;
            this.params = params != null ? params : Collections.emptyMap();
            this.notification = notification;
            this.invalidReason = invalidReason;
        }

        public boolean isInvalid() {
            return invalidReason != null;
        }
    }

    public static final class Parsed {
        public final List<Request> requests;
        public final boolean batch;

        Parsed(List<Request> requests, boolean batch) {
            this.requests = requests;
            this.batch = batch;
        }
    }

    public static class JsonRpcException extends Exception {
        private static final long serialVersionUID = 1L;
        private final int code;
        private final Object data;

        public JsonRpcException(int code, String message) {
            this(code, message, null);
        }

        public JsonRpcException(int code, String message, Object data) {
            super(message);
            this.code = code;
            this.data = data;
        }

        public int getCode() { return code; }
        public Object getData() { return data; }
    }

    /** Parses a body into requests. Throws JsonRpcException(PARSE_ERROR) for malformed JSON. */
    @SuppressWarnings("unchecked")
    public static Parsed parse(String body) throws JsonRpcException {
        Object root;
        try {
            root = MAPPER.readValue(body, Object.class);
        } catch (JsonProcessingException | StackOverflowError e) {
            throw new JsonRpcException(PARSE_ERROR, "Parse error");
        }
        if (root instanceof List) {
            List<Object> items = (List<Object>) root;
            if (items.isEmpty()) throw new JsonRpcException(INVALID_REQUEST, "Empty batch");
            List<Request> out = new ArrayList<>(items.size());
            for (Object item : items) out.add(toRequest(item));
            return new Parsed(out, true);
        }
        return new Parsed(Collections.singletonList(toRequest(root)), false);
    }

    @SuppressWarnings("unchecked")
    private static Request toRequest(Object item) {
        if (!(item instanceof Map)) {
            return new Request(null, null, null, false, "Request must be an object");
        }
        Map<String, Object> m = (Map<String, Object>) item;
        Object id = m.get("id");
        boolean hasId = m.containsKey("id") && id != null;
        if (!"2.0".equals(m.get("jsonrpc"))) {
            return new Request(hasId ? id : null, null, null, !hasId, "jsonrpc must be \"2.0\"");
        }
        Object method = m.get("method");
        if (!(method instanceof String) || ((String) method).isEmpty()) {
            return new Request(hasId ? id : null, null, null, !hasId, "method must be a string");
        }
        if (hasId && !(id instanceof String || id instanceof Number)) {
            return new Request(null, null, null, false, "id must be a string or number");
        }
        Object params = m.get("params");
        Map<String, Object> pm = null;
        if (params instanceof Map) {
            pm = (Map<String, Object>) params;
        } else if (params != null) {
            return new Request(hasId ? id : null, (String) method, null, !hasId, "params must be an object");
        }
        return new Request(hasId ? id : null, (String) method, pm, !hasId, null);
    }

    public static Map<String, Object> result(Object id, Object result) {
        Map<String, Object> m = new LinkedHashMap<>();
        m.put("jsonrpc", "2.0");
        m.put("id", id);
        m.put("result", result != null ? result : new LinkedHashMap<String, Object>());
        return m;
    }

    public static Map<String, Object> error(Object id, int code, String message, Object data) {
        Map<String, Object> err = new LinkedHashMap<>();
        err.put("code", code);
        err.put("message", message != null ? message : "Error");
        if (data != null) err.put("data", data);
        Map<String, Object> m = new LinkedHashMap<>();
        m.put("jsonrpc", "2.0");
        m.put("id", id);
        m.put("error", err);
        return m;
    }

    public static String write(Object value) {
        try {
            return MAPPER.writeValueAsString(value);
        } catch (JsonProcessingException e) {
            return "{\"jsonrpc\":\"2.0\",\"id\":null,\"error\":{\"code\":-32603,\"message\":\"Serialization error\"}}";
        }
    }

    public static String writePretty(Object value) {
        try {
            return MAPPER.writerWithDefaultPrettyPrinter().writeValueAsString(value);
        } catch (JsonProcessingException e) {
            return write(value);
        }
    }

    /** Reads a JSON string into a generic structure (Map/List/scalars); null on failure. */
    public static Object read(String json) {
        if (json == null || json.isEmpty()) return null;
        try {
            return MAPPER.readValue(json, Object.class);
        } catch (JsonProcessingException e) {
            return null;
        }
    }
}
