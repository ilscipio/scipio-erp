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

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.lang.reflect.Modifier;
import java.lang.reflect.Parameter;
import java.math.BigDecimal;
import java.sql.Timestamp;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.UtilDateTime;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.service.DispatchContext;
import org.ofbiz.service.ModelService;

import com.ilscipio.scipio.ce.base.component.ComponentReflectInfo;
import com.ilscipio.scipio.ce.base.component.ComponentReflectRegistry;
import com.ilscipio.scipio.mcp.catalog.ResultConverter;
import com.ilscipio.scipio.mcp.catalog.ServiceSchemaBuilder;
import com.ilscipio.scipio.mcp.def.McpAccess;
import com.ilscipio.scipio.mcp.def.McpParam;
import com.ilscipio.scipio.mcp.def.McpPrompt;
import com.ilscipio.scipio.mcp.def.McpResource;
import com.ilscipio.scipio.mcp.def.McpServer;
import com.ilscipio.scipio.mcp.def.McpServerExtension;
import com.ilscipio.scipio.mcp.def.McpServiceTool;
import com.ilscipio.scipio.mcp.def.McpTool;
import com.ilscipio.scipio.mcp.def.McpTopic;

/**
 * SCIPIO: 4.0.0: Builds {@link McpServerDef}s from {@code @McpServer} classes found in every component.
 */
public final class McpAnnotationReader {

    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());

    private McpAnnotationReader() {}

    /**
     * Reads every {@code @McpServer} class, then merges every {@code @McpServerExtension} class into the server it
     * names. Servers and extensions may live in any component (framework, applications, addons, hot-deploy).
     */
    public static List<McpServerDef> readAll(DispatchContext dctx) {
        Map<String, Pending> pending = new LinkedHashMap<>();
        List<Class<?>> extensions = new ArrayList<>();
        for (ComponentReflectInfo cri : ComponentReflectRegistry.getReflectInfos()) {
            for (Class<?> cls : cri.getReflectQuery().getAnnotatedClasses(McpServer.class)) {
                McpServer ann = cls.getAnnotation(McpServer.class);
                if (ann == null) continue;
                if (pending.containsKey(ann.name())) {
                    Debug.logWarning("MCP: duplicate server name " + ann.name() + " (" + cls.getName() + "); first kept. Use @McpServerExtension to add tools to an existing server.", module);
                    continue;
                }
                try {
                    pending.put(ann.name(), readServer(cls, ann, dctx));
                } catch (RuntimeException e) {
                    Debug.logError(e, "MCP: could not read server profile " + cls.getName(), module);
                }
            }
            extensions.addAll(cri.getReflectQuery().getAnnotatedClasses(McpServerExtension.class));
        }
        for (Class<?> cls : extensions) {
            McpServerExtension ext = cls.getAnnotation(McpServerExtension.class);
            if (ext == null) continue;
            Pending p = pending.get(ext.server());
            if (p == null) {
                Debug.logWarning("MCP: extension " + cls.getName() + " names unknown server " + ext.server() + "; skipped", module);
                continue;
            }
            try {
                p.builder.featuredServices(ext.featuredServices()).serviceAllow(ext.serviceAllow()).serviceDeny(ext.serviceDeny())
                        .entities(ext.entities());
                for (Class<? extends McpToolProvider> prov : ext.providers()) p.builder.provider(prov);
                addMembers(p, cls, dctx, ext.serviceTools(), ext.topics());
                Debug.logInfo("MCP: server " + ext.server() + " extended by " + cls.getName(), module);
            } catch (RuntimeException e) {
                Debug.logError(e, "MCP: could not read server extension " + cls.getName(), module);
            }
        }
        List<McpServerDef> out = new ArrayList<>();
        for (Pending p : pending.values()) out.add(p.build());
        return out;
    }

    /** Reads one {@code @McpServer} class into a finished definition (no extensions applied). */
    public static McpServerDef read(Class<?> cls, McpServer ann, DispatchContext dctx) {
        return readServer(cls, ann, dctx).build();
    }

    /** One server while its classes are read: standalone tools go to the builder, topic members wait for assembly. */
    private static final class Pending {
        final McpServerDef.Builder builder;
        final Set<String> names = new HashSet<>();
        final Map<String, List<McpToolDef>> topicMembers = new LinkedHashMap<>();
        final Map<String, McpTopic> topics = new LinkedHashMap<>();

        Pending(McpServerDef.Builder builder) {
            this.builder = builder;
        }

        void addTool(McpToolDef def, String source) {
            if (names.add(def.getName())) builder.tool(def);
            else Debug.logWarning("MCP: duplicate tool " + def.getName() + " in " + source + "; ignored", module);
        }

        McpServerDef build() {
            for (Map.Entry<String, List<McpToolDef>> e : topicMembers.entrySet()) {
                String topic = e.getKey();
                List<McpToolDef> members = new ArrayList<>(e.getValue());
                members.sort(java.util.Comparator.comparingInt(McpAnnotationReader::actionRank).thenComparingInt(McpToolDef::getOrder).thenComparing(McpToolDef::getName));
                McpTopic ann = topics.get(topic);
                if (ann == null) {
                    Debug.logWarning("MCP: topic " + topic + " has no @McpTopic (" + members.get(0).getSource() + "); title and description derived", module);
                }
                try {
                    McpToolDef composite = McpTopicTool.build(topic, ann != null ? ann.title() : "", ann != null ? ann.description() : "",
                            ann != null ? ann.order() : McpToolDef.DEFAULT_ORDER, ann != null && ann.featured(), members);
                    addTool(composite, members.get(0).getSource());
                } catch (RuntimeException ex) {
                    Debug.logError(ex, "MCP: could not build topic tool " + topic, module);
                }
            }
            return builder.build();
        }
    }

    /** Reads first (find, get, other reads), then creates, then the other writes; ties by {@code order}, then name. */
    static int actionRank(McpToolDef t) {
        String n = t.getName();
        if (n.equals("find") || n.equals("search") || n.equals("list")) return 0;
        if (n.equals("get")) return 1;
        if (t.isReadOnly()) return 2;
        if (n.startsWith("create")) return 3;
        return 4;
    }

    private static Pending readServer(Class<?> cls, McpServer ann, DispatchContext dctx) {
        McpServerDef.Builder b = McpServerDef.builder(ann.name())
                .title(ann.title()).description(ann.description()).component(ann.component())
                .webapps(ann.webapps()).featuredServices(ann.featuredServices())
                .serviceAllow(ann.serviceAllow()).serviceDeny(ann.serviceDeny()).entities(ann.entities())
                .allowAnonymous(ann.allowAnonymous()).requiredPermission(ann.requiredPermission()).hub(ann.hub())
                .sourceClass(cls.getName());
        for (Class<? extends McpToolProvider> p : ann.providers()) b.provider(p);
        Pending pending = new Pending(b);
        addMembers(pending, cls, dctx, ann.serviceTools(), ann.topics());
        return pending;
    }

    /**
     * Adds the tools, resources, prompts, service tools and topics declared by one class (server or extension).
     * A tool or service tool with a {@code topic} becomes one action of that composite tool; the composites are
     * assembled when the server is built, so extensions may add actions to an existing topic.
     */
    private static void addMembers(Pending p, Class<?> cls, DispatchContext dctx, McpServiceTool[] serviceTools, McpTopic[] topics) {
        for (McpTopic t : topics) {
            if (p.topics.putIfAbsent(t.name(), t) != null) {
                Debug.logWarning("MCP: duplicate topic " + t.name() + " in " + cls.getName() + "; first kept", module);
            }
        }
        for (Method m : cls.getDeclaredMethods()) {
            if (!Modifier.isStatic(m.getModifiers()) || !Modifier.isPublic(m.getModifiers())) continue;
            McpTool t = m.getAnnotation(McpTool.class);
            if (t != null) {
                try {
                    McpToolDef def = toolFromMethod(m, t);
                    if (!t.topic().isEmpty()) p.topicMembers.computeIfAbsent(t.topic(), k -> new ArrayList<>()).add(def);
                    else p.addTool(def, cls.getName());
                } catch (RuntimeException e) {
                    Debug.logError(e, "MCP: invalid tool method " + cls.getName() + "." + m.getName(), module);
                }
            }
            McpResource r = m.getAnnotation(McpResource.class);
            if (r != null) p.builder.resource(resourceFromMethod(m, r));
            McpPrompt pr = m.getAnnotation(McpPrompt.class);
            if (pr != null) p.builder.prompt(promptFromMethod(m, pr));
        }
        for (McpServiceTool st : serviceTools) {
            ModelService svc = dctx != null ? dctx.getModelServiceOrNull(st.service()) : null;
            if (svc == null) {
                Debug.logWarning("MCP: service tool " + st.service() + " in " + cls.getName() + " refers to an unknown service; skipped", module);
                continue;
            }
            McpToolDef def = toolFromService(st, svc, cls.getName());
            if (!st.topic().isEmpty()) p.topicMembers.computeIfAbsent(st.topic(), k -> new ArrayList<>()).add(def);
            else p.addTool(def, cls.getName());
        }
    }

    // ---- tools from methods ----

    public static McpToolDef toolFromMethod(Method m, McpTool t) {
        Parameter[] params = m.getParameters();
        if (params.length == 0 || !McpCallContext.class.isAssignableFrom(params[0].getType())) {
            throw new IllegalArgumentException("first parameter must be McpCallContext");
        }
        boolean mapMode = params.length == 2 && Map.class.isAssignableFrom(params[1].getType()) && params[1].getAnnotation(McpParam.class) == null;
        String[] names = new String[params.length];
        Class<?>[] types = new Class<?>[params.length];
        Map<String, Object> properties = new LinkedHashMap<>();
        List<String> required = new ArrayList<>();
        if (!mapMode) {
            for (int i = 1; i < params.length; i++) {
                McpParam mp = params[i].getAnnotation(McpParam.class);
                String name = mp != null && !mp.name().isEmpty() ? mp.name() : params[i].getName();
                names[i] = name;
                types[i] = params[i].getType();
                Map<String, Object> ps = mp != null && !mp.type().isEmpty() ? typeOnly(mp.type()) : ServiceSchemaBuilder.typeSchema(params[i].getType().getSimpleName());
                if (mp != null) {
                    if (!mp.description().isEmpty()) ps.put("description", mp.description());
                    if (mp.enumValues().length > 0) ps.put("enum", Arrays.asList(mp.enumValues()));
                    if (!mp.example().isEmpty()) ps.put("examples", java.util.Collections.singletonList(mp.example()));
                    if (mp.required()) required.add(name);
                } else {
                    required.add(name);
                }
                properties.put(name, ps);
            }
        }
        Map<String, Object> schema = McpToolDef.emptyObjectSchema();
        schema.put("properties", properties);
        if (!required.isEmpty()) schema.put("required", required);
        if (mapMode) schema.put("additionalProperties", true);
        final boolean finalMapMode = mapMode;
        m.setAccessible(true);
        return McpToolDef.builder(t.name())
                .title(t.title()).description(t.description()).inputSchema(schema)
                .readOnly(t.readOnly()).destructive(McpToolDef.Builder.triState(t.destructive()))
                .idempotent(McpToolDef.Builder.triState(t.idempotent())).featured(t.featured()).order(t.order())
                .publicAccess(t.access() == McpAccess.PUBLIC).requiresConfirmation(t.requiresConfirmation())
                .permission(t.permission()).tags(t.tags()).source(m.getDeclaringClass().getName() + "." + m.getName())
                .executor((ctx, args) -> {
                    Object[] callArgs = new Object[params.length];
                    callArgs[0] = ctx;
                    if (finalMapMode) {
                        callArgs[1] = args;
                    } else {
                        for (int i = 1; i < params.length; i++) {
                            callArgs[i] = convertArg(args.get(names[i]), types[i], names[i]);
                        }
                    }
                    return toResult(invoke(m, callArgs));
                }).build();
    }

    private static Map<String, Object> typeOnly(String type) {
        Map<String, Object> m = new LinkedHashMap<>();
        m.put("type", type);
        return m;
    }

    static Object invoke(Method m, Object[] args) throws Exception {
        try {
            return m.invoke(null, args);
        } catch (InvocationTargetException e) {
            Throwable cause = e.getCause() != null ? e.getCause() : e;
            if (cause instanceof Exception) throw (Exception) cause;
            throw new McpToolException(cause.getClass().getSimpleName() + ": " + cause.getMessage());
        }
    }

    static McpResult toResult(Object r) {
        if (r instanceof McpResult) return (McpResult) r;
        if (r == null) return McpResult.ok(new LinkedHashMap<String, Object>());
        if (r instanceof String) return McpResult.text((String) r);
        return McpResult.ok(ResultConverter.toJson(r));
    }

    /** Converts a JSON argument to the Java parameter type. */
    @SuppressWarnings("unchecked")
    public static Object convertArg(Object v, Class<?> type, String name) throws McpToolException {
        if (v == null) {
            if (type.isPrimitive()) throw new McpToolException("argument " + name + " is required");
            return null;
        }
        try {
            if (type.isInstance(v)) return v;
            if (type == String.class) return String.valueOf(v);
            if (type == Integer.class || type == int.class) return v instanceof Number ? ((Number) v).intValue() : Integer.parseInt(v.toString().trim());
            if (type == Long.class || type == long.class) return v instanceof Number ? ((Number) v).longValue() : Long.parseLong(v.toString().trim());
            if (type == Double.class || type == double.class) return v instanceof Number ? ((Number) v).doubleValue() : Double.parseDouble(v.toString().trim());
            if (type == Boolean.class || type == boolean.class) return v instanceof Boolean ? v : Boolean.parseBoolean(v.toString().trim());
            if (type == BigDecimal.class) return new BigDecimal(v.toString().trim());
            if (type == Timestamp.class) return parseTimestamp(v.toString().trim());
            if (List.class.isAssignableFrom(type)) {
                if (v instanceof Collection) return new ArrayList<>((Collection<Object>) v);
                return new ArrayList<>(Arrays.asList(v.toString().split("\\s*,\\s*")));
            }
            if (Map.class.isAssignableFrom(type) && v instanceof Map) return v;
            throw new McpToolException("argument " + name + " has unsupported type " + type.getSimpleName());
        } catch (NumberFormatException e) {
            throw new McpToolException("argument " + name + " must be a number");
        }
    }

    public static Timestamp parseTimestamp(String s) throws McpToolException {
        if (UtilValidate.isEmpty(s)) return null;
        try {
            if (s.contains("T")) {
                return Timestamp.from(java.time.OffsetDateTime.parse(s).toInstant());
            }
        } catch (RuntimeException e) {
            try {
                return Timestamp.valueOf(java.time.LocalDateTime.parse(s));
            } catch (RuntimeException ignored) {
                // fall through
            }
        }
        try {
            if (s.length() == 10) return Timestamp.valueOf(s + " 00:00:00");
            return Timestamp.valueOf(s);
        } catch (RuntimeException e) {
            Timestamp t = UtilDateTime.toTimestamp(s);
            if (t == null) throw new McpToolException("invalid date-time " + s + " (use ISO-8601 or yyyy-MM-dd HH:mm:ss)");
            return t;
        }
    }

    // ---- tools from services ----

    public static McpToolDef toolFromService(McpServiceTool st, ModelService svc, String source) {
        String name = !st.name().isEmpty() ? st.name() : ServiceSchemaBuilder.toSnakeCase(svc.name);
        Set<String> exclude = new HashSet<>(Arrays.asList(st.exclude()));
        Map<String, Object> fixed = new LinkedHashMap<>();
        for (String f : st.fixed()) {
            int eq = f.indexOf('=');
            if (eq > 0) fixed.put(f.substring(0, eq).trim(), f.substring(eq + 1).trim());
        }
        String description = !st.description().isEmpty() ? st.description() : (svc.description != null ? svc.description : "Service " + svc.name);
        final String serviceName = svc.name;
        return McpToolDef.builder(name)
                .title(svc.name).description(description)
                .inputSchema(ServiceSchemaBuilder.inputSchema(svc, exclude, fixed.keySet()))
                .outputSchema(ServiceSchemaBuilder.outputSchema(svc))
                .readOnly(st.readOnly()).destructive(McpToolDef.Builder.triState(st.destructive()))
                .idempotent(McpToolDef.Builder.triState(st.idempotent())).featured(st.featured()).order(st.order())
                .requiresConfirmation(st.requiresConfirmation()).permission(st.permission()).tags(st.tags())
                .serviceName(serviceName).source(source)
                .executor((ctx, args) -> {
                    Map<String, Object> params = convertServiceParams(svc, args);
                    params.putAll(fixed);
                    ServiceSchemaBuilder.applyDefaults(svc, params);
                    Map<String, Object> result = ctx.runService(serviceName, params);
                    return McpResult.ok(serviceResultToJson(svc, result));
                }).build();
    }

    /** Converts JSON values to the declared service parameter types (timestamps, decimals, numbers, strings). */
    public static Map<String, Object> convertServiceParams(ModelService svc, Map<String, Object> params) throws McpToolException {
        Map<String, Object> out = new LinkedHashMap<>();
        for (Map.Entry<String, Object> e : params.entrySet()) {
            org.ofbiz.service.ModelParam p = svc.getParam(e.getKey());
            Object v = e.getValue();
            if (p == null || v == null) {
                out.put(e.getKey(), v);
                continue;
            }
            String t = p.type != null ? p.type : "String";
            String simple = t.contains(".") ? t.substring(t.lastIndexOf('.') + 1) : t;
            switch (simple) {
                case "Timestamp": out.put(e.getKey(), parseTimestamp(String.valueOf(v))); break;
                case "BigDecimal": out.put(e.getKey(), convertArg(v, BigDecimal.class, e.getKey())); break;
                case "Long": out.put(e.getKey(), convertArg(v, Long.class, e.getKey())); break;
                case "Integer": out.put(e.getKey(), convertArg(v, Integer.class, e.getKey())); break;
                case "Double": out.put(e.getKey(), convertArg(v, Double.class, e.getKey())); break;
                case "Boolean": out.put(e.getKey(), convertArg(v, Boolean.class, e.getKey())); break;
                case "String": out.put(e.getKey(), v instanceof String ? v : String.valueOf(v)); break;
                default: out.put(e.getKey(), v); break;
            }
        }
        return out;
    }

    /** Keeps OUT parameters and the success message; drops framework objects. */
    public static Map<String, Object> serviceResultToJson(ModelService svc, Map<String, Object> result) {
        Map<String, Object> out = ResultConverter.toJsonMap(svc.makeValid(result, ModelService.OUT_PARAM, false, null));
        out.remove(ModelService.RESPONSE_MESSAGE);
        Object success = result.get(ModelService.SUCCESS_MESSAGE);
        if (success instanceof String && !((String) success).isEmpty()) out.put("successMessage", success);
        return out;
    }

    // ---- resources and prompts ----

    static McpResourceDef resourceFromMethod(Method m, McpResource r) {
        m.setAccessible(true);
        int count = m.getParameterCount();
        return new McpResourceDef(r.uri(), r.name(), r.description(), r.mimeType(), (ctx, uri, params) -> {
            Object v = count >= 2 ? invoke(m, new Object[] { ctx, params }) : invoke(m, new Object[] { ctx });
            return v != null ? v.toString() : "";
        });
    }

    static McpPromptDef promptFromMethod(Method m, McpPrompt p) {
        m.setAccessible(true);
        List<McpPromptDef.Arg> args = new ArrayList<>();
        for (McpPrompt.Arg a : p.arguments()) args.add(new McpPromptDef.Arg(a.name(), a.description(), a.required()));
        int count = m.getParameterCount();
        return new McpPromptDef(p.name(), p.description(), args, (ctx, arguments) -> {
            Object v = count >= 2 ? invoke(m, new Object[] { ctx, arguments }) : invoke(m, new Object[] { ctx });
            return v != null ? v.toString() : "";
        });
    }
}
