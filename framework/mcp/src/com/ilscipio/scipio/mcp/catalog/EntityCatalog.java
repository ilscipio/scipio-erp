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
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Set;
import java.util.TreeSet;

import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.GenericEntityException;
import org.ofbiz.entity.GenericValue;
import org.ofbiz.entity.condition.EntityCondition;
import org.ofbiz.entity.condition.EntityOperator;
import org.ofbiz.entity.model.ModelEntity;
import org.ofbiz.entity.model.ModelField;
import org.ofbiz.entity.model.ModelReader;
import org.ofbiz.entity.model.ModelRelation;
import org.ofbiz.entity.model.ModelViewEntity;
import org.ofbiz.entity.util.EntityQuery;

import com.ilscipio.scipio.mcp.registry.McpCallContext;
import com.ilscipio.scipio.mcp.registry.McpToolException;
import com.ilscipio.scipio.mcp.security.McpPolicy;
import com.ilscipio.scipio.mcp.security.McpRedactor;

/**
 * SCIPIO: 4.0.0: Entity discovery and guarded read/write access for the core entity tools.
 */
public final class EntityCatalog {

    private EntityCatalog() {}

    /** Component name derived from the entity package (org.ofbiz.order.order -> order). */
    public static String componentOf(ModelEntity me) {
        String pkg = me.getPackageName() != null ? me.getPackageName() : "";
        String[] parts = pkg.split("\\.");
        if (parts.length > 3 && "com".equals(parts[0]) && "ilscipio".equals(parts[1]) && "scipio".equals(parts[2])) return parts[3];
        if (parts.length > 2 && "org".equals(parts[0]) && "ofbiz".equals(parts[1])) return parts[2];
        return parts.length > 0 ? parts[parts.length - 1] : "";
    }

    public static List<Map<String, Object>> listEntities(Delegator delegator, String component, String query, int limit) throws McpToolException {
        try {
            ModelReader reader = delegator.getModelReader();
            String q = query != null ? query.trim().toLowerCase(Locale.ROOT) : "";
            List<Map<String, Object>> out = new ArrayList<>();
            for (String name : new TreeSet<>(reader.getEntityNames())) {
                if (McpPolicy.isEntityDenied(name)) continue;
                ModelEntity me = reader.getModelEntity(name);
                String comp = componentOf(me);
                if (UtilValidate.isNotEmpty(component) && !component.equals(comp)) continue;
                if (!q.isEmpty() && !name.toLowerCase(Locale.ROOT).contains(q)
                        && (me.getDescription() == null || !me.getDescription().toLowerCase(Locale.ROOT).contains(q))) continue;
                Map<String, Object> row = new LinkedHashMap<>();
                row.put("name", name);
                row.put("component", comp);
                row.put("view", me instanceof ModelViewEntity);
                if (UtilValidate.isNotEmpty(me.getDescription())) row.put("description", me.getDescription());
                out.add(row);
                if (out.size() >= limit) break;
            }
            return out;
        } catch (GenericEntityException e) {
            throw new McpToolException("Entity model unavailable: " + e.getMessage());
        }
    }

    public static Map<String, Object> describe(Delegator delegator, String entityName) throws McpToolException {
        ModelEntity me = requireEntity(delegator, entityName);
        Map<String, Object> out = new LinkedHashMap<>();
        out.put("name", me.getEntityName());
        out.put("component", componentOf(me));
        out.put("view", me instanceof ModelViewEntity);
        if (UtilValidate.isNotEmpty(me.getDescription())) out.put("description", me.getDescription());
        out.put("primaryKeys", me.getPkFieldNames());
        List<Map<String, Object>> fields = new ArrayList<>();
        for (ModelField f : me.getFieldsUnmodifiable()) {
            Map<String, Object> fm = new LinkedHashMap<>();
            fm.put("name", f.getName());
            fm.put("type", f.getType());
            if (f.getIsPk()) fm.put("pk", true);
            if (f.getIsAutoCreatedInternal()) fm.put("internal", true);
            if (UtilValidate.isNotEmpty(f.getDescription())) fm.put("description", f.getDescription());
            fields.add(fm);
        }
        out.put("fields", fields);
        List<Map<String, Object>> rels = new ArrayList<>();
        for (ModelRelation r : me.getRelationsList(true, true, true)) {
            Map<String, Object> rm = new LinkedHashMap<>();
            rm.put("type", r.getType());
            rm.put("entity", r.getRelEntityName());
            if (UtilValidate.isNotEmpty(r.getTitle())) rm.put("title", r.getTitle());
            rels.add(rm);
        }
        out.put("relations", rels);
        return out;
    }

    private static ModelEntity requireEntity(Delegator delegator, String entityName) throws McpToolException {
        if (UtilValidate.isEmpty(entityName)) throw new McpToolException("entityName is required");
        if (McpPolicy.isEntityDenied(entityName)) throw McpToolException.denied("Entity " + entityName + " is blocked by policy");
        ModelEntity me = delegator.getModelEntity(entityName);
        if (me == null) throw new McpToolException("Unknown entity " + entityName);
        return me;
    }

    /** Checks read access: server allowlist with base _VIEW, else MCP_ENTITY_READ (hub: MCP_ENTITY_READ or MCP_ADMIN). */
    public static void checkRead(McpCallContext ctx, String entityName) throws McpToolException {
        if (ctx.isAnonymous()) throw McpToolException.denied("Authentication required");
        if (McpPolicy.isEntityDenied(entityName)) throw McpToolException.denied("Entity " + entityName + " is blocked by policy");
        if (ctx.getServer().getEntities().contains(entityName)) return;
        if (ctx.hasPermission("MCP_ENTITY_READ") || ctx.hasPermission("MCP_ADMIN") || ctx.hasPermission("ENTITY_MAINT")) return;
        throw McpToolException.denied("Entity " + entityName + " is outside this server's allowlist; MCP_ENTITY_READ required");
    }

    public static void checkWrite(McpCallContext ctx, String entityName) throws McpToolException {
        if (ctx.isAnonymous()) throw McpToolException.denied("Authentication required");
        if (McpPolicy.isEntityDenied(entityName)) throw McpToolException.denied("Entity " + entityName + " is blocked by policy");
        if (ctx.getPrincipal().isReadOnly()) throw McpToolException.denied("Read-only token cannot write entities");
        if (!ctx.hasPermission("MCP_ENTITY_WRITE")) throw McpToolException.denied("MCP_ENTITY_WRITE required");
        if (!ctx.hasPermission("ENTITY_MAINT")) throw McpToolException.denied("ENTITY_MAINT required");
    }

    @SuppressWarnings("unchecked")
    static final String SYSTEM_PROPERTY = "SystemProperty";

    /**
     * The SystemProperty resources that no MCP entity call reads, stores or removes, even when an installation takes
     * SystemProperty off {@code mcp.entity.deny}: {@code mcp.entity.protectedSystemResources} (default {@code hubcheckout}, the
     * checkout key of a hosted store; SCIPIO: 4.0.0, W1-10d review).
     */
    public static List<String> protectedSystemResources() {
        String v = org.ofbiz.base.util.UtilProperties.getPropertyValue("mcp", "mcp.entity.protectedSystemResources", "hubcheckout");
        List<String> out = new ArrayList<>();
        for (String s : v.split(",")) {
            if (!s.trim().isEmpty()) out.add(s.trim());
        }
        if (!out.contains("hubcheckout")) out.add("hubcheckout");
        return out;
    }

    /** True for a SystemProperty row of a protected resource (pure, for tests). */
    public static boolean isProtectedRow(String entityName, Object systemResourceId, java.util.Collection<String> protectedResources) {
        return SYSTEM_PROPERTY.equals(entityName) && systemResourceId != null && protectedResources.contains(String.valueOf(systemResourceId).trim());
    }

    public static List<Map<String, Object>> find(McpCallContext ctx, String entityName, List<Map<String, Object>> conditions,
                                                 List<String> fields, List<String> orderBy, int limit) throws McpToolException {
        Delegator delegator = ctx.getDelegator();
        ModelEntity me = requireEntity(delegator, entityName);
        checkRead(ctx, entityName);
        // A protected (redacted) field may not drive a condition, a selection or an ordering: that would leak its value.
        McpRedactor redactor = McpRedactor.fromConfig();
        List<EntityCondition> conds = new ArrayList<>();
        if (conditions != null) {
            for (Map<String, Object> c : conditions) {
                Object cf = c.get("field");
                if (cf instanceof String && redactor.isSensitive((String) cf)) {
                    throw McpToolException.denied("Field " + cf + " is protected and cannot be used in a condition");
                }
                conds.add(toCondition(delegator, me, c));
            }
        }
        Set<String> select = null;
        if (fields != null && !fields.isEmpty()) {
            select = new HashSet<>();
            for (String f : fields) {
                if (me.getField(f) == null) throw new McpToolException("Unknown field " + f + " on " + entityName);
                if (redactor.isSensitive(f)) throw McpToolException.denied("Field " + f + " is protected and cannot be selected");
                select.add(f);
            }
        }
        if (orderBy != null) {
            for (String o : orderBy) {
                String f = o.startsWith("-") || o.startsWith("+") ? o.substring(1) : o;
                if (me.getField(f) == null) throw new McpToolException("Unknown orderBy field " + f + " on " + entityName);
                if (redactor.isSensitive(f)) throw McpToolException.denied("Field " + f + " is protected and cannot order a query");
            }
        }
        if (SYSTEM_PROPERTY.equals(entityName)) {
            // SCIPIO: 4.0.0: the rows of a protected resource (the checkout key, W1-10d review) never leave through MCP
            conds.add(EntityCondition.makeCondition(EntityCondition.makeCondition("systemResourceId", EntityOperator.NOT_IN, protectedSystemResources()),
                    EntityOperator.OR, EntityCondition.makeCondition("systemResourceId", EntityOperator.EQUALS, null)));
        }
        try {
            EntityQuery query = EntityQuery.use(delegator).from(entityName).maxRows(limit);
            if (!conds.isEmpty()) query = query.where(EntityCondition.makeCondition(conds, EntityOperator.AND));
            if (select != null) query = query.select(select);
            if (orderBy != null && !orderBy.isEmpty()) query = query.orderBy(orderBy);
            List<GenericValue> values = query.queryList();
            List<Map<String, Object>> out = new ArrayList<>(values.size());
            for (GenericValue gv : values) {
                out.add((Map<String, Object>) ResultConverter.toJson(gv));
            }
            return out;
        } catch (GenericEntityException e) {
            throw new McpToolException("Query failed: " + McpCallContextSafe.message(e));
        }
    }

    private static EntityCondition toCondition(Delegator delegator, ModelEntity me, Map<String, Object> c) throws McpToolException {
        String field = c.get("field") instanceof String ? (String) c.get("field") : null;
        String op = c.get("op") instanceof String ? ((String) c.get("op")).toLowerCase(Locale.ROOT) : "eq";
        Object value = c.get("value");
        if (field == null || me.getField(field) == null) throw new McpToolException("Unknown condition field " + field + " on " + me.getEntityName());
        switch (op) {
            case "isnull": return EntityCondition.makeCondition(field, EntityOperator.EQUALS, null);
            case "notnull": return EntityCondition.makeCondition(field, EntityOperator.NOT_EQUAL, null);
            case "in": case "notin": {
                if (!(value instanceof Collection)) throw new McpToolException("Condition " + op + " needs an array value");
                List<Object> typed = new ArrayList<>();
                for (Object v : (Collection<?>) value) typed.add(convert(delegator, me, field, v));
                org.ofbiz.entity.condition.EntityComparisonOperator<?, ?> cmp = "in".equals(op) ? EntityOperator.IN : EntityOperator.NOT_IN;
                return EntityCondition.makeCondition(field, cmp, typed);
            }
            case "like": return EntityCondition.makeCondition(field, EntityOperator.LIKE, String.valueOf(value));
            case "eq": return EntityCondition.makeCondition(field, EntityOperator.EQUALS, convert(delegator, me, field, value));
            case "ne": return EntityCondition.makeCondition(field, EntityOperator.NOT_EQUAL, convert(delegator, me, field, value));
            case "lt": return EntityCondition.makeCondition(field, EntityOperator.LESS_THAN, convert(delegator, me, field, value));
            case "le": return EntityCondition.makeCondition(field, EntityOperator.LESS_THAN_EQUAL_TO, convert(delegator, me, field, value));
            case "gt": return EntityCondition.makeCondition(field, EntityOperator.GREATER_THAN, convert(delegator, me, field, value));
            case "ge": return EntityCondition.makeCondition(field, EntityOperator.GREATER_THAN_EQUAL_TO, convert(delegator, me, field, value));
            default: throw new McpToolException("Unsupported condition op " + op + " (use eq, ne, lt, le, gt, ge, like, in, notIn, isNull, notNull)");
        }
    }

    /** Converts a JSON value to the field's Java type using the entity's own conversion. */
    public static Object convert(Delegator delegator, ModelEntity me, String field, Object value) throws McpToolException {
        if (value == null) return null;
        try {
            GenericValue tmp = delegator.makeValue(me.getEntityName());
            if (value instanceof String) {
                tmp.setString(field, (String) value);
            } else {
                tmp.setString(field, String.valueOf(value));
            }
            return tmp.get(field);
        } catch (RuntimeException e) {
            throw new McpToolException("Value " + value + " is not valid for field " + field + ": " + e.getMessage());
        }
    }

    public static Map<String, Object> store(McpCallContext ctx, String entityName, Map<String, Object> fields, boolean create) throws McpToolException {
        Delegator delegator = ctx.getDelegator();
        ModelEntity me = requireEntity(delegator, entityName);
        if (me instanceof ModelViewEntity) throw new McpToolException("Cannot write to view entity " + entityName);
        checkWrite(ctx, entityName);
        if (isProtectedRow(entityName, fields.get("systemResourceId"), protectedSystemResources())) {
            throw McpToolException.denied("SystemProperty " + fields.get("systemResourceId") + " is protected");
        }
        try {
            GenericValue gv = delegator.makeValue(entityName);
            for (Map.Entry<String, Object> e : fields.entrySet()) {
                if (me.getField(e.getKey()) == null) throw new McpToolException("Unknown field " + e.getKey() + " on " + entityName);
                gv.set(e.getKey(), convert(delegator, me, e.getKey(), e.getValue()));
            }
            if (create) {
                gv = delegator.create(gv);
            } else {
                GenericValue existing = delegator.findOne(entityName, gv.getPrimaryKey(), false);
                if (existing == null) throw new McpToolException("Record not found for update: " + gv.getPrimaryKey());
                existing.setNonPKFields(gv, true);
                existing.store();
                gv = existing;
            }
            return ResultConverter.toJsonMap(gv);
        } catch (GenericEntityException e) {
            throw new McpToolException("Store failed: " + McpCallContextSafe.message(e));
        }
    }

    public static Map<String, Object> remove(McpCallContext ctx, String entityName, Map<String, Object> pk) throws McpToolException {
        Delegator delegator = ctx.getDelegator();
        ModelEntity me = requireEntity(delegator, entityName);
        if (me instanceof ModelViewEntity) throw new McpToolException("Cannot remove from view entity " + entityName);
        checkWrite(ctx, entityName);
        if (isProtectedRow(entityName, pk.get("systemResourceId"), protectedSystemResources())) {
            throw McpToolException.denied("SystemProperty " + pk.get("systemResourceId") + " is protected");
        }
        try {
            GenericValue gv = delegator.makeValue(entityName);
            for (String pkField : me.getPkFieldNames()) {
                if (pk.get(pkField) == null) throw new McpToolException("Primary key field " + pkField + " is required");
                gv.set(pkField, convert(delegator, me, pkField, pk.get(pkField)));
            }
            int removed = delegator.removeByPrimaryKey(gv.getPrimaryKey());
            Map<String, Object> out = new LinkedHashMap<>();
            out.put("removed", removed);
            return out;
        } catch (GenericEntityException e) {
            throw new McpToolException("Remove failed: " + McpCallContextSafe.message(e));
        }
    }

    /** Small helper that keeps exception text short and free of stack details. */
    static final class McpCallContextSafe {
        static String message(Throwable t) {
            String m = t.getMessage();
            if (m == null) return t.getClass().getSimpleName();
            int nl = m.indexOf('\n');
            if (nl > 0) m = m.substring(0, nl);
            return m.length() > 400 ? m.substring(0, 400) : m;
        }
    }
}
