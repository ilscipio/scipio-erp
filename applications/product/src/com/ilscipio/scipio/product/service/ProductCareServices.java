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
package com.ilscipio.scipio.product.service;

import java.sql.Timestamp;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TreeSet;

import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.GenericEntityException;
import org.ofbiz.entity.GenericValue;
import org.ofbiz.entity.condition.EntityCondition;
import org.ofbiz.entity.model.ModelEntity;
import org.ofbiz.entity.model.ModelKeyMap;
import org.ofbiz.entity.model.ModelReader;
import org.ofbiz.entity.model.ModelRelation;
import org.ofbiz.entity.model.ModelViewEntity;
import org.ofbiz.service.DispatchContext;
import org.ofbiz.service.ServiceUtil;

import com.ilscipio.scipio.service.def.Attribute;
import com.ilscipio.scipio.service.def.Service;

/**
 * Product care (R4-7): the services behind the MCP actions catalog/product archive and delete.
 * <ul>
 * <li>archiveProduct: the sales end date of the product is now (Product.salesDiscontinuationDate). The product and its
 * orders stay.</li>
 * <li>deleteProductIfUnordered: removes a product, and its variants, only when no order line names one of them. Else
 * the service refuses with a clear message (archive the product instead).</li>
 * </ul>
 *
 * <p>SCIPIO: 4.0.0: Added (R4-7).</p>
 */
public class ProductCareServices {

    private static final String MODULE_NAME = ProductCareServices.class.getName();

    /** The entities of orders, returns, invoices and shipments: a row here that names the product forbids the delete. */
    static boolean isOrderRelated(String entityName) {
        for (String prefix : new String[] {"Order", "Return", "Invoice", "Shipment", "ItemIssuance", "Quote", "Requirement", "Picklist"}) {
            if (entityName.startsWith(prefix)) {
                return true;
            }
        }
        return false;
    }

    @Service(
        name = "archiveProduct",
        engine = "java",
        location = "com.ilscipio.scipio.product.service.ProductCareServices",
        invoke = "archiveProduct",
        description = "Ends the sales of a product: sets the sales end date (salesDiscontinuationDate) to now. "
                + "Needs the permission CATALOG_UPDATE.",
        auth = "true",
        attributes = {
            @Attribute(name = "productId", type = "String", mode = "IN", optional = "false"),
            @Attribute(name = "salesDiscontinuationDate", type = "Timestamp", mode = "OUT", optional = "true")
        }
    )
    public interface ArchiveProduct {}

    @Service(
        name = "deleteProductIfUnordered",
        engine = "java",
        location = "com.ilscipio.scipio.product.service.ProductCareServices",
        invoke = "deleteProductIfUnordered",
        description = "Deletes a product and its variants when no order line names them; else it refuses. "
                + "Needs the permission CATALOG_DELETE.",
        auth = "true",
        attributes = {
            @Attribute(name = "productId", type = "String", mode = "IN", optional = "false"),
            @Attribute(name = "deletedProductIds", type = "List", mode = "OUT", optional = "true")
        }
    )
    public interface DeleteProductIfUnordered {}

    public static Map<String, Object> archiveProduct(DispatchContext dctx, Map<String, ? extends Object> context) {
        GenericValue userLogin = (GenericValue) context.get("userLogin");
        if (!dctx.getSecurity().hasEntityPermission("CATALOG", "_UPDATE", userLogin)) {
            return ServiceUtil.returnError("Permission CATALOG_UPDATE is required.");
        }
        String productId = (String) context.get("productId");
        Delegator delegator = dctx.getDelegator();
        try {
            GenericValue product = delegator.findOne("Product", false, "productId", productId);
            if (product == null) {
                return ServiceUtil.returnError("Product not found: " + productId);
            }
            Timestamp now = new Timestamp(System.currentTimeMillis());
            product.set("salesDiscontinuationDate", now);
            product.store();
            Map<String, Object> out = ServiceUtil.returnSuccess();
            out.put("salesDiscontinuationDate", now);
            return out;
        } catch (GenericEntityException e) {
            return ServiceUtil.returnError("Archive failed: " + e.getMessage());
        }
    }

    public static Map<String, Object> deleteProductIfUnordered(DispatchContext dctx, Map<String, ? extends Object> context) {
        GenericValue userLogin = (GenericValue) context.get("userLogin");
        if (!dctx.getSecurity().hasEntityPermission("CATALOG", "_DELETE", userLogin)) {
            return ServiceUtil.returnError("Permission CATALOG_DELETE is required.");
        }
        String productId = (String) context.get("productId");
        Delegator delegator = dctx.getDelegator();
        try {
            if (delegator.findOne("Product", false, "productId", productId) == null) {
                return ServiceUtil.returnError("Product not found: " + productId);
            }
            List<String> ids = new ArrayList<>();
            ids.add(productId);
            for (GenericValue assoc : delegator.findByAnd("ProductAssoc",
                    org.ofbiz.base.util.UtilMisc.toMap("productId", productId, "productAssocTypeId", "PRODUCT_VARIANT"), null, false)) {
                String variantId = assoc.getString("productIdTo");
                if (variantId != null && !ids.contains(variantId)) {
                    ids.add(variantId);
                }
            }
            for (String id : ids) {
                long lines = delegator.findCountByCondition("OrderItem", EntityCondition.makeCondition("productId", id), null, null);
                if (lines > 0) {
                    return ServiceUtil.returnError("The product " + id + " is on " + lines + " order line" + (lines == 1 ? "" : "s")
                            + ". A product with an order cannot be deleted. Archive it instead.");
                }
            }
            // plan first (every dependent row, found by the entity relations), so that nothing is removed when an order row exists
            List<GenericValue> plan = new ArrayList<>();
            Set<String> seen = new HashSet<>();
            List<String> blockers = new ArrayList<>();
            Map<String, List<Child>> index = childIndex(delegator);
            for (String id : ids) {
                GenericValue product = delegator.findOne("Product", false, "productId", id);
                collect(delegator, index, product, plan, seen, blockers, 0);
            }
            if (!blockers.isEmpty()) {
                return ServiceUtil.returnError("The product has order rows (" + blockers.get(0) + "). A product with an order cannot be deleted. Archive it instead.");
            }
            // the plan lists the dependents before the rows they depend on; the service runs in one transaction
            for (GenericValue row : plan) {
                delegator.removeValue(row);
            }
            List<String> deleted = new ArrayList<>();
            for (int i = ids.size() - 1; i >= 0; i--) {
                deleted.add(ids.get(i));
            }
            Map<String, Object> out = ServiceUtil.returnSuccess();
            out.put("deletedProductIds", deleted);
            return out;
        } catch (GenericEntityException e) {
            return ServiceUtil.returnError("Delete failed: " + e.getMessage());
        }
    }

    /** One foreign key of an entity to a parent entity: the child field of each parent field. */
    private static final class Child {
        final String entity;
        final Map<String, String> childFieldOfParentField = new LinkedHashMap<>();

        Child(String entity) {
            this.entity = entity;
        }
    }

    /** The entities with a foreign key (a relation of type one) to each entity, from the entity model: parent entity to children. */
    private static Map<String, List<Child>> childIndex(Delegator delegator) throws GenericEntityException {
        Map<String, List<Child>> out = new HashMap<>();
        ModelReader reader = delegator.getModelReader();
        // a store database has the tables of its component profile only: an entity of another component has a model but no table
        for (String name : new TreeSet<>(reader.getEntityNames())) {
            ModelEntity model = reader.getModelEntityNoCheck(name);
            if (model == null || model instanceof ModelViewEntity) {
                continue;
            }
            if (!hasTable(delegator, model)) {
                continue;
            }
            for (ModelRelation rel : model.getRelationsList(true, false, false)) {
                if ("one".equals(rel.getType()) && rel.getRelEntityName() != null) {
                    Child child = new Child(name);
                    for (ModelKeyMap km : rel.getKeyMaps()) {
                        child.childFieldOfParentField.put(km.getRelFieldName(), km.getFieldName());
                    }
                    out.computeIfAbsent(rel.getRelEntityName(), k -> new ArrayList<>()).add(child);
                }
            }
        }
        return out;
    }

    /** The table names of each database helper (the set changes only with a restart or the desk schema check). Only a successful read is kept. */
    private static final Map<String, Set<String>> TABLES_OF_HELPER = new java.util.concurrent.ConcurrentHashMap<>();

    /** True when the database of the entity has the table of the entity (a table list that cannot be read counts as yes, with a warning). */
    private static boolean hasTable(Delegator delegator, ModelEntity model) {
        org.ofbiz.entity.datasource.GenericHelperInfo info = delegator.getGroupHelperInfo(delegator.getEntityGroupName(model.getEntityName()));
        if (info == null) {
            return true;
        }
        String helper = info.getHelperBaseName() + "/" + info.getHelperFullName();
        Set<String> tables = TABLES_OF_HELPER.get(helper);
        if (tables == null) {
            Set<String> out = new HashSet<>();
            try {
                java.util.TreeSet<String> names = new org.ofbiz.entity.jdbc.DatabaseUtil(info).getTableNames(null);
                if (names != null) {
                    for (String n : names) {
                        out.add(n.substring(n.lastIndexOf('.') + 1).toLowerCase(java.util.Locale.ROOT));
                    }
                }
            } catch (RuntimeException e) {
                org.ofbiz.base.util.Debug.logWarning("Product delete: the table list of the database " + helper + " cannot be read ("
                        + e.getMessage() + "); every table counts as present.", MODULE_NAME);
                return true;
            }
            if (out.isEmpty()) {
                org.ofbiz.base.util.Debug.logWarning("Product delete: the table list of the database " + helper
                        + " is empty; every table counts as present.", MODULE_NAME);
                return true;
            }
            TABLES_OF_HELPER.put(helper, out);
            tables = out;
        }
        return tables.contains(model.getPlainTableName().toLowerCase(java.util.Locale.ROOT));
    }

    /**
     * Adds every row that depends on the parent row to the plan, the deepest dependents first. A row of an order entity goes
     * to the blockers. Product itself is never a dependent (the variants are in the ID list of the caller).
     */
    private static void collect(Delegator delegator, Map<String, List<Child>> index, GenericValue parent, List<GenericValue> plan, Set<String> seen, List<String> blockers, int depth)
            throws GenericEntityException {
        if (parent == null || depth > 8) {
            return;
        }
        for (Child child : index.getOrDefault(parent.getEntityName(), java.util.Collections.<Child>emptyList())) {
            if ("Product".equals(child.entity) || child.childFieldOfParentField.isEmpty()) {
                continue;
            }
            Map<String, Object> where = new LinkedHashMap<>();
            for (Map.Entry<String, String> e : child.childFieldOfParentField.entrySet()) {
                where.put(e.getValue(), parent.get(e.getKey()));
            }
            List<GenericValue> rows = delegator.findByAnd(child.entity, where, null, false);
            if (rows.isEmpty()) {
                continue;
            }
            if (isOrderRelated(child.entity)) {
                blockers.add(child.entity);
                continue;
            }
            for (GenericValue row : rows) {
                if (seen.add(child.entity + row.getPrimaryKey())) {
                    collect(delegator, index, row, plan, seen, blockers, depth + 1);
                    plan.add(row);
                }
            }
        }
        if (depth == 0) {
            plan.add(parent);
        }
    }
}
