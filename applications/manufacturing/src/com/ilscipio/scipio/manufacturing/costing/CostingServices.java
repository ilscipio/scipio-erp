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
package com.ilscipio.scipio.manufacturing.costing;

import java.math.BigDecimal;
import java.sql.Timestamp;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;

import org.ofbiz.base.util.UtilDateTime;
import org.ofbiz.base.util.UtilMisc;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.GenericEntityException;
import org.ofbiz.entity.GenericValue;
import org.ofbiz.entity.condition.EntityCondition;
import org.ofbiz.entity.condition.EntityOperator;
import org.ofbiz.entity.util.EntityQuery;
import org.ofbiz.manufacturing.bom.BOMNode;
import org.ofbiz.manufacturing.bom.BOMTree;
import org.ofbiz.service.DispatchContext;
import org.ofbiz.service.GenericServiceException;
import org.ofbiz.service.LocalDispatcher;
import org.ofbiz.service.ServiceUtil;

/**
 * Manufacturing product standard costing and BOM where-used lookup services.
 *
 * <p>SCIPIO: 4.0.0: Added.</p>
 */
public class CostingServices {

    private CostingServices() {}

    /** Gets a product's standard cost breakdown from CostComponent entries, optionally recalculating first. */
    public static Map<String, Object> getProductStandardCost(DispatchContext dctx, Map<String, ? extends Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        String productId = (String) context.get("productId");
        String currencyUomId = (String) context.get("currencyUomId");
        if (UtilValidate.isEmpty(currencyUomId)) {
            currencyUomId = "USD";
        }
        String costComponentTypePrefix = (String) context.get("costComponentTypePrefix");
        if (UtilValidate.isEmpty(costComponentTypePrefix)) {
            costComponentTypePrefix = "EST_STD";
        }
        Boolean recalculate = (Boolean) context.get("recalculate");

        try {
            if (Boolean.TRUE.equals(recalculate)) {
                Map<String, Object> calcResult = dispatcher.runSync("calculateProductCosts", UtilMisc.<String, Object>toMap(
                        "productId", productId,
                        "currencyUomId", currencyUomId,
                        "costComponentTypePrefix", costComponentTypePrefix,
                        "userLogin", userLogin));
                if (ServiceUtil.isError(calcResult)) {
                    return ServiceUtil.returnError(ServiceUtil.getErrorMessage(calcResult));
                }
            }

            Timestamp now = UtilDateTime.nowTimestamp();
            List<GenericValue> costRows = queryStandardCostRows(delegator, productId, costComponentTypePrefix, currencyUomId, now);

            List<Map<String, Object>> costComponents = new LinkedList<>();
            BigDecimal totalCost = BigDecimal.ZERO;
            BigDecimal materialCost = BigDecimal.ZERO;
            BigDecimal laborCost = BigDecimal.ZERO;
            BigDecimal overheadCost = BigDecimal.ZERO;
            BigDecimal routingCost = BigDecimal.ZERO;
            Timestamp lastCalculatedDate = null;
            for (GenericValue costRow : costRows) {
                String costComponentTypeId = costRow.getString("costComponentTypeId");
                BigDecimal cost = costRow.getBigDecimal("cost");
                if (cost == null) {
                    cost = BigDecimal.ZERO;
                }
                GenericValue costComponentType = EntityQuery.use(delegator).from("CostComponentType")
                        .where("costComponentTypeId", costComponentTypeId).cache().queryOne();

                Map<String, Object> costComponentMap = new HashMap<>();
                costComponentMap.put("costComponentTypeId", costComponentTypeId);
                costComponentMap.put("description", costComponentType != null ? costComponentType.getString("description") : null);
                costComponentMap.put("cost", cost);
                costComponents.add(costComponentMap);

                totalCost = totalCost.add(cost);
                if (costComponentTypeId.endsWith("_MAT_COST")) {
                    materialCost = materialCost.add(cost);
                } else if (costComponentTypeId.endsWith("_LABOR_COST")) {
                    laborCost = laborCost.add(cost);
                } else if (costComponentTypeId.endsWith("_GEN_COST") || costComponentTypeId.endsWith("_IND_COST")
                        || costComponentTypeId.endsWith("_OTHER_COST")) {
                    overheadCost = overheadCost.add(cost);
                } else if (costComponentTypeId.endsWith("_ROUTE_COST")) {
                    routingCost = routingCost.add(cost);
                }

                Timestamp fromDate = costRow.getTimestamp("fromDate");
                if (fromDate != null && (lastCalculatedDate == null || fromDate.after(lastCalculatedDate))) {
                    lastCalculatedDate = fromDate;
                }
            }

            List<Map<String, Object>> components = new LinkedList<>();
            List<GenericValue> bomRows = EntityQuery.use(delegator).from("ProductAssoc")
                    .where("productId", productId, "productAssocTypeId", "MANUF_COMPONENT")
                    .orderBy("sequenceNum")
                    .filterByDate()
                    .queryList();
            for (GenericValue bomRow : bomRows) {
                String componentProductId = bomRow.getString("productIdTo");
                BigDecimal quantity = bomRow.getBigDecimal("quantity");
                BigDecimal scrapFactor = bomRow.getBigDecimal("scrapFactor");
                GenericValue componentProduct = EntityQuery.use(delegator).from("Product")
                        .where("productId", componentProductId).cache().queryOne();
                List<GenericValue> componentCostRows = queryStandardCostRows(delegator, componentProductId, costComponentTypePrefix, currencyUomId, now);
                BigDecimal unitCost = null;
                if (UtilValidate.isNotEmpty(componentCostRows)) {
                    unitCost = BigDecimal.ZERO;
                    for (GenericValue componentCostRow : componentCostRows) {
                        BigDecimal cost = componentCostRow.getBigDecimal("cost");
                        if (cost != null) {
                            unitCost = unitCost.add(cost);
                        }
                    }
                }
                BigDecimal lineCost = (quantity != null && unitCost != null) ? quantity.multiply(unitCost) : null;

                Map<String, Object> componentMap = new HashMap<>();
                componentMap.put("productId", componentProductId);
                componentMap.put("internalName", componentProduct != null ? componentProduct.getString("internalName") : null);
                componentMap.put("quantity", quantity);
                componentMap.put("scrapFactor", scrapFactor);
                componentMap.put("unitCost", unitCost);
                componentMap.put("lineCost", lineCost);
                components.add(componentMap);
            }

            Map<String, Object> result = ServiceUtil.returnSuccess();
            result.put("costComponents", costComponents);
            result.put("totalCost", totalCost);
            result.put("materialCost", materialCost);
            result.put("laborCost", laborCost);
            result.put("overheadCost", overheadCost);
            result.put("routingCost", routingCost);
            result.put("components", components);
            result.put("lastCalculatedDate", lastCalculatedDate);
            return result;
        } catch (GenericEntityException e) {
            return ServiceUtil.returnError(e.getMessage());
        } catch (GenericServiceException e) {
            return ServiceUtil.returnError(e.getMessage());
        }
    }

    /** Queries active CostComponent rows for a product matching a cost type prefix and currency. */
    private static List<GenericValue> queryStandardCostRows(Delegator delegator, String productId, String costComponentTypePrefix,
            String currencyUomId, Timestamp now) throws GenericEntityException {
        return EntityQuery.use(delegator).from("CostComponent")
                .where(EntityCondition.makeCondition("productId", productId),
                        EntityCondition.makeCondition("costComponentTypeId", EntityOperator.LIKE, costComponentTypePrefix + "%"),
                        EntityCondition.makeCondition("costUomId", currencyUomId))
                .filterByDate(now)
                .queryList();
    }

    /** Gets the list of products that (directly or indirectly) use the given product as a bill-of-materials component. */
    public static Map<String, Object> getProductWhereUsed(DispatchContext dctx, Map<String, ? extends Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        String productId = (String) context.get("productId");
        String bomType = (String) context.get("bomType");
        if (UtilValidate.isEmpty(bomType)) {
            bomType = "MANUF_COMPONENT";
        }
        Timestamp inDate = (Timestamp) context.get("inDate");
        if (inDate == null) {
            inDate = UtilDateTime.nowTimestamp();
        }

        try {
            BOMTree tree = new BOMTree(productId, bomType, inDate, BOMTree.IMPLOSION, delegator, dispatcher, userLogin);
            List<BOMNode> nodeList = new LinkedList<>();
            tree.print(nodeList, 0);

            List<Map<String, Object>> whereUsed = new LinkedList<>();
            for (BOMNode node : nodeList) {
                BOMNode parentNode = node.getParentNode();
                if (parentNode == null) {
                    // Root node (the input product itself): excluded from the where-used list.
                    continue;
                }
                GenericValue product = node.getProduct();
                GenericValue parentProduct = parentNode.getProduct();

                // NOTE: BOMNode#loadParents() (used for IMPLOSION) never sets quantityMultiplier from the
                // ProductAssoc row it walks through (unlike loadChildren(), used for EXPLOSION), so
                // node.getQuantity() always resolves to 1 here regardless of depth. The real per-level BOM
                // ratio is looked up directly from the ProductAssoc row linking parent -> node instead.
                // SCIPIO: in an implosion the node is the assembly and its tree parent is the component it uses
                BigDecimal quantity = queryComponentQuantity(delegator, product.getString("productId"),
                        parentProduct.getString("productId"), bomType, inDate);

                Map<String, Object> nodeMap = new HashMap<>();
                nodeMap.put("productId", product.getString("productId"));
                nodeMap.put("internalName", product.getString("internalName"));
                nodeMap.put("productTypeId", product.getString("productTypeId"));
                nodeMap.put("depth", node.getDepth());
                nodeMap.put("quantity", quantity != null ? quantity : node.getQuantity());
                nodeMap.put("quantityMultiplier", node.getQuantityMultiplier());
                nodeMap.put("parentProductId", parentProduct.getString("productId"));
                whereUsed.add(nodeMap);
            }

            Map<String, Object> result = ServiceUtil.returnSuccess();
            result.put("whereUsed", whereUsed);
            result.put("rootProductId", productId);
            result.put("count", whereUsed.size());
            return result;
        } catch (GenericEntityException e) {
            return ServiceUtil.returnError(e.getMessage());
        }
    }

    /** Looks up the BOM quantity of a component within its direct parent's ProductAssoc row. */
    private static BigDecimal queryComponentQuantity(Delegator delegator, String parentProductId, String componentProductId,
            String bomType, Timestamp inDate) throws GenericEntityException {
        GenericValue assoc = EntityQuery.use(delegator).from("ProductAssoc")
                .where("productId", parentProductId, "productIdTo", componentProductId, "productAssocTypeId", bomType)
                .filterByDate(inDate)
                .queryFirst();
        return assoc != null ? assoc.getBigDecimal("quantity") : null;
    }

}
