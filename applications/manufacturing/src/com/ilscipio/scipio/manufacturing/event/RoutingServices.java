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
/*
 * SCIPIO: Hand-written replacement for the RoutingServices.xml simple-methods.
 */
package com.ilscipio.scipio.manufacturing.event;

import java.sql.Timestamp;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.GenericEntityException;
import org.ofbiz.entity.GenericValue;
import org.ofbiz.entity.util.EntityQuery;
import org.ofbiz.entity.util.EntityUtil;
import org.ofbiz.service.DispatchContext;
import org.ofbiz.service.ServiceUtil;

/**
 * Manufacturing product routing lookup services.
 *
 * <p>SCIPIO: Hand-written replacement for RoutingServices.xml.</p>
 */
public class RoutingServices {

    private static final String MODULE = RoutingServices.class.getName();

    /**
     * Get the product's routing and routing tasks. Falls back to the virtual product's routing when the given
     * product is a variant with no routing of its own, and finally to DEFAULT_ROUTING unless ignoreDefaultRouting
     * is set to "Y".
     */
    public static Map<String, Object> getProductRouting(DispatchContext dctx, Map<String, ? extends Object> context) {
        Delegator delegator = dctx.getDelegator();
        String productId = (String) context.get("productId");
        String workEffortId = (String) context.get("workEffortId");
        Timestamp applicableDate = (Timestamp) context.get("applicableDate");
        String ignoreDefaultRouting = (String) context.get("ignoreDefaultRouting");

        Timestamp filterDate = UtilValidate.isNotEmpty(applicableDate) ? applicableDate : new Timestamp(System.currentTimeMillis());

        Map<String, Object> lookupRouting = new HashMap<>();
        lookupRouting.put("productId", productId);
        lookupRouting.put("workEffortGoodStdTypeId", "ROU_PROD_TEMPLATE");

        try {
            GenericValue routingGS = null;
            if (UtilValidate.isNotEmpty(workEffortId)) {
                lookupRouting.put("workEffortId", workEffortId);
                List<GenericValue> routings = EntityQuery.use(delegator).from("WorkEffortGoodStandard").where(lookupRouting).queryList();
                routings = EntityUtil.filterByDate(routings, filterDate);
                routingGS = EntityUtil.getFirst(routings);
                if (UtilValidate.isEmpty(routingGS)) {
                    // SCIPIO: mirrors the original filter-by-date="true" entity-condition, which filters by "now", not filterDate
                    List<GenericValue> virtualProductAssocList = EntityQuery.use(delegator)
                            .from("ProductAssoc")
                            .where("productIdTo", productId, "productAssocTypeId", "PRODUCT_VARIANT")
                            .filterByDate()
                            .queryList();
                    GenericValue virtualProductAssoc = EntityUtil.getFirst(virtualProductAssocList);
                    if (UtilValidate.isNotEmpty(virtualProductAssoc)) {
                        lookupRouting.put("productId", virtualProductAssoc.getString("productId"));
                        routings = EntityQuery.use(delegator).from("WorkEffortGoodStandard").where(lookupRouting).queryList();
                        routings = EntityUtil.filterByDate(routings, filterDate);
                        routingGS = EntityUtil.getFirst(routings);
                    }
                }
            } else {
                List<GenericValue> routings = EntityQuery.use(delegator).from("WorkEffortGoodStandard").where(lookupRouting).queryList();
                routings = EntityUtil.filterByDate(routings, filterDate);
                routingGS = EntityUtil.getFirst(routings);
                if (UtilValidate.isEmpty(routingGS)) {
                    List<GenericValue> virtualProductAssocList = EntityQuery.use(delegator)
                            .from("ProductAssoc")
                            .where("productIdTo", productId, "productAssocTypeId", "PRODUCT_VARIANT")
                            .queryList();
                    virtualProductAssocList = EntityUtil.filterByDate(virtualProductAssocList, filterDate);
                    GenericValue virtualProductAssoc = EntityUtil.getFirst(virtualProductAssocList);
                    if (UtilValidate.isNotEmpty(virtualProductAssoc)) {
                        lookupRouting.put("productId", virtualProductAssoc.getString("productId"));
                        lookupRouting.put("workEffortGoodStdTypeId", "ROU_PROD_TEMPLATE");
                        routings = EntityQuery.use(delegator).from("WorkEffortGoodStandard").where(lookupRouting).queryList();
                        routings = EntityUtil.filterByDate(routings, filterDate);
                        routingGS = EntityUtil.getFirst(routings);
                    }
                }
            }

            GenericValue routing = null;
            if (UtilValidate.isNotEmpty(routingGS)) {
                routing = EntityQuery.use(delegator).from("WorkEffort").where("workEffortId", routingGS.getString("workEffortId")).queryOne();
            } else if (UtilValidate.isEmpty(ignoreDefaultRouting) || "N".equals(ignoreDefaultRouting)) {
                routing = EntityQuery.use(delegator).from("WorkEffort").where("workEffortId", "DEFAULT_ROUTING").queryOne();
            }

            List<GenericValue> tasks = null;
            if (UtilValidate.isNotEmpty(routing)) {
                Map<String, Object> lookupTasks = new HashMap<>();
                lookupTasks.put("workEffortIdFrom", routing.getString("workEffortId"));
                lookupTasks.put("workEffortAssocTypeId", "ROUTING_COMPONENT");
                tasks = EntityQuery.use(delegator).from("WorkEffortAssoc").where(lookupTasks).orderBy("sequenceNum").queryList();
                tasks = EntityUtil.filterByDate(tasks);
            }

            Map<String, Object> result = ServiceUtil.returnSuccess();
            result.put("routing", routing);
            result.put("tasks", tasks);
            return result;
        } catch (GenericEntityException e) {
            Debug.logError(e, "Error getting product routing: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
    }

    /** Get the (currently valid) routing task assocs of a given routing, ordered by sequenceNum. */
    public static Map<String, Object> getRoutingTaskAssocs(DispatchContext dctx, Map<String, ? extends Object> context) {
        Delegator delegator = dctx.getDelegator();
        String workEffortId = (String) context.get("workEffortId");

        Map<String, Object> lookupTasks = new HashMap<>();
        lookupTasks.put("workEffortIdFrom", workEffortId);
        lookupTasks.put("workEffortAssocTypeId", "ROUTING_COMPONENT");
        try {
            List<GenericValue> routingTaskAssocs = EntityQuery.use(delegator).from("WorkEffortAssoc").where(lookupTasks).orderBy("sequenceNum").queryList();
            routingTaskAssocs = EntityUtil.filterByDate(routingTaskAssocs);
            Map<String, Object> result = ServiceUtil.returnSuccess();
            result.put("routingTaskAssocs", routingTaskAssocs);
            return result;
        } catch (GenericEntityException e) {
            Debug.logError(e, "Error getting routing task assocs: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
    }

    private RoutingServices() {}
}
