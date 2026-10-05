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
package com.ilscipio.scipio.product.event;

import java.math.BigDecimal;
import java.math.RoundingMode;
import java.sql.Timestamp;
import java.util.Calendar;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.GroovyUtil;
import org.ofbiz.base.util.UtilDateTime;
import org.ofbiz.base.util.UtilGenerics;
import org.ofbiz.base.util.UtilMisc;
import org.ofbiz.base.util.UtilProperties;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.GenericValue;
import org.ofbiz.entity.condition.EntityCondition;
import org.ofbiz.entity.util.EntityQuery;
import org.ofbiz.entity.util.EntityUtil;
import org.ofbiz.service.DispatchContext;
import org.ofbiz.service.LocalDispatcher;
import org.ofbiz.service.ServiceUtil;

/**
 * Auto-generated event class from Simple Method XML.
 *
 * <p>Generated from: component://product/script/org/ofbiz/product/cost/CostServices.xml</p>
 *
 * <p>SCIPIO: 4.0.0: Auto-generated.</p>
 */
public class CostServices {

    private static final String MODULE = CostServices.class.getName();


    /**
     * Cancels CostComponents
     */
    public static Map<String, Object> cancelCostComponents(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        Map<String, Object> costsAndMap = new HashMap<String, Object>();
        costsAndMap.put("costComponentId", context.get("costComponentId"));
        costsAndMap.put("productId", context.get("productId"));
        costsAndMap.put("costUomId", context.get("costUomId"));
        costsAndMap.put("costComponentTypeId", context.get("costComponentTypeId"));
        List<GenericValue> existingCosts = null;
        try {
            existingCosts = EntityQuery.use(delegator)
                    .from("CostComponent")
                    .where(costsAndMap)
                    .queryList();
        } catch (Exception e) {
            Debug.logError(e, "Error querying CostComponent: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        List<GenericValue> emptyField = EntityUtil.filterByDate(UtilGenerics.cast(existingCosts));
        if (existingCosts != null) {
            for (GenericValue existingCost : existingCosts) {
                Timestamp existingCost_thruDate = new Timestamp(System.currentTimeMillis());
                try {
                    delegator.store(existingCost);
                } catch (Exception e) {
                    Debug.logError(e, "Error storing value: " + e.getMessage(), MODULE);
                    return ServiceUtil.returnError(e.getMessage());
                }
            }
        }

        return result;
    }


    /**
     * Create a CostComponent and cancel the existing ones
     */
    public static Map<String, Object> recreateCostComponent(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        Map<String, Object> costsAndMap = new HashMap<String, Object>();
        costsAndMap.put("productId", context.get("productId"));
        costsAndMap.put("costUomId", context.get("costUomId"));
        costsAndMap.put("costComponentTypeId", context.get("costComponentTypeId"));
        List<GenericValue> existingCosts = null;
        try {
            existingCosts = EntityQuery.use(delegator)
                    .from("CostComponent")
                    .where(costsAndMap)
                    .queryList();
        } catch (Exception e) {
            Debug.logError(e, "Error querying CostComponent: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        List<GenericValue> emptyField = EntityUtil.filterByDate(UtilGenerics.cast(existingCosts));
        if (existingCosts != null) {
            for (GenericValue existingCost : existingCosts) {
                Timestamp existingCost_thruDate = new Timestamp(System.currentTimeMillis());
                try {
                    delegator.store(existingCost);
                } catch (Exception e) {
                    Debug.logError(e, "Error storing value: " + e.getMessage(), MODULE);
                    return ServiceUtil.returnError(e.getMessage());
                }
            }
        }
        GenericValue newEntity = delegator.makeValue("CostComponent");
        newEntity.setNonPKFields(context);
        ((GenericValue) newEntity).put("costComponentId", delegator.getNextSeqId("CostComponent"));
        if (UtilValidate.isEmpty(newEntity.get("fromDate"))) {
            Timestamp newEntity_fromDate = new Timestamp(System.currentTimeMillis());
        }
        try {
            delegator.create(newEntity);
        } catch (Exception e) {
            Debug.logError(e, "Error creating value: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        result.put("costComponentId", newEntity.get("costComponentId"));

        return result;
    }


    /**
     * Gets the product's costs (from CostComponent or ProductPrice)
     */
    public static Map<String, Object> getProductCost(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        Object productCost = null;
        GenericValue product = null;
        List<GenericValue> emptyField = null;
        List<GenericValue> virtualAssocs = null;
        GenericValue virtualAssoc = null;
        Map<String, Object> assocAndMap = null;
        Map<String, Object> inputMap = null;
        List<Object> orderByList = null;
        List<GenericValue> priceCosts = null;
        GenericValue priceCost = null;
        Object costsAndMap = null;
        List<GenericValue> costComponents = null;
        try {
            costComponents = EntityQuery.use(delegator)
                    .from("CostComponent")
                    .filterByDate()
                    .queryList();
        } catch (Exception e) {
            Debug.logError(e, "Error querying CostComponent: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        productCost = BigDecimal.ZERO;
        if (costComponents != null) {
            for (GenericValue costComponent : costComponents) {
                productCost = new BigDecimal(productCost.toString());
            }
        }
        if (java.util.Objects.equals(productCost, BigDecimal.ZERO)) {
            try {
                product = EntityQuery.use(delegator)
                        .from("Product")
                        .where(context)
                        .queryOne();
            } catch (Exception e) {
                Debug.logError(e, "Error querying Product: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
            assocAndMap.put("productIdTo", product.get("productId"));
            assocAndMap.put("productAssocTypeId", "PRODUCT_VARIANT");
            try {
                virtualAssocs = EntityQuery.use(delegator)
                        .from("ProductAssoc")
                        .where(assocAndMap)
                        .queryList();
            } catch (Exception e) {
                Debug.logError(e, "Error querying ProductAssoc: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
            emptyField = EntityUtil.filterByDate(UtilGenerics.cast(virtualAssocs));
            virtualAssoc = EntityUtil.getFirst((List<GenericValue>) virtualAssocs);
            if (UtilValidate.isNotEmpty(virtualAssoc)) {
                inputMap.put("productId", virtualAssoc.get("productId"));
                inputMap.put("currencyUomId", context.get("currencyUomId"));
                inputMap.put("costComponentTypePrefix", context.get("costComponentTypePrefix"));
                try {
                    Map<String, Object> serviceResult = dispatcher.runSync("getProductCost", inputMap);
                    if (ServiceUtil.isError(serviceResult)) {
                        return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
                    }
                    productCost = serviceResult.get("productCost");
                } catch (Exception e) {
                    Debug.logError(e, "Error calling getProductCost: " + e.getMessage(), MODULE);
                    return ServiceUtil.returnError(e.getMessage());
                }
            }
        }
        if (java.util.Objects.equals(productCost, BigDecimal.ZERO)) {
            orderByList.add("+supplierPrefOrderId");
            orderByList.add("+lastPrice");
            costsAndMap = new HashMap<String, Object>();
            ((Map<String, Object>) costsAndMap).put("productId", context.get("productId"));
            ((Map<String, Object>) costsAndMap).put("currencyUomId", context.get("currencyUomId"));
            try {
                priceCosts = EntityQuery.use(delegator)
                        .from("SupplierProduct")
                        .where(costsAndMap)
                        .queryList();
            } catch (Exception e) {
                Debug.logError(e, "Error querying SupplierProduct: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
            emptyField = EntityUtil.filterByDate(UtilGenerics.cast(priceCosts));
            priceCost = EntityUtil.getFirst((List<GenericValue>) priceCosts);
            if (UtilValidate.isNotEmpty(priceCost.get("lastPrice"))) {
                productCost = priceCost.get("lastPrice");
            }
            if (java.util.Objects.equals(productCost, BigDecimal.ZERO)) {
                costsAndMap = new HashMap<String, Object>();
                ((Map<String, Object>) costsAndMap).put("productId", context.get("productId"));
                try {
                    priceCosts = EntityQuery.use(delegator)
                            .from("SupplierProduct")
                            .where(costsAndMap)
                            .queryList();
                } catch (Exception e) {
                    Debug.logError(e, "Error querying SupplierProduct: " + e.getMessage(), MODULE);
                    return ServiceUtil.returnError(e.getMessage());
                }
                emptyField = EntityUtil.filterByDate(UtilGenerics.cast(priceCosts));
                priceCost = EntityUtil.getFirst((List<GenericValue>) priceCosts);
                if (UtilValidate.isNotEmpty(priceCost.get("lastPrice"))) {
                    inputMap = new HashMap<String, Object>();
                    inputMap.put("originalValue", priceCost.get("lastPrice"));
                    inputMap.put("uomId", priceCost.get("currencyUomId"));
                    inputMap.put("uomIdTo", context.get("currencyUomId"));
                    try {
                        Map<String, Object> serviceResult = dispatcher.runSync("convertUom", inputMap);
                        if (ServiceUtil.isError(serviceResult)) {
                            return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
                        }
                        productCost = serviceResult.get("convertedValue");
                    } catch (Exception e) {
                        Debug.logError(e, "Error calling convertUom: " + e.getMessage(), MODULE);
                        return ServiceUtil.returnError(e.getMessage());
                    }
                    if (UtilValidate.isEmpty(productCost)) {
                        Debug.logWarning("Currency conversion failed for ProductCost lookup; unable to convert from " + priceCost.get("currencyUomId") + " to " + context.get("currencyUomId"), MODULE);
                        productCost = BigDecimal.ZERO;
                    }
                }
            }
        }
        result.put("productCost", productCost);

        return result;
    }


    /**
     * Gets the production run task's costs
     */
    public static Map<String, Object> getTaskCost(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        GenericValue setupCost = null;
        GenericValue usageCost = null;
        List<GenericValue> emptyField = null;
        List<GenericValue> usageCosts = null;
        Map<String, Object> costsAndMap = null;
        GenericValue fixedAsset = null;
        List<GenericValue> setupCosts = null;
        Object totalCostComponentCost = null;
        GenericValue costComponentCalc = null;
        Map<String, Object> costsByType = null;
        Object totalCostComponentTime = null;
        GenericValue customMethod = null;
        Map<String, Object> inputMap = new HashMap<String, Object>();
        // set-service-fields from "parameters" to "inputMap" for service "getEstimatedTaskTime"
        inputMap.putAll(UtilMisc.toMap(context));
        inputMap.put("taskId", context.get("workEffortId"));
        Object totalEstimatedTaskTime = null;
        Object setupTime = null;
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("getEstimatedTaskTime", inputMap);
            if (ServiceUtil.isError(serviceResult)) {
                return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
            }
            totalEstimatedTaskTime = serviceResult.get("estimatedTaskTime");
            setupTime = serviceResult.get("setupTime");
        } catch (Exception e) {
            Debug.logError(e, "Error calling getEstimatedTaskTime: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        Object estimatedTaskTime = new BigDecimal(setupTime.toString());
        GenericValue task = null;
        try {
            task = EntityQuery.use(delegator)
                    .from("WorkEffort")
                    .where(context)
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying WorkEffort: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        if (UtilValidate.isNotEmpty(task)) {
            try {
                fixedAsset = task.getRelatedOne("FixedAsset", false);
            } catch (Exception e) {
                Debug.logError(e, "Error getting related one FixedAsset: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
            costsAndMap.put("amountUomId", context.get("currencyUomId"));
            costsAndMap.put("fixedAssetStdCostTypeId", "SETUP_COST");
            try {
                setupCosts = fixedAsset.getRelated("FixedAssetStdCost", null, null, false);
            } catch (Exception e) {
                Debug.logError(e, "Error getting related FixedAssetStdCost: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
            emptyField = EntityUtil.filterByDate(UtilGenerics.cast(setupCosts));
            setupCost = EntityUtil.getFirst((List<GenericValue>) setupCosts);
            costsAndMap.put("fixedAssetStdCostTypeId", "USAGE_COST");
            try {
                usageCosts = fixedAsset.getRelated("FixedAssetStdCost", null, null, false);
            } catch (Exception e) {
                Debug.logError(e, "Error getting related FixedAssetStdCost: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
            emptyField = EntityUtil.filterByDate(UtilGenerics.cast(usageCosts));
            usageCost = EntityUtil.getFirst((List<GenericValue>) usageCosts);
        }
        Object taskCost = (new BigDecimal(usageCost.get("amount").toString())).add(new BigDecimal(setupCost.get("amount").toString()));
        taskCost = new BigDecimal(taskCost.toString());
        List<GenericValue> weccs = null;
        try {
            weccs = task.getRelated("WorkEffortCostCalc", null, null, false);
        } catch (Exception e) {
            Debug.logError(e, "Error getting related WorkEffortCostCalc: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        emptyField = EntityUtil.filterByDate(UtilGenerics.cast(weccs));
        if (weccs != null) {
            for (GenericValue wecc : weccs) {
                totalCostComponentCost = null;
                totalCostComponentTime = null;
                try {
                    costComponentCalc = wecc.getRelatedOne("CostComponentCalc", false);
                } catch (Exception e) {
                    Debug.logError(e, "Error getting related one CostComponentCalc: " + e.getMessage(), MODULE);
                    return ServiceUtil.returnError(e.getMessage());
                }
                try {
                    customMethod = costComponentCalc.getRelatedOne("CustomMethod", false);
                } catch (Exception e) {
                    Debug.logError(e, "Error getting related one CustomMethod: " + e.getMessage(), MODULE);
                    return ServiceUtil.returnError(e.getMessage());
                }
                if (UtilValidate.isEmpty(customMethod)) {
                    if (UtilValidate.isNotEmpty(costComponentCalc.get("perMilliSecond"))) {
                        if (!java.util.Objects.equals(costComponentCalc.get("perMilliSecond"), BigDecimal.ZERO)) {
                            totalCostComponentTime = new BigDecimal(costComponentCalc.get("perMilliSecond").toString());
                            totalCostComponentCost = new BigDecimal(costComponentCalc.get("variableCost").toString());
                            totalCostComponentCost = new BigDecimal(costComponentCalc.get("fixedCost").toString());
                            costsByType.put((String) wecc.get("costComponentTypeId"), totalCostComponentCost);
                        }
                    }
                } else {
                }
            }
        }
        result.put("taskCost", taskCost);
        result.put("costsByType", costsByType);

        return result;
    }


    /**
     * Calculates estimated costs for all the products
     */
    public static Map<String, Object> calculateAllProductsCosts(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        Map<String, Object> inMap = null;
        List<GenericValue> products = null;
        try {
            products = EntityQuery.use(delegator)
                    .from("Product")
                    .queryList();
        } catch (Exception e) {
            Debug.logError(e, "Error querying Product: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        inMap.put("currencyUomId", context.get("currencyUomId"));
        inMap.put("costComponentTypePrefix", context.get("costComponentTypePrefix"));
        if (products != null) {
            for (GenericValue product : products) {
                inMap.put("productId", product.get("productId"));
                try {
                    Map<String, Object> serviceResult = dispatcher.runSync("calculateProductCosts", inMap);
                    if (ServiceUtil.isError(serviceResult)) {
                        return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
                    }
                } catch (Exception e) {
                    Debug.logError(e, "Error calling calculateProductCosts: " + e.getMessage(), MODULE);
                    return ServiceUtil.returnError(e.getMessage());
                }
            }
        }

        return result;
    }


    /**
     * Calculates the product's cost
     */
    public static Map<String, Object> calculateProductCosts(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        Object product = null;
        Object productCost = null;
        Object totalProductsCost = null;
        Object inputMap = null;
        Object taskCost = null;
        Object totalOtherTaskCost = null;
        Object callSvcMap = null;
        Object costsByType = null;
        Map<String, Object> totalCostsByType = null;
        Object totalTaskCost = null;
        Object customMethodParameters = null;
        GenericValue costComponentCalc = null;
        Object productCostAdjustment = null;
        Object totalCost = null;
        GenericValue customMethod = null;
        Map<String, Object> cancelMap = new HashMap<String, Object>();
        cancelMap.put("costComponentTypeId", "" + context.get("costComponentTypePrefix") + "_ROUTE_COST");
        cancelMap.put("productId", context.get("productId"));
        cancelMap.put("costUomId", context.get("currencyUomId"));
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("cancelCostComponents", cancelMap);
            if (ServiceUtil.isError(serviceResult)) {
                return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
            }
        } catch (Exception e) {
            Debug.logError(e, "Error calling cancelCostComponents: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        cancelMap.put("costComponentTypeId", "" + context.get("costComponentTypePrefix") + "_MAT_COST");
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("cancelCostComponents", cancelMap);
            if (ServiceUtil.isError(serviceResult)) {
                return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
            }
        } catch (Exception e) {
            Debug.logError(e, "Error calling cancelCostComponents: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        ((Map<String, Object>) callSvcMap).put("productId", context.get("productId"));
        Object componentsMap = null;
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("getManufacturingComponents", (Map<String, Object>) callSvcMap);
            if (ServiceUtil.isError(serviceResult)) {
                return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
            }
            componentsMap = serviceResult.get("componentsMap");
        } catch (Exception e) {
            Debug.logError(e, "Error calling getManufacturingComponents: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        GenericValue componentMap = null;
        if (UtilValidate.isNotEmpty(componentsMap)) {
            if (componentsMap != null) {
                for (Object componentMapEntry : (List<?>) componentsMap) {
                    inputMap = null;
                    product = ((Map<String, Object>) componentMapEntry).get("product");
                    ((Map<String, Object>) inputMap).put("productId", ((Map<String, Object>) product).get("productId"));
                    ((Map<String, Object>) inputMap).put("currencyUomId", context.get("currencyUomId"));
                    ((Map<String, Object>) inputMap).put("costComponentTypePrefix", context.get("costComponentTypePrefix"));
                    try {
                        Map<String, Object> serviceResult = dispatcher.runSync("getProductCost", (Map<String, Object>) inputMap);
                        if (ServiceUtil.isError(serviceResult)) {
                            return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
                        }
                        productCost = serviceResult.get("productCost");
                    } catch (Exception e) {
                        Debug.logError(e, "Error calling getProductCost: " + e.getMessage(), MODULE);
                        return ServiceUtil.returnError(e.getMessage());
                    }
                    totalProductsCost = new BigDecimal(productCost.toString());
                }
            }
        } else {
            inputMap = new HashMap<String, Object>();
            ((Map<String, Object>) inputMap).put("productId", context.get("productId"));
            ((Map<String, Object>) inputMap).put("currencyUomId", context.get("currencyUomId"));
            ((Map<String, Object>) inputMap).put("costComponentTypePrefix", context.get("costComponentTypePrefix"));
            try {
                Map<String, Object> serviceResult = dispatcher.runSync("getProductCost", (Map<String, Object>) inputMap);
                if (ServiceUtil.isError(serviceResult)) {
                    return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
                }
                productCost = serviceResult.get("productCost");
            } catch (Exception e) {
                Debug.logError(e, "Error calling getProductCost: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
            totalProductsCost = new BigDecimal(productCost.toString());
        }
        ((Map<String, Object>) callSvcMap).put("ignoreDefaultRouting", "Y");
        Object tasks = null;
        Object routing = null;
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("getProductRouting", (Map<String, Object>) callSvcMap);
            if (ServiceUtil.isError(serviceResult)) {
                return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
            }
            tasks = serviceResult.get("tasks");
            routing = serviceResult.get("routing");
        } catch (Exception e) {
            Debug.logError(e, "Error calling getProductRouting: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        if (tasks != null) {
            for (Object task : (List<?>) tasks) {
                callSvcMap = new HashMap<String, Object>();
                ((Map<String, Object>) callSvcMap).put("workEffortId", ((Map<String, Object>) task).get("workEffortIdTo"));
                ((Map<String, Object>) callSvcMap).put("currencyUomId", context.get("currencyUomId"));
                ((Map<String, Object>) callSvcMap).put("productId", context.get("productId"));
                ((Map<String, Object>) callSvcMap).put("routingId", ((Map<String, Object>) routing).get("workEffortId"));
                try {
                    Map<String, Object> serviceResult = dispatcher.runSync("getTaskCost", (Map<String, Object>) callSvcMap);
                    if (ServiceUtil.isError(serviceResult)) {
                        return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
                    }
                    taskCost = serviceResult.get("taskCost");
                    costsByType = serviceResult.get("costsByType");
                } catch (Exception e) {
                    Debug.logError(e, "Error calling getTaskCost: " + e.getMessage(), MODULE);
                    return ServiceUtil.returnError(e.getMessage());
                }
                totalTaskCost = new BigDecimal(taskCost.toString());
                for (Map.Entry<String, Object> entry : ((Map<String, Object>) costsByType).entrySet()) {
                    String costType = entry.getKey();
                    Object costAmount = entry.getValue();
                    if (UtilValidate.isNotEmpty(((Map<String, Object>) totalCostsByType).get(costType))) {
                        ((Map<String, Object>) totalCostsByType).put("${costType}", new BigDecimal(((Map<String, Object>) totalCostsByType).get(costType).toString()));
                    } else {
                        totalCostsByType.put(costType, costAmount);
                    }
                    totalOtherTaskCost = new BigDecimal(costAmount.toString());
                }
            }
        }
        totalCost = (new BigDecimal(totalProductsCost.toString())).add(new BigDecimal(totalOtherTaskCost.toString()));
        if (UtilValidate.isNotEmpty(totalTaskCost)) {
            if (((Comparable) totalTaskCost).compareTo(BigDecimal.ZERO) > 0) {
                callSvcMap = new HashMap<String, Object>();
                ((Map<String, Object>) callSvcMap).put("costComponentTypeId", "" + context.get("costComponentTypePrefix") + "_ROUTE_COST");
                ((Map<String, Object>) callSvcMap).put("productId", context.get("productId"));
                ((Map<String, Object>) callSvcMap).put("costUomId", context.get("currencyUomId"));
                ((Map<String, Object>) callSvcMap).put("cost", totalTaskCost);
                try {
                    Map<String, Object> serviceResult = dispatcher.runSync("recreateCostComponent", (Map<String, Object>) callSvcMap);
                    if (ServiceUtil.isError(serviceResult)) {
                        return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
                    }
                } catch (Exception e) {
                    Debug.logError(e, "Error calling recreateCostComponent: " + e.getMessage(), MODULE);
                    return ServiceUtil.returnError(e.getMessage());
                }
            }
        }
        if (UtilValidate.isNotEmpty(totalProductsCost)) {
            if (((Comparable) totalProductsCost).compareTo(BigDecimal.ZERO) > 0) {
                callSvcMap = new HashMap<String, Object>();
                ((Map<String, Object>) callSvcMap).put("costComponentTypeId", "" + context.get("costComponentTypePrefix") + "_MAT_COST");
                ((Map<String, Object>) callSvcMap).put("productId", context.get("productId"));
                ((Map<String, Object>) callSvcMap).put("costUomId", context.get("currencyUomId"));
                ((Map<String, Object>) callSvcMap).put("cost", totalProductsCost);
                try {
                    Map<String, Object> serviceResult = dispatcher.runSync("recreateCostComponent", (Map<String, Object>) callSvcMap);
                    if (ServiceUtil.isError(serviceResult)) {
                        return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
                    }
                } catch (Exception e) {
                    Debug.logError(e, "Error calling recreateCostComponent: " + e.getMessage(), MODULE);
                    return ServiceUtil.returnError(e.getMessage());
                }
            }
        }
        for (Map.Entry<String, Object> entry : ((Map<String, Object>) totalCostsByType).entrySet()) {
            String costType = entry.getKey();
            Object totalCostAmount = entry.getValue();
            callSvcMap = new HashMap<String, Object>();
            ((Map<String, Object>) callSvcMap).put("costComponentTypeId", "" + context.get("costComponentTypePrefix") + "_" + costType);
            ((Map<String, Object>) callSvcMap).put("productId", context.get("productId"));
            ((Map<String, Object>) callSvcMap).put("costUomId", context.get("currencyUomId"));
            ((Map<String, Object>) callSvcMap).put("cost", totalCostAmount);
            try {
                Map<String, Object> serviceResult = dispatcher.runSync("recreateCostComponent", (Map<String, Object>) callSvcMap);
                if (ServiceUtil.isError(serviceResult)) {
                    return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
                }
            } catch (Exception e) {
                Debug.logError(e, "Error calling recreateCostComponent: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
        }
        List<GenericValue> productCostComponentCalcs = null;
        try {
            productCostComponentCalcs = EntityQuery.use(delegator)
                    .from("ProductCostComponentCalc")
                    .filterByDate()
                    .queryList();
        } catch (Exception e) {
            Debug.logError(e, "Error querying ProductCostComponentCalc: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        if (productCostComponentCalcs != null) {
            for (GenericValue productCostComponentCalc : productCostComponentCalcs) {
                try {
                    costComponentCalc = productCostComponentCalc.getRelatedOne("CostComponentCalc", false);
                } catch (Exception e) {
                    Debug.logError(e, "Error getting related one CostComponentCalc: " + e.getMessage(), MODULE);
                    return ServiceUtil.returnError(e.getMessage());
                }
                try {
                    customMethod = costComponentCalc.getRelatedOne("CustomMethod", false);
                } catch (Exception e) {
                    Debug.logError(e, "Error getting related one CustomMethod: " + e.getMessage(), MODULE);
                    return ServiceUtil.returnError(e.getMessage());
                }
                if (UtilValidate.isEmpty(customMethod)) {
                    Debug.logWarning("Unable to create cost component for cost component calc with id [" + costComponentCalc.get("costComponentCalcId") + "] because customMethod is not set", MODULE);
                } else {
                    customMethodParameters = new HashMap<String, Object>();
                    ((Map<String, Object>) customMethodParameters).put("productCostComponentCalc", productCostComponentCalc);
                    ((Map<String, Object>) customMethodParameters).put("costComponentCalc", costComponentCalc);
                    ((Map<String, Object>) customMethodParameters).put("currencyUomId", context.get("currencyUomId"));
                    ((Map<String, Object>) customMethodParameters).put("costComponentTypePrefix", context.get("costComponentTypePrefix"));
                    ((Map<String, Object>) customMethodParameters).put("baseCost", totalCost);
                    try {
                        Map<String, Object> serviceResult = dispatcher.runSync("${customMethod.customMethodName}", (Map<String, Object>) customMethodParameters);
                        if (ServiceUtil.isError(serviceResult)) {
                            return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
                        }
                        productCostAdjustment = serviceResult.get("productCostAdjustment");
                    } catch (Exception e) {
                        Debug.logError(e, "Error calling ${customMethod.customMethodName}: " + e.getMessage(), MODULE);
                        return ServiceUtil.returnError(e.getMessage());
                    }
                    callSvcMap = new HashMap<String, Object>();
                    ((Map<String, Object>) callSvcMap).put("costComponentTypeId", "" + context.get("costComponentTypePrefix") + "_" + productCostComponentCalc.get("costComponentTypeId"));
                    ((Map<String, Object>) callSvcMap).put("productId", productCostComponentCalc.get("productId"));
                    ((Map<String, Object>) callSvcMap).put("costUomId", context.get("currencyUomId"));
                    ((Map<String, Object>) callSvcMap).put("cost", productCostAdjustment);
                    try {
                        Map<String, Object> serviceResult = dispatcher.runSync("recreateCostComponent", (Map<String, Object>) callSvcMap);
                        if (ServiceUtil.isError(serviceResult)) {
                            return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
                        }
                    } catch (Exception e) {
                        Debug.logError(e, "Error calling recreateCostComponent: " + e.getMessage(), MODULE);
                        return ServiceUtil.returnError(e.getMessage());
                    }
                    totalCost = new BigDecimal(productCostAdjustment.toString());
                }
            }
        }
        result.put("totalCost", totalCost);

        return result;
    }


    /**
     * Calculate inventory average cost for a product
     */
    public static Map<String, Object> calculateProductAverageCost(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        Object currencyUomId = null;
        BigDecimal absValOfTotalInvCost = null;
        Object totalQuantityOnHand = null;
        Boolean differentCurrencies = null;
        Object absValOfTotalQOH = null;
        BigDecimal totalInventoryCost = null;
        BigDecimal productAverageCost = null;
        List<GenericValue> inventoryItems = null;
        try {
            inventoryItems = EntityQuery.use(delegator)
                    .from("InventoryItem")
                    .queryList();
        } catch (Exception e) {
            Debug.logError(e, "Error querying InventoryItem: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        totalQuantityOnHand = BigDecimal.ZERO;
        totalInventoryCost = BigDecimal.ZERO;
        absValOfTotalQOH = BigDecimal.ZERO;
        absValOfTotalInvCost = BigDecimal.ZERO;
        differentCurrencies = Boolean.FALSE;
        if (inventoryItems != null) {
            for (GenericValue inventoryItem : inventoryItems) {
                totalQuantityOnHand = (new BigDecimal(totalQuantityOnHand.toString())).add(new BigDecimal(inventoryItem.get("quantityOnHandTotal").toString()));
                if (UtilValidate.isEmpty(currencyUomId)) {
                    currencyUomId = inventoryItem.get("currencyUomId");
                }
                if (Boolean.FALSE.equals(differentCurrencies)) {
                    if (java.util.Objects.equals(inventoryItem.get("currencyUomId"), currencyUomId)) {
                        totalInventoryCost = (new BigDecimal(totalInventoryCost.toString())).add((new BigDecimal(inventoryItem.get("quantityOnHandTotal").toString())).multiply(new BigDecimal(inventoryItem.get("unitCost").toString())));
                        if (((Comparable) inventoryItem.get("quantityOnHandTotal")).compareTo(0L) < 0) {
                            absValOfTotalQOH = (new BigDecimal(absValOfTotalQOH.toString())).add(new BigDecimal(inventoryItem.get("quantityOnHandTotal").toString()));
                            absValOfTotalInvCost = (new BigDecimal(absValOfTotalInvCost.toString())).add((new BigDecimal(inventoryItem.get("quantityOnHandTotal").toString())).multiply(new BigDecimal(inventoryItem.get("unitCost").toString())));
                        } else {
                            absValOfTotalQOH = (new BigDecimal(absValOfTotalQOH.toString())).add(new BigDecimal(inventoryItem.get("quantityOnHandTotal").toString()));
                            absValOfTotalInvCost = (new BigDecimal(absValOfTotalInvCost.toString())).add((new BigDecimal(inventoryItem.get("quantityOnHandTotal").toString())).multiply(new BigDecimal(inventoryItem.get("unitCost").toString())));
                        }
                    } else {
                        differentCurrencies = Boolean.TRUE;
                    }
                }
            }
        }
        if (!java.util.Objects.equals(absValOfTotalQOH, BigDecimal.ZERO)) {
            productAverageCost = (new BigDecimal(absValOfTotalInvCost.toString())).divide(new BigDecimal(absValOfTotalQOH.toString()), java.math.RoundingMode.HALF_UP);
        } else {
            productAverageCost = BigDecimal.ZERO;
        }
        result.put("totalQuantityOnHand", totalQuantityOnHand);
        if (Boolean.FALSE.equals(differentCurrencies)) {
            result.put("totalInventoryCost", totalInventoryCost);
            result.put("productAverageCost", productAverageCost);
            result.put("currencyUomId", currencyUomId);
        }

        return result;
    }


    /**
     * Update a Product Average Cost record on receive inventory
     */
    public static Map<String, Object> updateProductAverageCostOnReceiveInventory(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        List<String> error_list = new LinkedList<>();

        Object organizationPartyId = null;
        GenericValue facility = null;
        GenericValue productStore = null;
        String roundingMode = null;
        BigDecimal oldProductQuantity = null;
        Long timeDiff = null;
        Object quantityOnHandTotal = null;
        Map<String, Object> updateProductAverageCostMap = null;
        Map<String, Object> productAverageCostMap = null;
        String roundingDecimals = null;
        Timestamp nowTimestamp = null;
        Map<String, Object> serviceInMap = null;
        GenericValue inventoryItem = null;
        try {
            inventoryItem = EntityQuery.use(delegator)
                    .from("InventoryItem")
                    .where(context)
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying InventoryItem: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        organizationPartyId = inventoryItem.get("ownerPartyId");
        if (UtilValidate.isEmpty(organizationPartyId)) {
            try {
                facility = EntityQuery.use(delegator)
                        .from("Facility")
                        .where(context)
                        .queryOne();
            } catch (Exception e) {
                Debug.logError(e, "Error querying Facility: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
            organizationPartyId = facility.get("ownerPartyId");
            if (UtilValidate.isEmpty(organizationPartyId)) {
                try {
                    productStore = facility.getRelatedOne("ProductStore", false);
                } catch (Exception e) {
                    Debug.logError(e, "Error getting related one ProductStore: " + e.getMessage(), MODULE);
                    return ServiceUtil.returnError(e.getMessage());
                }
                organizationPartyId = productStore.get("ownerPartyId");
                if (UtilValidate.isEmpty(organizationPartyId)) {
                    {
                        String errorMsg = UtilProperties.getMessage("ProductUiLabels", "ProductOwnerPartyIsMissing", locale);
                        error_list.add(errorMsg);
                    }
                }
                if (!error_list.isEmpty()) {
                    return ServiceUtil.returnError(error_list);
                }
            }
        }
        List<GenericValue> productAverageCostList = null;
        try {
            productAverageCostList = EntityQuery.use(delegator)
                    .from("ProductAverageCost")
                    .where(UtilMisc.toMap("productId", context.get("productId"), "facilityId", context.get("facilityId"), "productAverageCostTypeId", "SIMPLE_AVG_COST", "organizationPartyId", organizationPartyId))
                    .filterByDate()
                    .queryList();
        } catch (Exception e) {
            Debug.logError(e, "Error querying entity: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        GenericValue productAverageCost = EntityUtil.getFirst((List<GenericValue>) productAverageCostList);
        // set-service-fields from "parameters" to "productAverageCostMap" for service "createProductAverageCost"
        productAverageCostMap.putAll(UtilMisc.toMap(context));
        productAverageCostMap.put("productAverageCostTypeId", "SIMPLE_AVG_COST");
        productAverageCostMap.put("organizationPartyId", organizationPartyId);
        if (UtilValidate.isEmpty(productAverageCost)) {
            productAverageCostMap.put("averageCost", inventoryItem.get("unitCost"));
        } else {
            // set-service-fields from "productAverageCost" to "updateProductAverageCostMap" for service "updateProductAverageCost"
            updateProductAverageCostMap.putAll(UtilMisc.toMap(productAverageCost));
            Timestamp updateProductAverageCostMap_thruDate = new Timestamp(System.currentTimeMillis());
            try {
                Map<String, Object> serviceResult = dispatcher.runSync("updateProductAverageCost", updateProductAverageCostMap);
                if (ServiceUtil.isError(serviceResult)) {
                    return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
                }
            } catch (Exception e) {
                Debug.logError(e, "Error calling updateProductAverageCost: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
            serviceInMap.put("productId", context.get("productId"));
            serviceInMap.put("facilityId", context.get("facilityId"));
            try {
                Map<String, Object> serviceResult = dispatcher.runSync("getInventoryAvailableByFacility", serviceInMap);
                if (ServiceUtil.isError(serviceResult)) {
                    return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
                }
                quantityOnHandTotal = serviceResult.get("quantityOnHandTotal");
            } catch (Exception e) {
                Debug.logError(e, "Error calling getInventoryAvailableByFacility: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
            oldProductQuantity = (BigDecimal) ((BigDecimal) quantityOnHandTotal).subtract((BigDecimal) context.get("quantityAccepted"));
            productAverageCostMap.put("averageCost", (BigDecimal) ((BigDecimal) ((Map<String, Object>) context.get("(productAverageCost")).get("averageCost * oldProductQuantity")).add((BigDecimal) ((Map<String, Object>) inventoryItem.get("unitCost * parameters")).get("quantityAccepted))/(quantityOnHandTotal")));
            roundingDecimals = UtilProperties.getMessage("arithmetic", "finaccount.decimals", locale);
            roundingMode = UtilProperties.getMessage("arithmetic", "finaccount.roundingSimpleMethod", locale);
            ((Map<String, Object>) productAverageCostMap).put("averageCost", (new BigDecimal(((Map<String, Object>) productAverageCostMap).get("averageCost").toString())).setScale(Integer.parseInt(roundingDecimals), RoundingMode.valueOf(String.valueOf(roundingMode).toUpperCase().replace("-", "_"))));
            nowTimestamp = new Timestamp(System.currentTimeMillis());
            timeDiff = (Long) GroovyUtil.eval("return nowTimestamp.getTime() - productAverageCost.fromDate.getTime()", UtilMisc.toMap("delegator", delegator, "dispatcher", dispatcher, "locale", locale, "userLogin", userLogin, "context", context, "parameters", context));
            if (((Comparable) timeDiff).compareTo(1000L) <= 0) {
                {
                    Calendar cal = Calendar.getInstance();
                    cal.setTime(new java.util.Date(((java.sql.Timestamp) nowTimestamp).getTime()));
                    cal.add(Calendar.SECOND, +1);
                    Timestamp productAverageCostMap_fromDate = new Timestamp(cal.getTimeInMillis());
                }
            } else {
                productAverageCostMap.put("fromDate", nowTimestamp);
            }
        }
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("createProductAverageCost", productAverageCostMap);
            if (ServiceUtil.isError(serviceResult)) {
                return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
            }
        } catch (Exception e) {
            Debug.logError(e, "Error calling createProductAverageCost: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        Debug.logInfo("For facilityId " + context.get("facilityId") + ", Average cost of product " + context.get("productId") + " is set from  " + ((Map<String, Object>) updateProductAverageCostMap).get("averageCost") + " to " + ((Map<String, Object>) productAverageCostMap).get("averageCost"), MODULE);

        return result;
    }


    /**
     * Service to get the average cost of product
     */
    public static Map<String, Object> getProductAverageCost(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        List<GenericValue> productAverageCostList = null;
        GenericValue productAverageCost = null;
        BigDecimal unitCost = null;
        Object inventoryItem = context.get("inventoryItem");
        Map<String, Object> getPartyAcctgPrefMap = new HashMap<String, Object>();
        getPartyAcctgPrefMap.put("organizationPartyId", ((Map<String, Object>) inventoryItem).get("ownerPartyId"));
        Object partyAccountingPreference = null;
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("getPartyAccountingPreferences", getPartyAcctgPrefMap);
            if (ServiceUtil.isError(serviceResult)) {
                return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
            }
            partyAccountingPreference = serviceResult.get("partyAccountingPreference");
        } catch (Exception e) {
            Debug.logError(e, "Error calling getPartyAccountingPreferences: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        if ("COGS_AVG_COST".equals(((Map<String, Object>) partyAccountingPreference).get("cogsMethodId"))) {
            try {
                productAverageCostList = EntityQuery.use(delegator)
                        .from("ProductAverageCost")
                        .where(UtilMisc.toMap("productAverageCostTypeId", "SIMPLE_AVG_COST", "organizationPartyId", ((Map<String, Object>) inventoryItem).get("ownerPartyId"), "productId", ((Map<String, Object>) inventoryItem).get("productId"), "facilityId", ((Map<String, Object>) inventoryItem).get("facilityId")))
                        .filterByDate()
                        .queryList();
            } catch (Exception e) {
                Debug.logError(e, "Error querying entity: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
            productAverageCost = EntityUtil.getFirst((List<GenericValue>) productAverageCostList);
        }
        if (UtilValidate.isNotEmpty(productAverageCost)) {
            unitCost = (BigDecimal) productAverageCost.get("averageCost");
        } else {
            unitCost = (BigDecimal) ((Map<String, Object>) inventoryItem).get("unitCost");
        }
        result.put("unitCost", unitCost);

        return result;
    }


    /**
     * Formula that creates a cost component equal to a percentage of total product cost
     */
    public static Map<String, Object> productCostPercentageFormula(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        Object productCostComponentCalc = context.get("productCostComponentCalc");
        Object costComponentCalc = context.get("costComponentCalc");
        Map<String, Object> inputMap = new HashMap<String, Object>();
        inputMap.put("productId", ((Map<String, Object>) productCostComponentCalc).get("productId"));
        inputMap.put("currencyUomId", context.get("currencyUomId"));
        inputMap.put("costComponentTypePrefix", context.get("costComponentTypePrefix"));
        Object productCost = null;
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("getProductCost", inputMap);
            if (ServiceUtil.isError(serviceResult)) {
                return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
            }
            productCost = serviceResult.get("productCost");
        } catch (Exception e) {
            Debug.logError(e, "Error calling getProductCost: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        BigDecimal productCostAdjustment = (new BigDecimal(context.get("baseCost").toString())).setScale(6, RoundingMode.HALF_UP);
        result.put("productCostAdjustment", productCostAdjustment);

        return result;
    }

}
