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
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.GroovyUtil;
import org.ofbiz.base.util.UtilMisc;
import org.ofbiz.base.util.UtilProperties;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.GenericValue;
import org.ofbiz.entity.condition.EntityCondition;
import org.ofbiz.entity.util.EntityQuery;
import org.ofbiz.security.Security;
import org.ofbiz.service.DispatchContext;
import org.ofbiz.service.LocalDispatcher;
import org.ofbiz.service.ServiceUtil;

/**
 * Auto-generated event class from Simple Method XML.
 *
 * <p>Generated from: component://product/script/org/ofbiz/product/price/PriceServices.xml</p>
 *
 * <p>SCIPIO: 4.0.0: Auto-generated.</p>
 */
public class PriceServices {

    private static final String MODULE = PriceServices.class.getName();


    /**
     * Create a Product Price
     */
    public static Map<String, Object> createProductPrice(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        Security security = dctx.getSecurity();
        List<String> error_list = new LinkedList<>();

        GenericValue newEntity = null;
        Object taxAuthCombinedId = null;
        List<GenericValue> taxAuthorityRateProductList = null;
        Object callingMethodName = "createProductPrice";
        Object checkAction = "CREATE";
        // TODO: Call simple-method "checkProductRelatedPermission" from "component://product/script/org/ofbiz/product/product/ProductServices.xml"
        if (!security.hasPermission("CATALOG_PRICE_MAINT", userLogin)) {
            return ServiceUtil.returnError(UtilProperties.getMessage("ProductUiLabels", "ProductPriceMaintPermissionError", locale));
        }
        if (!error_list.isEmpty()) {
            return ServiceUtil.returnError(error_list);
        }
        Map<String, Object> inlineResult = inlineHandlePriceWithTaxIncluded(dctx, context);
        if (ServiceUtil.isError(inlineResult)) {
            return inlineResult;
        }
        newEntity = delegator.makeValue("ProductPrice");
        newEntity.setNonPKFields(context);
        newEntity.setPKFields(context);
        Timestamp nowTimestamp = new Timestamp(System.currentTimeMillis());
        if (UtilValidate.isEmpty(newEntity.get("fromDate"))) {
            newEntity.put("fromDate", nowTimestamp);
        }
        result.put("fromDate", newEntity.get("fromDate"));
        newEntity.put("lastModifiedDate", nowTimestamp);
        newEntity.put("createdDate", nowTimestamp);
        newEntity.put("lastModifiedByUserLogin", userLogin.get("userLoginId"));
        newEntity.put("createdByUserLogin", userLogin.get("userLoginId"));
        try {
            delegator.create(newEntity);
        } catch (Exception e) {
            Debug.logError(e, "Error creating value: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }

        return result;
    }


    /**
     * Update an ProductPrice
     */
    public static Map<String, Object> updateProductPrice(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        Security security = dctx.getSecurity();
        List<String> error_list = new LinkedList<>();

        Object taxAuthCombinedId = null;
        List<GenericValue> taxAuthorityRateProductList = null;
        Object callingMethodName = "updateProductPrice";
        Object checkAction = "UPDATE";
        // TODO: Call simple-method "checkProductRelatedPermission" from "component://product/script/org/ofbiz/product/product/ProductServices.xml"
        if (!security.hasPermission("CATALOG_PRICE_MAINT", userLogin)) {
            return ServiceUtil.returnError(UtilProperties.getMessage("ProductUiLabels", "ProductPriceMaintPermissionError", locale));
        }
        if (!error_list.isEmpty()) {
            return ServiceUtil.returnError(error_list);
        }
        Map<String, Object> inlineResult = inlineHandlePriceWithTaxIncluded(dctx, context);
        if (ServiceUtil.isError(inlineResult)) {
            return inlineResult;
        }
        GenericValue lookedUpValue = null;
        try {
            lookedUpValue = EntityQuery.use(delegator)
                    .from("ProductPrice")
                    .where(context)
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying ProductPrice: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        result.put("oldPrice", lookedUpValue.get("price"));
        lookedUpValue.setNonPKFields(context);
        Timestamp nowTimestamp = new Timestamp(System.currentTimeMillis());
        lookedUpValue.put("lastModifiedDate", nowTimestamp);
        lookedUpValue.put("lastModifiedByUserLogin", userLogin.get("userLoginId"));
        try {
            delegator.store(lookedUpValue);
        } catch (Exception e) {
            Debug.logError(e, "Error storing value: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }

        return result;
    }


    /**
     * Delete an ProductPrice
     */
    public static Map<String, Object> deleteProductPrice(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        Security security = dctx.getSecurity();
        List<String> error_list = new LinkedList<>();

        Object callingMethodName = "deleteProductPrice";
        Object checkAction = "DELETE";
        // TODO: Call simple-method "checkProductRelatedPermission" from "component://product/script/org/ofbiz/product/product/ProductServices.xml"
        if (!security.hasPermission("CATALOG_PRICE_MAINT", userLogin)) {
            return ServiceUtil.returnError(UtilProperties.getMessage("ProductUiLabels", "ProductPriceMaintPermissionError", locale));
        }
        if (!error_list.isEmpty()) {
            return ServiceUtil.returnError(error_list);
        }
        GenericValue lookupPKMap = delegator.makeValue("ProductPrice");
        lookupPKMap.setPKFields(context);
        GenericValue lookedUpValue = null;
        try {
            lookedUpValue = EntityQuery.use(delegator)
                    .from("ProductPrice")
                    .where(lookupPKMap)
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error finding by primary key ProductPrice: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        result.put("oldPrice", lookedUpValue.get("price"));
        try {
            delegator.removeValue(lookedUpValue);
        } catch (Exception e) {
            Debug.logError(e, "Error removing value: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }

        return result;
    }


    /**
     * Inline Handle Price with Tax Included
     */
    public static Map<String, Object> inlineHandlePriceWithTaxIncluded(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        List<String> error_list = new LinkedList<>();

        Object taxAuthCombinedId = null;
        List<GenericValue> taxAuthorityRateProductList = null;
        if (UtilValidate.isNotEmpty(context.get("taxAuthCombinedId"))) {
            taxAuthCombinedId = context.get("taxAuthCombinedId");
            context.put("taxAuthGeoId", GroovyUtil.eval("taxAuthCombinedId.substring(0,taxAuthCombinedId.indexOf('::'))", UtilMisc.toMap("delegator", delegator, "dispatcher", dispatcher, "locale", locale, "userLogin", userLogin, "context", context, "parameters", context)));
            context.put("taxAuthPartyId", GroovyUtil.eval("taxAuthCombinedId.substring(taxAuthCombinedId.indexOf('::')+2)", UtilMisc.toMap("delegator", delegator, "dispatcher", dispatcher, "locale", locale, "userLogin", userLogin, "context", context, "parameters", context)));
        }
        Object parameters_priceWithTax = null;
        BigDecimal parameters_taxPercentage = null;
        Object parameters_price = null;
        if ((!(UtilValidate.isEmpty(context.get("taxAuthPartyId"))) && !(UtilValidate.isEmpty(context.get("taxAuthGeoId"))))) {
            context.put("priceWithTax", context.get("price"));
            if (UtilValidate.isEmpty(context.get("taxPercentage"))) {
                try {
                    taxAuthorityRateProductList = EntityQuery.use(delegator)
                            .from("TaxAuthorityRateProduct")
                            .filterByDate()
                            .queryList();
                } catch (Exception e) {
                    Debug.logError(e, "Error querying TaxAuthorityRateProduct: " + e.getMessage(), MODULE);
                    return ServiceUtil.returnError(e.getMessage());
                }
                context.put("taxPercentage", ((GenericValue) ((List<?>) taxAuthorityRateProductList).get(0)).get("taxPercentage"));
            }
            if (UtilValidate.isEmpty(context.get("taxPercentage"))) {
                {
                    String errorMsg = UtilProperties.getMessage("ProductUiLabels", "ProductPriceTaxPercentageNotFound", locale);
                    error_list.add(errorMsg);
                }
                if (!error_list.isEmpty()) {
                    return ServiceUtil.returnError(error_list);
                }
            }
            ((Map<String, Object>) context).put("taxAmount", ((new BigDecimal(context.get("priceWithTax").toString())).subtract((new BigDecimal(context.get("priceWithTax").toString())).divide(new BigDecimal(context.get("taxPercentage").toString()), java.math.RoundingMode.HALF_UP))).setScale(3, RoundingMode.HALF_UP));
            ((Map<String, Object>) context).put("priceWithoutTax", ((new BigDecimal(context.get("priceWithTax").toString())).subtract(new BigDecimal(context.get("taxAmount").toString()))).setScale(3, RoundingMode.HALF_UP));
            if ("Y".equals(context.get("taxInPrice"))) {
                context.put("price", context.get("priceWithTax"));
            } else {
                context.put("price", context.get("priceWithoutTax"));
            }
        }

        return result;
    }


    /**
     * Save History of ProductPrice Change
     */
    public static Map<String, Object> saveProductPriceChange(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        Security security = dctx.getSecurity();
        List<String> error_list = new LinkedList<>();

        if (!security.hasEntityPermission("CATALOG", "_UPDATE", userLogin)) {
            return ServiceUtil.returnError(UtilProperties.getMessage("ProductUiLabels", "ProductCatalogCreatePermissionError", locale));
        }
        if (!error_list.isEmpty()) {
            return ServiceUtil.returnError(error_list);
        }
        GenericValue newEntity = delegator.makeValue("ProductPriceChange");
        newEntity.setNonPKFields(context);
        String productPriceChangeId = delegator.getNextSeqId("ProductPriceChange");
        newEntity.put("productPriceChangeId", productPriceChangeId);
        Timestamp nowTimestamp = new Timestamp(System.currentTimeMillis());
        newEntity.put("changedDate", nowTimestamp);
        newEntity.put("changedByUserLogin", userLogin.get("userLoginId"));
        try {
            delegator.create(newEntity);
        } catch (Exception e) {
            Debug.logError(e, "Error creating value: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        result.put("productPriceChangeId", productPriceChangeId);

        return result;
    }


    /**
     * create a ProductPaymentMethodType
     */
    public static Map<String, Object> createProductPaymentMethodType(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        List<String> error_list = new LinkedList<>();

        Object callingMethodName = "createProductPaymentMethodType";
        Object checkAction = "CREATE";
        // TODO: Call simple-method "checkProductRelatedPermission" from "component://product/script/org/ofbiz/product/product/ProductServices.xml"
        if (!error_list.isEmpty()) {
            return ServiceUtil.returnError(error_list);
        }
        GenericValue newEntity = delegator.makeValue("ProductPaymentMethodType");
        newEntity.setPKFields(context);
        if (UtilValidate.isEmpty(newEntity.get("fromDate"))) {
            Timestamp newEntity_fromDate = new Timestamp(System.currentTimeMillis());
        }
        newEntity.setNonPKFields(context);
        try {
            delegator.create(newEntity);
        } catch (Exception e) {
            Debug.logError(e, "Error creating value: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }

        return result;
    }


    /**
     * update a ProductPaymentMethodType
     */
    public static Map<String, Object> updateProductPaymentMethodType(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        List<String> error_list = new LinkedList<>();

        Object callingMethodName = "updateProductPaymentMethodType";
        Object checkAction = "UPDATE";
        // TODO: Call simple-method "checkProductRelatedPermission" from "component://product/script/org/ofbiz/product/product/ProductServices.xml"
        if (!error_list.isEmpty()) {
            return ServiceUtil.returnError(error_list);
        }
        GenericValue lookedUpValue = null;
        try {
            lookedUpValue = EntityQuery.use(delegator)
                    .from("ProductPaymentMethodType")
                    .where(context)
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying ProductPaymentMethodType: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        lookedUpValue.setNonPKFields(context);
        try {
            delegator.store(lookedUpValue);
        } catch (Exception e) {
            Debug.logError(e, "Error storing value: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }

        return result;
    }


    /**
     * delete a ProductPaymentMethodType
     */
    public static Map<String, Object> deleteProductPaymentMethodType(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        List<String> error_list = new LinkedList<>();

        Object callingMethodName = "deleteProductPaymentMethodType";
        Object checkAction = "DELETE";
        // TODO: Call simple-method "checkProductRelatedPermission" from "component://product/script/org/ofbiz/product/product/ProductServices.xml"
        if (!error_list.isEmpty()) {
            return ServiceUtil.returnError(error_list);
        }
        GenericValue lookedUpValue = null;
        try {
            lookedUpValue = EntityQuery.use(delegator)
                    .from("ProductPaymentMethodType")
                    .where(context)
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying ProductPaymentMethodType: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        try {
            delegator.removeValue(lookedUpValue);
        } catch (Exception e) {
            Debug.logError(e, "Error removing value: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }

        return result;
    }


    /**
     * Create an ProductPriceRule
     */
    public static Map<String, Object> createProductPriceRule(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        Security security = dctx.getSecurity();
        List<String> error_list = new LinkedList<>();

        if (!security.hasEntityPermission("CATALOG", "_CREATE", userLogin)) {
            return ServiceUtil.returnError(UtilProperties.getMessage("ProductUiLabels", "ProductCatalogCreatePermissionError", locale));
        }
        if (!security.hasPermission("CATALOG_PRICE_MAINT", userLogin)) {
            return ServiceUtil.returnError(UtilProperties.getMessage("ProductUiLabels", "ProductPriceMaintPermissionError", locale));
        }
        if (!error_list.isEmpty()) {
            return ServiceUtil.returnError(error_list);
        }
        GenericValue newEntity = delegator.makeValue("ProductPriceRule");
        newEntity.setNonPKFields(context);
        if (UtilValidate.isEmpty(context.get("fromDate"))) {
            Timestamp newEntity_fromDate = new Timestamp(System.currentTimeMillis());
        }
        ((GenericValue) newEntity).put("productPriceRuleId", delegator.getNextSeqId("ProductPriceRule"));
        result.put("productPriceRuleId", newEntity.get("productPriceRuleId"));
        try {
            delegator.create(newEntity);
        } catch (Exception e) {
            Debug.logError(e, "Error creating value: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }

        return result;
    }


    /**
     * Update an ProductPriceRule
     */
    public static Map<String, Object> updateProductPriceRule(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        Security security = dctx.getSecurity();
        List<String> error_list = new LinkedList<>();

        if (!security.hasEntityPermission("CATALOG", "_UPDATE", userLogin)) {
            return ServiceUtil.returnError(UtilProperties.getMessage("ProductUiLabels", "ProductCatalogUpdatePermissionError", locale));
        }
        if (!security.hasPermission("CATALOG_PRICE_MAINT", userLogin)) {
            return ServiceUtil.returnError(UtilProperties.getMessage("ProductUiLabels", "ProductPriceMaintPermissionError", locale));
        }
        if (!error_list.isEmpty()) {
            return ServiceUtil.returnError(error_list);
        }
        GenericValue lookupPKMap = delegator.makeValue("ProductPriceRule");
        lookupPKMap.setPKFields(context);
        GenericValue lookedUpValue = null;
        try {
            lookedUpValue = EntityQuery.use(delegator)
                    .from("ProductPriceRule")
                    .where(lookupPKMap)
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error finding by primary key ProductPriceRule: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        lookedUpValue.setNonPKFields(context);
        try {
            delegator.store(lookedUpValue);
        } catch (Exception e) {
            Debug.logError(e, "Error storing value: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }

        return result;
    }


    /**
     * Delete an ProductPriceRule
     */
    public static Map<String, Object> deleteProductPriceRule(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        Security security = dctx.getSecurity();
        List<String> error_list = new LinkedList<>();

        if (!security.hasEntityPermission("CATALOG", "_DELETE", userLogin)) {
            return ServiceUtil.returnError(UtilProperties.getMessage("ProductUiLabels", "ProductCatalogDeletePermissionError", locale));
        }
        if (!security.hasPermission("CATALOG_PRICE_MAINT", userLogin)) {
            return ServiceUtil.returnError(UtilProperties.getMessage("ProductUiLabels", "ProductPriceMaintPermissionError", locale));
        }
        if (!error_list.isEmpty()) {
            return ServiceUtil.returnError(error_list);
        }
        GenericValue lookupPKMap = delegator.makeValue("ProductPriceRule");
        lookupPKMap.setPKFields(context);
        GenericValue lookedUpValue = null;
        try {
            lookedUpValue = EntityQuery.use(delegator)
                    .from("ProductPriceRule")
                    .where(lookupPKMap)
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error finding by primary key ProductPriceRule: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        try {
            delegator.removeValue(lookedUpValue);
        } catch (Exception e) {
            Debug.logError(e, "Error removing value: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }

        return result;
    }


    /**
     * Create an ProductPriceCond
     */
    public static Map<String, Object> createProductPriceCond(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        Security security = dctx.getSecurity();
        List<String> error_list = new LinkedList<>();

        if (!security.hasEntityPermission("CATALOG", "_CREATE", userLogin)) {
            return ServiceUtil.returnError(UtilProperties.getMessage("ProductUiLabels", "ProductCatalogCreatePermissionError", locale));
        }
        if (!security.hasPermission("CATALOG_PRICE_MAINT", userLogin)) {
            return ServiceUtil.returnError(UtilProperties.getMessage("ProductUiLabels", "ProductPriceMaintPermissionError", locale));
        }
        if (!error_list.isEmpty()) {
            return ServiceUtil.returnError(error_list);
        }
        if (UtilValidate.isNotEmpty(context.get("condValueInput"))) {
            context.put("condValue", context.get("condValueInput"));
        }
        GenericValue newEntity = delegator.makeValue("ProductPriceCond");
        newEntity.setNonPKFields(context);
        newEntity.setPKFields(context);
        delegator.setNextSubSeqId(newEntity, "productPriceCondSeqId", 2, 1);
        Object productPriceCondSeqId = newEntity.get("productPriceCondSeqId");
        result.put("productPriceCondSeqId", newEntity.get("productPriceCondSeqId"));
        try {
            delegator.create(newEntity);
        } catch (Exception e) {
            Debug.logError(e, "Error creating value: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }

        return result;
    }


    /**
     * Update an ProductPriceCond
     */
    public static Map<String, Object> updateProductPriceCond(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        Security security = dctx.getSecurity();
        List<String> error_list = new LinkedList<>();

        if (!security.hasEntityPermission("CATALOG", "_UPDATE", userLogin)) {
            return ServiceUtil.returnError(UtilProperties.getMessage("ProductUiLabels", "ProductCatalogUpdatePermissionError", locale));
        }
        if (!security.hasPermission("CATALOG_PRICE_MAINT", userLogin)) {
            return ServiceUtil.returnError(UtilProperties.getMessage("ProductUiLabels", "ProductPriceMaintPermissionError", locale));
        }
        if (!error_list.isEmpty()) {
            return ServiceUtil.returnError(error_list);
        }
        if ("PRIP_QUANTITY".equals(context.get("inputParamEnumId"))) {
            context.put("condValue", context.get("condValueInput"));
        }
        if ("PRIP_LIST_PRICE".equals(context.get("inputParamEnumId"))) {
            context.put("condValue", context.get("condValueInput"));
        }
        GenericValue lookupPKMap = delegator.makeValue("ProductPriceCond");
        lookupPKMap.setPKFields(context);
        GenericValue lookedUpValue = null;
        try {
            lookedUpValue = EntityQuery.use(delegator)
                    .from("ProductPriceCond")
                    .where(lookupPKMap)
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error finding by primary key ProductPriceCond: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        lookedUpValue.setNonPKFields(context);
        try {
            delegator.store(lookedUpValue);
        } catch (Exception e) {
            Debug.logError(e, "Error storing value: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }

        return result;
    }


    /**
     * Delete an ProductPriceCond
     */
    public static Map<String, Object> deleteProductPriceCond(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        Security security = dctx.getSecurity();
        List<String> error_list = new LinkedList<>();

        if (!security.hasEntityPermission("CATALOG", "_DELETE", userLogin)) {
            return ServiceUtil.returnError(UtilProperties.getMessage("ProductUiLabels", "ProductCatalogDeletePermissionError", locale));
        }
        if (!security.hasPermission("CATALOG_PRICE_MAINT", userLogin)) {
            return ServiceUtil.returnError(UtilProperties.getMessage("ProductUiLabels", "ProductPriceMaintPermissionError", locale));
        }
        if (!error_list.isEmpty()) {
            return ServiceUtil.returnError(error_list);
        }
        GenericValue lookupPKMap = delegator.makeValue("ProductPriceCond");
        lookupPKMap.setPKFields(context);
        GenericValue lookedUpValue = null;
        try {
            lookedUpValue = EntityQuery.use(delegator)
                    .from("ProductPriceCond")
                    .where(lookupPKMap)
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error finding by primary key ProductPriceCond: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        try {
            delegator.removeValue(lookedUpValue);
        } catch (Exception e) {
            Debug.logError(e, "Error removing value: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }

        return result;
    }


    /**
     * Create an ProductPriceAction
     */
    public static Map<String, Object> createProductPriceAction(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        Security security = dctx.getSecurity();
        List<String> error_list = new LinkedList<>();

        if (!security.hasEntityPermission("CATALOG", "_CREATE", userLogin)) {
            return ServiceUtil.returnError(UtilProperties.getMessage("ProductUiLabels", "ProductCatalogCreatePermissionError", locale));
        }
        if (!security.hasPermission("CATALOG_PRICE_MAINT", userLogin)) {
            return ServiceUtil.returnError(UtilProperties.getMessage("ProductUiLabels", "ProductPriceMaintPermissionError", locale));
        }
        if (!error_list.isEmpty()) {
            return ServiceUtil.returnError(error_list);
        }
        GenericValue newEntity = delegator.makeValue("ProductPriceAction");
        newEntity.setNonPKFields(context);
        newEntity.setPKFields(context);
        delegator.setNextSubSeqId(newEntity, "productPriceActionSeqId", 2, 1);
        Object productPriceActionSeqId = newEntity.get("productPriceActionSeqId");
        result.put("productPriceActionSeqId", newEntity.get("productPriceActionSeqId"));
        try {
            delegator.create(newEntity);
        } catch (Exception e) {
            Debug.logError(e, "Error creating value: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }

        return result;
    }


    /**
     * Update an ProductPriceAction
     */
    public static Map<String, Object> updateProductPriceAction(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        Security security = dctx.getSecurity();
        List<String> error_list = new LinkedList<>();

        if (!security.hasEntityPermission("CATALOG", "_UPDATE", userLogin)) {
            return ServiceUtil.returnError(UtilProperties.getMessage("ProductUiLabels", "ProductCatalogUpdatePermissionError", locale));
        }
        if (!security.hasPermission("CATALOG_PRICE_MAINT", userLogin)) {
            return ServiceUtil.returnError(UtilProperties.getMessage("ProductUiLabels", "ProductPriceMaintPermissionError", locale));
        }
        if (!error_list.isEmpty()) {
            return ServiceUtil.returnError(error_list);
        }
        GenericValue lookupPKMap = delegator.makeValue("ProductPriceAction");
        lookupPKMap.setPKFields(context);
        GenericValue lookedUpValue = null;
        try {
            lookedUpValue = EntityQuery.use(delegator)
                    .from("ProductPriceAction")
                    .where(lookupPKMap)
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error finding by primary key ProductPriceAction: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        lookedUpValue.setNonPKFields(context);
        try {
            delegator.store(lookedUpValue);
        } catch (Exception e) {
            Debug.logError(e, "Error storing value: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }

        return result;
    }


    /**
     * Delete an ProductPriceAction
     */
    public static Map<String, Object> deleteProductPriceAction(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        Security security = dctx.getSecurity();
        List<String> error_list = new LinkedList<>();

        if (!security.hasEntityPermission("CATALOG", "_DELETE", userLogin)) {
            return ServiceUtil.returnError(UtilProperties.getMessage("ProductUiLabels", "ProductCatalogDeletePermissionError", locale));
        }
        if (!security.hasPermission("CATALOG_PRICE_MAINT", userLogin)) {
            return ServiceUtil.returnError(UtilProperties.getMessage("ProductUiLabels", "ProductPriceMaintPermissionError", locale));
        }
        if (!error_list.isEmpty()) {
            return ServiceUtil.returnError(error_list);
        }
        GenericValue lookupPKMap = delegator.makeValue("ProductPriceAction");
        lookupPKMap.setPKFields(context);
        GenericValue lookedUpValue = null;
        try {
            lookedUpValue = EntityQuery.use(delegator)
                    .from("ProductPriceAction")
                    .where(lookupPKMap)
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error finding by primary key ProductPriceAction: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        try {
            delegator.removeValue(lookedUpValue);
        } catch (Exception e) {
            Debug.logError(e, "Error removing value: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }

        return result;
    }


    /**
     * Set the Value options for selected Price Rule Condition Input
     */
    public static Map<String, Object> getAssociatedPriceRulesConds(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        List<Object> productPriceRulesCondValues = null;
        List<GenericValue> condValues = null;
        Object option = null;
        String noOptions = null;
        if (("PRIP_QUANTITY".equals(context.get("inputParamEnumId")) || "PRIP_LIST_PRICE".equals(context.get("inputParamEnumId")))) {
            return result;
        }
        if ("PRIP_PRODUCT_ID".equals(context.get("inputParamEnumId"))) {
            try {
                condValues = EntityQuery.use(delegator)
                        .from("Product")
                        .queryList();
            } catch (Exception e) {
                Debug.logError(e, "Error querying Product: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
            if (condValues != null) {
                for (GenericValue condValue : condValues) {
                    option = "" + condValue.get("internalName") + ": " + condValue.get("productId");
                    productPriceRulesCondValues.add(option);
                }
            }
        }
        if ("PRIP_PROD_CAT_ID".equals(context.get("inputParamEnumId"))) {
            try {
                condValues = EntityQuery.use(delegator)
                        .from("ProductCategory")
                        .queryList();
            } catch (Exception e) {
                Debug.logError(e, "Error querying ProductCategory: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
            if (condValues != null) {
                for (GenericValue condValueEntry : condValues) {
                    option = "" + condValueEntry.get("categoryName") + " " + condValueEntry.get("description") + " [" + condValueEntry.get("productCategoryId") + "]: " + condValueEntry.get("productCategoryId");
                    productPriceRulesCondValues.add(option);
                }
            }
        }
        if ("PRIP_PROD_FEAT_ID".equals(context.get("inputParamEnumId"))) {
            try {
                condValues = EntityQuery.use(delegator)
                        .from("ProductFeatureType")
                        .queryList();
            } catch (Exception e) {
                Debug.logError(e, "Error querying ProductFeatureType: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
            if (condValues != null) {
                for (GenericValue condValueEntry : condValues) {
                    option = "" + condValueEntry.get("description") + ": " + condValueEntry.get("productFeatureTypeId");
                    productPriceRulesCondValues.add(option);
                }
            }
        }
        if (("PRIP_PARTY_ID".equals(context.get("inputParamEnumId")) || "PRIP_PARTY_GRP_MEM".equals(context.get("inputParamEnumId")))) {
            try {
                condValues = EntityQuery.use(delegator)
                        .from("PartyNameView")
                        .queryList();
            } catch (Exception e) {
                Debug.logError(e, "Error querying PartyNameView: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
            if (condValues != null) {
                for (GenericValue condValueEntry : condValues) {
                    option = "" + condValueEntry.get("firstName") + " " + condValueEntry.get("lastName") + condValueEntry.get("groupName") + ": " + condValueEntry.get("partyId");
                    productPriceRulesCondValues.add(option);
                }
            }
        }
        if ("PRIP_PARTY_CLASS".equals(context.get("inputParamEnumId"))) {
            try {
                condValues = EntityQuery.use(delegator)
                        .from("PartyClassificationGroup")
                        .queryList();
            } catch (Exception e) {
                Debug.logError(e, "Error querying PartyClassificationGroup: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
            if (condValues != null) {
                for (GenericValue condValueEntry : condValues) {
                    option = "" + condValueEntry.get("description") + ": " + condValueEntry.get("partyClassificationGroupId");
                    productPriceRulesCondValues.add(option);
                }
            }
        }
        if ("PRIP_ROLE_TYPE".equals(context.get("inputParamEnumId"))) {
            try {
                condValues = EntityQuery.use(delegator)
                        .from("RoleType")
                        .queryList();
            } catch (Exception e) {
                Debug.logError(e, "Error querying RoleType: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
            if (condValues != null) {
                for (GenericValue condValueEntry : condValues) {
                    option = "" + condValueEntry.get("description") + ": " + condValueEntry.get("roleTypeId");
                    productPriceRulesCondValues.add(option);
                }
            }
        }
        if ("PRIP_WEBSITE_ID".equals(context.get("inputParamEnumId"))) {
            try {
                condValues = EntityQuery.use(delegator)
                        .from("WebSite")
                        .queryList();
            } catch (Exception e) {
                Debug.logError(e, "Error querying WebSite: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
            if (condValues != null) {
                for (GenericValue condValueEntry : condValues) {
                    option = "" + condValueEntry.get("siteName") + ": " + condValueEntry.get("webSiteId");
                    productPriceRulesCondValues.add(option);
                }
            }
        }
        if ("PRIP_PROD_SGRP_ID".equals(context.get("inputParamEnumId"))) {
            try {
                condValues = EntityQuery.use(delegator)
                        .from("ProductStoreGroup")
                        .queryList();
            } catch (Exception e) {
                Debug.logError(e, "Error querying ProductStoreGroup: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
            if (condValues != null) {
                for (GenericValue condValueEntry : condValues) {
                    option = "" + condValueEntry.get("productStoreGroupName") + " (" + condValueEntry.get("description") + "): " + condValueEntry.get("productStoreGroupId");
                    productPriceRulesCondValues.add(option);
                }
            }
        }
        if ("PRIP_PROD_CLG_ID".equals(context.get("inputParamEnumId"))) {
            try {
                condValues = EntityQuery.use(delegator)
                        .from("ProdCatalog")
                        .queryList();
            } catch (Exception e) {
                Debug.logError(e, "Error querying ProdCatalog: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
            if (condValues != null) {
                for (GenericValue condValueEntry : condValues) {
                    option = "" + condValueEntry.get("catalogName") + ": " + condValueEntry.get("prodCatalogId");
                    productPriceRulesCondValues.add(option);
                }
            }
        }
        if ("PRIP_CURRENCY_UOMID".equals(context.get("inputParamEnumId"))) {
            try {
                condValues = EntityQuery.use(delegator)
                        .from("Uom")
                        .queryList();
            } catch (Exception e) {
                Debug.logError(e, "Error querying Uom: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
            if (condValues != null) {
                for (GenericValue condValueEntry : condValues) {
                    option = "" + condValueEntry.get("description") + ": " + condValueEntry.get("uomId");
                    productPriceRulesCondValues.add(option);
                }
            }
        }
        if (UtilValidate.isEmpty(productPriceRulesCondValues)) {
            noOptions = UtilProperties.getMessage("CommonUiLabels", "CommonNoOptions", locale);
            productPriceRulesCondValues.add(noOptions);
        }
        result.put("productPriceRulesCondValues", productPriceRulesCondValues);

        return result;
    }

}
