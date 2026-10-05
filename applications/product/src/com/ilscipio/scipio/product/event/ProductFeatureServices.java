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

import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.UtilMisc;
import org.ofbiz.base.util.UtilProperties;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.GenericValue;
import org.ofbiz.entity.condition.EntityCondition;
import org.ofbiz.entity.util.EntityQuery;
import org.ofbiz.entity.util.EntityUtil;
import org.ofbiz.security.Security;
import org.ofbiz.service.DispatchContext;
import org.ofbiz.service.LocalDispatcher;
import org.ofbiz.service.ServiceUtil;

/**
 * Auto-generated event class from Simple Method XML.
 *
 * <p>Generated from: component://product/script/org/ofbiz/product/feature/ProductFeatureServices.xml</p>
 *
 * <p>SCIPIO: 4.0.0: Auto-generated.</p>
 */
public class ProductFeatureServices {

    private static final String MODULE = ProductFeatureServices.class.getName();


    /**
     * Apply Feature to Product using Feature Type and ID Code
     */
    public static Map<String, Object> applyFeatureToProductFromTypeAndCode(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        List<String> error_list = new LinkedList<>();

        Map<String, Object> applyFeatureContext = null;
        Object callingMethodName = "applyFeatureToProductFromTypeAndCode";
        Object checkAction = "CREATE";
        // TODO: Call simple-method "checkProductRelatedPermission" from "component://product/script/org/ofbiz/product/product/ProductServices.xml"
        if (!error_list.isEmpty()) {
            return ServiceUtil.returnError(error_list);
        }
        List<GenericValue> productFeatures = null;
        try {
            productFeatures = EntityQuery.use(delegator)
                    .from("ProductFeature")
                    .where(UtilMisc.toMap("productFeatureTypeId", context.get("productFeatureTypeId"), "idCode", context.get("idCode")))
                    .queryList();
        } catch (Exception e) {
            Debug.logError(e, "Error querying entity: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        if (UtilValidate.isEmpty(productFeatures)) {
            {
                String errorMsg = UtilProperties.getMessage("ProductUiLabels", "ProductFeatureTypeAndIdCodeNotFound", locale);
                error_list.add(errorMsg);
            }
        }
        if (!error_list.isEmpty()) {
            return ServiceUtil.returnError(error_list);
        }
        if (productFeatures != null) {
            for (GenericValue productFeature : productFeatures) {
                // set-service-fields from "parameters" to "applyFeatureContext" for service "applyFeatureToProduct"
                applyFeatureContext.putAll(UtilMisc.toMap(context));
                applyFeatureContext.put("productFeatureId", productFeature.get("productFeatureId"));
                if (UtilValidate.isEmpty(((Map<String, Object>) applyFeatureContext).get("sequenceNum"))) {
                    applyFeatureContext.put("sequenceNum", productFeature.get("defaultSequenceNum"));
                }
                try {
                    Map<String, Object> serviceResult = dispatcher.runSync("applyFeatureToProduct", applyFeatureContext);
                    if (ServiceUtil.isError(serviceResult)) {
                        return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
                    }
                } catch (Exception e) {
                    Debug.logError(e, "Error calling applyFeatureToProduct: " + e.getMessage(), MODULE);
                    return ServiceUtil.returnError(e.getMessage());
                }
            }
        }

        return result;
    }


    /**
     * Create a Product Feature Type
     */
    public static Map<String, Object> createProductFeatureType(DispatchContext dctx, Map<String, Object> context) {
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
        if (!error_list.isEmpty()) {
            return ServiceUtil.returnError(error_list);
        }
        if (UtilValidate.isEmpty(context.get("productFeatureTypeId"))) {
            ((GenericValue) context).put("productFeatureTypeId", delegator.getNextSeqId("ProductFeatureType"));
        }
        if (!error_list.isEmpty()) {
            return ServiceUtil.returnError(error_list);
        }
        if (!(context.get("productFeatureTypeId") != null && ((String) context.get("productFeatureTypeId")).matches("^[a-zA-Z_0-9]+$"))) {
            {
                String errorMsg = UtilProperties.getMessage("ProductErrorUiLabels", "ProductFeatureTypeIdMustContainsLettersAndDigits", locale);
                error_list.add(errorMsg);
            }
        }
        if (!error_list.isEmpty()) {
            return ServiceUtil.returnError(error_list);
        }
        GenericValue newEntity = delegator.makeValue("ProductFeatureType");
        newEntity.setNonPKFields(context);
        newEntity.setPKFields(context);
        try {
            delegator.create(newEntity);
        } catch (Exception e) {
            Debug.logError(e, "Error creating value: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        result.put("productFeatureTypeId", newEntity.get("productFeatureTypeId"));

        return result;
    }


    /**
     * Create a ProductFeatureApplAttr
     */
    public static Map<String, Object> createProductFeatureApplAttr(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        Security security = dctx.getSecurity();
        List<String> error_list = new LinkedList<>();

        List<GenericValue> productFeatureAppls = null;
        GenericValue newEntity = null;
        GenericValue productFeatureAppl = null;
        if (!security.hasEntityPermission("CATALOG", "_CREATE", userLogin)) {
            return ServiceUtil.returnError(UtilProperties.getMessage("ProductUiLabels", "ProductCatalogCreatePermissionError", locale));
        }
        if (!error_list.isEmpty()) {
            return ServiceUtil.returnError(error_list);
        }
        newEntity = delegator.makeValue("ProductFeatureApplAttr");
        newEntity.setPKFields(context);
        newEntity.setNonPKFields(context);
        if (UtilValidate.isEmpty(newEntity.get("fromDate"))) {
            try {
                productFeatureAppls = EntityQuery.use(delegator)
                        .from("ProductFeatureAppl")
                        .filterByDate()
                        .queryList();
            } catch (Exception e) {
                Debug.logError(e, "Error querying ProductFeatureAppl: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
            productFeatureAppl = EntityUtil.getFirst((List<GenericValue>) productFeatureAppls);
            newEntity.put("fromDate", productFeatureAppl.get("fromDate"));
        }
        try {
            delegator.create(newEntity);
        } catch (Exception e) {
            Debug.logError(e, "Error creating value: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        if (!error_list.isEmpty()) {
            return ServiceUtil.returnError(error_list);
        }

        return result;
    }

}
