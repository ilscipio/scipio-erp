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
import org.ofbiz.entity.util.EntityQuery;
import org.ofbiz.entity.util.EntityUtil;
import org.ofbiz.security.Security;
import org.ofbiz.service.DispatchContext;
import org.ofbiz.service.LocalDispatcher;
import org.ofbiz.service.ServiceUtil;

/**
 * Auto-generated event class from Simple Method XML.
 *
 * <p>Generated from: component://product/script/org/ofbiz/product/rental/RentalServices.xml</p>
 *
 * <p>SCIPIO: 4.0.0: Auto-generated.</p>
 */
public class RentalServices {

    private static final String MODULE = RentalServices.class.getName();


    /**
     * Create an FixedAsset and link the asset to the product, used when a asset usage product is created
     */
    public static Map<String, Object> createFixedAssetAndLinkToProduct(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        Security security = dctx.getSecurity();
        List<String> error_list = new LinkedList<>();

        Map<String, Object> createFixedAsset = null;
        Object autoCreate = UtilProperties.getMessage("AccountingConfig", "accounting.fixedasset.autocreate", locale);
        if (!"Y".equals(autoCreate)) {
            return result;
        }
        if (!security.hasEntityPermission("CATALOG", "_UPDATE", userLogin)) {
            return ServiceUtil.returnError(UtilProperties.getMessage("ProductUiLabels", "ProductCatalogUpdatePermissionError", locale));
        }
        if (!error_list.isEmpty()) {
            return ServiceUtil.returnError(error_list);
        }
        createFixedAsset.put("fixedAssetTypeId", "PROPERTY");
        if (UtilValidate.isNotEmpty(context.get("productName"))) {
            createFixedAsset.put("fixedAssetName", context.get("productName"));
        }
        if (UtilValidate.isNotEmpty(context.get("internalName"))) {
            createFixedAsset.put("fixedAssetName", context.get("internalName"));
        }
        createFixedAsset.put("instanceOfProductId", context.get("productId"));
        Map<String, Object> newLink = new HashMap<String, Object>();
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("createFixedAsset", createFixedAsset);
            if (ServiceUtil.isError(serviceResult)) {
                return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
            }
            newLink.put("fixedAssetId", serviceResult.get("fixedAssetId"));
        } catch (Exception e) {
            Debug.logError(e, "Error calling createFixedAsset: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        newLink.put("fixedAssetProductTypeId", "FAPT_USE");
        newLink.put("productId", context.get("productId"));
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("addFixedAssetProduct", newLink);
            if (ServiceUtil.isError(serviceResult)) {
                return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
            }
        } catch (Exception e) {
            Debug.logError(e, "Error calling addFixedAssetProduct: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }

        return result;
    }


    /**
     * Most rental products are associated with one fixed asset only,                                  this service will return the first genericValue fixedAsset
     */
    public static Map<String, Object> getProductFirstRelatedFixedAsset(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        Security security = dctx.getSecurity();
        List<String> error_list = new LinkedList<>();

        GenericValue productFixedAsset = null;
        if (!security.hasEntityPermission("CATALOG", "_VIEW", userLogin)) {
            return ServiceUtil.returnError(UtilProperties.getMessage("ProductUiLabels", "ProductCatalogViewPermissionError", locale));
        }
        if (!error_list.isEmpty()) {
            return ServiceUtil.returnError(error_list);
        }
        GenericValue product = null;
        try {
            product = EntityQuery.use(delegator)
                    .from("Product")
                    .where(context)
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying Product: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        List<GenericValue> productFixedAssets = null;
        try {
            productFixedAssets = product.getRelated("FixedAssetProduct", null, null, false);
        } catch (Exception e) {
            Debug.logError(e, "Error getting related FixedAssetProduct: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        if (UtilValidate.isNotEmpty(productFixedAssets)) {
            productFixedAsset = EntityUtil.getFirst((List<GenericValue>) productFixedAssets);
            result.put("fixedAssetId", productFixedAsset.get("fixedAssetId"));
        }
        result.put("productId", context.get("productId"));

        return result;
    }

}
