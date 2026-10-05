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
import java.sql.Timestamp;
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
import org.ofbiz.service.DispatchContext;
import org.ofbiz.service.LocalDispatcher;
import org.ofbiz.service.ServiceUtil;

/**
 * Auto-generated event class from Simple Method XML.
 *
 * <p>Generated from: component://product/script/org/ofbiz/product/product/CustomerDigitalDownloadServices.xml</p>
 *
 * <p>SCIPIO: 4.0.0: Auto-generated.</p>
 */
public class CustomerDigitalDownloadServices {

    private static final String MODULE = CustomerDigitalDownloadServices.class.getName();


    /**
     * createCustomerDigitalDownloadProduct
     */
    public static Map<String, Object> createCustomerDigitalDownloadProduct(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        GenericValue addProductToCategoryMap = null;
        Timestamp nowTimestamp = new Timestamp(System.currentTimeMillis());
        GenericValue createProductMap = delegator.makeValue("Product");
        ((GenericValue) createProductMap).put("productId", delegator.getNextSeqId("Product"));
        Object productId = createProductMap.get("productId");
        createProductMap.put("productName", context.get("productName"));
        createProductMap.put("internalName", context.get("productName"));
        createProductMap.put("description", context.get("description"));
        createProductMap.put("productTypeId", "DIGITAL_GOOD");
        try {
            delegator.create(createProductMap);
        } catch (Exception e) {
            Debug.logError(e, "Error creating value: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        GenericValue createProductPriceMap = delegator.makeValue("ProductPrice");
        createProductPriceMap.put("productId", productId);
        createProductPriceMap.put("productPriceTypeId", "DEFAULT_PRICE");
        createProductPriceMap.put("productPricePurposeId", "PURCHASE");
        createProductPriceMap.put("currencyUomId", context.get("currencyUomId"));
        createProductPriceMap.put("productStoreGroupId", "_NA_");
        createProductPriceMap.put("fromDate", nowTimestamp);
        createProductPriceMap.put("price", context.get("price"));
        try {
            delegator.create(createProductPriceMap);
        } catch (Exception e) {
            Debug.logError(e, "Error creating value: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        GenericValue createProductSupplierMap = delegator.makeValue("SupplierProduct");
        createProductSupplierMap.put("productId", productId);
        createProductSupplierMap.put("partyId", userLogin.get("partyId"));
        createProductSupplierMap.put("currencyUomId", context.get("currencyUomId"));
        createProductSupplierMap.put("minimumOrderQuantity", BigDecimal.ONE);
        createProductSupplierMap.put("availableFromDate", nowTimestamp);
        try {
            delegator.create(createProductSupplierMap);
        } catch (Exception e) {
            Debug.logError(e, "Error creating value: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        result.put("productId", productId);
        result.put("currencyUomId", createProductSupplierMap.get("currencyUomId"));
        result.put("minimumOrderQuantity", createProductSupplierMap.get("minimumOrderQuantity"));
        result.put("availableFromDate", createProductSupplierMap.get("availableFromDate"));
        GenericValue productStore = null;
        try {
            productStore = EntityQuery.use(delegator)
                    .from("ProductStore")
                    .where(context)
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying ProductStore: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        if (UtilValidate.isNotEmpty(productStore.get("digProdUploadCategoryId"))) {
            addProductToCategoryMap = delegator.makeValue("ProductCategoryMember");
            addProductToCategoryMap.put("productId", productId);
            addProductToCategoryMap.put("productCategoryId", productStore.get("digProdUploadCategoryId"));
            try {
                delegator.create(addProductToCategoryMap);
            } catch (Exception e) {
                Debug.logError(e, "Error creating value: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
        }

        return result;
    }


    /**
     * updateCustomerDigitalDownloadProduct
     */
    public static Map<String, Object> updateCustomerDigitalDownloadProduct(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        List<String> error_list = new LinkedList<>();

        GenericValue product = null;
        List<GenericValue> productPriceList = null;
        GenericValue productPrice = null;
        Timestamp nowTimestamp = new Timestamp(System.currentTimeMillis());
        context.put("partyId", userLogin.get("partyId"));
        GenericValue supplierProduct = null;
        try {
            supplierProduct = EntityQuery.use(delegator)
                    .from("SupplierProduct")
                    .where(context)
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying SupplierProduct: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        if (UtilValidate.isEmpty(supplierProduct)) {
            {
                String errorMsg = UtilProperties.getMessage("ProductUiLabels", "ProductMustBeAssociatedWithSupplier", locale);
                error_list.add(errorMsg);
            }
        }
        if (!error_list.isEmpty()) {
            return ServiceUtil.returnError(error_list);
        }
        Object product_productName = null;
        Object product_description = null;
        if ((!(UtilValidate.isEmpty(context.get("productName"))) || !(UtilValidate.isEmpty(context.get("description"))))) {
            try {
                product = EntityQuery.use(delegator)
                        .from("Product")
                        .where(context)
                        .queryOne();
            } catch (Exception e) {
                Debug.logError(e, "Error querying Product: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
            product.put("productName", context.get("productName"));
            product.put("description", context.get("description"));
            try {
                delegator.store(product);
            } catch (Exception e) {
                Debug.logError(e, "Error storing value: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
        }
        if (UtilValidate.isNotEmpty(context.get("price"))) {
            try {
                productPriceList = EntityQuery.use(delegator)
                        .from("ProductPrice")
                        .where(UtilMisc.toMap("productId", context.get("productId"), "productPriceTypeId", "DEFAULT_PRICE", "productPricePurposeId", "PURCHASE", "productStoreGroupId", "_NA_"))
                        .queryList();
            } catch (Exception e) {
                Debug.logError(e, "Error querying entity: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
            productPrice = EntityUtil.getFirst((List<GenericValue>) productPriceList);
            productPrice.put("price", context.get("price"));
            try {
                delegator.store(productPrice);
            } catch (Exception e) {
                Debug.logError(e, "Error storing value: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
        }

        return result;
    }


    /**
     * deleteCustomerDigitalDownloadProduct
     */
    public static Map<String, Object> deleteCustomerDigitalDownloadProduct(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        List<String> error_list = new LinkedList<>();

        Timestamp nowTimestamp = new Timestamp(System.currentTimeMillis());
        List<GenericValue> supplierProductList = null;
        try {
            supplierProductList = EntityQuery.use(delegator)
                    .from("SupplierProduct")
                    .where(UtilMisc.toMap("productId", context.get("productId"), "partyId", userLogin.get("partyId")))
                    .queryList();
        } catch (Exception e) {
            Debug.logError(e, "Error querying entity: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        if (UtilValidate.isEmpty(supplierProductList)) {
            {
                String errorMsg = UtilProperties.getMessage("ProductUiLabels", "ProductMustBeAssociatedWithSupplier", locale);
                error_list.add(errorMsg);
            }
        }
        if (!error_list.isEmpty()) {
            return ServiceUtil.returnError(error_list);
        }
        if (supplierProductList != null) {
            for (GenericValue supplierProduct : supplierProductList) {
                supplierProduct.put("availableThruDate", nowTimestamp);
                try {
                    delegator.store(supplierProduct);
                } catch (Exception e) {
                    Debug.logError(e, "Error storing value: " + e.getMessage(), MODULE);
                    return ServiceUtil.returnError(e.getMessage());
                }
            }
        }
        List<GenericValue> productCategoryMemberList = null;
        try {
            productCategoryMemberList = EntityQuery.use(delegator)
                    .from("ProductCategoryMember")
                    .where(UtilMisc.toMap("productId", context.get("productId")))
                    .queryList();
        } catch (Exception e) {
            Debug.logError(e, "Error querying entity: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        if (productCategoryMemberList != null) {
            for (GenericValue productCategoryMember : productCategoryMemberList) {
                productCategoryMember.put("thruDate", nowTimestamp);
                try {
                    delegator.store(productCategoryMember);
                } catch (Exception e) {
                    Debug.logError(e, "Error storing value: " + e.getMessage(), MODULE);
                    return ServiceUtil.returnError(e.getMessage());
                }
            }
        }

        return result;
    }


    /**
     * addCustomerDigitalDownloadProductFile
     */
    public static Map<String, Object> addCustomerDigitalDownloadProductFile(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        List<String> error_list = new LinkedList<>();

        Timestamp nowTimestamp = new Timestamp(System.currentTimeMillis());
        List<GenericValue> supplierProductList = null;
        try {
            supplierProductList = EntityQuery.use(delegator)
                    .from("SupplierProduct")
                    .where(UtilMisc.toMap("productId", context.get("productId"), "partyId", userLogin.get("partyId")))
                    .queryList();
        } catch (Exception e) {
            Debug.logError(e, "Error querying entity: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        if (UtilValidate.isEmpty(supplierProductList)) {
            {
                String errorMsg = UtilProperties.getMessage("ProductUiLabels", "ProductMustBeAssociatedWithSupplier", locale);
                error_list.add(errorMsg);
            }
        }
        List<GenericValue> contentRoleList = null;
        try {
            contentRoleList = EntityQuery.use(delegator)
                    .from("ContentRole")
                    .where(UtilMisc.toMap("partyId", userLogin.get("partyId"), "contentId", context.get("contentId"), "roleTypeId", "OWNER"))
                    .queryList();
        } catch (Exception e) {
            Debug.logError(e, "Error querying entity: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        if (UtilValidate.isEmpty(contentRoleList)) {
            {
                String errorMsg = UtilProperties.getMessage("ProductUiLabels", "ProductCannotAssociatedToContent", locale);
                error_list.add(errorMsg);
            }
        }
        if (!error_list.isEmpty()) {
            return ServiceUtil.returnError(error_list);
        }
        GenericValue productContent = delegator.makeValue("ProductContent");
        productContent.put("productId", context.get("productId"));
        productContent.put("contentId", context.get("contentId"));
        productContent.put("productContentTypeId", "DIGITAL_DOWNLOAD");
        productContent.put("fromDate", nowTimestamp);
        try {
            delegator.create(productContent);
        } catch (Exception e) {
            Debug.logError(e, "Error creating value: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }

        return result;
    }


    /**
     * removeCustomerDigitalDownloadProductFile
     */
    public static Map<String, Object> removeCustomerDigitalDownloadProductFile(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        List<String> error_list = new LinkedList<>();

        Timestamp nowTimestamp = new Timestamp(System.currentTimeMillis());
        List<GenericValue> supplierProductList = null;
        try {
            supplierProductList = EntityQuery.use(delegator)
                    .from("SupplierProduct")
                    .where(UtilMisc.toMap("productId", context.get("productId"), "partyId", userLogin.get("partyId")))
                    .queryList();
        } catch (Exception e) {
            Debug.logError(e, "Error querying entity: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        if (UtilValidate.isEmpty(supplierProductList)) {
            {
                String errorMsg = UtilProperties.getMessage("ProductUiLabels", "ProductMustBeAssociatedWithSupplier", locale);
                error_list.add(errorMsg);
            }
        }
        if (!error_list.isEmpty()) {
            return ServiceUtil.returnError(error_list);
        }
        GenericValue productContent = null;
        try {
            productContent = EntityQuery.use(delegator)
                    .from("ProductContent")
                    .where(context)
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying ProductContent: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        productContent.put("thruDate", nowTimestamp);
        try {
            delegator.store(productContent);
        } catch (Exception e) {
            Debug.logError(e, "Error storing value: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }

        return result;
    }

}
