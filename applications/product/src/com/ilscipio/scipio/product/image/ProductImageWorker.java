/*******************************************************************************
 * Licensed to the Apache Software Foundation (ASF) under one
 * or more contributor license agreements.  See the NOTICE file
 * distributed with this work for additional information
 * regarding copyright ownership.  The ASF licenses this file
 * to you under the Apache License, Version 2.0 (the
 * "License"); you may not use this file except in compliance
 * with the License.  You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 * KIND, either express or implied.  See the License for the
 * specific language governing permissions and limitations
 * under the License.
 *******************************************************************************/
package com.ilscipio.scipio.product.image;

import java.io.IOException;
import java.sql.Timestamp;
import java.util.Collection;
import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;

import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.GeneralException;
import org.ofbiz.base.util.UtilMisc;
import org.ofbiz.base.util.UtilProperties;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.base.util.cache.UtilCache;
import org.ofbiz.common.image.ImageProfile;
import org.ofbiz.common.image.ImageVariantConfig;
import com.ilscipio.scipio.content.image.ContentImageWorker;
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.GenericEntityException;
import org.ofbiz.entity.GenericValue;
import org.ofbiz.entity.condition.EntityCondition;
import org.ofbiz.entity.condition.EntityOperator;
import org.ofbiz.entity.model.ModelEntity;
import org.ofbiz.entity.model.ModelUtil;
import org.ofbiz.service.DispatchContext;
import org.ofbiz.service.ServiceUtil;

/**
 * SCIPIO: Image utilities for product (specifically) image handling.
 * Added 2017-07-04.
 *
 * @see com.ilscipio.scipio.content.image.ContentImageWorker
 * @see org.ofbiz.product.image.ScaleImage
 */
public abstract class ProductImageWorker {
    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());
    public static final String PRODUCT_IMAGEPROP_FILEPATH = "/applications/product/config/ImageProperties.xml";
    private static final UtilCache<String, Boolean> productImageEnsureCache = UtilCache.createUtilCache("product.image.ensure", true);
    private static final boolean productImageEnsureEnabled = UtilProperties.getPropertyAsBoolean("cache", "product.image.ensure.enable", false);

    protected ProductImageWorker() {
    }

    /**
     * SCIPIO: Returns the full path to the ImageProperties.xml file to use for product image size definitions.
     * Uses the one from product component if available; otherwise falls back on the generic one under content component.
     * Added 2017-07-04.
     */
    public static String getProductImagePropertiesFullPath() throws IOException {
        String path = ImageVariantConfig.getImagePropertiesFullPath(PRODUCT_IMAGEPROP_FILEPATH);
        if (new java.io.File(path).exists()) {
            return path;
        } else {
            return ContentImageWorker.getContentImagePropertiesFullPath();
        }
    }

    public static String getProductImagePropertiesPath() throws IOException {
        String path = ImageVariantConfig.getImagePropertiesFullPath(PRODUCT_IMAGEPROP_FILEPATH);
        if (new java.io.File(path).exists()) {
            return PRODUCT_IMAGEPROP_FILEPATH;
        } else {
            return ContentImageWorker.getContentImagePropertiesPath();
        }
    }

    public static void ensureProductImage(DispatchContext dctx, Locale locale, GenericValue product, String productContentTypeId, String imageUrl, boolean async, boolean useUtilCache) {
        if (!productImageEnsureEnabled) {
            return;
        }
        String cacheKey = null;
        if (useUtilCache) {
            cacheKey = product.get("productId") + "::" + productContentTypeId;
            if (Boolean.TRUE.equals(productImageEnsureCache.get(cacheKey))) {
                return;
            }
        }
        try {
            ProductImageViewType imageViewType = ProductImageViewType.from(dctx.getDelegator(), productContentTypeId, true, true)
                    .getOriginal(true);
            ProductImageLocationInfo pili = ProductImageLocationInfo.from(dctx, locale,
                    product, imageViewType, imageUrl, null, true, false, useUtilCache, null);
            List<String> sizeTypeList = (pili != null) ? pili.getMissingVariantNames() : null;
            if (UtilValidate.isEmpty(sizeTypeList)) {
                if (useUtilCache) {
                    productImageEnsureCache.put(cacheKey, Boolean.TRUE);
                }
                return;
            }
            Map<String, Object> ctx = UtilMisc.toMap("productId", product.get("productId"), "productContentTypeId", productContentTypeId,
                    "sizeTypeList", sizeTypeList, "recreateExisting", true, "clearCaches", true); // NOTE: the getMissingVariantNames check above (for performance) already implements the recreateExisting logic
            if (async) {
                dctx.getDispatcher().runAsync("productImageAutoRescale", ctx, false);
            } else {
                Map<String, Object> servResult = dctx.getDispatcher().runSync("productImageAutoRescale", ctx);
                if (ServiceUtil.isError(servResult)) {
                    Debug.logError("Could not trigger image variant resizing for product [" + product.get("productId") + "] productContentTypeId ["
                            + productContentTypeId + "] imageLink [" + imageUrl + "]: " + ServiceUtil.getErrorMessage(servResult), module);
                    return;
                }
            }
            if (useUtilCache) {
                productImageEnsureCache.put(cacheKey, Boolean.TRUE);
            }
        } catch(Exception e) {
            Debug.logError("Could not trigger image variant resizing for product [" + product.get("productId") + "] productContentTypeId ["
                    + productContentTypeId + "] imageLink [" + imageUrl + "]: " + e.toString(), module);
        }
    }

    public static List<GenericValue> getVariantProductContentDataResourceRecords(Delegator delegator, String productId,
                                                                                 ProductImageViewType originalImageViewType, Timestamp moment,
                                                                                 boolean includeOriginal, boolean useCache) throws GeneralException, IllegalArgumentException {
        if (!originalImageViewType.isOriginal()) {
            throw new IllegalArgumentException("originalImageViewType not an original viewType: " + originalImageViewType);
        }
        List<String> pctIdList = originalImageViewType.getProductContentTypeIds(includeOriginal, useCache);
        if (pctIdList.isEmpty()) {
            return Collections.emptyList();
        }
        return delegator.from("ProductContentAndDataResource")
                .where(EntityCondition.makeCondition("productId", productId),
                        EntityCondition.makeCondition("productContentTypeId", EntityOperator.IN, pctIdList))
                .orderBy("-fromDate").filterByDate(moment).cache(useCache).queryList();
    }

    public static Map<ProductImageViewType, GenericValue> getVariantProductContentDataResourceRecordsByViewType(Delegator delegator, String productId,
                                                                                                                ProductImageViewType originalImageViewType, Timestamp moment,
                                                                                                                boolean includeOriginal, boolean useCache) throws GeneralException, IllegalArgumentException {
        if (!originalImageViewType.isOriginal()) {
            throw new IllegalArgumentException("originalImageViewType not an original viewType: " + originalImageViewType);
        }
        Map<String, GenericValue> pctIdMap = originalImageViewType.getProductContentTypesById(includeOriginal, useCache);
        if (pctIdMap.isEmpty()) {
            return Collections.emptyMap();
        }
        List<GenericValue> pcdrList = delegator.from("ProductContentAndDataResource")
                .where(EntityCondition.makeCondition("productId", productId),
                        EntityCondition.makeCondition("productContentTypeId", EntityOperator.IN, pctIdMap.keySet()))
                .orderBy("-fromDate").filterByDate(moment).cache(useCache).queryList();
        if (pcdrList.isEmpty()) {
            return Collections.emptyMap();
        }
        Map<ProductImageViewType, GenericValue> viewTypeMap = new LinkedHashMap<>();
        for(GenericValue pcdr : pcdrList) {
            String productContentTypeId = pcdr.getString("productContentTypeId");
            viewTypeMap.put(ProductImageViewType.from(pctIdMap.get(productContentTypeId), false, useCache), pcdr);
        }
        return viewTypeMap;
    }

    public static Map<String, GenericValue> getVariantProductContentDataResourceRecordsByViewSize(Delegator delegator, String productId,
                                                                                                  ProductImageViewType originalImageViewType, Timestamp moment,
                                                                                                  boolean includeOriginal, boolean useCache) throws GeneralException, IllegalArgumentException {
        if (!originalImageViewType.isOriginal()) {
            throw new IllegalArgumentException("originalImageViewType not an original viewType: " + originalImageViewType);
        }
        Map<String, GenericValue> pctIdMap = originalImageViewType.getProductContentTypesById(includeOriginal, useCache);
        if (pctIdMap.isEmpty()) {
            return Collections.emptyMap();
        }
        List<GenericValue> pcdrList = delegator.from("ProductContentAndDataResource")
                .where(EntityCondition.makeCondition("productId", productId),
                        EntityCondition.makeCondition("productContentTypeId", EntityOperator.IN, pctIdMap.keySet()))
                .orderBy("-fromDate").filterByDate(moment).cache(useCache).queryList();
        if (pcdrList.isEmpty()) {
            return Collections.emptyMap();
        }
        Map<String, GenericValue> viewTypeMap = new LinkedHashMap<>();
        for(GenericValue pcdr : pcdrList) {
            String productContentTypeId = pcdr.getString("productContentTypeId");
            ProductImageViewType imageViewType = ProductImageViewType.from(pctIdMap.get(productContentTypeId), false, useCache);
            viewTypeMap.put(imageViewType.getViewSize(), pcdr);
        }
        return viewTypeMap;
    }

    public static String getDataResourceImageUrl(GenericValue dataResource, boolean useEntityCache) throws GenericEntityException { // SCIPIO
        if (dataResource == null) {
            return null;
        }
        String dataResourceTypeId = dataResource.hasModelField("drDataResourceTypeId") ?
                dataResource.getString("drDataResourceTypeId") : dataResource.getString("dataResourceTypeId");
        if ("SHORT_TEXT".equals(dataResourceTypeId)) {
            return dataResource.hasModelField("drObjectInfo") ?
                    dataResource.getString("drObjectInfo") : dataResource.getString("objectInfo");
        } else if ("ELECTRONIC_TEXT".equals(dataResourceTypeId)) {
            String dataResourceId = dataResource.hasModelField("drDataResourceId") ?
                    dataResource.getString("drDataResourceId") : dataResource.getString("dataResourceId");
            GenericValue elecText = dataResource.getDelegator().findOne("ElectronicText", UtilMisc.toMap("dataResourceId", dataResourceId), useEntityCache);
            if (elecText != null) {
                return elecText.getString("textData");
            }
        }
        return null;
    }

    public static String getProductInlineImageFieldPrefix(Delegator delegator, String productContentTypeId) {
        String candidateFieldName = ModelUtil.dbNameToVarName(productContentTypeId);
        ModelEntity productModel = delegator.getModelEntity("Product");
        if (productModel.isField(candidateFieldName) && candidateFieldName.endsWith("Url")) {
            return candidateFieldName.substring(0, candidateFieldName.length() - "Url".length());
        }
        return null;
    }

    public static String getDefaultProductImageProfileName(Delegator delegator, String productContentTypeId) {
        return "IMAGE_PRODUCT-" + productContentTypeId;
    }

    /**
     * Gets image profile from mediaprofiles.properties.
     * NOTE: productContentTypeId should be ORIGINAL_IMAGE_URL or ADDITIONAL_IMAGE_x, not the size variants' IDs (LARGE_IMAGE_URL, ...)
     */
    public static ImageProfile getProductImageProfileOrDefault(Delegator delegator, String productContentTypeId, GenericValue product, GenericValue content, boolean useEntityCache, boolean useProfileCache) {
        String profileName;
        ImageProfile profile;
        if (content != null) {
            profileName = content.getString("mediaProfile");
            if (profileName != null) {
                profile = ImageProfile.getImageProfile(delegator, profileName, useProfileCache);
                if (profile != null) {
                    return profile;
                } else {
                    // Explicitly named missing profile is always an error
                    Debug.logError("Could not find image profile [" + profileName + "] in mediaprofiles.properties from " +
                            "Content.mediaProfile for content [" + content.get("contentId") + "] productContentTypeId [" + productContentTypeId +
                            "] product [" + product.get("productId") + "]", module);
                    return null;
                }
            }
        } else if (product != null) {
            profileName = product.getString("imageProfile");
            if (profileName != null) {
                profile = ImageProfile.getImageProfile(delegator, profileName, useProfileCache);
                if (profile != null) {
                    return profile;
                } else {
                    // Explicitly named missing profile is always an error
                    Debug.logError("Could not find image profile [" + profileName + "] in mediaprofiles.properties from " +
                            "Product.imageProfile for product [" + product.get("productId") + "] productContentTypeId [" + productContentTypeId + "]", module);
                    return null;
                }
            }
        }
        profileName = getDefaultProductImageProfileName(delegator, productContentTypeId);
        profile = ImageProfile.getImageProfile(delegator, profileName, useProfileCache);
        if (profile != null) {
            return profile;
        } else {
            // Clients may add extra productContentTypeId and they should add mediaprofiles.properties definitions
            Debug.logWarning("Could not find default image profile [" + profileName + "] in mediaprofiles.properties from " +
                    "productContentTypeId [" + productContentTypeId + "]; using IMAGE_PRODUCT", module);
        }
        profile = ImageProfile.getImageProfile(delegator, "IMAGE_PRODUCT", useProfileCache);
        if (profile != null) {
            return profile;
        } else {
            // Should not happen
            Debug.logError("Could not find image profile IMAGE_PRODUCT in mediaprofiles.properties; fatal error", module);
            return null;
        }
    }

    public static ImageProfile getDefaultProductImageProfile(Delegator delegator, String productContentTypeId, boolean useEntityCache, boolean useProfileCache) {
        return getProductImageProfileOrDefault(delegator, productContentTypeId, null, null, useEntityCache, useProfileCache);
    }
}
