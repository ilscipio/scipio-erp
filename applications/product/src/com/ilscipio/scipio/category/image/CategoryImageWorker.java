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
package com.ilscipio.scipio.category.image;

import com.ilscipio.scipio.content.image.ContentImageWorker;
import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.GeneralException;
import org.ofbiz.base.util.UtilMisc;
import org.ofbiz.base.util.UtilProperties;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.base.util.cache.UtilCache;
import org.ofbiz.common.image.ImageProfile;
import org.ofbiz.common.image.ImageVariantConfig;
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.GenericEntityException;
import org.ofbiz.entity.GenericValue;
import org.ofbiz.entity.condition.EntityCondition;
import org.ofbiz.entity.condition.EntityOperator;
import org.ofbiz.entity.model.ModelEntity;
import org.ofbiz.entity.model.ModelUtil;
import org.ofbiz.service.DispatchContext;
import org.ofbiz.service.ServiceUtil;

import java.io.IOException;
import java.sql.Timestamp;
import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;

/**
 * SCIPIO: Image utilities for category (specifically) image handling.
 * Added 2017-07-04.
 *
 * @see ContentImageWorker
 * @see org.ofbiz.product.image.ScaleImage
 */
public abstract class CategoryImageWorker {
    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());
    public static final String CATEGORY_IMAGEPROP_FILEPATH = "/applications/product/config/ImageProperties.xml";
    private static final UtilCache<String, Boolean> categoryImageEnsureCache = UtilCache.createUtilCache("category.image.ensure", true);
    private static final boolean categoryImageEnsureEnabled = UtilProperties.getPropertyAsBoolean("cache", "category.image.ensure.enable", false);

    protected CategoryImageWorker() {
    }

    /**
     * SCIPIO: Returns the full path to the ImageProperties.xml file to use for category image size definitions.
     * Uses the one from category component if available; otherwise falls back on the generic one under content component.
     * Added 2017-07-04.
     */
    public static String getCategoryImagePropertiesFullPath() throws IOException {
        String path = ImageVariantConfig.getImagePropertiesFullPath(CATEGORY_IMAGEPROP_FILEPATH);
        if (new java.io.File(path).exists()) {
            return path;
        } else {
            return ContentImageWorker.getContentImagePropertiesFullPath();
        }
    }

    public static String getCategoryImagePropertiesPath() throws IOException {
        String path = ImageVariantConfig.getImagePropertiesFullPath(CATEGORY_IMAGEPROP_FILEPATH);
        if (new java.io.File(path).exists()) {
            return CATEGORY_IMAGEPROP_FILEPATH;
        } else {
            return ContentImageWorker.getContentImagePropertiesPath();
        }
    }

    public static void ensureCategoryImage(DispatchContext dctx, Locale locale, GenericValue productCategory, String prodCatContentTypeId, String imageUrl, boolean async, boolean useUtilCache) {
        if (!categoryImageEnsureEnabled) {
            return;
        }
        String cacheKey = null;
        if (useUtilCache) {
            cacheKey = productCategory.getDelegator().getDelegatorName() + "::" + productCategory.get("productCategoryId") + "::" + prodCatContentTypeId; // SCIPIO: 4.0.0: per store (G14); was productId (always null)
            if (Boolean.TRUE.equals(categoryImageEnsureCache.get(cacheKey))) {
                return;
            }
        }
        try {
            CategoryImageViewType imageViewType = CategoryImageViewType.from(dctx.getDelegator(), prodCatContentTypeId, true, true)
                    .getOriginal(true);
            CategoryImageLocationInfo cili = CategoryImageLocationInfo.from(dctx, locale,
                    productCategory, imageViewType, imageUrl, null, true, false, useUtilCache, null);
            List<String> sizeTypeList = (cili != null) ? cili.getMissingVariantNames() : null;
            if (UtilValidate.isEmpty(sizeTypeList)) {
                if (useUtilCache) {
                    categoryImageEnsureCache.put(cacheKey, Boolean.TRUE);
                }
                return;
            }
            Map<String, Object> ctx = UtilMisc.toMap("productCategoryId", productCategory.get("productCategoryId"), "prodCatContentTypeId", prodCatContentTypeId,
                    "sizeTypeList", sizeTypeList, "recreateExisting", true, "clearCaches", true); // NOTE: the getMissingVariantNames check above (for performance) already implements the recreateExisting logic
            if (async) {
                dctx.getDispatcher().runAsync("categoryImageAutoRescale", ctx, false);
            } else {
                Map<String, Object> servResult = dctx.getDispatcher().runSync("categoryImageAutoRescale", ctx);
                if (ServiceUtil.isError(servResult)) {
                    Debug.logError("Could not trigger image variant resizing for product [" + productCategory.get("productCategoryId") + "] prodCatContentTypeId ["
                            + prodCatContentTypeId + "] imageLink [" + imageUrl + "]: " + ServiceUtil.getErrorMessage(servResult), module);
                    return;
                }
            }
            if (useUtilCache) {
                categoryImageEnsureCache.put(cacheKey, Boolean.TRUE);
            }
        } catch(Exception e) {
            Debug.logError("Could not trigger image variant resizing for product [" + productCategory.get("productCategoryId") + "] prodCatContentTypeId ["
                    + prodCatContentTypeId + "] imageLink [" + imageUrl + "]: " + e.toString(), module);
        }
    }

    public static List<GenericValue> getVariantProductCategoryContentDataResourceRecords(Delegator delegator, String productCategoryId,
                                                                                         CategoryImageViewType originalImageViewType, Timestamp moment,
                                                                                         boolean includeOriginal, boolean useCache) throws GeneralException, IllegalArgumentException {
        if (!originalImageViewType.isOriginal()) {
            throw new IllegalArgumentException("originalImageViewType not an original viewType: " + originalImageViewType);
        }
        List<String> pctIdList = originalImageViewType.getContentTypeIds(includeOriginal, useCache);
        if (pctIdList.isEmpty()) {
            return Collections.emptyList();
        }
        return delegator.from("ProductCategoryContentAndInfo")
                .where(EntityCondition.makeCondition("productCategoryId", productCategoryId),
                        EntityCondition.makeCondition("prodCatContentTypeId", EntityOperator.IN, pctIdList))
                .orderBy("-fromDate").filterByDate(moment).cache(useCache).queryList();
    }

    public static Map<CategoryImageViewType, GenericValue> getVariantProductCategoryContentDataResourceRecordsByViewType(Delegator delegator, String productCategoryId,
                                                                                                                CategoryImageViewType originalImageViewType, Timestamp moment,
                                                                                                                boolean includeOriginal, boolean useCache) throws GeneralException, IllegalArgumentException {
        if (!originalImageViewType.isOriginal()) {
            throw new IllegalArgumentException("originalImageViewType not an original viewType: " + originalImageViewType);
        }
        Map<String, GenericValue> pcctIdMap = originalImageViewType.getContentTypesById(includeOriginal, useCache);
        if (pcctIdMap.isEmpty()) {
            return Collections.emptyMap();
        }
        List<GenericValue> pccdrList = delegator.from("ProductCategoryContentAndInfo")
                .where(EntityCondition.makeCondition("productCategoryId", productCategoryId),
                        EntityCondition.makeCondition("prodCatContentTypeId", EntityOperator.IN, pcctIdMap.keySet()))
                .orderBy("-fromDate").filterByDate(moment).cache(useCache).queryList();
        if (pccdrList.isEmpty()) {
            return Collections.emptyMap();
        }
        Map<CategoryImageViewType, GenericValue> viewTypeMap = new LinkedHashMap<>();
        for(GenericValue pccdr : pccdrList) {
            String prodCatContentTypeId = pccdr.getString("prodCatContentTypeId");
            viewTypeMap.put(CategoryImageViewType.from(pcctIdMap.get(prodCatContentTypeId), useCache), pccdr);
        }
        return viewTypeMap;
    }

    public static Map<String, GenericValue> getVariantProductCategoryContentDataResourceRecordsByViewSize(Delegator delegator, String productCategoryId,
                                                                                                  CategoryImageViewType originalImageViewType, Timestamp moment,
                                                                                                  boolean includeOriginal, boolean useCache) throws GeneralException, IllegalArgumentException {
        if (!originalImageViewType.isOriginal()) {
            throw new IllegalArgumentException("originalImageViewType not an original viewType: " + originalImageViewType);
        }
        Map<String, GenericValue> pcctIdMap = originalImageViewType.getContentTypesById(includeOriginal, useCache);
        if (pcctIdMap.isEmpty()) {
            return Collections.emptyMap();
        }
        List<GenericValue> pccdrList = delegator.from("ProductCategoryContentAndInfo")
                .where(EntityCondition.makeCondition("productCategoryId", productCategoryId),
                        EntityCondition.makeCondition("prodCatContentTypeId", EntityOperator.IN, pcctIdMap.keySet()))
                .orderBy("-fromDate").filterByDate(moment).cache(useCache).queryList();
        if (pccdrList.isEmpty()) {
            return Collections.emptyMap();
        }
        Map<String, GenericValue> viewTypeMap = new LinkedHashMap<>();
        for(GenericValue pccdr : pccdrList) {
            String prodCatContentTypeId = pccdr.getString("prodCatContentTypeId");
            CategoryImageViewType imageViewType = CategoryImageViewType.from(pcctIdMap.get(prodCatContentTypeId), useCache);
            viewTypeMap.put(imageViewType.getViewSize(), pccdr);
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

    public static String getProductCategoryInlineImageFieldPrefix(Delegator delegator, String productContentTypeId) {
        String candidateFieldName = ModelUtil.dbNameToVarName(productContentTypeId);
        ModelEntity productCategoryModel = delegator.getModelEntity("ProductCategory");
        if (productCategoryModel.isField(candidateFieldName) && candidateFieldName.endsWith("Url")) {
            return candidateFieldName.substring(0, candidateFieldName.length() - "Url".length());
        }
        return null;
    }

    public static String getDefaultCategoryImageProfileName(Delegator delegator, String productContentTypeId) {
        return "IMAGE_CATEGORY-" + productContentTypeId;
    }

    /**
     * Gets image profile from mediaprofiles.properties.
     * NOTE: prodCatContentTypeId should be ORIGINAL_IMAGE_URL or ADDITIONAL_IMAGE_x, not the size variants' IDs (LARGE_IMAGE_URL, ...)
     */
    public static ImageProfile getCategoryImageProfileOrDefault(Delegator delegator, String prodCatContentTypeId, GenericValue productCategory, GenericValue content, boolean useEntityCache, boolean useProfileCache) {
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
                            "Content.mediaProfile for content [" + content.get("contentId") + "] prodCatContentTypeId [" + prodCatContentTypeId +
                            "] category [" + productCategory.get("productCategoryId") + "]", module);
                    return null;
                }
            }
        } else if (productCategory != null) {
            profileName = productCategory.getString("imageProfile");
            if (profileName != null) {
                profile = ImageProfile.getImageProfile(delegator, profileName, useProfileCache);
                if (profile != null) {
                    return profile;
                } else {
                    // Explicitly named missing profile is always an error
                    Debug.logError("Could not find image profile [" + profileName + "] in mediaprofiles.properties from " +
                            "Product.imageProfile for product [" + productCategory.get("productId") + "] productContentTypeId [" + prodCatContentTypeId + "]", module);
                    return null;
                }
            }
        }
        profileName = getDefaultCategoryImageProfileName(delegator, prodCatContentTypeId);
        profile = ImageProfile.getImageProfile(delegator, profileName, useProfileCache);
        if (profile != null) {
            return profile;
        } else {
            // Clients may add extra productContentTypeId and they should add mediaprofiles.properties definitions
            Debug.logWarning("Could not find default image profile [" + profileName + "] in mediaprofiles.properties from " +
                    "prodCatContentTypeId [" + prodCatContentTypeId + "]; using IMAGE_CATEGORY", module);
        }
        profile = ImageProfile.getImageProfile(delegator, "IMAGE_CATEGORY", useProfileCache);
        if (profile != null) {
            return profile;
        } else {
            // Should not happen
            Debug.logError("Could not find image profile IMAGE_CATEGORY in mediaprofiles.properties; fatal error", module);
            return null;
        }
    }

    public static ImageProfile getDefaultCategoryImageProfile(Delegator delegator, String productContentTypeId, boolean useEntityCache, boolean useProfileCache) {
        return getCategoryImageProfileOrDefault(delegator, productContentTypeId, null, null, useEntityCache, useProfileCache);
    }
}
