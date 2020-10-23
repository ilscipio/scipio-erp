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
package org.ofbiz.product.image;

import java.io.IOException;
import java.io.Serializable;
import java.util.Locale;
import java.util.Map;

import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.UtilMisc;
import org.ofbiz.base.util.UtilProperties;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.base.util.cache.UtilCache;
import org.ofbiz.common.image.ImageVariantConfig;
import org.ofbiz.content.image.ContentImageWorker;
import org.ofbiz.entity.GenericValue;
import org.ofbiz.service.DispatchContext;
import org.ofbiz.service.ServiceUtil;

/**
 * SCIPIO: Image utilities for product (specifically) image handling.
 * Added 2017-07-04.
 *
 * @see org.ofbiz.content.image.ContentImageWorker
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

    public static void ensureProductImage(DispatchContext dctx, Locale locale, GenericValue product, String productContentTypeId, String imageLink, boolean async, boolean useCache) {
        if (!productImageEnsureEnabled || Boolean.TRUE.equals(productImageEnsureCache.get(productContentTypeId))) {
            return;
        }
        try {
            // TODO: optimize the duplicate lookups (cached anyway)
            Map<String, String> sizeTypeMap = ProductImageServices.getProductImageMissingVariantSizeTypes(dctx, locale,
                    product, productContentTypeId, null, imageLink, useCache);
            if (UtilValidate.isEmpty(sizeTypeMap)) {
                productImageEnsureCache.put(productContentTypeId, Boolean.TRUE);
                return;
            }
            Map<String, Object> ctx = UtilMisc.toMap("productId", product.get("productId"), "productContentTypeId", productContentTypeId, "sizeTypeList", sizeTypeMap.keySet());
            if (async) {
                dctx.getDispatcher().runAsync("productImageAutoRescale", ctx, false);
            } else {
                Map<String, Object> servResult = dctx.getDispatcher().runSync("productImageAutoRescale", ctx);
                if (ServiceUtil.isError(servResult)) {
                    Debug.logError("Could not trigger image variant resizing for product [" + product.get("productId") + "] productContentTypeId ["
                            + productContentTypeId + "] imageLink [" + imageLink + "]: " + ServiceUtil.getErrorMessage(servResult), module);
                    return;
                }
            }
            productImageEnsureCache.put(productContentTypeId, Boolean.TRUE);
        } catch(Exception e) {
            Debug.logError("Could not trigger image variant resizing for product [" + product.get("productId") + "] productContentTypeId ["
                    + productContentTypeId + "] imageLink [" + imageLink + "]: " + e.toString(), module);
        }
    }

    public static class ImageViewType implements Serializable {
        protected final String viewType;
        protected final String viewNumber;

        protected ImageViewType(String viewType, String viewNumber) {
            this.viewType = viewType;
            this.viewNumber = viewNumber;
        }

        public static ImageViewType from(String viewType, String viewNumber) {
            return new ImageViewType(viewType, viewNumber);
        }

        public static ImageViewType from(String productContentTypeId) {
            String viewType;
            String viewNumber;
            if ("ORIGINAL_IMAGE_URL".equals(productContentTypeId) || "LARGE_IMAGE_URL".equals(productContentTypeId)
                    || "MEDIUM_IMAGE_URL".equals(productContentTypeId) || "SMALL_IMAGE_URL".equals(productContentTypeId) || "DETAIL_IMAGE_URL".equals(productContentTypeId)) {
                viewType = "main";
                viewNumber = null;
            } else if ("LARGE_IMAGE_ALT".equals(productContentTypeId) || "MEDIUM_IMAGE_ALT".equals(productContentTypeId) || "SMALL_IMAGE_ALT".equals(productContentTypeId) || "DETAIL_IMAGE_ALT".equals(productContentTypeId)) {
                viewType = "alt"; // TODO: REVIEW: where is this one used?
                viewNumber = null;
            } else if (productContentTypeId.startsWith("ADDITIONAL_IMAGE_")) {
                viewType = "additional";
                viewNumber = productContentTypeId.substring("ADDITIONAL_IMAGE_".length());
                if (viewNumber.isEmpty()) {
                    throw new IllegalArgumentException("Unsupported productContentTypeId [" + productContentTypeId
                            + "]: invalid productContentTypeId name format (ADDITIONAL_IMAGE_n)");
                }
            } else if (productContentTypeId.startsWith("XTRA_IMG_")) { // do the corresponding ADDITIONAL_IMAGE
                viewType = "additional";
                String viewInfo = productContentTypeId.substring("XTRA_IMG_".length());
                String[] parts = viewInfo.split("_", 2);
                if (parts.length != 2 || parts[0].isEmpty() || parts[1].isEmpty()) {
                    throw new IllegalArgumentException("Unsupported productContentTypeId [" + productContentTypeId
                            + "]: invalid productContentTypeId name format (XTRA_IMG_n_abc)");
                }
                viewNumber = parts[0];
            } else {
                throw new IllegalArgumentException("Unsupported productContentTypeId [" + productContentTypeId
                        + "]: invalid productContentTypeId name format (XTRA_IMG_n_abc)");
            }
            return from(viewType, viewNumber);
        }

        public String getViewType() {
            return viewType;
        }

        public String getViewNumber() {
            return viewNumber;
        }
    }

}
