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
package com.ilscipio.scipio.content.image;

import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.GeneralException;
import org.ofbiz.base.util.UtilMisc;
import org.ofbiz.base.util.UtilProperties;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.common.image.ImageProfile;
import org.ofbiz.common.image.ImageVariantConfig;
import org.ofbiz.entity.GenericValue;
import org.ofbiz.service.DispatchContext;

import java.util.Collection;
import java.util.Locale;
import java.util.Map;

public abstract class ContentImageLocationInfoFactory<T extends ContentImageLocationInfo, V extends ContentImageViewType> {
    private static Map<String, ContentImageLocationInfoFactory> FACTORY_MAP = UtilMisc.newMap();

    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());
    public abstract T make(DispatchContext dctx, String id, V imageViewType,
                           ImageVariantConfig variantConfig, String imagePath, Collection<String> sizeTypeList,
                           boolean useEntityCache, boolean useProfileCache, Map<String, Object> extraParams) throws IllegalArgumentException;

    public T from(DispatchContext dctx, String id, V imageViewType,
                  ImageVariantConfig variantConfig, String imageFilename,
                  Collection<String> sizeTypeList, boolean useEntityCache, boolean useProfileCache, Map<String, Object> extraParams) throws GeneralException {
        return make(dctx, id, imageViewType, variantConfig, imageFilename, sizeTypeList, useEntityCache, useProfileCache, extraParams);
    }

    public T from(DispatchContext dctx, String id, V imageViewType,
                  ImageProfile imageProfile, String imageFilename,
                  Collection<String> sizeTypeList, boolean useEntityCache, boolean useProfileCache, Map<String, Object> extraParams) throws GeneralException {
        // NOTE: this currently calls the non-cached readVariantConfig, because this is intended for backend
        return from(dctx, id, imageViewType, (imageProfile != null) ?
                        (useProfileCache ? imageProfile.getVariantConfig() : imageProfile.readVariantConfig()) : null,
                imageFilename, sizeTypeList, useEntityCache, useProfileCache, extraParams);
    }

    /**
     * Returns CommonImageLocationInfo or null if no image URL/location/variants applicable for the given mainEntity/contentTypeId
     * (ie: product/productContentTypeId or category/prodCatContentTypeId).
     * If passed *Content(ie: ProductContent or ProductCategoryContent) or imageUrl null attempts to determine from data.
     * Based on productImageAutoRescale (TODO?: deduplicate).
     */
    public abstract T from(DispatchContext dctx, Locale locale, GenericValue mainEntityValue, V imageViewType,
                  String imageUrl, Collection<String> sizeTypeList,
                  Boolean useParentImageUrl, boolean useEntityCache, boolean useProfileCache, Map<String, Object> extraParams) throws GeneralException;

    public static ContentImageLocationInfoFactory getInstance(String propertyFactoryName) {
        if (!FACTORY_MAP.containsKey(propertyFactoryName)) {
            String clsName = UtilProperties.getPropertyValue("catalog", propertyFactoryName);
            if (UtilValidate.isEmpty(clsName)) {
                FACTORY_MAP.put(propertyFactoryName, null);
            }
            try {
                Class<?> cls = Class.forName(clsName);
                FACTORY_MAP.put(propertyFactoryName, (ContentImageLocationInfoFactory) cls.getConstructor().newInstance());
            } catch (Exception e) {
                Debug.logError(e, "Invalid catalog#" + propertyFactoryName, module);
                FACTORY_MAP.put(propertyFactoryName, null);
            }
        }
        return FACTORY_MAP.get(propertyFactoryName);
    }
}



