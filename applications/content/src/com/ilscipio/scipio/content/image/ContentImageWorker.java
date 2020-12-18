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
package com.ilscipio.scipio.content.image;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Properties;
import java.util.Set;
import java.util.TimeZone;

import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.UtilMisc;
import org.ofbiz.base.util.UtilProperties;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.base.util.string.FlexibleStringExpander;
import org.ofbiz.common.image.ImageProfile;
import org.ofbiz.common.image.ImageVariantConfig;
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.GenericEntityException;
import org.ofbiz.entity.GenericValue;
import org.ofbiz.entity.condition.EntityCondition;
import org.ofbiz.entity.condition.EntityOperator;

/**
 * SCIPIO: Content/generic image utilities.
 * Added 2017-07-04.
 */
public abstract class ContentImageWorker {

    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());

    /**
     * Special image size type name designating the original (unscaled/unmodified) image.
     * <p>
     * This should be the same value as {@link org.ofbiz.webapp.content.ContentRequestWorker#ORIGINAL_SIZETYPE}
     * which is used in the <code>@contentUrl</code> macro.
     */
    public static final String ORIGINAL_SIZETYPE = "original";

    public static final String CONTENT_IMAGEPROP_FILEPATH = "/applications/content/config/ImageProperties.xml";

    public static final String CONTENTATTR_VARIANTCFG = "scpVariantCfg";

    /**
     * Keeps from overloading log with giant filenames.
     */
    public static final int LOG_INFO_MAXPATH = UtilProperties.getPropertyAsInteger("content", "image.log.info.maxpath", 80);

    public static final FlexibleStringExpander IMGSZ_CNTASSTYPEID_EXPR = FlexibleStringExpander.getInstance("IMGSZ_${sizetype}");

    /**
     * Default Content field expressions for contentImageDbScaleInAllSizeCore and such services.
     */
    public static final Map<String, FlexibleStringExpander> RESIZEIMG_CONTENT_FIELDEXPR;
    /**
     * Default DataResource field expressions for contentImageDbScaleInAllSizeCore and such services.
     */
    public static final Map<String, FlexibleStringExpander> RESIZEIMG_DATARESOURCE_FIELDEXPR;
    static {
        Map<String, FlexibleStringExpander> coExprMap = new HashMap<>();
        coExprMap.put("contentName", FlexibleStringExpander.getInstance("${fields.contentName}_${sizetype}"));
        RESIZEIMG_CONTENT_FIELDEXPR = Collections.unmodifiableMap(coExprMap);

        Map<String, FlexibleStringExpander> drExprMap = new HashMap<>();
        drExprMap.put("dataResourceName", FlexibleStringExpander.getInstance("${fields.dataResourceName}_${sizetype}"));
        drExprMap.put("objectInfo", FlexibleStringExpander.getInstance("${origfn}_${sizetype}.${ext}"));
        RESIZEIMG_DATARESOURCE_FIELDEXPR = Collections.unmodifiableMap(drExprMap);
    }

    protected ContentImageWorker() {
    }

    /**
     * Gets Content.mediaProfile or an inferred default for the detected content type, as defined in mediaprofiles.properties.
     * <p>Stock defaults: product-default, cms-default, default.</p>
     * <p>NOTE: common-default also exists but is not returned by this method because it's the fallback default.</p>
     */
    public static String getContentImageMediaProfileOrDefault(GenericValue content, boolean useCache) throws GenericEntityException {
        String mediaProfile = content.getString("mediaProfile");
        if (mediaProfile != null) {
            return mediaProfile;
        }
        // Infer default
        GenericValue productContent = content.getDelegator().from("ProductContent")
                .where("contentId", content.get("contentId")).cache(useCache).queryFirst();
        if (productContent != null) {
            return "IMAGE_PRODUCT-" + productContent.get("productContentTypeId");
        }
        String contentTypeId = content.getString("contentTypeId");
        if ("SCP_MEDIA".equals(contentTypeId) || "SCP_MEDIA_VARIANT".equals(contentTypeId)) {
            return "IMAGE_CONTENT";
        }
        return "IMAGE_DEFAULT";
    }

    /**
     * SCIPIO: Returns the full path to the ImageProperties.xml file to use for generic image size definitions.
     * 2017-08-08: This can now be defined either under content or common components.
     * Added 2017-07-04.
     */
    public static String getContentImagePropertiesFullPath() throws IOException {
        String path = ImageVariantConfig.getImagePropertiesFullPath(CONTENT_IMAGEPROP_FILEPATH);
        if (new java.io.File(path).exists()) {
            return path;
        } else {
            return ImageVariantConfig.getCommonImagePropertiesFullPath();
        }
    }

    public static String getContentImagePropertiesPath() throws IOException {
        String path = ImageVariantConfig.getImagePropertiesFullPath(CONTENT_IMAGEPROP_FILEPATH);
        if (new java.io.File(path).exists()) {
            return CONTENT_IMAGEPROP_FILEPATH;
        } else {
            return ImageVariantConfig.getCommonImagePropertiesPath();
        }
    }

    public static String formatLogInfoPath(String filename) {
        if (filename == null || filename.isEmpty()) return "[none]";
        else return "'"
                + (filename.length() > LOG_INFO_MAXPATH ? "..." + filename.substring(filename.length() - LOG_INFO_MAXPATH) : filename)
                + "'";
    }

//    public static Map<String, Object> getBufferedImageFromContentId(String contentId, Locale locale)
//            throws IllegalArgumentException, IOException {
//
//        /* VARIABLES */
//        BufferedImage bufImg;
//        Map<String, Object> result = new LinkedHashMap<String, Object>();
//
//        /* BUFFERED IMAGE */
//        try {
//            bufImg = null; // TODO
//            if (false) throw new IOException("NOT IMPLEMENTED"); //TODO
//            //bufImg = ImageIO.read(new File(fileLocation));
//        } catch (IllegalArgumentException e) {
//            String errMsg = UtilProperties.getMessage(ImageTransform.resource, "ImageTransform.input_is_null", locale) + " : " + contentId + " ; " + e.toString();
//            Debug.logError(errMsg, module);
//            result.put("errorMessage", errMsg);
//            return result;
//        } catch (IOException e) {
//            String errMsg = UtilProperties.getMessage(ImageTransform.resource, "ImageTransform.error_occurs_during_reading", locale) + " : " + contentId + " ; " + e.toString();
//            Debug.logError(errMsg, module);
//            result.put("errorMessage", errMsg);
//            return result;
//        }
//
//        result.put("responseMessage", "success");
//        result.put("bufferedImage", bufImg);
//        return result;
//    }

    public static List<GenericValue> getResizedImageContentAssocRecords(Delegator delegator, String contentId, boolean useCache) throws GenericEntityException {
        List<EntityCondition> condList = new ArrayList<>();
        condList.add(EntityCondition.makeCondition("contentId", contentId));
        condList.add(EntityCondition.makeCondition("contentAssocTypeId", EntityOperator.LIKE, "IMGSZ_%"));
        return delegator.findList("ContentAssoc",
                EntityCondition.makeCondition(condList, EntityOperator.AND), null, null, null, useCache);
    }

    public static List<GenericValue> getResizedImageContentAssocDataResourceRecords(Delegator delegator, String contentId, boolean useCache) throws GenericEntityException {
        List<EntityCondition> condList = new ArrayList<>();
        condList.add(EntityCondition.makeCondition("contentIdStart", contentId));
        condList.add(EntityCondition.makeCondition("caContentAssocTypeId", EntityOperator.LIKE, "IMGSZ_%"));
        return delegator.findList("ContentAssocDataResourceViewToReq",
                EntityCondition.makeCondition(condList, EntityOperator.AND), null, null, null, useCache);
    }

    public static Set<String> getResizedImageContentAssocContentIdTo(Delegator delegator, String contentId, boolean useCache) throws GenericEntityException {
        Set<String> contentIdListTo = new LinkedHashSet<>();
        List<GenericValue> contentAssocTo = getResizedImageContentAssocRecords(delegator, contentId, useCache);
        if (UtilValidate.isNotEmpty(contentAssocTo)) {
            for(GenericValue contentAssoc : contentAssocTo) {
                contentIdListTo.add(contentAssoc.getString("contentIdTo"));
            }
        }
        return contentIdListTo;
    }

    public static Map<String, Object> parseMapFieldExpr(Map<String, Object> map, Map<String, Object> imageCtx, TimeZone timeZone, Locale locale) {
        for(Map.Entry<String, Object> entry : map.entrySet()) {
            if (entry.getValue() instanceof FlexibleStringExpander) {
                entry.setValue(((FlexibleStringExpander) entry.getValue()).expandString(imageCtx, timeZone, locale));
            }
        }
        return map;
    }

    @Deprecated
    public static Map<String, Object> getImageContentVariantDetails(GenericValue content, boolean useCache) throws GenericEntityException {
        if (content == null) {
            return null;
        }
        Delegator delegator = content.getDelegator();
        String contentId = content.getString("contentId");
        Map<String, Object> icvd = new LinkedHashMap<>();
        String mediaProfileName = getContentImageMediaProfileOrDefault(content, useCache);
        String contentMediaProfileName = content.getString("mediaProfile");
        icvd.put("mediaProfileName", mediaProfileName);
        icvd.put("contentMediaProfileName", contentMediaProfileName);
        ImageProfile mediaProfile = ImageProfile.getImageProfile(delegator, mediaProfileName);
        if (mediaProfile == null) {
            Debug.logWarning("getImageContentVariantDetails: Could not find mediaProfile [" + mediaProfileName + "]; configuration changed?", module);
        } else {
            icvd.put("mediaProfile", mediaProfile);
        }
        if (contentMediaProfileName != null) {
            ImageProfile contentMediaProfile = ImageProfile.getImageProfile(delegator, contentMediaProfileName);
            if (mediaProfile == null) {
                Debug.logWarning("getImageContentVariantDetails: Could not find mediaProfile [" + mediaProfileName + "]; configuration changed?", module);
            } else {
                icvd.put("contentMediaProfile", contentMediaProfile);
            }
        }

        List<GenericValue> variantRecordList = getResizedImageContentAssocDataResourceRecords(delegator, contentId, useCache);
        if (UtilValidate.isNotEmpty(variantRecordList)) {
            icvd.put("variantRecordList", variantRecordList);
        }

        Map<String, Map<String, Object>> variants = new LinkedHashMap<>();
        if (UtilValidate.isNotEmpty(variantRecordList)) {
            for(GenericValue variantRecord : variantRecordList) {
                String sizeType = variantRecord.getString("caMapKey");
                Map<String, Object> variantMap = new LinkedHashMap<>();
                variantMap.put("sizeType", sizeType);
                variantMap.put("contentAssocTypeId", variantRecord.get("caContentAssocTypeId"));
                variantMap.put("variantRecord", variantRecord);
                Long imageWidth = variantRecord.getLong("drScpWidth");
                if (imageWidth != null) {
                    variantMap.put("imageWidth", imageWidth.intValue());
                }
                Long imageHeight = variantRecord.getLong("drScpHeight");
                if (imageHeight != null) {
                    variantMap.put("imageHeight", imageHeight.intValue());
                }
                ImageVariantConfig.VariantInfo variantConfig = null;
                String sizeId = variantRecord.getString("drSizeId");
                if (sizeId != null) {
                    GenericValue imageSizeDimension = delegator.findOne("ImageSizeDimension", UtilMisc.toMap("sizeId", sizeId), true);
                    if (imageSizeDimension != null) {
                        variantConfig = ImageVariantConfig.VariantInfo.fromImageSizeDimension(imageSizeDimension);
                    }
                }
                if (variantConfig == null && mediaProfile != null) {
                    variantConfig = mediaProfile.getVariantConfig().getVariant(sizeType);
                    if (variantConfig == null) {
                        Debug.logWarning("getImageContentVariantDetails: Could not find sizeType [" + sizeType
                                + "] in media profile [" + mediaProfile.getName() + "] for variant content [" + variantRecord.get("contentId") + "]; configuration changed?", module);
                    }
                }
                if (variantConfig != null) {
                    variantMap.put("variantConfig", variantConfig);
                    variantMap.put("presetWidth", variantConfig.getWidth());
                    variantMap.put("presetHeight", variantConfig.getHeight());
                } else {
                    Debug.logWarning("getImageContentVariantDetails: No variant config found for variant content [" + variantRecord.get("contentId") + "]", module);
                }
                // Get the effective preset data used to generate the media, IF available (not always)
                Map<String, Object> srcPreset = variantRecord.getJsonAsMap("drSrcPresetJson");
                if (srcPreset != null) {
                    ImageVariantConfig.VariantInfo srcVariantConfig = ImageVariantConfig.VariantInfo.fromMap(sizeType, srcPreset);
                    variantMap.put("srcVariantConfig", srcVariantConfig);
                    variantMap.put("srcPresetWidth", srcVariantConfig.getWidth());
                    variantMap.put("srcPresetHeight", srcVariantConfig.getHeight());
                }
                variants.put(sizeType, variantMap);
            }
            icvd.put("variants", variants);
        }
        return icvd;
    }

    @Deprecated
    public static Map<String, Object> getImageContentVariantDetails(Delegator delegator, String contentId, boolean useCache) throws GenericEntityException {
        return getImageContentVariantDetails(delegator.findOne("Content", UtilMisc.toMap("contentId", contentId), useCache), useCache);
    }

    @Deprecated
    public static Map<String, Object> getImageContentVariantDetailsSafe(GenericValue content, boolean useCache) {
        try {
            return getImageContentVariantDetails(content, useCache);
        } catch (GenericEntityException e) {
            Debug.logError(e, module);
            return null;
        }
    }

    @Deprecated
    public static Map<String, Object> getImageContentVariantDetailsSafe(Delegator delegator, String contentId, boolean useCache) {
        try {
            return getImageContentVariantDetails(delegator, contentId, useCache);
        } catch (GenericEntityException e) {
            Debug.logError(e, module);
            return null;
        }
    }
}
