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

    /** NOTE: Currently does not recognize product images anymore (they can't be mixed in current code), see ProductImageWorker.getProductImageProfileOrDefault */
    public static ImageProfile getContentImageProfileOrDefault(Delegator delegator, GenericValue content, boolean useEntityCache, boolean useUtilCache) {
        String profileName;
        ImageProfile profile;
        if (content != null) {
            profileName = content.getString("mediaProfile");
            if (profileName != null) {
                profile = ImageProfile.getImageProfile(delegator, profileName, useUtilCache);
                if (profile != null) {
                    return profile;
                } else {
                    // Explicitly named missing profile is always an error
                    Debug.logError("Could not find image profile [" + profileName + "] in mediaprofiles.properties from " +
                            "Content.mediaProfile for content [" + content.get("contentId") + "]", module);
                    return null;
                }
            }
            String contentTypeId = content.getString("contentTypeId");
            if ("SCP_MEDIA".equals(contentTypeId) || "SCP_MEDIA_VARIANT".equals(contentTypeId)) { // NOTE: variant should usually not be passed here!
                profile = ImageProfile.getImageProfile(delegator, "IMAGE_MEDIA", useUtilCache);
                if (profile != null) {
                    return profile;
                } else {
                    // Should not happen
                    Debug.logError("Could not find image profile IMAGE_MEDIA in mediaprofiles.properties; fatal error", module);
                    return null;
                }
            }
        }
        profile = ImageProfile.getImageProfile(delegator, "IMAGE_CONTENT", useUtilCache);
        if (profile != null) {
            return profile;
        } else {
            // Should not happen
            Debug.logError("Could not find image profile IMAGE_CONTENT in mediaprofiles.properties; fatal error", module);
            return null;
        }
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
}
