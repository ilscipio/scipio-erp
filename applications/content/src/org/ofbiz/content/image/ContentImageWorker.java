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
package org.ofbiz.content.image;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Set;
import java.util.TimeZone;

import org.ofbiz.base.util.UtilProperties;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.base.util.string.FlexibleStringExpander;
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

    public static final String module = ContentImageWorker.class.getName();
    
    /**
     * Special image size type name designating the original (unscaled/unmodified) image.
     * <p>
     * This should be the same value as {@link org.ofbiz.webapp.content.ContentRequestWorker#ORIGINAL_SIZETYPE}
     * which is used in the <code>@ofbizContentUrl</code> macro.
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
    
    public static Set<String> getResizedImageContentAssocContentIdTo(Delegator delegator, String contentId, boolean useCache) throws GenericEntityException {
        Set<String> contentIdListToRemove = new LinkedHashSet<>();
        List<GenericValue> contentAssocToRemove = getResizedImageContentAssocRecords(delegator, contentId, useCache);
        if (UtilValidate.isNotEmpty(contentAssocToRemove)) {
            for(GenericValue contentAssoc : contentAssocToRemove) {
                contentIdListToRemove.add(contentAssoc.getString("contentIdTo"));
            }
        }
        return contentIdListToRemove;
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
