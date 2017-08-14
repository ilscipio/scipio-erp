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

import java.awt.image.BufferedImage;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.sql.Timestamp;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.TimeZone;

import javax.imageio.ImageIO;

import org.apache.commons.io.FileUtils;
import org.ofbiz.base.location.FlexibleLocation;
import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.UtilDateTime;
import org.ofbiz.base.util.UtilGenerics;
import org.ofbiz.base.util.UtilMisc;
import org.ofbiz.base.util.UtilProperties;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.base.util.string.FlexibleStringExpander;
import org.ofbiz.common.image.ImageTransform;
import org.ofbiz.common.image.ImageVariantConfig;
import org.ofbiz.content.data.DataResourceWorker;
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.GenericEntityException;
import org.ofbiz.entity.GenericValue;
import org.ofbiz.entity.util.EntityQuery;
import org.ofbiz.entity.util.EntityUtilProperties;
import org.ofbiz.service.DispatchContext;
import org.ofbiz.service.GenericServiceException;
import org.ofbiz.service.LocalDispatcher;
import org.ofbiz.service.ServiceUtil;

import javolution.util.FastList;

/**
 * SCIPIO: Content/generic image services.
 * FIXME: MISSING DELETION CODE LOGIC
 * Added 2017-07-04.
 * <p->
 * Derived from:
 * <ul>
 * <li>{@link org.ofbiz.product.image.ScaleImage}</li>
 * <li>{@link org.ofbiz.product.imagemanagement.ImageManagementServices#scaleImageMangementInAllSize}</li>
 * <li>(potentially more, please update list as needed to track for maintenance purposes)</li>
 * </ul>
 */
public abstract class ContentImageServices {

    public static final String module = ContentImageServices.class.getName();

    // SCIPIO: FIXME?: don't really want this dependency, but not major issue 
    private static final String resourceProduct = "ProductErrorUiLabels";
    private static final Locale LOG_LANG = Locale.ENGLISH; // always EN
    
    protected ContentImageServices() {
    }

    /**
     * Core image file resizing service.
     * See contentImageFileScaleInAllSizeCore service interface for context params.
     * FIXME: MISSING DELETION CODE LOGIC
     */
    public static Map<String, Object> contentImageFileScaleInAllSizeCore(DispatchContext dctx, Map<String, ?> context) {
        Delegator delegator = dctx.getDelegator();
        String imageOrigPath = (String) context.get("imageOrigPath");
        String imageOrigUrl = (String) context.get("imageOrigUrl");
        String imageOrigFn = (String) context.get("imageOrigFn");
        String imageServerPath = (String) context.get("imageServerPath");
        String imageUrlPrefix = (String) context.get("imageUrlPrefix");
        String imageFnFmt = (String) context.get("imageFnFmt");
        String imageOrigFnFmt = (String) context.get("imageOrigFnFmt");
        Map<String, Object> imagePathArgs = UtilGenerics.checkMap(context.get("imagePathArgs"));
        String imagePropXmlPath = (String) context.get("imagePropXmlPath");
        Collection<String> sizeTypeList = UtilGenerics.checkList(context.get("sizeTypeList"));
        boolean copyOrig = Boolean.TRUE.equals(context.get("copyOrig"));
        boolean deleteOld = Boolean.TRUE.equals(context.get("deleteOld"));
        Map<String, Object> scalingOptions = UtilGenerics.checkMap(context.get("scalingOptions"));
        Locale locale = (Locale) context.get("locale");
        if (locale == null) locale = Locale.getDefault();
        
        final String origSizeType = ContentImageWorker.ORIGINAL_SIZETYPE;
        final String logPrefix = "contentImageFileScaleInAllSizeCore: ";

        long startTime = System.nanoTime();
        
        try {
            // SCIPIO: for these we now support component:// and file:// prefix in addition to plain absolute file location
            if (UtilValidate.isNotEmpty(imageOrigPath)) {
                imageOrigPath = FlexibleLocation.resolveFileUrlAsPathIfUrl(imageOrigPath, imageOrigPath);
            }
            if (UtilValidate.isNotEmpty(imageServerPath)) {
                imageServerPath = FlexibleLocation.resolveFileUrlAsPathIfUrl(imageServerPath, imageServerPath);
            }
            
            /* ImageProperties.xml */
            ImageVariantConfig imgPropCfg;
            if (UtilValidate.isEmpty(imagePropXmlPath)) {
                imagePropXmlPath = ContentImageWorker.getContentImagePropertiesPath();
            }
            try {
                imgPropCfg = ImageVariantConfig.fromImagePropertiesXml(imagePropXmlPath, locale);
            } catch(Exception e) {
                Debug.logError(logPrefix+UtilProperties.getMessage(resourceProduct, "ScaleImage.unable_to_parse", LOG_LANG) + " : " + imagePropXmlPath + " : " + e.getMessage(), module);
                return ServiceUtil.returnError(UtilProperties.getMessage(resourceProduct, "ScaleImage.unable_to_parse", locale) + " : " + imagePropXmlPath + " : " + e.getMessage());
            }
            if (sizeTypeList == null) {
                sizeTypeList = imgPropCfg.getVariantNames();
            }
            
            /* IMAGE */
            if (imageOrigFnFmt == null) imageOrigFnFmt = imageFnFmt;

            if (UtilValidate.isEmpty(imageOrigFn)) {
                if (UtilValidate.isNotEmpty(imageOrigPath)) {
                    imageOrigFn = imageOrigPath;
                } else if (UtilValidate.isNotEmpty(imageOrigUrl)) {
                    imageOrigFn = imageOrigUrl;
                } else {
                    throw new IllegalArgumentException("Required parameter missing: imageOrigFn/imageOrigPath/imageOrigUrl");
                }
                if (imageOrigFn.lastIndexOf("/") != -1) {
                    imageOrigFn = imageOrigFn.substring(imageOrigFn.lastIndexOf("/") + 1);
                }
            }
            
            // get Name and Extension
            if (imageOrigFn.lastIndexOf(".") <= 0 || imageOrigFn.lastIndexOf(".") >= (imageOrigFn.length() - 1)) { // SCIPIO: added this to prevent problems
                throw new IllegalArgumentException("Original image filename [" + imageOrigFn + "] has missing or improper file extension (image type)");
            }
            String imgExtension = imageOrigFn.substring(imageOrigFn.lastIndexOf(".") + 1);
            
            // paths
            Map<String, Object> imageContext = new HashMap<>(context);
            imageContext.put("tenantId", delegator.getDelegatorTenantId());
            imageContext.putAll(imagePathArgs);
            imageServerPath = FlexibleStringExpander.expandString(UtilValidate.isNotEmpty(imageServerPath) ? imageServerPath : EntityUtilProperties.getPropertyValue("content", "image.server.path", delegator), imageContext);
            imageUrlPrefix = FlexibleStringExpander.expandString(UtilValidate.isNotEmpty(imageUrlPrefix) ? imageUrlPrefix : EntityUtilProperties.getPropertyValue("content", "image.url.prefix", delegator), imageContext);
            imageServerPath = imageServerPath.endsWith("/") ? imageServerPath.substring(0, imageServerPath.length()-1) : imageServerPath;
            imageUrlPrefix = imageUrlPrefix.endsWith("/") ? imageUrlPrefix.substring(0, imageUrlPrefix.length()-1) : imageUrlPrefix;
            
            FlexibleStringExpander imageFnFmtExpander = FlexibleStringExpander.getInstance(imageFnFmt);
            FlexibleStringExpander imageOrigFnFmtExpander = FlexibleStringExpander.getInstance(imageOrigFnFmt);
            
            String bufImgPath;
            if (UtilValidate.isNotEmpty(imageOrigPath)) {
                bufImgPath = imageOrigPath;
            } else if (UtilValidate.isNotEmpty(imageOrigUrl)) {
                // TODO: improve this to support getting URL from any mount-point
                if (!imageOrigUrl.startsWith(imageUrlPrefix + "/")) throw new IllegalArgumentException("imageOrigUrl '" + imageOrigUrl + "' does not begin with expected imageUrlPrefix '" + imageUrlPrefix + "'");
                bufImgPath = imageServerPath + imageOrigUrl.substring(imageUrlPrefix.length());
            } else {
                bufImgPath = imageServerPath + "/" + expandImageFnFmt(imageOrigFnFmtExpander, origSizeType, imagePathArgs) + "." + imgExtension;
            }
            
            /* get original BUFFERED IMAGE */
            Map<String, Object> resultBufImgMap = ImageTransform.getBufferedImage(bufImgPath, locale);
            
            String targetDirectory = null;
            if ("success".equals(resultBufImgMap.get("responseMessage"))) {
                BufferedImage bufImg = (BufferedImage) resultBufImgMap.get("bufferedImage");
    
                // get Dimensions
                double imgHeight = bufImg.getHeight();
                double imgWidth = bufImg.getWidth();
                if (imgHeight == 0.0 || imgWidth == 0.0) {
                    Debug.logError(logPrefix+UtilProperties.getMessage(resourceProduct, "ScaleImage.one_current_image_dimension_is_null", LOG_LANG) + " : imgHeight = " + imgHeight + " ; imgWidth = " + imgWidth, module);
                    return ServiceUtil.returnError(UtilProperties.getMessage(resourceProduct, "ScaleImage.one_current_image_dimension_is_null", locale) + " : imgHeight = " + imgHeight + " ; imgWidth = " + imgWidth);
                }
                
                Map<String, String> imgUrlMap = new HashMap<>();
    
                int imageDeleteCount = 0;
                int imageCopyCount = 0;
                if (copyOrig) {
                    String sizeType = origSizeType;
                    String newFileLocation = expandImageFnFmt(imageOrigFnFmtExpander, sizeType, imagePathArgs);
                    String newFileLocExt = newFileLocation + "." + imgExtension;
                    String newFileFullLoc = imageServerPath + "/" + newFileLocExt;
                    if (bufImgPath.equals(newFileFullLoc)) {
                        Debug.logWarning(logPrefix+"copyOrig was requested, but output orig file would be same as input orig file (" + bufImgPath + ")", module);
                        
                        // put this so the caller gets a URL to the original even if didn't change
                        String imageUrl = imageUrlPrefix + "/" + newFileLocExt;
                        imgUrlMap.put(sizeType, imageUrl);
                    } else {
                        targetDirectory = imageServerPath + "/" + getExpandedFnFmtDirPrefix(newFileLocation);
                        try {
                            // Create the new directory
                            File targetDir = new File(targetDirectory);
                            if (!targetDir.exists()) {
                                boolean created = targetDir.mkdirs();
                                if (!created) {
                                    Debug.logError(logPrefix+UtilProperties.getMessage(resourceProduct, "ScaleImage.unable_to_create_target_directory", LOG_LANG) + " - " + targetDirectory, module);
                                    return ServiceUtil.returnError(UtilProperties.getMessage(resourceProduct, "ScaleImage.unable_to_create_target_directory", locale) + " - " + targetDirectory);
                                }
                            } else if (deleteOld) {
                            // TODO?: how do this here?
//                            // Delete existing image files
//                            // Images aren't ordered by productId (${location}/${viewtype}/${sizetype}/${id}) !!! BE CAREFUL !!!
//                             if (newFileLocation.endsWith("/" + "id-FIXME")) {
//                                try {
//                                    File[] files = targetDir.listFiles(); 
//                                    for (File file : files) {
//                                        if (file.isFile() && file.getName().startsWith("id-FIXME")) {
//                                            file.delete();
//                                        }
//                                    }
//                                } catch (SecurityException e) {
//                                    Debug.logError(e, logPrefix+e.getMessage(), module);
//                                }
//                            }
                            }
                        } catch (Exception e) {
                            Debug.logError(e, logPrefix+"Unexpected error during directory creation/deletion: " + e.getMessage(), module);
                            return ServiceUtil.returnError(UtilProperties.getMessage(resourceProduct, "ScaleImage.error_occurs_during_writing", locale));
                        }
                        
                        try {
                            FileUtils.copyFile(new File(bufImgPath), new File(newFileFullLoc));
                            imageCopyCount++;
                        } catch(Exception e) {
                            Debug.logError(e, logPrefix+"Error copying original file [" + bufImgPath + "] to [" + newFileFullLoc + "]: " + e.getMessage(), module);
                            return ServiceUtil.returnError(UtilProperties.getMessage(resourceProduct, "ScaleImage.error_occurs_during_writing", locale));
                        }
                        
                        String imageUrl = imageUrlPrefix + "/" + newFileLocExt;
                        imgUrlMap.put(sizeType, imageUrl);
                    }
                }
                
                /* Scale image for each size from ImageProperties.xml */
                int scaledImageCount = 0;
                for (String sizeType : sizeTypeList) {
                    if (!imgPropCfg.hasVariant(sizeType)) {
                        Debug.logError(logPrefix+"sizeType " + sizeType + " is not part of ImageProperties.xml; ignoring", module);
                        continue;
                    }
                    
                    // Scale
                    Map<String, Object> resultScaleImgMap = ImageTransform.scaleImage(bufImg, imgHeight, imgWidth, imgPropCfg.getVariantStringMap(), sizeType, locale, scalingOptions);
    
                    /* Write the new image file */
                    if ("success".equals(resultScaleImgMap.get("responseMessage"))) {
                        BufferedImage bufNewImg = (BufferedImage) resultScaleImgMap.get("bufferedImage");
    
                        // Build full path for the new scaled image
                        //imageFnToUse = sizeType + imageFnToUse.substring(imageFnToUse.lastIndexOf(".")); // BUG
                        String newFileLocation = expandImageFnFmt(imageFnFmtExpander, sizeType, imagePathArgs);
                        targetDirectory = imageServerPath + "/" + getExpandedFnFmtDirPrefix(newFileLocation);
                        try {
                            // Create the new directory
                            File targetDir = new File(targetDirectory);
                            if (!targetDir.exists()) {
                                boolean created = targetDir.mkdirs();
                                if (!created) {
                                    Debug.logError(logPrefix+UtilProperties.getMessage(resourceProduct, "ScaleImage.unable_to_create_target_directory", LOG_LANG) + " - " + targetDirectory, module);
                                    return ServiceUtil.returnError(UtilProperties.getMessage(resourceProduct, "ScaleImage.unable_to_create_target_directory", locale) + " - " + targetDirectory);
                                }
                            } else if (deleteOld) {
                            // TODO?: how do this here?
//                            // Delete existing image files
//                            // Images aren't ordered by productId (${location}/${viewtype}/${sizetype}/${id}) !!! BE CAREFUL !!!
//                             if (newFileLocation.endsWith("/" + "id-FIXME")) {
//                                try {
//                                    File[] files = targetDir.listFiles(); 
//                                    for (File file : files) {
//                                        if (file.isFile() && file.getName().startsWith("id-FIXME")) {
//                                            file.delete();
//                                        }
//                                    }
//                                } catch (SecurityException e) {
//                                    Debug.logError(e, logPrefix+e.getMessage(), module);
//                                }
//                            }
                            }
                        } catch (Exception e) {
                            Debug.logError(e, logPrefix+"Unexpected error during directory creation or file deletion: " + e.getMessage(), module);
                            return ServiceUtil.returnError(UtilProperties.getMessage(resourceProduct, "ScaleImage.error_occurs_during_writing", locale));
                        }
    
                        // write new image
                        String newFileLocExt = newFileLocation + "." + imgExtension;
                        String newFileFullLoc = imageServerPath + "/" + newFileLocExt;
                        try {
                            ImageIO.write(bufNewImg, imgExtension, new File(newFileFullLoc));
                            scaledImageCount++;
                        } catch (IllegalArgumentException e) {
                            Debug.logError(e, logPrefix+UtilProperties.getMessage(resourceProduct, "ScaleImage.one_parameter_is_null", LOG_LANG) + ": " + e.getMessage(), module);
                            return ServiceUtil.returnError(UtilProperties.getMessage(resourceProduct, "ScaleImage.one_parameter_is_null", locale));
                        } catch (IOException e) {
                            Debug.logError(e, logPrefix+UtilProperties.getMessage(resourceProduct, "ScaleImage.error_occurs_during_writing", LOG_LANG) + ": " + e.getMessage(), module);
                            return ServiceUtil.returnError(UtilProperties.getMessage(resourceProduct, "ScaleImage.error_occurs_during_writing", locale));
                        }
    
                        // Save each Url
                        String imageUrl = imageUrlPrefix + "/" + newFileLocExt;
                        imgUrlMap.put(sizeType, imageUrl);
                        
                    } else {
                        // SCIPIO: new
                        Debug.logError(logPrefix+ServiceUtil.getErrorMessage(resultScaleImgMap), module);
                        return ServiceUtil.returnError(ServiceUtil.getErrorMessage(resultScaleImgMap));
                    }
                } // Loop over sizeType
    
                // this is helpful info and doesn't do much harm
                //if (ImageUtil.verboseOn()) {
                long endTime = System.nanoTime();
                StringBuilder logSb = new StringBuilder(logPrefix);
                logSb.append("In ");
                logSb.append((endTime - startTime) / 1000000);
                logSb.append("ms created ");
                logSb.append(scaledImageCount);
                logSb.append(" scaled and ");
                logSb.append(imageCopyCount);
                logSb.append(" original copies of image ");
                logSb.append(ContentImageWorker.formatLogInfoPath(bufImgPath));
                if (targetDirectory != null) {
                    logSb.append(" under ");
                    logSb.append(ContentImageWorker.formatLogInfoPath(targetDirectory));
                }
                if (deleteOld) {
                    logSb.append(" (");
                    logSb.append(imageDeleteCount);
                    logSb.append(" deleted)");
                }
                if (imgUrlMap.size() > 0) {
                    logSb.append(" (sizes: ");
                    final String sizeSep = ", ";
                    for(String sizeType : imgUrlMap.keySet()) {
                        logSb.append(sizeType);
                        logSb.append(sizeSep);
                    }
                    logSb.setLength(logSb.length() - sizeSep.length());
                    logSb.append(")");
                }
                Debug.logInfo(logSb.toString(), module);
                //}
                
                Map<String, Object> result = ServiceUtil.returnSuccess();
                result.put("imageUrlMap", imgUrlMap);
                result.put("bufferedImage", resultBufImgMap.get("bufferedImage"));
                return result;
            } else {
                Debug.logError(logPrefix+UtilProperties.getMessage(resourceProduct, "ScaleImage.unable_to_scale_original_image", LOG_LANG) + ": " + imageOrigFn, module);
                return ServiceUtil.returnError(UtilProperties.getMessage(resourceProduct, "ScaleImage.unable_to_scale_original_image", locale) + " : " + imageOrigFn);
            }
        } catch(Exception e) {
            // FIXME?: more generic err msg
            Debug.logError(e, logPrefix+UtilProperties.getMessage(resourceProduct, "ScaleImage.unable_to_scale_original_image", LOG_LANG) + ": " + imageOrigFn + ": " + e.getMessage(), module);
            return ServiceUtil.returnError(UtilProperties.getMessage(resourceProduct, "ScaleImage.unable_to_scale_original_image", locale) + " : " + imageOrigFn);
        }
    }
    
    private static String expandImageFnFmt(FlexibleStringExpander exdr, String sizeType, Map<String, ?> context) {
        Map<String, Object> fnContext = new HashMap<>(context);
        fnContext.put("sizetype", sizeType);
        fnContext.put("type", sizeType);
        String newFileLocation = exdr.expandString(fnContext);
        if (newFileLocation.startsWith("/")) newFileLocation = newFileLocation.substring(1); // SCIPIO
        return newFileLocation;
    }
    
    private static String getExpandedFnFmtDirPrefix(String newFileLocation) {
        String newFilePathDirPrefix = "";
        if (newFileLocation.lastIndexOf("/") != -1) {
            newFilePathDirPrefix = newFileLocation.substring(0, newFileLocation.lastIndexOf("/") + 1); // adding 1 to include the trailing slash
        }
        return newFilePathDirPrefix;
    }
    
    /**
     * Core database image resizing service.
     * See contentImageDbScaleInAllSizeCore service interface for context params.
     * <p>
     * TODO: missing support to automatically reuse records for sizeTypes having same dimensions (yes happens)
     */
    public static Map<String, Object> contentImageDbScaleInAllSizeCore(DispatchContext dctx, Map<String, ?> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        GenericValue userLogin = (GenericValue) context.get("userLogin");
        TimeZone timeZone = (TimeZone) context.get("timeZone");
        String imageOrigContentId = (String) context.get("imageOrigContentId");
        String imageOrigFullFn = (String) context.get("imageOrigFn");
        //String imageOrigPath = (String) context.get("imageOrigPath"); // TODO?
        String imagePropXmlPath = (String) context.get("imagePropXmlPath");
        Collection<String> sizeTypeList = UtilGenerics.checkList(context.get("sizeTypeList"));
        //boolean copyOrig = Boolean.TRUE.equals(context.get("copyOrig"));
        boolean deleteOld = Boolean.TRUE.equals(context.get("deleteOld"));
        Map<String, Object> scalingOptions = UtilGenerics.checkMap(context.get("scalingOptions"));
        Map<String, Object> contentFields = UtilGenerics.checkMap(context.get("contentFields"));
        Map<String, Object> dataResourceFields = UtilGenerics.checkMap(context.get("dataResourceFields"));
        Map<String, Map<String, Object>> contentFieldsMap = UtilGenerics.checkMap(context.get("contentFieldsMap"));
        if (contentFieldsMap == null) contentFieldsMap = Collections.emptyMap();
        Map<String, Map<String, Object>> dataResourceFieldsMap = UtilGenerics.checkMap(context.get("dataResourceFieldsMap"));
        if (dataResourceFieldsMap == null) dataResourceFieldsMap = Collections.emptyMap();
        String fileSizeDataResAttrName = (String) context.get("fileSizeDataResAttrName");
        String targetFmtExt = (String) context.get("targetFmtExt");
        String contentAssocTypeIdExprStr = (String) context.get("contentAssocTypeId");
        Locale locale = (Locale) context.get("locale");
        if (locale == null) locale = Locale.getDefault();
        
        //final String origSizeType = ContentImageWorker.ORIGINAL_SIZETYPE;
        final String logPrefix = "contentImageDbScaleInAllSizeCore: ";

        long startTime = System.nanoTime();
        
        // USE SAME CREATED DATE FOR EVERYTHING RELATED
        Timestamp createdDate = (Timestamp) context.get("createdDate");
        if (createdDate == null) createdDate = UtilDateTime.nowTimestamp();
        
        try {
            // SCIPIO: for these we now support component:// and file:// prefix in addition to plain absolute file location
//            if (UtilValidate.isNotEmpty(imageOrigPath)) {
//                imageOrigPath = FlexibleLocation.resolveFileUrlAsPathIfUrl(imageOrigPath, imageOrigPath);
//            }
            
            /* ImageProperties.xml */
            ImageVariantConfig imgPropCfg;
            if (UtilValidate.isEmpty(imagePropXmlPath)) {
                imagePropXmlPath = ContentImageWorker.getContentImagePropertiesPath();
            }
            try {
                imgPropCfg = ImageVariantConfig.fromImagePropertiesXml(imagePropXmlPath, locale);
            } catch(Exception e) {
                Debug.logError(logPrefix+UtilProperties.getMessage(resourceProduct, "ScaleImage.unable_to_parse", LOG_LANG) + " : " + imagePropXmlPath + " : " + e.getMessage(), module);
                return ServiceUtil.returnError(UtilProperties.getMessage(resourceProduct, "ScaleImage.unable_to_parse", locale) + " : " + imagePropXmlPath + " : " + e.getMessage());
            }
            if (sizeTypeList == null) {
                sizeTypeList = imgPropCfg.getVariantNames();
            }
            
            /* IMAGE */

            GenericValue origContent = delegator.findOne("Content", UtilMisc.toMap("contentId", imageOrigContentId), false);
            if (origContent == null) {
                Debug.logError(logPrefix+UtilProperties.getMessage(resourceProduct, "ScaleImage.unable_to_parse", LOG_LANG) + " : Content: contentId: " + imageOrigContentId, module);
                return ServiceUtil.returnError(UtilProperties.getMessage(resourceProduct, "ScaleImage.unable_to_parse", locale) + " : Content: contentId: " + imageOrigContentId);
            }
            String origImageDataResourceId = origContent.getString("dataResourceId");
            GenericValue origDataResource = delegator.findOne("DataResource", UtilMisc.toMap("dataResourceId", origImageDataResourceId), false);
            if (origDataResource == null) {
                Debug.logError(logPrefix+UtilProperties.getMessage(resourceProduct, "ScaleImage.unable_to_parse", LOG_LANG) + " : DataResource: dataResourceId: " + origImageDataResourceId + " (contentId: " + imageOrigContentId + ")", module);
                return ServiceUtil.returnError(UtilProperties.getMessage(resourceProduct, "ScaleImage.unable_to_parse", locale) + " : DataResource: dataResourceId: " + origImageDataResourceId + " (contentId: " + imageOrigContentId + ")");
            }
            
            // old code: use getDataResourceStream for this instead, much more versatile
//            // FIXME: we should support images stored in filesystem here, but only ImageDataResource for now
//            if (!"IMAGE_OBJECT".equals(origDataResource.getString("dataResourceTypeId"))) {
//                Debug.logError(logPrefix+UtilProperties.getMessage(resourceProduct, "ScaleImage.unable_to_parse", LOG_LANG) + " : DataResource: dataResourceTypeId != IMAGE_OBJECT (dataResourceId: " + origImageDataResourceId + ") (contentId: " + imageOrigContentId + ")", module);
//                return ServiceUtil.returnError(UtilProperties.getMessage(resourceProduct, "ScaleImage.unable_to_parse", locale) + " : DataResource: dataResourceTypeId != IMAGE_OBJECT (dataResourceId: " + origImageDataResourceId + ") (contentId: " + imageOrigContentId + ")");
//            }
//            GenericValue origImageDataResource = delegator.findOne("ImageDataResource", UtilMisc.toMap("dataResourceId", origImageDataResourceId), false);
//            if (origImageDataResource == null) {
//                Debug.logError(logPrefix+UtilProperties.getMessage(resourceProduct, "ScaleImage.unable_to_parse", LOG_LANG) + " : ImageDataResource (dataResourceId: " + origImageDataResourceId + ") (contentId: " + imageOrigContentId + ")", module);
//                return ServiceUtil.returnError(UtilProperties.getMessage(resourceProduct, "ScaleImage.unable_to_parse", locale) + " : ImageDataResource (dataResourceId: " + origImageDataResourceId + ") (contentId: " + imageOrigContentId + ")");
//            }
//            
//            byte[] bytes = origImageDataResource.getBytes("imageData");
//            if (bytes == null) {
//                Debug.logError(logPrefix+UtilProperties.getMessage(resourceProduct, "ScaleImage.unable_to_parse", LOG_LANG) + " : ImageDataResource.imageData null (dataResourceId: " + origImageDataResourceId + ") (contentId: " + imageOrigContentId + ")", module);
//                return ServiceUtil.returnError(UtilProperties.getMessage(resourceProduct, "ScaleImage.unable_to_parse", locale) + " : ImageDataResource.imageData null (dataResourceId: " + origImageDataResourceId + ") (contentId: " + imageOrigContentId + ")");
//            }
//            
            
            Map<String, Object> streamResult = DataResourceWorker.getDataResourceStream(origDataResource, "false", null, locale, "/", false);
            BufferedImage bufImg;
            InputStream stream = (InputStream) streamResult.get("stream");
            try {
                bufImg = ImageIO.read(stream);
            } catch(Exception e) {
                Debug.logError(logPrefix+UtilProperties.getMessage(resourceProduct, "ScaleImage.unable_to_parse", LOG_LANG) + " : ImageDataResource.imageData (dataResourceId: " + origImageDataResourceId + ") (contentId: " + imageOrigContentId + ")", module);
                return ServiceUtil.returnError(UtilProperties.getMessage(resourceProduct, "ScaleImage.unable_to_parse", locale) + " : ImageDataResource.imageData (dataResourceId: " + origImageDataResourceId + ") (contentId: " + imageOrigContentId + ")");
            }   finally {
                stream.close();
            }
            
            // get Dimensions
            double imgHeight = bufImg.getHeight();
            double imgWidth = bufImg.getWidth();
            if (imgHeight == 0.0 || imgWidth == 0.0) {
                Debug.logError(logPrefix+UtilProperties.getMessage(resourceProduct, "ScaleImage.one_current_image_dimension_is_null", LOG_LANG) + " : imgHeight = " + imgHeight + " ; imgWidth = " + imgWidth, module);
                return ServiceUtil.returnError(UtilProperties.getMessage(resourceProduct, "ScaleImage.one_current_image_dimension_is_null", locale) + " : imgHeight = " + imgHeight + " ; imgWidth = " + imgWidth);
            }
            
            String imageOrigFullFnNoExt;
            if (UtilValidate.isNotEmpty(imageOrigFullFn)) {
                if (imageOrigFullFn.lastIndexOf(".") <= 0 || imageOrigFullFn.lastIndexOf(".") >= (imageOrigFullFn.length() - 1)) { // SCIPIO: added this to prevent problems
                    throw new IllegalArgumentException("Original image filename [" + imageOrigFullFn + "] has missing or improper file extension (image type)");
                }
                if (UtilValidate.isEmpty(targetFmtExt)) {
                    targetFmtExt = imageOrigFullFn.substring(imageOrigFullFn.lastIndexOf(".") + 1);
                }
                imageOrigFullFnNoExt = imageOrigFullFn.substring(0, imageOrigFullFn.lastIndexOf("."));
            } else {
                imageOrigFullFn = origDataResource.getString("objectInfo");
                if (UtilValidate.isEmpty(imageOrigFullFn)) {
                    Debug.logWarning(logPrefix+"no original image filename available (dataResourceId: " + origImageDataResourceId + ")", module);
                    imageOrigFullFn = "";
                }
                if (imageOrigFullFn != null && imageOrigFullFn.lastIndexOf(".") >= 1) {
                    imageOrigFullFnNoExt = imageOrigFullFn.substring(0, imageOrigFullFn.lastIndexOf("."));
                } else {
                    imageOrigFullFnNoExt = imageOrigFullFn;
                }
            }
            String imageOrigFnNoExt = imageOrigFullFnNoExt;
            if (imageOrigFnNoExt.lastIndexOf("/") >= 0) {
                imageOrigFnNoExt = imageOrigFnNoExt.substring(imageOrigFnNoExt.lastIndexOf("/") + 1);
            }
            
            // get target type (assumed same as extension)
            String mimeTypeId;
            if (UtilValidate.isEmpty(targetFmtExt)) {
                mimeTypeId = origDataResource.getString("mimeTypeId");
                if (UtilValidate.isEmpty(mimeTypeId)) {
                    Debug.logError(logPrefix+"can't determine output format (no targetFormatName or DataResource.mimeTypeId) (dataResourceId: " + origImageDataResourceId + ")", module);
                    return ServiceUtil.returnError("can't determine output format (no targetFormatName or DataResource.mimeTypeId) (dataResourceId: " + origImageDataResourceId + ")");
                }
                
                List<GenericValue> fileExtValues = FastList.newInstance();
                try {
                    fileExtValues = EntityQuery.use(delegator).from("FileExtension").where("mimeTypeId", mimeTypeId).queryList();
                    if (UtilValidate.isEmpty(fileExtValues)) {
                        
                    }
                    targetFmtExt = fileExtValues.get(0).getString("fileExtensionId");
                    if (fileExtValues.size() > 1) {
                        Debug.logWarning(logPrefix+"multiple FileExtension found for mimeTypeId '" + mimeTypeId + "'; using first: '" + targetFmtExt + "' (dataResourceId: " + origImageDataResourceId + ")", module);
                    }
                } catch (GenericEntityException e) {
                    Debug.logError(e, logPrefix+"can't determine output format from DataResource.mimeTypeId) (dataResourceId: " + origImageDataResourceId + "): " + e.getMessage(), module);
                    return ServiceUtil.returnError(e.getMessage());
                }
            } else {
                try {
                    GenericValue fileExt = EntityQuery.use(delegator).from("FileExtension").where("fileExtensionId", targetFmtExt).queryOne();
                    if (UtilValidate.isEmpty(fileExt)) {
                        Debug.logError(logPrefix+"no FileExtension (mime-type assoc) record for extension: " + targetFmtExt, module);
                        return ServiceUtil.returnError("No FileExtension (mime-type assoc) record for extension: " + targetFmtExt);
                    }
                    mimeTypeId = fileExt.getString("mimeTypeId");
                } catch (GenericEntityException e) {
                    Debug.logError(e, logPrefix+"can't determine output format from DataResource.mimeTypeId) (dataResourceId: " + origImageDataResourceId + "): " + e.getMessage(), module);
                    return ServiceUtil.returnError(e.getMessage());
                }
            }
            
            Map<String, String> imageContentIdMap = new HashMap<>();

            int imageDeleteCount = 0;
            if (deleteOld) {
                for(String contentIdToRemove : ContentImageWorker.getResizedImageContentAssocContentIdTo(delegator, imageOrigContentId, false)) {
                    // NOTE: this automatically removes the ContentAssoc
                    Map<String, Object> servCtx = new HashMap<>();
                    servCtx.put("userLogin", userLogin);
                    servCtx.put("locale", locale);
                    servCtx.put("timeZone", timeZone);
                    servCtx.put("contentId", contentIdToRemove);
                    try {
                        Map<String, Object> contentResult = dispatcher.runSync("removeContentAndRelated", servCtx);
                        if (!ServiceUtil.isSuccess(contentResult)) {
                            return ServiceUtil.returnError("Error removing image content: " + ServiceUtil.getErrorMessage(contentResult));
                        }
                    } catch (GenericServiceException e) {
                        Debug.logError(e, "Cms: Error removing image content: " + e.getMessage(), module);
                        return ServiceUtil.returnError(e.getMessage());
                    }
                }
            }

            int imageCopyCount = 0;
            // not supporting this for now
//            if (copyOrig) {
//                String sizeType = origSizeType;
//                
//                GenericValue dataResource = delegator.makeValue("DataResource");
//                dataResource.put("dataResourceTypeId", "IMAGE_OBJECT");
//                dataResource.put("dataResourceName", contentName); // TODO: REVIEW
//                dataResource.put("statusId", "CTNT_IN_PROGRESS");
//                dataResource.put("mimeTypeId", mimeType.getString("mimeTypeId"));
//                //dataResource.put("isPublic", "N");
//                dataResource.put("objectInfo", fileName);
//                dataResource.put("createdDate", createdDate);
//                delegator.createSetNextSeqId(dataResource);
//
//                if (UtilValidate.isNotEmpty(fileSizeDataResAttrName)) {
//                    GenericValue fileSizeDataResourceAttr = delegator.makeValue("DataResourceAttribute");
//                    fileSizeDataResourceAttr.put("dataResourceId", dataResource.get("dataResourceId"));
//                    fileSizeDataResourceAttr.put("attrName", fileSizeDataResAttrName);
//                    fileSizeDataResourceAttr.put("attrValue", String.valueOf(fileSizeConverted));
//                    fileSizeDataResourceAttr.create();
//                }
//                
//                String dataResourceId = (String) dataResource.get("dataResourceId");
//
//                String newOrigContentId = null; // TODO
//                    
//                imageContentIdMap.put(sizeType, newOrigContentId);
//            }
            
            /* Scale image for each size from ImageProperties.xml */
            
            Map<String, Object> imageCtx = new HashMap<>();
            imageCtx.put("ext", targetFmtExt);
            imageCtx.put("origfn", imageOrigFnNoExt);
            imageCtx.put("origfnnodir", imageOrigFullFnNoExt);
            Map<String, Object> fieldsCtx = new HashMap<>();
            fieldsCtx.putAll(origContent);
            fieldsCtx.putAll(origDataResource); // TODO: REVIEW: possible name clashes...
            imageCtx.put("fields", fieldsCtx);
            
            FlexibleStringExpander contentAssocTypeIdExdr;
            if (UtilValidate.isNotEmpty(contentAssocTypeIdExprStr)) {
                contentAssocTypeIdExdr = FlexibleStringExpander.getInstance(contentAssocTypeIdExprStr);
            } else {
                contentAssocTypeIdExdr = ContentImageWorker.IMGSZ_CNTASSTYPEID_EXPR;
            }
            
            int scaledImageCount = 0;
            for (String sizeType : sizeTypeList) {
                if (!imgPropCfg.hasVariant(sizeType)) {
                    Debug.logError(logPrefix+"sizeType " + sizeType + " is not part of ImageProperties.xml; ignoring", module);
                    continue;
                }
                
                // Scale
                Map<String, Object> resultScaleImgMap = ImageTransform.scaleImage(bufImg, imgHeight, imgWidth, imgPropCfg.getVariantStringMap(), sizeType, locale, scalingOptions);

                /* Write the new image file */
                if ("success".equals(resultScaleImgMap.get("responseMessage"))) {
                    BufferedImage bufNewImg = (BufferedImage) resultScaleImgMap.get("bufferedImage");

                    imageCtx.put("sizetype", sizeType);
                    imageCtx.put("type", sizeType);
                    
                    GenericValue dataResource = delegator.makeValue("DataResource");
                    dataResource.put("dataResourceTypeId", "IMAGE_OBJECT");
                    dataResource.put("createdDate", createdDate);
                    dataResource.put("mimeTypeId", mimeTypeId);
                    // caller should determine theses...
                    //dataResource.put("statusId", origDataResource.get("statusId")); 
                    //dataResource.put("isPublic", "N");
                    
                    // SCIPIO: 2017-08-11: now store width & height in new DataResource fields,
                    // due to very high probability we will need these, and with decent access speed.
                    dataResource.put("scpWidth", (long) bufNewImg.getWidth());
                    dataResource.put("scpHeight", (long) bufNewImg.getHeight());
                    
                    Map<String, Object> customDrFields = new HashMap<>();
                    customDrFields.putAll(ContentImageWorker.RESIZEIMG_DATARESOURCE_FIELDEXPR);
                    if (dataResourceFieldsMap.get(sizeType) != null) {
                        customDrFields.putAll(dataResourceFieldsMap.get(sizeType));
                    } else if (dataResourceFields != null) {
                        customDrFields.putAll(dataResourceFields);
                    }
                    // interpret flexible expressions for fields where we support it
                    customDrFields = ContentImageWorker.parseMapFieldExpr(customDrFields, imageCtx, timeZone, locale);

                    dataResource.setNonPKFields(customDrFields);
                    dataResource = delegator.createSetNextSeqId(dataResource);
                    String dataResourceId = dataResource.getString("dataResourceId");

                    byte[] byteout;
                    ByteArrayOutputStream byteos = new ByteArrayOutputStream();
                    try {
                        ImageIO.write(bufNewImg, targetFmtExt, byteos);
                        byteout = byteos.toByteArray();
                        scaledImageCount++;
                    } catch (IllegalArgumentException e) {
                        Debug.logError(e, logPrefix+UtilProperties.getMessage(resourceProduct, "ScaleImage.one_parameter_is_null", LOG_LANG) + ": " + e.getMessage(), module);
                        return ServiceUtil.returnError(UtilProperties.getMessage(resourceProduct, "ScaleImage.one_parameter_is_null", locale));
                    } catch (IOException e) {
                        Debug.logError(e, logPrefix+UtilProperties.getMessage(resourceProduct, "ScaleImage.error_occurs_during_writing", LOG_LANG) + ": " + e.getMessage(), module);
                        return ServiceUtil.returnError(UtilProperties.getMessage(resourceProduct, "ScaleImage.error_occurs_during_writing", locale));
                    } finally {
                        byteos.close();
                    }
                    
                    GenericValue imageDataResource = delegator.makeValue("ImageDataResource");
                    imageDataResource.put("imageData", byteout);
                    imageDataResource.put("dataResourceId", dataResource.get("dataResourceId"));
                    imageDataResource.create();
                    
                    if (UtilValidate.isNotEmpty(fileSizeDataResAttrName)) {
                        GenericValue fileSizeDataResourceAttr = delegator.makeValue("DataResourceAttribute");
                        fileSizeDataResourceAttr.put("dataResourceId", dataResourceId);
                        fileSizeDataResourceAttr.put("attrName", fileSizeDataResAttrName);
                        fileSizeDataResourceAttr.put("attrValue", String.valueOf(byteout.length));
                        fileSizeDataResourceAttr = fileSizeDataResourceAttr.create();
                    }
                    
                    GenericValue content = delegator.makeValue("Content");
                    content.put("createdDate", createdDate);
                    
                    Map<String, Object> customCoFields = new HashMap<>();
                    customCoFields.putAll(ContentImageWorker.RESIZEIMG_CONTENT_FIELDEXPR);
                    if (contentFieldsMap.get(sizeType) != null) {
                        customCoFields.putAll(contentFieldsMap.get(sizeType));
                    } else if (contentFields != null) {
                        customCoFields.putAll(contentFields);
                    }
                    // interpret flexible expressions for fields where we support it
                    customCoFields = ContentImageWorker.parseMapFieldExpr(customCoFields, imageCtx, timeZone, locale);
                    
                    content.setNonPKFields(customCoFields);
                    content.put("dataResourceId", dataResourceId);
                    content = delegator.createSetNextSeqId(content);
                    String resContentId = content.getString("contentId");
                    
                    String contentAssocTypeId = contentAssocTypeIdExdr.expandString(imageCtx).toUpperCase();
                    if (delegator.findOne("ContentAssocType", UtilMisc.toMap("contentAssocTypeId", contentAssocTypeId), false) == null) {
                        Debug.logInfo(logPrefix+"ContentAssocType for contentAssocTypeId '" + contentAssocTypeId
                                + "' does not yet exist; automatically creating...", module);
                        GenericValue contentAssocType = delegator.makeValue("ContentAssocType");
                        contentAssocType.put("contentAssocTypeId", contentAssocTypeId);
                        contentAssocType.put("description", "Image - " + sizeType.substring(0, 1).toUpperCase() + sizeType.substring(1).toLowerCase());
                        contentAssocType = contentAssocType.create();
                    }

                    GenericValue contentAssoc = delegator.makeValue("ContentAssoc");
                    contentAssoc.put("contentId", imageOrigContentId);
                    contentAssoc.put("contentIdTo", resContentId);
                    contentAssoc.put("contentAssocTypeId", contentAssocTypeId);
                    contentAssoc.put("fromDate", createdDate);
                    contentAssoc.put("mapKey", sizeType);
                    contentAssoc = contentAssoc.create();
                    
                    imageContentIdMap.put(sizeType, resContentId);
                } else {
                    // SCIPIO: new
                    Debug.logError(logPrefix+ServiceUtil.getErrorMessage(resultScaleImgMap), module);
                    return ServiceUtil.returnError(ServiceUtil.getErrorMessage(resultScaleImgMap));
                }
            } // Loop over sizeType

            // save the name of the image props def we used
            GenericValue contentAttr = delegator.findOne("ContentAttribute", 
                    UtilMisc.toMap("contentId", imageOrigContentId, "attrName", ContentImageWorker.CONTENTATTR_VARIANTCFG), false);
            if (contentAttr == null) {
                contentAttr = delegator.makeValue("ContentAttribute", 
                    UtilMisc.toMap("contentId", imageOrigContentId, "attrName", ContentImageWorker.CONTENTATTR_VARIANTCFG, "attrValue", imagePropXmlPath));
                contentAttr.create();
            } else {
                contentAttr.put("attrValue", imagePropXmlPath);
                contentAttr.store();
            }
            
            // this is helpful info and doesn't do much harm
            //if (ImageUtil.verboseOn()) {
            long endTime = System.nanoTime();
            StringBuilder logSb = new StringBuilder(logPrefix);
            logSb.append("In ");
            logSb.append((endTime - startTime) / 1000000);
            logSb.append("ms created ");
            logSb.append(scaledImageCount);
            logSb.append(" scaled and ");
            logSb.append(imageCopyCount);
            logSb.append(" original copies of image for contentId ");
            logSb.append(imageOrigContentId);
            if (deleteOld) {
                logSb.append(" (");
                logSb.append(imageDeleteCount);
                logSb.append(" deleted)");
            }
            if (imageContentIdMap.size() > 0) {
                logSb.append(" (sizes: ");
                final String sizeSep = ", ";
                for(String sizeType : imageContentIdMap.keySet()) {
                    logSb.append(sizeType);
                    logSb.append(sizeSep);
                }
                logSb.setLength(logSb.length() - sizeSep.length());
                logSb.append(")");
            }
            Debug.logInfo(logSb.toString(), module);
            //}
            
            Map<String, Object> result = ServiceUtil.returnSuccess();
            result.put("imageContentIdMap", imageContentIdMap);
            result.put("bufferedImage", bufImg);
            return result;

        } catch(Exception e) {
            // FIXME?: more generic err msg
            Debug.logError(e, logPrefix+UtilProperties.getMessage(resourceProduct, "ScaleImage.unable_to_scale_original_image", LOG_LANG) + ": " + imageOrigContentId + ": " + e.getMessage(), module);
            return ServiceUtil.returnError(UtilProperties.getMessage(resourceProduct, "ScaleImage.unable_to_scale_original_image", locale) + " : " + imageOrigContentId);
        }
    }

}
