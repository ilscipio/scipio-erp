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
import java.io.File;
import java.io.IOException;
import java.util.Collection;
import java.util.HashMap;
import java.util.Locale;
import java.util.Map;

import javax.imageio.ImageIO;

import org.apache.commons.io.FileUtils;
import org.ofbiz.base.location.FlexibleLocation;
import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.UtilGenerics;
import org.ofbiz.base.util.UtilProperties;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.base.util.string.FlexibleStringExpander;
import org.ofbiz.common.image.ImageTransform;
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.util.EntityUtilProperties;
import org.ofbiz.service.DispatchContext;
import org.ofbiz.service.ServiceUtil;

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
            Map<String, Map<String, String>> imgPropertyMap = new HashMap<>();
            String imagePropXmlPathFull;
            if (UtilValidate.isNotEmpty(imagePropXmlPath)) {
                imagePropXmlPathFull = ContentImageWorker.getImagePropertiesFullPath(imagePropXmlPath);
            } else {
                imagePropXmlPathFull = ContentImageWorker.getContentImagePropertiesFullPath();
            }
            Map<String, Object> resultXMLMap = ImageTransform.getXMLValue(imagePropXmlPathFull, locale);
            if ("success".equals(resultXMLMap.get("responseMessage"))) {
                imgPropertyMap.putAll(UtilGenerics.<Map<String, Map<String, String>>>cast(resultXMLMap.get("xml")));
            } else {
                Debug.logError(logPrefix+UtilProperties.getMessage(resourceProduct, "ScaleImage.unable_to_parse", LOG_LANG) + " : ImageProperties.xml", module);
                return ServiceUtil.returnError(UtilProperties.getMessage(resourceProduct, "ScaleImage.unable_to_parse", locale) + " : ImageProperties.xml");
            }
            if (sizeTypeList == null) {
                sizeTypeList = imgPropertyMap.keySet();
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
                    if (!imgPropertyMap.containsKey(sizeType)) {
                        Debug.logError(logPrefix+"sizeType " + sizeType + " is not part of ImageProperties.xml; ignoring", module);
                        continue;
                    }
                    
                    // Scale
                    Map<String, Object> resultScaleImgMap = ImageTransform.scaleImage(bufImg, imgHeight, imgWidth, imgPropertyMap, sizeType, locale, scalingOptions);
    
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
                        
                    } // scaleImgMap
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
     */
    public static Map<String, Object> contentImageDbScaleInAllSizeCore(DispatchContext dctx, Map<String, ?> context) {
        Delegator delegator = dctx.getDelegator();
        String imageOrigContentId = (String) context.get("imageOrigContentId");
        //String imageOrigPath = (String) context.get("imageOrigPath"); // TODO?
        String imagePropXmlPath = (String) context.get("imagePropXmlPath");
        Collection<String> sizeTypeList = UtilGenerics.checkList(context.get("sizeTypeList"));
        boolean copyOrig = Boolean.TRUE.equals(context.get("copyOrig"));
        boolean deleteOld = Boolean.TRUE.equals(context.get("deleteOld"));
        Map<String, Object> scalingOptions = UtilGenerics.checkMap(context.get("scalingOptions"));
        Locale locale = (Locale) context.get("locale");
        if (locale == null) locale = Locale.getDefault();
        
        final String origSizeType = ContentImageWorker.ORIGINAL_SIZETYPE;
        final String logPrefix = "contentImageDbScaleInAllSizeCore: ";

        long startTime = System.nanoTime();
        
        try {
            // SCIPIO: for these we now support component:// and file:// prefix in addition to plain absolute file location
//            if (UtilValidate.isNotEmpty(imageOrigPath)) {
//                imageOrigPath = FlexibleLocation.resolveFileUrlAsPathIfUrl(imageOrigPath, imageOrigPath);
//            }
            
            /* ImageProperties.xml */
            Map<String, Map<String, String>> imgPropertyMap = new HashMap<>();
            String imagePropXmlPathFull;
            if (UtilValidate.isNotEmpty(imagePropXmlPath)) {
                imagePropXmlPathFull = ContentImageWorker.getImagePropertiesFullPath(imagePropXmlPath);
            } else {
                imagePropXmlPathFull = ContentImageWorker.getContentImagePropertiesFullPath();
            }
            Map<String, Object> resultXMLMap = ImageTransform.getXMLValue(imagePropXmlPathFull, locale);
            if ("success".equals(resultXMLMap.get("responseMessage"))) {
                imgPropertyMap.putAll(UtilGenerics.<Map<String, Map<String, String>>>cast(resultXMLMap.get("xml")));
            } else {
                Debug.logError(logPrefix+UtilProperties.getMessage(resourceProduct, "ScaleImage.unable_to_parse", LOG_LANG) + " : ImageProperties.xml", module);
                return ServiceUtil.returnError(UtilProperties.getMessage(resourceProduct, "ScaleImage.unable_to_parse", locale) + " : ImageProperties.xml");
            }
            if (sizeTypeList == null) {
                sizeTypeList = imgPropertyMap.keySet();
            }
            
            /* IMAGE */

            // paths
            
            /* get original BUFFERED IMAGE */
            Map<String, Object> resultBufImgMap = ContentImageWorker.getBufferedImageFromContentId(imageOrigContentId, locale);
            
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
                
                Map<String, String> imageRecordMap = new HashMap<>();
    
                int imageDeleteCount = 0;
                int imageCopyCount = 0;
                if (copyOrig) {
                    String sizeType = origSizeType;
                    
                    String resContentId = null; // TODO
                        
                    imageRecordMap.put(sizeType, resContentId);
                }
                
                /* Scale image for each size from ImageProperties.xml */
                int scaledImageCount = 0;
                for (String sizeType : sizeTypeList) {
                    if (!imgPropertyMap.containsKey(sizeType)) {
                        Debug.logError(logPrefix+"sizeType " + sizeType + " is not part of ImageProperties.xml; ignoring", module);
                        continue;
                    }
                    
                    // Scale
                    Map<String, Object> resultScaleImgMap = ImageTransform.scaleImage(bufImg, imgHeight, imgWidth, imgPropertyMap, sizeType, locale, scalingOptions);
    
                    /* Write the new image file */
                    if ("success".equals(resultScaleImgMap.get("responseMessage"))) {
                        BufferedImage bufNewImg = (BufferedImage) resultScaleImgMap.get("bufferedImage");
    
                        String resContentId = null; // TODO
                        imageRecordMap.put(sizeType, resContentId);
                        
                    } // scaleImgMap
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
                logSb.append(" original copies of image for contentId ");
                logSb.append(imageOrigContentId);
                if (deleteOld) {
                    logSb.append(" (");
                    logSb.append(imageDeleteCount);
                    logSb.append(" deleted)");
                }
                if (imageRecordMap.size() > 0) {
                    logSb.append(" (sizes: ");
                    final String sizeSep = ", ";
                    for(String sizeType : imageRecordMap.keySet()) {
                        logSb.append(sizeType);
                        logSb.append(sizeSep);
                    }
                    logSb.setLength(logSb.length() - sizeSep.length());
                    logSb.append(")");
                }
                Debug.logInfo(logSb.toString(), module);
                //}
                
                Map<String, Object> result = ServiceUtil.returnSuccess();
                result.put("imageRecordMap", imageRecordMap);
                result.put("bufferedImage", resultBufImgMap.get("bufferedImage"));
                return result;
            } else {
                Debug.logError(logPrefix+UtilProperties.getMessage(resourceProduct, "ScaleImage.unable_to_scale_original_image", LOG_LANG) + ": " + imageOrigContentId, module);
                return ServiceUtil.returnError(UtilProperties.getMessage(resourceProduct, "ScaleImage.unable_to_scale_original_image", locale) + " : " + imageOrigContentId);
            }
        } catch(Exception e) {
            // FIXME?: more generic err msg
            Debug.logError(e, logPrefix+UtilProperties.getMessage(resourceProduct, "ScaleImage.unable_to_scale_original_image", LOG_LANG) + ": " + imageOrigContentId + ": " + e.getMessage(), module);
            return ServiceUtil.returnError(UtilProperties.getMessage(resourceProduct, "ScaleImage.unable_to_scale_original_image", locale) + " : " + imageOrigContentId);
        }
    }
 
}
