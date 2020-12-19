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

import java.awt.image.BufferedImage;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.sql.Timestamp;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Objects;
import java.util.TimeZone;

import javax.imageio.ImageIO;

import com.ilscipio.scipio.common.util.fileType.FileTypeUtil;
import org.apache.commons.io.FileUtils;
import org.ofbiz.base.location.FlexibleLocation;
import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.GeneralException;
import org.ofbiz.base.util.UtilDateTime;
import org.ofbiz.base.util.UtilGenerics;
import org.ofbiz.base.util.UtilMisc;
import org.ofbiz.base.util.UtilProperties;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.base.util.string.FlexibleStringExpander;
import org.ofbiz.common.image.ImageProfile;
import org.ofbiz.common.image.ImageTransform;
import org.ofbiz.common.image.ImageVariantConfig;
import org.ofbiz.common.image.storer.ImageStorers;
import org.ofbiz.content.data.DataResourceWorker;
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.GenericEntityException;
import org.ofbiz.entity.GenericValue;
import org.ofbiz.entity.condition.EntityCondition;
import org.ofbiz.entity.condition.EntityOperator;
import org.ofbiz.entity.util.EntityQuery;
import org.ofbiz.entity.util.EntityUtil;
import org.ofbiz.entity.util.EntityUtilProperties;
import org.ofbiz.service.DispatchContext;
import org.ofbiz.service.GenericServiceException;
import org.ofbiz.service.LocalDispatcher;
import org.ofbiz.service.ServiceContext;
import org.ofbiz.service.ServiceUtil;
import org.ofbiz.service.ServiceValidationException;

/**
 * SCIPIO: Content/generic image services.
 * FIXME: MISSING DELETION CODE LOGIC
 * Added 2017-07-04.
 * <p->
 * Derived from:
 * <ul>
 * <li><code>org.ofbiz.product.image.ScaleImage</code></li>
 * <li><code>org.ofbiz.product.imagemanagement.ImageManagementServices#scaleImageMangementInAllSize</code></li>
 * <li>(potentially more, please update list as needed to track for maintenance purposes)</li>
 * </ul>
 */
public abstract class ContentImageServices {

    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());

    // SCIPIO: FIXME?: don't really want this dependency, but not major issue
    private static final String resourceProduct = "ProductErrorUiLabels";
    private static final Locale LOG_LANG = Debug.getLogLocale();

    protected ContentImageServices() {
    }

    /**
     * Core image file resizing service.
     * See contentImageFileScaleInAllSizeCore service interface for context params.
     * SCIPIO: NOTE: This was originally a stock service, highly modified.
     * FIXME: MISSING DELETION CODE LOGIC
     */
    public static Map<String, Object> contentImageFileScaleInAllSizeCore(ServiceContext ctx) throws ServiceValidationException {
        DispatchContext dctx = ctx.dctx();
        Map<String, Object> context = ctx.context();
        Delegator delegator = dctx.getDelegator();
        String imageOrigPath = (String) context.get("imageOrigPath");
        String imageOrigUrl = (String) context.get("imageOrigUrl");
        String imageOrigFn = (String) context.get("imageOrigFn");
        String imageServerPath = (String) context.get("imageServerPath");
        String imageUrlPrefix = (String) context.get("imageUrlPrefix");
        String imageFnFmt = (String) context.get("imageFnFmt");
        String imageOrigFnFmt = (String) context.get("imageOrigFnFmt");
        Map<String, Object> imagePathArgs = UtilGenerics.checkMap(context.get("imagePathArgs"));
        if (imagePathArgs == null) {
            imagePathArgs = new HashMap<>();
        }
        String imagePropXmlPath = (String) context.get("imagePropXmlPath");
        Collection<String> sizeTypeList = UtilGenerics.cast(context.get("sizeTypeList"));
        boolean copyOrig = Boolean.TRUE.equals(context.get("copyOrig"));
        boolean deleteOld = Boolean.TRUE.equals(context.get("deleteOld"));
        Map<String, Object> scalingOptions = UtilGenerics.checkMap(context.get("scalingOptions"));
        Locale locale = (Locale) context.get("locale");
        if (locale == null) locale = Locale.getDefault();
        Map<String, Object> imageWriteOptions = UtilGenerics.cast(context.get("imageWriteOptions"));
        final String logPrefix = "contentImageFileScaleInAllSizeCore: ";

        Object imageProfileObj = context.get("imageProfile");
        ImageProfile imageProfile = null;
        if (imageProfileObj instanceof ImageProfile) {
            imageProfile = (ImageProfile) imageProfileObj;
        } else if (imageProfileObj instanceof String) {
            imageProfile = ImageProfile.getImageProfile(delegator, (String) imageProfileObj, false);
            if (imageProfile == null) {
                String errMsg = "Could not find mediaProfile [" + imageProfileObj + "]";
                Debug.logError(logPrefix + errMsg, module);
                return ServiceUtil.returnError(errMsg);
            }
        } else if (imageProfileObj != null) {
            throw new ServiceValidationException("Invalid imageProfile parameter: " + imageProfileObj.getClass().getName(), ctx.getModelService());
        }

        final String origSizeType = ContentImageWorker.ORIGINAL_SIZETYPE;

        long startTime = System.currentTimeMillis();

        try {
            // SCIPIO: for these we now support component:// and file:// prefix in addition to plain absolute file location
            if (UtilValidate.isNotEmpty(imageOrigPath)) {
                imageOrigPath = FlexibleLocation.resolveFileUrlAsPathIfUrl(imageOrigPath, imageOrigPath);
            }
            if (UtilValidate.isNotEmpty(imageServerPath)) {
                imageServerPath = FlexibleLocation.resolveFileUrlAsPathIfUrl(imageServerPath, imageServerPath);
            }

            if (imageProfile == null) {
                imageProfileObj = context.get("defaultImageProfile");
                if (imageProfileObj instanceof ImageProfile) {
                    imageProfile = (ImageProfile) imageProfileObj;
                } else if (imageProfileObj instanceof String) {
                    imageProfile = ImageProfile.getImageProfile(delegator, (String) imageProfileObj, false);
                    if (imageProfile == null) {
                        String errMsg = "Could not find mediaProfile [" + imageProfileObj + "]";
                        Debug.logError(logPrefix + errMsg, module);
                        return ServiceUtil.returnError(errMsg);
                    }
                } else {
                    throw new ServiceValidationException("Invalid or missing defaultImageProfile parameter: " + imageProfileObj.getClass().getName(), ctx.getModelService());
                }
            }

            /* ImageProperties.xml */
            ImageVariantConfig imgPropCfg = (ImageVariantConfig) context.get("imageVariantConfig");
            if (imgPropCfg == null) {
                if (UtilValidate.isNotEmpty(imagePropXmlPath)) {
                    try {
                        imgPropCfg = ImageVariantConfig.fromImagePropertiesXml(imagePropXmlPath, locale);
                    } catch (Exception e) {
                        Debug.logError(logPrefix + UtilProperties.getMessage(resourceProduct, "ScaleImage.unable_to_parse", LOG_LANG) + " : " + imagePropXmlPath + " : " + e.getMessage(), module);
                        return ServiceUtil.returnError(UtilProperties.getMessage(resourceProduct, "ScaleImage.unable_to_parse", locale) + " : " + imagePropXmlPath + " : " + e.getMessage());
                    }
                } else { // SCIPIO
                    imgPropCfg = imageProfile.readVariantConfig(); // NOTE: reads non-cached version
                }
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
            GenericValue origFileExt = EntityQuery.use(delegator).from("FileExtension").where("fileExtensionId", imgExtension).cache().queryOne();
            String origMimeTypeId = null;
            if (origFileExt == null) {
                Debug.logWarning(logPrefix+"Could not find mimeTypeId for file extension [" + imgExtension + "]", module);
            } else {
                origMimeTypeId = origFileExt.getString("mimeTypeId");
            }

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
            String imageUrlPrefixPrefix = ""; // SCIPIO: for special case client data
            if (UtilValidate.isNotEmpty(imageOrigPath)) {
                bufImgPath = imageOrigPath;
            } else if (UtilValidate.isNotEmpty(imageOrigUrl)) {
                // TODO: improve this to support getting URL from any mount-point
                if (imageUrlPrefix.startsWith("//") || imageUrlPrefix.contains("://")) {
                    if (!imageOrigUrl.startsWith(imageUrlPrefix + "/")) {
                        throw new IllegalArgumentException("imageOrigUrl [" + imageOrigUrl + "] does not begin with expected imageUrlPrefix [" + imageUrlPrefix + "]");
                    }
                    bufImgPath = imageServerPath + imageOrigUrl.substring(imageUrlPrefix.length());
                } else {
                    int i = imageOrigUrl.lastIndexOf(imageUrlPrefix + "/");
                    if (i < 0) {
                        throw new IllegalArgumentException("imageOrigUrl [" + imageOrigUrl + "] does not contain expected imageUrlPrefix [']" + imageUrlPrefix + "]");
                    }
                    imageUrlPrefixPrefix = imageOrigUrl.substring(0, i);
                    bufImgPath = imageServerPath + imageOrigUrl.substring(i + imageUrlPrefix.length());
                }
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
                ImageVariantConfig.VariantInfo originalVariantInfo = new ImageVariantConfig.VariantInfo("original",
                        (int) imgWidth, (int) imgHeight, null, null);

                Map<String, String> imgUrlMap = new LinkedHashMap<>();
                Map<String, Map<String, Object>> imgInfoMap = new LinkedHashMap<>(); // SCIPIO

                int imageDeleteCount = 0;
                int imageCopyCount = 0;
                if (copyOrig) {
                    String sizeType = origSizeType;
                    String newFileLocation = expandImageFnFmt(imageOrigFnFmtExpander, sizeType, imagePathArgs);

                    // SCIPIO: Make sure we have a filename
                    if (newFileLocation.isEmpty()) {
                        throw new IllegalArgumentException("Cannot determine newFileLocation for sizeType [" + sizeType + "] using expander [" + imageOrigFnFmtExpander +
                                "] and imagePathArgs " + imagePathArgs + "; empty result");
                    }
                    int lastSlashIndex = newFileLocation.lastIndexOf('/');
                    if (lastSlashIndex < 0 || lastSlashIndex >= (newFileLocation.length() - 1)) {
                        throw new IllegalArgumentException("Cannot determine newFileLocation for sizeType [" + sizeType + "] using expander [" + imageOrigFnFmtExpander +
                                "] and imagePathArgs " + imagePathArgs + "; no filename part: newFileLocation: [" + newFileLocation + "]");
                    }

                    String newFileLocExt = newFileLocation + "." + imgExtension;
                    String newFileFullLoc = imageServerPath + "/" + newFileLocExt;
                    if (bufImgPath.equals(newFileFullLoc)) {
                        Debug.logWarning(logPrefix+"copyOrig was requested, but output orig file would be same as input orig file (" + bufImgPath + ")", module);

                        // put this so the caller gets a URL to the original even if didn't change
                        String imageUrl = imageUrlPrefixPrefix + imageUrlPrefix + "/" + newFileLocExt;
                        imgUrlMap.put(sizeType, imageUrl);

                        Map<String, Object> sizeTypeInfo = new LinkedHashMap<>();
                        sizeTypeInfo.put("sizeType", sizeType);
                        sizeTypeInfo.put("url", imageUrl);
                        sizeTypeInfo.put("variantInfo", originalVariantInfo);
                        sizeTypeInfo.put("width", (int) imgWidth);
                        sizeTypeInfo.put("height", (int) imgHeight);
                        sizeTypeInfo.put("copyOrig", copyOrig);
                        sizeTypeInfo.put("mimeTypeId", origMimeTypeId);
                        imgInfoMap.put(sizeType, sizeTypeInfo);
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

                        String imageUrl = imageUrlPrefixPrefix + imageUrlPrefix + "/" + newFileLocExt;
                        imgUrlMap.put(sizeType, imageUrl);

                        Map<String, Object> sizeTypeInfo = new LinkedHashMap<>();
                        sizeTypeInfo.put("sizeType", sizeType);
                        sizeTypeInfo.put("url", imageUrl);
                        sizeTypeInfo.put("variantInfo", originalVariantInfo);
                        sizeTypeInfo.put("width", (int) imgWidth);
                        sizeTypeInfo.put("height", (int) imgHeight);
                        sizeTypeInfo.put("copyOrig", copyOrig);
                        sizeTypeInfo.put("mimeTypeId", origMimeTypeId);
                        imgInfoMap.put(sizeType, sizeTypeInfo);
                    }
                } else {
                    String sizeType = "original";
                    Map<String, Object> sizeTypeInfo = new LinkedHashMap<>();
                    sizeTypeInfo.put("sizeType", sizeType);
                    if (imageOrigUrl != null) {
                        sizeTypeInfo.put("url", imageOrigUrl);
                    }
                    sizeTypeInfo.put("variantInfo", originalVariantInfo);
                    sizeTypeInfo.put("width", (int) imgWidth);
                    sizeTypeInfo.put("height", (int) imgHeight);
                    sizeTypeInfo.put("copyOrig", copyOrig);
                    sizeTypeInfo.put("mimeTypeId", origMimeTypeId);
                    imgInfoMap.put(sizeType, sizeTypeInfo);
                }

                /* Scale image for each size from ImageProperties.xml */
                int successCount = 0;
                int scaleErrorCount = 0;
                int writeErrorCount = 0;
                int skipCount = 0; // TODO: currently implemented by caller
                for (String sizeType : sizeTypeList) {
                    ImageVariantConfig.VariantInfo variantInfo = imgPropCfg.getVariant(sizeType);
                    if (variantInfo == null) {
                        Debug.logError(logPrefix+"sizeType " + sizeType + " is not part of ImageProperties.xml; ignoring", module);
                        continue;
                    }

                    boolean keepOrig = false;
                    Integer targetWidth = variantInfo.getWidth();
                    Integer targetHeight = variantInfo.getHeight();
                    if (variantInfo.getUpscaleMode() != ImageVariantConfig.VariantInfo.UpscaleMode.ON) {
                        if (targetWidth == (int) imgWidth || targetHeight == (int) imgHeight) {
                            keepOrig = true;
                        } else if (targetWidth > (int) imgWidth && targetHeight > (int) imgHeight) {
                            if (variantInfo.getUpscaleMode() == ImageVariantConfig.VariantInfo.UpscaleMode.OMIT) {
                                continue;
                            } else if (variantInfo.getUpscaleMode() == ImageVariantConfig.VariantInfo.UpscaleMode.OFF) {
                                keepOrig = true;
                            }
                        }
                    }

                    // Scale
                    Map<String, Object> resultScaleImgMap = Collections.emptyMap();
                    if (!keepOrig) {
                        try {
                            resultScaleImgMap = ImageTransform.scaleImage(bufImg, imgHeight, imgWidth, targetHeight.doubleValue(), targetWidth.doubleValue(), locale, scalingOptions);
                            if (!ServiceUtil.isSuccess(resultScaleImgMap)) {
                                String errMsg = "Error scaling image for file [" + bufImgPath + "] sizeType [" + sizeType + "] from [" + imgWidth + "x" + imgHeight + "] to [" + targetWidth + "x" + targetHeight + "]"
                                        + ": " + ServiceUtil.getErrorMessage(resultScaleImgMap);
                                Debug.logError(logPrefix + errMsg, module);
                                scaleErrorCount++;
                                continue;
                            }
                        } catch(Exception e) {
                            String errMsg = "Error scaling image for file [" + bufImgPath + "] sizeType [" + sizeType + "] from [" + imgWidth + "x" + imgHeight + "] to [" + targetWidth + "x" + targetHeight + "]"
                                    + ": " + e.toString();
                            Debug.logError(logPrefix + errMsg, module);
                            scaleErrorCount++;
                            continue;
                        }
                    }

                    /* Write the new image file */
                    if (keepOrig || "success".equals(resultScaleImgMap.get("responseMessage"))) {
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
                        String targetFileType = imgExtension;
                        String mimeTypeId = origMimeTypeId;
                        // SCIPIO: 2020-09: Support for specific storage format
                        if (variantInfo != null && variantInfo.getFormat() != null) {
                            targetFileType = variantInfo.resolveFormatExt(delegator);
                            mimeTypeId = null;
                            GenericValue fileExt = EntityQuery.use(delegator).from("FileExtension").where("fileExtensionId", targetFileType).cache().queryOne();
                            if (fileExt == null) {
                                Debug.logWarning(logPrefix+"Could not find mimeTypeId for file extension [" + targetFileType + "]", module);
                            } else {
                                mimeTypeId = fileExt.getString("mimeTypeId");
                            }
                        }
                        String newFileLocExt = newFileLocation + "." + targetFileType;
                        String newFileFullLoc = imageServerPath + "/" + newFileLocExt;
                        if (keepOrig && Objects.equals(targetFileType, imgExtension)) {
                            try {
                                FileUtils.copyFile(new File(bufImgPath), new File(newFileFullLoc));
                            } catch (IOException e) {
                                Debug.logError(e, logPrefix + UtilProperties.getMessage(resourceProduct, "ScaleImage.error_occurs_during_writing", LOG_LANG) + ": " + e.getMessage(), module);
                                return ServiceUtil.returnError(UtilProperties.getMessage(resourceProduct, "ScaleImage.error_occurs_during_writing", locale));
                            }
                        } else {
                            try {
                                ImageStorers.write(keepOrig ? bufImg : bufNewImg, targetFileType, new File(newFileFullLoc),
                                        (imageProfile != null) ? imageProfile.getName() : null, imageWriteOptions, delegator); // SCIPIO: ImageIO->ImageStorers
                                successCount++;
                            } catch (Exception e) {
                                String errMsg = "Error writing image for file [" + bufImgPath + "] sizeType [" + sizeType + "] from [" + imgWidth + "x" + imgHeight + "] to [" + targetWidth + "x" + targetHeight + "]"
                                        + ": " + e.toString();
                                Debug.logError(logPrefix + errMsg, module);
                                writeErrorCount++;
                                continue;
                            }
                        }

                        // Save each Url
                        String imageUrl = imageUrlPrefixPrefix + imageUrlPrefix + "/" + newFileLocExt;
                        imgUrlMap.put(sizeType, imageUrl);

                        // SCIPIO
                        Map<String, Object> sizeTypeInfo = new LinkedHashMap<>();
                        sizeTypeInfo.put("sizeType", sizeType);
                        sizeTypeInfo.put("url", imageUrl);
                        sizeTypeInfo.put("variantInfo", variantInfo);
                        sizeTypeInfo.put("width", keepOrig ? (int) imgWidth : bufNewImg.getWidth());
                        sizeTypeInfo.put("height", keepOrig ? (int) imgHeight : bufNewImg.getHeight());
                        sizeTypeInfo.put("mimeTypeId", mimeTypeId);
                        imgInfoMap.put(sizeType, sizeTypeInfo);
                    } else {
                        // SCIPIO: new
                        Debug.logError(logPrefix+ServiceUtil.getErrorMessage(resultScaleImgMap), module);
                        return ServiceUtil.returnError(ServiceUtil.getErrorMessage(resultScaleImgMap));
                    }
                } // Loop over sizeType

                // this is helpful info and doesn't do much harm
                //if (ImageUtil.verboseOn()) {
                long endTime = System.currentTimeMillis();
                StringBuilder logSb = new StringBuilder();
                logSb.append("In ");
                logSb.append((endTime - startTime) / 1000000);
                logSb.append("ms processed ");
                logSb.append(successCount);
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
                logSb.append(" (success: ");
                logSb.append(successCount);
                logSb.append(", scaling errors: ");
                logSb.append(scaleErrorCount);
                logSb.append(", write errors: ");
                logSb.append(writeErrorCount);
                logSb.append(", skipped: ");
                logSb.append(skipCount);
                logSb.append(")");
                String msg = logSb.toString();
                //}

                int failCount = scaleErrorCount + writeErrorCount;
                Map<String, Object> result;
                if (failCount > 0) {
                    if (successCount > 0) {
                        result = ServiceUtil.returnFailure(msg);
                    } else {
                        // TODO: REVIEW: for now let caller decide to abort trans
                        //result = ServiceUtil.returnError(msg);
                        result = ServiceUtil.returnFailure(msg);
                    }
                    Debug.logError(logPrefix + msg, module);
                } else {
                    result = ServiceUtil.returnSuccess(msg);
                    Debug.logInfo(logPrefix + msg, module);
                }
                result.put("imageUrlMap", imgUrlMap);
                result.put("imageInfoMap", imgInfoMap); // SCIPIO
                result.put("bufferedImage", resultBufImgMap.get("bufferedImage"));
                result.put("successCount", successCount);
                result.put("failCount", failCount);
                result.put("skipCount", skipCount);
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

    public static String expandImageFnFmt(FlexibleStringExpander exdr, String sizeType, Map<String, ?> context) throws IllegalArgumentException {
        Map<String, Object> fnContext = new HashMap<>(context);
        fnContext.put("sizetype", sizeType);
        fnContext.put("type", sizeType);
        String newFileLocation = exdr.expandString(fnContext);
        if (newFileLocation.startsWith("/")) newFileLocation = newFileLocation.substring(1); // SCIPIO

        if (newFileLocation.isEmpty()) {
            throw new IllegalArgumentException("Cannot determine file location for sizeType [" + sizeType + "] using expander [" + exdr +
                    "] and imagePathArgs " + context + "; empty result; service call or configuration may be incomplete");
        }
        int lastSlashIndex = newFileLocation.lastIndexOf('/');
        if (lastSlashIndex >= 0 && lastSlashIndex >= (newFileLocation.length() - 1)) {
            throw new IllegalArgumentException("Cannot determine file location [" + newFileLocation + "] for sizeType [" + sizeType + "] using expander [" + exdr +
                    "] and imagePathArgs " + context + "; no filename part; service call or configuration may be incomplete");
        }
        return newFileLocation;
    }

    public static String getExpandedFnFmtDirPrefix(String newFileLocation) {
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
    public static Map<String, Object> contentImageDbScaleInAllSizeCore(ServiceContext ctx) throws ServiceValidationException {
        DispatchContext dctx = ctx.dctx();
        Map<String, Object> context = ctx.context();
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        GenericValue userLogin = (GenericValue) context.get("userLogin");
        TimeZone timeZone = (TimeZone) context.get("timeZone");
        String imageOrigContentId = (String) context.get("imageOrigContentId");
        String imageOrigFullFn = (String) context.get("imageOrigFn");
        //String imageOrigPath = (String) context.get("imageOrigPath"); // TODO?
        String imagePropXmlPath = (String) context.get("imagePropXmlPath");
        Collection<String> sizeTypeList = UtilGenerics.cast(context.get("sizeTypeList"));
        //boolean copyOrig = Boolean.TRUE.equals(context.get("copyOrig"));
        boolean deleteOld = Boolean.TRUE.equals(context.get("deleteOld"));
        boolean recreateExisting = Boolean.TRUE.equals(context.get("recreateExisting"));
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
        Map<String, Object> imageWriteOptions = UtilGenerics.cast(context.get("imageWriteOptions"));
        final String logPrefix = "contentImageDbScaleInAllSizeCore: ";

        Object imageProfileObj = context.get("imageProfile");
        ImageProfile imageProfile = null;
        if (imageProfileObj instanceof ImageProfile) {
            imageProfile = (ImageProfile) imageProfileObj;
        } else if (imageProfileObj instanceof String) {
            imageProfile = ImageProfile.getImageProfile(delegator, (String) imageProfileObj, false);
            if (imageProfile == null) {
                String errMsg = "Could not find mediaProfile [" + imageProfileObj + "] for content image [" + imageOrigContentId + "]";
                Debug.logError(logPrefix + errMsg, module);
                return ServiceUtil.returnError(errMsg);
            }
        } else if (imageProfileObj != null) {
            throw new ServiceValidationException("Invalid imageProfile parameter: " + imageProfileObj.getClass().getName(), ctx.getModelService());
        }

        //final String origSizeType = ContentImageWorker.ORIGINAL_SIZETYPE;

        long startTime = System.currentTimeMillis();

        // USE SAME CREATED DATE FOR EVERYTHING RELATED
        Timestamp createdDate = (Timestamp) context.get("createdDate");
        if (createdDate == null) createdDate = UtilDateTime.nowTimestamp();

        try {
            GenericValue origContent = delegator.findOne("Content", UtilMisc.toMap("contentId", imageOrigContentId), false);
            if (origContent == null) {
                Debug.logError(logPrefix+UtilProperties.getMessage(resourceProduct, "ScaleImage.unable_to_parse", LOG_LANG) + " : Content: contentId: " + imageOrigContentId, module);
                return ServiceUtil.returnError(UtilProperties.getMessage(resourceProduct, "ScaleImage.unable_to_parse", locale) + " : Content: contentId: " + imageOrigContentId);
            }
            String origMediaProfileName = origContent.getString("mediaProfile");
            if (imageProfile == null && origMediaProfileName != null) {
                imageProfile = ImageProfile.getImageProfile(delegator, origMediaProfileName, false);
                if (imageProfile == null) {
                    Debug.logError(logPrefix + "Could not find mediaProfile [" + origMediaProfileName + "] for content image [" + imageOrigContentId + "], using IMAGE_CONTENT default", module);
                }
            }
            if (imageProfile == null) {
                imageProfileObj = context.get("defaultImageProfile");
                if (imageProfileObj instanceof ImageProfile) {
                    imageProfile = (ImageProfile) imageProfileObj;
                } else if (imageProfileObj instanceof String) {
                    imageProfile = ImageProfile.getImageProfile(delegator, (String) imageProfileObj, false);
                    if (imageProfile == null) {
                        String errMsg = "Could not find mediaProfile [" + imageProfileObj + "] for content image [" + imageOrigContentId + "]";
                        Debug.logError(logPrefix + errMsg, module);
                        return ServiceUtil.returnError(errMsg);
                    }
                } else {
                    throw new ServiceValidationException("Invalid or missing defaultImageProfile parameter: " + imageProfileObj.getClass().getName(), ctx.getModelService());
                }
            }

            String origImageDataResourceId = origContent.getString("dataResourceId");
            GenericValue origDataResource = delegator.findOne("DataResource", UtilMisc.toMap("dataResourceId", origImageDataResourceId), false);
            if (origDataResource == null) {
                Debug.logError(logPrefix+UtilProperties.getMessage(resourceProduct, "ScaleImage.unable_to_parse", LOG_LANG) + " : DataResource: dataResourceId: " + origImageDataResourceId + " (contentId: " + imageOrigContentId + ")", module);
                return ServiceUtil.returnError(UtilProperties.getMessage(resourceProduct, "ScaleImage.unable_to_parse", locale) + " : DataResource: dataResourceId: " + origImageDataResourceId + " (contentId: " + imageOrigContentId + ")");
            }

            // SCIPIO: for these we now support component:// and file:// prefix in addition to plain absolute file location
            //if (UtilValidate.isNotEmpty(imageOrigPath)) {
            //    imageOrigPath = FlexibleLocation.resolveFileUrlAsPathIfUrl(imageOrigPath, imageOrigPath);
            //}
            ImageVariantConfig imgPropCfg = (ImageVariantConfig) context.get("imageVariantConfig");
            if (imgPropCfg == null) {
                if (UtilValidate.isNotEmpty(imagePropXmlPath)) {
                    try {
                        imgPropCfg = ImageVariantConfig.fromImagePropertiesXml(imagePropXmlPath, locale);
                    } catch (Exception e) {
                        Debug.logError(logPrefix + UtilProperties.getMessage(resourceProduct, "ScaleImage.unable_to_parse", LOG_LANG) + " : " + imagePropXmlPath + " : " + e.getMessage(), module);
                        return ServiceUtil.returnError(UtilProperties.getMessage(resourceProduct, "ScaleImage.unable_to_parse", locale) + " : " + imagePropXmlPath + " : " + e.getMessage());
                    }
                } else { // SCIPIO
                    imgPropCfg = imageProfile.readVariantConfig(); // NOTE: reads non-cached version
                }
            }
            if (sizeTypeList == null) {
                sizeTypeList = imgPropCfg.getVariantNames();
            }

            /* IMAGE */

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
                if (bufImg == null) { // SCIPIO: may be null
                    Debug.logError(logPrefix+"Could not read/parse image file type to determine dimensions" + " : ImageDataResource.imageData (dataResourceId: " + origImageDataResourceId + ") (contentId: " + imageOrigContentId + ")", module);
                    return ServiceUtil.returnError(UtilProperties.getMessage(resourceProduct, "ScaleImage.unable_to_parse", locale) + " : ImageDataResource.imageData (dataResourceId: " + origImageDataResourceId + ") (contentId: " + imageOrigContentId + ")");
                }
            } catch(Exception e) {
                Debug.logError(logPrefix+"Could not read/parse image file type to determine dimensions" + " : ImageDataResource.imageData (dataResourceId: " + origImageDataResourceId + ") (contentId: " + imageOrigContentId + "): " + e.toString(), module);
                return ServiceUtil.returnError(UtilProperties.getMessage(resourceProduct, "ScaleImage.unable_to_parse", locale) + " : ImageDataResource.imageData (dataResourceId: " + origImageDataResourceId + ") (contentId: " + imageOrigContentId + ")");
            } finally {
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
                    Debug.logWarning(logPrefix+"No original image filename available (dataResourceId: " + origImageDataResourceId + ")", module);
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
            String origMimeTypeId;
            if (UtilValidate.isEmpty(targetFmtExt)) {
                origMimeTypeId = origDataResource.getString("mimeTypeId");
                if (UtilValidate.isEmpty(origMimeTypeId)) {
                    Debug.logError(logPrefix+"Can't determine output format (no targetFormatName or DataResource.mimeTypeId) (dataResourceId: " + origImageDataResourceId + ")", module);
                    return ServiceUtil.returnError("Can't determine output format (no targetFormatName or DataResource.mimeTypeId) (dataResourceId: " + origImageDataResourceId + ")");
                }

                List<GenericValue> fileExtValues;
                try {
                    fileExtValues = EntityQuery.use(delegator).from("FileExtension").where("mimeTypeId", origMimeTypeId).cache().queryList();
                    if (UtilValidate.isNotEmpty(fileExtValues)) {
                        targetFmtExt = fileExtValues.get(0).getString("fileExtensionId");
                        if (fileExtValues.size() > 1) {
                            Debug.logInfo(logPrefix+"Multiple FileExtension found for mimeTypeId '" + origMimeTypeId + "'; using first: '" + targetFmtExt + "' (dataResourceId: " + origImageDataResourceId + ")", module);
                        }
                    } else {
                        targetFmtExt = EntityUtilProperties.getPropertyValue("content", "image.thumbs.fileType.default", "jpg", delegator);
                        Debug.logWarning(logPrefix+"can't determine thumbnail output format from mimeTypeId '" + origMimeTypeId + "' (dataResourceId: " + origImageDataResourceId
                                + "); unknown?; using system default: " + targetFmtExt, module);
                        GenericValue fileExt = EntityQuery.use(delegator).from("FileExtension").where("fileExtensionId", targetFmtExt).cache().queryOne();
                        if (UtilValidate.isEmpty(fileExt)) {
                            Debug.logError(logPrefix+"Can't determine thumbnail output format from file type '" + targetFmtExt + "' (dataResourceId: " + origImageDataResourceId + ")", module);
                            return ServiceUtil.returnError("Can't determine thumbnail output format from file type '" + targetFmtExt + "' (dataResourceId: " + origImageDataResourceId + ")");
                        }
                        origMimeTypeId = fileExt.getString("mimeTypeId");
                    }
                } catch (GenericEntityException e) {
                    Debug.logError(e, logPrefix+"Can't determine output format from mimeTypeId '" + origMimeTypeId + "' (dataResourceId: " + origImageDataResourceId + "): " + e.getMessage(), module);
                    return ServiceUtil.returnError("Can't determine output format from mimeTypeId '" + origMimeTypeId + "' (dataResourceId: " + origImageDataResourceId + "): " + e.getMessage());
                }
            } else {
                try {
                    GenericValue fileExt = EntityQuery.use(delegator).from("FileExtension").where("fileExtensionId", targetFmtExt).cache().queryOne();
                    if (UtilValidate.isEmpty(fileExt)) {
                        Debug.logError(logPrefix+"No FileExtension (mime-type assoc) record for extension: " + targetFmtExt, module);
                        return ServiceUtil.returnError("No FileExtension (mime-type assoc) record for extension: " + targetFmtExt);
                    }
                    origMimeTypeId = fileExt.getString("mimeTypeId");
                } catch (GenericEntityException e) {
                    Debug.logError(e, logPrefix+"Can't determine output format from DataResource.mimeTypeId) (dataResourceId: " + origImageDataResourceId + "): " + e.getMessage(), module);
                    return ServiceUtil.returnError("Can't determine output format from DataResource.mimeTypeId) (dataResourceId: " + origImageDataResourceId + "): " + e.getMessage());
                }
            }

            Map<String, String> imageContentIdMap = new HashMap<>();

            List<GenericValue> prevContentAssocList = ContentImageWorker.getResizedImageContentAssocRecords(delegator, imageOrigContentId, false);

            int imageDeleteCount = 0;
            if (deleteOld) {
                for(GenericValue contentAssoc : prevContentAssocList) {
                    // NOTE: this automatically removes the ContentAssoc
                    Map<String, Object> servCtx = new HashMap<>();
                    servCtx.put("userLogin", userLogin);
                    servCtx.put("locale", locale);
                    servCtx.put("timeZone", timeZone);
                    servCtx.put("contentId", contentAssoc.get("contentIdTo"));
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
                prevContentAssocList = Collections.emptyList();
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
            //imageCtx.put("ext", targetFmtExt);
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

            int successCount = 0;
            int scaleErrorCount = 0;
            int writeErrorCount = 0;
            int skipCount = 0;
            for (String sizeType : sizeTypeList) {
                ImageVariantConfig.VariantInfo variantInfo = imgPropCfg.getVariant(sizeType);
                if (variantInfo == null) {
                    Debug.logError(logPrefix+"sizeType " + sizeType + " is not part of ImageProperties.xml; ignoring", module);
                    continue;
                }

                // Check if dimensions are equal to the original image or resizing is prohibited by the ImageProperties.xml upscale mode
                boolean useOrigImage = false;
                Integer targetWidth = variantInfo.getWidth();
                Integer targetHeight = variantInfo.getHeight();
                if (variantInfo.getUpscaleMode() != ImageVariantConfig.VariantInfo.UpscaleMode.ON) {
                    if (targetWidth == (int) imgWidth || targetHeight == (int) imgHeight) {
                        useOrigImage = true;
                    } else if (targetWidth > (int) imgWidth && targetHeight > (int) imgHeight) {
                        if (variantInfo.getUpscaleMode() == ImageVariantConfig.VariantInfo.UpscaleMode.OMIT) {
                            continue;
                        } else if (variantInfo.getUpscaleMode() == ImageVariantConfig.VariantInfo.UpscaleMode.OFF) {
                            useOrigImage = true;
                        }
                    }
                }

                String targetFormat = targetFmtExt;
                String mimeTypeId = origMimeTypeId;
                // SCIPIO: 2020-09: Support for specific storage format
                if (variantInfo != null && variantInfo.getFormat() != null) {
                    targetFormat = variantInfo.resolveFormatExt(delegator);
                    GenericValue fileExt = EntityQuery.use(delegator).from("FileExtension").where("fileExtensionId", targetFormat).cache().queryOne();
                    if (UtilValidate.isEmpty(fileExt)) {
                        Debug.logError(logPrefix+"No FileExtension (mime-type assoc) record for extension: " + targetFormat, module);
                        return ServiceUtil.returnError("No FileExtension (mime-type assoc) record for extension: " + targetFormat);
                    }
                    mimeTypeId = fileExt.getString("mimeTypeId");
                }

                imageCtx.put("ext", targetFormat);
                imageCtx.put("sizetype", sizeType);
                imageCtx.put("type", sizeType);
                String contentAssocTypeId = contentAssocTypeIdExdr.expandString(imageCtx).toUpperCase();

                GenericValue contentAssoc = EntityUtil.getFirstMatchingField(prevContentAssocList, "contentAssocTypeId", contentAssocTypeId);
                GenericValue dataResource = null;
                GenericValue imageDataResource = null;
                GenericValue content = null;
                if (contentAssoc != null) {
                    content = delegator.findOne("Content", UtilMisc.toMap("contentId", contentAssoc.get("contentIdTo")), false);
                    if (content == null) {
                        Debug.logError("Could not find Content [" + contentAssoc.get("contentIdTo") + "] for contentAssocTypeId [" + contentAssocTypeId + "]", module);
                        return ServiceUtil.returnError("Could not find content [" + contentAssoc.get("contentIdTo") + "] for contentAssocTypeId [" + contentAssocTypeId + "]");
                    }
                    dataResource = delegator.findOne("DataResource", UtilMisc.toMap("dataResourceId", content.get("dataResourceId")), false);
                    if (dataResource == null) {
                        Debug.logError("Could not find DataResource [" + content.get("dataResourceId") + "] for Content [" + contentAssoc.get("contentIdTo") + "] for contentAssocTypeId [" + contentAssocTypeId + "]", module);
                        return ServiceUtil.returnError("Could not find DataResource [" + content.get("dataResourceId") + "] for Content [" + contentAssoc.get("contentIdTo") + "] for contentAssocTypeId [" + contentAssocTypeId + "]");
                    }
                    if (!recreateExisting) {
                        Long prevWidth = dataResource.getLong("scpWidth");
                        Long prevHeight = dataResource.getLong("scpHeight");
                        String prevMimeTypeId = dataResource.getString("mimeTypeId");
                        long effWidth;
                        long effHeight;
                        if (useOrigImage) {
                            effWidth = bufImg.getWidth();
                            effHeight = bufImg.getHeight();
                        } else {
                            Map<String, Object> scaleDims = ImageTransform.getScaleImageDimensions(imgHeight, imgWidth, targetHeight.doubleValue(), targetWidth.doubleValue(), locale);
                            if (!ServiceUtil.isSuccess(scaleDims) || scaleDims.get("width") == null || scaleDims.get("height") == null) {
                                String errMsg = "Error getting scaled image dimensions for contentId [" + imageOrigContentId + "] sizeType [" + sizeType + "] from [" + imgWidth + "x" + imgHeight + "] to [" + targetWidth + "x" + targetHeight + "]"
                                        + ": " + ServiceUtil.getErrorMessage(scaleDims);
                                Debug.logError(logPrefix + errMsg, module);
                                scaleErrorCount++;
                                continue;
                            }
                            effWidth = (int) scaleDims.get("width");
                            effHeight = (int) scaleDims.get("height");
                        }
                        if (Objects.equals(prevWidth, effWidth) && Objects.equals(prevHeight, effHeight) && Objects.equals(prevMimeTypeId, mimeTypeId)) {
                            if (Debug.verboseOn()) {
                                Debug.logInfo(logPrefix + "contentAssocTypeId [" + contentAssocTypeId + "] has same dimensions and mimeType, not recreating image variant", module);
                            }
                            skipCount++;
                            continue;
                        }
                    }
                }

                // Scale
                Map<String, Object> resultScaleImgMap = Collections.emptyMap();
                if (!useOrigImage) {
                    try {
                        resultScaleImgMap = ImageTransform.scaleImage(bufImg, imgHeight, imgWidth, targetHeight.doubleValue(), targetWidth.doubleValue(), locale, scalingOptions);
                        if (!ServiceUtil.isSuccess(resultScaleImgMap)) {
                            String errMsg = "Error scaling image for contentId [" + imageOrigContentId + "] sizeType [" + sizeType + "] from [" + imgWidth + "x" + imgHeight + "] to [" + targetWidth + "x" + targetHeight + "]"
                                    + ": " + ServiceUtil.getErrorMessage(resultScaleImgMap);
                            Debug.logError(logPrefix + errMsg, module);
                            scaleErrorCount++;
                            continue;
                        }
                    } catch(Exception e) {
                        String errMsg = "Error scaling image for contentId [" + imageOrigContentId + "] sizeType [" + sizeType + "] from [" + imgWidth + "x" + imgHeight + "] to [" + targetWidth + "x" + targetHeight + "]"
                                + ": " + e.toString();
                        Debug.logError(logPrefix + errMsg, module);
                        scaleErrorCount++;
                        continue;
                    }
                }

                /* Write the new image file */
                if (useOrigImage || "success".equals(resultScaleImgMap.get("responseMessage"))) {
                    BufferedImage bufNewImg = (BufferedImage) resultScaleImgMap.get("bufferedImage");

                    byte[] byteout;
                    if (useOrigImage && Objects.equals(targetFormat, targetFmtExt) && streamResult.get("streamBytes") != null) {
                        byteout = (byte[]) streamResult.get("streamBytes");
                    } else {
                        ByteArrayOutputStream byteos = new ByteArrayOutputStream();
                        try {
                            ImageStorers.write(useOrigImage ? bufImg : bufNewImg, targetFormat, byteos,
                                    (imageProfile != null) ? imageProfile.getName() : null, imageWriteOptions, delegator); // SCIPIO: ImageIO->ImageStorers
                            byteout = byteos.toByteArray();
                        } catch (Exception e) {
                            String errMsg = "Error writing image for contentId [" + imageOrigContentId + "] sizeType [" + sizeType + "] from [" + imgWidth + "x" + imgHeight + "] to [" + targetWidth + "x" + targetHeight + "]"
                                    + ": " + e.toString();
                            Debug.logError(logPrefix + errMsg, module);
                            writeErrorCount++;
                            continue;
                        } finally {
                            byteos.close();
                        }
                    }

                    boolean newDataResource = (dataResource == null);
                    if (newDataResource) {
                        dataResource = delegator.makeValue("DataResource");
                    }
                    dataResource.put("dataResourceTypeId", "IMAGE_OBJECT");
                    dataResource.put("createdDate", createdDate);
                    dataResource.put("mimeTypeId", mimeTypeId);
                    // caller should determine theses...
                    //dataResource.put("statusId", origDataResource.get("statusId"));
                    //dataResource.put("isPublic", "N");

                    // SCIPIO: 2017-08-11: now store width & height in new DataResource fields,
                    // due to very high probability we will need these, and with decent access speed.
                    if (bufNewImg != null) {
                        dataResource.put("scpWidth", (long) bufNewImg.getWidth());
                        dataResource.put("scpHeight", (long) bufNewImg.getHeight());
                    } else {
                        dataResource.put("scpWidth", (long) bufImg.getWidth());
                        dataResource.put("scpHeight", (long) bufImg.getHeight());
                    }
                    dataResource.setJson("srcPresetJson", variantInfo.configToMap());

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
                    if (newDataResource) {
                        dataResource = delegator.createSetNextSeqId(dataResource);
                    } else {
                        dataResource.store();
                    }
                    String dataResourceId = dataResource.getString("dataResourceId");

                    imageDataResource = delegator.makeValue("ImageDataResource");
                    imageDataResource.put("imageData", byteout);
                    imageDataResource.put("dataResourceId", dataResource.get("dataResourceId"));
                    if (newDataResource) {
                        imageDataResource = imageDataResource.create();
                    } else {
                        imageDataResource.store();
                    }

                    if (UtilValidate.isNotEmpty(fileSizeDataResAttrName)) {
                        GenericValue fileSizeDataResourceAttr = delegator.makeValue("DataResourceAttribute");
                        fileSizeDataResourceAttr.put("dataResourceId", dataResourceId);
                        fileSizeDataResourceAttr.put("attrName", fileSizeDataResAttrName);
                        fileSizeDataResourceAttr.put("attrValue", String.valueOf(byteout.length));
                        fileSizeDataResourceAttr = fileSizeDataResourceAttr.createOrStore();
                    }

                    boolean newContent = (content == null);
                    if (newContent) {
                        content = delegator.makeValue("Content");
                        content.put("createdDate", createdDate);
                    }

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
                    if (newContent) {
                        content = delegator.createSetNextSeqId(content);
                    } else {
                        content.store();
                    }
                    String resContentId = content.getString("contentId");

                    if (delegator.findOne("ContentAssocType", UtilMisc.toMap("contentAssocTypeId", contentAssocTypeId), false) == null) {
                        Debug.logInfo(logPrefix+"ContentAssocType for contentAssocTypeId '" + contentAssocTypeId
                                + "' does not yet exist; automatically creating...", module);
                        GenericValue contentAssocType = delegator.makeValue("ContentAssocType");
                        contentAssocType.put("contentAssocTypeId", contentAssocTypeId);
                        contentAssocType.put("description", "Image - " + sizeType.substring(0, 1).toUpperCase() + sizeType.substring(1).toLowerCase());
                        contentAssocType = contentAssocType.create();
                    }

                    if (contentAssoc == null) {
                        contentAssoc = delegator.makeValue("ContentAssoc");
                        contentAssoc.put("contentId", imageOrigContentId);
                        contentAssoc.put("contentIdTo", resContentId);
                        contentAssoc.put("contentAssocTypeId", contentAssocTypeId);
                        contentAssoc.put("fromDate", createdDate);
                        contentAssoc.put("mapKey", sizeType);
                        contentAssoc = contentAssoc.create();
                    }

                    imageContentIdMap.put(sizeType, resContentId);
                    successCount++;
                } else {
                    // SCIPIO: new
                    Debug.logError(logPrefix+ServiceUtil.getErrorMessage(resultScaleImgMap), module);
                    return ServiceUtil.returnError(ServiceUtil.getErrorMessage(resultScaleImgMap));
                }
            } // Loop over sizeType

            // save the name of the image props def we used
            // NOTE: 2020-11: These are now moot because of Content.mediaProfile, but doesn't hurt
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
            long endTime = System.currentTimeMillis();
            StringBuilder logSb = new StringBuilder();
            logSb.append("In ");
            logSb.append((endTime - startTime) / 1000000);
            logSb.append("ms processed ");
            logSb.append(successCount);
            logSb.append(" scaled and ");
            logSb.append(imageCopyCount);
            logSb.append(" original copies of image for contentId [");
            logSb.append(imageOrigContentId);
            logSb.append("]");
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
            logSb.append(" (success: ");
            logSb.append(successCount);
            logSb.append(", scaling errors: ");
            logSb.append(scaleErrorCount);
            logSb.append(", write errors: ");
            logSb.append(writeErrorCount);
            logSb.append(", skipped: ");
            logSb.append(skipCount);
            logSb.append(")");

            String msg = logSb.toString();
            //}

            int failCount = scaleErrorCount + writeErrorCount;
            Map<String, Object> result;
            if (failCount > 0) {
                if (successCount > 0) {
                    result = ServiceUtil.returnFailure(msg);
                } else {
                    // TODO: REVIEW: for now let caller decide to abort trans
                    //result = ServiceUtil.returnError(msg);
                    result = ServiceUtil.returnFailure(msg);
                }
                Debug.logError(logPrefix + msg, module);
            } else {
                result = ServiceUtil.returnSuccess(msg);
                Debug.logInfo(logPrefix + msg, module);
            }
            result.put("imageContentIdMap", imageContentIdMap);
            result.put("bufferedImage", bufImg);
            result.put("successCount", successCount);
            result.put("failCount", failCount);
            result.put("skipCount", skipCount);
            return result;
        } catch(Exception e) {
            // FIXME?: more generic err msg
            Debug.logError(e, logPrefix+UtilProperties.getMessage(resourceProduct, "ScaleImage.unable_to_scale_original_image", LOG_LANG) + ": " + imageOrigContentId + ": " + e.getMessage(), module);
            return ServiceUtil.returnError(UtilProperties.getMessage(resourceProduct, "ScaleImage.unable_to_scale_original_image", locale) + " : " + imageOrigContentId);
        }
    }

    public static Map<String, Object> contentImageAutoRescale(ServiceContext ctx) {
        String contentId = ctx.attr("contentId");
        GenericValue contentDataResource = ctx.attr("contentDataResource");
        boolean createNew = ctx.attr("createNew", true);
        boolean deleteOld = ctx.attr("deleteOld", false);
        boolean recreateExisting = ctx.attr("recreateExisting", false);
        boolean nonFatal = ctx.attr("nonFatal", false);
        boolean doLog = ctx.attr("doLog", false);
        String progressInfo = ctx.attr("progressInfo");
        Timestamp moment = ctx.attr("moment", UtilDateTime::nowTimestamp);

        int variantSuccessCount = 0;
        int variantFailCount = 0;
        int variantSkipCount = 0;
        try {
            if (contentDataResource == null) {
                contentDataResource = ctx.delegator().findOne("ContentDataResourceRequiredView", UtilMisc.toMap("contentId", contentId), false);
                if (contentDataResource == null) {
                    throw new GeneralException("Content [" + contentId + "] not found");
                }
            }
            GenericValue content = contentDataResource.extractViewMember("Content");
            GenericValue dataResource = contentDataResource.extractViewMember("DataResource");

            // Check explicit mediaProfile first
            String imageProfileName = content.getString("mediaProfile");
            ImageProfile imageProfile = null;
            if (imageProfileName != null) {
                imageProfile = ImageProfile.getImageProfile(ctx.delegator(), imageProfileName, false);
                if (imageProfile == null) {
                    throw new GeneralException("Content.mediaProfile [" + imageProfileName + "] for content [" + contentId + "] not found in mediaprofiles.properties");
                }
            }

            if (imageProfile == null || !createNew) {
                // Get the first variant (for default fields)
                EntityCondition cond = EntityCondition.makeCondition(
                        EntityCondition.makeCondition("contentIdStart", contentId),
                        EntityOperator.AND,
                        EntityCondition.makeCondition("caContentAssocTypeId", EntityOperator.LIKE, "IMGSZ_%"));
                GenericValue variantContentAssoc = ctx.delegator().from("ContentAssocViewTo").where(cond).filterByDate(moment).queryFirst();

                //GenericValue variantContent = null;
                GenericValue variantDataResource = null;
                if (variantContentAssoc != null) {
                    if (imageProfile == null) {
                        //variantContent = variantContentAssoc.extractViewMember("Content");
                        variantDataResource = ctx.delegator().from("DataResource").where("dataResourceId", variantContentAssoc.get("dataResourceId")).queryOne();
                        if (variantDataResource == null) {
                            throw new GenericEntityException("Could not find DataResource [" + variantContentAssoc.get("dataResourceId") + "] for variant image content [" + variantContentAssoc.get("contentId") + "]");
                        }
                    }
                } else if (!createNew) {
                    return UtilMisc.put(ServiceUtil.returnSuccess("No existing variant images for content [" + contentId + "], not regenerating image size variants"),
                            "reason", "no-variants");
                }

                if (imageProfile == null && variantContentAssoc != null) {
                    String variantSizeId = variantDataResource.getString("sizeId");
                    if (variantSizeId != null) {
                        GenericValue variantImageSize = ctx.delegator().from("ImageSize").where("sizeId", variantSizeId).queryFirst();
                        if (variantImageSize == null) {
                            throw new GenericEntityException("Content [" + contentId + "] variant content [" + variantContentAssoc.get("contentId") + "] has invalid sizeId [" + variantSizeId + "]");
                        }
                        imageProfile = ImageProfile.getImageProfile(ctx.delegator(), variantImageSize.getString("presetId"), false);
                        if (imageProfile == null) {
                            throw new GeneralException("Content [" + contentId + "] sizeId [" + variantSizeId + "] preset [" + variantImageSize.getString("presetId") + "] is not a valid media profile");
                        }

                        // Auto-update records
                        Debug.logWarning("contentImageAutoRescale: Content [" + contentId + "] has sizeId ImageSizePreset variants but no Content.mediaProfile, " +
                                "auto-updating field with mediaProfile [" + imageProfile.getName() + "]", module);
                        content = ctx.delegator().findOne("Content", UtilMisc.toMap("contentId", contentId), false);
                        if (content == null) {
                            throw new GeneralException("Content [" + contentId + "] not found after re-query");
                        }
                        content.set("mediaProfile", imageProfile.getName());
                        content.store();

                        // re-query
                        contentDataResource = ctx.delegator().findOne("ContentDataResourceRequiredView", UtilMisc.toMap("contentId", contentId), false);
                        if (contentDataResource == null) {
                            throw new GeneralException("Content [" + contentId + "] not found after re-query");
                        }
                        content = contentDataResource.extractViewMember("Content");
                    }
                }
            }

            if (imageProfile == null) {
                imageProfile = ContentImageWorker.getContentImageProfileOrDefault(ctx.delegator(), content, false, false);
            }

            Map<String, Object> contentFields = new HashMap<>();
            Map<String, Object> dataResourceFields = new HashMap<>();

            // TODO: Support for other Content and DataResource fields should be analyzed, but difficult to carry over automatically,
            //  may need variantContent and variantDataResource
            //if (variantContentAssoc != null) {
            //
            //} else {
            //    Debug.logInfo("contentImageAutoRescale: No variant content images found for content [" + contentId
            //            + "] to regenerate fields from for auto-resize; using default fields", module);
            contentFields.putAll(ContentImageWorker.RESIZEIMG_CONTENT_FIELDEXPR);
            contentFields.put("contentTypeId", "SCP_MEDIA_VARIANT");

            dataResourceFields.putAll(ContentImageWorker.RESIZEIMG_DATARESOURCE_FIELDEXPR);
            dataResourceFields.put("dataResourceTypeId", "IMAGE_OBJECT");
            dataResourceFields.put("statusId", dataResource.get("statusId"));
            dataResourceFields.put("isPublic", dataResource.get("isPublic"));
            //}

            Map<String, Object> resizeCtx = ctx.makeValidInContext("contentImageDbScaleInAllSizeCore", ctx);
            resizeCtx.put("imageOrigContentId", contentId);
            resizeCtx.put("imageProfile", imageProfile);
            resizeCtx.put("fileSizeDataResAttrName", FileTypeUtil.FILE_SIZE_ATTRIBUTE_NAME);
            resizeCtx.put("deleteOld", deleteOld); // NOTE: NOT RECOMMENDED anymore
            resizeCtx.put("recreateExisting", recreateExisting);
            resizeCtx.put("createdDate", moment);
            resizeCtx.put("contentFields", contentFields);
            resizeCtx.put("dataResourceFields", dataResourceFields);

            if (doLog) {
                Debug.logInfo("contentImageAutoRescale: Rebuilding variants for image content [" + contentId + "]"
                        + (progressInfo != null ? " (" + progressInfo + ")" : ""), module);
            }
            Map<String, Object> resizeResult = ctx.dispatcher().runSync("contentImageDbScaleInAllSizeCore", resizeCtx, nonFatal);
            if (resizeResult.get("successCount") != null) {
                variantSuccessCount += (Integer) resizeResult.get("successCount");
            }
            if (resizeResult.get("failCount") != null) {
                variantFailCount += (Integer) resizeResult.get("failCount");
            }
            if (resizeResult.get("skipCount") != null) {
                variantSkipCount += (Integer) resizeResult.get("skipCount");
            }
            if (nonFatal) {
                if (!ServiceUtil.isSuccess(resizeResult)) {
                    return UtilMisc.put(ServiceUtil.returnFailure("Error creating resized images: " + ServiceUtil.getErrorMessage(resizeResult)),
                            "variantSuccessCount", variantSuccessCount, "variantFailCount", variantFailCount, "variantSkipCount", variantSkipCount);
                }
            } else {
                if (!ServiceUtil.isSuccess(resizeResult)) {
                    throw new GeneralException("Error creating resized images: " + ServiceUtil.getErrorMessage(resizeResult));
                }
            }
        } catch (Exception e) {
            String errMsg = "Could not auto-resize variant images for content [" + contentId + "]: " + e.getMessage();
            Debug.logError(e, "contentImageAutoRescale: " + errMsg, module);
            return UtilMisc.put(ServiceUtil.returnError(errMsg), "variantSuccessCount", variantSuccessCount, "variantFailCount", variantFailCount, "variantSkipCount", variantSkipCount);
        }
        return UtilMisc.put(ServiceUtil.returnSuccess(), "variantSuccessCount", variantSuccessCount, "variantFailCount", variantFailCount, "variantSkipCount", variantSkipCount);
    }
}