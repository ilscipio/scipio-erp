/*
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
 */
import org.ofbiz.base.util.Debug
import org.ofbiz.base.util.HttpRequestFileUpload
import org.ofbiz.base.util.UtilDateTime
import org.ofbiz.base.util.string.FlexibleStringExpander
import org.ofbiz.entity.transaction.GenericTransactionException
import org.ofbiz.entity.transaction.TransactionUtil
import org.ofbiz.entity.util.EntityUtilProperties
import org.ofbiz.service.ServiceUtil

import javax.transaction.Transaction

module = "EditCategoryContent.groovy"

context.nowTimestampString = UtilDateTime.nowTimestamp().toString();

// pagination for the category content list
viewIndex = Integer.valueOf(parameters.VIEW_INDEX  ?: 0);
viewSize = Integer.valueOf(parameters.VIEW_SIZE ?: EntityUtilProperties.getPropertyValue("widget", "widget.form.defaultViewSize", "20", delegator));

productCategoryContentList = from("ProductCategoryContent").where("productCategoryId", productCategory.productCategoryId)
    .cursorScrollInsensitive().orderBy("prodCatContentTypeId").queryPagedList(viewIndex, viewSize)

context.viewIndex = productCategoryContentList.getViewIndex()
context.viewSize = productCategoryContentList.getViewSize()
context.listSize = productCategoryContentList.getListSize()
context.lowIndex = productCategoryContentList.getStartIndex()
context.highIndex = productCategoryContentList.getEndIndex()
context.productCategoryContentList = productCategoryContentList.getData()

// make the image file formats
context.tenantId = delegator.getDelegatorTenantId();
imageFilenameFormat = EntityUtilProperties.getPropertyValue('catalog', 'image.filename.format', delegator);
imageServerPath = FlexibleStringExpander.expandString(EntityUtilProperties.getPropertyValue("catalog", "image.server.path", delegator), context);
imageUrlPrefix = FlexibleStringExpander.expandString(EntityUtilProperties.getPropertyValue("catalog", "image.url.prefix",delegator), context);
imageServerPath = imageServerPath.endsWith("/") ? imageServerPath.substring(0, imageServerPath.length()-1) : imageServerPath;
imageUrlPrefix = imageUrlPrefix.endsWith("/") ? imageUrlPrefix.substring(0, imageUrlPrefix.length()-1) : imageUrlPrefix;
context.imageFilenameFormat = imageFilenameFormat;
context.imageServerPath = imageServerPath;
context.imageUrlPrefix = imageUrlPrefix;

filenameExpander = FlexibleStringExpander.getInstance(imageFilenameFormat);
context.imageNameSmall  = imageUrlPrefix + "/" + filenameExpander.expandString([location : 'categories', id : productCategoryId, type : 'small']);
context.imageNameMedium = imageUrlPrefix + "/" + filenameExpander.expandString([location : 'categories', id : productCategoryId, type : 'medium']);
context.imageNameLarge  = imageUrlPrefix + "/" + filenameExpander.expandString([location : 'categories', id : productCategoryId, type : 'large']);
context.imageNameDetail = imageUrlPrefix + "/" + filenameExpander.expandString([location : 'categories', id : productCategoryId, type : 'detail']);
context.imageNameOriginal = imageUrlPrefix + "/" + filenameExpander.expandString([location : 'categories', id : productCategoryId, type : 'original']);

// Start ProductContent stuff
productCategoryContent = null;
if (productCategory) {
    productCategoryContent = productCategory.getRelated('ProductCategoryContent', null, ['prodCatContentTypeId'], false);
}
context.productCategoryContent = productCategoryContent;
// End ProductContent stuff

tryEntity = true;
if (request.getAttribute("_ERROR_MESSAGE_")) {
    tryEntity = false;
}
if (!productCategory) {
    tryEntity = false;
}

if ("true".equalsIgnoreCase((String) request.getParameter("tryEntity"))) {
    tryEntity = true;
}
context.tryEntity = tryEntity;

// UPLOADING STUFF
forLock = new Object();
contentType = null;
String fileType = request.getParameter("upload_file_type");
if (fileType) {

    context.fileType = fileType;

    fileLocation = filenameExpander.expandString([location : 'categories', id : productCategoryId, type : fileType]);
    filePathPrefix = "";
    filenameToUse = fileLocation;
    if (fileLocation.lastIndexOf("/") != -1) {
        filePathPrefix = fileLocation.substring(0, fileLocation.lastIndexOf("/") + 1); // adding 1 to include the trailing slash
        filenameToUse = fileLocation.substring(fileLocation.lastIndexOf("/") + 1);
    }

    int i1;
    if (contentType && (i1 = contentType.indexOf("boundary=")) != -1) {
        contentType = contentType.substring(i1 + 9);
        contentType = "--" + contentType;
    }

    defaultFileName = filenameToUse + "_temp";
    uploadObject = new HttpRequestFileUpload();
    uploadObject.setOverrideFilename(defaultFileName);
    uploadObject.setSavePath(imageServerPath + "/" + filePathPrefix);
    uploadObject.doUpload(request);

    clientFileName = uploadObject.getFilename();
    if (clientFileName) {
        context.clientFileName = clientFileName;
    }

    if (clientFileName && clientFileName.length() > 0) {
        if (clientFileName.lastIndexOf(".") > 0 && clientFileName.lastIndexOf(".") < clientFileName.length()) {
            filenameToUse += clientFileName.substring(clientFileName.lastIndexOf("."));
        } else {
            filenameToUse += ".jpg";
        }

        context.clientFileName = clientFileName;
        context.filenameToUse = filenameToUse;

        characterEncoding = "UTF-8"; // SCIPIO: ALWAYS use UTF-8 for filenames, not request encoding: characterEncoding = request.getCharacterEncoding();
        imageUrl = imageUrlPrefix + "/" + filePathPrefix + java.net.URLEncoder.encode(filenameToUse, characterEncoding);

        try {
            file = new File(imageServerPath + "/" + filePathPrefix, defaultFileName);
            file1 = new File(imageServerPath + "/" + filePathPrefix, filenameToUse);
            try {
                // Delete existing image files
                File targetDir = new File(imageServerPath + "/" + filePathPrefix);
                // Images are ordered by productCategoryId (${location}/${id}/${viewtype}/${sizetype})
                if (!filenameToUse.startsWith(productCategoryId + ".")) {
                    File[] files = targetDir.listFiles();
                    for(File file : files) {
                        if (file.isFile() && file.getName().contains(filenameToUse.substring(0, filenameToUse.indexOf(".")+1)) && !"original".equals(fileType)) {
                            file.delete();
                        } else if(file.isFile() && "original".equals(fileType) && !file.getName().equals(defaultFileName)) {
                            file.delete();
                        }
                    }
                    // Images aren't ordered by productCategoryId (${location}/${viewtype}/${sizetype}/${id}) !!! BE CAREFUL !!!
                } else {
                    File[] files = targetDir.listFiles();
                    for(File file : files) {
                        if (file.isFile() && !file.getName().equals(defaultFileName) && file.getName().startsWith(productCategoryId + ".")) file.delete();
                    }
                }
            } catch (Exception e) {
                Debug.logError(e, "error deleting existing file (not necessarily a problem)", module);
            }
            file.renameTo(file1);
        } catch (Exception e) {
            Debug.logError(e, module);
        }

        if (imageUrl && imageUrl.length() > 0) {
            context.imageUrl = imageUrl;

            // SCIPIO: Fixed transaction breaking screen - PUT ENTITY CHANGES HERE
            Transaction suspendedTransaction = null;
            boolean beganTransaction = false;
            try {
                if (TransactionUtil.isTransactionInPlace()) {
                    suspendedTransaction = TransactionUtil.suspend();
                }
                try {
                    beganTransaction = TransactionUtil.begin();

                    productCategory.set(fileType + "ImageUrl", imageUrl);

                    // SCIPIO: mediaProfile on Product (only)
                    // FIXME: if ORIGINAL_IMAGE_URL is defined as ProductCategoryContent, this will be ignored
                    def imageProfile = parameters.imageProfile;
                    if (imageProfile == null) {
                        imageProfile = uploadObject.getFieldValue("imageProfile");
                    }
                    if (imageProfile instanceof String) {
                        productCategory.set("imageProfile", imageProfile ? imageProfile : null);
                    }

                    // call scaleImageInAllSize
                    if ("original".equals(fileType)) {
                        productCategory.store();
                        try {
                            def servCtx = dispatcher.runSync("categoryImageAutoRescale", [userLogin:context.userLogin,
                                                                                         locale:context.locale, productCategory:productCategory, prodCatContentTypeId:"ORIGINAL_IMAGE_URL", createSizeTypeContent:true,
                                                                                         recreateExisting:true, clearCaches:true]);
                            if (!ServiceUtil.isSuccess(servCtx)) {
                                def errorMessageList = context.errorMessageList ?: [];
                                errorMessageList.addAll("categoryImageAutoRescale: " + ServiceUtil.getErrorMessage(servCtx));
                                context.errorMessageList = errorMessageList;
                                context.isError = true;
                            }
                        } catch(Exception e) {
                            Debug.logError(e, module);
                            def errorMessageList = context.errorMessageList ?: [];
                            errorMessageList.addAll("categoryImageAutoRescale: " + e.toString());
                            context.errorMessageList = errorMessageList;
                            context.isError = true;
                        }
                    } else {
                        productCategory.store();
                    }

                    TransactionUtil.commit(beganTransaction);
                } catch (Exception e) {
                    String errMsg = e.toString();
                    Debug.logError(e, errMsg, module);
                    try {
                        TransactionUtil.rollback(beganTransaction, errMsg, e);
                    } catch (GenericTransactionException e2) {
                        Debug.logError(e2, "Unable to rollback transaction", module);
                    }
                }
            } finally {
                if (suspendedTransaction != null) {
                    try {
                        TransactionUtil.resume(suspendedTransaction);
                    } catch (GenericTransactionException e) {
                        Debug.logError(e, "Error resuming suspended transaction", module);
                    }
                }
            }
        }
    }
}
