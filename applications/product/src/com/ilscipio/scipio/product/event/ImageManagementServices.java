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
package com.ilscipio.scipio.product.event;

import java.sql.Timestamp;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.GroovyUtil;
import org.ofbiz.base.util.ScriptUtil;
import org.ofbiz.base.util.UtilMisc;
import org.ofbiz.base.util.UtilProperties;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.GenericValue;
import org.ofbiz.entity.condition.EntityCondition;
import org.ofbiz.entity.util.EntityQuery;
import org.ofbiz.entity.util.EntityUtil;
import org.ofbiz.service.DispatchContext;
import org.ofbiz.service.LocalDispatcher;
import org.ofbiz.service.ServiceUtil;

/**
 * Auto-generated event class from Simple Method XML.
 *
 * <p>Generated from: component://product/script/org/ofbiz/product/imagemanagement/ImageManagementServices.xml</p>
 *
 * <p>SCIPIO: 4.0.0: Auto-generated.</p>
 */
public class ImageManagementServices {

    private static final String MODULE = ImageManagementServices.class.getName();


    /**
     * Method to upload multiple images for product
     */
    public static Map<String, Object> UploadProductImages(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        Map<String, Object> addAdditionalViewForProductMap = null;
        // set-service-fields from "parameters" to "addAdditionalViewForProductMap" for service "addMultipleuploadForProduct"
        addAdditionalViewForProductMap.putAll(UtilMisc.toMap(context));
        if (UtilValidate.isNotEmpty(context.get("_additionalImageOne_fileName"))) {
            addAdditionalViewForProductMap.put("productId", context.get("productId"));
            addAdditionalViewForProductMap.put("imageResize", context.get("imageResize"));
            addAdditionalViewForProductMap.put("uploadedFile", context.get("additionalImageOne"));
            addAdditionalViewForProductMap.put("productContentTypeId", "IMAGE");
            addAdditionalViewForProductMap.put("_uploadedFile_fileName", context.get("_additionalImageOne_fileName"));
            addAdditionalViewForProductMap.put("_uploadedFile_contentType", context.get("_additionalImageOne_contentType"));
            try {
                Map<String, Object> serviceResult = dispatcher.runSync("addMultipleuploadForProduct", addAdditionalViewForProductMap);
                if (ServiceUtil.isError(serviceResult)) {
                    return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
                }
            } catch (Exception e) {
                Debug.logError(e, "Error calling addMultipleuploadForProduct: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
            addAdditionalViewForProductMap = new HashMap<String, Object>();
        }
        if (UtilValidate.isNotEmpty(context.get("_additionalImageTwo_fileName"))) {
            addAdditionalViewForProductMap.put("productId", context.get("productId"));
            addAdditionalViewForProductMap.put("imageResize", context.get("imageResize"));
            addAdditionalViewForProductMap.put("uploadedFile", context.get("additionalImageTwo"));
            addAdditionalViewForProductMap.put("productContentTypeId", "IMAGE");
            addAdditionalViewForProductMap.put("_uploadedFile_fileName", context.get("_additionalImageTwo_fileName"));
            addAdditionalViewForProductMap.put("_uploadedFile_contentType", context.get("_additionalImageTwo_contentType"));
            try {
                Map<String, Object> serviceResult = dispatcher.runSync("addMultipleuploadForProduct", addAdditionalViewForProductMap);
                if (ServiceUtil.isError(serviceResult)) {
                    return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
                }
            } catch (Exception e) {
                Debug.logError(e, "Error calling addMultipleuploadForProduct: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
            addAdditionalViewForProductMap = new HashMap<String, Object>();
        }
        if (UtilValidate.isNotEmpty(context.get("_additionalImageThree_fileName"))) {
            addAdditionalViewForProductMap.put("productId", context.get("productId"));
            addAdditionalViewForProductMap.put("imageResize", context.get("imageResize"));
            addAdditionalViewForProductMap.put("uploadedFile", context.get("additionalImageThree"));
            addAdditionalViewForProductMap.put("productContentTypeId", "IMAGE");
            addAdditionalViewForProductMap.put("_uploadedFile_fileName", context.get("_additionalImageThree_fileName"));
            addAdditionalViewForProductMap.put("_uploadedFile_contentType", context.get("_additionalImageThree_contentType"));
            try {
                Map<String, Object> serviceResult = dispatcher.runSync("addMultipleuploadForProduct", addAdditionalViewForProductMap);
                if (ServiceUtil.isError(serviceResult)) {
                    return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
                }
            } catch (Exception e) {
                Debug.logError(e, "Error calling addMultipleuploadForProduct: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
            addAdditionalViewForProductMap = new HashMap<String, Object>();
        }
        if (UtilValidate.isNotEmpty(context.get("_additionalImageFour_fileName"))) {
            addAdditionalViewForProductMap.put("productId", context.get("productId"));
            addAdditionalViewForProductMap.put("imageResize", context.get("imageResize"));
            addAdditionalViewForProductMap.put("uploadedFile", context.get("additionalImageFour"));
            addAdditionalViewForProductMap.put("productContentTypeId", "IMAGE");
            addAdditionalViewForProductMap.put("_uploadedFile_fileName", context.get("_additionalImageFour_fileName"));
            addAdditionalViewForProductMap.put("_uploadedFile_contentType", context.get("_additionalImageFour_contentType"));
            try {
                Map<String, Object> serviceResult = dispatcher.runSync("addMultipleuploadForProduct", addAdditionalViewForProductMap);
                if (ServiceUtil.isError(serviceResult)) {
                    return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
                }
            } catch (Exception e) {
                Debug.logError(e, "Error calling addMultipleuploadForProduct: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
            addAdditionalViewForProductMap = new HashMap<String, Object>();
        }
        if (UtilValidate.isNotEmpty(context.get("_additionalImageFive_fileName"))) {
            addAdditionalViewForProductMap.put("productId", context.get("productId"));
            addAdditionalViewForProductMap.put("imageResize", context.get("imageResize"));
            addAdditionalViewForProductMap.put("uploadedFile", context.get("additionalImageFive"));
            addAdditionalViewForProductMap.put("productContentTypeId", "IMAGE");
            addAdditionalViewForProductMap.put("_uploadedFile_fileName", context.get("_additionalImageFive_fileName"));
            addAdditionalViewForProductMap.put("_uploadedFile_contentType", context.get("_additionalImageFive_contentType"));
            try {
                Map<String, Object> serviceResult = dispatcher.runSync("addMultipleuploadForProduct", addAdditionalViewForProductMap);
                if (ServiceUtil.isError(serviceResult)) {
                    return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
                }
            } catch (Exception e) {
                Debug.logError(e, "Error calling addMultipleuploadForProduct: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
            addAdditionalViewForProductMap = new HashMap<String, Object>();
        }
        if (UtilValidate.isNotEmpty(context.get("_additionalImageSix_fileName"))) {
            addAdditionalViewForProductMap.put("productId", context.get("productId"));
            addAdditionalViewForProductMap.put("imageResize", context.get("imageResize"));
            addAdditionalViewForProductMap.put("uploadedFile", context.get("additionalImageSix"));
            addAdditionalViewForProductMap.put("productContentTypeId", "IMAGE");
            addAdditionalViewForProductMap.put("_uploadedFile_fileName", context.get("_additionalImageSix_fileName"));
            addAdditionalViewForProductMap.put("_uploadedFile_contentType", context.get("_additionalImageSix_contentType"));
            try {
                Map<String, Object> serviceResult = dispatcher.runSync("addMultipleuploadForProduct", addAdditionalViewForProductMap);
                if (ServiceUtil.isError(serviceResult)) {
                    return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
                }
            } catch (Exception e) {
                Debug.logError(e, "Error calling addMultipleuploadForProduct: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
            addAdditionalViewForProductMap = new HashMap<String, Object>();
        }
        if (UtilValidate.isNotEmpty(context.get("_additionalImageSeven_fileName"))) {
            addAdditionalViewForProductMap.put("productId", context.get("productId"));
            addAdditionalViewForProductMap.put("imageResize", context.get("imageResize"));
            addAdditionalViewForProductMap.put("uploadedFile", context.get("additionalImageSeven"));
            addAdditionalViewForProductMap.put("productContentTypeId", "IMAGE");
            addAdditionalViewForProductMap.put("_uploadedFile_fileName", context.get("_additionalImageSeven_fileName"));
            addAdditionalViewForProductMap.put("_uploadedFile_contentType", context.get("_additionalImageSeven_contentType"));
            try {
                Map<String, Object> serviceResult = dispatcher.runSync("addMultipleuploadForProduct", addAdditionalViewForProductMap);
                if (ServiceUtil.isError(serviceResult)) {
                    return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
                }
            } catch (Exception e) {
                Debug.logError(e, "Error calling addMultipleuploadForProduct: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
            addAdditionalViewForProductMap = new HashMap<String, Object>();
        }
        if (UtilValidate.isNotEmpty(context.get("_additionalImageEight_fileName"))) {
            addAdditionalViewForProductMap.put("productId", context.get("productId"));
            addAdditionalViewForProductMap.put("imageResize", context.get("imageResize"));
            addAdditionalViewForProductMap.put("uploadedFile", context.get("additionalImageEight"));
            addAdditionalViewForProductMap.put("productContentTypeId", "IMAGE");
            addAdditionalViewForProductMap.put("_uploadedFile_fileName", context.get("_additionalImageEight_fileName"));
            addAdditionalViewForProductMap.put("_uploadedFile_contentType", context.get("_additionalImageEight_contentType"));
            try {
                Map<String, Object> serviceResult = dispatcher.runSync("addMultipleuploadForProduct", addAdditionalViewForProductMap);
                if (ServiceUtil.isError(serviceResult)) {
                    return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
                }
            } catch (Exception e) {
                Debug.logError(e, "Error calling addMultipleuploadForProduct: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
            addAdditionalViewForProductMap = new HashMap<String, Object>();
        }
        if (UtilValidate.isNotEmpty(context.get("_additionalImageNine_fileName"))) {
            addAdditionalViewForProductMap.put("productId", context.get("productId"));
            addAdditionalViewForProductMap.put("imageResize", context.get("imageResize"));
            addAdditionalViewForProductMap.put("uploadedFile", context.get("additionalImageNine"));
            addAdditionalViewForProductMap.put("productContentTypeId", "IMAGE");
            addAdditionalViewForProductMap.put("_uploadedFile_fileName", context.get("_additionalImageNine_fileName"));
            addAdditionalViewForProductMap.put("_uploadedFile_contentType", context.get("_additionalImageNine_contentType"));
            try {
                Map<String, Object> serviceResult = dispatcher.runSync("addMultipleuploadForProduct", addAdditionalViewForProductMap);
                if (ServiceUtil.isError(serviceResult)) {
                    return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
                }
            } catch (Exception e) {
                Debug.logError(e, "Error calling addMultipleuploadForProduct: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
            addAdditionalViewForProductMap = new HashMap<String, Object>();
        }
        if (UtilValidate.isNotEmpty(context.get("_additionalImageTen_fileName"))) {
            addAdditionalViewForProductMap.put("productId", context.get("productId"));
            addAdditionalViewForProductMap.put("imageResize", context.get("imageResize"));
            addAdditionalViewForProductMap.put("uploadedFile", context.get("additionalImageTen"));
            addAdditionalViewForProductMap.put("productContentTypeId", "IMAGE");
            addAdditionalViewForProductMap.put("_uploadedFile_fileName", context.get("_additionalImageTen_fileName"));
            addAdditionalViewForProductMap.put("_uploadedFile_contentType", context.get("_additionalImageTen_contentType"));
            try {
                Map<String, Object> serviceResult = dispatcher.runSync("addMultipleuploadForProduct", addAdditionalViewForProductMap);
                if (ServiceUtil.isError(serviceResult)) {
                    return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
                }
            } catch (Exception e) {
                Debug.logError(e, "Error calling addMultipleuploadForProduct: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
            addAdditionalViewForProductMap = new HashMap<String, Object>();
        }
        result.put("productId", context.get("productId"));

        return result;
    }


    /**
     * Remove Content From Product and Image File
     */
    public static Map<String, Object> removeProductContentAndImageFile(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        List<String> error_list = new LinkedList<>();

        List<GenericValue> contentAssocs = null;
        GenericValue lookedUpValue = null;
        GenericValue lookupPKMap = null;
        Map<String, Object> removeContent = null;
        List<GenericValue> checkDefaultImage = null;
        try {
            checkDefaultImage = EntityQuery.use(delegator)
                    .from("ProductContent")
                    .where(UtilMisc.toMap("productId", context.get("productId"), "contentId", context.get("contentId"), "productContentTypeId", "DEFAULT_IMAGE"))
                    .queryList();
        } catch (Exception e) {
            Debug.logError(e, "Error querying entity: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        GenericValue contentAssoc = null;
        if (UtilValidate.isEmpty(checkDefaultImage)) {
            try {
                contentAssocs = EntityQuery.use(delegator)
                        .from("ContentAssoc")
                        .where(UtilMisc.toMap("contentId", context.get("contentId"), "contentAssocTypeId", "IMAGE_THUMBNAIL"))
                        .queryList();
            } catch (Exception e) {
                Debug.logError(e, "Error querying entity: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
            if (UtilValidate.isNotEmpty(contentAssocs)) {
                if (contentAssocs != null) {
                    for (GenericValue contentAssocEntry : contentAssocs) {
                        try {
                            delegator.removeValue(contentAssocEntry);
                        } catch (Exception e) {
                            Debug.logError(e, "Error removing value: " + e.getMessage(), MODULE);
                            return ServiceUtil.returnError(e.getMessage());
                        }
                        removeContent.put("contentId", contentAssocEntry.get("contentIdTo"));
                        removeContent.put("productId", context.get("productId"));
                        try {
                            Map<String, Object> serviceResult = dispatcher.runSync("removeProductContentForImageManagement", removeContent);
                            if (ServiceUtil.isError(serviceResult)) {
                                return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
                            }
                        } catch (Exception e) {
                            Debug.logError(e, "Error calling removeProductContentForImageManagement: " + e.getMessage(), MODULE);
                            return ServiceUtil.returnError(e.getMessage());
                        }
                    }
                }
            }
            lookupPKMap = delegator.makeValue("ProductContent");
            lookupPKMap.setPKFields(context);
            try {
                lookedUpValue = EntityQuery.use(delegator)
                        .from(lookupPKMap.getEntityName())
                        .where(lookupPKMap)
                        .queryOne();
            } catch (Exception e) {
                Debug.logError(e, "Error finding by primary key : " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
            try {
                delegator.removeValue(lookedUpValue);
            } catch (Exception e) {
                Debug.logError(e, "Error removing value: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
            removeContent.put("contentId", context.get("contentId"));
            removeContent.put("productId", context.get("productId"));
            try {
                Map<String, Object> serviceResult = dispatcher.runSync("removeProductContentForImageManagement", removeContent);
                if (ServiceUtil.isError(serviceResult)) {
                    return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
                }
            } catch (Exception e) {
                Debug.logError(e, "Error calling removeProductContentForImageManagement: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
        } else {
            {
                String errorMsg = UtilProperties.getMessage("ProductErrorUiLabels", "ImageManagementErrorRmoveDefaultImage", locale);
                error_list.add(errorMsg);
            }
            Debug.logError("Cannot remove image contentId " + context.get("contentId"), MODULE);
            if (!error_list.isEmpty()) {
                return ServiceUtil.returnError(error_list);
            }
        }

        return result;
    }


    /**
     * Remove Content From Product
     */
    public static Map<String, Object> removeProductContentForImageManagement(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        List<GenericValue> contentRoles = null;
        try {
            contentRoles = EntityQuery.use(delegator)
                    .from("ContentRole")
                    .where(UtilMisc.toMap("contentId", context.get("contentId")))
                    .queryList();
        } catch (Exception e) {
            Debug.logError(e, "Error querying entity: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        if (UtilValidate.isNotEmpty(contentRoles)) {
            try {
                delegator.removeAll(contentRoles);
            } catch (Exception e) {
                Debug.logError(e, "Error removing value: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
        }
        List<GenericValue> contentApprovals = null;
        try {
            contentApprovals = EntityQuery.use(delegator)
                    .from("ContentApproval")
                    .queryList();
        } catch (Exception e) {
            Debug.logError(e, "Error querying ContentApproval: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        if (contentApprovals != null) {
            for (GenericValue contentApproval : contentApprovals) {
                try {
                    delegator.removeValue(contentApproval);
                } catch (Exception e) {
                    Debug.logError(e, "Error removing value: " + e.getMessage(), MODULE);
                    return ServiceUtil.returnError(e.getMessage());
                }
            }
        }
        List<GenericValue> contentKeywords = null;
        try {
            contentKeywords = EntityQuery.use(delegator)
                    .from("ContentKeyword")
                    .where(UtilMisc.toMap("contentId", context.get("contentId")))
                    .queryList();
        } catch (Exception e) {
            Debug.logError(e, "Error querying entity: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        if (contentKeywords != null) {
            for (GenericValue contentKeyword : contentKeywords) {
                try {
                    delegator.removeValue(contentKeyword);
                } catch (Exception e) {
                    Debug.logError(e, "Error removing value: " + e.getMessage(), MODULE);
                    return ServiceUtil.returnError(e.getMessage());
                }
            }
        }
        GenericValue content = null;
        try {
            content = EntityQuery.use(delegator)
                    .from("Content")
                    .where(UtilMisc.toMap("contentId", context.get("contentId")))
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying Content: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        Map<String, Object> removeContentPKMap = new HashMap<String, Object>();
        removeContentPKMap.put("contentId", context.get("contentId"));
        Map<String, Object> removeContentMap = new HashMap<String, Object>();
        // set-service-fields from "removeContentPKMap" to "removeContentMap" for service "removeContent"
        removeContentMap.putAll(UtilMisc.toMap(removeContentPKMap));
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("removeContent", removeContentMap);
            if (ServiceUtil.isError(serviceResult)) {
                return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
            }
        } catch (Exception e) {
            Debug.logError(e, "Error calling removeContent: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        Object dataResourceId = content.get("dataResourceId");
        List<GenericValue> dataResourceRoles = null;
        try {
            dataResourceRoles = EntityQuery.use(delegator)
                    .from("DataResourceRole")
                    .where(UtilMisc.toMap("dataResourceId", dataResourceId))
                    .queryList();
        } catch (Exception e) {
            Debug.logError(e, "Error querying entity: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        if (UtilValidate.isNotEmpty(dataResourceRoles)) {
            try {
                delegator.removeAll(dataResourceRoles);
            } catch (Exception e) {
                Debug.logError(e, "Error removing value: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
        }
        GenericValue dataResource = null;
        try {
            dataResource = EntityQuery.use(delegator)
                    .from("DataResource")
                    .where(UtilMisc.toMap("dataResourceId", dataResourceId))
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying DataResource: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        Map<String, Object> removeImageFile = new HashMap<String, Object>();
        removeImageFile.put("productId", context.get("productId"));
        removeImageFile.put("contentId", context.get("contentId"));
        removeImageFile.put("objectInfo", dataResource.get("objectInfo"));
        removeImageFile.put("dataResourceName", dataResource.get("dataResourceName"));
        Map<String, Object> removeImageFileMap = new HashMap<String, Object>();
        // set-service-fields from "removeImageFile" to "removeImageFileMap" for service "removeImageFileForImageManagement"
        removeImageFileMap.putAll(UtilMisc.toMap(removeImageFile));
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("removeImageFileForImageManagement", removeImageFileMap);
            if (ServiceUtil.isError(serviceResult)) {
                return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
            }
        } catch (Exception e) {
            Debug.logError(e, "Error calling removeImageFileForImageManagement: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        Map<String, Object> removeDataResourcePKMap = new HashMap<String, Object>();
        removeDataResourcePKMap.put("dataResourceId", dataResourceId);
        Map<String, Object> removeDataResourceMap = new HashMap<String, Object>();
        // set-service-fields from "removeDataResourcePKMap" to "removeDataResourceMap" for service "removeDataResource"
        removeDataResourceMap.putAll(UtilMisc.toMap(removeDataResourcePKMap));
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("removeDataResource", removeDataResourceMap);
            if (ServiceUtil.isError(serviceResult)) {
                return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
            }
        } catch (Exception e) {
            Debug.logError(e, "Error calling removeDataResource: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }

        return result;
    }


    /**
     * Set Image Detail
     */
    public static Map<String, Object> setImageDetail(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        GenericValue productContent = null;
        GenericValue dataResource = null;
        try {
            productContent = EntityQuery.use(delegator)
                    .from("ProductContent")
                    .where(context)
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying ProductContent: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        productContent.put("sequenceNum", context.get("sequenceNum"));
        try {
            delegator.store(productContent);
        } catch (Exception e) {
            Debug.logError(e, "Error storing value: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        if (UtilValidate.isNotEmpty(context.get("sequenceNum"))) {
            Map<String, Object> scriptContext = new HashMap<String, Object>();
            try {
                scriptContext.put("delegator", delegator);
                scriptContext.put("dispatcher", dispatcher);
                scriptContext.put("locale", locale);
                scriptContext.put("userLogin", userLogin);
                scriptContext.put("context", context);
                scriptContext.put("parameters", context);
                ScriptUtil.executeScript("component://product/webapp/catalog/WEB-INF/actions/imagemanagement/sortSequenceNum.groovy", null, scriptContext);
            } catch (Exception e) {
                Debug.logError(e, "Error executing Groovy script: " + e.getMessage(), MODULE);
            }
            // Extract script bindings into local scope
            Object multiPartMap = scriptContext.get("multiPartMap");
            productContent.put("sequenceNum", context.get("sequenceNum"));
            try {
                delegator.store(productContent);
            } catch (Exception e) {
                Debug.logError(e, "Error storing value: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
        }
        GenericValue content = null;
        try {
            content = EntityQuery.use(delegator)
                    .from("Content")
                    .where(context)
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying Content: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        content.put("description", context.get("description"));
        try {
            delegator.store(content);
        } catch (Exception e) {
            Debug.logError(e, "Error storing value: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        if ("IM_APPROVED".equals(content.get("statusId"))) {
            try {
                dataResource = EntityQuery.use(delegator)
                        .from("DataResource")
                        .where(UtilMisc.toMap("dataResourceId", content.get("dataResourceId")))
                        .queryOne();
            } catch (Exception e) {
                Debug.logError(e, "Error querying DataResource: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
            dataResource.put("isPublic", context.get("drIsPublic"));
            try {
                delegator.store(dataResource);
            } catch (Exception e) {
                Debug.logError(e, "Error storing value: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
        }

        return result;
    }


    /**
     * Update Status Image Management
     */
    public static Map<String, Object> updateStatusImageManagement(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        List<GenericValue> contentApprovals = null;
        GenericValue contentApproval = null;
        Long countParty = null;
        List<GenericValue> productContents = null;
        GenericValue productContent = null;
        List<GenericValue> checkApproveList = null;
        List<GenericValue> checkRejects = null;
        GenericValue content = null;
        Long countApprove = null;
        Timestamp nowTimestamp = new Timestamp(System.currentTimeMillis());
        context.put("checkStatusId", GroovyUtil.eval("import org.ofbiz.base.util.StringUtil;         checkStatusVal = parameters.checkStatusId;         statusId = StringUtil.split(checkStatusVal, \"/\");         if (statusId) return statusId[0];", UtilMisc.toMap("delegator", delegator, "dispatcher", dispatcher, "locale", locale, "userLogin", userLogin, "context", context, "parameters", context)));
        Object autoApproveImage = UtilProperties.getMessage("catalog.properties", "image.management.autoApproveImage", locale);
        Object multipleApproval = UtilProperties.getMessage("catalog.properties", "image.management.multipleApproval", locale);
        if ("Y".equals(autoApproveImage)) {
            try {
                contentApprovals = EntityQuery.use(delegator)
                        .from("ContentApproval")
                        .where(UtilMisc.toMap("contentId", context.get("contentId"), "roleTypeId", "IMAGEAPPROVER"))
                        .queryList();
            } catch (Exception e) {
                Debug.logError(e, "Error querying entity: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
            if (contentApprovals != null) {
                for (GenericValue contentApproval_iter : contentApprovals) {
                    contentApproval = contentApproval_iter;
                    contentApproval.put("approvalStatusId", context.get("checkStatusId"));
                    try {
                        delegator.store(contentApproval);
                    } catch (Exception e) {
                        Debug.logError(e, "Error storing value: " + e.getMessage(), MODULE);
                        return ServiceUtil.returnError(e.getMessage());
                    }
                }
            }
        } else {
            try {
                contentApprovals = EntityQuery.use(delegator)
                        .from("ContentApproval")
                        .where(UtilMisc.toMap("partyId", userLogin.get("partyId"), "contentId", context.get("contentId"), "roleTypeId", "IMAGEAPPROVER"))
                        .queryList();
            } catch (Exception e) {
                Debug.logError(e, "Error querying entity: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
            contentApproval = EntityUtil.getFirst((List<GenericValue>) contentApprovals);
            contentApproval.put("approvalStatusId", context.get("checkStatusId"));
            try {
                delegator.store(contentApproval);
            } catch (Exception e) {
                Debug.logError(e, "Error storing value: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
        }
        GenericValue checkReject = null;
        GenericValue checkApprove = null;
        if ("IM_REJECTED".equals(context.get("checkStatusId"))) {
            try {
                checkRejects = EntityQuery.use(delegator)
                        .from("ContentApproval")
                        .queryList();
            } catch (Exception e) {
                Debug.logError(e, "Error querying ContentApproval: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
            if (checkRejects != null) {
                for (GenericValue checkRejectEntry : checkRejects) {
                    checkRejectEntry.put("statusId", "IM_REJECTED");
                    try {
                        delegator.store(checkRejectEntry);
                    } catch (Exception e) {
                        Debug.logError(e, "Error storing value: " + e.getMessage(), MODULE);
                        return ServiceUtil.returnError(e.getMessage());
                    }
                }
            }
            try {
                content = EntityQuery.use(delegator)
                        .from("Content")
                        .where(context)
                        .queryOne();
            } catch (Exception e) {
                Debug.logError(e, "Error querying Content: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
            content.put("statusId", "IM_REJECTED");
            content.put("createdByUserLogin", userLogin.get("userLoginId"));
            try {
                delegator.store(content);
            } catch (Exception e) {
                Debug.logError(e, "Error storing value: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
        } else {
            if ("IM_APPROVED".equals(context.get("checkStatusId"))) {
                if ("Y".equals(multipleApproval)) {
                    countParty = null;
                    try {
                        countParty = EntityQuery.use(delegator)
                                .from("ContentApproval")
                                .queryCount();
                    } catch (Exception e) {
                        Debug.logError(e, "Error counting ContentApproval: " + e.getMessage(), MODULE);
                    }
                    if ("1".equals(countParty)) {
                        try {
                            content = EntityQuery.use(delegator)
                                    .from("Content")
                                    .where(context)
                                    .queryOne();
                        } catch (Exception e) {
                            Debug.logError(e, "Error querying Content: " + e.getMessage(), MODULE);
                            return ServiceUtil.returnError(e.getMessage());
                        }
                        content.put("statusId", "IM_APPROVED");
                        try {
                            delegator.store(content);
                        } catch (Exception e) {
                            Debug.logError(e, "Error storing value: " + e.getMessage(), MODULE);
                            return ServiceUtil.returnError(e.getMessage());
                        }
                        try {
                            productContents = EntityQuery.use(delegator)
                                    .from("ProductContent")
                                    .where(UtilMisc.toMap("contentId", context.get("contentId"), "productContentTypeId", "IMAGE"))
                                    .queryList();
                        } catch (Exception e) {
                            Debug.logError(e, "Error querying entity: " + e.getMessage(), MODULE);
                            return ServiceUtil.returnError(e.getMessage());
                        }
                        productContent = EntityUtil.getFirst((List<GenericValue>) productContents);
                        productContent.put("purchaseFromDate", nowTimestamp);
                        try {
                            delegator.store(productContent);
                        } catch (Exception e) {
                            Debug.logError(e, "Error storing value: " + e.getMessage(), MODULE);
                            return ServiceUtil.returnError(e.getMessage());
                        }
                    } else {
                        countApprove = null;
                        try {
                            countApprove = EntityQuery.use(delegator)
                                    .from("ContentApproval")
                                    .queryCount();
                        } catch (Exception e) {
                            Debug.logError(e, "Error counting ContentApproval: " + e.getMessage(), MODULE);
                        }
                        if (((Comparable) countApprove).compareTo(2L) >= 0) {
                            try {
                                content = EntityQuery.use(delegator)
                                        .from("Content")
                                        .where(context)
                                        .queryOne();
                            } catch (Exception e) {
                                Debug.logError(e, "Error querying Content: " + e.getMessage(), MODULE);
                                return ServiceUtil.returnError(e.getMessage());
                            }
                            content.put("statusId", "IM_APPROVED");
                            try {
                                delegator.store(content);
                            } catch (Exception e) {
                                Debug.logError(e, "Error storing value: " + e.getMessage(), MODULE);
                                return ServiceUtil.returnError(e.getMessage());
                            }
                            try {
                                productContents = EntityQuery.use(delegator)
                                        .from("ProductContent")
                                        .where(UtilMisc.toMap("contentId", context.get("contentId"), "productContentTypeId", "IMAGE"))
                                        .queryList();
                            } catch (Exception e) {
                                Debug.logError(e, "Error querying entity: " + e.getMessage(), MODULE);
                                return ServiceUtil.returnError(e.getMessage());
                            }
                            productContent = EntityUtil.getFirst((List<GenericValue>) productContents);
                            productContent.put("purchaseFromDate", nowTimestamp);
                            try {
                                delegator.store(productContent);
                            } catch (Exception e) {
                                Debug.logError(e, "Error storing value: " + e.getMessage(), MODULE);
                                return ServiceUtil.returnError(e.getMessage());
                            }
                            try {
                                checkApproveList = EntityQuery.use(delegator)
                                        .from("ContentApproval")
                                        .queryList();
                            } catch (Exception e) {
                                Debug.logError(e, "Error querying ContentApproval: " + e.getMessage(), MODULE);
                                return ServiceUtil.returnError(e.getMessage());
                            }
                            if (checkApproveList != null) {
                                for (GenericValue checkApproveEntry : checkApproveList) {
                                    checkApproveEntry.put("approvalStatusId", "IM_APPROVED");
                                    try {
                                        delegator.store(checkApproveEntry);
                                    } catch (Exception e) {
                                        Debug.logError(e, "Error storing value: " + e.getMessage(), MODULE);
                                        return ServiceUtil.returnError(e.getMessage());
                                    }
                                }
                            }
                        }
                    }
                } else {
                    try {
                        content = EntityQuery.use(delegator)
                                .from("Content")
                                .where(context)
                                .queryOne();
                    } catch (Exception e) {
                        Debug.logError(e, "Error querying Content: " + e.getMessage(), MODULE);
                        return ServiceUtil.returnError(e.getMessage());
                    }
                    content.put("statusId", "IM_APPROVED");
                    try {
                        delegator.store(content);
                    } catch (Exception e) {
                        Debug.logError(e, "Error storing value: " + e.getMessage(), MODULE);
                        return ServiceUtil.returnError(e.getMessage());
                    }
                    try {
                        productContents = EntityQuery.use(delegator)
                                .from("ProductContent")
                                .where(UtilMisc.toMap("contentId", context.get("contentId"), "productContentTypeId", "IMAGE"))
                                .queryList();
                    } catch (Exception e) {
                        Debug.logError(e, "Error querying entity: " + e.getMessage(), MODULE);
                        return ServiceUtil.returnError(e.getMessage());
                    }
                    productContent = EntityUtil.getFirst((List<GenericValue>) productContents);
                    productContent.put("purchaseFromDate", nowTimestamp);
                    try {
                        delegator.store(productContent);
                    } catch (Exception e) {
                        Debug.logError(e, "Error storing value: " + e.getMessage(), MODULE);
                        return ServiceUtil.returnError(e.getMessage());
                    }
                    try {
                        checkApproveList = EntityQuery.use(delegator)
                                .from("ContentApproval")
                                .queryList();
                    } catch (Exception e) {
                        Debug.logError(e, "Error querying ContentApproval: " + e.getMessage(), MODULE);
                        return ServiceUtil.returnError(e.getMessage());
                    }
                    if (checkApproveList != null) {
                        for (GenericValue checkApproveEntry : checkApproveList) {
                            checkApproveEntry.put("approvalStatusId", "IM_APPROVED");
                            try {
                                delegator.store(checkApproveEntry);
                            } catch (Exception e) {
                                Debug.logError(e, "Error storing value: " + e.getMessage(), MODULE);
                                return ServiceUtil.returnError(e.getMessage());
                            }
                        }
                    }
                }
            }
        }

        return result;
    }


    /**
     * Add Rejected Reason Image Management
     */
    public static Map<String, Object> addRejectedReasonImageManagement(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        GenericValue content = null;
        Timestamp nowTimestam = new Timestamp(System.currentTimeMillis());
        try {
            content = EntityQuery.use(delegator)
                    .from("Content")
                    .where(context)
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying Content: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        if (UtilValidate.isNotEmpty(context.get("description"))) {
            if ("RETAKE_PHOTO".equals(context.get("description"))) {
                content.put("description", "Re-take Photo");
            }
            if ("REMOVE_LOGO".equals(context.get("description"))) {
                content.put("description", "Remove Logo");
            }
            if ("OTHER".equals(context.get("description"))) {
                content.put("description", "Other");
            }
        } else {
            content.put("description", "Other");
        }
        try {
            delegator.store(content);
        } catch (Exception e) {
            Debug.logError(e, "Error storing value: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        List<GenericValue> productContents = null;
        try {
            productContents = EntityQuery.use(delegator)
                    .from("ProductContent")
                    .where(UtilMisc.toMap("contentId", context.get("contentId"), "productContentTypeId", "IMAGE"))
                    .queryList();
        } catch (Exception e) {
            Debug.logError(e, "Error querying entity: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        GenericValue productContent = EntityUtil.getFirst((List<GenericValue>) productContents);
        productContent.put("thruDate", nowTimestam);
        try {
            delegator.store(productContent);
        } catch (Exception e) {
            Debug.logError(e, "Error storing value: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }

        return result;
    }


    /**
     * Create Content Approval of Image
     */
    public static Map<String, Object> createImageContentApproval(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        Map<String, Object> contentApproval = null;
        Timestamp nowTimestam = new Timestamp(System.currentTimeMillis());
        List<GenericValue> partyRoles = null;
        try {
            partyRoles = EntityQuery.use(delegator)
                    .from("PartyRole")
                    .where(UtilMisc.toMap("roleTypeId", "IMAGEAPPROVER"))
                    .queryList();
        } catch (Exception e) {
            Debug.logError(e, "Error querying entity: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        if (partyRoles != null) {
            for (GenericValue partyRole : partyRoles) {
                contentApproval.put("partyId", partyRole.get("partyId"));
                contentApproval.put("contentId", context.get("contentId"));
                contentApproval.put("roleTypeId", "IMAGEAPPROVER");
                contentApproval.put("approvalDate", nowTimestam);
                contentApproval.put("approvalStatusId", "IM_PENDING");
                try {
                    Map<String, Object> serviceResult = dispatcher.runSync("createContentApproval", contentApproval);
                    if (ServiceUtil.isError(serviceResult)) {
                        return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
                    }
                } catch (Exception e) {
                    Debug.logError(e, "Error calling createContentApproval: " + e.getMessage(), MODULE);
                    return ServiceUtil.returnError(e.getMessage());
                }
            }
        }

        return result;
    }


    /**
     * Remove Content Approval of Image
     */
    public static Map<String, Object> removeImageContentApproval(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        List<GenericValue> contentApprovals = null;
        try {
            contentApprovals = EntityQuery.use(delegator)
                    .from("ContentApproval")
                    .where(UtilMisc.toMap("partyId", context.get("partyId"), "roleTypeId", "IMAGEAPPROVER"))
                    .queryList();
        } catch (Exception e) {
            Debug.logError(e, "Error querying entity: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        if (contentApprovals != null) {
            for (GenericValue contentApproval : contentApprovals) {
                try {
                    delegator.removeValue(contentApproval);
                } catch (Exception e) {
                    Debug.logError(e, "Error removing value: " + e.getMessage(), MODULE);
                    return ServiceUtil.returnError(e.getMessage());
                }
            }
        }

        return result;
    }


    /**
     * Resize Images
     */
    public static Map<String, Object> resizeImages(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        Map<String, Object> resizeImageMap = null;
        List<GenericValue> productContentAndInfos = null;
        Map<String, Object> removeImageBySizeMap = null;
        Map<String, Object> createNewImageThumbnailMap = null;
        if ("resizeAllImages".equals(context.get("resizeOption"))) {
            try {
                productContentAndInfos = EntityQuery.use(delegator)
                        .from("ProductContentAndInfo")
                        .where(UtilMisc.toMap("productId", context.get("productId"), "productContentTypeId", "IMAGE"))
                        .queryList();
            } catch (Exception e) {
                Debug.logError(e, "Error querying entity: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
            if (productContentAndInfos != null) {
                for (GenericValue productContentAndInfo : productContentAndInfos) {
                    resizeImageMap.put("productId", productContentAndInfo.get("productId"));
                    resizeImageMap.put("dataResourceName", productContentAndInfo.get("drDataResourceName"));
                    resizeImageMap.put("resizeWidth", context.get("size"));
                    try {
                        Map<String, Object> serviceResult = dispatcher.runSync("resizeImageOfProduct", resizeImageMap);
                        if (ServiceUtil.isError(serviceResult)) {
                            return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
                        }
                    } catch (Exception e) {
                        Debug.logError(e, "Error calling resizeImageOfProduct: " + e.getMessage(), MODULE);
                        return ServiceUtil.returnError(e.getMessage());
                    }
                }
            }
        }
        if ("createNewThumbnail".equals(context.get("resizeOption"))) {
            removeImageBySizeMap.put("productId", context.get("productId"));
            removeImageBySizeMap.put("mapKey", context.get("size"));
            try {
                Map<String, Object> serviceResult = dispatcher.runSync("removeImageBySize", removeImageBySizeMap);
                if (ServiceUtil.isError(serviceResult)) {
                    return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
                }
            } catch (Exception e) {
                Debug.logError(e, "Error calling removeImageBySize: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
            try {
                productContentAndInfos = EntityQuery.use(delegator)
                        .from("ProductContentAndInfo")
                        .where(UtilMisc.toMap("productId", context.get("productId"), "productContentTypeId", "IMAGE"))
                        .queryList();
            } catch (Exception e) {
                Debug.logError(e, "Error querying entity: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
            if (productContentAndInfos != null) {
                for (GenericValue productContentAndInfoEntry : productContentAndInfos) {
                    createNewImageThumbnailMap.put("productId", productContentAndInfoEntry.get("productId"));
                    createNewImageThumbnailMap.put("contentId", productContentAndInfoEntry.get("contentId"));
                    createNewImageThumbnailMap.put("dataResourceName", productContentAndInfoEntry.get("drDataResourceName"));
                    createNewImageThumbnailMap.put("drObjectInfo", productContentAndInfoEntry.get("drObjectInfo"));
                    createNewImageThumbnailMap.put("sizeWidth", context.get("size"));
                    try {
                        Map<String, Object> serviceResult = dispatcher.runSync("createNewImageThumbnail", createNewImageThumbnailMap);
                        if (ServiceUtil.isError(serviceResult)) {
                            return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
                        }
                    } catch (Exception e) {
                        Debug.logError(e, "Error calling createNewImageThumbnail: " + e.getMessage(), MODULE);
                        return ServiceUtil.returnError(e.getMessage());
                    }
                }
            }
        }

        return result;
    }


    /**
     * Remove Image By Size
     */
    public static Map<String, Object> removeImageBySize(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        List<GenericValue> contentAssocs = null;
        Map<String, Object> removeContent = null;
        List<GenericValue> productContentAndInfos = null;
        try {
            productContentAndInfos = EntityQuery.use(delegator)
                    .from("ProductContentAndInfo")
                    .queryList();
        } catch (Exception e) {
            Debug.logError(e, "Error querying ProductContentAndInfo: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        if (productContentAndInfos != null) {
            for (GenericValue productContentAndInfo : productContentAndInfos) {
                try {
                    contentAssocs = EntityQuery.use(delegator)
                            .from("ContentAssoc")
                            .where(UtilMisc.toMap("contentId", productContentAndInfo.get("contentId"), "contentAssocTypeId", "IMAGE_THUMBNAIL", "mapKey", context.get("mapKey")))
                            .queryList();
                } catch (Exception e) {
                    Debug.logError(e, "Error querying entity: " + e.getMessage(), MODULE);
                    return ServiceUtil.returnError(e.getMessage());
                }
                if (UtilValidate.isNotEmpty(contentAssocs)) {
                    if (contentAssocs != null) {
                        for (GenericValue contentAssoc : contentAssocs) {
                            try {
                                delegator.removeValue(contentAssoc);
                            } catch (Exception e) {
                                Debug.logError(e, "Error removing value: " + e.getMessage(), MODULE);
                                return ServiceUtil.returnError(e.getMessage());
                            }
                            removeContent.put("contentId", contentAssoc.get("contentIdTo"));
                            removeContent.put("productId", context.get("productId"));
                            try {
                                Map<String, Object> serviceResult = dispatcher.runSync("removeProductContentForImageManagement", removeContent);
                                if (ServiceUtil.isError(serviceResult)) {
                                    return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
                                }
                            } catch (Exception e) {
                                Debug.logError(e, "Error calling removeProductContentForImageManagement: " + e.getMessage(), MODULE);
                                return ServiceUtil.returnError(e.getMessage());
                            }
                        }
                    }
                }
            }
        }

        return result;
    }

}
