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
import org.ofbiz.base.util.UtilMisc;
import org.ofbiz.base.util.UtilProperties;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.GenericValue;
import org.ofbiz.entity.util.EntityQuery;
import org.ofbiz.entity.util.EntityUtil;
import org.ofbiz.service.DispatchContext;
import org.ofbiz.service.LocalDispatcher;
import org.ofbiz.service.ServiceUtil;

/**
 * Auto-generated event class from Simple Method XML.
 *
 * <p>Generated from: component://product/script/org/ofbiz/product/product/ProductContentServices.xml</p>
 *
 * <p>SCIPIO: 4.0.0: Auto-generated.</p>
 */
public class ProductContentServices {

    private static final String MODULE = ProductContentServices.class.getName();


    /**
     * Create Content For Product
     */
    public static Map<String, Object> createProductContent(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        GenericValue newEntity = null;
        Timestamp nowTimestamp = null;
        newEntity = delegator.makeValue("ProductContent");
        newEntity.setPKFields(context);
        newEntity.setNonPKFields(context);
        if (UtilValidate.isEmpty(newEntity.get("fromDate"))) {
            nowTimestamp = new Timestamp(System.currentTimeMillis());
            newEntity.put("fromDate", nowTimestamp);
        }
        try {
            delegator.create(newEntity);
        } catch (Exception e) {
            Debug.logError(e, "Error creating value: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        Map<String, Object> updateContent = new HashMap<String, Object>();
        // set-service-fields from "parameters" to "updateContent" for service "updateContent"
        updateContent.putAll(UtilMisc.toMap(context));
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("updateContent", updateContent);
            if (ServiceUtil.isError(serviceResult)) {
                return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
            }
        } catch (Exception e) {
            Debug.logError(e, "Error calling updateContent: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        result.put("contentId", newEntity.get("contentId"));
        result.put("productId", newEntity.get("productId"));
        result.put("productContentTypeId", newEntity.get("productContentTypeId"));

        return result;
    }


    /**
     * Update Content For Product
     */
    public static Map<String, Object> updateProductContent(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        GenericValue lookupPKMap = delegator.makeValue("ProductContent");
        lookupPKMap.setPKFields(context);
        GenericValue lookedUpValue = null;
        try {
            lookedUpValue = EntityQuery.use(delegator)
                    .from(lookupPKMap.getEntityName())
                    .where(lookupPKMap)
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error finding by primary key : " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        lookedUpValue.setNonPKFields(context);
        try {
            delegator.store(lookedUpValue);
        } catch (Exception e) {
            Debug.logError(e, "Error storing value: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        Map<String, Object> updateContent = new HashMap<String, Object>();
        // set-service-fields from "parameters" to "updateContent" for service "updateContent"
        updateContent.putAll(UtilMisc.toMap(context));
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("updateContent", updateContent);
            if (ServiceUtil.isError(serviceResult)) {
                return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
            }
        } catch (Exception e) {
            Debug.logError(e, "Error calling updateContent: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }

        return result;
    }


    /**
     * Remove Content From Product
     */
    public static Map<String, Object> removeProductContent(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        GenericValue lookupPKMap = delegator.makeValue("ProductContent");
        lookupPKMap.setPKFields(context);
        GenericValue lookedUpValue = null;
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

        return result;
    }


    /**
     * Create Email Content For Product
     */
    public static Map<String, Object> createEmailContentForProduct(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        Map<String, Object> createProductContent = new HashMap<String, Object>();
        // set-service-fields from "parameters" to "createProductContent" for service "createProductContent"
        createProductContent.putAll(UtilMisc.toMap(context));
        Map<String, Object> createEmail = new HashMap<String, Object>();
        // set-service-fields from "parameters" to "createEmail" for service "createEmailContent"
        createEmail.putAll(UtilMisc.toMap(context));
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("createEmailContent", createEmail);
            if (ServiceUtil.isError(serviceResult)) {
                return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
            }
            createProductContent.put("contentId", serviceResult.get("contentId"));
        } catch (Exception e) {
            Debug.logError(e, "Error calling createEmailContent: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("createProductContent", createProductContent);
            if (ServiceUtil.isError(serviceResult)) {
                return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
            }
        } catch (Exception e) {
            Debug.logError(e, "Error calling createProductContent: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }

        return result;
    }


    /**
     * Update Email Content For Product
     */
    public static Map<String, Object> updateEmailContentForProduct(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        Map<String, Object> updateProductContent = new HashMap<String, Object>();
        // set-service-fields from "parameters" to "updateProductContent" for service "updateProductContent"
        updateProductContent.putAll(UtilMisc.toMap(context));
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("updateProductContent", updateProductContent);
            if (ServiceUtil.isError(serviceResult)) {
                return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
            }
        } catch (Exception e) {
            Debug.logError(e, "Error calling updateProductContent: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        Map<String, Object> updateEmail = new HashMap<String, Object>();
        // set-service-fields from "parameters" to "updateEmail" for service "updateEmailContent"
        updateEmail.putAll(UtilMisc.toMap(context));
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("updateEmailContent", updateEmail);
            if (ServiceUtil.isError(serviceResult)) {
                return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
            }
        } catch (Exception e) {
            Debug.logError(e, "Error calling updateEmailContent: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }

        return result;
    }


    /**
     * Create Download Content For Product
     */
    public static Map<String, Object> createDownloadContentForProduct(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        Map<String, Object> createProductContent = new HashMap<String, Object>();
        // set-service-fields from "parameters" to "createProductContent" for service "createProductContent"
        createProductContent.putAll(UtilMisc.toMap(context));
        Map<String, Object> persistContentAndAssoc = new HashMap<String, Object>();
        // set-service-fields from "parameters" to "persistContentAndAssoc" for service "persistContentAndAssoc"
        persistContentAndAssoc.putAll(UtilMisc.toMap(context));
        persistContentAndAssoc.put("contentTypeId", "DOCUMENT");
        persistContentAndAssoc.put("dataResourceTypeId", "IMAGE_OBJECT");
        persistContentAndAssoc.put("contentName", context.get("_imageData_fileName"));
        persistContentAndAssoc.put("mimeTypeId", context.get("_imageData_contentType"));
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("persistContentAndAssoc", persistContentAndAssoc);
            if (ServiceUtil.isError(serviceResult)) {
                return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
            }
            createProductContent.put("contentId", serviceResult.get("contentId"));
        } catch (Exception e) {
            Debug.logError(e, "Error calling persistContentAndAssoc: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("createProductContent", createProductContent);
            if (ServiceUtil.isError(serviceResult)) {
                return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
            }
        } catch (Exception e) {
            Debug.logError(e, "Error calling createProductContent: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }

        return result;
    }


    /**
     * Update Email Content For Product
     */
    public static Map<String, Object> updateDownloadContentForProduct(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        Map<String, Object> updateProductContent = new HashMap<String, Object>();
        // set-service-fields from "parameters" to "updateProductContent" for service "updateProductContent"
        updateProductContent.putAll(UtilMisc.toMap(context));
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("updateProductContent", updateProductContent);
            if (ServiceUtil.isError(serviceResult)) {
                return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
            }
        } catch (Exception e) {
            Debug.logError(e, "Error calling updateProductContent: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        Map<String, Object> updateDownload = new HashMap<String, Object>();
        // set-service-fields from "parameters" to "updateDownload" for service "updateDownloadContent"
        updateDownload.putAll(UtilMisc.toMap(context));
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("updateDownloadContent", updateDownload);
            if (ServiceUtil.isError(serviceResult)) {
                return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
            }
        } catch (Exception e) {
            Debug.logError(e, "Error calling updateDownloadContent: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }

        return result;
    }


    /**
     * Create Simple Text Content For Product
     */
    public static Map<String, Object> createSimpleTextContentForProduct(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        Map<String, Object> createProductContentMap = new HashMap<String, Object>();
        // set-service-fields from "parameters" to "createProductContentMap" for service "createProductContent"
        createProductContentMap.putAll(UtilMisc.toMap(context));
        Map<String, Object> createSimpleTextMap = new HashMap<String, Object>();
        // set-service-fields from "parameters" to "createSimpleTextMap" for service "createSimpleTextContent"
        createSimpleTextMap.putAll(UtilMisc.toMap(context));
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("createSimpleTextContent", createSimpleTextMap);
            if (ServiceUtil.isError(serviceResult)) {
                return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
            }
            createProductContentMap.put("contentId", serviceResult.get("contentId"));
        } catch (Exception e) {
            Debug.logError(e, "Error calling createSimpleTextContent: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("createProductContent", createProductContentMap);
            if (ServiceUtil.isError(serviceResult)) {
                return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
            }
        } catch (Exception e) {
            Debug.logError(e, "Error calling createProductContent: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }

        return result;
    }


    /**
     * Update Simple Text Content For Product
     */
    public static Map<String, Object> updateSimpleTextContentForProduct(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        Map<String, Object> updateProductContent = new HashMap<String, Object>();
        // set-service-fields from "parameters" to "updateProductContent" for service "updateProductContent"
        updateProductContent.putAll(UtilMisc.toMap(context));
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("updateProductContent", updateProductContent);
            if (ServiceUtil.isError(serviceResult)) {
                return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
            }
        } catch (Exception e) {
            Debug.logError(e, "Error calling updateProductContent: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        Map<String, Object> updateSimpleText = new HashMap<String, Object>();
        // set-service-fields from "parameters" to "updateSimpleText" for service "updateSimpleTextContent"
        updateSimpleText.putAll(UtilMisc.toMap(context));
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("updateSimpleTextContent", updateSimpleText);
            if (ServiceUtil.isError(serviceResult)) {
                return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
            }
        } catch (Exception e) {
            Debug.logError(e, "Error calling updateSimpleTextContent: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }

        return result;
    }


    /**
     * Create Simple Text Content For Alternate Locale
     */
    public static Map<String, Object> createSimpleTextContentForAlternateLocale(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        // TODO: Call simple-method "createSimpleTextContentForAlternateLocale" from "component://content/script/org/ofbiz/content/content/LocalizedContentServices.xml"

        return result;
    }


    /**
     * Create ProductFeature-DataResource
     */
    public static Map<String, Object> createProductFeatureDataResource(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        GenericValue newEntity = delegator.makeValue("ProductFeatureDataResource");
        newEntity.setPKFields(context);
        try {
            delegator.create(newEntity);
        } catch (Exception e) {
            Debug.logError(e, "Error creating value: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }

        return result;
    }


    /**
     * Remove ProductFeature-DataResource
     */
    public static Map<String, Object> removeProductFeatureDataResource(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        GenericValue lookupPKMap = delegator.makeValue("ProductFeatureDataResource");
        lookupPKMap.setPKFields(context);
        GenericValue lookedUpValue = null;
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

        return result;
    }


    /**
     * Method to upload multiple Additional View images for product
     */
    public static Map<String, Object> uploadProductAdditionalViewImages(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        Map<String, Object> addAdditionalViewForProductMap = null;
        // set-service-fields from "parameters" to "addAdditionalViewForProductMap" for service "addAdditionalViewForProduct"
        addAdditionalViewForProductMap.putAll(UtilMisc.toMap(context));
        if (UtilValidate.isNotEmpty(context.get("additionalImageOne"))) {
            addAdditionalViewForProductMap.put("productId", context.get("productId"));
            addAdditionalViewForProductMap.put("uploadedFile", context.get("additionalImageOne"));
            addAdditionalViewForProductMap.put("productContentTypeId", "ADDITIONAL_IMAGE_1");
            addAdditionalViewForProductMap.put("_uploadedFile_fileName", context.get("_additionalImageOne_fileName"));
            addAdditionalViewForProductMap.put("_uploadedFile_contentType", context.get("_additionalImageOne_contentType"));
            addAdditionalViewForProductMap.put("imageProfile", context.get("additionalImageOne_imageProfile"));
            try {
                Map<String, Object> serviceResult = dispatcher.runSync("addAdditionalViewForProduct", addAdditionalViewForProductMap);
                if (ServiceUtil.isError(serviceResult)) {
                    return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
                }
            } catch (Exception e) {
                Debug.logError(e, "Error calling addAdditionalViewForProduct: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
            addAdditionalViewForProductMap = new HashMap<String, Object>();
        }
        if (UtilValidate.isNotEmpty(context.get("additionalImageTwo"))) {
            addAdditionalViewForProductMap.put("productId", context.get("productId"));
            addAdditionalViewForProductMap.put("uploadedFile", context.get("additionalImageTwo"));
            addAdditionalViewForProductMap.put("productContentTypeId", "ADDITIONAL_IMAGE_2");
            addAdditionalViewForProductMap.put("_uploadedFile_fileName", context.get("_additionalImageTwo_fileName"));
            addAdditionalViewForProductMap.put("_uploadedFile_contentType", context.get("_additionalImageTwo_contentType"));
            addAdditionalViewForProductMap.put("imageProfile", context.get("additionalImageTwo_imageProfile"));
            try {
                Map<String, Object> serviceResult = dispatcher.runSync("addAdditionalViewForProduct", addAdditionalViewForProductMap);
                if (ServiceUtil.isError(serviceResult)) {
                    return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
                }
            } catch (Exception e) {
                Debug.logError(e, "Error calling addAdditionalViewForProduct: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
            addAdditionalViewForProductMap = new HashMap<String, Object>();
        }
        if (UtilValidate.isNotEmpty(context.get("additionalImageThree"))) {
            addAdditionalViewForProductMap.put("productId", context.get("productId"));
            addAdditionalViewForProductMap.put("uploadedFile", context.get("additionalImageThree"));
            addAdditionalViewForProductMap.put("productContentTypeId", "ADDITIONAL_IMAGE_3");
            addAdditionalViewForProductMap.put("_uploadedFile_fileName", context.get("_additionalImageThree_fileName"));
            addAdditionalViewForProductMap.put("_uploadedFile_contentType", context.get("_additionalImageThree_contentType"));
            addAdditionalViewForProductMap.put("imageProfile", context.get("additionalImageThree_imageProfile"));
            try {
                Map<String, Object> serviceResult = dispatcher.runSync("addAdditionalViewForProduct", addAdditionalViewForProductMap);
                if (ServiceUtil.isError(serviceResult)) {
                    return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
                }
            } catch (Exception e) {
                Debug.logError(e, "Error calling addAdditionalViewForProduct: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
            addAdditionalViewForProductMap = new HashMap<String, Object>();
        }
        if (UtilValidate.isNotEmpty(context.get("additionalImageFour"))) {
            addAdditionalViewForProductMap.put("productId", context.get("productId"));
            addAdditionalViewForProductMap.put("uploadedFile", context.get("additionalImageFour"));
            addAdditionalViewForProductMap.put("productContentTypeId", "ADDITIONAL_IMAGE_4");
            addAdditionalViewForProductMap.put("_uploadedFile_fileName", context.get("_additionalImageFour_fileName"));
            addAdditionalViewForProductMap.put("_uploadedFile_contentType", context.get("_additionalImageFour_contentType"));
            addAdditionalViewForProductMap.put("imageProfile", context.get("additionalImageFour_imageProfile"));
            try {
                Map<String, Object> serviceResult = dispatcher.runSync("addAdditionalViewForProduct", addAdditionalViewForProductMap);
                if (ServiceUtil.isError(serviceResult)) {
                    return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
                }
            } catch (Exception e) {
                Debug.logError(e, "Error calling addAdditionalViewForProduct: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
            addAdditionalViewForProductMap = new HashMap<String, Object>();
        }
        result.put("productId", context.get("productId"));

        return result;
    }


    /**
     * Create Content For Product Promo
     */
    public static Map<String, Object> createProductPromoContent(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        GenericValue newEntity = null;
        Timestamp nowTimestamp = null;
        newEntity = delegator.makeValue("ProductPromoContent");
        newEntity.setPKFields(context);
        newEntity.setNonPKFields(context);
        if (UtilValidate.isEmpty(newEntity.get("fromDate"))) {
            nowTimestamp = new Timestamp(System.currentTimeMillis());
            newEntity.put("fromDate", nowTimestamp);
        }
        try {
            delegator.create(newEntity);
        } catch (Exception e) {
            Debug.logError(e, "Error creating value: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        result.put("contentId", newEntity.get("contentId"));
        result.put("productPromoId", newEntity.get("productPromoId"));
        result.put("productPromoContentTypeId", newEntity.get("productPromoContentTypeId"));

        return result;
    }


    /**
     * Update Content For Product Promo
     */
    public static Map<String, Object> updateProductPromoContent(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        GenericValue lookupPKMap = delegator.makeValue("ProductPromoContent");
        lookupPKMap.setPKFields(context);
        GenericValue lookedUpValue = null;
        try {
            lookedUpValue = EntityQuery.use(delegator)
                    .from(lookupPKMap.getEntityName())
                    .where(lookupPKMap)
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error finding by primary key : " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        lookedUpValue.setNonPKFields(context);
        try {
            delegator.store(lookedUpValue);
        } catch (Exception e) {
            Debug.logError(e, "Error storing value: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }

        return result;
    }


    /**
     * Remove Content From Product Promo
     */
    public static Map<String, Object> removeProductPromoContent(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        GenericValue lookedUpValue = null;
        Timestamp nowTimestamp = null;
        GenericValue lookupPKMap = delegator.makeValue("ProductPromoContent");
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
        lookedUpValue.setNonPKFields(context);
        if (UtilValidate.isEmpty(lookedUpValue.get("thruDate"))) {
            nowTimestamp = new Timestamp(System.currentTimeMillis());
            lookedUpValue.put("thruDate", nowTimestamp);
        }
        try {
            delegator.store(lookedUpValue);
        } catch (Exception e) {
            Debug.logError(e, "Error storing value: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }

        return result;
    }


    /**
     * Update Product SEO
     */
    public static Map<String, Object> updateContentSEOForProduct(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        Map<String, Object> createTextContentMap = null;
        List<GenericValue> productContents = null;
        GenericValue productContent = null;
        GenericValue electronicText = null;
        if (UtilValidate.isNotEmpty(context.get("title"))) {
            try {
                productContents = EntityQuery.use(delegator)
                        .from("ProductContentAndInfo")
                        .where(UtilMisc.toMap("productId", context.get("productId"), "productContentTypeId", "PAGE_TITLE"))
                        .queryList();
            } catch (Exception e) {
                Debug.logError(e, "Error querying entity: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
            if (UtilValidate.isNotEmpty(productContents)) {
                productContent = EntityUtil.getFirst((List<GenericValue>) productContents);
                try {
                    electronicText = EntityQuery.use(delegator)
                            .from("ElectronicText")
                            .where(UtilMisc.toMap("dataResourceId", productContent.get("dataResourceId")))
                            .queryOne();
                } catch (Exception e) {
                    Debug.logError(e, "Error querying ElectronicText: " + e.getMessage(), MODULE);
                    return ServiceUtil.returnError(e.getMessage());
                }
                if (UtilValidate.isNotEmpty(electronicText)) {
                    electronicText.put("textData", context.get("title"));
                    try {
                        delegator.store(electronicText);
                    } catch (Exception e) {
                        Debug.logError(e, "Error storing value: " + e.getMessage(), MODULE);
                        return ServiceUtil.returnError(e.getMessage());
                    }
                }
            } else {
                createTextContentMap.put("productId", context.get("productId"));
                createTextContentMap.put("productContentTypeId", "PAGE_TITLE");
                createTextContentMap.put("text", context.get("title"));
                try {
                    Map<String, Object> serviceResult = dispatcher.runSync("createSimpleTextContentForProduct", createTextContentMap);
                    if (ServiceUtil.isError(serviceResult)) {
                        return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
                    }
                } catch (Exception e) {
                    Debug.logError(e, "Error calling createSimpleTextContentForProduct: " + e.getMessage(), MODULE);
                    return ServiceUtil.returnError(e.getMessage());
                }
            }
        }
        if (UtilValidate.isNotEmpty(context.get("metaKeyword"))) {
            try {
                productContents = EntityQuery.use(delegator)
                        .from("ProductContentAndInfo")
                        .where(UtilMisc.toMap("productId", context.get("productId"), "productContentTypeId", "META_KEYWORD"))
                        .queryList();
            } catch (Exception e) {
                Debug.logError(e, "Error querying entity: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
            if (UtilValidate.isNotEmpty(productContents)) {
                productContent = EntityUtil.getFirst((List<GenericValue>) productContents);
                try {
                    electronicText = EntityQuery.use(delegator)
                            .from("ElectronicText")
                            .where(UtilMisc.toMap("dataResourceId", productContent.get("dataResourceId")))
                            .queryOne();
                } catch (Exception e) {
                    Debug.logError(e, "Error querying ElectronicText: " + e.getMessage(), MODULE);
                    return ServiceUtil.returnError(e.getMessage());
                }
                if (UtilValidate.isNotEmpty(electronicText)) {
                    electronicText.put("textData", context.get("metaKeyword"));
                    try {
                        delegator.store(electronicText);
                    } catch (Exception e) {
                        Debug.logError(e, "Error storing value: " + e.getMessage(), MODULE);
                        return ServiceUtil.returnError(e.getMessage());
                    }
                }
            } else {
                createTextContentMap.put("productId", context.get("productId"));
                createTextContentMap.put("productContentTypeId", "META_KEYWORD");
                createTextContentMap.put("text", context.get("metaKeyword"));
                try {
                    Map<String, Object> serviceResult = dispatcher.runSync("createSimpleTextContentForProduct", createTextContentMap);
                    if (ServiceUtil.isError(serviceResult)) {
                        return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
                    }
                } catch (Exception e) {
                    Debug.logError(e, "Error calling createSimpleTextContentForProduct: " + e.getMessage(), MODULE);
                    return ServiceUtil.returnError(e.getMessage());
                }
            }
        }
        if (UtilValidate.isNotEmpty(context.get("metaDescription"))) {
            try {
                productContents = EntityQuery.use(delegator)
                        .from("ProductContentAndInfo")
                        .where(UtilMisc.toMap("productId", context.get("productId"), "productContentTypeId", "META_DESCRIPTION"))
                        .queryList();
            } catch (Exception e) {
                Debug.logError(e, "Error querying entity: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
            if (UtilValidate.isNotEmpty(productContents)) {
                productContent = EntityUtil.getFirst((List<GenericValue>) productContents);
                try {
                    electronicText = EntityQuery.use(delegator)
                            .from("ElectronicText")
                            .where(UtilMisc.toMap("dataResourceId", productContent.get("dataResourceId")))
                            .queryOne();
                } catch (Exception e) {
                    Debug.logError(e, "Error querying ElectronicText: " + e.getMessage(), MODULE);
                    return ServiceUtil.returnError(e.getMessage());
                }
                if (UtilValidate.isNotEmpty(electronicText)) {
                    electronicText.put("textData", context.get("metaDescription"));
                    try {
                        delegator.store(electronicText);
                    } catch (Exception e) {
                        Debug.logError(e, "Error storing value: " + e.getMessage(), MODULE);
                        return ServiceUtil.returnError(e.getMessage());
                    }
                }
            } else {
                createTextContentMap.put("productId", context.get("productId"));
                createTextContentMap.put("productContentTypeId", "META_DESCRIPTION");
                createTextContentMap.put("text", context.get("metaDescription"));
                try {
                    Map<String, Object> serviceResult = dispatcher.runSync("createSimpleTextContentForProduct", createTextContentMap);
                    if (ServiceUtil.isError(serviceResult)) {
                        return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
                    }
                } catch (Exception e) {
                    Debug.logError(e, "Error calling createSimpleTextContentForProduct: " + e.getMessage(), MODULE);
                    return ServiceUtil.returnError(e.getMessage());
                }
            }
        }

        return result;
    }

}
