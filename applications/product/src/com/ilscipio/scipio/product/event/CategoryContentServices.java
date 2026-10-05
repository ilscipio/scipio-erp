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
 * <p>Generated from: component://product/script/org/ofbiz/product/category/CategoryContentServices.xml</p>
 *
 * <p>SCIPIO: 4.0.0: Auto-generated.</p>
 */
public class CategoryContentServices {

    private static final String MODULE = CategoryContentServices.class.getName();


    /**
     * Create Content For Product Category
     */
    public static Map<String, Object> createCategoryContent(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        GenericValue newEntity = null;
        Timestamp nowTimestamp = null;
        newEntity = delegator.makeValue("ProductCategoryContent");
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
        result.put("productCategoryId", newEntity.get("productCategoryId"));
        result.put("prodCatContentTypeId", newEntity.get("prodCatContentTypeId"));

        return result;
    }


    /**
     * Update Content For Category
     */
    public static Map<String, Object> updateCategoryContent(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        GenericValue lookupPKMap = delegator.makeValue("ProductCategoryContent");
        lookupPKMap.setPKFields(context);
        GenericValue lookedUpValue = null;
        try {
            lookedUpValue = EntityQuery.use(delegator)
                    .from("ProductCategoryContent")
                    .where(lookupPKMap)
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error finding by primary key ProductCategoryContent: " + e.getMessage(), MODULE);
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
            result.put("contentId", serviceResult.get("contentId"));
        } catch (Exception e) {
            Debug.logError(e, "Error calling updateContent: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }

        return result;
    }


    /**
     * Remove Content From Category
     */
    public static Map<String, Object> removeCategoryContent(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        GenericValue lookupPKMap = delegator.makeValue("ProductCategoryContent");
        lookupPKMap.setPKFields(context);
        GenericValue lookedUpValue = null;
        try {
            lookedUpValue = EntityQuery.use(delegator)
                    .from("ProductCategoryContent")
                    .where(lookupPKMap)
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error finding by primary key ProductCategoryContent: " + e.getMessage(), MODULE);
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
     * Create Simple Text Content For Product Category
     */
    public static Map<String, Object> createSimpleTextContentForCategory(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        Map<String, Object> createCategoryContentMap = new HashMap<String, Object>();
        // set-service-fields from "parameters" to "createCategoryContentMap" for service "createCategoryContent"
        createCategoryContentMap.putAll(UtilMisc.toMap(context));
        Map<String, Object> createSimpleTextMap = new HashMap<String, Object>();
        // set-service-fields from "parameters" to "createSimpleTextMap" for service "createSimpleTextContent"
        createSimpleTextMap.putAll(UtilMisc.toMap(context));
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("createSimpleTextContent", createSimpleTextMap);
            if (ServiceUtil.isError(serviceResult)) {
                return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
            }
            createCategoryContentMap.put("contentId", serviceResult.get("contentId"));
        } catch (Exception e) {
            Debug.logError(e, "Error calling createSimpleTextContent: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("createCategoryContent", createCategoryContentMap);
            if (ServiceUtil.isError(serviceResult)) {
                return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
            }
            result.put("contentId", serviceResult.get("contentId"));
        } catch (Exception e) {
            Debug.logError(e, "Error calling createCategoryContent: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }

        return result;
    }


    /**
     * Update Simple Text Content For Product Category
     */
    public static Map<String, Object> updateSimpleTextContentForCategory(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        Map<String, Object> updateCategoryContent = new HashMap<String, Object>();
        // set-service-fields from "parameters" to "updateCategoryContent" for service "updateCategoryContent"
        updateCategoryContent.putAll(UtilMisc.toMap(context));
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("updateCategoryContent", updateCategoryContent);
            if (ServiceUtil.isError(serviceResult)) {
                return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
            }
            result.put("contentId", serviceResult.get("contentId"));
        } catch (Exception e) {
            Debug.logError(e, "Error calling updateCategoryContent: " + e.getMessage(), MODULE);
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
     * Update SEO Content For Product Category
     */
    public static Map<String, Object> updateContentSEOForCategory(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        List<GenericValue> productCategoryContents = null;
        GenericValue productCategoryContent = null;
        Map<String, Object> createTextContentMap = null;
        GenericValue electronicText = null;
        if (UtilValidate.isNotEmpty(context.get("title"))) {
            try {
                productCategoryContents = EntityQuery.use(delegator)
                        .from("ProductCategoryContentAndInfo")
                        .where(UtilMisc.toMap("productCategoryId", context.get("productCategoryId"), "prodCatContentTypeId", "PAGE_TITLE"))
                        .queryList();
            } catch (Exception e) {
                Debug.logError(e, "Error querying entity: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
            if (UtilValidate.isNotEmpty(productCategoryContents)) {
                productCategoryContent = EntityUtil.getFirst((List<GenericValue>) productCategoryContents);
                try {
                    electronicText = EntityQuery.use(delegator)
                            .from("ElectronicText")
                            .where(UtilMisc.toMap("dataResourceId", productCategoryContent.get("dataResourceId")))
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
                result.put("contentId", productCategoryContent.get("contentId"));
            } else {
                createTextContentMap.put("productCategoryId", context.get("productCategoryId"));
                createTextContentMap.put("prodCatContentTypeId", "PAGE_TITLE");
                createTextContentMap.put("text", context.get("title"));
                try {
                    Map<String, Object> serviceResult = dispatcher.runSync("createSimpleTextContentForCategory", createTextContentMap);
                    if (ServiceUtil.isError(serviceResult)) {
                        return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
                    }
                    result.put("contentId", serviceResult.get("contentId"));
                } catch (Exception e) {
                    Debug.logError(e, "Error calling createSimpleTextContentForCategory: " + e.getMessage(), MODULE);
                    return ServiceUtil.returnError(e.getMessage());
                }
            }
        }
        if (UtilValidate.isNotEmpty(context.get("metaKeyword"))) {
            try {
                productCategoryContents = EntityQuery.use(delegator)
                        .from("ProductCategoryContentAndInfo")
                        .where(UtilMisc.toMap("productCategoryId", context.get("productCategoryId"), "prodCatContentTypeId", "META_KEYWORD"))
                        .queryList();
            } catch (Exception e) {
                Debug.logError(e, "Error querying entity: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
            if (UtilValidate.isNotEmpty(productCategoryContents)) {
                productCategoryContent = EntityUtil.getFirst((List<GenericValue>) productCategoryContents);
                try {
                    electronicText = EntityQuery.use(delegator)
                            .from("ElectronicText")
                            .where(UtilMisc.toMap("dataResourceId", productCategoryContent.get("dataResourceId")))
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
                result.put("contentId", productCategoryContent.get("contentId"));
            } else {
                createTextContentMap.put("productCategoryId", context.get("productCategoryId"));
                createTextContentMap.put("prodCatContentTypeId", "META_KEYWORD");
                createTextContentMap.put("text", context.get("metaKeyword"));
                try {
                    Map<String, Object> serviceResult = dispatcher.runSync("createSimpleTextContentForCategory", createTextContentMap);
                    if (ServiceUtil.isError(serviceResult)) {
                        return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
                    }
                    result.put("contentId", serviceResult.get("contentId"));
                } catch (Exception e) {
                    Debug.logError(e, "Error calling createSimpleTextContentForCategory: " + e.getMessage(), MODULE);
                    return ServiceUtil.returnError(e.getMessage());
                }
            }
        }
        if (UtilValidate.isNotEmpty(context.get("metaDescription"))) {
            try {
                productCategoryContents = EntityQuery.use(delegator)
                        .from("ProductCategoryContentAndInfo")
                        .where(UtilMisc.toMap("productCategoryId", context.get("productCategoryId"), "prodCatContentTypeId", "META_DESCRIPTION"))
                        .queryList();
            } catch (Exception e) {
                Debug.logError(e, "Error querying entity: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
            if (UtilValidate.isNotEmpty(productCategoryContents)) {
                productCategoryContent = EntityUtil.getFirst((List<GenericValue>) productCategoryContents);
                try {
                    electronicText = EntityQuery.use(delegator)
                            .from("ElectronicText")
                            .where(UtilMisc.toMap("dataResourceId", productCategoryContent.get("dataResourceId")))
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
                result.put("contentId", productCategoryContent.get("contentId"));
            } else {
                createTextContentMap.put("productCategoryId", context.get("productCategoryId"));
                createTextContentMap.put("prodCatContentTypeId", "META_DESCRIPTION");
                createTextContentMap.put("text", context.get("metaDescription"));
                try {
                    Map<String, Object> serviceResult = dispatcher.runSync("createSimpleTextContentForCategory", createTextContentMap);
                    if (ServiceUtil.isError(serviceResult)) {
                        return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
                    }
                    result.put("contentId", serviceResult.get("contentId"));
                } catch (Exception e) {
                    Debug.logError(e, "Error calling createSimpleTextContentForCategory: " + e.getMessage(), MODULE);
                    return ServiceUtil.returnError(e.getMessage());
                }
            }
        }

        return result;
    }


    /**
     * Create Related URL Content For Product Category
     */
    public static Map<String, Object> createRelatedUrlContentForCategory(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        Map<String, Object> dataResource = new HashMap<String, Object>();
        dataResource.put("dataResourceName", context.get("title"));
        dataResource.put("dataResourceTypeId", "URL_RESOURCE");
        dataResource.put("mimeTypeId", "text/plain");
        Object url = "" + GroovyUtil.eval("temp = parameters.url;             temp = temp.trim();             if(temp.indexOf(\"http://\") == 0) return temp;             if(temp.indexOf(\"https://\") == 0) return temp;             if(temp.indexOf(\"http://\") != 0) return \"http://\" + temp;", UtilMisc.toMap("delegator", delegator, "dispatcher", dispatcher, "locale", locale, "userLogin", userLogin, "context", context, "parameters", context));
        dataResource.put("objectInfo", url);
        dataResource.put("localeString", context.get("localeString"));
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("createDataResource", dataResource);
            if (ServiceUtil.isError(serviceResult)) {
                return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
            }
            context.put("dataResourceId", serviceResult.get("dataResourceId"));
        } catch (Exception e) {
            Debug.logError(e, "Error calling createDataResource: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        Map<String, Object> content = new HashMap<String, Object>();
        content.put("contentTypeId", "DOCUMENT");
        content.put("dataResourceId", context.get("dataResourceId"));
        content.put("contentName", context.get("title"));
        content.put("description", context.get("description"));
        content.put("localeString", context.get("localeString"));
        content.put("createdByUserLogin", userLogin.get("userLoginId"));
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("createContent", content);
            if (ServiceUtil.isError(serviceResult)) {
                return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
            }
            context.put("contentId", serviceResult.get("contentId"));
        } catch (Exception e) {
            Debug.logError(e, "Error calling createContent: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        Map<String, Object> createCategoryContentMap = new HashMap<String, Object>();
        // set-service-fields from "parameters" to "createCategoryContentMap" for service "createCategoryContent"
        createCategoryContentMap.putAll(UtilMisc.toMap(context));
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("createCategoryContent", createCategoryContentMap);
            if (ServiceUtil.isError(serviceResult)) {
                return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
            }
            result.put("contentId", serviceResult.get("contentId"));
        } catch (Exception e) {
            Debug.logError(e, "Error calling createCategoryContent: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }

        return result;
    }


    /**
     * Update Related URL Content For Product Category
     */
    public static Map<String, Object> updateRelatedUrlContentForCategory(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        Map<String, Object> updateCategoryContent = new HashMap<String, Object>();
        // set-service-fields from "parameters" to "updateCategoryContent" for service "updateCategoryContent"
        updateCategoryContent.putAll(UtilMisc.toMap(context));
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("updateCategoryContent", updateCategoryContent);
            if (ServiceUtil.isError(serviceResult)) {
                return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
            }
        } catch (Exception e) {
            Debug.logError(e, "Error calling updateCategoryContent: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        Map<String, Object> dataResource = new HashMap<String, Object>();
        dataResource.put("dataResourceId", context.get("dataResourceId"));
        dataResource.put("dataResourceName", context.get("title"));
        dataResource.put("objectInfo", context.get("url"));
        dataResource.put("localeString", context.get("localeString"));
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("updateDataResource", dataResource);
            if (ServiceUtil.isError(serviceResult)) {
                return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
            }
        } catch (Exception e) {
            Debug.logError(e, "Error calling updateDataResource: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        Map<String, Object> updateContent = new HashMap<String, Object>();
        updateContent.put("contentId", context.get("contentId"));
        updateContent.put("contentName", context.get("title"));
        updateContent.put("description", context.get("description"));
        updateContent.put("localeString", context.get("localeString"));
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("updateContent", updateContent);
            if (ServiceUtil.isError(serviceResult)) {
                return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
            }
            result.put("contentId", serviceResult.get("contentId"));
        } catch (Exception e) {
            Debug.logError(e, "Error calling updateContent: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }

        return result;
    }


    /**
     * Create Download Content For Category
     */
    public static Map<String, Object> createDownloadContentForCategory(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        Map<String, Object> createCategoryContent = new HashMap<String, Object>();
        // set-service-fields from "parameters" to "createCategoryContent" for service "createCategoryContent"
        createCategoryContent.putAll(UtilMisc.toMap(context));
        Map<String, Object> data = new HashMap<String, Object>();
        data.put("dataResourceTypeId", context.get("dataResourceTypeId"));
        data.put("dataResourceName", context.get("_imageData_fileName"));
        data.put("mimeTypeId", context.get("_imageData_contentType"));
        data.put("uploadedFile", context.get("imageData"));
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("createDataResource", data);
            if (ServiceUtil.isError(serviceResult)) {
                return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
            }
            context.put("dataResourceId", serviceResult.get("dataResourceId"));
        } catch (Exception e) {
            Debug.logError(e, "Error calling createDataResource: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        Map<String, Object> attachMap = new HashMap<String, Object>();
        // set-service-fields from "parameters" to "attachMap" for service "attachUploadToDataResource"
        attachMap.putAll(UtilMisc.toMap(context));
        attachMap.put("uploadedFile", context.get("imageData"));
        attachMap.put("_uploadedFile_fileName", context.get("_imageData_fileName"));
        attachMap.put("_uploadedFile_contentType", context.get("_imageData_contentType"));
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("attachUploadToDataResource", attachMap);
            if (ServiceUtil.isError(serviceResult)) {
                return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
            }
        } catch (Exception e) {
            Debug.logError(e, "Error calling attachUploadToDataResource: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        Map<String, Object> contentMap = new HashMap<String, Object>();
        // set-service-fields from "parameters" to "contentMap" for service "createContentFromDataResource"
        contentMap.putAll(UtilMisc.toMap(context));
        contentMap.put("contentTypeId", "DOCUMENT");
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("createContentFromDataResource", contentMap);
            if (ServiceUtil.isError(serviceResult)) {
                return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
            }
            createCategoryContent.put("contentId", serviceResult.get("contentId"));
        } catch (Exception e) {
            Debug.logError(e, "Error calling createContentFromDataResource: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        createCategoryContent.put("contentId", context.get("contentId"));
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("createCategoryContent", createCategoryContent);
            if (ServiceUtil.isError(serviceResult)) {
                return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
            }
            result.put("contentId", serviceResult.get("contentId"));
        } catch (Exception e) {
            Debug.logError(e, "Error calling createCategoryContent: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }

        return result;
    }


    /**
     * Update Download Content For Category
     */
    public static Map<String, Object> updateDownloadContentForCategory(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        Map<String, Object> attachMap = new HashMap<String, Object>();
        attachMap.put("uploadedFile", context.get("imageData"));
        attachMap.put("_uploadedFile_fileName", context.get("_imageData_fileName"));
        attachMap.put("_uploadedFile_contentType", context.get("_imageData_contentType"));
        attachMap.put("mimeTypeId", context.get("_imageData_contentType"));
        attachMap.put("dataResourceId", context.get("fileDataResourceId"));
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("attachUploadToDataResource", attachMap);
            if (ServiceUtil.isError(serviceResult)) {
                return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
            }
        } catch (Exception e) {
            Debug.logError(e, "Error calling attachUploadToDataResource: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        Map<String, Object> updateCategoryContent = new HashMap<String, Object>();
        // set-service-fields from "parameters" to "updateCategoryContent" for service "updateCategoryContent"
        updateCategoryContent.putAll(UtilMisc.toMap(context));
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("updateCategoryContent", updateCategoryContent);
            if (ServiceUtil.isError(serviceResult)) {
                return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
            }
            result.put("contentId", serviceResult.get("contentId"));
        } catch (Exception e) {
            Debug.logError(e, "Error calling updateCategoryContent: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }

        return result;
    }


    /**
     * Method to upload multiple Additional View images for category
     */
    public static Map<String, Object> uploadCategoryAdditionalViewImages(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        Map<String, Object> addAdditionalViewForCategoryMap = null;
        // set-service-fields from "parameters" to "addAdditionalViewForCategoryMap" for service "addAdditionalViewForCategory"
        addAdditionalViewForCategoryMap.putAll(UtilMisc.toMap(context));
        if (UtilValidate.isNotEmpty(context.get("additionalImageOne"))) {
            addAdditionalViewForCategoryMap.put("productCategoryId", context.get("productCategoryId"));
            addAdditionalViewForCategoryMap.put("uploadedFile", context.get("additionalImageOne"));
            addAdditionalViewForCategoryMap.put("prodCatContentTypeId", "ADDITIONAL_IMAGE_1");
            addAdditionalViewForCategoryMap.put("_uploadedFile_fileName", context.get("_additionalImageOne_fileName"));
            addAdditionalViewForCategoryMap.put("_uploadedFile_contentType", context.get("_additionalImageOne_contentType"));
            addAdditionalViewForCategoryMap.put("imageProfile", context.get("additionalImageOne_imageProfile"));
            try {
                Map<String, Object> serviceResult = dispatcher.runSync("addAdditionalViewForCategory", addAdditionalViewForCategoryMap);
                if (ServiceUtil.isError(serviceResult)) {
                    return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
                }
            } catch (Exception e) {
                Debug.logError(e, "Error calling addAdditionalViewForCategory: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
            addAdditionalViewForCategoryMap = new HashMap<String, Object>();
        }
        if (UtilValidate.isNotEmpty(context.get("additionalImageTwo"))) {
            addAdditionalViewForCategoryMap.put("productCategoryId", context.get("productCategoryId"));
            addAdditionalViewForCategoryMap.put("uploadedFile", context.get("additionalImageTwo"));
            addAdditionalViewForCategoryMap.put("prodCatContentTypeId", "ADDITIONAL_IMAGE_2");
            addAdditionalViewForCategoryMap.put("_uploadedFile_fileName", context.get("_additionalImageTwo_fileName"));
            addAdditionalViewForCategoryMap.put("_uploadedFile_contentType", context.get("_additionalImageTwo_contentType"));
            addAdditionalViewForCategoryMap.put("imageProfile", context.get("additionalImageTwo_imageProfile"));
            try {
                Map<String, Object> serviceResult = dispatcher.runSync("addAdditionalViewForCategory", addAdditionalViewForCategoryMap);
                if (ServiceUtil.isError(serviceResult)) {
                    return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
                }
            } catch (Exception e) {
                Debug.logError(e, "Error calling addAdditionalViewForCategory: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
            addAdditionalViewForCategoryMap = new HashMap<String, Object>();
        }
        if (UtilValidate.isNotEmpty(context.get("additionalImageThree"))) {
            addAdditionalViewForCategoryMap.put("productCategoryId", context.get("productCategoryId"));
            addAdditionalViewForCategoryMap.put("uploadedFile", context.get("additionalImageThree"));
            addAdditionalViewForCategoryMap.put("prodCatContentTypeId", "ADDITIONAL_IMAGE_3");
            addAdditionalViewForCategoryMap.put("_uploadedFile_fileName", context.get("_additionalImageThree_fileName"));
            addAdditionalViewForCategoryMap.put("_uploadedFile_contentType", context.get("_additionalImageThree_contentType"));
            addAdditionalViewForCategoryMap.put("imageProfile", context.get("additionalImageThree_imageProfile"));
            try {
                Map<String, Object> serviceResult = dispatcher.runSync("addAdditionalViewForCategory", addAdditionalViewForCategoryMap);
                if (ServiceUtil.isError(serviceResult)) {
                    return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
                }
            } catch (Exception e) {
                Debug.logError(e, "Error calling addAdditionalViewForCategory: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
            addAdditionalViewForCategoryMap = new HashMap<String, Object>();
        }
        if (UtilValidate.isNotEmpty(context.get("additionalImageFour"))) {
            addAdditionalViewForCategoryMap.put("productCategoryId", context.get("productCategoryId"));
            addAdditionalViewForCategoryMap.put("uploadedFile", context.get("additionalImageFour"));
            addAdditionalViewForCategoryMap.put("prodCatContentTypeId", "ADDITIONAL_IMAGE_4");
            addAdditionalViewForCategoryMap.put("_uploadedFile_fileName", context.get("_additionalImageFour_fileName"));
            addAdditionalViewForCategoryMap.put("_uploadedFile_contentType", context.get("_additionalImageFour_contentType"));
            addAdditionalViewForCategoryMap.put("imageProfile", context.get("additionalImageFour_imageProfile"));
            try {
                Map<String, Object> serviceResult = dispatcher.runSync("addAdditionalViewForCategory", addAdditionalViewForCategoryMap);
                if (ServiceUtil.isError(serviceResult)) {
                    return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
                }
            } catch (Exception e) {
                Debug.logError(e, "Error calling addAdditionalViewForCategory: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
            addAdditionalViewForCategoryMap = new HashMap<String, Object>();
        }
        result.put("productCategoryId", context.get("productCategoryId"));

        return result;
    }

}
