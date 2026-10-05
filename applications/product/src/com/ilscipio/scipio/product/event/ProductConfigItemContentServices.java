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
import org.ofbiz.service.DispatchContext;
import org.ofbiz.service.LocalDispatcher;
import org.ofbiz.service.ServiceUtil;

/**
 * Auto-generated event class from Simple Method XML.
 *
 * <p>Generated from: component://product/script/org/ofbiz/product/config/ProductConfigItemContentServices.xml</p>
 *
 * <p>SCIPIO: 4.0.0: Auto-generated.</p>
 */
public class ProductConfigItemContentServices {

    private static final String MODULE = ProductConfigItemContentServices.class.getName();


    /**
     * Create Content For ProductConfigItem
     */
    public static Map<String, Object> createProductConfigItemContent(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        GenericValue newEntity = null;
        Timestamp nowTimestamp = null;
        newEntity = delegator.makeValue("ProdConfItemContent");
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
        result.put("configItemId", newEntity.get("configItemId"));
        result.put("confItemContentTypeId", newEntity.get("confItemContentTypeId"));

        return result;
    }


    /**
     * Update Content For ProductConfigItem
     */
    public static Map<String, Object> updateProductConfigItemContent(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        GenericValue lookupPKMap = delegator.makeValue("ProdConfItemContent");
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
     * Remove Content From ProductConfigItem
     */
    public static Map<String, Object> removeProductConfigItemContent(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        GenericValue lookupPKMap = delegator.makeValue("ProdConfItemContent");
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
     * Create Simple Text Content For Product
     */
    public static Map<String, Object> createSimpleTextContentForProductConfigItem(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        Map<String, Object> createProductConfigItemContent = new HashMap<String, Object>();
        // set-service-fields from "parameters" to "createProductConfigItemContent" for service "createProductConfigItemContent"
        createProductConfigItemContent.putAll(UtilMisc.toMap(context));
        Map<String, Object> createSimpleText = new HashMap<String, Object>();
        // set-service-fields from "parameters" to "createSimpleText" for service "createSimpleTextContent"
        createSimpleText.putAll(UtilMisc.toMap(context));
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("createSimpleTextContent", createSimpleText);
            if (ServiceUtil.isError(serviceResult)) {
                return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
            }
            createProductConfigItemContent.put("contentId", serviceResult.get("contentId"));
        } catch (Exception e) {
            Debug.logError(e, "Error calling createSimpleTextContent: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("createProductConfigItemContent", createProductConfigItemContent);
            if (ServiceUtil.isError(serviceResult)) {
                return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
            }
        } catch (Exception e) {
            Debug.logError(e, "Error calling createProductConfigItemContent: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }

        return result;
    }


    /**
     * Update Simple Text Content For Product
     */
    public static Map<String, Object> updateSimpleTextContentForProductConfigItem(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        Map<String, Object> updateProductConfigItemContent = new HashMap<String, Object>();
        // set-service-fields from "parameters" to "updateProductConfigItemContent" for service "updateProductConfigItemContent"
        updateProductConfigItemContent.putAll(UtilMisc.toMap(context));
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("updateProductConfigItemContent", updateProductConfigItemContent);
            if (ServiceUtil.isError(serviceResult)) {
                return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
            }
        } catch (Exception e) {
            Debug.logError(e, "Error calling updateProductConfigItemContent: " + e.getMessage(), MODULE);
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

}
