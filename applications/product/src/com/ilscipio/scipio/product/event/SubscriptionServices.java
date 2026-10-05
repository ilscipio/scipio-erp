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
import org.ofbiz.security.Security;
import org.ofbiz.service.DispatchContext;
import org.ofbiz.service.LocalDispatcher;
import org.ofbiz.service.ServiceUtil;

/**
 * Auto-generated event class from Simple Method XML.
 *
 * <p>Generated from: component://product/script/org/ofbiz/product/subscription/SubscriptionServices.xml</p>
 *
 * <p>SCIPIO: 4.0.0: Auto-generated.</p>
 */
public class SubscriptionServices {

    private static final String MODULE = SubscriptionServices.class.getName();


    /**
     * Create a Subscription
     */
    public static Map<String, Object> createSubscription(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        GenericValue newEntity = null;
        GenericValue resource = null;
        List<GenericValue> resourceList = null;
        newEntity = delegator.makeValue("Subscription");
        if (UtilValidate.isEmpty(context.get("subscriptionId"))) {
            ((GenericValue) newEntity).put("subscriptionId", delegator.getNextSeqId("Subscription"));
        } else {
            newEntity.put("subscriptionId", context.get("subscriptionId"));
        }
        result.put("subscriptionId", newEntity.get("subscriptionId"));
        if (UtilValidate.isNotEmpty(context.get("subscriptionResourceId"))) {
            if (UtilValidate.isNotEmpty(context.get("productId"))) {
                try {
                    resourceList = EntityQuery.use(delegator)
                            .from("ProductSubscriptionResource")
                            .where(UtilMisc.toMap("subscriptionResourceId", context.get("subscriptionResourceId"), "productId", context.get("productId")))
                            .filterByDate()
                            .queryList();
                } catch (Exception e) {
                    Debug.logError(e, "Error querying entity: " + e.getMessage(), MODULE);
                    return ServiceUtil.returnError(e.getMessage());
                }
                resource = EntityUtil.getFirst((List<GenericValue>) resourceList);
                if (UtilValidate.isNotEmpty(resource)) {
                    newEntity.setNonPKFields((Map<String, Object>) resource);
                }
            }
        }
        newEntity.setNonPKFields(context);
        try {
            delegator.create(newEntity);
        } catch (Exception e) {
            Debug.logError(e, "Error creating value: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }

        return result;
    }


    /**
     * check if a party has a subscription
     */
    public static Map<String, Object> isSubscribed(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        Boolean found = null;
        GenericValue subscription = null;
        Map<String, Object> pfInput = new HashMap<String, Object>();
        pfInput.put("inputFields", context);
        pfInput.put("entityName", "Subscription");
        pfInput.put("filterByDate", context.get("filterByDate"));
        Object pfResultList = null;
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("performFindList", pfInput);
            if (ServiceUtil.isError(serviceResult)) {
                return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
            }
            pfResultList = serviceResult.get("list");
        } catch (Exception e) {
            Debug.logError(e, "Error calling performFindList: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        if (UtilValidate.isEmpty(pfResultList)) {
            found = Boolean.FALSE;
        } else {
            found = Boolean.TRUE;
            subscription = EntityUtil.getFirst((List<GenericValue>) pfResultList);
            result.put("subscriptionId", subscription.get("subscriptionId"));
        }
        result.put("isSubscribed", found);

        return result;
    }


    /**
     * Get Subscription data
     */
    public static Map<String, Object> getSubscription(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        GenericValue subscription = null;
        try {
            subscription = EntityQuery.use(delegator)
                    .from("Subscription")
                    .where(context)
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying Subscription: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        result.put("subscriptionId", context.get("subscriptionId"));
        if (UtilValidate.isNotEmpty(subscription)) {
            result.put("subscription", subscription);
        }

        return result;
    }


    /**
     * Create (when not exist) or update (when exist) a Subscription attribute
     */
    public static Map<String, Object> updateSubscriptionAttribute(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        GenericValue newEntity = null;
        result.put("subscriptionId", context.get("subscriptionId"));
        GenericValue lookedUpValue = null;
        try {
            lookedUpValue = EntityQuery.use(delegator)
                    .from("SubscriptionAttribute")
                    .where(context)
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying SubscriptionAttribute: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        if (UtilValidate.isEmpty(lookedUpValue)) {
            newEntity = delegator.makeValue("SubscriptionAttribute");
            newEntity.setPKFields(context);
            newEntity.setNonPKFields(context);
            try {
                delegator.create(newEntity);
            } catch (Exception e) {
                Debug.logError(e, "Error creating value: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
        } else {
            lookedUpValue.setNonPKFields(context);
            try {
                delegator.store(lookedUpValue);
            } catch (Exception e) {
                Debug.logError(e, "Error storing value: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
        }

        return result;
    }


    /**
     * Subscription permission checking logic
     */
    public static Map<String, Object> subscriptionPermissionCheck(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        Security security = dctx.getSecurity();
        Boolean hasPermission = null;
        String primaryPermission = "CATALOG";
        String mainAction = (String) context.get("mainAction");
        if (mainAction == null) {
            return ServiceUtil.returnError(UtilProperties.getMessage("CommonUiLabels", "CommonPermissionMainActionAttributeMissing", locale));
        }
        if (security.hasPermission(primaryPermission + "_" + mainAction, userLogin) || security.hasPermission(primaryPermission + "_ADMIN", userLogin)) {
            hasPermission = Boolean.TRUE;
            result.put("hasPermission", Boolean.TRUE);
        } else {
            hasPermission = Boolean.FALSE;
            result.put("hasPermission", Boolean.FALSE);
            result.put("failMessage", UtilProperties.getMessage("CommonUiLabels", "CommonGenericPermissionError", locale));
        }
        if ((Boolean.FALSE.equals(hasPermission) && "VIEW".equals(context.get("mainAction")))) {
            if (security.hasPermission("CATALOG_READ", userLogin)) {
                hasPermission = Boolean.TRUE;
                result.put("hasPermission", hasPermission);
            }
        }

        return result;
    }

}
