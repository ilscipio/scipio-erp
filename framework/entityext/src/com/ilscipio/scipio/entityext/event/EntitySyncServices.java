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
package com.ilscipio.scipio.entityext.event;

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
 * <p>Generated from: component://entityext/script/org/ofbiz/entityext/synchronization/EntitySyncServices.xml</p>
 *
 * <p>SCIPIO: 4.0.0: Auto-generated.</p>
 */
public class EntitySyncServices {

    private static final String MODULE = EntitySyncServices.class.getName();


    /**
     * Create a EntitySync
     */
    public static Map<String, Object> createEntitySync(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        GenericValue newEntity = delegator.makeValue("EntitySync");
        newEntity.setNonPKFields(context);
        String entitySyncId = delegator.getNextSeqId("EntitySync");
        newEntity.put("entitySyncId", entitySyncId);
        result.put("entitySyncId", entitySyncId);
        try {
            delegator.create(newEntity);
        } catch (Exception e) {
            Debug.logError(e, "Error creating value: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }

        return result;
    }


    /**
     * Update a EntitySync, set the Status to ESR_NOT_STARTED, but ONLY if running (ie in ESR_RUNNING)
     */
    public static Map<String, Object> resetEntitySyncStatusToNotStarted(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        GenericValue valueToStore = null;
        Map<String, Object> lookupPKMap = new HashMap<String, Object>();
        lookupPKMap.put("entitySyncId", context.get("entitySyncId"));
        try {
            valueToStore = EntityQuery.use(delegator)
                    .from("EntitySync")
                    .where(lookupPKMap)
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error finding by primary key EntitySync: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        if ("ESR_RUNNING".equals(valueToStore.get("runStatusId"))) {
            valueToStore.put("runStatusId", "ESR_NOT_STARTED");
            try {
                delegator.store(valueToStore);
            } catch (Exception e) {
                Debug.logError(e, "Error storing value: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
        }

        return result;
    }


    /**
     * Create EntitySyncHistory
     */
    public static Map<String, Object> createEntitySyncHistory(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        GenericValue newEntity = delegator.makeValue("EntitySyncHistory");
        newEntity.setPKFields(context);
        Timestamp newEntity_startDate = new Timestamp(System.currentTimeMillis());
        result.put("startDate", newEntity.get("startDate"));
        newEntity.setNonPKFields(context);
        try {
            delegator.create(newEntity);
        } catch (Exception e) {
            Debug.logError(e, "Error creating value: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        GenericValue entitySync = null;
        try {
            entitySync = EntityQuery.use(delegator)
                    .from("EntitySync")
                    .where(context)
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying EntitySync: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        entitySync.put("lastHistoryStartDate", newEntity.get("startDate"));
        try {
            delegator.store(entitySync);
        } catch (Exception e) {
            Debug.logError(e, "Error storing value: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }

        return result;
    }


    /**
     * Delete EntitySyncHistory
     */
    public static Map<String, Object> deleteEntitySyncHistory(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        GenericValue lookupPKMap = delegator.makeValue("EntitySyncHistory");
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
     * Create EntitySyncInclude
     */
    public static Map<String, Object> createEntitySyncInclude(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        GenericValue newEntity = delegator.makeValue("EntitySyncInclude");
        newEntity.setPKFields(context);
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
     * Delete EntitySyncInclude
     */
    public static Map<String, Object> deleteEntitySyncInclude(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        GenericValue lookupPKMap = delegator.makeValue("EntitySyncInclude");
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
     * Check user permission for entity sync.
     */
    public static Map<String, Object> entitySyncPermissionCheck(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        Object primaryPermission = "ENTITY_SYNC";
        // TODO: Call simple-method "genericBasePermissionCheck" from "component://common/script/org/ofbiz/common/permission/CommonPermissionServices.xml"

        return result;
    }

}
