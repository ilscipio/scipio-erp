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
package com.ilscipio.scipio.workeffort.event;

import java.math.BigDecimal;
import java.sql.Timestamp;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.UtilGenerics;
import org.ofbiz.base.util.UtilMisc;
import org.ofbiz.base.util.UtilProperties;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.GenericValue;
import org.ofbiz.entity.condition.EntityCondition;
import org.ofbiz.entity.util.EntityQuery;
import org.ofbiz.entity.util.EntityUtil;
import org.ofbiz.security.Security;
import org.ofbiz.service.DispatchContext;
import org.ofbiz.service.LocalDispatcher;
import org.ofbiz.service.ServiceUtil;
import org.ofbiz.workeffort.workeffort.WorkEffortKeywordIndex;
import org.ofbiz.workeffort.workeffort.WorkEffortPartyAssignmentServices;

/**
 * Auto-generated event class from Simple Method XML.
 *
 * <p>Generated from: component://workeffort/script/org/ofbiz/workeffort/workeffort/WorkEffortSimpleServices.xml</p>
 *
 * <p>SCIPIO: 4.0.0: Auto-generated.</p>
 */
public class WorkEffortSimpleServices {

    private static final String MODULE = WorkEffortSimpleServices.class.getName();


    /**
     * Create Work Effort and assign to a party with a role
     */
    public static Map<String, Object> createWorkEffortAndPartyAssign(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        List<String> error_list = new LinkedList<>();

        Map<String, Object> create = new HashMap<String, Object>();
        // set-service-fields from "parameters" to "create" for service "createWorkEffort"
        create.putAll(UtilMisc.toMap(context));
        Object workEffortId = null;
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("createWorkEffort", create);
            if (ServiceUtil.isError(serviceResult)) {
                return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
            }
            workEffortId = serviceResult.get("workEffortId");
        } catch (Exception e) {
            Debug.logError(e, "Error calling createWorkEffort: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        if (!error_list.isEmpty()) {
            return ServiceUtil.returnError(error_list);
        }
        GenericValue newEntity = delegator.makeValue("WorkEffortPartyAssignment");
        newEntity.setPKFields(context);
        newEntity.setNonPKFields(context);
        newEntity.put("workEffortId", workEffortId);
        if (UtilValidate.isEmpty(newEntity.get("fromDate"))) {
            newEntity.set("fromDate", new Timestamp(System.currentTimeMillis())); // SCIPIO: fixed lost assignment
        }
        newEntity.put("assignedByUserLoginId", userLogin.get("userLoginId"));
        try {
            delegator.create(newEntity);
        } catch (Exception e) {
            Debug.logError(e, "Error creating value: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        result.put("workEffortId", workEffortId);

        return result;
    }


    /**
     * Create Work Effort
     */
    public static Map<String, Object> createWorkEffort(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        List<String> error_list = new LinkedList<>();

        GenericValue newEntity = null;
        GenericValue workFullfillment = null;
        GenericValue lookedUpValue = null;
        Object goodStatusId = null;
        GenericValue custRequestWorkEffort = null;
        Map<String, Object> newWorkEffortContent = null;
        Map<String, Object> updCustReq = null;
        List<GenericValue> custRequestContents = null;
        Object entity = null;
        newEntity = delegator.makeValue("WorkEffort");
        if (UtilValidate.isEmpty(context.get("workEffortId"))) {
            ((GenericValue) newEntity).put("workEffortId", delegator.getNextSeqId("WorkEffort"));
        } else {
            if (context.get("workEffortId") == null || ((String) context.get("workEffortId")).trim().isEmpty()) {
                error_list.add("Invalid ID for field parameters.workEffortId");
            }
            if (!error_list.isEmpty()) {
                return ServiceUtil.returnError(error_list);
            }
            newEntity.put("workEffortId", context.get("workEffortId"));
        }
        result.put("workEffortId", newEntity.get("workEffortId"));
        newEntity.setNonPKFields(context);
        Timestamp nowTimestamp = new Timestamp(System.currentTimeMillis());
        newEntity.put("lastStatusUpdate", nowTimestamp);
        newEntity.put("lastModifiedDate", nowTimestamp);
        newEntity.put("createdDate", nowTimestamp);
        newEntity.put("revisionNumber", 1L);
        newEntity.put("lastModifiedByUserLogin", userLogin.get("userLoginId"));
        newEntity.put("createdByUserLogin", userLogin.get("userLoginId"));
        try {
            delegator.create(newEntity);
        } catch (Exception e) {
            Debug.logError(e, "Error creating value: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        GenericValue newWorkEffortStatus = delegator.makeValue("WorkEffortStatus");
        newWorkEffortStatus.put("workEffortId", newEntity.get("workEffortId"));
        newWorkEffortStatus.put("statusId", newEntity.get("currentStatusId"));
        newWorkEffortStatus.put("statusDatetime", nowTimestamp);
        newWorkEffortStatus.put("setByUserLogin", userLogin.get("userLoginId"));
        try {
            delegator.create(newWorkEffortStatus);
        } catch (Exception e) {
            Debug.logError(e, "Error creating value: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        if (UtilValidate.isNotEmpty(context.get("requirementId"))) {
            workFullfillment = delegator.makeValue("WorkRequirementFulfillment");
            workFullfillment.put("workEffortId", newEntity.get("workEffortId"));
            workFullfillment.put("requirementId", context.get("requirementId"));
            try {
                delegator.create(workFullfillment);
            } catch (Exception e) {
                Debug.logError(e, "Error creating value: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
        }
        if (UtilValidate.isNotEmpty(context.get("custRequestId"))) {
            try {
                lookedUpValue = EntityQuery.use(delegator)
                        .from("CustRequest")
                        .where(context)
                        .queryOne();
            } catch (Exception e) {
                Debug.logError(e, "Error querying CustRequest: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
            goodStatusId = "CRQ_ACCEPTED";
            if (!java.util.Objects.equals(lookedUpValue.get("statusId"), goodStatusId)) {
                entity = "Customer request";
                {
                    String errorMsg = UtilProperties.getMessage("CommonUiLabels", "CommonErrorStatusNotValid", locale);
                    error_list.add(errorMsg);
                }
                if (!error_list.isEmpty()) {
                    return ServiceUtil.returnError(error_list);
                }
            }
            custRequestWorkEffort = delegator.makeValue("CustRequestWorkEffort");
            custRequestWorkEffort.put("workEffortId", newEntity.get("workEffortId"));
            custRequestWorkEffort.put("custRequestId", context.get("custRequestId"));
            try {
                delegator.create(custRequestWorkEffort);
            } catch (Exception e) {
                Debug.logError(e, "Error creating value: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
            updCustReq.put("custRequestId", context.get("custRequestId"));
            updCustReq.put("statusId", "CRQ_REVIEWED");
            updCustReq.put("webSiteId", context.get("webSiteId"));
            try {
                Map<String, Object> serviceResult = dispatcher.runSync("setCustRequestStatus", updCustReq);
                if (ServiceUtil.isError(serviceResult)) {
                    return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
                }
            } catch (Exception e) {
                Debug.logError(e, "Error calling setCustRequestStatus: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
            try {
                custRequestContents = EntityQuery.use(delegator)
                        .from("CustRequestContent")
                        .where(UtilMisc.toMap("custRequestId", context.get("custRequestId")))
                        .queryList();
            } catch (Exception e) {
                Debug.logError(e, "Error querying entity: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
            if (custRequestContents != null) {
                for (GenericValue custRequestContent : custRequestContents) {
                    newWorkEffortContent.put("workEffortId", newEntity.get("workEffortId"));
                    newWorkEffortContent.put("contentId", custRequestContent.get("contentId"));
                    newWorkEffortContent.put("workEffortContentTypeId", "SUPPORTING_MEDIA");
                    try {
                        Map<String, Object> serviceResult = dispatcher.runSync("createWorkEffortContent", newWorkEffortContent);
                        if (ServiceUtil.isError(serviceResult)) {
                            return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
                        }
                    } catch (Exception e) {
                        Debug.logError(e, "Error calling createWorkEffortContent: " + e.getMessage(), MODULE);
                        return ServiceUtil.returnError(e.getMessage());
                    }
                }
            }
        }

        return result;
    }


    /**
     * Update Work Effort
     */
    public static Map<String, Object> updateWorkEffort(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        List<String> error_list = new LinkedList<>();

        GenericValue lookedUpValue = null;
        GenericValue newWorkEffortStatus = null;
        List<GenericValue> validChange = null;
        try {
            lookedUpValue = EntityQuery.use(delegator)
                    .from("WorkEffort")
                    .where(context)
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying WorkEffort: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        GenericValue savedValue = GenericValue.create((GenericValue) lookedUpValue);
        Timestamp nowTimestamp = new Timestamp(System.currentTimeMillis());
        Object lookedUpValue_lastStatusUpdate = null;
        Object newWorkEffortStatus_workEffortId = null;
        Object newWorkEffortStatus_statusId = null;
        Object newWorkEffortStatus_reason = null;
        Object newWorkEffortStatus_statusDatetime = null;
        Object newWorkEffortStatus_setByUserLogin = null;
        if ((!(UtilValidate.isEmpty(context.get("currentStatusId"))) && !java.util.Objects.equals(context.get("currentStatusId"), lookedUpValue.get("currentStatusId")))) {
            if (UtilValidate.isNotEmpty(lookedUpValue.get("currentStatusId"))) {
                try {
                    validChange = EntityQuery.use(delegator)
                            .from("StatusValidChange")
                            .where(UtilMisc.toMap("statusId", lookedUpValue.get("currentStatusId"), "statusIdTo", context.get("currentStatusId")))
                            .queryList();
                } catch (Exception e) {
                    Debug.logError(e, "Error querying entity: " + e.getMessage(), MODULE);
                    return ServiceUtil.returnError(e.getMessage());
                }
                if (UtilValidate.isEmpty(validChange)) {
                    {
                        String errorMsg = UtilProperties.getMessage("WorkEffortUiLabels", "WorkEffortStatusChangeNotValid", locale);
                        error_list.add(errorMsg);
                    }
                    Debug.logError("The status change from " + lookedUpValue.get("currentStatusId") + " to " + context.get("currentStatusId") + " is not a valid change", MODULE);
                    if (!error_list.isEmpty()) {
                        return ServiceUtil.returnError(error_list);
                    }
                }
            }
            lookedUpValue.put("lastStatusUpdate", nowTimestamp);
            newWorkEffortStatus = delegator.makeValue("WorkEffortStatus");
            newWorkEffortStatus.put("workEffortId", lookedUpValue.get("workEffortId"));
            newWorkEffortStatus.put("statusId", context.get("currentStatusId"));
            newWorkEffortStatus.put("reason", context.get("reason"));
            newWorkEffortStatus.put("statusDatetime", nowTimestamp);
            newWorkEffortStatus.put("setByUserLogin", userLogin.get("userLoginId"));
            try {
                delegator.create(newWorkEffortStatus);
            } catch (Exception e) {
                Debug.logError(e, "Error creating value: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
        }
        lookedUpValue.setNonPKFields(context);
        if (!java.util.Objects.equals(lookedUpValue, savedValue)) {
            lookedUpValue.put("lastModifiedDate", nowTimestamp);
            lookedUpValue.put("lastModifiedByUserLogin", userLogin.get("userLoginId"));
            if (UtilValidate.isNotEmpty(lookedUpValue.get("revisionNumber"))) {
                lookedUpValue.put("revisionNumber", ((Number) lookedUpValue.get("revisionNumber")).longValue() + 1L);
            } else {
                lookedUpValue.put("revisionNumber", 1L);
            }
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
     * Delete Work Effort
     */
    public static Map<String, Object> deleteWorkEffort(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        Security security = dctx.getSecurity();
        List<String> error_list = new LinkedList<>();

        List<GenericValue> wepaList = null;
        try {
            wepaList = EntityQuery.use(delegator)
                    .from("WorkEffortPartyAssignment")
                    .where(UtilMisc.toMap("workEffortId", context.get("workEffortId"), "partyId", userLogin.get("partyId")))
                    .queryList();
        } catch (Exception e) {
            Debug.logError(e, "Error querying entity: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        if (UtilValidate.isEmpty(wepaList)) {
            if (!security.hasEntityPermission("WORKEFFORTMGR", "_DELETE", userLogin)) {
                return ServiceUtil.returnError(UtilProperties.getMessage("WorkEffortUiLabels", "WorkEffortDeletePermissionError", locale));
            }
            if (!error_list.isEmpty()) {
                return ServiceUtil.returnError(error_list);
            }
        }
        GenericValue lookedUpValue = null;
        try {
            lookedUpValue = EntityQuery.use(delegator)
                    .from("WorkEffort")
                    .where(context)
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying WorkEffort: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        try {
            lookedUpValue.removeRelated("WorkEffortKeyword");
        } catch (Exception e) {
            Debug.logError(e, "Error removing related WorkEffortKeyword: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        try {
            lookedUpValue.removeRelated("WorkEffortAttribute");
        } catch (Exception e) {
            Debug.logError(e, "Error removing related WorkEffortAttribute: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        try {
            lookedUpValue.removeRelated("WorkOrderItemFulfillment");
        } catch (Exception e) {
            Debug.logError(e, "Error removing related WorkOrderItemFulfillment: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        try {
            lookedUpValue.removeRelated("FromWorkEffortAssoc");
        } catch (Exception e) {
            Debug.logError(e, "Error removing related FromWorkEffortAssoc: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        try {
            lookedUpValue.removeRelated("ToWorkEffortAssoc");
        } catch (Exception e) {
            Debug.logError(e, "Error removing related ToWorkEffortAssoc: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        try {
            lookedUpValue.removeRelated("NoteData");
        } catch (Exception e) {
            Debug.logError(e, "Error removing related NoteData: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        try {
            lookedUpValue.removeRelated("RecurrenceInfo");
        } catch (Exception e) {
            Debug.logError(e, "Error removing related RecurrenceInfo: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        try {
            lookedUpValue.removeRelated("RuntimeData");
        } catch (Exception e) {
            Debug.logError(e, "Error removing related RuntimeData: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        try {
            lookedUpValue.removeRelated("WorkEffortPartyAssignment");
        } catch (Exception e) {
            Debug.logError(e, "Error removing related WorkEffortPartyAssignment: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        try {
            lookedUpValue.removeRelated("WorkEffortFixedAssetAssign");
        } catch (Exception e) {
            Debug.logError(e, "Error removing related WorkEffortFixedAssetAssign: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        try {
            lookedUpValue.removeRelated("WorkEffortSkillStandard");
        } catch (Exception e) {
            Debug.logError(e, "Error removing related WorkEffortSkillStandard: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        try {
            lookedUpValue.removeRelated("WorkEffortStatus");
        } catch (Exception e) {
            Debug.logError(e, "Error removing related WorkEffortStatus: " + e.getMessage(), MODULE);
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
     * Copy a WorkEffort
     */
    public static Map<String, Object> copyWorkEffort(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        List<String> error_list = new LinkedList<>();

        Object errorString = null;
        String targetWorkEffortId = null;
        Map<String, Object> copyWorkEffortAssocsCtx = new HashMap<>();
        Object keyMap = null;
        GenericValue newRelatedValue = null;
        Object relatedIdFieldName = null;
        Object excludeExpiredRelations = null;
        Object modelRelationList = null;
        GenericValue newRelatedPks = null;
        GenericValue duplicateCheck = null;
        Object relationName = null;
        List<GenericValue> emptyField = null;
        Object fromDateModelField = null;
        String relatedEntityName = null;
        List<GenericValue> relationValues = null;
        GenericValue sourceWorkEffort = null;
        try {
            sourceWorkEffort = EntityQuery.use(delegator)
                    .from("WorkEffort")
                    .where(UtilMisc.toMap("workEffortId", context.get("sourceWorkEffortId")))
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying WorkEffort: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        if (UtilValidate.isEmpty(sourceWorkEffort)) {
            errorString = "sourceWorkEffortId = " + context.get("sourceWorkEffortId");
            {
                String errorMsg = UtilProperties.getMessage("WorkEffortUiLabels", "WorkEffortNotFound", locale);
                error_list.add(errorMsg);
            }
            if (!error_list.isEmpty()) {
                return ServiceUtil.returnError(error_list);
            }
        }
        targetWorkEffortId = (String) context.get("targetWorkEffortId");
        if (UtilValidate.isEmpty(targetWorkEffortId)) {
            targetWorkEffortId = delegator.getNextSeqId("WorkEffort");
        }
        Map<String, Object> createWorkEffortCtx = new HashMap<String, Object>();
        // set-service-fields from "sourceWorkEffort" to "createWorkEffortCtx" for service "createWorkEffort"
        createWorkEffortCtx.putAll(UtilMisc.toMap(sourceWorkEffort));
        createWorkEffortCtx.put("workEffortId", targetWorkEffortId);
        createWorkEffortCtx.put("userLogin", context.get("userLogin"));
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("createWorkEffort", createWorkEffortCtx);
            if (ServiceUtil.isError(serviceResult)) {
                return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
            }
        } catch (Exception e) {
            Debug.logError(e, "Error calling createWorkEffort: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        if (!error_list.isEmpty()) {
            return ServiceUtil.returnError(error_list);
        }
        GenericValue targetWorkEffort = null;
        try {
            targetWorkEffort = EntityQuery.use(delegator)
                    .from("WorkEffort")
                    .where(UtilMisc.toMap("workEffortId", targetWorkEffortId))
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying WorkEffort: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        Object copyWorkEffortAssocs = context.get("copyWorkEffortAssocs");
        if ("Y".equals(copyWorkEffortAssocs)) {
            // set-service-fields from "parameters" to "copyWorkEffortAssocsCtx" for service "copyWorkEffortAssocs"
            copyWorkEffortAssocsCtx.putAll(UtilMisc.toMap(context));
            copyWorkEffortAssocsCtx.put("targetWorkEffortId", targetWorkEffortId);
            try {
                Map<String, Object> serviceResult = dispatcher.runSync("copyWorkEffortAssocs", copyWorkEffortAssocsCtx);
                if (ServiceUtil.isError(serviceResult)) {
                    return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
                }
            } catch (Exception e) {
                Debug.logError(e, "Error calling copyWorkEffortAssocs: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
            if (!error_list.isEmpty()) {
                return ServiceUtil.returnError(error_list);
            }
        }
        Object copyRelatedValues = context.get("copyRelatedValues");
        if ("Y".equals(copyRelatedValues)) {
            excludeExpiredRelations = context.get("excludeExpiredRelations");
            modelRelationList = ((Map<String, Object>) ((Map<String, Object>) context.get("groovy:delegator")).get("getModelEntity('WorkEffort')")).get("getRelationsManyList();");
            if (modelRelationList != null) {
                for (Object modelRelation : (List<?>) modelRelationList) {
                    relatedEntityName = (String) ((Map<String, Object>) context.get("groovy:modelRelation")).get("getRelEntityName();");
                    if (!"WorkEffortAssoc".equals(relatedEntityName)) {
                        relationName = ((Map<String, Object>) context.get("groovy:modelRelation")).get("getCombinedName();");
                        keyMap = ((Map<String, Object>) context.get("groovy:modelRelation")).get("findKeyMap('workEffortId');");
                        if (UtilValidate.isNotEmpty(keyMap)) {
                            relatedIdFieldName = ((Map<String, Object>) context.get("groovy:keyMap")).get("getRelFieldName();");
                            try {
                                relationValues = sourceWorkEffort.getRelated("${relationName}", null, null, false);
                            } catch (Exception e) {
                                Debug.logError(e, "Error getting related ${relationName}: " + e.getMessage(), MODULE);
                                return ServiceUtil.returnError(e.getMessage());
                            }
                            if ("Y".equals(excludeExpiredRelations)) {
                                fromDateModelField = ((Map<String, Object>) ((Map<String, Object>) context.get("groovy:delegator")).get("getModelEntity(relatedEntityName)")).get("getField('fromDate');");
                                if (UtilValidate.isNotEmpty(fromDateModelField)) {
                                    emptyField = EntityUtil.filterByDate(UtilGenerics.cast(relationValues));
                                }
                            }
                            if (relationValues != null) {
                                for (GenericValue relatedValue : relationValues) {
                                    newRelatedValue = GenericValue.create((GenericValue) relatedValue);
                                    newRelatedValue.put((String) relatedIdFieldName, targetWorkEffortId);
                                    newRelatedPks = delegator.makeValue(relatedEntityName);
                                    newRelatedPks.setPKFields((Map<String, Object>) newRelatedValue);
                                    try {
                                        duplicateCheck = EntityQuery.use(delegator)
                                                .from(relatedEntityName)
                                                .where(newRelatedPks)
                                                .queryOne();
                                    } catch (Exception e) {
                                        Debug.logError(e, "Error finding by primary key ${relatedEntityName}: " + e.getMessage(), MODULE);
                                        return ServiceUtil.returnError(e.getMessage());
                                    }
                                    if (UtilValidate.isEmpty(duplicateCheck)) {
                                        try {
                                            delegator.create(newRelatedValue);
                                        } catch (Exception e) {
                                            Debug.logError(e, "Error creating value: " + e.getMessage(), MODULE);
                                            return ServiceUtil.returnError(e.getMessage());
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
        result.put("workEffortId", targetWorkEffortId);

        return result;
    }


    /**
     * Make a Communication Event WorkEffort
     */
    public static Map<String, Object> makeCommunicationEventWorkEffort(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        GenericValue eventWe = null;
        GenericValue lookupMap = delegator.makeValue("CommunicationEventWorkEff");
        lookupMap.setPKFields(context);
        try {
            eventWe = EntityQuery.use(delegator)
                    .from("CommunicationEventWorkEff")
                    .where(lookupMap)
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error finding by primary key CommunicationEventWorkEff: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        if (UtilValidate.isNotEmpty(eventWe.get("workEffortId"))) {
            eventWe.setNonPKFields(context);
            eventWe.put("description", context.get("relationDescription"));
            try {
                delegator.store(eventWe);
            } catch (Exception e) {
                Debug.logError(e, "Error storing value: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
        }
        if (UtilValidate.isEmpty(eventWe.get("workEffortId"))) {
            lookupMap.setNonPKFields(context);
            eventWe.put("description", context.get("relationDescription"));
            try {
                delegator.create(lookupMap);
            } catch (Exception e) {
                Debug.logError(e, "Error creating value: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
        }
        result.put("workEffortId", lookupMap.get("workEffortId"));
        result.put("communicationEventId", lookupMap.get("communicationEventId"));

        return result;
    }


    /**
     * Update a CommunicationEventWorkEff
     */
    public static Map<String, Object> updateCommunicationEventWorkEff(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        List<String> error_list = new LinkedList<>();

        GenericValue communicationEventWorkEff = delegator.makeValue("CommunicationEventWorkEff");
        communicationEventWorkEff.setPKFields(context);
        try {
            communicationEventWorkEff = EntityQuery.use(delegator)
                    .from("CommunicationEventWorkEff")
                    .where(communicationEventWorkEff)
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error finding by primary key CommunicationEventWorkEff: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        if (UtilValidate.isEmpty(communicationEventWorkEff)) {
            {
                String errorMsg = UtilProperties.getMessage("WorkEffortUiLabels", "WorkEffortCannotUpdateContactInfo", locale);
                error_list.add(errorMsg);
            }
        }
        if (!error_list.isEmpty()) {
            return ServiceUtil.returnError(error_list);
        }
        communicationEventWorkEff.setNonPKFields(context);
        try {
            delegator.store(communicationEventWorkEff);
        } catch (Exception e) {
            Debug.logError(e, "Error storing value: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }

        return result;
    }


    /**
     * Delete a CommunicationEventWorkEff
     */
    public static Map<String, Object> deleteCommunicationEventWorkEff(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        List<String> error_list = new LinkedList<>();

        GenericValue communicationEventWorkEff = delegator.makeValue("CommunicationEventWorkEff");
        communicationEventWorkEff.setPKFields(context);
        try {
            communicationEventWorkEff = EntityQuery.use(delegator)
                    .from("CommunicationEventWorkEff")
                    .where(communicationEventWorkEff)
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error finding by primary key CommunicationEventWorkEff: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        if (UtilValidate.isEmpty(communicationEventWorkEff)) {
            {
                String errorMsg = UtilProperties.getMessage("WorkEffortUiLabels", "WorkEffortCannotDeleteContactInfo", locale);
                error_list.add(errorMsg);
            }
        }
        if (!error_list.isEmpty()) {
            return ServiceUtil.returnError(error_list);
        }
        try {
            delegator.removeValue(communicationEventWorkEff);
        } catch (Exception e) {
            Debug.logError(e, "Error removing value: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }

        return result;
    }


    /**
     * Assign Party to Work Effort
     */
    public static Map<String, Object> assignPartyToWorkEffort(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        List<String> error_list = new LinkedList<>();

        GenericValue firstAssignment = null;
        Object emptyField = null;
        List<GenericValue> currentAssignments = null;
        try {
            currentAssignments = EntityQuery.use(delegator)
                    .from("WorkEffortPartyAssignment")
                    .where(UtilMisc.toMap("workEffortId", context.get("workEffortId"), "partyId", context.get("partyId"), "roleTypeId", context.get("roleTypeId")))
                    .filterByDate()
                    .queryList();
        } catch (Exception e) {
            Debug.logError(e, "Error querying entity: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        if (UtilValidate.isNotEmpty(currentAssignments)) {
            firstAssignment = EntityUtil.getFirst((List<GenericValue>) currentAssignments);
            {
                String errorMsg = UtilProperties.getMessage("WorkEffortUiLabels", "WorkEffortPartyAssignmentError", locale);
                error_list.add(errorMsg);
            }
        }
        if (!error_list.isEmpty()) {
            return ServiceUtil.returnError(error_list);
        }
        Map<String, Object> ensurePartyRoleCtx = new HashMap<String, Object>();
        ensurePartyRoleCtx.put("partyId", context.get("partyId"));
        ensurePartyRoleCtx.put("roleTypeId", context.get("roleTypeId"));
        ensurePartyRoleCtx.put("userLogin", context.get("userLogin")); // SCIPIO: keep the caller identity for the nested service
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("ensurePartyRole", ensurePartyRoleCtx);
            if (ServiceUtil.isError(serviceResult)) {
                return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
            }
        } catch (Exception e) {
            Debug.logError(e, "Error calling ensurePartyRole: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        GenericValue newEntity = delegator.makeValue("WorkEffortPartyAssignment");
        newEntity.setPKFields(context);
        newEntity.setNonPKFields(context);
        if (UtilValidate.isEmpty(newEntity.get("fromDate"))) {
            newEntity.set("fromDate", new Timestamp(System.currentTimeMillis())); // SCIPIO: fixed lost assignment
        }
        result.put("fromDate", newEntity.get("fromDate"));
        newEntity.put("assignedByUserLoginId", userLogin.get("userLoginId"));
        if (UtilValidate.isNotEmpty(context.get("statusId"))) {
            Timestamp newEntity_statusDateTime = new Timestamp(System.currentTimeMillis());
            try {
                WorkEffortPartyAssignmentServices.updateWorkflowEngine((GenericValue) newEntity, (GenericValue) userLogin, dispatcher);
            } catch (Exception e) {
                Debug.logError(e, "Error calling WorkEffortPartyAssignmentServices.updateWorkflowEngine: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
        }
        try {
            delegator.create(newEntity);
        } catch (Exception e) {
            Debug.logError(e, "Error creating value: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }

        return result;
    }


    /**
     * Update WorkEffortPartyAssignment entity
     */
    public static Map<String, Object> updatePartyToWorkEffortAssignment(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        Object emptyField = null;
        GenericValue workEffortPartyAssignment = null;
        try {
            workEffortPartyAssignment = EntityQuery.use(delegator)
                    .from("WorkEffortPartyAssignment")
                    .where(context)
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying WorkEffortPartyAssignment: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        Object oldStatusId = workEffortPartyAssignment.get("statusId");
        workEffortPartyAssignment.setNonPKFields(context);
        if (!java.util.Objects.equals(context.get("statusId"), oldStatusId)) {
            Timestamp workEffortPartyAssignment_statusDateTime = new Timestamp(System.currentTimeMillis());
            try {
                WorkEffortPartyAssignmentServices.updateWorkflowEngine((GenericValue) workEffortPartyAssignment, (GenericValue) userLogin, dispatcher);
            } catch (Exception e) {
                Debug.logError(e, "Error calling WorkEffortPartyAssignmentServices.updateWorkflowEngine: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
        }
        try {
            delegator.store(workEffortPartyAssignment);
        } catch (Exception e) {
            Debug.logError(e, "Error storing value: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }

        return result;
    }


    /**
     * Update WorkEffortPartyAssignment entity
     */
    public static Map<String, Object> deletePartyToWorkEffortAssignment(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        Map<String, Object> del = new HashMap<String, Object>();
        // set-service-fields from "parameters" to "del" for service "updatePartyToWorkEffortAssignment"
        del.putAll(UtilMisc.toMap(context));
        Timestamp del_thruDate = new Timestamp(System.currentTimeMillis());
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("updatePartyToWorkEffortAssignment", del);
            if (ServiceUtil.isError(serviceResult)) {
                return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
            }
        } catch (Exception e) {
            Debug.logError(e, "Error calling updatePartyToWorkEffortAssignment: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }

        return result;
    }


    /**
     * Service that deletes a WorkEffortPartyAssignment entity
     */
    public static Map<String, Object> unassignPartyFromWorkEffort(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        GenericValue workEffortPartyAssignment = null;
        try {
            workEffortPartyAssignment = EntityQuery.use(delegator)
                    .from("WorkEffortPartyAssignment")
                    .where(context)
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying WorkEffortPartyAssignment: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        try {
            delegator.removeValue(workEffortPartyAssignment);
        } catch (Exception e) {
            Debug.logError(e, "Error removing value: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }

        return result;
    }


    /**
     * Create a WorkEffortContactMech
     */
    public static Map<String, Object> createWorkEffortContactMech(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        List<String> error_list = new LinkedList<>();

        GenericValue newValue = null;
        newValue = delegator.makeValue("WorkEffortContactMech");
        if (UtilValidate.isEmpty(context.get("contactMechId"))) {
            if (UtilValidate.isEmpty(context.get("contactMechTypeId"))) {
                {
                    String errorMsg = UtilProperties.getMessage("WorkEffortUiLabels", "WorkEffortRequiredFieldMissingContactMechIdOrContactMechTypeId", locale);
                    error_list.add(errorMsg);
                }
            }
            if (!error_list.isEmpty()) {
                return ServiceUtil.returnError(error_list);
            }
            if (UtilValidate.isNotEmpty(context.get("partyId"))) {
                // set-service-fields from "parameters" to "context" for service "createPartyContactMech"
                context.putAll(UtilMisc.toMap(context));
                try {
                    Map<String, Object> serviceResult = dispatcher.runSync("createPartyContactMech", context);
                    if (ServiceUtil.isError(serviceResult)) {
                        return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
                    }
                    newValue.put("contactMechId", serviceResult.get("contactMechId"));
                } catch (Exception e) {
                    Debug.logError(e, "Error calling createPartyContactMech: " + e.getMessage(), MODULE);
                    return ServiceUtil.returnError(e.getMessage());
                }
                Debug.logInfo("Party ContactMech created", MODULE);
            } else {
                // set-service-fields from "parameters" to "context" for service "createContactMech"
                context.putAll(UtilMisc.toMap(context));
                try {
                    Map<String, Object> serviceResult = dispatcher.runSync("createContactMech", context);
                    if (ServiceUtil.isError(serviceResult)) {
                        return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
                    }
                    newValue.put("contactMechId", serviceResult.get("contactMechId"));
                } catch (Exception e) {
                    Debug.logError(e, "Error calling createContactMech: " + e.getMessage(), MODULE);
                    return ServiceUtil.returnError(e.getMessage());
                }
                Debug.logInfo("ContactMech created", MODULE);
            }
        } else {
            newValue.put("contactMechId", context.get("contactMechId"));
        }
        Debug.logInfo("Creating a WorkEffortContactMech", MODULE);
        newValue.put("workEffortId", context.get("workEffortId"));
        newValue.setNonPKFields(context);
        result.put("contactMechId", newValue.get("contactMechId"));
        result.put("contactMechId", newValue.get("contactMechId"));
        Timestamp newValue_fromDate = new Timestamp(System.currentTimeMillis());
        try {
            delegator.create(newValue);
        } catch (Exception e) {
            Debug.logError(e, "Error creating value: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }

        return result;
    }


    /**
     * Update a WorkEffortContactMech
     */
    public static Map<String, Object> updateWorkEffortContactMech(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        List<String> error_list = new LinkedList<>();

        GenericValue newWorkEffortContactMech = null;
        newWorkEffortContactMech = delegator.makeValue("WorkEffortContactMech");
        GenericValue workEffortContactMech = null;
        try {
            workEffortContactMech = EntityQuery.use(delegator)
                    .from("WorkEffortContactMech")
                    .where(context)
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying WorkEffortContactMech: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        if (UtilValidate.isEmpty(workEffortContactMech)) {
            {
                String errorMsg = UtilProperties.getMessage("WorkEffortUiLabels", "WorkEffortCannotUpdateContactInfo", locale);
                error_list.add(errorMsg);
            }
        }
        if (!error_list.isEmpty()) {
            return ServiceUtil.returnError(error_list);
        }
        newWorkEffortContactMech = GenericValue.create((GenericValue) workEffortContactMech);
        if (UtilValidate.isEmpty(context.get("newContactMechId"))) {
            Debug.logInfo("Calling map procs", MODULE);
            // TODO: Convert call-map-processor (in-map: parameters, out-map: context)
            // xml-resource: component://party/script/org/ofbiz/party/contact/ContactMechMapProcs.xml, processor-name: updateContactMech
            context = new HashMap<String, Object>();
            {
                Object _val = context.get("contactMechId");
                context.put("contactMechId", _val != null ? _val.toString() : null);
            }
            Debug.logInfo("Calling generic updateContactMech method", MODULE);
            try {
                Map<String, Object> serviceResult = dispatcher.runSync("updateContactMech", context);
                if (ServiceUtil.isError(serviceResult)) {
                    return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
                }
                newWorkEffortContactMech.put("contactMechId", serviceResult.get("contactMechId"));
            } catch (Exception e) {
                Debug.logError(e, "Error calling updateContactMech: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
        } else {
            newWorkEffortContactMech.put("contactMechId", context.get("newContactMechId"));
        }
        if (!java.util.Objects.equals(context.get("contactMechId"), newWorkEffortContactMech.get("contactMechId"))) {
            newWorkEffortContactMech.setNonPKFields(context);
            Timestamp newWorkEffortContactMech_fromDate = new Timestamp(System.currentTimeMillis());
            Timestamp workEffortContactMech_thruDate = new Timestamp(System.currentTimeMillis());
            try {
                delegator.create(newWorkEffortContactMech);
            } catch (Exception e) {
                Debug.logError(e, "Error creating value: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
            try {
                delegator.store(workEffortContactMech);
            } catch (Exception e) {
                Debug.logError(e, "Error storing value: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
        }
        result.put("contactMechId", newWorkEffortContactMech.get("contactMechId"));
        result.put("contactMechId", newWorkEffortContactMech.get("contactMechId"));

        return result;
    }


    /**
     * Delete a WorkEffortContactMech
     */
    public static Map<String, Object> deleteWorkEffortContactMech(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        List<String> error_list = new LinkedList<>();

        GenericValue workEffortContactMech = null;
        try {
            workEffortContactMech = EntityQuery.use(delegator)
                    .from("WorkEffortContactMech")
                    .where(context)
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying WorkEffortContactMech: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        if (UtilValidate.isEmpty(workEffortContactMech)) {
            {
                String errorMsg = UtilProperties.getMessage("WorkEffortUiLabels", "WorkEffortCannotDeleteContactInfo", locale);
                error_list.add(errorMsg);
            }
        }
        if (!error_list.isEmpty()) {
            return ServiceUtil.returnError(error_list);
        }
        try {
            delegator.removeValue(workEffortContactMech);
        } catch (Exception e) {
            Debug.logError(e, "Error removing value: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }

        return result;
    }


    /**
     * Create a PostalAddress for WorkEffort
     */
    public static Map<String, Object> createWorkEffortPostalAddress(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        GenericValue newValue = null;
        newValue = delegator.makeValue("WorkEffortContactMech");
        Debug.logInfo("Creating postal address", MODULE);
        if (UtilValidate.isNotEmpty(context.get("addToParty"))) {
            // TODO: Convert call-map-processor (in-map: parameters, out-map: context)
            // xml-resource: component://party/script/org/ofbiz/party/contact/PartyContactMechMapProcs.xml, processor-name: postalAddress
            context = new HashMap<String, Object>();
            try {
                Map<String, Object> serviceResult = dispatcher.runSync("createPartyPostalAddress", context);
                if (ServiceUtil.isError(serviceResult)) {
                    return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
                }
                newValue.put("contactMechId", serviceResult.get("contactMechId"));
            } catch (Exception e) {
                Debug.logError(e, "Error calling createPartyPostalAddress: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
            Debug.logInfo("Party ContactMech created", MODULE);
        } else {
            // TODO: Convert call-map-processor (in-map: parameters, out-map: context)
            // xml-resource: component://party/script/org/ofbiz/party/contact/ContactMechMapProcs.xml, processor-name: postalAddress
            context = new HashMap<String, Object>();
            try {
                Map<String, Object> serviceResult = dispatcher.runSync("createPostalAddress", context);
                if (ServiceUtil.isError(serviceResult)) {
                    return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
                }
                newValue.put("contactMechId", serviceResult.get("contactMechId"));
            } catch (Exception e) {
                Debug.logError(e, "Error calling createPostalAddress: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
        }
        Debug.logInfo("ContactMech for postal address was " + newValue.get("contactMechId") + ", now creating work effort contact mech", MODULE);
        // TODO: Convert call-map-processor (in-map: parameters, out-map: context2)
        // xml-resource: component://workeffort/script/org/ofbiz/workeffort/workeffort/WorkflowMapProcessors.xml, processor-name: workEffortContactMech
        Map<String, Object> context2 = new HashMap<String, Object>();
        context2.put("contactMechId", newValue.get("contactMechId"));
        Debug.logInfo("Copied id to context2: " + ((Map<String, Object>) context2).get("contactMechId"), MODULE);
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("createWorkEffortContactMech", context2);
            if (ServiceUtil.isError(serviceResult)) {
                return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
            }
        } catch (Exception e) {
            Debug.logError(e, "Error calling createWorkEffortContactMech: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        result.put("contactMechId", newValue.get("contactMechId"));
        result.put("contactMechId", newValue.get("contactMechId"));

        return result;
    }


    /**
     * Update a PostalAddress for WorkEffort
     */
    public static Map<String, Object> updateWorkEffortPostalAddress(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        GenericValue newValue = delegator.makeValue("WorkEffortContactMech");
        // TODO: Convert call-map-processor (in-map: parameters, out-map: context)
        // xml-resource: component://party/script/org/ofbiz/party/contact/ContactMechMapProcs.xml, processor-name: postalAddress
        context = new HashMap<String, Object>();
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("updatePostalAddress", context);
            if (ServiceUtil.isError(serviceResult)) {
                return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
            }
            newValue.put("contactMechId", serviceResult.get("contactMechId"));
        } catch (Exception e) {
            Debug.logError(e, "Error calling updatePostalAddress: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        // TODO: Convert call-map-processor (in-map: parameters, out-map: context2)
        // xml-resource: component://workeffort/script/org/ofbiz/workeffort/workeffort/WorkflowMapProcessors.xml, processor-name: workEffortContactMech
        Map<String, Object> context2 = new HashMap<String, Object>();
        context2.put("newContactMechId", newValue.get("contactMechId"));
        context2.put("contactMechTypeId", "POSTAL_ADDRESS");
        Debug.logInfo("Copied id to context2: " + ((Map<String, Object>) context2).get("newContactMechId"), MODULE);
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("updateWorkEffortContactMech", context2);
            if (ServiceUtil.isError(serviceResult)) {
                return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
            }
        } catch (Exception e) {
            Debug.logError(e, "Error calling updateWorkEffortContactMech: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        result.put("contactMechId", newValue.get("contactMechId"));
        result.put("contactMechId", newValue.get("contactMechId"));

        return result;
    }


    /**
     * Create a TelecomNumber for WorkEffort
     */
    public static Map<String, Object> createWorkEffortTelecomNumber(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        GenericValue newValue = null;
        newValue = delegator.makeValue("WorkEffortContactMech");
        Debug.logInfo("Creating telecom number", MODULE);
        if (UtilValidate.isNotEmpty(context.get("addToParty"))) {
            // TODO: Convert call-map-processor (in-map: parameters, out-map: context)
            // xml-resource: component://party/script/org/ofbiz/party/contact/PartyContactMechMapProcs.xml, processor-name: telecomNumber
            context = new HashMap<String, Object>();
            try {
                Map<String, Object> serviceResult = dispatcher.runSync("createPartyTelecomNumber", context);
                if (ServiceUtil.isError(serviceResult)) {
                    return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
                }
                newValue.put("contactMechId", serviceResult.get("contactMechId"));
            } catch (Exception e) {
                Debug.logError(e, "Error calling createPartyTelecomNumber: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
            Debug.logInfo("Party ContactMech created", MODULE);
        } else {
            // TODO: Convert call-map-processor (in-map: parameters, out-map: context)
            // xml-resource: component://party/script/org/ofbiz/party/contact/ContactMechMapProcs.xml, processor-name: telecomNumber
            context = new HashMap<String, Object>();
            try {
                Map<String, Object> serviceResult = dispatcher.runSync("createTelecomNumber", context);
                if (ServiceUtil.isError(serviceResult)) {
                    return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
                }
                newValue.put("contactMechId", serviceResult.get("contactMechId"));
            } catch (Exception e) {
                Debug.logError(e, "Error calling createTelecomNumber: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
        }
        // TODO: Convert call-map-processor (in-map: parameters, out-map: context2)
        // xml-resource: component://workeffort/script/org/ofbiz/workeffort/workeffort/WorkflowMapProcessors.xml, processor-name: workEffortContactMech
        Map<String, Object> context2 = new HashMap<String, Object>();
        context2.put("contactMechId", newValue.get("contactMechId"));
        Debug.logInfo("Copied id to context2: " + ((Map<String, Object>) context2).get("contactMechId"), MODULE);
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("createWorkEffortContactMech", context2);
            if (ServiceUtil.isError(serviceResult)) {
                return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
            }
        } catch (Exception e) {
            Debug.logError(e, "Error calling createWorkEffortContactMech: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        result.put("contactMechId", newValue.get("contactMechId"));
        result.put("contactMechId", newValue.get("contactMechId"));

        return result;
    }


    /**
     * Update a TelecomNumber for WorkEffort
     */
    public static Map<String, Object> updateWorkEffortTelecomNumber(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        GenericValue newValue = delegator.makeValue("WorkEffortContactMech");
        // TODO: Convert call-map-processor (in-map: parameters, out-map: context)
        // xml-resource: component://party/script/org/ofbiz/party/contact/ContactMechMapProcs.xml, processor-name: telecomNumber
        context = new HashMap<String, Object>();
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("updateTelecomNumber", context);
            if (ServiceUtil.isError(serviceResult)) {
                return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
            }
            newValue.put("contactMechId", serviceResult.get("contactMechId"));
        } catch (Exception e) {
            Debug.logError(e, "Error calling updateTelecomNumber: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        // TODO: Convert call-map-processor (in-map: parameters, out-map: context2)
        // xml-resource: component://workeffort/script/org/ofbiz/workeffort/workeffort/WorkflowMapProcessors.xml, processor-name: workEffortContactMech
        Map<String, Object> context2 = new HashMap<String, Object>();
        context2.put("newContactMechId", newValue.get("contactMechId"));
        context2.put("contactMechTypeId", "TELECOM_NUMBER");
        Debug.logInfo("Copied id to context2: " + ((Map<String, Object>) context2).get("newContactMechId"), MODULE);
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("updateWorkEffortContactMech", context2);
            if (ServiceUtil.isError(serviceResult)) {
                return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
            }
        } catch (Exception e) {
            Debug.logError(e, "Error calling updateWorkEffortContactMech: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        Debug.logInfo("Setting result id: " + newValue.get("contactMechId"), MODULE);
        result.put("contactMechId", newValue.get("contactMechId"));
        result.put("contactMechId", newValue.get("contactMechId"));

        return result;
    }


    /**
     * Create an email address for WorkEffort
     */
    public static Map<String, Object> createWorkEffortEmailAddress(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        List<String> error_list = new LinkedList<>();

        // TODO: Convert call-map-processor (in-map: parameters, out-map: cwecmMap)
        // xml-resource: component://workeffort/script/org/ofbiz/workeffort/workeffort/WorkflowMapProcessors.xml, processor-name: emailAddress
        Map<String, Object> cwecmMap = new HashMap<String, Object>();
        if (!error_list.isEmpty()) {
            return ServiceUtil.returnError(error_list);
        }
        cwecmMap.put("contactMechTypeId", "EMAIL_ADDRESS");
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("createWorkEffortContactMech", cwecmMap);
            if (ServiceUtil.isError(serviceResult)) {
                return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
            }
            result.put("contactMechId", serviceResult.get("contactMechId"));
        } catch (Exception e) {
            Debug.logError(e, "Error calling createWorkEffortContactMech: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }

        return result;
    }


    /**
     * Update an email address for WorkEffort
     */
    public static Map<String, Object> updateWorkEffortEmailAddress(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        List<String> error_list = new LinkedList<>();

        // TODO: Convert call-map-processor (in-map: parameters, out-map: uwecmMap)
        // xml-resource: component://workeffort/script/org/ofbiz/workeffort/workeffort/WorkflowMapProcessors.xml, processor-name: emailAddress
        Map<String, Object> uwecmMap = new HashMap<String, Object>();
        if (!error_list.isEmpty()) {
            return ServiceUtil.returnError(error_list);
        }
        uwecmMap.put("contactMechTypeId", "EMAIL_ADDRESS");
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("updateWorkEffortContactMech", uwecmMap);
            if (ServiceUtil.isError(serviceResult)) {
                return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
            }
            result.put("contactMechId", serviceResult.get("contactMechId"));
        } catch (Exception e) {
            Debug.logError(e, "Error calling updateWorkEffortContactMech: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }

        return result;
    }


    /**
     * Quick Assign Party To WorkEffort as Owner
     */
    public static Map<String, Object> quickAssignPartyToWorkEffort(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        GenericValue newWorkEffortPartyAssignment = null;
        List<Object> newPartyRoleList = null;
        GenericValue newPartyRole = null;
        Timestamp nowTimestamp = null;
        if (UtilValidate.isNotEmpty(context.get("quickAssignPartyId"))) {
            nowTimestamp = new Timestamp(System.currentTimeMillis());
            newPartyRole = delegator.makeValue("PartyRole");
            newPartyRole.put("partyId", context.get("quickAssignPartyId"));
            newPartyRole.put("roleTypeId", "CAL_OWNER");
            newPartyRoleList.add(newPartyRole);
            try {
                delegator.storeAll(UtilGenerics.cast(newPartyRoleList));
            } catch (Exception e) {
                Debug.logError(e, "Error storing list: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
            newWorkEffortPartyAssignment = delegator.makeValue("WorkEffortPartyAssignment");
            newWorkEffortPartyAssignment.put("workEffortId", context.get("workEffortId"));
            newWorkEffortPartyAssignment.put("partyId", context.get("quickAssignPartyId"));
            newWorkEffortPartyAssignment.put("roleTypeId", "CAL_OWNER");
            newWorkEffortPartyAssignment.put("statusId", "PRTYASGN_ASSIGNED");
            newWorkEffortPartyAssignment.put("fromDate", nowTimestamp);
            try {
                delegator.create(newWorkEffortPartyAssignment);
            } catch (Exception e) {
                Debug.logError(e, "Error creating value: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
        }

        return result;
    }


    /**
     * Quick Assign Party To WorkEffort
     */
    public static Map<String, Object> quickAssignPartyToWorkEffortWithRole(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        GenericValue newWorkEffortPartyAssignment = null;
        List<Object> newPartyRoleList = null;
        GenericValue newPartyRole = null;
        if (UtilValidate.isNotEmpty(context.get("quickAssignPartyId"))) {
            newPartyRole = delegator.makeValue("PartyRole");
            newPartyRole.put("partyId", context.get("quickAssignPartyId"));
            newPartyRole.put("roleTypeId", context.get("roleTypeId"));
            newPartyRoleList.add(newPartyRole);
            try {
                delegator.storeAll(UtilGenerics.cast(newPartyRoleList));
            } catch (Exception e) {
                Debug.logError(e, "Error storing list: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
            newWorkEffortPartyAssignment = delegator.makeValue("WorkEffortPartyAssignment");
            newWorkEffortPartyAssignment.put("workEffortId", context.get("workEffortId"));
            newWorkEffortPartyAssignment.put("partyId", context.get("quickAssignPartyId"));
            newWorkEffortPartyAssignment.put("roleTypeId", context.get("roleTypeId"));
            newWorkEffortPartyAssignment.put("statusId", "CAL_ACCEPTED");
            Timestamp newWorkEffortPartyAssignment_fromDate = new Timestamp(System.currentTimeMillis());
            try {
                delegator.create(newWorkEffortPartyAssignment);
            } catch (Exception e) {
                Debug.logError(e, "Error creating value: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
        }

        return result;
    }


    /**
     * Create Work Effort Note
     */
    public static Map<String, Object> createWorkEffortNote(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        GenericValue newEntity = null;
        newEntity = delegator.makeValue("NoteData");
        ((GenericValue) newEntity).put("noteId", delegator.getNextSeqId("NoteData"));
        result.put("noteId", newEntity.get("noteId"));
        newEntity.put("noteInfo", context.get("noteInfo"));
        if (UtilValidate.isNotEmpty(context.get("noteParty"))) {
            newEntity.put("noteParty", context.get("noteParty"));
        } else {
            newEntity.put("noteParty", ((GenericValue) context.get("userLogin")).getString("partyId"));
        }
        newEntity.put("noteName", context.get("noteName"));
        Timestamp newEntity_noteDateTime = new Timestamp(System.currentTimeMillis());
        try {
            delegator.create(newEntity);
        } catch (Exception e) {
            Debug.logError(e, "Error creating value: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        GenericValue newWorkEffortNote = delegator.makeValue("WorkEffortNote");
        newWorkEffortNote.put("noteId", newEntity.get("noteId"));
        newWorkEffortNote.put("workEffortId", context.get("workEffortId"));
        newWorkEffortNote.put("internalNote", context.get("internalNote"));
        try {
            delegator.create(newWorkEffortNote);
        } catch (Exception e) {
            Debug.logError(e, "Error creating value: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }

        return result;
    }


    /**
     * Update Work Effort Note
     */
    public static Map<String, Object> updateWorkEffortNote(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        GenericValue lookedUpValue = null;
        try {
            lookedUpValue = EntityQuery.use(delegator)
                    .from("WorkEffortNote")
                    .where(context)
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying WorkEffortNote: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        GenericValue lookedUpValueForNoteData = null;
        try {
            lookedUpValueForNoteData = EntityQuery.use(delegator)
                    .from("NoteData")
                    .where(context)
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying NoteData: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        lookedUpValueForNoteData.setNonPKFields(context);
        lookedUpValue.setNonPKFields(context);
        try {
            delegator.store(lookedUpValue);
        } catch (Exception e) {
            Debug.logError(e, "Error storing value: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        try {
            delegator.store(lookedUpValueForNoteData);
        } catch (Exception e) {
            Debug.logError(e, "Error storing value: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }

        return result;
    }


    /**
     * Create a WorkEffort and association
     */
    public static Map<String, Object> createWorkEffortAndAssoc(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        List<String> error_list = new LinkedList<>();

        GenericValue lookedUpValue = null;
        Object workEffortIdTo = null;
        Map<String, Object> createWorkEffortAssocParams = null;
        Map<String, Object> createWorkeEffortParams = null;
        if (UtilValidate.isEmpty(context.get("fromDate"))) {
            Timestamp parameters_fromDate = new Timestamp(System.currentTimeMillis());
        } else {
            try {
                lookedUpValue = EntityQuery.use(delegator)
                        .from("WorkEffortAssoc")
                        .where(context)
                        .queryOne();
            } catch (Exception e) {
                Debug.logError(e, "Error querying WorkEffortAssoc: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
        }
        if (UtilValidate.isNotEmpty(lookedUpValue)) {
            {
                String errorMsg = UtilProperties.getMessage("WorkEffortUiLabels", "WorkEffortWorkEffortAssocIdAlreadyExist", locale);
                error_list.add(errorMsg);
            }
            if (!error_list.isEmpty()) {
                return ServiceUtil.returnError(error_list);
            }
        } else {
            if (UtilValidate.isEmpty(context.get("workEffortIdTo"))) {
                // set-service-fields from "parameters" to "createWorkeEffortParams" for service "createWorkEffort"
                createWorkeEffortParams.putAll(UtilMisc.toMap(context));
                try {
                    Map<String, Object> serviceResult = dispatcher.runSync("createWorkEffort", createWorkeEffortParams);
                    if (ServiceUtil.isError(serviceResult)) {
                        return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
                    }
                    workEffortIdTo = serviceResult.get("workEffortId");
                    result.put("workEffortId", serviceResult.get("workEffortId"));
                } catch (Exception e) {
                    Debug.logError(e, "Error calling createWorkEffort: " + e.getMessage(), MODULE);
                    return ServiceUtil.returnError(e.getMessage());
                }
                context.put("workEffortIdTo", workEffortIdTo);
            }
            // set-service-fields from "parameters" to "createWorkEffortAssocParams" for service "createWorkEffortAssoc"
            createWorkEffortAssocParams.putAll(UtilMisc.toMap(context));
            try {
                Map<String, Object> serviceResult = dispatcher.runSync("createWorkEffortAssoc", createWorkEffortAssocParams);
                if (ServiceUtil.isError(serviceResult)) {
                    return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
                }
            } catch (Exception e) {
                Debug.logError(e, "Error calling createWorkEffortAssoc: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
        }

        return result;
    }


    /**
     * Create a WorkEffort association
     */
    public static Map<String, Object> createWorkEffortAssoc(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        List<String> error_list = new LinkedList<>();

        GenericValue lookedUpValue = null;
        GenericValue newEntity = null;
        if (UtilValidate.isEmpty(context.get("fromDate"))) {
            Timestamp parameters_fromDate = new Timestamp(System.currentTimeMillis());
        } else {
            try {
                lookedUpValue = EntityQuery.use(delegator)
                        .from("WorkEffortAssoc")
                        .where(context)
                        .queryOne();
            } catch (Exception e) {
                Debug.logError(e, "Error querying WorkEffortAssoc: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
        }
        if (UtilValidate.isNotEmpty(lookedUpValue)) {
            {
                String errorMsg = UtilProperties.getMessage("WorkEffortUiLabels", "WorkEffortWorkEffortAssocIdAlreadyExist", locale);
                error_list.add(errorMsg);
            }
            if (!error_list.isEmpty()) {
                return ServiceUtil.returnError(error_list);
            }
        } else {
            newEntity = delegator.makeValue("WorkEffortAssoc");
            newEntity.setPKFields(context);
            newEntity.setNonPKFields(context);
            if (UtilValidate.isEmpty(newEntity.get("sequenceNum"))) {
                newEntity.put("sequenceNum", 0L);
            }
            try {
                delegator.create(newEntity);
            } catch (Exception e) {
                Debug.logError(e, "Error creating value: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
        }
        result.put("workEffortIdFrom", newEntity.get("workEffortIdFrom"));
        result.put("workEffortIdTo", newEntity.get("workEffortIdTo"));
        result.put("workEffortAssocTypeId", newEntity.get("workEffortAssocTypeId"));
        result.put("fromDate", newEntity.get("fromDate"));

        return result;
    }


    /**
     * Update a WorkEffort association
     */
    public static Map<String, Object> updateWorkEffortAssoc(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        GenericValue lookedUpValue = null;
        try {
            lookedUpValue = EntityQuery.use(delegator)
                    .from("WorkEffortAssoc")
                    .where(context)
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying WorkEffortAssoc: " + e.getMessage(), MODULE);
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
     * Remove a WorkEffort association
     */
    public static Map<String, Object> removeWorkEffortAssoc(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        GenericValue lookedUpValue = null;
        try {
            lookedUpValue = EntityQuery.use(delegator)
                    .from("WorkEffortAssoc")
                    .where(context)
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying WorkEffortAssoc: " + e.getMessage(), MODULE);
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
     * Copy WorkEffort associations
     */
    public static Map<String, Object> copyWorkEffortAssocs(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        List<String> error_list = new LinkedList<>();

        List<GenericValue> emptyField = null;
        Object workEffortIdTo = null;
        Map<String, Object> copyWorkEffortCtx = new HashMap<>();
        GenericValue newWorkEffortAssoc = null;
        Object deepCopy = context.get("deepCopy");
        Object excludeExpiredAssocs = context.get("excludeExpiredAssocs");
        List<GenericValue> workEffortAssocs = null;
        try {
            workEffortAssocs = EntityQuery.use(delegator)
                    .from("WorkEffortAssoc")
                    .where(UtilMisc.toMap("workEffortIdFrom", context.get("sourceWorkEffortId")))
                    .queryList();
        } catch (Exception e) {
            Debug.logError(e, "Error querying entity: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        if ("Y".equals(excludeExpiredAssocs)) {
            emptyField = EntityUtil.filterByDate(UtilGenerics.cast(workEffortAssocs));
        }
        if (workEffortAssocs != null) {
            for (GenericValue workEffortAssoc : workEffortAssocs) {
                workEffortIdTo = workEffortAssoc.get("workEffortIdTo");
                if ("Y".equals(deepCopy)) {
                    copyWorkEffortCtx = new HashMap<String, Object>();
                    // set-service-fields from "parameters" to "copyWorkEffortCtx" for service "copyWorkEffort"
                    copyWorkEffortCtx.putAll(UtilMisc.toMap(context));
                    copyWorkEffortCtx.remove("targetWorkEffortId");
                    copyWorkEffortCtx.put("sourceWorkEffortId", workEffortIdTo);
                    copyWorkEffortCtx.put("copyWorkEffortAssocs", "Y");
                    try {
                        Map<String, Object> serviceResult = dispatcher.runSync("copyWorkEffort", copyWorkEffortCtx);
                        if (ServiceUtil.isError(serviceResult)) {
                            return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
                        }
                        workEffortIdTo = serviceResult.get("workEffortId");
                    } catch (Exception e) {
                        Debug.logError(e, "Error calling copyWorkEffort: " + e.getMessage(), MODULE);
                        return ServiceUtil.returnError(e.getMessage());
                    }
                    if (!error_list.isEmpty()) {
                        return ServiceUtil.returnError(error_list);
                    }
                }
                newWorkEffortAssoc = GenericValue.create((GenericValue) workEffortAssoc);
                newWorkEffortAssoc.put("workEffortIdFrom", context.get("targetWorkEffortId"));
                newWorkEffortAssoc.put("workEffortIdTo", workEffortIdTo);
                try {
                    delegator.create(newWorkEffortAssoc);
                } catch (Exception e) {
                    Debug.logError(e, "Error creating value: " + e.getMessage(), MODULE);
                    return ServiceUtil.returnError(e.getMessage());
                }
            }
        }

        return result;
    }


    /**
     * Create a link between a WorkEffort and a Product
     */
    public static Map<String, Object> createWorkEffortGoodStandard(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        List<String> error_list = new LinkedList<>();

        GenericValue newEntity = null;
        GenericValue lookedUpValue = null;
        try {
            lookedUpValue = EntityQuery.use(delegator)
                    .from("WorkEffortGoodStandard")
                    .where(context)
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying WorkEffortGoodStandard: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        if (UtilValidate.isNotEmpty(lookedUpValue)) {
            {
                String errorMsg = UtilProperties.getMessage("WorkEffortUiLabels", "WorkEffortWorkEffortGoodStandardAlreadyExist", locale);
                error_list.add(errorMsg);
            }
            if (!error_list.isEmpty()) {
                return ServiceUtil.returnError(error_list);
            }
        } else {
            newEntity = delegator.makeValue("WorkEffortGoodStandard");
            newEntity.setPKFields(context);
            if (UtilValidate.isEmpty(newEntity.get("fromDate"))) {
                newEntity.set("fromDate", new Timestamp(System.currentTimeMillis())); // SCIPIO: fixed lost assignment
            }
            newEntity.setNonPKFields(context);
            try {
                delegator.create(newEntity);
            } catch (Exception e) {
                Debug.logError(e, "Error creating value: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
        }

        return result;
    }


    /**
     * Update a link between a WorkEffort and a Product
     */
    public static Map<String, Object> updateWorkEffortGoodStandard(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        GenericValue lookedUpValue = null;
        try {
            lookedUpValue = EntityQuery.use(delegator)
                    .from("WorkEffortGoodStandard")
                    .where(context)
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying WorkEffortGoodStandard: " + e.getMessage(), MODULE);
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
     * Remove a link between a WorkEffort and a Product
     */
    public static Map<String, Object> removeWorkEffortGoodStandard(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        GenericValue lookedUpValue = null;
        try {
            lookedUpValue = EntityQuery.use(delegator)
                    .from("WorkEffortGoodStandard")
                    .where(context)
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying WorkEffortGoodStandard: " + e.getMessage(), MODULE);
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
     * Create or update WorkEffortInventoryAssign
     */
    public static Map<String, Object> assignInventoryToWorkEffort(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        GenericValue newEntity = null;
        Object operationName = "Create or update WorkEffortInventoryAssign";
        GenericValue foundEntity = null;
        try {
            foundEntity = EntityQuery.use(delegator)
                    .from("WorkEffortInventoryAssign")
                    .where(context)
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying WorkEffortInventoryAssign: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        if (UtilValidate.isNotEmpty(foundEntity)) {
            foundEntity.set("quantity", (new BigDecimal(foundEntity.get("quantity").toString())).doubleValue());
            try {
                delegator.store(foundEntity);
            } catch (Exception e) {
                Debug.logError(e, "Error storing value: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
        } else {
            newEntity = delegator.makeValue("WorkEffortInventoryAssign");
            newEntity.setPKFields(context);
            newEntity.setNonPKFields(context);
            try {
                delegator.create(newEntity);
            } catch (Exception e) {
                Debug.logError(e, "Error creating value: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
        }

        return result;
    }


    /**
     * Create a CustRequestWorkEffort
     */
    public static Map<String, Object> createWorkEffortRequest(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        List<String> error_list = new LinkedList<>();

        GenericValue lookupMap = delegator.makeValue("CustRequestWorkEffort");
        lookupMap.setPKFields(context);
        GenericValue custRequestWorkEffort = null;
        try {
            custRequestWorkEffort = EntityQuery.use(delegator)
                    .from("CustRequestWorkEffort")
                    .where(lookupMap)
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error finding by primary key CustRequestWorkEffort: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        if (UtilValidate.isNotEmpty(custRequestWorkEffort)) {
            {
                String errorMsg = UtilProperties.getMessage("WorkEffortUiLabels", "WorkEffortCustRequestAlreadyExists", locale);
                error_list.add(errorMsg);
            }
            if (!error_list.isEmpty()) {
                return ServiceUtil.returnError(error_list);
            }
        }
        lookupMap.setNonPKFields(context);
        try {
            delegator.create(lookupMap);
        } catch (Exception e) {
            Debug.logError(e, "Error creating value: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        result.put("lookupMap", context.get("custRequestId"));

        return result;
    }


    /**
     * Delete a CustRequestWorkEffort
     */
    public static Map<String, Object> deleteWorkEffortRequest(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        List<String> error_list = new LinkedList<>();

        GenericValue custRequestWorkEffort = null;
        try {
            custRequestWorkEffort = EntityQuery.use(delegator)
                    .from("CustRequestWorkEffort")
                    .where(context)
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying CustRequestWorkEffort: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        if (UtilValidate.isEmpty(custRequestWorkEffort)) {
            {
                String errorMsg = UtilProperties.getMessage("WorkEffortUiLabels", "WorkEffortCustRequestDoesNotExists", locale);
                error_list.add(errorMsg);
            }
            if (!error_list.isEmpty()) {
                return ServiceUtil.returnError(error_list);
            }
        }
        try {
            delegator.removeValue(custRequestWorkEffort);
        } catch (Exception e) {
            Debug.logError(e, "Error removing value: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }

        return result;
    }


    /**
     * Create a CustRequestItemWorkEffort
     */
    public static Map<String, Object> createWorkEffortRequestItem(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        List<String> error_list = new LinkedList<>();

        GenericValue lookupMap = delegator.makeValue("CustRequestItemWorkEffort");
        lookupMap.setPKFields(context);
        GenericValue custRequestItemWorkEffort = null;
        try {
            custRequestItemWorkEffort = EntityQuery.use(delegator)
                    .from("CustRequestItemWorkEffort")
                    .where(lookupMap)
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error finding by primary key CustRequestItemWorkEffort: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        if (UtilValidate.isNotEmpty(custRequestItemWorkEffort)) {
            {
                String errorMsg = UtilProperties.getMessage("WorkEffortUiLabels", "WorkEffortCustRequestItemAlreadyExists", locale);
                error_list.add(errorMsg);
            }
            if (!error_list.isEmpty()) {
                return ServiceUtil.returnError(error_list);
            }
        }
        lookupMap.setNonPKFields(context);
        try {
            delegator.create(lookupMap);
        } catch (Exception e) {
            Debug.logError(e, "Error creating value: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }

        return result;
    }


    /**
     * Delete a CustRequestItemWorkEffort
     */
    public static Map<String, Object> deleteWorkEffortRequestItem(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        List<String> error_list = new LinkedList<>();

        GenericValue custRequestItemWorkEffort = null;
        try {
            custRequestItemWorkEffort = EntityQuery.use(delegator)
                    .from("CustRequestItemWorkEffort")
                    .where(context)
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying CustRequestItemWorkEffort: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        if (UtilValidate.isEmpty(custRequestItemWorkEffort.get("custRequestItemSeqId"))) {
            {
                String errorMsg = UtilProperties.getMessage("WorkEffortUiLabels", "WorkEffortCustRequestItemDoesNotExists", locale);
                error_list.add(errorMsg);
            }
            if (!error_list.isEmpty()) {
                return ServiceUtil.returnError(error_list);
            }
        }
        try {
            delegator.removeValue(custRequestItemWorkEffort);
        } catch (Exception e) {
            Debug.logError(e, "Error removing value: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }

        return result;
    }


    /**
     * Checks to see if a CustRequestItem exists
     */
    public static Map<String, Object> checkCustRequestItemExists(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        Object custRequestItemExists = null;
        GenericValue lookupMap = delegator.makeValue("CustRequestItem");
        lookupMap.setPKFields(context);
        GenericValue custRequestItem = null;
        try {
            custRequestItem = EntityQuery.use(delegator)
                    .from("CustRequestItem")
                    .where(lookupMap)
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error finding by primary key CustRequestItem: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        if (UtilValidate.isNotEmpty(custRequestItem)) {
            custRequestItemExists = "true";
            result.put("custRequestItemExists", custRequestItemExists);
            Debug.logInfo("custRequestItemExists: " + custRequestItemExists, MODULE);
        } else {
            Debug.logInfo("custRequestItemExists: empty", MODULE);
        }

        return result;
    }


    /**
     * Create a QuoteWorkEffort
     */
    public static Map<String, Object> createWorkEffortQuote(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        List<String> error_list = new LinkedList<>();

        GenericValue lookupMap = delegator.makeValue("QuoteWorkEffort");
        lookupMap.setPKFields(context);
        GenericValue quoteWorkEffort = null;
        try {
            quoteWorkEffort = EntityQuery.use(delegator)
                    .from("QuoteWorkEffort")
                    .where(lookupMap)
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error finding by primary key QuoteWorkEffort: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        if (UtilValidate.isNotEmpty(quoteWorkEffort)) {
            {
                String errorMsg = UtilProperties.getMessage("WorkEffortUiLabels", "WorkEffortQuoteAlreadyExists", locale);
                error_list.add(errorMsg);
            }
            if (!error_list.isEmpty()) {
                return ServiceUtil.returnError(error_list);
            }
        }
        lookupMap.setNonPKFields(context);
        try {
            delegator.create(lookupMap);
        } catch (Exception e) {
            Debug.logError(e, "Error creating value: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        result.put("lookupMap", context.get("quoteId"));

        return result;
    }


    /**
     * Delete a QuoteWorkEffort
     */
    public static Map<String, Object> deleteWorkEffortQuote(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        List<String> error_list = new LinkedList<>();

        GenericValue quoteWorkEffort = null;
        try {
            quoteWorkEffort = EntityQuery.use(delegator)
                    .from("QuoteWorkEffort")
                    .where(context)
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying QuoteWorkEffort: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        if (UtilValidate.isEmpty(quoteWorkEffort)) {
            {
                String errorMsg = UtilProperties.getMessage("WorkEffortUiLabels", "WorkEffortQuoteDoesNotExists", locale);
                error_list.add(errorMsg);
            }
            if (!error_list.isEmpty()) {
                return ServiceUtil.returnError(error_list);
            }
        }
        try {
            delegator.removeValue(quoteWorkEffort);
        } catch (Exception e) {
            Debug.logError(e, "Error removing value: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }

        return result;
    }


    /**
     * Create a WorkRequirementFulfillment
     */
    public static Map<String, Object> createWorkRequirementFulfillment(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        List<String> error_list = new LinkedList<>();

        GenericValue lookupMap = delegator.makeValue("WorkRequirementFulfillment");
        lookupMap.setPKFields(context);
        GenericValue workRequirementFulfillment = null;
        try {
            workRequirementFulfillment = EntityQuery.use(delegator)
                    .from("WorkRequirementFulfillment")
                    .where(lookupMap)
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error finding by primary key WorkRequirementFulfillment: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        if (UtilValidate.isNotEmpty(workRequirementFulfillment)) {
            {
                String errorMsg = UtilProperties.getMessage("WorkEffortUiLabels", "WorkEffortRequirementFulfillmentAlreadyExists", locale);
                error_list.add(errorMsg);
            }
            if (!error_list.isEmpty()) {
                return ServiceUtil.returnError(error_list);
            }
        }
        lookupMap.setNonPKFields(context);
        try {
            delegator.create(lookupMap);
        } catch (Exception e) {
            Debug.logError(e, "Error creating value: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        result.put("lookupMap", context.get("requirementId"));

        return result;
    }


    /**
     * Delete a WorkRequirementFulfillment
     */
    public static Map<String, Object> deleteWorkRequirementFulfillment(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        List<String> error_list = new LinkedList<>();

        GenericValue workRequirementFulfillment = null;
        try {
            workRequirementFulfillment = EntityQuery.use(delegator)
                    .from("WorkRequirementFulfillment")
                    .where(context)
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying WorkRequirementFulfillment: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        if (UtilValidate.isEmpty(workRequirementFulfillment)) {
            {
                String errorMsg = UtilProperties.getMessage("WorkEffortUiLabels", "WorkEffortRequirementFulfillmentDoesNotExists", locale);
                error_list.add(errorMsg);
            }
            if (!error_list.isEmpty()) {
                return ServiceUtil.returnError(error_list);
            }
        }
        try {
            delegator.removeValue(workRequirementFulfillment);
        } catch (Exception e) {
            Debug.logError(e, "Error removing value: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }

        return result;
    }


    /**
     * Create a ShoppingListWorkEffort
     */
    public static Map<String, Object> createShoppingListWorkEffort(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        List<String> error_list = new LinkedList<>();

        GenericValue lookupMap = delegator.makeValue("ShoppingListWorkEffort");
        lookupMap.setPKFields(context);
        GenericValue shoppingListWorkEffort = null;
        try {
            shoppingListWorkEffort = EntityQuery.use(delegator)
                    .from("ShoppingListWorkEffort")
                    .where(lookupMap)
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error finding by primary key ShoppingListWorkEffort: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        if (UtilValidate.isNotEmpty(shoppingListWorkEffort)) {
            {
                String errorMsg = UtilProperties.getMessage("WorkEffortUiLabels", "WorkEffortShoppingListAlreadyExists", locale);
                error_list.add(errorMsg);
            }
            if (!error_list.isEmpty()) {
                return ServiceUtil.returnError(error_list);
            }
        }
        lookupMap.setNonPKFields(context);
        try {
            delegator.create(lookupMap);
        } catch (Exception e) {
            Debug.logError(e, "Error creating value: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        result.put("lookupMap", context.get("shoppingListId"));

        return result;
    }


    /**
     * Delete a ShoppingListWorkEffort
     */
    public static Map<String, Object> deleteShoppingListWorkEffort(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        List<String> error_list = new LinkedList<>();

        GenericValue shoppingListWorkEffort = null;
        try {
            shoppingListWorkEffort = EntityQuery.use(delegator)
                    .from("ShoppingListWorkEffort")
                    .where(context)
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying ShoppingListWorkEffort: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        if (UtilValidate.isEmpty(shoppingListWorkEffort)) {
            {
                String errorMsg = UtilProperties.getMessage("WorkEffortUiLabels", "WorkEffortShoppingListDoesNotExists", locale);
                error_list.add(errorMsg);
            }
            if (!error_list.isEmpty()) {
                return ServiceUtil.returnError(error_list);
            }
        }
        try {
            delegator.removeValue(shoppingListWorkEffort);
        } catch (Exception e) {
            Debug.logError(e, "Error removing value: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }

        return result;
    }


    /**
     * Create a OrderHeaderWorkEffort
     */
    public static Map<String, Object> createOrderHeaderWorkEffort(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        List<String> error_list = new LinkedList<>();

        GenericValue lookupMap = delegator.makeValue("OrderHeaderWorkEffort");
        lookupMap.setPKFields(context);
        GenericValue orderWorkEffort = null;
        try {
            orderWorkEffort = EntityQuery.use(delegator)
                    .from("OrderHeaderWorkEffort")
                    .where(lookupMap)
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error finding by primary key OrderHeaderWorkEffort: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        if (UtilValidate.isNotEmpty(orderWorkEffort)) {
            {
                String errorMsg = UtilProperties.getMessage("WorkEffortUiLabels", "WorkEffortOrderHeaderAlreadyExists", locale);
                error_list.add(errorMsg);
            }
            if (!error_list.isEmpty()) {
                return ServiceUtil.returnError(error_list);
            }
        }
        lookupMap.setNonPKFields(context);
        try {
            delegator.create(lookupMap);
        } catch (Exception e) {
            Debug.logError(e, "Error creating value: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        result.put("lookupMap", context.get("orderId"));

        return result;
    }


    /**
     * Delete a OrderHeaderWorkEffort
     */
    public static Map<String, Object> deleteOrderHeaderWorkEffort(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        List<String> error_list = new LinkedList<>();

        GenericValue orderWorkEffort = null;
        try {
            orderWorkEffort = EntityQuery.use(delegator)
                    .from("OrderHeaderWorkEffort")
                    .where(context)
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying OrderHeaderWorkEffort: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        if (UtilValidate.isEmpty(orderWorkEffort)) {
            {
                String errorMsg = UtilProperties.getMessage("WorkEffortUiLabels", "WorkEffortOrderHeaderDoesNotExists", locale);
                error_list.add(errorMsg);
            }
            if (!error_list.isEmpty()) {
                return ServiceUtil.returnError(error_list);
            }
        }
        try {
            delegator.removeValue(orderWorkEffort);
        } catch (Exception e) {
            Debug.logError(e, "Error removing value: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }

        return result;
    }


    /**
     * Based on task's estimate dates, write assign entries for the fixed asset the task is assigned to
     */
    public static Map<String, Object> setWorkEffortFixedAssetAssign(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        GenericValue workEffort = null;
        try {
            workEffort = EntityQuery.use(delegator)
                    .from("WorkEffort")
                    .where(context)
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying WorkEffort: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        Map<String, Object> findMap = new HashMap<String, Object>();
        findMap.put("workEffortId", workEffort.get("workEffortId"));
        findMap.put("fixedAssetId", workEffort.get("fixedAssetId"));
        List<GenericValue> existingAssignments = null;
        try {
            existingAssignments = EntityQuery.use(delegator)
                    .from("WorkEffortFixedAssetAssign")
                    .where(findMap)
                    .queryList();
        } catch (Exception e) {
            Debug.logError(e, "Error querying WorkEffortFixedAssetAssign: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        List<GenericValue> emptyField = EntityUtil.filterByDate(UtilGenerics.cast(existingAssignments));
        if (existingAssignments != null) {
            for (GenericValue existingAssignment : existingAssignments) {
                try {
                    delegator.removeValue(existingAssignment);
                } catch (Exception e) {
                    Debug.logError(e, "Error removing value: " + e.getMessage(), MODULE);
                    return ServiceUtil.returnError(e.getMessage());
                }
            }
        }
        GenericValue newEntity = delegator.makeValue("WorkEffortFixedAssetAssign");
        newEntity.put("workEffortId", workEffort.get("workEffortId"));
        newEntity.put("fixedAssetId", workEffort.get("fixedAssetId"));
        newEntity.put("statusId", workEffort.get("currentStatusId"));
        newEntity.put("fromDate", workEffort.get("estimatedStartDate"));
        newEntity.put("thruDate", workEffort.get("estimatedCompletionDate"));
        try {
            delegator.create(newEntity);
        } catch (Exception e) {
            Debug.logError(e, "Error creating value: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }

        return result;
    }


    /**
     * Create a WorkEffort FixedAsset Standard
     */
    public static Map<String, Object> createWorkEffortFixedAssetStd(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        List<String> error_list = new LinkedList<>();

        GenericValue newWEFixedAssetStd = null;
        GenericValue lookedUpValue = null;
        try {
            lookedUpValue = EntityQuery.use(delegator)
                    .from("WorkEffortFixedAssetStd")
                    .where(context)
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying WorkEffortFixedAssetStd: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        if (UtilValidate.isNotEmpty(lookedUpValue)) {
            {
                String errorMsg = UtilProperties.getMessage("WorkEffortUiLabels", "WorkEffortFixedAssetAlreadyExist", locale);
                error_list.add(errorMsg);
            }
            if (!error_list.isEmpty()) {
                return ServiceUtil.returnError(error_list);
            }
        } else {
            newWEFixedAssetStd = delegator.makeValue("WorkEffortFixedAssetStd");
            newWEFixedAssetStd.setPKFields(context);
            newWEFixedAssetStd.setNonPKFields(context);
            try {
                delegator.create(newWEFixedAssetStd);
            } catch (Exception e) {
                Debug.logError(e, "Error creating value: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
        }

        return result;
    }


    /**
     * Update an existing WorkEffort FixedAsset Standard
     */
    public static Map<String, Object> updateWorkEffortFixedAssetStd(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        GenericValue lookedUpValue = null;
        try {
            lookedUpValue = EntityQuery.use(delegator)
                    .from("WorkEffortFixedAssetStd")
                    .where(context)
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying WorkEffortFixedAssetStd: " + e.getMessage(), MODULE);
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
     * Delete a WorkEffort FixedAsset Standard
     */
    public static Map<String, Object> removeWorkEffortFixedAssetStd(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        GenericValue lookedUpValue = null;
        try {
            lookedUpValue = EntityQuery.use(delegator)
                    .from("WorkEffortFixedAssetStd")
                    .where(context)
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying WorkEffortFixedAssetStd: " + e.getMessage(), MODULE);
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
     * Create a WorkEffort FixedAsset Assign
     */
    public static Map<String, Object> createWorkEffortFixedAssetAssign(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        List<String> error_list = new LinkedList<>();

        GenericValue newWEFixedAssetAssign = null;
        GenericValue prodRunTask = null;
        GenericValue lookedUpValue = null;
        try {
            lookedUpValue = EntityQuery.use(delegator)
                    .from("WorkEffortFixedAssetAssign")
                    .where(context)
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying WorkEffortFixedAssetAssign: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        if (UtilValidate.isNotEmpty(lookedUpValue)) {
            {
                String errorMsg = UtilProperties.getMessage("WorkEffortUiLabels", "WorkEffortFixedAssetAlreadyExist", locale);
                error_list.add(errorMsg);
            }
        } else {
            newWEFixedAssetAssign = delegator.makeValue("WorkEffortFixedAssetAssign");
            newWEFixedAssetAssign.setPKFields(context);
            newWEFixedAssetAssign.setNonPKFields(context);
            if (UtilValidate.isEmpty(context.get("fromDate"))) {
                try {
                    prodRunTask = EntityQuery.use(delegator)
                            .from("WorkEffort")
                            .where(context)
                            .queryOne();
                } catch (Exception e) {
                    Debug.logError(e, "Error querying WorkEffort: " + e.getMessage(), MODULE);
                    return ServiceUtil.returnError(e.getMessage());
                }
                Timestamp newWEFixedAssetAssign_fromDate = new Timestamp(System.currentTimeMillis());
                if (UtilValidate.isNotEmpty(prodRunTask.get("estimatedStartDate"))) {
                    newWEFixedAssetAssign.put("fromDate", prodRunTask.get("estimatedStartDate"));
                }
                if (UtilValidate.isNotEmpty(prodRunTask.get("actualStartDate"))) {
                    newWEFixedAssetAssign.put("fromDate", prodRunTask.get("actualStartDate"));
                }
            }
            try {
                delegator.create(newWEFixedAssetAssign);
            } catch (Exception e) {
                Debug.logError(e, "Error creating value: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
        }

        return result;
    }


    /**
     * Update an existing WorkEffort FixedAsset Assign
     */
    public static Map<String, Object> updateWorkEffortFixedAssetAssign(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        GenericValue lookedUpValue = null;
        try {
            lookedUpValue = EntityQuery.use(delegator)
                    .from("WorkEffortFixedAssetAssign")
                    .where(context)
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying WorkEffortFixedAssetAssign: " + e.getMessage(), MODULE);
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
     * Remove a WorkEffort FixedAsset Assign
     */
    public static Map<String, Object> removeWorkEffortFixedAssetAssign(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        GenericValue lookedUpValue = null;
        try {
            lookedUpValue = EntityQuery.use(delegator)
                    .from("WorkEffortFixedAssetAssign")
                    .where(context)
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying WorkEffortFixedAssetAssign: " + e.getMessage(), MODULE);
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
     * Create Work Effort Content
     */
    public static Map<String, Object> createWorkEffortContent(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        GenericValue newEntity = null;
        Timestamp nowTimestamp = null;
        newEntity = delegator.makeValue("WorkEffortContent");
        newEntity.setNonPKFields(context);
        newEntity.setPKFields(context);
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

        return result;
    }


    /**
     * Update Work Effort Content
     */
    public static Map<String, Object> updateWorkEffortContent(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        GenericValue lookedUpValue = null;
        try {
            lookedUpValue = EntityQuery.use(delegator)
                    .from("WorkEffortContent")
                    .where(context)
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying WorkEffortContent: " + e.getMessage(), MODULE);
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
     * Remove a WorkEffort Content
     */
    public static Map<String, Object> deleteWorkEffortContent(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        GenericValue lookedUpValue = null;
        try {
            lookedUpValue = EntityQuery.use(delegator)
                    .from("WorkEffortContent")
                    .where(context)
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying WorkEffortContent: " + e.getMessage(), MODULE);
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
     * Create Work Effort Review
     */
    public static Map<String, Object> createWorkEffortReview(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        List<String> error_list = new LinkedList<>();

        GenericValue newEntity = null;
        Timestamp nowTimestamp = null;
        GenericValue lookedUpValue = null;
        try {
            lookedUpValue = EntityQuery.use(delegator)
                    .from("WorkEffortReview")
                    .where(context)
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying WorkEffortReview: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        if (UtilValidate.isNotEmpty(lookedUpValue)) {
            {
                String errorMsg = UtilProperties.getMessage("WorkEffortUiLabels", "WorkEffortWorkEffortReviewAlreadyExist", locale);
                error_list.add(errorMsg);
            }
            if (!error_list.isEmpty()) {
                return ServiceUtil.returnError(error_list);
            }
        } else {
            newEntity = delegator.makeValue("WorkEffortReview");
            newEntity.setNonPKFields(context);
            newEntity.setPKFields(context);
            if (UtilValidate.isEmpty(newEntity.get("userLoginId"))) {
                newEntity.put("userLoginId", ((GenericValue) context.get("userLogin")).getString("userLoginId"));
            }
            if (UtilValidate.isEmpty(newEntity.get("reviewDate"))) {
                nowTimestamp = new Timestamp(System.currentTimeMillis());
                newEntity.put("reviewDate", nowTimestamp);
            }
            try {
                delegator.create(newEntity);
            } catch (Exception e) {
                Debug.logError(e, "Error creating value: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
        }

        return result;
    }


    /**
     * Update Work Effort Review
     */
    public static Map<String, Object> updateWorkEffortReview(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        GenericValue lookedUpValue = null;
        try {
            lookedUpValue = EntityQuery.use(delegator)
                    .from("WorkEffortReview")
                    .where(context)
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying WorkEffortReview: " + e.getMessage(), MODULE);
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
     * Remove a WorkEffort Review
     */
    public static Map<String, Object> deleteWorkEffortReview(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        GenericValue lookedUpValue = null;
        try {
            lookedUpValue = EntityQuery.use(delegator)
                    .from("WorkEffortReview")
                    .where(context)
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying WorkEffortReview: " + e.getMessage(), MODULE);
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
     * Index the Keywords for a WorkEffort
     */
    public static Map<String, Object> indexWorkEffortKeywords(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        List<String> error_list = new LinkedList<>();

        GenericValue workEffort = null;
        Map<String, Object> findWorkEffortMap = null;
        workEffort = (GenericValue) context.get("workEffort");
        if (UtilValidate.isEmpty(workEffort)) {
            findWorkEffortMap.put("workEffortId", context.get("workEffortId"));
            try {
                workEffort = EntityQuery.use(delegator)
                        .from("WorkEffort")
                        .where(findWorkEffortMap)
                        .queryOne();
            } catch (Exception e) {
                Debug.logError(e, "Error finding by primary key WorkEffort: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
        }
        if (!error_list.isEmpty()) {
            return ServiceUtil.returnError(error_list);
        }
        try {
            WorkEffortKeywordIndex.indexKeywords((GenericValue) workEffort);
        } catch (Exception e) {
            Debug.logError(e, "Error calling WorkEffortKeywordIndex.indexKeywords: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }

        return result;
    }


    /**
     * Create Work Effort Keyword
     */
    public static Map<String, Object> createWorkEffortKeyword(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        List<String> error_list = new LinkedList<>();

        GenericValue newEntity = null;
        GenericValue lookedUpValue = null;
        try {
            lookedUpValue = EntityQuery.use(delegator)
                    .from("WorkEffortKeyword")
                    .where(context)
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying WorkEffortKeyword: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        if (UtilValidate.isNotEmpty(lookedUpValue)) {
            {
                String errorMsg = UtilProperties.getMessage("WorkEffortUiLabels", "WorkEffortKeywordAlreadyExist", locale);
                error_list.add(errorMsg);
            }
        } else {
            newEntity = delegator.makeValue("WorkEffortKeyword");
            if (UtilValidate.isEmpty(context.get("workEffortId"))) {
                {
                    String errorMsg = UtilProperties.getMessage("WorkEffortUiLabels", "WorkEffortIdMissing", locale);
                    error_list.add(errorMsg);
                }
            }
            if (UtilValidate.isEmpty(context.get("keyword"))) {
                {
                    String errorMsg = UtilProperties.getMessage("ProductUiLabels", "productevents.keyword_missing", locale);
                    error_list.add(errorMsg);
                }
            }
            if (!error_list.isEmpty()) {
                return ServiceUtil.returnError(error_list);
            }
            newEntity.setPKFields(context);
            newEntity.setNonPKFields(context);
            try {
                delegator.create(newEntity);
            } catch (Exception e) {
                Debug.logError(e, "Error creating value: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
        }
        if (!error_list.isEmpty()) {
            return ServiceUtil.returnError(error_list);
        }

        return result;
    }


    /**
     * Remove a WorkEffort Keyword
     */
    public static Map<String, Object> deleteWorkEffortKeyword(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        GenericValue lookedUpValue = null;
        try {
            lookedUpValue = EntityQuery.use(delegator)
                    .from("WorkEffortKeyword")
                    .where(context)
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying WorkEffortKeyword: " + e.getMessage(), MODULE);
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
     * Create all Work Effort Keyword
     */
    public static Map<String, Object> createWorkEffortKeywords(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        Map<String, Object> findWorkEffortMap = new HashMap<String, Object>();
        findWorkEffortMap.put("workEffortId", context.get("workEffortId"));
        GenericValue workEffortInstance = null;
        try {
            workEffortInstance = EntityQuery.use(delegator)
                    .from("WorkEffort")
                    .where(findWorkEffortMap)
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error finding by primary key WorkEffort: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        try {
            WorkEffortKeywordIndex.indexKeywords((GenericValue) workEffortInstance);
        } catch (Exception e) {
            Debug.logError(e, "Error calling WorkEffortKeywordIndex.indexKeywords: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }

        return result;
    }


    /**
     * Remove all WorkEffort Keyword
     */
    public static Map<String, Object> deleteWorkEffortKeywords(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        List<String> error_list = new LinkedList<>();

        Map<String, Object> findWorkEffortMap = new HashMap<String, Object>();
        findWorkEffortMap.put("workEffortId", context.get("workEffortId"));
        GenericValue workEffortInstance = null;
        try {
            workEffortInstance = EntityQuery.use(delegator)
                    .from("WorkEffort")
                    .where(findWorkEffortMap)
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error finding by primary key WorkEffort: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        if (UtilValidate.isNotEmpty(workEffortInstance.get("workEffortId"))) {
            try {
                workEffortInstance.removeRelated("WorkEffortKeyword");
            } catch (Exception e) {
                Debug.logError(e, "Error removing related WorkEffortKeyword: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
        }
        if (!error_list.isEmpty()) {
            return ServiceUtil.returnError(error_list);
        }

        return result;
    }


    /**
     * Duplicate a WorkEffort
     */
    public static Map<String, Object> duplicateWorkEffort(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        Security security = dctx.getSecurity();
        List<String> error_list = new LinkedList<>();

        String workEffortId = null;
        GenericValue oldWorkEffort = null;
        List<GenericValue> statusList = null;
        GenericValue oldStatus = null;
        GenericValue newTempValue = null;
        GenericValue foundValue = null;
        List<GenericValue> foundValues = null;
        List<GenericValue> foundValuesAll = null;
        Object removeWorkEffortAssocs = context.get("removeWorkEffortAssocs");
        Object removeWorkEffortContents = context.get("removeWorkEffortContents");
        Object removeWorkEffortNotes = context.get("removeWorkEffortNotes");
        Object removeWorkEffortAssignmentRates = context.get("removeWorkEffortAssignmentRates");
        if (("Y".equals(removeWorkEffortAssocs) || "Y".equals(removeWorkEffortContents) || "Y".equals(removeWorkEffortNotes) || "Y".equals(removeWorkEffortAssignmentRates))) {
            if (!security.hasEntityPermission("WORKEFFORTMGR", "_DELETE", userLogin)) {
                return ServiceUtil.returnError(UtilProperties.getMessage("WorkEffortUiLabels", "WorkEffortDeletePermissionError", locale));
            }
            if (!error_list.isEmpty()) {
                return ServiceUtil.returnError(error_list);
            }
        }
        workEffortId = (String) context.get("workEffortId");
        if (UtilValidate.isEmpty(workEffortId)) {
            workEffortId = delegator.getNextSeqId("WorkEffort");
        }
        try {
            oldWorkEffort = EntityQuery.use(delegator)
                    .from("WorkEffort")
                    .where(UtilMisc.toMap("workEffortId", context.get("oldWorkEffortId")))
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying WorkEffort: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        if (UtilValidate.isEmpty(context.get("statusId"))) {
            try {
                oldStatus = EntityQuery.use(delegator)
                        .from("StatusItem")
                        .where(UtilMisc.toMap("statusId", oldWorkEffort.get("currentStatusId")))
                        .queryOne();
            } catch (Exception e) {
                Debug.logError(e, "Error querying StatusItem: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
            try {
                statusList = EntityQuery.use(delegator)
                        .from("StatusItem")
                        .where(UtilMisc.toMap("statusTypeId", oldStatus.get("statusTypeId")))
                        .queryList();
            } catch (Exception e) {
                Debug.logError(e, "Error querying entity: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
            oldWorkEffort.put("currentStatusId", ((GenericValue) ((List<?>) statusList).get(0)).get("statusId"));
        } else {
            oldWorkEffort.put("currentStatusId", context.get("statusId"));
        }
        Map<String, Object> createWorkEffortCtx = new HashMap<String, Object>();
        // set-service-fields from "oldWorkEffort" to "createWorkEffortCtx" for service "createWorkEffort"
        createWorkEffortCtx.putAll(UtilMisc.toMap(oldWorkEffort));
        createWorkEffortCtx.put("workEffortId", workEffortId);
        createWorkEffortCtx.put("userLogin", context.get("userLogin"));
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("createWorkEffort", createWorkEffortCtx);
            if (ServiceUtil.isError(serviceResult)) {
                return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
            }
        } catch (Exception e) {
            Debug.logError(e, "Error calling createWorkEffort: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        if (!error_list.isEmpty()) {
            return ServiceUtil.returnError(error_list);
        }
        GenericValue newWorkEffort = null;
        try {
            newWorkEffort = EntityQuery.use(delegator)
                    .from("WorkEffort")
                    .where(context)
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying WorkEffort: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        Map<String, Object> workEffortAssocFindContext = new HashMap<String, Object>();
        workEffortAssocFindContext.put("workEffortIdFrom", context.get("oldWorkEffortId"));
        Map<String, Object> reverseWorkEffortFindContext = new HashMap<String, Object>();
        reverseWorkEffortFindContext.put("workEffortIdTo", context.get("oldWorkEffortId"));
        Object duplicateWorkEffortAssocs = context.get("duplicateWorkEffortAssocs");
        if ("Y".equals(duplicateWorkEffortAssocs)) {
            try {
                foundValues = EntityQuery.use(delegator)
                        .from("WorkEffortAssoc")
                        .where(workEffortAssocFindContext)
                        .queryList();
            } catch (Exception e) {
                Debug.logError(e, "Error querying WorkEffortAssoc: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
            if (foundValues != null) {
                for (GenericValue foundValue_iter : foundValues) {
                    foundValue = foundValue_iter;
                    newTempValue = GenericValue.create((GenericValue) foundValue);
                    newTempValue.put("workEffortIdFrom", workEffortId);
                    try {
                        delegator.create(newTempValue);
                    } catch (Exception e) {
                        Debug.logError(e, "Error creating value: " + e.getMessage(), MODULE);
                        return ServiceUtil.returnError(e.getMessage());
                    }
                }
            }
            try {
                foundValues = EntityQuery.use(delegator)
                        .from("WorkEffortAssoc")
                        .where(UtilMisc.toMap("workEffortIdTo", context.get("oldWorkEffortId")))
                        .queryList();
            } catch (Exception e) {
                Debug.logError(e, "Error querying entity: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
            if (foundValues != null) {
                for (GenericValue foundValue_iter : foundValues) {
                    foundValue = foundValue_iter;
                    newTempValue = GenericValue.create((GenericValue) foundValue);
                    newTempValue.put("workEffortIdTo", workEffortId);
                    try {
                        delegator.create(newTempValue);
                    } catch (Exception e) {
                        Debug.logError(e, "Error creating value: " + e.getMessage(), MODULE);
                        return ServiceUtil.returnError(e.getMessage());
                    }
                }
            }
        }
        Map<String, Object> workEffortFindContext = new HashMap<String, Object>();
        workEffortFindContext.put("workEffortId", context.get("oldWorkEffortId"));
        Object duplicateWorkEffortNotes = context.get("duplicateWorkEffortNotes");
        if ("Y".equals(duplicateWorkEffortNotes)) {
            try {
                foundValues = EntityQuery.use(delegator)
                        .from("WorkEffortNote")
                        .where(workEffortFindContext)
                        .queryList();
            } catch (Exception e) {
                Debug.logError(e, "Error querying WorkEffortNote: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
            if (foundValues != null) {
                for (GenericValue foundValue_iter : foundValues) {
                    foundValue = foundValue_iter;
                    newTempValue = GenericValue.create((GenericValue) foundValue);
                    newTempValue.put("workEffortId", workEffortId);
                    try {
                        delegator.create(newTempValue);
                    } catch (Exception e) {
                        Debug.logError(e, "Error creating value: " + e.getMessage(), MODULE);
                        return ServiceUtil.returnError(e.getMessage());
                    }
                }
            }
        }
        Object duplicateWorkEffortContents = context.get("duplicateWorkEffortContents");
        if ("Y".equals(duplicateWorkEffortContents)) {
            try {
                foundValues = EntityQuery.use(delegator)
                        .from("WorkEffortContent")
                        .where(workEffortFindContext)
                        .queryList();
            } catch (Exception e) {
                Debug.logError(e, "Error querying WorkEffortContent: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
            if (foundValues != null) {
                for (GenericValue foundValue_iter : foundValues) {
                    foundValue = foundValue_iter;
                    newTempValue = GenericValue.create((GenericValue) foundValue);
                    newTempValue.put("workEffortId", workEffortId);
                    try {
                        delegator.create(newTempValue);
                    } catch (Exception e) {
                        Debug.logError(e, "Error creating value: " + e.getMessage(), MODULE);
                        return ServiceUtil.returnError(e.getMessage());
                    }
                }
            }
        }
        Object duplicateWorkEffortAssignmentRates = context.get("duplicateWorkEffortAssignmentRates");
        if ("Y".equals(duplicateWorkEffortAssignmentRates)) {
            try {
                foundValuesAll = EntityQuery.use(delegator)
                        .from("RateAmount")
                        .where(workEffortFindContext)
                        .queryList();
            } catch (Exception e) {
                Debug.logError(e, "Error querying RateAmount: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
            foundValues = EntityUtil.filterByDate(UtilGenerics.cast(foundValuesAll));
            if (foundValues != null) {
                for (GenericValue foundValue_iter : foundValues) {
                    foundValue = foundValue_iter;
                    newTempValue = GenericValue.create((GenericValue) foundValue);
                    newTempValue.put("workEffortId", workEffortId);
                    try {
                        delegator.create(newTempValue);
                    } catch (Exception e) {
                        Debug.logError(e, "Error creating value: " + e.getMessage(), MODULE);
                        return ServiceUtil.returnError(e.getMessage());
                    }
                }
            }
        }
        if ("Y".equals(removeWorkEffortAssocs)) {
            try {
                delegator.removeByAnd("WorkEffortAssoc", workEffortAssocFindContext);
            } catch (Exception e) {
                Debug.logError(e, "Error removing WorkEffortAssoc: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
            try {
                delegator.removeByAnd("WorkEffortAssoc", reverseWorkEffortFindContext);
            } catch (Exception e) {
                Debug.logError(e, "Error removing WorkEffortAssoc: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
        }
        if ("Y".equals(removeWorkEffortContents)) {
            try {
                delegator.removeByAnd("WorkEffortContent", workEffortFindContext);
            } catch (Exception e) {
                Debug.logError(e, "Error removing WorkEffortContent: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
        }
        if ("Y".equals(removeWorkEffortNotes)) {
            try {
                delegator.removeByAnd("WorkEffortNote", workEffortFindContext);
            } catch (Exception e) {
                Debug.logError(e, "Error removing WorkEffortNote: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
        }
        if ("Y".equals(removeWorkEffortAssignmentRates)) {
            try {
                delegator.removeByAnd("RateAmount", workEffortFindContext);
            } catch (Exception e) {
                Debug.logError(e, "Error removing RateAmount: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
        }
        result.put("workEffortId", workEffortId);

        return result;
    }


    /**
     * Create WorkEffortSkillStandard
     */
    public static Map<String, Object> createWorkEffortSkillStandard(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        GenericValue newEntity = delegator.makeValue("WorkEffortSkillStandard");
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
     * Update WorkEffortSkillStandard
     */
    public static Map<String, Object> updateWorkEffortSkillStandard(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        GenericValue lookedUpValue = null;
        try {
            lookedUpValue = EntityQuery.use(delegator)
                    .from("WorkEffortSkillStandard")
                    .where(context)
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying WorkEffortSkillStandard: " + e.getMessage(), MODULE);
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
     * Delete WorkEffortSkillStandard
     */
    public static Map<String, Object> deleteWorkEffortSkillStandard(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        GenericValue lookedUpValue = null;
        try {
            lookedUpValue = EntityQuery.use(delegator)
                    .from("WorkEffortSkillStandard")
                    .where(context)
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying WorkEffortSkillStandard: " + e.getMessage(), MODULE);
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
     * Create Work Effort InventoryProduced
     */
    public static Map<String, Object> createWorkEffortInventoryProduced(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        GenericValue newEntity = delegator.makeValue("WorkEffortInventoryProduced");
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
     * Delete Work Effort InventoryProduced
     */
    public static Map<String, Object> deleteWorkEffortInventoryProduced(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        GenericValue lookedUpValue = null;
        try {
            lookedUpValue = EntityQuery.use(delegator)
                    .from("WorkEffortInventoryProduced")
                    .where(context)
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying WorkEffortInventoryProduced: " + e.getMessage(), MODULE);
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
     * test to create new event (workeffort) service
     */
    public static Map<String, Object> testCreateEventService(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        List<String> error_list = new LinkedList<>();

        Map<String, Object> updateEventMap = null;
        Debug.logInfo("====================Create an event test case==========================================", MODULE);
        Map<String, Object> createEventMap = new HashMap<String, Object>();
        createEventMap.put("workEffortTypeId", "EVENT");
        createEventMap.put("quickAssignPartyId", "DemoCustomer");
        createEventMap.put("workEffortName", "Create Work Effort");
        createEventMap.put("currentStatusId", "CAL_TENTATIVE");
        GenericValue createEventMap_userLogin = null;
        try {
            createEventMap_userLogin = EntityQuery.use(delegator)
                    .from("UserLogin")
                    .where(UtilMisc.toMap("userLoginId", "system"))
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying UserLogin: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        Map<String, Object> eventMap = new HashMap<String, Object>();
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("createWorkEffort", createEventMap);
            if (ServiceUtil.isError(serviceResult)) {
                return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
            }
            eventMap.put("workEffortId", serviceResult.get("workEffortId"));
        } catch (Exception e) {
            Debug.logError(e, "Error calling createWorkEffort: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        Map<String, Object> inlineResult = testUpdateEventService(dctx, context);
        if (ServiceUtil.isError(inlineResult)) {
            return inlineResult;
        }
        GenericValue workEffort = null;
        try {
            workEffort = EntityQuery.use(delegator)
                    .from("WorkEffort")
                    .where(UtilMisc.toMap("workEffortId", ((Map<String, Object>) eventMap).get("workEffortId")))
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying WorkEffort: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        assert !(UtilValidate.isEmpty(workEffort)) : "Assertion failed: not if-empty";
        assert java.util.Objects.equals(workEffort.get("workEffortId"), ((Map<String, Object>) eventMap).get("workEffortId")) : "Assertion failed: if-compare-field";
        assert java.util.Objects.equals(workEffort.get("workEffortTypeId"), ((Map<String, Object>) updateEventMap).get("workEffortTypeId")) : "Assertion failed: if-compare-field";
        assert java.util.Objects.equals(workEffort.get("workEffortName"), ((Map<String, Object>) updateEventMap).get("workEffortName")) : "Assertion failed: if-compare-field";
        assert java.util.Objects.equals(workEffort.get("currentStatusId"), ((Map<String, Object>) updateEventMap).get("currentStatusId")) : "Assertion failed: if-compare-field";
        if (!error_list.isEmpty()) {
            return ServiceUtil.returnError(error_list);
        }

        return result;
    }


    /**
     * test to update an event(workeffort) service
     */
    public static Map<String, Object> testUpdateEventService(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        List<String> error_list = new LinkedList<>();

        Debug.logInfo("====================Update an event test case==========================================", MODULE);
        Map<String, Object> updateEventMap = new HashMap<String, Object>();
        updateEventMap.put("workEffortId", ((Map<String, Object>) context.get("eventMap")).get("workEffortId"));
        updateEventMap.put("workEffortTypeId", "EVENT");
        updateEventMap.put("workEffortName", "Update an event");
        updateEventMap.put("currentStatusId", "CAL_ACCEPTED");
        GenericValue updateEventMap_userLogin = null;
        try {
            updateEventMap_userLogin = EntityQuery.use(delegator)
                    .from("UserLogin")
                    .where(UtilMisc.toMap("userLoginId", "system"))
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying UserLogin: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("updateWorkEffort", updateEventMap);
            if (ServiceUtil.isError(serviceResult)) {
                return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
            }
        } catch (Exception e) {
            Debug.logError(e, "Error calling updateWorkEffort: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        if (!error_list.isEmpty()) {
            return ServiceUtil.returnError(error_list);
        }

        return result;
    }


    /**
     * test to create new project(workeffort) service
     */
    public static Map<String, Object> testCreateProjectService(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        List<String> error_list = new LinkedList<>();

        Map<String, Object> updateProjectMap = null;
        Map<String, Object> workEffortNoteMap = null;
        Map<String, Object> createWorkEffortNoteMap = null;
        Debug.logInfo("====================Create a new project test case==========================================", MODULE);
        Map<String, Object> createProjectMap = new HashMap<String, Object>();
        createProjectMap.put("workEffortTypeId", "PROJECT");
        createProjectMap.put("quickAssignPartyId", "DemoCustomer");
        createProjectMap.put("workEffortName", "Create a project");
        createProjectMap.put("currentStatusId", "CAL_TENTATIVE");
        GenericValue createProjectMap_userLogin = null;
        try {
            createProjectMap_userLogin = EntityQuery.use(delegator)
                    .from("UserLogin")
                    .where(UtilMisc.toMap("userLoginId", "system"))
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying UserLogin: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        Map<String, Object> projectMap = new HashMap<String, Object>();
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("createWorkEffort", createProjectMap);
            if (ServiceUtil.isError(serviceResult)) {
                return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
            }
            projectMap.put("workEffortId", serviceResult.get("workEffortId"));
        } catch (Exception e) {
            Debug.logError(e, "Error calling createWorkEffort: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        Map<String, Object> inlineResult = testUpdateProjectService(dctx, context);
        if (ServiceUtil.isError(inlineResult)) {
            return inlineResult;
        }
        inlineResult = testCreateWorkEffortNoteService(dctx, context);
        if (ServiceUtil.isError(inlineResult)) {
            return inlineResult;
        }
        GenericValue workEffort = null;
        try {
            workEffort = EntityQuery.use(delegator)
                    .from("WorkEffort")
                    .where(UtilMisc.toMap("workEffortId", ((Map<String, Object>) projectMap).get("workEffortId")))
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying WorkEffort: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        GenericValue noteData = null;
        try {
            noteData = EntityQuery.use(delegator)
                    .from("NoteData")
                    .where(UtilMisc.toMap("noteId", ((Map<String, Object>) workEffortNoteMap).get("noteId")))
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying NoteData: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        assert !(UtilValidate.isEmpty(workEffort)) : "Assertion failed: not if-empty";
        assert java.util.Objects.equals(workEffort.get("workEffortId"), ((Map<String, Object>) projectMap).get("workEffortId")) : "Assertion failed: if-compare-field";
        assert java.util.Objects.equals(workEffort.get("workEffortTypeId"), ((Map<String, Object>) updateProjectMap).get("workEffortTypeId")) : "Assertion failed: if-compare-field";
        assert java.util.Objects.equals(workEffort.get("workEffortName"), ((Map<String, Object>) updateProjectMap).get("workEffortName")) : "Assertion failed: if-compare-field";
        assert java.util.Objects.equals(workEffort.get("currentStatusId"), ((Map<String, Object>) updateProjectMap).get("currentStatusId")) : "Assertion failed: if-compare-field";
        assert !(UtilValidate.isEmpty(noteData)) : "Assertion failed: not if-empty";
        assert java.util.Objects.equals(noteData.get("noteParty"), ((Map<String, Object>) createWorkEffortNoteMap).get("noteParty")) : "Assertion failed: if-compare-field";
        assert java.util.Objects.equals(noteData.get("noteInfo"), ((Map<String, Object>) createWorkEffortNoteMap).get("noteInfo")) : "Assertion failed: if-compare-field";
        if (!error_list.isEmpty()) {
            return ServiceUtil.returnError(error_list);
        }

        return result;
    }


    /**
     * test to update an project(workeffort) service
     */
    public static Map<String, Object> testUpdateProjectService(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        List<String> error_list = new LinkedList<>();

        Debug.logInfo("====================Update a project test case==========================================", MODULE);
        Map<String, Object> updateProjectMap = new HashMap<String, Object>();
        updateProjectMap.put("workEffortId", ((Map<String, Object>) context.get("projectMap")).get("workEffortId"));
        updateProjectMap.put("workEffortTypeId", "PROJECT");
        updateProjectMap.put("workEffortName", "Update a project");
        updateProjectMap.put("currentStatusId", "CAL_ACCEPTED");
        GenericValue updateProjectMap_userLogin = null;
        try {
            updateProjectMap_userLogin = EntityQuery.use(delegator)
                    .from("UserLogin")
                    .where(UtilMisc.toMap("userLoginId", "system"))
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying UserLogin: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("updateWorkEffort", updateProjectMap);
            if (ServiceUtil.isError(serviceResult)) {
                return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
            }
        } catch (Exception e) {
            Debug.logError(e, "Error calling updateWorkEffort: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        if (!error_list.isEmpty()) {
            return ServiceUtil.returnError(error_list);
        }

        return result;
    }


    /**
     * test to create new workeffort note service
     */
    public static Map<String, Object> testCreateWorkEffortNoteService(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        List<String> error_list = new LinkedList<>();

        Debug.logInfo("====================Create a work effort note test case==========================================", MODULE);
        Map<String, Object> createWorkEffortNoteMap = new HashMap<String, Object>();
        createWorkEffortNoteMap.put("workEffortId", ((Map<String, Object>) context.get("projectMap")).get("workEffortId"));
        createWorkEffortNoteMap.put("noteParty", "DemoCustomer");
        createWorkEffortNoteMap.put("noteInfo", "This is a note for party 'DemoCustomer'");
        GenericValue createWorkEffortNoteMap_userLogin = null;
        try {
            createWorkEffortNoteMap_userLogin = EntityQuery.use(delegator)
                    .from("UserLogin")
                    .where(UtilMisc.toMap("userLoginId", "system"))
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying UserLogin: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        Map<String, Object> workEffortNoteMap = new HashMap<String, Object>();
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("createWorkEffortNote", createWorkEffortNoteMap);
            if (ServiceUtil.isError(serviceResult)) {
                return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
            }
            workEffortNoteMap.put("noteId", serviceResult.get("noteId"));
        } catch (Exception e) {
            Debug.logError(e, "Error calling createWorkEffortNote: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        if (!error_list.isEmpty()) {
            return ServiceUtil.returnError(error_list);
        }

        return result;
    }


    /**
     * get the planned and estimated hours for a task and add to the highInfo map
     */
    public static Map<String, Object> getHours(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        Map<String, Object> highInfo = null;
        GenericValue timesheet = null;
        List<GenericValue> partyRates = null;
        Double originalActualHours = null;
        GenericValue partyRate = null;
        List<GenericValue> estimates = null;
        try {
            estimates = ((GenericValue) context.get("lowInfo")).getRelated("WorkEffortSkillStandard", null, null, false);
        } catch (Exception e) {
            Debug.logError(e, "Error getting related WorkEffortSkillStandard: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        if (UtilValidate.isNotEmpty(estimates)) {
            if (estimates != null) {
                for (GenericValue estimate : estimates) {
                    if (UtilValidate.isNotEmpty(estimate.get("estimatedDuration"))) {
                        if (UtilValidate.isNotEmpty(((Map<String, Object>) highInfo).get("plannedHours"))) {
                            ((Map<String, Object>) highInfo).put("plannedHours", (new BigDecimal(((Map<String, Object>) highInfo).get("plannedHours").toString())).doubleValue());
                        } else {
                            highInfo.put("plannedHours", estimate.get("estimatedDuration"));
                        }
                    }
                }
            }
        }
        List<GenericValue> actuals = null;
        try {
            actuals = ((GenericValue) context.get("lowInfo")).getRelated("TimeEntry", null, null, false);
        } catch (Exception e) {
            Debug.logError(e, "Error getting related TimeEntry: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        if (UtilValidate.isNotEmpty(actuals)) {
            if (actuals != null) {
                for (GenericValue actual : actuals) {
                    if (UtilValidate.isNotEmpty(actual.get("hours"))) {
                        try {
                            timesheet = actual.getRelatedOne("Timesheet", false);
                        } catch (Exception e) {
                            Debug.logError(e, "Error getting related one Timesheet: " + e.getMessage(), MODULE);
                            return ServiceUtil.returnError(e.getMessage());
                        }
                        originalActualHours = (Double) actual.get("hours");
                        try {
                            partyRates = EntityQuery.use(delegator)
                                    .from("PartyRate")
                                    .queryList();
                        } catch (Exception e) {
                            Debug.logError(e, "Error querying PartyRate: " + e.getMessage(), MODULE);
                            return ServiceUtil.returnError(e.getMessage());
                        }
                        if (UtilValidate.isNotEmpty(partyRates)) {
                            partyRate = EntityUtil.getFirst((List<GenericValue>) partyRates);
                            if (UtilValidate.isNotEmpty(partyRate.get("percentageUsed"))) {
                                actual.set("hours", (new BigDecimal(partyRate.get("percentageUsed").toString())).doubleValue());
                                actual.set("hours", (new BigDecimal(actual.get("hours").toString())).doubleValue());
                            }
                        }
                        Double highInfo_originalActualHours = null;
                        Double highInfo_actualHours = null;
                        Double highInfo_actualNonBilledHours = null;
                        if ((UtilValidate.isEmpty(context.get("hoursPartyId")) || (!(UtilValidate.isEmpty(context.get("hoursPartyId"))) && java.util.Objects.equals(timesheet.get("partyId"), context.get("hoursPartyId"))))) {
                            if (UtilValidate.isNotEmpty(((Map<String, Object>) highInfo).get("originalActualHours"))) {
                                ((Map<String, Object>) highInfo).put("originalActualHours", (new BigDecimal(((Map<String, Object>) highInfo).get("originalActualHours").toString())).doubleValue());
                            } else {
                                highInfo.put("originalActualHours", originalActualHours);
                            }
                            if (UtilValidate.isNotEmpty(((Map<String, Object>) highInfo).get("actualHours"))) {
                                ((Map<String, Object>) highInfo).put("actualHours", (new BigDecimal(((Map<String, Object>) highInfo).get("actualHours").toString())).doubleValue());
                            } else {
                                highInfo.put("actualHours", actual.get("hours"));
                            }
                            if (UtilValidate.isEmpty(actual.get("invoiceId"))) {
                                if (UtilValidate.isNotEmpty(((Map<String, Object>) highInfo).get("actualNonBilledHours"))) {
                                    ((Map<String, Object>) highInfo).put("actualNonBilledHours", (new BigDecimal(((Map<String, Object>) highInfo).get("actualNonBilledHours").toString())).doubleValue());
                                } else {
                                    highInfo.put("actualNonBilledHours", actual.get("hours"));
                                }
                            }
                        }
                        if (UtilValidate.isNotEmpty(((Map<String, Object>) highInfo).get("actualTotalHours"))) {
                            ((Map<String, Object>) highInfo).put("actualTotalHours", (new BigDecimal(((Map<String, Object>) highInfo).get("actualTotalHours").toString())).doubleValue());
                        } else {
                            highInfo.put("actualTotalHours", actual.get("hours"));
                        }
                        if (UtilValidate.isEmpty(actual.get("invoiceId"))) {
                            if (UtilValidate.isNotEmpty(((Map<String, Object>) highInfo).get("actualNonBilledTotalHours"))) {
                                ((Map<String, Object>) highInfo).put("actualNonBilledTotalHours", (new BigDecimal(((Map<String, Object>) highInfo).get("actualNonBilledTotalHours").toString())).doubleValue());
                            } else {
                                highInfo.put("actualNonBilledTotalHours", actual.get("hours"));
                            }
                        }
                    }
                }
            }
        }

        return result;
    }


    /**
     * Create WorkEffortSurvey
     */
    public static Map<String, Object> createWorkEffortSurveyAppl(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        GenericValue newEntity = null;
        Timestamp nowTimeStamp = null;
        newEntity = delegator.makeValue("WorkEffortSurveyAppl");
        newEntity.setPKFields(context);
        newEntity.setNonPKFields(context);
        if (UtilValidate.isEmpty(newEntity.get("fromDate"))) {
            nowTimeStamp = new Timestamp(System.currentTimeMillis());
            newEntity.put("fromDate", nowTimeStamp);
        }
        try {
            delegator.create(newEntity);
        } catch (Exception e) {
            Debug.logError(e, "Error creating value: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }

        return result;
    }


    /**
     * Update WorkEffortSurvey
     */
    public static Map<String, Object> updateWorkEffortSurveyAppl(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        GenericValue lookedUpValue = null;
        try {
            lookedUpValue = EntityQuery.use(delegator)
                    .from("WorkEffortSurveyAppl")
                    .where(context)
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying WorkEffortSurveyAppl: " + e.getMessage(), MODULE);
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
     * Delete Work Effort Survey
     */
    public static Map<String, Object> deleteWorkEffortSurveyAppl(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        GenericValue lookedUpValue = null;
        try {
            lookedUpValue = EntityQuery.use(delegator)
                    .from("WorkEffortSurveyAppl")
                    .where(context)
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying WorkEffortSurveyAppl: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        Timestamp nowTimestamp = new Timestamp(System.currentTimeMillis());
        lookedUpValue.put("thruDate", nowTimestamp);
        try {
            delegator.store(lookedUpValue);
        } catch (Exception e) {
            Debug.logError(e, "Error storing value: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }

        return result;
    }


    /**
     * Get All Work Efforts Related To An iCalendar Publish Point
     */
    public static Map<String, Object> getICalWorkEfforts(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        List<Object> workEfforts = null;
        List<GenericValue> resultList = null;
        Object workEffortId = context.get("workEffortId");
        List<GenericValue> assignedParties = null;
        try {
            assignedParties = EntityQuery.use(delegator)
                    .from("WorkEffortPartyAssignment")
                    .filterByDate()
                    .queryList();
        } catch (Exception e) {
            Debug.logError(e, "Error querying WorkEffortPartyAssignment: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        if (assignedParties != null) {
            for (GenericValue assignedParty : assignedParties) {
                try {
                    resultList = EntityQuery.use(delegator)
                            .from("WorkEffortAndPartyAssign")
                            .filterByDate()
                            .queryList();
                } catch (Exception e) {
                    Debug.logError(e, "Error querying WorkEffortAndPartyAssign: " + e.getMessage(), MODULE);
                    return ServiceUtil.returnError(e.getMessage());
                }
                workEfforts.addAll(resultList);
            }
        }
        List<GenericValue> assignedFixedAssets = null;
        try {
            assignedFixedAssets = EntityQuery.use(delegator)
                    .from("WorkEffortFixedAssetAssign")
                    .where(UtilMisc.toMap("workEffortId", workEffortId))
                    .filterByDate()
                    .queryList();
        } catch (Exception e) {
            Debug.logError(e, "Error querying entity: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        if (assignedFixedAssets != null) {
            for (GenericValue assignedFixedAsset : assignedFixedAssets) {
                try {
                    resultList = EntityQuery.use(delegator)
                            .from("WorkEffortAndFixedAssetAssign")
                            .filterByDate()
                            .queryList();
                } catch (Exception e) {
                    Debug.logError(e, "Error querying WorkEffortAndFixedAssetAssign: " + e.getMessage(), MODULE);
                    return ServiceUtil.returnError(e.getMessage());
                }
                workEfforts.addAll(resultList);
            }
        }
        try {
            resultList = EntityQuery.use(delegator)
                    .from("WorkEffortAssocToView")
                    .filterByDate()
                    .queryList();
        } catch (Exception e) {
            Debug.logError(e, "Error querying WorkEffortAssocToView: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        workEfforts.addAll(resultList);
        workEfforts = (List<Object>) ((Map<String, Object>) ((Map<String, Object>) context.get("groovy:org")).get("ofbiz")).get("workeffort.workeffort.WorkEffortWorker.removeDuplicateWorkEfforts(workEfforts);");
        result.put("workEfforts", workEfforts);

        return result;
    }


    /**
     * Get The Party iCalendar URL
     */
    public static Map<String, Object> getPartyICalUrl(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        Object partyId = context.get("partyId");
        List<GenericValue> contactMechs = null;
        try {
            contactMechs = EntityQuery.use(delegator)
                    .from("PartyContactWithPurpose")
                    .queryList();
        } catch (Exception e) {
            Debug.logError(e, "Error querying PartyContactWithPurpose: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        if (((Comparable) context.get("util:size(contactMechs)")).compareTo(0) > 0) {
            result.put("iCalUrl", ((GenericValue) ((List<?>) contactMechs).get(0)).get("infoString"));
        }

        return result;
    }

}
