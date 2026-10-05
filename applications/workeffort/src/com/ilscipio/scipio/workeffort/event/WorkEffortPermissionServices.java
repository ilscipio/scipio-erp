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
import org.ofbiz.security.Security;
import org.ofbiz.entity.util.EntityQuery;
import org.ofbiz.entity.util.EntityUtil;
import org.ofbiz.service.DispatchContext;
import org.ofbiz.service.LocalDispatcher;
import org.ofbiz.service.ServiceUtil;

/**
 * Auto-generated event class from Simple Method XML.
 *
 * <p>Generated from: component://workeffort/script/org/ofbiz/workeffort/permission/WorkEffortPermissionServices.xml</p>
 *
 * <p>SCIPIO: 4.0.0: Auto-generated.</p>
 */
public class WorkEffortPermissionServices {

    private static final String MODULE = WorkEffortPermissionServices.class.getName();


    /**
     * Check user has WorkEffort Manager permission
     */
    public static Map<String, Object> workEffortManagerPermission(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        Security security = dctx.getSecurity();
        String primaryPermission = "WORKEFFORTMGR";
        String mainAction = (String) context.get("mainAction");
        if (mainAction == null) {
            return ServiceUtil.returnError(UtilProperties.getMessage("CommonUiLabels", "CommonPermissionMainActionAttributeMissing", locale));
        }
        if (security.hasPermission(primaryPermission + "_" + mainAction, userLogin) || security.hasPermission(primaryPermission + "_ADMIN", userLogin)) {
            result.put("hasPermission", Boolean.TRUE);
        } else {
            result.put("hasPermission", Boolean.FALSE);
            result.put("failMessage", UtilProperties.getMessage("CommonUiLabels", "CommonGenericPermissionError", locale));
        }

        return result;
    }


    /**
     */
    public static Map<String, Object> workEffortGenericPermission(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        Object workEffortId = null;
        GenericValue workEffort = null;
        Object primaryPermission = null;
        Map<String, Object> inlineResult = null;
        Map<String, Object> lookupRoleWorkEffortMap = null;
        List<GenericValue> roleParties = null;
        List<GenericValue> emptyField = null;
        GenericValue workEffortParent = null;
        Boolean hasPermission = null;
        Map<String, Object> workEffortLookUpMap = null;
        String failMessage = null;
        List<GenericValue> rolePartyGroups = null;
        Map<String, Object> lookupPartyRoleMap = null;
        Map<String, Object> lookupPartyRoleWorkEffortMap = null;
        GenericValue rolePartyGroup = null;
        List<GenericValue> partyGroupRelationships = null;
        Security security = dctx.getSecurity();
        primaryPermission = "WORKEFFORTMGR";
        String mainAction = (String) context.get("mainAction");
        if (mainAction == null) {
            return ServiceUtil.returnError(UtilProperties.getMessage("CommonUiLabels", "CommonPermissionMainActionAttributeMissing", locale));
        }
        if (security.hasPermission((String) primaryPermission + "_" + mainAction, userLogin) || security.hasPermission((String) primaryPermission + "_ADMIN", userLogin)) {
            hasPermission = Boolean.TRUE;
            result.put("hasPermission", Boolean.TRUE);
        } else {
            hasPermission = Boolean.FALSE;
            failMessage = UtilProperties.getMessage("CommonUiLabels", "CommonGenericPermissionError", locale);
            result.put("hasPermission", Boolean.FALSE);
            result.put("failMessage", failMessage);
        }
        if (!("true".equals(hasPermission))) {
            Debug.logInfo("The user does not have WORKEFFORTMGR permission", MODULE);
            primaryPermission = "WORKEFFORTMGR_ROLE";
            if (security.hasPermission((String) primaryPermission + "_" + mainAction, userLogin) || security.hasPermission((String) primaryPermission + "_ADMIN", userLogin)) {
                hasPermission = Boolean.TRUE;
                result.put("hasPermission", Boolean.TRUE);
            } else {
                hasPermission = Boolean.FALSE;
                failMessage = UtilProperties.getMessage("CommonUiLabels", "CommonGenericPermissionError", locale);
                result.put("hasPermission", Boolean.FALSE);
                result.put("failMessage", failMessage);
            }
            if ("true".equals(hasPermission)) {
                Debug.logInfo("User has ROLE permission, now checking if user is in required ROLE ", MODULE);
                if (("CREATE".equals(context.get("mainAction")) && !(UtilValidate.isEmpty(context.get("workEffortParentId"))))) {
                    workEffortId = context.get("workEffortParentId");
                    inlineResult = workEffortPartyAnyRolePermission(dctx, context);
                    if (ServiceUtil.isError(inlineResult)) {
                        return inlineResult;
                    }
                } else if ("UPDATE".equals(context.get("mainAction"))) {
                    workEffortId = context.get("workEffortId");
                    inlineResult = workEffortPartyOwnerRolePermission(dctx, context);
                    if (ServiceUtil.isError(inlineResult)) {
                        return inlineResult;
                    }
                    try {
                        workEffort = EntityQuery.use(delegator)
                                .from("WorkEffort")
                                .where(UtilMisc.toMap("workEffortId", context.get("workEffortId")))
                                .queryOne();
                    } catch (Exception e) {
                        Debug.logError(e, "Error querying WorkEffort: " + e.getMessage(), MODULE);
                        return ServiceUtil.returnError(e.getMessage());
                    }
                    if (("true".equals(hasPermission) && !(UtilValidate.isEmpty(context.get("workEffortParentId"))) && !java.util.Objects.equals(context.get("workEffortParentId"), workEffort.get("workEffortParentId")))) {
                        Debug.logInfo(" User is in Cal Owner role and can update, Now checking if user has access to parent workeffort ", MODULE);
                        workEffortId = context.get("workEffortParentId");
                        inlineResult = workEffortPartyOwnerRolePermission(dctx, context);
                        if (ServiceUtil.isError(inlineResult)) {
                            return inlineResult;
                        }
                    }
                    if (!("true".equals(hasPermission))) {
                        Debug.logInfo(" User does not have Direct access to this workeffort checking if its member of PartyGroup that has required permission ", MODULE);
                        workEffortId = context.get("workEffortId");
                        inlineResult = workEffortPartyGroupRolePermission(dctx, context);
                        if (ServiceUtil.isError(inlineResult)) {
                            return inlineResult;
                        }
                        if (("true".equals(hasPermission) && !(UtilValidate.isEmpty(context.get("workEffortParentId"))) && !java.util.Objects.equals(context.get("workEffortParentId"), workEffort.get("workEffortParentId")))) {
                            workEffortId = context.get("workEffortParentId");
                            inlineResult = workEffortPartyGroupRolePermission(dctx, context);
                            if (ServiceUtil.isError(inlineResult)) {
                                return inlineResult;
                            }
                        }
                    }
                }
            }
        }

        return result;
    }


    /**
     * Check if Party is in CAL_OWNER or CAL_DELEGATE role with WorkEffort
     */
    public static Map<String, Object> workEffortPartyOwnerRolePermission(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        Object roleTypeId = context.get("roleTypeId");
        Object workEffortId = null;
        Map<String, Object> lookupRoleWorkEffortMap = null;
        List<GenericValue> roleParties = null;
        List<GenericValue> emptyField = null;
        GenericValue workEffortParent = null;
        Boolean hasPermission = null;
        Map<String, Object> workEffortLookUpMap = null;
        String failMessage = null;
        if (UtilValidate.isEmpty(workEffortId)) {
            workEffortId = context.get("workEffortParentId");
        }
        while (!(UtilValidate.isEmpty(workEffortId))) {
            lookupRoleWorkEffortMap.put("workEffortId", workEffortId);
            lookupRoleWorkEffortMap.put("partyId", userLogin.get("partyId"));
            lookupRoleWorkEffortMap.put("roleTypeId", "CAL_OWNER");
            Debug.logInfo("Running find-by-and: " + lookupRoleWorkEffortMap, MODULE);
            try {
                roleParties = EntityQuery.use(delegator)
                        .from("WorkEffortPartyAssignment")
                        .where(lookupRoleWorkEffortMap)
                        .queryList();
            } catch (Exception e) {
                Debug.logError(e, "Error querying WorkEffortPartyAssignment: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
            emptyField = EntityUtil.filterByDate(UtilGenerics.cast(roleParties));
            Debug.logInfo("Found role parties: " + roleParties, MODULE);
            if (UtilValidate.isEmpty(roleParties)) {
                Debug.logInfo("Party " + userLogin.get("partyId") + " is not in " + roleTypeId + " role with workEffort: " + workEffortId, MODULE);
                lookupRoleWorkEffortMap.put("roleTypeId", "CAL_DELEGATE");
                try {
                    roleParties = EntityQuery.use(delegator)
                            .from("WorkEffortPartyAssignment")
                            .where(lookupRoleWorkEffortMap)
                            .queryList();
                } catch (Exception e) {
                    Debug.logError(e, "Error querying WorkEffortPartyAssignment: " + e.getMessage(), MODULE);
                    return ServiceUtil.returnError(e.getMessage());
                }
            }
            emptyField = EntityUtil.filterByDate(UtilGenerics.cast(roleParties));
            if (UtilValidate.isNotEmpty(roleParties)) {
                hasPermission = Boolean.TRUE;
                result.put("hasPermission", hasPermission);
                Debug.logInfo("Party " + userLogin.get("partyId") + " is in " + ((Map<String, Object>) lookupRoleWorkEffortMap).get("roleTypeId") + " role with workEffort: " + workEffortId, MODULE);
                workEffortId = null;
            } else {
                Debug.logInfo("Party " + userLogin.get("partyId") + " is not in " + roleTypeId + " role with workEffort: " + workEffortId, MODULE);
                failMessage = UtilProperties.getMessage("WorkEffortUiLabels", "WorkEffortNotInRolePermissionError", locale);
                hasPermission = Boolean.FALSE;
                result.put("hasPermission", hasPermission);
                result.put("failMessage", failMessage);
                workEffortLookUpMap.put("workEffortId", workEffortId);
                try {
                    workEffortParent = EntityQuery.use(delegator)
                            .from("WorkEffort")
                            .where(workEffortLookUpMap)
                            .queryOne();
                } catch (Exception e) {
                    Debug.logError(e, "Error finding by primary key WorkEffort: " + e.getMessage(), MODULE);
                    return ServiceUtil.returnError(e.getMessage());
                }
                workEffortId = workEffortParent.get("workEffortParentId");
                if (UtilValidate.isEmpty(workEffortParent.get("workEffortParentId"))) {
                    workEffortId = null;
                }
            }
        }

        return result;
    }


    /**
     * Check if Party is in ANY role with WorkEffort
     */
    public static Map<String, Object> workEffortPartyAnyRolePermission(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        Object workEffortId = null;
        Map<String, Object> lookupRoleWorkEffortMap = null;
        List<GenericValue> roleParties = null;
        List<GenericValue> emptyField = null;
        GenericValue workEffortParent = null;
        Boolean hasPermission = null;
        Map<String, Object> workEffortLookUpMap = null;
        String failMessage = null;
        if (UtilValidate.isEmpty(workEffortId)) {
            workEffortId = context.get("workEffortParentId");
        }
        while (!(UtilValidate.isEmpty(workEffortId))) {
            lookupRoleWorkEffortMap.put("workEffortId", workEffortId);
            lookupRoleWorkEffortMap.put("partyId", userLogin.get("partyId"));
            try {
                roleParties = EntityQuery.use(delegator)
                        .from("WorkEffortPartyAssignment")
                        .where(lookupRoleWorkEffortMap)
                        .queryList();
            } catch (Exception e) {
                Debug.logError(e, "Error querying WorkEffortPartyAssignment: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
            emptyField = EntityUtil.filterByDate(UtilGenerics.cast(roleParties));
            if (UtilValidate.isNotEmpty(roleParties)) {
                hasPermission = Boolean.TRUE;
                result.put("hasPermission", hasPermission);
                Debug.logInfo("Party " + userLogin.get("partyId") + " is associated with workEffort: " + workEffortId, MODULE);
                workEffortId = null;
            } else {
                Debug.logInfo("Party " + userLogin.get("partyId") + " is not associated with workEffort: " + workEffortId, MODULE);
                failMessage = UtilProperties.getMessage("WorkEffortUiLabels", "WorkEffortNotInRolePermissionError", locale);
                hasPermission = Boolean.FALSE;
                result.put("hasPermission", hasPermission);
                result.put("failMessage", failMessage);
                workEffortLookUpMap.put("workEffortId", workEffortId);
                try {
                    workEffortParent = EntityQuery.use(delegator)
                            .from("WorkEffort")
                            .where(workEffortLookUpMap)
                            .queryOne();
                } catch (Exception e) {
                    Debug.logError(e, "Error finding by primary key WorkEffort: " + e.getMessage(), MODULE);
                    return ServiceUtil.returnError(e.getMessage());
                }
                workEffortId = workEffortParent.get("workEffortParentId");
                if (UtilValidate.isEmpty(workEffortParent.get("workEffortParentId"))) {
                    workEffortId = null;
                }
            }
        }

        return result;
    }


    /**
     * Check Permission to Update Timesheet
     */
    public static Map<String, Object> timesheetUpdatePermission(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        List<String> error_list = new LinkedList<>();

        Boolean hasPermission = null;
        String failMessage = null;
        Map<String, Object> lookupRoleWorkEffortMap = null;
        List<GenericValue> roleParties = null;
        List<GenericValue> emptyField = null;
        Object workEffortId = null;
        GenericValue workEffort = null;
        Object primaryPermission = null;
        Map<String, Object> inlineResult = null;
        context.put("mainAction", "UPDATE");
        inlineResult = workEffortGenericPermission(dctx, context);
        if (ServiceUtil.isError(inlineResult)) {
            return inlineResult;
        }
        if (!error_list.isEmpty()) {
            return ServiceUtil.returnError(error_list);
        }
        if (!java.util.Objects.equals(context.get("partyId"), userLogin.get("partyId"))) {
            failMessage = UtilProperties.getMessage("WorkEffortUiLabels", "WorkEffortTimesheetNotInRolePermissionError", locale);
            hasPermission = Boolean.FALSE;
            result.put("hasPermission", hasPermission);
            result.put("failMessage", failMessage);
        }
        if (UtilValidate.isNotEmpty(workEffortId)) {
            lookupRoleWorkEffortMap.put("workEffortId", workEffortId);
            lookupRoleWorkEffortMap.put("partyId", userLogin.get("partyId"));
            try {
                roleParties = EntityQuery.use(delegator)
                        .from("WorkEffortPartyAssignByRole")
                        .where(lookupRoleWorkEffortMap)
                        .queryList();
            } catch (Exception e) {
                Debug.logError(e, "Error querying WorkEffortPartyAssignByRole: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
            emptyField = EntityUtil.filterByDate(UtilGenerics.cast(roleParties));
            if (UtilValidate.isEmpty(roleParties)) {
                failMessage = UtilProperties.getMessage("WorkEffortUiLabels", "WorkEffortTimesheetNotInRolePermissionError", locale);
                hasPermission = Boolean.FALSE;
                result.put("hasPermission", hasPermission);
                result.put("failMessage", failMessage);
            }
        }

        return result;
    }


    /**
     * Check if Party is party member of PartyGroup that is in CAL_OWNER or CAL_DELEGATE role with WorkEffort
     */
    public static Map<String, Object> workEffortPartyGroupRolePermission(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        Object workEffortId = null;
        Map<String, Object> lookupRoleWorkEffortMap = null;
        List<GenericValue> emptyField = null;
        GenericValue workEffortParent = null;
        Boolean hasPermission = null;
        Map<String, Object> workEffortLookUpMap = null;
        List<GenericValue> rolePartyGroups = null;
        Map<String, Object> lookupPartyRoleMap = null;
        Map<String, Object> lookupPartyRoleWorkEffortMap = null;
        List<GenericValue> partyGroupRelationships = null;
        String failMessage = null;
        if (UtilValidate.isEmpty(workEffortId)) {
            workEffortId = context.get("workEffortParentId");
        }
        while (!(UtilValidate.isEmpty(workEffortId))) {
            lookupPartyRoleWorkEffortMap.put("workEffortId", workEffortId);
            lookupPartyRoleWorkEffortMap.put("roleTypeId", "CAL_OWNER");
            lookupPartyRoleWorkEffortMap.put("partyTypeId", "PARTY_GROUP");
            Debug.logInfo("Running find-by-and: " + lookupPartyRoleWorkEffortMap, MODULE);
            try {
                rolePartyGroups = EntityQuery.use(delegator)
                        .from("WorkEffortPartyAssignView")
                        .where(lookupPartyRoleWorkEffortMap)
                        .queryList();
            } catch (Exception e) {
                Debug.logError(e, "Error querying WorkEffortPartyAssignView: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
            emptyField = EntityUtil.filterByDate(UtilGenerics.cast(rolePartyGroups));
            Debug.logInfo("Found role parties Group: " + rolePartyGroups, MODULE);
            if (UtilValidate.isEmpty(rolePartyGroups)) {
                Debug.logInfo("No Party Group found in CAL_OWNER role with workEffort: " + workEffortId, MODULE);
                lookupRoleWorkEffortMap.put("roleTypeId", "CAL_DELEGATE");
                try {
                    rolePartyGroups = EntityQuery.use(delegator)
                            .from("WorkEffortPartyAssignView")
                            .where(lookupRoleWorkEffortMap)
                            .queryList();
                } catch (Exception e) {
                    Debug.logError(e, "Error querying WorkEffortPartyAssignView: " + e.getMessage(), MODULE);
                    return ServiceUtil.returnError(e.getMessage());
                }
            }
            emptyField = EntityUtil.filterByDate(UtilGenerics.cast(rolePartyGroups));
            GenericValue rolePartyGroup = null;
            if (UtilValidate.isNotEmpty(rolePartyGroups)) {
                if (rolePartyGroups != null) {
                    for (GenericValue rolePartyGroupEntry : rolePartyGroups) {
                        lookupPartyRoleMap.put("partyIdFrom", rolePartyGroupEntry.get("partyId"));
                        lookupPartyRoleMap.put("partyIdTo", userLogin.get("partyId"));
                        Debug.logInfo("Conditions: " + lookupPartyRoleMap, MODULE);
                        try {
                            partyGroupRelationships = EntityQuery.use(delegator)
                                    .from("PartyRelationship")
                                    .where(lookupPartyRoleMap)
                                    .queryList();
                        } catch (Exception e) {
                            Debug.logError(e, "Error querying PartyRelationship: " + e.getMessage(), MODULE);
                            return ServiceUtil.returnError(e.getMessage());
                        }
                        Debug.logInfo("Found role parties relations: " + partyGroupRelationships, MODULE);
                        if (UtilValidate.isNotEmpty(partyGroupRelationships)) {
                            hasPermission = Boolean.TRUE;
                            result.put("hasPermission", hasPermission);
                            Debug.logInfo("Party " + userLogin.get("partyId") + " is associated with workEffort: " + workEffortId, MODULE);
                        }
                    }
                }
                workEffortId = null;
            } else {
                Debug.logInfo("Party " + userLogin.get("partyId") + " is not associated with workEffort: " + workEffortId, MODULE);
                failMessage = UtilProperties.getMessage("WorkEffortUiLabels", "WorkEffortNotInRolePermissionError", locale);
                hasPermission = Boolean.FALSE;
                result.put("hasPermission", hasPermission);
                result.put("failMessage", failMessage);
                workEffortLookUpMap.put("workEffortId", workEffortId);
                try {
                    workEffortParent = EntityQuery.use(delegator)
                            .from("WorkEffort")
                            .where(workEffortLookUpMap)
                            .queryOne();
                } catch (Exception e) {
                    Debug.logError(e, "Error finding by primary key WorkEffort: " + e.getMessage(), MODULE);
                    return ServiceUtil.returnError(e.getMessage());
                }
                workEffortId = workEffortParent.get("workEffortParentId");
                if (UtilValidate.isEmpty(workEffortParent.get("workEffortParentId"))) {
                    workEffortId = null;
                }
            }
        }

        return result;
    }


    /**
     * Check iCalendar Permission
     */
    public static Map<String, Object> workEffortICalendarPermission(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        Object workEffortId = null;
        GenericValue workEffort = null;
        Object isDelegate = null;
        Object isOrganizer = null;
        Object isOwner = null;
        Boolean hasPermission = null;
        List<GenericValue> partyAssignments = null;
        Object primaryPermission = null;
        Map<String, Object> inlineResult = null;
        Debug.logVerbose("workEffortICalendarPermission invoked for workEffortId " + context.get("workEffortId") + ",             user login partyId = " + userLogin.get("partyId"), MODULE);
        inlineResult = workEffortManagerPermission(dctx, context);
        if (ServiceUtil.isError(inlineResult)) {
            return inlineResult;
        }
        if (("true".equals(hasPermission) && !(UtilValidate.isEmpty(context.get("workEffortId"))))) {
            hasPermission = Boolean.FALSE;
            workEffortId = context.get("workEffortId");
            try {
                workEffort = EntityQuery.use(delegator)
                        .from("WorkEffort")
                        .where(context)
                        .queryOne();
            } catch (Exception e) {
                Debug.logError(e, "Error querying WorkEffort: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
            if (UtilValidate.isNotEmpty(workEffort)) {
                try {
                    partyAssignments = EntityQuery.use(delegator)
                            .from("WorkEffortPartyAssignment")
                            .filterByDate()
                            .queryList();
                } catch (Exception e) {
                    Debug.logError(e, "Error querying WorkEffortPartyAssignment: " + e.getMessage(), MODULE);
                    return ServiceUtil.returnError(e.getMessage());
                }
                isDelegate = "false";
                isOwner = "false";
                isOrganizer = "false";
                if (partyAssignments != null) {
                    for (GenericValue partyAssignment : partyAssignments) {
                        if ("CAL_OWNER".equals(partyAssignment.get("roleTypeId"))) {
                            isDelegate = "true";
                            isOwner = "true";
                            isOrganizer = "true";
                        } else {
                            if ("CAL_ORGANIZER".equals(partyAssignment.get("roleTypeId"))) {
                                isOrganizer = "true";
                            } else {
                                if ("CAL_DELEGATE".equals(partyAssignment.get("roleTypeId"))) {
                                    isDelegate = "true";
                                }
                            }
                        }
                    }
                }
                if ("PUBLISH_PROPS".equals(workEffort.get("workEffortTypeId"))) {
                    Debug.logVerbose("Checking publish properties permission, isOwner = " + isOwner + ", isDelegate = " + isDelegate, MODULE);
                    if (("true".equals(isOwner) || ("WES_CONFIDENTIAL".equals(workEffort.get("scopeEnumId")) && "true".equals(isDelegate)))) {
                        hasPermission = Boolean.TRUE;
                    }
                } else {
                    Debug.logVerbose("Checking work effort update permission, isOrganizer = " + isOrganizer, MODULE);
                    if ("true".equals(isOrganizer)) {
                        hasPermission = Boolean.TRUE;
                    }
                }
            }
            result.put("hasPermission", hasPermission);
        }

        return result;
    }

}
