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
package com.ilscipio.scipio.commonext.event;

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
import org.ofbiz.entity.condition.EntityCondition;
import org.ofbiz.entity.util.EntityQuery;
import org.ofbiz.entity.util.EntityUtil;
import org.ofbiz.service.DispatchContext;
import org.ofbiz.service.LocalDispatcher;
import org.ofbiz.service.ServiceUtil;

/**
 * Auto-generated event class from Simple Method XML.
 *
 * <p>Generated from: component://commonext/script/org/ofbiz/SystemInfoServices.xml</p>
 *
 * <p>SCIPIO: 4.0.0: Auto-generated.</p>
 */
public class SystemInfoServices {

    private static final String MODULE = SystemInfoServices.class.getName();


    /**
     * Create a system to to a specific party
     */
    public static Map<String, Object> createSystemInfoNote(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        context.put("partyId", context.get("partyId"));
        GenericValue noteData = delegator.makeValue("NoteData");
        noteData.setNonPKFields(context);
        Timestamp noteData_noteDateTime = new Timestamp(System.currentTimeMillis());
        ((GenericValue) noteData).put("noteId", delegator.getNextSeqId("NoteData"));
        noteData.put("noteName", "SYSTEMNOTE");
        try {
            delegator.create(noteData);
        } catch (Exception e) {
            Debug.logError(e, "Error creating value: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }

        return result;
    }


    /**
     * get the attributes of the SystemInfoNotes portlet for a userlogin
     */
    public static Map<String, Object> getPortletAttributeMap(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        Object attributeMap = null;
        Object haveUserLogin = null;
        Map<String, Object> paMap = null;
        List<GenericValue> ulList = null;
        try {
            ulList = EntityQuery.use(delegator)
                    .from("UserLogin")
                    .where(UtilMisc.toMap("partyId", context.get("partyId")))
                    .queryList();
        } catch (Exception e) {
            Debug.logError(e, "Error querying entity: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        if (UtilValidate.isNotEmpty(ulList)) {
            haveUserLogin = "true";
            userLogin = EntityUtil.getFirst((List<GenericValue>) ulList);
            paMap.put("ownerUserLoginId", userLogin.get("userLoginId"));
            paMap.put("portalPortletId", "SystemInfoNotes");
            try {
                Map<String, Object> serviceResult = dispatcher.runSync("getPortletAttributes", paMap);
                if (ServiceUtil.isError(serviceResult)) {
                    return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
                }
                attributeMap = serviceResult.get("attributeMap");
            } catch (Exception e) {
                Debug.logError(e, "Error calling getPortletAttributes: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
        }

        return result;
    }


    /**
     * Delete SystemInfo Note
     */
    public static Map<String, Object> deleteSystemInfoNote(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        GenericValue noteData = null;
        try {
            noteData = EntityQuery.use(delegator)
                    .from("NoteData")
                    .where(context)
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying NoteData: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        try {
            noteData.removeRelated("CustRequestItemNote");
        } catch (Exception e) {
            Debug.logError(e, "Error removing related CustRequestItemNote: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        try {
            noteData.removeRelated("CustRequestNote");
        } catch (Exception e) {
            Debug.logError(e, "Error removing related CustRequestNote: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        try {
            noteData.removeRelated("MarketingCampaignNote");
        } catch (Exception e) {
            Debug.logError(e, "Error removing related MarketingCampaignNote: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        try {
            noteData.removeRelated("OrderHeaderNote");
        } catch (Exception e) {
            Debug.logError(e, "Error removing related OrderHeaderNote: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        try {
            noteData.removeRelated("PartyNote");
        } catch (Exception e) {
            Debug.logError(e, "Error removing related PartyNote: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        try {
            noteData.removeRelated("QuoteNote");
        } catch (Exception e) {
            Debug.logError(e, "Error removing related QuoteNote: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        try {
            noteData.removeRelated("WorkEffortNote");
        } catch (Exception e) {
            Debug.logError(e, "Error removing related WorkEffortNote: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        try {
            delegator.removeValue(noteData);
        } catch (Exception e) {
            Debug.logError(e, "Error removing value: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }

        return result;
    }


    /**
     * delete all system notes from a particular user
     */
    public static Map<String, Object> deleteAllSystemNotes(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        Map<String, Object> removeData = new HashMap<String, Object>();
        removeData.put("noteParty", ((GenericValue) context.get("userLogin")).getString("partyId"));
        removeData.put("noteName", "SYSTEMNOTE");
        try {
            delegator.removeByAnd("NoteData", removeData);
        } catch (Exception e) {
            Debug.logError(e, "Error removing NoteData: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }

        return result;
    }


    /**
     */
    public static Map<String, Object> getSystemInfoStatus(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        GenericValue comm = null;
        List<Object> systemInfoStatus = null;
        Map<String, Object> status = null;
        GenericValue assign = null;
        Long comCount = null;
        try {
            comCount = EntityQuery.use(delegator)
                    .from("CommunicationEventRole")
                    .queryCount();
        } catch (Exception e) {
            Debug.logError(e, "Error counting CommunicationEventRole: " + e.getMessage(), MODULE);
        }
        List<GenericValue> comms = null;
        try {
            comms = EntityQuery.use(delegator)
                    .from("CommunicationEventAndRole")
                    .queryList();
        } catch (Exception e) {
            Debug.logError(e, "Error querying CommunicationEventAndRole: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        if (((Comparable) comCount).compareTo(0L) > 0) {
            status.put("noteInfo", "Open communication events: " + comCount);
            comm = EntityUtil.getFirst((List<GenericValue>) comms);
            status.put("noteDateTime", comm.get("entryDate"));
            systemInfoStatus.add(status);
            status = new HashMap<String, Object>();
        }
        List<GenericValue> assigns = null;
        try {
            assigns = EntityQuery.use(delegator)
                    .from("WorkEffortAndPartyAssign")
                    .where(UtilMisc.toMap("partyId", ((GenericValue) context.get("userLogin")).getString("partyId"), "statusId", "PAS_ASSIGNED", "workEffortTypeId", "TASK"))
                    .filterByDate()
                    .queryList();
        } catch (Exception e) {
            Debug.logError(e, "Error querying entity: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        Long assignCount = null;
        try {
            assignCount = EntityQuery.use(delegator)
                    .from("WorkEffortAndPartyAssign")
                    .queryCount();
        } catch (Exception e) {
            Debug.logError(e, "Error counting WorkEffortAndPartyAssign: " + e.getMessage(), MODULE);
        }
        if (((Comparable) assignCount).compareTo(0L) > 0) {
            status.put("noteInfo", "Assigned and not completed tasks: " + assignCount);
            assign = EntityUtil.getFirst((List<GenericValue>) assigns);
            status.put("noteDateTime", assign.get("fromDate"));
            systemInfoStatus.add(status);
        }
        if (UtilValidate.isNotEmpty(systemInfoStatus)) {
            result.put("systemInfoStatus", systemInfoStatus);
        }

        return result;
    }


    /**
     */
    public static Map<String, Object> getSystemInfoNotes(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        List<GenericValue> systemInfoNotes = null;
        Integer viewSize = (Integer) context.get("viewSize");
        Integer viewIndex = (Integer) context.get("viewIndex");
        if ("false".equals(context.get("showAll"))) {
            try {
                systemInfoNotes = EntityQuery.use(delegator)
                        .from("NoteData")
                        .where(UtilMisc.toMap("noteParty", ((GenericValue) context.get("userLogin")).getString("partyId"), "noteName", "SYSTEMNOTE"))
                        .queryList();
            } catch (Exception e) {
                Debug.logError(e, "Error querying entity: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
        } else {
            try {
                systemInfoNotes = EntityQuery.use(delegator)
                        .from("NoteData")
                        .where(UtilMisc.toMap("noteParty", ((GenericValue) context.get("userLogin")).getString("partyId"), "noteName", "SYSTEMNOTE"))
                        .queryList();
            } catch (Exception e) {
                Debug.logError(e, "Error querying entity: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
        }
        if (UtilValidate.isNotEmpty(systemInfoNotes)) {
            result.put("systemInfoNotes", systemInfoNotes);
        }

        return result;
    }


    /**
     * Get the last 3 system info notes (SCIPIO: DEPRECATED)
     */
    public static Map<String, Object> getLastSystemInfoNote(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        List<GenericValue> systemInfoNotes = null;
        Object lastSystemInfoNote3 = null;
        Object lastSystemInfoNote2 = null;
        Object lastSystemInfoNote1 = null;
        if (UtilValidate.isNotEmpty(context.get("userLogin"))) {
            try {
                systemInfoNotes = EntityQuery.use(delegator)
                        .from("NoteData")
                        .where(UtilMisc.toMap("noteParty", ((GenericValue) context.get("userLogin")).getString("partyId"), "noteName", "SYSTEMNOTE"))
                        .queryList();
            } catch (Exception e) {
                Debug.logError(e, "Error querying entity: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
        } else {
            try {
                systemInfoNotes = EntityQuery.use(delegator)
                        .from("NoteData")
                        .where(UtilMisc.toMap("noteParty", "_NA_", "noteName", "SYSTEMNOTE"))
                        .queryList();
            } catch (Exception e) {
                Debug.logError(e, "Error querying entity: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
        }
        if (UtilValidate.isNotEmpty(systemInfoNotes)) {
            lastSystemInfoNote1 = ((List<?>) systemInfoNotes).get(0);
            result.put("lastSystemInfoNote1", lastSystemInfoNote1);
            if (UtilValidate.isNotEmpty(((List<?>) systemInfoNotes).get(2))) {
                lastSystemInfoNote2 = ((List<?>) systemInfoNotes).get(1);
                result.put("lastSystemInfoNote2", lastSystemInfoNote2);
            }
            if (UtilValidate.isNotEmpty(((List<?>) systemInfoNotes).get(3))) {
                lastSystemInfoNote3 = ((List<?>) systemInfoNotes).get(2);
                result.put("lastSystemInfoNote3", lastSystemInfoNote3);
            }
        }

        return result;
    }


    /**
     * Fetches System notifications/messages from the database
     */
    public static Map<String, Object> getSystemMessages(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        List<GenericValue> messages = null;
        Integer viewSize = (Integer) context.get("viewSize");
        Integer viewIndex = (Integer) context.get("viewIndex");
        Boolean showAll = (Boolean) context.get("showAll");
        String toPartyId = (String) context.get("toPartyId");
        Long count = null;
        try {
            count = EntityQuery.use(delegator)
                    .from("SystemMessages")
                    .queryCount();
        } catch (Exception e) {
            Debug.logError(e, "Error counting SystemMessages: " + e.getMessage(), MODULE);
        }
        if ("false".equals(showAll)) {
            try {
                messages = EntityQuery.use(delegator)
                        .from("SystemMessages")
                        .where(UtilMisc.toMap("toPartyId", toPartyId))
                        .queryList();
            } catch (Exception e) {
                Debug.logError(e, "Error querying entity: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
        } else {
            try {
                messages = EntityQuery.use(delegator)
                        .from("SystemMessages")
                        .where(UtilMisc.toMap("toPartyId", toPartyId))
                        .queryList();
            } catch (Exception e) {
                Debug.logError(e, "Error querying entity: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
        }
        if (UtilValidate.isNotEmpty(messages)) {
            result.put("messages", messages);
            result.put("count", count);
        }

        return result;
    }


    /**
     * Creates a new system message from a notedata entry
     */
    public static Map<String, Object> convertSystemMessageFromNoteData(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        GenericValue systemMessage = null;
        systemMessage = delegator.makeValue("SystemMessages");
        ((GenericValue) systemMessage).put("messageId", delegator.getNextSeqId("SystemMessages"));
        systemMessage.put("fromPartyId", context.get("system"));
        systemMessage.put("toPartyId", context.get("noteParty"));
        systemMessage.put("title", context.get("noteName"));
        systemMessage.put("description", context.get("noteInfo"));
        if (UtilValidate.isNotEmpty(context.get("moreInfoUrl"))) {
            systemMessage.put("url", context.get("moreInfoUrl"));
            if (UtilValidate.isNotEmpty(context.get("moreInfoItemName"))) {
                systemMessage.put("url", "" + systemMessage.get("url") + "?" + context.get("moreInfoItemName") + "=" + context.get("moreInfoItemId"));
            }
        }
        systemMessage.put("createdStamp", context.get("noteDateTime"));
        systemMessage.put("isRead", "N");
        try {
            delegator.create(systemMessage);
        } catch (Exception e) {
            Debug.logError(e, "Error creating value: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }

        return result;
    }

}
