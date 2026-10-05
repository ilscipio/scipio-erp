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
package com.ilscipio.scipio.common.event;

import java.math.BigDecimal;
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
import org.ofbiz.security.Security;
import org.ofbiz.service.DispatchContext;
import org.ofbiz.service.LocalDispatcher;
import org.ofbiz.service.ServiceUtil;

/**
 * Auto-generated event class from Simple Method XML.
 *
 * <p>Generated from: component://common/script/org/ofbiz/common/PortalPageServices.xml</p>
 *
 * <p>SCIPIO: 4.0.0: Auto-generated.</p>
 */
public class PortalPageServices {

    private static final String MODULE = PortalPageServices.class.getName();


    /**
     * Moves a PortalPortlet from the actual portalPage to a different one
     */
    public static Map<String, Object> movePortletToPortalPage(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        GenericValue portalPage = null;
        Map<String, Object> inlineResult = checkOwnerShip(dctx, context);
        if (ServiceUtil.isError(inlineResult)) {
            return inlineResult;
        }
        GenericValue oldEntity = null;
        try {
            oldEntity = EntityQuery.use(delegator)
                    .from("PortalPagePortlet")
                    .where(context)
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying PortalPagePortlet: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        context.put("portalPageId", context.get("newPortalPageId"));
        // TODO: Call simple-method "copyIfRequiredSystemPage" from "component://common/script/org/ofbiz/common/PortalPageMethods.xml"
        context.put("newPortalPageId", context.get("portalPageId"));
        GenericValue newEntity = delegator.makeValue("PortalPagePortlet");
        newEntity.put("portalPortletId", context.get("portalPortletId"));
        newEntity.put("portalPageId", context.get("newPortalPageId"));
        newEntity.put("columnNum", "1");
        delegator.setNextSubSeqId(newEntity, "portletSeqId", 5, 1);
        Object portletSeqId = newEntity.get("portletSeqId");
        try {
            delegator.create(newEntity);
        } catch (Exception e) {
            Debug.logError(e, "Error creating value: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        try {
            delegator.removeValue(oldEntity);
        } catch (Exception e) {
            Debug.logError(e, "Error removing value: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }

        return result;
    }


    /**
     * Add a new Column to a PortalPage
     */
    public static Map<String, Object> addPortalPageColumn(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        GenericValue portalPage = null;
        Map<String, Object> inlineResult = checkOwnerShip(dctx, context);
        if (ServiceUtil.isError(inlineResult)) {
            return inlineResult;
        }
        GenericValue newEntity = delegator.makeValue("PortalPageColumn");
        newEntity.setPKFields(context);
        if (UtilValidate.isEmpty(context.get("columnSeqId"))) {
            delegator.setNextSubSeqId(newEntity, "columnSeqId", 5, 1);
            Object columnSeqId = newEntity.get("columnSeqId");
        }
        result.put("columnSeqId", newEntity.get("columnSeqId"));
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
     * Delete a Column from a PortalPage
     */
    public static Map<String, Object> deletePortalPageColumn(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        List<String> error_list = new LinkedList<>();

        Map<String, Object> deletePortalPagePortletInMap = null;
        List<GenericValue> portalPortletList = null;
        GenericValue portalPage = null;
        Map<String, Object> inlineResult = checkOwnerShip(dctx, context);
        if (ServiceUtil.isError(inlineResult)) {
            return inlineResult;
        }
        GenericValue column = null;
        try {
            column = EntityQuery.use(delegator)
                    .from("PortalPageColumn")
                    .where(context)
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying PortalPageColumn: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        if (UtilValidate.isNotEmpty(column)) {
            try {
                portalPortletList = EntityQuery.use(delegator)
                        .from("PortalPagePortlet")
                        .where(UtilMisc.toMap("portalPageId", column.get("portalPageId"), "columnSeqId", column.get("columnSeqId")))
                        .queryList();
            } catch (Exception e) {
                Debug.logError(e, "Error querying entity: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
            if (portalPortletList != null) {
                for (GenericValue portalPortlet : portalPortletList) {
                    // set-service-fields from "portalPortlet" to "deletePortalPagePortletInMap" for service "deletePortalPagePortlet"
                    deletePortalPagePortletInMap.putAll(UtilMisc.toMap(portalPortlet));
                    try {
                        Map<String, Object> serviceResult = dispatcher.runSync("deletePortalPagePortlet", deletePortalPagePortletInMap);
                        if (ServiceUtil.isError(serviceResult)) {
                            return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
                        }
                    } catch (Exception e) {
                        Debug.logError(e, "Error calling deletePortalPagePortlet: " + e.getMessage(), MODULE);
                        return ServiceUtil.returnError(e.getMessage());
                    }
                }
            }
            try {
                delegator.removeValue(column);
            } catch (Exception e) {
                Debug.logError(e, "Error removing value: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
        }
        if (!error_list.isEmpty()) {
            return ServiceUtil.returnError(error_list);
        }

        return result;
    }


    /**
     * Add a registered PortalPortlet to a PortalPage
     */
    public static Map<String, Object> createPortalPagePortlet(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        GenericValue portalPage = null;
        Map<String, Object> inlineResult = checkOwnerShip(dctx, context);
        if (ServiceUtil.isError(inlineResult)) {
            return inlineResult;
        }
        GenericValue newEntity = delegator.makeValue("PortalPagePortlet");
        newEntity.setPKFields(context);
        List<GenericValue> portlets = null;
        try {
            portlets = EntityQuery.use(delegator)
                    .from("PortalPagePortlet")
                    .queryList();
        } catch (Exception e) {
            Debug.logError(e, "Error querying PortalPagePortlet: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        GenericValue portalPagePortlet = EntityUtil.getFirst((List<GenericValue>) portlets);
        if (UtilValidate.isEmpty(portalPagePortlet.get("sequenceNum"))) {
            newEntity.set("sequenceNum", 1);
        } else {
            newEntity.set("sequenceNum", (new BigDecimal(portalPagePortlet.get("sequenceNum").toString())).longValue());
        }
        delegator.setNextSubSeqId(newEntity, "portletSeqId", 5, 1);
        Object portletSeqId = newEntity.get("portletSeqId");
        result.put("portletSeqId", newEntity.get("portletSeqId"));
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
     * Delete a PortalPortlet from a PortalPageColumn
     */
    public static Map<String, Object> deletePortalPagePortlet(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        GenericValue newEntity = null;
        GenericValue portalPage = null;
        Map<String, Object> inlineResult = checkOwnerShip(dctx, context);
        if (ServiceUtil.isError(inlineResult)) {
            return inlineResult;
        }
        GenericValue portlet = null;
        try {
            portlet = EntityQuery.use(delegator)
                    .from("PortalPagePortlet")
                    .where(context)
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying PortalPagePortlet: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        if (UtilValidate.isNotEmpty(portlet)) {
            newEntity = delegator.makeValue("PortletAttribute");
            newEntity.put("portalPageId", portlet.get("portalPageId"));
            newEntity.put("portalPortletId", portlet.get("portalPortletId"));
            newEntity.put("portletSeqId", portlet.get("portletSeqId"));
            try {
                delegator.removeByAnd("PortletAttribute", newEntity);
            } catch (Exception e) {
                Debug.logError(e, "Error removing PortletAttribute: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
            try {
                delegator.removeValue(portlet);
            } catch (Exception e) {
                Debug.logError(e, "Error removing value: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
        }

        return result;
    }


    /**
     * Get all attributes of a Portlet either by providing userLogin or portalPageid with portalPortletId
     */
    public static Map<String, Object> getPortletAttributes(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        List<String> error_list = new LinkedList<>();

        List<GenericValue> ppList = null;
        GenericValue portalPage = null;
        Map<String, Object> attributeMap = null;
        if (UtilValidate.isEmpty(context.get("ownerUserLoginId"))) {
            if (UtilValidate.isEmpty(context.get("portalPageId"))) {
                Debug.logError("Service getPortletAttributes did not receive either ownerUserLoginId OR portalPageId", MODULE);
                error_list.add("Service getPortletAttributes did not receive either ownerUserLoginId OR portalPageId");
            }
        }
        if (!error_list.isEmpty()) {
            return ServiceUtil.returnError(error_list);
        }
        if (UtilValidate.isNotEmpty(context.get("ownerUserLoginId"))) {
            try {
                ppList = EntityQuery.use(delegator)
                        .from("PortalPageAndPortlet")
                        .where(UtilMisc.toMap("ownerUserLoginId", context.get("ownerUserLoginId"), "portalPortletId", context.get("portalPortletId")))
                        .queryList();
            } catch (Exception e) {
                Debug.logError(e, "Error querying entity: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
            portalPage = EntityUtil.getFirst((List<GenericValue>) ppList);
            context.put("portalPageId", portalPage.get("portalPageId"));
        }
        List<GenericValue> attributeList = null;
        try {
            attributeList = EntityQuery.use(delegator)
                    .from("PortletAttribute")
                    .queryList();
        } catch (Exception e) {
            Debug.logError(e, "Error querying PortletAttribute: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        if (UtilValidate.isNotEmpty(attributeList)) {
            if (attributeList != null) {
                for (GenericValue attributeRecord : attributeList) {
                    attributeMap.put((String) attributeRecord.get("attrName"), attributeRecord.get("attrValue"));
                }
            }
            result.put("attributeMap", attributeMap);
        }

        return result;
    }


    /**
     * Create a new Portal Page
     */
    public static Map<String, Object> createPortalPage(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        GenericValue newPortalPage = null;
        if (UtilValidate.isNotEmpty(context.get("portalPageName"))) {
            newPortalPage = delegator.makeValue("PortalPage");
            newPortalPage.setPKFields(context);
            if (UtilValidate.isEmpty(newPortalPage.get("portalPageId"))) {
                ((GenericValue) newPortalPage).put("portalPageId", delegator.getNextSeqId("PortalPage"));
            }
            newPortalPage.setNonPKFields(context);
            newPortalPage.put("ownerUserLoginId", ((GenericValue) context.get("userLogin")).getString("userLoginId"));
            if (UtilValidate.isEmpty(context.get("sequenceNum"))) {
                delegator.setNextSubSeqId(newPortalPage, "sequenceNum", 5, 1);
                Object sequenceNum = newPortalPage.get("sequenceNum");
            }
            try {
                delegator.create(newPortalPage);
            } catch (Exception e) {
                Debug.logError(e, "Error creating value: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
            result.put("portalPageId", newPortalPage.get("portalPageId"));
        }

        return result;
    }


    /**
     * Delete a Portal Page
     */
    public static Map<String, Object> deletePortalPage(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        GenericValue getOldSequenceNum = null;
        Map<String, Object> first = null;
        List<GenericValue> checkSequenceNums = null;
        GenericValue checkSequenceNum = null;
        GenericValue portalPage = null;
        GenericValue getPortalPage = null;
        try {
            getPortalPage = EntityQuery.use(delegator)
                    .from("PortalPage")
                    .where(context)
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying PortalPage: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        if (UtilValidate.isNotEmpty(getPortalPage.get("originalPortalPageId"))) {
            try {
                getOldSequenceNum = EntityQuery.use(delegator)
                        .from("PortalPage")
                        .where(UtilMisc.toMap("portalPageId", getPortalPage.get("originalPortalPageId")))
                        .queryOne();
            } catch (Exception e) {
                Debug.logError(e, "Error querying PortalPage: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
            try {
                checkSequenceNums = EntityQuery.use(delegator)
                        .from("PortalPage")
                        .queryList();
            } catch (Exception e) {
                Debug.logError(e, "Error querying PortalPage: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
            checkSequenceNum = EntityUtil.getFirst((List<GenericValue>) checkSequenceNums);
            if (UtilValidate.isNotEmpty(checkSequenceNum.get("portalPageId"))) {
                first.put("portalPageId", checkSequenceNum.get("portalPageId"));
                first.put("sequenceNum", getPortalPage.get("sequenceNum"));
                try {
                    Map<String, Object> serviceResult = dispatcher.runSync("updatePortalPage", first);
                    if (ServiceUtil.isError(serviceResult)) {
                        return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
                    }
                } catch (Exception e) {
                    Debug.logError(e, "Error calling updatePortalPage: " + e.getMessage(), MODULE);
                    return ServiceUtil.returnError(e.getMessage());
                }
            }
        }
        Map<String, Object> inlineResult = checkOwnerShip(dctx, context);
        if (ServiceUtil.isError(inlineResult)) {
            return inlineResult;
        }
        try {
            portalPage.removeRelated("PortalPageColumn");
        } catch (Exception e) {
            Debug.logError(e, "Error removing related PortalPageColumn: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        try {
            portalPage.removeRelated("PortalPagePortlet");
        } catch (Exception e) {
            Debug.logError(e, "Error removing related PortalPagePortlet: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        try {
            delegator.removeValue(portalPage);
        } catch (Exception e) {
            Debug.logError(e, "Error removing value: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }

        return result;
    }


    /**
     * Check the ownership of a Portal Page
     */
    public static Map<String, Object> checkOwnerShip(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        Security security = dctx.getSecurity();
        List<String> error_list = new LinkedList<>();

        GenericValue portalPage = null;
        if (UtilValidate.isNotEmpty(context.get("portalPageId"))) {
            try {
                portalPage = EntityQuery.use(delegator)
                        .from("PortalPage")
                        .where(context)
                        .queryOne();
            } catch (Exception e) {
                Debug.logError(e, "Error querying PortalPage: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
            if (UtilValidate.isEmpty(portalPage)) {
                {
                    String errorMsg = UtilProperties.getMessage("CommonUiLabels", "PortalPageNotFound", locale);
                    error_list.add(errorMsg);
                }
            }
            if (!error_list.isEmpty()) {
                return ServiceUtil.returnError(error_list);
            }
            if ((!"${parameters.userLogin.userLoginId}".equals(portalPage.get("ownerUserLoginId")) && !(security.hasEntityPermission("MYPORTALBASE", "_ADMIN", userLogin)))) {
                {
                    String errorMsg = UtilProperties.getMessage("CommonUiLabels", "PortalPageNotOwned", locale);
                    error_list.add(errorMsg);
                }
            }
            if (!error_list.isEmpty()) {
                return ServiceUtil.returnError(error_list);
            }
        }

        return result;
    }


    /**
     * Update the portal page sequence numbers
     */
    public static Map<String, Object> updatePortalPageSeq(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        List<GenericValue> getDatas = null;
        GenericValue portalPage = null;
        Map<String, Object> inlineResult = checkOwnerShip(dctx, context);
        if (ServiceUtil.isError(inlineResult)) {
            return inlineResult;
        }
        GenericValue getSequenceNum = null;
        try {
            getSequenceNum = EntityQuery.use(delegator)
                    .from("PortalPage")
                    .where(context)
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying PortalPage: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        if ("UP".equals(context.get("mode"))) {
            try {
                getDatas = EntityQuery.use(delegator)
                        .from("PortalPage")
                        .queryList();
            } catch (Exception e) {
                Debug.logError(e, "Error querying PortalPage: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
        }
        if ("DWN".equals(context.get("mode"))) {
            try {
                getDatas = EntityQuery.use(delegator)
                        .from("PortalPage")
                        .queryList();
            } catch (Exception e) {
                Debug.logError(e, "Error querying PortalPage: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
        }
        if ("TOP".equals(context.get("mode"))) {
            try {
                getDatas = EntityQuery.use(delegator)
                        .from("PortalPage")
                        .queryList();
            } catch (Exception e) {
                Debug.logError(e, "Error querying PortalPage: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
        }
        if ("BOT".equals(context.get("mode"))) {
            try {
                getDatas = EntityQuery.use(delegator)
                        .from("PortalPage")
                        .queryList();
            } catch (Exception e) {
                Debug.logError(e, "Error querying PortalPage: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
        }
        GenericValue getData = EntityUtil.getFirst((List<GenericValue>) getDatas);
        portalPage.put("sequenceNum", getData.get("sequenceNum"));
        try {
            delegator.store(portalPage);
        } catch (Exception e) {
            Debug.logError(e, "Error storing value: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        Map<String, Object> first = new HashMap<String, Object>();
        first.put("portalPageId", getData.get("portalPageId"));
        first.put("sequenceNum", getSequenceNum.get("sequenceNum"));
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("updatePortalPage", first);
            if (ServiceUtil.isError(serviceResult)) {
                return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
            }
        } catch (Exception e) {
            Debug.logError(e, "Error calling updatePortalPage: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }

        return result;
    }


    /**
     * Updates a portlet Seq No for the Drag and Drop Feature
     */
    public static Map<String, Object> updatePortletSeqDragDrop(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        List<GenericValue> modifyPpList = null;
        GenericValue destiPp = null;
        Object newSequenceNo = null;
        Long increase = null;
        GenericValue portalPage = null;
        context.put("portalPageId", context.get("o_portalPageId"));
        Map<String, Object> inlineResult = checkOwnerShip(dctx, context);
        if (ServiceUtil.isError(inlineResult)) {
            return inlineResult;
        }
        GenericValue originPp = null;
        try {
            originPp = EntityQuery.use(delegator)
                    .from("PortalPagePortlet")
                    .where(UtilMisc.toMap("portalPageId", context.get("o_portalPageId"), "portalPortletId", context.get("o_portalPortletId"), "portletSeqId", context.get("o_portletSeqId")))
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying PortalPagePortlet: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        if (UtilValidate.isEmpty(originPp)) {
            return ServiceUtil.returnError(ServiceUtil.getErrorMessage(result));
        }
        Object columnSeqId = context.get("destinationColumn");
        if (context.get("mode") != null /* TODO: operator contains */) {
            try {
                destiPp = EntityQuery.use(delegator)
                        .from("PortalPagePortlet")
                        .where(UtilMisc.toMap("portalPageId", context.get("d_portalPageId"), "portalPortletId", context.get("d_portalPortletId"), "portletSeqId", context.get("d_portletSeqId")))
                        .queryOne();
            } catch (Exception e) {
                Debug.logError(e, "Error querying PortalPagePortlet: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
            try {
                modifyPpList = EntityQuery.use(delegator)
                        .from("PortalPagePortlet")
                        .queryList();
            } catch (Exception e) {
                Debug.logError(e, "Error querying PortalPagePortlet: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
            increase = 1L;
            newSequenceNo = destiPp.get("sequenceNum");
        }
        if ("DRAGDROPAFTER".equals(context.get("mode"))) {
            try {
                destiPp = EntityQuery.use(delegator)
                        .from("PortalPagePortlet")
                        .where(UtilMisc.toMap("portalPageId", context.get("d_portalPageId"), "portalPortletId", context.get("d_portalPortletId"), "portletSeqId", context.get("d_portletSeqId")))
                        .queryOne();
            } catch (Exception e) {
                Debug.logError(e, "Error querying PortalPagePortlet: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
            try {
                modifyPpList = EntityQuery.use(delegator)
                        .from("PortalPagePortlet")
                        .queryList();
            } catch (Exception e) {
                Debug.logError(e, "Error querying PortalPagePortlet: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
            increase = -1L;
            newSequenceNo = destiPp.get("sequenceNum");
        }
        if (context.get("mode") != null /* TODO: operator contains */) {
            newSequenceNo = "0";
        }
        if (UtilValidate.isNotEmpty(modifyPpList)) {
            if (modifyPpList != null) {
                for (GenericValue modifyPp : modifyPpList) {
                    if (UtilValidate.isEmpty(modifyPp.get("sequenceNum"))) {
                        modifyPp.put("sequenceNum", "newSequenceNo");
                    } else {
                        modifyPp.set("sequenceNum", (new BigDecimal(increase.toString())).longValue());
                        increase = (new BigDecimal(increase.toString())).longValue();
                    }
                    try {
                        delegator.store(modifyPp);
                    } catch (Exception e) {
                        Debug.logError(e, "Error storing value: " + e.getMessage(), MODULE);
                        return ServiceUtil.returnError(e.getMessage());
                    }
                }
            }
        }
        originPp.put("columnSeqId", columnSeqId);
        originPp.put("sequenceNum", newSequenceNo);
        try {
            delegator.store(originPp);
        } catch (Exception e) {
            Debug.logError(e, "Error storing value: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }

        return result;
    }

}
