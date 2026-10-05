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
package com.ilscipio.scipio.accounting.event;

import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.UtilHttp;
import org.ofbiz.base.util.UtilMisc;
import org.ofbiz.base.util.UtilProperties;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.GenericValue;
import org.ofbiz.entity.condition.EntityCondition;
import org.ofbiz.entity.util.EntityQuery;
import org.ofbiz.service.LocalDispatcher;

/**
 * Auto-generated event class from Simple Method XML.
 *
 * <p>Generated from: component://accounting/script/org/ofbiz/accounting/UpgradeServices.xml</p>
 *
 * <p>SCIPIO: 4.0.0: Auto-generated.</p>
 */
public class UpgradeServices {

    private static final String MODULE = UpgradeServices.class.getName();


    /**
     * Migrate statusId to GlReconciliation entity
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String migrateStatusToGlReconciliation(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        List<GenericValue> glReconciliationList = null;
        try {
            glReconciliationList = EntityQuery.use(delegator)
                    .from("GlReconciliation")
                    .queryList();
        } catch (Exception e) {
            Debug.logError(e, "Error querying GlReconciliation: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        if (glReconciliationList != null) {
            for (GenericValue glReconciliation : glReconciliationList) {
                if (UtilValidate.isEmpty(((Map<String, Object>) glReconciliation).get("statusId"))) {
                    if (UtilValidate.isEmpty(((Map<String, Object>) glReconciliation).get("reconciledBalance"))) {
                        glReconciliation.put("statusId", "GLREC_CREATED");
                    } else {
                        glReconciliation.put("statusId", "GLREC_RECONCILED");
                    }
                    try {
                        delegator.store(glReconciliation);
                    } catch (Exception e) {
                        Debug.logError(e, "Error storing value: " + e.getMessage(), MODULE);
                        request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                        return "error";
                    }
                }
            }
        }

        return "success";
    }


    /**
     * Migrate statusId to FinAccountTrans entity
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String migrateStatusToFinAccountTrans(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        List<GenericValue> finAccountTransList = null;
        try {
            finAccountTransList = EntityQuery.use(delegator)
                    .from("FinAccountTrans")
                    .queryList();
        } catch (Exception e) {
            Debug.logError(e, "Error querying FinAccountTrans: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        if (finAccountTransList != null) {
            for (GenericValue finAccountTrans : finAccountTransList) {
                if (UtilValidate.isEmpty(((Map<String, Object>) finAccountTrans).get("statusId"))) {
                    finAccountTrans.put("statusId", "FINACT_TRNS_APPROVED");
                    try {
                        delegator.store(finAccountTrans);
                    } catch (Exception e) {
                        Debug.logError(e, "Error storing value: " + e.getMessage(), MODULE);
                        request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                        return "error";
                    }
                }
            }
        }

        return "success";
    }


    /**
     * Copy FixedAssetMaintMeter To FixedAssetMeter
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String migrateFixedAssetMaintMeter(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        GenericValue lookedUpValue = null;
        GenericValue newEntity = null;
        List<GenericValue> maintMeterList = null;
        try {
            maintMeterList = EntityQuery.use(delegator)
                    .from("FixedAssetMaintMeter")
                    .queryList();
        } catch (Exception e) {
            Debug.logError(e, "Error querying FixedAssetMaintMeter: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        if (maintMeterList != null) {
            for (GenericValue maintMeter : maintMeterList) {
                newEntity = delegator.makeValue("FixedAssetMeter");
                newEntity.setPKFields((Map<String, Object>) maintMeter);
                newEntity.setNonPKFields((Map<String, Object>) maintMeter);
                newEntity.put("readingDate", ((Map<String, Object>) maintMeter).get("createdStamp"));
                try {
                    lookedUpValue = EntityQuery.use(delegator)
                            .from("FixedAssetMeter")
                            .where(UtilMisc.toMap("fixedAssetId", ((Map<String, Object>) newEntity).get("fixedAssetId"), "productMeterTypeId", ((Map<String, Object>) newEntity).get("productMeterTypeId"), "readingDate", ((Map<String, Object>) newEntity).get("readingDate")))
                            .queryOne();
                } catch (Exception e) {
                    Debug.logError(e, "Error querying FixedAssetMeter: " + e.getMessage(), MODULE);
                    request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                    return "error";
                }
                if (UtilValidate.isEmpty(lookedUpValue)) {
                    try {
                        delegator.create(newEntity);
                    } catch (Exception e) {
                        Debug.logError(e, "Error creating value: " + e.getMessage(), MODULE);
                        request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                        return "error";
                    }
                    if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                            UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
                        return "error";
                    }
                }
            }
        }

        return "success";
    }


    /**
     * Copy AgreementWorkEffortAppl To AgreementWorkEffortApplic
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String migrateAgreementWorkEffortAppl(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        GenericValue newEntity = null;
        List<GenericValue> agreementWorkEffortApplList = null;
        try {
            agreementWorkEffortApplList = EntityQuery.use(delegator)
                    .from("OldAgreementWorkEffortAppl")
                    .queryList();
        } catch (Exception e) {
            Debug.logError(e, "Error querying OldAgreementWorkEffortAppl: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        if (agreementWorkEffortApplList != null) {
            for (GenericValue agreementWorkEffortAppl : agreementWorkEffortApplList) {
                newEntity = delegator.makeValue("AgreementWorkEffortApplic");
                newEntity.setPKFields((Map<String, Object>) agreementWorkEffortAppl);
                try {
                    delegator.create(newEntity);
                } catch (Exception e) {
                    Debug.logError(e, "Error creating value: " + e.getMessage(), MODULE);
                    request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                    return "error";
                }
                if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                        UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
                    return "error";
                }
            }
        }

        return "success";
    }

}
