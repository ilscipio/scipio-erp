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
import org.ofbiz.base.util.UtilMisc;
import org.ofbiz.base.util.UtilProperties;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.GenericValue;
import org.ofbiz.service.DispatchContext;
import org.ofbiz.service.LocalDispatcher;
import org.ofbiz.service.ServiceUtil;

/**
 * Auto-generated event class from Simple Method XML.
 *
 * <p>Generated from: component://workeffort/script/org/ofbiz/workeffort/workeffort/WorkflowSimpleEvents.xml</p>
 *
 * <p>SCIPIO: 4.0.0: Auto-generated.</p>
 */
public class WorkflowSimpleEvents {

    private static final String MODULE = WorkflowSimpleEvents.class.getName();


    /**
     * Create Work Effort
     */
    public static Map<String, Object> acceptAssignment(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        List<String> error_list = new LinkedList<>();

        // TODO: Convert call-map-processor (in-map: parameters, out-map: context)
        // xml-resource: component://workeffort/script/org/ofbiz/workeffort/workeffort/WorkflowMapProcessors.xml, processor-name: assignmentMap
        context = new HashMap<String, Object>();
        if (!error_list.isEmpty()) {
            return ServiceUtil.returnError(error_list);
        }
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("wfAcceptAssignment", context);
            if (ServiceUtil.isError(serviceResult)) {
                return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
            }
        } catch (Exception e) {
            Debug.logError(e, "Error calling wfAcceptAssignment: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }

        return result;
    }


    /**
     * Accept a ROLE Assignment
     */
    public static Map<String, Object> acceptRoleAssignment(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        List<String> error_list = new LinkedList<>();

        // TODO: Convert call-map-processor (in-map: parameters, out-map: context)
        // xml-resource: component://workeffort/script/org/ofbiz/workeffort/workeffort/WorkflowMapProcessors.xml, processor-name: assignmentMap
        context = new HashMap<String, Object>();
        if (!error_list.isEmpty()) {
            return ServiceUtil.returnError(error_list);
        }
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("wfAcceptRoleAssignment", context);
            if (ServiceUtil.isError(serviceResult)) {
                return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
            }
        } catch (Exception e) {
            Debug.logError(e, "Error calling wfAcceptRoleAssignment: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }

        return result;
    }


    /**
     * Creates WorkEffort
     */
    public static Map<String, Object> createWorkEffort(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        List<String> error_list = new LinkedList<>();

        // TODO: Convert call-map-processor (in-map: parameters, out-map: context)
        // xml-resource: component://workeffort/script/org/ofbiz/workeffort/workeffort/WorkflowMapProcessors.xml, processor-name: createWorkEffortMap
        context = new HashMap<String, Object>();
        if (!error_list.isEmpty()) {
            return ServiceUtil.returnError(error_list);
        }
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("createWorkEffort", context);
            if (ServiceUtil.isError(serviceResult)) {
                return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
            }
        } catch (Exception e) {
            Debug.logError(e, "Error calling createWorkEffort: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }

        return result;
    }


    /**
     * Creates WorkEffort
     */
    public static Map<String, Object> createWorkEffortAndAssoc(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        List<String> error_list = new LinkedList<>();

        // TODO: Convert call-map-processor (in-map: parameters, out-map: context)
        // xml-resource: component://workeffort/script/org/ofbiz/workeffort/workeffort/WorkflowMapProcessors.xml, processor-name: createWorkEffortMap
        context = new HashMap<String, Object>();
        if (!error_list.isEmpty()) {
            return ServiceUtil.returnError(error_list);
        }
        // TODO: Convert call-map-processor (in-map: parameters, out-map: context)
        // xml-resource: component://workeffort/script/org/ofbiz/workeffort/workeffort/WorkflowMapProcessors.xml, processor-name: createWorkEffortAssocMap
        context = new HashMap<String, Object>();
        if (!error_list.isEmpty()) {
            return ServiceUtil.returnError(error_list);
        }
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("createWorkEffortAndAssoc", context);
            if (ServiceUtil.isError(serviceResult)) {
                return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
            }
        } catch (Exception e) {
            Debug.logError(e, "Error calling createWorkEffortAndAssoc: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }

        return result;
    }


    /**
     * Creates WorkEffortNote
     */
    public static Map<String, Object> createWorkEffortNote(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        List<String> error_list = new LinkedList<>();

        // TODO: Convert call-map-processor (in-map: parameters, out-map: context)
        // xml-resource: component://workeffort/script/org/ofbiz/workeffort/workeffort/WorkflowMapProcessors.xml, processor-name: createWorkEffortNoteMap
        context = new HashMap<String, Object>();
        if (!error_list.isEmpty()) {
            return ServiceUtil.returnError(error_list);
        }
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("createWorkEffortNote", context);
            if (ServiceUtil.isError(serviceResult)) {
                return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
            }
        } catch (Exception e) {
            Debug.logError(e, "Error calling createWorkEffortNote: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }

        return result;
    }

}
