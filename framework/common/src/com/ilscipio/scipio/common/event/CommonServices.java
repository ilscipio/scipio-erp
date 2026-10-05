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
import java.math.RoundingMode;
import java.sql.Timestamp;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.TimeZone;
import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.GroovyUtil;
import org.ofbiz.base.util.UtilDateTime;
import org.ofbiz.base.util.UtilMisc;
import org.ofbiz.base.util.UtilProperties;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.common.CommonWorkers;
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
 * <p>Generated from: component://common/script/org/ofbiz/common/CommonServices.xml</p>
 *
 * <p>SCIPIO: 4.0.0: Auto-generated.</p>
 */
public class CommonServices {

    private static final String MODULE = CommonServices.class.getName();


    /**
     * Main permission logic
     */
    public static Map<String, Object> commonGenericPermission(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        Object primaryPermission = "COMMON";
        // TODO: Call simple-method "genericBasePermissionCheck" from "component://common/script/org/ofbiz/common/permission/CommonPermissionServices.xml"

        return result;
    }


    /**
     * Create a KeywordThesaurus
     */
    public static Map<String, Object> createKeywordThesaurus(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        Security security = dctx.getSecurity();
        List<String> error_list = new LinkedList<>();

        if (!security.hasEntityPermission("CATALOG", "_CREATE", userLogin)) {
            return ServiceUtil.returnError(UtilProperties.getMessage("CommonUiLabels", "CommonGenericPermissionError", locale));
        }
        if (!error_list.isEmpty()) {
            return ServiceUtil.returnError(error_list);
        }
        GenericValue newEntity = delegator.makeValue("KeywordThesaurus");
        newEntity.setPKFields(context);
        newEntity.setNonPKFields(context);
        newEntity.put("enteredKeyword", ((String) newEntity.get("enteredKeyword")).toLowerCase());
        newEntity.put("alternateKeyword", ((String) newEntity.get("alternateKeyword")).toLowerCase());
        try {
            delegator.create(newEntity);
        } catch (Exception e) {
            Debug.logError(e, "Error creating value: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }

        return result;
    }


    /**
     * Update a KeywordThesaurus
     */
    public static Map<String, Object> updateKeywordThesaurus(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        Security security = dctx.getSecurity();
        List<String> error_list = new LinkedList<>();

        if (!security.hasEntityPermission("CATALOG", "_UPDATE", userLogin)) {
            return ServiceUtil.returnError(UtilProperties.getMessage("CommonUiLabels", "CommonGenericPermissionError", locale));
        }
        if (!error_list.isEmpty()) {
            return ServiceUtil.returnError(error_list);
        }
        GenericValue newEntity = delegator.makeValue("KeywordThesaurus");
        newEntity.setPKFields(context);
        newEntity.setNonPKFields(context);
        try {
            delegator.store(newEntity);
        } catch (Exception e) {
            Debug.logError(e, "Error storing value: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }

        return result;
    }


    /**
     * Delete a KeywordThesaurus
     */
    public static Map<String, Object> deleteKeywordThesaurus(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        Security security = dctx.getSecurity();
        List<String> error_list = new LinkedList<>();

        if (!security.hasEntityPermission("CATALOG", "_DELETE", userLogin)) {
            return ServiceUtil.returnError(UtilProperties.getMessage("CommonUiLabels", "CommonGenericPermissionError", locale));
        }
        if (!error_list.isEmpty()) {
            return ServiceUtil.returnError(error_list);
        }
        GenericValue newEntity = delegator.makeValue("KeywordThesaurus");
        newEntity.setPKFields(context);
        try {
            delegator.removeByAnd("KeywordThesaurus", newEntity);
        } catch (Exception e) {
            Debug.logError(e, "Error removing KeywordThesaurus: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }

        return result;
    }


    /**
     * Create a new dated UOM conversion entity
     */
    public static Map<String, Object> createUomConversionDated(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        GenericValue newEntity = delegator.makeValue("UomConversionDated");
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
     * Convert UOM values
     */
    public static Map<String, Object> convertUom(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        List<String> error_list = new LinkedList<>();

        Object asOfDate = null;
        Timestamp nowTimestamp = null;
        List<GenericValue> uomConversions = null;
        GenericValue uomConversion = null;
        Object roundingMode = null;
        BigDecimal roundedValue = null;
        Object decimalScale = null;
        Map<String, Object> customParms = null;
        BigDecimal convertedValue = null;
        if (java.util.Objects.equals(context.get("uomId"), context.get("uomIdTo"))) {
            result.put("convertedValue", context.get("originalValue"));
            return result;
        }
        if (UtilValidate.isEmpty(context.get("asOfDate"))) {
            nowTimestamp = new Timestamp(System.currentTimeMillis());
            asOfDate = nowTimestamp;
        } else {
            asOfDate = context.get("asOfDate");
        }
        try {
            uomConversion = EntityQuery.use(delegator)
                    .from("UomConversion")
                    .where(context)
                    .cache()
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying UomConversion: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        if (UtilValidate.isEmpty(uomConversion)) {
            try {
                uomConversions = EntityQuery.use(delegator)
                        .from("UomConversionDated")
                        .cache()
                        .queryList();
            } catch (Exception e) {
                Debug.logError(e, "Error querying UomConversionDated: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
            uomConversion = EntityUtil.getFirst((List<GenericValue>) uomConversions);
            if (UtilValidate.isEmpty(uomConversion)) {
                if (UtilValidate.isNotEmpty(context.get("purposeEnumId"))) {
                    try {
                        uomConversions = EntityQuery.use(delegator)
                                .from("UomConversionDated")
                                .cache()
                                .queryList();
                    } catch (Exception e) {
                        Debug.logError(e, "Error querying UomConversionDated: " + e.getMessage(), MODULE);
                        return ServiceUtil.returnError(e.getMessage());
                    }
                    uomConversion = EntityUtil.getFirst((List<GenericValue>) uomConversions);
                }
            }
        }
        Debug.logVerbose("using conversion factor=" + uomConversion.get("conversionFactor"), MODULE);
        if (UtilValidate.isEmpty(uomConversion)) {
            {
                String errorMsg = UtilProperties.getMessage("CommonUiLabels", "CommonNoUomConversionFound", locale);
                error_list.add(errorMsg);
            }
            if (!error_list.isEmpty()) {
                return ServiceUtil.returnError(error_list);
            }
        } else {
            if (UtilValidate.isNotEmpty(uomConversion.get("customMethodId"))) {
                Debug.logVerbose("using custom conversion customMethodId=" + uomConversion.get("customMethodId"), MODULE);
                // set-service-fields from "parameters" to "customParms" for service "convertUomCustom"
                customParms.putAll(UtilMisc.toMap(context));
                customParms.put("uomConversion", uomConversion);
                try {
                    Map<String, Object> serviceResult = dispatcher.runSync("convertUomCustom", customParms);
                    if (ServiceUtil.isError(serviceResult)) {
                        return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
                    }
                    convertedValue = (BigDecimal) serviceResult.get("convertedValue");
                } catch (Exception e) {
                    Debug.logError(e, "Error calling convertUomCustom: " + e.getMessage(), MODULE);
                    return ServiceUtil.returnError(e.getMessage());
                }
                Debug.logVerbose("Custom UoM conversion returning convertedValue=" + convertedValue, MODULE);
            } else {
                convertedValue = ((new BigDecimal(context.get("originalValue").toString())).multiply(new BigDecimal(uomConversion.get("conversionFactor").toString()))).setScale(15, RoundingMode.HALF_UP);
            }
            roundingMode = uomConversion.get("roundingMode");
            decimalScale = uomConversion.get("decimalScale");
            if (UtilValidate.isNotEmpty(roundingMode)) {
                roundedValue = (new BigDecimal(convertedValue.toString())).setScale(((Number) decimalScale).intValue(), RoundingMode.valueOf(String.valueOf(roundingMode).toUpperCase().replace("-", "_")));
                convertedValue = roundedValue;
            }
        }
        result.put("convertedValue", convertedValue);
        Debug.logVerbose("Uom conversion of [" + context.get("originalValue") + "] from [" + context.get("uomId") + "] to [" + context.get("uomIdTo") + "] using conversion factor [" + uomConversion.get("conversionFactor") + "], result is [" + convertedValue + "]", MODULE);

        return result;
    }


    /**
     * Convert UOM values using CustomMethod
     */
    public static Map<String, Object> convertUomCustom(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        List<String> error_list = new LinkedList<>();

        Map<String, Object> args = null;
        Object customMethodId = ((Map<String, Object>) context.get("uomConversion")).get("customMethodId");
        GenericValue customMethod = null;
        try {
            customMethod = EntityQuery.use(delegator)
                    .from("CustomMethod")
                    .where(context)
                    .cache()
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying CustomMethod: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        if (UtilValidate.isEmpty(customMethod.get("customMethodName"))) {
            {
                String errorMsg = UtilProperties.getMessage("CommonUiLabels", "CommonNoCustomMethodName", locale);
                error_list.add(errorMsg);
            }
            if (!error_list.isEmpty()) {
                return ServiceUtil.returnError(error_list);
            }
        } else {
            Debug.logVerbose("calling custom method " + customMethod.get("customMethodName"), MODULE);
            args.put("arguments", context);
            try {
                Map<String, Object> serviceResult = dispatcher.runSync("${customMethod.customMethodName}", args);
                if (ServiceUtil.isError(serviceResult)) {
                    return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
                }
                result.put("convertedValue", serviceResult.get("convertedValue"));
            } catch (Exception e) {
                Debug.logError(e, "Error calling ${customMethod.customMethodName}: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
        }

        return result;
    }


    /**
     * Look up progress made in File Upload process
     */
    public static Map<String, Object> getFileUploadProgressStatus(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        List<String> error_list = new LinkedList<>();

        Object uploadProgressListener = context.get("uploadProgressListener");
        if (UtilValidate.isNotEmpty(uploadProgressListener)) {
            try {
                context.put("contentLength", uploadProgressListener.getClass().getMethod("getContentLength").invoke(uploadProgressListener));
            } catch (Exception e) {
                Debug.logError(e, "Error calling getContentLength: " + e.getMessage(), MODULE);
            }
            result.put("contentLength", context.get("contentLength"));
            try {
                context.put("bytesRead", uploadProgressListener.getClass().getMethod("getBytesRead").invoke(uploadProgressListener));
            } catch (Exception e) {
                Debug.logError(e, "Error calling getBytesRead: " + e.getMessage(), MODULE);
            }
            result.put("bytesRead", context.get("bytesRead"));
            try {
                context.put("hasStarted", uploadProgressListener.getClass().getMethod("hasStarted").invoke(uploadProgressListener));
            } catch (Exception e) {
                Debug.logError(e, "Error calling hasStarted: " + e.getMessage(), MODULE);
            }
            result.put("hasStarted", context.get("hasStarted"));
            try {
                Map<String, Object> scriptContext = new HashMap<String, Object>();
                scriptContext.put("delegator", delegator);
                scriptContext.put("dispatcher", dispatcher);
                scriptContext.put("locale", locale);
                scriptContext.put("userLogin", userLogin);
                scriptContext.put("context", context);
                scriptContext.put("parameters", context);
                Object scriptResult = GroovyUtil.eval("contentLength = parameters.get(\"contentLength\")\n                bytesRead = parameters.get(\"bytesRead\")\n                int readPercent = (bytesRead*100)/contentLength\n                parameters.put(\"readPercent\", readPercent)", scriptContext);
            } catch (Exception e) {
                Debug.logError(e, "Error executing Groovy script: " + e.getMessage(), MODULE);
            }
            result.put("readPercent", context.get("readPercent"));
            result.put("hasStarted", context.get("hasStarted"));
            try {
                Map<String, Object> scriptContext = new HashMap<String, Object>();
                scriptContext.put("delegator", delegator);
                scriptContext.put("dispatcher", dispatcher);
                scriptContext.put("locale", locale);
                scriptContext.put("userLogin", userLogin);
                scriptContext.put("context", context);
                scriptContext.put("parameters", context);
                Object scriptResult = GroovyUtil.eval("context.eventErrorMessage = uploadProgressListener.getEventMessages()._ERROR_MESSAGE_;\n                context.eventErrorMessageList = uploadProgressListener.getEventMessages()._ERROR_MESSAGE_LIST_;", scriptContext);
            } catch (Exception e) {
                Debug.logError(e, "Error executing Groovy script: " + e.getMessage(), MODULE);
            }
            if (UtilValidate.isNotEmpty(context.get("eventErrorMessageList"))) {
                error_list.add("${eventErrorMessageList[0]}");
                if (!error_list.isEmpty()) {
                    return ServiceUtil.returnError(error_list);
                }
            }
            if (UtilValidate.isNotEmpty(context.get("eventErrorMessage"))) {
                error_list.add("${eventErrorMessage}");
                if (!error_list.isEmpty()) {
                    return ServiceUtil.returnError(error_list);
                }
            }
        }

        return result;
    }


    /**
     * Get visual theme resources
     */
    public static Map<String, Object> getVisualThemeResources(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        List<String> error_list = new LinkedList<>();

        Object visualThemeId = null;
        String defaultVisualThemeId = null;
        List<GenericValue> resourceList = null;
        Object resourceTypeEnumId = null;
        String warningMsg = null;
        List<Object> themeResources_resourceTypeEnumId_ = null;
        Object resourceValue = null;
        visualThemeId = context.get("visualThemeId");
        Object themeResources = context.get("themeResources");
        try {
            resourceList = EntityQuery.use(delegator)
                    .from("VisualThemeResource")
                    .cache()
                    .queryList();
        } catch (Exception e) {
            Debug.logError(e, "Error querying VisualThemeResource: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        if (UtilValidate.isEmpty(resourceList)) {
            Debug.logError("Could not find the '" + visualThemeId + "' theme, reverting back to the good old OFBiz theme...", MODULE);
            visualThemeId = null;
            defaultVisualThemeId = UtilProperties.getMessage("general", "VISUAL_THEME", locale);
            if (UtilValidate.isNotEmpty(defaultVisualThemeId)) {
                try {
                    resourceList = EntityQuery.use(delegator)
                            .from("VisualThemeResource")
                            .cache()
                            .queryList();
                } catch (Exception e) {
                    Debug.logError(e, "Error querying VisualThemeResource: " + e.getMessage(), MODULE);
                    return ServiceUtil.returnError(e.getMessage());
                }
                visualThemeId = defaultVisualThemeId;
            }
        }
        if (UtilValidate.isEmpty(resourceList)) {
            {
                String errorMsg = UtilProperties.getMessage("CommonUiLabels", "CommonVisualThemeResourcesNotFound", locale);
                error_list.add(errorMsg);
            }
            if (!error_list.isEmpty()) {
                return ServiceUtil.returnError(error_list);
            }
        }
        if (resourceList != null) {
            for (GenericValue resourceRecord : resourceList) {
                resourceTypeEnumId = resourceRecord.get("resourceTypeEnumId");
                resourceValue = resourceRecord.get("resourceValue");
                if (UtilValidate.isEmpty(resourceValue)) {
                    warningMsg = UtilProperties.getMessage("CommonUiLabels", "CommonVisualThemeInvalidRecord", locale);
                    Debug.logWarning(warningMsg, MODULE);
                } else {
                    ((Map<String, Object>) themeResources).put((String) context.get("resourceTypeEnumId]["), resourceValue);
                }
            }
        }
        if (UtilValidate.isEmpty(((Map<String, Object>) themeResources).get("VT_ID"))) {
            ((List<Object>) ((Map<String, Object>) themeResources).get("VT_ID")).add(visualThemeId);
        }
        result.put("visualThemeId", visualThemeId);
        result.put("themeResources", themeResources);

        return result;
    }


    /**
     * Update a note
     */
    public static Map<String, Object> updateNote(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        GenericValue noteData = null;
        try {
            noteData = EntityQuery.use(delegator)
                    .from("NoteData")
                    .where(UtilMisc.toMap("noteId", context.get("noteId")))
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying NoteData: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        noteData.setNonPKFields(context);
        try {
            delegator.store(noteData);
        } catch (Exception e) {
            Debug.logError(e, "Error storing value: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        result.put("noteId", noteData.get("noteId"));

        return result;
    }


    /**
     * Returns a list of country
     */
    public static Map<String, Object> getCountryList(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        Object countryName = null;
        List<Object> countryList = null;
        Object geoList = null;
        try {
            geoList = CommonWorkers.getCountryList(delegator);
        } catch (Exception e) {
            Debug.logError(e, "Error calling CommonWorkers.getCountryList: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        if (geoList != null) {
            for (Object countryGeo : (List<?>) geoList) {
                countryName = "" + ((Map<String, Object>) countryGeo).get("geoName") + ": " + ((Map<String, Object>) countryGeo).get("geoId");
                countryList.add(countryName);
            }
        }
        result.put("countryList", countryList);

        return result;
    }


    /**
     * set the state options for selected country
     */
    public static Map<String, Object> getAssociatedStateList(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        Object stateName = null;
        List<Object> stateList = null;
        String noOptions = null;
        Object countryGeoId = context.get("countryGeoId");
        Object listOrderBy = context.get("listOrderBy");
        Object geoList = null;
        try {
            geoList = CommonWorkers.getAssociatedStateList(delegator, (String) countryGeoId, (String) listOrderBy);
        } catch (Exception e) {
            Debug.logError(e, "Error calling CommonWorkers.getAssociatedStateList: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        if (geoList != null) {
            for (Object stateGeo : (List<?>) geoList) {
                stateName = "" + ((Map<String, Object>) stateGeo).get("geoName") + ": " + ((Map<String, Object>) stateGeo).get("geoId");
                stateList.add(stateName);
            }
        }
        if (UtilValidate.isEmpty(stateList)) {
            noOptions = UtilProperties.getMessage("CommonUiLabels", "CommonNoStatesProvinces", locale);
            stateList.add(noOptions);
        }
        result.put("stateList", stateList);

        return result;
    }


    /**
     * Link Geos to another Geo
     */
    public static Map<String, Object> linkGeos(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        List<String> error_list = new LinkedList<>();

        GenericValue oldGeoAssoc = null;
        GenericValue newGeoAssoc = null;
        List<GenericValue> geoAssocs = null;
        try {
            geoAssocs = EntityQuery.use(delegator)
                    .from("GeoAssoc")
                    .where(UtilMisc.toMap("geoId", context.get("geoId"), "geoAssocTypeId", context.get("geoAssocTypeId")))
                    .queryList();
        } catch (Exception e) {
            Debug.logError(e, "Error querying entity: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        Object oldGeoIds = GroovyUtil.eval("org.ofbiz.entity.util.EntityUtil.getFieldListFromEntityList(geoAssocs, 'geoIdTo', true);", UtilMisc.toMap("delegator", delegator, "dispatcher", dispatcher, "locale", locale, "userLogin", userLogin, "context", context, "parameters", context));
        if (context.get("geoIds") != null) {
            for (GenericValue geoIdTo : (List<GenericValue>) context.get("geoIds")) {
                if (oldGeoIds != null /* TODO: field compare operator contains */) {
                } else {
                    try {
                        oldGeoAssoc = EntityQuery.use(delegator)
                                .from("GeoAssoc")
                                .where(UtilMisc.toMap("geoId", context.get("geoId"), "geoIdTo", geoIdTo))
                                .queryOne();
                    } catch (Exception e) {
                        Debug.logError(e, "Error querying GeoAssoc: " + e.getMessage(), MODULE);
                        return ServiceUtil.returnError(e.getMessage());
                    }
                    if (UtilValidate.isEmpty(oldGeoAssoc)) {
                        newGeoAssoc = delegator.makeValue("GeoAssoc");
                        newGeoAssoc.put("geoId", context.get("geoId"));
                        newGeoAssoc.put("geoIdTo", geoIdTo);
                        newGeoAssoc.put("geoAssocTypeId", context.get("geoAssocTypeId"));
                        try {
                            delegator.create(newGeoAssoc);
                        } catch (Exception e) {
                            Debug.logError(e, "Error creating value: " + e.getMessage(), MODULE);
                            return ServiceUtil.returnError(e.getMessage());
                        }
                    }
                }
            }
        }
        if (!error_list.isEmpty()) {
            return ServiceUtil.returnError(error_list);
        }

        return result;
    }


    /**
     * get related geos to a geo through a geoAssoc
     */
    public static Map<String, Object> getRelatedGeos(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        Object noOptions = null;
        List<Object> geoList = null;
        List<GenericValue> geoAssoc = null;
        try {
            geoAssoc = EntityQuery.use(delegator)
                    .from("GeoAssoc")
                    .where(UtilMisc.toMap("geoId", context.get("geoId"), "geoAssocTypeId", context.get("geoAssocTypeId")))
                    .queryList();
        } catch (Exception e) {
            Debug.logError(e, "Error querying entity: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        GenericValue geo = null;
        if (UtilValidate.isEmpty(geoAssoc)) {
            noOptions = "____";
            geoList.add(noOptions);
        } else {
            if (geoAssoc != null) {
                for (GenericValue geoEntry : geoAssoc) {
                    geoList.add(geoEntry.get("geoIdTo"));
                }
            }
        }
        result.put("geoList", geoList);

        return result;
    }


    /**
     * Returns true if an UomConversion record exists
     */
    public static Map<String, Object> checkUomConversion(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        Boolean exist = null;
        GenericValue uomConversion = null;
        try {
            uomConversion = EntityQuery.use(delegator)
                    .from("UomConversion")
                    .where(UtilMisc.toMap("uomId", context.get("uomId"), "uomIdTo", context.get("uomIdTo")))
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying UomConversion: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        if (UtilValidate.isNotEmpty(uomConversion)) {
            exist = Boolean.TRUE;
        } else {
            exist = Boolean.FALSE;
        }
        result.put("exist", exist);

        return result;
    }


    /**
     * Returns true if an UomConversionDated record exists
     */
    public static Map<String, Object> checkUomConversionDated(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        Boolean exist = null;
        List<GenericValue> uomConversions = null;
        try {
            uomConversions = EntityQuery.use(delegator)
                    .from("UomConversionDated")
                    .filterByDate()
                    .queryList();
        } catch (Exception e) {
            Debug.logError(e, "Error querying UomConversionDated: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        if (UtilValidate.isNotEmpty(uomConversions)) {
            exist = Boolean.TRUE;
        } else {
            exist = Boolean.FALSE;
        }
        result.put("exist", exist);

        return result;
    }


    /**
     */
    public static Map<String, Object> getServerTimestamp(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        Timestamp serverTimestamp = new Timestamp(System.currentTimeMillis());
        result.put("serverTimestamp", serverTimestamp);

        return result;
    }


    /**
     */
    public static Map<String, Object> getServerTimeZone(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        String serverTimeZone = String.valueOf(java.util.TimeZone.getDefault());
        result.put("serverTimeZone", serverTimeZone);

        return result;
    }


    /**
     */
    public static Map<String, Object> getServerTimestampAsLong(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        Long serverTimestamp = System.currentTimeMillis();
        result.put("serverTimestamp", serverTimestamp);

        return result;
    }


    /**
     */
    public static Map<String, Object> getServerTimestampAsString(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        Object dateTimeFormat = null;
        TimeZone targetTimeZone = null;
        Object serverTimestamp = new Timestamp(System.currentTimeMillis());
        Debug.logInfo("args: " + context, MODULE);
        dateTimeFormat = context.get("dateTimeFormat");
        if (UtilValidate.isEmpty(dateTimeFormat)) {
            dateTimeFormat = "yyyy-MM-dd HH:mm:ss.SSS";
        }
        targetTimeZone = (TimeZone) context.get("timeZone");
        if ((UtilValidate.isEmpty(targetTimeZone) || "true".equals(context.get("useServerTz")))) {
            targetTimeZone = (TimeZone) java.util.TimeZone.getDefault();
            Debug.logInfo("targetTimeZone: " + targetTimeZone, MODULE);
        }
        try {
            serverTimestamp = UtilDateTime.timeStampToString((Timestamp) serverTimestamp, (String) dateTimeFormat, (TimeZone) targetTimeZone, (Locale) context.get("locale"));
        } catch (Exception e) {
            Debug.logError(e, "Error calling UtilDateTime.timeStampToString: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        result.put("serverTimestamp", serverTimestamp);

        return result;
    }

}
