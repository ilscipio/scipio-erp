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
package com.ilscipio.scipio.manufacturing.event;

import java.sql.Timestamp;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Locale;
import java.util.Map;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.GeneralException;
import org.ofbiz.base.util.ObjectType;
import org.ofbiz.base.util.UtilDateTime;
import org.ofbiz.base.util.UtilGenerics;
import org.ofbiz.base.util.UtilHttp;
import org.ofbiz.base.util.UtilMisc;
import org.ofbiz.base.util.UtilProperties;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.GenericEntityException;
import org.ofbiz.entity.GenericValue;
import org.ofbiz.entity.util.EntityQuery;
import org.ofbiz.service.DispatchContext;
import org.ofbiz.service.GenericServiceException;
import org.ofbiz.service.LocalDispatcher;
import org.ofbiz.service.ModelService;
import org.ofbiz.service.ServiceUtil;

/**
 * Bill-of-materials edit services and the BOM edit screen request event.
 *
 * <p>Hand-written replacement for: component://manufacturing/script/org/ofbiz/manufacturing/bom/BomSimpleMethods.xml
 * and component://manufacturing/script/org/ofbiz/manufacturing/bom/BomMapProcs.xml</p>
 *
 * <p>SCIPIO: 4.0.0: Hand-written from minilang.</p>
 */
public class BomSimpleMethods {

    private static final String MODULE = BomSimpleMethods.class.getName();
    private static final String RESOURCE = "ManufacturingUiLabels";

    private BomSimpleMethods() {}

    /** Creates a ProductAssoc, rejecting the assoc if it would create a duplicate ancestor loop in the BOM. */
    public static Map<String, Object> createBOMAssoc(DispatchContext dctx, Map<String, ? extends Object> context) {
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        Map<String, Object> parameters = UtilGenerics.cast(context);

        Timestamp fromDate = (Timestamp) parameters.get("fromDate");
        if (fromDate == null) {
            fromDate = UtilDateTime.nowTimestamp();
        }

        Map<String, Object> searchCtx = new HashMap<>();
        searchCtx.put("productId", parameters.get("productId"));
        searchCtx.put("productIdTo", parameters.get("productIdTo"));
        searchCtx.put("fromDate", fromDate);
        searchCtx.put("productAssocTypeId", parameters.get("productAssocTypeId"));
        searchCtx.put("userLogin", parameters.get("userLogin"));
        Map<String, Object> searchResult;
        try {
            searchResult = dispatcher.runSync("searchDuplicatedAncestor", searchCtx);
        } catch (GenericServiceException e) {
            Debug.logError(e, "Error calling searchDuplicatedAncestor: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        if (ServiceUtil.isError(searchResult)) {
            return ServiceUtil.returnError(ServiceUtil.getErrorMessage(searchResult));
        }
        GenericValue duplicatedProductAssoc = (GenericValue) searchResult.get("duplicatedProductAssoc");
        if (UtilValidate.isNotEmpty(duplicatedProductAssoc)) {
            List<Object> productToProduct = UtilMisc.toList(
                    duplicatedProductAssoc.get("productId"), duplicatedProductAssoc.get("productIdTo"));
            String errorMessage = UtilProperties.getMessage(RESOURCE, "ManufacturingDuplicatedComponentInBOM", productToProduct, locale);
            Map<String, Object> result = ServiceUtil.returnSuccess();
            result.put("errorMessage", errorMessage);
            return result;
        }

        Map<String, Object> createCtx = new HashMap<>(parameters);
        createCtx.put("fromDate", fromDate);
        Map<String, Object> createResult;
        try {
            createResult = dispatcher.runSync("createProductAssoc", createCtx);
        } catch (GenericServiceException e) {
            Debug.logError(e, "Error calling createProductAssoc: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        if (ServiceUtil.isError(createResult)) {
            return ServiceUtil.returnError(ServiceUtil.getErrorMessage(createResult));
        }
        return ServiceUtil.returnSuccess();
    }

    /** Copies every (date-filtered) ProductAssoc of a product onto another product, via createBOMAssoc, ignoring per-row errors. */
    public static Map<String, Object> copyBOMAssocs(DispatchContext dctx, Map<String, ? extends Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Map<String, Object> parameters = UtilGenerics.cast(context);

        GenericValue lookupMap = delegator.makeValue("ProductAssoc");
        lookupMap.setPKFields(parameters);
        List<GenericValue> assocs;
        try {
            assocs = EntityQuery.use(delegator).from("ProductAssoc").where(lookupMap).filterByDate().queryList();
        } catch (GenericEntityException e) {
            Debug.logError(e, "Error finding ProductAssoc by and: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        for (GenericValue assoc : assocs) {
            Map<String, Object> createCtx = new HashMap<>(assoc);
            createCtx.put("productId", parameters.get("copyToProductId"));
            createCtx.put("userLogin", parameters.get("userLogin"));
            try {
                // break-on-error="false" in the original: log and continue on a per-row failure
                Map<String, Object> createResult = dispatcher.runSync("createBOMAssoc", dispatcher.getDispatchContext().makeValidContext("createBOMAssoc", ModelService.IN_PARAM, createCtx));
                if (ServiceUtil.isError(createResult)) {
                    Debug.logWarning("Error calling createBOMAssoc: " + ServiceUtil.getErrorMessage(createResult), MODULE);
                }
            } catch (GenericServiceException e) {
                Debug.logWarning(e, "Error calling createBOMAssoc: " + e.getMessage(), MODULE);
            }
        }
        return ServiceUtil.returnSuccess();
    }

    /** Creates a Product Manufacturing Rule, failing if one already exists for the same primary key. */
    public static Map<String, Object> addProductManufacturingRule(DispatchContext dctx, Map<String, ? extends Object> context) {
        Delegator delegator = dctx.getDelegator();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");
        Map<String, Object> parameters = UtilGenerics.cast(context);

        if (!dctx.getSecurity().hasEntityPermission("MANUFACTURING", "_CREATE", userLogin)) {
            return ServiceUtil.returnError(UtilProperties.getMessage(RESOURCE, "ManufacturingMachineGroupPermissionError", locale));
        }

        GenericValue lookupPKMap = delegator.makeValue("ProductManufacturingRule");
        lookupPKMap.setPKFields(parameters);
        GenericValue newEntity;
        try {
            newEntity = EntityQuery.use(delegator).from("ProductManufacturingRule").where(lookupPKMap).queryOne();
        } catch (GenericEntityException e) {
            Debug.logError(e, "Error finding ProductManufacturingRule by primary key: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        if (UtilValidate.isNotEmpty(newEntity) && UtilValidate.isNotEmpty(newEntity.get("ruleId"))) {
            return ServiceUtil.returnError(UtilProperties.getMessage(RESOURCE, "ManufacturingMachineGroupIdAlreadyExist", locale));
        }

        newEntity = delegator.makeValue("ProductManufacturingRule");
        String ruleId = (String) parameters.get("ruleId");
        if (UtilValidate.isEmpty(ruleId)) {
            ruleId = delegator.getNextSeqId("ProductManufacturingRule");
        }
        Map<String, Object> newFields = new HashMap<>(parameters);
        newFields.put("ruleId", ruleId);
        newEntity.setPKFields(newFields);
        newEntity.setNonPKFields(newFields);
        try {
            delegator.create(newEntity);
        } catch (GenericEntityException e) {
            Debug.logError(e, "Error creating ProductManufacturingRule: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        return ServiceUtil.returnSuccess();
    }

    /** Updates the non-PK fields of an existing Product Manufacturing Rule. */
    public static Map<String, Object> updateProductManufacturingRule(DispatchContext dctx, Map<String, ? extends Object> context) {
        Delegator delegator = dctx.getDelegator();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");
        Map<String, Object> parameters = UtilGenerics.cast(context);

        if (!dctx.getSecurity().hasEntityPermission("MANUFACTURING", "_UPDATE", userLogin)) {
            return ServiceUtil.returnError(UtilProperties.getMessage(RESOURCE, "ManufacturingMachineGroupPermissionError", locale));
        }

        GenericValue lookupPKMap = delegator.makeValue("ProductManufacturingRule");
        lookupPKMap.setPKFields(parameters);
        GenericValue lookedUpValue;
        try {
            lookedUpValue = EntityQuery.use(delegator).from("ProductManufacturingRule").where(lookupPKMap).queryOne();
        } catch (GenericEntityException e) {
            Debug.logError(e, "Error finding ProductManufacturingRule by primary key: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        lookedUpValue.setNonPKFields(parameters);
        try {
            delegator.store(lookedUpValue);
        } catch (GenericEntityException e) {
            Debug.logError(e, "Error storing ProductManufacturingRule: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        return ServiceUtil.returnSuccess();
    }

    /** Removes a Product Manufacturing Rule, if it exists. */
    public static Map<String, Object> deleteProductManufacturingRule(DispatchContext dctx, Map<String, ? extends Object> context) {
        Delegator delegator = dctx.getDelegator();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");
        Map<String, Object> parameters = UtilGenerics.cast(context);

        if (!dctx.getSecurity().hasEntityPermission("MANUFACTURING", "_DELETE", userLogin)) {
            return ServiceUtil.returnError(UtilProperties.getMessage(RESOURCE, "ManufacturingCalendarPermissionError", locale));
        }

        GenericValue lookupPKMap = delegator.makeValue("ProductManufacturingRule");
        lookupPKMap.setPKFields(parameters);
        GenericValue lookedUpValue;
        try {
            lookedUpValue = EntityQuery.use(delegator).from("ProductManufacturingRule").where(lookupPKMap).queryOne();
        } catch (GenericEntityException e) {
            Debug.logError(e, "Error finding ProductManufacturingRule by primary key: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        if (UtilValidate.isNotEmpty(lookedUpValue) && UtilValidate.isNotEmpty(lookedUpValue.get("ruleId"))) {
            try {
                delegator.removeValue(lookedUpValue);
            } catch (GenericEntityException e) {
                Debug.logError(e, "Error removing ProductManufacturingRule: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
        }
        return ServiceUtil.returnSuccess();
    }

    /**
     * Adds, updates, deletes or copies BOM components, dispatched on the UPDATE_MODE parameter.
     *
     * <p>Public (not merely package-visible) because the caller test class lives in the sibling
     * {@code com.ilscipio.scipio.manufacturing.test} package, which cannot see package-private members here.</p>
     */
    public static Map<String, Object> editBom(Delegator delegator, LocalDispatcher dispatcher, GenericValue userLogin,
            Map<String, Object> params, Locale locale) {
        String updateMode = (String) params.get("UPDATE_MODE");
        List<String> errorMessages = new LinkedList<>();
        Map<String, Object> context = new HashMap<>(params);
        context.put("userLogin", userLogin);

        try {
            if ("UPDATE".equals(updateMode)) {
                errorMessages.addAll(prepareUpdateBOMAssoc(params, context, locale));
                if (!errorMessages.isEmpty()) {
                    return ServiceUtil.returnError(errorMessages);
                }
                Map<String, Object> serviceResult = dispatcher.runSync("updateProductAssoc", dispatcher.getDispatchContext().makeValidContext("updateProductAssoc", ModelService.IN_PARAM, context));
                if (ServiceUtil.isError(serviceResult)) {
                    return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
                }
            } else if ("CREATE".equals(updateMode)) {
                errorMessages.addAll(prepareUpdateBOMAssoc(params, context, locale));
                if (!errorMessages.isEmpty()) {
                    return ServiceUtil.returnError(errorMessages);
                }
                Map<String, Object> serviceResult = dispatcher.runSync("createBOMAssoc", dispatcher.getDispatchContext().makeValidContext("createBOMAssoc", ModelService.IN_PARAM, context));
                if (ServiceUtil.isError(serviceResult)) {
                    return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
                }
                // NOTE: original XML comments out check-errors here; the error is only surfaced through
                // the deferred check at the bottom of this method, same as the original simple-method's
                // implicit end-of-method error check.
                Object errorMessage = serviceResult.get("errorMessage");
                if (UtilValidate.isNotEmpty(errorMessage)) {
                    errorMessages.add(errorMessage.toString());
                }
            } else if ("DELETE".equals(updateMode)) {
                errorMessages.addAll(prepareDeleteProductAssoc(params, context, locale));
                if (!errorMessages.isEmpty()) {
                    return ServiceUtil.returnError(errorMessages);
                }
                Map<String, Object> serviceResult = dispatcher.runSync("deleteProductAssoc", dispatcher.getDispatchContext().makeValidContext("deleteProductAssoc", ModelService.IN_PARAM, context));
                if (ServiceUtil.isError(serviceResult)) {
                    return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
                }
            } else if ("COPY".equals(updateMode)) {
                Map<String, Object> serviceResult = dispatcher.runSync("copyBOMAssocs", dispatcher.getDispatchContext().makeValidContext("copyBOMAssocs", ModelService.IN_PARAM, context));
                if (ServiceUtil.isError(serviceResult)) {
                    return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
                }
            }
        } catch (GenericServiceException e) {
            Debug.logError(e, "Error editing BOM: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }

        if (!errorMessages.isEmpty()) {
            return ServiceUtil.returnError(errorMessages);
        }
        return ServiceUtil.returnSuccess();
    }

    /** Request event wrapper for {@link #editBom}. */
    public static String eventEditBOM(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> params = UtilGenerics.cast(UtilHttp.getCombinedMap(request));

        Map<String, Object> result = editBom(delegator, dispatcher, userLogin, params, locale);
        if (ServiceUtil.isError(result)) {
            Object errorMessageList = result.get("errorMessageList");
            if (UtilValidate.isNotEmpty(errorMessageList)) {
                request.setAttribute("_ERROR_MESSAGE_LIST_", errorMessageList);
            } else {
                request.setAttribute("_ERROR_MESSAGE_", ServiceUtil.getErrorMessage(result));
            }
            return "error";
        }
        return "success";
    }

    /* ===== BomMapProcs.xml equivalents ===== */

    /** Equivalent of the prepareUpdateBOMAssoc simple-map-processor. Returns the accumulated error messages. */
    private static List<String> prepareUpdateBOMAssoc(Map<String, Object> in, Map<String, Object> out, Locale locale) {
        List<String> errorMessages = new LinkedList<>();
        copyField(in, out, "productId");
        requireNotEmpty(in, "productId", locale, errorMessages, "ManufacturingProductId");
        copyField(in, out, "productIdTo");
        requireNotEmpty(in, "productIdTo", locale, errorMessages, "ManufacturingProductIdTo");
        copyField(in, out, "productAssocTypeId");
        requireNotEmpty(in, "productAssocTypeId", locale, errorMessages, "ManufacturingProductAssocTypeIdMissing");
        convertField(in, out, "fromDate", "Timestamp", locale, errorMessages, "ManufacturingFromDateBadFormat");
        convertField(in, out, "thruDate", "Timestamp", locale, errorMessages, "ManufacturingThruDateBadFormat");
        convertField(in, out, "quantity", "BigDecimal", locale, errorMessages, "ManufacturingQuantityBadFormat");
        convertField(in, out, "scrapFactor", "BigDecimal", locale, errorMessages, "ManufacturingScrapFactorBadFormat");
        convertField(in, out, "sequenceNum", "Long", locale, errorMessages, "ManufacturingSequenceNumBadFormat");
        return errorMessages;
    }

    /** Equivalent of the prepareDeleteProductAssoc simple-map-processor. Returns the accumulated error messages. */
    private static List<String> prepareDeleteProductAssoc(Map<String, Object> in, Map<String, Object> out, Locale locale) {
        List<String> errorMessages = new LinkedList<>();
        convertField(in, out, "fromDate", "Timestamp", locale, errorMessages, "ManufacturingFromDateBadFormat");
        return errorMessages;
    }

    /** Mirrors the simple-map-processor &lt;copy/&gt; operation (blank strings become null, in and out fields share a name). */
    private static void copyField(Map<String, Object> in, Map<String, Object> out, String field) {
        Object value = in.get(field);
        if (value instanceof String && ((String) value).isEmpty()) {
            out.put(field, null);
            return;
        }
        out.put(field, value);
    }

    /** Mirrors the simple-map-processor &lt;not-empty&gt; operation: checks the *input* field, adds a label on failure. */
    private static void requireNotEmpty(Map<String, Object> in, String field, Locale locale, List<String> errorMessages, String labelKey) {
        Object value = in.get(field);
        boolean empty = (value instanceof String) ? UtilValidate.isEmpty((String) value) : (value == null);
        if (empty) {
            errorMessages.add(UtilProperties.getMessage(RESOURCE, labelKey, locale));
        }
    }

    /** Mirrors the simple-map-processor &lt;convert&gt; operation using {@link ObjectType#simpleTypeConvert}. */
    private static void convertField(Map<String, Object> in, Map<String, Object> out, String field, String type, Locale locale,
            List<String> errorMessages, String labelKey) {
        Object value = in.get(field);
        if (value == null || (value instanceof String && ((String) value).isEmpty())) {
            out.put(field, null);
            return;
        }
        Object converted;
        try {
            converted = ObjectType.simpleTypeConvert(value, type, null, locale);
        } catch (GeneralException e) {
            errorMessages.add(UtilProperties.getMessage(RESOURCE, labelKey, locale));
            return;
        }
        if (converted != null) {
            out.put(field, converted);
        }
    }

}
