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
import org.ofbiz.base.util.GroovyUtil;
import org.ofbiz.base.util.UtilHttp;
import org.ofbiz.base.util.UtilMisc;
import org.ofbiz.base.util.UtilProperties;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.GenericValue;
import org.ofbiz.entity.condition.EntityCondition;
import org.ofbiz.entity.util.EntityQuery;
import org.ofbiz.entity.util.EntityUtil;
import org.ofbiz.order.shoppingcart.CheckOutEvents;
import org.ofbiz.order.shoppingcart.ShoppingCartEvents;
import org.ofbiz.service.LocalDispatcher;
import org.ofbiz.service.ServiceUtil;

/**
 * Auto-generated event class from Simple Method XML.
 *
 * <p>Generated from: component://accounting/script/org/ofbiz/accounting/test/InvoicePerShipmentTests.xml</p>
 *
 * <p>SCIPIO: 4.0.0: Auto-generated.</p>
 */
public class InvoicePerShipmentTests {

    private static final String MODULE = InvoicePerShipmentTests.class.getName();


    /**
     * Test Invoice Per Shipment Set False
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String testInvoicePerShipmentSetFalse(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        try {
            Map<String, Object> scriptContext = new HashMap<>();
            scriptContext.put("delegator", delegator);
            scriptContext.put("dispatcher", dispatcher);
            scriptContext.put("locale", locale);
            scriptContext.put("userLogin", userLogin);
            scriptContext.put("context", context);
            scriptContext.put("parameters", context);
            scriptContext.put("request", request);
            scriptContext.put("response", response);
            Object scriptResult = GroovyUtil.eval("import org.ofbiz.base.util.UtilProperties;\n            UtilProperties.setPropertyValueInMemory(\"AccountingConfig\", \"create.invoice.per.shipment\", \"N\");", scriptContext);
        } catch (Exception e) {
            Debug.logError(e, "Error executing Groovy script: " + e.getMessage(), MODULE);
        }
        Debug.logInfo("===== >>> Set Accounting.properties / create.invoice.per.shipment = N", MODULE);
        request.getSession().setAttribute("orderMode", null);
        request = (HttpServletRequest) context.get("request");
        response = (HttpServletResponse) context.get("response");
        Object result = null;
        try {
            result = ShoppingCartEvents.routeOrderEntry(request, response);
        } catch (Exception e) {
            Debug.logError(e, "Error calling ShoppingCartEvents.routeOrderEntry: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        Debug.logInfo("===== >>> Event : routeOrderEntry, Response : " + result, MODULE);
        try {
            userLogin = EntityQuery.use(delegator)
                    .from("UserLogin")
                    .where(UtilMisc.toMap("userLoginId", "admin"))
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying UserLogin: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        try {
            Map<String, Object> scriptContext = new HashMap<>();
            scriptContext.put("delegator", delegator);
            scriptContext.put("dispatcher", dispatcher);
            scriptContext.put("locale", locale);
            scriptContext.put("userLogin", userLogin);
            scriptContext.put("context", context);
            scriptContext.put("parameters", context);
            scriptContext.put("request", request);
            scriptContext.put("response", response);
            Object scriptResult = GroovyUtil.eval("request.setParameter(\"orderMode\", \"SALES_ORDER\");\n            request.setParameter(\"productStoreId\", \"ScipioShop\");\n            request.setParameter(\"partyId\", \"DemoCustomer\");\n            request.setParameter(\"currencyUom\", \"USD\");\n            session = request.getSession();\n            session.setAttribute(\"userLogin\", userLogin);", scriptContext);
        } catch (Exception e) {
            Debug.logError(e, "Error executing Groovy script: " + e.getMessage(), MODULE);
        }
        try {
            result = ShoppingCartEvents.initializeOrderEntry(request, response);
        } catch (Exception e) {
            Debug.logError(e, "Error calling ShoppingCartEvents.initializeOrderEntry: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        Debug.logInfo("===== >>> Event : initializeOrderEntry, Response : " + result, MODULE);
        try {
            result = ShoppingCartEvents.setOrderCurrencyAgreementShipDates(request, response);
        } catch (Exception e) {
            Debug.logError(e, "Error calling ShoppingCartEvents.setOrderCurrencyAgreementShipDates: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        Debug.logInfo("===== >>> Event : setOrderCurrencyAgreementShipDates, Response : " + result, MODULE);
        try {
            Map<String, Object> scriptContext = new HashMap<>();
            scriptContext.put("delegator", delegator);
            scriptContext.put("dispatcher", dispatcher);
            scriptContext.put("locale", locale);
            scriptContext.put("userLogin", userLogin);
            scriptContext.put("context", context);
            scriptContext.put("parameters", context);
            scriptContext.put("request", request);
            scriptContext.put("response", response);
            Object scriptResult = GroovyUtil.eval("request.setParameter(\"add_product_id\", \"PH-1000\");", scriptContext);
        } catch (Exception e) {
            Debug.logError(e, "Error executing Groovy script: " + e.getMessage(), MODULE);
        }
        try {
            result = ShoppingCartEvents.addToCart(request, response);
        } catch (Exception e) {
            Debug.logError(e, "Error calling ShoppingCartEvents.addToCart: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        Debug.logInfo("===== >>> Event : addToCart, Response : " + result, MODULE);
        try {
            Map<String, Object> scriptContext = new HashMap<>();
            scriptContext.put("delegator", delegator);
            scriptContext.put("dispatcher", dispatcher);
            scriptContext.put("locale", locale);
            scriptContext.put("userLogin", userLogin);
            scriptContext.put("context", context);
            scriptContext.put("parameters", context);
            scriptContext.put("request", request);
            scriptContext.put("response", response);
            Object scriptResult = GroovyUtil.eval("request.setParameter(\"checkoutpage\", \"quick\");\n            request.setParameter(\"shipping_contact_mech_id\", \"9015\");\n            request.setParameter(\"shipping_method\", \"GROUND@UPS\");\n            request.setParameter(\"checkOutPaymentId\", \"EXT_COD\");\n            request.setParameter(\"is_gift\", \"false\");\n            request.setParameter(\"may_split\", \"false\");", scriptContext);
        } catch (Exception e) {
            Debug.logError(e, "Error executing Groovy script: " + e.getMessage(), MODULE);
        }
        request.setAttribute("shoppingCart", null);
        try {
            result = CheckOutEvents.setQuickCheckOutOptions(request, response);
        } catch (Exception e) {
            Debug.logError(e, "Error calling CheckOutEvents.setQuickCheckOutOptions: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        Debug.logInfo("===== >>> Event : setQuickCheckOutOptions, Response : " + result, MODULE);
        try {
            result = CheckOutEvents.createOrder(request, response);
        } catch (Exception e) {
            Debug.logError(e, "Error calling CheckOutEvents.createOrder: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        Debug.logInfo("===== >>> Event : createOrder, Response : " + result, MODULE);
        try {
            result = CheckOutEvents.processPayment(request, response);
        } catch (Exception e) {
            Debug.logError(e, "Error calling CheckOutEvents.processPayment: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        Debug.logInfo("===== >>> Event : processPayment, Response : " + result, MODULE);
        // TODO: Convert <call-service-asynch> element
        try {
            result = ShoppingCartEvents.destroyCart(request, response);
        } catch (Exception e) {
            Debug.logError(e, "Error calling ShoppingCartEvents.destroyCart: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        Debug.logInfo("===== >>> Event : destroyCart, Response = " + result, MODULE);
        List<GenericValue> orderHeaders = null;
        try {
            orderHeaders = EntityQuery.use(delegator)
                    .from("OrderHeader")
                    .queryList();
        } catch (Exception e) {
            Debug.logError(e, "Error querying OrderHeader: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        GenericValue orderHeader = EntityUtil.getFirst((List<GenericValue>) orderHeaders);
        Debug.logInfo("xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx : " + orderHeader, MODULE);
        try {
            Map<String, Object> scriptContext = new HashMap<>();
            scriptContext.put("delegator", delegator);
            scriptContext.put("dispatcher", dispatcher);
            scriptContext.put("locale", locale);
            scriptContext.put("userLogin", userLogin);
            scriptContext.put("context", context);
            scriptContext.put("parameters", context);
            scriptContext.put("request", request);
            scriptContext.put("response", response);
            Object scriptResult = GroovyUtil.eval("import org.ofbiz.shipment.packing.PackingSession;\n            packingSession = new PackingSession(dispatcher, userLogin);\n            packingSession.setPrimaryOrderId(orderHeader.get(\"orderId\"));\n            packingSession.setPrimaryShipGroupSeqId(\"00001\");\n            parameters.put(\"packingSession\", packingSession);", scriptContext);
        } catch (Exception e) {
            Debug.logError(e, "Error executing Groovy script: " + e.getMessage(), MODULE);
        }
        Map<String, Object> packInput = new HashMap<>();
        packInput.put("orderId", ((Map<String, Object>) orderHeader).get("orderId"));
        packInput.put("shipGroupSeqId", "00001");
        packInput.put("packingSession", context.get("packingSession"));
        packInput.put("nextPackageSeq", 1);
        packInput.put("userLogin", userLogin);
        packInput.put("pkg", "1");
        packInput.put("qty", "1");
        packInput.put("prd", "PH-1000");
        packInput.put("ite", "00001");
        packInput.put("wgt", "0");
        packInput.put("numPackages", "1");
        Object responseMessage = null;
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("packBulkItems", packInput);
            if (ServiceUtil.isError(serviceResult)) {
                String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                request.setAttribute("_ERROR_MESSAGE_", errMsg);
                return "error";
            }
            responseMessage = serviceResult.get("responseMessage");
        } catch (Exception e) {
            Debug.logError(e, "Error calling packBulkItems: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        Debug.logInfo("===== >>> Service: packBulkItems, Response = " + responseMessage, MODULE);
        Map<String, Object> completePackInput = new HashMap<>();
        // set-service-fields from "packInput" to "completePackInput" for service "completePack"
        completePackInput.putAll(UtilMisc.toMap(packInput));
        Object shipmentId = null;
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("completePack", completePackInput);
            if (ServiceUtil.isError(serviceResult)) {
                String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                request.setAttribute("_ERROR_MESSAGE_", errMsg);
                return "error";
            }
            shipmentId = serviceResult.get("shipmentId");
        } catch (Exception e) {
            Debug.logError(e, "Error calling completePack: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        Debug.logInfo("===== >>> Service: completePack, shipmentId = " + shipmentId, MODULE);
        List<GenericValue> invoices = null;
        try {
            invoices = EntityQuery.use(delegator)
                    .from("OrderItemBillingAndInvoiceAndItem")
                    .where(UtilMisc.toMap("orderId", ((Map<String, Object>) orderHeader).get("orderId")))
                    .queryList();
        } catch (Exception e) {
            Debug.logError(e, "Error querying OrderItemBillingAndInvoiceAndItem: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        assert UtilValidate.isEmpty(invoices) : "Assertion failed: if-empty";
        if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
            return "error";
        }

        return "success";
    }


    /**
     * Test Invoice Per Shipment Set True
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String testInvoicePerShipmentSetTrue(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        try {
            Map<String, Object> scriptContext = new HashMap<>();
            scriptContext.put("delegator", delegator);
            scriptContext.put("dispatcher", dispatcher);
            scriptContext.put("locale", locale);
            scriptContext.put("userLogin", userLogin);
            scriptContext.put("context", context);
            scriptContext.put("parameters", context);
            scriptContext.put("request", request);
            scriptContext.put("response", response);
            Object scriptResult = GroovyUtil.eval("import org.ofbiz.base.util.UtilProperties;\n            UtilProperties.setPropertyValueInMemory(\"AccountingConfig\", \"create.invoice.per.shipment\", \"Y\");", scriptContext);
        } catch (Exception e) {
            Debug.logError(e, "Error executing Groovy script: " + e.getMessage(), MODULE);
        }
        Debug.logInfo("===== >>> Set Accounting.properties / create.invoice.per.shipment = Y", MODULE);
        request.getSession().setAttribute("orderMode", null);
        request = (HttpServletRequest) context.get("request");
        response = (HttpServletResponse) context.get("response");
        Object result = null;
        try {
            result = ShoppingCartEvents.routeOrderEntry(request, response);
        } catch (Exception e) {
            Debug.logError(e, "Error calling ShoppingCartEvents.routeOrderEntry: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        Debug.logInfo("===== >>> Event : routeOrderEntry, Response = " + result, MODULE);
        try {
            userLogin = EntityQuery.use(delegator)
                    .from("UserLogin")
                    .where(UtilMisc.toMap("userLoginId", "admin"))
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying UserLogin: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        try {
            Map<String, Object> scriptContext = new HashMap<>();
            scriptContext.put("delegator", delegator);
            scriptContext.put("dispatcher", dispatcher);
            scriptContext.put("locale", locale);
            scriptContext.put("userLogin", userLogin);
            scriptContext.put("context", context);
            scriptContext.put("parameters", context);
            scriptContext.put("request", request);
            scriptContext.put("response", response);
            Object scriptResult = GroovyUtil.eval("request.setParameter(\"orderMode\", \"SALES_ORDER\");\n            request.setParameter(\"productStoreId\", \"ScipioShop\");\n            request.setParameter(\"partyId\", \"DemoCustomer\");\n            request.setParameter(\"currencyUom\", \"USD\");\n            session = request.getSession();\n            session.setAttribute(\"userLogin\", userLogin);", scriptContext);
        } catch (Exception e) {
            Debug.logError(e, "Error executing Groovy script: " + e.getMessage(), MODULE);
        }
        try {
            result = ShoppingCartEvents.initializeOrderEntry(request, response);
        } catch (Exception e) {
            Debug.logError(e, "Error calling ShoppingCartEvents.initializeOrderEntry: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        Debug.logInfo("===== >>> Event : initializeOrderEntry, Response = " + result, MODULE);
        try {
            result = ShoppingCartEvents.setOrderCurrencyAgreementShipDates(request, response);
        } catch (Exception e) {
            Debug.logError(e, "Error calling ShoppingCartEvents.setOrderCurrencyAgreementShipDates: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        Debug.logInfo("===== >>> Event : setOrderCurrencyAgreementShipDates, Response = " + result, MODULE);
        try {
            Map<String, Object> scriptContext = new HashMap<>();
            scriptContext.put("delegator", delegator);
            scriptContext.put("dispatcher", dispatcher);
            scriptContext.put("locale", locale);
            scriptContext.put("userLogin", userLogin);
            scriptContext.put("context", context);
            scriptContext.put("parameters", context);
            scriptContext.put("request", request);
            scriptContext.put("response", response);
            Object scriptResult = GroovyUtil.eval("request.setParameter(\"add_product_id\", \"PH-1000\");", scriptContext);
        } catch (Exception e) {
            Debug.logError(e, "Error executing Groovy script: " + e.getMessage(), MODULE);
        }
        try {
            result = ShoppingCartEvents.addToCart(request, response);
        } catch (Exception e) {
            Debug.logError(e, "Error calling ShoppingCartEvents.addToCart: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        Debug.logInfo("===== >>> Event : addToCart, Response = " + result, MODULE);
        try {
            Map<String, Object> scriptContext = new HashMap<>();
            scriptContext.put("delegator", delegator);
            scriptContext.put("dispatcher", dispatcher);
            scriptContext.put("locale", locale);
            scriptContext.put("userLogin", userLogin);
            scriptContext.put("context", context);
            scriptContext.put("parameters", context);
            scriptContext.put("request", request);
            scriptContext.put("response", response);
            Object scriptResult = GroovyUtil.eval("request.setParameter(\"checkoutpage\", \"quick\");\n            request.setParameter(\"shipping_contact_mech_id\", \"9015\");\n            request.setParameter(\"shipping_method\", \"GROUND@UPS\");\n            request.setParameter(\"checkOutPaymentId\", \"EXT_COD\");\n            request.setParameter(\"is_gift\", \"false\");\n            request.setParameter(\"may_split\", \"false\");", scriptContext);
        } catch (Exception e) {
            Debug.logError(e, "Error executing Groovy script: " + e.getMessage(), MODULE);
        }
        request.setAttribute("shoppingCart", null);
        try {
            result = CheckOutEvents.setQuickCheckOutOptions(request, response);
        } catch (Exception e) {
            Debug.logError(e, "Error calling CheckOutEvents.setQuickCheckOutOptions: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        Debug.logInfo("===== >>> Event : setQuickCheckOutOptions, Response = " + result, MODULE);
        try {
            result = CheckOutEvents.createOrder(request, response);
        } catch (Exception e) {
            Debug.logError(e, "Error calling CheckOutEvents.createOrder: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        Debug.logInfo("===== >>> Event : createOrder, Response = " + result, MODULE);
        try {
            result = CheckOutEvents.processPayment(request, response);
        } catch (Exception e) {
            Debug.logError(e, "Error calling CheckOutEvents.processPayment: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        Debug.logInfo("===== >>> Event : processPayment, Response = " + result, MODULE);
        // TODO: Convert <call-service-asynch> element
        try {
            result = ShoppingCartEvents.destroyCart(request, response);
        } catch (Exception e) {
            Debug.logError(e, "Error calling ShoppingCartEvents.destroyCart: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        Debug.logInfo("===== >>> Event : destroyCart, Response = " + result, MODULE);
        List<GenericValue> orderHeaders = null;
        try {
            orderHeaders = EntityQuery.use(delegator)
                    .from("OrderHeader")
                    .queryList();
        } catch (Exception e) {
            Debug.logError(e, "Error querying OrderHeader: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        GenericValue orderHeader = EntityUtil.getFirst((List<GenericValue>) orderHeaders);
        try {
            Map<String, Object> scriptContext = new HashMap<>();
            scriptContext.put("delegator", delegator);
            scriptContext.put("dispatcher", dispatcher);
            scriptContext.put("locale", locale);
            scriptContext.put("userLogin", userLogin);
            scriptContext.put("context", context);
            scriptContext.put("parameters", context);
            scriptContext.put("request", request);
            scriptContext.put("response", response);
            Object scriptResult = GroovyUtil.eval("import org.ofbiz.shipment.packing.PackingSession;\n            packingSession = new PackingSession(dispatcher, userLogin);\n            packingSession.setPrimaryOrderId(orderHeader.get(\"orderId\"));\n            packingSession.setPrimaryShipGroupSeqId(\"00001\");\n            parameters.put(\"packingSession\", packingSession);", scriptContext);
        } catch (Exception e) {
            Debug.logError(e, "Error executing Groovy script: " + e.getMessage(), MODULE);
        }
        Map<String, Object> packInput = new HashMap<>();
        packInput.put("orderId", ((Map<String, Object>) orderHeader).get("orderId"));
        packInput.put("shipGroupSeqId", "00001");
        packInput.put("packingSession", context.get("packingSession"));
        packInput.put("nextPackageSeq", 1);
        packInput.put("userLogin", userLogin);
        packInput.put("pkg", "1");
        packInput.put("qty", "1");
        packInput.put("prd", "PH-1000");
        packInput.put("ite", "00001");
        packInput.put("wgt", "0");
        packInput.put("numPackages", "1");
        Object responseMessage = null;
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("packBulkItems", packInput);
            if (ServiceUtil.isError(serviceResult)) {
                String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                request.setAttribute("_ERROR_MESSAGE_", errMsg);
                return "error";
            }
            responseMessage = serviceResult.get("responseMessage");
        } catch (Exception e) {
            Debug.logError(e, "Error calling packBulkItems: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        Debug.logInfo("===== >>> Service: packBulkItems, Response = " + responseMessage, MODULE);
        Map<String, Object> completePackInput = new HashMap<>();
        // set-service-fields from "packInput" to "completePackInput" for service "completePack"
        completePackInput.putAll(UtilMisc.toMap(packInput));
        Object shipmentId = null;
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("completePack", completePackInput);
            if (ServiceUtil.isError(serviceResult)) {
                String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                request.setAttribute("_ERROR_MESSAGE_", errMsg);
                return "error";
            }
            shipmentId = serviceResult.get("shipmentId");
        } catch (Exception e) {
            Debug.logError(e, "Error calling completePack: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        Debug.logInfo("===== >>> Service: completePack, shipmentId = " + shipmentId, MODULE);
        List<GenericValue> invoices = null;
        try {
            invoices = EntityQuery.use(delegator)
                    .from("OrderItemBillingAndInvoiceAndItem")
                    .where(UtilMisc.toMap("orderId", ((Map<String, Object>) orderHeader).get("orderId")))
                    .queryList();
        } catch (Exception e) {
            Debug.logError(e, "Error querying OrderItemBillingAndInvoiceAndItem: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        assert !(UtilValidate.isEmpty(invoices)) : "Assertion failed: not if-empty";
        if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
            return "error";
        }

        return "success";
    }


    /**
     * Test Invoice Per Shipment Set Order False
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String testInvoicePerShipmentSetOrderFalse(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        request.getSession().setAttribute("orderMode", null);
        request = (HttpServletRequest) context.get("request");
        response = (HttpServletResponse) context.get("response");
        Object result = null;
        try {
            result = ShoppingCartEvents.routeOrderEntry(request, response);
        } catch (Exception e) {
            Debug.logError(e, "Error calling ShoppingCartEvents.routeOrderEntry: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        Debug.logInfo("===== >>> Event : routeOrderEntry, Response = " + result, MODULE);
        try {
            userLogin = EntityQuery.use(delegator)
                    .from("UserLogin")
                    .where(UtilMisc.toMap("userLoginId", "admin"))
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying UserLogin: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        try {
            Map<String, Object> scriptContext = new HashMap<>();
            scriptContext.put("delegator", delegator);
            scriptContext.put("dispatcher", dispatcher);
            scriptContext.put("locale", locale);
            scriptContext.put("userLogin", userLogin);
            scriptContext.put("context", context);
            scriptContext.put("parameters", context);
            scriptContext.put("request", request);
            scriptContext.put("response", response);
            Object scriptResult = GroovyUtil.eval("request.setParameter(\"orderMode\", \"SALES_ORDER\");\n            request.setParameter(\"productStoreId\", \"ScipioShop\");\n            request.setParameter(\"partyId\", \"DemoCustomer\");\n            request.setParameter(\"currencyUom\", \"USD\");\n            session = request.getSession();\n            session.setAttribute(\"userLogin\", userLogin);", scriptContext);
        } catch (Exception e) {
            Debug.logError(e, "Error executing Groovy script: " + e.getMessage(), MODULE);
        }
        try {
            result = ShoppingCartEvents.initializeOrderEntry(request, response);
        } catch (Exception e) {
            Debug.logError(e, "Error calling ShoppingCartEvents.initializeOrderEntry: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        Debug.logInfo("===== >>> Event : initializeOrderEntry, Response = " + result, MODULE);
        try {
            result = ShoppingCartEvents.setOrderCurrencyAgreementShipDates(request, response);
        } catch (Exception e) {
            Debug.logError(e, "Error calling ShoppingCartEvents.setOrderCurrencyAgreementShipDates: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        Debug.logInfo("===== >>> Event : setOrderCurrencyAgreementShipDates, Response = " + result, MODULE);
        try {
            Map<String, Object> scriptContext = new HashMap<>();
            scriptContext.put("delegator", delegator);
            scriptContext.put("dispatcher", dispatcher);
            scriptContext.put("locale", locale);
            scriptContext.put("userLogin", userLogin);
            scriptContext.put("context", context);
            scriptContext.put("parameters", context);
            scriptContext.put("request", request);
            scriptContext.put("response", response);
            Object scriptResult = GroovyUtil.eval("request.setParameter(\"add_product_id\", \"CAM-2644\");", scriptContext);
        } catch (Exception e) {
            Debug.logError(e, "Error executing Groovy script: " + e.getMessage(), MODULE);
        }
        try {
            result = ShoppingCartEvents.addToCart(request, response);
        } catch (Exception e) {
            Debug.logError(e, "Error calling ShoppingCartEvents.addToCart: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        Debug.logInfo("===== >>> Event : addToCart, Response = " + result, MODULE);
        try {
            Map<String, Object> scriptContext = new HashMap<>();
            scriptContext.put("delegator", delegator);
            scriptContext.put("dispatcher", dispatcher);
            scriptContext.put("locale", locale);
            scriptContext.put("userLogin", userLogin);
            scriptContext.put("context", context);
            scriptContext.put("parameters", context);
            scriptContext.put("request", request);
            scriptContext.put("response", response);
            Object scriptResult = GroovyUtil.eval("request.setParameter(\"checkoutpage\", \"quick\");\n            request.setParameter(\"shipping_contact_mech_id\", \"9015\");\n            request.setParameter(\"shipping_method\", \"GROUND@UPS\");\n            request.setParameter(\"checkOutPaymentId\", \"EXT_COD\");\n            request.setParameter(\"is_gift\", \"false\");\n            request.setParameter(\"may_split\", \"false\");", scriptContext);
        } catch (Exception e) {
            Debug.logError(e, "Error executing Groovy script: " + e.getMessage(), MODULE);
        }
        request.setAttribute("shoppingCart", null);
        try {
            result = CheckOutEvents.setQuickCheckOutOptions(request, response);
        } catch (Exception e) {
            Debug.logError(e, "Error calling CheckOutEvents.setQuickCheckOutOptions: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        Debug.logInfo("===== >>> Event : setQuickCheckOutOptions, Response = " + result, MODULE);
        try {
            result = CheckOutEvents.createOrder(request, response);
        } catch (Exception e) {
            Debug.logError(e, "Error calling CheckOutEvents.createOrder: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        Debug.logInfo("===== >>> Event : createOrder, Response = " + result, MODULE);
        try {
            result = CheckOutEvents.processPayment(request, response);
        } catch (Exception e) {
            Debug.logError(e, "Error calling CheckOutEvents.processPayment: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        Debug.logInfo("===== >>> Event : processPayment, Response = " + result, MODULE);
        // TODO: Convert <call-service-asynch> element
        try {
            result = ShoppingCartEvents.destroyCart(request, response);
        } catch (Exception e) {
            Debug.logError(e, "Error calling ShoppingCartEvents.destroyCart: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        Debug.logInfo("===== >>> Event : destroyCart, Response = " + result, MODULE);
        List<GenericValue> orderHeaders = null;
        try {
            orderHeaders = EntityQuery.use(delegator)
                    .from("OrderHeader")
                    .queryList();
        } catch (Exception e) {
            Debug.logError(e, "Error querying OrderHeader: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        GenericValue orderHeader = EntityUtil.getFirst((List<GenericValue>) orderHeaders);
        Map<String, Object> orderInput = new HashMap<>();
        orderInput.put("orderId", ((Map<String, Object>) orderHeader).get("orderId"));
        orderInput.put("invoicePerShipment", "N");
        orderInput.put("userLogin", userLogin);
        Object responseMessage = null;
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("updateOrderHeader", orderInput);
            if (ServiceUtil.isError(serviceResult)) {
                String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                request.setAttribute("_ERROR_MESSAGE_", errMsg);
                return "error";
            }
            responseMessage = serviceResult.get("responseMessage");
        } catch (Exception e) {
            Debug.logError(e, "Error calling updateOrderHeader: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        Debug.logInfo("===== >>> Service : updateOrderHeader / invoicePerShipment = N,  Response = " + responseMessage, MODULE);
        try {
            Map<String, Object> scriptContext = new HashMap<>();
            scriptContext.put("delegator", delegator);
            scriptContext.put("dispatcher", dispatcher);
            scriptContext.put("locale", locale);
            scriptContext.put("userLogin", userLogin);
            scriptContext.put("context", context);
            scriptContext.put("parameters", context);
            scriptContext.put("request", request);
            scriptContext.put("response", response);
            Object scriptResult = GroovyUtil.eval("import org.ofbiz.shipment.packing.PackingSession;\n            packingSession = new PackingSession(dispatcher, userLogin);\n            packingSession.setPrimaryOrderId(orderHeader.get(\"orderId\"));\n            packingSession.setPrimaryShipGroupSeqId(\"00001\");\n            parameters.put(\"packingSession\", packingSession);", scriptContext);
        } catch (Exception e) {
            Debug.logError(e, "Error executing Groovy script: " + e.getMessage(), MODULE);
        }
        Map<String, Object> packInput = new HashMap<>();
        packInput.put("orderId", ((Map<String, Object>) orderHeader).get("orderId"));
        packInput.put("shipGroupSeqId", "00001");
        packInput.put("packingSession", context.get("packingSession"));
        packInput.put("nextPackageSeq", 1);
        packInput.put("userLogin", userLogin);
        packInput.put("pkg", "1");
        packInput.put("qty", "1");
        packInput.put("prd", "CAM-2644");
        packInput.put("ite", "00001");
        packInput.put("wgt", "0");
        packInput.put("numPackages", "1");
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("packBulkItems", packInput);
            if (ServiceUtil.isError(serviceResult)) {
                String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                request.setAttribute("_ERROR_MESSAGE_", errMsg);
                return "error";
            }
            responseMessage = serviceResult.get("responseMessage");
        } catch (Exception e) {
            Debug.logError(e, "Error calling packBulkItems: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        Debug.logInfo("===== >>> Service: packBulkItems, Response = " + responseMessage, MODULE);
        Map<String, Object> completePackInput = new HashMap<>();
        // set-service-fields from "packInput" to "completePackInput" for service "completePack"
        completePackInput.putAll(UtilMisc.toMap(packInput));
        Object shipmentId = null;
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("completePack", completePackInput);
            if (ServiceUtil.isError(serviceResult)) {
                String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                request.setAttribute("_ERROR_MESSAGE_", errMsg);
                return "error";
            }
            shipmentId = serviceResult.get("shipmentId");
        } catch (Exception e) {
            Debug.logError(e, "Error calling completePack: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        Debug.logInfo("===== >>> Service: completePack, shipmentId = " + shipmentId, MODULE);
        List<GenericValue> invoices = null;
        try {
            invoices = EntityQuery.use(delegator)
                    .from("OrderItemBillingAndInvoiceAndItem")
                    .where(UtilMisc.toMap("orderId", ((Map<String, Object>) orderHeader).get("orderId")))
                    .queryList();
        } catch (Exception e) {
            Debug.logError(e, "Error querying OrderItemBillingAndInvoiceAndItem: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        assert UtilValidate.isEmpty(invoices) : "Assertion failed: if-empty";
        if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
            return "error";
        }

        return "success";
    }


    /**
     * Test Invoice Per Shipment Set Order True
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String testInvoicePerShipmentSetOrderTrue(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        request.getSession().setAttribute("orderMode", null);
        request = (HttpServletRequest) context.get("request");
        response = (HttpServletResponse) context.get("response");
        Object result = null;
        try {
            result = ShoppingCartEvents.routeOrderEntry(request, response);
        } catch (Exception e) {
            Debug.logError(e, "Error calling ShoppingCartEvents.routeOrderEntry: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        Debug.logInfo("===== >>> Event : routeOrderEntry, Response = " + result, MODULE);
        try {
            userLogin = EntityQuery.use(delegator)
                    .from("UserLogin")
                    .where(UtilMisc.toMap("userLoginId", "admin"))
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying UserLogin: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        try {
            Map<String, Object> scriptContext = new HashMap<>();
            scriptContext.put("delegator", delegator);
            scriptContext.put("dispatcher", dispatcher);
            scriptContext.put("locale", locale);
            scriptContext.put("userLogin", userLogin);
            scriptContext.put("context", context);
            scriptContext.put("parameters", context);
            scriptContext.put("request", request);
            scriptContext.put("response", response);
            Object scriptResult = GroovyUtil.eval("request.setParameter(\"orderMode\", \"SALES_ORDER\");\n            request.setParameter(\"productStoreId\", \"ScipioShop\");\n            request.setParameter(\"partyId\", \"DemoCustomer\");\n            request.setParameter(\"currencyUom\", \"USD\");\n            session = request.getSession();\n            session.setAttribute(\"userLogin\", userLogin);", scriptContext);
        } catch (Exception e) {
            Debug.logError(e, "Error executing Groovy script: " + e.getMessage(), MODULE);
        }
        try {
            result = ShoppingCartEvents.initializeOrderEntry(request, response);
        } catch (Exception e) {
            Debug.logError(e, "Error calling ShoppingCartEvents.initializeOrderEntry: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        Debug.logInfo("===== >>> Event : initializeOrderEntry, Response = " + result, MODULE);
        try {
            result = ShoppingCartEvents.setOrderCurrencyAgreementShipDates(request, response);
        } catch (Exception e) {
            Debug.logError(e, "Error calling ShoppingCartEvents.setOrderCurrencyAgreementShipDates: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        Debug.logInfo("===== >>> Event : setOrderCurrencyAgreementShipDates, Response = " + result, MODULE);
        try {
            Map<String, Object> scriptContext = new HashMap<>();
            scriptContext.put("delegator", delegator);
            scriptContext.put("dispatcher", dispatcher);
            scriptContext.put("locale", locale);
            scriptContext.put("userLogin", userLogin);
            scriptContext.put("context", context);
            scriptContext.put("parameters", context);
            scriptContext.put("request", request);
            scriptContext.put("response", response);
            Object scriptResult = GroovyUtil.eval("request.setParameter(\"add_product_id\", \"CAM-2644\");", scriptContext);
        } catch (Exception e) {
            Debug.logError(e, "Error executing Groovy script: " + e.getMessage(), MODULE);
        }
        try {
            result = ShoppingCartEvents.addToCart(request, response);
        } catch (Exception e) {
            Debug.logError(e, "Error calling ShoppingCartEvents.addToCart: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        Debug.logInfo("===== >>> Event : addToCart, Response = " + result, MODULE);
        try {
            Map<String, Object> scriptContext = new HashMap<>();
            scriptContext.put("delegator", delegator);
            scriptContext.put("dispatcher", dispatcher);
            scriptContext.put("locale", locale);
            scriptContext.put("userLogin", userLogin);
            scriptContext.put("context", context);
            scriptContext.put("parameters", context);
            scriptContext.put("request", request);
            scriptContext.put("response", response);
            Object scriptResult = GroovyUtil.eval("request.setParameter(\"checkoutpage\", \"quick\");\n            request.setParameter(\"shipping_contact_mech_id\", \"9015\");\n            request.setParameter(\"shipping_method\", \"GROUND@UPS\");\n            request.setParameter(\"checkOutPaymentId\", \"EXT_COD\");\n            request.setParameter(\"is_gift\", \"false\");\n            request.setParameter(\"may_split\", \"false\");", scriptContext);
        } catch (Exception e) {
            Debug.logError(e, "Error executing Groovy script: " + e.getMessage(), MODULE);
        }
        request.setAttribute("shoppingCart", null);
        try {
            result = CheckOutEvents.setQuickCheckOutOptions(request, response);
        } catch (Exception e) {
            Debug.logError(e, "Error calling CheckOutEvents.setQuickCheckOutOptions: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        Debug.logInfo("===== >>> Event : setQuickCheckOutOptions, Response = " + result, MODULE);
        try {
            result = CheckOutEvents.createOrder(request, response);
        } catch (Exception e) {
            Debug.logError(e, "Error calling CheckOutEvents.createOrder: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        Debug.logInfo("===== >>> Event : createOrder, Response = " + result, MODULE);
        try {
            result = CheckOutEvents.processPayment(request, response);
        } catch (Exception e) {
            Debug.logError(e, "Error calling CheckOutEvents.processPayment: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        Debug.logInfo("===== >>> Event : processPayment, Response = " + result, MODULE);
        // TODO: Convert <call-service-asynch> element
        try {
            result = ShoppingCartEvents.destroyCart(request, response);
        } catch (Exception e) {
            Debug.logError(e, "Error calling ShoppingCartEvents.destroyCart: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        Debug.logInfo("===== >>> Event : destroyCart, Response = " + result, MODULE);
        List<GenericValue> orderHeaders = null;
        try {
            orderHeaders = EntityQuery.use(delegator)
                    .from("OrderHeader")
                    .queryList();
        } catch (Exception e) {
            Debug.logError(e, "Error querying OrderHeader: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        GenericValue orderHeader = EntityUtil.getFirst((List<GenericValue>) orderHeaders);
        Map<String, Object> orderInput = new HashMap<>();
        orderInput.put("orderId", ((Map<String, Object>) orderHeader).get("orderId"));
        orderInput.put("invoicePerShipment", "Y");
        orderInput.put("userLogin", userLogin);
        Object responseMessage = null;
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("updateOrderHeader", orderInput);
            if (ServiceUtil.isError(serviceResult)) {
                String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                request.setAttribute("_ERROR_MESSAGE_", errMsg);
                return "error";
            }
            responseMessage = serviceResult.get("responseMessage");
        } catch (Exception e) {
            Debug.logError(e, "Error calling updateOrderHeader: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        Debug.logInfo("===== >>> Service : updateOrderHeader / invoicePerShipment = Y,  Response = " + responseMessage, MODULE);
        try {
            Map<String, Object> scriptContext = new HashMap<>();
            scriptContext.put("delegator", delegator);
            scriptContext.put("dispatcher", dispatcher);
            scriptContext.put("locale", locale);
            scriptContext.put("userLogin", userLogin);
            scriptContext.put("context", context);
            scriptContext.put("parameters", context);
            scriptContext.put("request", request);
            scriptContext.put("response", response);
            Object scriptResult = GroovyUtil.eval("import org.ofbiz.shipment.packing.PackingSession;\n            packingSession = new PackingSession(dispatcher, userLogin);\n            packingSession.setPrimaryOrderId(orderHeader.get(\"orderId\"));\n            packingSession.setPrimaryShipGroupSeqId(\"00001\");\n            parameters.put(\"packingSession\", packingSession);", scriptContext);
        } catch (Exception e) {
            Debug.logError(e, "Error executing Groovy script: " + e.getMessage(), MODULE);
        }
        Map<String, Object> packInput = new HashMap<>();
        packInput.put("orderId", ((Map<String, Object>) orderHeader).get("orderId"));
        packInput.put("shipGroupSeqId", "00001");
        packInput.put("packingSession", context.get("packingSession"));
        packInput.put("nextPackageSeq", 1);
        packInput.put("userLogin", userLogin);
        packInput.put("pkg", "1");
        packInput.put("qty", "1");
        packInput.put("prd", "CAM-2644");
        packInput.put("ite", "00001");
        packInput.put("wgt", "0");
        packInput.put("numPackages", "1");
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("packBulkItems", packInput);
            if (ServiceUtil.isError(serviceResult)) {
                String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                request.setAttribute("_ERROR_MESSAGE_", errMsg);
                return "error";
            }
            responseMessage = serviceResult.get("responseMessage");
        } catch (Exception e) {
            Debug.logError(e, "Error calling packBulkItems: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        Debug.logInfo("===== >>> Service: packBulkItems, Response = " + responseMessage, MODULE);
        Map<String, Object> completePackInput = new HashMap<>();
        // set-service-fields from "packInput" to "completePackInput" for service "completePack"
        completePackInput.putAll(UtilMisc.toMap(packInput));
        Object shipmentId = null;
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("completePack", completePackInput);
            if (ServiceUtil.isError(serviceResult)) {
                String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                request.setAttribute("_ERROR_MESSAGE_", errMsg);
                return "error";
            }
            shipmentId = serviceResult.get("shipmentId");
        } catch (Exception e) {
            Debug.logError(e, "Error calling completePack: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        Debug.logInfo("===== >>> Service: completePack, shipmentId = " + shipmentId, MODULE);
        List<GenericValue> invoices = null;
        try {
            invoices = EntityQuery.use(delegator)
                    .from("OrderItemBillingAndInvoiceAndItem")
                    .where(UtilMisc.toMap("orderId", ((Map<String, Object>) orderHeader).get("orderId")))
                    .queryList();
        } catch (Exception e) {
            Debug.logError(e, "Error querying OrderItemBillingAndInvoiceAndItem: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        assert !(UtilValidate.isEmpty(invoices)) : "Assertion failed: not if-empty";
        if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
            return "error";
        }

        return "success";
    }

}
