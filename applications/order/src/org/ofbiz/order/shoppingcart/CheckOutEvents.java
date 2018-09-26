/*******************************************************************************
 * Licensed to the Apache Software Foundation (ASF) under one
 * or more contributor license agreements.  See the NOTICE file
 * distributed with this work for additional information
 * regarding copyright ownership.  The ASF licenses this file
 * to you under the Apache License, Version 2.0 (the
 * "License"); you may not use this file except in compliance
 * with the License.  You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 * KIND, either express or implied.  See the License for the
 * specific language governing permissions and limitations
 * under the License.
 *******************************************************************************/
package org.ofbiz.order.shoppingcart;

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.servlet.http.HttpSession;

import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.GeneralException;
import org.ofbiz.base.util.GeneralRuntimeException;
import org.ofbiz.base.util.MapProcessor;
import org.ofbiz.base.util.MapProcessorInvoker;
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
import org.ofbiz.marketing.tracking.TrackingCodeEvents;
import org.ofbiz.minilang.SimpleMapProcessorProcessor;
import org.ofbiz.order.order.OrderReadHelper;
import org.ofbiz.party.party.PartyWorker;
import org.ofbiz.product.store.ProductStoreWorker;
import org.ofbiz.service.GenericServiceException;
import org.ofbiz.service.LocalDispatcher;
import org.ofbiz.service.ModelService;
import org.ofbiz.service.ServiceErrorException;
import org.ofbiz.service.ServiceUtil;
import org.ofbiz.webapp.stats.VisitHandler;
import org.ofbiz.webapp.website.WebSiteWorker;

/**
 * Events used for processing checkout and orders.
 */
public class CheckOutEvents {

    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());
    public static final String resource = "OrderUiLabels";
    public static final String resource_error = "OrderErrorUiLabels";

    public static String cartNotEmpty(HttpServletRequest request, HttpServletResponse response) {
        ShoppingCart cart = ShoppingCartEvents.getCartObject(request);

        if (cart != null && UtilValidate.isNotEmpty(cart.items())) {
            return "success";
        } else {
            String errMsg = UtilProperties.getMessage(resource_error, "checkevents.cart_empty", (cart != null ? cart.getLocale() : UtilHttp.getLocale(request)));
            request.setAttribute("_ERROR_MESSAGE_", errMsg);
            return "error";
        }
    }

    public static String setCheckOutPages(HttpServletRequest request, HttpServletResponse response) {
        if ("error".equals(CheckOutEvents.cartNotEmpty(request, response)) == true) {
            return "error";
        }

        HttpSession session = request.getSession();

        // SCIPIO: Read the "checkoutpage" from attributes first, so events may modify
        //String curPage = request.getParameter("checkoutpage");
        String curPage = getRequestAttribOrParam(request, "checkoutpage");
        Debug.logInfo("CheckoutPage: " + curPage, module);

        ShoppingCart cart = (ShoppingCart) session.getAttribute("shoppingCart");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Delegator delegator = (Delegator) request.getAttribute("delegator");

        GenericValue userLogin = cart.getUserLogin();
        if (userLogin == null) userLogin = (GenericValue) session.getAttribute("userLogin");
        if (curPage == null) {
            try {
                cart.createDropShipGroups(dispatcher);
            } catch (CartItemModifyException e) {
                Debug.logError(e, module);
            }
        } else if ("shippingoptions".equals(curPage) == true) {
            //remove empty ship group
            cart.cleanUpShipGroups();
        }
        CheckOutHelper checkOutHelper = new CheckOutHelper(dispatcher, delegator, cart);

        if ("shippingaddress".equals(curPage) == true) {
            // Set the shipping address options
            // SCIPIO: Allow inlined address creation
            //String shippingContactMechId = request.getParameter("shipping_contact_mech_id");
            String shippingContactMechId;
            try {
                shippingContactMechId = getRequestAttribOrParam(request, "shipping_contact_mech_id");
                Map<String, Object> contactMechResult = checkShipContactMechIdForNew(request, shippingContactMechId, "newShipAddr_");
                if (ServiceUtil.isSuccess(contactMechResult)) {
                    shippingContactMechId = (String) contactMechResult.get("contactMechId");
                } else {
                    ServiceUtil.appendMessageLists(request, contactMechResult);
                    return "error";
                }
            } catch (GeneralException e1) {
                Debug.logError(e1, module); // unexpected error
                request.setAttribute("_ERROR_MESSAGE_", UtilProperties.getMessage(resource_error, "OrderUnexpectedErrorHelp", (cart != null ? cart.getLocale() : Locale.getDefault())));
                return "error";
            }

            String taxAuthPartyGeoIds = request.getParameter("taxAuthPartyGeoIds");
            String partyTaxId = request.getParameter("partyTaxId");
            String isExempt = request.getParameter("isExempt");

            List<String> errorMessages = new ArrayList<String>();
            Map<String, Object> errorMaps = new HashMap<String, Object>();
            for (int shipGroupIndex = 0; shipGroupIndex < cart.getShipGroupSize(); shipGroupIndex++) {
                // set the shipping method
                if (shippingContactMechId == null) {
                    shippingContactMechId = (String) request.getAttribute("contactMechId"); // FIXME
                }
                String supplierPartyId = (String) request.getAttribute(shipGroupIndex + "_supplierPartyId");
                Map<String, ? extends Object> callResult = checkOutHelper.finalizeOrderEntryShip(shipGroupIndex, shippingContactMechId, supplierPartyId);
                ServiceUtil.addErrors(errorMessages, errorMaps, callResult);
            }

            // if taxAuthPartyGeoIds is not empty drop that into the database
            if (UtilValidate.isNotEmpty(taxAuthPartyGeoIds)) {
                try {
                    Map<String, ? extends Object> createCustomerTaxAuthInfoResult = dispatcher.runSync("createCustomerTaxAuthInfo",
                            UtilMisc.<String, Object>toMap("partyId", cart.getPartyId(), "taxAuthPartyGeoIds", taxAuthPartyGeoIds, "partyTaxId", partyTaxId, "isExempt", isExempt, "userLogin", userLogin));
                    ServiceUtil.getMessages(request, createCustomerTaxAuthInfoResult, null);
                    if (ServiceUtil.isError(createCustomerTaxAuthInfoResult)) {
                        return "error";
                    }
                } catch (GenericServiceException e) {
                    String errMsg = "Error setting customer tax info: " + e.toString();
                    request.setAttribute("_ERROR_MESSAGE_", errMsg);
                    return "error";
                }
            }

            Map<String, ? extends Object> callResult = checkOutHelper.setCheckOutShippingAddress(shippingContactMechId);
            ServiceUtil.getMessages(request, callResult, null);

            if (!(ServiceUtil.isError(callResult))) {
                // No errors so push the user onto the next page
                curPage = "shippingoptions";
            }
        } else if ("shippingoptions".equals(curPage) == true) {
            // Set the general shipping options
            String shippingMethod = request.getParameter("shipping_method");
            String shippingInstructions = request.getParameter("shipping_instructions");
            String orderAdditionalEmails = request.getParameter("order_additional_emails");
            String maySplit = request.getParameter("may_split");
            String giftMessage = request.getParameter("gift_message");
            String isGift = request.getParameter("is_gift");
            String internalCode = request.getParameter("internalCode");
            String shipBeforeDate =  request.getParameter("shipBeforeDate");
            String shipAfterDate = request.getParameter("shipAfterDate");
            Map<String, ? extends Object> callResult = ServiceUtil.returnSuccess();

            for (int shipGroupIndex = 0; shipGroupIndex < cart.getShipGroupSize(); shipGroupIndex++) {
                callResult = checkOutHelper.finalizeOrderEntryOptions(shipGroupIndex, shippingMethod, shippingInstructions, maySplit, giftMessage, isGift, internalCode, shipBeforeDate, shipAfterDate, orderAdditionalEmails);
                ServiceUtil.getMessages(request, callResult, null);
            }
            if (!(callResult.get(ModelService.RESPONSE_MESSAGE).equals(ModelService.RESPOND_ERROR))) {
                // No errors so push the user onto the next page
                curPage = "payment";
            }
        } else if ("payment".equals(curPage) == true) {
            // Set the payment options
            // SCIPIO: Handle new error cases
            Map<String, Map<String, Object>> selectedPaymentMethods;
            try {
                selectedPaymentMethods = getSelectedPaymentMethods(request);
            } catch (ServiceErrorException e) {
                Debug.logInfo(e.getMessage(), module); // regular error (probably user)
                ServiceUtil.appendMessageLists(request, e.getServiceResult());
                return "error";
            } catch (GeneralException e) {
                Debug.logError(e, module); // unexpected error
                request.setAttribute("_ERROR_MESSAGE_", UtilProperties.getMessage(resource_error, "OrderUnexpectedErrorHelp", (cart != null ? cart.getLocale() : Locale.getDefault())));
                return "error";
            }

            String billingAccountId = request.getParameter("billingAccountId");
            if (UtilValidate.isNotEmpty(billingAccountId)) {
                BigDecimal billingAccountAmt = null;
                billingAccountAmt = determineBillingAccountAmount(billingAccountId, request.getParameter("billingAccountAmount"), dispatcher);
                if ((billingAccountId != null) && !"_NA_".equals(billingAccountId) && (billingAccountAmt == null)) {
                    request.setAttribute("_ERROR_MESSAGE_", UtilProperties.getMessage(resource_error,"OrderInvalidAmountSetForBillingAccount", UtilMisc.toMap("billingAccountId",billingAccountId), cart.getLocale()));
                    return "error";
                }
                selectedPaymentMethods.put("EXT_BILLACT", UtilMisc.<String, Object>toMap("amount", billingAccountAmt, "securityCode", null));
            }

            List<String> singleUsePayments = new ArrayList<String>();

            // SCIPIO: Support single-use for arbitrary pay methods
            addSingleUsePayments(request, selectedPaymentMethods, singleUsePayments);

            // check for gift card not on file
            Map<String, Object> params = UtilHttp.getParameterMap(request);
            Map<String, Object> gcResult = checkOutHelper.checkGiftCard(params, selectedPaymentMethods);
            ServiceUtil.getMessages(request, gcResult, null);
            if (gcResult.get(ModelService.RESPONSE_MESSAGE).equals(ModelService.RESPOND_ERROR)) {
                return "error";
            } else {
                String gcPaymentMethodId = (String) gcResult.get("paymentMethodId");
                BigDecimal gcAmount = (BigDecimal) gcResult.get("amount");
                if (gcPaymentMethodId != null) {
                    selectedPaymentMethods.put(gcPaymentMethodId, UtilMisc.<String, Object>toMap("amount", gcAmount, "securityCode", null));
                    if ("Y".equalsIgnoreCase(request.getParameter("singleUseGiftCard"))) {
                        singleUsePayments.add(gcPaymentMethodId);
                    }

                    // SCIPIO: Save the info of which paymentMethodId was just created for the new card
                    saveToNewPaymentMethodIdMap(request, "_NEW_GIFT_CARD_", gcPaymentMethodId);
                }
            }

            // SCIPIO: 2016-05-09: This check moved AFTER gift card check so can use GC alone
            if (UtilValidate.isEmpty(selectedPaymentMethods)) {
                // SCIPIO: 2016-04-21: patch is based on logic from org.ofbiz.order.shoppingcart.CheckOutHelper.setCheckOutPaymentInternal
                // we need an error message and the behavior for whether to require it is now based on ProductStore.reqPayMethForFreeOrders,
                // otherwise the different checkout methods known to stock are too inconsistent.
                // The default is Y which was the original behavior of this part of code, and is the most "safe" default.
                GenericValue productStore = ProductStoreWorker.getProductStore(request);
                if (cart.getGrandTotal().compareTo(BigDecimal.ZERO) != 0 || !(productStore != null && "N".equals(productStore.getString("reqPayMethForFreeOrders")))) {
                    request.setAttribute("_ERROR_MESSAGE_", UtilProperties.getMessage(resource_error,"checkhelper.select_method_of_payment",
                            (cart != null ? cart.getLocale() : Locale.getDefault())));
                    return "error";
                }
            }

            Map<String, Object> callResult = checkOutHelper.setCheckOutPayment(selectedPaymentMethods, singleUsePayments, billingAccountId);
            ServiceUtil.getMessages(request, callResult, null);

            if (!(callResult.get(ModelService.RESPONSE_MESSAGE).equals(ModelService.RESPOND_ERROR))) {
                // No errors so push the user onto the next page
                curPage = "confirm";
            }
        } else {
            curPage = determineInitialCheckOutPage(cart);
        }

        return curPage;
    }

    /**
     * SCIPIO: Adds the IDs of single use payments from selected methods to the given list.
     */
    static void addSingleUsePayments(HttpServletRequest request, Map<String, Map<String, Object>> selectedPaymentMethods, List<String> singleUsePayments) {
        if (selectedPaymentMethods != null) {
            for(Map.Entry<String, Map<String, Object>> entry : selectedPaymentMethods.entrySet()) {
                Map<String, Object> info = entry.getValue();
                if (info != null && Boolean.TRUE.equals(info.get("singleUsePayment"))) {
                    singleUsePayments.add(entry.getKey());
                }
            }
        }
    }


    private static final String DEFAULT_INIT_CHECKOUT_PAGE = "shippingaddress";

    /**
     * Method to determine the initial checkout page based on requirements. This will also set
     * any cart variables necessary to satisfy the requirements, such as setting the
     * shipment method according to the type of items in the cart.
     */
    public static String determineInitialCheckOutPage(ShoppingCart cart) {
        String page = DEFAULT_INIT_CHECKOUT_PAGE;
        if (cart == null) return page;

        // if no shipping applies, set the no shipment method and skip to payment
        if (!cart.shippingApplies()) {
            cart.setAllShipmentMethodTypeId("NO_SHIPPING");
            cart.setAllCarrierPartyId("_NA_");
            page = "payment";
        }

        return page;
    }

    public static String setCheckOutError(HttpServletRequest request, HttpServletResponse response) {
        // SCIPIO: Read the "checkoutpage" from attributes first, so events may modify
        //String currentPage = request.getParameter("checkoutpage");
        String currentPage = getRequestAttribOrParam(request, "checkoutpage");
        if (UtilValidate.isEmpty(currentPage)) {
            return "error";
        } else {
            return currentPage;
        }
    }

    /**
     * Use for quickcheckout submit.  It calculates the tax before setting the payment options.
     * Shipment option should already be set by the quickcheckout form.
     */
    public static String setQuickCheckOutOptions(HttpServletRequest request, HttpServletResponse response) {
        String result = calcTax(request, response);
        if ("error".equals(result)) return "error";
        return setCheckOutOptions(request, response);
    }

    public static String setPartialCheckOutOptions(HttpServletRequest request, HttpServletResponse response) {
        // SCIPIO: We currently *may* run into additional problems if try to create new records during partial saves.
        // Also, note the fixme below is a stock fixme, not by us; it shouldn't be changed with current implementation (which is a "best-effort")...
        request.setAttribute("checkoutUseNewRecords", Boolean.FALSE);

        // FIXME response need to be checked ?
        setCheckOutOptions(request, response);
        request.setAttribute("_ERROR_MESSAGE_", null);
        return "success";
    }

    public static String setCartShipToCustomerParty(HttpServletRequest request, HttpServletResponse response) {
        ShoppingCart cart = (ShoppingCart) request.getSession().getAttribute("shoppingCart");
        String shipToCustomerPartyId = request.getParameter("shipToCustomerPartyId");
        cart.setShipToCustomerPartyId(shipToCustomerPartyId);
        cart.setAllShippingContactMechId(null);
        return "success";
    }

    public static String checkPaymentMethods(HttpServletRequest request, HttpServletResponse response) {
        ShoppingCart cart = (ShoppingCart) request.getSession().getAttribute("shoppingCart");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        CheckOutHelper checkOutHelper = new CheckOutHelper(dispatcher, delegator, cart);
        Map<String, Object> resp = checkOutHelper.validatePaymentMethods();
        if (ServiceUtil.isError(resp)) {
            request.setAttribute("_ERROR_MESSAGE_", ServiceUtil.getErrorMessage(resp));
            return "error";
        }
        return "success";
    }

    /**
     * SCIPIO: Alternative event to {@link #checkPaymentMethods} that may be run <i>before</i> payment method
     * selection.
     * <p>
     * TODO?: Currently, this event does nothing. See comments.
     */
    public static String checkPaymentMethodsBeforePayment(HttpServletRequest request, HttpServletResponse response) {
        /* SCIPIO: TODO?
        ShoppingCart cart = (ShoppingCart) request.getSession().getAttribute("shoppingCart");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        CheckOutHelper checkOutHelper = new CheckOutHelper(dispatcher, delegator, cart);
        */
        // SCIPIO: TODO?: Currently, in this event we do nothing. This is because checkOutHelper.validatePaymentMethods()
        // performs mainly a total amount validation which causes errors when going backward in checkout due
        // to change shipping options which can affect totals and result a negative balance error.
        // This method is thus currently a placeholder.
        /*
        Map<String, Object> resp = checkOutHelper.validatePaymentMethods();
        if (ServiceUtil.isError(resp)) {
            request.setAttribute("_ERROR_MESSAGE_", ServiceUtil.getErrorMessage(resp));
            return "error";
        }
        */
        return "success";
    }

    public static Map<String, Map<String, Object>> getSelectedPaymentMethods(HttpServletRequest request) throws ServiceErrorException, GeneralException {
        ShoppingCart cart = (ShoppingCart) request.getSession().getAttribute("shoppingCart");
        Map<String, Map<String, Object>> selectedPaymentMethods = new HashMap<String, Map<String, Object>>();

        // SCIPIO: Allow override via request attribs
        String[] paymentMethods;
        if (request.getAttribute("checkOutPaymentId") != null) {
            Object copid = request.getAttribute("checkOutPaymentId");
            if (copid instanceof String) {
                paymentMethods = new String[]{ (String) copid };
            } else if (copid instanceof List) {
                paymentMethods = (UtilGenerics.<String>checkList(copid)).toArray(new String[]{});
            } else {
                Debug.logError("Scipio: checkOutPaymentId request attrib was neither string nor list; treating as null", module);
                //paymentMethods = request.getParameterValues("checkOutPaymentId");
                paymentMethods = null;
            }
        } else {
            paymentMethods = request.getParameterValues("checkOutPaymentId");
        }

        if (UtilValidate.isNotEmpty(request.getParameter("issuerId"))) {
            request.setAttribute("issuerId", request.getParameter("issuerId"));
        }

        //String errMsg = null; // SCIPIO: now unused

        if (paymentMethods != null) {
            for (int i = 0; i < paymentMethods.length; i++) {
                Map<String, Object> paymentMethodInfo = new HashMap<String, Object>();

                // SCIPIO: Allow inlined address creation
                String paymentMethodId = paymentMethods[i];
                String origPaymentMethodId = paymentMethods[i];
                Map<String, Object> payMethResult = checkPaymentMethodIdForNew(request, paymentMethodId, "newCreditCard_", "newEftAccount_");
                if (ServiceUtil.isSuccess(payMethResult)) {
                    paymentMethodId = (String) payMethResult.get("paymentMethodId");
                    origPaymentMethodId = (String) payMethResult.get("origPaymentMethodId");
                    // SPECIAL CASE: paymentMethodId may be null or empty, in which case we skip
                    if (UtilValidate.isEmpty(paymentMethodId)) {
                        continue;
                    }
                } else {
                    throw new ServiceErrorException("Could not get pay method: " + ServiceUtil.getErrorMessage(payMethResult), payMethResult);
                }

                String securityCode = request.getParameter("securityCode_" + origPaymentMethodId);
                if (UtilValidate.isNotEmpty(securityCode)) {
                    paymentMethodInfo.put("securityCode", securityCode);
                }
                String amountStr = request.getParameter("amount_" + origPaymentMethodId);
                BigDecimal amount = null;
                if (UtilValidate.isNotEmpty(amountStr) && !"REMAINING".equals(amountStr)) {
                    try {
                        amount = new BigDecimal(amountStr);
                    } catch (NumberFormatException e) {
                        // SCIPIO: Let caller handle
                        throw new ServiceErrorException("Invalid amount set for payment method: " + amountStr, ServiceUtil.returnError(
                                UtilProperties.getMessage(resource_error, "checkevents.invalid_amount_set_for_payment_method", (cart != null ? cart.getLocale() : Locale.getDefault()))));
                        //Debug.logError(e, module);
                        //errMsg = UtilProperties.getMessage(resource_error, "checkevents.invalid_amount_set_for_payment_method", (cart != null ? cart.getLocale() : Locale.getDefault()));
                        //request.setAttribute("_ERROR_MESSAGE_", errMsg);
                        //return null;
                    }
                }
                paymentMethodInfo.put("amount", amount);

                // SCIPIO: Get single-use flag
                Boolean singleUseBool = null;
                String singleUseFlag = request.getParameter("singleUsePayment_" + origPaymentMethodId);
                if ("Y".equals(singleUseFlag)) {
                    singleUseBool = Boolean.TRUE;
                } else if ("N".equals(singleUseFlag)) {
                    singleUseBool = Boolean.FALSE;
                }
                paymentMethodInfo.put("singleUsePayment", singleUseBool);

                selectedPaymentMethods.put(paymentMethodId, paymentMethodInfo); // SCIPIO: edited
            }
        }
        Debug.logInfo("Selected Payment Methods : " + selectedPaymentMethods, module);
        return selectedPaymentMethods;
    }

    // this servlet is used by quick checkout
    public static String setCheckOutOptions(HttpServletRequest request, HttpServletResponse response) {
        ShoppingCart cart = (ShoppingCart) request.getSession().getAttribute("shoppingCart");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Delegator delegator = (Delegator) request.getAttribute("delegator");

        // Set the payment options
        // SCIPIO: Handle new error cases
        Map<String, Map<String, Object>> selectedPaymentMethods;
        try {
            selectedPaymentMethods = getSelectedPaymentMethods(request);
        } catch (ServiceErrorException e) {
            Debug.logInfo(e.getMessage(), module); // regular error (probably user)
            ServiceUtil.appendMessageLists(request, e.getServiceResult());
            return "error";
        } catch (GeneralException e) {
            Debug.logError(e, module); // unexpected error
            request.setAttribute("_ERROR_MESSAGE_", UtilProperties.getMessage(resource_error, "OrderUnexpectedErrorHelp", (cart != null ? cart.getLocale() : Locale.getDefault())));
            return "error";
        }

        CheckOutHelper checkOutHelper = new CheckOutHelper(dispatcher, delegator, cart);

        // get the billing account and amount
        String billingAccountId = request.getParameter("billingAccountId");
        if (UtilValidate.isNotEmpty(billingAccountId)) {
            BigDecimal billingAccountAmt = null;
            billingAccountAmt = determineBillingAccountAmount(billingAccountId, request.getParameter("billingAccountAmount"), dispatcher);
            if (billingAccountAmt == null) {
                request.setAttribute("_ERROR_MESSAGE_", UtilProperties.getMessage(resource_error,"OrderInvalidAmountSetForBillingAccount", UtilMisc.toMap("billingAccountId",billingAccountId), (cart != null ? cart.getLocale() : Locale.getDefault())));
                return "error";
            }
            selectedPaymentMethods.put("EXT_BILLACT", UtilMisc.<String, Object>toMap("amount", billingAccountAmt, "securityCode", null));
        }

        if (selectedPaymentMethods == null) {
            return "error";
        }

        String shippingMethod = request.getParameter("shipping_method");

        // SCIPIO: Allow inlined address creation
        //String shippingContactMechId = request.getParameter("shipping_contact_mech_id");
        String shippingContactMechId;
        try {
            shippingContactMechId = getRequestAttribOrParam(request, "shipping_contact_mech_id");
            Map<String, Object> contactMechResult = checkShipContactMechIdForNew(request, shippingContactMechId, "newShipAddr_");
            if (ServiceUtil.isSuccess(contactMechResult)) {
                shippingContactMechId = (String) contactMechResult.get("contactMechId");
            } else {
                ServiceUtil.appendMessageLists(request, contactMechResult);
                return "error";
            }
        } catch (GeneralException e1) {
            Debug.logError(e1, module); // unexpected error
            request.setAttribute("_ERROR_MESSAGE_", UtilProperties.getMessage(resource_error, "OrderUnexpectedErrorHelp", (cart != null ? cart.getLocale() : Locale.getDefault())));
            return "error";
        }

        String taxAuthPartyGeoIds = request.getParameter("taxAuthPartyGeoIds");
        String partyTaxId = request.getParameter("partyTaxId");
        String isExempt = request.getParameter("isExempt");

        String shippingInstructions = request.getParameter("shipping_instructions");
        String orderAdditionalEmails = request.getParameter("order_additional_emails");
        String maySplit = request.getParameter("may_split");
        String giftMessage = request.getParameter("gift_message");
        String isGift = request.getParameter("is_gift");
        String internalCode = request.getParameter("internalCode");
        String shipBeforeDate = request.getParameter("shipBeforeDate");
        String shipAfterDate = request.getParameter("shipAfterDate");

        List<String> singleUsePayments = new ArrayList<String>();

        // SCIPIO: Support single-use for arbitrary pay methods
        addSingleUsePayments(request, selectedPaymentMethods, singleUsePayments);

        // get a request map of parameters
        Map<String, Object> params = UtilHttp.getParameterMap(request);

        // if taxAuthPartyGeoIds is not empty drop that into the database
        if (UtilValidate.isNotEmpty(taxAuthPartyGeoIds)) {
            try {
                Map<String, Object> createCustomerTaxAuthInfoResult = dispatcher.runSync("createCustomerTaxAuthInfo",
                        UtilMisc.toMap("partyId", cart.getPartyId(), "taxAuthPartyGeoIds", taxAuthPartyGeoIds, "partyTaxId", partyTaxId, "isExempt", isExempt));
                ServiceUtil.getMessages(request, createCustomerTaxAuthInfoResult, null);
                if (ServiceUtil.isError(createCustomerTaxAuthInfoResult)) {
                    return "error";
                }
            } catch (GenericServiceException e) {
                String errMsg = "Error setting customer tax info: " + e.toString();
                request.setAttribute("_ERROR_MESSAGE_", errMsg);
                return "error";
            }
        }

        // check for gift card not on file
        Map<String, Object> gcResult = checkOutHelper.checkGiftCard(params, selectedPaymentMethods);
        ServiceUtil.getMessages(request, gcResult, null);
        if (ServiceUtil.isError(gcResult)) {
            return "error";
        }

        String gcPaymentMethodId = (String) gcResult.get("paymentMethodId");
        BigDecimal gcAmount = (BigDecimal) gcResult.get("amount");
        if (gcPaymentMethodId != null) {
            selectedPaymentMethods.put(gcPaymentMethodId, UtilMisc.<String, Object>toMap("amount", gcAmount, "securityCode", null));
            if ("Y".equalsIgnoreCase(request.getParameter("singleUseGiftCard"))) {
                singleUsePayments.add(gcPaymentMethodId);
            }

            // SCIPIO: Save the info of which paymentMethodId was just created for the new card
            saveToNewPaymentMethodIdMap(request, "_NEW_GIFT_CARD_", gcPaymentMethodId);
        }

        Map<String, Object> optResult = checkOutHelper.setCheckOutOptions(shippingMethod, shippingContactMechId, selectedPaymentMethods,
                singleUsePayments, billingAccountId, shippingInstructions,
                orderAdditionalEmails, maySplit, giftMessage, isGift, internalCode, shipBeforeDate, shipAfterDate);

        ServiceUtil.getMessages(request, optResult, null);
        if (ServiceUtil.isError(optResult)) {
            return "error";
        }

        return "success";
    }

    // Create order event - uses createOrder service for processing
    public static String createOrder(HttpServletRequest request, HttpServletResponse response) {
        HttpSession session = request.getSession();
        ShoppingCart cart = ShoppingCartEvents.getCartObject(request);
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        GenericValue userLogin = (GenericValue) session.getAttribute("userLogin");
        CheckOutHelper checkOutHelper = new CheckOutHelper(dispatcher, delegator, cart);
        Map<String, Object> callResult;

        if (UtilValidate.isEmpty(userLogin)) {
            userLogin = cart.getUserLogin();
            session.setAttribute("userLogin", userLogin);
        }
        // remove this whenever creating an order so quick reorder cache will refresh/recalc
        session.removeAttribute("_QUICK_REORDER_PRODUCTS_");

        boolean areOrderItemsExploded = explodeOrderItems(delegator, cart);

        //get the TrackingCodeOrder List
        List<GenericValue> trackingCodeOrders = TrackingCodeEvents.makeTrackingCodeOrders(request);
        String distributorId = (String) session.getAttribute("_DISTRIBUTOR_ID_");
        String affiliateId = (String) session.getAttribute("_AFFILIATE_ID_");
        String visitId = VisitHandler.getVisitId(session);
        String webSiteId = WebSiteWorker.getWebSiteId(request);

        callResult = checkOutHelper.createOrder(userLogin, distributorId, affiliateId, trackingCodeOrders, areOrderItemsExploded, visitId, webSiteId);
        if (callResult != null) {
            ServiceUtil.getMessages(request, callResult, null);
            if (ServiceUtil.isError(callResult)) {
                // messages already setup with the getMessages call, just return the error response code
                return "error";
            }
            if (callResult.get(ModelService.RESPONSE_MESSAGE).equals(ModelService.RESPOND_SUCCESS)) {
                // set the orderId for use by chained events
                String orderId = cart.getOrderId();
                request.setAttribute("orderId", orderId);
                request.setAttribute("orderAdditionalEmails", cart.getOrderAdditionalEmails());
            }
        }

        String issuerId = request.getParameter("issuerId");
        if (UtilValidate.isNotEmpty(issuerId)) {
            request.setAttribute("issuerId", issuerId);
        }


        return cart.getOrderType().toLowerCase();
    }

    // Event wrapper for the tax calc.
    public static String calcTax(HttpServletRequest request, HttpServletResponse response) {
        try {
            calcTax(request);
        } catch (GeneralException e) {
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        return "success";
    }

    // Invoke the taxCalc
    private static void calcTax(HttpServletRequest request) throws GeneralException {
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        ShoppingCart cart = ShoppingCartEvents.getCartObject(request);
        CheckOutHelper checkOutHelper = new CheckOutHelper(dispatcher, delegator, cart);

        //Calculate and add the tax adjustments
        checkOutHelper.calcAndAddTax();
    }

    public static boolean explodeOrderItems(Delegator delegator, ShoppingCart cart) {
        if (cart == null) return false;
        GenericValue productStore = ProductStoreWorker.getProductStore(cart.getProductStoreId(), delegator);
        if (productStore == null || productStore.get("explodeOrderItems") == null) {
            return false;
        }
        return productStore.getBoolean("explodeOrderItems");
    }

    public static String checkShipmentNeeded(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        ShoppingCart cart = ShoppingCartEvents.getCartObject(request);
        GenericValue productStore = null;
        try {
            productStore = EntityQuery.use(delegator).from("ProductStore").where("productStoreId", cart.getProductStoreId()).cache().queryOne();
            Debug.logInfo("checkShipmentNeeded: reqShipAddrForDigItems=" + productStore.getString("reqShipAddrForDigItems"), module);
        } catch (GenericEntityException e) {
            Debug.logError(e, "Error getting ProductStore: " + e.toString(), module);
        }

        if (productStore != null && "N".equals(productStore.getString("reqShipAddrForDigItems"))) {
            Debug.logInfo("checkShipmentNeeded: cart.containOnlyDigitalGoods()=" + cart.containOnlyDigitalGoods(), module);
            // don't require shipping for all digital items
            if (cart.containOnlyDigitalGoods()) {
                return "shipmentNotNeeded";
            }
        }

        return "shipmentNeeded";
    }

    // Event wrapper for processPayment.
    public static String processPayment(HttpServletRequest request, HttpServletResponse response) {
        // run the process payment process + approve order when complete; may also run sync fulfillments
        int failureCode = 0;
        try {
            if (!processPayment(request)) {
                failureCode = 1;
            }
        } catch (GeneralException e) {
            Debug.logError(e, module);
            ServiceUtil.setMessages(request, e.getMessage(), null, null);
            failureCode = 2;
        } catch (GeneralRuntimeException e) {
            Debug.logError(e, module);
            ServiceUtil.setMessages(request, e.getMessage(), null, null);
        }

        // event return based on failureCode
        switch (failureCode) {
            case 0:
                return "success";
            case 1:
                return "fail";
            default:
                return "error";
        }
    }

    private static boolean processPayment(HttpServletRequest request) throws GeneralException {
        HttpSession session = request.getSession();
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        ShoppingCart cart = (ShoppingCart) request.getSession().getAttribute("shoppingCart");
        GenericValue userLogin = (GenericValue) session.getAttribute("userLogin");
        CheckOutHelper checkOutHelper = new CheckOutHelper(dispatcher, delegator, cart);

        // check if the order is to be held (processing)
        boolean holdOrder = cart.getHoldOrder();

        // load the ProductStore settings
        GenericValue productStore = ProductStoreWorker.getProductStore(cart.getProductStoreId(), delegator);
        Map<String, Object> callResult = checkOutHelper.processPayment(productStore, userLogin, false, holdOrder);

        if (ServiceUtil.isError(callResult)) {
            // clear out the rejected payment methods (if any) from the cart, so they don't get re-authorized
            cart.clearDeclinedPaymentMethods(delegator);
            // null out the orderId for next pass
            cart.setOrderId(null);
        }

        // generate any messages required
        ServiceUtil.getMessages(request, callResult, null);

        // check for customer message(s)
        List<String> messages = UtilGenerics.checkList(callResult.get("authResultMsgs"));
        if (UtilValidate.isNotEmpty(messages)) {
            request.setAttribute("_EVENT_MESSAGE_LIST_", messages);
        }

        // determine whether it was a success or failure
        return (callResult.get(ModelService.RESPONSE_MESSAGE).equals(ModelService.RESPOND_SUCCESS));
    }

    public static String checkOrderBlacklist(HttpServletRequest request, HttpServletResponse response) {
        HttpSession session = request.getSession();
        ShoppingCart cart = (ShoppingCart) session.getAttribute("shoppingCart");
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        CheckOutHelper checkOutHelper = new CheckOutHelper(null, delegator, cart);
        String result;

        Map<String, Object> callResult = checkOutHelper.checkOrderBlackList();
        if (callResult.get(ModelService.RESPONSE_MESSAGE).equals(ModelService.RESPOND_ERROR)) {
            request.setAttribute("_ERROR_MESSAGE_", callResult.get(ModelService.ERROR_MESSAGE));
            result = "error";
        } else if (callResult.get(ModelService.RESPONSE_MESSAGE).equals(ModelService.RESPOND_FAIL)) {
            request.setAttribute("_ERROR_MESSAGE_", callResult.get(ModelService.ERROR_MESSAGE));
            result = "failed";
        } else {
            result = (String) callResult.get(ModelService.SUCCESS_MESSAGE);
        }

        return result;
    }

    public static String failedBlacklistCheck(HttpServletRequest request, HttpServletResponse response) {
        HttpSession session = request.getSession();
        ShoppingCart cart = (ShoppingCart) session.getAttribute("shoppingCart");
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        String orderPartyId = cart.getOrderPartyId();
        GenericValue userLogin = PartyWorker.findPartyLatestUserLogin(orderPartyId, delegator);
        GenericValue currentUser = (GenericValue) session.getAttribute("userLogin");
        String result;

        // Load the properties store
        GenericValue productStore = ProductStoreWorker.getProductStore(cart.getProductStoreId(), delegator);
        CheckOutHelper checkOutHelper = new CheckOutHelper(dispatcher, delegator, cart);
        Map<String, Object> callResult = checkOutHelper.failedBlacklistCheck(userLogin, productStore);

        //Generate any messages required
        ServiceUtil.getMessages(request, callResult, null);

        // wipe the session
        if (("anonymous".equals(currentUser.getString("userLoginId"))) || (currentUser.getString("userLoginId")).equals(userLogin.getString("userLoginId"))) {
            session.invalidate();
        }
        //Determine whether it was a success or not
        if (callResult.get(ModelService.RESPONSE_MESSAGE).equals(ModelService.RESPOND_ERROR)) {
            result = (String) callResult.get(ModelService.ERROR_MESSAGE);
            request.setAttribute("_ERROR_MESSAGE_", result);
            result = "error";
        } else {
            result = (String) callResult.get(ModelService.ERROR_MESSAGE);
            request.setAttribute("_ERROR_MESSAGE_", result);
            result = "success";
        }
        return result;
    }

    public static String checkExternalCheckout(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        ShoppingCart cart = ShoppingCartEvents.getCartObject(request);
        GenericValue productStore = ProductStoreWorker.getProductStore(cart.getProductStoreId(), delegator);
        String paymentMethodTypeId = request.getParameter("paymentMethodTypeId");
        if ("EXT_PAYPAL".equals(paymentMethodTypeId) || cart.getPaymentMethodTypeIds().contains("EXT_PAYPAL")) {
            try {
                GenericValue payPalProdStorePaySetting = EntityQuery.use(delegator).from("ProductStorePaymentSetting").where("productStoreId", productStore.getString("productStoreId"), "paymentMethodTypeId", "EXT_PAYPAL").queryFirst();
                if (payPalProdStorePaySetting != null) {
                    GenericValue gatewayConfig = payPalProdStorePaySetting.getRelatedOne("PaymentGatewayConfig", false);
                    if (gatewayConfig != null && "PAYFLOWPRO".equals(gatewayConfig.getString("paymentGatewayConfigTypeId"))) {
                        return "paypal";
                    }
                }
            } catch (GenericEntityException e) {
                Debug.logError(e, module);
            }
        }
        return "success";
    }

    public static String checkExternalPayment(HttpServletRequest request, HttpServletResponse response) {
        // warning there can only be ONE payment preference for this to work
        // you cannot accept multiple payment type when using an external gateway
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        String result;

        String orderId = (String) request.getAttribute("orderId");
        CheckOutHelper checkOutHelper = new CheckOutHelper(null, delegator, null);
        Map<String, Object> callResult = checkOutHelper.checkExternalPayment(orderId);

        //Generate any messages required
        ServiceUtil.getMessages(request, callResult, null);

        // any error messages have prepared for display, return the type ('error' if failed)
        result = (String) callResult.get("type");
        return result;
    }

    public static String finalizeOrderEntry(HttpServletRequest request, HttpServletResponse response) {
        ShoppingCart cart = (ShoppingCart) request.getSession().getAttribute("shoppingCart");
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");

        Map<String, Object> paramMap = UtilHttp.getParameterMap(request);
        String shippingContactMechId = null;
        String shippingMethod = null;
        BigDecimal shipEstimate = null;
        String shippingInstructions = null;
        String maySplit = null;
        String giftMessage = null;
        String isGift = null;
        String internalCode = null;
        String methodType = null;
        String shipBeforeDate = null;
        String shipAfterDate = null;
        String internalOrderNotes = null;
        String shippingNotes = null;
        String shipToPartyId = null;

        String mode = request.getParameter("finalizeMode");
        Debug.logInfo("FinalizeMode: " + mode, module);
        // necessary to avoid infinite looping when in a funny state, and will go right back to beginning
        if (mode == null) {
            return "customer";
        }

        // check the userLogin object
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");

        // if null then we must be an anonymous shopper
        if (userLogin == null) {
            // remove auto-login fields
            request.getSession().removeAttribute("autoUserLogin");
            request.getSession().removeAttribute("autoName");
            // clear out the login fields from the cart
            try {
                cart.setAutoUserLogin(null, dispatcher);
            } catch (CartItemModifyException e) {
                Debug.logError(e, module);
            }
        }

        // Reassign items requiring drop-shipping to new or existing drop-ship groups
        if ("init".equals(mode) || "default".equals(mode)) {
            try {
                cart.createDropShipGroups(dispatcher);
            } catch (CartItemModifyException e) {
                Debug.logError(e, module);
            }
        }

        // set the customer info
        if ("default".equals(mode)) {
            cart.setDefaultCheckoutOptions(dispatcher);
        }

        // remove the empty ship groups
        if ("removeEmptyShipGroups".equals(mode)) {
            cart.cleanUpShipGroups();
        }

        // set the customer info
        if ("cust".equals(mode)) {
            String partyId = (String) request.getAttribute("partyId");
            if (partyId != null) {
                cart.setOrderPartyId(partyId);
                // no userLogin means we are an anonymous shopper; fake the UL for service calls
                if (userLogin == null) {
                    try {
                        userLogin = EntityQuery.use(delegator).from("UserLogin").where("userLoginId", "anonymous").queryOne();
                    } catch (GenericEntityException e) {
                        Debug.logError(e, module);
                    }
                    if (userLogin != null) {
                        userLogin.set("partyId", partyId);
                    }
                    request.getSession().setAttribute("userLogin", userLogin);
                    try {
                        cart.setUserLogin(userLogin, dispatcher);
                    } catch (CartItemModifyException e) {
                        Debug.logError(e, module);
                    }
                    Debug.logInfo("Anonymous user-login has been activated", module);
                }
            }
        }

        if ("addpty".equals(mode)) {
            cart.setAttribute("addpty", "Y");
        }

        if ("term".equals(mode)) {
           cart.setOrderTermSet(true);
        }

        CheckOutHelper checkOutHelper = new CheckOutHelper(dispatcher, delegator, cart);

        // ====================================================================================
        if ("ship".equals(mode) || "options".equals(mode)) {
            Map<String, Object> callResult = ServiceUtil.returnSuccess();
            List<String> errorMessages = new ArrayList<String>();
            Map<String, Object> errorMaps = new HashMap<String, Object>();
            for (int shipGroupIndex = 0; shipGroupIndex < cart.getShipGroupSize(); shipGroupIndex++) {
                // set the shipping method
                if ("ship".equals(mode)) {
                    shippingContactMechId = request.getParameter(shipGroupIndex + "_shipping_contact_mech_id");
                    String facilityId = request.getParameter(shipGroupIndex + "_shipGroupFacilityId");
                    if (shippingContactMechId == null) {
                        shippingContactMechId = (String) request.getAttribute("contactMechId");
                    } else if("PURCHASE_ORDER".equals(cart.getOrderType())){
                        String[] shipInfo = shippingContactMechId.split("_@_");
                        if(shipInfo.length > 1){
                            shippingContactMechId = shipInfo[0];
                            facilityId = shipInfo[1];
                        }
                    }
                    String supplierPartyId = request.getParameter(shipGroupIndex + "_supplierPartyId");
                    if (UtilValidate.isNotEmpty(facilityId)) {
                        cart.setShipGroupFacilityId(shipGroupIndex, facilityId);
                    }
                    // If shipTo party is different than order party
                    shipToPartyId = request.getParameter("shipToPartyId");
                    if (UtilValidate.isNotEmpty(shipToPartyId)) {
                        cart.setShipToCustomerPartyId(shipToPartyId);
                    } else {
                        cart.setShipToCustomerPartyId(request.getParameter("orderPartyId"));
                    }
                    callResult = checkOutHelper.finalizeOrderEntryShip(shipGroupIndex, shippingContactMechId, supplierPartyId);
                    ServiceUtil.addErrors(errorMessages, errorMaps, callResult);
                }
                // set the options
                if ("options".equals(mode)) {
                    shippingMethod = request.getParameter(shipGroupIndex + "_shipping_method");
                    if (UtilValidate.isEmpty(shippingMethod)) {
                        shippingMethod = request.getParameter("shipping_method");
                    }
                    shippingInstructions = request.getParameter(shipGroupIndex + "_shipping_instructions");
                    if (UtilValidate.isEmpty(shippingInstructions))
                        shippingInstructions = request.getParameter("shipping_instructions");
                    maySplit = request.getParameter(shipGroupIndex + "_may_split");
                    if (UtilValidate.isEmpty(maySplit))
                        maySplit = request.getParameter("may_split");
                    giftMessage = request.getParameter(shipGroupIndex + "_gift_message");
                    isGift = request.getParameter(shipGroupIndex + "_is_gift");
                    internalCode = request.getParameter("internalCode"); // FIXME
                    shipBeforeDate = request.getParameter("sgi" + shipGroupIndex + "_shipBeforeDate");
                    shipAfterDate = request.getParameter("sgi" + shipGroupIndex + "_shipAfterDate");
                    internalOrderNotes = request.getParameter("internal_order_notes");
                    shippingNotes = request.getParameter("shippingNotes");
                    if (UtilValidate.isNotEmpty(request.getParameter(shipGroupIndex + "_ship_estimate"))) {
                        shipEstimate = new BigDecimal(request.getParameter(shipGroupIndex + "_ship_estimate"));
                    }
                    cart.clearOrderNotes();
                    cart.clearInternalOrderNotes();
                    if (shipEstimate == null) {  // allow ship estimate to be set manually if a purchase order
                        callResult = checkOutHelper.finalizeOrderEntryOptions(shipGroupIndex, shippingMethod, shippingInstructions, maySplit, giftMessage, isGift, internalCode, shipBeforeDate, shipAfterDate, internalOrderNotes, shippingNotes);
                    } else {
                        callResult = checkOutHelper.finalizeOrderEntryOptions(shipGroupIndex, shippingMethod, shippingInstructions, maySplit, giftMessage, isGift, internalCode, shipBeforeDate, shipAfterDate, internalOrderNotes, shippingNotes, shipEstimate);
                    }
                    ServiceUtil.addErrors(errorMessages, errorMaps, callResult);
                }
            }
            //See whether we need to return an error or not
            callResult = ServiceUtil.returnSuccess();
            if (errorMessages.size() > 0) {
                callResult.put(ModelService.ERROR_MESSAGE_LIST,  errorMessages);
                callResult.put(ModelService.RESPONSE_MESSAGE, ModelService.RESPOND_ERROR);
            }
            if (errorMaps.size() > 0) {
                callResult.put(ModelService.ERROR_MESSAGE_MAP, errorMaps);
                callResult.put(ModelService.RESPONSE_MESSAGE, ModelService.RESPOND_ERROR);
            }
            // generate any messages required
            ServiceUtil.getMessages(request, callResult, null);
            // determine whether it was a success or not
            if (callResult.get(ModelService.RESPONSE_MESSAGE).equals(ModelService.RESPOND_ERROR)) {
                if ("ship".equals(mode)) return "shipping";
                if ("options".equals(mode)) return "options";
                return "error";
            }
        }
        // ###############################################################################

        // check for offline payment type
        // payment option; if offline we skip the payment screen
        methodType = request.getParameter("paymentMethodType");
        if ("offline".equals(methodType)) {
            Debug.logInfo("Changing mode from->to: " + mode + "->payment", module);
            mode = "payment";
        }

        if ("payment".equals(mode)) {
            Map<String, Object> callResult = ServiceUtil.returnSuccess();
            List<String> errorMessages = new ArrayList<String>();
            Map<String, Object> errorMaps = new HashMap<String, Object>();

            // Set the payment options
            // SCIPIO: Handle new error cases
            Map<String, Map<String, Object>> selectedPaymentMethods;
            try {
                selectedPaymentMethods = getSelectedPaymentMethods(request);
            } catch (ServiceErrorException e) {
                Debug.logInfo(e.getMessage(), module); // regular error (probably user)
                ServiceUtil.appendMessageLists(request, e.getServiceResult());
                return "error";
            } catch (GeneralException e) {
                Debug.logError(e, module); // unexpected error
                request.setAttribute("_ERROR_MESSAGE_", UtilProperties.getMessage(resource_error, "OrderUnexpectedErrorHelp", (cart != null ? cart.getLocale() : Locale.getDefault())));
                return "error";
            }

            // Set the billing account (if any)
            String billingAccountId = request.getParameter("billingAccountId");
            if (UtilValidate.isNotEmpty(billingAccountId)) {
                BigDecimal billingAccountAmt = null;
                billingAccountAmt = determineBillingAccountAmount(billingAccountId, request.getParameter("billingAccountAmount"), dispatcher);
                if (billingAccountAmt == null) {
                    request.setAttribute("_ERROR_MESSAGE_", UtilProperties.getMessage(resource_error,"OrderInvalidAmountSetForBillingAccount", UtilMisc.toMap("billingAccountId",billingAccountId), (cart != null ? cart.getLocale() : Locale.getDefault())));
                    return "error";
                }
                selectedPaymentMethods.put("EXT_BILLACT", UtilMisc.<String, Object>toMap("amount", billingAccountAmt, "securityCode", null));
            }

            // If the user has just created a new payment method, add it to the map with a null amount, so that
            //  it becomes the sole payment method for the order.
            // SCIPIO: FIXME?: This is stock code and it appears to be deprecated or not work properly.
            String newPaymentMethodId = (String) request.getAttribute("paymentMethodId");
            if (! UtilValidate.isEmpty(newPaymentMethodId)) {
                selectedPaymentMethods.put(newPaymentMethodId, null);
                if (!selectedPaymentMethods.containsKey(newPaymentMethodId)) {
                    selectedPaymentMethods.put(newPaymentMethodId, UtilMisc.toMap("amount", null, "securityCode", null));
                }
            }

            // The selected payment methods are set
            errorMessages.addAll(checkOutHelper.setCheckOutPaymentInternal(selectedPaymentMethods, null, billingAccountId));
            // Verify if a gift card has been selected during order entry
            callResult = checkOutHelper.checkGiftCard(paramMap, selectedPaymentMethods);
            ServiceUtil.addErrors(errorMessages, errorMaps, callResult);
            if (errorMessages.size() == 0 && errorMaps.size() == 0) {
                String gcPaymentMethodId = (String) callResult.get("paymentMethodId");
                BigDecimal giftCardAmount = (BigDecimal) callResult.get("amount");

                // SCIPIO: Save the info of which paymentMethodId was just created for the new card
                if (gcPaymentMethodId != null) {
                    saveToNewPaymentMethodIdMap(request, "_NEW_GIFT_CARD_", gcPaymentMethodId);
                }

                // WARNING: if gcPaymentMethodId is not empty, all the previously set payment methods will be removed
                Map<String, Object> gcCallRes = checkOutHelper.finalizeOrderEntryPayment(gcPaymentMethodId, giftCardAmount, true, true);
                ServiceUtil.addErrors(errorMessages, errorMaps, gcCallRes);
            }
            //See whether we need to return an error or not
            callResult = ServiceUtil.returnSuccess();
            if (errorMessages.size() > 0) {
                callResult.put(ModelService.ERROR_MESSAGE_LIST, errorMessages);
                callResult.put(ModelService.RESPONSE_MESSAGE, ModelService.RESPOND_ERROR);
            }
            if (errorMaps.size() > 0) {
                callResult.put(ModelService.ERROR_MESSAGE_MAP, errorMaps);
                callResult.put(ModelService.RESPONSE_MESSAGE, ModelService.RESPOND_ERROR);
            }
            // generate any messages required
            ServiceUtil.getMessages(request, callResult, null);
            // determine whether it was a success or not
            if (callResult.get(ModelService.RESPONSE_MESSAGE).equals(ModelService.RESPOND_ERROR)) {
                return "paymentError";
            }
        }
        // determine where to direct the browser
        return determineNextFinalizeStep(request, response);
    }

    public static String determineNextFinalizeStep(HttpServletRequest request, HttpServletResponse response) {
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        ShoppingCart cart = (ShoppingCart) request.getSession().getAttribute("shoppingCart");
        // flag anoymous checkout to bypass additional party settings
        boolean isAnonymousCheckout = false;
        if (userLogin != null && "anonymous".equals(userLogin.getString("userLoginId"))) {
            isAnonymousCheckout = true;
        }

        // determine where to direct the browser
        // these are the default values
        boolean requireCustomer = true;
        boolean requireNewShippingAddress = false;
        boolean requireShipping = true;
        boolean requireOptions = true;
        boolean requireShipGroups = false;
        boolean requirePayment = !"PURCHASE_ORDER".equals(cart.getOrderType());
        boolean requireTerm = true;
        boolean requireAdditionalParty = isAnonymousCheckout;
        boolean isSingleUsePayment = true;
        // these options are not available to anonymous shoppers (security)
        if (userLogin != null && !"anonymous".equals(userLogin.getString("userLoginId"))) {
            String requireCustomerStr = request.getParameter("finalizeReqCustInfo");
            String requireNewShippingAddressStr = request.getParameter("finalizeReqNewShipAddress");
            String requireShippingStr = request.getParameter("finalizeReqShipInfo");
            String requireOptionsStr = request.getParameter("finalizeReqOptions");
            String requirePaymentStr = request.getParameter("finalizeReqPayInfo");
            String requireTermStr = request.getParameter("finalizeReqTermInfo");
            String requireAdditionalPartyStr = request.getParameter("finalizeReqAdditionalParty");
            String requireShipGroupsStr = request.getParameter("finalizeReqShipGroups");
            String singleUsePaymentStr = request.getParameter("singleUsePayment");
            requireCustomer = requireCustomerStr == null || "true".equalsIgnoreCase(requireCustomerStr);
            requireNewShippingAddress = requireNewShippingAddressStr != null && "true".equalsIgnoreCase(requireNewShippingAddressStr);
            requireShipping = requireShippingStr == null || "true".equalsIgnoreCase(requireShippingStr);
            requireOptions = requireOptionsStr == null || "true".equalsIgnoreCase(requireOptionsStr);
            requireShipGroups = requireShipGroupsStr != null && "true".equalsIgnoreCase(requireShipGroupsStr);
            if (requirePayment) {
                requirePayment = requirePaymentStr == null || "true".equalsIgnoreCase(requirePaymentStr);
            }
            if (requireTerm) {
                requireTerm = requireTermStr == null || "true".equalsIgnoreCase(requireTermStr);
            }
            requireAdditionalParty = requireAdditionalPartyStr == null || "true".equalsIgnoreCase(requireAdditionalPartyStr);
            isSingleUsePayment = singleUsePaymentStr != null && "Y".equalsIgnoreCase(singleUsePaymentStr) ? true : false;
        }

        boolean shippingAddressSet = true;
        boolean shippingOptionsSet = true;
        for (int shipGroupIndex = 0; shipGroupIndex < cart.getShipGroupSize(); shipGroupIndex++) {
            String shipContactMechId = cart.getShippingContactMechId(shipGroupIndex);
            if (shipContactMechId == null) {
                shippingAddressSet = false;
            }
            String shipmentMethodTypeId = cart.getShipmentMethodTypeId(shipGroupIndex);
            if (shipmentMethodTypeId == null) {
                shippingOptionsSet = false;
            }
        }

        String customerPartyId = cart.getPartyId();

        String[] processOrder = {"customer", "shipping", "shipGroups", "options", "term", "payment",
                                 "addparty", "paysplit"};

        if ("PURCHASE_ORDER".equals(cart.getOrderType())) {
            // Force checks for the following
            requireCustomer = true; requireShipping = true; requireOptions = true;
            processOrder = new String[] {"customer", "term", "shipping", "shipGroups", "options", "payment",
                                         "addparty", "paysplit"};
        }

        for (int i = 0; i < processOrder.length; i++) {
            String currProcess = processOrder[i];
            if ("customer".equals(currProcess)) {
                if (requireCustomer && (customerPartyId == null || "_NA_".equals(customerPartyId))) {
                    return "customer";
                }
            } else if ("shipping".equals(currProcess)) {
                if (requireShipping) {
                    if (requireNewShippingAddress) {
                        return "shippingAddress";
                    } else if (!shippingAddressSet) {
                        return "shipping";
                    }
                }
            } else if ("shipGroups".equals(currProcess)) {
                if (requireShipGroups) {
                    return "shipGroups";
                }
            } else if ("options".equals(currProcess)) {
                if (requireOptions && !shippingOptionsSet) {
                    return "options";
                }
            } else if ("term".equals(currProcess)) {
                if (requireTerm && !cart.isOrderTermSet()) {
                    return "term";
                }
            } else if ("payment".equals(currProcess)) {
                List<String> paymentMethodIds = cart.getPaymentMethodIds();
                List<String> paymentMethodTypeIds = cart.getPaymentMethodTypeIds();
                if (requirePayment && UtilValidate.isEmpty(paymentMethodIds) && UtilValidate.isEmpty(paymentMethodTypeIds)) {
                    return "payment";
                }
            } else if ("addparty".equals(currProcess)) {
                if (requireAdditionalParty && cart.getAttribute("addpty") == null) {
                    return "addparty";
                }
            } else if ("paysplit".equals(currProcess)) {
                if (isSingleUsePayment) {
                    return "paysplit";
                }
            }
        }

        // Finally, if all checks go through, finalize the order.

       // this is used to go back to a previous page in checkout after processing all of the changes, just to make sure we get everything...
        String checkoutGoTo = request.getParameter("checkoutGoTo");
        if (UtilValidate.isNotEmpty(checkoutGoTo)) {
            return checkoutGoTo;
        }

        if ("SALES_ORDER".equals(cart.getOrderType())) {
            return "sales";
        } else {
            return "po";
        }
    }

    public static String finalizeOrderEntryError(HttpServletRequest request, HttpServletResponse response) {
        String finalizePage = request.getParameter("finalizeMode");
        if (UtilValidate.isEmpty(finalizePage)) {
            return "error";
        } else {
            return finalizePage;
        }
    }

    /**
     * Determine what billing account amount to use based on the form input.
     * This method returns the amount that will be charged to the billing account.
     *
     * An amount can be associated with the billingAccountId with a
     * parameter billingAccountAmount.  If no amount is specified, then
     * the entire available balance of the given billing account will be used.
     * If there is an error, a null will be returned.
     *
     * @return  Amount to charge billing account or null if there was an error
     */
    private static BigDecimal determineBillingAccountAmount(String billingAccountId, String billingAccountAmount, LocalDispatcher dispatcher) {
        BigDecimal billingAccountAmt = null;

        // set the billing account amount to the minimum of billing account available balance or amount input if less than balance
        if (UtilValidate.isNotEmpty(billingAccountId)) {
            // parse the amount to a decimal
            if (UtilValidate.isNotEmpty(billingAccountAmount)) {
                try {
                    billingAccountAmt = new BigDecimal(billingAccountAmount);
                } catch (NumberFormatException e) {
                    return null;
                }
            }
            if (billingAccountAmt == null) {
                billingAccountAmt = BigDecimal.ZERO;
            }
            BigDecimal availableBalance = CheckOutHelper.availableAccountBalance(billingAccountId, dispatcher);

            // set amount to be charged to entered amount unless it exceeds the available balance
            BigDecimal chargeAmount = BigDecimal.ZERO;
            if (billingAccountAmt.compareTo(availableBalance) < 0) {
                chargeAmount = billingAccountAmt;
            } else {
                chargeAmount = availableBalance;
            }
            if (chargeAmount.compareTo(BigDecimal.ZERO) < 0.0) {
                chargeAmount = BigDecimal.ZERO;
            }

            return chargeAmount;
        } else {
            return null;
        }
    }

    /** Create a replacement order from an existing order against a lost shipment etc. **/
    public static String createReplacementOrder(HttpServletRequest request, HttpServletResponse response) {
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        HttpSession session = request.getSession();
        GenericValue userLogin = (GenericValue) session.getAttribute("userLogin");
        ShoppingCart cart = (ShoppingCart) request.getSession().getAttribute("shoppingCart");

        Map<String, Object> context = cart.makeCartMap(dispatcher, false);
        String originalOrderId = request.getParameter("orderId");

        // create the replacement order adjustment
        List <GenericValue>orderAdjustments = UtilGenerics.checkList(context.get("orderAdjustments"));
        List <GenericValue>orderItems = UtilGenerics.checkList(context.get("orderItems"));
        OrderReadHelper orderReadHelper = new OrderReadHelper(orderAdjustments, orderItems);
        BigDecimal grandTotal = orderReadHelper.getOrderGrandTotal();
        if (grandTotal.compareTo(new BigDecimal(0)) != 0) {
            GenericValue adjustment = delegator.makeValue("OrderAdjustment");
            adjustment.set("orderAdjustmentTypeId", "REPLACE_ADJUSTMENT");
            adjustment.set("amount", grandTotal.negate());
            adjustment.set("comments", "ReShip Order for Order #" + originalOrderId);
            adjustment.set("createdDate", UtilDateTime.nowTimestamp());
            adjustment.set("createdByUserLogin", userLogin.getString("userLoginId"));
            cart.addAdjustment(adjustment);
        }
        // create the order association
        List<ShoppingCartItem> cartLines = cart.items();
        for (ShoppingCartItem sci : cartLines) {
            int index = cart.getItemIndex(sci);
            try {
                GenericValue orderItem = EntityQuery.use(delegator).from("OrderItem")
                                             .where("orderId", originalOrderId, "isPromo", sci.getIsPromo() ? "Y" : "N",
                                                     "productId", sci.getProductId(), "orderItemTypeId", sci.getItemType())
                                             .queryFirst();
                if (orderItem != null) {
                    sci.setAssociatedOrderId(orderItem.getString("orderId"));
                    sci.setAssociatedOrderItemSeqId(orderItem.getString("orderItemSeqId"));
                    sci.setOrderItemAssocTypeId("REPLACEMENT");
                    cart.addItem(index, sci);
                }
            } catch (GenericEntityException e) {
                Debug.logError(e, module);
            } catch (CartItemModifyException e) {
                Debug.logError(e.getMessage(), module);
            }
        }

        String result = createOrder(request, response);
        if ("error".equals(result)) {
            return "error";
        } else {
            return "success";
        }
    }

    /**
     * SCIPIO: Checks a ship contact meth ID; if it has the special value _NEW_, it will check other
     * params and try to create a new address before returning the new contact mech ID. This allows inlining
     * new ship address forms.
     * <p>
     * Mostly for checkout's shipping_contact_mech_id.
     * <p>
     * NOTE: This checks request attribs before request parameters so that other events may influence.
     * <p>
     * NOTE: This also sets a newShipContactMechInfoMap map in request attributes that screens may need.
     */
    public static Map<String, Object> checkShipContactMechIdForNew(HttpServletRequest request, String contactMechId, String paramPrefix) throws GeneralException {
        String origContactMechId = contactMechId;
        if ("_NEW_".equals(contactMechId)) {
            // We need extra validation here because createPostalAddressAndPurposes is too generic and fails to do it and this is faster than making extra service

            Collection<MapProcessor> paramValidators = null;
            String serviceName;
            String setShippingPurpose = getRequestAttribOrParam(request, paramPrefix + "setShippingPurpose");
            Map<String, Object> overrideParams = UtilMisc.toMap("setBillingPurpose", null);

            // We can only set either setShippingPurpose or contactMechPurposeTypeId here
            if ("Y".equals(setShippingPurpose)) {
                overrideParams.put("setShippingPurpose", "Y");
                overrideParams.put("contactMechPurposeTypeId", null);
            } else {
                overrideParams.put("setShippingPurpose", null);
                overrideParams.put("contactMechPurposeTypeId", "SHIPPING_LOCATION");
            }

            // SPECIAL CASE: Events may request that new record creation be disabled. In this case, return nothing.
            if (Boolean.FALSE.equals(request.getAttribute("checkoutUseNewRecords"))) {
                Map<String, Object> result = ServiceUtil.returnSuccess();
                result.put("contactMechId", null);
                result.put("origContactMechId", origContactMechId);
                return result;
            }

            paramValidators = UtilMisc.<MapProcessor>toList(getPostalAddressValidator(request));
            serviceName = "createPostalAddressAndPurposes";

            Map<String, Object> servResult = runServiceFromParams(request, paramPrefix, serviceName, overrideParams, paramValidators);

            if (!ServiceUtil.isSuccess(servResult)) {
                Debug.logInfo("Could not create new ship contact mech during checkout: " + ServiceUtil.getErrorMessage(servResult), module);
                return servResult;
            } else {
                contactMechId = (String) servResult.get("contactMechId");
            }

            Map<String, Object> newShipContactMechInfoMap = UtilGenerics.checkMap(request.getAttribute("newShipContactMechInfoMap"));
            if (newShipContactMechInfoMap == null) {
                newShipContactMechInfoMap = new HashMap<String, Object>();
            }
            Map<String, Object> info = new HashMap<String, Object>();
            info.put("contactMechId", contactMechId);
            newShipContactMechInfoMap.put(origContactMechId, info);
            request.setAttribute("newShipContactMechInfoMap", newShipContactMechInfoMap);

            Map<String, Object> result = ServiceUtil.returnSuccess();
            result.put("contactMechId", contactMechId);
            result.put("origContactMechId", origContactMechId);
            result.put("paramPrefix", paramPrefix);
            return result;
        } else {
            Map<String, Object> result = ServiceUtil.returnSuccess();
            result.put("contactMechId", contactMechId);
            result.put("origContactMechId", origContactMechId);
            return result;
        }
    }

    /**
     * SCIPIO: Checks a pay method contact meth ID; if it has the special value _NEW_CREDIT_CARD_ or _NEW_EFT_ACCOUNT_ as prefix, it will check other
     * params and try to create new records before returning the new contact mech ID. This allows inlining
     * new ship address forms. NOTE: it can support multiple of each.
     * <p>
     * Mostly for checkout's checkOutPaymentId.
     * <p>
     * NOTE: This checks request attribs before request parameters so that other events may influence.
     * <p>
     * NOTE: This also sets a newPaymentMethodInfoMap map in request attributes that screens may need.
     */
    public static Map<String, Object> checkPaymentMethodIdForNew(HttpServletRequest request, String paymentMethodId,
            String ccParamPrefix, String eftParamPrefix) throws GeneralException {

        String origPaymentMethodId = paymentMethodId;
        Collection<MapProcessor> paramValidators = null;
        String serviceName;
        Map<String, Object> overrideParams = new HashMap<String, Object>();
        String paramPrefix;

        if (paymentMethodId != null && paymentMethodId.startsWith("_NEW_CREDIT_CARD_")) {
            // SPECIAL CASE: Events may request that new record creation be disabled. In this case, return nothing.
            if (Boolean.FALSE.equals(request.getAttribute("checkoutUseNewRecords"))) {
                Map<String, Object> result = ServiceUtil.returnSuccess();
                result.put("paymentMethodId", null);
                result.put("origPaymentMethodId", origPaymentMethodId);
                return result;
            }

            paramPrefix = ccParamPrefix + paymentMethodId.substring("_NEW_CREDIT_CARD_".length());
            String addrContactMechId = getRequestAttribOrParam(request, paramPrefix + "contactMechId");
            if (UtilValidate.isNotEmpty(addrContactMechId)) {
                if ("_NEW_".equals(addrContactMechId)) {
                    // SCIPIO: NOTE: Unlike stock code elsewhere, here we assume the _NEW_ is accompanied with
                    // other inline stuff already (not delayed to another screen)
                    serviceName = "createCreditCardAndAddress";
                    paramValidators = UtilMisc.<MapProcessor>toList(
                            SimpleMapProcessorProcessor.getInstance(request, "component://accounting/script/org/ofbiz/accounting/payment/PaymentMapProcs.xml",
                                    "createCreditCard"),
                            getPostalAddressValidator(request)
                            );
                    overrideParams.put("contactMechId", null);
                } else {
                    serviceName = "createCreditCard";
                    paramValidators = UtilMisc.<MapProcessor>toList(
                            SimpleMapProcessorProcessor.getInstance(request, "component://accounting/script/org/ofbiz/accounting/payment/PaymentMapProcs.xml",
                            "createCreditCard")
                            );
                }
            } else {
                // TODO: Localize
                // NOTE: The schema doesn't enforce this
                return ServiceUtil.returnError("No contact mech (billing address) specified for new credit card");
            }
        } else if (paymentMethodId != null && paymentMethodId.startsWith("_NEW_EFT_ACCOUNT_")) {
            // SPECIAL CASE: Events may request that new record creation be disabled. In this case, return nothing.
            if (Boolean.FALSE.equals(request.getAttribute("checkoutUseNewRecords"))) {
                Map<String, Object> result = ServiceUtil.returnSuccess();
                result.put("paymentMethodId", null);
                result.put("origPaymentMethodId", origPaymentMethodId);
                return result;
            }

            paramPrefix = eftParamPrefix + paymentMethodId.substring("_NEW_EFT_ACCOUNT_".length());
            String addrContactMechId = getRequestAttribOrParam(request, paramPrefix + "contactMechId");
            if (UtilValidate.isNotEmpty(addrContactMechId)) {
                if ("_NEW_".equals(addrContactMechId)) {
                    serviceName = "createEftAccountAndAddress";
                    paramValidators = UtilMisc.<MapProcessor>toList(
                            SimpleMapProcessorProcessor.getInstance(request, "component://accounting/script/org/ofbiz/accounting/payment/PaymentMapProcs.xml",
                                    "createEftAccount"),
                            getPostalAddressValidator(request)
                            );
                    overrideParams.put("contactMechId", null);
                } else {
                    serviceName = "createEftAccount";
                    paramValidators = UtilMisc.<MapProcessor>toList(
                            SimpleMapProcessorProcessor.getInstance(request, "component://accounting/script/org/ofbiz/accounting/payment/PaymentMapProcs.xml",
                            "createEftAccount")
                            );
                }
            } else {
                // TODO: Localize
                // NOTE: The schema doesn't enforce this
                return ServiceUtil.returnError("No contact mech (billing address) specified for new EFT account");
            }
        } else {
            Map<String, Object> result = ServiceUtil.returnSuccess();
            result.put("paymentMethodId", paymentMethodId);
            result.put("origPaymentMethodId", origPaymentMethodId);
            return result;
        }

        Map<String, Object> servResult = runServiceFromParams(request, paramPrefix, serviceName, overrideParams, paramValidators);

        if (!ServiceUtil.isSuccess(servResult)) {
            Debug.logInfo("Could not create new pay method during checkout: " + ServiceUtil.getErrorMessage(servResult), module);
            return servResult;
        } else {
            paymentMethodId = (String) servResult.get("paymentMethodId");
        }

        // UPDATED: If we successfully created a pay method during this request, set a map in request attributes
        // that maps the _NEW_xxxx to the new ID. Screens may need this to work around lack of global event transactions and param preselection issues.
        saveToNewPaymentMethodIdMap(request, origPaymentMethodId, paymentMethodId);

        Map<String, Object> result = ServiceUtil.returnSuccess();
        result.put("paymentMethodId", paymentMethodId);
        result.put("origPaymentMethodId", origPaymentMethodId);
        result.put("paramPrefix", paramPrefix);
        return result;
    }

    static void saveToNewPaymentMethodIdMap(HttpServletRequest request, String origId, String paymentMethodId)  {
        Map<String, Object> newPaymentMethodInfoMap = UtilGenerics.checkMap(request.getAttribute("newPaymentMethodIdMap"));
        if (newPaymentMethodInfoMap == null) {
            newPaymentMethodInfoMap = new HashMap<String, Object>();
        }
        Map<String, Object> info = new HashMap<String, Object>();
        info.put("paymentMethodId", paymentMethodId);
        newPaymentMethodInfoMap.put(origId, info);
        request.setAttribute("newPaymentMethodInfoMap", newPaymentMethodInfoMap);

    }

    public static MapProcessor getPostalAddressValidator(HttpServletRequest request) {
        return SimpleMapProcessorProcessor.getInstance(request, "component://party/script/org/ofbiz/party/contact/PartyContactMechMapProcs.xml",
                "postalAddress");
    }


    /**
     * SCIPIO: Service invocation helper; uses combine req attribs + params.
     * <p>
     * NOTE: This checks request attribs before request parameters so that other events may influence.
     */
    public static Map<String, Object> runServiceFromParams(HttpServletRequest request, String paramPrefix,
            String serviceName, Map<String, Object> overrideParams, Collection<MapProcessor> paramValidators) throws GeneralException {

        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");

        Map<String, Object> combinedMap = UtilHttp.getCombinedMap(request);
        Map<String, Object> context;
        if (paramPrefix.isEmpty()) {
            context = combinedMap;
        } else {
            context = UtilMisc.getPrefixedMapEntries(combinedMap, paramPrefix);
        }

        context.putAll(overrideParams);
        if (paramValidators != null && !paramValidators.isEmpty()) {
            MapProcessorInvoker mpi = new MapProcessorInvoker(context, UtilHttp.getLocale(request));
            mpi.process(paramValidators, true, false);
            List<String> errorMessages = mpi.getAllErrorMessages();
            if (!errorMessages.isEmpty()) {
                Map<String, Object> validateRes = ServiceUtil.returnError(errorMessages);
                Debug.logInfo("Could not validate fields: " + ServiceUtil.getErrorMessage(validateRes), module);
                return validateRes;
            }
        }
        Map<String, Object> servCtx = dispatcher.getDispatchContext().makeValidContext(serviceName, ModelService.IN_PARAM, context);
        servCtx.put("userLogin", combinedMap.get("userLogin"));
        servCtx.put("locale", combinedMap.get("locale"));
        return dispatcher.runSync(serviceName, servCtx);
    }

    /**
     * SCIPIO: Local util to get request attrib or param of same name. If request attrib is null, falls back
     * to params.
     * <p>
     * <strong>WARN</strong>: If request attrib is non-null but empty, params are ignored. This is
     * intentional, to allow empty as an override value.
     */
    static String getRequestAttribOrParam(HttpServletRequest request, String name) {
        String res;
        Object resObj = request.getAttribute(name);
        if (resObj != null) {
            if (resObj instanceof String) {
                res = (String) resObj;
            } else {
                res = resObj.toString();
                Debug.logWarning("Scipio: WARNING: Reading non-string request attribute '" + name +
                        "' as string (value: '" + res + "')", module);
            }
        } else {
            res = request.getParameter(name);
        }
        return res;
    }

    static String getRequestAttribOrParamPrefix(HttpServletRequest request, String name) {
        if (name == null || name.isEmpty()) {
            return "";
        }
        String res = getRequestAttribOrParam(request, name);
        if (res == null) {
            res = "";
        }
        return res;
    }

    // SCIPIO: Alternative pattern meant for integration with setCheckoutError; not yet needed.
//    /**
//     * SCIPIO: This creates a note in the request of the type of error we ran into.
//     */
//    static void registerCheckoutError(HttpServletRequest request, String errorType) {
//        List<String> errors = getCheckoutErrors(request);
//        errors.add(errorType);
//        request.setAttribute("checkoutErrors", errors);
//    }
//
//    /**
//     * SCIPIO: Gets a list of checkout error types recorded during the request so far.
//     */
//    static List<String> getCheckoutErrors(HttpServletRequest request) {
//        List<String> errors = UtilGenerics.checkList(request.getAttribute("checkoutErrors"));
//        if (errors == null) {
//            errors = new LinkedList<String>();
//        }
//        return errors;
//    }

}
