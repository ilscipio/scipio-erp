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
package org.ofbiz.order.shoppingcart.shipping;

import java.math.BigDecimal;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Locale;
import java.util.Map;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.GeneralException;
import org.ofbiz.base.util.UtilMisc;
import org.ofbiz.base.util.UtilProperties;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.GenericEntityException;
import org.ofbiz.entity.GenericValue;
import org.ofbiz.entity.condition.EntityCondition;
import org.ofbiz.entity.condition.EntityConditionList;
import org.ofbiz.entity.condition.EntityOperator;
import org.ofbiz.entity.util.EntityQuery;
import org.ofbiz.entity.util.EntityUtil;
import org.ofbiz.entity.util.EntityUtilProperties;
import org.ofbiz.order.order.OrderReadHelper;
import org.ofbiz.order.shoppingcart.ShoppingCart;
import org.ofbiz.order.shoppingcart.product.ProductPromoWorker;
import org.ofbiz.product.store.ProductStoreWorker;
import org.ofbiz.service.GenericServiceException;
import org.ofbiz.service.LocalDispatcher;
import org.ofbiz.service.ServiceErrorException;
import org.ofbiz.service.ServiceUtil;

/**
 * ShippingEvents - Events used for processing shipping fees
 */
public class ShippingEvents {

    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());
    private static final String resource_error = "OrderErrorUiLabels"; // SCIPIO

    public static String getShipEstimate(HttpServletRequest request, HttpServletResponse response) {
        ShoppingCart cart = (ShoppingCart) request.getSession().getAttribute("shoppingCart");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Delegator delegator = (Delegator) request.getAttribute("delegator");

        int shipGroups = cart.getShipGroupSize();
        for (int i = 0; i < shipGroups; i++) {
            String shipmentMethodTypeId = cart.getShipmentMethodTypeId(i);
            if (UtilValidate.isEmpty(shipmentMethodTypeId)) {
                continue;
            }
            Map<String, Object> result = getShipGroupEstimate(dispatcher, delegator, cart, i);
            ServiceUtil.getMessages(request, result, null, "", "", "", "", null, null);
            // SCIPIO
            //if (result.get(ModelService.RESPONSE_MESSAGE).equals(ModelService.RESPOND_ERROR)) {
            if (ServiceUtil.isError(result)) {
                return "error";
            }

            BigDecimal shippingTotal = (BigDecimal) result.get("shippingTotal");
            if (shippingTotal == null) {
                shippingTotal = BigDecimal.ZERO;
            }
            cart.setItemShipGroupEstimate(shippingTotal, i);
        }

        ProductPromoWorker.doPromotions(cart, dispatcher);
        // all done
        return "success";
    }

    public static Map<String, Object> getShipGroupEstimate(LocalDispatcher dispatcher, Delegator delegator, ShoppingCart cart, int groupNo) {
        // check for shippable items
        if (!cart.shippingApplies()) {
            Map<String, Object> responseResult = ServiceUtil.returnSuccess();
            responseResult.put("shippingTotal", BigDecimal.ZERO);
            return responseResult;
        }

        String shipmentMethodTypeId = cart.getShipmentMethodTypeId(groupNo);
        String carrierPartyId = cart.getCarrierPartyId(groupNo);
        String productStoreShipMethId = cart.getProductStoreShipMethId(groupNo);

        return getShipGroupEstimate(dispatcher, delegator, cart.getLocale(), cart.getOrderType(), shipmentMethodTypeId, carrierPartyId, null, // SCIPIO: 2018-11-09: Added locale
                cart.getShippingContactMechId(groupNo), cart.getProductStoreId(), cart.getSupplierPartyId(groupNo), cart.getShippableItemInfo(groupNo),
                cart.getShippableWeight(groupNo), cart.getShippableQuantity(groupNo), cart.getShippableTotal(groupNo), cart.getPartyId(), productStoreShipMethId, cart.isAllowMissingShipEstimates());
    }

    public static Map<String, Object> getShipEstimate(LocalDispatcher dispatcher, Delegator delegator, Locale locale, OrderReadHelper orh, String shipGroupSeqId,
            boolean allowMissingShipEstimates) { // SCIPIO: 2018-11-09: Added locale, allowMissingShipEstimates
        // check for shippable items
        if (!orh.shippingApplies()) {
            Map<String, Object> responseResult = ServiceUtil.returnSuccess();
            responseResult.put("shippingTotal", BigDecimal.ZERO);
            return responseResult;
        }

        GenericValue shipGroup = orh.getOrderItemShipGroup(shipGroupSeqId);
        String shipmentMethodTypeId = shipGroup.getString("shipmentMethodTypeId");
        String carrierRoleTypeId = shipGroup.getString("carrierRoleTypeId");
        String carrierPartyId = shipGroup.getString("carrierPartyId");
        String supplierPartyId = shipGroup.getString("supplierPartyId");

        GenericValue shipAddr = orh.getShippingAddress(shipGroupSeqId);
        if (shipAddr == null) {
            return UtilMisc.<String, Object>toMap("shippingTotal", BigDecimal.ZERO);
        }

        String contactMechId = shipAddr.getString("contactMechId");
        String partyId = null;
        GenericValue partyObject = orh.getPlacingParty();
        if (partyObject != null) {
             partyId = partyObject.getString("partyId");
        }
        return getShipGroupEstimate(dispatcher, delegator, locale, orh.getOrderTypeId(), shipmentMethodTypeId, carrierPartyId, carrierRoleTypeId,
                contactMechId, orh.getProductStoreId(), supplierPartyId, orh.getShippableItemInfo(shipGroupSeqId), orh.getShippableWeight(shipGroupSeqId),
                orh.getShippableQuantity(shipGroupSeqId), orh.getShippableTotal(shipGroupSeqId), partyId, null, allowMissingShipEstimates);
    }

    /**
     * @deprecated SCIPIO: 2018-11-09: Use overload with Locale instead.
     */
    @Deprecated
    public static Map<String, Object> getShipEstimate(LocalDispatcher dispatcher, Delegator delegator, OrderReadHelper orh, String shipGroupSeqId) {
        return getShipEstimate(dispatcher, delegator, Locale.getDefault(), orh, shipGroupSeqId, false);
    }

    // version with no support for using the supplier's address as the origin
    public static Map<String, Object> getShipGroupEstimate(LocalDispatcher dispatcher, Delegator delegator, Locale locale, String orderTypeId, // SCIPIO: 2018-11-09: Added locale, allowMissingShipEstimates
            String shipmentMethodTypeId, String carrierPartyId, String carrierRoleTypeId, String shippingContactMechId,
            String productStoreId, List<Map<String, Object>> itemInfo, BigDecimal shippableWeight, BigDecimal shippableQuantity,
            BigDecimal shippableTotal, String partyId, String productStoreShipMethId, boolean allowMissingShipEstimates) {
        return getShipGroupEstimate(dispatcher, delegator, locale, orderTypeId, shipmentMethodTypeId, carrierPartyId,
                carrierRoleTypeId, shippingContactMechId, productStoreId, null, itemInfo,
                shippableWeight, shippableQuantity, shippableTotal, partyId, productStoreShipMethId, allowMissingShipEstimates);
    }

    /**
     * @deprecated SCIPIO: 2018-11-09: Use overload with Locale instead.
     */
    @Deprecated
    public static Map<String, Object> getShipGroupEstimate(LocalDispatcher dispatcher, Delegator delegator, String orderTypeId,
            String shipmentMethodTypeId, String carrierPartyId, String carrierRoleTypeId, String shippingContactMechId,
            String productStoreId, List<Map<String, Object>> itemInfo, BigDecimal shippableWeight, BigDecimal shippableQuantity,
            BigDecimal shippableTotal, String partyId, String productStoreShipMethId) {
        return getShipGroupEstimate(dispatcher, delegator, Locale.getDefault(), orderTypeId, shipmentMethodTypeId, carrierPartyId,
                carrierRoleTypeId, shippingContactMechId, productStoreId, null, itemInfo,
                shippableWeight, shippableQuantity, shippableTotal, partyId, productStoreShipMethId, false);
    }

    /**
     * @deprecated SCIPIO: 2018-11-09: Use overload with Locale instead.
     */
    @Deprecated
    public static Map<String, Object> getShipGroupEstimate(LocalDispatcher dispatcher, Delegator delegator, String orderTypeId,
            String shipmentMethodTypeId, String carrierPartyId, String carrierRoleTypeId, String shippingContactMechId,
            String productStoreId, String supplierPartyId, List<Map<String, Object>> itemInfo, BigDecimal shippableWeight, BigDecimal shippableQuantity,
            BigDecimal shippableTotal, String partyId, String productStoreShipMethId) {
        return getShipGroupEstimate(dispatcher, delegator, Locale.getDefault(), orderTypeId, 
                shipmentMethodTypeId, carrierPartyId, carrierRoleTypeId, shippingContactMechId, 
                productStoreId, supplierPartyId, itemInfo, shippableWeight, shippableQuantity, 
                shippableTotal, partyId, productStoreShipMethId, false);
    }

    public static Map<String, Object> getShipGroupEstimate(LocalDispatcher dispatcher, Delegator delegator, Locale locale, String orderTypeId, // SCIPIO: 2018-11-09: Added locale, allowMissingShipEstimates
            String shipmentMethodTypeId, String carrierPartyId, String carrierRoleTypeId, String shippingContactMechId,
            String productStoreId, String supplierPartyId, List<Map<String, Object>> itemInfo, BigDecimal shippableWeight, BigDecimal shippableQuantity,
            BigDecimal shippableTotal, String partyId, String productStoreShipMethId, boolean allowMissingShipEstimates) {
        // SCIPIO: This message assumes too much about the caller's intentions. Leave out the second part.
        //String standardMessage = "A problem occurred calculating shipping. Fees will be calculated offline.";
        //String standardMessage = UtilProperties.getMessage(resource_error, "shippingevents.problem_calculating_shipping", locale);
        List<String> errorMessageList = new LinkedList<String>();

        if ("NO_SHIPPING".equals(shipmentMethodTypeId)) {
            return ServiceUtil.returnSuccess();
        }

        if (shipmentMethodTypeId == null || carrierPartyId == null) {
            if ("SALES_ORDER".equals(orderTypeId)) {
                // SCIPIO
                //errorMessageList.add("Please Select Your Shipping Method.");
                errorMessageList.add(UtilProperties.getMessage(resource_error, "shippingevents.select_shipping_method", locale));
                return ServiceUtil.returnError(errorMessageList);
            } else {
                return ServiceUtil.returnSuccess();
            }
        }

        if (carrierRoleTypeId == null) {
            carrierRoleTypeId = "CARRIER";
        }

        // if as supplier is associated, then we have a drop shipment and should use the origin shipment address of it
        String shippingOriginContactMechId = null;
        if (supplierPartyId != null) {
            try {
                GenericValue originAddress = getShippingOriginContactMech(delegator, supplierPartyId);
                if (originAddress == null) {
                    // SCIPIO
                    //return ServiceUtil.returnError("Cannot find the origin shipping address (SHIP_ORIG_LOCATION) for the supplier with ID ["+supplierPartyId+"]. Will not be able to calculate drop shipment estimate.");
                    return ServiceUtil.returnError(UtilProperties.getMessage(resource_error, "shippingevents.cannot_find_origin_ship_addr", UtilMisc.toMap("supplierPartyId", supplierPartyId), locale));
                }
                shippingOriginContactMechId = originAddress.getString("contactMechId");
            } catch (GeneralException e) {
                return ServiceUtil.returnError(UtilProperties.getMessage(resource_error, "shippingevents.problem_calculating_shipping_try_again", locale));
            }
        }

        // no shippable items; we won't charge any shipping at all
        if (shippableQuantity.compareTo(BigDecimal.ZERO) == 0) {
            Map<String, Object> result = ServiceUtil.returnSuccess();
            result.put("shippingTotal", BigDecimal.ZERO);
            return result;
        }

        // check for an external service call
        GenericValue storeShipMethod = ProductStoreWorker.getProductStoreShipmentMethod(delegator, productStoreId,
                shipmentMethodTypeId, carrierPartyId, carrierRoleTypeId);

        if (storeShipMethod == null) {
            // SCIPIO
            //errorMessageList.add("No applicable shipment method found.");
            errorMessageList.add(UtilProperties.getMessage(resource_error, "shippingevents.ship_meth_not_applicable", locale));
            return ServiceUtil.returnError(errorMessageList);
        }

        // the initial amount before manual estimates
        // SCIPIO: 2018-11-09: Start with null, only return a number if got something usable
        //BigDecimal shippingTotal = BigDecimal.ZERO;
        BigDecimal shippingTotal = null;

        // prepare the service invocation fields
        Map<String, Object> serviceFields = new HashMap<String, Object>();
        serviceFields.put("initialEstimateAmt", shippingTotal);
        serviceFields.put("shippableTotal", shippableTotal);
        serviceFields.put("shippableQuantity", shippableQuantity);
        serviceFields.put("shippableWeight", shippableWeight);
        serviceFields.put("shippableItemInfo", itemInfo);
        serviceFields.put("productStoreId", productStoreId);
        serviceFields.put("carrierRoleTypeId", "CARRIER");
        serviceFields.put("carrierPartyId", carrierPartyId);
        serviceFields.put("shipmentMethodTypeId", shipmentMethodTypeId);
        serviceFields.put("shippingContactMechId", shippingContactMechId);
        serviceFields.put("shippingOriginContactMechId", shippingOriginContactMechId);
        serviceFields.put("partyId", partyId);
        serviceFields.put("productStoreShipMethId", productStoreShipMethId);
        serviceFields.put("locale", locale); // SCIPIO

        // call the external shipping service
        try {
            BigDecimal externalAmt = null;
            if (UtilValidate.isNotEmpty(shippingContactMechId)) {
                externalAmt = getExternalShipEstimate(dispatcher, storeShipMethod, serviceFields);
            }
            if (externalAmt != null) {
                shippingTotal = externalAmt;
            }
        } catch (ServiceErrorException e) { // SCIPIO: Added 2018-11-12
            // SCIPIO: TODO: REVIEW: For now, return failure as error...
            // NOTE: The most important part in this case is that we return initialEstimateAmt null for failures,
            // and DON'T run the generic ship estimate;
            // secondarily, it _may_ be important in some cases that the caller aborts in this case...
            //if (ServiceUtil.isFailure(e.getServiceResult())) {
            //    return ServiceUtil.returnFailure(standardMessage);
            //}
            if (allowMissingShipEstimates) {
                if (ServiceUtil.isFailure(e.getServiceResult())) {
                    // TODO: REVIEW: here we can return either success or failure, for now return success for backward-compat...
                    //return ServiceUtil.returnFailure(UtilProperties.getMessage(resource_error, "shippingevents.problem_calculating_shipping_offline", locale));
                    Debug.logWarning(UtilProperties.getMessage(resource_error, "shippingevents.problem_calculating_shipping_offline", Locale.ENGLISH), module);
                    return ServiceUtil.returnSuccess(UtilProperties.getMessage(resource_error, "shippingevents.problem_calculating_shipping_offline", locale));
                }
            }
            return ServiceUtil.returnError(UtilProperties.getMessage(resource_error, "shippingevents.problem_calculating_shipping_try_again", locale));
        } catch (GeneralException e) {
            return ServiceUtil.returnError(UtilProperties.getMessage(resource_error, "shippingevents.problem_calculating_shipping_try_again", locale));
        }

        // update the initial amount
        serviceFields.put("initialEstimateAmt", shippingTotal);

        // call the generic estimate service
        try {
            BigDecimal genericAmt = getGenericShipEstimate(dispatcher, storeShipMethod, serviceFields);
            if (genericAmt != null) {
                if (shippingTotal == null) { // SCIPIO: 2018-11-09
                    shippingTotal = genericAmt;
                } else {
                    shippingTotal = shippingTotal.add(genericAmt);
                }
            }
        } catch (ServiceErrorException e) { // SCIPIO: Added 2018-11-12
            // SCIPIO: TODO: REVIEW: For now, return failure as error...
            // NOTE: The most important part in this case is that we return initialEstimateAmt null for failures, instead of zero.
            //if (ServiceUtil.isFailure(e.getServiceResult())) {
            //    return ServiceUtil.returnFailure(standardMessage);
            //}
            if (allowMissingShipEstimates) {
                if (ServiceUtil.isFailure(e.getServiceResult())) {
                    // TODO: REVIEW: here we can return either success or failure, for now return success for backward-compat...
                    //return ServiceUtil.returnFailure(UtilProperties.getMessage(resource_error, "shippingevents.problem_calculating_shipping_offline", locale));
                    Debug.logWarning(UtilProperties.getMessage(resource_error, "shippingevents.problem_calculating_shipping_offline", Locale.ENGLISH), module);
                    return ServiceUtil.returnSuccess(UtilProperties.getMessage(resource_error, "shippingevents.problem_calculating_shipping_offline", locale));
                }
            }
            return ServiceUtil.returnError(UtilProperties.getMessage(resource_error, "shippingevents.problem_calculating_shipping_try_again", locale));
        } catch (GeneralException e) {
            return ServiceUtil.returnError(UtilProperties.getMessage(resource_error, "shippingevents.problem_calculating_shipping_try_again", locale));
        }

        // return the totals
        Map<String, Object> responseResult = ServiceUtil.returnSuccess();
        responseResult.put("shippingTotal", shippingTotal);
        return responseResult;
    }

    public static BigDecimal getGenericShipEstimate(LocalDispatcher dispatcher, GenericValue storeShipMeth, Map <String, ? extends Object>context) throws GeneralException {
        // invoke the generic estimate service next -- append to estimate amount
        Map<String, Object> genericEstimate = null;
        BigDecimal genericShipAmt = null;
        try {
            genericEstimate = dispatcher.runSync("calcShipmentCostEstimate", context);
        } catch (GenericServiceException e) {
            Debug.logError(e, "Shipment Service Error during calcShipmentCostEstimate (exception): " + e.getMessage(), module); // SCIPIO: show message!
            throw new GeneralException();
        }
        
        // SCIPIO: 2018-11-09: Fixed:
        // * First isFailure check caused exception throw
        // * Second isFailure check was unreachable and should not be returning -1 (return null instead)
        //if (ServiceUtil.isError(genericEstimate) || ServiceUtil.isFailure(genericEstimate)) {
        if (ServiceUtil.isError(genericEstimate)) {
            Debug.logError("Error getting generic shipment cost estimate: " + ServiceUtil.getErrorMessage(genericEstimate), module);
            throw new ServiceErrorException(ServiceUtil.getErrorMessage(genericEstimate), genericEstimate); // SCIPIO: ServiceErrorException
        } else if (ServiceUtil.isFailure(genericEstimate)) {
            // SCIPIO: 2018-11-09: Don't return -1, this is not interpreted properly by callers!
            //genericShipAmt = BigDecimal.ONE.negate();
            // SCIPIO: 2018-11-12: We have to indicate to caller that a failure happened... so always DO throw an exception here
            // DEV NOTE: DO NOT REMOVE THIS EXCEPTION - if problems, change the caller(s)!
            Debug.logError("Failure getting generic shipment cost estimate: " + ServiceUtil.getErrorMessage(genericEstimate), module);
            throw new ServiceErrorException(ServiceUtil.getErrorMessage(genericEstimate), genericEstimate);
        } else {
            genericShipAmt = (BigDecimal) genericEstimate.get("shippingEstimateAmount");
        }
        return genericShipAmt;
    }

    public static String getShipmentCustomMethod(Delegator delegator, String shipmentCustomMethodId) {
        String serviceName = null;
        GenericValue customMethod = null;
        try {
            customMethod = EntityQuery.use(delegator).from("CustomMethod").where("customMethodId", shipmentCustomMethodId).queryOne();
            if (customMethod != null) {
                serviceName = customMethod.getString("customMethodName");
            }
        } catch (GenericEntityException e) {
            Debug.logError(e, module);
        }
        return serviceName;
    }

    public static BigDecimal getExternalShipEstimate(LocalDispatcher dispatcher, GenericValue storeShipMeth, Map<String, Object> context) throws GeneralException {
        String shipmentCustomMethodId = storeShipMeth.getString("shipmentCustomMethodId");
        Delegator delegator = dispatcher.getDelegator();
        String serviceName = "";
        if (UtilValidate.isNotEmpty(shipmentCustomMethodId)) {
            serviceName = getShipmentCustomMethod(dispatcher.getDelegator(), shipmentCustomMethodId);
        }
        if (UtilValidate.isEmpty(serviceName)) {
            serviceName = storeShipMeth.getString("serviceName");
        }
        // invoke the external shipping estimate service
        BigDecimal externalShipAmt = null;
        if (serviceName != null) {
            String doEstimates = EntityUtilProperties.getPropertyValue("shipment", "shipment.doratecheck", "true", delegator);
            //If all estimates are not turned off, check for the individual one
            if ("true".equals(doEstimates)) {
                String dothisEstimate = EntityUtilProperties.getPropertyValue("shipment", "shipment.doratecheck." + serviceName, "true", delegator);
                if ("false".equals(dothisEstimate))
                 serviceName = null;
            } else {
                //Rate checks inhibited
                serviceName = null;
            }
        }
        if (serviceName != null) {
            String shipmentGatewayConfigId = storeShipMeth.getString("shipmentGatewayConfigId");
            String configProps = storeShipMeth.getString("configProps");
            if (UtilValidate.isNotEmpty(serviceName)) {
                // prepare the external service context
                context.put("serviceConfigProps", configProps);
                context.put("shipmentCustomMethodId", shipmentCustomMethodId);
                context.put("shipmentGatewayConfigId", shipmentGatewayConfigId);

                // invoke the service
                Map<String, Object> serviceResp = null;
                try {
                    Debug.logInfo("Service: " + serviceName + " / shipmentGatewayConfigId : " + shipmentGatewayConfigId + " / configProps : " + configProps + " -- " + context, module);
                    // because we don't want to blow up too big or rollback the transaction when this happens, always have it run in its own transaction...
                    serviceResp = dispatcher.runSync(serviceName, context, 0, true);
                } catch (GenericServiceException e) {
                    Debug.logError(e, "Shipment Service Error during " + serviceName + " (exception): " + e.getMessage(), module); // SCIPIO: show message!
                    throw new GeneralException(e);
                }
                if (ServiceUtil.isError(serviceResp)) {
                    String errMsg = "Error getting external shipment cost estimate: " + ServiceUtil.getErrorMessage(serviceResp);
                    Debug.logError(errMsg, module);
                    throw new ServiceErrorException(errMsg, serviceResp); // SCIPIO: ServiceErrorException
                } else if (ServiceUtil.isFailure(serviceResp)) {
                    String errMsg = "Failure getting external shipment cost estimate: " + ServiceUtil.getErrorMessage(serviceResp);
                    Debug.logError(errMsg, module);
                    // SCIPIO: 2018-11-12: We have to indicate to caller that a failure happened... so always DO throw an exception here
                    // DEV NOTE: DO NOT REMOVE THIS EXCEPTION - if problems, change the caller(s)!
                    //// should not throw an Exception here, otherwise getShipGroupEstimate would return an error, causing all sorts of services like add or update order item to abort
                    throw new ServiceErrorException(errMsg, serviceResp);
                } else {
                    externalShipAmt = (BigDecimal) serviceResp.get("shippingEstimateAmount");
                }
            }
        }
        return externalShipAmt;
    }

    /**
     * Attempts to get the supplier's shipping origin address and failing that, the general location.
     */
    public static GenericValue getShippingOriginContactMech(Delegator delegator, String supplierPartyId) throws GeneralException {
        List<EntityCondition> conditions = UtilMisc.toList(
                EntityCondition.makeCondition("partyId", EntityOperator.EQUALS, supplierPartyId),
                EntityCondition.makeCondition("contactMechTypeId", EntityOperator.EQUALS, "POSTAL_ADDRESS"),
                EntityCondition.makeCondition("contactMechPurposeTypeId", EntityOperator.IN, UtilMisc.toList("SHIP_ORIG_LOCATION", "GENERAL_LOCATION")),
                EntityUtil.getFilterByDateExpr("contactFromDate", "contactThruDate"),
                EntityUtil.getFilterByDateExpr("purposeFromDate", "purposeThruDate")
        );
        EntityConditionList<EntityCondition> ecl = EntityCondition.makeCondition(conditions, EntityOperator.AND);

        List<GenericValue> addresses = delegator.findList("PartyContactWithPurpose", ecl, null, UtilMisc.toList("contactMechPurposeTypeId DESC"), null, false);

        GenericValue generalAddress = null;
        GenericValue originAddress = null;
        for (GenericValue address : addresses) {
            if ("GENERAL_LOCATION".equals(address.get("contactMechPurposeTypeId")))
                generalAddress = address;
            else if ("SHIP_ORIG_LOCATION".equals(address.get("contactMechPurposeTypeId")))
                originAddress = address;
        }
        return originAddress != null ? originAddress : generalAddress;
    }
}



