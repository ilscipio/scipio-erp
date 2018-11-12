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
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;

import javax.transaction.Transaction;

import org.ofbiz.base.util.Debug;
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.GenericValue;
import org.ofbiz.entity.transaction.GenericTransactionException;
import org.ofbiz.entity.transaction.TransactionUtil;
import org.ofbiz.order.shoppingcart.ShoppingCart;
import org.ofbiz.product.store.ProductStoreWorker;
import org.ofbiz.service.LocalDispatcher;
import org.ofbiz.service.ServiceUtil;

public class ShippingEstimateWrapper {

    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());

    // SCIPIO: 2018-11-09: All fields now final.
    protected final Delegator delegator;
    protected final LocalDispatcher dispatcher;

    protected final Map<GenericValue, BigDecimal> shippingEstimates;
    protected final List<GenericValue> shippingMethods;

    protected final GenericValue shippingAddress;
    protected final Map<String, BigDecimal> shippableItemFeatures;
    protected final List<BigDecimal> shippableItemSizes;
    protected final List<Map<String, Object>> shippableItemInfo;
    protected final String productStoreId;
    protected final BigDecimal shippableQuantity;
    protected final BigDecimal shippableWeight;
    protected final BigDecimal shippableTotal;
    protected final String partyId;
    protected final String supplierPartyId;

    protected final Locale locale; // SCIPIO: 2018-11-09: Added locale
    protected final List<GenericValue> validShippingMethods; // SCIPIO: 2018-11-09

    protected final boolean allowMissingEstimates = false; // SCIPIO: TODO: property for this...

    public static ShippingEstimateWrapper getWrapper(LocalDispatcher dispatcher, ShoppingCart cart, int shipGroup) {
        return new ShippingEstimateWrapper(dispatcher, cart, shipGroup);
    }

    public ShippingEstimateWrapper(LocalDispatcher dispatcher, ShoppingCart cart, int shipGroup) {
        this.dispatcher = dispatcher;
        this.delegator = cart.getDelegator();

        this.shippableItemFeatures = cart.getFeatureIdQtyMap(shipGroup);
        this.shippableItemSizes = cart.getShippableSizes(shipGroup);
        this.shippableItemInfo = cart.getShippableItemInfo(shipGroup);
        this.shippableQuantity = cart.getShippableQuantity(shipGroup);
        this.shippableWeight = cart.getShippableWeight(shipGroup);
        this.shippableTotal = cart.getShippableTotal(shipGroup);
        this.shippingAddress = cart.getShippingAddress(shipGroup);
        this.productStoreId = cart.getProductStoreId();
        this.partyId = cart.getPartyId();
        this.supplierPartyId = cart.getSupplierPartyId(shipGroup);
        
        this.locale = cart.getLocale();

        List<GenericValue> shippingMethods = this.loadShippingMethods(); // SCIPIO: Locals
        Map<GenericValue, BigDecimal> shippingEstimates = this.loadEstimates(shippingMethods);
        this.shippingMethods = shippingMethods;
        this.shippingEstimates = shippingEstimates;
        
        List<GenericValue> validShippingMethods = new ArrayList<>(shippingMethods.size());
        for(GenericValue shipMethod : shippingMethods) {
            if (isValidShippingMethod(shipMethod)) {
                validShippingMethods.add(shipMethod);
            }
        }
        this.validShippingMethods = validShippingMethods;
    }

    protected List<GenericValue> loadShippingMethods() { // SCIPIO: Added return value
        try {
            return ProductStoreWorker.getAvailableStoreShippingMethods(delegator, productStoreId,
                    shippingAddress, shippableItemSizes, shippableItemFeatures, shippableWeight, shippableTotal);
        } catch (Throwable t) {
            Debug.logError(t, module);
        }
        return null;
    }

    protected Map<GenericValue, BigDecimal> loadEstimates(List<GenericValue> shippingMethods) { // SCIPIO: Added return value
        Map<GenericValue, BigDecimal> shippingEstimates = new HashMap<>();
        if (shippingMethods != null) {
            for (GenericValue shipMethod : shippingMethods) {
                String shippingMethodTypeId = shipMethod.getString("shipmentMethodTypeId");
                String carrierRoleTypeId = shipMethod.getString("roleTypeId");
                String carrierPartyId = shipMethod.getString("partyId");
                String productStoreShipMethId = shipMethod.getString("productStoreShipMethId");
                String shippingCmId = shippingAddress != null ? shippingAddress.getString("contactMechId") : null;

                // SCIPIO: 2018-11-09: Wrap each shipping method in its own transaction so that service errors
                // don't affect the caller or other ship methods.
                Transaction parentTx = null;
                boolean beganTransaction = false;
                try {
                    try {
                        parentTx = TransactionUtil.suspend();
                    } catch (GenericTransactionException e) {
                        Debug.logError(e, "Could not suspend transaction for loadEstimates: " + e.getMessage(), module);
                    }
                    try {
                        try {
                            beganTransaction = TransactionUtil.begin(7200);
                        } catch (GenericTransactionException e) {
                            Debug.logError(e, "Could not begin transaction for loadEstimates", module);
                        }

                        // SCIPIO: 2018-11-09: Added locale
                        Map<String, Object> estimateMap = ShippingEvents.getShipGroupEstimate(dispatcher, delegator, locale, "SALES_ORDER",
                                shippingMethodTypeId, carrierPartyId, carrierRoleTypeId, shippingCmId, productStoreId,
                                supplierPartyId, shippableItemInfo, shippableWeight, shippableQuantity, shippableTotal, partyId, productStoreShipMethId);

                        if (ServiceUtil.isSuccess(estimateMap)) {
                            BigDecimal shippingTotal = (BigDecimal) estimateMap.get("shippingTotal");
                            shippingEstimates.put(shipMethod, shippingTotal);
                        }

                    } finally {
                        try {
                            TransactionUtil.commit(beganTransaction);
                        } catch (GenericTransactionException e) {
                            Debug.logError(e, "Could not commit nested transaction for loadEstimates: " + e.getMessage(), module);
                        }
                    }
                } finally {
                    // resume/restore parent transaction
                    if (parentTx != null) {
                        try {
                            TransactionUtil.resume(parentTx);
                        } catch (GenericTransactionException e) {
                            Debug.logError(e, "Could not resume parent nested transaction for loadEstimates: " + e.getMessage(), module);
                        }
                    }
                }
            }
        }
        return shippingEstimates;
    }

    /**
     * SCIPIO: Returns only valid shipping methods for selection for the current process.
     * ALIAS for {@link #getValidShippingMethods()}.
     * <p>
     * <strong>NOTE:</strong> As of 2018-11-09, this method only returns shipping methods
     * deemed "valid" for the current process. Prior to this, this used to return methods
     * even if they missed estimates and this is not allowed; to get that behavior again,
     * use {@link #getAllShippingMethods()}.
     * <p>
     * Largely depends on {@link #isAllowMissingEstimates()}.
     * <p>
     * Modified 2018-11-09.
     */
    public List<GenericValue> getShippingMethods() {
        //return shippingMethods;
        return validShippingMethods;
    }

    /**
     * SCIPIO: Returns only valid shipping methods for selection for the current process.
     * ALIAS for {@link #getShippingMethods()}.
     * <p>
     * Added 2018-11-09.
     */
    public List<GenericValue> getValidShippingMethods() {
        return validShippingMethods;
    }

    /**
     * SCIPIO: Returns all shipping methods for the store even if invalid for selection (e.g. bad estimates).
     * <p>
     * This implements the old behavior of {@link #getShippingMethods()} prior to 2018-11-09.
     * <p>
     * Added 2018-11-09.
     */
    public List<GenericValue> getAllShippingMethods() {
        return shippingMethods;
    }

    public Map<GenericValue, BigDecimal> getAllEstimates() {
        return shippingEstimates;
    }

    public BigDecimal getShippingEstimate(GenericValue storeCarrierShipMethod) {
        return shippingEstimates.get(storeCarrierShipMethod);
    }

    /**
     * SCIPIO: If true, {@link #getShippingMethods()} will return methods
     * even if they returned no valid estimates.
     */
    public boolean isAllowMissingEstimates() {
        return allowMissingEstimates;
    }

    /**
     * SCIPIO: Returns true if the shipping method is valid for selection.
     * Added 2018-11-09.
     */
    public boolean isValidShippingMethod(GenericValue storeCarrierShipMethod) {
        return isAllowMissingEstimates() || isValidEstimate(getShippingEstimate(storeCarrierShipMethod), storeCarrierShipMethod);
    }

    /**
     * SCIPIO: Checks if the given ship method has a valid estimate.
     * NOTE: Does NOT necessarily imply the whole shipping method is valid for usage;
     * use {@link #isValidShippingMethod} for that.
     * Added 2018-11-09.
     */
    public boolean isValidEstimate(GenericValue storeCarrierShipMethod) {
        return isValidEstimate(getShippingEstimate(storeCarrierShipMethod), storeCarrierShipMethod);
    }

    /**
     * SCIPIO: isValidShippingEstimate.
     * Added 2018-11-09.
     */
    public boolean isValidEstimate(BigDecimal estimate, GenericValue storeCarrierShipMethod) {
        if (!(estimate == null || estimate.compareTo(BigDecimal.ZERO) < 0)) { // Same logic as PayPalServices.payPalCheckoutUpdate
            return true;
        }
        return ("NO_SHIPPING".equals(storeCarrierShipMethod.get("shipmentMethodTypeId"))); // Special case
    }
}
