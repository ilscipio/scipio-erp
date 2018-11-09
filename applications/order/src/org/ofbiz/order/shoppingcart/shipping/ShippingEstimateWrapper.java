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
import java.util.List;
import java.util.Locale;
import java.util.Map;

import org.ofbiz.base.util.Debug;
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.GenericValue;
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

                // SCIPIO: 2018-11-09: Added locale
                Map<String, Object> estimateMap = ShippingEvents.getShipGroupEstimate(dispatcher, delegator, locale, "SALES_ORDER",
                        shippingMethodTypeId, carrierPartyId, carrierRoleTypeId, shippingCmId, productStoreId,
                        supplierPartyId, shippableItemInfo, shippableWeight, shippableQuantity, shippableTotal, partyId, productStoreShipMethId);

                if (ServiceUtil.isSuccess(estimateMap)) {
                    BigDecimal shippingTotal = (BigDecimal) estimateMap.get("shippingTotal");
                    shippingEstimates.put(shipMethod, shippingTotal);
                }
            }
        }
        return shippingEstimates;
    }

    public List<GenericValue> getShippingMethods() {
        return shippingMethods;
    }

    public Map<GenericValue, BigDecimal> getAllEstimates() {
        return shippingEstimates;
    }

    public BigDecimal getShippingEstimate(GenericValue storeCarrierShipMethod) {
        return shippingEstimates.get(storeCarrierShipMethod);
    }

}
