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

import javax.servlet.http.HttpSession;
import javax.servlet.http.HttpSessionEvent;
import javax.servlet.http.HttpSessionListener;

import org.ofbiz.base.crypto.HashCrypt;
import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.UtilDateTime;
import org.ofbiz.base.util.UtilIO;
import org.ofbiz.base.util.UtilMisc;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.DelegatorFactory;
import org.ofbiz.entity.GenericEntityException;
import org.ofbiz.entity.GenericValue;
import org.ofbiz.entity.condition.EntityCondition;
import org.ofbiz.entity.condition.EntityOperator;
import org.ofbiz.entity.model.DynamicViewEntity;
import org.ofbiz.entity.model.ModelKeyMap;
import org.ofbiz.entity.transaction.TransactionUtil;
import org.ofbiz.entity.util.EntityQuery;
import org.ofbiz.product.product.ProductSearchSession;
import org.ofbiz.product.store.ProductStoreWorker;
import org.ofbiz.webapp.stats.VisitHandler;

import java.security.SecureRandom;
import java.util.List;

/**
 * HttpSessionListener that saves information about abandoned carts
 */
public class CartEventListener implements HttpSessionListener {

    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());

    public CartEventListener() {}

    @Override
    public void sessionCreated(HttpSessionEvent event) {
        HttpSession session = event.getSession();

        // SCIPIO: Dedicated lock for main "shoppingCart" session attribute
        CartSync.createSetLockObject(session);

        // SCIPIO: Dedicated lock for product search (NOTE: tiny performance enhancement only, not really needed anymore)
        ProductSearchSession.createSetSyncObject(session);
    }

    @Override
    public void sessionDestroyed(HttpSessionEvent event) {
        HttpSession session = event.getSession();
        ShoppingCart cart = (ShoppingCart) session.getAttribute("shoppingCart");
        if (cart == null || (cart != null && cart.items().size() == 0)) {
            Debug.logInfo("No cart to save, doing nothing.", module);
            return;
        }

        String delegatorName = (String) session.getAttribute("delegatorName");
        Delegator delegator = null;
        if (UtilValidate.isNotEmpty(delegatorName)) {
            delegator = DelegatorFactory.getDelegator(delegatorName);
        }
        if (delegator == null) {
            Debug.logError("Could not find delegator with delegatorName in session, not saving abandoned cart info.", module);
            return;
        }

        // SCIPIO: 2.1.0: Added new check to avoid abandoned cart store if saveAbandonedCart flag is null or false
        GenericValue productStore = ProductStoreWorker.getProductStore(cart.getProductStoreId(), delegator);
        if (UtilValidate.isNotEmpty(productStore) && productStore.getBoolean("saveAbandonedCart")) {
            boolean beganTransaction = false;

            Debug.logInfo("Saving abandoned cart", module);

            try {
                beganTransaction = TransactionUtil.begin();

                GenericValue visit = VisitHandler.getVisit(session);
                if (visit == null) {
                    Debug.logInfo("Could not get the current visit, not saving abandoned cart info.", module);
                    return;
                }

                // SCIPIO: 2.1.0: Set abandoned cart status to cancelled if customer already has abandoned carts
                GenericValue userLogin = cart.getUserLogin();
                String partyId = cart.getPartyId();
                if (UtilValidate.isNotEmpty(userLogin)) {
                    // TODO: Convert this into an entity-view
                    DynamicViewEntity dve = new DynamicViewEntity();
                    dve.addMemberEntity("CA", "CartAbandoned");
                    dve.addMemberEntity("CAS", "CartAbandonedStatus");
                    dve.addMemberEntity("V", "Visit");
                    dve.addMemberEntity("P", "Party");
                    dve.addMemberEntity("UL", "UserLogin");

                    // Get the latest for any given party visit
                    dve.addAlias("CA", "visitId", null, null, null, false, "max");
                    dve.addAlias("CAS", "statusId", null, null, null, true, null);
                    dve.addAlias("V", "partyId", null, null, null, true, null);
                    dve.addAlias("V", "userLoginId", null, null, null, true, null);

                    dve.addViewLink("CA", "V", false, UtilMisc.toList(ModelKeyMap.makeKeyMapList("visitId")));
                    dve.addViewLink("CA", "CAS", false, UtilMisc.toList(ModelKeyMap.makeKeyMapList("visitId")));
                    dve.addViewLink("V", "P", true, UtilMisc.toList(ModelKeyMap.makeKeyMapList("partyId")));
                    dve.addViewLink("V", "UL", true, UtilMisc.toList(ModelKeyMap.makeKeyMapList("userLoginId")));

                    EntityCondition condition = EntityCondition.makeCondition(UtilMisc.toList(
                            EntityCondition.makeCondition("partyId", EntityOperator.EQUALS, partyId),
                            EntityCondition.makeCondition("userLoginId", EntityOperator.EQUALS, userLogin.getString("userLoginId"))
                    ), EntityOperator.OR);

                    condition = EntityCondition.makeCondition(condition, EntityCondition.makeCondition("statusId", EntityOperator.IN, UtilMisc.toList("AB_PENDING", "AB_IN_PROGRESS")));
                    List<GenericValue> currentAbandonedCarts = EntityQuery.use(delegator).from(dve).where(condition).queryList();
                    List<GenericValue> abandonedCartsAndStatus = UtilMisc.toList();
                    for (GenericValue currentAbandonedCart : currentAbandonedCarts) {
                        GenericValue currentAbandonedCartStatus = currentAbandonedCart.getRelatedOne("AbandonedCartStatus");
                        currentAbandonedCartStatus.put("statusId", "AB_CANCELLED");
                        currentAbandonedCart.put("thruDate", UtilDateTime.nowTimestamp());
                        abandonedCartsAndStatus.add(currentAbandonedCart);
                        abandonedCartsAndStatus.add(currentAbandonedCartStatus);
                    }
                    delegator.storeAll(abandonedCartsAndStatus);
                }

                // SCIPIO: 2.1.0: New enclosing entity for abandoned cart lines
                GenericValue cartAbandoned = delegator.makeValue("CartAbandoned");
                cartAbandoned.set("visitId", visit.get("visitId"));
                cartAbandoned.set("fromDate", UtilDateTime.nowTimestamp());
                cartAbandoned.set("productStoreId", cart.getProductStoreId());
                cartAbandoned.set("webSiteId", cart.getWebSiteId());
                cartAbandoned.set("locale", cart.getLocale().toString());
                cartAbandoned.set("currencyUomId", cart.getCurrency());
                cartAbandoned.create();

                String visitHash;
                synchronized (this) {
                    visitHash = HashCrypt.digestHash("MD5", visit.getString("visitId").getBytes(UtilIO.getUtf8()));
                }
                GenericValue cartAbandonedStatus = delegator.makeValue("CartAbandonedStatus");
                cartAbandonedStatus.set("visitId", visit.get("visitId"));
                cartAbandonedStatus.set("statusId", "AB_PENDING");
                cartAbandonedStatus.set("visitHash", visitHash);
                cartAbandonedStatus.create();

                int seqId = 1;
                for (ShoppingCartItem cartItem : cart) {
                    GenericValue cartAbandonedLine = delegator.makeValue("CartAbandonedLine");
                    cartAbandonedLine.set("visitId", visit.get("visitId"));
                    cartAbandonedLine.set("cartAbandonedLineSeqId", Integer.toString(seqId));
                    cartAbandonedLine.set("productId", cartItem.getProductId());
                    cartAbandonedLine.set("prodCatalogId", cartItem.getProdCatalogId());
                    cartAbandonedLine.set("quantity", cartItem.getQuantity());
                    cartAbandonedLine.set("reservStart", cartItem.getReservStart());
                    cartAbandonedLine.set("reservLength", cartItem.getReservLength());
                    cartAbandonedLine.set("reservPersons", cartItem.getReservPersons());
                    cartAbandonedLine.set("unitPrice", cartItem.getBasePrice());
                    cartAbandonedLine.set("reserv2ndPPPerc", cartItem.getReserv2ndPPPerc());
                    cartAbandonedLine.set("reservNthPPPerc", cartItem.getReservNthPPPerc());
                    if (cartItem.getConfigWrapper() != null) {
                        cartAbandonedLine.set("configId", cartItem.getConfigWrapper().getConfigId());
                    }
                    cartAbandonedLine.set("totalWithAdjustments", cartItem.getItemSubTotal());
                    //not doing pre-reservations now, so this is always N
                    cartAbandonedLine.set("wasReserved", "N");
                    cartAbandonedLine.create();

                    seqId++;
                }
            } catch (GenericEntityException e) {
                try {
                    // only rollback the transaction if we started one...
                    TransactionUtil.rollback(beganTransaction, "Error saving abandoned cart info", e);
                } catch (GenericEntityException e2) {
                    Debug.logError(e2, "Could not rollback transaction: " + e2.toString(), module);
                }

                Debug.logError(e, "An entity engine error occurred while saving abandoned cart information", module);
            } finally {
                // only commit the transaction if we started one... this will throw an exception if it fails
                try {
                    TransactionUtil.commit(beganTransaction);
                } catch (GenericEntityException e) {
                    Debug.logError(e, "Could not commit transaction for entity engine error occurred while saving abandoned cart information", module);
                }
            }
        }
    }
}
