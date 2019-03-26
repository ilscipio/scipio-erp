/*
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
 */

import org.ofbiz.base.util.*;
import org.ofbiz.entity.*;
import org.ofbiz.entity.util.*;
import org.ofbiz.accounting.payment.*;
import org.ofbiz.party.contact.*;
import org.ofbiz.product.store.*;

cart = org.ofbiz.order.shoppingcart.ShoppingCartEvents.getCartObject(request); // SCIPIO: Must use accessor, not this: session.getAttribute("shoppingCart");
currencyUomId = cart.getCurrency();
userLogin = context.userLogin; // SCIPIO: use context login instead: session.getAttribute("userLogin");
partyId = cart.getPartyId();
party = from("Party").where("partyId", partyId).cache(true).queryOne();
productStoreId = ProductStoreWorker.getProductStoreId(request);

checkOutPaymentId = "";
if (cart) {
    if (cart.getPaymentMethodIds()) {
        checkOutPaymentId = cart.getPaymentMethodIds().get(0);
    } else if (cart.getPaymentMethodTypeIds()) {
        checkOutPaymentId = cart.getPaymentMethodTypeIds().get(0);
    }
}

finAccounts = from("FinAccountAndRole").where("partyId", partyId, "roleTypeId", "OWNER").filterByDate(UtilDateTime.nowTimestamp(), "fromDate", "thruDate", "roleFromDate", "roleThruDate").queryList();
context.finAccounts = finAccounts;

context.shoppingCart = cart;
context.userLogin = userLogin;
context.productStoreId = productStoreId;
context.checkOutPaymentId = checkOutPaymentId;
paymentMethodList = EntityUtil.filterByDate(party?.getRelated("PaymentMethod", null, ["paymentMethodTypeId"], false), true);
context.paymentMethodList = paymentMethodList;

billingAccountList = (party) ? BillingAccountWorker.makePartyBillingAccountList(userLogin, currencyUomId, partyId, delegator, dispatcher) : null; // SCIPIO: missing party check
if (billingAccountList) {
    context.selectedBillingAccountId = cart.getBillingAccountId();
    context.billingAccountList = billingAccountList;
}

checkIdealPayment = false;
productStore = ProductStoreWorker.getProductStore(request);
productStorePaymentSettingList = productStore.getRelated("ProductStorePaymentSetting", null, null, true);
productStorePaymentSettingIter = productStorePaymentSettingList.iterator();
while (productStorePaymentSettingIter.hasNext()) {
    productStorePaymentSetting = productStorePaymentSettingIter.next();
    if (productStorePaymentSetting.get("paymentMethodTypeId") == "EXT_IDEAL") {
        checkIdealPayment = true;
    }
    
}

if (checkIdealPayment) {
    issuerList = org.ofbiz.accounting.thirdparty.ideal.IdealEvents.getIssuerList();
    if (issuerList) {
        context.issuerList = issuerList;
    }
}

// SCIPIO: Make party, person, partyGroup available
context.party = party;
person = from("Person").where("partyId", partyId).cache(true).queryOne();
context.person = person;
partyGroup = from("PartyGroup").where("partyId", partyId).cache(true).queryOne();
context.partyGroup = partyGroup;


// SCIPIO: Need a list of ALL payment meths selected in cart
// WARN: Th
checkOutPaymentIdSet = [] as Set;
if (cart) {
    if (cart.getPaymentMethodIds()) {
        checkOutPaymentIdSet.addAll(cart.getPaymentMethodIds());
    } 
    if (cart.getPaymentMethodTypeIds()) {
        checkOutPaymentIdSet.addAll(cart.getPaymentMethodTypeIdsNoPaymentMethodIds());
    }
}
context.checkOutPaymentIdSet = checkOutPaymentIdSet;
context.checkOutPaymentIdList = checkOutPaymentIdSet as List;


// SCIPIO: SPECIAL CASE:
// It is possible that a new CC && EFT were created during event and committed even though a later error occurred
// So to handle this we need a special check to avoid a resubmissions and to select the newly-created
context.newEftAccountParams = parameters;
context.newCreditCardParams = parameters;
context.newGiftCardParams = parameters;
if (parameters.checkOutPaymentId || "Y" == parameters.addGiftCard) {
    checkOutPayIdParamList = parameters.checkOutPaymentId;
    if (!checkOutPayIdParamList) {
        checkOutPayIdParamList = [];
    }
    else if (!(checkOutPayIdParamList instanceof Collection)) {
        checkOutPayIdParamList = [checkOutPayIdParamList];
    }
    if (checkOutPayIdParamList.contains("_NEW_CREDIT_CARD_")) {
        newPaymentMethodInfo = parameters.newPaymentMethodInfoMap?._NEW_CREDIT_CARD_;
        if (newPaymentMethodInfo && context.paymentMethodList) {
            // make sure it appears in pay meth list
            for(pm in context.paymentMethodList) {
                if (pm.paymentMethodId == newPaymentMethodInfo.paymentMethodId) {
                    // Make sure the newly-created card is selected
                    Collections.replaceAll(checkOutPayIdParamList, "_NEW_CREDIT_CARD_", newPaymentMethodInfo.paymentMethodId);
                    // Prevent filling the new CC info fields
                    context.newCreditCardParams = [:];
                    break;
                }
            }
        }
    }
    if (checkOutPayIdParamList.contains("_NEW_EFT_ACCOUNT_")) {
        newPaymentMethodInfo = parameters.newPaymentMethodInfoMap?._NEW_EFT_ACCOUNT_;
        if (newPaymentMethodInfo && context.paymentMethodList) {
            // make sure it appears in pay meth list
            for(pm in context.paymentMethodList) {
                if (pm.paymentMethodId == newPaymentMethodInfo.paymentMethodId) {
                    // Make sure the newly-created eft is selected
                    Collections.replaceAll(checkOutPayIdParamList, "_NEW_EFT_ACCOUNT_", newPaymentMethodInfo.paymentMethodId);
                    // Prevent filling the new EFT info fields
                    context.newEftAccountParams = [:];
                    break;
                }
            }
        }
    }
    if ("Y" == parameters.addGiftCard) {
        newPaymentMethodInfo = parameters.newPaymentMethodInfoMap?._NEW_GIFT_CARD_;
        if (newPaymentMethodInfo && context.paymentMethodList) {
            // make sure it appears in pay meth list
            for(pm in context.paymentMethodList) {
                if (pm.paymentMethodId == newPaymentMethodInfo.paymentMethodId) {
                    // Make sure the newly-created card is selected
                    if (checkOutPayIdParamList.contains("_NEW_GIFT_CARD_")) {
                        Collections.replaceAll(checkOutPayIdParamList, "_NEW_GIFT_CARD_", newPaymentMethodInfo.paymentMethodId);
                    } else {
                        checkOutPayIdParamList.add(newPaymentMethodInfo.paymentMethodId);
                    }
                    // Prevent filling the new EFT info fields (also prevents addGiftCard from selecting)
                    context.newGiftCardParams = [:];
                    break;
                }
            }
        }
    }
    
    parameters.checkOutPaymentId = checkOutPayIdParamList;
}


// SCIPIO: group pay methods by payment method type
paymentMethodListsByType = new LinkedHashMap();
if (paymentMethodList) {
    for (paymentMethod in paymentMethodList) {
        payMethList = paymentMethodListsByType[paymentMethod.paymentMethodTypeId];
        if (payMethList == null) {
            payMethList = [];
        }
        payMethList.add(paymentMethod);
        paymentMethodListsByType[paymentMethod.paymentMethodTypeId] = payMethList;
    }
}
context.paymentMethodListsByType = paymentMethodListsByType;


