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

import org.ofbiz.entity.*;
import org.ofbiz.entity.util.*;
import org.ofbiz.base.util.*;
import org.ofbiz.accounting.payment.*;
import org.ofbiz.order.shoppingcart.*;
import org.ofbiz.party.contact.*;

// SCIPIO: Some fixes to prevent crash on missing userLogin

cart = org.ofbiz.order.shoppingcart.ShoppingCartEvents.getCartObject(request); // SCIPIO: Must use accessor, not this: session.getAttribute("shoppingCart");
currencyUomId = cart.getCurrency();
payType = parameters.paymentMethodType;
if (!payType && parameters.useGc) {
    payType = "GC";
}
context.cart = cart;
context.paymentMethodType = payType;

partyId = cart.getPartyId() ?: userLogin?.partyId;
context.partyId = partyId;

// nuke the event messages
request.removeAttribute("_EVENT_MESSAGE_");

if (partyId && !partyId.equals("_NA_")) {
    party = from("Party").where("partyId", partyId).queryOne();
    person = party.getRelatedOne("Person", false);
    context.party = party;
    context.person = person;
    if (party) {
        context.paymentMethodList = EntityUtil.filterByDate(party.getRelated("PaymentMethod", null, null, false));

        billingAccountList = BillingAccountWorker.makePartyBillingAccountList(userLogin, currencyUomId, partyId, delegator, dispatcher);
        if (billingAccountList) {
            context.selectedBillingAccountId = cart.getBillingAccountId();
            context.billingAccountList = billingAccountList;
        }
    }
}

if (parameters.useShipAddr && cart.getShippingContactMechId()) {
    shippingContactMech = cart.getShippingContactMechId();
    postalAddress = from("PostalAddress").where("contactMechId", shippingContactMech).queryOne();
    context.useEntityFields = "Y";
    context.postalFields = postalAddress;

    if (postalAddress && partyId) {
        partyContactMech = from("PartyContactMech").where("partyId", partyId, "contactMechId", postalAddress.contactMechId).orderBy("-fromDate").filterByDate().queryFirst();
        context.partyContactMech = partyContactMech;
    }
} else {
    context.postalFields = UtilHttp.getParameterMap(request);
}

if (cart && !parameters.singleUsePayment) {
    if (cart.getPaymentMethodIds() ) {
        checkOutPaymentId = cart.getPaymentMethodIds()[0];
        context.checkOutPaymentId = checkOutPaymentId;
        paymentMethod = from("PaymentMethod").where("paymentMethodId", checkOutPaymentId).queryOne();
        account = null;

        if ("CREDIT_CARD".equals(paymentMethod.paymentMethodTypeId)) {
            account = paymentMethod.getRelatedOne("CreditCard", false);
            context.creditCard = account;
            context.paymentMethodType = "CC";
        } else if ("EFT_ACCOUNT".equals(paymentMethod.paymentMethodTypeId)) {
            account = paymentMethod.getRelatedOne("EftAccount", false);
            context.eftAccount = account;
            context.paymentMethodType = "EFT";
        } else if ("GIFT_CARD".equals(paymentMethod.paymentMethodTypeId)) {
            account = paymentMethod.getRelatedOne("GiftCard", false);
            context.giftCard = account;
            context.paymentMethodType = "GC";
        } else {
            context.paymentMethodType = "offline";
        }
        if (account && parameters.useShipAddr) {
            address = account.getRelatedOne("PostalAddress", false);
            context.postalAddress = address;
            context.postalFields = address;
        }
    } else if (cart.getPaymentMethodTypeIds()) {
        checkOutPaymentId = cart.getPaymentMethodTypeIds()[0];
        context.checkOutPaymentId = checkOutPaymentId;
    }
}

requestPaymentMethodType = parameters.paymentMethodType;
if (requestPaymentMethodType) {
    context.paymentMethodType = requestPaymentMethodType;
}
