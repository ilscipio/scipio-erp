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

cart = ShoppingCartEvents.getCartObject(request);
context.cart = cart;

partyId = cart.getPartyId();
currencyUomId = cart.getCurrency();

// SCIPIO: Some patches to prevent missing userLogin crash

if (!partyId) {
    partyId = userLogin?.partyId;
}
context.partyId = partyId;

if (partyId && !partyId.equals("_NA_")) {
    party = from("Party").where("partyId", partyId).queryOne();
    person = party.getRelatedOne("Person", false);
    context.party = party;
    context.person = person;
}

// nuke the event messages
request.removeAttribute("_EVENT_MESSAGE_");

if (parameters.useShipAddr && cart.getShippingContactMechId()) {
    shippingContactMech = cart.getShippingContactMechId();
    postalAddress = from("PostalAddress").where("contactMechId", shippingContactMech).queryOne();
    context.useEntityFields = "Y";
    context.postalAddress = postalAddress;

    if (postalAddress && partyId) {
        partyContactMech = from("PartyContactMech").where("partyId", partyId, "contactMechId", postalAddress.contactMechId).orderBy("-fromDate").filterByDate().queryFirst();
        context.partyContactMech = partyContactMech;
    }
} else {
    context.postalAddress = UtilHttp.getParameterMap(request);
}

if (cart) {
    if (cart.getPaymentMethodIds()) {
        paymentMethods = cart.getPaymentMethods();
        paymentMethods.each { paymentMethod ->
            account = null;
            if ("CREDIT_CARD".equals(paymentMethod?.paymentMethodTypeId)) {
                account = paymentMethod.getRelatedOne("CreditCard", false);
                context.creditCard = account;
                context.paymentMethodTypeId = "CREDIT_CARD";
            } else if ("EFT_ACCOUNT".equals(paymentMethod?.paymentMethodTypeId)) {
                account = paymentMethod.getRelatedOne("EftAccount", false);
                context.eftAccount = account;
                context.paymentMethodTypeId = "EFT_ACCOUNT";
            } else if ("GIFT_CARD".equals(paymentMethod?.paymentMethodTypeId)) {
                account = paymentMethod.getRelatedOne("GiftCard", false);
                context.giftCard = account;
                context.paymentMethodTypeId = "GIFT_CARD";
                context.addGiftCard = "Y";
            } else {
                context.paymentMethodTypeId = "EXT_OFFLINE";
            }
            if (account && !parameters.useShipAddr) {
                address = account.getRelatedOne("PostalAddress", false);
                context.postalAddress = address;
            }
        }
    }
}

if (!parameters.useShipAddr) {
    if (cart && context.postalAddress) {
        postalAddress = context.postalAddress;
        shippingContactMechId = cart.getShippingContactMechId();
        contactMechId = postalAddress.contactMechId;
        if (shippingContactMechId?.equals(contactMechId)) {
            context.useShipAddr = "Y";
        }
    }
} else {
    context.useShipAddr = parameters.useShipAddr;
}

// Added here to satisfy genericaddress.ftl
if (context.postalAddress) {
    postalAddress = context.postalAddress;
    parameters.address1 = postalAddress.address1;
    parameters.address2 = postalAddress.address2;
    parameters.city = postalAddress.city;
    parameters.stateProvinceGeoId = postalAddress.stateProvinceGeoId;
    parameters.postalCode = postalAddress.postalCode;
    parameters.countryGeoId = postalAddress.countryGeoId;
    parameters.contactMechId = postalAddress.contactMechId;
    if (context.creditCard) {
       context.callSubmitForm = true;
    }
}
