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
import org.ofbiz.base.util.*;
import org.ofbiz.order.shoppingcart.*;
import org.ofbiz.order.shoppingcart.shipping.*;
import org.ofbiz.party.contact.*;
import org.ofbiz.product.store.*;

shoppingCart = org.ofbiz.order.shoppingcart.ShoppingCartEvents.getCartObject(request); // SCIPIO: Must use accessor, not this: session.getAttribute("shoppingCart");
context.shoppingCart = shoppingCart;
productStore = ProductStoreWorker.getProductStore(request);

if (shoppingCart) {
    shippingEstWpr = new ShippingEstimateWrapper(dispatcher, shoppingCart, 0);
    context.shippingEstWpr = shippingEstWpr;
    context.carrierShipmentMethodList = shippingEstWpr.getShippingMethods();
}

// nuke the event messages
request.removeAttribute("_EVENT_MESSAGE_");

party = null;
partyId = session.getAttribute("orderPartyId");
if (partyId) {
    party = from("Party").where("partyId", partyId).queryOne();
    context.party = party;
}


context.emailList = ContactHelper.getContactMechByType(party, "EMAIL_ADDRESS", false);

if (shoppingCart?.getShipmentMethodTypeId() && shoppingCart.getCarrierPartyId()) {
    context.chosenShippingMethod = shoppingCart.getShipmentMethodTypeId() + '@' + shoppingCart.getCarrierPartyId();
    context.callSubmitForm = true;
}

context.productStoreId = productStore.productStoreId;
context.productStore = productStore;
