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

import org.ofbiz.base.util.*;
import org.ofbiz.entity.*;
import org.ofbiz.entity.util.*;
import org.ofbiz.party.contact.*;
import org.ofbiz.product.store.*;
import org.ofbiz.order.shoppingcart.shipping.*;

cart = org.ofbiz.order.shoppingcart.ShoppingCartEvents.getCartObject(request); // SCIPIO: Must use accessor, not this: session.getAttribute("shoppingCart");
party = userLogin?.getRelatedOne("Party", false); // SCIPIO: Prevent NPE
productStore = ProductStoreWorker.getProductStore(request);

if (cart) {
    shippingEstWpr = new ShippingEstimateWrapper(dispatcher, cart, 0);
    context.shippingEstWpr = shippingEstWpr;
    context.carrierShipmentMethodList = shippingEstWpr.getShippingMethods();
}

context.shoppingCart = cart;
context.userLogin = userLogin;
context.productStoreId = productStore.productStoreId;
context.productStore = productStore;
context.emailList = ContactHelper.getContactMechByType(party, "EMAIL_ADDRESS", false);

if (cart.getShipmentMethodTypeId() && cart.getCarrierPartyId()) {
    context.chosenShippingMethod = cart.getShipmentMethodTypeId() + '@' + cart.getCarrierPartyId();
}
