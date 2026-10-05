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

import org.ofbiz.order.shoppingcart.*;

cart = org.ofbiz.order.shoppingcart.ShoppingCartEvents.getCartObject(request); // SCIPIO: Must use accessor, not this: session.getAttribute("shoppingCart");

trailClassMap = [:];

if (cart.getShippingContactMechId()) {
    context.shipAddr = "Y";
    trailClassMap.shipAddr = "submenutextright";
}

if (cart.getShipmentMethodTypeId()) {
    context.shipOptions = "Y";
    trailClassMap.shipAddr = "submenutext";
    trailClassMap.shipOptions = "submenutextright";
}

if (parameters.paymentMethodType || cart.getPaymentMethodIds() || cart.getPaymentMethodTypeIds()) {
    context.billing = "Y";
    trailClassMap.shipOptions = "submenutext";
    trailClassMap.paymentType = "submenutextright";
}
context.trailClass = trailClassMap;
