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


final module = "CheckoutCommon.groovy";

// SCIPIO: TODO? Determine these in smarter way, to unify anon and registered checkouts

if (!context.checkoutType) {
    context.checkoutType = "full"
}

if (!context.checkoutMode) {
    context.checkoutMode = "registered"
}

cart = org.ofbiz.order.shoppingcart.ShoppingCartEvents.getCartObject(request); // SCIPIO: Must use accessor, not this: session.getAttribute("shoppingCart");
cartSize = 0;
if (cart != null) {
    cartSize = cart.size();
}

context.cart = cart;
context.cartSize = cartSize;

