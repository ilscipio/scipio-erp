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

paymentMethodTypeId = parameters.paymentMethodTypeId ?: "CREDIT_CARD";

// nuke the event messages
request.removeAttribute("_EVENT_MESSAGE_");

if (cart?.getPaymentMethodIds()) {
    paymentMethods = cart.getPaymentMethods();
    paymentMethods.each {paymentMethod ->
        if ("CREDIT_CARD".equals(paymentMethod?.paymentMethodTypeId)) {
            paymentMethodId = paymentMethod.paymentMethodId;
            parameters.paymentMethodId = paymentMethodId;
        }
    }
}

