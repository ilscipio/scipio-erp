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
import org.ofbiz.base.util.Debug
import org.ofbiz.entity.util.EntityQuery
import org.ofbiz.service.ServiceUtil


abandonedCart = context.abandonedCart
abandonedCartStatus = context.abandonedCartStatus
abandonedCartLines = context.abandonedCartLines
userLogin = context.userLogin

applyStorePromotions = (context.applyStorePromotions) ? context.applyStorePromotions : parameters.applyStorePromotions
cartPartyId = (context.cartPartyId) ? context.cartPartyId : parameters.cartPartyId

visitId = context.visitId
if (visitId && !abandonedCart) {
    abandonedCart = EntityQuery.use(delegator).from("CartAbandoned").where(["visitId" : visitId]).cache().queryOne();
    abandonedCartLines = abandonedCart.getRelated("CartAbandonedLine", true);
}

if (abandonedCart) {
    if (!visitId) {
        visitId = abandonedCart.visitId
    }
    loadCartFromAbandonedCartCtx = ["visitId" : visitId, "abandonedCart": abandonedCart, "abandonedCartStatus": abandonedCartStatus, "abandonedCartLines": abandonedCartLines,
                                    "applyStorePromotions": applyStorePromotions, "cartPartyId": cartPartyId, "userLogin": userLogin]

    loadCartFromAbandonedCartResult = dispatcher.runSync("loadCartFromAbandonedCart", loadCartFromAbandonedCartCtx);
    if (ServiceUtil.isSuccess(loadCartFromAbandonedCartResult)) {
        shoppingCart = loadCartFromAbandonedCartResult.get("shoppingCart")
        context.shoppingCart = shoppingCart
        context.shoppingCartSize = shoppingCart?.size() ?: 0;
    }
} else {
    Debug.logWarning("Can't prepare cart recovery for visit: " + visitId)
}