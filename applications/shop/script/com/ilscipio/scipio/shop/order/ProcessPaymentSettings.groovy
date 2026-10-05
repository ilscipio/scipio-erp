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

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.ofbiz.base.util.UtilHttp;
import org.ofbiz.base.util.UtilMisc;
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.GenericValue;
import org.ofbiz.order.shoppingcart.CartUpdate
import org.ofbiz.order.shoppingcart.CheckOutHelper;
import org.ofbiz.order.shoppingcart.ShoppingCart;
import org.ofbiz.order.shoppingcart.ShoppingCartEvents;
import org.ofbiz.service.LocalDispatcher;
import org.ofbiz.service.ModelService;
import org.ofbiz.service.ServiceUtil;

//cart = ShoppingCartEvents.getCartObject(request);
CartUpdate cartUpdate = CartUpdate.updateSection(request);
try { // SCIPIO
cart = cartUpdate.getCartForUpdate();
    
dispatcher = request.getAttribute("dispatcher");
delegator = request.getAttribute("delegator");
checkOutHelper = new CheckOutHelper(dispatcher, delegator, cart);
paramMap = UtilHttp.getParameterMap(request);

paymentMethodTypeId = paramMap.paymentMethodTypeId;
errorMessages = [];
errorMaps = [:];

if (paymentMethodTypeId) {
    paymentMethodId = request.getAttribute("paymentMethodId");
    if ("EXT_OFFLINE".equals(paymentMethodTypeId)) {
        paymentMethodId = "EXT_OFFLINE";
    }
    singleUsePayment = paramMap.singleUsePayment;
    appendPayment = paramMap.appendPayment;
    isSingleUsePayment = "Y".equalsIgnoreCase(singleUsePayment) ?: false;
    doAppendPayment = "Y".equalsIgnoreCase(appendPayment) ?: false;
    callResult = checkOutHelper.finalizeOrderEntryPayment(paymentMethodId, null, isSingleUsePayment, doAppendPayment);
    cpi = cart.getPaymentInfo(paymentMethodId, null, null, null, true);
    cpi.securityCode = paramMap.cardSecurityCode;
    ServiceUtil.addErrors(errorMessages, errorMaps, callResult);
}

if (!errorMessages && !errorMaps) {
    selPaymentMethods = null;
    addGiftCard = paramMap.addGiftCard;
    if ("Y".equalsIgnoreCase(addGiftCard)) {
        selPaymentMethods = [paymentMethodTypeId : null];
        callResult = checkOutHelper.checkGiftCard(paramMap, selPaymentMethods);
        ServiceUtil.addErrors(errorMessages, errorMaps, callResult);
        if (!errorMessages && !errorMaps) {
            gcPaymentMethodId = callResult.paymentMethodId;
            giftCardAmount = callResult.amount;
            gcCallRes = checkOutHelper.finalizeOrderEntryPayment(gcPaymentMethodId, giftCardAmount, true, true);
            ServiceUtil.addErrors(errorMessages, errorMaps, gcCallRes);
        }
    }
}

//See whether we need to return an error or not
callResult = ServiceUtil.returnSuccess();
if (errorMessages || errorMaps) {
    request.setAttribute(ModelService.ERROR_MESSAGE_LIST, errorMessages);
    request.setAttribute(ModelService.RESPONSE_MESSAGE, ModelService.RESPOND_ERROR);
    return "error";
}

cartUpdate.commit(cart); // SCIPIO
} finally {
    cartUpdate.close();
}

return "success";
