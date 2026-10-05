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
import org.ofbiz.entity.*
import org.ofbiz.order.order.OrderReadHelper;
import org.ofbiz.party.contact.*;

orderId = parameters.orderId;
context.orderId = orderId;

party = userLogin.getRelatedOne("Party", false);
context.party = party;

returnTypes = from("ReturnType").orderBy("sequenceId").queryList();
context.returnTypes = returnTypes;

returnReasons = from("ReturnReason").orderBy("sequenceId").queryList();
context.returnReasons = returnReasons;

if (orderId) {
    returnRes = runService('getReturnableItems', [orderId : orderId]);
    context.returnableItems = returnRes.returnableItems;
    orderHeader = from("OrderHeader").where("orderId", orderId).queryOne();
    context.orderHeader = orderHeader;

    // SCIPIO
    orh = new OrderReadHelper(orderHeader);
    context.orh = orh;
}

returnItemTypeMap = from("ReturnItemTypeMap").where("returnHeaderTypeId", "CUSTOMER_RETURN").queryList();
typeMap = new HashMap();
returnItemTypeMap.each { value -> typeMap[value.returnItemMapKey] = value.returnItemTypeId }
context.returnItemTypeMap = typeMap;

//put in the return to party information from the order header
if (orderId) {
    order = from("OrderHeader").where("orderId", orderId).queryOne();
    productStore = order.getRelatedOne("ProductStore", false);
    context.toPartyId = productStore.payToPartyId;
}

context.shippingContactMechList = ContactHelper.getContactMech(party, "SHIPPING_LOCATION", "POSTAL_ADDRESS", false);

// SCIPIO
hasReturnPermission = org.ofbiz.order.order.OrderReturnEvents.ReturnHandler.hasReturnPermission(context.security, context.userLogin, context.orderHeader);
context.hasReturnPermission = hasReturnPermission;
