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
import org.ofbiz.base.util.cache.UtilCache
import org.ofbiz.entity.condition.EntityCondition
import org.ofbiz.entity.condition.EntityOperator
import org.ofbiz.entity.model.DynamicViewEntity
import org.ofbiz.entity.model.ModelKeyMap
import org.ofbiz.order.order.OrderReadHelper

import java.sql.Timestamp;

orderReadHelper = context.orderReadHelper;
if(!context.orderHeader) {
    if (context.orderId) {
        context.orderHeader = from('OrderHeader').where('orderId', context.orderId).cache(true).queryFirst();
    }
    if (request.getAttribute("orderId")) {
        context.orderHeader = from('OrderHeader').where('orderId', request.getAttribute("orderId")).cache(true).queryFirst();
    }
}
if(context.orderHeader){
    if(!orderReadHelper){
        orderReadHelper = new OrderReadHelper(dispatcher, context.locale, context.orderHeader);
    }
    if(!context.displayParty){
        if ("PURCHASE_ORDER".equals(context.orderHeader.orderTypeId)) {
            displayParty = orderReadHelper.getSupplierAgent();
        } else {
            displayParty = orderReadHelper.getPlacingParty();
        }
    }

    customerId = displayParty.partyId;
    emailAddress = orderReadHelper.getOrderEmailString();


    //SQL magic
    List<String> orderIds = orderReadHelper.getAllCustomerOrderIdsFromOrderEmail(UtilMisc.toList("ORDER_CANCELLED","ORDER_REJECTED"));
    Map<String,Object> orderStats = orderReadHelper.getCustomerOrderMktgStats(orderIds,true,["ITEM_COMPLETED","ITEM_APPROVED"],["RETURN_CANCELLED"]);

    context.orderCount = orderStats.orderCount;
    context.orderItemValue = orderStats.orderItemValue;
    context.orderItemCount = orderStats.orderItemCount;
    context.returnCount = orderStats.returnCount;
    context.returnItemValue = orderStats.returnItemValue;
    context.returnItemCount = orderStats.returnItemCount;
    context.returnItemRatio = orderStats.returnItemRatio;
    context.rfmRecency=orderStats.rfmRecency;
    context.rfmFrequency=orderStats.rfmFrequency;
    context.rfmMonetary = orderStats.rfmMonetary;
    context.rfmRecencyScore=orderStats.rfmRecencyScore;
    context.rfmFrequencyScore=orderStats.rfmFrequencyScore;
    context.rfmMonetaryScore=orderStats.rfmMonetaryScore;
}