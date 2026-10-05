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
/**
 * SCIPIO: Specific to orderitems template.
 */

import org.ofbiz.order.order.OrderReadHelper;

// SCIPIO: Each item may have downloadable files; make them available here (by productId)
productDownloads = [:];
orderItems = context.orderItems;
if (orderItems) {
    for (orderItem in orderItems) {
        downloadProductContentAndInfoList = from("ProductContentAndInfo").where("productId", orderItem.productId, "productContentTypeId", "DIGITAL_DOWNLOAD").orderBy("sequenceNum ASC").cache(true).queryList();
        if (downloadProductContentAndInfoList) {
            productDownloads[orderItem.productId] = downloadProductContentAndInfoList;
        }
    }
}
context.productDownloads = productDownloads;

// SCIPIO: OrderItemAttributes and ProductConfigWrappers
orderHeader = context.orderHeader;
if (orderHeader?.orderId) {
    orh = context.localOrderReadHelper;
    if (orh == null) {
        orh = new OrderReadHelper(dispatcher, context.locale, orderHeader);
    }
    context.localOrderReadHelper = orh;
    orderItemProdCfgMap = orh.getProductConfigWrappersByOrderItemSeqId(orderItems);
    context.orderItemProdCfgMap = orderItemProdCfgMap;
} else {
    // Only do this if it's not a persisted order...
    cart = context.cart;
    if (cart != null) {
        orderItemAttrMap = cart.makeAllOrderItemAttributesByOrderItemSeqId();
        context.orderItemAttrMap = orderItemAttrMap;
        orderItemProdCfgMap = cart.getProductConfigWrappersByOrderItemSeqId();
        context.orderItemProdCfgMap = orderItemProdCfgMap;
        orderItemSurvResMap = cart.makeAllOrderItemSurveyResponsesByOrderItemSeqId();
        context.orderItemSurvResMap = orderItemSurvResMap;
    }
}
