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
 * SCIPIO: Common prep for any email or non-webapp render (missing request is assumed) that needs access to general
 * productStore and webSite fields, which it tries to determine any way possible.
 * <p>
 * If an orderHeader/orderId is present (optional), it will exploit it to get OrderHeader.productStoreId to determine
 * the productStore field.
 */

import org.ofbiz.base.util.*;
import org.ofbiz.webapp.website.WebSiteWorker;

final module = "CommonEmailStoreInfo.groovy";

def orderHeaderRead = false;
def orderHeader = null;
getOrderHeader = {
    if (orderHeaderRead) {
        return orderHeader;
    }
    orderHeaderRead = true;
    orderHeader = context.orderHeader;
    def orderId = orderHeader?.orderId ?: context.orderId ?: parameters.orderId;
    if (!orderHeader && orderId) {
        orderHeader = from("OrderHeader").where("orderId", orderId).queryOne();
        if (!orderHeader) {
            Debug.logError("Could not find OrderHeader for orderId: " + orderId, module);
        }
    }
    return orderHeader;
}
// should probably be left to OrderStatus.groovy...
//orderHeader = getOrderHeader();
//context.orderHeader = orderHeader;
//context.orderId = webSite?.orderId;

getProductStore = {
    def productStore = context.productStore; // context.productStore; // make sure he have the one from the header
    def productStoreId = productStore?.productStoreId ?: context.productStoreId ?:
        parameters.productStoreId ?: getOrderHeader().productStoreId;
    if (!productStore && productStoreId) {
        productStore = from("ProductStore").where("productStoreId", productStoreId).cache().queryOne();
        if (!productStore) {
            Debug.logError("Could not find ProductStore for productStoreId: " + productStoreId, module);
        }
    }
    return productStore;
}
def productStore = getProductStore();
context.productStore = productStore;
context.productStoreId = productStore?.productStoreId;

getWebSite = {
    def webSite = context.webSite;
    def webSiteId = webSite?.webSiteId ?: context.webSiteId ?: parameters.webSiteId;
    if (!webSite && webSiteId) {
        webSite = from("WebSite").where("webSiteId", webSiteId).cache().queryOne();
        if (!webSite) {
            Debug.logError("Could not find WebSite for webSiteId: " + webSiteId, module);
        }
    }
    return webSite;
}
def webSite = getWebSite();
context.webSite = webSite;
context.webSiteId = webSite?.webSiteId;

