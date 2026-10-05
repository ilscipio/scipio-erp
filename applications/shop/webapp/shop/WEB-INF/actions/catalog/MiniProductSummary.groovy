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


import org.ofbiz.base.util.cache.UtilCache

import java.math.BigDecimal;
import java.util.Map;

import org.ofbiz.base.util.*;
import org.ofbiz.entity.*;
import org.ofbiz.service.*;
import org.ofbiz.product.product.ProductContentWrapper;
import org.ofbiz.product.config.ProductConfigWorker;
import org.ofbiz.product.catalog.*;
import org.ofbiz.product.store.*;
import org.ofbiz.order.shoppingcart.*;
import org.ofbiz.webapp.website.WebSiteWorker;

final module = "MiniProductSummary.groovy";

UtilCache<String, Map> productCache = UtilCache.getOrCreateUtilCache("product.miniproductsummary.rendered", 0,0,
        UtilProperties.getPropertyAsLong("cache", "product.miniproductsummary.rendered.expireTime", 86400000),
        UtilProperties.getPropertyAsBoolean("cache", "product.miniproductsummary.rendered.softReference", true));
Boolean useCache = UtilProperties.getPropertyAsBoolean("cache", "product.miniproductsummary.rendered.enable", false);
miniProduct = context.miniProduct ? context.miniProduct : request.getAttribute("miniProduct");
optProductId = request.getAttribute("optProductId");
webSiteId = WebSiteWorker.getWebSiteId(request);
prodCatalogId = CatalogWorker.getCurrentCatalogId(request);
productStoreId = ProductStoreWorker.getProductStoreId(request);
cart = ShoppingCartEvents.getCartObject(request);

context.remove("totalPrice");
context.miniProdFormName = request.getAttribute("miniProdFormName");
context.miniProdQuantity = request.getAttribute("miniProdQuantity");
context.nowTimeLong = nowTimestamp.getTime();


/**
 * Creates a unique product cachekey
 * */
getProductCacheKey = {
    if (userLogin) {
        return delegator.getDelegatorName()+"::"+optProductId+"::"+webSiteId+"::"+prodCatalogId+"::"+productStoreId+"::"+cart.getCurrency()+"::"+userLogin.partyId;
    } else {
        return delegator.getDelegatorName()+"::"+optProductId+"::"+webSiteId+"::"+prodCatalogId+"::"+productStoreId+"::"+cart.getCurrency()+"::"+"_NA_";
    }
}

if(!miniProduct){
    String cacheKey = getProductCacheKey();
    if (useCache) {
        Map cachedValue = productCache.get(cacheKey);
        if (cachedValue != null) {
            miniProduct = cachedValue.miniProduct;
            context.miniProduct = cachedValue.miniProduct;
            context.price = cachedValue.price;
            context.priceResult = cachedValue.priceResult;
        }
    }

    if (!miniProduct && optProductId) {
        miniProduct = from("Product").where("productId", optProductId).cache().queryOne();

        if(!miniProduct){
            Debug.logWarning("Shop: Product '" + productId + "' not found in DB (caching/solr sync?)", module);
            return
        }
        context.miniProduct = miniProduct;

        // calculate the "your" price
        priceParams = [product : miniProduct,
                       prodCatalogId : prodCatalogId,
                       webSiteId : webSiteId,
                       currencyUomId : cart.getCurrency(),
                       autoUserLogin : autoUserLogin,
                       productStoreId : productStoreId];
        if (userLogin) priceParams.partyId = userLogin.partyId;
        priceResult = runService('calculateProductPrice', priceParams);
        // returns: isSale, price, orderItemPriceInfos
        context.priceResult = priceResult;
        // Check if Price has to be displayed with tax
        if (productStore.get("showPricesWithVatTax").equals("Y")) {
            Map priceMap = runService('calcTaxForDisplay', ["basePrice": priceResult.get("price"), "locale": locale, "productId": optProductId, "productStoreId": productStoreId]);
            context.price = priceMap.get("priceWithTax");
        } else {
            context.price = priceResult.get("price");
        }

        // cache
        prodMap = [:];
        prodMap.priceResult = context.priceResult;
        prodMap.price = context.price;
        prodMap.miniProduct = context.miniProduct;
        productCache.put(cacheKey,prodMap)
    }
}

if(miniProduct){
    // get aggregated product totalPrice
    if ("AGGREGATED".equals(miniProduct.productTypeId) || "AGGREGATED_SERVICE".equals(miniProduct.productTypeId)) {
        configWrapper = ProductConfigWorker.getProductConfigWrapper(optProductId, cart.getCurrency(), request);
        if (configWrapper) {
            configWrapper.setDefaultConfig();
            // Check if Config Price has to be displayed with tax
            if (productStore.get("showPricesWithVatTax").equals("Y")) {
                BigDecimal totalPriceNoTax = configWrapper.getTotalPrice();
                Map totalPriceMap = runService('calcTaxForDisplay', ["basePrice": totalPriceNoTax, "locale": locale, "productId": optProductId, "productStoreId": productStoreId]);
                context.totalPrice = totalPriceMap.get("priceWithTax");
            } else {
                context.totalPrice = configWrapper.getTotalPrice();
            }
        }
    }

    // make the miniProductContentWrapper
    ProductContentWrapper miniProductContentWrapper = new ProductContentWrapper(miniProduct, request);
    context.miniProductContentWrapper = miniProductContentWrapper;
}