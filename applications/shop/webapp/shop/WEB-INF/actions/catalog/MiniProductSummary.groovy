/*
 * Licensed to the Apache Software Foundation (ASF) under one
 * or more contributor license agreements.  See the NOTICE file
 * distributed with this work for additional information
 * regarding copyright ownership.  The ASF licenses this file
 * to you under the Apache License, Version 2.0 (the
 * "License"); you may not use this file except in compliance
 * with the License.  You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 * KIND, either express or implied.  See the License for the
 * specific language governing permissions and limitations
 * under the License.
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


// Setup
UtilCache<String, Map> productCache = UtilCache.getOrCreateUtilCache("product.miniproductsummary.rendered", 0,0,
        UtilMisc.toLongObject(UtilProperties.getPropertyValue("cache", "product.miniproductsummary.rendered.expireTime","0")),
        UtilMisc.booleanValue(UtilProperties.getPropertyValue("cache", "product.miniproductsummary.rendered.softReference","true"), true));
Boolean useCache = UtilMisc.booleanValue(UtilProperties.getPropertyValue("cache", "product.miniproductsummary.rendered.enable","false"), false);
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
String getProductCacheKey(){
    if (userLogin){
        return optProductId+"::"+webSiteId+"::"+prodCatalogId+"::"+productStoreId+"::"+cart.getCurrency()+"::"+userLogin.partyId;
    }else{
        return optProductId+"::"+webSiteId+"::"+prodCatalogId+"::"+productStoreId+"::"+cart.getCurrency()+"::"+"_NA_";
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