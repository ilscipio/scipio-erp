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

/*
 * This script is also referenced by the shop's screens and
 * should not contain order component's specific code.
 */

import org.ofbiz.base.util.*
import org.ofbiz.base.util.cache.UtilCache;
import org.ofbiz.entity.*;
import org.ofbiz.entity.util.*;
import org.ofbiz.order.shoppingcart.*;
import org.ofbiz.product.catalog.*;
import org.ofbiz.product.config.ProductConfigWorker;
import org.ofbiz.product.product.ProductContentWrapper;
import org.ofbiz.product.store.*;
import org.ofbiz.service.*;
import com.ilscipio.scipio.solr.*;

// SCIPIO: NOTE: This script is responsible for checking whether solr is applicable (if no check, implies the shop assumes solr is always enabled).
final module = "ProductSummary.groovy";

UtilCache<String, Map> productCache = UtilCache.getOrCreateUtilCache("product.productsummary.rendered", 0,0,
        UtilProperties.getPropertyAsLong("cache", "product.productsummary.rendered.expireTime", 0L),
        UtilProperties.getPropertyAsBoolean("cache", "product.productsummary.rendered.softReference",true));
Boolean useCache = UtilProperties.getPropertyAsBoolean("cache", "product.productsummary.rendered.enable", false);
kwsArgs = context.kwsArgs ?: [:];
cfgPropRes = kwsArgs.cfgPropRes ?: application.getAttribute("shopSearchCfgRes") ?: "shop";
cfgPropPrefix = kwsArgs.cfgPropPrefix != null ? kwsArgs.cfgPropPrefix : "shop.";
searchDataSrc = UtilProperties.getPropertyValue(cfgPropRes, cfgPropPrefix+"search.solr.dataSrc", "entity");
context.searchDataSrc = searchDataSrc;
avoidEntityData = (searchDataSrc == "solr");
context.avoidEntityData = avoidEntityData;

//either optProduct, optProductId or productId must be specified
product = request.getAttribute("optProduct");
optProductId = request.getAttribute("optProductId");
productId = product?.productId ?: optProductId ?: request.getAttribute("productId");
solrProduct = request.getAttribute("solrProduct");
solrProducts = context.solrProducts;
categoryId = null;
reviews = null;
sizeProductFeatureAndAppls = null;
autoUserLogin = context.autoUserLogin; // SCIPIO: use context login instead: session.getAttribute("autoUserLogin");
userLogin = context.userLogin; // SCIPIO: use context login instead: session.getAttribute("userLogin");
webSiteId = CatalogWorker.getWebSiteId(request);
catalogId = CatalogWorker.getCurrentCatalogId(request);
cart = ShoppingCartEvents.getCartObject(request);
productStore = null;
productStoreId = null;
facilityId = null;
if (cart.isSalesOrder()) {
    productStore = ProductStoreWorker.getProductStore(request);
    productStoreId = productStore.productStoreId;
    context.productStoreId = productStoreId;
    facilityId = productStore.inventoryFacilityId;
}

if (!solrProduct && solrProducts && productId) {
    // FIXME: inefficient
    for(sp in solrProducts) {
        if (sp.productId == productId) {
            solrProduct = sp;
            break;
        }
    }
}

def toBigDecimalCurrency(priceVal) { // SCIPIO
    if (priceVal == null) return null;
    if (!(priceVal instanceof BigDecimal)) {
        if (priceVal instanceof String) {
            priceVal = new BigDecimal(priceVal);
        } else {
            priceVal = BigDecimal.valueOf(priceVal);
        }
    }
    priceVal.setScale(ShoppingCart.scale, ShoppingCart.rounding);
};

/**
 * Creates a unique product cachekey
 * */
getProductCacheKey = {
    if (userLogin) {
        return delegator.getDelegatorName()+"::"+productId+"::"+webSiteId+"::"+catalogId+"::"+productStoreId+"::"+cart.getCurrency()+"::"+userLogin.partyId;
    } else {
        return delegator.getDelegatorName()+"::"+productId+"::"+webSiteId+"::"+catalogId+"::"+productStoreId+"::"+cart.getCurrency()+"::"+"_NA_";
    }
}

// IMPORTANT: These cannot be removed with context.remove due to MapStack; you must set them to explicit null
context.daysToShip = null;
context.averageRating = null;
context.numRatings = null;
context.totalPrice = null;

//context.product = null;
context.productId = null;
context.hasProduct = null;
context.price = null;
context.requireAmount = null;
context.solrProduct = null;
context.categoryId = null;
context.productReviews = null;
context.sizeProductFeatureAndAppls = null;
context.numRatings = null;
context.averageRating = null;
context.mainProducts = null;
context.virtualJavaScript = null;
context.variantPriceList = null;
context.daysToShip = null;
context.solrTitle = null;
context.title = null;
context.description = null;
context.longdescription = null;

// get the product entity
String cacheKey = getProductCacheKey();
if (useCache) {
    Map cachedValue = productCache.get(cacheKey);
    if (cachedValue != null) {
        product = context.product;
        context.product = cachedValue.product;
        context.productId = cachedValue.productId;
        context.hasProduct = cachedValue.hasProduct;
        context.price = cachedValue.price;
        context.requireAmount = cachedValue.requireAmount;
        context.solrProduct = cachedValue.solrProduct;
        context.categoryId = cachedValue.categoryId;
        context.productReviews = cachedValue.productReviews;
        context.sizeProductFeatureAndAppls = cachedValue.sizeProductFeatureAndAppls;
        context.numRatings = cachedValue.numRatings;
        context.averageRating = cachedValue.averageRating;
        context.mainProducts = cachedValue.mainProducts;
        context.virtualJavaScript = cachedValue.virtualJavaScript;
        context.variantPriceList = cachedValue.variantPriceList;
        context.daysToShip = cachedValue.daysToShip;
        context.solrTitle = cachedValue.solrTitle;
        context.title = cachedValue.title;
        context.description = cachedValue.description;
        context.longdescription = cachedValue.longdescription;
    }
}

if(!context.product){
    if (!avoidEntityData && !product && productId) {
        product = from("Product").where("productId", productId).cache().queryOne();
    }

    if (product) {

        /***********
         Entity Product
         **********/
        //if order is purchase then don't calculate available inventory for product.
        if (cart.isSalesOrder()) {
            /*resultOutput = dispatcher.runSync("getInventoryAvailableByFacility", [productId : product.productId, facilityId : facilityId, useCache : true]);
            totalAvailableToPromise = resultOutput.availableToPromiseTotal;
            if (totalAvailableToPromise && totalAvailableToPromise.doubleValue() > 0) {
                productFacility = delegator.findOne("ProductFacility", [productId : product.productId, facilityId : facilityId], true);
                if (productFacility?.daysToShip != null) {
                    context.daysToShip = productFacility.daysToShip;
                }
            }*/
        } else {
            supplierProducts = delegator.findByAnd("SupplierProduct", [productId : productId], ["-availableFromDate"], true);
            supplierProduct = EntityUtil.getFirst(supplierProducts);
            if (supplierProduct?.standardLeadTimeDays != null) {
                standardLeadTimeDays = supplierProduct.standardLeadTimeDays;
                daysToShip = standardLeadTimeDays + 1;
                context.daysToShip = daysToShip;
            }
        }

        categoryId = parameters.category_id ?: request.getAttribute("productCategoryId");

        // get the product price
        if (cart.isSalesOrder()) {
            // sales order: run the "calculateProductPrice" service
            priceContext = [product : product, currencyUomId : cart.getCurrency(),
                            autoUserLogin : autoUserLogin, userLogin : userLogin];
            priceContext.webSiteId = webSiteId;
            priceContext.prodCatalogId = catalogId;
            priceContext.productStoreId = productStoreId;
            priceContext.agreementId = cart.getAgreementId();
            priceContext.partyId = cart.getPartyId();  // IMPORTANT: otherwise it'll be calculating prices using the logged in user which could be a CSR instead of the customer
            priceContext.checkIncludeVat = "Y";
            priceContext.getMinimumVariantPrice = true;
            priceMap = dispatcher.runSync("calculateProductPrice", priceContext);

            context.price = priceMap;
        } else {
            // purchase order: run the "calculatePurchasePrice" service
            priceContext = [product : product, currencyUomId : cart.getCurrency(),
                            partyId : cart.getPartyId(), userLogin : userLogin];
            priceMap = dispatcher.runSync("calculatePurchasePrice", priceContext);

            context.price = priceMap;
        }

        // get the product review(s)
        reviews = product.getRelated("ProductReview", null, ["-postedDateTime"], false);
        sizeProductFeatureAndAppls = delegator.findByAnd("ProductFeatureAndAppl", [productId : productId, productFeatureTypeId : "SIZE"], ["sequenceNum", "defaultSequenceNum"], true);

    } else if (solrProduct) {

        /***********
         SOLR Product
         **********/

        solrCurrency = com.ilscipio.scipio.solr.SolrProductUtil.getConfiguredDefaultCurrency(delegator,
                org.ofbiz.product.store.ProductStoreWorker.getProductStore(request)); // SCIPIO

        solrProductWorker = SolrValueWorker.getWorker(solrProduct, context.locale, productStore ?: ProductStoreWorker.getProductStore(request));

        context.solrTitle = solrProductWorker.getFieldValueI18nForDisplay("title");
        context.title = context.solrTitle;
        context.description = solrProductWorker.getFieldValueI18nForDisplay("description");
        context.longdescription = solrProductWorker.getFieldValueI18nForDisplay("longdescription");
        context.longDescription = context.longdescription;

        categoryId = parameters.category_id ?: request.getAttribute("productCategoryId");
        // get the product price
        def priceMap = [price: toBigDecimalCurrency(solrProduct.defaultPrice),
                        listPrice: toBigDecimalCurrency(solrProduct.listPrice),
                        currencyUsed: cart.getCurrency() ?: solrCurrency
        ];
        if (cart.getCurrency() && solrCurrency != cart.getCurrency()) {
            // NOTE: CONVERSION LOGIC FROM: org.ofbiz.product.price.PriceServices.calculateProductPrice(DispatchContext, Map<String, ? extends Object>)
            priceResults = dispatcher.runSync("convertUom", ["uomId": solrCurrency, "uomIdTo": cart.getCurrency(),
                                                             "originalValue": priceMap.price, "defaultDecimalScale": Long.valueOf(2), "defaultRoundingMode": "HalfUp"]);
            if (ServiceUtil.isError(priceResults) || (priceResults.get("convertedValue") == null)) {
                Debug.logWarning("Unable to convert default price for product  " + productId, module);
            } else {
                priceMap.price = priceResults.convertedValue;
            }
            if (priceMap.listPrice != null) {
                priceResults = dispatcher.runSync("convertUom", ["uomId": solrCurrency, "uomIdTo": cart.getCurrency(),
                                                                 "originalValue": priceMap.listPrice, "defaultDecimalScale": Long.valueOf(2), "defaultRoundingMode": "HalfUp"]);
                if (ServiceUtil.isError(priceResults) || (priceResults.get("convertedValue") == null)) {
                    Debug.logWarning("Unable to convert list price for product  " + productId, module);
                } else {
                    priceMap.listPrice = priceResults.convertedValue;
                }
            }
        }
        //context.totalPrice = priceMap.price;
        context.price = priceMap;
        // get aggregated product totalPrice
        // SCIPIO: TODO: REVIEW
        //    if ("AGGREGATED".equals(product.productTypeId)) {
        //        configWrapper = ProductConfigWorker.getProductConfigWrapper(productId, cart.getCurrency(), request);
        //        if (configWrapper) {
        //            configWrapper.setDefaultConfig();
        //            context.totalPrice = configWrapper.getTotalPrice();
        //        }
        //    }
    }

    //
    // get the average rating
    if (reviews) {
        totalProductRating = 0;
        numRatings = 0;
        reviews.each { productReview ->
            productRating = productReview.productRating;
            if (productRating) {
                totalProductRating += productRating;
                numRatings++;
            }
        }
        if (numRatings) {
            context.averageRating = totalProductRating/numRatings;
            context.numRatings = numRatings;
        }
    }

    context.product = product;
    context.productId = productId; // SCIPIO
    context.hasProduct = (product != null || solrProduct != null);
    context.requireAmount = (product != null) ? product.requireAmount : (solrProduct?.requireAmount_b == true ? "Y" : "N"); // SCIPIO
    context.solrProduct = solrProduct;
    context.categoryId = categoryId;
    context.productReviews = reviews;
    context.sizeProductFeatureAndAppls = sizeProductFeatureAndAppls;

    // cache
    prodMap = [:];
    prodMap.product = context.product;
    prodMap.productId = context.productId;
    prodMap.hasProduct = context.hasProduct;
    prodMap.requireAmount = context.requireAmount;
    prodMap.price = context.price;
    prodMap.solrProduct = context.solrProduct;
    prodMap.categoryId = context.categoryId;
    prodMap.productReviews = context.productReviews;
    prodMap.sizeProductFeatureAndAppls = context.sizeProductFeatureAndAppls;
    prodMap.numRatings = context.numRatings;
    prodMap.averageRating = context.averageRating;
    prodMap.mainProducts = context.mainProducts;
    prodMap.virtualJavaScript = context.virtualJavaScript;
    prodMap.variantPriceList = context.variantPriceList;
    prodMap.daysToShip = context.daysToShip;
    prodMap.solrTitle = context.solrTitle
    prodMap.title = context.title
    prodMap.description = context.description
    prodMap.longdescription = context.longdescription
    productCache.put(cacheKey,prodMap);
}

if(context.product) {
    // get aggregated product totalPrice
    if ("AGGREGATED".equals(context.product.productTypeId)) {
        configWrapper = ProductConfigWorker.getProductConfigWrapper(productId, cart.getCurrency(), request);
        if (configWrapper) {
            configWrapper.setDefaultConfig();
            context.totalPrice = configWrapper.getTotalPrice();
        }
    }

    // make the productContentWrapper
    productContentWrapper = new ProductContentWrapper(context.product, request);
    context.productContentWrapper = productContentWrapper;
} else {
    if (productId && !solrProduct) { // SCIPIO: report this, could be due to inefficient caching or solr setup
        Debug.logWarning("Shop: Product '" + productId + "' not found in DB (caching/solr sync?)", module);
    }
}
