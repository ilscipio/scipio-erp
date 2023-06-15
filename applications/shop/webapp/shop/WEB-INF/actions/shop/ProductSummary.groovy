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
import com.ilscipio.scipio.solr.*
import org.ofbiz.webapp.website.WebSiteWorker;

// SCIPIO: NOTE: This script is responsible for checking whether solr is applicable (if no check, implies the shop assumes solr is always enabled).
final module = "ProductSummary.groovy";

UtilCache<String, Map> productCache = UtilCache.getOrCreateUtilCache("product.productsummary.rendered", 0,0,
        UtilProperties.getPropertyAsLong("cache", "product.productsummary.rendered.expireTime", 86400000),
        UtilProperties.getPropertyAsBoolean("cache", "product.productsummary.rendered.softReference",true));
Boolean useCache = UtilProperties.getPropertyAsBoolean("cache", "product.productsummary.rendered.enable", false);
kwsArgs = context.kwsArgs ?: [:];
cfgPropRes = kwsArgs.cfgPropRes ?: application.getAttribute("shopSearchCfgRes") ?: "shop";
cfgPropPrefix = kwsArgs.cfgPropPrefix != null ? kwsArgs.cfgPropPrefix : "shop.";
searchDataSrc = UtilProperties.getPropertyValue(cfgPropRes, cfgPropPrefix+"search.solr.dataSrc", "entity");
context.searchDataSrc = searchDataSrc;
avoidEntityData = (searchDataSrc == "solr");
context.avoidEntityData = avoidEntityData;

// Either optProduct, optProductId or productId must be specified
// SCIPIO: NOTE: optProduct(Id) must always be checked before context/request product(Id) because the latter
//  may be referencing an object in the parent screen lingering in context/request in that case;
//  likewise, optProductId must have precedence over "product" and "solrProduct" (backward-compat)
product = request.getAttribute("optProduct")
optProductId = request.getAttribute("optProductId")
productId = product?.productId ?: optProductId ?: context.productId ?: request.getAttribute("productId");
if (!productId) {
    Debug.logError("Missing productId", module);
}
solrProduct = context.solrProduct;
if (solrProduct == null) {
    solrProduct = request.getAttribute("solrProduct")
}
// NOTE: solrProduct could be an artifact from parent screen, so make sure productId is good
if (solrProduct != null && (solrProduct.productId != productId || !productId)) {
    solrProduct = null;
}
solrProducts = context.solrProducts;
if (solrProduct == null && solrProducts && productId) {
    for (sp in solrProducts) {
        if (sp.productId == productId) {
            solrProduct = sp;
            break;
        }
    }
}

categoryId = null;
reviews = null;
sizeProductFeatureAndAppls = null;
autoUserLogin = context.autoUserLogin; // SCIPIO: use context login instead: session.getAttribute("autoUserLogin");
userLogin = context.userLogin; // SCIPIO: use context login instead: session.getAttribute("userLogin");
webSiteId = WebSiteWorker.getWebSiteId(request);
catalogId = CatalogWorker.getCurrentCatalogId(request);
cart = ShoppingCartEvents.getCartObject(request);

productStore = context.productStore;
if (productStore == null) {
    productStore = ProductStoreWorker.getProductStore(request)
}
productStoreId = productStore?.productStoreId;
context.productStoreId = productStoreId;
facilityId = null;
if (cart.isSalesOrder()) {
    facilityId = productStore.inventoryFacilityId;
    if (!avoidEntityData && !facilityId) {
        productStoreFacility = from("ProductStoreFacility").select("facilityId").where("productStoreId", productStoreId).filterByDate().cache().queryFirst();
        if (productStoreFacility != null) {
            facilityId = productStoreFacility.facilityId;
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
    return delegator.getDelegatorName()+"::"+productId+"::"+webSiteId+"::"+catalogId+"::"+productStoreId+"::"+cart.getCurrency()+"::"+(userLogin?.partyId ?: "_NA_");
}
List<String> cacheFields = [
        "averageRating",
        "categoryId",
        "daysToShip",
        "description",
        "hasProduct",
        "longDescription",
        "mainProducts",
        "numRatings",
        "price",
        "product",
        "productId",
        "productReviews",
        "requireAmount",
        "sizeProductFeatureAndAppls",
        "solrProduct",
        "solrTitle",
        "title",
        "totalPrice",
        "variantPriceList",
        "virtualJavaScript"
]

// Reset context vars with explicit null, including product and productId (essential for check below to work)
// IMPORTANT: These cannot be removed with context.remove due to MapStack; you must set them to explicit null
// (Cast helps IDEA compiler infer context var type)
UtilMisc.putNull((Map<String, Object>) context, cacheFields);

// get the product entity
String cacheKey = null;
Map cachedCtx = null;
if (useCache) {
    cacheKey = getProductCacheKey();
    cachedCtx = productCache.get(cacheKey);
    if (cachedCtx != null) {
        product = cachedCtx.product;
        UtilMisc.putKeys(context, cachedCtx, cacheFields);
    }
}

// Always look this up here because Product is too important to miss even if solrProduct is set; used to be done further below
if (product == null && productId) {
    product = from("Product").where("productId", productId).cache().queryOne();
    context.product = product;
}

if (cachedCtx == null && productId) {
    if (product != null) {

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

    } else if (solrProduct != null) {

        /***********
         SOLR Product
         **********/

        solrCurrency = SolrProductUtil.getConfiguredDefaultCurrency(delegator, productStore); // SCIPIO

        solrProductWorker = SolrValueWorker.getWorker(solrProduct, context.locale, productStore);

        context.solrTitle = solrProductWorker.getFieldValueI18nForDisplay("title");
        context.title = context.solrTitle;
        context.description = solrProductWorker.getFieldValueI18nForDisplay("description");
        context.longDescription = solrProductWorker.getFieldValueI18nForDisplay("longdescription");

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
    cachedCtx = UtilMisc.putKeys([:], context, cacheFields);
    if (cacheKey == null) {
        cacheKey = getProductCacheKey();
    }
    productCache.put(cacheKey, cachedCtx);
}

context.longdescription = context.longDescription; // compatibility

if (product != null) {
    // get aggregated product totalPrice
    if ("AGGREGATED".equals(product.productTypeId)) {
        configWrapper = ProductConfigWorker.getProductConfigWrapper(productId, cart.getCurrency(), request);
        if (configWrapper != null) {
            configWrapper.setDefaultConfig();
            context.totalPrice = configWrapper.getTotalPrice();
        }
    }
}

productContentWrapper = (product != null) ? new ProductContentWrapper(product, request) : null;
context.productContentWrapper = productContentWrapper;
