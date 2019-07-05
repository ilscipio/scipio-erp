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

import org.ofbiz.base.util.*;
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
// SCIPIO: config
kwsArgs = context.kwsArgs ?: [:];
cfgPropRes = kwsArgs.cfgPropRes ?: application.getAttribute("shopSearchCfgRes") ?: "shop";
cfgPropPrefix = kwsArgs.cfgPropPrefix != null ? kwsArgs.cfgPropPrefix : "shop.";
searchDataSrc = UtilProperties.getPropertyValue(cfgPropRes, cfgPropPrefix+"search.solr.dataSrc", "entity");
context.searchDataSrc = searchDataSrc;
avoidEntityData = (searchDataSrc == "solr");
context.avoidEntityData = avoidEntityData;

solrCurrency = com.ilscipio.scipio.solr.SolrProductUtil.getConfiguredDefaultCurrency(delegator,
    org.ofbiz.product.store.ProductStoreWorker.getProductStore(request)); // SCIPIO

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

//either optProduct, optProductId or productId must be specified
product = request.getAttribute("optProduct");
optProductId = request.getAttribute("optProductId");
productId = product?.productId ?: optProductId ?: request.getAttribute("productId");
solrProduct = request.getAttribute("solrProduct");
solrProducts = context.solrProducts;
if (!solrProduct && solrProducts && productId) {
    // FIXME: inefficient
    for(sp in solrProducts) {
        if (sp.productId == productId) {
            solrProduct = sp;
            break;
        }
    }
}

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
autoUserLogin = context.autoUserLogin; // SCIPIO: use context login instead: session.getAttribute("autoUserLogin");
userLogin = context.userLogin; // SCIPIO: use context login instead: session.getAttribute("userLogin");

context.remove("daysToShip");
context.remove("averageRating");
context.remove("numRatings");
context.remove("totalPrice");

// get the product entity
if (!avoidEntityData && !product && productId) {
    product = delegator.findOne("Product", [productId : productId], true);
}
if (product) {
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
       supplierProducts = delegator.findByAnd("SupplierProduct", [productId : product.productId], ["-availableFromDate"], true);
       supplierProduct = EntityUtil.getFirst(supplierProducts);
       if (supplierProduct?.standardLeadTimeDays != null) {
           standardLeadTimeDays = supplierProduct.standardLeadTimeDays;
           daysToShip = standardLeadTimeDays + 1;
           context.daysToShip = daysToShip;
       }
    }
    // make the productContentWrapper
    productContentWrapper = new ProductContentWrapper(product, request);
    context.productContentWrapper = productContentWrapper;
} else if (solrProduct) {
    solrProductWorker = SolrValueWorker.getWorker(solrProduct, context.locale, productStore ?: ProductStoreWorker.getProductStore(request));

    context.solrTitle = solrProductWorker.getFieldValueI18nForDisplay("title");
    context.title = context.solrTitle;
    context.description = solrProductWorker.getFieldValueI18nForDisplay("description");
    context.longdescription = solrProductWorker.getFieldValueI18nForDisplay("longdescription");
    context.longDescription = context.longdescription;
} else {
    if (productId) { // SCIPIO: report this, could be due to inefficient caching or solr setup
        Debug.logWarning("Shop: Product '" + productId + "' not found in DB (caching/solr sync?)", module);
    }
}

categoryId = null;
reviews = null;
if (product) {
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

    // get aggregated product totalPrice
    if ("AGGREGATED".equals(product.productTypeId)) {
        configWrapper = ProductConfigWorker.getProductConfigWrapper(productId, cart.getCurrency(), request);
        if (configWrapper) {
            configWrapper.setDefaultConfig();
            context.totalPrice = configWrapper.getTotalPrice();
        }
    }

    // get the product review(s)
    reviews = product.getRelated("ProductReview", null, ["-postedDateTime"], false);
} else if (solrProduct) {
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

// an example of getting features of a certain type to show
sizeProductFeatureAndAppls = null;
if (!avoidEntityData) { // SCIPIO
    sizeProductFeatureAndAppls = delegator.findByAnd("ProductFeatureAndAppl", [productId : productId, productFeatureTypeId : "SIZE"], ["sequenceNum", "defaultSequenceNum"], true);
}

context.product = product;
context.productId = productId; // SCIPIO
context.hasProduct = (product != null || solrProduct != null);
context.requireAmount = (product != null) ? product.requireAmount : (solrProduct?.requireAmount_b == true ? "Y" : "N"); // SCIPIO
context.solrProduct = solrProduct;
context.categoryId = categoryId;
context.productReviews = reviews;
context.sizeProductFeatureAndAppls = sizeProductFeatureAndAppls;

