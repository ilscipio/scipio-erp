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
 * NOTE: This script is also referenced by the shop's screens and
 * should not contain order component's specific code.
 */

import org.ofbiz.base.util.*;
import org.ofbiz.entity.*;
import org.ofbiz.service.*;
import org.ofbiz.product.catalog.*;
import org.ofbiz.product.category.CategoryContentWrapper;
import org.ofbiz.product.category.CategoryWorker;
import org.ofbiz.product.store.ProductStoreWorker;

// SCIPIO: NOTE: This script is responsible for checking whether solr is applicable (if no check, implies the shop assumes solr is always enabled).

final module = "CategoryDetail.groovy";

// SCIPIO: this allows to use the script for local scopes without affecting request
localVarsOnly = context.localVarsOnly;
if (localVarsOnly == null) {
    localVarsOnly = false;
}
context.remove("localVarsOnly");

try {
    productCategoryId = context.productCategoryId;
    viewSize = context.viewSize;
    viewIndex = context.viewIndex;
    currIndex = context.currIndex;
    
    if (!localVarsOnly) {
        if (!productCategoryId) {
            productCategoryId = request.getAttribute("productCategoryId");
        }
        if (!viewSize) {
            viewSize = parameters.VIEW_SIZE;
        }
        if (!viewIndex) {
            viewIndex = parameters.VIEW_INDEX;
        }
        if (!currIndex) {
            currIndex = parameters.CURR_INDEX;
        }
    }
    
    context.productCategoryId = productCategoryId;
    currentCatalogId = CatalogWorker.getCurrentCatalogId(request);
    
    // get the product category & members
    result = dispatcher.runSync("solrProductsSearch",
        [productCategoryId:productCategoryId, viewSize:viewSize, viewIndex:viewIndex, 
         locale:context.locale, userLogin:context.userLogin, timeZone:context.timeZone],
        -1, true); // SEPARATE TRANSACTION so error doesn't crash screen
    if (!ServiceUtil.isSuccess(result)) {
        throw new Exception("Error in solrProductsSearch: " + ServiceUtil.getErrorMessage(result));
    }
    
    productCategory = delegator.findOne("ProductCategory", UtilMisc.toMap("productCategoryId", productCategoryId), true);
    solrProducts = result.results;
    
    // Prevents out of stock product to be displayed on site
    productStore = ProductStoreWorker.getProductStore(request);
    if(productStore) {
        if("N".equals(productStore.showOutOfStockProducts)) {
            productsInStock = [];
            solrProducts.each { productCategoryMember ->
                productFacility = delegator.findOne("ProductFacility", [productId : productCategoryMember.productId, facilityId : productStore.inventoryFacilityId], true);
                if(productFacility) {
                    if(productFacility.lastInventoryCount >= 1) {
                        productsInStock.add(productCategoryMember);
                    }
                }
            }
            context.solrProducts = productsInStock;
        } else {
            context.solrProducts = solrProducts;
        }
    }
    
    /*
    subCatList = [];
    if (CategoryWorker.checkTrailItem(request, productCategory.getString("productCategoryId")) || (!UtilValidate.isEmpty(productCategoryId) && productCategoryId == productCategory.productCategoryId))
        subCatList = CategoryWorker.getRelatedCategoriesRet(request, "subCatList", productCategory.getString("productCategoryId"), true);
    
    context.productSubCategoryList = subCatList;
    */

    context.listIndex = 0;
    if (result.viewSize > 0) {
        context.listIndex = Math.ceil(result.listSize/result.viewSize);
    }
    // SCIPIO: this may not make sense anymore since SOLR patches
    //if (!viewSize.equals(String.valueOf(result.viewSize))) {
    //    pageViewSize = Integer.parseInt(viewSize).intValue();
    //    context.listIndex = Math.ceil(result.listSize/pageViewSize);
    //    context.pageViewSize = pageViewSize;
    //}
    
    
    context.productCategory = productCategory;
    context.viewIndex = result.viewIndex;
    context.viewSize = result.viewSize;
    context.listSize = result.listSize;
    
    if (!currIndex)
        context.currIndex = 1;
    else
        context.currIndex = Integer.parseInt(currIndex).intValue();
    
    // set the content path prefix
    contentPathPrefix = CatalogWorker.getContentPathPrefix(request);
    context.put("contentPathPrefix", contentPathPrefix);
    
    /* SCIPIO: do NOT do this for now (or ever?)
    parameters.VIEW_SIZE = viewSize;
    parameters.VIEW_INDEX = viewIndex;
    parameters.CURR_INDEX = CURR_INDEX;
    */
    
} catch(Exception e) {
    Debug.logError(e, e.getMessage(), module);
}
