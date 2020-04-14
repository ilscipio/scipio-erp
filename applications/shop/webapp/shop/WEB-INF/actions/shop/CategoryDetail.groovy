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

import org.ofbiz.base.util.cache.UtilCache

import static org.junit.Assert.assertArrayEquals

import org.ofbiz.base.util.*;
import org.ofbiz.entity.*;
import org.ofbiz.service.*;
import org.ofbiz.product.catalog.*;
import org.ofbiz.product.category.CategoryContentWrapper;
import org.ofbiz.product.category.CategoryWorker;
import org.ofbiz.product.product.ProductSearch.SortKeywordRelevancy;
import org.ofbiz.product.store.ProductStoreWorker;
import com.ilscipio.scipio.solr.*;

// SCIPIO: NOTE: This script is responsible for checking whether solr is applicable (if no check, implies the shop assumes solr is always enabled).

final module = "CategoryDetail.groovy";
def DEBUG = false;

UtilCache<String, Map> categoryCache = UtilCache.getOrCreateUtilCache("category.categorydetail.rendered", 0,0,
        UtilProperties.getPropertyAsLong("cache", "category.categorydetail.rendered.expireTime", 86400000),
        UtilProperties.getPropertyAsBoolean("cache", "category.categorydetail.rendered.softReference", true));
Boolean useCache = UtilProperties.getPropertyAsBoolean("cache", "category.categorydetail.rendered.enable", false);
Boolean lookupCategory = true;
// SCIPIO: this allows to use the script for local scopes without affecting request
localVarsOnly = context.localVarsOnly;
if (localVarsOnly == null) {
    localVarsOnly = false;
}
context.remove("localVarsOnly");

nowTimestamp = context.nowTimestamp ?: UtilDateTime.nowTimestamp();
productStore = context.productStore ?: ProductStoreWorker.getProductStore(request);

try {
    catArgs = context.catArgs ? new LinkedHashMap(context.catArgs) : new LinkedHashMap();
    catArgs.priceSortField = catArgs.priceSortField ?: "exists"; // "min", "exists", "exact"

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

    // SCIPIO: sorting
    // NOTE: We exploit "SortKeywordRelevancy" as a "none" value
    sortOrderDef = context.sortOrderDef != null ? context.sortOrderDef : "";
    context.sortOrderDef = sortOrderDef;
    sortAscendingDef = context.sortAscendingDef != null ? context.sortAscendingDef : true;
    context.sortAscendingDef = sortAscendingDef;
    resultSortOrder = null;
    if (parameters.sortOrder != null) {
        resultSortOrder = org.ofbiz.product.product.ProductSearchSession.parseSortOrder(
                parameters.sortOrder?.toString() ?: "SortKeywordRelevancy", !"N".equals(parameters.sortAscending));
        if (resultSortOrder != null && (session.getAttribute("scpProdSortOrder") == null || "Y" == parameters.sortChg)) {
            session.setAttribute("scpProdSortOrder", resultSortOrder);
            // NOTE: no thread safety required (not important)
        }
    } else {
        resultSortOrder = session.getAttribute("scpProdSortOrder"); // NOTE: no thread safety required (not important)
        if (resultSortOrder == null) {
            // check to see if can get a default from KeywordSearch.groovy options from session
            resultSortOrder = org.ofbiz.product.product.ProductSearchSession.getProductSearchOptionsIfExist(request)?.getResultSortOrder();
        }
    }
    sortOrder = null;
    sortAscending = null;
    if (resultSortOrder != null) {
        sortOrder = resultSortOrder.getOrderName();
        if ("KeywordRelevancy" == sortOrder) sortOrder = "";
        else if (sortOrder && !sortOrder.startsWith("Sort")) sortOrder = "Sort" + sortOrder;
        sortAscending = resultSortOrder.isAscending();

        catArgs.sortBy = SolrProductUtil.getSearchSortByExpr(resultSortOrder, catArgs.priceSortField, productStore, delegator, locale)
        catArgs.sortByReverse = (catArgs.sortBy) ? !resultSortOrder.isAscending() : null;
        catArgs.searchSortOrderString = (catArgs.sortBy || resultSortOrder instanceof SortKeywordRelevancy) ? resultSortOrder.prettyPrintSortOrder(false, locale) : null;
    }
    context.sortOrder = sortOrder;
    context.sortAscending = sortAscending;
    context.sortOrderEff = (sortOrder != null) ? sortOrder : sortOrderDef;
    context.sortAscendingEff = ((sortAscending != null) ? sortAscending : sortAscendingDef) ? "Y" : "N";

    catArgs.queryFilters = catArgs.queryFilters ? new ArrayList(catArgs.queryFilters) : new ArrayList();

    /**
     * Creates a unique product cachekey
     */
    getCategoryCacheKey = {
        String localeStr = "";
        if (context.locale == null) {
            localeStr = UtilProperties.getPropertyValue("scipiosetup", "store.defaultLocaleString");
        } else {
            localeStr = context.locale.toString();
        }
        // DEV NOTE: don't use UtilMisc.toInteger here because it falls back to value 0 which is vague for these variables; toIntegerObject returns null
        return delegator.getDelegatorName()+"::"+localeStr+"::"+productCategoryId+"::"+currentCatalogId+"::"+UtilMisc.toIntegerObject(viewSize)+"::"+
                UtilMisc.toIntegerObject(viewIndex)+"::"+UtilMisc.toIntegerObject(currIndex)+"::"+catArgs.priceSortField+"::"+sortOrder+"::"+sortAscending+"::"+
                catArgs.queryFilters+"::"+catArgs.useDefaultFilters;
    }

    // get the product entity
    String cacheKey = getCategoryCacheKey();
    if (useCache) {
        Map cachedValue = categoryCache.get(cacheKey);
        if (cachedValue != null) {
            if (DEBUG) {
                Debug.logInfo("Found cached category results for key [" + cacheKey + "]", module);
            }
            context.solrProducts = cachedValue.solrProducts;
            context.listIndex = cachedValue.listIndex;
            context.productCategory = cachedValue.productCategory;
            context.viewIndex = cachedValue.viewIndex;
            context.viewSize = cachedValue.viewSize;
            context.listSize = cachedValue.listSize;
            context.currIndex = cachedValue.currIndex;
            lookupCategory=false;
        } else {
            if (DEBUG) {
                Debug.logInfo("No cached category results for key [" + cacheKey + "]; doing lookup", module);
            }
        }
    }

    if(lookupCategory){

        viewProductCategoryId = CatalogWorker.getCatalogViewAllowCategoryId(delegator, currentCatalogId);
        if (viewProductCategoryId) {
            catArgs.queryFilters.add("cat:"+SolrExprUtil.escapeTermFull("0/"+viewProductCategoryId));
        }

        // get the product category & members
        result = dispatcher.runSync("solrProductsSearch",
                [productStore   : productStore, productCategoryId: productCategoryId, queryFilters: catArgs.queryFilters, useDefaultFilters: catArgs.useDefaultFilters,
                 filterTimestamp: nowTimestamp, viewSize: viewSize, viewIndex: viewIndex, sortBy: catArgs.sortBy, sortByReverse: catArgs.sortByReverse,
                 locale         : context.locale, userLogin: context.userLogin, timeZone: context.timeZone],
                -1, true); // SEPARATE TRANSACTION so error doesn't crash screen
        if (!ServiceUtil.isSuccess(result)) {
            throw new Exception("Error in solrProductsSearch: " + ServiceUtil.getErrorMessage(result));
        }

        productCategory = delegator.findOne("ProductCategory", UtilMisc.toMap("productCategoryId", productCategoryId), true);
        solrProducts = result.results;

        context.solrProducts = solrProducts;

        context.listIndex = 0;
        if (result.viewSize != null && result.viewSize > 0) {
            context.listIndex = Math.ceil(result.listSize / result.viewSize);
        }

        context.productCategory = productCategory;
        context.viewIndex = result.viewIndex;
        context.viewSize = result.viewSize;
        context.listSize = result.listSize;

        if (!currIndex) {
            context.currIndex = 1;
        } else {
            context.currIndex = Integer.parseInt(currIndex).intValue();
        }

        // cache
        catMap = [:];
        catMap.solrProducts = context.solrProducts;
        catMap.listIndex = context.listIndex;
        catMap.productCategory = context.productCategory;
        catMap.viewIndex = context.viewIndex;
        catMap.viewSize = context.viewSize;
        catMap.listSize = context.listSize;
        catMap.currIndex = context.currIndex;
        categoryCache.put(cacheKey,catMap)
    }

    // set the content path prefix
    contentPathPrefix = CatalogWorker.getContentPathPrefix(request);
    context.put("contentPathPrefix", contentPathPrefix);

} catch(Exception e) {
    Debug.logError("Solr: Error searching products in category: " + e.toString(), module);
}