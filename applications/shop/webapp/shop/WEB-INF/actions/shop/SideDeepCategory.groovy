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
import org.ofbiz.product.catalog.*
import org.ofbiz.product.store.ProductStoreWorker;
import org.ofbiz.service.*;
import com.ilscipio.scipio.solr.*;

// SCIPIO: NOTE: This script is responsible for checking whether solr is applicable (if no check, implies the shop assumes solr is always enabled).

final module = "SideDeepCategory.groovy";

final boolean DEBUG = Debug.verboseOn();
//final boolean DEBUG = true;

currentTrail = org.ofbiz.product.category.CategoryWorker.getCategoryPathFromTrailAsList(request);
currentCatalogId = CatalogWorker.getCurrentCatalogId(request);
// SCIPIO: IMPORTANT: Check request attribs before parameters map
curCategoryId = parameters.category_id ?: parameters.CATEGORY_ID ?: request.getAttribute("productCategoryId") ?: parameters.productCategoryId ?: "";
//curProductId = parameters.product_id ?: "" ?: parameters.PRODUCT_ID ?: "";    
topCategoryId = CatalogWorker.getCatalogTopCategoryId(request, currentCatalogId);

nowTimestamp = context.nowTimestamp ?: UtilDateTime.nowTimestamp();
productStore = context.productStore ?: ProductStoreWorker.getProductStore(request);

infoMap = [currentTrail:currentTrail, curCategoryId:curCategoryId];

if (DEBUG) Debug.logVerbose("Category (pre-resolve): " + infoMap, module);

catLevel = null; // use null here, not empty list
if (curCategoryId) {
    catArgs = context.catArgs ? new HashMap(context.catArgs) : new HashMap();
    catArgs.queryFilters = catArgs.queryFilters ? new ArrayList(catArgs.queryFilters) : new ArrayList();

    try {
        // TODO?: cache results?
        result = dispatcher.runSync("solrSideDeepCategory",
            [productStore:productStore, productCategoryId:curCategoryId, catalogId:currentCatalogId, 
             currentTrail:currentTrail, queryFilters:catArgs.queryFilters, useDefaultFilters:catArgs.useDefaultFilters,
             filterTimestamp:nowTimestamp, locale:context.locale, userLogin:context.userLogin, timeZone:context.timeZone],
            -1, true); // SEPARATE TRANSACTION so error doesn't crash screen
        if (!ServiceUtil.isSuccess(result)) {
            throw new Exception("Error in solrSideDeepCategory: " + ServiceUtil.getErrorMessage(result));
        }
        catLevel = result.categories;
    } catch(Exception e) {
        Debug.logError(e, e.getMessage(), module);
    }
}

// SCIPIO: promo category (added for testing purposes; uncomment line below to remove again)
promoCategoryId = CatalogWorker.getCatalogPromotionsCategoryId(request, currentCatalogId);
//promoCategoryId = null;

// SCIPIO: best-sell category (added for testing purposes; uncomment line below to remove again)
bestSellCategoryId = CatalogWorker.getCatalogBestSellCategoryId(request, currentCatalogId);
//bestSellCategoryId = null;

currentCategoryPath = null;
if (curCategoryId) {
    currentCategoryPath = SolrCategoryUtil.getCategoryNameWithTrail(curCategoryId, currentCatalogId, false, 
        dispatcher.getDispatchContext(), currentTrail);
}
context.currentCategoryPath = currentCategoryPath;
infoMap.currentCategoryPath = currentCategoryPath;

Debug.logInfo("Current category: " + infoMap, module);
if (DEBUG) {
    Debug.logInfo("Side deep categories: " + catLevel, module);
}

context.catList = catLevel;
topLevelList = [topCategoryId];
if (promoCategoryId) {
    // SCIPIO: Adding best-sell to top-levels for testing
    topLevelList.add(promoCategoryId);
}
if (bestSellCategoryId) {
    topLevelList.add(bestSellCategoryId);
}
context.topLevelList = topLevelList;
context.curCategoryId = curCategoryId;
context.topCategoryId = topCategoryId;

context.promoCategoryId = promoCategoryId;
context.bestSellCategoryId = bestSellCategoryId;

// SCIPIO: if multiple top categories, need to record the current base category
if (topLevelList.size() >= 2) {
    baseCategoryId = topCategoryId; // default if somehow none found
    if (currentCategoryPath) {
        currentPathRoot = currentCategoryPath.split("/")[0];
        for(catId in topLevelList) {
            if (catId == currentPathRoot) {
                baseCategoryId = catId;
                break;
            }
        }
    }
} else if (topLevelList.size() >= 1) {
    baseCategoryId = topLevelList[0];
} else {
    baseCategoryId = null;
}
context.baseCategoryId = baseCategoryId;

context.catHelper = com.ilscipio.scipio.shop.category.CategoryHelper.newInstance(context);


