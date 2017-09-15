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


import org.ofbiz.base.util.*;
import org.ofbiz.product.catalog.*;
import org.ofbiz.product.category.*;
import org.ofbiz.service.*;
import com.ilscipio.scipio.solr.*;
import org.apache.solr.client.solrj.*;
import org.apache.solr.client.solrj.response.*;
import org.apache.commons.lang.StringUtils;

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

infoMap = [currentTrail:currentTrail, curCategoryId:curCategoryId];

if (DEBUG) Debug.logVerbose("Category (pre-resolve): " + infoMap, module);

catLevel = null; // use null here, not empty list
if (curCategoryId) {
    try {
        // TODO?: cache results?
        result = dispatcher.runSync("solrSideDeepCategory",
            [productCategoryId:curCategoryId, catalogId:currentCatalogId, 
             currentTrail:currentTrail, 
             locale:context.locale, userLogin:context.userLogin, timeZone:context.timeZone],
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


