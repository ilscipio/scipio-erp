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
import javolution.util.FastMap;
import javolution.util.FastList;
import com.ilscipio.solr.SolrUtil;
import org.ofbiz.product.product.ProductContentWrapper;

// SCIPIO: NOTE: This script is responsible for checking whether solr is applicable.

module = "BreadcrumbsGroovy";
breadcrumbsList = FastList.newInstance();


try{
    currentTrail = org.ofbiz.product.category.CategoryWorker.getCategoryPathFromTrailAsList(request);
    
    currentCatalogId = CatalogWorker.getCurrentCatalogId(request);
    // SCIPIO: IMPORTANT: Check request attribs before parameters map
    curCategoryId = parameters.category_id ?: parameters.CATEGORY_ID ?: request.getAttribute("productCategoryId") ?: parameters.productCategoryId ?: "";
    curProductId = parameters.product_id ?: "" ?: parameters.PRODUCT_ID ?: "";
    if(UtilValidate.isEmpty(curCategoryId)){
        if (context.product) {
            curCategoryId = product.primaryProductCategoryId;
        }
    }
    
    topCategoryId = CatalogWorker.getCatalogTopCategoryId(request, currentCatalogId);
    productCategoryId = curCategoryId;
    
    validBreadcrumb = topCategoryId + "/";
    
    dctx = dispatcher.getDispatchContext();
    categoryPath = com.ilscipio.solr.CategoryUtil.getCategoryNameWithTrail(productCategoryId,currentCatalogId,dctx,currentTrail);
    breadcrumbs = categoryPath.split("/");
    for (breadcrumb in breadcrumbs) {
        if (!breadcrumb.equals(topCategoryId) && !breadcrumbsList.contains(breadcrumb))
            breadcrumbsList.add(breadcrumb);
        if (breadcrumb.equals(curCategoryId))
            break;
    }
    
    if(context.product){
        if(context.productContentWrapper == null){
        productContentWrapper = new ProductContentWrapper(product, request);
        context.productContentWrapper = productContentWrapper;
        }
    }
    
}catch(Exception e){
    // We are not in a store, so we continue with regular page based breadcrumbs
}
context.breadcrumbsList = breadcrumbsList;

/*
I think there is a conceptual mistake here. The breadcrumbs don't really care if another category exists or not, nor do they list EVERY category they have.
They are rather to be seen as a way of leading up to a certain directory

if (curCategoryId) {
    availableBreadcrumbsList = dispatcher.runSync("solrAvailableCategories",[productCategoryId:curCategoryId,productId:null,displayProducts:false,catalogId:currentCatalogId,currentTrail:currentTrail]);
    validBreadcrumb = curCategoryId;
} else if (curProductId) {
    availableBreadcrumbsList = dispatcher.runSync("solrAvailableCategories",[productCategoryId:null,productId:curProductId,displayProducts:false,catalogId:currentCatalogId,currentTrail:currentTrail]);
}


if (availableBreadcrumbsList) {
    breadcrumbsList = FastList.newInstance();
    for (availableBreadcrumbs in availableBreadcrumbsList.get("categories").keySet()) {
        breadcrumbs = availableBreadcrumbs.split("/");
        if (availableBreadcrumbs.contains(validBreadcrumb)) {
            for (breadcrumb in breadcrumbs) {
                if (!breadcrumb.equals(topCategoryId) && !breadcrumbsList.contains(breadcrumb))
                    breadcrumbsList.add(breadcrumb);
                if (breadcrumb.equals(curCategoryId))
                    break;
            }
        }
    }
    context.breadcrumbsList = breadcrumbsList;
}
*/
