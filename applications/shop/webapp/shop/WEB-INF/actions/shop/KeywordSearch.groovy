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
import org.ofbiz.product.catalog.*;
import org.ofbiz.product.feature.*;
import org.ofbiz.product.product.*;
import org.ofbiz.product.category.CategoryWorker;
import javolution.util.FastList;
import javolution.util.FastMap;

// SCIPIO: NOTE: This script is responsible for checking whether solr is applicable (if no check, implies the shop assumes solr is always enabled).

module = "KeywordSearch.groovy";

// SCIPIO: this allows to use the script for local scopes without affecting request
localVarsOnly = context.localVarsOnly;
if (localVarsOnly == null) {
    localVarsOnly = false;
}
context.remove("localVarsOnly");

searchString = context.searchString;
searchFilter = context.searchFilter;
viewSize = context.viewSize;
viewIndex = context.viewIndex;
currIndex = context.currIndex;
if (!localVarsOnly) {
    if (!searchString) {
        searchString = parameters.SEARCH_STRING;
    }
    if (!searchFilter) {
        searchFilter = parameters.SEARCH_FILTER;
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
    
    
if (!searchString)
    searchString = "*:*";
    
if (searchFilter && !searchFilter.contains("isVirtual"))
    searchFilter += " -isVirtual:true";
else if (!searchFilter)
    searchFilter = " -isVirtual:true";


storeCatalogs = CatalogWorker.getStoreCatalogs(request);

catalogFilter = "";
for (catalog in storeCatalogs)
    catalogFilter += " catalog:" + catalog.prodCatalogId;
searchFilter += catalogFilter;

result = dispatcher.runSync("solrKeywordSearch",[query:searchString,queryFilter:searchFilter,viewSize:viewSize, viewIndex:viewIndex]);
/*Debug.logInfo("query:"+searchString
            +" queryFilter:"+searchFilter
            +" viewSize:"+viewSize
            +" viewIndex:"+viewIndex
            ,"");*/
//Debug.logInfo("Result: "+result,"");

context.listIndex = 0;
if (result.viewSize > 0)
    context.listIndex = Math.ceil(result.listSize/result.viewSize);
// SCIPIO: this may not make sense anymore since SOLR patches
//if (!viewSize.equals(String.valueOf(result.viewSize))) {
//    pageViewSize = Integer.parseInt(viewSize).intValue();
//    context.listIndex = Math.ceil(result.listSize/pageViewSize);
//    context.pageViewSize = pageViewSize;
//}

context.isCorrectlySpelled = result.isCorrectlySpelled;
context.facetQueries = result.facetQueries;
context.products = result.results;
context.listSize = result.listSize;
context.viewIndex = result.viewIndex;
context.viewSize = result.viewSize;

context.suggestions = result.suggestions;
context.searchConstraintStrings = [];
context.currentSearch = searchString;
context.currentFilter = searchFilter;

if (!currIndex)
    context.currIndex = 1;
else
    context.currIndex = Integer.parseInt(currIndex).intValue();    

categoriesTrail = FastMap.newInstance();
for (facetField in result.facetFields.keySet())
    if (facetField.equals("cat"))
        categoriesTrail = result.facetFields.get(facetField);
context.filterCategories = FastMap.newInstance();
for (categoryTrail in categoriesTrail.keySet()) {
    if (categoryTrail.split("/").length > 0) {
        productCategory = delegator.findOne("ProductCategory", UtilMisc.toMap("productCategoryId", categoryTrail.split("/")[categoryTrail.split("/").length - 1]), true)
        context.filterCategories.put(productCategory, UtilMisc.toMap(categoryTrail, categoriesTrail.get(categoryTrail)));
//          Debug.log("filterCategory " + categoryTrail.split("/")[categoryTrail.split("/").length - 1]);
    }
}

/* SCIPIO: do NOT do this from here (not needed and may cause issues)
parameters.VIEW_SIZE = viewSize;
parameters.VIEW_INDEX = viewIndex;
parameters.SEARCH_STRING = searchString;
parameters.SEARCH_FILTER = searchFilter;
parameters.CURR_INDEX = currIndex;
*/
