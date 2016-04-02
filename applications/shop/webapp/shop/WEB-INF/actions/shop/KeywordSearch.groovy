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

// Cato: NOTE: This script is responsible for checking whether solr is applicable.

module = "KeywordSearch.groovy";


if (!parameters.SEARCH_STRING)
    parameters.SEARCH_STRING = "*:*";

if (parameters.SEARCH_FILTER && !parameters.SEARCH_FILTER.contains("isVirtual"))
    parameters.SEARCH_FILTER += " -isVirtual:true";
else if (!parameters.SEARCH_FILTER)
    parameters.SEARCH_FILTER = " -isVirtual:true";

storeCatalogs = CatalogWorker.getStoreCatalogs(request);

catalogFilter = "";
for (catalog in storeCatalogs)
    catalogFilter += " catalog:" + catalog.prodCatalogId;
parameters.SEARCH_FILTER += catalogFilter;

if (context.viewSize)
    parameters.VIEW_SIZE = context.viewSize;
if (context.viewIndex)
    parameters.VIEW_INDEX = context.viewIndex;
result = dispatcher.runSync("solrKeywordSearch",[query:parameters.SEARCH_STRING,queryFilter:parameters.SEARCH_FILTER,viewSize:parameters.VIEW_SIZE, viewIndex:parameters.VIEW_INDEX]);
/*Debug.logInfo("query:"+parameters.SEARCH_STRING
            +" queryFilter:"+parameters.SEARCH_FILTER
            +" viewSize:"+parameters.VIEW_SIZE
            +" viewIndex:"+parameters.VIEW_INDEX
            ,"");*/
//Debug.logInfo("Result: "+result,"");

context.isCorrectlySpelled = result.isCorrectlySpelled;
context.facetQueries = result.facetQueries;
context.products = result.results;
context.listSize = result.listSize;
context.viewIndex = result.viewIndex;
context.viewSize = result.viewSize;
Debug.log("listSize ====> " + context.listSize + " viewIndex =====> " + context.viewIndex + " viewSize ======> " + context.viewSize);
if (result.viewSize > 0)
    context.listIndex = Math.ceil(result.listSize/result.viewSize);
if (!parameters.VIEW_SIZE.equals(String.valueOf(context.viewSize))) {
    pageViewSize = Integer.parseInt(parameters.VIEW_SIZE).intValue();
    context.listIndex = Math.ceil(result.listSize/pageViewSize);
    context.pageViewSize = pageViewSize;
}

context.suggestions = result.suggestions;
context.searchConstraintStrings = [];
context.currentSearch = parameters.SEARCH_STRING;
context.currentFilter = parameters.SEARCH_FILTER;


if (!parameters.CURR_INDEX)
    context.currIndex = 1;
else
    context.currIndex = Integer.parseInt(parameters.CURR_INDEX).intValue();    


categoriesTrail = FastMap.newInstance();
for (facetField in result.facetFields.keySet())
    if (facetField.equals("cat"))
        categoriesTrail = result.facetFields.get(facetField);
context.filterCategories = FastMap.newInstance();
for (categoryTrail in categoriesTrail.keySet()) {
    if (categoryTrail.split("/").length > 0) {
        productCategory = delegator.findByPrimaryKeyCache("ProductCategory", UtilMisc.toMap("productCategoryId", categoryTrail.split("/")[categoryTrail.split("/").length - 1]))
        context.filterCategories.put(productCategory, UtilMisc.toMap(categoryTrail, categoriesTrail.get(categoryTrail)));
//          Debug.log("filterCategory " + categoryTrail.split("/")[categoryTrail.split("/").length - 1]);
    }
}
