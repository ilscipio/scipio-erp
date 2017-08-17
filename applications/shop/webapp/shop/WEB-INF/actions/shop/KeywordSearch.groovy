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
 * <p>
 * 2017-08-16: Script is revamped for fixes + field compatibility with old ofbiz KeywordSearch.groovy +
 * to avoid breaking compatibility with keywordsearch.ftl or other templates that might be using this.
 */

import org.ofbiz.base.util.*;
import org.ofbiz.service.ServiceUtil;
import org.ofbiz.product.catalog.*;
import org.ofbiz.product.feature.*;
import org.ofbiz.product.product.*;
import org.ofbiz.product.category.CategoryWorker;

// SCIPIO: NOTE: This script is responsible for checking whether solr is applicable (if no check, implies the shop assumes solr is always enabled).

final module = "KeywordSearch.groovy";

// SCIPIO: this allows to use the script for local scopes without affecting request
localVarsOnly = context.localVarsOnly;
if (localVarsOnly == null) localVarsOnly = false;
context.remove("localVarsOnly");

searchString = context.searchString;
searchFilter = context.searchFilter;
viewSize = context.viewSize;
viewIndex = context.viewIndex;
currIndex = context.currIndex; // TODO: REVIEW: why do we need a currIndex? isn't it same as viewIndex here?
// SCIPIO: NOTE: in the original ProductSearchSession code, there was a disconnect between the paging
// flag and the actual result paging; here we actually will honor the paging flag (if specified)
paging = context.paging; // NOTE: this is a Y/N string indicator
noConditionFind = context.noConditionFind;
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
    if (!paging) {
        paging = parameters.PAGING;
    }
    if (!noConditionFind) {
        noConditionFind = parameters.noConditionFind;
    }
}
// this map used in log, so admin can debug
searchParamMap = [:];
searchParamMap.searchString = searchString;
searchParamMap.searchFilter = searchFilter;
searchParamMap.viewSize = viewSize;
searchParamMap.viewIndex = viewIndex;
searchParamMap.paging = paging;

// NOTE: these context assigns are here in case of fail
// FIXME?: this message assumes searchString is always for "keywords"... the old ProductSearchSession had more options/params
context.searchSortOrderString = new org.ofbiz.product.product.ProductSearch.SortKeywordRelevancy().prettyPrintSortOrder(false, context.locale);
context.paging = paging; // in case fail

if ("N".equals(paging)) {
    // NOTE: null is for the service; these will be set to 0 upon return (for the template)
    viewSize = null;
    viewIndex = null;
    currIndex = null;
} else {
    if (viewSize == null || viewSize.toString().isEmpty()) viewSize = UtilProperties.getPropertyAsInteger("general.properties", "record.paginate.defaultViewSize", 20);
    if (viewIndex == null || viewIndex.toString().isEmpty()) viewIndex = 0;
    if (currIndex == null || currIndex.toString().isEmpty()) currIndex = 1; // TODO: REVIEW: why 1??
}

if ("Y".equals(noConditionFind) || searchString) {
try {
    if (!searchString) searchString = "*:*";
        
    if (searchFilter && !searchFilter.contains("isVirtual")) searchFilter += " -isVirtual:true";
    else if (!searchFilter) searchFilter = " -isVirtual:true";
    
    storeCatalogs = CatalogWorker.getStoreCatalogs(request);
    
    catalogFilter = "";
    for (catalog in storeCatalogs)
        catalogFilter += " catalog:" + catalog.prodCatalogId;
    searchFilter += catalogFilter;

    // service requires these as string, but NOTE: it returns them as int
    if (viewSize != null) viewSize = viewSize.toString();
    if (viewIndex != null) viewIndex = viewIndex.toString();
    
    // map info for debug
    searchParamMap.searchString = searchString;
    searchParamMap.searchFilter = searchFilter;
    searchParamMap.viewSize = viewSize;
    searchParamMap.viewIndex = viewIndex;
    
    if (Debug.verboseOn()) Debug.logVerbose("Keyword search params: " + searchParamMap, module);
    if (Debug.infoOn()) Debug.logInfo("Keyword search params: " + searchParamMap, module);
    
    result = dispatcher.runSync("solrKeywordSearch", [
        query:searchString, queryFilter:searchFilter, 
        viewSize:viewSize, viewIndex:viewIndex, 
        locale:context.locale, userLogin:context.userLogin, timeZone:context.timeZone
    ]);
    if (ServiceUtil.isError(result)) {
        throw new Exception(ServiceUtil.getErrorMessage(result));
    }
    
    // SCIPIO: NOTE: the real paging mode depends on whether a viewSize was in fact used in the query.
    // unlike old ofbiz stock code, we are binding the query pagination to the pagination display.
    paging = (result.viewSize != null && result.viewSize > 0) ? "Y" : "N";
    
    context.listIndex = 0;
    if (result.viewSize != null && result.viewSize > 0)
        context.listIndex = Math.ceil(result.listSize/result.viewSize);
    // SCIPIO: this may not make sense anymore since SOLR patches
    //if (!viewSize.equals(String.valueOf(result.viewSize))) {
    //    pageViewSize = Integer.parseInt(viewSize).intValue();
    //    context.listIndex = Math.ceil(result.listSize/pageViewSize);
    //    context.pageViewSize = pageViewSize;
    //}
    
    context.isCorrectlySpelled = result.isCorrectlySpelled;
    context.facetQueries = result.facetQueries;
    context.solrProducts = result.results; // new 2017-08-16: prefer this field name
    context.products = result.results; // LEGACY solr script field; for new code, solrProducts should be used, to disambiguate type
    context.listSize = result.listSize;
    context.viewIndex = result.viewIndex;
    context.viewSize = result.viewSize;
    
    context.suggestions = result.suggestions;
    context.searchConstraintStrings = [];
    context.currentSearch = searchString;
    context.currentFilter = searchFilter;
    
    // SCIPIO: 2017-08-16: the following context assignments are template compatibility/legacy fields - based on: 
    //   component://order/webapp/ordermgr/WEB-INF/actions/entry/catalog/KeywordSearch.groovy
    //<!-- LEGACY FIELDS
    productIds = [];
    if (context.products) {
        for(solrProduct in context.products) {
            productIds.add(solrProduct.productId);
        }
    }
    context.productIds = productIds;
    //context.viewIndex = result.viewIndex; // already set above
    //context.viewSize = result.viewSize; // already set above
    //context.listSize = result.listSize; // already set above
    // special non-null checks: just in case template depended on these being non-null (old code always had these non-null)
    if (context.viewIndex == null) context.viewIndex = 0;
    if (context.viewSize == null) context.viewSize = 0;
    if (context.listSize == null) context.listSize = 0;
    if (context.viewIndex != null && context.viewSize != null) {
        final viewIndexFirst = 0; // hopefully this will never change
        context.lowIndex = (context.viewIndex - viewIndexFirst) * context.viewSize;
        context.highIndex = ((context.viewIndex - viewIndexFirst) + 1) * context.viewSize;
    } else {
        context.lowIndex = 0;
        context.highIndex = 0;
    }
    context.paging = paging;
    context.previousViewSize = context.viewSize; // FIXME?: we don't currently record any "previous" view size...
    //context.searchConstraintStrings = ...; // already set above
    //context.searchSortOrderString = ...; // already set above
    context.noConditionFind = noConditionFind;
    //-->
    
    if (!currIndex) context.currIndex = 1; // TODO: REVIEW: why 1??
    else if (!(currIndex instanceof Integer)) context.currIndex = Integer.parseInt(currIndex).intValue();    
    
    categoriesTrail = [:];
    for (facetField in result.facetFields.keySet())
        if (facetField.equals("cat"))
            categoriesTrail = result.facetFields.get(facetField);
    context.filterCategories = [:];
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
} catch(Exception e) {
    Debug.logError(e, "Error running solrKeywordSearch or processing parameters or results: " + e.getMessage() 
        + "; search params: " + searchParamMap, module);
    errorMessageList = context.errorMessageList;
    if (errorMessageList == null) errorMessageList = [];
    errorMessageList.add(UtilProperties.getMessage("CommonErrorUiLabels", "CommonErrorOccurredContactSupport", context.locale));
    context.errorMessageList = errorMessageList;
}
}
