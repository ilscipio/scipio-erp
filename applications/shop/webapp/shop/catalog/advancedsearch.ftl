<#--
Licensed to the Apache Software Foundation (ASF) under one
or more contributor license agreements.  See the NOTICE file
distributed with this work for additional information
regarding copyright ownership.  The ASF licenses this file
to you under the Apache License, Version 2.0 (the
"License"); you may not use this file except in compliance
with the License.  You may obtain a copy of the License at

http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing,
software distributed under the License is distributed on an
"AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
KIND, either express or implied.  See the License for the
specific language governing permissions and limitations
under the License.
-->
<#include "catalogcommon.ftl">

<#assign searchOptionsHistoryList = Static["org.ofbiz.product.product.ProductSearchSession"].getSearchOptionsHistoryList(session)/>
<#assign currentCatalogId = Static["org.ofbiz.product.catalog.CatalogWorker"].getCurrentCatalogId(request)/>
<@heading>${uiLabelMap.ProductAdvancedSearchInCategory}</@heading>
<form name="advtokeywordsearchform" method="post" action="<@ofbizUrl>keywordsearch</@ofbizUrl>">
  <#-- SCIPIO: don't hardcode these
  <input type="hidden" name="VIEW_SIZE" value="10"/>
  <input type="hidden" name="PAGING" value="Y"/>-->
  <@table type="fields">
    <input type="hidden" name="SEARCH_CATALOG_ID" value="${currentCatalogId}" />
    <#if searchCategory?has_content>
        <input type="hidden" name="SEARCH_CATEGORY_ID" value="${searchCategoryId!}"/>
        <@field type="display" label=uiLabelMap.ProductCategory>
            ${(searchCategory.description)!}
        </@field>
        <@field type="generic" label=uiLabelMap.ProductIncludeSubCategories>
            <@field type="radio" name="SEARCH_SUB_CATEGORIES" value="Y" checked=true label=uiLabelMap.CommonYes/>
            <@field type="radio" name="SEARCH_SUB_CATEGORIES" value="N" label=uiLabelMap.CommonNo/>
        </@field>
    </#if>
    <@field type="generic" label=uiLabelMap.ProductKeywords>
        <@field type="text" name="SEARCH_STRING" size="32" value=(requestParameters.SEARCH_STRING!"") />

        <@field type="radio" name="SEARCH_OPERATOR" value="OR" checked=(searchOperator == "OR") label=uiLabelMap.CommonAny />
        <@field type="radio" name="SEARCH_OPERATOR" value="AND" checked=(searchOperator == "AND") label=uiLabelMap.CommonAll/>
    </@field>
    <#list productFeatureTypeIdsOrdered as productFeatureTypeId>
      <#assign findPftMap = {"productFeatureTypeId":productFeatureTypeId}>
      <#assign productFeatureType = delegator.findOne("ProductFeatureType", findPftMap, true)>
      <#assign productFeatures = productFeaturesByTypeMap[productFeatureTypeId]>
      <@field type="select" label=((productFeatureType.get("description",locale))!) name="pft_${productFeatureTypeId}">
          <option value="">- ${uiLabelMap.CommonSelectAny} -</option>
        <#list productFeatures as productFeature>
          <option value="${productFeature.productFeatureId}">${productFeature.description!productFeature.productFeatureId}</option>
        </#list>
      </@field>
    </#list>
    <@field type="generic" label=uiLabelMap.ProductSortedBy>
          <@field type="select" name="sortOrder">
            <option value="SortKeywordRelevancy">${uiLabelMap.ProductKeywordRelevancy}</option>
            <option value="SortProductField:productName">${uiLabelMap.ProductProductName}</option>
            <option value="SortProductField:totalQuantityOrdered">${uiLabelMap.ProductPopularityByOrders}</option>
            <option value="SortProductField:totalTimesViewed">${uiLabelMap.ProductPopularityByViews}</option>
            <option value="SortProductField:averageCustomerRating">${uiLabelMap.ProductCustomerRating}</option>
            <option value="SortProductPrice:LIST_PRICE">${uiLabelMap.ProductListPrice}</option>
            <option value="SortProductPrice:DEFAULT_PRICE">${uiLabelMap.ProductDefaultPrice}</option>
            <#if productFeatureTypes?? && productFeatureTypes?has_content>
              <#list productFeatureTypes as productFeatureType>
                <option value="SortProductFeature:${productFeatureType.productFeatureTypeId}">${productFeatureType.description!productFeatureType.productFeatureTypeId}</option>
              </#list>
            </#if>
          </@field>
          <@field type="radio" name="sortAscending" value="Y" checked=true label=uiLabelMap.EcommerceLowToHigh/>
          <@field type="radio" name="sortAscending" value="N" label=uiLabelMap.EcommerceHighToLow/>
    </@field>
    <#if searchConstraintStrings?has_content>
      <@field type="generic" label=uiLabelMap.ProductLastSearch>
            <#list searchConstraintStrings as searchConstraintString>
                <div>&nbsp;-&nbsp;${searchConstraintString}</div>
            </#list>
            <div>${uiLabelMap.ProductSortedBy}: ${searchSortOrderString}</div>
            <div>
              <@field type="radio" name="clearSearch" value="Y" checked=true label=uiLabelMap.ProductNewSearch/>
              <@field type="radio" name="clearSearch" value="N" label=uiLabelMap.CommonRefineSearch/>
            </div>
      </@field>
    </#if>
    <@field type="submit" submitType="link" href="javascript:document.advtokeywordsearchform.submit()" class="${styles.link_run_sys!} ${styles.action_find!}" text=uiLabelMap.CommonFind />
  </@table>

  <#if searchOptionsHistoryList?has_content>
    
    <@section title="${rawLabel('OrderLastSearches')}...">
      <div>
        <a href="<@ofbizUrl>clearSearchOptionsHistoryList</@ofbizUrl>" class="${styles.link_run_session!} ${styles.action_clear!}">${uiLabelMap.OrderClearSearchHistory}</a>
        ${uiLabelMap.OrderClearSearchHistoryNote}
      </div>
    <#list searchOptionsHistoryList as searchOptions>
    <#-- searchOptions type is ProductSearchSession.ProductSearchOptions -->
        <div>
          ${uiLabelMap.EcommerceSearchNumber}${searchOptions_index + 1}
          <a href="<@ofbizUrl>setCurrentSearchFromHistoryAndSearch?searchHistoryIndex=${searchOptions_index}&amp;clearSearch=N</@ofbizUrl>" class="${styles.link_run_sys!} ${styles.action_find!}">${uiLabelMap.CommonSearch}</a>
          <a href="<@ofbizUrl>setCurrentSearchFromHistory?searchHistoryIndex=${searchOptions_index}</@ofbizUrl>" class="${styles.link_nav!} ${styles.action_find!}">${uiLabelMap.CommonRefine}</a>
        </div>
        <#assign constraintStrings = searchOptions.searchGetConstraintStrings(false, delegator, locale)>
        <#list constraintStrings as constraintString>
          <div>&nbsp;-&nbsp;${constraintString}</div>
        </#list>
        <#if searchOptions_has_next>
          
        </#if>
    </#list>
    </@section>
  </#if>
</form>
