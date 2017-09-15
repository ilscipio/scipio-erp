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
<#include "component://shop/webapp/shop/catalog/catalogcommon.ftl">

<#assign searchOptionsHistoryList = Static["org.ofbiz.product.product.ProductSearchSession"].getSearchOptionsHistoryList(session)/>
<#assign currentCatalogId = Static["org.ofbiz.product.catalog.CatalogWorker"].getCurrentCatalogId(request)/>

<form name="advtokeywordsearchform" id="advtokeywordsearchform" method="post" action="<@ofbizUrl>keywordsearch</@ofbizUrl>">
  <#-- SCIPIO: don't hardcode these
  <input type="hidden" name="VIEW_SIZE" value="10"/>
  <input type="hidden" name="PAGING" value="Y"/>-->
  <input type="hidden" name="SEARCH_CATALOG_ID" value="${currentCatalogId}" />
  
  <#macro menuContent menuArgs={}>
      <@menu args=menuArgs>
          <@menuitem type="link" href=makeOfbizUrl("advancedsearch?resetSearch=true") text=uiLabelMap.CommonReset class="+${styles.action_nav!} ${styles.action_update!}"/>
      </@menu>
  </#macro>
  <@section title=uiLabelMap.ProductAdvancedSearch menuContent=menuContent><#-- uiLabelMap.ProductAdvancedSearchInCategory -->
  
    <#-- SCIPIO: DEV NOTE: the search supported multiple entries here, with naming SEARCH_STRINGx x=1..9;
        these could be added (js input [+] button), but they are not thoroughly tested with the new groovy code -->
    <@field type="generic" label=uiLabelMap.ProductKeywords>
        <@field type="text" name="SEARCH_STRING" size="32" value=(requestParameters.SEARCH_STRING!searchString!"") />

        <#-- SCIPIO: WARN: 2017-08-22: the "AND" option here may not be entirely reliable with Solr at this time;
            the parsing needs work; see com.ilscipio.scipio.solr.SolrUtil#addPrefixToAllKeywords -->
        <@field type="radio" name="SEARCH_OPERATOR" value="OR" checked=(searchOperator == "OR") label=uiLabelMap.CommonAny />
        <@field type="radio" name="SEARCH_OPERATOR" value="AND" checked=(searchOperator == "AND") label=uiLabelMap.CommonAll/>
    </@field>
  
    <#macro categorySelectOptions catList selCatId searchCatId indentPrefix="&nbsp;&nbsp;&nbsp;" depth=0 currPrefix="">
      <#list catList as catEntry>
        <#local cat = catEntry.value>
        <#local catName = getProductCategoryDisplayName(cat)>
        <#local selStr = "">
        <#local catId = rawString(cat.productCategoryId!)>
        <#if selCatId?seq_contains(catId)>
          <#assign selCatFound = true><#-- NOTE: this isn't very useful anymore, since this is now a list... -->
          <#local selStr = " selected=selected">
        </#if>
        <#if catId==searchCatId>
          <#assign searchCatFound = true>
        </#if>
        <option value="${escapeVal(cat.productCategoryId, 'html')}"${selStr}>${currPrefix}${escapeVal(catName, 'html')}</option>
        <#if catEntry.children?has_content>
          <@categorySelectOptions catList=catEntry.children selCatId=selCatId searchCatId=searchCatId indentPrefix=indentPrefix depth=(depth+1) currPrefix=(currPrefix+indentPrefix)/>
        </#if>
      </#list>
    </#macro>
  
    <@field type="select" name="SEARCH_CATEGORY_ID" label=uiLabelMap.ProductCategory multiple=true>
      <#if showSearchAnyCat><#-- DEV NOTE: see also "preferDefaultSearchCat" in CommonSearchOptions.groovy -->
        <@field type="option" value="" selected=(!searchCategoryIdSel?has_content)>${uiLabelMap.CommonAny}</@field>
      </#if>
        <#if searchCategoryIdSel?has_content>
          <#if searchCategoryIdSel?is_sequence>
            <#assign selCatId = rewrapObject(searchCategoryIdSel, 'raw-simple')>
          <#else>
            <#assign selCatId = [rawString(searchCategoryIdSel)]>
          </#if>
        <#else>
          <#assign selCatId = []>
        </#if>
        <#assign selCatFound = false>
        <#assign searchCatFound = false>
        <#assign catOptions>
          <@categorySelectOptions catList=((categoryTree.searchChildren)![]) selCatId=selCatId searchCatId=(searchCategoryId!)/>
          <@categorySelectOptions catList=((categoryTree.regChildren)![]) selCatId=selCatId searchCatId=(searchCategoryId!)/>
        </#assign>
      <#-- SCIPIO: FIXME: REMOVED: NOT PROPERLY CHECK FOR BELONGING TO CATALOG (SECURITY)
        <#if !searchCatFound && searchCategory?has_content>
          <@field type="option" value=(searchCategoryId!) selected=(rawString(searchCategoryIdSel!)==rawString(searchCategoryId!) || (!showSearchAnyCat && !searchCategoryIdSel?has_content))
            >${escapeVal(Static["org.ofbiz.product.category.CategoryContentWrapper"].getProductCategoryContentAsText(searchCategory, "CATEGORY_NAME", locale, dispatcher, "raw")!, 'html')}</@field>
        </#if>
      -->
        ${catOptions}
    </@field>
    <#-- SCIPIO: old code - forced a single category
    <input type="hidden" name="SEARCH_CATEGORY_ID" value="${searchCategoryId!}"/>
    <@field type="display" label=uiLabelMap.ProductCategory>
        ${(searchCategory.description)!}
    </@field>
    -->
    <@field type="generic" label=uiLabelMap.ProductIncludeSubCategories>
        <@field type="radio" name="SEARCH_SUB_CATEGORIES" value="Y" checked=searchIncludeSubCat label=uiLabelMap.CommonYes/>
        <@field type="radio" name="SEARCH_SUB_CATEGORIES" value="N" checked=(!searchIncludeSubCat) label=uiLabelMap.CommonNo/>
    </@field>
        
  <#-- TODO/FIXME: 2017-08-18: search can't currently honor this; should be fixed in near future...
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
  -->
    <@field type="generic" label=uiLabelMap.ProductSortedBy>
        <#-- SCIPIO: these are now hidden, using js to set from select -->
        <@field type="hidden" name="sortOrder" value=sortOrder/>
        <@field type="hidden" name="sortAscending" value=sortAscending?string("Y","N")/>
        
        <@field type="select" id="advsort-sortOrder-select"><#-- name="sortOrder" -->
            <#-- NOTE: @productSortOrderSelectXxx macros defined in catalogcommon.ftl -->
            <@productSortOrderSelectOptions sortOrder=sortOrder sortAscending=sortAscending/>
        </@field>
        <@productSortOrderSelectScript id="advsort-sortOrder-select" formId="advtokeywordsearchform" submitForm=false/>
      <#-- SCIPIO: now integrated into the drop-down options
        <@field type="radio" name="sortAscending" value="Y" checked=true label=uiLabelMap.EcommerceLowToHigh/>
        <@field type="radio" name="sortAscending" value="N" label=uiLabelMap.EcommerceHighToLow/>
      -->
    </@field>
    <#if searchConstraintStrings?has_content>
      <@field type="generic" label=uiLabelMap.ProductLastSearch>
            <#list searchConstraintStrings as searchConstraintString>
                <div>&nbsp;-&nbsp;${searchConstraintString}</div>
            </#list>
            
         <#-- SCIPIO: 2017-09-14: REMOVED this option - it works in completely counter-intuitive way
            <div>${uiLabelMap.ProductSortedBy}: ${searchSortOrderString}</div>
            <div>
              <@field type="radio" name="clearSearch" value="Y" checked=true label=uiLabelMap.ProductNewSearch/>
              <@field type="radio" name="clearSearch" value="N" label=uiLabelMap.CommonRefineSearch/>
            </div>
         -->
            <@field type="hidden" name="clearSearch" value="Y"/>
      </@field>
    </#if>
    <@field type="submit" submitType="link" href="javascript:document.advtokeywordsearchform.submit()" class="${styles.link_run_sys!} ${styles.action_find!}" text=uiLabelMap.CommonFind />

  </@section>

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
      <#-- 
        <#if searchOptions_has_next>
        </#if>
      -->
    </#list>
    </@section>
  </#if>

</form>
