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
<#-- SCIPIO: 2017-08-23: based on component://order/webapp/ordermgr/entry/catalog/keywordsearch.ftl -->

<#include "component://shop/webapp/shop/catalog/catalogcommon.ftl">

<#macro searchCatNameList catIds>
  <#if !catIds?is_sequence>
    <#local catIds = [catIds]>
  </#if>
  <#list catIds as catId>
    ${escapeVal(getProductCategoryDisplayName(catId), 'html')}<#if catId?has_next>, </#if><#t/>
  </#list>
</#macro>

<#macro menuContent menuArgs={}>
  <@row>
    <@cell columns=8 medium=8 class="+${styles.text_left!}">
      <div class="kws-searchinfo"
        <#if !displaySearchString??>
          <#if !searchString!?trim?has_content || searchString == "*:*">
            <#assign displaySearchString = "(" + rawLabel('CommonAll') + ")">
          <#else>
            <#assign displaySearchString = searchString>
          </#if>
        </#if>
        <#-- this will be read from session, for now: ?SEARCH_CATEGORY_ID=${(requestParameters.SEARCH_CATEGORY_ID)!} -->
        <#-- FIXME: localization flawed -->
        <#if displaySearchString?has_content><label style="display:inline;">${escapeVal(rawLabel('ProductYouSearchedFor')?cap_first, 'html')}:</label> ${escapeVal(displaySearchString, 'html')} <#rt/>
            <#t/><#if searchCategoryIdEff?has_content> <label style="display:inline;">${escapeVal(rawLabel('CommonIn')?lower_case, 'html')}</label> <@searchCatNameList catIds=searchCategoryIdEff /></#if></#if>
            <#t/>(<a href="<@ofbizUrl>advancedsearch</@ofbizUrl>" class="${styles.action_nav!} ${styles.action_find!} kws-refinesearch-link" style="white-space:nowrap;">${uiLabelMap.CommonRefineSearch}</a>)
        <#if displaySearchString?has_content && fullSuggestions?has_content>
          <br/><span class="kws-suggestion"><em><label style="display:inline;">${uiLabelMap.ShopDidYouMean}:</label> <a href="javascript:jQuery('#kws-suggestion-form').submit();void(0);">${fullSuggestions[0]!}</a></em></span>
            <form method="post" action="<@ofbizUrl>keywordsearch</@ofbizUrl>" style="display:none;" id="kws-suggestion-form"">
                <#-- WARN: TODO: REIMPLEMENT: using clearSearch=N relies on session; this works for single tab,
                    but frustrates polyvalent web users; ideally should do it parameter-based (requires revisit advancedsearch at same time) -->
                <@field type="hidden" name="clearSearch" value="N"/>
                <#-- TODO: use parameters to reproduce query
                <@field type="hidden" name="clearSearch" value="Y"/>
                ...
                -->
                <#-- SCIPIO: WARN: 2017-09-14: replaceConstraints only partially implemented, see ProductSearchSession -->
                <@field type="hidden" name="replaceConstraints" value="Y"/>
        
                <@field type="hidden" name="SEARCH_STRING" value=(fullSuggestions[0]!)/>
                <@field type="hidden" name="SEARCH_OPERATOR" value=(searchOperator!)/>
            </form>
        </#if>
      </div>
    </@cell>
    <@cell columns=4 medium=4 class="+${styles.text_right!}">
        <#-- NOTE: @productSortOrderSelectXxx macros defined in catalogcommon.ftl -->
        <form method="post" action="<@ofbizUrl>keywordsearch</@ofbizUrl>" style="display:none;" id="kwssort-form"">
            <#-- WARN: TODO: REIMPLEMENT: using clearSearch=N relies on session; this works for single tab,
                but frustrates polyvalent web users; ideally should do it parameter-based (requires revisit advancedsearch at same time) -->
            <@field type="hidden" name="clearSearch" value="N"/>
            <#-- TODO: use parameters to reproduce query
            <@field type="hidden" name="clearSearch" value="Y"/>
            ...
            -->
    
            <@field type="hidden" name="sortOrder" value=(sortOrder!sortOrderDef)/>
            <@field type="hidden" name="sortAscending" value=(sortAscending!sortAscendingDef)?string("Y","N")/>
        </form>
      <div class="kwssort-sortOrder-select-wrapper">
        <label for="kwssort-sortOrder-select" style="display:inline;">${uiLabelMap.ProductSortedBy}:</label>
        <@field type="select" inline=true id="kwssort-sortOrder-select" style="display:inline;" label=uiLabelMap.ProductSortedBy>
            <@productSortOrderSelectOptions sortOrder=(sortOrder!sortOrderDef) sortAscending=(sortAscending!sortAscendingDef)/>
        </@field>
        <@productSortOrderSelectScript id="kwssort-sortOrder-select" formId="kwssort-form" submitForm=true/>
      </div>
    </@cell>
  </@row>
</#macro>
<#assign searchTitleMarkup>${uiLabelMap.ProductProductSearch}<#--<#if currentSearch?has_content>: ${currentSearch} </#if> (<a href="<@ofbizUrl>advancedsearch?SEARCH_CATEGORY_ID=${(requestParameters.SEARCH_CATEGORY_ID)!}</@ofbizUrl>" class="${styles.action_nav!} ${styles.action_find!}">${uiLabelMap.CommonRefineSearch}</a>)--></#assign>
<#-- DEV NOTE: could have used this to save space, but title becomes encumbered: menuLayoutTitle="inline-title" -->
<@section title=wrapAsRaw({'htmlmarkup':searchTitleMarkup, 'raw':rawLabel('ProductProductSearch')}) menuContent=menuContent>

<#-- SCIPIO: ToDo: reimplement refined search
<@row>
    <@cell>
    ${uiLabelMap.ProductYouSearchedFor}
    <#list searchConstraintStrings as searchConstraintString>
      <a href="<@ofbizUrl>keywordsearch?removeConstraint=${searchConstraintString_index}&amp;clearSearch=N</@ofbizUrl>" class="${styles.link_run_session!} ${styles.action_remove!}">X</a>&nbsp;${searchConstraintString}</li>
    </#list>
    </@cell>
</@row>-->

<#-- 
<@row>
    <@cell columns=6>
        ${uiLabelMap.CommonSortedBy}: ${searchSortOrderString} (<a href="<@ofbizUrl>advancedsearch?SEARCH_CATEGORY_ID=${(requestParameters.SEARCH_CATEGORY_ID)!}</@ofbizUrl>" class="${styles.action_nav!} ${styles.action_find!}">${uiLabelMap.CommonRefineSearch}</a>)
    </@cell>
    <@cell columns=6 class="+${styles.text_right!}">
        
    </@cell>
</@row>
-->

<#if !productIds?has_content>
  <@commonMsg type="result-norecord">${uiLabelMap.ProductNoResultsFound}.</@commonMsg>
<#else>
  <#-- SCIPIO: 2017-08-16: special pagination check. in legacy code this basically was always true. -->
  <#if !pagingEnabled?? || !pagingEnabled?is_boolean>
    <#if (paging!) == "Y">
      <#assign pagingEnabled = true>
    <#elseif (paging!) == "N">
      <#assign pagingEnabled = false>
    <#else>
      <#assign pagingEnabled = ((viewSize!1) > 0)><#-- (default is enabled) -->
    </#if>
  </#if>
  <@paginate enabled=pagingEnabled mode="content" url=makeOfbizUrl("keywordsearch") paramStr="~clearSearch=N" paramDelim="/" paramPrefix="~" viewSize=(viewSize!1) viewIndex=(viewIndex!0) listSize=(listSize!0)>
    <@grid columns=4>
        <#-- SCIPIO: DEV NOTE: this shouldn't try to pass solrProduct - we are relying on the entity Product for calculating prices -->
        <#list productIds as productId> 
            <#-- note that there is no boundary range because that is being done before the list is put in the content -->
            <li><@render resource=productsummaryScreen reqAttribs={"optProductId":productId, "listIndex":productId_index}/></li>
        </#list>
    </@grid>
  </@paginate>
</#if>

</@section>
