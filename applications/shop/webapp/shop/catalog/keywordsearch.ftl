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

<#-- FIXME?: this data prep is in ftl for compat reasons... move later -->
<#if !kwsParams??>
  <#assign kwsParams = Static["org.ofbiz.product.product.ProductSearchSession"].getProductSearchOptions(session)!>
</#if>
<#if !sortOrder?has_content>
  <#if kwsParams?has_content>
    <#assign sortOrder = rawString((kwsParams.getResultSortOrder().getOrderName())!"")>
    <#if sortOrder?has_content>
      <#assign sortOrder = "Sort" + sortOrder>
    </#if>
  </#if>
  <#if !sortOrder?has_content>
    <#assign sortOrder = "SortKeywordRelevancy">
  </#if>
</#if>
<#if !sortAscending?? || !sortAscending?is_boolean>
  <#if kwsParams?has_content>
    <#assign sortAscending = (kwsParams.getResultSortOrder().isAscending())>
  <#else>
    <#assign sortAscending = true>
  </#if>
</#if>
<#if !displaySearchString?has_content>
  <#-- TODO: REVIEW: this will likely show the solr query string - not the orig keyword string -
      but this is not all bad because it's informative... -->
  <#if currentSearch?? || searchString??><#-- FIXME: can't easily read this from kwsParams? -->
    <#assign displaySearchString = rawString(currentSearch!searchString!"")>
    <#if !displaySearchString?has_content || displaySearchString == "*:*">
      <#assign displaySearchString = "(" + rawLabel('CommonAll') + ")">
    </#if>
  <#else>
    <#assign displaySearchString = ""><#-- groovy didn't set, oh well -->
  </#if>
</#if>

<#macro productSortOrderSelect sortOrder sortAscending id="kwssort">
    <form method="post" action="<@ofbizUrl>keywordsearch</@ofbizUrl>" style="display:none;" id="${id}-form">
        <#-- WARN: using clearSearch=N relies on session -->
        <@field type="hidden" name="clearSearch" value="N"/>
        
        <#-- TODO: use parameters to reproduce query
        <@field type="hidden" name="clearSearch" value="Y"/>
        -->

        <@field type="hidden" name="sortOrder" value=sortOrder/>
        <@field type="hidden" name="sortAscending" value=sortAscending?string("Y","N")/>
    </form>
    <#-- SCIPIO: NOTE: see also advancedsearch.ftl sort options; may differ -->
    <label for="${id}-sortOrder-select" style="display:inline;">${uiLabelMap.ProductSortedBy}:</label>
    <@field type="select" inline=true id="${id}-sortOrder-select" style="display:inline;">
        <option value="SortKeywordRelevancy"<#if sortOrder == "SortKeywordRelevancy"> selected="selected"</#if>>${uiLabelMap.ProductKeywordRelevancy}</option>
        <option value="SortProductField:productName"<#if sortOrder == "SortProductField:productName"> selected="selected"</#if>>${uiLabelMap.ProductProductName}</option>
        <option value="SortProductPrice:LIST_PRICE#ASC"<#if sortOrder == "SortProductPrice:LIST_PRICE" && sortAscending> selected="selected"</#if>>${uiLabelMap.ProductListPrice}: ${uiLabelMap.EcommerceLowToHigh}</option>
        <option value="SortProductPrice:LIST_PRICE#DESC"<#if sortOrder == "SortProductPrice:LIST_PRICE" && !sortAscending> selected="selected"</#if>>${uiLabelMap.ProductListPrice}: ${uiLabelMap.EcommerceHighToLow}</option>
      <#if (showAdvFields!false) == true><#-- the list price should fall back on the default price -->
        <option value="SortProductPrice:DEFAULT_PRICE#ASC"<#if sortOrder == "SortProductPrice:DEFAULT_PRICE" && sortAscending> selected="selected"</#if>>${uiLabelMap.ProductDefaultPrice}: ${uiLabelMap.EcommerceLowToHigh}</option>
        <option value="SortProductPrice:DEFAULT_PRICE#DESC"<#if sortOrder == "SortProductPrice:DEFAULT_PRICE" && !sortAscending> selected="selected"</#if>>${uiLabelMap.ProductDefaultPrice}: ${uiLabelMap.EcommerceHighToLow}</option>
      </#if>
    </@field>
    
    <@script>
        jQuery(document).ready(function() {
            var endsWith = function(str, suffix) {
                return str.indexOf(suffix, str.length - suffix.length) !== -1;
            };
            var sortOrderChange = function(elem, submit) {
                elem = jQuery(elem);
                var sortOrder = elem.val();
                var asc = true;
                if (endsWith(sortOrder, "#ASC")) {
                    sortOrder = sortOrder.substring(0, sortOrder.length-4);
                } else if (endsWith(sortOrder, "#DESC")) {
                    sortOrder = sortOrder.substring(0, sortOrder.length-5);
                    asc = false;
                }
                var formElem = jQuery('#${id}-form');
                jQuery('input[name=sortOrder]', formElem).val(sortOrder);
                jQuery('input[name=sortAscending]', formElem).val(asc ? "Y" : "N");
                if (submit) {
                    formElem.submit();
                }
            };
            var sortOrderElem = jQuery('#${id}-sortOrder-select');
            sortOrderChange(sortOrderElem, false);
            sortOrderElem.change(function() { sortOrderChange(this, true); });
        });
    </@script>
</#macro>

<#macro menuContent menuArgs>
  <@row>
    <@cell columns=6 class="+${styles.text_left!}">
        <#if displaySearchString?has_content><label style="display:inline;">${escapeVal(rawLabel('ProductYouSearchedFor')?cap_first, 'html')}:</label> ${escapeVal(displaySearchString, 'html')} </#if>(<a href="<@ofbizUrl>advancedsearch?SEARCH_CATEGORY_ID=${(requestParameters.SEARCH_CATEGORY_ID)!}</@ofbizUrl>" class="${styles.action_nav!} ${styles.action_find!}">${uiLabelMap.CommonRefineSearch}</a>)
    </@cell>
    <@cell columns=6 class="+${styles.text_right!}">
        <@productSortOrderSelect sortOrder=sortOrder sortAscending=sortAscending id="kwssort"/>
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
