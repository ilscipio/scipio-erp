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

<#if currentSearchCategory??>
  <@section id="layeredNav" title=uiLabelMap.EcommerceLayeredNavigation>
    <#escape x as x?xml>
      <#if productCategory.productCategoryId != currentSearchCategory.productCategoryId>
        <#assign currentSearchCategoryName = categoryContentWrapper.get("CATEGORY_NAME")! />
        <#list searchConstraintStrings as searchConstraintString>
          <#if (searchConstraintString?string?index_of(currentSearchCategoryName) >= 0)>
            <div id="searchConstraints">&nbsp;<a href="<@ofbizUrl>category/~category_id=${productCategoryId}?removeConstraint=${searchConstraintString_index}&amp;clearSearch=N<#if previousCategoryId??>&amp;searchCategoryId=${previousCategoryId}</#if></@ofbizUrl>" class="${styles.link_run_session!} ${styles.action_remove!}">X</a><#noescape>&nbsp;${searchConstraintString}</#noescape></div>
          </#if>
        </#list>
      </#if>
    </#escape>
    <#list searchConstraintStrings as searchConstraintString>
      <#if (searchConstraintString?string?index_of("Category: ") >= 0) && (searchConstraintString != "Exclude Variants")>
        <div id="searchConstraints">&nbsp;<a href="<@ofbizUrl>category/~category_id=${productCategoryId}?removeConstraint=${searchConstraintString_index}&amp;clearSearch=N<#if currentSearchCategory??>&amp;searchCategoryId=${currentSearchCategory.productCategoryId}</#if></@ofbizUrl>" class="${styles.link_run_session!} ${styles.action_remove!}">X</a>&nbsp;${searchConstraintString}</div>
      </#if>
    </#list>
    <#if showSubCats>
      <div id="searchFilter">
        <strong>${uiLabelMap.ProductCategories}</strong>
        <ul>
          <#list subCategoryList as category>
            <#assign subCategoryContentWrapper = category.categoryContentWrapper />
            <#assign categoryName = subCategoryContentWrapper.get("CATEGORY_NAME")! />
            <li><a href="<@ofbizUrl>category/~category_id=${productCategoryId}?SEARCH_CATEGORY_ID${index}=${category.productCategoryId}&amp;searchCategoryId=${category.productCategoryId}&amp;clearSearch=N</@ofbizUrl>">${categoryName!} (${category.count})</a></li>
          </#list>
        </ul>
      </div>
    </#if>
    <#if showColors>
      <div id="searchFilter">
        <strong>${colorFeatureType.description}</strong>
        <ul>
          <#list colors as color>
            <li><a href="<@ofbizUrl>category/~category_id=${productCategoryId}?pft_${color.productFeatureTypeId}=${color.productFeatureId}&amp;clearSearch=N<#if currentSearchCategory??>&amp;searchCategoryId=${currentSearchCategory.productCategoryId}</#if></@ofbizUrl>">${color.description} (${color.featureCount})</a></li>
          </#list>
        </ul>
      </div>
    </#if>
    <#if showPriceRange>
      <div id="searchFilter">
        <strong>${uiLabelMap.EcommercePriceRange}</strong>
        <ul>
          <#list priceRangeList as priceRange>
            <li><a href="<@ofbizUrl>category/~category_id=${productCategoryId}?LIST_PRICE_LOW=${priceRange.low}&amp;LIST_PRICE_HIGH=${priceRange.high}&amp;clearSearch=N<#if currentSearchCategory??>&amp;searchCategoryId=${currentSearchCategory.productCategoryId}</#if></@ofbizUrl>"><@ofbizCurrency amount=priceRange.low /> - <@ofbizCurrency amount=priceRange.high /> (${priceRange.count})</a><li>
          </#list>
        </ul>
      </div>
    </#if>
  </@section>
</#if>
