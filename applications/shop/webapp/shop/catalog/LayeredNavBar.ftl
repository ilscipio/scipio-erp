<#--
Scipio Commerce
Copyright (C) Ilscipio GmbH

This file is part of Scipio Commerce. Scipio Commerce is free software: you
can redistribute it and modify it under the terms of the GNU Affero General
Public License, version 3, as published by the Free Software Foundation.
Scipio Commerce is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License
for more details. You should have received a copy of the license with this
work (file LICENSE). If not, see <https://www.gnu.org/licenses/agpl-3.0.html>.
A commercial license is available from Ilscipio GmbH.

SPDX-License-Identifier: AGPL-3.0-only
-->
<#include "component://shop/webapp/shop/catalog/catalogcommon.ftl">

<#if currentSearchCategory??>
  <@section id="layeredNav" title=uiLabelMap.EcommerceLayeredNavigation>
    <#escape x as x?xml>
      <#if productCategory.productCategoryId != currentSearchCategory.productCategoryId>
        <#assign currentSearchCategoryName = categoryContentWrapper.get("CATEGORY_NAME")! />
        <#list searchConstraintStrings as searchConstraintString>
          <#if (searchConstraintString?string?index_of(currentSearchCategoryName) >= 0)>
            <div id="searchConstraints">&nbsp;<a href="<@pageUrl>category/~category_id=${productCategoryId}?removeConstraint=${searchConstraintString_index}&amp;clearSearch=N<#if previousCategoryId??>&amp;searchCategoryId=${previousCategoryId}</#if></@pageUrl>" class="${styles.link_run_session!} ${styles.action_remove!}">X</a><#noescape>&nbsp;${searchConstraintString}</#noescape></div>
          </#if>
        </#list>
      </#if>
    </#escape>
    <#list searchConstraintStrings as searchConstraintString>
      <#if (searchConstraintString?string?index_of("Category: ") >= 0) && (searchConstraintString != "Exclude Variants")>
        <div id="searchConstraints">&nbsp;<a href="<@pageUrl>category/~category_id=${productCategoryId}?removeConstraint=${searchConstraintString_index}&amp;clearSearch=N<#if currentSearchCategory??>&amp;searchCategoryId=${currentSearchCategory.productCategoryId}</#if></@pageUrl>" class="${styles.link_run_session!} ${styles.action_remove!}">X</a>&nbsp;${searchConstraintString}</div>
      </#if>
    </#list>
    <#if showSubCats>
      <div id="searchFilter">
        <strong>${uiLabelMap.ProductCategories}</strong>
        <ul>
          <#list subCategoryList as category>
            <#assign subCategoryContentWrapper = category.categoryContentWrapper />
            <#assign categoryName = subCategoryContentWrapper.get("CATEGORY_NAME")! />
            <li><a href="<@pageUrl>category/~category_id=${productCategoryId}?SEARCH_CATEGORY_ID${index}=${category.productCategoryId}&amp;searchCategoryId=${category.productCategoryId}&amp;clearSearch=N</@pageUrl>">${categoryName!} (${category.count})</a></li>
          </#list>
        </ul>
      </div>
    </#if>
    <#if showColors>
      <div id="searchFilter">
        <strong>${colorFeatureType.description}</strong>
        <ul>
          <#list colors as color>
            <li><a href="<@pageUrl>category/~category_id=${productCategoryId}?pft_${color.productFeatureTypeId}=${color.productFeatureId}&amp;clearSearch=N<#if currentSearchCategory??>&amp;searchCategoryId=${currentSearchCategory.productCategoryId}</#if></@pageUrl>">${color.description} (${color.featureCount})</a></li>
          </#list>
        </ul>
      </div>
    </#if>
    <#if showPriceRange>
      <div id="searchFilter">
        <strong>${uiLabelMap.EcommercePriceRange}</strong>
        <ul>
          <#list priceRangeList as priceRange>
            <li><a href="<@pageUrl>category/~category_id=${productCategoryId}?LIST_PRICE_LOW=${priceRange.low}&amp;LIST_PRICE_HIGH=${priceRange.high}&amp;clearSearch=N<#if currentSearchCategory??>&amp;searchCategoryId=${currentSearchCategory.productCategoryId}</#if></@pageUrl>"><@ofbizCurrency amount=priceRange.low /> - <@ofbizCurrency amount=priceRange.high /> (${priceRange.count})</a><li>
          </#list>
        </ul>
      </div>
    </#if>
  </@section>
</#if>
