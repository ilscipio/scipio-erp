<#--
This file is subject to the terms and conditions defined in
 file 'LICENSE', which is part of this source code package.
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
