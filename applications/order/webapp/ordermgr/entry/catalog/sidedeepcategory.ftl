<#--
This file is subject to the terms and conditions defined in the
files 'LICENSE' and 'NOTICE', which are part of this source
code package.
-->
<#include "../ordercommon.ftl">

<#-- variable setup and worker calls -->
<#if (requestAttributes.topLevelList)??><#assign topLevelList = requestAttributes.topLevelList></#if>
<#if (requestAttributes.curCategoryId)??><#assign curCategoryId = requestAttributes.curCategoryId></#if>

<#-- looping macro -->
<#macro categoryList parentCategory category>
  <#-- jleroux: This whole block does not make sense to me --> 
  <#--if parentCategory.productCategoryId != category.productCategoryId>
    <#local pStr = "/~pcategory=" + parentCategory.productCategoryId>
  </#if-->
  <#-- SCIPIO: Must use context or accessor
  <#local shoppingCart = sessionAttributes.shoppingCart!>-->
  <#local shoppingCart = getShoppingCart()!>
  <#local purchaseOrder = (shoppingCart.isPurchaseOrder())!false>
  <#if curCategoryId?? && curCategoryId == category.productCategoryId>
    <div class="browsecategorytext">
     <#if catContentWrappers?? && catContentWrappers[category.productCategoryId]?? && catContentWrappers[category.productCategoryId].get("CATEGORY_NAME")?has_content>
       <#if purchaseOrder>
         <a href="<@ofbizUrl>keywordsearch/~SEARCH_CATEGORY_ID=${category.productCategoryId}/~SEARCH_SUPPLIER_ID=${shoppingCart.partyId!}/~category_id=${category.productCategoryId}${pStr!}</@ofbizUrl>" class="browsecategorybuttondisabled">${catContentWrappers[category.productCategoryId].get("CATEGORY_NAME")}</a>
       <#else>
         <a href="<@ofbizUrl>category?category_id=${category.productCategoryId}${pStr!}</@ofbizUrl>" class="browsecategorybuttondisabled">${catContentWrappers[category.productCategoryId].get("CATEGORY_NAME")}</a>
       </#if>
     <#elseif catContentWrappers?? && catContentWrappers[category.productCategoryId]?? && catContentWrappers[category.productCategoryId].get("DESCRIPTION")?has_content>
       <#if purchaseOrder>
         <a href="<@ofbizUrl>keywordsearch/~SEARCH_CATEGORY_ID=${category.productCategoryId}/~SEARCH_SUPPLIER_ID=${shoppingCart.partyId!}/~category_id=${category.productCategoryId}${pStr!}</@ofbizUrl>" class="browsecategorybuttondisabled">${catContentWrappers[category.productCategoryId].get("DESCRIPTION")}</a>
       <#else>
         <a href="<@ofbizUrl>category?category_id=${category.productCategoryId}${pStr!}</@ofbizUrl>" class="browsecategorybuttondisabled">${catContentWrappers[category.productCategoryId].get("DESCRIPTION")}</a>
       </#if>
     <#else>
      <#if purchaseOrder>
        <a href="<@ofbizUrl>keywordsearch/~SEARCH_CATEGORY_ID=${category.productCategoryId}/~SEARCH_SUPPLIER_ID=${shoppingCart.partyId!}/~category_id=${category.productCategoryId}${pStr!}</@ofbizUrl>" class="browsecategorybuttondisabled">${category.categoryName!}</a>
      <#else>
        <a href="<@ofbizUrl>category?category_id=${category.productCategoryId}${pStr!}</@ofbizUrl>" class="browsecategorybuttondisabled">${category.categoryName?default(category.description)?default(category.productCategoryId)}</a>
      </#if>
     </#if>
    </div>
  <#else>
    <div class="browsecategorytext">
     <#if catContentWrappers[category.productCategoryId].get("CATEGORY_NAME")?has_content>
      <#if purchaseOrder>
        <a href="<@ofbizUrl>keywordsearch/~SEARCH_CATEGORY_ID=${category.productCategoryId}/~SEARCH_SUPPLIER_ID=${shoppingCart.partyId!}/~category_id=${category.productCategoryId}${pStr!}</@ofbizUrl>" class="browsecategorybutton">${catContentWrappers[category.productCategoryId].get("CATEGORY_NAME")}</a>
      <#else>
        <a href="<@ofbizUrl>category?category_id=${category.productCategoryId}${pStr!}</@ofbizUrl>" class="browsecategorybutton">${catContentWrappers[category.productCategoryId].get("CATEGORY_NAME")}</a>
      </#if>
     <#elseif catContentWrappers[category.productCategoryId].get("DESCRIPTION")?has_content>
      <#if purchaseOrder>
        <a href="<@ofbizUrl>keywordsearch/~SEARCH_CATEGORY_ID=${category.productCategoryId}/~SEARCH_SUPPLIER_ID=${shoppingCart.partyId!}/~category_id=${category.productCategoryId}${pStr!}</@ofbizUrl>" class="browsecategorybutton">${catContentWrappers[category.productCategoryId].get("DESCRIPTION")}</a>
      <#else>
        <a href="<@ofbizUrl>category?category_id=${category.productCategoryId}${pStr!}</@ofbizUrl>" class="browsecategorybutton">${catContentWrappers[category.productCategoryId].get("DESCRIPTION")}</a>
      </#if>
     <#else>
      <#if purchaseOrder>
        <a href="<@ofbizUrl>keywordsearch/~SEARCH_CATEGORY_ID=${category.productCategoryId}/~SEARCH_SUPPLIER_ID=${shoppingCart.partyId!}/~category_id=${category.productCategoryId}${pStr!}</@ofbizUrl>" class="browsecategorybutton">${category.categoryName!}</a>
      <#else>
        <a href="<@ofbizUrl>category?category_id=${category.productCategoryId}${pStr!}</@ofbizUrl>" class="browsecategorybutton">${category.categoryName?default(category.description)?default(category.productCategoryId)}</a>
      </#if>
     </#if>
    </div>
  </#if>

  <#if (Static["org.ofbiz.product.category.CategoryWorker"].checkTrailItem(request, category.getString("productCategoryId"))) || (curCategoryId?? && curCategoryId == category.productCategoryId)>
    <#local subCatList = Static["org.ofbiz.product.category.CategoryWorker"].getRelatedCategoriesRet(request, "subCatList", category.getString("productCategoryId"), true)>
    <#if subCatList??>
      <#list subCatList as subCat>
        <div class="browsecategorylist">
          <@categoryList parentCategory=category category=subCat/>
        </div>
      </#list>
    </#if>
  </#if>
</#macro>

<#if topLevelList?has_content>
    <@section title=uiLabelMap.ProductBrowseCategories>
        <div class="browsecategorylist">
          <#list topLevelList as category>
            <@categoryList parentCategory=category category=category/>
          </#list>
        </div>
    </@section>
</#if>
