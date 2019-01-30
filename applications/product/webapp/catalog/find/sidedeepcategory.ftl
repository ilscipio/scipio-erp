<#--
This file is subject to the terms and conditions defined in the
files 'LICENSE' and 'NOTICE', which are part of this source
code package.
-->

<#-- looping macro -->
<#macro categoryList parentCategory category>
  <#if parentCategory.productCategoryId != category.productCategoryId>
    <#local pStr = "&amp;pcategory=" + parentCategory.productCategoryId>
  </#if>
  <#if curCategoryId?? && curCategoryId == category.productCategoryId>
    <div class="browsecategorytext">
        <#if catContentWrappers?? && catContentWrappers[category.productCategoryId]?? && catContentWrappers[category.productCategoryId].get("CATEGORY_NAME")?has_content>
          <a href="<@pageUrl>EditCategory?productCategoryId=${category.productCategoryId}${pStr!}</@pageUrl>" class="browsecategorybuttondisabled">${catContentWrappers[category.productCategoryId].get("CATEGORY_NAME")} [${category.productCategoryId}]</a>
        <#elseif catContentWrappers?? && catContentWrappers[category.productCategoryId]?? && catContentWrappers[category.productCategoryId].get("DESCRIPTION")?has_content>
          <a href="<@pageUrl>EditCategory?productCategoryId=${category.productCategoryId}${pStr!}</@pageUrl>" class="browsecategorybuttondisabled">${catContentWrappers[category.productCategoryId].get("DESCRIPTION")} [${category.productCategoryId}]</a>
        <#else>
          <a href="<@pageUrl>EditCategory?productCategoryId=${category.productCategoryId}${pStr!}</@pageUrl>" class="browsecategorybuttondisabled">${category.categoryName?default(category.description)!} [${category.productCategoryId}]</a>
       </#if>
    </div>
  <#else>
    <div class="browsecategorytext">
        <#if catContentWrappers?? && catContentWrappers[category.productCategoryId]?? && catContentWrappers[category.productCategoryId].get("CATEGORY_NAME")?has_content>
          <a href="<@pageUrl>EditCategory?productCategoryId=${category.productCategoryId}${pStr!}</@pageUrl>" class="browsecategorybutton">${catContentWrappers[category.productCategoryId].get("CATEGORY_NAME")} [${category.productCategoryId}]</a>
        <#elseif catContentWrappers?? && catContentWrappers[category.productCategoryId]?? && catContentWrappers[category.productCategoryId].get("DESCRIPTION")?has_content>
          <a href="<@pageUrl>EditCategory?productCategoryId=${category.productCategoryId}${pStr!}</@pageUrl>" class="browsecategorybutton">${catContentWrappers[category.productCategoryId].get("DESCRIPTION")} [${category.productCategoryId}]</a>
        <#else>
          <a href="<@pageUrl>EditCategory?productCategoryId=${category.productCategoryId}${pStr!}</@pageUrl>" class="browsecategorybutton">${category.categoryName?default(category.description)!} [${category.productCategoryId}]</a>
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

<div><a href="<@pageUrl>ChooseTopCategory</@pageUrl>" class="${styles.link_nav!} ${styles.action_update!} ${styles.action_scope_session!}">${uiLabelMap.ProductChooseTopCategory}</a></div>
<div class="browsecategorylist">
<#if currentTopCategory??>
  <#if curCategoryId?? && curCategoryId == currentTopCategory.productCategoryId>
    <div style="text-indent: -10px;"><b>${currentTopCategory.categoryName?default("No Name")} [${currentTopCategory.productCategoryId}]</b></div>
  <#else>
    <div class="browsecategorytext"><a href="<@pageUrl>EditCategory?productCategoryId=${currentTopCategory.productCategoryId}</@pageUrl>" class="browsecategorybutton">${currentTopCategory.categoryName?default(currentTopCategory.description)!} [${currentTopCategory.productCategoryId}]</a></div>
  </#if>
</#if>
  <div class="browsecategorylist">
    <#if topLevelList??>
      <#list topLevelList as category>
        <@categoryList parentCategory=category category=category/>
      </#list>
    </#if>
  </div>
</div>
