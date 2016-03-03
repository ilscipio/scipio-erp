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
<#-- variable setup and worker calls -->
<#if (requestAttributes.topLevelList)??><#assign topLevelList = requestAttributes.topLevelList></#if>
<#if (requestAttributes.curCategoryId)??><#assign curCategoryId = requestAttributes.curCategoryId></#if>

<#-- looping macro -->
<#macro categoryList parentCategory category wrapInBox>
  <#if catContentWrappers?? && catContentWrappers[category.productCategoryId]?? && catContentWrappers[category.productCategoryId].get("CATEGORY_NAME", "html")??>
    <#assign categoryName = catContentWrappers[category.productCategoryId].get("CATEGORY_NAME", "html")>
  <#else>
    <#assign categoryName = category.categoryName!>
  </#if>
  <#if catContentWrappers?? && catContentWrappers[category.productCategoryId]?? && catContentWrappers[category.productCategoryId].get("DESCRIPTION", "html")??>
    <#assign categoryDescription = catContentWrappers[category.productCategoryId].get("DESCRIPTION", "html")>
  <#else>
    <#assign categoryDescription = category.description!>
  </#if>
  <#if curCategoryId?? && curCategoryId == category.productCategoryId>
    <#assign browseCategoryButtonClass = "browsecategorybuttondisabled">
  <#else>
    <#assign browseCategoryButtonClass = "browsecategorybutton">
  </#if>
  <#local listEntries>
        <li class="browsecategorytext">
          <#if parentCategory?has_content>
            <#assign parentCategoryId = parentCategory.productCategoryId/>
          <#else>
            <#assign parentCategoryId = ""/>
          </#if>
          <a href="<@ofbizCatalogAltUrl productCategoryId=category.productCategoryId previousCategoryId=parentCategoryId/>" class="${browseCategoryButtonClass}"><#if categoryName?has_content>${categoryName}<#else>${categoryDescription?default("")}</#if></a>

  <#if (Static["org.ofbiz.product.category.CategoryWorker"].checkTrailItem(request, category.getString("productCategoryId"))) || (curCategoryId?? && curCategoryId == category.productCategoryId)>
    <#local subCatList = Static["org.ofbiz.product.category.CategoryWorker"].getRelatedCategoriesRet(request, "subCatList", category.getString("productCategoryId"), true)>
    <#if subCatList??>
      <#list subCatList as subCat>
        <ul class="browsecategorylist">
          <@categoryList parentCategory=category category=subCat wrapInBox="N"/>
        </ul>
      </#list>
    </#if>
  </#if>
  </li>
  </#local>
  <#if wrapInBox == "Y">
    <#assign sectionTitle><#if categoryDescription?has_content>${categoryDescription}<#else>${categoryName?default("")}</#if></#assign>
    <@section title=sectionTitle id="sidedeepcategory" class="+screenlet">
      <ul class="browsecategorylist">
        ${listEntries}
      </ul>
    </@section>
  <#else>
    ${listEntries}
  </#if>
</#macro>

<#if topLevelList?has_content>
  <@section title=uiLabelMap.ProductBrowseCategories id="sidedeepcategory" class="+screenlet">
    <ul class="browsecategorylist">
      <#list topLevelList as category>
        <@categoryList parentCategory="" category=category wrapInBox="N"/>
      </#list>
    </ul>
  </@section>
</#if>
