<#--
This file is subject to the terms and conditions defined in the
files 'LICENSE' and 'NOTICE', which are part of this source
code package.
-->
<div class="browsecategorylist">
  <#assign sortList = UtilMisc.toList("prodCatalogCategoryTypeId", "sequenceNum", "productCategoryId")>
  <#list prodCatalogs as prodCatalog>
  <#if curProdCatalogId?? && curProdCatalogId == prodCatalog.prodCatalogId>
    <#assign prodCatalogCategories = prodCatalog.getRelated("ProdCatalogCategory", null, sortList, true)>
    <div class="browsecategorytext"><a href="<@pageUrl>EditProdCatalog?prodCatalogId=${prodCatalog.prodCatalogId}</@pageUrl>" class="browsecategorybutton">${prodCatalog.catalogName!}</a></div>
      <div class="browsecategorylist">
        <#list prodCatalogCategories as prodCatalogCategory>
          <#assign productCategory = prodCatalogCategory.getRelatedOne("ProductCategory", true)>
          <div class="browsecategorytext"><a href="<@pageUrl>EditCategory?CATALOG_TOP_CATEGORY=${prodCatalogCategory.productCategoryId}&amp;productCategoryId=${prodCatalogCategory.productCategoryId}</@pageUrl>" class="browsecategorybutton">${(productCategory.categoryName)?default(productCategory.description)?default(productCategory.productCategoryId)}</a></div>
        </#list>
      </div>
  <#else>
    <div class="browsecategorytext"><a href="<@pageUrl>EditProdCatalog?prodCatalogId=${prodCatalog.prodCatalogId}</@pageUrl>" class="browsecategorybutton">${prodCatalog.catalogName!}</a></div>
  </#if>
  </#list>
</div>
