<#--
This file is subject to the terms and conditions defined in the
files 'LICENSE' and 'NOTICE', which are part of this source
code package.
-->
<#if productExportList?has_content>
  <#list productExportList as productExportMap><#assign productCategoryCount=0/><#assign productFeatureCount=0/>
    ${productExportMap.productId}    <#if productExportMap.productFeatureCustom?has_content>${productExportMap.productFeatureCustom.description!}</#if>    <#list productExportMap.productCategories as productCategoryAndMember><#if productCategoryAndMember.categoryName?has_content><#if (productCategoryCount > 0)>,</#if>${productCategoryAndMember.categoryName}<#assign productCategoryCount=productCategoryCount + 1/></#if></#list>    <#list productExportMap.productFeatures as productFeatureAndAppl><#if productFeatureAndAppl.description?has_content><#if (productFeatureCount > 0)>,</#if>${productFeatureAndAppl.description}<#assign productFeatureCount=productFeatureCount + 1/></#if></#list>
  </#list>
<#else>
    ${uiLabelMap.ProductErrorNothingToExport}
</#if>  