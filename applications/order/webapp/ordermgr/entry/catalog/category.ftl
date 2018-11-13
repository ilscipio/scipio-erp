<#--
This file is subject to the terms and conditions defined in
 file 'LICENSE', which is part of this source code package.
-->

${pages.get("/entry/OrderEntryTabBar.ftl")}
<#if productCategory?has_content>
  ${pages.get(detailTemplate)}
<#else>
  <@commonMsg type="error">${uiLabelMap.ProductCategoryNotFoundForCategoryID} ${requestParameters.category_id!}!</@commonMsg>
</#if>


