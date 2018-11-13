<#--
This file is subject to the terms and conditions defined in
 file 'LICENSE', which is part of this source code package.
-->
${pages.get("/entry/OrderEntryTabBar.ftl")}
<#if requestAttributes.product?has_content>
  ${pages.get(requestAttributes.detailTemplate)}
<#else>
  <#assign productId = requestAttributes.productId!>
  <@commonMsg type="error">${uiLabelMap.ProductProductNotFound} ${productId!}!</@commonMsg>
</#if>
