<#--
This file is subject to the terms and conditions defined in
 file 'LICENSE', which is part of this source code package.
-->

<#-- Render the category page -->
<#if requestAttributes.productCategoryId?has_content>
  <@render resource="component://shop/widget/CatalogScreens.xml#bestSellingCategory" />
  <@render resource="component://shop/widget/CatalogScreens.xml#category-include" />
<#else>
  <center><@heading>${uiLabelMap.EcommerceNoPROMOTIONCategory}</@heading></center>
</#if>
