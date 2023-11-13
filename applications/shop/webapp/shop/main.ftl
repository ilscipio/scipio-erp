<#--
This file is subject to the terms and conditions defined in the
files 'LICENSE' and 'NOTICE', which are part of this source
code package.
-->

<#-- Render the category page -->
<#if requestAttributes.productCategoryId?has_content>
  <@render resource="component://shop/widget/CatalogScreens.xml#bestSellingCategory" />
  <@render resource="component://shop/widget/CatalogScreens.xml#category-include" />
  <#assign isMaileonComponentPresent = Static["org.ofbiz.base.component.ComponentConfig"].isComponentPresent("maileon")!false>
  <#if !isMaileonComponentPresent>
    <@maileonSubscribeForm />
  </#if>
<#else>
  <center><@heading>${uiLabelMap.EcommerceNoPROMOTIONCategory}</@heading></center>
</#if>
