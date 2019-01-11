<#-- SCIPIO: Common catalog macros and definitions -->

<#-- NOTE: Always use !"#" or other missing value operator when calling this -->
<#function makeProductShopPageUrl productId>
  <#local productPageUrl = getPropertyValue("catalog", "shop.default.link.product.prefix")!>
  <#if productPageUrl?has_content && productId?has_content>
    <#return makeOfbizInterWebappUrl(productPageUrl+(escapeVal(productId, 'url')))>
  </#if>
</#function>

<#-- NOTE: Always use !"#" or other missing value operator when calling this -->
<#function makeCategoryShopPageUrl categoryId>
  <#local categoryPageUrl = getPropertyValue("catalog", "shop.default.link.category.prefix")!>
  <#if categoryPageUrl?has_content && categoryId?has_content>
    <#return makeOfbizInterWebappUrl(categoryPageUrl+(escapeVal(categoryId, 'url')))>
  </#if>
</#function>

<#macro productShopPageUrlMenuItem productId text=uiLabelMap.CommonShopPage class="+${styles.action_nav!} ${styles.action_view!}"
    target="_blank">
  <#local productPageUrl = makeProductShopPageUrl(productId)>
  <#if productPageUrl?has_content>
    <@menuitem type="link" href=productPageUrl text=text class=class target=target/>
  </#if>
</#macro>

<#macro categoryShopPageUrlMenuItem categoryId text=uiLabelMap.CommonShopPage class="+${styles.action_nav!} ${styles.action_view!}"
    target="_blank">
  <#local categoryPageUrl = makeCategoryShopPageUrl(categoryId)>
  <#if categoryPageUrl?has_content>
    <@menuitem type="link" href=categoryPageUrl text=text class=class target=target/>
  </#if>
</#macro>
