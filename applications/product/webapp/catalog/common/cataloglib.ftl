<#-- SCIPIO: Common Catalog utilities and definitions library
    May be imported by other applications' templates, preferably using:
      <#import "component://content/webapp/content/common/contentlib.ftl" as contentlib>
    NOTE: For this application's own templates, please include common.ftl instead (which includes this). -->

<#import "component://content/webapp/content/common/contentlib.ftl" as contentlib><#-- @stdLocField dependency -->

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

<#-- Catalog category/product simple text content localized fields
    Composed using @stcLocField.
    These were written to submit to the services (or any similar wrapper services):
    * replaceProductCategoryContentLocalizedSimpleTexts
    * replaceProductContentLocalizedSimpleTexts
-->
<#macro catalogStcLocFields objectType fieldsInfoMap=false values=false params={} onAddClick="" 
    parsedParamName="contentFieldsByType" paramNamePrefix="contentFields_" localeOpts={} extraArgs...>
  <#local fieldsInfoMap = getCatalogLocFieldsInfo(fieldsInfoMap)>
  <#local fieldsInfo = (fieldsInfoMap[objectType])!>
  <#if fieldsInfo?has_content>
    <#if (fieldsInfoMap.general.localeOpts)??>
      <#local localeOpts = toSimpleMap(fieldsInfoMap.general.localeOpts) + localeOpts>
    </#if>
    <#if values?is_boolean>
      <#-- pre-read the params, otherwise @stcLocField will reparse them several times. -->
      <#local values = contentlib.getStcLocFieldParsedParams(params, parsedParamName, paramNamePrefix)!{}>
    </#if>
    <#list fieldsInfo.typeNames as typeName>
      <#local fieldName = fieldsInfo.fieldNames[typeName?index]>
      <#local typeInfoMap = (fieldsInfo.typeInfoMap[rawString(typeName)])!{}>
      <#local inputType = "input">
      <#if "LONG_TEXT" == (typeInfoMap.inputType)!>
        <#local inputType = "textarea">
      </#if>
      <#local mainTooltip = "">
      <#local altTooltip = "">
      <#if fieldsInfo.typeField??>
        <#local tooltipVars = {"typeField":fieldsInfo.typeField, "typeName":typeName}>
        <#local mainTooltip = getLabel("ContentMainStcLocFieldInfo", "ContentUiLabels", tooltipVars)!>
        <#local altTooltip = getLabel("ContentAltStcLocFieldInfo", "ContentUiLabels", tooltipVars)!>
      </#if>
      <#-- @stcLocField from content common.ftl -->
      <@contentlib.stcLocField values=values inputType=inputType typeName=typeName entityFieldName=fieldName paramNamePrefix=paramNamePrefix params=params 
        label=getCatalogLocFieldLabel(typeName, fieldName, typeInfoMap) tooltip="" mainTooltip=mainTooltip altTooltip=altTooltip localeOpts=localeOpts/>
    </#list>
  </#if>
</#macro>

<#-- Gets passed map, or catalogLocFieldsInfo in main/global namespace if not set, or get from groovy script as fallback/default -->
<#function getCatalogLocFieldsInfo fieldsInfoMap=false>
  <#if fieldsInfoMap?is_hash_ex>
    <#return toSimpleMap(fieldsInfoMap)>
  <#elseif catalogLocFieldsInfo?is_hash_ex><#-- common variable name -->
    <#return toSimpleMap(catalogLocFieldsInfo)>
  <#else>
    <#local dummy = Static["org.ofbiz.base.util.GroovyUtil"].runScriptAtLocation("component://product/webapp/catalog/WEB-INF/actions/catalog/GetCatalogLocFieldsInfo.groovy", null, context)!>
    <#return toSimpleMap(context.catalogLocFieldsInfo!{})>
  </#if>
</#function>

<#function getCatalogLocFieldLabel typeName fieldName typeInfoMap>
  <#return uiLabelMap[(typeInfoMap.label)!("FormFieldTitle_"+fieldName)]>
</#function>

