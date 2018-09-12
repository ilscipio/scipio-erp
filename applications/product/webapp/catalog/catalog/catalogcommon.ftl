<#-- SCIPIO: Common catalog catalog macros and definitions -->

<#include "component://content/webapp/content/common/common.ftl"><#-- @stdLocField -->
<#include "component://product/webapp/catalog/common/common.ftl">

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
      <#local values = getStcLocFieldParsedParams(params, parsedParamName, paramNamePrefix)!{}>
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
      <@stcLocField values=values inputType=inputType typeName=typeName entityFieldName=fieldName paramNamePrefix=paramNamePrefix params=params 
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
