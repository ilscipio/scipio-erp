<#-- SCIPIO: common macros for catalog tree and forms -->

<#include "component://content/webapp/content/common/common.ftl"><#-- @stdLocField -->
<#include "component://product/webapp/catalog/catalog/catalogcommon.ftl">
<#import "component://common/webcommon/includes/listLocalesMacros.ftl" as listLocaleMacros>

<#macro ectMarkupOut dir args={}>
  <#if dir?is_directive>
    <@dir args=args/><#t/>
  <#else>
    ${dir}<#t/>
  </#if>
</#macro>

<#-- fields needed for every form submitted back to the catalog tree
    initialValues are required only if form is submitted NOT by the tree, otherwise tree automatically fills them. -->
<#macro ectCommonTreeFormFields params={} initialValues={}>
    <#-- ectTargetNodePath implements the node pre-selection; maintains the selection event if event error -->
    <@field type="hidden" name="ectTargetNodePath" value=(initialValues.ectTargetNodePath!) class="+ect-inputfield"/>
    <#-- ectNewTargetNodePath is used instead of ectTargetNodePath when event succeeds -->
    <@field type="hidden" name="ectNewTargetNodePath" value=(initialValues.ectNewTargetNodePath!) class="+ect-inputfield"/>
    <#-- id of the submitted form -->
    <@field type="hidden" name="ectSubmittedFormId" value=(initialValues.ectSubmittedFormId!) class="+ect-inputfield"/>
</#macro>

<#assign ectLocFieldLabelMap = {
    "productName":uiLabelMap.ProductProductName
}>
<#function ectGetLocFieldLabel fieldName typeName>
    <#return ectLocFieldLabelMap[fieldName]!uiLabelMap["FormFieldTitle_"+fieldName]>
</#function>

<#-- Creates the initial localized fields for ProductContent/ProductCategoryContent ALTERNATE_LOCALE fields
    at initial load (event error) and when js not available, and writes out the markup template used
    by the tree to load localized fields from tree data.
    See corresponding js in ScpCatalogCommon.js StcLocFieldHandler.rebuildLocalizedFieldEntries.
    DEV NOTE: PLEASE KEEP BOTH IMPL IN SYNC. -->
<#macro ectLocalizedFields objectType params={} onAddClick="" parsedParamName="simpleTextViewsByType" 
    paramNamePrefix="contentFields_" localeArgs={} extraArgs...>
    
    <#-- FIXME -->
    <@alert type="warning">WARNING: <strong>LOCALIZED FIELDS ARE NOT YET SAVED ON SUBMIT</strong> (2017-10-27)</@alert>
 
  <#local fieldInfo = (ectObjectLocalizedFields[objectType])!>
  <#if fieldInfo?has_content>
    <#-- pre-read the params, otherwise @stcLocField will reparse them several times. -->
    <#local valueListsByType = getStcLocFieldParsedParams(params, parsedParamName, paramNamePrefix)!{}>
    <#list fieldInfo.typeNames as typeName>
      <#local fieldName = fieldInfo.fieldNames[typeName?index]>
      <#local typeInfo = (fieldInfo.typeInfo[rawString(typeName)])!{}>
      <#local inputType = "input">
      <#if (typeInfo.isLong)!false == true>
        <#local inputType = "textarea">
      </#if>
      <#-- @stcLocField from content common.ftl -->
      <@stcLocField values=valueListsByType inputType=inputType typeName=typeName entityFieldName=fieldName paramNamePrefix=paramNamePrefix params=params 
        label=ectGetLocFieldLabel(fieldName, typeName) tooltip="" localeArgs=localeArgs/>
    </#list>
  </#if>
</#macro>
