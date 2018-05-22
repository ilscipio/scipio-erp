<#-- SCIPIO: Common content macros and definitions -->

<#import "component://common/webcommon/includes/listLocalesMacros.ftl" as listLocaleMacros>

<#-- Special localized text field macros
    Intended for ProductContent/ProductCategoryContent fields with ALTERNATE_LOCALE support,
    but this should support anything fitting that entity pattern. -->

<#-- Parses simple text content localized field initial value params for paramNamePrefix. 
    Returns a map of typeNames to entryDataLists, where each entryDataList is a list of maps with keys: localeString, textData. -->
<#function parseStcLocFieldParams params paramNamePrefix="" allowPreparsed=false>
  <#return toSimpleMap(Static["org.ofbiz.content.content.LocalizedContentWorker"].parseLocalizedSimpleTextContentFieldParams(params, paramNamePrefix, allowPreparsed)!{})>
</#function>

<#-- Retrieves the incoming initial values for localized field from params[parsedParamName], 
    or if not set, parses them from params for paramNamePrefix.
    Returns a map of typeNames to entryDataLists, where each entryDataList is a list of maps with keys: localeString, textData.
    Supports both pre-parse from groovy and standalone operation. 
    DEV NOTE: there should be no correctness/security issues with having the parsedParamName map in the parameters
    map, because user-supplied parameters are never stored there in Map form. -->
<#function getStcLocFieldParsedParams params parsedParamName="" paramNamePrefix="" allowPreparsed=false>
  <#if parsedParamName?has_content>
    <#local values = params[parsedParamName]!false>
    <#if isObjectType("map", values)>
      <#return toSimpleMap(values)>
    </#if>
  </#if>
  <#if paramNamePrefix?has_content>
    <#return parseStcLocFieldParams(params, paramNamePrefix, allowPreparsed)>
  </#if>
</#function>

<#function makeStcLocFieldNamePrefix paramNamePrefix typeName entityFieldName index>
    <#return rawString(Static["org.ofbiz.content.content.LocalizedContentWorker"].makeLocalizedSimpleTextContentFieldStringParamPrefix(paramNamePrefix, typeName, index))>
</#function>

<#-- Outer container markup for @stcLocField
    Includes a hidden entry markup template for javascript (there is some redundancy, but having it
    here simplifies code and is REQUIRED due to styling problems with @row/@cell when the template
    is stored elsewhere). -->
<#macro stcLocFieldContainer containerClass="" onAddClick="" wrapperArgs={} entryTmpl=false mainEntryArgs={} altEntryArgs={} footTmpl="" footArgs={} extraArgs...>
    <#local containerClass = addClassArg(containerClass, "+stc-locfield")>
    <@field type="generic" containerClass=containerClass args=wrapperArgs><#t/>
      <div class="stc-locfield-entries"><#nested></div><#t/>
      <div class="stc-locfield-add-cnt"><a href="javascript:void(0);" class="stc-locfield-add"<#t/>
        <#if onAddClick?has_content> onClick="${onAddClick}"</#if>>[+]</a></div><#t/>
      <#if !entryTmpl?is_boolean>
        <div style="display:none;"><#t/>
          <div class="stc-locfield-markup-mainEntry"><@entryTmpl args=mainEntryArgs/></div><#t/>
          <div class="stc-locfield-markup-altEntry"><@entryTmpl args=altEntryArgs/></div><#t/>
        </div><#t/>
      </#if>
      <#if footTmpl?is_directive><@footTmpl args=footArgs/><#else>${footTmpl}</#if>
    </@field><#t/>
</#macro>

<#-- Entry markup template for @stcLocField 
    This must support a no-argument invocation for the JS template. -->
<#macro stcLocFieldEntry args={}>
  <#local entryData = args.entryData!{}>
  <@row class="+stc-locfield-entry"><#t/>
    <@cell small=2><#t/>
      <@field type="select" name=(args.localeFieldName!"") class="+stc-locfield-locale"><#t/>
        <#local localeOpts = args.localeOpts!{}>
        <@listLocaleMacros.availableLocalesOptions expandCountries=(localeOpts.expandCountries!true) 
            allowEmpty=(args.allowEmpty!localeOpts.allowEmpty!false) allowExtra=true allowExtraEmpty=true
            currentLocale=(entryData.localeString!"")/><#t/>
      </@field><#t/>
    </@cell><#t/>
    <@cell small=10><#t/>
      <@field type=(args.inputType!"input") tooltip=(args.tooltip!"") args=(args.inputArgs!{}) name=(args.textFieldName!"") class="+stc-locfield-text" value=(entryData.textData!)/><#t/>
    </@cell><#t/>
  </@row><#t/>
</#macro>

<#-- Simple text content localized field, a simple text input with separate entries for each locale.
    
    Originally written to produce submitted parameters compatible with the service interface:
      replaceEntityContentLocalizedSimpleTextsInterface
    notably these wrapper services:
      replaceProductContentLocalizedSimpleTexts
      replaceProductCategoryContentLocalizedSimpleTexts
    Args may be tweaked to work with others.

    REQUIRES:
    * /content/images/ScpContentCommon.js (StcLocFieldHandler)
    
    Creates the initial localized fields for ProductContent/ProductCategoryContent ALTERNATE_LOCALE fields
    at initial load (event error), but also supports other types of localized fields.
    
    values can be a single entryDataList, a hash of typeNames to entryDataLists,
    or if not specified, it will be looked up in params using getStcLocFieldParsedParams, which
    returns a hash of typeNames to entryDataLists.
    entryDataList is a list of hashes containing the keys: localeString, textData (can pass a list of view-entities).
    The FIRST entry in entryDataList is always assumed to be the main/original Content record linked by
    ProductContent/ProductCategoryContent, whereas the others are the ALTERNATE_LOCALE Content records
    in any order.
    
    The markup and especially name-building function can be overridden in the parameters 
    to suit the service/event.
    
    entityFieldName is auto-determined from typeName if not passed.
    
    Example:
        <@stcLocField typeName="PRODUCT_NAME"
            paramNamePrefix="contentFields_" params=parameters label=uiLabelMap.ProductProductName
            inputType="textarea"/>
            
    TODO: currently this only support    
-->
<#macro stcLocField typeName paramNamePrefix="contentFields_" entityFieldName=true values=false params={} parsedParamName=""
    label="" tooltip="" mainTooltip="" altTooltip="" inputType="input" wrapperArgs={} inputArgs={} localeOpts={} onAddClick="" 
    containerMarkup=false entryMarkup=false namePrefixFunc=false extraArgs...>
  <#local typeName = rawString(typeName)>
  <#local paramNamePrefix = rawString(paramNamePrefix)>
  <#if entityFieldName?is_boolean>
    <#local entityFieldName = rawString(Static["org.ofbiz.entity.model.ModelUtil"].dbNameToVarName(typeName))>
  <#else>
    <#local entityFieldName = rawString(entityFieldName)>
  </#if>
  <#if values?is_sequence>
    <#local entryDataList = values>
  <#elseif isObjectType("map", values)>
    <#local entryDataList = (values[typeName]![])>
  <#else>
    <#local values = getStcLocFieldParsedParams(params, parsedParamName, paramNamePrefix)!{}>
    <#local entryDataList = (values[typeName]![])>
  </#if>
  <#t/>
  <#if !onAddClick?has_content>
    <#local onAddClick>stcLocFieldHandler.handleFieldAdd(this);</#local>
  </#if>
  <#if !containerMarkup?is_directive>
    <#local containerMarkup = stcLocFieldContainer>
  </#if>
  <#if !entryMarkup?is_directive>
    <#local entryMarkup = stcLocFieldEntry>
  </#if>
  <#if !namePrefixFunc?is_macro>
    <#local namePrefixFunc = makeStcLocFieldNamePrefix>
  </#if>
  <#-- FIXME: need a separate div for html data attributes due to missing @field support for this -->
  <#local dataElem><#t/>
    <div style="display:none;" class="stc-locfield-data" data-stclf-type-name="${escapeVal(typeName, 'html')}"<#t/>
        <#t/> data-stclf-param-name-prefix="${escapeVal(paramNamePrefix, 'html')}"></div><#t/>
  </#local>
  <#local cmnEntryArgs = {"inputArgs":inputArgs, "inputType":inputType, "localeOpts":localeOpts}>
  <@containerMarkup typeName=typeName fieldName=entityFieldName paramNamePrefix=paramNamePrefix
      onAddClick=onAddClick entryTmpl=entryMarkup footTmpl=dataElem wrapperArgs=(wrapperArgs+{"label":label, "tooltip":tooltip})
      mainEntryArgs=(cmnEntryArgs+{"entryData":{"localeString":false}, "allowEmpty":true, "tooltip":mainTooltip})
      altEntryArgs=(cmnEntryArgs+{"entryData":{"localeString":false}, "allowEmpty":false, "tooltip":altTooltip})>
    <#-- NOTE: the first (index 0) must always allows empty locale, whereas the following entries
        should only show empty locale if somehow they were empty in the system - 
        user will have to correct these on update -->
    <#if entryDataList?has_content>
      <#-- add the main/default entry (Product[Category]Content, index zero) + alt entries (ContentAssoc ALTERNATE_LOCALE) -->
      <#list entryDataList as entryData>
        <#local namePrefix = namePrefixFunc(paramNamePrefix, typeName, entityFieldName, entryData?index)>
        <#local isMain = (entryData?index == 0)>
        <@entryMarkup args=(cmnEntryArgs+{"entryData":entryData, "localeFieldName":namePrefix+"localeString", 
            "textFieldName":namePrefix+"textData", "allowEmpty":isMain, "tooltip":isMain?then(mainTooltip, altTooltip)})/>
      </#list>
    <#else>
      <#-- add empty main/default entry (Product[Category]Content) -->
      <#local namePrefix = namePrefixFunc(paramNamePrefix, typeName, entityFieldName, 0)>
      <@entryMarkup args=(cmnEntryArgs+{"entryData":{}, "localeFieldName":namePrefix+"localeString", 
          "textFieldName":namePrefix+"textData", "allowEmpty":true, "tooltip":mainTooltip})/>
    </#if>
  </@containerMarkup>
</#macro>
