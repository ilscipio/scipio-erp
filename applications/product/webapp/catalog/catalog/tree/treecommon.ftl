<#-- SCIPIO: common macros for catalog tree and forms -->

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

<#function ectGetLocParams fieldInfo params>
    <#-- MUST BE ALREADY PARSED BY DATA PREP (too slow here), as params.simpleTextViewsByType -->
    <#return (params.simpleTextViewsByType)!{}>
</#function>

<#-- NOTE: must be same as: ScpEctCommon.js ScpEctFormHelper.makeLocFieldNamePrefix -->
<#function ectMakeLocFieldNamePrefix typeName index>
    <#return rawString(Static["org.ofbiz.product.category.CategoryWorker"].makeLocalizedSimpleTextContentFieldStringParamPrefix('contentField_', typeName, index))>
</#function>

<#assign ectLocFieldLabelMap = {
    "productName":uiLabelMap.ProductProductName
}>
<#function ectGetLocFieldLabel fieldName typeName>
    <#return ectLocFieldLabelMap[fieldName]!uiLabelMap["FormFieldTitle_"+fieldName]>
</#function>

<#-- NOTE: this one no longer used as JS template -->
<#macro ectDefMarkupLocFieldCnt args={}>
    <@field type="general" containerClass="+ect-locfield ect-locfield-for-${args.typeName}" args=(args.fieldArgs!{})><#t/>
      <div class="ect-locfield-entries"><#nested></div><#t/>
      <div class="ect-locfield-add-cnt"><a href="javascript:void(0);" class="etc-locfield-add"<#t/>
        <#if args.onAddClick?has_content> onClick="${args.onAddClick}"</#if>>[+]</a></div><#t/>
      <#-- NOTE: due to styling problems with @row/@cell, we are forced to include this template inside each field
        container, which causes bunch of copies for nothing... however it also simplifies the code, so it's acceptable -->
      <#if args.entryTmpl??>
      <div style="display:none;"><#t/>
        <div class="ect-markup-locFieldEntry"><@args.entryTmpl/></div><#t/>
      </div><#t/>
      </#if>
    </@field><#t/>
</#macro>

<#-- Used as a template for js and freemarker and for generating initial fields on load
    WARN: This will not respond to context like normal fields when used in JS... 
    NOTE: must match ScpEctCommon.js ScpEctFormHelper.buildLocalizedFieldEntry -->
<#macro ectDefMarkupLocFieldEntry args={}>
  <#local localeFieldName = "">
  <#local textFieldName = "">
  <#if args.typeName?has_content>
    <#local namePrefix = ectMakeLocFieldNamePrefix(args.typeName, args.index)>
    <#local localeFieldName = namePrefix+"localeString">
    <#local textFieldName = namePrefix+"textData">
  </#if>
  <@row class="+ect-locfield-entry"><#t/>
    <@cell small=2><#t/>
      <@field type="select" name=localeFieldName class="+ect-locfield-locale"><#t/>
        <@listLocaleMacros.availableLocalesOptions expandCountries=true allowExtra=true allowEmpty=true
          currentLocale=(args.entryData.localeString)!/><#t/>
      </@field><#t/>
    </@cell><#t/>
    <@cell small=10><#t/>
      <@field type="input" name=textFieldName class="+ect-locfield-text" value=(args.entryData.textData)!/><#t/>
    </@cell><#t/>
  </@row><#t/>
</#macro>

<#-- Creates the initial localized fields at initial load (event error) and when js not available.
    See corresponding js in ScpEctCommon.js ScpEctFormHelper.rebuildLocalizedFieldEntries.
    DEV NOTE: PLEASE KEEP BOTH IMPL IN SYNC. -->
<#macro ectLocalizedFields objectType params={} 
    fieldCntMarkup=ectDefMarkupLocFieldCnt fieldEntryMarkup=ectDefMarkupLocFieldEntry onAddClick="" formId=true>
  <#-- didn't need
  <#if formId?is_boolean>
    <#local formId = (readRequestStack("scipioFormInfoStack").id)!"">
  </#if>-->
  <#if !onAddClick?has_content>
    <#local onAddClick>scpEctFormHelper.handleFieldAdd(this);</#local>
  </#if>
  <#local fieldInfo = (ectObjectLocalizedFields[objectType])!>
  <#if fieldInfo?has_content>

    <#local valueListsByType = ectGetLocParams(fieldInfo, params)>
    <#list fieldInfo.typeNames as typeName>
      <#local fieldName = fieldInfo.fieldNames[typeName?index]>
      <@fieldCntMarkup args={"onAddClick":onAddClick, "entryTmpl":fieldEntryMarkup,
          "typeName":typeName, "fieldName":fieldName,
          "fieldArgs":{"label":ectGetLocFieldLabel(fieldName, typeName), "tooltip":""}}>
        <#local entryDataList = (valueListsByType[typeName]![])>
        <#if entryDataList?has_content>
          <#-- add the main/default entry (Product[Category]Content, index zero) + ContentAssoc entries -->
          <#list entryDataList as entryData>
            <@fieldEntryMarkup args={"typeName":typeName, "index":entryData?index, "entryData":entryData}/>
          </#list>
        <#else>
          <#-- add empty main/default entry (Product[Category]Content) -->
          <@fieldEntryMarkup args={"typeName":typeName, "index":0, "entryData":{}}/>
        </#if>
      </@fieldCntMarkup>
    </#list>
    <#-- TEMPLATE MARKUP FOR JAVASCRIPT -->
    <#--<div style="display:none;">-->
        <#-- no longer needed
        <div class="ect-markup-locFieldCnt"><@fieldCntMarkup args={}/></div>-->
        <#-- NOTE: due to styling problems with @row/@cell, we are forced to include this inside the field
            container, which causes bunch of copies for nothing... but it works out
        <div class="ect-markup-locFieldEntry"><@fieldEntryMarkup args={}/></div> -->
    <#--</div>-->
  </#if>
</#macro>
