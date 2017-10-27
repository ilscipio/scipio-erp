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

<#function ectParseLocParams fieldInfo params>
      <#-- TODO -->
      <#return params>
</#function>

<#-- NOTE: must be same as: ScpEctCommon.js ScpEctFormHelper.makeLocFieldNamePrefix -->
<#function ectMakeLocFieldNamePrefix typeName index>
    <#return 'contentField_' + typeName + '.' + index + '.'>
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
    fieldCnt=ectDefMarkupLocFieldCnt fieldEntry=ectDefMarkupLocFieldEntry onAddClick="" formId=true>
  <#-- didn't need
  <#if formId?is_boolean>
    <#local formId = (readRequestStack("scipioFormInfoStack").id)!"">
  </#if>-->
  <#if !onAddClick?has_content>
    <#local onAddClick>scpEctFormHelper.handleFieldAdd(this);</#local>
  </#if>
  <#local fieldInfo = (ectObjectLocalizedFields[objectType])!>
  <#if fieldInfo?has_content>
    <#local fieldLabelMap = {
        "productName":uiLabelMap.ProductProductName
    }>
    <#local fieldValues = ectParseLocParams(fieldInfo, params)>
    <#list fieldInfo.fieldNames as fieldName>
      <#local typeName = fieldInfo.typeNames[fieldName?index]>
      <#local fieldLabel = fieldLabelMap[fieldName]!uiLabelMap["FormFieldTitle_"+fieldName]>
      <@fieldCnt args={"onAddClick":onAddClick, "entryTmpl":fieldEntry,
          "fieldName":fieldName, "typeName":typeName,
          "fieldArgs":{"label":fieldLabel, "tooltip":""}}>
        <#local values = (fieldValues[fieldName]![])>
        <#if values?has_content>
          <#-- add the main/default entry (Product[Category]Content, index zero) + ContentAssoc entries -->
          <#list values as entryData>
            <@fieldEntry args={"typeName":typeName, "index":entryData?index, "entryData":entryData}/>
          </#list>
        <#else>
          <#-- add empty main/default entry (Product[Category]Content) -->
          <@fieldEntry args={"typeName":typeName, "index":0, "entryData":{}}/>
        </#if>
      </@fieldCnt>
    </#list>
    <#-- TEMPLATE MARKUP FOR JAVASCRIPT -->
    <#--<div style="display:none;">-->
        <#-- no longer needed
        <div class="ect-markup-locFieldCnt"><@fieldCnt args={}/></div>-->
        <#-- NOTE: due to styling problems with @row/@cell, we are forced to include this inside the field
            container, which causes bunch of copies for nothing... but it works out
        <div class="ect-markup-locFieldEntry"><@fieldEntry args={}/></div> -->
    <#--</div>-->
  </#if>
</#macro>
