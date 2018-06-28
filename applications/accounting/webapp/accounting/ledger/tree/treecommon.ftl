<#-- SCIPIO: common macros for glAccount tree and forms -->

<#import "component://common/webcommon/includes/listLocalesMacros.ftl" as listLocaleMacros>

<#macro acctgMarkupOut dir args={}>
  <#if dir?is_directive>
    <@dir args=args/><#t/>
  <#else>
    ${dir}<#t/>
  </#if>
</#macro>

<#-- fields needed for every form submitted back to the glAccount tree
    initialValues are required only if form is submitted NOT by the tree, otherwise tree automatically fills them. -->
<#macro acctgCommonTreeFormFields params={} initialValues={}>
    <#-- acctgTargetNodePath implements the node pre-selection; maintains the selection event if event error -->
    <@field type="hidden" name="acctgTargetNodePath" value=(initialValues.acctgTargetNodePath!) class="+acctg-inputfield"/>
    <#-- acctgNewTargetNodePath is used instead of acctgTargetNodePath when event succeeds -->
    <@field type="hidden" name="acctgNewTargetNodePath" value=(initialValues.acctgNewTargetNodePath!) class="+acctg-inputfield"/>
    <#-- id of the submitted form -->
    <@field type="hidden" name="acctgSubmittedFormId" value=(initialValues.acctgSubmittedFormId!) class="+acctg-inputfield"/>
</#macro>

<#function acctgGetLocParams fieldInfo params>
    <#-- MUST BE ALREADY PARSED BY DATA PREP (too slow here), as params.simpleTextViewsByType -->
    <#return (params.simpleTextViewsByType)!{}>
</#function>

<#-- NOTE: must be same as: ScpEctCommon.js ScpEctFormHelper.makeLocFieldNamePrefix -->
<#function acctgMakeLocFieldNamePrefix typeName index>
   
</#function>

<#assign acctgLocFieldLabelMap = {
    "accountName":uiLabelMap.CommonName
}>
<#function acctgGetLocFieldLabel fieldName typeName>
    <#return acctgLocFieldLabelMap[fieldName]!uiLabelMap["FormFieldTitle_"+fieldName]>
</#function>

<#-- NOTE: this one no longer used as JS template -->
<#macro acctgDefMarkupLocFieldCnt args={}>
    <@field type="generic" containerClass="+acctg-locfield acctg-locfield-for-${args.typeName}" args=(args.fieldArgs!{})><#t/>
      <div class="acctg-locfield-entries"><#nested></div><#t/>
      <div class="acctg-locfield-add-cnt"><a href="javascript:void(0);" class="acctg-locfield-add"<#t/>
        <#if args.onAddClick?has_content> onClick="${args.onAddClick}"</#if>>[+]</a></div><#t/>
      <#-- NOTE: due to styling problems with @row/@cell, we are forced to include this template inside each field
        container, which causes bunch of copies for nothing... however it also simplifies the code, so it's acceptable -->
      <#if args.entryTmpl??>
      <div style="display:none;"><#t/>
        <div class="acctg-markup-locFieldEntry"><@args.entryTmpl/></div><#t/>
      </div><#t/>
      </#if>
    </@field><#t/>
</#macro>

<#-- Used as a template for js and freemarker and for generating initial fields on load
    WARN: This will not respond to context like normal fields when used in JS... 
    NOTE: must match ScpEctCommon.js ScpEctFormHelper.buildLocalizedFieldEntry -->
<#macro acctgDefMarkupLocFieldEntry args={}>
  <#local localeFieldName = "">
  <#local textFieldName = "">
  <#if args.typeName?has_content>
    <#local namePrefix = acctgMakeLocFieldNamePrefix(args.typeName, args.index)>
    <#local localeFieldName = namePrefix+"localeString">
    <#local textFieldName = namePrefix+"textData">
  </#if>
  <@row class="+acctg-locfield-entry"><#t/>
    <@cell small=2><#t/>
      <@field type="select" name=localeFieldName class="+acctg-locfield-locale"><#t/>
        <@listLocaleMacros.availableLocalesOptions expandCountries=true allowExtra=true allowEmpty=true
          currentLocale=(args.entryData.localeString)!/><#t/>
      </@field><#t/>
    </@cell><#t/>
    <@cell small=10><#t/>
      <@field type="input" name=textFieldName class="+acctg-locfield-text" value=(args.entryData.textData)!/><#t/>
    </@cell><#t/>
  </@row><#t/>
</#macro>

<#-- Creates the initial localized fields at initial load (event error) and when js not available.
    See corresponding js in ScpEctCommon.js ScpEctFormHelper.rebuildLocalizedFieldEntries.
    DEV NOTE: PLEASE KEEP BOTH IMPL IN SYNC. -->
<#macro acctgLocalizedFields objectType params={} 
    fieldCntMarkup=acctgDefMarkupLocFieldCnt fieldEntryMarkup=ectDefMarkupLocFieldEntry onAddClick="" formId=true>
    
    <@alert type="warning">WARNING: <strong>LOCALIZED FIELDS ARE NOT YET SAVED ON SUBMIT</strong> (2017-10-27)</@alert>
    
  <#-- didn't need
  <#if formId?is_boolean>
    <#local formId = (readRequestStack("scipioFormInfoStack").id)!"">
  </#if>-->
  <#if !onAddClick?has_content>
    <#local onAddClick>sglpEctFormHelper.handleFieldAdd(this);</#local>
  </#if>
  <#local fieldInfo = (acctgObjectLocalizedFields[objectType])!>
  <#if fieldInfo?has_content>

    <#local valueListsByType = acctgGetLocParams(fieldInfo, params)>
    <#list fieldInfo.typeNames as typeName>
      <#local fieldName = fieldInfo.fieldNames[typeName?index]>
      <@fieldCntMarkup args={"onAddClick":onAddClick, "entryTmpl":fieldEntryMarkup,
          "typeName":typeName, "fieldName":fieldName,
          "fieldArgs":{"label":acctgGetLocFieldLabel(fieldName, typeName), "tooltip":""}}>
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
