<#-- SCIPIO: common macros for glAccount tree and forms -->

<#import "component://common/webcommon/includes/listLocalesMacros.ftl" as listLocaleMacros>

<#macro egltMarkupOut dir args={}>
  <#if dir?is_directive>
    <@dir args=args/><#t/>
  <#else>
    ${dir}<#t/>
  </#if>
</#macro>

<#-- fields needed for every form submitted back to the catalog tree
    initialValues are required only if form is submitted NOT by the tree, otherwise tree automatically fills them. -->
<#macro egltCommonTreeFormFields params={} initialValues={}>
    <#-- egltTargetNodePath implements the node pre-selection; maintains the selection event if event error -->
    <@field type="hidden" name="egltTargetNodePath" value=(initialValues.egltTargetNodePath!) class="+eglt-inputfield"/>
    <#-- egltNewTargetNodePath is used instead of egltTargetNodePath when event succeeds -->
    <@field type="hidden" name="egltNewTargetNodePath" value=(initialValues.egltNewTargetNodePath!) class="+eglt-inputfield"/>
    <#-- id of the submitted form -->
    <@field type="hidden" name="egltSubmittedFormId" value=(initialValues.egltSubmittedFormId!) class="+eglt-inputfield"/>
</#macro>

<#function egltGetLocParams fieldInfo params>
    <#-- MUST BE ALREADY PARSED BY DATA PREP (too slow here), as params.simpleTextViewsByType -->
    <#return (params.simpleTextViewsByType)!{}>
</#function>

<#-- NOTE: must be same as: ScpEctCommon.js ScpEctFormHelper.makeLocFieldNamePrefix -->
<#function egltMakeLocFieldNamePrefix typeName index>
   
</#function>

<#assign egltLocFieldLabelMap = {
    "accountName":uiLabelMap.AccountingAccountName
}>
<#function egltGetLocFieldLabel fieldName typeName>
    <#return egltLocFieldLabelMap[fieldName]!uiLabelMap["FormFieldTitle_"+fieldName]>
</#function>

<#-- NOTE: this one no longer used as JS template -->
<#macro egltDefMarkupLocFieldCnt args={}>
    <@field type="general" containerClass="+eglt-locfield eglt-locfield-for-${args.typeName}" args=(args.fieldArgs!{})><#t/>
      <div class="eglt-locfield-entries"><#nested></div><#t/>
      <div class="eglt-locfield-add-cnt"><a href="javascript:void(0);" class="eglt-locfield-add"<#t/>
        <#if args.onAddClick?has_content> onClick="${args.onAddClick}"</#if>>[+]</a></div><#t/>
      <#-- NOTE: due to styling problems with @row/@cell, we are forced to include this template inside each field
        container, which causes bunch of copies for nothing... however it also simplifies the code, so it's acceptable -->
      <#if args.entryTmpl??>
      <div style="display:none;"><#t/>
        <div class="eglt-markup-locFieldEntry"><@args.entryTmpl/></div><#t/>
      </div><#t/>
      </#if>
    </@field><#t/>
</#macro>

<#-- Used as a template for js and freemarker and for generating initial fields on load
    WARN: This will not respond to context like normal fields when used in JS... 
    NOTE: must match ScpEctCommon.js ScpEctFormHelper.buildLocalizedFieldEntry -->
<#macro egltDefMarkupLocFieldEntry args={}>
  <#local localeFieldName = "">
  <#local textFieldName = "">
  <#if args.typeName?has_content>
    <#local namePrefix = egltMakeLocFieldNamePrefix(args.typeName, args.index)>
    <#local localeFieldName = namePrefix+"localeString">
    <#local textFieldName = namePrefix+"textData">
  </#if>
  <@row class="+eglt-locfield-entry"><#t/>
    <@cell small=2><#t/>
      <@field type="select" name=localeFieldName class="+eglt-locfield-locale"><#t/>
        <@listLocaleMacros.availableLocalesOptions expandCountries=true allowExtra=true allowEmpty=true
          currentLocale=(args.entryData.localeString)!/><#t/>
      </@field><#t/>
    </@cell><#t/>
    <@cell small=10><#t/>
      <@field type="input" name=textFieldName class="+eglt-locfield-text" value=(args.entryData.textData)!/><#t/>
    </@cell><#t/>
  </@row><#t/>
</#macro>

<#-- Creates the initial localized fields at initial load (event error) and when js not available.
    See corresponding js in ScpEctCommon.js ScpEctFormHelper.rebuildLocalizedFieldEntries.
    DEV NOTE: PLEASE KEEP BOTH IMPL IN SYNC. -->
<#macro egltLocalizedFields objectType params={} 
    fieldCntMarkup=egltDefMarkupLocFieldCnt fieldEntryMarkup=ectDefMarkupLocFieldEntry onAddClick="" formId=true>
    
    <@alert type="warning">WARNING: <strong>LOCALIZED FIELDS ARE NOT YET SAVED ON SUBMIT</strong> (2017-10-27)</@alert>
    
  <#-- didn't need
  <#if formId?is_boolean>
    <#local formId = (readRequestStack("scipioFormInfoStack").id)!"">
  </#if>-->
  <#if !onAddClick?has_content>
    <#local onAddClick>sglpEctFormHelper.handleFieldAdd(this);</#local>
  </#if>
  <#local fieldInfo = (ectObjectLocalizedFields[objectType])!>
  <#if fieldInfo?has_content>

    <#local valueListsByType = egltGetLocParams(fieldInfo, params)>
    <#list fieldInfo.typeNames as typeName>
      <#local fieldName = fieldInfo.fieldNames[typeName?index]>
      <@fieldCntMarkup args={"onAddClick":onAddClick, "entryTmpl":fieldEntryMarkup,
          "typeName":typeName, "fieldName":fieldName,
          "fieldArgs":{"label":egltGetLocFieldLabel(fieldName, typeName), "tooltip":""}}>
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
