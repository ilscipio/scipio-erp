<#-- Common setup macros, definitions, etc. -->

<#function makeSetupStepUrl name stepState=true>
  <#if stepState?is_boolean>
    <#local stepState = (setupStepStates[name])!{}>
  </#if>
  <#local paramStr = addParamsToStrUrlEnc("", stepState.stepParams!)>
  <#if paramStr?has_content>
    <#local paramStr = "?" + paramStr>
  </#if>
  <#return makeOfbizUrl("setup"+name?cap_first + paramStr)>
</#function>

<#macro defaultWizardFormFields stepName=true stepState=true>
  <#if stepName?is_boolean && stepName == true>
    <#local stepName = setupStep><#-- main context -->
  </#if>
  <#if !stepName?is_boolean && stepName?has_content>
    <@field type="hidden" name="scpSubmitSetupStep" value=stepName/>
  
    <#if stepState?is_boolean>
      <#local stepState = (setupStepStates[stepName])!{}>
    </#if>
    <#local stepParams = toSimpleMap(stepState.stepParams!{})>
    <#list stepParams?keys as paramName>
      <@field type="hidden" name=paramName value=stepParams[paramName]!/>
    </#list>
  </#if>
</#macro>

<#-- Form field value logic - emulates org.ofbiz.widget.model.ModelFormField#getEntry
TODO: in future supposed to be handled with a form library method #getAutoValue

values = user-configurable and hidden
fixedValues = special: params that were hardcoded to preset values in stock ofbizsetup (handling differently here)
-->
<#function getWizardFormFieldValueMaps args={}>
  <#local record = args.record!true>
  <#local useReqParams = args.useReqParams!0><#-- default true -->
  <#local defaults = args.defaults!{}><#-- default values (NOTE: this is not in form widgets) -->
  <#local strictRecord = args.strictRecord!false><#-- if false it converts the GenericValue to map copy to prevent crash on unknown fields -->
  
  <#local recordIsContext = false>
  <#if record?is_boolean>
    <#if record>
      <#local record = context>
      <#local recordIsContext = true>
    <#else>
      <#local record = {}>
    </#if>
  </#if>
  
  <#if strictRecord || recordIsContext>
    <#local record = toSimpleMap(record)>
  <#else>
    <#local record = copyMap(record)>
  </#if>
  <#local defaults = toSimpleMap(defaults)>
  
  <#if useReqParams?is_number>
    <#local useReqParams = useRequestParameters!"">
  </#if>
  <#if ((isError!false) == true && !(useReqParams?is_boolean && useReqParams == false))
        || (useReqParams?is_boolean && useReqParams == true)><#-- check if service error or forced parameters -->
    <#local values = toSimpleMap(parameters)>
    <#local fixedValues = defaults><#-- this is stock ofbizsetup behavior -->
  <#else>
    <#if recordIsContext && !(useReqParams?is_boolean && useReqParams == false)>
      <#-- SPECIAL: this allows to pre-fill some fields when accessing New/Create screen initially -->
      <#local values = toSimpleMap(parameters) + record>
    <#else>
      <#local values = record><#-- DON'T use defaults if record is present -->
    </#if>
    <#if !recordIsContext>
      <#-- SPECIAL: different from stock ofbizsetup: we keep the values from the record so we 
          don't corrupt existing records - only new ones should get the defaults -->
      <#local fixedValues = record>
    <#else>
      <#local fixedValues = defaults><#-- this is stock ofbizsetup behavior -->
    </#if>
  </#if>
  <#return {"values":values, "fixedValues":fixedValues}>
</#function>
