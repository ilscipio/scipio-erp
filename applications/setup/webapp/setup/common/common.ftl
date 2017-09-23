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