<#include "component://setup/webapp/setup/common/common.ftl">

<#macro setupNavStep name icon="fa fa-info">
  <#-- NOTE: the "disabled" logic is overridden and determined by SetupWorker -->
  <#local stepState = (setupStepStates[name])!{}>
  <#local paramStr = addParamsToStrUrlEnc("", stepState.stepParams!)>
  <#if paramStr?has_content>
    <#local paramStr = "?" + paramStr>
  </#if>
  <@step name=name 
    icon=icon 
    href=makeOfbizUrl("setup"+name?cap_first + paramStr) 
    completed=((stepState.completed)!false) 
    disabled=((stepState.disabled)!true)
    ><#nested></@step>
</#macro>

<@section>
    <@nav type="steps" activeElem=(setupStep!"organization")>
        <@setupNavStep name="organization" icon="fa fa-info">${uiLabelMap.SetupOrganization}</@setupNavStep>
        <@setupNavStep name="store" icon="fa fa-info">${uiLabelMap.CommonStore}</@setupNavStep>
        <@setupNavStep name="user" icon="fa fa-info">${uiLabelMap.PartyParty}</@setupNavStep>
        <@setupNavStep name="accounting" icon="fa fa-credit-card">${uiLabelMap.AccountingAccounting}</@setupNavStep>
        <@setupNavStep name="facility" icon="fa fa-building">${uiLabelMap.SetupFacility}</@setupNavStep>
        <@setupNavStep name="catalog" icon="fa fa-info">${uiLabelMap.ProductCatalog}</@setupNavStep>
        <@setupNavStep name="website" icon="fa fa-info">${uiLabelMap.SetupWebSite}</@setupNavStep>
    </@nav>
</@section>
