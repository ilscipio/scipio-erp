<#include "component://setup/webapp/setup/common/common.ftl">

<#assign setupStepIconMap = {
    "organization": "fa fa-info", 
    "store": "fa fa-info", 
    "user": "fa fa-info", 
    "accounting": "fa fa-credit-card", 
    "facility": "fa fa-building", 
    "catalog": "fa fa-info", 
    "website": "fa fa-info"
}>
<#assign setupStepIconDef = "fa fa-info">

<#macro setupNavStep name icon=setupStepIconDef>
  <#-- NOTE: the "disabled" logic is overridden and determined by SetupWorker -->
  <#local stepState = (setupStepStates[name])!{}>
  <@step name=name 
    icon=icon 
    href=makeSetupStepUrl(name, stepState)
    completed=((stepState.completed)!false) 
    disabled=((setupStepDisabledMap[name])!true)
    >${uiLabelMap[rawString((setupStepTitlePropMap[name])!"")]}</@step>
</#macro>

<@section>
    <@nav type="steps" activeElem=(setupStep!"")>
      <#list setupStepList as stepName>
        <@setupNavStep name=stepName icon=(setupStepIconMap[stepName]!setupStepIconDef)/>
      </#list>
    </@nav>
</@section>
