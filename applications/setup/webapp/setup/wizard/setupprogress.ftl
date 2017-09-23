<#include "component://setup/webapp/setup/common/common.ftl">

<#macro setupNavStep name icon="fa fa-info">
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
        <@setupNavStep name="organization" icon="fa fa-info"/>
        <@setupNavStep name="store" icon="fa fa-info"/>
        <@setupNavStep name="user" icon="fa fa-info"/>
        <@setupNavStep name="accounting" icon="fa fa-credit-card"/>
        <@setupNavStep name="facility" icon="fa fa-building"/>
        <@setupNavStep name="catalog" icon="fa fa-info"/>
        <@setupNavStep name="website" icon="fa fa-info"/>
    </@nav>
</@section>
