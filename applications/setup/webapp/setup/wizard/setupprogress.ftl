<#include "component://setup/webapp/setup/common/common.ftl">

<#-- NOTE: for icons and labels, see SetupWizardStaticProperties.groovy -->

<#macro setupNavStep name>
  <#-- NOTE: the "disabled" logic is overridden and determined by SetupWorker -->
  <#local stepState = (setupStepStates[name])!{}>
  <@step name=name 
    icon=("fa "+rawString((setupStepIconMap[name])!(setupStepIconMap["default"])!))
    href=makeSetupStepUrl(name, stepState)
    completed=((stepState.completed)!false) 
    disabled=((setupStepDisabledMap[name])!true)
    >${uiLabelMap[rawString((setupStepTitlePropMap[name])!"")]}</@step>
</#macro>

<@section>
    <@nav type="steps" activeElem=(setupStep!"")>
      <#list setupStepList as stepName>
        <@setupNavStep name=stepName/>
      </#list>
    </@nav>
</@section>
