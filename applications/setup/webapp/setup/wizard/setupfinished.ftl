<#include "component://setup/webapp/setup/common/common.ftl">

  <@alert type="info">
    <#if incompleteSteps?has_content>
      <p>${uiLabelMap.SetupIncompleteMsgStart}</p>
      <ul>
        <#list incompleteSteps as stepName>
          <#-- TODO?: link to the steps? (need params) -->
          <#assign stepTitle = uiLabelMap[rawString((setupStepTitlePropMap[stepName])!"")]>
          <#if ((setupStepDisabledMap[stepName])!true) == false>
            <li><a href="${escapeFullUrl(makeSetupStepUrl(stepName), 'html')}">${stepTitle}</a></li>
          <#else>
            <li>${stepTitle}</li>
          </#if>
        </#list>
      </ul>
      <p>${uiLabelMap.SetupIncompleteMsgEnd}</p>
    <#else>
      ${uiLabelMap.SetupCompleteMsg}
    </#if>
  </@alert>
  