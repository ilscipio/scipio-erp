<#include "component://setup/webapp/setup/common/common.ftl">

  <@alert type="error">
    ${setupErrorMsg!(uiLabelMap.CommonError)}
  </@alert>
  <#if autoNextSetupStep?has_content>
    <a href="${escapeFullUrl(makeSetupStepUrl(autoNextSetupStep), 'html')}"<#t/>
       <#t/> class="${styles.link_nav!} ${styles.action_continue!}">${uiLabelMap.CommonContinue}</a>
  </#if>
