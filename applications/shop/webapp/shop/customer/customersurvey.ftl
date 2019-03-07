<#--
This file is subject to the terms and conditions defined in the
files 'LICENSE' and 'NOTICE', which are part of this source
code package.
-->
<#include "component://shop/webapp/shop/customer/customercommon.ftl">

<@section title=((survey.surveyName)!)>
    <#-- Render the survey -->
    <#if surveyWrapper?has_content>
        <#-- SCIPIO: 2019-03-06: Now supports surveyAction and surveyMarkup error/missing fallback
            NOTE: surveyWrapper.render(context) may return null/void/missing/empty upon error or upon empty output -->
        <form method="post" enctype="multipart/form-data" action="<@pageUrl uri=surveyAction!'profilesurvey' escapeAs='html'/>">
          <#assign surveyMarkup = surveyWrapper.render(context)!>
          <#if surveyMarkup?has_content>
            ${surveyMarkup}
          <#else>
            <@commonMsg type="result">${uiLabelMap.OrderNothingToDoHere}</@commonMsg>
            <a href="<@pageUrl uri='main'/>" class="${styles.link_nav} ${styles.action_view}">${uiLabelMap.CommonHome}</a>
          </#if>
        </form>
    <#else>
        <@commonMsg type="result">${uiLabelMap.OrderNothingToDoHere}</@commonMsg>
        <a href="<@pageUrl uri='main'/>" class="${styles.link_nav} ${styles.action_view}">${uiLabelMap.CommonHome}</a>
    </#if>
</@section>
