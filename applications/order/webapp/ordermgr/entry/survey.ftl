<#--
This file is subject to the terms and conditions defined in the
files 'LICENSE' and 'NOTICE', which are part of this source
code package.
-->

<#-- Render the survey -->
<#if requestAttributes.surveyWrapper?? && requestAttributes.surveyAction??>
  <form method="post" enctype="multipart/form-data" action="<@pageUrl>${requestAttributes.surveyAction}</@pageUrl>">
    ${requestAttributes.surveyWrapper.renderSurvey(context)}
  </form>
<#else>
  <@commonMsg type="info">${uiLabelMap.OrderNothingToDoHere}</@commonMsg>
</#if>
