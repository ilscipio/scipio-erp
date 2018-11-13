<#--
This file is subject to the terms and conditions defined in
 file 'LICENSE', which is part of this source code package.
-->

<#-- Render the survey -->
<#if requestAttributes.surveyWrapper?? && requestAttributes.surveyAction??>
  <form method="post" enctype="multipart/form-data" action="<@ofbizUrl>${requestAttributes.surveyAction}</@ofbizUrl>">
    ${requestAttributes.surveyWrapper.renderSurvey(context)}
  </form>
<#else>
  <@commonMsg type="info">${uiLabelMap.OrderNothingToDoHere}</@commonMsg>
</#if>
