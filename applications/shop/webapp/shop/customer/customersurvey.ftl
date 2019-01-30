<#--
This file is subject to the terms and conditions defined in the
files 'LICENSE' and 'NOTICE', which are part of this source
code package.
-->
<#include "component://shop/webapp/shop/customer/customercommon.ftl">

<@section title=((survey.surveyName)!)>
    <#-- Render the survey -->
    <#if surveyWrapper?has_content>
        <form method="post" enctype="multipart/form-data" action="<@pageUrl>profilesurvey/profilesurvey</@pageUrl>">
          ${surveyWrapper.render(context)}
        </form>
    <#else>
        <@commonMsg type="result">${uiLabelMap.OrderNothingToDoHere}</@commonMsg>
    </#if>
</@section>
