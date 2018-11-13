<#--
This file is subject to the terms and conditions defined in
 file 'LICENSE', which is part of this source code package.
-->
<#include "component://shop/webapp/shop/customer/customercommon.ftl">

<@section title=((survey.surveyName)!)>
    <#-- Render the survey -->
    <#if surveyWrapper?has_content>
        <form method="post" enctype="multipart/form-data" action="<@ofbizUrl>profilesurvey/profilesurvey</@ofbizUrl>">
          ${surveyWrapper.render(context)}
        </form>
    <#else>
        <@commonMsg type="result">${uiLabelMap.OrderNothingToDoHere}</@commonMsg>
    </#if>
</@section>
