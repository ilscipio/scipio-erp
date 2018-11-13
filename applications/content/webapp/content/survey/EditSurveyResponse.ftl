<#--
This file is subject to the terms and conditions defined in
 file 'LICENSE', which is part of this source code package.
-->

<#-- Render the survey -->
<#if surveyString?has_content>
  <form method="post" enctype="multipart/form-data" action="<@ofbizUrl>updateSurveyResponse</@ofbizUrl>" name="EditSurveyResponseForm">
    <@field type="lookup" label=uiLabelMap.PartyPartyId value=surveyPartyId!(userLogin.partyId!) formName="EditSurveyResponseForm" name="partyId" id="partyId" fieldFormName="LookupPartyName"/>
    <#-- pass through the dataResourceId so the response can be associated with it -->
    <input type="hidden" name="dataResourceId" value="${parameters.dataResourceId!}"/>
    <input type="hidden" name="rootContentId" value="${parameters.rootContentId!}"/>
    ${rawString(surveyString)}
  </form>
<#else>
  <@commonMsg type="error">Problem rendering the survey.</@commonMsg>
</#if>
