<#--
This file is subject to the terms and conditions defined in
 file 'LICENSE', which is part of this source code package.
-->

<#if additionalFields?has_content>
  <#list additionalFields.keySet() as field>
    <input type="hidden" name="${field}" value="${additionalFields.get(field)}"/>
  </#list>
</#if>

<#-- update response -->
<#if surveyResponseId?has_content>
  <input type="hidden" name="surveyResponseId" value="${surveyResponseId}"/>
</#if>

<#-- party ID -->
<#if partyId?has_content>
  <input type="hidden" name="partyId" value="${partyId}"/>
</#if>

<#-- survey ID -->
<input type="hidden" name="surveyId" value="${survey.surveyId}"/>

<@table type="data-complex"> <#-- orig: width="100%" border="0" cellpadding="2" cellspacing="0" -->
  <#list surveyQuestionAndAppls as surveyQuestionAndAppl>
    <#-- get an answer from the answerMap -->
    <#if surveyAnswers?has_content>
      <#assign answer = surveyAnswers.get(surveyQuestionAndAppl.surveyQuestionId)!>
    </#if>

    <@tr>
      <#-- standard question options -->
      <@td align='left'>
        <div>${surveyQuestionAndAppl.question!}</div>
        <#if surveyQuestionAndAppl.hint?has_content>
          <div>${surveyQuestionAndAppl.hint}</div>
        </#if>
      </@td>
    </@tr>

      <@tr>
        <@td align="center">
          <#if surveyQuestionAndAppl.surveyQuestionTypeId == "BOOLEAN">
            <#assign selectedOption = (answer.booleanResponse)?default("Y")>
            <select name="answers_${surveyQuestionAndAppl.surveyQuestionId}">
              <#if surveyQuestionAndAppl.requiredField?default("N") != "Y">
                <option value=""></option>
              </#if>
              <option value="Y" <#if "Y" == selectedOption>selected="selected"</#if>>Y</option>
              <option value="N" <#if "N" == selectedOption>selected="selected"</#if>>N</option>
            </select>
          <#elseif surveyQuestionAndAppl.surveyQuestionTypeId == "TEXTAREA">
            <textarea cols="40" rows="5" name="answers_${surveyQuestionAndAppl.surveyQuestionId}">${(answer.textResponse)!}</textarea>
          <#elseif surveyQuestionAndAppl.surveyQuestionTypeId == "TEXT_SHORT">
            <input type="text" size="15" class="textBox" name="answers_${surveyQuestionAndAppl.surveyQuestionId}" value="${(answer.textResponse)!}"/>
          <#elseif surveyQuestionAndAppl.surveyQuestionTypeId == "TEXT_LONG">
            <input type="text" size="35" class="textBox" name="answers_${surveyQuestionAndAppl.surveyQuestionId}" value="${(answer.textResponse)!}"/>
          <#elseif surveyQuestionAndAppl.surveyQuestionTypeId == "EMAIL">
            <input type="text" size="30" class="textBox" name="answers_${surveyQuestionAndAppl.surveyQuestionId}" value="${(answer.textResponse)!}"/>
          <#elseif surveyQuestionAndAppl.surveyQuestionTypeId == "URL">
            <input type="text" size="40" class="textBox" name="answers_${surveyQuestionAndAppl.surveyQuestionId}" value="${(answer.textResponse)!}"/>
          <#elseif surveyQuestionAndAppl.surveyQuestionTypeId == "DATE">
            <input type="text" size="12" class="textBox" name="answers_${surveyQuestionAndAppl.surveyQuestionId}" value="${(answer.textResponse)!}"/>
          <#elseif surveyQuestionAndAppl.surveyQuestionTypeId == "CREDIT_CARD">
            <input type="text" size="20" class="textBox" name="answers_${surveyQuestionAndAppl.surveyQuestionId}" value="${(answer.textResponse)!}"/>
          <#elseif surveyQuestionAndAppl.surveyQuestionTypeId == "GIFT_CARD">
            <input type="text" size="20" class="textBox" name="answers_${surveyQuestionAndAppl.surveyQuestionId}" value="${(answer.textResponse)!}"/>
          <#elseif surveyQuestionAndAppl.surveyQuestionTypeId == "NUMBER_CURRENCY">
            <input type="text" size="6" class="textBox" name="answers_${surveyQuestionAndAppl.surveyQuestionId}" value="${(answer.currencyResponse)!}"/>
          <#elseif surveyQuestionAndAppl.surveyQuestionTypeId == "NUMBER_FLOAT">
            <input type="text" size="6" class="textBox" name="answers_${surveyQuestionAndAppl.surveyQuestionId}" value="${(answer.floatResponse)!}"/>
          <#elseif surveyQuestionAndAppl.surveyQuestionTypeId == "NUMBER_LONG">
            <input type="text" size="6" class="textBox" name="answers_${surveyQuestionAndAppl.surveyQuestionId}" value="${(answer.numericResponse?string("#"))!}"/>
          <#elseif surveyQuestionAndAppl.surveyQuestionTypeId == "PASSWORD">
            <input type="password" size="30" class="textBox" name="answers_${surveyQuestionAndAppl.surveyQuestionId}" value="${(answer.textResponse)!}"/>
          <#elseif surveyQuestionAndAppl.surveyQuestionTypeId == "OPTION">
            <#assign options = surveyQuestionAndAppl.getRelated("SurveyQuestionOption", null, sequenceSort, false)!>
            <#assign selectedOption = (answer.surveyOptionSeqId)?default("_NA_")>
            <select name="answers_${surveyQuestionAndAppl.surveyQuestionId}">
              <#if surveyQuestionAndAppl.requiredField?default("N") != "Y">
                <option value=""></option>
              </#if>
              <#if options?has_content>
                <#list options as option>
                  <option value="${option.surveyOptionSeqId}" <#if option.surveyOptionSeqId == selectedOption>selected="selected"</#if>>${option.description!}</option>
                </#list>
              <#else>
                <option value="">Nothing to choose</option>
              </#if>
            </select>
          <#else>
            <div>Unsupported question type : ${surveyQuestionAndAppl.surveyQuestionTypeId}</div>
          </#if>
          <#if surveyQuestionAndAppl.requiredField?default("N") == "Y">
            <span>*</span>
          <#else>
            <span>[optional]</span>
          </#if>
        </@td>

    </@tr>
  </#list>
  <@tr>
    <@td align="center"><input type="submit" value="<#if survey.submitCaption?has_content>${survey.submitCaption}<#else>Submit</#if>" class="${styles.link_run_sys!} ${styles.action_add!}"/></@td>
  </@tr>
</@table>
