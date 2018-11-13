<#--
This file is subject to the terms and conditions defined in the
files 'LICENSE' and 'NOTICE', which are part of this source
code package.
-->

<@table type="data-complex"> <#-- orig: width="100%" border="0" cellpadding="2" cellspacing="0" -->
  <#list surveyQuestionAndAppls as surveyQuestionAndAppl>

    <#-- get an answer from the answerMap -->
    <#if surveyAnswers?has_content>
      <#assign answer = surveyAnswers.get(surveyQuestionAndAppl.surveyQuestionId)!>
    </#if>

    <#-- get the question results -->
    <#if surveyResults?has_content>
      <#assign results = surveyResults.get(surveyQuestionAndAppl.surveyQuestionId)!>
    </#if>

    <@tr>
      <#-- standard question options -->
      <@td align='left'>
        <#assign answerString = "answers">
        <#if (results._total?default(0) == 1)>
           <#assign answerString = "answer">
        </#if>
        <div>${surveyQuestionAndAppl.question!} (${results._total?default(0)?string.number} ${answerString})</div>
      </@td>
    </@tr>

    <@tr type="util">
      <@td><hr /></@td>
    </@tr>

    <@tr>
      <@td>
        <#if surveyQuestionAndAppl.surveyQuestionTypeId == "BOOLEAN">
          <#assign selectedOption = (answer.booleanResponse)?default("Y")>
          <div><span style="white-space: nowrap;">
            <#if "Y" == selectedOption><b>==>&nbsp;<font color="red"></#if>${uiLabelMap.CommonY}<#if "Y" == selectedOption></font></b></#if>&nbsp;[${results._yes_total?default(0)?string("#")} / ${results._yes_percent?default(0)?string("#")}%]
          </span></div>
          <div><span style="white-space: nowrap;">
            <#if "N" == selectedOption><b>==>&nbsp;<font color="red"></#if>${uiLabelMap.CommonN}<#if "N" == selectedOption></font></b></#if>&nbsp;[${results._no_total?default(0)?string("#")} / ${results._no_percent?default(0)?string("#")}%]
          </span></div>

        <#elseif surveyQuestionAndAppl.surveyQuestionTypeId == "OPTION">
          <#assign options = surveyQuestionAndAppl.getRelated("SurveyQuestionOption", null, sequenceSort, false)!>
          <#assign selectedOption = (answer.surveyOptionSeqId)?default("_NA_")>
          <#if options?has_content>
            <#list options as option>
              <#assign optionResults = results.get(option.surveyOptionSeqId)!>
                <div><span style="white-space: nowrap;">
                  <#if option.surveyOptionSeqId == selectedOption><b>==>&nbsp;<font color="red"></#if>
                  ${option.description!}
                  <#if option.surveyOptionSeqId == selectedOption></font></b></#if>
                  &nbsp;[${optionResults._total?default(0)?string("#")} / ${optionResults._percent?default(0?string("#"))}%]
                </span></div>
            </#list>
          </#if>
        <#else>
          <div>${uiLabelMap.EcommerceUnsupportedQuestionType}${surveyQuestionAndAppl.surveyQuestionTypeId}</div>
        </#if>
      </@td>
    </@tr>
  </#list>
</@table>
