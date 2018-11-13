<#--
This file is subject to the terms and conditions defined in
 file 'LICENSE', which is part of this source code package.
-->

<#assign uiLabelMap = Static["org.ofbiz.base.util.UtilProperties"].getResourceBundleMap("CommonUiLabels", locale)>

<h1>${survey.description!}</h1>
<br />

<@table type="data-complex"> <#-- orig: width="100%" border="0" cellpadding="2" cellspacing="0" -->
  <#list surveyQuestionAndAppls as surveyQuestionAndAppl>

    <#-- special formatting for select boxes -->
    <#assign align = "left">
    <#if (surveyQuestionAndAppl.surveyQuestionTypeId == "BOOLEAN" || surveyQuestionAndAppl.surveyQuestionTypeId == "OPTION")>
      <#assign align = "right">
    </#if>

    <#-- get an answer from the answerMap -->
    <#if surveyAnswers?has_content>
      <#assign answer = surveyAnswers.get(surveyQuestionAndAppl.surveyQuestionId)!>
    </#if>

    <#-- get the question results -->
    <#if surveyResults?has_content>
      <#assign results = surveyResults.get(surveyQuestionAndAppl.surveyQuestionId)!>
    </#if>

    <@tr>

      <#-- seperator options -->
      <#if surveyQuestionAndAppl.surveyQuestionTypeId == "SEPERATOR_TEXT">
        <@td colspan="5">${surveyQuestionAndAppl.question!}</@td>
      <#elseif surveyQuestionAndAppl.surveyQuestionTypeId == "SEPERATOR_LINE">
        <@td colspan="5"><hr /></@td>
      <#else>

        <#-- standard question options -->
        <@td align='right' nowrap="nowrap">
          <#assign answerString = "answers">
          <#if (results._total?default(0) == 1)>
             <#assign answerString = "answer">
          </#if>
          <div>${surveyQuestionAndAppl.question!} (${results._total?default(0)?string.number} ${answerString})</div>
          <#if surveyQuestionAndAppl.hint?has_content>
            <div>${surveyQuestionAndAppl.hint}</div>
          </#if>
        </@td>
        <@td width='1'>&nbsp;</@td>

        <@td align="${align}">
          <#if surveyQuestionAndAppl.surveyQuestionTypeId == "BOOLEAN">
            <#assign selectedOption = (answer.booleanResponse)?default("Y")>
            <div><span style="white-space: nowrap;">
              <#if "Y" == selectedOption><b>==>&nbsp;<font color="red"></#if>${uiLabelMap.CommonY}<#if "Y" == selectedOption></font></b></#if>&nbsp;[${results._yes_total?default(0)?string("#")} / ${results._yes_percent?default(0)?string("#")}%]
            </span></div>
            <div><span style="white-space: nowrap;">
              <#if "N" == selectedOption><b>==>&nbsp;<font color="red"></#if>N<#if "N" == selectedOption></font></b></#if>&nbsp;[${results._no_total?default(0)?string("#")} / ${results._no_percent?default(0)?string("#")}%]
            </span></div>
          <#elseif surveyQuestionAndAppl.surveyQuestionTypeId == "TEXTAREA">
            <div>${(answer.textResponse)!}</div>
          <#elseif surveyQuestionAndAppl.surveyQuestionTypeId == "TEXT_SHORT">
            <div>${(answer.textResponse)!}</div>
          <#elseif surveyQuestionAndAppl.surveyQuestionTypeId == "TEXT_LONG">
            <div>${(answer.textResponse)!}</div>
          <#elseif surveyQuestionAndAppl.surveyQuestionTypeId == "EMAIL">
            <div>${(answer.textResponse)!}</div>
          <#elseif surveyQuestionAndAppl.surveyQuestionTypeId == "URL">
            <div>${(answer.textResponse)!}</div>
          <#elseif surveyQuestionAndAppl.surveyQuestionTypeId == "DATE">
            <div>${(answer.textResponse)!}</div>
          <#elseif surveyQuestionAndAppl.surveyQuestionTypeId == "CREDIT_CARD">
            <div>${(answer.textResponse)!}</div>
          <#elseif surveyQuestionAndAppl.surveyQuestionTypeId == "GIFT_CARD">
            <div>${(answer.textResponse)!}</div>
          <#elseif surveyQuestionAndAppl.surveyQuestionTypeId == "NUMBER_CURRENCY">
            <div>${answer.currencyResponse?number?default(0)}</div>
          <#elseif surveyQuestionAndAppl.surveyQuestionTypeId == "NUMBER_FLOAT">
            <div>${answer.floatResponse?number?default(0)?string("#")}</div>
          <#elseif surveyQuestionAndAppl.surveyQuestionTypeId == "NUMBER_LONG">
            <div>${answer.numericResponse?number?default(0)?string("#")}&nbsp;[${uiLabelMap.CommonTally}: ${results._tally?default(0)?string("#")} / ${uiLabelMap.CommonAverage}: ${results._average?default(0)?string("#")}]</div>
          <#elseif surveyQuestionAndAppl.surveyQuestionTypeId == "PASSWORD">
            <div>[${uiLabelMap.CommonNotShown}]</div>
          <#elseif surveyQuestionAndAppl.surveyQuestionTypeId == "CONTENT">
            <#if answer.contentId?has_content>
              <#assign content = answer.getRelatedOne("Content", false)>
              <a href="<@ofbizInterWebappUrl>/content/control/img?imgId=${content.dataResourceId}</@ofbizInterWebappUrl>" class="${styles.link_nav_info_id!}">${answer.contentId}</a>&nbsp;-&nbsp;${content.contentName!}
            </#if>
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
            <div>${uiLabelMap.EcommerceUnsupportedQuestionType}: ${surveyQuestionAndAppl.surveyQuestionTypeId}</div>
          </#if>
        </@td>
        <@td width="90%">&nbsp;</@td>
      </#if>
    </@tr>
  </#list>
</@table>
