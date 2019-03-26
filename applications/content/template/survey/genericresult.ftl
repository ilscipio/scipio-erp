<#--
This file is subject to the terms and conditions defined in the
files 'LICENSE' and 'NOTICE', which are part of this source
code package.
-->

<#-- SCIPIO: Duplicated from:
        component://shop/templates/survey/genericresult.ftl
    The Shop template (the original) displays for frontend while the Content one displays for backend apps. -->

<#if !uiLabelMap??>
  <#assign uiLabelMap = Static["org.ofbiz.base.util.UtilProperties"].getResourceBundleMap("CommonUiLabels", locale)>
</#if>

<@heading>${survey.get("description", locale)!}</@heading>

<@table type="data-complex">
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
        <@td colspan="4">${surveyQuestionAndAppl.question!}</@td>
      <#elseif surveyQuestionAndAppl.surveyQuestionTypeId == "SEPERATOR_LINE">
        <@td colspan="4"><hr /></@td>
      <#else>

        <#-- standard question options -->
        <@td align='right' nowrap="nowrap" width="30%"><#-- SCIPIO: Set 30% here (instead of the 90% further below) -->
          <#assign answerString = "answers">
          <#if ((results._total!0) == 1)>
             <#assign answerString = "answer">
          </#if>
          <div>${surveyQuestionAndAppl.question!} (${results._total!0?string.number} ${answerString})</div>
          <#if surveyQuestionAndAppl.hint?has_content>
            <div>${surveyQuestionAndAppl.hint}</div>
          </#if>
        </@td>

        <@td align=align>
          <#if surveyQuestionAndAppl.surveyQuestionTypeId == "BOOLEAN">
            <#assign selectedOption = raw((answer.booleanResponse)!"Y")>
            <div><span style="white-space: nowrap;">
              <#if "Y" == selectedOption><b>==>&nbsp;<font color="red"></#if>${uiLabelMap.CommonY}<#if "Y" == selectedOption></font></b></#if>&nbsp;[${results._yes_total!0?string("#")} / ${results._yes_percent!0?string("#")}%]
            </span></div>
            <div><span style="white-space: nowrap;">
              <#if "N" == selectedOption><b>==>&nbsp;<font color="red"></#if>N<#if "N" == selectedOption></font></b></#if>&nbsp;[${results._no_total!0?string("#")} / ${results._no_percent!0?string("#")}%]
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
            <div>${answer.currencyResponse!0?number}</div>
          <#elseif surveyQuestionAndAppl.surveyQuestionTypeId == "NUMBER_FLOAT">
            <div>${answer.floatResponse!0?number?string("#")}</div>
          <#elseif surveyQuestionAndAppl.surveyQuestionTypeId == "NUMBER_LONG">
            <div>${answer.numericResponse!0?number?string("#")}&nbsp;[${uiLabelMap.CommonTally}: ${results._tally!0?string("#")} / ${uiLabelMap.CommonAverage}: ${results._average!0?string("#")}]</div>
          <#elseif surveyQuestionAndAppl.surveyQuestionTypeId == "PASSWORD">
            <div>[${uiLabelMap.CommonNotShown}]</div>
          <#elseif surveyQuestionAndAppl.surveyQuestionTypeId == "CONTENT">
            <#if answer.contentId?has_content>
              <#assign content = answer.getRelatedOne("Content", false)>
              <a href="<@serverUrl>/content/control/img?imgId=${content.dataResourceId}</@serverUrl>" class="${styles.link_nav_info_id!}">${answer.contentId}</a>&nbsp;-&nbsp;${content.contentName!}
            </#if>
          <#elseif surveyQuestionAndAppl.surveyQuestionTypeId == "OPTION">
            <#assign options = surveyQuestionAndAppl.getRelated("SurveyQuestionOption", null, sequenceSort, false)!>
            <#assign selectedOption = raw((answer.surveyOptionSeqId)!"_NA_")>
            <#if options?has_content>
              <#list options as option>
                <#assign optionSeqId = raw(option.surveyOptionSeqId!)><#-- SCIPIO: Refactored + fixed escaping -->
                <#assign optionResults = results.get(optionSeqId)!>
                  <div><span style="white-space: nowrap;">
                    <#if optionSeqId == selectedOption><b>==>&nbsp;<font color="red"></#if>
                    ${option.description!}
                    <#if optionSeqId == selectedOption></font></b></#if>
                    &nbsp;[${optionResults._total!0?string("#")} / ${optionResults._percent!0?string("#")}%]
                  </span></div>
              </#list>
            </#if>
          <#else>
            <div>${uiLabelMap.EcommerceUnsupportedQuestionType}: ${surveyQuestionAndAppl.surveyQuestionTypeId}</div>
          </#if>
        </@td>
        <#-- SCIPIO: I fail to see how this helps anything
        <@td width="90%">&nbsp;</@td>-->
      </#if>
    </@tr>
  </#list>
</@table>
