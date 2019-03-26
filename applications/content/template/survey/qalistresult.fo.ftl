<#--
This file is subject to the terms and conditions defined in the
files 'LICENSE' and 'NOTICE', which are part of this source
code package.
-->

<#-- SCIPIO: Based on:
        component://shop/templates/survey/genericresult.ftl
    The Shop template (the original) displays for frontend while the Content one displays for backend apps.
    NOTE: There is a second frontend-only template at:
        component://shop/templates/survey/qalistresult.ftl -->

<#if !uiLabelMap??>
  <#assign uiLabelMap = Static["org.ofbiz.base.util.UtilProperties"].getResourceBundleMap("CommonUiLabels", locale)>
</#if>

<#-- Render options -->
<#assign srqaArgs = srqaArgs!{}>
<#assign minOn = srqaArgs.minimal!true>
<#assign unselOn = srqaArgs.showUnsel!(!minOn)>
<#assign statsOn = srqaArgs.stats!(!minOn)>
<#assign maxEntries = srqaArgs.max!-1>
<#assign listClass = srqaArgs.listClass!"">

<#assign maxReached = false>
<#-- SCIPIO: TODO: allow callers to override the list-block attributes (and other attributes) -->
<fo:list-block line-height="10pt" start-indent="2mm" provisional-distance-between-starts="3mm" provisional-label-separation="1mm">
  <#list surveyQuestionAndAppls as surveyQuestionAndAppl>
    <#if (surveyQuestionAndAppl?index == maxEntries)>
      <fo:list-item>
        <fo:list-item-label end-indent="label-end()"><fo:block><fo:inline font-family="Symbol">&#x2022;</fo:inline></fo:block></fo:list-item-label>
        <fo:list-item-body start-indent="body-start()"><fo:block>...</fo:block></fo:list-item-body>
      </fo:list-item>
      <#break/>
    </#if>

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

      <#-- seperator options -->
      <#if surveyQuestionAndAppl.surveyQuestionTypeId == "SEPERATOR_TEXT">
        <#--<li>${surveyQuestionAndAppl.question!}</li>-->
      <#elseif surveyQuestionAndAppl.surveyQuestionTypeId == "SEPERATOR_LINE">
        <#--<li><hr /></li>-->
      <#else>

    <fo:list-item>
      <fo:list-item-label end-indent="label-end()"><fo:block><fo:inline font-family="Symbol">&#x2022;</fo:inline></fo:block></fo:list-item-label>
      <fo:list-item-body start-indent="body-start()">
        <fo:block>

        <#-- standard question options -->
        <fo:inline>
          <#assign answerString = "answers">
          <#if ((results._total!0) == 1)>
             <#assign answerString = "answer">
          </#if>
          ${surveyQuestionAndAppl.question!}<#if unselOn> (${results._total!0?string.number} ${answerString})</#if>
          <#if surveyQuestionAndAppl.hint?has_content>
            ${surveyQuestionAndAppl.hint}
          </#if>
        </fo:inline>

        <fo:inline>
          <#if surveyQuestionAndAppl.surveyQuestionTypeId == "BOOLEAN">
            <#assign selectedOption = raw((answer.booleanResponse)!"Y")>
            <#if unselOn>
              <fo:inline<#if "Y" == selectedOption> font-weight="bold"</#if>>
                <#if "Y" == selectedOption><strong class="survres-selected">${uiLabelMap.CommonY}</strong><#else>${uiLabelMap.CommonY}</#if><#rt/>
                <#lt/><#if statsOn>&nbsp;[${results._yes_total!0?string("#")} / ${results._yes_percent!0?string("#")}%]</#if>
              </fo:inline>
              <fo:inline<#if "Y" == selectedOption> font-weight="bold"</#if>>
                <#if "N" == selectedOption><strong class="survres-selected">${uiLabelMap.CommonN}</strong><#else>${uiLabelMap.CommonN}</#if><#rt/>
                <#lt/><#if statsOn>&nbsp;[${results._no_total!0?string("#")} / ${results._no_percent!0?string("#")}%]</#if>
              </fo:inline>
            <#else>
              ${selectedOption}
            </#if>
          <#elseif surveyQuestionAndAppl.surveyQuestionTypeId == "TEXTAREA">
            ${(answer.textResponse)!}
          <#elseif surveyQuestionAndAppl.surveyQuestionTypeId == "TEXT_SHORT">
            ${(answer.textResponse)!}
          <#elseif surveyQuestionAndAppl.surveyQuestionTypeId == "TEXT_LONG">
            ${(answer.textResponse)!}
          <#elseif surveyQuestionAndAppl.surveyQuestionTypeId == "EMAIL">
            ${(answer.textResponse)!}
          <#elseif surveyQuestionAndAppl.surveyQuestionTypeId == "URL">
            ${(answer.textResponse)!}
          <#elseif surveyQuestionAndAppl.surveyQuestionTypeId == "DATE">
            ${(answer.textResponse)!}
          <#elseif surveyQuestionAndAppl.surveyQuestionTypeId == "CREDIT_CARD">
            ${(answer.textResponse)!}
          <#elseif surveyQuestionAndAppl.surveyQuestionTypeId == "GIFT_CARD">
            ${(answer.textResponse)!}
          <#elseif surveyQuestionAndAppl.surveyQuestionTypeId == "NUMBER_CURRENCY">
            ${answer.currencyResponse!0?number}
          <#elseif surveyQuestionAndAppl.surveyQuestionTypeId == "NUMBER_FLOAT">
            ${answer.floatResponse!0?number?string("#")}
          <#elseif surveyQuestionAndAppl.surveyQuestionTypeId == "NUMBER_LONG">
            ${answer.numericResponse!0?number?string("#")}<#rt/>
            <#lt/><#if statsOn>&nbsp;[${uiLabelMap.CommonTally}: ${results._tally!0?string("#")} / ${uiLabelMap.CommonAverage}: ${results._average!0?string("#")}]</#if>
          <#elseif surveyQuestionAndAppl.surveyQuestionTypeId == "PASSWORD">
            [${uiLabelMap.CommonNotShown}]
          <#elseif surveyQuestionAndAppl.surveyQuestionTypeId == "CONTENT">
            <#-- SCIPIO: TODO?
            <#if answer.contentId?has_content>
              <#assign content = answer.getRelatedOne("Content", false)>
              <a href="<@serverUrl>/content/control/img?imgId=${content.dataResourceId}</@serverUrl>" class="${styles.link_nav_info_id!}">${answer.contentId}</a>&nbsp;-&nbsp;${content.contentName!}
            </#if>-->
          <#elseif surveyQuestionAndAppl.surveyQuestionTypeId == "OPTION">
            <#assign options = surveyQuestionAndAppl.getRelated("SurveyQuestionOption", null, sequenceSort, false)!>
            <#assign selectedOption = raw((answer.surveyOptionSeqId)!"_NA_")>
            <#if options?has_content>
              <#list options as option>
                <#assign optionSeqId = raw(option.surveyOptionSeqId!)><#-- SCIPIO: Refactored + fixed escaping -->
                <#assign optionResults = results.get(optionSeqId)!>
                  <#if !unselOn || optionSeqId == selectedOption>
                    <fo:inline<#if "Y" == selectedOption> font-weight="bold"</#if>>
                      ${option.description!}
                      <#if statsOn>&nbsp;[${optionResults._total!0?string("#")} / ${optionResults._percent!0?string("#")}%]</#if>
                    </fo:inline>
                  </#if>
              </#list>
            </#if>
          <#else>
            ${uiLabelMap.EcommerceUnsupportedQuestionType}: ${surveyQuestionAndAppl.surveyQuestionTypeId}
          </#if>
        </fo:inline>

        </fo:block>
      </fo:list-item-body>
    </fo:list-item>

      </#if>
  </#list>
</fo:list-block>
