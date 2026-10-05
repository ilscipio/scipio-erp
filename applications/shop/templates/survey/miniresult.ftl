<#--
Scipio Commerce
Copyright (C) Ilscipio GmbH

This file is part of Scipio Commerce. Scipio Commerce is free software: you
can redistribute it and modify it under the terms of the GNU Affero General
Public License, version 3, as published by the Free Software Foundation.
Scipio Commerce is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License
for more details. You should have received a copy of the license with this
work (file LICENSE). If not, see <https://www.gnu.org/licenses/agpl-3.0.html>.
A commercial license is available from Ilscipio GmbH.

SPDX-License-Identifier: AGPL-3.0-only
-->

<@table type="data-complex">
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
