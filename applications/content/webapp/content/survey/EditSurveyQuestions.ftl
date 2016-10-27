<#--
Licensed to the Apache Software Foundation (ASF) under one
or more contributor license agreements.  See the NOTICE file
distributed with this work for additional information
regarding copyright ownership.  The ASF licenses this file
to you under the Apache License, Version 2.0 (the
"License"); you may not use this file except in compliance
with the License.  You may obtain a copy of the License at

http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing,
software distributed under the License is distributed on an
"AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
KIND, either express or implied.  See the License for the
specific language governing permissions and limitations
under the License.
-->
<@section title="${rawLabel('PageTitleEditSurveyQuestions')} ${rawLabel('ContentSurveySurveyId')} ${rawString(surveyId)}">
      <@table type="data-list" autoAltRows=true> <#-- orig: class="basic-table hover-bar" --> <#-- orig: cellspacing="0" -->
        <@thead>
        <@tr class="header-row">
          <@th>${uiLabelMap.CommonId}</@th>
          <@th>${uiLabelMap.CommonType}</@th>
          <@th>${uiLabelMap.ContentSurveryCategory}</@th>
          <@th>${uiLabelMap.CommonDescription}</@th>
          <@th>${uiLabelMap.ContentSurveyQuestion}</@th>
          <@th>${uiLabelMap.CommonPage}</@th>
          <@th>${uiLabelMap.ContentSurveyMultiResp}</@th>
          <@th>${uiLabelMap.ContentSurveyMultiRespColumn}</@th>
          <@th>${uiLabelMap.CommonRequired}</@th>
          <@th>${uiLabelMap.CommonSequenceNum}</@th>
          <@th>${uiLabelMap.ContentSurveyWithQuestion}</@th>
          <@th>${uiLabelMap.ContentSurveyWithOption}</@th>
          <@th>&nbsp;</@th>
          <@th>&nbsp;</@th>
          <@th>&nbsp;</@th>
        </@tr>
        </@thead>
        <@tbody>
        <#list surveyQuestionAndApplList as surveyQuestionAndAppl>
          <#assign questionType = surveyQuestionAndAppl.getRelatedOne("SurveyQuestionType", true)/>
          <#assign questionCat = surveyQuestionAndAppl.getRelatedOne("SurveyQuestionCategory", true)!/>
          <#assign currentSurveyPage = surveyQuestionAndAppl.getRelatedOne("SurveyPage", true)!/>
          <#assign currentSurveyMultiResp = surveyQuestionAndAppl.getRelatedOne("SurveyMultiResp", true)!/>
          <#if currentSurveyMultiResp?has_content>
            <#assign currentSurveyMultiRespColumns = currentSurveyMultiResp.getRelated("SurveyMultiRespColumn", null, null, false)/>
          <#else>
            <#assign currentSurveyMultiRespColumns = []/>
          </#if>
          
            <@tr>
            <form method="post" action="<@ofbizUrl>updateSurveyQuestionAppl</@ofbizUrl>">
              <input type="hidden" name="surveyId" value="${surveyQuestionAndAppl.surveyId}" />
              <input type="hidden" name="surveyQuestionId" value="${surveyQuestionAndAppl.surveyQuestionId}" />
              <input type="hidden" name="fromDate" value="${surveyQuestionAndAppl.fromDate}" />
              <@td>${surveyQuestionAndAppl.surveyQuestionId}</@td>
              <@td>${questionType.get("description",locale)}</@td>
              <@td>${(questionCat.description)!}</@td>
              <@td>${surveyQuestionAndAppl.description!}</@td>
              <@td><input type="text" name="question" size="30" value="${(surveyQuestionAndAppl.question!)?html}" /></@td>
              <@td>
                <select name="surveyPageId">
                  <#if surveyQuestionAndAppl.surveyPageSeqId?has_content>
                    <option value="${surveyQuestionAndAppl.surveyPageSeqId}">${(currentSurveyPage.pageName)!} [${surveyQuestionAndAppl.surveyPageSeqId}]</option>
                    <option value="${surveyQuestionAndAppl.surveyPageSeqId}">----</option>
                  </#if>
                  <option value=""></option>
                  <#list surveyPageList as surveyPage>
                    <option value="${surveyPage.surveyPageSeqId}">${surveyPage.pageName!} [${surveyPage.surveyPageSeqId}]</option>
                  </#list>
                </select>
              </@td>
              <@td>
                <select name="surveyMultiRespId">
                  <#if surveyQuestionAndAppl.surveyMultiRespId?has_content>
                    <option value="${surveyQuestionAndAppl.surveyMultiRespId}">${(currentSurveyMultiResp.multiRespTitle)!} [${surveyQuestionAndAppl.surveyMultiRespId}]</option>
                    <option value="${surveyQuestionAndAppl.surveyMultiRespId}">----</option>
                  </#if>
                  <option value=""></option>
                  <#list surveyMultiRespList as surveyMultiResp>
                    <option value="${surveyMultiResp.surveyMultiRespId}">${surveyMultiResp.multiRespTitle} [${surveyMultiResp.surveyMultiRespId}]</option>
                  </#list>
                </select>
              </@td>
              <#if currentSurveyMultiRespColumns?has_content>
              <@td>
                <select name="surveyMultiRespColId">
                  <#if surveyQuestionAndAppl.surveyMultiRespColId?has_content>
                    <#assign currentSurveyMultiRespColumn = surveyQuestionAndAppl.getRelatedOne("SurveyMultiRespColumn", false)/>
                    <option value="${currentSurveyMultiRespColumn.surveyMultiRespColId}">${(currentSurveyMultiRespColumn.columnTitle)!} [${currentSurveyMultiRespColumn.surveyMultiRespColId}]</option>
                    <option value="${currentSurveyMultiRespColumn.surveyMultiRespColId}">----</option>
                  </#if>
                  <option value=""></option>
                  <#list currentSurveyMultiRespColumns as currentSurveyMultiRespColumn>
                    <option value="${currentSurveyMultiRespColumn.surveyMultiRespColId}">${currentSurveyMultiRespColumn.columnTitle} [${currentSurveyMultiRespColumn.surveyMultiRespColId}]</option>
                  </#list>
                </select>
              </@td>
              <#else>
                <@td><input type="text" name="surveyMultiRespColId" size="4" value="${surveyQuestionAndAppl.surveyMultiRespColId!}"/></@td>
              </#if>
              <@td>
                <select name="requiredField">
                  <option>${surveyQuestionAndAppl.requiredField!"N"}</option>
                  <option value="${surveyQuestionAndAppl.requiredField!"N"}">----</option>
                  <option>Y</option><option>N</option>
                </select>
              </@td>
              <@td><input type="text" name="sequenceNum" size="5" value="${surveyQuestionAndAppl.sequenceNum!}"/></@td>
              <@td><input type="text" name="withSurveyQuestionId" size="5" value="${surveyQuestionAndAppl.withSurveyQuestionId!}"/></@td>
              <@td><input type="text" name="withSurveyOptionSeqId" size="5" value="${surveyQuestionAndAppl.withSurveyOptionSeqId!}"/></@td>
              <@td><input type="submit" value="${uiLabelMap.CommonUpdate}" class="${styles.link_run_sys!} ${styles.action_update!}"/></@td>
              <@td><a href="<@ofbizUrl>EditSurveyQuestions?surveyId=${requestParameters.surveyId}&amp;surveyQuestionId=${surveyQuestionAndAppl.surveyQuestionId}#edit</@ofbizUrl>" class="${styles.link_nav!} ${styles.action_update!}">${uiLabelMap.CommonEdit}&nbsp;${uiLabelMap.ContentSurveyQuestion}</a></@td>
              </form>
              <@td>
                <form id="removeSurveyQuestion_${surveyQuestionAndAppl.surveyQuestionId}" action="<@ofbizUrl>removeSurveyQuestionAppl</@ofbizUrl>" method="post">
                  <input type="hidden" name="surveyId" value="${surveyQuestionAndAppl.surveyId}" />
                  <input type="hidden" name="surveyQuestionId" value="${surveyQuestionAndAppl.surveyQuestionId}" />
                  <input type="hidden" name="fromDate" value="${surveyQuestionAndAppl.fromDate}" />
                  <a href="javascript:document.getElementById('removeSurveyQuestion_${surveyQuestionAndAppl.surveyQuestionId}').submit();" class="${styles.link_run_sys!} ${styles.action_remove!}">${uiLabelMap.CommonRemove}</a>
                </form>
              </@td>
            </@tr>
        </#list>
        </@tbody>
      </@table>
</@section>
<#-- apply question from category -->
<#if surveyQuestionCategory?has_content>
    <@section title="${rawLabel('ContentSurveyApplyQuestionFromCategory')} - ${rawString(surveyQuestionCategory.description!)} [${rawString(surveyQuestionCategory.surveyQuestionCategoryId)}]">
        <a name="appl">
        <@table type="data-list" autoAltRows=true> <#-- orig: class="basic-table hover-bar" --> <#-- orig: cellspacing="0" -->
          <@thead>
            <@tr class="header-row">
                <@th>${uiLabelMap.CommonId}</@th>
                <@th>${uiLabelMap.CommonDescription}</@th>
                <@th>${uiLabelMap.CommonType}</@th>
                <@th>${uiLabelMap.ContentSurveyQuestion}</@th>
                <@th>${uiLabelMap.CommonPage}</@th>
                <@th>${uiLabelMap.ContentSurveyMultiResp}</@th>
                <@th>${uiLabelMap.ContentSurveyMultiRespColumn}</@th>
                <@th>${uiLabelMap.CommonRequired}</@th>
                <@th>${uiLabelMap.CommonSequenceNum}</@th>
                <@th>${uiLabelMap.ContentSurveyWithQuestion}</@th>
                <@th>${uiLabelMap.ContentSurveyWithOption}</@th>
                <@th>&nbsp;</@th>
              </@tr>
            </@thead>
          <#list categoryQuestions as question>
            <#assign questionType = question.getRelatedOne("SurveyQuestionType", false)>
            <form method="post" action="<@ofbizUrl>createSurveyQuestionAppl</@ofbizUrl>">
              <input type="hidden" name="surveyId" value="${requestParameters.surveyId}" />
              <input type="hidden" name="surveyQuestionId" value="${question.surveyQuestionId}" />
              <input type="hidden" name="surveyQuestionCategoryId" value="${requestParameters.surveyQuestionCategoryId}" />
              <@tr>
                <@td><a href="<@ofbizUrl>EditSurveyQuestions?surveyId=${requestParameters.surveyId}&amp;surveyQuestionId=${question.surveyQuestionId}&amp;surveyQuestionCategoryId=${requestParameters.surveyQuestionCategoryId}#edit</@ofbizUrl>" class="${styles.link_nav_info_id!}">${question.surveyQuestionId}</a></@td>
                <@td>${question.description!}</@td>
                <@td>${questionType.get("description",locale)}</@td>
                <@td>${question.question!}</@td>
              <@td>
                <select name="surveyPageId">
                  <option value=""></option>
                  <#list surveyPageList as surveyPage>
                    <option value="${surveyPage.surveyPageSeqId}">${surveyPage.pageName} [${surveyPage.surveyPageSeqId}]</option>
                  </#list>
                </select>
              </@td>
              <@td>
                <select name="surveyMultiRespId">
                  <option value=""></option>
                  <#list surveyMultiRespList as surveyMultiResp>
                    <option value="${surveyMultiResp.surveyMultiRespId}">${surveyMultiResp.multiRespTitle} [${surveyMultiResp.surveyMultiRespId}]</option>
                  </#list>
                </select>
              </@td>
                <@td><input type="text" name="surveyMultiRespColId" size="4"/></@td>
                <@td>
                  <select name="requiredField">
                    <option>N</option>
                    <option>Y</option>
                  </select>
                </@td>
                <@td><input type="text" name="sequenceNum" size="5"/></@td>
                <@td><input type="text" name="withSurveyQuestionId" size="5"/></@td>
                <@td><input type="text" name="withSurveyOptionSeqId" size="5"/></@td>
                <@td><input type="submit" value="${uiLabelMap.CommonApply}" class="${styles.link_run_sys!} ${styles.action_update!}"/></@td>
              </@tr>
            </form>
          </#list>
        </@table>
    </@section>
</#if>
<@section title=uiLabelMap.ContentSurveyApplyQuestionFromCategory>
      <form method="post" action="<@ofbizUrl>EditSurveyQuestions</@ofbizUrl>">
        <input type="hidden" name="surveyId" value="${requestParameters.surveyId}"/>
        <select name="surveyQuestionCategoryId">
          <#list questionCategories as category>
            <option value="${category.surveyQuestionCategoryId}">${category.description?default("??")} [${category.surveyQuestionCategoryId}]</option>
          </#list>
        </select>
        &nbsp;
        <input type="submit" value="${uiLabelMap.CommonApply}" class="${styles.link_run_sys!} ${styles.action_update!}"/>
      </form>
</@section>


  <#-- new question / category -->
  <#if requestParameters.newCategory?default("N") == "Y">
    <#assign sectionTitle = uiLabelMap.ContentSurveyCreateQuestionCategory/>
    <#macro menuContent menuArgs={}>
      <@menu args=menuArgs>  
        <@menuitem type="link" href=makeOfbizUrl("EditSurveyQuestions?surveyId=${requestParameters.surveyId}") text="${rawLabel('CommonNew')} ${rawLabel('ContentSurveyQuestion')}" class="+${styles.action_nav!} ${styles.action_add!}" />
      </@menu>
    </#macro>
  <#else>
    <#if surveyQuestionId?has_content>
      <#assign sectionTitle>${rawLabel('CommonEdit')} ${rawLabel('ContentSurveyQuestion')}</#assign>
      <#macro menuContent menuArgs={}>
        <@menu args=menuArgs>  
          <@menuitem type="link" href=makeOfbizUrl("EditSurveyQuestions?surveyId=${requestParameters.surveyId}") text="${rawLabel('CommonNew')} ${rawLabel('ContentSurveyQuestion')}" class="+${styles.action_nav!} ${styles.action_add!}" />
        </@menu>
      </#macro>
    <#else>
      <#assign sectionTitle = uiLabelMap.ContentSurveyCreateQuestion/>
      <#macro menuContent menuArgs={}>
        <@menu args=menuArgs>  
        </@menu>
      </#macro>
    </#if>

    <#macro menuContent menuArgs={}>
      <@menu args=menuArgs>
        ${menuContent}
        <@menuitem type="link" href=makeOfbizUrl("EditSurveyQuestions?surveyId=${requestParameters.surveyId}&newCategory=Y") text="${rawLabel('CommonNew')} ${rawLabel('ContentSurveyQuestion')} ${rawLabel('ContentSurveryCategory')}" class="+${styles.action_nav!} ${styles.action_add!}" />
      </@menu>
    </#macro>
  </#if>
<@section title=sectionTitle menuContent=menuContent>
  <#if requestParameters.newCategory?default("N") == "Y">
    ${createSurveyQuestionCategoryWrapper.renderFormString(context)}
  <#else>
    ${createSurveyQuestionWrapper.renderFormString(context)}
  </#if>
</@section>

<#if (surveyQuestion?has_content && (surveyQuestion.surveyQuestionTypeId!"") == "OPTION")>
  <@section title="${rawLabel('ContentSurveyOptions')} - ${rawLabel('CommonId')}) ${rawString(surveyQuestion.surveyQuestionId!)}">
    <@table type="data-list" autoAltRows=true> <#-- orig: class="basic-table hover-bar" --> <#-- orig: cellspacing="0" -->
     <@thead>
      <@tr class="header-row">
        <@th>${uiLabelMap.CommonDescription}</@th>
        <@th>${uiLabelMap.CommonSequenceNum}</@th>
        <@th>&nbsp;</@th>
        <@th>&nbsp;</@th>
      </@tr>
      </@thead>
      <#list questionOptions as option>
        <@tr>
          <@td>${option.description!}</@td>
          <@td>${option.sequenceNum!}</@td>
          <@td><a href="<@ofbizUrl>EditSurveyQuestions?surveyId=${requestParameters.surveyId}&amp;surveyQuestionId=${option.surveyQuestionId}&amp;surveyOptionSeqId=${option.surveyOptionSeqId}</@ofbizUrl>" class="${styles.link_nav!} ${styles.action_update!}">${uiLabelMap.CommonEdit}</a></@td>
          <@td>
            <form id="deleteSurveyQuestionOption_${option_index}" action="<@ofbizUrl>deleteSurveyQuestionOption</@ofbizUrl>" method="post">
              <input type="hidden" name="surveyId" value="${requestParameters.surveyId}" />
              <input type="hidden" name="surveyQuestionId" value="${option.surveyQuestionId}" />
              <input type="hidden" name="surveyOptionSeqId" value="${option.surveyOptionSeqId}" />
              <a href="javascript:document.getElementById('deleteSurveyQuestionOption_${option_index}').submit();" class="${styles.link_run_sys!} ${styles.action_remove!}">${uiLabelMap.CommonRemove}</a>
            </form>
          </@td>
        </@tr>
      </#list>
    </@table>
  </@section>

    <#if !surveyQuestionOption?has_content>
      <#assign sectionTitle = uiLabelMap.ContentSurveyCreateQuestionOption/>
      <#macro menuContent menuArgs={}>
        <@menu args=menuArgs>
        </@menu>
      </#macro>
    <#else>
      <#assign sectionTitle = uiLabelMap.ContentSurveyEditQuestionOption/>
      <#macro menuContent menuArgs={}>
        <@menu args=menuArgs>
          <@menuitem type="link" href=makeOfbizUrl("EditSurveyQuestions?surveyId=${requestParameters.surveyId}&surveyQuestionId=${surveyQuestionOption.surveyQuestionId}") text="[${uiLabelMap.CommonNew} ${uiLabelMap.ContentSurveyOption}]" class="+${styles.action_nav!} ${styles.action_add!}" />
        </@menu>
      </#macro>
    </#if>
  <@section title=sectionTitle menuContent=menuContent>
    ${createSurveyOptionWrapper.renderFormString()}
  </@section>
</#if>
