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

<@section title="${uiLabelMap.CommonList} ${uiLabelMap.EcommerceSurveys}">
  <@fields type="default-manual-widgetonly">
    <#if workEffortSurveyAppls?has_content>
      <@table type="data-list"> <#-- orig: class="basic-table hover-bar" --> <#-- orig: cellspacing="0" -->
      <@thead>
        <@tr class="header-row">
          <@th>${uiLabelMap.ContentSurveySurveyId}</@th>
          <@th>${uiLabelMap.CommonFromDateTime}</@th>
          <@th>${uiLabelMap.CommonThruDateTime}</@th>
          <@th>&nbsp;</@th>
          <@th>&nbsp;</@th>
          <@th>&nbsp;</@th>
        </@tr>
        </@thead>
        <#list workEffortSurveyAppls as workEffortSurveyAppl>
          <#if workEffortSurveyAppl?has_content>
            <#assign productStoreSurveyAppls = workEffortSurveyAppl.getRelated("ProductStoreSurveyAppl", null, null, false)>
            <#list productStoreSurveyAppls as productStoreSurveyAppl>
              <#if productStoreSurveyAppl?has_content>
                <#assign survey = productStoreSurveyAppl.getRelatedOne("Survey", false)>
                <@tr>
                <#-- Cato: FIXME: invalid form within table -->
                <form method="post" action="<@ofbizUrl>updateWorkEffortSurveyAppl</@ofbizUrl>" name="editWorkEffortSurveyAppl_${workEffortSurveyAppl_index}">
                  <@td><a href="<@ofbizInterWebappUrl>/content/control/EditSurvey?surveyId=${workEffortSurveyAppl.surveyId!}</@ofbizInterWebappUrl>" class="${styles.link_nav_info_idname!}">${workEffortSurveyAppl.surveyId!} - ${survey.surveyName!}</a></@td>
                  <@td>${workEffortSurveyAppl.fromDate!}</@td>
                  <@td>
                    <@field type="datetime" name="thruDate" value=((workEffortSurveyAppl.thruDate)!) size="25" maxlength="30" id="thruDate1" />
                  </@td>
                  <@td><a href="<@ofbizUrl>testWorkEffortSurvey?productStoreSurveyId=${productStoreSurveyAppl.productStoreSurveyId!}&amp;workEffortId=${workEffortSurveyAppl.workEffortId!}</@ofbizUrl>" class="${styles.link_nav!} ${styles.action_update!}">${uiLabelMap.EcommerceTakeSurvey}</a></@td>
                  <#if !isReadable??>
                    <input type="hidden" name="surveyId" value="${workEffortSurveyAppl.surveyId!}"/>
                    <input type="hidden" name="workEffortId" value="${workEffortSurveyAppl.workEffortId!}"/>
                    <input type="hidden" name="fromDate" value="${workEffortSurveyAppl.fromDate!}"/>
                    <@td><@field type="submit" name="submitBtn" text=uiLabelMap.CommonUpdate class="${styles.link_run_sys!} ${styles.action_update!}" /></@td>
                </form>
                    <@td>
                      <form id="deleteWorkEffortSurveyAppl_${workEffortSurveyAppl_index}" method="post" action="<@ofbizUrl>deleteWorkEffortSurveyAppl</@ofbizUrl>">
                        <input type="hidden" name="surveyId" value="${workEffortSurveyAppl.surveyId!}" />
                        <input type="hidden" name="workEffortId" value="${workEffortSurveyAppl.workEffortId!}" />
                        <input type="hidden" name="fromDate" value="${workEffortSurveyAppl.fromDate!}" />
                        <@field type="submit" submitType="link" href="javascript:document.getElementById('deleteWorkEffortSurveyAppl_${workEffortSurveyAppl_index}').submit()" class="${styles.link_run_sys!} ${styles.action_remove!}" text="${uiLabelMap.CommonDelete}" />
                      </form>
                    </@td>
                  </#if>
                </@tr>
              </#if>
            </#list>
          </#if>
        </#list>
      </@table>
    </#if>
  </@fields>
</@section>
