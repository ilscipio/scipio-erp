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
    <#if workEffortSurveyAppls?has_content>
      <@table type="data-list" class="+hover-bar" cellspacing="0"> <#-- orig: class="basic-table hover-bar" -->
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
                  <form method="post" action="<@ofbizUrl>updateWorkEffortSurveyAppl</@ofbizUrl>" name="editWorkEffortSurveyAppl_${workEffortSurveyAppl_index}">
                  <@td><a href="/content/control/EditSurvey?surveyId=${workEffortSurveyAppl.surveyId!}" class="${styles.link_record_idname!}">${workEffortSurveyAppl.surveyId!} - ${survey.surveyName!}</a></@td>
                  <@td>${workEffortSurveyAppl.fromDate!}</@td>
                  <@td>
                    <@htmlTemplate.renderDateTimeField name="thruDate" event="" action="" className=""  title="Format: yyyy-MM-dd HH:mm:ss.SSS" value="${(workEffortSurveyAppl.thruDate)!}" size="25" maxlength="30" id="thruDate1" dateType="date" shortDateInput=false timeDropdownParamName="" defaultDateTimeString="" localizedIconTitle="" timeDropdown="" timeHourName="" classString="" hour1="" hour2="" timeMinutesName="" minutes="" isTwelveHour="" ampmName="" amSelected="" pmSelected="" compositeType="" formName=""/>
                    </@td>
                  <@td><a href="<@ofbizUrl>testWorkEffortSurvey?productStoreSurveyId=${productStoreSurveyAppl.productStoreSurveyId!}&amp;workEffortId=${workEffortSurveyAppl.workEffortId!}</@ofbizUrl>" class="${styles.link_action!}">${uiLabelMap.EcommerceTakeSurvey}</a></@td>
                  <#if !isReadable??>
                    <input type="hidden" name="surveyId" value="${workEffortSurveyAppl.surveyId!}"/>
                    <input type="hidden" name="workEffortId" value="${workEffortSurveyAppl.workEffortId!}"/>
                    <input type="hidden" name="fromDate" value="${workEffortSurveyAppl.fromDate!}"/>
                    <@td><input type="submit" name="submitBtn" value='${uiLabelMap.CommonUpdate}' /> </@td>
                  </form>
                    <@td>
                      <form id="deleteWorkEffortSurveyAppl_${workEffortSurveyAppl_index}" method="post" action="<@ofbizUrl>deleteWorkEffortSurveyAppl</@ofbizUrl>">
                        <input type="hidden" name="surveyId" value="${workEffortSurveyAppl.surveyId!}" />
                        <input type="hidden" name="workEffortId" value="${workEffortSurveyAppl.workEffortId!}" />
                        <input type="hidden" name="fromDate" value="${workEffortSurveyAppl.fromDate!}" />
                        <a href="javascript:document.getElementById('deleteWorkEffortSurveyAppl_${workEffortSurveyAppl_index}').submit()" class="${styles.link_action!}">${uiLabelMap.CommonDelete}</a>
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
</@section>
