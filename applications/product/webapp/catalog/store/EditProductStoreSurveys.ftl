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
<@section title="${uiLabelMap.PageTitleEditProductStoreSurveys}">
    <@table type="data-list" autoAltRows=true cellspacing="0"> <#-- orig: class="basic-table" -->
          <@thead>
            <@tr class="header-row">
              <@th>${uiLabelMap.CommonType}</@th>
              <@th>${uiLabelMap.CommonName}</@th>
              <@th>${uiLabelMap.CommonSurveys}</@th>
              <@th>${uiLabelMap.ProductProduct}</@th>
              <@th>${uiLabelMap.ProductCategory}</@th>
              <@th>${uiLabelMap.CommonFromDate}</@th>
              <@th>${uiLabelMap.CommonSequenceNum}</@th>
              <@th>&nbsp;</@th>
            </@tr>
          </@thead>
            <#list productStoreSurveys as storeSurvey>
              <#assign surveyType = storeSurvey.getRelatedOne("SurveyApplType", false)>
              <#assign survey = storeSurvey.getRelatedOne("Survey", false)>
              <@tr valign="middle">
                <@td>${surveyType.get("description",locale)}</@td>
                <@td>${storeSurvey.groupName!}</@td>
                <@td><a href="/content/control/EditSurvey?surveyId=${storeSurvey.surveyId}" class="${styles.link_desc!}">${survey.description?default("[" + survey.surveyId + "]")}</a></@td>
                <@td>${storeSurvey.productId?default("${uiLabelMap.CommonNA}")}</@td>
                <@td>${storeSurvey.productCategoryId?default("${uiLabelMap.CommonNA}")}</@td>
                <@td>${storeSurvey.fromDate!?string}</@td>
                <@td>${storeSurvey.sequenceNum!}</@td>
                <@td>
                  <form name="deleteProductStoreSurveyAppl_${storeSurvey_index}" method="post" action="<@ofbizUrl>deleteProductStoreSurveyAppl</@ofbizUrl>">
                    <input type="hidden" name="productStoreId" value="${productStoreId}" />
                    <input type="hidden" name="productStoreSurveyId" value="${storeSurvey.productStoreSurveyId}" />
                    <a href="javascript:document.deleteProductStoreSurveyAppl_${storeSurvey_index}.submit()" class="${styles.link_action!}">${uiLabelMap.CommonDelete}</a>
                  </form>
                </@td>
              </@tr>
            </#list>
    </@table>
</@section>

<@section title="${uiLabelMap.PageTitleAddProductStoreSurveys}">
        <form name="addSurvey" action="<@ofbizUrl>createProductStoreSurveyAppl</@ofbizUrl>" method="post">
            <input type="hidden" name="productStoreId" value="${productStoreId}" />
              <@field type="generic" label="${uiLabelMap.CommonType}">
                  <select name="surveyApplTypeId">
                    <#list surveyApplTypes as type>
                      <option value="${type.surveyApplTypeId}">${type.get("description",locale)}</option>
                    </#list>
                  </select>
              </@field>
              <@field type="generic" label="${uiLabelMap.CommonGroup} ${uiLabelMap.CommonName}">
                  <input type="text" size="20" name="groupName" />
              </@field>
              <@field type="generic" label="${uiLabelMap.CommonSurveys}">
                  <select name="surveyId">
                    <#list surveys as survey>
                      <option value="${survey.surveyId}">${survey.description?default("[" + survey.surveyId + "]")}</option>
                    </#list>
                  </select>
              </@field>
              <@field type="generic" label="${uiLabelMap.ProductProductId}">
                  <input type="text" size="20" name="productId" />
              </@field>
              <@field type="generic" label="${uiLabelMap.ProductCategoryId}">
                  <@htmlTemplate.lookupField formName="addSurvey" name="productCategoryId" id="productCategoryId" fieldFormName="LookupProductCategory"/>
              </@field>
              <@field type="generic" label="${uiLabelMap.CommonFromDate}">
                  <@htmlTemplate.renderDateTimeField name="fromDate" event="" action="" className=""  title="Format: yyyy-MM-dd HH:mm:ss.SSS" value="" size="25" maxlength="30" id="fromDate1" dateType="date" shortDateInput=false timeDropdownParamName="" defaultDateTimeString="" localizedIconTitle="" timeDropdown="" timeHourName="" classString="" hour1="" hour2="" timeMinutesName="" minutes="" isTwelveHour="" ampmName="" amSelected="" pmSelected="" compositeType="" formName=""/>
              </@field>
              <@field type="generic" label="${uiLabelMap.CommonThruDate}">
                  <@htmlTemplate.renderDateTimeField name="thruDate" event="" action="" className=""  title="Format: yyyy-MM-dd HH:mm:ss.SSS" value="" size="25" maxlength="30" id="thruDate1" dateType="date" shortDateInput=false timeDropdownParamName="" defaultDateTimeString="" localizedIconTitle="" timeDropdown="" timeHourName="" classString="" hour1="" hour2="" timeMinutesName="" minutes="" isTwelveHour="" ampmName="" amSelected="" pmSelected="" compositeType="" formName=""/>
              </@field>
              <@field type="generic" label="${uiLabelMap.ProductStoreSurveyTemplatePath}">
                  <input type="text" size="30" name="surveyTemplate" />
              </@field>
              <@field type="generic" label="${uiLabelMap.ProductStoreSurveyResultTemplatePath}">
                  <input type="text" size="30" name="resultTemplate" />
              </@field>
              <@field type="generic" label="${uiLabelMap.CommonSequenceNum}">
                  <input type="text" size="5" name="sequenceNum" />
              </@field>
              <@field type="submitarea">
                  <input type="submit" class="${styles.link_action!}" value="${uiLabelMap.CommonAdd}" />
              </@field>
        </form>
</@section>