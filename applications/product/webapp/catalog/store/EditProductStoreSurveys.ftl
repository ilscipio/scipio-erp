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
<@section title=uiLabelMap.PageTitleEditProductStoreSurveys>
    <@table type="data-list" autoAltRows=true> <#-- orig: class="basic-table" --> <#-- orig: cellspacing="0" -->
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
                <@td><a href="<@ofbizInterWebappUrl>/content/control/EditSurvey?surveyId=${storeSurvey.surveyId}</@ofbizInterWebappUrl>" class="${styles.link_nav_info_desc!}">${survey.description?default("[" + survey.surveyId + "]")}</a></@td>
                <@td>${storeSurvey.productId?default("${uiLabelMap.CommonNA}")}</@td>
                <@td>${storeSurvey.productCategoryId?default("${uiLabelMap.CommonNA}")}</@td>
                <@td>${storeSurvey.fromDate!?string}</@td>
                <@td>${storeSurvey.sequenceNum!}</@td>
                <@td>
                  <form name="deleteProductStoreSurveyAppl_${storeSurvey_index}" method="post" action="<@ofbizUrl>deleteProductStoreSurveyAppl</@ofbizUrl>">
                    <input type="hidden" name="productStoreId" value="${productStoreId}" />
                    <input type="hidden" name="productStoreSurveyId" value="${storeSurvey.productStoreSurveyId}" />
                    <a href="javascript:document.deleteProductStoreSurveyAppl_${storeSurvey_index}.submit()" class="${styles.link_run_sys!} ${styles.action_remove!}">${uiLabelMap.CommonDelete}</a>
                  </form>
                </@td>
              </@tr>
            </#list>
    </@table>
</@section>

<@section title=uiLabelMap.PageTitleAddProductStoreSurveys>
        <form name="addSurvey" action="<@ofbizUrl>createProductStoreSurveyAppl</@ofbizUrl>" method="post">
            <input type="hidden" name="productStoreId" value="${productStoreId}" />
              <@field type="select" label=uiLabelMap.CommonType name="surveyApplTypeId">
                    <#list surveyApplTypes as type>
                      <option value="${type.surveyApplTypeId}">${type.get("description",locale)}</option>
                    </#list>
              </@field>
              <@field type="input" label="${rawString(uiLabelMap.CommonGroup)} ${rawString(uiLabelMap.CommonName)}" size="20" name="groupName" />
              <@field type="select" label=uiLabelMap.CommonSurveys name="surveyId">
                    <#list surveys as survey>
                      <option value="${survey.surveyId}">${survey.description?default("[" + survey.surveyId + "]")}</option>
                    </#list>
              </@field>
              <@field type="input" label=uiLabelMap.ProductProductId size="20" name="productId" />
              <@field type="lookup" label=uiLabelMap.ProductCategoryId formName="addSurvey" name="productCategoryId" id="productCategoryId" fieldFormName="LookupProductCategory"/>
              <@field type="datetime" label=uiLabelMap.CommonFromDate name="fromDate" value="" size="25" maxlength="30" id="fromDate1"/>
              <@field type="datetime" label=uiLabelMap.CommonThruDate name="thruDate" value="" size="25" maxlength="30" id="thruDate1"/>
              <@field type="input" label=uiLabelMap.ProductStoreSurveyTemplatePath size="30" name="surveyTemplate" />
              <@field type="input" label=uiLabelMap.ProductStoreSurveyResultTemplatePath size="30" name="resultTemplate" />
              <@field type="input" label=uiLabelMap.CommonSequenceNum size="5" name="sequenceNum" />
              <@field type="submit" class="+${styles.link_run_sys!} ${styles.action_add!}" text=uiLabelMap.CommonAdd />
        </form>
</@section>