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
<@section title="${uiLabelMap.CommonAdvancedSearch}">

    <form name="advToKeyWordSearchForm" method="post" action="<@ofbizUrl>ContentSearchResults</@ofbizUrl>">
      <#-- Cato: don't hardcode this... use sys default -->
      <#--<input type="hidden" name="VIEW_SIZE" value="25"/>-->
        <@field type="generic" label="${uiLabelMap.ContentKeywords}">
            <input type="text" name="SEARCH_STRING" size="40" value="${requestParameters.SEARCH_STRING!}"/>&nbsp;
              ${uiLabelMap.CommonAny}<input type="radio" name="SEARCH_OPERATOR" value="OR" <#if searchOperator == "OR">checked="checked"</#if>/>
              ${uiLabelMap.CommonAll}<input type="radio" name="SEARCH_OPERATOR" value="AND" <#if searchOperator == "AND">checked="checked"</#if>/>
        </@field>
        <@field type="generic" label="${uiLabelMap.FormFieldTitle_contentId}">
            <@htmlTemplate.lookupField value="${requestParameters.SEARCH_CONTENT_ID!}" formName="advToKeyWordSearchForm" name="SEARCH_CONTENT_ID" id="SEARCH_CONTENT_ID" fieldFormName="LookupContent"/>
        </@field>
        <@field type="generic" label="${uiLabelMap.FormFieldTitle_contentAssocTypeId}">
              <select name="contentAssocTypeId">
                <option value="">- ${uiLabelMap.ContentAnyAssocType} -</option>
                  <#list contentAssocTypes as contentAssocType>
                      <option value="${contentAssocType.contentAssocTypeId}">${contentAssocType.description}</option>
                  </#list>
              </select>
                  ${uiLabelMap.ContentIncludeAllSubContents}?
                  ${uiLabelMap.CommonYes}<input type="radio" name="SEARCH_SUB_CONTENTS" value="Y" checked="checked"/>
                  ${uiLabelMap.CommonNo}<input type="radio" name="SEARCH_SUB_CONTENTS" value="N"/>
        </@field>
        <@field type="generic" label="${uiLabelMap.PartyPartyId}">
            <@htmlTemplate.lookupField value="${requestParameters.partyId!}" formName="advToKeyWordSearchForm" name="partyId" id="partyId" fieldFormName="LookupPartyName"/>
        </@field>
        <@field type="generic" label="${uiLabelMap.PartyRoleTypeId}">
              <select name="partyRoleTypeId">
                <option value="">- ${uiLabelMap.CommonAnyRoleType} -</option>
                <#list roleTypes as roleType>
                   <option value="${roleType.roleTypeId}">${roleType.description}</option>
                 </#list>
              </select>
        </@field>
        <@field type="generic" label="${uiLabelMap.ContentLastUpdatedDateFilter}">
            <@field type="generic" label="${uiLabelMap.CommonFrom}">
                <@htmlTemplate.renderDateTimeField name="fromDate" event="" action="" className=""  title="Format: yyyy-MM-dd HH:mm:ss.SSS" value="${requestParameters.fromDate!}" size="25" maxlength="30" id="fromDate1" dateType="date" shortDateInput=false timeDropdownParamName="" defaultDateTimeString="" localizedIconTitle="" timeDropdown="" timeHourName="" classString="" hour1="" hour2="" timeMinutesName="" minutes="" isTwelveHour="" ampmName="" amSelected="" pmSelected="" compositeType="" formName=""/>
            </@field>
            <@field type="generic" label="${uiLabelMap.CommonThru}">
                <@htmlTemplate.renderDateTimeField name="thruDate" event="" action="" className=""  title="Format: yyyy-MM-dd HH:mm:ss.SSS" value="${requestParameters.thruDate!}" size="25" maxlength="30" id="thruDate1" dateType="date" shortDateInput=false timeDropdownParamName="" defaultDateTimeString="" localizedIconTitle="" timeDropdown="" timeHourName="" classString="" hour1="" hour2="" timeMinutesName="" minutes="" isTwelveHour="" ampmName="" amSelected="" pmSelected="" compositeType="" formName=""/>
            </@field>
        </@field>
        <@field type="generic" label="${uiLabelMap.CommonSortedBy}">
              <select name="sortOrder">
                <option value="SortKeywordRelevancy">${uiLabelMap.ProductKeywordRelevancy}</option>
                <option value="SortContentField:contentName">${uiLabelMap.FormFieldTitle_contentName}</option>
              </select>
              ${uiLabelMap.ProductLowToHigh}<input type="radio" name="sortAscending" value="Y" checked="checked"/>
              ${uiLabelMap.ProductHighToLow}<input type="radio" name="sortAscending" value="N"/>
        </@field>
        <#if searchConstraintStrings?has_content>
          <@field type="generic" label="${uiLabelMap.ProductLastSearch}">
              <#list searchConstraintStrings as searchConstraintString>
                    <div>&nbsp;-&nbsp;${searchConstraintString}</div>
                </#list>
                <div>${uiLabelMap.CommonSortedBy} ${searchSortOrderString}</div>
                <div>
                  ${uiLabelMap.ProductNewSearch}<input type="radio" name="clearSearch" value="Y" checked="checked"/>
                  ${uiLabelMap.CommonRefineSearch}<input type="radio" name="clearSearch" value="N"/>
                </div>
          </@field>
        </#if>
        <@field type="submit" submitType="link" href="javascript:document.advToKeyWordSearchForm.submit()" class="${styles.link_run_sys!} ${styles.action_find!}" text="${uiLabelMap.CommonFind}" />
        <input type="image" src="<@ofbizContentUrl>/images/spacer.gif</@ofbizContentUrl>" onclick="javascript:document.advToKeyWordSearchForm.submit();"/>
    </form>
</@section>
