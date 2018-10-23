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
<@section title=uiLabelMap.CommonAdvancedSearch>
    <form name="advToKeyWordSearchForm" method="post" action="<@ofbizUrl>WorkEffortSearchResults</@ofbizUrl>">
        <#-- SCIPIO: don't hardcode
        <input type="hidden" name="VIEW_SIZE" value="25"/>-->
        <@field type="generic" label=uiLabelMap.WorkEffortKeywords>
            <@field type="input" name="SEARCH_STRING" size="40" value=(requestParameters.SEARCH_STRING!)/>&nbsp;
            <@field type="radio" name="SEARCH_OPERATOR" value="OR" checked=(searchOperator == "OR") label=uiLabelMap.CommonAny/>
            <@field type="radio" name="SEARCH_OPERATOR" value="AND" checked=(searchOperator == "AND") label=uiLabelMap.CommonAll/>
        </@field>
        <@field type="input" label=uiLabelMap.WorkEffortReviews name="SEARCH_STRING_REVIEW_TEXT" size="40" value=(requestParameters.SEARCH_STRING_REVIEW_TEXT!) />
        <@field type="lookup" label=uiLabelMap.FormFieldTitle_workEffortId value=(requestParameters.SEARCH_WORK_EFFORT_ID!) formName="advToKeyWordSearchForm" name="SEARCH_WORK_EFFORT_ID" id="SEARCH_WORK_EFFORT_ID" fieldFormName="LookupWorkEffort"/>
        <@field type="generic" label=uiLabelMap.FormFieldTitle_workEffortAssocTypeId>
            <@field type="select" name="workEffortAssocTypeId">
                 <option value="">- ${uiLabelMap.WorkEffortAnyAssocType} -</option>
               <#list workEffortAssocTypes as workEffortAssocType>
                 <option value="${workEffortAssocType.workEffortAssocTypeId}">${workEffortAssocType.description}</option>
               </#list>
            </@field>
            ${uiLabelMap.WorkEffortIncludeAllSubWorkEfforts}?
            <@field type="radio" name="SEARCH_SUB_WORK_EFFORTS" value="Y" checked=true label=uiLabelMap.CommonYes/>
            <@field type="radio" name="SEARCH_SUB_WORK_EFFORTS" value="N" label=uiLabelMap.CommonNo/>
        </@field>
        <@field type="lookup" label=uiLabelMap.PartyPartyId value=(requestParameters.partyId!) formName="advToKeyWordSearchForm" name="partyId" id="partyId" fieldFormName="LookupPartyName"/>
        <@field type="select" label=uiLabelMap.PartyRoleTypeId name="partyRoleTypeId">
            <option value="">- ${uiLabelMap.CommonAnyRoleType} -</option>
          <#list roleTypes as roleType>
            <option value="${roleType.roleTypeId}">${roleType.description}</option>
          </#list>
        </@field>
        <@field type="lookup" label=uiLabelMap.WorkEffortProductId1 value=(requestParameters.productId_1!) formName="advToKeyWordSearchForm" name="productId_1" id="productId_1" fieldFormName="LookupProduct"/>
        <@field type="lookup" label=uiLabelMap.WorkEffortProductId2 value=(requestParameters.productId_2!) formName="advToKeyWordSearchForm" name="productId_2" id="productId_2" fieldFormName="LookupProduct"/>
        <@field type="generic" label=uiLabelMap.WorkEffortLastUpdatedDateFilter>
            <@field type="datetime" label=uiLabelMap.CommonFrom name="fromDate" value=(requestParameters.fromDate!) size="25" maxlength="30" id="fromDate1"/>
            <@field type="datetime" label=uiLabelMap.CommonThru name="thruDate" value=(requestParameters.thruDate!) size="25" maxlength="30" id="thruDate1"/>
        </@field>
        <@field type="generic" label=uiLabelMap.CommonSortedBy>
            <@field type="select" name="sortOrder">
                <option value="SortKeywordRelevancy">${uiLabelMap.ProductKeywordRelevancy}</option>
                <option value="SortWorkEffortField:workEffortName">${uiLabelMap.WorkEffortName}</option>
            </@field>
            <@field type="radio" name="sortAscending" value="Y" checked=true label=uiLabelMap.ProductLowToHigh/>
            <@field type="radio" name="sortAscending" value="N" label=uiLabelMap.ProductHighToLow/>
        </@field>
        <#if searchConstraintStrings?has_content>
          <@field type="generic" label=uiLabelMap.ProductLastSearch>
              <#list searchConstraintStrings as searchConstraintString>
                    <div>&nbsp;-&nbsp;${searchConstraintString}</div>
                </#list>
                <div>${uiLabelMap.CommonSortedBy} ${searchSortOrderString}</div>
                <div>
                  <@field type="radio" name="clearSearch" value="Y" checked=true label=uiLabelMap.ProductNewSearch/>
                  <@field type="radio" name="clearSearch" value="N" label=uiLabelMap.CommonRefineSearch/>
                </div>
          </@field>
        </#if>
        <@field type="submitarea">
            <@field type="submit" submitType="link" href="javascript:document.advToKeyWordSearchForm.submit()" class="+${styles.link_run_sys!} ${styles.action_find!}" text=uiLabelMap.CommonFind />
            <@field type="submit" submitType="image" src=makeOfbizContentUrl("/images/spacer.gif") onClick="javascript:document.advToKeyWordSearchForm.submit();"/>
        </@field>
    </form>
</@section>
