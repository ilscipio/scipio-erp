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

<#if workEffortId?has_content>
<form action="<@ofbizUrl>DuplicateWorkEffort</@ofbizUrl>" method="post">
    <input type="hidden" name="oldWorkEffortId" value="${workEffortId}"/>
    <@field type="text" size="20" maxlength="20" name="workEffortId" label=uiLabelMap.ProductDuplicateRemoveSelectedWithNewId/>
    <@field type="generic" label=uiLabelMap.CommonDuplicate>
        <@field type="checkbox" name="duplicateWorkEffortAssignmentRates" label=uiLabelMap.FormFieldTitle_rate value="Y" checked=true/>
        <@field type="checkbox" name="duplicateWorkEffortAssocs" label=uiLabelMap.WorkEffortAssoc value="Y" checked=true/>
        <@field type="checkbox" name="duplicateWorkEffortContents" label=uiLabelMap.ProductContent value="Y" checked=true/>
        <@field type="checkbox" name="duplicateWorkEffortNotes" label=uiLabelMap.WorkEffortNotes value="Y" checked=true/>
    </@field>
    <@field type="generic" label=uiLabelMap.CommonRemove>
        <@field type="checkbox" name="removeWorkEffortAssignmentRates" label=uiLabelMap.FormFieldTitle_rate value="Y"/>
        <@field type="checkbox" name="removeWorkEffortAssocs" label=uiLabelMap.WorkEffortAssoc value="Y"/>
        <@field type="checkbox" name="removeWorkEffortContents" label=uiLabelMap.ProductContent value="Y"/>
        <@field type="checkbox" name="removeWorkEffortNotes" label=uiLabelMap.WorkEffortNotes value="Y"/>
    </@field>
    <@field type="select" name="statusId" label=uiLabelMap.FormFieldTitle_statusId>
        <option value=""></option>
      <#list workEffortStatusList as weStatus>
        <option value="${weStatus.statusId}"<#rt/>
            <#lt/><#if rawString(workEffort.currentStatusId!) == rawString(weStatus.statusId!)> selected="selected"</#if>>${weStatus.get("description", locale)!weStatus.statusId}</option>
      </#list>
    </@field>
    <@field type="submit" class="${styles.link_run_sys!} ${styles.action_copy!}" text=uiLabelMap.CommonDuplicate/>
</form>
</#if>
