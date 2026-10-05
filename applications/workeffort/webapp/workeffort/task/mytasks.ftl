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
<#--
Changes to this file: Copyright (C) Ilscipio GmbH. The changes are licensed
under the GNU Affero General Public License, version 3, or a commercial
license from Ilscipio GmbH (file LICENSE). The original code stays under
the Apache License, version 2.0, as stated above.
-->
<#macro menuContent menuArgs={}>
  <@menu args=menuArgs>
    <@menuitem type="link" href=makePageUrl("EditWorkEffort?workEffortTypeId=TASK&currentStatusId=CAL_NEEDS_ACTION") text=uiLabelMap.WorkEffortNewTask class="+${styles.action_nav!} ${styles.action_add!}" />
  </@menu>
</#macro>
<@section menuContent=menuContent>
    <#if tasks?has_content && (tasks.size() > 0)>
        <@section title=uiLabelMap.WorkEffortAssignedTasks>
        <@paginate mode="content" url=makePageUrl("searchorders") viewSize=viewSize!1 viewIndex=viewIndex!1 listSize=orderListSize!0 altParam=true paramStr=paramStr forcePost=true viewIndexFirst=1>
            <@table type="data-list" autoAltRows=true>
                <@thead>
                    <@tr class="header-row-2">
                        <@th>${uiLabelMap.CommonStartDateTime}</@th>
                        <@th>${uiLabelMap.WorkEffortTaskName}</@th>
                        <@th>${uiLabelMap.WorkEffortPriority}</@th>
                        <@th>${uiLabelMap.WorkEffortStatus}</@th>
                    </@tr>
                </@thead>
                <#list tasks as workEffort>
                    <@tr>
                        <@td>${(workEffort.estimatedStartDate)!}</@td>
                        <@td><a href="<@pageUrl>WorkEffortSummary?workEffortId=${workEffort.workEffortId}</@pageUrl>">${workEffort.workEffortName}</a></@td>
                        <@td>${workEffort.priority!}</@td>
                        <@td>${(delegator.findOne("StatusItem", {"statusId":workEffort.getString("currentStatusId")}, true).get("description",locale))!}</@td>
                    </@tr>
                </#list>
            </@table>
        </@paginate>
        </@section>
    </#if>
      
    <#if activities?has_content && (activities.size() > 0)>
        <@section title=uiLabelMap.WorkEffortWorkflowActivitiesUser>
            <@table type="data-list" autoAltRows=true>
                <@tr class="header-row-2">
                    <@td>${uiLabelMap.CommonStartDateTime}</@td>
                    <@td>${uiLabelMap.WorkEffortPriority}</@td>
                    <@td>${uiLabelMap.WorkEffortActivityStatus}</@td>
                    <@td>${uiLabelMap.WorkEffortMyStatus}</@td>
                    <#-- <@td>${uiLabelMap.PartyPartyId}</@td> -->
                    <@td>${uiLabelMap.PartyRole}</@td>
                    <@td>${uiLabelMap.WorkEffortActivityName}</@td>
                    <@td>${uiLabelMap.CommonEdit}</@td>
                </@tr>
                <#list activities as workEffort>
                    <@tr>
                        <@td>${(workEffort.estimatedStartDate)!}</@td>
                        <@td>${workEffort.priority!}</@td>
                        <@td>${(delegator.findOne("StatusItem", {"statusId":workEffort.getString("currentStatusId")}, true).get("description",locale))!}</@td>
                        <@td>${(delegator.findOne("StatusItem", {"statusId":workEffort.getString("statusId")}, true).get("description",locale))!}</@td>
                        <#-- <@td>${workEffort.partyId}</@td> -->
                        <@td>${workEffort.roleTypeId}</@td>
                        <@td><a href="<@pageUrl>WorkEffortSummary?workEffortId=${workEffort.workEffortId}</@pageUrl>">${workEffort.workEffortName}</a></@td>
                        <@td class="button-col"><a href="<@pageUrl>WorkEffortSummary?workEffortId=${workEffort.workEffortId}</@pageUrl>">${workEffort.workEffortId}</a></@td>
                    </@tr>
                </#list>
            </@table>
        </@section>
    </#if>
      
    <#if roleActivities?has_content && (roleActivities.size() > 0)>
        <@section title=uiLabelMap.WorkEffortWorkflowActivitiesUserRole>
            <@table type="data-list" autoAltRows=true>
                <@tr class="header-row-2">
                    <@td>${uiLabelMap.CommonStartDateTime}</@td>
                    <@td>${uiLabelMap.WorkEffortPriority}</@td>
                    <@td>${uiLabelMap.WorkEffortActivityStatus}</@td>
                    <@td>${uiLabelMap.WorkEffortMyStatus}</@td>
                    <#-- <@td>${uiLabelMap.PartyPartyId}</@td> -->
                    <@td>${uiLabelMap.PartyRole}</@td>
                    <@td>${uiLabelMap.WorkEffortActivityName}</@td>
                    <@td>${uiLabelMap.CommonEdit}</@td>
                </@tr>
                <#list roleActivities as workEffort>
                    <@tr>
                        <@td>${(workEffort.estimatedStartDate)!}</@td>
                        <@td>${workEffort.priority!}</@td>
                        <@td>${(delegator.findOne("StatusItem", {"statusId":workEffort.getString("currentStatusId")}, true).get("description",locale))!}</@td>
                        <@td>${(delegator.findOne("StatusItem", {"statusId":workEffort.getString("statusId")}, true).get("description",locale))!}</@td>
                        <#-- <@td>${workEffort.partyId}</@td> -->
                        <@td>${workEffort.roleTypeId}</@td>
                        <@td><a href="<@pageUrl>WorkEffortSummary?workEffortId=${workEffort.workEffortId}</@pageUrl>">${workEffort.workEffortName}</a></@td>
                        <@td class="button-col"><a href="<@pageUrl>acceptRoleAssignment?workEffortId=${workEffort.workEffortId}&amp;partyId=${workEffort.partyId}&amp;roleTypeId=${workEffort.roleTypeId}&amp;fromDate=${workEffort.fromDate.toString()}</@pageUrl>">${uiLabelMap.WorkEffortAcceptAssignment}&nbsp;[${workEffort.workEffortId}]</a></@td>
                    </@tr>
                </#list>
            </@table>
        </@section>
    </#if>
    
    <#if groupActivities?has_content && (groupActivities.size() > 0)>
        <@section title=uiLabelMap.WorkEffortWorkflowActivitiesUserGroup>
            <@table type="data-list" autoAltRows=true>
                <@tr class="header-row-2">
                    <@td>${uiLabelMap.CommonStartDateTime}</@td>
                    <@td>${uiLabelMap.WorkEffortPriority}</@td>
                    <@td>${uiLabelMap.WorkEffortActivityStatus}</@td>
                    <@td>${uiLabelMap.WorkEffortMyStatus}</@td>
                    <@td>${uiLabelMap.PartyGroupPartyId}</@td>
                    <#-- <@td>${uiLabelMap.PartyRole}</@td> -->
                    <@td>${uiLabelMap.WorkEffortActivityName}</@td>
                    <@td>${uiLabelMap.CommonEdit}</@td>
                </@tr>
                <#list groupActivities as workEffort>
                    <@tr>
                        <@td>${(workEffort.estimatedStartDate)!}</@td>
                        <@td>${workEffort.priority!}</@td>
                        <@td>${(delegator.findOne("StatusItem", {"statusId":workEffort.getString("currentStatusId")}, true).get("description",locale))!}</@td>
                        <@td>${(delegator.findOne("StatusItem", {"statusId":workEffort.getString("statusId")}, true).get("description",locale))!}</@td>
                        <@td>${workEffort.groupPartyId}</@td>
                        <#-- <@td>${workEffort.roleTypeId}</@td> -->
                        <@td><a href="<@pageUrl>WorkEffortSummary?workEffortId=${workEffort.workEffortId}</@pageUrl>">${workEffort.workEffortName}</a></@td>
                        <@td class="button-col"><a href="<@pageUrl>acceptassignment?workEffortId=${workEffort.workEffortId}&amp;partyId=${workEffort.partyId}&amp;roleTypeId=${workEffort.roleTypeId}&amp;fromDate=${workEffort.fromDate}</@pageUrl>">${uiLabelMap.WorkEffortAcceptAssignment}&nbsp;[${workEffort.workEffortId}]</a></@td>
                    </@tr>
                </#list>
            </@table>
        </@section>
    </#if>
</@section>
