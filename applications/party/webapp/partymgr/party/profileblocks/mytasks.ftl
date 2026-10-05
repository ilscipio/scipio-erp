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

<#setting locale = locale.toString()>
<#setting time_zone = timeZone.getID()>

<#macro menuContent menuArgs={}>
  <@menu args=menuArgs>
    <@menuitem type="link" href=makeServerUrl("/workeffort/control/EditWorkEffort?workEffortTypeId=TASK&currentStatusId=CAL_NEEDS_ACTION") text=uiLabelMap.WorkEffortNewTask class="+${styles.action_nav!} ${styles.action_add!}" />
  </@menu>
</#macro>
<@section title=uiLabelMap.WorkEffortMyCurrentTaskList menuContent=menuContent>

  <@section title=uiLabelMap.WorkEffortAssignedTasks>
  <@table type="data-list" autoAltRows=true>
   <@thead>
    <@tr class="header-row">
      <@th>${uiLabelMap.CommonStartDateTime}</@th>
      <@th>${uiLabelMap.WorkEffortPriority}</@th>
      <@th>${uiLabelMap.WorkEffortStatus}</@th>
      <@th>${uiLabelMap.WorkEffortTaskName}</@th>
      <@th>${uiLabelMap.CommonEdit}</@th>
    </@tr>
    </@thead>
    <@tbody>
    <#list tasks as workEffort>
      <@tr>
        <@td>${(workEffort.estimatedStartDate.toString())!}</@td>
        <@td>${workEffort.priority!}</@td>
        <@td>${(delegator.findOne("StatusItem", {"statusId":workEffort.getString("currentStatusId")}, true).get("description",locale))!}</@td>
        <@td><a href="<@serverUrl>/workeffort/control/WorkEffortSummary?workEffortId=${workEffort.workEffortId}</@serverUrl>" class="${styles.link_nav_info_name!}">${workEffort.workEffortName}</a></@td>
        <@td class="button-col"><a href="<@serverUrl>/workeffort/control/WorkEffortSummary?workEffortId=${workEffort.workEffortId}</@serverUrl>" class="${styles.link_nav_info_id!}">${workEffort.workEffortId}</a></@td>
      </@tr>
    </#list>
    </@tbody>
  </@table>
  </@section>
  
  <#if (activities.size() > 0)>
    <@section title=uiLabelMap.WorkEffortWorkflowActivitiesUser>
    <@table type="data-list" autoAltRows=true>
      <@thead>
      <@tr class="header-row">
        <@th>${uiLabelMap.CommonStartDateTime}</@th>
        <@th>${uiLabelMap.WorkEffortPriority}</@th>
        <@th>${uiLabelMap.WorkEffortActivityStatus}</@th>
        <@th>${uiLabelMap.WorkEffortMyStatus}</@th>
        <#-- <@th>${uiLabelMap.PartyPartyId}</@th> -->
        <@th>${uiLabelMap.PartyRole}</@th>
        <@th>${uiLabelMap.WorkEffortActivityName}</@th>
        <@th>${uiLabelMap.CommonEdit}</@th>
      </@tr>
      </@thead>
      <@tbody>
      <#list activities as workEffort>
        <@tr>
          <@td>${(workEffort.estimatedStartDate.toString())!}</@td>
          <@td>${workEffort.priority!}</@td>
          <@td>${(delegator.findOne("StatusItem", {"statusId":workEffort.getString("currentStatusId")}, true).get("description",locale))!}</@td>
          <@td>${(delegator.findOne("StatusItem", {"statusId":workEffort.getString("statusId")}, true).get("description",locale))!}</@td>
          <#-- <@td>${workEffort.partyId}</@td> -->
          <@td>${workEffort.roleTypeId}</@td>
          <@td><a href="<@serverUrl>/workeffort/control/WorkEffortSummary?workEffortId=${workEffort.workEffortId}</@serverUrl>" class="${styles.link_nav_info_name!}">${workEffort.workEffortName}</a></@td>
          <@td class="button-col"><a href="<@serverUrl>/workeffort/control/WorkEffortSummary?workEffortId=${workEffort.workEffortId}</@serverUrl>" class="${styles.link_nav_info_id!}">${workEffort.workEffortId}</a></@td>
        </@tr>
      </#list>
      </@tbody>
    </@table>
    </@section>
  </#if>
  <#if (roleActivities.size() > 0)>
    <@section title=uiLabelMap.WorkEffortWorkflowActivitiesUserRole>
    <@table type="data-list" autoAltRows=true>
      <@thead>
      <@tr class="header-row">
        <@th>${uiLabelMap.CommonStartDateTime}</@th>
        <@th>${uiLabelMap.WorkEffortPriority}</@th>
        <@th>${uiLabelMap.WorkEffortActivityStatus}</@th>
        <@th>${uiLabelMap.WorkEffortMyStatus}</@th>
        <#-- <@th>${uiLabelMap.PartyPartyId}</@th> -->
        <@th>${uiLabelMap.PartyRole}</@th>
        <@th>${uiLabelMap.WorkEffortActivityName}</@th>
        <@th>${uiLabelMap.CommonEdit}</@th>
      </@tr>
      </@thead>
      <@tbody>
      <#list roleActivities as workEffort>
        <@tr>
          <@td>${(workEffort.estimatedStartDate.toString())!}</@td>
          <@td>${workEffort.priority!}</@td>
          <@td>${(delegator.findOne("StatusItem", {"statusId":workEffort.getString("currentStatusId")}, true).get("description",locale))!}</@td>
          <@td>${(delegator.findOne("StatusItem", {"statusId":workEffort.getString("statusId")}, true).get("description",locale))!}</@td>
          <#-- <@td>${workEffort.partyId}</@td> -->
          <@td>${workEffort.roleTypeId}</@td>
          <@td><a href="<@serverUrl>/workeffort/control/WorkEffortSummary?workEffortId=${workEffort.workEffortId}</@serverUrl>" class="${styles.link_nav_info_name!}">${workEffort.workEffortName}</a></@td>
          <@td class="button-col"><a href="<@serverUrl>/workeffort/control/acceptRoleAssignment?workEffortId=${workEffort.workEffortId}&amp;partyId=${workEffort.partyId}&amp;roleTypeId=${workEffort.roleTypeId}&amp;fromDate=${workEffort.fromDate.toString()}</@serverUrl>" class="${styles.link_run_sys!} ${styles.action_update!}">${uiLabelMap.WorkEffortAcceptAssignment}&nbsp;[${workEffort.workEffortId}]</a></@td>
        </@tr>
      </#list>
      </@tbody>
    </@table>
    </@section>
  </#if>
  <#if (groupActivities.size() > 0)>
    <@section title=uiLabelMap.WorkEffortWorkflowActivitiesUserGroup>
    <@table type="data-list" autoAltRows=true>
     <@thead>
      <@tr class="header-row">
        <@th>${uiLabelMap.CommonStartDateTime}</@th>
        <@th>${uiLabelMap.WorkEffortPriority}</@th>
        <@th>${uiLabelMap.WorkEffortActivityStatus}</@th>
        <@th>${uiLabelMap.WorkEffortMyStatus}</@th>
        <@th>${uiLabelMap.PartyGroupPartyId}</@th>
        <#-- <@th>${uiLabelMap.PartyRole}</@th> -->
        <@th>${uiLabelMap.WorkEffortActivityName}</@th>
        <@th>${uiLabelMap.CommonEdit}</@th>
      </@tr>
      </@thead>
      <@tbody>
      <#list groupActivities as workEffort>
        <@tr>
          <@td>${(workEffort.estimatedStartDate.toString())!}</@td>
          <@td>${workEffort.priority!}</@td>
          <@td>${(delegator.findOne("StatusItem", {"statusId":workEffort.getString("currentStatusId")}, true).get("description",locale))!}</@td>
          <@td>${(delegator.findOne("StatusItem", {"statusId":workEffort.getString("statusId")}, true).get("description",locale))!}</@td>
          <@td>${workEffort.groupPartyId}</@td>
          <#-- <@td>${workEffort.roleTypeId}</@td> -->
          <@td><a href="<@serverUrl>/workeffort/control/WorkEffortSummary?workEffortId=${workEffort.workEffortId}</@serverUrl>" class="${styles.link_nav_info_name!}">${workEffort.workEffortName}</a></@td>
          <@td class="button-col"><a href="<@serverUrl>/workeffort/control/acceptassignment?workEffortId=${workEffort.workEffortId}&amp;partyId=${workEffort.partyId}&amp;roleTypeId=${workEffort.roleTypeId}&amp;fromDate=${workEffort.fromDate}</@serverUrl>" class="${styles.link_run_sys!} ${styles.action_update!}">${uiLabelMap.WorkEffortAcceptAssignment}&nbsp;[${workEffort.workEffortId}]</a></@td>
        </@tr>
      </#list>
      </@tbody>
    </@table>
    </@section>
  </#if>
  
</@section>
