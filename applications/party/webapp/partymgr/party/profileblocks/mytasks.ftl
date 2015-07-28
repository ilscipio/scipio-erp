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

<#setting locale = locale.toString()>
<#setting time_zone = timeZone.getID()>

<#assign menuHtml>
    <li><a href="/workeffort/control/EditWorkEffort?workEffortTypeId=TASK&amp;currentStatusId=CAL_NEEDS_ACTION" class="button tiny">${uiLabelMap.WorkEffortNewTask}</a></li>
</#assign>
<@section title="${uiLabelMap.WorkEffortMyCurrentTaskList}" menuHtml=menuHtml>

  <@section title="${uiLabelMap.WorkEffortAssignedTasks}">
  <table class="basic-table hover-bar" cellspacing="0">
   <thead>
    <tr class="header-row">
      <th>${uiLabelMap.CommonStartDateTime}</th>
      <th>${uiLabelMap.WorkEffortPriority}</th>
      <th>${uiLabelMap.WorkEffortStatus}</th>
      <th>${uiLabelMap.WorkEffortTaskName}</th>
      <th>${uiLabelMap.CommonEdit}</th>
    </tr>
    </thead>
    <#assign alt_row = false>
    <#list tasks as workEffort>
      <tr class="<#if alt_row>odd<#else>even</#if>">
        <td>${(workEffort.estimatedStartDate.toString())!}</td>
        <td>${workEffort.priority!}</td>
        <td>${(delegator.findOne("StatusItem", Static["org.ofbiz.base.util.UtilMisc"].toMap("statusId", workEffort.getString("currentStatusId")), true).get("description",locale))!}</td>
        <td><a href="/workeffort/control/WorkEffortSummary?workEffortId=${workEffort.workEffortId}" class="button tiny">${workEffort.workEffortName}</a></td>
        <td class="button-col"><a href="/workeffort/control/WorkEffortSummary?workEffortId=${workEffort.workEffortId}" class="button tiny">${workEffort.workEffortId}</a></td>
      </tr>
      <#assign alt_row = !alt_row>
    </#list>
  </table>
  </@section>
  
  <#if (activities.size() > 0)>
    <@section title="${uiLabelMap.WorkEffortWorkflowActivitiesUser}">
    <table class="basic-table hover-bar" cellspacing="0">
      <thead>
      <tr class="header-row">
        <th>${uiLabelMap.CommonStartDateTime}</th>
        <th>${uiLabelMap.WorkEffortPriority}</th>
        <th>${uiLabelMap.WorkEffortActivityStatus}</th>
        <th>${uiLabelMap.WorkEffortMyStatus}</th>
        <#-- <th>${uiLabelMap.PartyPartyId}</th> -->
        <th>${uiLabelMap.PartyRole}</th>
        <th>${uiLabelMap.WorkEffortActivityName}</th>
        <th>${uiLabelMap.CommonEdit}</th>
      </tr>
      </thead>
      <#assign alt_row = false>
      <#list activities as workEffort>
        <tr class="<#if alt_row>odd<#else>even</#if>">
          <td>${(workEffort.estimatedStartDate.toString())!}</td>
          <td>${workEffort.priority!}</td>
          <td>${(delegator.findOne("StatusItem", Static["org.ofbiz.base.util.UtilMisc"].toMap("statusId", workEffort.getString("currentStatusId")), true).get("description",locale))!}</td>
          <td>${(delegator.findOne("StatusItem", Static["org.ofbiz.base.util.UtilMisc"].toMap("statusId", workEffort.getString("statusId")), true).get("description",locale))!}</td>
          <#-- <td>${workEffort.partyId}</td> -->
          <td>${workEffort.roleTypeId}</td>
          <td><a href="/workeffort/control/WorkEffortSummary?workEffortId=${workEffort.workEffortId}" class="button tiny">${workEffort.workEffortName}</a></td>
          <td class="button-col"><a href="/workeffort/control/WorkEffortSummary?workEffortId=${workEffort.workEffortId}" class="button tiny">${workEffort.workEffortId}</a></td>
        </tr>
        <#assign alt_row = !alt_row>
      </#list>
    </table>
    </@section>
  </#if>
  <#if (roleActivities.size() > 0)>
    <@section title="${uiLabelMap.WorkEffortWorkflowActivitiesUserRole}">
    <table class="basic-table hover-bar" cellspacing="0">
      <thead>
      <tr class="header-row">
        <th>${uiLabelMap.CommonStartDateTime}</th>
        <th>${uiLabelMap.WorkEffortPriority}</th>
        <th>${uiLabelMap.WorkEffortActivityStatus}</th>
        <th>${uiLabelMap.WorkEffortMyStatus}</th>
        <#-- <th>${uiLabelMap.PartyPartyId}</th> -->
        <th>${uiLabelMap.PartyRole}</th>
        <th>${uiLabelMap.WorkEffortActivityName}</th>
        <th>${uiLabelMap.CommonEdit}</th>
      </tr>
      </thead>
      <#assign alt_row = false>
      <#list roleActivities as workEffort>
        <tr class="<#if alt_row>odd<#else>even</#if>">
          <td>${(workEffort.estimatedStartDate.toString())!}</td>
          <td>${workEffort.priority!}</td>
          <td>${(delegator.findOne("StatusItem", Static["org.ofbiz.base.util.UtilMisc"].toMap("statusId", workEffort.getString("currentStatusId")), true).get("description",locale))!}</td>
          <td>${(delegator.findOne("StatusItem", Static["org.ofbiz.base.util.UtilMisc"].toMap("statusId", workEffort.getString("statusId")), true).get("description",locale))!}</td>
          <#-- <td>${workEffort.partyId}</td> -->
          <td>${workEffort.roleTypeId}</td>
          <td><a href="/workeffort/control/WorkEffortSummary?workEffortId=${workEffort.workEffortId}" class="button tiny">${workEffort.workEffortName}</a></td>
          <td class="button-col"><a href="/workeffort/control/acceptRoleAssignment?workEffortId=${workEffort.workEffortId}&amp;partyId=${workEffort.partyId}&amp;roleTypeId=${workEffort.roleTypeId}&amp;fromDate=${workEffort.fromDate.toString()}" class="button tiny">${uiLabelMap.WorkEffortAcceptAssignment}&nbsp;[${workEffort.workEffortId}]</a></td>
        </tr>
        <#assign alt_row = !alt_row>
      </#list>
    </table>
    </@section>
  </#if>
  <#if (groupActivities.size() > 0)>
    <@section title="${uiLabelMap.WorkEffortWorkflowActivitiesUserGroup}">
    <table class="basic-table hover-bar" cellspacing="0">
     <thead>
      <tr class="header-row">
        <th>${uiLabelMap.CommonStartDateTime}</th>
        <th>${uiLabelMap.WorkEffortPriority}</th>
        <th>${uiLabelMap.WorkEffortActivityStatus}</th>
        <th>${uiLabelMap.WorkEffortMyStatus}</th>
        <th>${uiLabelMap.PartyGroupPartyId}</th>
        <#-- <th>${uiLabelMap.PartyRole}</th> -->
        <th>${uiLabelMap.WorkEffortActivityName}</th>
        <th>${uiLabelMap.CommonEdit}</th>
      </tr>
      </thead>
      <#assign alt_row = false>
      <#list groupActivities as workEffort>
        <tr class="<#if alt_row>odd<#else>even</#if>">
          <td>${(workEffort.estimatedStartDate.toString())!}</td>
          <td>${workEffort.priority!}</td>
          <td>${(delegator.findOne("StatusItem", Static["org.ofbiz.base.util.UtilMisc"].toMap("statusId", workEffort.getString("currentStatusId")), true).get("description",locale))!}</td>
          <td>${(delegator.findOne("StatusItem", Static["org.ofbiz.base.util.UtilMisc"].toMap("statusId", workEffort.getString("statusId")), true).get("description",locale))!}</td>
          <td>${workEffort.groupPartyId}</td>
          <#-- <td>${workEffort.roleTypeId}</td> -->
          <td><a href="/workeffort/control/WorkEffortSummary?workEffortId=${workEffort.workEffortId}" class="button tiny">${workEffort.workEffortName}</a></td>
          <td class="button-col"><a href="/workeffort/control/acceptassignment?workEffortId=${workEffort.workEffortId}&amp;partyId=${workEffort.partyId}&amp;roleTypeId=${workEffort.roleTypeId}&amp;fromDate=${workEffort.fromDate}" class="button tiny">${uiLabelMap.WorkEffortAcceptAssignment}&nbsp;[${workEffort.workEffortId}]</a></td>
        </tr>
        <#assign alt_row = !alt_row>
      </#list>
    </table>
    </@section>
  </#if>
  
</@section>
