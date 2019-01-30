<#--
This file is subject to the terms and conditions defined in the
files 'LICENSE' and 'NOTICE', which are part of this source
code package.
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
