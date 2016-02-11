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

<#assign sectionTitle>
  <#if task?has_content>
    ${uiLabelMap.PageTitleEditTask}&nbsp;#${project.workEffortId!} ${uiLabelMap.CommonInformation}
  <#else>
    ${uiLabelMap.PageTitleAddTask}
  </#if>
</#assign>
<@section title=sectionTitle>
    <#assign workEffortIdFrom = parameters.workEffortIdFrom>
    <#if task?has_content>
      <form name="addTaskAndAssocForm" method="get" action="<@ofbizUrl>updateTaskAndAssoc</@ofbizUrl>">
    <#else>
      <form name="addTaskAndAssocForm" method="get" action="<@ofbizUrl>createTaskAndAssoc</@ofbizUrl>">
    </#if>
          <#if !(task??)>
            <input type="hidden" name="workEffortTypeId" value="${parameters.workEffortTypeId!}"/>
          <#else>
            <input type="hidden" name="workEffortTypeId" value="${task.workEffortTypeId!}"/>
            <input type="hidden" name="workEffortId" value="${task.workEffortId!}"/>
            <input type="hidden" name="workEffortName" value="${task.workEffortName!}"/>
          </#if>
            <input type="hidden" name="workEffortIdFrom" value="${workEffortIdFrom!}"/>
            <input type="hidden" name="workEffortParentId" value="${workEffortIdFrom!}"/>
            <input type="hidden" name="workEffortAssocTypeId" value="WORK_EFF_BREAKDOWN"/>
            
        <@heading>${uiLabelMap.ProjectMgrTaskDetails}</@heading>    

        <@field type="lookup" label="${uiLabelMap.ProjectMgrQuickAssignPartyId}" formName="addTaskAndAssocForm" name="quickAssignPartyId" id="quickAssignPartyId" fieldFormName="LookupPartyName"/>
        
        <#if task??>
          <@field type="display" label="${uiLabelMap.ProjectMgrWorkEffortId}">
            ${task.workEffortId!}
          </@field>
        </#if>
        
        <@field type="generic" label="${uiLabelMap.CommonName}*">
            <#if task??>
              ${task.workEffortName!}<span class="tooltip">${uiLabelMap.CommonRequired}</span>
            <#else>
              <input type="text" name="workEffortName" value=""/><span class="tooltip">${uiLabelMap.CommonRequired}</span>
            </#if>
        </@field>
        <@field type="generic" label="${uiLabelMap.CommonDescription}">
            <#if task??>
              <input type="text" name="description" value="${task.description!}"/>
            <#else>
              <input type="text" name="description" value=""/>
          </#if>
        </@field>
        <@field type="select" label="${uiLabelMap.CommonStatus}" name="currentStatusId">
              <#if task??>
                <#assign currentStatus = task.geRelatedOne("CurrentStatusItem")!>
                <option selected="selected" value="${currentStatus.currentStatusId}">${currentStatus.description}</option>
                <#assign statusValidChangeToDetailList = delegator.findByAnd("StatusValidChangeToDetail", Static["org.ofbiz.base.util.UtilMisc"].toMap("statusId", currentStatus.currentStatusId), null, false)>
                <#list statusValidChangeToDetailList as statusValidChangeToDetail>
                  <option value="${statusValidChangeToDetail.statusId}">[${uiLabelMap.WorkEffortGeneral}]${statusValidChangeToDetail.description}</option>
                </#list>
              <#else>
                <#assign statusItemGenrals = delegator.findByAnd("StatusItem", Static["org.ofbiz.base.util.UtilMisc"].toMap("statusTypeId", "CALENDAR_STATUS"), null, false)>
                <#assign statusItemTasks = delegator.findByAnd("StatusItem", Static["org.ofbiz.base.util.UtilMisc"].toMap("statusTypeId", "TASK_STATUS"), null, false)>
                <#assign statusItemEvents = delegator.findByAnd("StatusItem", Static["org.ofbiz.base.util.UtilMisc"].toMap("statusTypeId", "EVENT_STATUS"), null, false)>
                <#list statusItemGenrals as statusItem>
                  <option value="${statusItem.statusId!}">[${uiLabelMap.WorkEffortGeneral}]${statusItem.description}</option>
                </#list>
                <#list statusItemTasks as statusItem>
                  <option value="${statusItem.statusId!}">[${uiLabelMap.WorkEffortTask}]${statusItem.description}</option>
                </#list>
                <#list statusItemEvents as statusItem>
                  <option value="${statusItem.statusId!}">[${uiLabelMap.WorkEffortEvent}]${statusItem.description}</option>
                </#list>
              </#if>
        </@field>
        <@field type="generic" label="${uiLabelMap.CommonPriority}">
            <#if task?has_content>
              <#assign priority = task.priority!>
            </#if>
            <select name="priority" size="1">
              <#if priority??>
                <option selected="selected" value="${priority}">${priority}</option>
                <option></option>
                <option value="1">${uiLabelMap.WorkEffortPriorityOne}</option>
                <option value="2">${uiLabelMap.WorkEffortPriorityTwo}</option>
                <option value="3">${uiLabelMap.WorkEffortPriorityThree}</option>
                <option value="4">${uiLabelMap.WorkEffortPriorityFour}</option>
                <option value="5">${uiLabelMap.WorkEffortPriorityFive}</option>
                <option value="6">${uiLabelMap.WorkEffortPrioritySix}</option>
                <option value="7">${uiLabelMap.WorkEffortPrioritySeventh}</option>
                <option value="8">${uiLabelMap.WorkEffortPriorityEight}</option>
                <option value="9">${uiLabelMap.WorkEffortPriorityNine}</option>
              <#else>
                <option></option>
                <option value="1">${uiLabelMap.WorkEffortPriorityOne}</option>
                <option value="2">${uiLabelMap.WorkEffortPriorityTwo}</option>
                <option value="3">${uiLabelMap.WorkEffortPriorityThree}</option>
                <option value="4">${uiLabelMap.WorkEffortPriorityFour}</option>
                <option value="5">${uiLabelMap.WorkEffortPriorityFive}</option>
                <option value="6">${uiLabelMap.WorkEffortPrioritySix}</option>
                <option value="7">${uiLabelMap.WorkEffortPrioritySeventh}</option>
                <option value="8">${uiLabelMap.WorkEffortPriorityEight}</option>
                <option value="9">${uiLabelMap.WorkEffortPriorityNine}</option>
              </#if>
            </select>
        </@field>
        <#assign enumerations = delegator.findByAnd("Enumeration", Static["org.ofbiz.base.util.UtilMisc"].toMap("enumTypeId", "WORK_EFF_SCOPE"), null, false)>
        <@field type="select" label="${uiLabelMap.ProjectMgrWorkEffortScopeEnumId}" name="scopeEnumId">
              <#if task??>
                <#assign scopeEnumId = task.scopeEnumId!>
                <#list enumerations as enumeration>
                  <option <#if "${enumeration.enumId}" == scopeEnumId!>selected="selected"</#if>>${enumeration.description}</option>
                </#list>
              <#else>
                <#list enumerations as enumeration>
                  <option value="${enumeration.enumId}">${enumeration.description}</option>
                </#list>
              </#if>
        </@field>
        <@field type="generic" label="${uiLabelMap.WorkEffortEstimatedStartDate}">
            <#if task??>
              <@htmlTemplate.renderDateTimeField name="estimatedStartDate" className="" event="" action=""  title="Format: yyyy-MM-dd HH:mm:ss.SSS" value="${task.estimatedStartDate!}" size="25" maxlength="30" id="estimatedStartDate1" dateType="date" shortDateInput=false timeDropdownParamName="" defaultDateTimeString="" localizedIconTitle="" timeDropdown="" timeHourName="" classString="" hour1="" hour2="" timeMinutesName="" minutes="" isTwelveHour="" ampmName="" amSelected="" pmSelected="" compositeType="" formName=""/>
            <#else>
              <@htmlTemplate.renderDateTimeField name="estimatedStartDate" className="" event="" action=""  title="Format: yyyy-MM-dd HH:mm:ss.SSS" value="" size="25" maxlength="30" id="estimatedStartDate1" dateType="date" shortDateInput=false timeDropdownParamName="" defaultDateTimeString="" localizedIconTitle="" timeDropdown="" timeHourName="" classString="" hour1="" hour2="" timeMinutesName="" minutes="" isTwelveHour="" ampmName="" amSelected="" pmSelected="" compositeType="" formName=""/>
            </#if>
        </@field>
         <@field type="generic" label="${uiLabelMap.WorkEffortEstimatedCompletionDate}">
             <#if task??>
               <@htmlTemplate.renderDateTimeField name="estimatedCompletionDate" className="" event="" action=""  title="Format: yyyy-MM-dd HH:mm:ss.SSS" value="${task.estimatedCompletionDate!}" size="25" maxlength="30" id="estimatedCompletionDate1" dateType="date" shortDateInput=false timeDropdownParamName="" defaultDateTimeString="" localizedIconTitle="" timeDropdown="" timeHourName="" classString="" hour1="" hour2="" timeMinutesName="" minutes="" isTwelveHour="" ampmName="" amSelected="" pmSelected="" compositeType="" formName=""/>
             <#else>
               <@htmlTemplate.renderDateTimeField name="estimatedCompletionDate" className="" event="" action=""  title="Format: yyyy-MM-dd HH:mm:ss.SSS" value="" size="25" maxlength="30" id="estimatedCompletionDate1" dateType="date" shortDateInput=false timeDropdownParamName="" defaultDateTimeString="" localizedIconTitle="" timeDropdown="" timeHourName="" classString="" hour1="" hour2="" timeMinutesName="" minutes="" isTwelveHour="" ampmName="" amSelected="" pmSelected="" compositeType="" formName=""/>
             </#if>
         </@field>
         <@field type="generic" label="${uiLabelMap.FormFieldTitle_actualStartDate}">
             <#if task??>
               <@htmlTemplate.renderDateTimeField name="actualStartDate" event="" action="" className=""  title="Format: yyyy-MM-dd HH:mm:ss.SSS" value="${task.actualStartDate!}" size="25" maxlength="30" id="actualStartDate1" dateType="date" shortDateInput=false timeDropdownParamName="" defaultDateTimeString="" localizedIconTitle="" timeDropdown="" timeHourName="" classString="" hour1="" hour2="" timeMinutesName="" minutes="" isTwelveHour="" ampmName="" amSelected="" pmSelected="" compositeType="" formName=""/>
             <#else>
               <@htmlTemplate.renderDateTimeField name="actualStartDate" event="" action="" className=""  title="Format: yyyy-MM-dd HH:mm:ss.SSS" value="" size="25" maxlength="30" id="actualStartDate1" dateType="date" shortDateInput=false timeDropdownParamName="" defaultDateTimeString="" localizedIconTitle="" timeDropdown="" timeHourName="" classString="" hour1="" hour2="" timeMinutesName="" minutes="" isTwelveHour="" ampmName="" amSelected="" pmSelected="" compositeType="" formName=""/>
             </#if>
         </@field>
         <@field type="generic" label="${uiLabelMap.FormFieldTitle_actualCompletionDate}">
             <#if task??>
               <@htmlTemplate.renderDateTimeField name="actualCompletionDate" event="" action="" className=""  title="Format: yyyy-MM-dd HH:mm:ss.SSS" value="${task.actualCompletionDate!}" size="25" maxlength="30" id="actualCompletionDate2" dateType="date" shortDateInput=false timeDropdownParamName="" defaultDateTimeString="" localizedIconTitle="" timeDropdown="" timeHourName="" classString="" hour1="" hour2="" timeMinutesName="" minutes="" isTwelveHour="" ampmName="" amSelected="" pmSelected="" compositeType="" formName=""/>
             <#else>
               <@htmlTemplate.renderDateTimeField name="actualCompletionDate" event="" action="" className=""  title="Format: yyyy-MM-dd HH:mm:ss.SSS" value="" size="25" maxlength="30" id="actualCompletionDate2" dateType="date" shortDateInput=false timeDropdownParamName="" defaultDateTimeString="" localizedIconTitle="" timeDropdown="" timeHourName="" classString="" hour1="" hour2="" timeMinutesName="" minutes="" isTwelveHour="" ampmName="" amSelected="" pmSelected="" compositeType="" formName=""/>
             </#if>
         </@field>
         <@field type="submit" name="submit" text="${uiLabelMap.CommonSave}" class="${styles.link_run_sys!} ${styles.action_update!}"/>
     </form>
</@section>
