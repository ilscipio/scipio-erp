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

<@script>
function copyAndAddRoutingTask() {
    document.addtaskassocform.copyTask.value = "Y";
    document.addtaskassocform.submit();
}
function addRoutingTask() {
    document.addtaskassocform.copyTask.value = "N";
    document.addtaskassocform.submit();
}
</@script>

<#if security.hasEntityPermission("MANUFACTURING", "_CREATE", session)>
<form method="post" action="<@ofbizUrl>AddRoutingTaskAssoc</@ofbizUrl>" name="addtaskassocform">
    <input type="hidden" name="workEffortId" value="${workEffortId}"/>
    <input type="hidden" name="workEffortIdFrom" value="${workEffortId}"/>
    <input type="hidden" name="workEffortAssocTypeId" value="ROUTING_COMPONENT"/>
    <input type="hidden" name="copyTask" value="N"/>
    <@row>
      <@cell columns=6>
        <@field type="lookup" label=uiLabelMap.ManufacturingRoutingTaskId formName="addtaskassocform" name="workEffortIdTo" id="workEffortIdTo" fieldFormName="LookupRoutingTask"/>
      </@cell>
      <@cell columns=6>
        <@field type="datetime" label=uiLabelMap.CommonFromDate name="fromDate" value="" size="25" maxlength="30" id="fromDate_1"/>
      </@cell>
    </@row>
    <@row>
      <@cell columns=6>
        <@field type="input" label=uiLabelMap.CommonSequenceNum name="sequenceNum" size="10"/>
      </@cell>
      <@cell columns=6>
        <@field type="datetime" label=uiLabelMap.CommonThruDate name="thruDate" value="" size="25" maxlength="30" id="thruDate_1"/>
      </@cell>
    </@row>

        <@field type="submitarea">
            <@field type="submit" submitType="link" href="javascript:addRoutingTask();" class="+${styles.link_run_sys!} ${styles.action_add!}" text=uiLabelMap.ManufacturingAddExistingRoutingTask />
            <#-- &nbsp;-&nbsp; -->
            <@field type="submit" submitType="link" href="javascript:copyAndAddRoutingTask();" class="+${styles.link_run_sys!} ${styles.action_copy!}" text=uiLabelMap.ManufacturingCopyAndAddRoutingTask />
        </@field>
</form>
</#if>