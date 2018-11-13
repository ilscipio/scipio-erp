<#--
This file is subject to the terms and conditions defined in
 file 'LICENSE', which is part of this source code package.
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