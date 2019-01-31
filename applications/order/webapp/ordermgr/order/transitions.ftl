<#--
This file is subject to the terms and conditions defined in the
files 'LICENSE' and 'NOTICE', which are part of this source
code package.
-->

<#if inProcess??>
  <@section title=uiLabelMap.OrderProcessingStatus>
          <#-- Suspended Processes -->
          <#if workEffortStatus == "WF_SUSPENDED">
            <form action="<@pageUrl>releasehold</@pageUrl>" method="post" name="activityForm">
            <@fields type="default-manual">
              <input type="hidden" name="workEffortId" value="${workEffortId}" />
            <@row>
              <@cell columns=9>
                <@field type="display">
                  ${uiLabelMap.OrderProcessingInHold}&nbsp;${uiLabelMap.OrderProcessingInHoldNote}
                </@field>
              </@cell>
              <@cell columns=3>
                <@field type="submit" submitType="link" href="javascript:document.activityForm.submit()" class="+${styles.link_run_sys!} ${styles.action_update!}" text=uiLabelMap.OrderRelease />
              </@cell>
            </@row>
            </@fields>
            </form>
          </#if>
          <#-- Active Processes -->
          <#if workEffortStatus == "WF_RUNNING">
            <form action="<@pageUrl>holdorder</@pageUrl>" method="post" name="activityForm">
            <@fields type="default-manual">
              <input type="hidden" name="workEffortId" value="${workEffortId}" />
            <@row>
              <@cell columns=9>
                <@field type="display">
                  ${uiLabelMap.OrderProcessingInActive}
                </@field>
              </@cell>
              <@cell columns=3>
                <@field type="submit" submitType="link" href="javascript:document.activityForm.submit()" class="+${styles.link_run_sys!} ${styles.action_update!}" text=uiLabelMap.OrderHold />
              </@cell>
            </@row>
            </@fields>
            </form>
          </#if>
  </@section>
</#if>

<#if wfTransitions?? && wfTransitions?has_content>
  <@section title=uiLabelMap.OrderProcessingTransitions>
          <form action="<@pageUrl>completeassignment</@pageUrl>" method="post" name="transitionForm">
          <@fields type="default-manual">
            <input type="hidden" name="workEffortId" value="${workEffortId}" />
            <input type="hidden" name="partyId" value="${assignPartyId}" />
            <input type="hidden" name="roleTypeId" value="${assignRoleTypeId}" />
            <input type="hidden" name="fromDate" value="${fromDate}" />
          <@row>
            <@cell columns=9>
              <@field type="select" name="approvalCode">
                    <#list wfTransitions as trans>
                      <#if trans.extendedAttributes?has_content>
                        <#assign attrs = StringUtil.strToMap(trans.extendedAttributes)>
                        <#if attrs.approvalCode??>
                          <option value="${attrs.approvalCode}">${trans.transitionName}</option>
                        </#if>
                      </#if>
                    </#list>
              </@field>
            </@cell>
            <@cell columns=3>
              <@field type="submit" submitType="link" href="javascript:document.transitionForm.submit()" class="+${styles.link_run_sys!} ${styles.action_update!}" text=uiLabelMap.CommonContinue />
            </@cell>
          </@row>
          </@fields>
          </form>
  </@section>
</#if>