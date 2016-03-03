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

<#if inProcess??>
  <@section title=uiLabelMap.OrderProcessingStatus>
          <#-- Suspended Processes -->
          <#if workEffortStatus == "WF_SUSPENDED">
            <form action="<@ofbizUrl>releasehold</@ofbizUrl>" method="post" name="activityForm">
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
            <form action="<@ofbizUrl>holdorder</@ofbizUrl>" method="post" name="activityForm">
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
          <form action="<@ofbizUrl>completeassignment</@ofbizUrl>" method="post" name="transitionForm">
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
                        <#assign attrs = Static["org.ofbiz.base.util.StringUtil"].strToMap(trans.extendedAttributes)>
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