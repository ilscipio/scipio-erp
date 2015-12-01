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
    function viewOrder(form) {
        if (form.taskStatus.value == "WF_NOT_STARTED") {
            if (form.delegate.checked) {
                form.action = "<@ofbizUrl>acceptassignment</@ofbizUrl>";
            } else {
                form.action = "<@ofbizUrl>orderview</@ofbizUrl>";
            }
        } else {
            if (form.delegate.checked) {
                form.action = "<@ofbizUrl>delegateassignment</@ofbizUrl>";
            } else {
                form.action = "<@ofbizUrl>orderview</@ofbizUrl>";
            }
        }
        form.submit();
    }
</@script>

<#if security.hasEntityPermission("ORDERMGR", "_VIEW", session)>
  <#assign tasksFound = false>
  <@section title="${uiLabelMap.OrderOrderNeedingAttention}">
        <#-- FIXME: pure structure table, should keep innermost tables only -->
        <@table type="generic" class="${styles.table_basic!}" autoAltRows=false cellspacing="0"> <#-- orig: class="basic-table" -->
            <@tr>
              <@td width='100%'>
                <#if poList?has_content>
                  <#assign tasksFound = true>
                  <@table type="generic" class="${styles.table_basic!}" autoAltRows=false cellspacing="0"> <#-- orig: class="basic-table" -->
                    <@tr>
                      <@td>
                        <@heading>${uiLabelMap.OrderOrderPurchaseToBeScheduled}</@heading>
                        <@table type="data-list" autoAltRows=true cellspacing="0" class="+hover-bar"> <#-- orig: class="basic-table hover-bar" -->
                         <@thead>
                          <@tr class="header-row">
                            <@th>${uiLabelMap.OrderOrderNumber}</@th>
                            <@th>${uiLabelMap.CommonName}</@th>
                            <@th>${uiLabelMap.OrderOrderDate}</@th>
                            <@th>${uiLabelMap.CommonStatus}</@th>
                            <@th width="1" align="right">${uiLabelMap.OrderOrderItems}</@th>
                            <@th width="1" align="right">${uiLabelMap.OrderItemTotal}</@th>
                            <@th width="1">&nbsp;&nbsp;</@th>
                            <@th width="1">&nbsp;&nbsp;</@th>
                          </@tr>
                          </@thead>
                          <#list poList as orderHeaderAndRole>
                            <#assign orh = Static["org.ofbiz.order.order.OrderReadHelper"].getHelper(orderHeaderAndRole)>
                            <#assign statusItem = orderHeaderAndRole.getRelatedOne("StatusItem", true)>
                            <#assign placingParty = orh.getPlacingParty()!>
                            <@tr valign="middle">
                              <@td><a href="<@ofbizUrl>orderview?orderId=${orderHeaderAndRole.orderId}</@ofbizUrl>" class="${styles.link_id!}">${orderHeaderAndRole.orderId}</a></@td>
                              <@td>
                                  <#assign partyId = "_NA_">
                                  <#if placingParty?has_content>
                                    <#assign partyId = placingParty.partyId>
                                    <#if placingParty.getEntityName() == "Person">
                                      <#if placingParty.lastName??>
                                        ${placingParty.lastName}<#if placingParty.firstName??>, ${placingParty.firstName}</#if>
                                      <#else>
                                        ${uiLabelMap.CommonNA}
                                      </#if>
                                    <#else>
                                      <#if placingParty.groupName??>
                                        ${placingParty.groupName}
                                      <#else>
                                        ${uiLabelMap.CommonNA}
                                      </#if>
                                    </#if>
                                  <#else>
                                    ${uiLabelMap.CommonNA}
                                  </#if>
                                </@td>
                              <@td><span style="white-space: nowrap;">${orderHeaderAndRole.getString("orderDate")}</span></@td>
                              <@td>${statusItem.get("description",locale)?default(statusItem.statusId?default("N/A"))}</@td>
                              <@td align="right">${orh.getTotalOrderItemsQuantity()?string.number}</@td>
                              <@td align="right"><@ofbizCurrency amount=orh.getOrderGrandTotal() isoCode=orderHeaderAndRole.currencyUom!/></@td>
                              <@td width="1">&nbsp;&nbsp;</@td>
                              <@td align='right'>
                                <a href="<@ofbizUrl>OrderDeliveryScheduleInfo?orderId=${orderHeaderAndRole.orderId}</@ofbizUrl>" class='${styles.button_default!}'>Schedule&nbsp;Delivery</a>
                              </@td>
                            </@tr>
                          </#list>
                        </@table>
                      </@td>
                    </@tr>
                  </@table>
                </#if>

                <#if partyTasks?has_content>
                  <#assign tasksFound = true>
                  <@table type="generic" class="${styles.table_basic!}" autoAltRows=false cellspacing="0" class="+hover-bar"> <#-- orig: class="basic-table hover-bar" -->
                    <@tr>
                      <@td>
                        <@heading>${uiLabelMap.OrderWorkflow}</@heading>
                        <@table type="data-list" autoAltRows=true cellspacing="0"> <#-- orig: class="basic-table" -->
                         <@thead>
                          <@tr class="header-row">
                            <@th><a href="<@ofbizUrl>tasklist?sort=orderId</@ofbizUrl>">${uiLabelMap.OrderOrderNumber}</a></@th>
                            <@th><a href="<@ofbizUrl>tasklist?sort=name</@ofbizUrl>">${uiLabelMap.CommonName}</a></@th>
                            <@th><a href="<@ofbizUrl>tasklist?sort=orderDate</@ofbizUrl>">${uiLabelMap.OrderOrderDate}</a></@th>
                            <@th width="1" align="right"><a href="<@ofbizUrl>tasklist?sort=grandTotal</@ofbizUrl>">Total</a></@th>
                            <@th width="1">&nbsp;&nbsp;</@th>
                            <@th><a href="<@ofbizUrl>tasklist?sort=actualStartDate</@ofbizUrl>">${uiLabelMap.OrderStartDateTime}</a></@th>
                            <@th><a href="<@ofbizUrl>tasklist?sort=priority</@ofbizUrl>">${uiLabelMap.CommonPriority}</a></@th>
                            <@th><a href="<@ofbizUrl>tasklist?sort=currentStatusId</@ofbizUrl>">${uiLabelMap.CommonMyStatus}</a></@th>
                          </@tr>
                          </@thead>
                          <#list partyTasks as task>
                            <@tr valign="middle">
                              <@td>
                                <#assign orderStr = "orderId=" + task.orderId + "&amp;partyId=" + userLogin.partyId + "&amp;roleTypeId=" + task.roleTypeId + "&amp;workEffortId=" + task.workEffortId + "&amp;fromDate=" + task.get("fromDate").toString()>
                                <a href="<@ofbizUrl>orderview?${orderStr}</@ofbizUrl>" class="${styles.button_default!}">
                                  ${task.orderId}
                                </a>
                              </@td>
                              <@td>
                                  <#if task.customerPartyId??>
                                    <a href="${customerDetailLink}${task.customerPartyId}${StringUtil.wrapString(externalKeyParam)}" target="partymgr" class="${styles.button_default!}">${Static["org.ofbiz.order.task.TaskWorker"].getCustomerName(task)}</a>
                                  <#else>
                                    N/A
                                  </#if>
                                </@td>
                              <@td>
                                  ${task.get("orderDate").toString()}
                                </@td>
                              <@td width="1" align="right"><@ofbizCurrency amount=task.grandTotal isoCode=orderCurrencyMap.get(task.orderId)/></@td>
                              <@td width="1">&nbsp;&nbsp;</@td>
                              <@td>
                                <#if task.actualStartDate??>
                                  <#assign actualStartDate = task.get("actualStartDate").toString()>
                                <#else>
                                  <#assign actualStartDate = "N/A">
                                </#if>
                                <div>${actualStartDate}</div>
                              </@td>
                              <@td>${task.priority?default("0")}</@td>
                              <@td>
                                <a href="/workeffort/control/activity?workEffortId=${task.workEffortId}${StringUtil.wrapString(externalKeyParam)}" target="workeffort" class="${styles.button_default!}">
                                  ${Static["org.ofbiz.order.task.TaskWorker"].getPrettyStatus(task)}
                                </a>
                              </@td>
                            </@tr>
                          </#list>
                        </@table>
                      </@td>
                    </@tr>
                  </@table>
                </#if>

                <#if roleTasks?has_content>
                  <#assign tasksFound = true>
                  <@table type="generic" class="${styles.table_basic!}" autoAltRows=false cellspacing="0"> <#-- orig: class="basic-table" -->
                    <@tr>
                      <@td>
                        <@heading>${uiLabelMap.CommonWorkflowActivityUserRole}</@heading>
                        <@table type="data-list" autoAltRows=true cellspacing="0" class="+hover-bar"> <#-- orig: class="basic-table hover-bar" -->
                         <@thead>
                          <@tr class="header-row">
                            <@th><a href="<@ofbizUrl>tasklist?sort=orderId</@ofbizUrl>">${uiLabelMap.OrderOrderNumber}</a></@th>
                            <@th><a href="<@ofbizUrl>tasklist?sort=name</@ofbizUrl>">${uiLabelMap.CommonName}</a></@th>
                            <@th><a href="<@ofbizUrl>tasklist?sort=orderDate</@ofbizUrl>">${uiLabelMap.OrderOrderDate}</a></@th>
                            <@th width="1" align="right"><a href="<@ofbizUrl>tasklist?sort=grandTotal</@ofbizUrl>">${uiLabelMap.CommonTotal}</a></@th>
                            <@th width="1">&nbsp;&nbsp;</@th>
                            <@th><a href="<@ofbizUrl>tasklist?sort=actualStartDate</@ofbizUrl>">${uiLabelMap.CommonStartDateTime}</a></@th>
                            <@th><a href="<@ofbizUrl>tasklist?sort=wepaPartyId</@ofbizUrl>">${uiLabelMap.PartyParty}</a></@th>
                            <@th><a href="<@ofbizUrl>tasklist?sort=roleTypeId</@ofbizUrl>">${uiLabelMap.PartyRole}</a></@th>
                            <@th><a href="<@ofbizUrl>tasklist?sort=priority</@ofbizUrl>">${uiLabelMap.CommonPriority}</a></@th>
                            <@th><a href="<@ofbizUrl>tasklist?sort=currentStatusId</@ofbizUrl>">${uiLabelMap.CommonStatus}</a></@th>
                            <@th>&nbsp;</@th>
                          </@tr>
                          </@thead>
                          <#list roleTasks as task>
                            <form method="get" name="F${task.workEffortId}">
                              <input type="hidden" name="orderId" value="${task.orderId}" />
                              <input type="hidden" name="workEffortId" value="${task.workEffortId}" />
                              <input type="hidden" name="taskStatus" value="${task.currentStatusId}" />
                              <#if task.statusId?? && task.statusId == "CAL_SENT">
                                <input type="hidden" name="partyId" value="${userLogin.partyId}" />
                                <input type="hidden" name="roleTypeId" value="${task.roleTypeId}" />
                                <input type="hidden" name="fromDate" value="${task.get("fromDate").toString()}" />
                              <#else>
                                <input type="hidden" name="partyId" value="${userLogin.partyId}" />
                                <input type="hidden" name="roleTypeId" value="${task.roleTypeId}" />
                                <input type="hidden" name="fromDate" value="${task.get("fromDate").toString()}" />
                                <input type="hidden" name="fromPartyId" value="${task.wepaPartyId}" />
                                <input type="hidden" name="fromRoleTypeId" value="${task.roleTypeId}" />
                                <input type="hidden" name="fromFromDate" value="${task.get("fromDate").toString()}" />
                                <input type="hidden" name="toPartyId" value="${userLogin.partyId}" />
                                <input type="hidden" name="toRoleTypeId" value="${task.roleTypeId}" />
                                <input type="hidden" name="toFromDate" value="${now}" />
                                <input type="hidden" name="startActivity" value="true" />
                              </#if>
                              <@tr valign="middle">
                                <@td>
                                  <a href="javascript:viewOrder(document.F${task.workEffortId});" class="${styles.button_default!}">
                                    ${task.orderId}
                                  </a>
                                </@td>
                                <@td>
                                  <#if task.customerPartyId??>
                                  <a href="${customerDetailLink}${task.customerPartyId}${StringUtil.wrapString(externalKeyParam)}" target="partymgr" class="${styles.button_default!}">${Static["org.ofbiz.order.task.TaskWorker"].getCustomerName(task)}</a>
                                  <#else>
                                  &nbsp;
                                  </#if>
                                </@td>
                                <@td>
                                    ${task.get("orderDate").toString()}
                                  </@td>
                                <@td width="1" align="right"><@ofbizCurrency amount=task.grandTotal isoCode=orderCurrencyMap.get(task.orderId)/></@td>
                                <@td width="1">&nbsp;&nbsp;</@td>
                                <@td>
                                  <#if task.actualStartDate??>
                                    <#assign actualStartDate = task.get("actualStartDate").toString()>
                                  <#else>
                                    <#assign actualStartDate = "N/A">
                                  </#if>
                                  <div>${actualStartDate}</div>
                                </@td>
                                <@td>
                                  <#if task.wepaPartyId == "_NA_">
                                    <div>N/A</div>
                                  <#else>
                                    <a href="${customerDetailLink}${task.wepaPartyId}${StringUtil.wrapString(externalKeyParam)}" target="partymgr" class="${styles.link_id!}">${task.wepaPartyId}</a>
                                  </#if>
                                </@td>
                                <@td>${Static["org.ofbiz.order.task.TaskWorker"].getRoleDescription(task)}</@td>
                                <@td>${task.priority?default("0")}</@td>
                                <@td>
                                  <a href="/workeffort/control/activity?workEffortId=${task.workEffortId}" target="workeffort" class="${styles.button_default!}">
                                    ${Static["org.ofbiz.order.task.TaskWorker"].getPrettyStatus(task)}
                                  </a>
                                </@td>
                                <#if task.statusId?? && task.statusId == "CAL_SENT">
                                  <@td align="right"><input type="checkbox" name="delegate" value="true" checked="checked" /></@td>
                                <#else>
                                  <@td align="right"><input type="checkbox" name="delegate" value="true" /></@td>
                                </#if>
                              </@tr>
                            </form>
                          </#list>
                        </@table>
                      </@td>
                    </@tr>
                  </@table>
                </#if>
                <#if !tasksFound>
                  <@resultMsg>${uiLabelMap.CommonNoTaskAssigned}</@resultMsg>
                </#if>
              </@td>
            </@tr>
        </@table>
  </@section>
<#else>
  <@alert type="error">You do not have permission to view this page. (ORDERMGR_VIEW or ORDERMGR_ADMIN needed)</@alert>
</#if>