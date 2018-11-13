<#--
This file is subject to the terms and conditions defined in
 file 'LICENSE', which is part of this source code package.
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
  <@section title=uiLabelMap.OrderOrderNeedingAttention>

    <#if poList?has_content>
      <#assign tasksFound = true>
      <@section title=uiLabelMap.OrderOrderPurchaseToBeScheduled>
        <@table type="data-list" autoAltRows=true> <#-- orig: class="basic-table hover-bar" --> <#-- orig: cellspacing="0" -->
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
              <@td><a href="<@ofbizUrl>orderview?orderId=${orderHeaderAndRole.orderId}</@ofbizUrl>" class="${styles.link_nav_info_id!}">${orderHeaderAndRole.orderId}</a></@td>
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
              <@td>${statusItem.get("description",locale)!(statusItem.statusId!(uiLabelMap.CommonNA))}</@td>
              <@td align="right">${orh.getTotalOrderItemsQuantity()?string.number}</@td>
              <@td align="right"><@ofbizCurrency amount=orh.getOrderGrandTotal() isoCode=(orderHeaderAndRole.currencyUom!)/></@td>
              <@td width="1">&nbsp;&nbsp;</@td>
              <@td align='right'>
                <a href="<@ofbizUrl>OrderDeliveryScheduleInfo?orderId=${orderHeaderAndRole.orderId}</@ofbizUrl>" class="${styles.link_nav!} ${styles.action_update!}">Schedule&nbsp;Delivery</a>
              </@td>
            </@tr>
          </#list>
        </@table>
      </@section>
    </#if>

    <#if partyTasks?has_content>
      <#assign tasksFound = true>
      <@section title=uiLabelMap.OrderWorkflow>
        <@table type="data-list" autoAltRows=true> <#-- orig: class="basic-table" --> <#-- orig: cellspacing="0" -->
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
                <a href="<@ofbizUrl>orderview?${orderStr}</@ofbizUrl>" class="${styles.link_nav_info_id!}">
                  ${task.orderId}
                </a>
              </@td>
              <@td>
                  <#if task.customerPartyId??>
                    <a href="${customerDetailLink}${task.customerPartyId}${rawString(externalKeyParam)}" target="partymgr" class="${styles.link_nav_info_name!}">${Static["org.ofbiz.order.task.TaskWorker"].getCustomerName(task)}</a>
                  <#else>
                    ${uiLabelMap.CommonNA}
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
                  <#assign actualStartDate = uiLabelMap.CommonNA>
                </#if>
                <div>${actualStartDate}</div>
              </@td>
              <@td>${task.priority!"0"}</@td>
              <@td>
                <a href="<@ofbizInterWebappUrl>/workeffort/control/activity?workEffortId=${task.workEffortId}${rawString(externalKeyParam)}</@ofbizInterWebappUrl>" target="workeffort" class="${styles.link_nav_info_name!}">
                  ${Static["org.ofbiz.order.task.TaskWorker"].getPrettyStatus(task)}
                </a>
              </@td>
            </@tr>
          </#list>
        </@table>
      </@section>
    </#if>

    <#if roleTasks?has_content>
      <#assign tasksFound = true>
      <@section title=uiLabelMap.CommonWorkflowActivityUserRole>
        <@fields type="default-manual">
        <@table type="data-list" autoAltRows=true> <#-- orig: class="basic-table hover-bar" --> <#-- orig: cellspacing="0" -->
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
              <@tr valign="middle">
                <@td>
                  <a href="javascript:viewOrder(document.F${task.workEffortId});" class="${styles.link_nav_info_id!}">${task.orderId}</a>
                </@td>
                <@td>
                  <#if task.customerPartyId??>
                    <a href="${customerDetailLink}${task.customerPartyId}${rawString(externalKeyParam)}" target="partymgr" class="${styles.link_nav_info_name!}">${Static["org.ofbiz.order.task.TaskWorker"].getCustomerName(task)}</a>
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
                    <#assign actualStartDate = uiLabelMap.CommonNA>
                  </#if>
                  <div>${actualStartDate}</div>
                </@td>
                <@td>
                  <#if task.wepaPartyId == "_NA_">
                    ${uiLabelMap.CommonNA}
                  <#else>
                    <a href="${customerDetailLink}${task.wepaPartyId}${rawString(externalKeyParam)}" target="partymgr" class="${styles.link_nav_info_id!}">${task.wepaPartyId}</a>
                  </#if>
                </@td>
                <@td>${Static["org.ofbiz.order.task.TaskWorker"].getRoleDescription(task)}</@td>
                <@td>${task.priority!"0"}</@td>
                <@td>
                  <a href="<@ofbizInterWebappUrl>/workeffort/control/activity?workEffortId=${task.workEffortId}</@ofbizInterWebappUrl>" target="workeffort" class="${styles.link_nav_info_name!}">
                    ${Static["org.ofbiz.order.task.TaskWorker"].getPrettyStatus(task)}
                  </a>
                </@td>
                <@td align="right">
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
                      <#if task.statusId?? && task.statusId == "CAL_SENT">
                        <@field type="checkbox" name="delegate" value="true" checked="checked" />
                      <#else>
                        <@field type="checkbox" name="delegate" value="true" />
                      </#if>
                    </form>
                </@td>
              </@tr>
          </#list>
        </@table>
        </@fields>
      </@section>
    </#if>
    <#if !tasksFound>
      <@commonMsg type="result-norecord">${uiLabelMap.CommonNoTaskAssigned}</@commonMsg>
    </#if>
  </@section>
<#else>
  <@commonMsg type="error">You do not have permission to view this page. (ORDERMGR_VIEW or ORDERMGR_ADMIN needed)</@commonMsg>
</#if>