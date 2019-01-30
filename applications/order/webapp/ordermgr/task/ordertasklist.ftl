<#--
This file is subject to the terms and conditions defined in the
files 'LICENSE' and 'NOTICE', which are part of this source
code package.
-->

<@script>
    function viewOrder(form) {
        if (form.taskStatus.value == "WF_NOT_STARTED") {
            if (form.delegate.checked) {
                form.action = "<@pageUrl>acceptassignment</@pageUrl>";
            } else {
                form.action = "<@pageUrl>orderview</@pageUrl>";
            }
        } else {
            if (form.delegate.checked) {
                form.action = "<@pageUrl>delegateassignment</@pageUrl>";
            } else {
                form.action = "<@pageUrl>orderview</@pageUrl>";
            }
        }
        form.submit();
    }
</@script>

<#if security.hasEntityPermission("ORDERMGR", "_VIEW", request)>
  <#assign tasksFound = false>
  <@section title=uiLabelMap.OrderOrderNeedingAttention>

    <#if poList?has_content>
      <#assign tasksFound = true>
      <@section title=uiLabelMap.OrderOrderPurchaseToBeScheduled>
        <@table type="data-list" autoAltRows=true>
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
              <@td><a href="<@pageUrl>orderview?orderId=${orderHeaderAndRole.orderId}</@pageUrl>" class="${styles.link_nav_info_id!}">${orderHeaderAndRole.orderId}</a></@td>
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
                <a href="<@pageUrl>OrderDeliveryScheduleInfo?orderId=${orderHeaderAndRole.orderId}</@pageUrl>" class="${styles.link_nav!} ${styles.action_update!}">Schedule&nbsp;Delivery</a>
              </@td>
            </@tr>
          </#list>
        </@table>
      </@section>
    </#if>

    <#if partyTasks?has_content>
      <#assign tasksFound = true>
      <@section title=uiLabelMap.OrderWorkflow>
        <@table type="data-list" autoAltRows=true>
         <@thead>
          <@tr class="header-row">
            <@th><a href="<@pageUrl>tasklist?sort=orderId</@pageUrl>">${uiLabelMap.OrderOrderNumber}</a></@th>
            <@th><a href="<@pageUrl>tasklist?sort=name</@pageUrl>">${uiLabelMap.CommonName}</a></@th>
            <@th><a href="<@pageUrl>tasklist?sort=orderDate</@pageUrl>">${uiLabelMap.OrderOrderDate}</a></@th>
            <@th width="1" align="right"><a href="<@pageUrl>tasklist?sort=grandTotal</@pageUrl>">Total</a></@th>
            <@th width="1">&nbsp;&nbsp;</@th>
            <@th><a href="<@pageUrl>tasklist?sort=actualStartDate</@pageUrl>">${uiLabelMap.OrderStartDateTime}</a></@th>
            <@th><a href="<@pageUrl>tasklist?sort=priority</@pageUrl>">${uiLabelMap.CommonPriority}</a></@th>
            <@th><a href="<@pageUrl>tasklist?sort=currentStatusId</@pageUrl>">${uiLabelMap.CommonMyStatus}</a></@th>
          </@tr>
          </@thead>
          <#list partyTasks as task>
            <@tr valign="middle">
              <@td>
                <#assign orderStr = "orderId=" + task.orderId + "&amp;partyId=" + userLogin.partyId + "&amp;roleTypeId=" + task.roleTypeId + "&amp;workEffortId=" + task.workEffortId + "&amp;fromDate=" + task.get("fromDate").toString()>
                <a href="<@pageUrl>orderview?${orderStr}</@pageUrl>" class="${styles.link_nav_info_id!}">
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
                <a href="<@serverUrl>/workeffort/control/activity?workEffortId=${task.workEffortId}${rawString(externalKeyParam)}</@serverUrl>" target="workeffort" class="${styles.link_nav_info_name!}">
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
        <@table type="data-list" autoAltRows=true>
         <@thead>
          <@tr class="header-row">
            <@th><a href="<@pageUrl>tasklist?sort=orderId</@pageUrl>">${uiLabelMap.OrderOrderNumber}</a></@th>
            <@th><a href="<@pageUrl>tasklist?sort=name</@pageUrl>">${uiLabelMap.CommonName}</a></@th>
            <@th><a href="<@pageUrl>tasklist?sort=orderDate</@pageUrl>">${uiLabelMap.OrderOrderDate}</a></@th>
            <@th width="1" align="right"><a href="<@pageUrl>tasklist?sort=grandTotal</@pageUrl>">${uiLabelMap.CommonTotal}</a></@th>
            <@th width="1">&nbsp;&nbsp;</@th>
            <@th><a href="<@pageUrl>tasklist?sort=actualStartDate</@pageUrl>">${uiLabelMap.CommonStartDateTime}</a></@th>
            <@th><a href="<@pageUrl>tasklist?sort=wepaPartyId</@pageUrl>">${uiLabelMap.PartyParty}</a></@th>
            <@th><a href="<@pageUrl>tasklist?sort=roleTypeId</@pageUrl>">${uiLabelMap.PartyRole}</a></@th>
            <@th><a href="<@pageUrl>tasklist?sort=priority</@pageUrl>">${uiLabelMap.CommonPriority}</a></@th>
            <@th><a href="<@pageUrl>tasklist?sort=currentStatusId</@pageUrl>">${uiLabelMap.CommonStatus}</a></@th>
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
                  <a href="<@serverUrl>/workeffort/control/activity?workEffortId=${task.workEffortId}</@serverUrl>" target="workeffort" class="${styles.link_nav_info_name!}">
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