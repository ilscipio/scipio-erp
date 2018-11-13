<#--
This file is subject to the terms and conditions defined in
 file 'LICENSE', which is part of this source code package.
-->

<@script>
    function paginateOrderList(viewSize, viewIndex) {
        document.paginationForm.viewSize.value = viewSize;
        document.paginationForm.viewIndex.value = viewIndex;
        document.paginationForm.submit();
    }
</@script>

<@section title=uiLabelMap.OrderOrderFound>
    <#-- SCIPIO: using @paginate, but loop still relevant
    <form name="paginationForm" method="post" action="<@ofbizUrl>viewProductOrder</@ofbizUrl>">
      <input type="hidden" name="viewSize"/>
      <input type="hidden" name="viewIndex"/>-->
      <#if paramIdList?? && paramIdList?has_content>
        <#list paramIdList as paramIds>
          <#assign paramId = paramIds.split("=")/>
          <#if "productId" == paramId[0]>
            <#assign productId = paramId[1]/>
          </#if>
          <#--<input type="hidden" name="${paramId[0]}" value="${paramId[1]}"/>-->
        </#list>
      </#if>
    <#--</form>-->
  <#if orderList?has_content && productId??>
  <#-- forcePost required because search done from service event with https="true" -->
  <@paginate mode="content" url=makeOfbizUrl("viewProductOrder") paramStr=paramList viewSize=viewSize!1 viewIndex=viewIndex!0 listSize=orderListSize!0 altParam=true viewIndexFirst=1 forcePost=true>
    <@table type="data-list"> <#-- orig: class="basic-table hover-bar" --> <#-- orig: cellspacing="0" -->
     <@thead>
      <@tr class="header-row">
        <@th>${uiLabelMap.OrderOrderId}</@th>
        <@th>${uiLabelMap.FormFieldTitle_itemStatusId}</@th>
        <@th>${uiLabelMap.FormFieldTitle_orderItemSeqId}</@th>
        <@th>${uiLabelMap.OrderDate}</@th>
        <@th>${uiLabelMap.OrderUnitPrice}</@th>
        <@th>${uiLabelMap.OrderQuantity}</@th>
        <@th>${uiLabelMap.OrderOrderType}</@th>
      </@tr>
      </@thead>
        <#list orderList as order>
          <#assign orderItems = delegator.findByAnd("OrderItem", {"orderId" : order.orderId, "productId" : productId}, null, false)/>
          <#list orderItems as orderItem>
            <@tr>
              <@td><a href="<@ofbizInterWebappUrl>/ordermgr/control/orderview?orderId=${orderItem.orderId}</@ofbizInterWebappUrl>" class="${styles.link_nav_info_id!}">${orderItem.orderId}</a></@td>
              <#assign currentItemStatus = orderItem.getRelatedOne("StatusItem", false)/>
              <@td>${currentItemStatus.get("description",locale)?default(currentItemStatus.statusId)}</@td>
              <@td>${orderItem.orderItemSeqId}</@td>
              <@td>${order.orderDate}</@td>
              <@td>${orderItem.unitPrice}</@td>
              <@td>${orderItem.quantity}</@td>
              <#assign currentOrderType = order.getRelatedOne("OrderType", false)/>
              <@td>${currentOrderType.get("description",locale)?default(currentOrderType.orderTypeId)}</@td>
            </@tr>
          </#list>
        </#list>
    </@table>
  </@paginate>
  <#else>
    <@commonMsg type="result-norecord">${uiLabelMap.OrderNoOrderFound}.</@commonMsg>
  </#if>
</@section>
