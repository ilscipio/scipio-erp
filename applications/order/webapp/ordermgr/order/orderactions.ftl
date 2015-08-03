<#if security.hasEntityPermission("ORDERMGR", "_UPDATE", session) && (!orderHeader.salesChannelEnumId?? || orderHeader.salesChannelEnumId != "POS_SALES_CHANNEL")>
<#-- ${uiLabelMap.OrderActions}-->
<@section>
      <ul class="button-group">
        <#if security.hasEntityPermission("FACILITY", "_CREATE", session) && ((orderHeader.statusId == "ORDER_APPROVED") || (orderHeader.statusId == "ORDER_SENT"))>
          <#-- Special shipment options -->
          <#if orderHeader.orderTypeId == "SALES_ORDER">
            <li>
            <form name="quickShipOrder" method="post" action="<@ofbizUrl>quickShipOrder</@ofbizUrl>">
              <input type="hidden" name="orderId" value="${orderId}"/>
            </form>
            <a href="javascript:document.quickShipOrder.submit()" class="${styles.button_default!}">${uiLabelMap.OrderQuickShipEntireOrder}</a></li>
          <#else> <#-- PURCHASE_ORDER -->
            <#--<#if orderHeader.orderTypeId == "PURCHASE_ORDER">${uiLabelMap.ProductDestinationFacility}</#if>-->
            <#if ownedFacilities?has_content>
              
              
              <#-- FIXME
              <#if !allShipments?has_content>
                  <li>
                     <form action="/facility/control/quickShipPurchaseOrder?externalLoginKey=${externalLoginKey}" method="post">
                       <input type="hidden" name="initialSelected" value="Y"/>
                       <input type="hidden" name="orderId" value="${orderId}"/>
                       <input type="hidden" name="purchaseOrderId" value="${orderId}"/>
                      <select name="facilityId">
                        <#list ownedFacilities as facility>
                          <option value="${facility.facilityId}">${facility.facilityName}</option>
                        </#list>
                      </select>
                      <input type="submit" class="smallSubmit" value="${uiLabelMap.OrderQuickReceivePurchaseOrder}"/>
                     </form>
                  </li>
                  <li>
                    <form name="receivePurchaseOrderForm" action="/facility/control/quickShipPurchaseOrder?externalLoginKey=${externalLoginKey}" method="post">
                      <input type="hidden" name="initialSelected" value="Y"/>
                      <input type="hidden" name="orderId" value="${orderId}"/>
                      <input type="hidden" name="purchaseOrderId" value="${orderId}"/>
                      <input type="hidden" name="partialReceive" value="Y"/>
                      <select name="facilityId">
                        <#list ownedFacilities as facility>
                          <option value="${facility.facilityId}">${facility.facilityName}</option>
                        </#list>
                      </select>
                      </form>
                      <a href="javascript:document.receivePurchaseOrderForm.submit()" class="${styles.button_default!}">${uiLabelMap.CommonReceive}</a>
                  </li>
              <#else>
                  <li>
                    <form name="receiveInventoryForm" action="/facility/control/ReceiveInventory" method="post">
                      <input type="hidden" name="initialSelected" value="Y"/>
                      <input type="hidden" name="purchaseOrderId" value="${orderId!}"/>
                      <select name="facilityId">
                        <#list ownedFacilities as facility>
                          <option value="${facility.facilityId}">${facility.facilityName}</option>
                        </#list>
                      </select>
                    </form>
                    <a href="javascript:document.receiveInventoryForm.submit()" class="${styles.button_default!}">${uiLabelMap.OrderQuickReceivePurchaseOrder}</a>
                  </li>
                  <li>
                    <form name="partialReceiveInventoryForm" action="/facility/control/ReceiveInventory" method="post">
                      <input type="hidden" name="initialSelected" value="Y"/>
                      <input type="hidden" name="purchaseOrderId" value="${orderId!}"/>
                      <input type="hidden" name="partialReceive" value="Y"/>
                      <select name="facilityId">
                        <#list ownedFacilities as facility>
                           <option value="${facility.facilityId}">${facility.facilityName}</option>
                         </#list>
                       </select>
                    </form>
                    <a href="javascript:document.partialReceiveInventoryForm.submit()" class="${styles.button_default!}">${uiLabelMap.CommonReceive}</a>
                  </li>
              </#if>
              
              <#if orderHeader.statusId != "ORDER_COMPLETED">
                  <li>
                    <form action="<@ofbizUrl>completePurchaseOrder?externalLoginKey=${externalLoginKey}</@ofbizUrl>" method="post">
                     <input type="hidden" name="orderId" value="${orderId}"/>
                    <select name="facilityId">
                      <#list ownedFacilities as facility>
                        <option value="${facility.facilityId}">${facility.facilityName}</option>
                      </#list>
                    </select>
                    <input type="submit" class="smallSubmit" value="${uiLabelMap.OrderForceCompletePurchaseOrder}"/>
                    </form>
                  </li>
              </#if>
              -->
            </#if>
          </#if>
        </#if>
        <#-- Refunds/Returns for Sales Orders and Delivery Schedules -->
        <#if orderHeader.statusId != "ORDER_COMPLETED" && orderHeader.statusId != "ORDER_CANCELLED">
          <li><a href="<@ofbizUrl>OrderDeliveryScheduleInfo?orderId=${orderId}</@ofbizUrl>" class="${styles.button_default!}">${uiLabelMap.OrderViewEditDeliveryScheduleInfo}</a></li>
        </#if>
        <#if security.hasEntityPermission("ORDERMGR", "_RETURN", session) && orderHeader.statusId == "ORDER_COMPLETED">
          <#if returnableItems?has_content>
            <li>
            <form name="quickRefundOrder" method="post" action="<@ofbizUrl>quickRefundOrder</@ofbizUrl>">
              <input type="hidden" name="orderId" value="${orderId}"/>
              <input type="hidden" name="receiveReturn" value="true"/>
              <input type="hidden" name="returnHeaderTypeId" value="${returnHeaderTypeId}"/>
            </form>
            <a href="javascript:document.quickRefundOrder.submit()" class="${styles.button_default!}">${uiLabelMap.OrderQuickRefundEntireOrder}</a>
            </li>
            <li>
            <form name="quickreturn" method="post" action="<@ofbizUrl>quickreturn</@ofbizUrl>">
              <input type="hidden" name="orderId" value="${orderId}"/>
              <input type="hidden" name="party_id" value="${partyId!}"/>
              <input type="hidden" name="returnHeaderTypeId" value="${returnHeaderTypeId}"/>
              <input type="hidden" name="needsInventoryReceive" value="${needsInventoryReceive?default("N")}"/>
            </form>
            <a href="javascript:document.quickreturn.submit()" class="${styles.button_default!}">${uiLabelMap.OrderCreateReturn}</a>
            </li>
          </#if>
        </#if>

        <#if orderHeader?has_content && orderHeader.statusId != "ORDER_CANCELLED">
          <#if orderHeader.statusId != "ORDER_COMPLETED">
            <#--
              <li><a href="<@ofbizUrl>cancelOrderItem?${paramString}</@ofbizUrl>" class="${styles.button_default!}">${uiLabelMap.OrderCancelAllItems}</a></li>
            -->
            <li><a href="<@ofbizUrl>editOrderItems?${paramString}</@ofbizUrl>" class="${styles.button_default!}">${uiLabelMap.OrderEditItems}</a></li>
            <li>
            <form name="createOrderItemShipGroup" method="post" action="<@ofbizUrl>createOrderItemShipGroup</@ofbizUrl>">
              <input type="hidden" name="orderId" value="${orderId}"/>
            </form>
            <a href="javascript:document.createOrderItemShipGroup.submit()" class="${styles.button_default!}">${uiLabelMap.OrderCreateShipGroup}</a>
            </li>
          </#if>
          <li><a href="<@ofbizUrl>loadCartFromOrder?${paramString}&amp;finalizeMode=init</@ofbizUrl>" class="${styles.button_default!}">${uiLabelMap.OrderCreateAsNewOrder}</a></li>
          <#if orderHeader.statusId == "ORDER_COMPLETED">
            <li><a href="<@ofbizUrl>loadCartForReplacementOrder?${paramString}</@ofbizUrl>" class="${styles.button_default!}">${uiLabelMap.OrderCreateReplacementOrder}</a></li>
          </#if>
        </#if>
        <li><a href="<@ofbizUrl>OrderHistory?orderId=${orderId}</@ofbizUrl>" class="${styles.button_default!}">${uiLabelMap.OrderViewOrderHistory}</a></li>
        <li><a href="<@ofbizUrl>order.pdf?orderId=${orderId}</@ofbizUrl>" target="_blank" class="${styles.button_default!}">PDF</a></li>
      </ul>
      
      
      <ul class="button-group">
                <#if currentStatus.statusId == "ORDER_APPROVED" && orderHeader.orderTypeId == "SALES_ORDER">
                  <li><a href="javascript:document.PrintOrderPickSheet.submit()" class="${styles.button_default!}">${uiLabelMap.FormFieldTitle_printPickSheet}</a>
                  <form name="PrintOrderPickSheet" method="post" action="<@ofbizUrl>orderPickSheet.pdf</@ofbizUrl>" target="_BLANK">
                    <input type="hidden" name="facilityId" value="${storeFacilityId!}"/>
                    <input type="hidden" name="orderId" value="${orderHeader.orderId!}"/>
                    <input type="hidden" name="maxNumberOfOrdersToPrint" value="1"/>
                  </form>
                  </li>
                </#if>
                <#if currentStatus.statusId == "ORDER_CREATED" || currentStatus.statusId == "ORDER_PROCESSING">
                  <li><a href="javascript:document.OrderApproveOrder.submit()" class="${styles.button_default!}">${uiLabelMap.OrderApproveOrder}</a>
                  <form name="OrderApproveOrder" method="post" action="<@ofbizUrl>changeOrderStatus/orderview</@ofbizUrl>">
                    <input type="hidden" name="statusId" value="ORDER_APPROVED"/>
                    <input type="hidden" name="newStatusId" value="ORDER_APPROVED"/>
                    <input type="hidden" name="setItemStatus" value="Y"/>
                    <input type="hidden" name="workEffortId" value="${workEffortId!}"/>
                    <input type="hidden" name="orderId" value="${orderId!}"/>
                    <input type="hidden" name="partyId" value="${assignPartyId!}"/>
                    <input type="hidden" name="roleTypeId" value="${assignRoleTypeId!}"/>
                    <input type="hidden" name="fromDate" value="${fromDate!}"/>
                  </form>
                  </li>
                <#elseif currentStatus.statusId == "ORDER_APPROVED">
                  <li><a href="javascript:document.OrderHold.submit()" class="${styles.button_default!}">${uiLabelMap.OrderHold}</a>
                  <form name="OrderHold" method="post" action="<@ofbizUrl>changeOrderStatus/orderview</@ofbizUrl>">
                    <input type="hidden" name="statusId" value="ORDER_HOLD"/>
                    <input type="hidden" name="workEffortId" value="${workEffortId!}"/>
                    <input type="hidden" name="orderId" value="${orderId!}"/>
                    <input type="hidden" name="partyId" value="${assignPartyId!}"/>
                    <input type="hidden" name="roleTypeId" value="${assignRoleTypeId!}"/>
                    <input type="hidden" name="fromDate" value="${fromDate!}"/>
                  </form>
                  </li>
                <#elseif currentStatus.statusId == "ORDER_HOLD">
                  <li><a href="javascript:document.OrderApproveOrder.submit()" class="${styles.button_default!}">${uiLabelMap.OrderApproveOrder}</a>
                  <form name="OrderApproveOrder" method="post" action="<@ofbizUrl>changeOrderStatus/orderview</@ofbizUrl>">
                    <input type="hidden" name="statusId" value="ORDER_APPROVED"/>
                    <input type="hidden" name="setItemStatus" value="Y"/>
                    <input type="hidden" name="workEffortId" value="${workEffortId!}"/>
                    <input type="hidden" name="orderId" value="${orderId!}"/>
                    <input type="hidden" name="partyId" value="${assignPartyId!}"/>
                    <input type="hidden" name="roleTypeId" value="${assignRoleTypeId!}"/>
                    <input type="hidden" name="fromDate" value="${fromDate!}"/>
                  </form>
                  </li>
                </#if>
                <#if currentStatus.statusId != "ORDER_COMPLETED" && currentStatus.statusId != "ORDER_CANCELLED">
                  <li><a href="javascript:document.OrderCancel.submit()" class="${styles.button_default!}">${uiLabelMap.OrderCancelOrder}</a>
                  <form name="OrderCancel" method="post" action="<@ofbizUrl>changeOrderStatus/orderview</@ofbizUrl>">
                    <input type="hidden" name="statusId" value="ORDER_CANCELLED"/>
                    <input type="hidden" name="setItemStatus" value="Y"/>
                    <input type="hidden" name="workEffortId" value="${workEffortId!}"/>
                    <input type="hidden" name="orderId" value="${orderId!}"/>
                    <input type="hidden" name="partyId" value="${assignPartyId!}"/>
                    <input type="hidden" name="roleTypeId" value="${assignRoleTypeId!}"/>
                    <input type="hidden" name="fromDate" value="${fromDate!}"/>
                  </form>
                  </li>
                </#if>
                <#if setOrderCompleteOption>
                  <li><a href="javascript:document.OrderCompleteOrder.submit()" class="${styles.button_default!}">${uiLabelMap.OrderCompleteOrder}</a>
                  <form name="OrderCompleteOrder" method="post" action="<@ofbizUrl>changeOrderStatus</@ofbizUrl>">
                    <input type="hidden" name="statusId" value="ORDER_COMPLETED"/>
                    <input type="hidden" name="orderId" value="${orderId!}"/>
                  </form>
                  </li>
                </#if>
            </ul>
      
    </@section>
</#if>