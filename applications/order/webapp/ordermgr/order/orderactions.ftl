<#if security.hasEntityPermission("ORDERMGR", "_UPDATE", request) && (!orderHeader.salesChannelEnumId?? || orderHeader.salesChannelEnumId != "POS_SALES_CHANNEL")>
  <@section> <#-- title=uiLabelMap.OrderActions -->
      <@menu type="button">
        <#if security.hasEntityPermission("FACILITY", "_CREATE", request) && ((orderHeader.statusId == "ORDER_APPROVED") || (orderHeader.statusId == "ORDER_SENT"))>
          <#-- Special shipment options -->
          <#if orderHeader.orderTypeId == "SALES_ORDER">
          <#else> <#-- PURCHASE_ORDER -->
            <#--<#if orderHeader.orderTypeId == "PURCHASE_ORDER">${uiLabelMap.ProductDestinationFacility}</#if>-->
            <#if ownedFacilities?has_content>
              
              <#-- FIXME
              <#if !allShipments?has_content>
                  <@menuitem type="generic">
                     <form action="<@serverUrl>/facility/control/quickShipPurchaseOrder<#if externalLoginKey?has_content>?externalLoginKey=${externalLoginKey}</#if></@serverUrl>" method="post">
                       <input type="hidden" name="initialSelected" value="Y"/>
                       <input type="hidden" name="orderId" value="${orderId}"/>
                       <input type="hidden" name="purchaseOrderId" value="${orderId}"/>
                      <select name="facilityId">
                        <#list ownedFacilities as facility>
                          <option value="${facility.facilityId}">${facility.facilityName}</option>
                        </#list>
                      </select>
                      <input type="submit" class="${styles.link_run_sys!} ${styles.action_add!}" value="${uiLabelMap.OrderQuickReceivePurchaseOrder}"/>
                     </form>
                  </@menuitem>
                  <@menuitem type="generic">
                    <form name="receivePurchaseOrderForm" action="<@serverUrl>/facility/control/quickShipPurchaseOrder<#if externalLoginKey?has_content>?externalLoginKey=${externalLoginKey}</#if></@serverUrl>" method="post">
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
                      <a href="javascript:document.receivePurchaseOrderForm.submit()" class="${styles.link_run_sys!} ${styles.action_receive!}">${uiLabelMap.CommonReceive}</a>
                  </@menuitem>
              <#else>
                  <@menuitem type="generic">
                    <form name="receiveInventoryForm" action="<@serverUrl>/facility/control/ReceiveInventory</@serverUrl>" method="post">
                      <input type="hidden" name="initialSelected" value="Y"/>
                      <input type="hidden" name="purchaseOrderId" value="${orderId!}"/>
                      <select name="facilityId">
                        <#list ownedFacilities as facility>
                          <option value="${facility.facilityId}">${facility.facilityName}</option>
                        </#list>
                      </select>
                    </form>
                    <a href="javascript:document.receiveInventoryForm.submit()" class="${styles.link_run_sys!} ${styles.action_receive!}">${uiLabelMap.OrderQuickReceivePurchaseOrder}</a>
                  </@menuitem>
                  <@menuitem type="generic">
                    <form name="partialReceiveInventoryForm" action="<@serverUrl>/facility/control/ReceiveInventory</@serverUrl>" method="post">
                      <input type="hidden" name="initialSelected" value="Y"/>
                      <input type="hidden" name="purchaseOrderId" value="${orderId!}"/>
                      <input type="hidden" name="partialReceive" value="Y"/>
                      <select name="facilityId">
                        <#list ownedFacilities as facility>
                           <option value="${facility.facilityId}">${facility.facilityName}</option>
                         </#list>
                       </select>
                    </form>
                    <a href="javascript:document.partialReceiveInventoryForm.submit()" class="${styles.link_run_sys!} ${styles.action_receive!}">${uiLabelMap.CommonReceive}</a>
                  </@menuitem>
              </#if>
              
              <#if orderHeader.statusId != "ORDER_COMPLETED">
                  <@menuitem type="generic">
                    <form action="<@pageUrl>completePurchaseOrder<#if externalLoginKey?has_content>?externalLoginKey=${externalLoginKey}</#if></@pageUrl>" method="post">
                     <input type="hidden" name="orderId" value="${orderId}"/>
                    <select name="facilityId">
                      <#list ownedFacilities as facility>
                        <option value="${facility.facilityId}">${facility.facilityName}</option>
                      </#list>
                    </select>
                    <input type="submit" class="${styles.link_run_sys!} ${styles.action_complete!}" value="${uiLabelMap.OrderForceCompletePurchaseOrder}"/>
                    </form>
                  </@menuitem>
              </#if>
              -->
            </#if>
          </#if>
        </#if>
      </@menu>
      
      <@menu type="button">
        <#if currentStatus.statusId == "ORDER_CREATED" || currentStatus.statusId == "ORDER_PROCESSING">
          <@menuitem type="link" href="javascript:document.OrderApproveOrder.submit()" text=uiLabelMap.OrderApproveOrder class="+${styles.action_run_sys!} ${styles.action_updatestatus!}"><form name="OrderApproveOrder" method="post" action="<@pageUrl>changeOrderStatus/orderview</@pageUrl>">
            <input type="hidden" name="statusId" value="ORDER_APPROVED"/>
            <input type="hidden" name="newStatusId" value="ORDER_APPROVED"/>
            <input type="hidden" name="setItemStatus" value="Y"/>
            <input type="hidden" name="workEffortId" value="${workEffortId!}"/>
            <input type="hidden" name="orderId" value="${orderId!}"/>
            <input type="hidden" name="partyId" value="${assignPartyId!}"/>
            <input type="hidden" name="roleTypeId" value="${assignRoleTypeId!}"/>
            <input type="hidden" name="fromDate" value="${fromDate!}"/>
          </form></@menuitem>
        <#elseif currentStatus.statusId == "ORDER_APPROVED">
          <@menuitem type="link" href="javascript:document.OrderHold.submit()" text=uiLabelMap.OrderHold class="+${styles.action_run_sys!} ${styles.action_updatestatus!}"><form name="OrderHold" method="post" action="<@pageUrl>changeOrderStatus/orderview</@pageUrl>">
            <input type="hidden" name="statusId" value="ORDER_HOLD"/>
            <input type="hidden" name="workEffortId" value="${workEffortId!}"/>
            <input type="hidden" name="orderId" value="${orderId!}"/>
            <input type="hidden" name="partyId" value="${assignPartyId!}"/>
            <input type="hidden" name="roleTypeId" value="${assignRoleTypeId!}"/>
            <input type="hidden" name="fromDate" value="${fromDate!}"/>
          </form></@menuitem>
        <#elseif currentStatus.statusId == "ORDER_HOLD">
          <@menuitem type="link" href="javascript:document.OrderApproveOrder.submit()" text=uiLabelMap.OrderApproveOrder class="+${styles.action_run_sys!} ${styles.action_updatestatus!}"><form name="OrderApproveOrder" method="post" action="<@pageUrl>changeOrderStatus/orderview</@pageUrl>">
            <input type="hidden" name="statusId" value="ORDER_APPROVED"/>
            <input type="hidden" name="setItemStatus" value="Y"/>
            <input type="hidden" name="workEffortId" value="${workEffortId!}"/>
            <input type="hidden" name="orderId" value="${orderId!}"/>
            <input type="hidden" name="partyId" value="${assignPartyId!}"/>
            <input type="hidden" name="roleTypeId" value="${assignRoleTypeId!}"/>
            <input type="hidden" name="fromDate" value="${fromDate!}"/>
          </form></@menuitem>
        </#if>
        <#-- SCIPIO: 2.0.0: Added ORDER_SENT in the condition so orders can't be cancelled if already sent -->
        <#if currentStatus.statusId != "ORDER_COMPLETED" && currentStatus.statusId != "ORDER_CANCELLED" && currentStatus.statusId != "ORDER_SENT">
          <@menuitem type="link" href="javascript:document.OrderCancel.submit()" text=uiLabelMap.OrderCancelOrder class="+${styles.action_run_sys!} ${styles.action_terminate!}"><form name="OrderCancel" method="post" action="<@pageUrl>changeOrderStatus/orderview</@pageUrl>">
            <input type="hidden" name="statusId" value="ORDER_CANCELLED"/>
            <input type="hidden" name="setItemStatus" value="Y"/>
            <input type="hidden" name="workEffortId" value="${workEffortId!}"/>
            <input type="hidden" name="orderId" value="${orderId!}"/>
            <input type="hidden" name="partyId" value="${assignPartyId!}"/>
            <input type="hidden" name="roleTypeId" value="${assignRoleTypeId!}"/>
            <input type="hidden" name="fromDate" value="${fromDate!}"/>
          </form></@menuitem>
        </#if>
        <#if setOrderCompleteOption>
          <@menuitem type="link" href="javascript:document.OrderCompleteOrder.submit()" text=uiLabelMap.OrderCompleteOrder class="+${styles.action_run_sys!} ${styles.action_complete!} ${styles.action_importance_high!}"><form name="OrderCompleteOrder" method="post" action="<@pageUrl>changeOrderStatus</@pageUrl>">
            <input type="hidden" name="statusId" value="ORDER_COMPLETED"/>
            <input type="hidden" name="orderId" value="${orderId!}"/>
          </form></@menuitem>
        </#if>
        <#-- Migrated to OrderShippingSubTabBar
        <#if currentStatus.statusId == "ORDER_APPROVED" && orderHeader.orderTypeId == "SALES_ORDER">
          <@menuitem type="link" href="javascript:document.PrintOrderPickSheet.submit()" text=uiLabelMap.FormFieldTitle_printPickSheet class="+${styles.action_run_sys!} ${styles.action_export!}"><form name="PrintOrderPickSheet" method="post" action="<@pageUrl>orderPickSheet.pdf</@pageUrl>" target="_BLANK">
            <input type="hidden" name="facilityId" value="${storeFacilityId!}"/>
            <input type="hidden" name="orderId" value="${orderHeader.orderId!}"/>
            <input type="hidden" name="maxNumberOfOrdersToPrint" value="1"/>
          </form></@menuitem>
        </#if>
        -->

        <#-- migrated from OrderSubTabBar -->
        <#-- Order Modification -->
        <#-- Disabled for now, until usefulness is evaluated
        <#if currentStatus.statusId != "ORDER_CANCELLED">
          <@menuitem type="link" href=makePageUrl("loadCartFromOrder?orderId=${orderId}&finalizeMode=init") text=uiLabelMap.OrderCreateAsNewOrder class="+${styles.action_run_session!} ${styles.action_add!}"/>
        </#if>
        -->
        <#if currentStatus.statusId == "ORDER_COMPLETED" && currentStatus.statusId != "ORDER_CANCELLED">
          <@menuitem type="link" href=makePageUrl("loadCartFromOrder?orderId=${orderId}&finalizeMode=init") text=uiLabelMap.OrderCreateReplacementOrder class="+${styles.action_run_sys!} ${styles.action_add!}"/>
        </#if>
          <#-- SCIPIO: 2.1.0: Added ORDER_SENT in the condition so order items can't be edited if already sent -->
        <#if currentStatus.statusId != "ORDER_COMPLETED" && currentStatus.statusId != "ORDER_CANCELLED" && currentStatus.statusId != "ORDER_SENT">
          <@menuitem type="link" href=makePageUrl("editOrderItems?orderId=${orderId}") text=uiLabelMap.OrderEditItems class="+${styles.action_nav!}"/>
        </#if>

        <#-- Shipping -->
        <#-- Migreated to Shipment Information-->
            <#-- SCIPIO: 2.1.0: Added ORDER_SENT in the condition so order items can't be edited if already sent -->
            <#if ((!singleOrderItem?has_content || (singleOrderItem?has_content && !singleOrderItem)) &&
                (!allOrderItemsShipped?has_content) || (allOrderItemsShipped?has_content && !allOrderItemsShipped)) &&
                (currentStatus.statusId != "ORDER_COMPLETED" && currentStatus.statusId != "ORDER_CANCELLED" && currentStatus.statusId != "ORDER_SENT")>
              <@menuitem type="generic">
                 <form action="<@pageUrl>createOrderItemShipGroup</@pageUrl>" method="post">
                    <input type="hidden" name="orderId" value="${orderId}"/>
                    <input type="submit" class="${styles.link_run_sys!} ${styles.action_add!}" value="${uiLabelMap.OrderCreateShipGroup}"/>
                 </form>
              </@menuitem>
            </#if>
            <#if security.hasEntityPermission("FACILITY","_CREATE", request)>
                <#if orderHeader.orderTypeId == "SALES_ORDER">
                    <#if orderHeader.statusId == "ORDER_APPROVED">
                        <@menuitem type="generic">
                            <form action="<@pageUrl>quickShipOrder</@pageUrl>" method="post">
                                <input type="hidden" name="orderId" value="${orderId}"/>
                                <input type="submit" class="${styles.link_run_sys!} ${styles.action_complete!}" value="${uiLabelMap.OrderQuickShipEntireOrder}"/>
                            </form>
                        </@menuitem>
<#--                    <#elseif orderHeader.statusId == "ORDER_SENT">-->
<#--                        <@menuitem type="generic">-->
<#--                            <form action="<@pageUrl>completeSalesOrder</@pageUrl>" method="post">-->
<#--                                <input type="hidden" name="orderId" value="${orderId}"/>-->
<#--                                <input type="submit" class="${styles.link_run_sys!} ${styles.action_complete!}" value="${uiLabelMap.OrderComplete}"/>-->
<#--                            </form>-->
<#--                        </@menuitem>-->
                    </#if>
                </#if>
            </#if>
        <#-- Return / Refund -->
        <#if security.hasEntityPermission("ORDERMGR","_RETURN", request)>
            <#-- Apart from the returnableItems check, this is really just a duplicate of OrderQuickRefundEntireOrder
            <#if returnableItems?has_content>
                <#if currentStatus.statusId == "ORDER_COMPLETED">
                    <@menuitem type="generic">
                        <form action="<@pageUrl>quickreturn</@pageUrl>" method="post">
                            <input type="hidden" name="orderId" value="${orderId!}"/>
                            <input type="hidden" name="partyId" value="${partyId!}"/>
                            <input type="hidden" name="returnHeaderTypeId" value="${returnHeaderTypeId!}"/>
                            <input type="hidden" name="needsInventoryReceive" value="${needsInventoryReceive!}"/>
                            <input type="submit" class="${styles.action_nav!}" value="${uiLabelMap.OrderQuickRefundEntireOrder}"/>
                        </form>
                    </@menuitem>
                </#if>
            </#if>-->
            <#if currentStatus.statusId == "ORDER_COMPLETED">
              <@menuitem type="generic">
                <form action="<@pageUrl>quickreturn</@pageUrl>" method="post">
                    <input type="hidden" name="orderId" value="${orderId!}"/>
                    <input type="hidden" name="partyId" value="${partyId!}"/>
                    <input type="hidden" name="returnHeaderTypeId" value="${returnHeaderTypeId!}"/>
                    <input type="hidden" name="needsInventoryReceive" value="${needsInventoryReceive!}"/>
                    <input type="submit" class="${styles.action_nav!} ${styles.action_terminate!}" value="${uiLabelMap.OrderCreateReturn}"/>
                </form>
              </@menuitem>
            </#if>
        </#if>
        <#-- Export -->
            <@menuitem type="link" href=makePageUrl("order.pdf?orderId=${orderId}") text="PDF" class="+${styles.action_run_sys!} ${styles.action_export!}"/>
      </@menu>
       
  </@section>
</#if>