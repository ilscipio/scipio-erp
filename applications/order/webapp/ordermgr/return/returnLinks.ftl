<#--
This file is subject to the terms and conditions defined in the
files 'LICENSE' and 'NOTICE', which are part of this source
code package.
-->
<#assign selected = activeSubMenuItem?default("void")>
<#if returnHeader??>
    <@menu type="button">
      <@menuitem type="link" href=makePageUrl("returnMain?returnId=${returnId!}") text=uiLabelMap.OrderReturnHeader selected=(selected=="OrderReturnHeader") class="+${styles.action_nav!} ${styles.action_terminate!}" />
      <@menuitem type="link" href=makePageUrl("returnItems?returnId=${returnId!}") text=uiLabelMap.OrderReturnItems selected=(selected=="OrderReturnItems") class="+${styles.action_nav!} ${styles.action_terminate!}" />
      <@menuitem type="link" href=makePageUrl("ReturnHistory?returnId=${returnId!}") text=uiLabelMap.OrderReturnHistory selected=(selected=="OrderReturnHistory") class="+${styles.action_nav!} ${styles.action_view!}" />
    </@menu>
  <#if selected != "OrderReturnHistory">
    <@menu type="button" class="+button-style-1">
      <@menuitem type="link" href=makePageUrl("return.pdf?returnId=${returnId!}") text="PDF" target="_BLANK" class="+${styles.action_run_sys!} ${styles.action_export!}" />
      <#if returnId??>
        <#assign returnItems = delegator.findByAnd("ReturnItem", {"returnId":returnId, "returnTypeId":"RTN_REFUND"}, null, false)/>
        <#if returnItems?has_content>
          <#assign orderId = (Static["org.ofbiz.entity.util.EntityUtil"].getFirst(returnItems)).getString("orderId")/>
          <#assign partyId = "${(returnHeader.fromPartyId)!}"/>
          <@menuitem type="link" href=makePageUrl("setOrderCurrencyAgreementShipDates?partyId=${partyId!}&originOrderId=${orderId!}") text="${rawLabel('OrderCreateExchangeOrder')} ${rawLabel('CommonFor')} ${raw(orderId!)}" class="+${styles.action_run_sys!} ${styles.action_add!}" />
        </#if>
        <#if "RETURN_ACCEPTED" == returnHeader.statusId>
          <#assign returnItems = delegator.findByAnd("ReturnItem", {"returnId" : returnId}, null, false)/>
          <#if returnItems?has_content>
            <#assign orderId = (Static["org.ofbiz.entity.util.EntityUtil"].getFirst(returnItems)).getString("orderId")/>
            <#assign shipGroupAssoc = Static["org.ofbiz.entity.util.EntityUtil"].getFirst(delegator.findByAnd("OrderItemShipGroupAssoc", {"orderId" : orderId}, null, false))/>
            <#assign shipGroup = delegator.findOne("OrderItemShipGroup", {"orderId" : orderId, "shipGroupSeqId" : shipGroupAssoc.shipGroupSeqId}, false)>
            <#if shipGroup?? && shipGroup.shipmentMethodTypeId != "NO_SHIPPING">
              <#assign shipGroupShipment = Static["org.ofbiz.entity.util.EntityUtil"].getFirst(delegator.findByAnd("Shipment", {"primaryOrderId" : shipGroup.orderId, "primaryShipGroupSeqId" : shipGroup.shipGroupSeqId}, null, false))/>
              <#if shipGroupShipment??>
                <#assign shipmentRouteSegment = Static["org.ofbiz.entity.util.EntityUtil"].getFirst(delegator.findByAnd("ShipmentRouteSegment", {"shipmentId" : shipGroupShipment.shipmentId}, null, false))!>
                <#if shipmentRouteSegment??>
                  <#if "UPS" == shipmentRouteSegment.carrierPartyId!>
                    <@menuitem type="link" href="javascript:document.upsEmailReturnLabel.submit();" text=uiLabelMap.ProductEmailReturnShippingLabelUPS class="+${styles.action_run_sys!} ${styles.action_send!}">
                      <form name="upsEmailReturnLabel" method="post" action="<@pageUrl>upsEmailReturnLabelReturn</@pageUrl>">
                        <input type="hidden" name="returnId" value="${returnId}"/>
                        <input type="hidden" name="shipmentId" value="${shipGroupShipment.shipmentId}"/>
                        <input type="hidden" name="shipmentRouteSegmentId" value="${shipmentRouteSegment.shipmentRouteSegmentId}" />
                      </form>
                    </@menuitem>
                  </#if>
                </#if>
              </#if>
            </#if>
          </#if>
        </#if>
      </#if>
    </@menu>
  </#if>
<#else>
  <#--<@heading>${uiLabelMap.OrderCreateNewReturn}</@heading>-->
  <#if requestParameters.returnId?has_content>
    <@commonMsg type="error">${uiLabelMap.OrderNoReturnFoundWithId} : ${requestParameters.returnId}</@commonMsg>
  </#if>
</#if>
