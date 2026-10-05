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
<#--
Changes to this file: Copyright (C) Ilscipio GmbH. The changes are licensed
under the GNU Affero General Public License, version 3, or a commercial
license from Ilscipio GmbH (file LICENSE). The original code stays under
the Apache License, version 2.0, as stated above.
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
        <#assign returnItems = delegator.findByAnd("ReturnItem", {"returnId":returnId, "returnTypeId":"RTN_REFUND"}, ["returnItemSeqId"], false)/>
        <#if returnItems?has_content>
          <#assign orderId = (Static["org.ofbiz.entity.util.EntityUtil"].getFirst(returnItems)).getString("orderId")/>
          <#assign partyId = "${(returnHeader.fromPartyId)!}"/>
          <@menuitem type="link" href=makePageUrl("setOrderCurrencyAgreementShipDates?partyId=${partyId!}&originOrderId=${orderId!}") text="${rawLabel('OrderCreateExchangeOrder')} ${rawLabel('CommonFor')} ${raw(orderId!)}" class="+${styles.action_run_sys!} ${styles.action_add!}" />
        </#if>
        <#if "RETURN_ACCEPTED" == returnHeader.statusId>
          <#assign returnItems = delegator.findByAnd("ReturnItem", {"returnId" : returnId}, ["returnItemSeqId"], false)/>
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
