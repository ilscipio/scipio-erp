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
<#assign selected = tabButtonItem?default("void")>
<#if returnHeader??>
    <@menu type="button">
      <li<#if selected="OrderReturnHeader"> class="selected"</#if>><a href="<@ofbizUrl>returnMain?returnId=${returnId!}</@ofbizUrl>" class="${styles.menu_button_itemlink!}">${uiLabelMap.OrderReturnHeader}</a></li>
      <li<#if selected="OrderReturnItems"> class="selected"</#if>><a href="<@ofbizUrl>returnItems?returnId=${returnId!}</@ofbizUrl>" class="${styles.menu_button_itemlink!}">${uiLabelMap.OrderReturnItems}</a></li>
      <li<#if selected="OrderReturnHistory"> class="selected"</#if>><a href="<@ofbizUrl>ReturnHistory?returnId=${returnId!}</@ofbizUrl>" class="${styles.menu_button_itemlink!}">${uiLabelMap.OrderReturnHistory}</a></li>
    </@menu>
  <#if selected != "OrderReturnHistory">
    <@menu type="button" class="+button-style-1">
      <li><a href="<@ofbizUrl>return.pdf?returnId=${returnId!}</@ofbizUrl>" target="_BLANK" class="${styles.menu_button_itemlink!}">PDF</a></li>
      <#if returnId??>
        <#assign returnItems = delegator.findByAnd("ReturnItem", Static["org.ofbiz.base.util.UtilMisc"].toMap("returnId", returnId, "returnTypeId", "RTN_REFUND"), null, false)/>
        <#if returnItems?has_content>
          <#assign orderId = (Static["org.ofbiz.entity.util.EntityUtil"].getFirst(returnItems)).getString("orderId")/>
          <#assign partyId = "${(returnHeader.fromPartyId)!}"/>
          <li><a href="<@ofbizUrl>setOrderCurrencyAgreementShipDates?partyId=${partyId!}&amp;originOrderId=${orderId!}</@ofbizUrl>" class="${styles.menu_button_itemlink!}">${uiLabelMap.OrderCreateExchangeOrder} ${uiLabelMap.CommonFor} ${orderId!}</a></li>
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
                <#assign shipmentRouteSegment = Static["org.ofbiz.entity.util.EntityUtil"].getFirst(delegator.findByAnd("ShipmentRouteSegment", {"shipmentId" : shipGroupShipment.shipmentId}, null, false))>
                <#if shipmentRouteSegment??>
                  <#if "UPS" == shipmentRouteSegment.carrierPartyId>
                    <li><a href="javascript:document.upsEmailReturnLabel.submit();" class="${styles.menu_button_itemlink!}">${uiLabelMap.ProductEmailReturnShippingLabelUPS}</a>
                    <form name="upsEmailReturnLabel" method="post" action="<@ofbizUrl>upsEmailReturnLabelReturn</@ofbizUrl>">
                      <input type="hidden" name="returnId" value="${returnId}"/>
                      <input type="hidden" name="shipmentId" value="${shipGroupShipment.shipmentId}"/>
                      <input type="hidden" name="shipmentRouteSegmentId" value="${shipmentRouteSegment.shipmentRouteSegmentId}" />
                    </form></li>
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
  <@heading>${uiLabelMap.OrderCreateNewReturn}</@heading>
  <#if requestParameters.returnId?has_content>
    <@alert type="error">${uiLabelMap.OrderNoReturnFoundWithId} : ${requestParameters.returnId}</@alert>
  </#if>
</#if>
