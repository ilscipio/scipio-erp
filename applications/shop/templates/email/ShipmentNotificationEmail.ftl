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

<#if baseEcommerceSecureUrl??><#assign urlPrefix = baseEcommerceSecureUrl/></#if>
<#if shipment?has_content>
  <@section title=(title!)>
    <@table type="data-complex"> <#-- orig: border="0" cellpadding="0" cellspacing="0" -->
      <@tbody>
        <@tr>
          <@td><b>${uiLabelMap.OrderTrackingNumber}</b></@td>
        </@tr>
        <#list orderShipmentInfoSummaryList as orderShipmentInfoSummary>
          <@tr>
            <@td>
              Code: ${orderShipmentInfoSummary.trackingCode!"[Not Yet Known]"}
              <#if orderShipmentInfoSummary.carrierPartyId?has_content>(${uiLabelMap.ProductCarrier}: ${orderShipmentInfoSummary.carrierPartyId})</#if>
            </@td>
          </@tr>
        </#list>
      </@tbody>
    </@table>

    <@section title="<b>${uiLabelMap.EcommerceShipmentItems}</b>">
      <@table type="data-complex"> <#-- orig: width="100%" border="0" cellpadding="0" -->
        <@tr valign="bottom">
          <@td width="35%"><span class="tableheadtext"><b>${uiLabelMap.OrderProduct}</b></span></@td>
          <@td width="10%" align="right"><span class="tableheadtext"><b>${uiLabelMap.OrderQuantity}</b></span></@td>
        </@tr>
        <@tr type="util"><@td colspan="10"><hr /></@td></@tr>
        <#list shipmentItems as shipmentItem>
          <#assign productId = shipmentItem.productId>
          <#assign product = shipmentItem.getRelatedOne("Product", false)>
          <@tr>
            <@td colspan="1" valign="top">${productId!} - ${product.internalName!}</@td>
            <@td align="right" valign="top">${shipmentItem.quantity!}</@td>
          </@tr>
        </#list>
        <@tr type="util"><@td colspan="10"><hr /></@td></@tr>
      </@table>
    </@section>
  </@section>
</#if>
