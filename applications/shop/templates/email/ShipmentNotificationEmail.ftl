<#--
Scipio Commerce
Copyright (C) Ilscipio GmbH

This file is part of Scipio Commerce. Scipio Commerce is free software: you
can redistribute it and modify it under the terms of the GNU Affero General
Public License, version 3, as published by the Free Software Foundation.
Scipio Commerce is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License
for more details. You should have received a copy of the license with this
work (file LICENSE). If not, see <https://www.gnu.org/licenses/agpl-3.0.html>.
A commercial license is available from Ilscipio GmbH.

SPDX-License-Identifier: AGPL-3.0-only
-->

<#if baseEcommerceSecureUrl??><#assign urlPrefix = baseEcommerceSecureUrl/></#if>
<#if shipment?has_content>
  <@section title=(title!)>
    <@table type="data-complex">
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

    <@section title=uiLabelMap.EcommerceShipmentItems>
      <@table type="data-complex">
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
