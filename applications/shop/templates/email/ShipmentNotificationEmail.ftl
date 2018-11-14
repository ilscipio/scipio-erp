<#--
This file is subject to the terms and conditions defined in the
files 'LICENSE' and 'NOTICE', which are part of this source
code package.
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
