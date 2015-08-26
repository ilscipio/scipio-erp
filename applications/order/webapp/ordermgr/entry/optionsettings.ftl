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

<#if security.hasEntityPermission("ORDERMGR", "_CREATE", session) || security.hasEntityPermission("ORDERMGR", "_PURCHASE_CREATE", session)>
<@section>
<form method="post" action="<@ofbizUrl>finalizeOrder</@ofbizUrl>" name="checkoutsetupform">
      <@row>
        <@cell class="${styles.grid_large!}6">      
            <@table type="fields" width="100%" cellpadding="1" border="0" cellpadding="0" cellspacing="0">
              <@thead>
              <@tr>
                <@th>
                  ${uiLabelMap.OrderInternalNote}
                </@th>
                <@th>
                  ${uiLabelMap.OrderShippingNotes}
                </@th>
              </@tr>
              </@thead>
              <@tr>
                <@td>
                  <textarea cols="30" rows="3" name="internal_order_notes"><#if (cart.getInternalOrderNotes().size()>0)>${(cart.getInternalOrderNotes()[0])!}</#if></textarea>
                </@td>
                <@td>
                  <textarea cols="30" rows="3" name="shippingNotes"><#if (cart.getOrderNotes().size()>0)>${(cart.getOrderNotes()[0])!}</#if></textarea>
                </@td>
              </@tr>
            </@table>
    </@cell>
  </@row>
</@section>
              <input type="hidden" name="finalizeMode" value="options"/>
<#list 1..cart.getShipGroupSize() as currIndex>
<#assign shipGroupIndex = currIndex - 1>

<#if cart.getShipmentMethodTypeId(shipGroupIndex)?? && cart.getCarrierPartyId(shipGroupIndex)??>
    <#assign chosenShippingMethod = cart.getShipmentMethodTypeId(shipGroupIndex) + '@' + cart.getCarrierPartyId(shipGroupIndex)>
</#if>
<#assign supplierPartyId = cart.getSupplierPartyId(shipGroupIndex)!>
<#assign supplier =  delegator.findOne("PartyGroup", Static["org.ofbiz.base.util.UtilMisc"].toMap("partyId", supplierPartyId), false)! />
  <#assign sectionTitle>${uiLabelMap.OrderShipGroup} ${uiLabelMap.CommonNbr} ${currIndex}<#if supplier?has_content> - ${supplier.groupName?default(supplier.partyId)}</#if></#assign>
  <@section title=sectionTitle>   
    <@row>
        <@cell class="${styles.grid_large!}6">
            <@table type="fields" class="basic-table">  
               <#if cart.getOrderType() != "PURCHASE_ORDER">

              <@tr>
                     <@td class="${styles.grid_large!}3 top">
                       ${uiLabelMap.ProductShipmentMethod}
                </@td>
                     <@td>
                <#assign shipEstimateWrapper = Static["org.ofbiz.order.shoppingcart.shipping.ShippingEstimateWrapper"].getWrapper(dispatcher, cart, 0)>
                <#assign carrierShipmentMethods = shipEstimateWrapper.getShippingMethods()>
                <#list carrierShipmentMethods as carrierShipmentMethod>
                        
                        
                    <#assign shippingMethod = carrierShipmentMethod.shipmentMethodTypeId + "@" + carrierShipmentMethod.partyId>
                            <#assign radioText>
                      <#if carrierShipmentMethod.partyId != "_NA_">${carrierShipmentMethod.partyId!}&nbsp;</#if>${carrierShipmentMethod.description!}
                      <#if cart.getShippingContactMechId(shipGroupIndex)??>
                        <#assign shippingEst = shipEstimateWrapper.getShippingEstimate(carrierShipmentMethod)?default(-1)>
                        <#if shippingEst?has_content>
                          &nbsp;-&nbsp;
                          <#if (shippingEst > -1)>
                            <@ofbizCurrency amount=shippingEst isoCode=cart.getCurrency()/>
                          <#else>
                            Calculated Offline
                          </#if>
                        </#if>
                      </#if>
                            </#assign>
                            <@field type="radio" name="${shipGroupIndex?default(0)}_shipping_method" value="${shippingMethod}" id="${shipGroupIndex?default(0)}_shipping_method_${shippingMethod}" label=radioText checked=chosenShippingMethod?default("N@A")/>                   
                </#list>
                <#if !carrierShipmentMethodList?? || carrierShipmentMethodList?size == 0>
                        
                            <@field type="radio" name="${shipGroupIndex?default(0)}_shipping_method" value="Default" checked="checked" label="${uiLabelMap.FacilityNoOtherShippingMethods}"/>
        
                        </#if>
                             
                  </@td>
                </@tr>
                    
               <#else>
                   <@tr>
                     <@td class="${styles.grid_large!}3">
                       ${uiLabelMap.OrderOrderShipEstimate}
                     </@td>
                     <@td>
                     <input type='hidden' name='${shipGroupIndex?default("0")}_shipping_method' value="STANDARD@_NA_" />
                       <input type='text' name='${shipGroupIndex?default("0")}_ship_estimate' value="${cart.getItemShipGroupEstimate(shipGroupIndex?default('0'))!}"/>
                     </@td>
                   </@tr>
               </#if>
                <@tr>
                  <@td class="${styles.grid_large!}3">
                    ${uiLabelMap.FacilityShipOnceOrAvailable}
                  </@td>
                  <@td>
                    <@field type="radio" name="${shipGroupIndex?default(0)}_may_split" value="false" checked=cart.getMaySplit(shipGroupIndex)?default("N") label="${uiLabelMap.FacilityWaitEntireOrderReady}"/>
                    <@field type="radio" name="${shipGroupIndex?default(0)}_may_split" value="true" checked=cart.getMaySplit(shipGroupIndex)?default("N") label="${uiLabelMap.FacilityShipAvailable}"/>
                  </@td>
                </@tr>
                <@tr>
                  <@td class="${styles.grid_large!}3">
                    ${uiLabelMap.OrderShipBeforeDate}
                  </@td>
                    <@td>
                      <@htmlTemplate.renderDateTimeField name="sgi${shipGroupIndex?default('0')}_shipBeforeDate" event="" action="" value="${(cart.getShipBeforeDate(shipGroupIndex))!}" className=""  title="Format: yyyy-MM-dd HH:mm:ss.SSS" size="25" maxlength="30" id="sgi${shipGroupIndex?default('0')}_shipBeforeDate" dateType="date" shortDateInput=false timeDropdownParamName="" defaultDateTimeString="" localizedIconTitle="" timeDropdown="" timeHourName="" classString="" hour1="" hour2="" timeMinutesName="" minutes="" isTwelveHour="" ampmName="" amSelected="" pmSelected="" compositeType="" formName=""/>
                    </@td>
                </@tr>
                <@tr>
                  <@td class="${styles.grid_large!}3">
                    ${uiLabelMap.OrderShipAfterDate}
                  </@td>
                    <@td>
                      <@htmlTemplate.renderDateTimeField name="sgi${shipGroupIndex?default('0')}_shipAfterDate" event="" action="" value="${(cart.getShipAfterDate(shipGroupIndex))!}" className=""  title="Format: yyyy-MM-dd HH:mm:ss.SSS" size="25" maxlength="30" id="sgi${shipGroupIndex?default('0')}_shipAfterDate" dateType="date" shortDateInput=false timeDropdownParamName="" defaultDateTimeString="" localizedIconTitle="" timeDropdown="" timeHourName="" classString="" hour1="" hour2="" timeMinutesName="" minutes="" isTwelveHour="" ampmName="" amSelected="" pmSelected="" compositeType="" formName=""/>
                    </@td>
                </@tr>
                <@tr>
                  <@td class="${styles.grid_large!}3">
                    ${uiLabelMap.FacilitySpecialInstructions}
                  </@td>
                  <@td>
                    <textarea cols="30" rows="3" name="${shipGroupIndex?default("0")}_shipping_instructions">${cart.getShippingInstructions(shipGroupIndex)!}</textarea>
                  </@td>
                </@tr>

                <#if cart.getOrderType() == 'PURCHASE_ORDER'>
                    <input type="hidden" name="${shipGroupIndex?default('0')}_is_gift" value="false" />
                <#else>
                    <#if (productStore.showCheckoutGiftOptions)?default('Y') != 'N'>
                        <@tr>
                            <@td class="${styles.grid_large!}3">${uiLabelMap.OrderIsThisGift}</@td>
                            <@td>
                                <@field type="radio" name="${shipGroupIndex?default('0')}_is_gift" value="true" checked=cart.getIsGift(shipGroupIndex)?default('N') label="${uiLabelMap.CommonYes}"/>
                                <@field type="radio" name="${shipGroupIndex?default('0')}_is_gift" value="false" checked=cart.getIsGift(shipGroupIndex)?default('Y') label="${uiLabelMap.CommonNo}"/>
                            </@td>
                        </@tr>
                    </#if>
                    <@tr>
                        <@td>
                            ${uiLabelMap.OrderGiftMessage}
                        </@td>
                        <@td>
                            <textarea cols="30" rows="3" name="${shipGroupIndex?default('0')}_gift_message">${cart.getGiftMessage(shipGroupIndex)!}</textarea>
                        </@td>
                    </@tr>
                </#if>
              </@table>
          </@cell>
      </@row>
   </@section>
</#list>
</form>
<#else>
  <@alert type="error">${uiLabelMap.OrderViewPermissionError}</@alert>
</#if>
