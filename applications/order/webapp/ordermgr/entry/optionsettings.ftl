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
<#assign columns=6>
  <form method="post" action="<@ofbizUrl>finalizeOrder</@ofbizUrl>" name="checkoutsetupform">
  <@section>
    <@row>
      <@cell columns=columns>   
        <@field type="textarea" label="${uiLabelMap.OrderInternalNote}" name="internal_order_notes" cols="30" rows="3"><#rt>
          <#if (cart.getInternalOrderNotes().size()>0)>${(cart.getInternalOrderNotes()[0])!}</#if><#t>
        </@field><#lt>
        <@field type="textarea" label="${uiLabelMap.OrderShippingNotes}" name="shippingNotes" cols="30" rows="3"><#rt>
          <#if (cart.getOrderNotes().size()>0)>${(cart.getOrderNotes()[0])!}</#if><#t>
        </@field><#lt>
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
        <@cell columns=columns>
            <#if cart.getOrderType() != "PURCHASE_ORDER">
                <@field type="generic" label="${uiLabelMap.ProductShipmentMethod}">
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
                    <@field type="radio" inlineItems=false name="${shipGroupIndex?default(0)}_shipping_method" value="${shippingMethod}" id="${shipGroupIndex?default(0)}_shipping_method_${shippingMethod}" label=radioText checked=(shippingMethod == (chosenShippingMethod!"N@A"))/>                   
                </#list>
                <#if !carrierShipmentMethodList?? || carrierShipmentMethodList?size == 0>
                    <@field type="radio" inlineItems=false name="${shipGroupIndex?default(0)}_shipping_method" value="Default" checked=true label="${uiLabelMap.FacilityNoOtherShippingMethods}"/>
                </#if>
                </@field>  
            <#else>
                <@field type="generic" label="${uiLabelMap.OrderOrderShipEstimate}">
                    <input type="hidden" name="${shipGroupIndex?default("0")}_shipping_method" value="STANDARD@_NA_" />
                    <input type="text" name="${shipGroupIndex?default("0")}_ship_estimate" value="${cart.getItemShipGroupEstimate(shipGroupIndex?default('0'))!}"/>
                </@field>
            </#if>

                <@field type="generic" label="${uiLabelMap.FacilityShipOnceOrAvailable}">
                    <@field type="radio" inlineItems=false name="${shipGroupIndex?default(0)}_may_split" value="false" checked=((cart.getMaySplit(shipGroupIndex)!"N") == "N") label="${uiLabelMap.FacilityWaitEntireOrderReady}"/>
                    <@field type="radio" inlineItems=false name="${shipGroupIndex?default(0)}_may_split" value="true" checked=((cart.getMaySplit(shipGroupIndex)!"N") == "Y") label="${uiLabelMap.FacilityShipAvailable}"/>
                </@field>
                <@field type="generic" label="${uiLabelMap.OrderShipBeforeDate}">
                    <@htmlTemplate.renderDateTimeField name="sgi${shipGroupIndex?default('0')}_shipBeforeDate" event="" action="" value="${(cart.getShipBeforeDate(shipGroupIndex))!}" className=""  title="Format: yyyy-MM-dd HH:mm:ss.SSS" size="25" maxlength="30" id="sgi${shipGroupIndex?default('0')}_shipBeforeDate" dateType="date" shortDateInput=false timeDropdownParamName="" defaultDateTimeString="" localizedIconTitle="" timeDropdown="" timeHourName="" classString="" hour1="" hour2="" timeMinutesName="" minutes="" isTwelveHour="" ampmName="" amSelected="" pmSelected="" compositeType="" formName=""/>
                </@field>
                <@field type="generic" label="${uiLabelMap.OrderShipAfterDate}">
                    <@htmlTemplate.renderDateTimeField name="sgi${shipGroupIndex?default('0')}_shipAfterDate" event="" action="" value="${(cart.getShipAfterDate(shipGroupIndex))!}" className=""  title="Format: yyyy-MM-dd HH:mm:ss.SSS" size="25" maxlength="30" id="sgi${shipGroupIndex?default('0')}_shipAfterDate" dateType="date" shortDateInput=false timeDropdownParamName="" defaultDateTimeString="" localizedIconTitle="" timeDropdown="" timeHourName="" classString="" hour1="" hour2="" timeMinutesName="" minutes="" isTwelveHour="" ampmName="" amSelected="" pmSelected="" compositeType="" formName=""/>
                </@field>
                <@field type="generic" label="${uiLabelMap.FacilitySpecialInstructions}">
                    <textarea cols="30" rows="3" name="${shipGroupIndex?default("0")}_shipping_instructions">${cart.getShippingInstructions(shipGroupIndex)!}</textarea>
                </@field>

            <#if cart.getOrderType() == 'PURCHASE_ORDER'>
                <input type="hidden" name="${shipGroupIndex?default('0')}_is_gift" value="false" />
            <#else>
              <#if (productStore.showCheckoutGiftOptions)?default('Y') != 'N'>
                <@field type="generic" label="${uiLabelMap.OrderIsThisGift}">
                    <@field type="radio" name="${shipGroupIndex?default('0')}_is_gift" value="true" checked=((cart.getIsGift(shipGroupIndex)!'Y') == 'Y') label="${uiLabelMap.CommonYes}"/>
                        <@field type="radio" name="${shipGroupIndex?default('0')}_is_gift" value="false" checked=((cart.getIsGift(shipGroupIndex)!'N') == 'N') label="${uiLabelMap.CommonNo}"/>
                </@field>
              </#if>
                <@field type="generic" label="${uiLabelMap.OrderGiftMessage}">
                    <textarea cols="30" rows="3" name="${shipGroupIndex?default('0')}_gift_message">${cart.getGiftMessage(shipGroupIndex)!}</textarea>
                </@field>
            </#if>

          </@cell>
      </@row>
   </@section>
</#list>
  </form>

<#else>
  <@commonMsg type="error">${uiLabelMap.OrderViewPermissionError}</@commonMsg>
</#if>
