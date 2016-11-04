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

  <#-- SCIPIO: Warning to prevent confusion... -->
  <#if cart.getOrderType() != "PURCHASE_ORDER" && (!carrierShipmentMethodList?? || carrierShipmentMethodList?size == 0)>
    <@alert type="warning">${uiLabelMap.CommonWarning}: ${uiLabelMap.OrderNoShipMethodAvailable} ${uiLabelMap.OrderMayNotProceedWithOrder}</@alert>
  </#if>

  <form method="post" action="<@ofbizUrl>finalizeOrder</@ofbizUrl>" name="checkoutsetupform">
      <input type="hidden" name="finalizeMode" value="options"/>
              
<#list 1..cart.getShipGroupSize() as currIndex>
<#assign shipGroupIndex = currIndex - 1>

<#if cart.getShipmentMethodTypeId(shipGroupIndex)?? && cart.getCarrierPartyId(shipGroupIndex)??>
    <#assign chosenShippingMethod = cart.getShipmentMethodTypeId(shipGroupIndex) + '@' + cart.getCarrierPartyId(shipGroupIndex)>
</#if>
<#assign supplierPartyId = cart.getSupplierPartyId(shipGroupIndex)!>
<#assign supplier =  delegator.findOne("PartyGroup", {"partyId":supplierPartyId}, false)! />
  <#assign sectionTitle>${rawLabel('OrderShipGroup')} ${rawLabel('CommonNbr')} ${currIndex}<#if supplier?has_content> - ${rawString(supplier.groupName!(supplier.partyId))}</#if></#assign>
  <@section title=sectionTitle>   
    <@row>
        <@cell columns=columns>
            <#if cart.getOrderType() != "PURCHASE_ORDER">
                <@field type="generic" label=uiLabelMap.ProductShipmentMethod>
                <#assign shipEstimateWrapper = Static["org.ofbiz.order.shoppingcart.shipping.ShippingEstimateWrapper"].getWrapper(dispatcher, cart, 0)>
                <#assign carrierShipmentMethods = shipEstimateWrapper.getShippingMethods()>
                <#list carrierShipmentMethods as carrierShipmentMethod>
                    <#assign shippingMethod = carrierShipmentMethod.shipmentMethodTypeId + "@" + carrierShipmentMethod.partyId>
                    <#assign radioText>
                      <#if carrierShipmentMethod.partyId != "_NA_">${carrierShipmentMethod.partyId!}&nbsp;</#if>${carrierShipmentMethod.description!}
                      <#if cart.getShippingContactMechId(shipGroupIndex)??>
                        <#assign shippingEst = shipEstimateWrapper.getShippingEstimate(carrierShipmentMethod)!(-1)>
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
                    <@field type="radio" inlineItems=false name="${shipGroupIndex!0}_shipping_method" value=shippingMethod id="${shipGroupIndex!0}_shipping_method_${shippingMethod}" label=wrapAsRaw(radioText, 'htmlmarkup') checked=(shippingMethod == (chosenShippingMethod!"N@A"))/>                   
                </#list>
                <#if !carrierShipmentMethodList?? || carrierShipmentMethodList?size == 0>
                    <@field type="radio" inlineItems=false name="${shipGroupIndex!0}_shipping_method" value="Default" checked=true label=uiLabelMap.FacilityNoOtherShippingMethods/>
                </#if>
                </@field>  
            <#else>
                <input type="hidden" name="${shipGroupIndex!'0'}_shipping_method" value="STANDARD@_NA_" />
                <@field type="input" label=uiLabelMap.OrderOrderShipEstimate name="${shipGroupIndex!'0'}_ship_estimate" value=(cart.getItemShipGroupEstimate(shipGroupIndex!'0')!) />
            </#if>

                <@field type="generic" label=uiLabelMap.FacilityShipOnceOrAvailable>
                    <@field type="radio" inlineItems=false name="${shipGroupIndex!0}_may_split" value="false" checked=((cart.getMaySplit(shipGroupIndex)!"N") == "N") label=uiLabelMap.FacilityWaitEntireOrderReady/>
                    <@field type="radio" inlineItems=false name="${shipGroupIndex!0}_may_split" value="true" checked=((cart.getMaySplit(shipGroupIndex)!"N") == "Y") label=uiLabelMap.FacilityShipAvailable/>
                </@field>
                <@field type="datetime" label=uiLabelMap.OrderShipBeforeDate name="sgi${shipGroupIndex!'0'}_shipBeforeDate" value=((cart.getShipBeforeDate(shipGroupIndex))!) size="25" maxlength="30" id="sgi${shipGroupIndex!'0'}_shipBeforeDate"/>
                <@field type="datetime" label=uiLabelMap.OrderShipAfterDate name="sgi${shipGroupIndex!'0'}_shipAfterDate" value=((cart.getShipAfterDate(shipGroupIndex))!) size="25" maxlength="30" id="sgi${shipGroupIndex!'0'}_shipAfterDate"/>
                <@field type="textarea" label=uiLabelMap.FacilitySpecialInstructions cols="30" rows="3" name="${shipGroupIndex!'0'}_shipping_instructions">${cart.getShippingInstructions(shipGroupIndex)!}</@field>

            <#if cart.getOrderType() == 'PURCHASE_ORDER'>
                <input type="hidden" name="${shipGroupIndex!'0'}_is_gift" value="false" />
            <#else>
              <#if ((productStore.showCheckoutGiftOptions)!'Y') != 'N'>
                <@field type="generic" label=uiLabelMap.OrderIsThisGift>
                    <@field type="radio" name="${shipGroupIndex!'0'}_is_gift" value="true" checked=((cart.getIsGift(shipGroupIndex)!'Y') == 'Y') label=uiLabelMap.CommonYes/>
                    <@field type="radio" name="${shipGroupIndex!'0'}_is_gift" value="false" checked=((cart.getIsGift(shipGroupIndex)!'N') == 'N') label=uiLabelMap.CommonNo/>
                </@field>
              </#if>
                <@field type="textarea" label=uiLabelMap.OrderGiftMessage cols="30" rows="3" name="${shipGroupIndex!'0'}_gift_message">${cart.getGiftMessage(shipGroupIndex)!}</@field>
            </#if>

          </@cell>
      </@row>
   </@section>
</#list>

  <@section title=getLabel("AcctgTransType.description.INTERNAL_ACCTG_TRANS", "AccountingEntityLabels")>
    <@row>
      <@cell columns=columns>   
        <@field type="textarea" label=uiLabelMap.OrderInternalNote name="internal_order_notes" cols="30" rows="3"><#rt>
          <#if (cart.getInternalOrderNotes().size()>0)>${(cart.getInternalOrderNotes()[0])!}</#if><#t>
        </@field><#lt>
        <@field type="textarea" label=uiLabelMap.OrderShippingNotes name="shippingNotes" cols="30" rows="3"><#rt>
          <#if (cart.getOrderNotes().size()>0)>${(cart.getOrderNotes()[0])!}</#if><#t>
        </@field><#lt>
      </@cell>
    </@row>
  </@section>

  </form>

<#else>
  <@commonMsg type="error">${uiLabelMap.OrderViewPermissionError}</@commonMsg>
</#if>
