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
<#include "component://shop/webapp/shop/order/ordercommon.ftl">

<#-- SCIPIO: DEPRECATED TEMPLATE -->

<@section><#-- title=uiLabelMap.OrderShippingInformation -->
  <form id="shipOptionsAndShippingInstructions" method="post" action="<@ofbizUrl>processShipOptions</@ofbizUrl>" name="${parameters.formNameValue}">
      <input type="hidden" name="finalizeMode" value="options"/>
      <@field type="generic" label=uiLabelMap.OrderSelectShippingMethod>
      <#list carrierShipmentMethodList as carrierShipmentMethod>
          <#assign shippingMethod = carrierShipmentMethod.shipmentMethodTypeId + "@" + carrierShipmentMethod.partyId>
          <#assign fieldLabel>
            <#if shoppingCart.getShippingContactMechId()??>
              <#assign shippingEst = shippingEstWpr.getShippingEstimate(carrierShipmentMethod)!(-1)>
            </#if>
            <#if carrierShipmentMethod.partyId != "_NA_">${carrierShipmentMethod.partyId!}&nbsp;</#if>${carrierShipmentMethod.description!}
              <#if shippingEst?has_content> - <#if (shippingEst > -1)><@ofbizCurrency amount=shippingEst isoCode=shoppingCart.getCurrency()/><#else>${uiLabelMap.OrderCalculatedOffline}</#if>
             </#if>
          </#assign>
          <@field type="radio" inlineItems=false id="shipping_method_${shippingMethod}" name="shipping_method" value=(shippingMethod) checked=(shippingMethod == (chosenShippingMethod!"N@A")) label=wrapAsRaw(fieldLabel, 'htmlmarkup')/>
      </#list>
      <#if !carrierShipmentMethodList?? || carrierShipmentMethodList?size == 0>
          <@field type="radio" name="shipping_method" value="Default" checked=true label="${rawLabel('OrderUseDefault')}."/>
      </#if>
      </@field>

        <@field type="generic" label="${rawLabel('OrderShipAllAtOnce')}?">
          <@field type="radio" inlineItems=false id="maySplit_N" checked=((shoppingCart.getMaySplit()!"N") == "N") name="may_split" value="false" label=uiLabelMap.OrderPleaseWaitUntilBeforeShipping />
          <@field type="radio" inlineItems=false id="maySplit_Y" checked=((shoppingCart.getMaySplit()!"N") == "Y") name="may_split" value="true" label=uiLabelMap.OrderPleaseShipItemsBecomeAvailable />
        </@field>

        <@field type="textarea" label=uiLabelMap.OrderSpecialInstructions cols="30" rows="3" name="shipping_instructions">${shoppingCart.getShippingInstructions()!}</@field>
        <@field type="input" name="correspondingPoId" value=(shoppingCart.getPoNumber()!) label=uiLabelMap.OrderPoNumber/>

    <#if (productStore.showCheckoutGiftOptions!) != "N">

      <@field type="generic" label=uiLabelMap.OrderIsThisGift>
        <@field type="radio" id="is_gift_Y" checked=((shoppingCart.getIsGift()!"Y") == "Y") name="is_gift" value="true" label=uiLabelMap.CommonYes />
        <@field type="radio" id="is_gift_N" checked=((shoppingCart.getIsGift()!"N") == "N") name="is_gift" value="false" label=uiLabelMap.CommonNo />
      </@field>
      <@field type="textarea" label=uiLabelMap.OrderGiftMessage name="gift_message">${shoppingCart.getGiftMessage()!}</@field>   
    </#if>

    <#--
      <@field type="submit" class="${styles.link_run_session!} ${styles.action_update!}" text=uiLabelMap.CommonContinue/>
    -->
  </form>
</@section>

<@checkoutActionsMenu directLinks=true />
