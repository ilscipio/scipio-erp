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
<#include "ordercommon.ftl">

<#-- Scipio: DEPRECATED TEMPLATE -->

<@script>

jQuery(document).ready(init);

function init() {
    var optForm = document.quickAnonOptSetupForm;
    document.getElementById("noShippingMethodSelectedError").innerHTML = "";
}

function aroundOptSubmitOrder(invocation) {
    var formToSubmit = document.quickAnonOptSetupForm;
    var shipMethodOption = "none";
    for (var i=0; i < formToSubmit.shipping_method.length; i++) {
        if (formToSubmit.shipping_method[i].checked){
            shipMethodOption = formToSubmit.shipping_method[i].value;
        }
    }
    if (shipMethodOption != "none") {
        jQuery.ajax({
            url: formToSubmit.action,
            type: "POST",
            data: jQuery("#quickAnonOptSetupForm").serialize(),
            success: function(data) {
               document.getElementById("optInfoSection").innerHTML = data;
            }
        });
    } else {
        document.getElementById("noShippingMethodSelectedError").innerHTML = "${uiLabelMap.EcommerceMessagePleaseSelectShippingMethod}";
    }
}

function eventTrigger (e) {
    if (! e)
        e = event;
    return e.target || e.srcElement;
}

function onClickShippingMethod(e) {
    var obj = eventTrigger (e);
    shippingMethodChanged(obj.value);
    return true;
}

</@script>

<@section id="optInfoSection">
  <form id="quickAnonOptSetupForm" method="post" action="<@ofbizUrl>quickAnonProcessShipOptions</@ofbizUrl>" name="quickAnonOptSetupForm">

  <@row>
    <@cell columns=6>
      <div class="errorMessage" id="noShippingMethodSelectedError"></div>
      <@field type="generic" label=uiLabelMap.OrderMethod>
        <#list carrierShipmentMethodList as carrierShipmentMethod>
           <#if shoppingCart.getShippingContactMechId()??>
               <#assign shippingEst = shippingEstWpr.getShippingEstimate(carrierShipmentMethod)?default(-1)>
           </#if>
           <#assign fieldLabel><#if carrierShipmentMethod.partyId != "_NA_">${carrierShipmentMethod.partyId!}&nbsp;</#if>${carrierShipmentMethod.description!}<#if shippingEst?has_content> - <#if (shippingEst > -1)><@ofbizCurrency amount=shippingEst isoCode=shoppingCart.getCurrency()/><#else>${uiLabelMap.OrderCalculatedOffline}</#if></#if></#assign>
           
           <#assign shippingMethod = carrierShipmentMethod.shipmentMethodTypeId + "@" + carrierShipmentMethod.partyId>
           <@field type="radio" onClick="return onClickShippingMethod(event)" name="shipping_method" value=(shippingMethod) checked=(shippingMethod == (chosenShippingMethod!"N@A")) label=wrapAsRaw(fieldLabel, 'html')/>
        </#list>
        <#if !carrierShipmentMethodList?? || carrierShipmentMethodList?size == 0>
          <@field type="radio" onClick="return onClickShippingMethod(event)" name="shipping_method" value="Default" checked=true label=uiLabelMap.OrderUseDefault />
        </#if>
      </@field>
    </@cell>
    <@cell columns=6>
        <@field type="textarea" label=uiLabelMap.OrderSpecialInstructions cols="30" rows="3" name="shipping_instructions">${shoppingCart.getShippingInstructions()!}</@field>
        <#--<hr />-->
        <@field type="input" label=uiLabelMap.OrderPoNumber name="correspondingPoId" size="15" value=(shoppingCart.getPoNumber()!)/>

      <#if (productStore.showCheckoutGiftOptions!) != "N">
        <#--<hr />-->
        <@field type="generic" label=uiLabelMap.OrderIsThisGift>
          <@field type="radio" checked=((shoppingCart.getIsGift()!"Y") == "Y") name="is_gift" value="true" label=uiLabelMap.CommonYes />
          <@field type="radio" checked=((shoppingCart.getIsGift()!"N") == "N") name="is_gift" value="false" label=uiLabelMap.CommonNo />
        </@field>
        <#--<hr />-->
        <@field type="textarea" label=uiLabelMap.OrderGiftMessage cols="30" rows="3" name="gift_message">${shoppingCart.getGiftMessage()!}</@field>
      </#if>
    </@cell>
  </@row>

  <#--<hr />-->

  <@row>
    <@cell columns=6>
      <@field type="generic" label="${rawString(uiLabelMap.OrderShipAllAtOnce)}?">
        <@field type="radio" checked=((shoppingCart.getMaySplit()!"N") == "N") name="may_split" value="false" label=uiLabelMap.OrderPleaseWaitUntilBeforeShipping />
        <@field type="radio" checked=((shoppingCart.getMaySplit()!"N") == "Y") name="may_split" value="true" label=uiLabelMap.OrderPleaseShipItemsBecomeAvailable />
      </@field>
    </@cell>
  </@row>

  </form>
</@section> 
