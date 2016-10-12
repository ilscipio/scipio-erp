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

<@script>
function submitForm(form, mode, value) {
    if (mode == "DN") {
        // done action; checkout
        form.action="<@ofbizUrl>checkoutoptions</@ofbizUrl>";
        form.submit();
    } else if (mode == "CS") {
        // continue shopping
        form.action="<@ofbizUrl>updateCheckoutOptions/showcart</@ofbizUrl>";
        form.submit();
    } else if (mode == "NA") {
        // new address
        form.action="<@ofbizUrl>updateCheckoutOptions/editcontactmech?preContactMechTypeId=POSTAL_ADDRESS&contactMechPurposeTypeId=SHIPPING_LOCATION&DONE_PAGE=checkoutoptions</@ofbizUrl>";
        form.submit();
    } else if (mode == "EA") {
        // edit address
        form.action="<@ofbizUrl>updateCheckoutOptions/editcontactmech?DONE_PAGE=checkoutshippingaddress&contactMechId="+value+"</@ofbizUrl>";
        form.submit();
    } else if (mode == "NC") {
        // new credit card
        form.action="<@ofbizUrl>updateCheckoutOptions/editcreditcard?DONE_PAGE=checkoutoptions</@ofbizUrl>";
        form.submit();
    } else if (mode == "EC") {
        // edit credit card
        form.action="<@ofbizUrl>updateCheckoutOptions/editcreditcard?DONE_PAGE=checkoutoptions&paymentMethodId="+value+"</@ofbizUrl>";
        form.submit();
    } else if (mode == "NE") {
        // new eft account
        form.action="<@ofbizUrl>updateCheckoutOptions/editeftaccount?DONE_PAGE=checkoutoptions</@ofbizUrl>";
        form.submit();
    } else if (mode == "EE") {
        // edit eft account
        form.action="<@ofbizUrl>updateCheckoutOptions/editeftaccount?DONE_PAGE=checkoutoptions&paymentMethodId="+value+"</@ofbizUrl>";
        form.submit();
    }
}

</@script>

<@section title="${rawString(uiLabelMap.OrderHowShallWeShipIt)}?"><#-- Scipio: No numbers for multi-page checkouts, make checkout too rigid: 2) ${uiLabelMap.OrderHowShallWeShipIt}? -->
    <form method="post" name="checkoutInfoForm" id="checkoutInfoForm">
        <#--<fieldset>-->
            <input type="hidden" name="checkoutpage" value="shippingoptions"/>

            <#-- Scipio: switched from top-level inverted fields to generic with label because otherwise too inconsistent with
                everything else on this form and with some other pages -->
            <#assign selectedShippingMethod = rawString(parameters.shipping_method!chosenShippingMethod!"N@A")>
            <#-- FIXME?: wrapAsRaw is not ideal -->
            <@field type="generic" label=wrapAsRaw("<strong>${uiLabelMap.OrderShippingMethod}</strong>", 'html') required=true>
            <@fields inlineItems=false>
              <#list carrierShipmentMethodList as carrierShipmentMethod>
                <#-- Scipio: For shop, will not show ship methods whose shipping estimates returned an error.
                    Selecting them here causes the next events to fail (offline calc for these was not supported in ecommerce). 
                    Some stores may want to let customers place
                    orders with offline calculation, but we know the failure is very likely to be misconfiguration
                    or connectivity failure, and by default we can't assume the store is equipped to handle offlines in these cases.
                    NOTE: Failure is subtly noted by the absence of ship estimate (null). -->
                <#if shippingEstWpr.getShippingEstimate(carrierShipmentMethod)??>
                  <#assign shippingMethod = carrierShipmentMethod.shipmentMethodTypeId + "@" + carrierShipmentMethod.partyId>
                  <#assign labelContent>
                    <#if shoppingCart.getShippingContactMechId()??>
                      <#assign shippingEst = shippingEstWpr.getShippingEstimate(carrierShipmentMethod)?default(-1)>
                    </#if>
                    <#if carrierShipmentMethod.partyId != "_NA_">${carrierShipmentMethod.partyId!}&nbsp;</#if>${carrierShipmentMethod.description!}
                    <#if shippingEst?has_content> - <#if (shippingEst > -1)><@ofbizCurrency amount=shippingEst isoCode=shoppingCart.getCurrency()/><#else>${uiLabelMap.OrderCalculatedOffline}</#if></#if>
                  </#assign>
                  <#--<@commonInvField type="generic" labelContent=labelContent>-->
                  <#-- FIXME?: wrapAsRaw is not ideal -->
                  <@field type="radio" name="shipping_method" value=(shippingMethod!"") checked=(shippingMethod == selectedShippingMethod) label=wrapAsRaw(labelContent, 'html') /><#--inline=true -->
                  <#--</@commonInvField>-->
                </#if>
              </#list>
              <#if !carrierShipmentMethodList?? || carrierShipmentMethodList?size == 0>
                <#assign labelContent>${uiLabelMap.OrderUseDefault}.</#assign>
                <#--<@commonInvField type="generic" labelContent=labelContent>-->
                <#-- FIXME?: wrapAsRaw is not ideal -->
                <@field type="radio" name="shipping_method" value="Default" checked=true label=wrapAsRaw(labelContent, 'html')/><#--inline=true -->
                <#--</@commonInvField>-->
              </#if>
            </@fields>
            </@field>

            <br/>
            <#--<hr />-->
              
            <@field type="generic" label="${rawString(uiLabelMap.OrderShipAllAtOnce)}?">
              <@fields inlineItems=false>
              <@field type="radio" checked=("Y" != (parameters.may_split!shoppingCart.getMaySplit()!"N")) name="may_split" value="false" label="${rawString(uiLabelMap.OrderPleaseWaitUntilBeforeShipping)}."/>
              <@field type="radio" checked=("Y" == (parameters.may_split!shoppingCart.getMaySplit()!"N")) name="may_split" value="true" label="${rawString(uiLabelMap.OrderPleaseShipItemsBecomeAvailable)}."/>
              </@fields>
            </@field>
              <#--<hr />-->
    <#-- limit len -->
    <@fields type="inherit-all" fieldArgs={"gridArgs":{"totalLarge":8}}>
              <@field type="textarea" title=uiLabelMap.OrderSpecialInstructions cols="30" rows="3" wrap="hard" name="shipping_instructions" label=uiLabelMap.OrderSpecialInstructions>${parameters.shipping_instructions!shoppingCart.getShippingInstructions()!}</@field>
       
              <#--<hr />-->

              <#if shoppingCart.getPoNumber()?? && shoppingCart.getPoNumber() != "(none)">
                <#assign currentPoNumber = shoppingCart.getPoNumber()>
              </#if>
              <@field type="input" label=uiLabelMap.OrderPoNumber name="correspondingPoId" size="15" value=(parameters.correspondingPoId!currentPoNumber!)/>
     
            <#if (productStore.showCheckoutGiftOptions!) != "N">
              <#--<hr />-->
              <@field type="generic" label=uiLabelMap.OrderIsThisGift>
                    <@field type="radio" checked=("Y" == (parameters.is_gift!shoppingCart.getIsGift()!"N")) name="is_gift" value="true" label=uiLabelMap.CommonYes />
                    <@field type="radio" checked=("Y" != (parameters.is_gift!shoppingCart.getIsGift()!"N")) name="is_gift" value="false" label=uiLabelMap.CommonNo />
              </@field>
              <#--<hr />-->

              <@field type="textarea" label=uiLabelMap.OrderGiftMessage cols="30" rows="3" wrap="hard" name="gift_message">${parameters.gift_message!shoppingCart.getGiftMessage()!}</@field>
     
            <#else>
              <input type="hidden" name="is_gift" value="false"/>
            </#if>
              <#--<hr />-->
              <@field type="generic" label=uiLabelMap.PartyEmailAddresses>
                  <div>${uiLabelMap.OrderEmailSentToFollowingAddresses}:</div>
                  <div>
                    <b>
                      <#list emailList as email>
                        ${email.infoString!}<#if email_has_next>,</#if>
                      </#list>
                    </b>
                  </div>
                  <div>${uiLabelMap.OrderUpdateEmailAddress} <a href="<@ofbizUrl>viewprofile?DONE_PAGE=checkoutoptions</@ofbizUrl>" class="${styles.link_nav_info!} ${styles.action_view!}" target="_BLANK">${uiLabelMap.PartyProfile}</a>.</div>
                  <br />
                  <div>${uiLabelMap.OrderCommaSeperatedEmailAddresses}:</div>
                  <@field type="input" widgetOnly=true size="30" name="order_additional_emails" value=(parameters.order_additional_emails!shoppingCart.getOrderAdditionalEmails()!)/>
              </@field>
        <#--</fieldset>-->
    </@fields>

    </form>
</@section>

<@checkoutActionsMenu directLinks=false formName="checkoutInfoForm" />


