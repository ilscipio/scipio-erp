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

<@section title="2)&nbsp;${uiLabelMap.OrderHowShallWeShipIt}?">
    <form method="post" name="checkoutInfoForm">
        <fieldset>
            <input type="hidden" name="checkoutpage" value="shippingoptions"/>

              <#list carrierShipmentMethodList as carrierShipmentMethod>
                <#assign shippingMethod = carrierShipmentMethod.shipmentMethodTypeId + "@" + carrierShipmentMethod.partyId>
                <#assign labelContent>
                      <#if shoppingCart.getShippingContactMechId()??>
                        <#assign shippingEst = shippingEstWpr.getShippingEstimate(carrierShipmentMethod)?default(-1)>
                      </#if>
                      <#if carrierShipmentMethod.partyId != "_NA_">${carrierShipmentMethod.partyId!}&nbsp;</#if>${carrierShipmentMethod.description!}
                      <#if shippingEst?has_content> - <#if (shippingEst > -1)><@ofbizCurrency amount=shippingEst isoCode=shoppingCart.getCurrency()/><#else>${uiLabelMap.OrderCalculatedOffline}</#if></#if></#assign>
                <@invertedField type="generic" labelContent=labelContent>
                    <@field type="radio" inline=true name="shipping_method" value=(shippingMethod!"") checked=(shippingMethod == rawString(chosenShippingMethod!"N@A")) />
                </@invertedField>
              </#list>
              <#if !carrierShipmentMethodList?? || carrierShipmentMethodList?size == 0>
                <#assign labelContent>${uiLabelMap.OrderUseDefault}.</#assign>
                <@invertedField type="generic" labelContent=labelContent>
                    <@field type="radio" inline=true name="shipping_method" value="Default" checked=true />
                </@invertedField>
              </#if>

              <hr />
              
            <@field type="generic" label="${uiLabelMap.OrderShipAllAtOnce}?">
              <@fields inlineItems=false>
              <@field type="radio" checked=("Y" != (shoppingCart.getMaySplit()!"N")) name="may_split" value="false" label="${uiLabelMap.OrderPleaseWaitUntilBeforeShipping}."/>
              <@field type="radio" checked=("Y" == (shoppingCart.getMaySplit()!"N")) name="may_split" value="true" label="${uiLabelMap.OrderPleaseShipItemsBecomeAvailable}."/>
              </@fields>
            </@field>
              <#--<hr />-->

              <@field type="textarea" title=uiLabelMap.OrderSpecialInstructions cols="30" rows="3" wrap="hard" name="shipping_instructions" label=uiLabelMap.OrderSpecialInstructions>${shoppingCart.getShippingInstructions()!}</@field>
       
              <#--<hr />-->

              <#if shoppingCart.getPoNumber()?? && shoppingCart.getPoNumber() != "(none)">
                <#assign currentPoNumber = shoppingCart.getPoNumber()>
              </#if>
              <@field type="input" label=uiLabelMap.OrderPoNumber name="correspondingPoId" size="15" value=(currentPoNumber!)/>
     
            <#if (productStore.showCheckoutGiftOptions!) != "N">
              <#--<hr />-->
              <@field type="generic" label=uiLabelMap.OrderIsThisGift>
                    <@field type="radio" checked=("Y" == (shoppingCart.getIsGift()!"N")) name="is_gift" value="true" label=uiLabelMap.CommonYes />
                    <@field type="radio" checked=("Y" != (shoppingCart.getIsGift()!"N")) name="is_gift" value="false" label=uiLabelMap.CommonNo />
              </@field>
              <#--<hr />-->

              <@field type="textarea" label=uiLabelMap.OrderGiftMessage cols="30" rows="3" wrap="hard" name="gift_message">${shoppingCart.getGiftMessage()!}</@field>
     
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
                  <div>${uiLabelMap.OrderUpdateEmailAddress} <a href="<@ofbizUrl>viewprofile?DONE_PAGE=checkoutoptions</@ofbizUrl>" class="${styles.link_nav_info!} ${styles.action_view!}">${uiLabelMap.PartyProfile}</a>.</div>
                  <br />
                  <div>${uiLabelMap.OrderCommaSeperatedEmailAddresses}:</div>
                  <@field type="input" widgetOnly=true size="30" name="order_additional_emails" value=(shoppingCart.getOrderAdditionalEmails()!)/>
              </@field>
        </fieldset>
    </form>
</@section>

<@row>
  <@cell columns=6>
    <@menu type="button">
      <@menuitem type="link" href="javascript:submitForm(document.checkoutInfoForm, 'CS', '');" class="+${styles.action_nav!} ${styles.action_cancel!}" text=uiLabelMap.OrderBacktoShoppingCart />
    </@menu>
  </@cell>
  <@cell columns=6 class="+${styles.text_right!}">
    <@menu type="button">
      <@menuitem type="link" href="javascript:submitForm(document.checkoutInfoForm, 'DN', '');" class="+${styles.action_run_session!} ${styles.action_continue!}" text=uiLabelMap.CommonNext />
    </@menu>
  </@cell>
</@row>

