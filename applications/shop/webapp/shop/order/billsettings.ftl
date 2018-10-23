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

<@script>
function shipBillAddr() {
    <#if (requestParameters.singleUsePayment!"N") == "Y">
      <#assign singleUse = "&amp;singleUsePayment=Y">
    <#else>
      <#assign singleUse = "">
    </#if>
    if (document.billsetupform.useShipAddr.checked) {
        window.location.replace("setBilling?createNew=Y&amp;finalizeMode=payment&amp;useGc=${requestParameters.useGc!}&amp;paymentMethodType=${paymentMethodType!}&amp;useShipAddr=Y${singleUse}");
    } else {
        window.location.replace("setBilling?createNew=Y&amp;finalizeMode=payment&amp;useGc=${requestParameters.useGc!}&amp;paymentMethodType=${paymentMethodType!}${singleUse}");
    }
}
</@script>

<#macro menuContent menuArgs={}>
  <#if (requestParameters.singleUsePayment!"N") != "Y">
    <#-- SCIPIO: WARN: Freemarker does not support capturing output of screens.render currently, we must
        pass our args to the screen so it will do the @menu itself -->
    <@render resource=anonymoustrailScreen ctxVars={"anontrailMenuArgs":menuArgs}/>
  </#if>
</#macro>
<@section menuContent=menuContent><#-- title=uiLabelMap.AccountingPaymentInformation-->
  <@fields type="default" checkboxType="simple-standard">
    <#if (paymentMethodType?? && !requestParameters.resetType?has_content) || (finalizeMode!"") == "payment">
      <#-- after initial screen; show detailed screens for selected type -->
      <#if paymentMethodType == "CC">
        <#if creditCard?has_content && postalAddress?has_content>
          <form method="post" action="<@ofbizUrl>changeCreditCardAndBillingAddress</@ofbizUrl>" name="billsetupform">
            <input type="hidden" name="paymentMethodId" value="${creditCard.paymentMethodId!}" />
            <input type="hidden" name="contactMechId" value="${postalAddress.contactMechId!}" />
        <#elseif requestParameters.useShipAddr??>
          <form method="post" action="<@ofbizUrl>enterCreditCard</@ofbizUrl>" name="billsetupform">
        <#else>
          <form method="post" action="<@ofbizUrl>enterCreditCardAndBillingAddress</@ofbizUrl>" name="billsetupform">
        </#if>
      </#if>
      <#if paymentMethodType == "EFT">
        <#if eftAccount?has_content && postalAddress?has_content>
          <form method="post" action="<@ofbizUrl>changeEftAccountAndBillingAddress</@ofbizUrl>" name="billsetupform">
            <input type="hidden" name="paymentMethodId" value="${eftAccount.paymentMethodId!}" />
            <input type="hidden" name="contactMechId" value="${postalAddress.contactMechId!}" />
        <#elseif requestParameters.useShipAddr??>
          <form method="post" action="<@ofbizUrl>enterEftAccount</@ofbizUrl>" name="billsetupform">
        <#else>
          <form method="post" action="<@ofbizUrl>enterEftAccountAndBillingAddress</@ofbizUrl>" name="billsetupform">
        </#if>
      </#if>
      <#if paymentMethodType == "GC">
        <form method="post" action="<@ofbizUrl>finalizeOrder</@ofbizUrl>" name="billsetupform">
      </#if>

      <#if requestParameters.singleUsePayment?default("N") == "Y">
        <input type="hidden" name="singleUsePayment" value="Y" />
        <input type="hidden" name="appendPayment" value="Y" />
      </#if>

      <input type="hidden" name="contactMechTypeId" value="POSTAL_ADDRESS" />
      <input type="hidden" name="partyId" value="${partyId}" />
      <input type="hidden" name="paymentMethodType" value="${paymentMethodType}" />
      <input type="hidden" name="finalizeMode" value="payment" />
      <input type="hidden" name="createNew" value="Y" />
      <#if requestParameters.useShipAddr??>
        <input type="hidden" name="contactMechId" value="${postalFields.contactMechId}" />
      </#if>

        <#if cart.getShippingContactMechId()?? && paymentMethodType != "GC">
          <#assign labelContent>${uiLabelMap.FacilityBillingAddressSameShipping}</#assign>
          <@checkAddressInvField type="checkbox" checkboxType="simple" name="useShipAddr" value="Y" onClick="javascript:shipBillAddr();" checked=(requestParameters.useShipAddr??) labelContent=labelContent />

          <hr />
   
        </#if>

        <#if (paymentMethodType == "CC" || paymentMethodType == "EFT")>
          <@section title=uiLabelMap.PartyBillingAddress>
              <@render resource="component://shop/widget/OrderScreens.xml#genericaddress" />
          </@section>
        </#if>

        <#-- credit card fields -->
        <#if paymentMethodType == "CC">
          <#if !creditCard?has_content>
            <#assign creditCard = requestParameters>
          </#if>
          <#--<hr />-->
          <@section title=uiLabelMap.AccountingCreditCardInformation>
              <@render resource="component://accounting/widget/CommonScreens.xml#creditCardFields" />
          </@section>
        </#if>

        <#-- eft fields -->
        <#if paymentMethodType =="EFT">
          <#if !eftAccount?has_content>
            <#assign eftAccount = requestParameters>
          </#if>
          <#--<hr />-->
          
        <@section title=uiLabelMap.AccountingEFTAccountInformation>
          <@field type="input" label=uiLabelMap.AccountingNameOnAccount required=true size="30" maxlength="60" name="nameOnAccount" value=(eftAccount.nameOnAccount!) />
          <@field type="input" label=uiLabelMap.AccountingCompanyNameOnAccount size="30" maxlength="60" name="companyNameOnAccount" value=(eftAccount.companyNameOnAccount!) />
          <@field type="input" label=uiLabelMap.AccountingBankName required=true size="30" maxlength="60" name="bankName" value=(eftAccount.bankName!) />
          <@field type="input" label=uiLabelMap.AccountingRoutingNumber required=true size="10" maxlength="30" name="routingNumber" value=(eftAccount.routingNumber!) />
          <@field type="select" label=uiLabelMap.AccountingAccountType required=true name="accountType">
                <option>${eftAccount.accountType!}</option>
                <option></option>
                <option>Checking</option>
                <option>Savings</option>
          </@field>
          <@field type="input" label=uiLabelMap.AccountingAccountNumber required=true size="20" maxlength="40" name="accountNumber" value=(eftAccount.accountNumber!) />
          <@field type="input" label=uiLabelMap.CommonDescription size="30" maxlength="60" name="description" value=(eftAccount.description!) />
        </@section>
        </#if>

        <#-- gift card fields -->
        <#if (requestParameters.useGc!"") == "GC" || paymentMethodType == "GC">
          <#assign giftCard = requestParameters>
          <input type="hidden" name="addGiftCard" value="Y" />
          <#if paymentMethodType != "GC">
            <#--<hr />-->
          </#if>
        <@section title=uiLabelMap.AccountingGiftCardInformation>
          <@field type="input" label=uiLabelMap.AccountingGiftCardNumber required=true size="20" maxlength="60" name="giftCardNumber" value=(giftCard.cardNumber!) />
          <@field type="input" label=uiLabelMap.AccountingPINNumber required=true size="10" maxlength="60" name="giftCardPin" value=(giftCard.pinNumber!) />
          <@field type="input" label=uiLabelMap.CommonDescription size="30" maxlength="60" name="description" value=(giftCard.description!) />
          <#if paymentMethodType != "GC">
            <@field type="input" label=uiLabelMap.AccountingAmountToUse required=true size="5" maxlength="10" name="giftCardAmount" value=(giftCard.pinNumber!) />
          </#if>
        </@section>
        </#if>

        <#--
          <@field type="submit" class="${styles.link_run_session!} ${styles.action_continue!}" text=uiLabelMap.CommonContinue />
        -->
    <#else>
      <#-- initial screen show a list of options -->
      <form method="post" action="<@ofbizUrl>finalizeOrder</@ofbizUrl>" name="billsetupform">
        <input type="hidden" name="finalizeMode" value="payment" />
        <input type="hidden" name="createNew" value="Y" />
        <#if productStorePaymentMethodTypeIdMap.GIFT_CARD??>
          <#assign labelContent>${uiLabelMap.AccountingCheckGiftCard}</#assign>
          <@checkAddressInvField type="checkbox" name="useGc" value="GC" checked=(paymentMethodType?? && paymentMethodType == "GC") labelContent=labelContent />

          <#--<hr />-->
        </#if>
        <#if productStorePaymentMethodTypeIdMap.EXT_OFFLINE??>
          <#assign labelContent>${uiLabelMap.OrderPaymentOfflineCheckMoney}</#assign>
          <@checkAddressInvField type="radio" name="paymentMethodType" value="offline" checked=(paymentMethodType?? && paymentMethodType == "offline") labelContent=labelContent />

          <#--<hr />-->
        </#if>
        <#if productStorePaymentMethodTypeIdMap.CREDIT_CARD??>
          <#assign labelContent>${uiLabelMap.AccountingVisaMastercardAmexDiscover}</#assign>
          <@checkAddressInvField type="radio" name="paymentMethodType" value="CC" checked=(paymentMethodType?? && paymentMethodType == "CC") labelContent=labelContent />

          <#--<hr />-->
        </#if>
        <#if productStorePaymentMethodTypeIdMap.EFT_ACCOUNT??>
          <#assign labelContent>${uiLabelMap.AccountingAHCElectronicCheck}</#assign>
          <@checkAddressInvField type="radio" name="paymentMethodType" value="EFT" checked=(paymentMethodType?? && paymentMethodType == "EFT") labelContent=labelContent />

        </#if>

        <#--
          <@field type="submit" class="${styles.link_run_session!} ${styles.action_continue!}" text=uiLabelMap.CommonContinue />
        -->
      </form>
    </#if>
  </@fields>
</@section>

<@checkoutActionsMenu directLinks=true formName="billsetupform" />

