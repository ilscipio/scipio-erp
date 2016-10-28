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

<#-- SCIPIO: DEPRECATED TEMPLATE -->

<#if requestParameters.paymentMethodTypeId?has_content>
   <#assign paymentMethodTypeId = "${requestParameters.paymentMethodTypeId!}">
</#if>
<@script>
function shipBillAddr() {
    <#-- SCIPIO: NOTE: delim here is &, NOT &amp; -->
    <#if (requestParameters.singleUsePayment!"N") == "Y">
      <#assign singleUse = "&singleUsePayment=Y">
    <#else>
      <#assign singleUse = "">
    </#if>
    if (document.billsetupform.useShipAddr.checked) {
        window.location.replace("setPaymentInformation?createNew=Y&addGiftCard=${requestParameters.addGiftCard!?js_string}&paymentMethodTypeId=${paymentMethodTypeId!?js_string}&useShipAddr=Y${singleUse}");
    } else {
        window.location.replace("setPaymentInformation?createNew=Y&addGiftCard=${requestParameters.addGiftCard!?js_string}&paymentMethodTypeId=${paymentMethodTypeId!?js_string}${singleUse}");
    }
}
</@script>

<@section><#--  title=uiLabelMap.AccountingPaymentInformation -->
  <@fields fieldArgs={"checkboxType":"simple-standard"}><#-- FIXME: proper type="..." -->
      <#-- after initial screen; show detailed screens for selected type -->
  <#if (paymentMethodTypeId!) == "CREDIT_CARD">
    <#if creditCard?has_content && postalAddress?has_content && !requestParameters.useShipAddr??>
      <form method="post" action="<@ofbizUrl>changeCreditCardAndBillingAddress</@ofbizUrl>" name="${parameters.formNameValue}">
        <input type="hidden" name="paymentMethodId" value="${creditCard.paymentMethodId!}"/>
        <input type="hidden" name="contactMechId" value="${postalAddress.contactMechId!}"/>
    <#elseif requestParameters.useShipAddr??>
      <form method="post" action="<@ofbizUrl>enterCreditCard</@ofbizUrl>" name="${parameters.formNameValue}">
    <#else>
      <form method="post" action="<@ofbizUrl>enterCreditCardAndBillingAddress</@ofbizUrl>" name="${parameters.formNameValue}">
    </#if>
  <#elseif (paymentMethodTypeId!) == "EFT_ACCOUNT">
    <#if eftAccount?has_content && postalAddress?has_content>
      <form method="post" action="<@ofbizUrl>changeEftAccountAndBillingAddress</@ofbizUrl>" name="${parameters.formNameValue}">
        <input type="hidden" name="paymentMethodId" value="${eftAccount.paymentMethodId!}"/>
        <input type="hidden" name="contactMechId" value="${postalAddress.contactMechId!}"/>
    <#elseif requestParameters.useShipAddr??>
      <form method="post" action="<@ofbizUrl>enterEftAccount</@ofbizUrl>" name="${parameters.formNameValue}">
    <#else>
      <form method="post" action="<@ofbizUrl>enterEftAccountAndBillingAddress</@ofbizUrl>" name="${parameters.formNameValue}">
    </#if>
  <#elseif (paymentMethodTypeId!) == "GIFT_CARD"> <#--Don't know much how this is handled -->
    <form method="post" action="<@ofbizUrl>enterGiftCard</@ofbizUrl>" name="${parameters.formNameValue}">
  <#elseif (paymentMethodTypeId!) == "EXT_OFFLINE">
    <form method="post" action="<@ofbizUrl>processPaymentSettings</@ofbizUrl>" name="${parameters.formNameValue}">
  <#else>
    <@commonMsg type="error">${uiLabelMap.AccountingPaymentMethodTypeNotHandled} ${paymentMethodTypeId!uiLabelMap.CommonNA}</@commonMsg>
    <form method="post" action="#" name="${parameters.formNameValue!}">
  </#if>

      <#if (requestParameters.singleUsePayment!"N") == "Y">
        <input type="hidden" name="singleUsePayment" value="Y"/>
        <input type="hidden" name="appendPayment" value="Y"/>
      </#if>
      <input type="hidden" name="contactMechTypeId" value="POSTAL_ADDRESS"/>
      <input type="hidden" name="partyId" value="${partyId}"/>
      <input type="hidden" name="paymentMethodTypeId" value="${paymentMethodTypeId!}"/>
      <input type="hidden" name="createNew" value="Y"/>
      <#if requestParameters.useShipAddr??>
        <input type="hidden" name="contactMechId" value="${parameters.contactMechId!}"/>
      </#if>

        <#if cart.getShippingContactMechId()?? && (paymentMethodTypeId!) != "GIFT_CARD">
          <#assign labelContent>${uiLabelMap.FacilityBillingAddressSameShipping}</#assign>
          <@commonInvField type="checkbox" name="useShipAddr" value="Y" onClick="javascript:shipBillAddr();" checked=(useShipAddr??) labelContent=labelContent />
          <#--<hr />-->
        </#if>

        <#if ((paymentMethodTypeId!) == "CREDIT_CARD" || (paymentMethodTypeId!) == "EFT_ACCOUNT")>
          <@section title=uiLabelMap.PartyBillingAddress>
              <@render resource="component://shop/widget/OrderScreens.xml#genericaddress" />
          </@section>
        </#if>

        <#-- credit card fields -->
        <#if (paymentMethodTypeId!) == "CREDIT_CARD">
          <#if !creditCard?has_content>
            <#assign creditCard = requestParameters>
          </#if>
          <#--<hr />-->
          <@section title=uiLabelMap.AccountingCreditCardInformation>
              <@render resource="component://accounting/widget/CommonScreens.xml#creditCardFields" />
          </@section>
        </#if>

        <#-- eft fields -->
        <#if (paymentMethodTypeId!) =="EFT_ACCOUNT">
          <#if !eftAccount?has_content>
            <#assign eftAccount = requestParameters>
          </#if>
          <#--<hr />-->
        <@section title=uiLabelMap.AccountingEFTAccountInformation>
          <@field type="input" label=uiLabelMap.AccountingNameOnAccount required=true size="30" maxlength="60" name="nameOnAccount" value=(eftAccount.nameOnAccount!)/>
          <@field type="input" label=uiLabelMap.AccountingCompanyNameOnAccount size="30" maxlength="60" name="companyNameOnAccount" value=(eftAccount.companyNameOnAccount!)/>
          <@field type="input" label=uiLabelMap.AccountingBankName required=true size="30" maxlength="60" name="bankName" value=(eftAccount.bankName!)/>
          <@field type="input" label=uiLabelMap.AccountingRoutingNumber required=true size="10" maxlength="30" name="routingNumber" value=(eftAccount.routingNumber!)/>
          <@field type="select" label=uiLabelMap.AccountingAccountType required=true name="accountType">
                <option>${eftAccount.accountType!}</option>
                <option></option>
                <option>Checking</option>
                <option>Savings</option>
          </@field>
          <@field type="input" label=uiLabelMap.AccountingAccountNumber required=true size="20" maxlength="40" name="accountNumber" value=(eftAccount.accountNumber!)/>
          <@field type="input" label=uiLabelMap.CommonDescription size="30" maxlength="60" name="description" value=(eftAccount.description!)/>
        </@section>
        </#if>

        <#-- gift card fields -->
        <#if (requestParameters.addGiftCard!"") == "Y" || (paymentMethodTypeId!) == "GIFT_CARD">
          <input type="hidden" name="addGiftCard" value="Y"/>
          <#assign giftCard = giftCard!>
          <#if (paymentMethodTypeId!) != "GIFT_CARD">
            <#--<hr />-->
          </#if>
        <@section title=uiLabelMap.AccountingGiftCardInformation>
          <@field type="input" label=uiLabelMap.AccountingGiftCardNumber required=true size="20" maxlength="60" name="giftCardNumber" value=(giftCard.cardNumber!)/>
          <@field type="input" label=uiLabelMap.AccountingPINNumber required=true size="10" maxlength="60" name="giftCardPin" value=(giftCard.pinNumber!)/>
          <@field type="input" label=uiLabelMap.CommonDescription size="30" maxlength="60" name="description" value=(giftCard.description!)/>
          <#if (paymentMethodTypeId!) != "GIFT_CARD">
            <@field type="input" label=uiLabelMap.AccountingAmountToUse required=true size="5" maxlength="10" name="giftCardAmount" value=(giftCard.pinNumber!)/>
          </#if>
        </@section>
        </#if>
      <#--
        <@field type="submit" class="${styles.link_run_session!} ${styles.action_update!}" text=uiLabelMap.CommonContinue/>
      -->
    </form>
  </@fields>
</@section>

<@checkoutActionsMenu text=OrderContinueToFinalOrderReview directLinks=true />
