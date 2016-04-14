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
    <@menu args=menuArgs>
      <@render resource=anonymoustrailScreen />
    </@menu>
  </#if>
</#macro>
<@section title=uiLabelMap.AccountingPaymentInformation menuContent=menuContent>
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
          <#assign postfixContent></#assign>
          <@invertedField type="generic" labelContent=labelContent postfixContent=postfixContent>
              <@field type="checkbox" inline=true name="useShipAddr" value="Y" onClick="javascript:shipBillAddr();" checked=(requestParameters.useShipAddr??) />
          </@invertedField>

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

          <@field type="submit" class="${styles.link_run_session!} ${styles.action_continue!}" text=uiLabelMap.CommonContinue />

    <#else>
      <#-- initial screen show a list of options -->
      <form method="post" action="<@ofbizUrl>finalizeOrder</@ofbizUrl>" name="billsetupform">
        <input type="hidden" name="finalizeMode" value="payment" />
        <input type="hidden" name="createNew" value="Y" />
        <#if productStorePaymentMethodTypeIdMap.GIFT_CARD??>
          <#assign labelContent>${uiLabelMap.AccountingCheckGiftCard}</#assign>
          <#assign postfixContent></#assign>
          <@invertedField type="generic" labelContent=labelContent postfixContent=postfixContent>
              <@field type="checkbox" container=false name="useGc" value="GC" checked=(paymentMethodType?? && paymentMethodType == "GC") />
          </@invertedField>

          <#--<hr />-->
        </#if>
        <#if productStorePaymentMethodTypeIdMap.EXT_OFFLINE??>
          <#assign labelContent>${uiLabelMap.OrderPaymentOfflineCheckMoney}</#assign>
          <#assign postfixContent></#assign>
          <@invertedField type="generic" labelContent=labelContent postfixContent=postfixContent>
              <@field type="radio" container=false name="paymentMethodType" value="offline" checked=(paymentMethodType?? && paymentMethodType == "offline") />
          </@invertedField>

          <#--<hr />-->
        </#if>
        <#if productStorePaymentMethodTypeIdMap.CREDIT_CARD??>
          <#assign labelContent>${uiLabelMap.AccountingVisaMastercardAmexDiscover}</#assign>
          <#assign postfixContent></#assign>
          <@invertedField type="generic" labelContent=labelContent postfixContent=postfixContent>
              <@field type="radio" container=false name="paymentMethodType" value="CC" checked=(paymentMethodType?? && paymentMethodType == "CC") />
          </@invertedField>

          <#--<hr />-->
        </#if>
        <#if productStorePaymentMethodTypeIdMap.EFT_ACCOUNT??>
          <#assign labelContent>${uiLabelMap.AccountingAHCElectronicCheck}</#assign>
          <#assign postfixContent></#assign>
          <@invertedField type="generic" labelContent=labelContent postfixContent=postfixContent>
              <@field type="radio" container=false name="paymentMethodType" value="EFT" checked=(paymentMethodType?? && paymentMethodType == "EFT") />
          </@invertedField>

        </#if>
          <@field type="submit" class="${styles.link_run_session!} ${styles.action_continue!}" text=uiLabelMap.CommonContinue />

      </form>
    </#if>
</@section>
