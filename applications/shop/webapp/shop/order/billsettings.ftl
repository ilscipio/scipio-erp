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
      ${screens.render(anonymoustrailScreen)}
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

      <@table type="fields"> <#-- orig: width="100%" border="0" cellpadding="1" cellspacing="0" -->
        <#if cart.getShippingContactMechId()?? && paymentMethodType != "GC">
          <@tr>
            <@td width="26%" align="right" valign="top">
              <input type="checkbox" name="useShipAddr" value="Y" onclick="javascript:shipBillAddr();" <#if requestParameters.useShipAddr??>checked="checked"</#if> />
            </@td>
            <@td colspan="2" valign="center">${uiLabelMap.FacilityBillingAddressSameShipping}
            </@td>
          </@tr>
          <@tr type="util">
            <@td colspan="2"><hr /></@td>
          </@tr>
        </#if>

        <#if (paymentMethodType == "CC" || paymentMethodType == "EFT")>
          <@tr>
            <@td width="26%" align="right" valign="top"><div class="tableheadtext">${uiLabelMap.PartyBillingAddress}</div></@td>
            <@td width="74%">&nbsp;</@td>
          </@tr>
          ${screens.render("component://shop/widget/OrderScreens.xml#genericaddress")}
        </#if>

        <#-- credit card fields -->
        <#if paymentMethodType == "CC">
          <#if !creditCard?has_content>
            <#assign creditCard = requestParameters>
          </#if>
          <@tr type="util">
            <@td colspan="2"><hr /></@td>
          </@tr>
          <@tr>
            <@td width="26%" align="right" valign="top"><div class="tableheadtext">${uiLabelMap.AccountingCreditCardInformation}</div></@td>
            <@td width="74%">&nbsp;</@td>
          </@tr>

          ${screens.render("component://accounting/widget/CommonScreens.xml#creditCardFields")}
        </#if>

        <#-- eft fields -->
        <#if paymentMethodType =="EFT">
          <#if !eftAccount?has_content>
            <#assign eftAccount = requestParameters>
          </#if>
          <@tr type="util">
            <@td colspan="2"><hr /></@td>
          </@tr>
          <@tr>
            <@td width="26%" align="right" valign="top"><div class="tableheadtext">${uiLabelMap.AccountingEFTAccountInformation}</div></@td>
            <@td width="74%">&nbsp;</@td>
          </@tr>
          <@tr>
            <@td width="26%" align="right" valign="middle">${uiLabelMap.AccountingNameOnAccount}</@td>
            <@td width="74%">
              <input type="text" class="inputBox" size="30" maxlength="60" name="nameOnAccount" value="${eftAccount.nameOnAccount!}" />
            *</@td>
          </@tr>
          <@tr>
            <@td width="26%" align="right" valign="middle">${uiLabelMap.AccountingCompanyNameOnAccount}</@td>
            <@td width="74%">
              <input type="text" class="inputBox" size="30" maxlength="60" name="companyNameOnAccount" value="${eftAccount.companyNameOnAccount!}" />
            </@td>
          </@tr>
          <@tr>
            <@td width="26%" align="right" valign="middle">${uiLabelMap.AccountingBankName}</@td>
            <@td width="74%">
              <input type="text" class="inputBox" size="30" maxlength="60" name="bankName" value="${eftAccount.bankName!}" />
            *</@td>
          </@tr>
          <@tr>
            <@td width="26%" align="right" valign="middle">${uiLabelMap.AccountingRoutingNumber}</@td>
            <@td width="74%">
              <input type="text" class="inputBox" size="10" maxlength="30" name="routingNumber" value="${eftAccount.routingNumber!}" />
            *</@td>
          </@tr>
          <@tr>
            <@td width="26%" align="right" valign="middle">${uiLabelMap.AccountingAccountType}</@td>
            <@td width="74%">
              <select name="accountType" class="selectBox">
                <option>${eftAccount.accountType!}</option>
                <option></option>
                <option>Checking</option>
                <option>Savings</option>
              </select>
            *</@td>
          </@tr>
          <@tr>
            <@td width="26%" align="right" valign="middle">${uiLabelMap.AccountingAccountNumber}</@td>
            <@td width="74%">
              <input type="text" class="inputBox" size="20" maxlength="40" name="accountNumber" value="${eftAccount.accountNumber!}" />
            *</@td>
          </@tr>
          <@tr>
            <@td width="26%" align="right" valign="middle">${uiLabelMap.CommonDescription}</@td>
            <@td width="74%">
              <input type="text" class="inputBox" size="30" maxlength="60" name="description" value="${eftAccount.description!}" />
            </@td>
          </@tr>
        </#if>

        <#-- gift card fields -->
        <#if requestParameters.useGc?default("") == "GC" || paymentMethodType == "GC">
          <#assign giftCard = requestParameters>
          <input type="hidden" name="addGiftCard" value="Y" />
          <#if paymentMethodType != "GC">
            <@tr type="util">
              <@td colspan="2"><hr /></@td>
            </@tr>
          </#if>
          <@tr>
            <@td width="26%" align="right" valign="top"><div class="tableheadtext">${uiLabelMap.AccountingGiftCardInformation}</div></@td>
            <@td width="74%">&nbsp;</@td>
          </@tr>
          <@tr>
            <@td width="26%" align="right" valign="middle">${uiLabelMap.AccountingGiftCardNumber}</@td>
            <@td width="74%">
              <input type="text" class="inputBox" size="20" maxlength="60" name="giftCardNumber" value="${giftCard.cardNumber!}" />
            *</@td>
          </@tr>
          <@tr>
            <@td width="26%" align="right" valign="middle">${uiLabelMap.AccountingPINNumber}</@td>
            <@td width="74%">
              <input type="text" class="inputBox" size="10" maxlength="60" name="giftCardPin" value="${giftCard.pinNumber!}" />
            *</@td>
          </@tr>
          <@tr>
            <@td width="26%" align="right" valign="middle">${uiLabelMap.CommonDescription}</@td>
            <@td width="74%">
              <input type="text" class="inputBox" size="30" maxlength="60" name="description" value="${giftCard.description!}" />
            </@td>
          </@tr>
          <#if paymentMethodType != "GC">
            <@tr>
              <@td width="26%" align="right" valign="middle">${uiLabelMap.AccountingAmountToUse}</@td>
              <@td width="74%">
                <input type="text" class="inputBox" size="5" maxlength="10" name="giftCardAmount" value="${giftCard.pinNumber!}" />
              *</@td>
            </@tr>
          </#if>
        </#if>
        <@tfoot>
          <@tr>
            <@td align="center" colspan="2">
              <input type="submit" class="${styles.link_run_session!} ${styles.action_update!}" value="Continue" />
            </@td>
          </@tr>
        </@tfoot>
      </@table>
    <#else>
      <#-- initial screen show a list of options -->
      <form method="post" action="<@ofbizUrl>finalizeOrder</@ofbizUrl>" name="billsetupform">
        <input type="hidden" name="finalizeMode" value="payment" />
        <input type="hidden" name="createNew" value="Y" />
        <@table type="fields"> <#-- orig: width="100%" border="0" cellpadding="1" cellspacing="0" -->
          <#if productStorePaymentMethodTypeIdMap.GIFT_CARD??>
          <@tr>
            <@td width='5%' nowrap="nowrap"><input type="checkbox" name="useGc" value="GC" <#if paymentMethodType?? && paymentMethodType == "GC">checked="checked"</#if> /></@td>
            <@td width='95%' nowrap="nowrap">${uiLabelMap.AccountingCheckGiftCard}</@td>
          </@tr>
          <@tr type="util"><@td colspan="2"><hr /></@td></@tr>
          </#if>
          <#if productStorePaymentMethodTypeIdMap.EXT_OFFLINE??>
          <@tr>
            <@td width='5%' nowrap="nowrap"><input type="radio" name="paymentMethodType" value="offline" <#if paymentMethodType?? && paymentMethodType == "offline">checked="checked"</#if> /></@td>
            <@td width='95%'nowrap="nowrap">${uiLabelMap.OrderPaymentOfflineCheckMoney}</@td>
          </@tr>
          <@tr type="util"><@td colspan="2"><hr /></@td></@tr>
          </#if>
          <#if productStorePaymentMethodTypeIdMap.CREDIT_CARD??>
          <@tr>
            <@td width='5%' nowrap="nowrap"><input type="radio" name="paymentMethodType" value="CC" <#if paymentMethodType?? && paymentMethodType == "CC">checked="checked"</#if> /></@td>
            <@td width='95%' nowrap="nowrap">${uiLabelMap.AccountingVisaMastercardAmexDiscover}</@td>
          </@tr>
          <@tr type="util"><@td colspan="2"><hr /></@td></@tr>
          </#if>
          <#if productStorePaymentMethodTypeIdMap.EFT_ACCOUNT??>
          <@tr>
            <@td width='5%' nowrap="nowrap"><input type="radio" name="paymentMethodType" value="EFT" <#if paymentMethodType?? && paymentMethodType == "EFT">checked="checked"</#if> /></@td>
            <@td width='95%' nowrap="nowrap">${uiLabelMap.AccountingAHCElectronicCheck}</@td>
          </@tr>
          </#if>
          <@tr>
            <@td align="center" colspan="2">
              <input type="submit" class="${styles.link_run_session!} ${styles.action_update!}" value="${uiLabelMap.CommonContinue}" />
            </@td>
          </@tr>
        </@table>
      </form>
    </#if>
</@section>
