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
<#-- Cato: TODO: convert template (maybe wait until after updates from branch) - this is not yet part of orderentry... -->
<#-- TODO : Need formatting -->
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
    } else if (mode == "NC") {
        // new credit card
        form.action="<@ofbizUrl>updateCheckoutOptions/editcreditcard?DONE_PAGE=checkoutpayment</@ofbizUrl>";
        form.submit();
    } else if (mode == "EC") {
        // edit credit card
        form.action="<@ofbizUrl>updateCheckoutOptions/editcreditcard?DONE_PAGE=checkoutpayment&paymentMethodId="+value+"</@ofbizUrl>";
        form.submit();
    } else if (mode == "GC") {
        // edit gift card
        form.action="<@ofbizUrl>updateCheckoutOptions/editgiftcard?paymentMethodId="+value+"</@ofbizUrl>";
        form.submit();
    } else if (mode == "NE") {
        // new eft account
        form.action="<@ofbizUrl>updateCheckoutOptions/editeftaccount?DONE_PAGE=checkoutpayment</@ofbizUrl>";
        form.submit();
    } else if (mode == "EE") {
        // edit eft account
        form.action="<@ofbizUrl>updateCheckoutOptions/editeftaccount?DONE_PAGE=checkoutpayment&paymentMethodId="+value+"</@ofbizUrl>";
        form.submit();
    }else if(mode = "EG")
    //edit gift card
        form.action="<@ofbizUrl>updateCheckoutOptions/editgiftcard?DONE_PAGE=checkoutpayment&paymentMethodId="+value+"</@ofbizUrl>";
        form.submit();
}

$(document).ready(function(){
var issuerId = "";
    if ($('#checkOutPaymentId_IDEAL').attr('checked') == true) {
        $('#issuers').show();
        issuerId = $('#issuer').val();
        $('#issuerId').val(issuerId);
    } else {
        $('#issuers').hide();
        $('#issuerId').val('');
    }
    $('input:radio').click(function(){
        if ($(this).val() == "EXT_IDEAL") {
            $('#issuers').show();
            issuerId = $('#issuer').val();
            $('#issuerId').val(issuerId);
        } else {
            $('#issuers').hide();
            $('#issuerId').val('');
        }
    });
    $('#issuer').change(function(){
        issuerId = $(this).val();
        $('#issuerId').val(issuerId);
    });
});
</@script>

 
<#assign cart = shoppingCart! />

<form method="post" id="checkoutInfoForm" action="">
    <input type="hidden" name="checkoutpage" value="payment" />
    <input type="hidden" name="BACK_PAGE" value="checkoutoptions" />
    <input type="hidden" name="issuerId" id="issuerId" value="" />

    <@section title="3) ${uiLabelMap.OrderHowShallYouPay}?">
            <#-- Payment Method Selection -->
            <div>
                <#if productStorePaymentMethodTypeIdMap.CREDIT_CARD??>
                  <a href="javascript:submitForm(document.getElementById('checkoutInfoForm'), 'NC', '');" class="${styles.link_nav!} ${styles.action_add!}">${uiLabelMap.CommonAdd} ${uiLabelMap.AccountingCreditCard}</a>
                </#if>
                <#if productStorePaymentMethodTypeIdMap.EFT_ACCOUNT??>
                  <a href="javascript:submitForm(document.getElementById('checkoutInfoForm'), 'NE', '');" class="${styles.link_nav!} ${styles.action_add!}">${uiLabelMap.CommonAdd} ${uiLabelMap.AccountingEFTAccount}</a>
                </#if>
            </div>
            <#if productStorePaymentMethodTypeIdMap.EXT_OFFLINE??>
              <#assign labelContent><label for="checkOutPaymentId_OFFLINE">${uiLabelMap.OrderPaymentOfflineCheckMoney}</label></#assign><#-- Cato: Use full so clearer: OrderMoneyOrder -->
              <@invertedField type="generic" labelContent=labelContent>
                  <input type="radio" id="checkOutPaymentId_OFFLINE" name="checkOutPaymentId" value="EXT_OFFLINE" <#if "EXT_OFFLINE" == checkOutPaymentId>checked="checked"</#if> />
              </@invertedField>
            </#if>
            <#if productStorePaymentMethodTypeIdMap.EXT_COD??>
              <#assign labelContent><label for="checkOutPaymentId_COD">${uiLabelMap.OrderCOD}</label></#assign>
              <@invertedField type="generic" labelContent=labelContent>
                  <input type="radio" id="checkOutPaymentId_COD" name="checkOutPaymentId" value="EXT_COD" <#if "EXT_COD" == checkOutPaymentId>checked="checked"</#if> />
              </@invertedField>
            </#if>
            <#if productStorePaymentMethodTypeIdMap.EXT_WORLDPAY??>
              <#assign labelContent><label for="checkOutPaymentId_WORLDPAY">${uiLabelMap.AccountingPayWithWorldPay}</label></#assign>
              <@invertedField type="generic" labelContent=labelContent>
                  <input type="radio" id="checkOutPaymentId_WORLDPAY" name="checkOutPaymentId" value="EXT_WORLDPAY" <#if "EXT_WORLDPAY" == checkOutPaymentId>checked="checked"</#if> />
              </@invertedField>
            </#if>
            <#if productStorePaymentMethodTypeIdMap.EXT_PAYPAL??>
              <#assign labelContent><label for="checkOutPaymentId_PAYPAL">${uiLabelMap.AccountingPayWithPayPal}</label></#assign>
              <@invertedField type="generic" labelContent=labelContent>
                  <input type="radio" id="checkOutPaymentId_PAYPAL" name="checkOutPaymentId" value="EXT_PAYPAL" <#if "EXT_PAYPAL" == checkOutPaymentId>checked="checked"</#if> />
              </@invertedField>
            </#if>
            <#if productStorePaymentMethodTypeIdMap.EXT_IDEAL??>
              <#assign labelContent><label for="checkOutPaymentId_IDEAL">${uiLabelMap.AccountingPayWithiDEAL}</label></#assign>
              <@invertedField type="generic" labelContent=labelContent>
                  <input type="radio" id="checkOutPaymentId_IDEAL" name="checkOutPaymentId" value="EXT_IDEAL" <#if "EXT_IDEAL" == checkOutPaymentId>checked="checked"</#if> />
              </@invertedField>
              
              <div id="issuers">
              <div><label >${uiLabelMap.AccountingBank}</label></div>
                <select name="issuer" id="issuer">
                <#if issuerList?has_content>
                    <#list issuerList as issuer>
                        <option value="${issuer.getIssuerID()}" >${issuer.getIssuerName()}</option>
                    </#list>
                </#if>
              </select>
              </div>
            </#if>
            <#if !paymentMethodList?has_content>
              <div>
                  <strong>${uiLabelMap.AccountingNoPaymentMethods}.</strong>
              </div>
            <#else>
              <#list paymentMethodList as paymentMethod>
                <#if paymentMethod.paymentMethodTypeId == "GIFT_CARD">
                 <#if productStorePaymentMethodTypeIdMap.GIFT_CARD??>
                  <#assign giftCard = paymentMethod.getRelatedOne("GiftCard", false) />

                  <#if giftCard?has_content && giftCard.cardNumber?has_content>
                    <#assign giftCardNumber = "" />
                    <#assign pcardNumber = giftCard.cardNumber />
                    <#if pcardNumber?has_content>
                      <#assign psize = pcardNumber?length - 4 />
                      <#if (0 < psize)>
                        <#list 0 .. psize-1 as foo>
                          <#assign giftCardNumber = giftCardNumber + "*" />
                        </#list>
                        <#assign giftCardNumber = giftCardNumber + pcardNumber[psize .. psize + 3] />
                      <#else>
                        <#assign giftCardNumber = pcardNumber />
                      </#if>
                    </#if>
                  </#if>

                  <div>
                      <input type="checkbox" id="checkOutPayment_${paymentMethod.paymentMethodId}" name="checkOutPaymentId" value="${paymentMethod.paymentMethodId}" <#if cart.isPaymentSelected(paymentMethod.paymentMethodId)>checked="checked"</#if> />
                      <label for="checkOutPayment_${paymentMethod.paymentMethodId}">${uiLabelMap.AccountingGift}:${giftCardNumber}</label>
                        <#if paymentMethod.description?has_content>(${paymentMethod.description})</#if>
                        <a href="javascript:submitForm(document.getElementById('checkoutInfoForm'), 'EG', '${paymentMethod.paymentMethodId}');" class="${styles.link_nav!} ${styles.action_update!}">${uiLabelMap.CommonUpdate}</a>
                        <strong>${uiLabelMap.OrderBillUpTo}:</strong> <input type="text" size="5" name="amount_${paymentMethod.paymentMethodId}" value="<#if (cart.getPaymentAmount(paymentMethod.paymentMethodId)?default(0) > 0)>${cart.getPaymentAmount(paymentMethod.paymentMethodId)?string("##0.00")}</#if>" />
                  </div>
                 </#if>
                <#elseif paymentMethod.paymentMethodTypeId == "CREDIT_CARD">
                 <#if productStorePaymentMethodTypeIdMap.CREDIT_CARD??>
                  <#assign creditCard = paymentMethod.getRelatedOne("CreditCard", false) />
                  <div>
                      <input type="checkbox" id="checkOutPayment_${paymentMethod.paymentMethodId}" name="checkOutPaymentId" value="${paymentMethod.paymentMethodId}" <#if cart.isPaymentSelected(paymentMethod.paymentMethodId)>checked="checked"</#if> />
                      <label for="checkOutPayment_${paymentMethod.paymentMethodId}">CC:${Static["org.ofbiz.party.contact.ContactHelper"].formatCreditCard(creditCard)}</label>
                        <#if paymentMethod.description?has_content>(${paymentMethod.description})</#if>
                        <a href="javascript:submitForm(document.getElementById('checkoutInfoForm'), 'EC', '${paymentMethod.paymentMethodId}');" class="${styles.link_nav!} ${styles.action_update!}">${uiLabelMap.CommonUpdate}</a>
                        <label for="amount_${paymentMethod.paymentMethodId}"><strong>${uiLabelMap.OrderBillUpTo}:</strong></label><input type="text" size="5" id="amount_${paymentMethod.paymentMethodId}" name="amount_${paymentMethod.paymentMethodId}" value="<#if (cart.getPaymentAmount(paymentMethod.paymentMethodId)?default(0) > 0)>${cart.getPaymentAmount(paymentMethod.paymentMethodId)?string("##0.00")}</#if>" />
                  </div>
                 </#if>
                <#elseif paymentMethod.paymentMethodTypeId == "EFT_ACCOUNT">
                 <#if productStorePaymentMethodTypeIdMap.EFT_ACCOUNT??>
                  <#assign eftAccount = paymentMethod.getRelatedOne("EftAccount", false) />
                  <div>
                      <input type="radio" id="checkOutPayment_${paymentMethod.paymentMethodId}" name="checkOutPaymentId" value="${paymentMethod.paymentMethodId}" <#if paymentMethod.paymentMethodId == checkOutPaymentId>checked="checked"</#if> />
                      <label for="checkOutPayment_${paymentMethod.paymentMethodId}">${uiLabelMap.AccountingEFTAccount}:${eftAccount.bankName!}: ${eftAccount.accountNumber!}</label>
                        <#if paymentMethod.description?has_content><p>(${paymentMethod.description})</p></#if>
                      <a href="javascript:submitForm(document.getElementById('checkoutInfoForm'), 'EE', '${paymentMethod.paymentMethodId}');" class="${styles.link_nav!} ${styles.action_update!}">${uiLabelMap.CommonUpdate}</a>
                  </div>
                 </#if>
                </#if>
              </#list>
            </#if>

            <#-- special billing account functionality to allow use w/ a payment method -->
            <#if productStorePaymentMethodTypeIdMap.EXT_BILLACT??>
              <#if billingAccountList?has_content>
                <div>
                    <select name="billingAccountId" id="billingAccountId">
                      <option value=""></option>
                        <#list billingAccountList as billingAccount>
                          <#assign availableAmount = billingAccount.accountBalance>
                          <#assign accountLimit = billingAccount.accountLimit>
                          <option value="${billingAccount.billingAccountId}" <#if billingAccount.billingAccountId == (selectedBillingAccountId!"")>selected="selected"</#if>>${billingAccount.description!""} [${billingAccount.billingAccountId}] ${uiLabelMap.EcommerceAvailable} <@ofbizCurrency amount=availableAmount isoCode=billingAccount.accountCurrencyUomId/> ${uiLabelMap.EcommerceLimit} <@ofbizCurrency amount=accountLimit isoCode=billingAccount.accountCurrencyUomId/></option>
                        </#list>
                    </select>
                    <label for="billingAccountId">${uiLabelMap.FormFieldTitle_billingAccountId}</label>
                </div>
                <div>
                    <input type="text" size="5" id="billingAccountAmount" name="billingAccountAmount" value="" />
                    <label for="billingAccountAmount">${uiLabelMap.OrderBillUpTo}</label>
                </div>
              </#if>
            </#if>
            <#-- end of special billing account functionality -->

            <#if productStorePaymentMethodTypeIdMap.GIFT_CARD??>
              <div>
                  <input type="checkbox" id="addGiftCard" name="addGiftCard" value="Y" />
                  <input type="hidden" name="singleUseGiftCard" value="Y" />
                  <label for="addGiftCard">${uiLabelMap.AccountingUseGiftCardNotOnFile}</label>
              </div>
              <div>
                  <label for="giftCardNumber">${uiLabelMap.AccountingNumber}</label>
                  <input type="text" size="15" id="giftCardNumber" name="giftCardNumber" value="${(requestParameters.giftCardNumber)!}" onfocus="document.getElementById('addGiftCard').checked=true;" />
              </div>
              <#if cart.isPinRequiredForGC(delegator)>
              <div>
                  <label for="giftCardPin">${uiLabelMap.AccountingPIN}</label>
                  <input type="text" size="10" id="giftCardPin" name="giftCardPin" value="${(requestParameters.giftCardPin)!}" onfocus="document.getElementById('addGiftCard').checked=true;" />
              </div>
              </#if>
              <div>
                  <label for="giftCardAmount">${uiLabelMap.AccountingAmount}</label>
                  <input type="text" size="6" id="giftCardAmount" name="giftCardAmount" value="${(requestParameters.giftCardAmount)!}" onfocus="document.getElementById('addGiftCard').checked=true;" />
              </div>
            </#if>

              <div>
                    <#if productStorePaymentMethodTypeIdMap.CREDIT_CARD??><a href="<@ofbizUrl>setBilling?paymentMethodType=CC&amp;singleUsePayment=Y</@ofbizUrl>" class="${styles.link_run_session!} ${styles.action_update!}">${uiLabelMap.AccountingSingleUseCreditCard}</a></#if>
                    <#if productStorePaymentMethodTypeIdMap.GIFT_CARD??><a href="<@ofbizUrl>setBilling?paymentMethodType=GC&amp;singleUsePayment=Y</@ofbizUrl>" class="${styles.link_run_session!} ${styles.action_update!}">${uiLabelMap.AccountingSingleUseGiftCard}</a></#if>
                    <#if productStorePaymentMethodTypeIdMap.EFT_ACCOUNT??><a href="<@ofbizUrl>setBilling?paymentMethodType=EFT&amp;singleUsePayment=Y</@ofbizUrl>" class="${styles.link_run_session!} ${styles.action_update!}">${uiLabelMap.AccountingSingleUseEFTAccount}</a></#if>
              </div>
            <#-- End Payment Method Selection -->
    </@section>
</form>

<@menu type="button">
  <@menuitem type="link" href="javascript:submitForm(document.getElementById('checkoutInfoForm'), 'CS', '');" class="+${styles.action_nav!} ${styles.action_cancel!}" text=uiLabelMap.OrderBacktoShoppingCart />
  <@menuitem type="link" href="javascript:submitForm(document.getElementById('checkoutInfoForm'), 'DN', '');" class="+${styles.action_run_session!} ${styles.action_continue!}" text=uiLabelMap.OrderContinueToFinalOrderReview />
</@menu>
