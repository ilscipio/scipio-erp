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
<#-- Cato: Duplicated (forcefully) from component://order/webapp/ordermgr/entry/checkoutpayment.ftl -->

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
    } else if (mode = "EG") {
        // edit gift card
        form.action="<@ofbizUrl>updateCheckoutOptions/editgiftcard?DONE_PAGE=checkoutpayment&paymentMethodId="+value+"</@ofbizUrl>";
        form.submit();
    }
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


function updatePayMethVisibility() {
    updateNewCreditCardFormVisibility();
}

<#-- Cato: show new credit card form -->
function updateNewCreditCardFormVisibility() {
    var radio = jQuery('#newcreditcardfield .newcreditcardradioarea input');
    if (radio.length > 0) {
        if (radio.is(":checked")) {
            jQuery('#newcreditcardcontent').show();
            jQuery('#newcreditcardcontent').focus();
        } else {
            jQuery('#newcreditcardcontent').hide();
        }
    }
}

jQuery(document).ready(function() {
    <#-- FIXME: error message div for ajax failures
    jQuery("#newShipAddr_countryGeoId").change(function() {
        getAssociatedStateList('newShipAddr_countryGeoId', 'newShipAddr_stateProvinceGeoId', null, null);
    });
    getAssociatedStateList('newShipAddr_countryGeoId', 'newShipAddr_stateProvinceGeoId', null, null);
     -->

    <#-- Cato: Needed for page refreshes to work -->
    updatePayMethVisibility();
    
    <#-- Cato: FIXME? Which will work currently depends on markup -->
    jQuery('input.pay-select-radio').change(updatePayMethVisibility);
    jQuery('.pay-select-radio input').change(updatePayMethVisibility);
});

</@script>

 
<#assign cart = shoppingCart! />

<#macro menuContent menuArgs={}>
  <@menu args=menuArgs>
  <#if productStorePaymentMethodTypeIdMap.CREDIT_CARD??>
    <@menuitem type="link" href="javascript:submitForm(document.getElementById('checkoutInfoForm'), 'NC', '');" class="+${styles.action_nav!} ${styles.action_add!}" text="${uiLabelMap.CommonAdd} ${uiLabelMap.AccountingCreditCard}" />
  </#if>
  <#if productStorePaymentMethodTypeIdMap.EFT_ACCOUNT??>
    <@menuitem type="link" href="javascript:submitForm(document.getElementById('checkoutInfoForm'), 'NE', '');" class="+${styles.action_nav!} ${styles.action_add!}" text="${uiLabelMap.CommonAdd} ${uiLabelMap.AccountingEFTAccount}" />
  </#if>
  <#-- Cato: These used to be on their own menu below... -->
  <#if productStorePaymentMethodTypeIdMap.CREDIT_CARD??><@menuitem type="link" href=makeOfbizUrl("setBilling?paymentMethodType=CC&amp;singleUsePayment=Y") class="+${styles.action_run_session!} ${styles.action_update!}" text=uiLabelMap.AccountingSingleUseCreditCard /></#if>
  <#if productStorePaymentMethodTypeIdMap.GIFT_CARD??><@menuitem type="link"  href=makeOfbizUrl("setBilling?paymentMethodType=GC&amp;singleUsePayment=Y") class="+${styles.action_run_session!} ${styles.action_update!}" text=uiLabelMap.AccountingSingleUseGiftCard /></#if>
  <#if productStorePaymentMethodTypeIdMap.EFT_ACCOUNT??><@menuitem type="link" href=makeOfbizUrl("setBilling?paymentMethodType=EFT&amp;singleUsePayment=Y") class="+${styles.action_run_session!} ${styles.action_update!}" text=uiLabelMap.AccountingSingleUseEFTAccount /></#if>
  </@menu>
</#macro>
<@section title="${uiLabelMap.OrderHowShallYouPay}?" menuContent=menuContent><#-- Cato: No numbers for multi-page checkouts, make checkout too rigid: 3) ${uiLabelMap.OrderHowShallYouPay}? -->

  <#-- Cato: allow ro remember via params first, over stored -->
  <#assign selectedCheckOutPaymentId = parameters.checkOutPaymentId!checkOutPaymentId!"">
  <#assign selectedBillingAccountId = parameters.selectedBillingAccountId!selectedBillingAccountId!"">

  <@fields checkboxType="simple">
    <form method="post" id="checkoutInfoForm" name="checkoutInfoForm" action="">
        <input type="hidden" name="checkoutpage" value="payment" />
        <input type="hidden" name="BACK_PAGE" value="checkoutoptions" />
        <input type="hidden" name="issuerId" id="issuerId" value="" />
            <#-- Payment Method Selection -->

            <#if productStorePaymentMethodTypeIdMap.EXT_OFFLINE??>
              <#macro payMethContent args={}><label for="checkOutPaymentId_OFFLINE">${uiLabelMap.OrderPaymentOfflineCheckMoney}</label></#macro><#-- Cato: Use full so clearer: OrderMoneyOrder -->
              <@checkoutInvField type="radio" id="checkOutPaymentId_OFFLINE" name="checkOutPaymentId" value="EXT_OFFLINE" checked=("EXT_OFFLINE" == selectedCheckOutPaymentId) labelContent=payMethContent class="+pay-select-radio"/>
            </#if>
            <#if productStorePaymentMethodTypeIdMap.EXT_COD??>
              <#macro payMethContent args={}><label for="checkOutPaymentId_COD">${uiLabelMap.OrderCOD}</label></#macro>
              <@checkoutInvField type="radio" type="radio" id="checkOutPaymentId_COD" name="checkOutPaymentId" value="EXT_COD" checked=("EXT_COD" == selectedCheckOutPaymentId) labelContent=payMethContent class="+pay-select-radio" />
            </#if>
            <#if productStorePaymentMethodTypeIdMap.EXT_WORLDPAY??>
              <#macro payMethContent args={}><label for="checkOutPaymentId_WORLDPAY">${uiLabelMap.AccountingPayWithWorldPay}</label></#macro>
              <@checkoutInvField type="radio" id="checkOutPaymentId_WORLDPAY" name="checkOutPaymentId" value="EXT_WORLDPAY" checked=("EXT_WORLDPAY" == selectedCheckOutPaymentId) labelContent=payMethContent class="+pay-select-radio"/>
            </#if>
            <#if productStorePaymentMethodTypeIdMap.EXT_PAYPAL??>
              <#macro payMethContent args={}><label for="checkOutPaymentId_PAYPAL">${uiLabelMap.AccountingPayWithPayPal}</label></#macro>
              <@checkoutInvField type="radio" id="checkOutPaymentId_PAYPAL" name="checkOutPaymentId" value="EXT_PAYPAL" checked=("EXT_PAYPAL" == selectedCheckOutPaymentId) labelContent=payMethContent class="+pay-select-radio" />
            </#if>
            <#if productStorePaymentMethodTypeIdMap.EXT_IDEAL??>
              <#macro payMethContent args={}>
                <label for="checkOutPaymentId_IDEAL">${uiLabelMap.AccountingPayWithiDEAL}</label>
                <div id="issuers">
                  <#if issuerList?has_content>
                    <@field type="select" name="issuer" id="issuer" label=uiLabelMap.AccountingBank>
                      <#list issuerList as issuer>
                        <option value="${issuer.getIssuerID()}"<#if issuer.getIssuerID() == (parameters.issuer!)> selected="selected"</#if>>${issuer.getIssuerName()}</option>
                      </#list>
                    </@field>
                  </#if>
                </div>
              </#macro>
              <@checkoutInvField type="radio" id="checkOutPaymentId_IDEAL" name="checkOutPaymentId" value="EXT_IDEAL" checked=("EXT_IDEAL" == selectedCheckOutPaymentId) labelContent=payMethContent class="+pay-select-radio" />
            </#if>

            <#if !paymentMethodList?has_content>
              <#-- Cato: This is too weird because there are clearly options above, they mean the user has no payment
                methods set in his account: 
              <@alert type="warning">${uiLabelMap.AccountingNoPaymentMethods}.</@alert>-->
            <#else>
              <#list paymentMethodList as paymentMethodLocal>
                <#-- Cato: workaround for access from macros -->
                <#assign paymentMethod = paymentMethodLocal>
              
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

                  <#macro payMethContent args={}>
                    <label for="checkOutPayment_${paymentMethod.paymentMethodId}">${uiLabelMap.AccountingGift}: ${giftCardNumber}</label>
                    <#if paymentMethod.description?has_content>(${paymentMethod.description})</#if>
                    <a href="javascript:submitForm(document.getElementById('checkoutInfoForm'), 'EG', '${paymentMethod.paymentMethodId}');" class="${styles.link_nav!} ${styles.action_update!}">${uiLabelMap.CommonUpdate}</a>
                    <#if parameters["amount_${paymentMethod.paymentMethodId}"]??>
                      <#assign fieldValue = parameters["amount_${paymentMethod.paymentMethodId}"]>  
                    <#else>
                      <#assign fieldValue><#if ((cart.getPaymentAmount(paymentMethod.paymentMethodId)!0) > 0)>${cart.getPaymentAmount(paymentMethod.paymentMethodId)?string("##0.00")}</#if></#assign>
                    </#if>
                    <@field type="input" label=uiLabelMap.OrderBillUpTo size="5" name="amount_${paymentMethod.paymentMethodId}" value=fieldValue />
                  </#macro>
                  <@checkoutInvField type="checkbox" id="checkOutPayment_${paymentMethod.paymentMethodId}" name="checkOutPaymentId" value="${paymentMethod.paymentMethodId}" checked=cart.isPaymentSelected(paymentMethod.paymentMethodId) labelContent=payMethContent />
                 </#if>
                <#elseif paymentMethod.paymentMethodTypeId == "CREDIT_CARD">
                  <#if productStorePaymentMethodTypeIdMap.CREDIT_CARD??>
                    <#assign creditCard = paymentMethod.getRelatedOne("CreditCard", false) />
                    <#macro payMethContent args={}>
                      <label for="checkOutPayment_${paymentMethod.paymentMethodId}">${uiLabelMap.AccountingCreditCard}: <@formattedCreditCard creditCard=creditCard paymentMethod=paymentMethod verbose=true /></label>
                      <#if paymentMethod.description?has_content>(${paymentMethod.description})</#if>
                      <a href="javascript:submitForm(document.getElementById('checkoutInfoForm'), 'EC', '${paymentMethod.paymentMethodId}');" class="${styles.link_nav!} ${styles.action_update!}">${uiLabelMap.CommonUpdate}</a>
                      <#if parameters["amount_${paymentMethod.paymentMethodId}"]??>
                        <#assign fieldValue = parameters["amount_${paymentMethod.paymentMethodId}"]>
                      <#else>
                        <#assign fieldValue><#if ((cart.getPaymentAmount(paymentMethod.paymentMethodId)!0) > 0)>${cart.getPaymentAmount(paymentMethod.paymentMethodId)?string("##0.00")}</#if></#assign>
                      </#if>
                      <@field type="input" label=uiLabelMap.OrderBillUpTo size="5" id="amount_${paymentMethod.paymentMethodId}" name="amount_${paymentMethod.paymentMethodId}" value=fieldValue />
                    </#macro>
                    <#-- Cato: NOTE: I've changed this from checkbox to radio, because I'm not sure why this would be an addon while EFT is not (from user POV)
                        cart.isPaymentSelected(paymentMethod.paymentMethodId) -->
                    <@checkoutInvField type="radio" id="checkOutPayment_${paymentMethod.paymentMethodId}" name="checkOutPaymentId" value="${paymentMethod.paymentMethodId}" checked=(paymentMethod.paymentMethodId == selectedCheckOutPaymentId) labelContent=payMethContent class="+pay-select-radio"/>
                  </#if>
                <#elseif paymentMethod.paymentMethodTypeId == "EFT_ACCOUNT">
                  <#if productStorePaymentMethodTypeIdMap.EFT_ACCOUNT??>
                    <#assign eftAccount = paymentMethod.getRelatedOne("EftAccount", false) />
                    <#macro payMethContent args={}>
                      <label for="checkOutPayment_${paymentMethod.paymentMethodId}">${uiLabelMap.AccountingEFTAccount}: ${eftAccount.bankName!}: ${eftAccount.accountNumber!}</label>
                      <#if paymentMethod.description?has_content><p>(${paymentMethod.description})</p></#if>
                      <a href="javascript:submitForm(document.getElementById('checkoutInfoForm'), 'EE', '${paymentMethod.paymentMethodId}');" class="${styles.link_nav!} ${styles.action_update!}">${uiLabelMap.CommonUpdate}</a>
                    </#macro>
                    <@checkoutInvField type="radio" id="checkOutPayment_${paymentMethod.paymentMethodId}" name="checkOutPaymentId" value="${paymentMethod.paymentMethodId}" checked=(paymentMethod.paymentMethodId == selectedCheckOutPaymentId) labelContent=payMethContent class="+pay-select-radio"/>
                  </#if>
                </#if>
              </#list>
            </#if>

            <#-- Cato: JS-based new credit card option (WIP) -->
            <#if productStorePaymentMethodTypeIdMap.CREDIT_CARD??>
              <#macro payMethContent args={}>
                <label for="newCreditCard">${uiLabelMap.AccountingCreditCard}: ${uiLabelMap.CommonNew}</label>
                <div id="newcreditcardcontent">
                  (NOT IMPLEMENTED)
                  <input type="hidden" name="newCreditCardPrefix" value="newCreditCard_" />
                  
                  <#-- Cato: FIELDS BASED ON editcreditcard.ftl -->
                  <@render resource="component://accounting/widget/CommonScreens.xml#creditCardFields" 
                    ctxVars={"ccfFieldNamePrefix": "newCreditCard_"} />
                  <@field type="generic" label=uiLabelMap.PartyBillingAddress>
                    <@render resource="component://shop/widget/CustomerScreens.xml#billaddresspick" 
                        ctxVars={"billAddrNewAddrInline":true, 
                            "billAddrFieldNamePrefix":"newCreditCard_address_",
                            "billAddrNewAddrContentId":"newcreditcard_newbilladdrcontent",
                            "billAddrFieldClass":"new-cc-bill-addr-pick-radio"
                            }/>
                  </@field>

                  <#if userHasAccount>
                    <@field type="checkbox" checkboxType="simple-standard" name="newCreditCard_save" value="Y" checked=((parameters.newCreditCard_save!) == "Y") label="Save to Account"/> <#-- TODO: Localize -->
                  </#if>
                  <@field type="input" label=uiLabelMap.OrderBillUpTo size="5" name="newCreditCard_amount" value=(parameters.newCreditCard_amount!) />
                </div>
              </#macro>
              <@checkoutInvField type="radio" id="newCreditCard" name="checkOutPaymentId" value="_NEW_CREDIT_CARD_" labelContent=payMethContent checked=("_NEW_CREDIT_CARD_" == selectedCheckOutPaymentId)
                containerId="newcreditcardfield" id="newcreditcardradio" widgetAreaClass="+newcreditcardradioarea" class="+pay-select-radio" />
            </#if>

            <#-- special billing account functionality to allow use w/ a payment method -->
            <#if productStorePaymentMethodTypeIdMap.EXT_BILLACT??>
              <#if billingAccountList?has_content>
                <@field type="select" name="billingAccountId" id="billingAccountId" label=uiLabelMap.FormFieldTitle_billingAccountId>
                  <option value=""></option>
                    <#list billingAccountList as billingAccount>
                      <#assign availableAmount = billingAccount.accountBalance>
                      <#assign accountLimit = billingAccount.accountLimit>
                      <option value="${billingAccount.billingAccountId}"<#if billingAccount.billingAccountId == (selectedBillingAccountId!"")> selected="selected"</#if>>${billingAccount.description!""} [${billingAccount.billingAccountId}] ${uiLabelMap.EcommerceAvailable} <@ofbizCurrency amount=availableAmount isoCode=billingAccount.accountCurrencyUomId/> ${uiLabelMap.EcommerceLimit} <@ofbizCurrency amount=accountLimit isoCode=billingAccount.accountCurrencyUomId/></option>
                    </#list>
                </@field>
                <@field type="input" size="5" id="billingAccountAmount" name="billingAccountAmount" value=(parameters.billingAccountAmount!) label=uiLabelMap.OrderBillUpTo/>
              </#if>
            </#if>
            <#-- end of special billing account functionality -->

            <#if productStorePaymentMethodTypeIdMap.GIFT_CARD??>
              <input type="hidden" name="singleUseGiftCard" value="Y" />
              <#macro payMethContent args={}>
                <label for="addGiftCard">${uiLabelMap.AccountingUseGiftCardNotOnFile}</label>
                <@field type="input" size="15" id="giftCardNumber" name="giftCardNumber" value=((parameters.giftCardNumber)!) onFocus="document.getElementById('addGiftCard').checked=true;" label=uiLabelMap.AccountingNumber/>
                <#if cart.isPinRequiredForGC(delegator)>
                  <@field type="input" size="10" id="giftCardPin" name="giftCardPin" value=((parameters.giftCardPin)!) onFocus="document.getElementById('addGiftCard').checked=true;" label=uiLabelMap.AccountingPIN/>
                </#if>
                <@field type="input" size="6" id="giftCardAmount" name="giftCardAmount" value=((parameters.giftCardAmount)!) onFocus="document.getElementById('addGiftCard').checked=true;" label=uiLabelMap.AccountingAmount/>
              </#macro>
              <@checkoutInvField type="checkbox" id="addGiftCard" name="addGiftCard" value="Y" labelContent=payMethContent checked=((parameters.addGiftCard!) == "Y")/>
            </#if>

          <#-- Cato: put as part of other menu for now...
            <@menu type="button">
                <#if productStorePaymentMethodTypeIdMap.CREDIT_CARD??><@menuitem type="link" href=makeOfbizUrl("setBilling?paymentMethodType=CC&amp;singleUsePayment=Y") class="+${styles.action_run_session!} ${styles.action_update!}" text=uiLabelMap.AccountingSingleUseCreditCard /></#if>
                <#if productStorePaymentMethodTypeIdMap.GIFT_CARD??><@menuitem type="link"  href=makeOfbizUrl("setBilling?paymentMethodType=GC&amp;singleUsePayment=Y") class="+${styles.action_run_session!} ${styles.action_update!}" text=uiLabelMap.AccountingSingleUseGiftCard /></#if>
                <#if productStorePaymentMethodTypeIdMap.EFT_ACCOUNT??><@menuitem type="link" href=makeOfbizUrl("setBilling?paymentMethodType=EFT&amp;singleUsePayment=Y") class="+${styles.action_run_session!} ${styles.action_update!}" text=uiLabelMap.AccountingSingleUseEFTAccount /></#if>
            </@menu>
          -->

            <#-- End Payment Method Selection -->
    </form>
    
  </@fields>
</@section>

<#-- Cato: Not this label, may be intermediate payment screens: uiLabelMap.OrderContinueToFinalOrderReview -->
<@checkoutActionsMenu directLinks=false formName="checkoutInfoForm" text=uiLabelMap.CommonContinue />

