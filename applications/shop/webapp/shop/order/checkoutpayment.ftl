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
<#-- Scipio: Duplicated (forcefully) from component://order/webapp/ordermgr/entry/checkoutpayment.ftl -->

<#-- TODO: PAYPAL, IDEAL, WORLDPAY -->
<#-- TODO: SHOW GIFT CARD BALANCE -->
<#-- TODO?: Amount boxes could pre-calculate and adjust automatically, but it's not clear if the leave-empty
    logic is exactly replicable in JS (must be exact) or might require AJAX calls -->

<#include "ordercommon.ftl">

<#-- Scipio: TODO: convert template (maybe wait until after updates from branch) - this is not yet part of orderentry... -->
<#-- TODO : Need formatting -->
<@script>
function submitForm(form, mode, value) {
    if (mode == "DN") {
        // done action; checkout
        form.action="<@ofbizUrl>checkoutoptions</@ofbizUrl>";
        
        <#-- Scipio: must process some checkboxes -->
        if (jQuery('#newCreditCard_saveToAccount').is(":checked")) {
            jQuery('#singleUsePayment__NEW_CREDIT_CARD_').val("N");
        } else {
            jQuery('#singleUsePayment__NEW_CREDIT_CARD_').val("Y");
        }
        
        <#-- Scipio: must process some checkboxes -->
        if (jQuery('#newEftAccount_saveToAccount').is(":checked")) {
            jQuery('#singleUsePayment__NEW_EFT_ACCOUNT_').val("N");
        } else {
            jQuery('#singleUsePayment__NEW_EFT_ACCOUNT_').val("Y");
        }
        
        <#-- Scipio: must process some checkboxes -->
        if (jQuery('#newGiftCard_saveToAccount').is(":checked")) {
            jQuery('#singleUseGiftCard').val("N");
        } else {
            jQuery('#singleUseGiftCard').val("Y");
        }
        
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
    } else if (mode == "EG") {
        // edit gift card
        form.action="<@ofbizUrl>updateCheckoutOptions/editgiftcard?DONE_PAGE=checkoutpayment&paymentMethodId="+value+"</@ofbizUrl>";
        form.submit();
    } else if (mode == "EA") { <#-- SCIPIO: new -->
        // edit address
        form.action="<@ofbizUrl>updateCheckoutOptions/editcontactmech?DONE_PAGE=checkoutpayment&contactMechId="+value+"</@ofbizUrl>";
        form.submit();
    }
}

<#-- Scipio: now covered by script further below 
jQuery(document).ready(function(){
    var issuerId = "";
    if ($('#checkOutPaymentId_IDEAL').attr('checked') == true) {
        $('#content_IDEAL').show();
        issuerId = $('#issuer').val();
        $('#issuerId').val(issuerId);
    } else {
        $('#content_IDEAL').hide();
        $('#issuerId').val('');
    }
    $('input:radio').click(function(){
        if ($(this).val() == "EXT_IDEAL") {
            $('#content_IDEAL').show();
            issuerId = $('#issuer').val();
            $('#issuerId').val(issuerId);
        } else {
            $('#content_IDEAL').hide();
            $('#issuerId').val('');
        }
    });
    $('#issuer').change(function(){
        issuerId = $(this).val();
        $('#issuerId').val(issuerId);
    });

});
-->
</@script>

 
<#assign cart = shoppingCart! />

<#-- Scipio: We need to show total because this influence user's decision and amounts he will enter -->
<p><strong>${uiLabelMap.OrderOrderTotal}:</strong> <@ofbizCurrency amount=((cart.getDisplayGrandTotal())!0) isoCode=cart.getCurrency()/></p>

<#macro menuContent menuArgs={}>
  <@menu args=menuArgs>
<#-- Scipio: all deprecated
  <#if productStorePaymentMethodTypeIdMap.CREDIT_CARD??>
    <@menuitem type="link" href="javascript:submitForm(document.getElementById('checkoutInfoForm'), 'NC', '');" class="+${styles.action_nav!} ${styles.action_add!}" text="${rawString(uiLabelMap.CommonAdd)} ${rawString(uiLabelMap.AccountingCreditCard)}" />
  </#if>
  <#if productStorePaymentMethodTypeIdMap.EFT_ACCOUNT??>
    <@menuitem type="link" href="javascript:submitForm(document.getElementById('checkoutInfoForm'), 'NE', '');" class="+${styles.action_nav!} ${styles.action_add!}" text="${rawString(uiLabelMap.CommonAdd)} ${rawString(uiLabelMap.AccountingEFTAccount)}" />
  </#if>
  <#if productStorePaymentMethodTypeIdMap.CREDIT_CARD??><@menuitem type="link" href=makeOfbizUrl("setBilling?paymentMethodType=CC&amp;singleUsePayment=Y") class="+${styles.action_run_session!} ${styles.action_update!}" text=uiLabelMap.AccountingSingleUseCreditCard /></#if>
  <#if productStorePaymentMethodTypeIdMap.GIFT_CARD??><@menuitem type="link"  href=makeOfbizUrl("setBilling?paymentMethodType=GC&amp;singleUsePayment=Y") class="+${styles.action_run_session!} ${styles.action_update!}" text=uiLabelMap.AccountingSingleUseGiftCard /></#if>
  <#if productStorePaymentMethodTypeIdMap.EFT_ACCOUNT??><@menuitem type="link" href=makeOfbizUrl("setBilling?paymentMethodType=EFT&amp;singleUsePayment=Y") class="+${styles.action_run_session!} ${styles.action_update!}" text=uiLabelMap.AccountingSingleUseEFTAccount /></#if>
-->
  </@menu>
</#macro>
<@section title="${rawString(uiLabelMap.OrderHowShallYouPay)}?" menuContent=menuContent menuLayoutGeneral="bottom"><#-- Scipio: No numbers for multi-page checkouts, make checkout too rigid: 3) ${uiLabelMap.OrderHowShallYouPay}? -->
  <#-- Scipio: allow remember form filled via parameters first, over stored -->
  <#-- NOTE: parameters.checkOutPaymentId may already be a list -->
  <#-- FIXME?: There is no real verification of correctness of exclusivity of the payment methods here... -->
  <#assign selectedCheckOutPaymentId = parameters.checkOutPaymentId!checkOutPaymentIdList!"">
  <#if selectedCheckOutPaymentId?has_content>
    <#if selectedCheckOutPaymentId?is_enumerable>
      <#assign selectedCheckOutPaymentIdList = selectedCheckOutPaymentId>
    <#else>
      <#assign selectedCheckOutPaymentIdList = [selectedCheckOutPaymentId?string]>
    </#if>
  <#else>
    <#assign selectedCheckOutPaymentIdList = []>
  </#if>
  <#assign selectedBillingAccountId = (parameters.billingAccountId!selectedBillingAccountId!"")?string>

  <form method="post" id="checkoutInfoForm" name="checkoutInfoForm" action="">
    <input type="hidden" name="checkoutpage" value="payment" />
    <input type="hidden" name="BACK_PAGE" value="checkoutoptions" />
    <input type="hidden" name="issuerId" id="issuerId" value="" />

    <#-- Scipio: Scipio: Used to build a radio/checkbox to content div mapping for JS -->
    <#function registerFieldContent fieldContentArgs>
      <#assign fieldContentIdMapList = (fieldContentIdMapList![]) + ([fieldContentArgs])>
      <#return "">
    </#function>
    <#function getPayMethTypeDesc paymentMethodTypeId>
      <#local desc = (productStorePaymentMethodSettingByTypeMap[paymentMethodTypeId].getRelatedOne("PaymentMethodType").get("description", locale))!false>
      <#if !desc?is_boolean>
        <#return desc>
      </#if>
      <#-- return nothing otherwise -->
    </#function>


    <#macro payMethInfoPanel title updateAction="">
      <#-- Scipio: Every non-new may meth option gets a panel, for consistency
          because the "new" pay meths show their fields below the title, currently
          doing the same for panel, though not sure about look
      <@row>
        <@cell columns=2>
          <@heading>${title}</@heading>
        </@cell>
        <@cell columns=10>
      -->
        <@heading>${title}</@heading>
        <@row>
          <@cell small=6>
        
          <div class="pay-meth-info-panel-container">
            <#assign bottomContent = false>
            <#if updateAction?has_content>
              <#assign bottomContent>
                <a href="javascript:submitForm(document.getElementById('checkoutInfoForm'), '${updateAction}', '${paymentMethod.paymentMethodId}');" class="${styles.link_nav!} ${styles.action_update!} panel-update-link">${uiLabelMap.CommonUpdate}</a>
              </#assign>
            </#if>
            <@panel bottomContent=bottomContent class="+pay-meth-info-panel">
              <#nested>
            </@panel>
          </div>
          </@cell>
        </@row>
      <#--
        </@cell>
      </@row>
      -->
      <#--<br/>-->
    </#macro>

    <#macro payMethAmountField type="pay-meth" payMethId="" params=true id=true name=true>
      <#if params?is_boolean>
        <#local params = parameters>
      </#if>
      <#if name?is_boolean>
        <#local name = "amount_${payMethId}">
      </#if>
      <#if id?is_boolean>
        <#local id = name>
      </#if>
      <#assign realCurPayAmount = "">
      <#if type == "simple" || type == "new-pay-meth">
          <#assign fieldValue = params[name]!>
      <#else>
          <#assign curPayAmountStr = "">
          <#assign fieldValue = "">
          <#if params[name]??>
            <#assign fieldValue = params[name]>
          <#else>
            <#-- Scipio: changed to use getPaymentOrigAmount (new method) instead of getPaymentAmount because we want to be consistent
                and only show the amounts if the user originally entered one. Otherwise, leave null, and submit will recalculate as needed: cart.getPaymentAmount(paymentMethod.paymentMethodId) -->
            <#assign curPayAmountStr><#if ((cart.getPaymentOrigAmount(payMethId)!0) > 0)>${cart.getPaymentOrigAmount(payMethId)?string("##0.00")}</#if></#assign>
            <#-- Also get the real current amount, but we'll only show it in tooltip (if use this amount as value, sometimes resubmission issues when going back pages) -->
            <#if ((cart.getPaymentAmount(payMethId)!0) > 0)>
              <#assign realCurPayAmount = cart.getPaymentAmount(payMethId)>
            </#if>
            <#-- Scipio: NOTE: We ONLY set the previous pay amount as field value if the payments are adequate to cover the current amount (NOTE: currently this definition is very strict - see ShoppingCart).
                Otherwise, the amount specified here is likely to be invalid if resubmitted as-is (as in stock it does not really function as a "bill up to" amount).
                FIXME?: There is an inconsistency here: user may have left this empty, but re-viewing payment will populate this field. Currently ShoppingCart
                    offers no way to distinguish between user-entered and validation-determined amounts. -->
            <#if cart.isPaymentsAdequate()>
              <#assign fieldValue = curPayAmountStr>
            </#if>
          </#if>
      </#if> 
      <#assign fieldDescription = uiLabelMap.AccountingLeaveEmptyForMaximumAmount + ".">
      <#assign realCurPayAmountFullStr = "">
      <#if realCurPayAmount?has_content>
        <#assign realCurPayAmountFullStr>${uiLabelMap.CommonCurrent}: <@ofbizCurrency amount=realCurPayAmount isoCode=cart.getCurrency()/></#assign>
      </#if>

      <#assign postfixContent>
        <@row>
          <@cell small=8><#-- class="+${styles.float_right!}"-->
            <@commonMsg type="info-important" closable=false>${fieldDescription}</@commonMsg>
          </@cell>
        </@row>
      </#assign>
      <#-- Scipio: NOTE: Stock ofbiz labels this as "bill up to", but it does NOT function as a "bill up to" but rather as an exact amount.
          Unless this behavior is changed, show "Amount" instead of "BillUpTo": uiLabelMap.OrderBillUpTo -->
      <@field type="input" label=uiLabelMap.AccountingAmount size="5" id="${id}" name="${name}" value=fieldValue 
        tooltip=realCurPayAmountFullStr postfix=true collapsePostfix=false postfixColumns=9 postfixContent=postfixContent />  
    </#macro>

    <#-- Scipio: Payment method content and markup.
        This pattern allows to avoid duplicating the control/loop/ordering logic and keeps the definitions for each pay method together so easier to follow alongside the original.
        The macro is invoked in steps with a different type each time. 
        WARN: Freemarker language limits use of this because can't define nested macros. Only for this page. -->
    <#macro paymentMethodContent showPrimary=false showSupplemental=false showSelect=false showDetails=false>
      <#local detailHeadingArgs = {}><#-- current is good: {"relLevel":+1} -->

      <#local primSupplSuffix = "">
      <#if showPrimary>
        <#local primSupplSuffix = "_primary">
      </#if>
      <#if showSupplemental>
        <#local primSupplSuffix = "_suppl">
      </#if>

      <@fields type="inherit-all" checkboxType="simple-standard" inlineItems=false>

      <#if showPrimary>
        <#if productStorePaymentMethodTypeIdMap.EXT_OFFLINE??>
          <#assign methodLabel>${getLabel('PaymentMethodType.description.EXT_OFFLINE', 'AccountingEntityLabels')} (${uiLabelMap.OrderMoneyOrder})</#assign>
          <#if showSelect>
            <#-- ${uiLabelMap.OrderPaymentOfflineCheckMoney} -->
            <#assign dummy = registerFieldContent({"fieldId":"checkOutPaymentId_OFFLINE", "contentId":"content_OFFLINE"})>
            <@field type="radio" id="checkOutPaymentId_OFFLINE" name="checkOutPaymentId" value="EXT_OFFLINE" checked=(selectedCheckOutPaymentIdList?seq_contains("EXT_OFFLINE")) 
              class="+pay-select-radio pay-select-field" label=methodLabel /><#--tooltip=(getPayMethTypeDesc("EXT_OFFLINE")!)-->
          </#if>
          <#if showDetails>
            <@section containerId="content_OFFLINE" containerClass="+pay-meth-content" containerStyle="display:none;"><#-- title=methodLabel -->
              <@payMethInfoPanel title=methodLabel>
                <#-- Scipio: TODO?: These descriptions could probably be integrated into the entity values using get("xxx", locale)... -->
                <p>${uiLabelMap.OrderPaymentDescOffline}</p>
              </@payMethInfoPanel>
              <@payMethAmountField payMethId="EXT_OFFLINE"/>
            </@section>
          </#if>
        </#if>
        <#if productStorePaymentMethodTypeIdMap.EXT_COD??>
          <#assign methodLabel>${getLabel('PaymentMethodType.description.EXT_COD', 'AccountingEntityLabels')} (${uiLabelMap.OrderCOD})</#assign>
          <#if showSelect>
            <#assign dummy = registerFieldContent({"fieldId":"checkOutPaymentId_COD", "contentId":"content_COD"})>
            <@field type="radio" type="radio" id="checkOutPaymentId_COD" name="checkOutPaymentId" value="EXT_COD" checked=(selectedCheckOutPaymentIdList?seq_contains("EXT_COD")) 
              class="+pay-select-radio pay-select-field" label=methodLabel /><#--tooltip=(getPayMethTypeDesc("EXT_COD")!)-->
          </#if>
          <#if showDetails>
            <@section containerId="content_COD" containerClass="+pay-meth-content" containerStyle="display:none;"><#-- title=methodLabel-->
              <@payMethInfoPanel title=methodLabel>
                <p>${uiLabelMap.OrderPaymentDescCOD}</p>
              </@payMethInfoPanel>
              <@payMethAmountField payMethId="EXT_COD"/>
            </@section>
          </#if>
        </#if>
    <#-- Scipio: TODO?: Uncomment if Paypal implemented -->
    <#if false>
        <#if productStorePaymentMethodTypeIdMap.EXT_PAYPAL??>
          <#if showSelect>
            <#assign dummy = registerFieldContent({"fieldId":"checkOutPaymentId_PAYPAL", "contentId":"content_PAYPAL"})>
            <@field type="radio" id="checkOutPaymentId_PAYPAL" name="checkOutPaymentId" value="EXT_PAYPAL" checked=(selectedCheckOutPaymentIdList?seq_contains("EXT_PAYPAL")) 
              class="+pay-select-radio pay-select-field" label=uiLabelMap.AccountingPayWithPayPal /><#--tooltip=(getPayMethTypeDesc("EXT_PAYPAL")!) -->
          </#if>
          <#if showDetails>
            <@section containerId="content_PAYPAL" containerClass="+pay-meth-content" containerStyle="display:none;"><#-- title=uiLabelMap.AccountingPayWithPayPal-->
              <@payMethInfoPanel title=uiLabelMap.AccountingPayWithPayPal>
                <p>${uiLabelMap.OrderPaymentDescPaypal}</p>
              </@payMethInfoPanel>
            </@section>
          </#if>
        </#if>
    </#if>
    <#-- Scipio: TODO?: Uncomment if Worldpay implemented -->
    <#if false>
        <#if productStorePaymentMethodTypeIdMap.EXT_WORLDPAY??>
          <#if showSelect>
            <#assign dummy = registerFieldContent({"fieldId":"checkOutPaymentId_WORLDPAY", "contentId":"content_WORLDPAY"})>
            <@field type="radio" id="checkOutPaymentId_WORLDPAY" name="checkOutPaymentId" value="EXT_WORLDPAY" checked=(selectedCheckOutPaymentIdList?seq_contains("EXT_WORLDPAY")) 
              class="+pay-select-radio pay-select-field" label=uiLabelMap.AccountingPayWithWorldPay /><#--tooltip=(getPayMethTypeDesc("EXT_WORLDPAY")!)-->
          </#if>
          <#if showDetails>
            <@section containerId="content_WORLDPAY" containerClass="+pay-meth-content" containerStyle="display:none;"><#-- title=uiLabelMap.AccountingPayWithWorldPay-->
              <@payMethInfoPanel title=uiLabelMap.AccountingPayWithWorldPay>
                <p>${uiLabelMap.OrderPaymentDescWorldpay}</p>
              </@payMethInfoPanel>
            </@section>
          </#if>
        </#if>
    </#if>
    <#-- Scipio: TODO?: Uncomment if iDEAL implemented -->
    <#if false>
        <#if productStorePaymentMethodTypeIdMap.EXT_IDEAL??>
          <#if showSelect>
            <#assign dummy = registerFieldContent({"fieldId":"checkOutPaymentId_IDEAL", "contentId":"content_IDEAL"})>
            <@field type="radio" id="checkOutPaymentId_IDEAL" name="checkOutPaymentId" value="EXT_IDEAL" checked=(selectedCheckOutPaymentIdList?seq_contains("EXT_IDEAL")) 
              class="+pay-select-radio pay-select-field" label=uiLabelMap.AccountingPayWithiDEAL /><#--tooltip=(getPayMethTypeDesc("EXT_IDEAL")!) -->
          </#if>
          <#if showDetails>
            <@section containerId="content_IDEAL" containerClass="+pay-meth-content" containerStyle="display:none;"><#-- title=uiLabelMap.AccountingPayWithiDEAL-->
              <@payMethInfoPanel title=uiLabelMap.AccountingPayWithiDEAL>
                <p>${uiLabelMap.OrderPaymentDescIdeal}</p>
              </@payMethInfoPanel>
              <#if issuerList?has_content>
                <@field type="select" name="issuer" id="issuer" label=uiLabelMap.AccountingBank>
                  <#list issuerList as issuer>
                    <option value="${issuer.getIssuerID()}"<#if issuer.getIssuerID() == (parameters.issuer!)> selected="selected"</#if>>${issuer.getIssuerName()}</option>
                  </#list>
                </@field>
              </#if>
            </@section>
          </#if>
        </#if>
    </#if>

        <#if productStorePaymentMethodTypeIdMap.CREDIT_CARD??>
          <#-- User's credit cards -->
          <#if (paymentMethodListsByType.CREDIT_CARD)?has_content>
            <#list paymentMethodListsByType.CREDIT_CARD as paymentMethodLocal>
              <#assign paymentMethod = paymentMethodLocal><#-- Scipio: workaround for access from macros -->
              <#assign creditCard = paymentMethod.getRelatedOne("CreditCard", false) />
              <#assign methodShortInfo><@formattedCreditCardShort creditCard=creditCard paymentMethod=paymentMethod verbose=true /></#assign>
              <#if showSelect>
                <#assign dummy = registerFieldContent({"fieldId":"checkOutPaymentId_${paymentMethod.paymentMethodId}", "contentId":"content_${paymentMethod.paymentMethodId}"})>
               <#-- Scipio: NOTE: I've changed this from checkbox to radio, because I'm not sure why this would be an addon while EFT is not (from user POV)
                    cart.isPaymentSelected(paymentMethod.paymentMethodId) -->
                <@field type="radio" id="checkOutPaymentId_${paymentMethod.paymentMethodId}" name="checkOutPaymentId" value=paymentMethod.paymentMethodId checked=(selectedCheckOutPaymentIdList?seq_contains(paymentMethod.paymentMethodId)) 
                  class="+pay-select-radio pay-select-field" label="${rawString(uiLabelMap.AccountingCreditCard)}: ${rawString(methodShortInfo)}" /><#--tooltip=(getPayMethTypeDesc("CREDIT_CARD")!) -->
              </#if>
              <#if showDetails>
                <@section containerId="content_${paymentMethod.paymentMethodId}" containerClass="+pay-meth-content" containerStyle="display:none;"><#--title=uiLabelMap.AccountingCreditCard-->
                  <@payMethInfoPanel title=uiLabelMap.AccountingCreditCard updateAction='EC'>
                    <@formattedCreditCardDetail creditCard=creditCard paymentMethod=paymentMethod />
                    <#if (paymentMethod.get("description", locale))?has_content><br/>(${paymentMethod.get("description", locale)})</#if>
                  </@payMethInfoPanel>
                  <@payMethAmountField payMethId=paymentMethod.paymentMethodId/>
                </@section>
              </#if>
            </#list>
          </#if>
        
          <#-- Scipio: JS-based new credit card option -->
          <#assign methodShortInfo><em>${uiLabelMap.CommonNew}</em></#assign>
          <#if showSelect>
            <#assign dummy = registerFieldContent({"fieldId":"newCreditCard", "contentId":"content__NEW_CREDIT_CARD"})>
            <@field type="radio" id="newCreditCard" name="checkOutPaymentId" value="_NEW_CREDIT_CARD_" 
              checked=(selectedCheckOutPaymentIdList?seq_contains("_NEW_CREDIT_CARD_")) class="+pay-select-radio pay-select-field" 
              label="${rawString(uiLabelMap.AccountingCreditCard)}: ${rawString(methodShortInfo)}" /><#--tooltip=(getPayMethTypeDesc("CREDIT_CARD")!) -->
          </#if>
          <#if showDetails>
            <@section containerId="content__NEW_CREDIT_CARD" containerClass="+new-item-selection-content pay-meth-content" 
                containerStyle="display:none;" title=uiLabelMap.AccountingCreditCard><#-- let JS do the following (there is no point; even if do it, still need JS for page refresh): <#if (!selectedCheckOutPaymentIdList?seq_contains("_NEW_CREDIT_CARD_"))> style="display:none;"</#if> -->
              
              <#-- Scipio: FIELDS BASED ON editcreditcard.ftl -->
              <#assign titleOnCard = "">
              <#assign firstNameOnCard = "">
              <#assign middleNameOnCard = "">
              <#assign lastNameOnCard = "">
              <#if person?has_content>
                <#assign titleOnCard = person.personalTitle!>
                <#assign firstNameOnCard = person.firstName!>
                <#assign middleNameOnCard = person.middleName!>
                <#assign lastNameOnCard = person.lastName!>
              </#if>
              <@fields type="inherit-all" fieldArgs={"gridArgs":{"totalLarge":8}}>
                <@render resource="component://shop/widget/CustomerScreens.xml#creditCardFields" 
                  ctxVars={
                    "ccfFieldNamePrefix": "newCreditCard_",
                    "ccfFallbacks":{
                        "titleOnCard":titleOnCard,
                        "firstNameOnCard":firstNameOnCard, 
                        "middleNameOnCard":middleNameOnCard, 
                        "lastNameOnCard":lastNameOnCard
                    },
                    "ccfParams":newCreditCardParams!parameters
                  }/>
              </@fields>
              <@field type="generic" label=uiLabelMap.PartyBillingAddress>
                <@render resource="component://shop/widget/CustomerScreens.xml#billaddresspickfields" 
                    ctxVars={
                        "bapfUseNewAddr":true,
                        "bapfUseUpdate":true,
                        "bapfUpdateLink":"javascript:submitForm(document.checkoutInfoForm, 'EA', '_CONTACT_MECH_ID_');",
                        "bapfNewAddrInline":true, 
                        "bapfFieldNamePrefix":"newCreditCard_",
                        "bapfNewAddrContentId":"newcreditcard_newbilladdrcontent",
                        "bapfPickFieldClass":"new-cc-bill-addr-pick-radio",
                        "bapfNewAddrFieldId":"newcreditcard_newaddrradio",
                        "bapfParams":newCreditCardParams!parameters,
                        "bapfNewAddressFieldsWrapperArgs": {
                            "type":"default",
                            "ignoreParentField":true,
                            "fieldArgs": {
                                "gridArgs": {
                                    "totalLarge":8
                                }
                            }
                        }
                    }/>
              </@field>

              <#if userHasAccount>
                <#-- Scipio: This should be the opposite of "single use payment"... so we use form submit hook to set the hidden field -->
                <@field type="checkbox" checkboxType="simple-standard" id="newCreditCard_saveToAccount" name="saveToAccount__NEW_CREDIT_CARD_" value="Y" 
                    checked=((newCreditCardParams.singleUsePayment__NEW_CREDIT_CARD_!"") != "Y") label=uiLabelMap.OrderSaveToAccount/>
                <input type="hidden" id="singleUsePayment__NEW_CREDIT_CARD_" name="singleUsePayment__NEW_CREDIT_CARD_" value="" />
              </#if>
              <@payMethAmountField payMethId="_NEW_CREDIT_CARD_" type="new-pay-meth" params=newCreditCardParams />
            </@section>
          </#if>
        </#if>

        <#if productStorePaymentMethodTypeIdMap.EFT_ACCOUNT??>
          <#-- User's EFT accounts -->
          <#if (paymentMethodListsByType.EFT_ACCOUNT)?has_content>
            <#list paymentMethodListsByType.EFT_ACCOUNT as paymentMethodLocal>
              <#assign paymentMethod = paymentMethodLocal><#-- Scipio: workaround for access from macros -->
              <#assign eftAccount = paymentMethod.getRelatedOne("EftAccount", false) />
              <#assign methodShortInfo><@formattedEftAccountShort eftAccount=eftAccount paymentMethod=paymentMethod /></#assign>
              <#if showSelect>
                <#assign dummy = registerFieldContent({"fieldId":"checkOutPaymentId_${paymentMethod.paymentMethodId}", "contentId":"content_${paymentMethod.paymentMethodId}"})>
                <@field type="radio" id="checkOutPaymentId_${paymentMethod.paymentMethodId}" name="checkOutPaymentId" value=(paymentMethod.paymentMethodId) checked=(selectedCheckOutPaymentIdList?seq_contains(paymentMethod.paymentMethodId)) 
                  class="+pay-select-radio pay-select-field" label="${rawString(uiLabelMap.AccountingEFTAccount)}: ${rawString(methodShortInfo)}" /><#--tooltip=(getPayMethTypeDesc("EFT_ACCOUNT")!) -->
              </#if>
              <#if showDetails>
                <@section containerId="content_${paymentMethod.paymentMethodId}" containerClass="+pay-meth-content" containerStyle="display:none;"><#--title=uiLabelMap.AccountingEFTAccount-->
                  <@payMethInfoPanel title=uiLabelMap.AccountingEFTAccount updateAction='EE'>
                    <@formattedEftAccountDetail eftAccount=eftAccount paymentMethod=paymentMethod />
                    <#if paymentMethod.get("description", locale)?has_content><br/>(${paymentMethod.get("description", locale)})</#if>
                  </@payMethInfoPanel>
                  <#-- Scipio: NOTE: This field was added by us, was missing for EFT accounts -->
                  <@payMethAmountField payMethId=paymentMethod.paymentMethodId/>
                </@section>
              </#if>
            </#list>
          </#if>

          <#-- Scipio: JS-based new eft account option -->
          <#assign methodShortInfo><em>${uiLabelMap.CommonNew}</em></#assign>
          <#if showSelect>
            <#assign dummy = registerFieldContent({"fieldId":"newEftAccount", "contentId":"content__NEW_EFT_ACCOUNT_"})>
            <@field type="radio" id="newEftAccount" name="checkOutPaymentId" value="_NEW_EFT_ACCOUNT_" checked=(selectedCheckOutPaymentIdList?seq_contains("_NEW_EFT_ACCOUNT_"))
              class="+pay-select-radio pay-select-field" label="${rawString(uiLabelMap.AccountingEFTAccount)}: ${rawString(methodShortInfo)}" /><#--tooltip=(getPayMethTypeDesc("EFT_ACCOUNT")!) -->
          </#if>
          <#if showDetails>
            <@section containerId="content__NEW_EFT_ACCOUNT_" containerClass="+new-item-selection-content pay-meth-content" 
                containerStyle="display:none;" title=uiLabelMap.AccountingEFTAccount><#-- let JS do it: <#if (!selectedCheckOutPaymentIdList?seq_contains("_NEW_EFT_ACCOUNT_"))> style="display:none;"</#if> -->
              
              <#-- Scipio: FIELDS BASED ON editeftaccount.ftl -->
              <#assign nameOnAccount = "">
              <#if person?has_content>
                <#-- TODO: Unhardcode -->
                <#assign nameOnAccount = "${person.firstName!} ${person.lastName!}">
              </#if>
              <@fields type="inherit-all" fieldArgs={"gridArgs":{"totalLarge":8}}>
                <@render resource="component://shop/widget/CustomerScreens.xml#eftAccountFields" 
                  ctxVars={
                    "eafFieldNamePrefix": "newEftAccount_",
                    "eafFallbacks":{"nameOnAccount":nameOnAccount},
                    "eafParams":newEftAccountParams!parameters
                    } />
              </@fields>
              <@field type="generic" label=uiLabelMap.PartyBillingAddress>
                <@render resource="component://shop/widget/CustomerScreens.xml#billaddresspickfields" 
                    ctxVars={
                        "bapfUseNewAddr":true,
                        "bapfUseUpdate":true, 
                        "bapfUpdateLink":"javascript:submitForm(document.checkoutInfoForm, 'EA', '_CONTACT_MECH_ID_');",
                        "bapfNewAddrInline":true, 
                        "bapfFieldNamePrefix":"newEftAccount_",
                        "bapfNewAddrContentId":"neweftaccount_newbilladdrcontent",
                        "bapfPickFieldClass":"new-eft-bill-addr-pick-radio",
                        "bapfNewAddrFieldId":"neweftaccount_newaddrradio",
                        "bapfParams":newEftAccountParams!parameters,
                        "bapfNewAddressFieldsWrapperArgs": {
                            "type":"default",
                            "ignoreParentField":true,
                            "fieldArgs": {
                                "gridArgs": {
                                    "totalLarge":8
                                }
                            }
                        }
                    }/>
              </@field>

              <#if userHasAccount>
                <#-- Scipio: This should be the opposite of "single use payment"... so we use form submit hook to set the hidden field -->
                <@field type="checkbox" checkboxType="simple-standard" id="newEftAccount_saveToAccount" name="saveToAccount__NEW_EFT_ACCOUNT_" 
                    value="Y" checked=((newEftAccountParams.singleUsePayment__NEW_EFT_ACCOUNT_!"") != "Y") label=uiLabelMap.OrderSaveToAccount/>
                <input type="hidden" id="singleUsePayment__NEW_EFT_ACCOUNT_" name="singleUsePayment__NEW_EFT_ACCOUNT_" value="" />
              </#if>
              <@payMethAmountField payMethId="_NEW_EFT_ACCOUNT_" params=newEftAccountParams type="new-pay-meth" />
            </@section>
          </#if>
        </#if>

        <#-- Scipio: "Additional Payment Options" radio, which leads further below -->
        <#if showSelect>
          <#if (productStorePaymentMethodTypeIdMap.EXT_BILLACT?? && billingAccountList?has_content) || productStorePaymentMethodTypeIdMap.GIFT_CARD??>
            <#assign methodLabel>${uiLabelMap.AccountingAdditionalPaymentMethods} (<#rt>
                <#if (productStorePaymentMethodTypeIdMap.EXT_BILLACT?? && billingAccountList?has_content)><#t>
                ${uiLabelMap.AccountingBillingAccount}<#t>
                <#if (productStorePaymentMethodTypeIdMap.GIFT_CARD??)>, </#if><#t>
                </#if><#t>
                <#if (productStorePaymentMethodTypeIdMap.GIFT_CARD??)>${uiLabelMap.AccountingGiftCard}</#if><#t>
            )</#assign><#lt>
            <#-- Special case: click never hides content -->
            <#assign dummy = registerFieldContent({"fieldId":"supplPayMeth", "contentId":"paymeth_supplemental", "noHideContent":true})>
            <@field type="radio" id="supplPayMeth" name="checkOutPaymentId" value="" checked=false 
              class="+pay-select-radio pay-select-field" label=methodLabel /><#-- checked handled by JS for the moment --><#--tooltip=uiLabelMap.AccountingAdditionalPaymentMethods -->
          </#if>
        </#if>

      </#if><#-- /showPrimary -->

        <#-- Scipio: The following fields are supplemental and difficult to manage, so will not try to
            combine into the above (the pre-selection logic becomes too complicated and ambiguous when we have entries
            for them in both primary and supplemental, and the javascript becomes too heavy). 
            Instead, we have a "other/additional" radio button in the primary to fulfill the HTML requirements. -->

        <#-- special billing account functionality to allow use w/ a payment method -->
        <#if productStorePaymentMethodTypeIdMap.EXT_BILLACT??>
          <#if billingAccountList?has_content>
              <#if showSupplemental>
                <#if showSelect>
                  <#-- MUST SUBMIT EMPTY VALUE FOR checkOutPaymentId -->
                  <#assign dummy = registerFieldContent({"fieldId":"checkOutPaymentId_billingAccount${primSupplSuffix}", "contentId":"content_BILLACT${primSupplSuffix}"})>
                  <@field type="checkbox" id="checkOutPaymentId_billingAccount${primSupplSuffix}" name="checkOutPaymentId" value="" checked=(selectedBillingAccountId?has_content) 
                    class="+pay-select-checkbox pay-select-field" label=uiLabelMap.AccountingBillingAccount />
                </#if>
              </#if>
              <#if showDetails && showSupplemental>
                <@section containerId="content_BILLACT${primSupplSuffix}" containerClass="+pay-meth-content" containerStyle="display:none;" title=uiLabelMap.AccountingBillingAccount>
                  <@field type="select" name="billingAccountId" id="billingAccountId${primSupplSuffix}" label=uiLabelMap.FormFieldTitle_billingAccountId>
                    <option value=""></option>
                    <#list billingAccountList as billingAccount>
                      <#assign availableAmount = billingAccount.accountBalance>
                      <#assign accountLimit = billingAccount.accountLimit>
                      <option value="${billingAccount.billingAccountId}"<#if billingAccount.billingAccountId == selectedBillingAccountId> selected="selected"</#if>><#rt>
                        <#lt>${billingAccount.description!""} [${billingAccount.billingAccountId}] ${uiLabelMap.EcommerceAvailable} <#rt>
                        <#lt><@ofbizCurrency amount=availableAmount isoCode=billingAccount.accountCurrencyUomId/> ${uiLabelMap.EcommerceLimit} <#rt>
                        <#lt><@ofbizCurrency amount=accountLimit isoCode=billingAccount.accountCurrencyUomId/></option>
                    </#list>
                  </@field>
                  <@payMethAmountField name="billingAccountAmount" id="billingAccountAmount${primSupplSuffix}" params=parameters simple=true /><#-- label=uiLabelMap.OrderBillUpTo -->
                </@section>
              </#if>
          </#if>
        </#if>

        <#if productStorePaymentMethodTypeIdMap.GIFT_CARD??>
          <#-- User's gift cards -->
          <#if (paymentMethodListsByType.GIFT_CARD)?has_content>
            <#list paymentMethodListsByType.GIFT_CARD as paymentMethodLocal>
              <#assign paymentMethod = paymentMethodLocal><#-- Scipio: workaround for access from macros -->
              <#assign giftCard = paymentMethod.getRelatedOne("GiftCard", false) />
              <#assign methodShortInfo><@formattedGiftCardShort giftCard=giftCard paymentMethod=paymentMethod /></#assign>
              <#if showSupplemental>
                <#if showSelect>
                  <#assign dummy = registerFieldContent({"fieldId":"checkOutPaymentId_${paymentMethod.paymentMethodId}${primSupplSuffix}", "contentId":"content_${paymentMethod.paymentMethodId}${primSupplSuffix}"})>
                  <@field type="checkbox" id="checkOutPaymentId_${paymentMethod.paymentMethodId}${primSupplSuffix}" name="checkOutPaymentId" value=(paymentMethod.paymentMethodId) 
                    class="+pay-select-checkbox pay-select-field" checked=(selectedCheckOutPaymentIdList?seq_contains(paymentMethod.paymentMethodId)) 
                    label="${rawString(uiLabelMap.AccountingGiftCard)}: ${rawString(methodShortInfo)}" /><#--tooltip=(getPayMethTypeDesc("GIFT_CARD")!) -->
                </#if>
              </#if>
              <#if showDetails && showSupplemental>
                <@section containerId="content_${paymentMethod.paymentMethodId}${primSupplSuffix}" containerClass="+pay-meth-content" containerStyle="display:none;"><#--title=uiLabelMap.AccountingGiftCard -->
                  <@payMethInfoPanel title=uiLabelMap.AccountingGiftCard updateAction='EG'>
                    <@formattedGiftCardDetail giftCard=giftCard paymentMethod=paymentMethod />
                    <#if paymentMethod.get("description", locale)?has_content><br/>(${paymentMethod.get("description", locale)})</#if>
                  </@payMethInfoPanel>
                  <@payMethAmountField payMethId=paymentMethod.paymentMethodId/>
                </@section>
              </#if>
            </#list>
          </#if>

          <#-- Gift card not on file -->
          <#-- Scipio: NOTE: This was initially implemented in stock code and is not 1-for-1 equivalent to our new inline
              forms for CC && EFT above... 
              TODO?: Reimplement using _NEW_GIFT_CARD_ hook --> 
          <#if showSupplemental>
            <#if showSelect>
              <#-- MUST SUBMIT EMPTY VALUE FOR checkOutPaymentId -->
              <#assign dummy = registerFieldContent({"fieldId":"checkOutPaymentId_addGiftCard${primSupplSuffix}", "contentId":"content__NEW_GIFT_CARD_${primSupplSuffix}"})>
              <@field type="checkbox" id="checkOutPaymentId_addGiftCard${primSupplSuffix}" name="addGiftCard" value="Y" checked=((selectedCheckOutPaymentIdList?seq_contains("_NEW_GIFT_CARD_")) || ((newGiftCardParams.addGiftCard!) == "Y")) 
                class="+pay-select-checkbox pay-select-field" label=uiLabelMap.AccountingUseGiftCardNotOnFile /><#--tooltip=(getPayMethTypeDesc("GIFT_CARD")!) -->
            </#if>
          </#if>
          <#if showDetails && showSupplemental>
            <@section containerId="content__NEW_GIFT_CARD_${primSupplSuffix}" containerClass="+pay-meth-content" containerStyle="display:none;" title=uiLabelMap.AccountingGiftCard>
            <@fields type="inherit-all" fieldArgs={"gridArgs":{"totalLarge":8}}>
              <@field type="input" size="15" id="giftCardNumber${primSupplSuffix}" name="giftCardNumber" value=((newGiftCardParams.giftCardNumber)!) label=uiLabelMap.AccountingNumber
                tooltip="DemoCustomer: test: 123412341234 or 432143214321"/><#--onFocus="document.getElementById('addGiftCard').checked=true;"-->
              <#if cart.isPinRequiredForGC(delegator)>
                <@field type="input" size="10" id="giftCardPin${primSupplSuffix}" name="giftCardPin" value=((newGiftCardParams.giftCardPin)!) label=uiLabelMap.AccountingPIN/><#--onFocus="document.getElementById('addGiftCard').checked=true;"-->
              </#if>

              <#-- Scipio: Unhardcode the single-use flag so it follows our new inlines above
              <input type="hidden" name="singleUseGiftCard" value="Y" /> -->
              <#if userHasAccount>
                <#-- Scipio: This should be the opposite of "single use payment"... so we use form submit hook to set the hidden field -->
                <@field type="checkbox" checkboxType="simple-standard" id="newGiftCard_saveToAccount" name="saveToAccount__NEW_GIFT_CARD_" 
                    value="Y" checked=((singleUseGiftCard!"") != "Y") label=uiLabelMap.OrderSaveToAccount/>
                <input type="hidden" id="singleUseGiftCard" name="singleUseGiftCard" value="N" />
              </#if>
            </@fields>

              <@payMethAmountField type="new-pay-meth" id="giftCardAmount${primSupplSuffix}" name="giftCardAmount" params=newGiftCardParams/><#--onFocus="document.getElementById('addGiftCard').checked=true;"-->
            </@section>
          </#if>
        </#if>
      </@fields>
    </#macro>


  <#-- Scipio: Main structure
      NOTE: Some pay methods (EXT_BILLACT, GIFT_CARD) work as both main payments and as supplemental, so they reappear twice and have to be managed differently.
          Because we using radios, at least one must be selected by user otherwise breaks html expectations (stock code allowed selecting none; was sketchy) -->

<div id="paymethselection" class="pay-meth-selection">
  <@section containerId="paymeth_primary" containerClass="+pay-meth-options-all-content pay-meth-primary">
    <div id="paymethselect_primary" class="pay-meth-options">
      <#assign fieldLabel><strong>${uiLabelMap.AccountingPaymentMethod}</strong></#assign>
      <@field type="generic" label=fieldLabel required=true>
        <@paymentMethodContent showPrimary=true showSelect=true />
      </@field>
    </div>
    <@section containerId="paymethcontent_primary" containerClass="+pay-meth-all-content">
      <@paymentMethodContent showPrimary=true showDetails=true />
    </@section>
  </@section>

  <@section containerId="paymeth_supplemental" containerClass="+pay-meth-options-all-content pay-meth-supplemental"><#-- always show now: style="display:none;" -->
    <div id="paymethselect_supplemental" class="pay-meth-options">
      <#assign fieldLabel><strong>${uiLabelMap.AccountingAdditionalPaymentMethods}</strong></#assign>
      <@field type="generic" label=fieldLabel>
        <@paymentMethodContent showSupplemental=true showSelect=true />
      </@field>
    </div>
    <@section containerId="paymethcontent_supplemental" containerClass="+pay-meth-all-content">
      <@paymentMethodContent showSupplemental=true showDetails=true />
    </@section>
  </@section>
</div>
    
  </form>

  <#-- Scipio: Pay method visibility scripts (must be at end of file) -->

  <@script>
 
    <#-- Scipio: Enable JS-based content reveal for pay methods -->
    <@initItemSelectionWithContentFormScript itemFieldClass="pay-select-field" 
        contentItems=fieldContentIdMapList updateCallbackPostVisibJs=updateCallbackPostVisibJs />

    jQuery(document).ready(function() {
        var allPrimaryItems = getScipioFieldCheckElemsByClass('pay-select-radio');
        var allSupplItems = getScipioFieldCheckElemsByClass('pay-select-checkbox');
    
        <#-- Special case: if any of the supplemental payment method checkboxes are checked and no other radios
            are selected, then select the "additional payment methods" radio so the HTML makes sense -->  
        var updateAdditionalRadio = function() {
            if (jQuery(this).is(":checked")) {
                var radioChecked = false;
                allPrimaryItems.each(function(i, obj) {
                    if (jQuery(obj).is(":checked")) {
                        radioChecked = true;
                    }
                });
                if (!radioChecked) {
                    jQuery('#supplPayMeth').prop('checked', true);
                }
            }
        };
        allSupplItems.each(function(i, e) {
            updateAdditionalRadio.call(e);
        });
        allSupplItems.change(updateAdditionalRadio);
    });

  </@script>

</@section>

<#-- Scipio: Not this label, as there may be intermediate payment screens: uiLabelMap.OrderContinueToFinalOrderReview -->
<@checkoutActionsMenu directLinks=false formName="checkoutInfoForm" text=uiLabelMap.CommonContinue />

