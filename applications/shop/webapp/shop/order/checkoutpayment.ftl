<#--
This file is subject to the terms and conditions defined in the
files 'LICENSE' and 'NOTICE', which are part of this source
code package.
-->
<#-- SCIPIO: Duplicated (forcefully) from component://order/webapp/ordermgr/entry/checkoutpayment.ftl -->

<#-- TODO: PAYPAL, IDEAL, WORLDPAY -->
<#-- TODO: SHOW GIFT CARD BALANCE -->
<#-- TODO?: Amount boxes could pre-calculate and adjust automatically, but it's not clear if the leave-empty
    logic is exactly replicable in JS (must be exact) or might require AJAX calls -->

<#include "component://shop/webapp/shop/order/ordercommon.ftl">

<#-- SCIPIO: TODO: convert template (maybe wait until after updates from branch) - this is not yet part of orderentry... -->
<#-- TODO : Need formatting -->
<@script>
function submitForm(form, mode, value) {
    if (mode == "DN") {
        // done action; checkout
        form.action="<@pageUrl>checkoutoptions</@pageUrl>";
        
        <#-- SCIPIO: must process some checkboxes -->
        if (jQuery('#newCreditCard_saveToAccount').is(":checked")) {
            jQuery('#singleUsePayment__NEW_CREDIT_CARD_').val("N");
        } else {
            jQuery('#singleUsePayment__NEW_CREDIT_CARD_').val("Y");
        }
        
        <#-- SCIPIO: must process some checkboxes -->
        if (jQuery('#newEftAccount_saveToAccount').is(":checked")) {
            jQuery('#singleUsePayment__NEW_EFT_ACCOUNT_').val("N");
        } else {
            jQuery('#singleUsePayment__NEW_EFT_ACCOUNT_').val("Y");
        }
        
        <#-- SCIPIO: must process some checkboxes -->
        if (jQuery('#newGiftCard_saveToAccount').is(":checked")) {
            jQuery('#singleUseGiftCard').val("N");
        } else {
            jQuery('#singleUseGiftCard').val("Y");
        }
        
        form.submit();
    } else if (mode == "CS") {
        // continue shopping
        form.action="<@pageUrl>updateCheckoutOptions/showcart</@pageUrl>";
        form.submit();
    } else if (mode == "NC") {
        // new credit card
        form.action="<@pageUrl>updateCheckoutOptions/editcreditcard?DONE_PAGE=checkoutpayment</@pageUrl>";
        form.submit();
    } else if (mode == "EC") {
        // edit credit card
        form.action="<@pageUrl>updateCheckoutOptions/editcreditcard?DONE_PAGE=checkoutpayment&paymentMethodId="+value+"</@pageUrl>";
        form.submit();
    } else if (mode == "GC") {
        // edit gift card
        form.action="<@pageUrl>updateCheckoutOptions/editgiftcard?paymentMethodId="+value+"</@pageUrl>";
        form.submit();
    } else if (mode == "NE") {
        // new eft account
        form.action="<@pageUrl>updateCheckoutOptions/editeftaccount?DONE_PAGE=checkoutpayment</@pageUrl>";
        form.submit();
    } else if (mode == "EE") {
        // edit eft account
        form.action="<@pageUrl>updateCheckoutOptions/editeftaccount?DONE_PAGE=checkoutpayment&paymentMethodId="+value+"</@pageUrl>";
        form.submit();
    } else if (mode == "EG") {
        // edit gift card
        form.action="<@pageUrl>updateCheckoutOptions/editgiftcard?DONE_PAGE=checkoutpayment&paymentMethodId="+value+"</@pageUrl>";
        form.submit();
    } else if (mode == "EA") { <#-- SCIPIO: new -->
        // edit address
        form.action="<@pageUrl>updateCheckoutOptions/editcontactmech?DONE_PAGE=checkoutpayment&contactMechId="+value+"</@pageUrl>";
        form.submit();
    }
}

<#-- SCIPIO: now covered by script further below 
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

<#-- SCIPIO: We need to show total because this influence user's decision and amounts he will enter -->
<p><strong>${uiLabelMap.OrderOrderTotal}:</strong> <@ofbizCurrency amount=((cart.getDisplayGrandTotal())!0) isoCode=cart.getCurrency()/></p>

<#macro menuContent menuArgs={}>
  <@menu args=menuArgs>
<#-- SCIPIO: all deprecated
  <#if productStorePaymentMethodTypeIdMap.CREDIT_CARD??>
    <@menuitem type="link" href="javascript:submitForm(document.getElementById('checkoutInfoForm'), 'NC', '');" class="+${styles.action_nav!} ${styles.action_add!}" text="${rawLabel('CommonAdd')} ${rawLabel('AccountingCreditCard')}" />
  </#if>
  <#if productStorePaymentMethodTypeIdMap.EFT_ACCOUNT??>
    <@menuitem type="link" href="javascript:submitForm(document.getElementById('checkoutInfoForm'), 'NE', '');" class="+${styles.action_nav!} ${styles.action_add!}" text="${rawLabel('CommonAdd')} ${rawLabel('AccountingEFTAccount')}" />
  </#if>
  <#if productStorePaymentMethodTypeIdMap.CREDIT_CARD??><@menuitem type="link" href=makePageUrl("setBilling?paymentMethodType=CC&singleUsePayment=Y") class="+${styles.action_run_session!} ${styles.action_update!}" text=uiLabelMap.AccountingSingleUseCreditCard /></#if>
  <#if productStorePaymentMethodTypeIdMap.GIFT_CARD??><@menuitem type="link"  href=makePageUrl("setBilling?paymentMethodType=GC&singleUsePayment=Y") class="+${styles.action_run_session!} ${styles.action_update!}" text=uiLabelMap.AccountingSingleUseGiftCard /></#if>
  <#if productStorePaymentMethodTypeIdMap.EFT_ACCOUNT??><@menuitem type="link" href=makePageUrl("setBilling?paymentMethodType=EFT&singleUsePayment=Y") class="+${styles.action_run_session!} ${styles.action_update!}" text=uiLabelMap.AccountingSingleUseEFTAccount /></#if>
-->
  </@menu>
</#macro>
<@section title="${rawLabel('OrderHowShallYouPay')}?" menuContent=menuContent menuLayoutGeneral="bottom"><#-- SCIPIO: No numbers for multi-page checkouts, make checkout too rigid: 3) ${uiLabelMap.OrderHowShallYouPay}? -->
  <#-- SCIPIO: allow remember form filled via parameters first, over stored -->
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



  <#-- SCIPIO: Main structure
      NOTE: Some pay methods (EXT_BILLACT, GIFT_CARD) work as both main payments and as supplemental, so they reappear twice and have to be managed differently.
          Because we using radios, at least one must be selected by user otherwise breaks html expectations (stock code allowed selecting none; was sketchy) -->

    <div id="paymethselection" class="pay-meth-selection">
      <@section containerId="paymeth_primary" containerClass="+pay-meth-options-all-content pay-meth-primary">
        <div id="paymethselect_primary" class="pay-meth-options">
          <#assign fieldLabel><strong>${uiLabelMap.AccountingPaymentMethod}</strong></#assign>
          <@field type="generic" label=wrapAsRaw(fieldLabel, 'htmlmarkup') required=true>
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
          <@field type="generic" label=wrapAsRaw(fieldLabel, 'htmlmarkup')>
            <@paymentMethodContent showSupplemental=true showSelect=true />
          </@field>
        </div>
        <@section containerId="paymethcontent_supplemental" containerClass="+pay-meth-all-content">
          <@paymentMethodContent showSupplemental=true showDetails=true />
        </@section>
      </@section>
    </div>
  </form>
  <#-- SCIPIO: Pay method visibility scripts (must be at end of file) -->
  <@script>
 
    <#-- SCIPIO: Enable JS-based content reveal for pay methods -->
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

<#-- SCIPIO: Not this label, as there may be intermediate payment screens: uiLabelMap.OrderContinueToFinalOrderReview -->
<@checkoutActionsMenu directLinks=false formName="checkoutInfoForm" text=uiLabelMap.CommonContinue />

