<#--
This file is subject to the terms and conditions defined in the
files 'LICENSE' and 'NOTICE', which are part of this source
code package.
-->
<#-- SCIPIO: WARN: 
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
This template is no longer used by shop. If core fixes are applied to this file,
they may need to be duplicated to:
  component://shop/webapp/shop/order/checkoutpayment.ftl
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
-->

<#include "component://order/webapp/ordermgr/common/common.ftl">
<#import "component://accounting/webapp/accounting/common/acctlib.ftl" as acctlib>
<#-- SCIPIO: TODO: convert template (maybe wait until after updates from branch) - this is not yet part of orderentry... -->
<#-- TODO : Need formatting -->
<@script>
function submitForm(form, mode, value) {
    if (mode == "DN") {
        // done action; checkout
        form.action="<@pageUrl>checkoutoptions</@pageUrl>";
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
    }
}
</@script>

 
<#assign cart = shoppingCart! />

<@section title="${rawLabel('AccountingPayment')}"><#-- SCIPIO: No numbers for multi-page checkouts, make checkout too rigid: 3) ${uiLabelMap.OrderHowShallYouPay}? -->
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
  
    <form method="post" id="checkoutsetupform" name="checkoutsetupform" action="<@pageUrl>finalizeOrder</@pageUrl>">
        <input type="hidden" name="checkoutpage" value="payment" />
        <input type="hidden" name="finalizeMode" value="payment"/>
        <input type="hidden" name="BACK_PAGE" value="checkoutoptions" />
        <input type="hidden" name="issuerId" id="issuerId" value="" />
        <#-- Payment Method Selection -->
        <div id="paymethselection" class="pay-meth-selection">
            <@section containerId="paymeth_primary" containerClass="+pay-meth-options-all-content pay-meth-primary">
                <div id="paymethselect_primary" class="pay-meth-options">
                  <#assign fieldLabel><strong>${uiLabelMap.AccountingPaymentMethod}</strong></#assign>
                  <@field type="generic" label=wrapAsRaw(fieldLabel, 'htmlmarkup') required=true>
                    <@orderlib.paymentMethodContent showPrimary=true showSelect=true cart=cart />
                  </@field>
                </div>
                <@section containerId="paymethcontent_primary" containerClass="+pay-meth-all-content">
                  <@orderlib.paymentMethodContent showPrimary=true showDetails=true cart=cart />
                </@section>
            </@section>
            <@section containerId="paymeth_supplemental" containerClass="+pay-meth-options-all-content pay-meth-supplemental"><#-- always show now: style="display:none;" -->
                <div id="paymethselect_supplemental" class="pay-meth-options">
                  <#assign fieldLabel><strong>${uiLabelMap.AccountingAdditionalPaymentMethods}</strong></#assign>
                  <@field type="generic" label=wrapAsRaw(fieldLabel, 'htmlmarkup')>
                    <@orderlib.paymentMethodContent showSupplemental=true showSelect=true cart=cart />
                  </@field>
                </div>
                <@section containerId="paymethcontent_supplemental" containerClass="+pay-meth-all-content">
                    <@orderlib.paymentMethodContent showSupplemental=true showDetails=true cart=cart />
                </@section>
            </@section>
        </div>
    </form>
</@section>

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

