<#--
This file is subject to the terms and conditions defined in the
files 'LICENSE' and 'NOTICE', which are part of this source
code package.
-->

<#-- SCIPIO: "Quick Finalize" / "One Page Checkout" template for order entry
    This is the nearest equivalent of shop OnePageCheckout for orderentry.
    NOTE: In the past this was shared by shop, but is no longer supported in shop in Scipio (OnePageCheckout superior). -->

<#include "component://order/webapp/ordermgr/common/common.ftl">
<#import "component://accounting/webapp/accounting/common/acctlib.ftl" as acctlib>

<@script>
function submitForm(form, mode, value) {
    if (mode == "DN") {
        // done action; checkout
        <#-- SCIPIO: NOTE: ?checkoutType=quick is what makes the review page show the "quick" top menu instead of the full one -->
        form.action="<@pageUrl>checkout?checkoutType=quick</@pageUrl>";
        form.submit();
    } else if (mode == "CS") {
        // continue shopping
        form.action="<@pageUrl>updateCheckoutOptions/showcart</@pageUrl>";
        form.submit();
    } else if (mode == "NA") {
        // new address
        form.action="<@pageUrl>updateCheckoutOptions/editcontactmech?DONE_PAGE=quickcheckout&partyId=${shoppingCart.getPartyId()}&preContactMechTypeId=POSTAL_ADDRESS&contactMechPurposeTypeId=SHIPPING_LOCATION</@pageUrl>";
        form.submit();
    } else if (mode == "EA") {
        // edit address
        form.action="<@pageUrl>updateCheckoutOptions/editcontactmech?DONE_PAGE=quickcheckout&partyId=${shoppingCart.getPartyId()}&contactMechId="+value+"</@pageUrl>";
        form.submit();
    } else if (mode == "NC") {
        // new credit card
        form.action="<@pageUrl>updateCheckoutOptions/editcreditcard?DONE_PAGE=quickcheckout&partyId=${shoppingCart.getPartyId()}</@pageUrl>";
        form.submit();
    } else if (mode == "EC") {
        // edit credit card
        form.action="<@pageUrl>updateCheckoutOptions/editcreditcard?DONE_PAGE=quickcheckout&partyId=${shoppingCart.getPartyId()}&paymentMethodId="+value+"</@pageUrl>";
        form.submit();
    } else if (mode == "GC") {
        // edit gift card
        form.action="<@pageUrl>updateCheckoutOptions/editgiftcard?DONE_PAGE=quickcheckout&partyId=${shoppingCart.getPartyId()}&paymentMethodId="+value+"</@pageUrl>";
        form.submit();
    } else if (mode == "NE") {
        // new eft account
        form.action="<@pageUrl>updateCheckoutOptions/editeftaccount?DONE_PAGE=quickcheckout&partyId=${shoppingCart.getPartyId()}</@pageUrl>";
        form.submit();
    } else if (mode == "EE") {
        // edit eft account
        form.action="<@pageUrl>updateCheckoutOptions/editeftaccount?DONE_PAGE=quickcheckout&partyId=${shoppingCart.getPartyId()}&paymentMethodId="+value+"</@pageUrl>";
        form.submit();
    } else if (mode == "SP") {
        // split payment
        form.action="<@pageUrl>updateCheckoutOptions/checkoutpayment?partyId=${shoppingCart.getPartyId()}</@pageUrl>";
        form.submit();
    } else if (mode == "SA") {
        // selected shipping address
        form.action="<@pageUrl>updateCheckoutOptions/quickcheckout</@pageUrl>";
        form.submit();
    } else if (mode == "SC") {
        // selected ship to party
        form.action="<@pageUrl>cartUpdateShipToCustomerParty</@pageUrl>";
        form.submit();
    }
}

function updateShippingMethod(shippingMethod) {
    if (shippingMethod == 'NO_SHIPPING') {
        $('.shipping-method-required').hide();
        $('#paymentMethodSection .paymentMethodSectionTitle [class^=heading-level-]').html("2) ${rawLabel('OrderHowShallYouPay')}?");
    } else {
        $('.shipping-method-required').show();
        $('#paymentMethodSection .paymentMethodSectionTitle [class^=heading-level-]').html("3) ${rawLabel('OrderHowShallYouPay')}?");
    }
}

$(document).ready(function() {
    var selectedShippingMethod = '${rawString(chosenShippingMethod!parameters.shipping_method)!''}';
    if (selectedShippingMethod) {
        shippingMethod = selectedShippingMethod.split('@');
        if (shippingMethod.length == 2) {
            updateShippingMethod(shippingMethod[0]);
        }
    }

});
</@script>

<#assign shipping = !shoppingCart.containAllWorkEffortCartItems()> <#-- contains items which need shipping? -->

<form method="post" name="checkoutInfoForm">
  <input type="hidden" name="checkoutpage" value="quick"/>
  <input type="hidden" name="BACK_PAGE" value="quickcheckout"/>

    <@section title=shipping?then("1) ${rawLabel('OrderHowShallWeShipIt')}?", "1) ${rawLabel('OrderOptions')}?")>
        <@fields type="default-manual">

            <@table type="fields" class="+${styles.table_spacing_tiny_hint!}" width="100%">
                <#if shipping == true>
                    <#assign chosenShippingMethod = raw(chosenShippingMethod!"N@A")>
                    <#list carrierShipmentMethodList as carrierShipmentMethod>
                        <#assign shippingMethod = raw(carrierShipmentMethod.shipmentMethodTypeId) + "@" + raw(carrierShipmentMethod.partyId)>
                        <@tr>
                            <@td width="1%">
                                <@field type="radio" name="shipping_method" value=shippingMethod checked=(shippingMethod == chosenShippingMethod) onClick="javascript:updateShippingMethod('${carrierShipmentMethod.shipmentMethodTypeId}');"/>
                            </@td>
                            <@td>
                                <#assign shippingEst = ""><#-- SCIPIO -->
                                <#if shoppingCart.getShippingContactMechId()??>
                                    <#assign shippingEst = shippingEstWpr.getShippingEstimate(carrierShipmentMethod)!-1>
                                </#if>
                                <#if carrierShipmentMethod.partyId != "_NA_">${carrierShipmentMethod.partyId!}&nbsp;</#if>${carrierShipmentMethod.description!}
                                <#if shippingEst?has_content><#if (shippingEst > -1)> - <@ofbizCurrency amount=shippingEst isoCode=shoppingCart.getCurrency()/><#elseif raw(carrierShipmentMethod.shipmentMethodTypeId!) != "NO_SHIPPING"> - ${uiLabelMap.OrderCalculatedOffline}</#if></#if><#-- SCIPIO: NO_SHIPPING check -->
                            </@td>
                        </@tr>
                    </#list>
                    <#if !carrierShipmentMethodList?? || carrierShipmentMethodList?size == 0>
                        <@tr class="shipping-method-required">
                            <@td width="1%">
                                <@field type="radio" name="shipping_method" value="Default" checked=true/>
                            </@td>
                            <@td>${uiLabelMap.OrderUseDefault}.</@td>
                        </@tr>
                    </#if>
                        <#--<@tr type="util"><@td colspan="2"><hr /></@td></@tr>-->
                    <@tr class="shipping-method-required">
                        <@td colspan="2">
                            <@heading>${uiLabelMap.OrderShipAllAtOnce}?</@heading>
                        </@td>
                    </@tr>
                    <@tr class="shipping-method-required">
                        <@td>
                            <@field type="radio" checked=((shoppingCart.getMaySplit()!"N") == "N") name="may_split" value="false"/>
                        </@td>
                        <@td>${uiLabelMap.OrderPleaseWaitUntilBeforeShipping}.</@td>
                    </@tr>
                    <@tr class="shipping-method-required">
                        <@td>
                            <@field type="radio" name="may_split" value="true" checked=((shoppingCart.getMaySplit()!"N") == "Y")/>
                        </@td>
                        <@td>${uiLabelMap.OrderPleaseShipItemsBecomeAvailable}.</@td>
                    </@tr>
                        <#--<@tr type="util"><@td colspan="2"><hr /></@td></@tr>-->
                <#else>
                    <input type="hidden" name="shipping_method" value="NO_SHIPPING@_NA_"/>
                    <input type="hidden" name="may_split" value="false"/>
                    <input type="hidden" name="is_gift" value="false"/>
                </#if>
                <@tr>
                    <@td colspan="2">
                        <@heading>${uiLabelMap.OrderSpecialInstructions}</@heading>
                    </@td>
                </@tr>
                <@tr>
                    <@td colspan="2">
                        <@field type="textarea" cols="30" rows="3" wrap="hard" name="shipping_instructions">${shoppingCart.getShippingInstructions()!}</@field>
                    </@td>
                </@tr>
                <#if shipping == true>
                    <#if (productStore.showCheckoutGiftOptions!) != "N" && (giftEnable!) != "N">
                            <#--<@tr type="util"><@td colspan="2"><hr /></@td></@tr>-->
                        <@tr>
                            <@td colspan="2">
                                <span><b>${uiLabelMap.OrderIsThisGift}</b></span>
                                <@field type="radio" checked=((shoppingCart.getIsGift()!"Y") == "Y") name="is_gift" value="true" label=uiLabelMap.CommonYes />
                                <@field type="radio" checked=((shoppingCart.getIsGift()!"N") == "N") name="is_gift" value="false" label=uiLabelMap.CommonNo />
                            </@td>
                        </@tr>
                            <#--<@tr type="util"><@td colspan="2"><hr /></@td></@tr>-->
                        <@tr>
                            <@td colspan="2">
                                <@heading>${uiLabelMap.OrderGiftMessage}</@heading>
                            </@td>
                        </@tr>
                        <@tr>
                            <@td colspan="2">
                                <@field type="textarea" cols="30" rows="3" wrap="hard" name="gift_message">${shoppingCart.getGiftMessage()!}</@field>
                            </@td>
                        </@tr>
                    <#else>
                        <input type="hidden" name="is_gift" value="false"/>
                    </#if>
                </#if>
                    <#--<@tr type="util"><@td colspan="2"><hr /></@td></@tr>-->
                <@tr>
                    <@td colspan="2">
                        <@heading>${uiLabelMap.PartyEmailAddresses}</@heading>
                    </@td>
                </@tr>
                <@tr>
                    <@td colspan="2">
                        <div>${uiLabelMap.OrderEmailSentToFollowingAddresses}:</div>
                        <div>
                            <b>
                                <#list emailList as email>
                                        ${email.infoString!}<#if email_has_next>,</#if>
                                </#list>
                            </b>
                        </div>
                        <div>${uiLabelMap.OrderUpdateEmailAddress} <a href="<#if customerDetailLink??>${customerDetailLink}${shoppingCart.getPartyId()}${raw(externalKeyParam!)}" target="partymgr"
                                                                                                                                                                                  <#else><@pageUrl>viewprofile?DONE_PAGE=quickcheckout</@pageUrl>"</#if> class="${styles.link_nav!}">${uiLabelMap.PartyProfile}</a>.</div>
                        <br />
                        <div>${uiLabelMap.OrderCommaSeperatedEmailAddresses}:</div>
                        <@field type="input" size="30" name="order_additional_emails" value=(shoppingCart.getOrderAdditionalEmails()!)/>
                    </@td>
                </@tr>
            </@table>
        </@fields>
    </@section>


    <@section title=shipping?then("2) ${rawLabel('OrderWhereShallWeShipIt')}?", rawLabel('OrderInformationAboutYou')) class="shipping-method-required">
        <@fields type="default-manual">
            <#-- SCIPIO: TODO: convert tables -->
                <@table type="fields" class="+${styles.table_spacing_tiny_hint!}" width="100%">
                  <@tr>
                    <@td colspan="2">
                      <span>${uiLabelMap.OrderShipToParty}:</span>
                      <@field type="select" name="shipToCustomerPartyId" onChange="javascript:submitForm(document.checkoutInfoForm, 'SC', null);">
                          <#list cartParties as cartParty>
                          <option value="${cartParty}">${cartParty}</option>
                          </#list>
                      </@field>
                    </@td>
                  </@tr>
                  <@tr>
                    <@td colspan="2">
                      <span>${uiLabelMap.CommonAdd}:</span>
                      <a href="javascript:submitForm(document.checkoutInfoForm, 'NA', '');" class="${styles.link_nav!} ${styles.action_add!}">${uiLabelMap.PartyAddNewAddress}</a>
                    </@td>
                  </@tr>
                  <#if (shoppingCart.getTotalQuantity() > 1) && !shoppingCart.containAllWorkEffortCartItems()> <#-- no splitting when only rental items -->
                    <#--<@tr type="util"><@td colspan="2"><hr /></@td></@tr>-->
                    <@tr>
                      <@td colspan="2" align="center">
                        <a href="<@pageUrl>splitship</@pageUrl>" class="${styles.link_nav!} ${styles.action_update!}">${uiLabelMap.OrderSplitIntoMultipleShipments}</a>
                        <#if (shoppingCart.getShipGroupSize() > 1)>
                          <div class="${styles.text_color_alert!}">${uiLabelMap.OrderNOTEMultipleShipmentsExist}.</div>
                        </#if>
                      </@td>
                    </@tr>
                  </#if>
                   <#if shippingContactMechList?has_content>
                     <#--<@tr type="util"><@td colspan="2"><hr /></@td></@tr>-->
                     <#list shippingContactMechList as shippingContactMech>
                       <#assign shippingAddress = shippingContactMech.getRelatedOne("PostalAddress", false)>
                       <@tr>
                         <@td width="1%">
                           <@field type="radio" name="shipping_contact_mech_id" value=shippingAddress.contactMechId onClick="javascript:submitForm(document.checkoutInfoForm, 'SA', null);" checked=((shoppingCart.getShippingContactMechId()!"") == shippingAddress.contactMechId)/>
                         </@td>
                         <@td width="99%">
                             <#if shippingAddress.toName?has_content><b>${uiLabelMap.CommonTo}:</b>&nbsp;${shippingAddress.toName}<br /></#if>
                             <#if shippingAddress.attnName?has_content><b>${uiLabelMap.PartyAddrAttnName}:</b>&nbsp;${shippingAddress.attnName}<br /></#if>
                             <#if shippingAddress.address1?has_content>${shippingAddress.address1}<br /></#if>
                             <#if shippingAddress.address2?has_content>${shippingAddress.address2}<br /></#if>
                             <#if shippingAddress.city?has_content>${shippingAddress.city}</#if>
                             <#if shippingAddress.stateProvinceGeoId?has_content><br />${shippingAddress.stateProvinceGeoId}</#if>
                             <#if shippingAddress.postalCode?has_content><br />${shippingAddress.postalCode}</#if>
                             <#if shippingAddress.countryGeoId?has_content><br />${shippingAddress.countryGeoId}</#if>
                             <a href="javascript:submitForm(document.checkoutInfoForm, 'EA', '${escapeVal(shippingAddress.contactMechId, 'js-html')}');" class="${styles.link_run_session!} ${styles.action_update!}">${uiLabelMap.CommonUpdate}</a>
                           </@td>
                       </@tr>
                       <#if shippingContactMech_has_next>
                         <#--<@tr type="util"><@td colspan="2"><hr /></@td></@tr>-->
                       </#if>
                     </#list>
                   </#if>
                 </@table>

                <#-- Party Tax Info -->
                <#-- commented out by default because the TaxAuthority drop-down is just too wide...
                <hr />
                <div>&nbsp;${uiLabelMap.PartyTaxIdentification}</div>
                <@render resource="component://order/widget/ordermgr/OrderEntryOrderScreens.xml#customertaxinfo" />
                -->
        </@fields>
    </@section>



    <@section title="3) ${rawLabel('OrderHowShallYouPay')}?" id="paymentMethodSection" titleContainerClass="paymentMethodSectionTitle">
        <#assign subFieldType = "default"><#-- SCIPIO: for sub-fields --><#-- "default-compact" -->

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
        <div id="paymethselection" class="pay-meth-selection">
            <@section containerId="paymeth_primary" containerClass="+pay-meth-options-all-content pay-meth-primary">
                <div id="paymethselect_primary" class="pay-meth-options">
                  <#assign fieldLabel><strong>${uiLabelMap.AccountingPaymentMethod}</strong></#assign>
                  <@field type="generic" label=wrapAsRaw(fieldLabel, 'htmlmarkup') required=true>
                    <@orderlib.paymentMethodContent showPrimary=true showSelect=true cart=shoppingCart />
                  </@field>
                </div>
                <@section containerId="paymethcontent_primary" containerClass="+pay-meth-all-content">
                  <@orderlib.paymentMethodContent showPrimary=true showDetails=true cart=shoppingCart />
                </@section>
            </@section>
            <@section containerId="paymeth_supplemental" containerClass="+pay-meth-options-all-content pay-meth-supplemental"><#-- always show now: style="display:none;" -->
                <div id="paymethselect_supplemental" class="pay-meth-options">
                  <#assign fieldLabel><strong>${uiLabelMap.AccountingAdditionalPaymentMethods}</strong></#assign>
                  <@field type="generic" label=wrapAsRaw(fieldLabel, 'htmlmarkup')>
                    <@orderlib.paymentMethodContent showSupplemental=true showSelect=true cart=shoppingCart />
                  </@field>
                </div>
                <@section containerId="paymethcontent_supplemental" containerClass="+pay-meth-all-content">
                    <@orderlib.paymentMethodContent showSupplemental=true showDetails=true cart=shoppingCart />
                </@section>
            </@section>
        </div>
    </@section>
</form>

<#-- SCIPIO: NOTE: we still need this because the page is long so it's too annoying for user to scroll way back up to click "continue" -->
<@row>
  <@cell>
    <@menu type="button">
      <#-- SCIPIO: I don't like this view-override
      <@menuitem type="link" href="javascript:submitForm(document.checkoutInfoForm, 'CS', '');" text=uiLabelMap.OrderBacktoShoppingCart class="+${styles.action_nav!} ${styles.action_cancel!}" />-->
      <@menuitem type="link" href="javascript:submitForm(document.checkoutInfoForm, 'DN', '');" text=uiLabelMap.CommonContinue class="+${styles.action_run_session!} ${styles.action_continue!}" /><#-- OrderContinueToFinalOrderReview -->
    </@menu>
  </@cell>
</@row>

<@script>
    <#-- SCIPIO: Enable JS-based content reveal for pay methods -->
    <@orderlib.initItemSelectionWithContentFormScript itemFieldClass="pay-select-field" 
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

