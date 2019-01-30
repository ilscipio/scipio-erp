<#--
This file is subject to the terms and conditions defined in the
files 'LICENSE' and 'NOTICE', which are part of this source
code package.
-->
<#include "component://shop/webapp/shop/order/ordercommon.ftl">

<#-- SCIPIO: DEPRECATED TEMPLATE -->

<#if requestParameters.paymentMethodTypeId?has_content>
   <#assign paymentMethodTypeId = (requestParameters.paymentMethodTypeId!"")?string>
</#if>
<@script>

jQuery(document).ready(init);

function init() {
    getPaymentInformation();
    var paymentForm = document.setPaymentInformation;
}

function aroundSubmitOrder(invocation) {
    var formToSubmit = document.setPaymentInformation;
    var paymentMethodTypeOption = document.setPaymentInformation.paymentMethodTypeOptionList.options[document.setPaymentInformation.paymentMethodTypeOptionList.selectedIndex].value;
    if(paymentMethodTypeOption == "none"){
        document.setPaymentInformation.action = "<@pageUrl>quickAnonAddGiftCardToCart</@pageUrl>";
    }

    jQuery.ajax({
        url: formToSubmit.action,
        type: "POST",
        data: jQuery("#setPaymentInformation").serialize(),
        success: function(data) {
            if (paymentMethodTypeOption != "EXT_OFFLINE"){
                if(paymentMethodTypeOption == "none"){
                    document.getElementById("noPaymentMethodSelectedError").innerHTML = "${escapeVal(uiLabelMap.EcommerceMessagePleaseSelectPaymentMethod, 'js')}";
                } else {
                    document.getElementById("paymentInfoSection").innerHTML = data;
                }
            }
        }
    }).done(function() {
        processOrder();
    });
}

function getGCInfo() {
    if (document.setPaymentInformation.addGiftCard.checked) {
      jQuery.ajax({
          url: "<@pageUrl>quickAnonGcInfo</@pageUrl>",
          type: "POST",
          success: function(data) {
              document.getElementById("giftCardSection").innerHTML = data;
          }
      });
    } else {
        document.getElementById("giftCardSection").innerHTML = "";
    }
}

function getPaymentInformation() {
  document.getElementById("noPaymentMethodSelectedError").innerHTML = "";
  var paymentMethodTypeOption = document.setPaymentInformation.paymentMethodTypeOptionList.options[document.setPaymentInformation.paymentMethodTypeOptionList.selectedIndex].value;
  var connectionObject;
   if (paymentMethodTypeOption.length > 0){
      if(paymentMethodTypeOption == "CREDIT_CARD"){

        jQuery.ajax({
            url: "<@pageUrl>quickAnonCcInfo</@pageUrl>",
            type: "POST",
            success: function(data) {
                document.getElementById("paymentInfoSection").innerHTML = data;
            }
        });

        document.setPaymentInformation.paymentMethodTypeId.value = "CREDIT_CARD";
        document.setPaymentInformation.action = "<@pageUrl>quickAnonEnterCreditCard</@pageUrl>";
      } else if (paymentMethodTypeOption == "EFT_ACCOUNT"){

       jQuery.ajax({
            url: "<@pageUrl>quickAnonEftInfo</@pageUrl>",
            type: "POST",
            success: function(data) {
                document.getElementById("paymentInfoSection").innerHTML = data;
            }
        });

        document.setPaymentInformation.paymentMethodTypeId.value = "EFT_ACCOUNT";
        document.setPaymentInformation.action = "<@pageUrl>quickAnonEnterEftAccount</@pageUrl>";
      } else if (paymentMethodTypeOption == "EXT_OFFLINE"){
        document.setPaymentInformation.paymentMethodTypeId.value = "EXT_OFFLINE";
        document.getElementById("paymentInfoSection").innerHTML = "";
        document.setPaymentInformation.action = "<@pageUrl>quickAnonEnterExtOffline</@pageUrl>";
      } else {
        document.setPaymentInformation.paymentMethodTypeId.value = "none";
        document.getElementById("paymentInfoSection").innerHTML = "";
      }
   }
}
</@script>
<@section title=uiLabelMap.AccountingPaymentInformation>
    <form id="setPaymentInformation" method="post" action="<@pageUrl>quickAnonAddGiftCardToCart</@pageUrl>" name="setPaymentInformation">

      <#if (requestParameters.singleUsePayment!"N") == "Y">
        <input type="hidden" name="singleUsePayment" value="Y"/>
        <input type="hidden" name="appendPayment" value="Y"/>
      </#if>
      <input type="hidden" name="contactMechTypeId" value="POSTAL_ADDRESS"/>
      <input type="hidden" name="partyId" value="${partyId!}"/>
      <input type="hidden" name="paymentMethodTypeId" value="${paymentMethodTypeId!}"/>
      <input type="hidden" name="createNew" value="Y"/>
      <#assign billingContactMechId = sessionAttributes.billingContactMechId!><#-- SCIPIO: Access session only once -->
      <#if billingContactMechId?has_content>
        <input type="hidden" name="contactMechId" value="${billingContactMechId}"/>
      </#if>

      <div class="errorMessage" id="noPaymentMethodSelectedError"></div>
      
      <@field type="select" label=uiLabelMap.OrderSelectPaymentMethod name="paymentMethodTypeOptionList" onChange="javascript:getPaymentInformation();">
           <option value="none">Select One</option>
         <#if productStorePaymentMethodTypeIdMap.CREDIT_CARD??>
           <option value="CREDIT_CARD"<#if (parameters.paymentMethodTypeId!"") == "CREDIT_CARD"> selected="selected"</#if>>${uiLabelMap.AccountingVisaMastercardAmexDiscover}</option>
         </#if>
         <#if productStorePaymentMethodTypeIdMap.EFT_ACCOUNT??>
           <option value="EFT_ACCOUNT"<#if (parameters.paymentMethodTypeId!"") == "EFT_ACCOUNT"> selected="selected"</#if>>${uiLabelMap.AccountingAHCElectronicCheck}</option>
         </#if>
         <#if productStorePaymentMethodTypeIdMap.EXT_OFFLINE??>
           <option value="EXT_OFFLINE"<#if (parameters.paymentMethodTypeId!"") == "EXT_OFFLINE"> selected="selected"</#if>>${uiLabelMap.OrderPaymentOfflineCheckMoney}</option>
         </#if>
      </@field>
      <#-- SCIPIO: Loaded using javascript... -->
      <div id="paymentInfoSection"></div>
      
      <#--<hr />-->

        <#-- gift card fields -->
        <#if productStorePaymentMethodTypeIdMap.GIFT_CARD??>
          <@field type="checkbox" id="addGiftCard" name="addGiftCard" value="Y" onClick="javascript:getGCInfo();" label=uiLabelMap.AccountingCheckGiftCard />
          <#-- SCIPIO: Loaded using javascript... -->
          <div id="giftCardSection"></div>
        </#if>
    </form>
</@section>
