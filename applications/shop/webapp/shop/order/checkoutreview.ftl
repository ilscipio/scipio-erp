<#--
This file is subject to the terms and conditions defined in the
files 'LICENSE' and 'NOTICE', which are part of this source
code package.
-->
<#include "component://shop/webapp/shop/order/ordercommon.ftl">

<@script>
    var clicked = 0;
    function processOrder() {
        if (clicked == 0) {
            clicked++;
            //window.location.replace("<@pageUrl>processorder</@pageUrl>");
            document["${escapeVal(parameters.formNameValue, 'js')}"].processButton.value="${escapeVal(uiLabelMap.OrderSubmittingOrder, 'js')}";
            document["${escapeVal(parameters.formNameValue, 'js')}"].processButton.disabled=true;
            document["${escapeVal(parameters.formNameValue, 'js')}"].submit();
        } else {
            showErrorAlert("${escapeVal(uiLabelMap.CommonErrorMessage2, 'js')}","${escapeVal(uiLabelMap.YoureOrderIsBeingProcessed, 'js')}");
        }
    }
</@script>

<#if !isDemoStore?? || isDemoStore><@alert type="info">${uiLabelMap.OrderDemoFrontNote}.</@alert></#if>

<#if validPaymentMethodTypeForSubscriptions && subscriptions>
    <@alert type="warning">Your order contains subscriptions and each subscription payment through PayPal must be authorized separately. Therefore you can activate them once the order is created. <#if !orderContainsSubscriptionItemsOnly>The rest of items require just one authorization so you will be redirected to PayPal when the order gets submitted</#if></@alert>
</#if>

<#if cart?? && (0 < cart.size())>
  <@render resource="component://shop/widget/OrderScreens.xml#orderheader" />
  <@render resource="component://shop/widget/OrderScreens.xml#orderitems" />

  <#if paymentMethodType.paymentMethodTypeId == "EXT_STRIPE">
    <#assign pk = Static["com.ilscipio.scipio.accounting.payment.stripe.StripeHelper"].getPublishableKey(request)!>
    <#assign stripeIntegrationMode = Static["com.ilscipio.scipio.accounting.payment.stripe.StripeHelper"].getIntegrationMode(request)!>
    <#assign stripePaymentIntent = sessionAttributes[Static["com.ilscipio.scipio.accounting.payment.stripe.StripeHelper"].STRIPE_PAYMENT_INTENT]!>
    <@renderStripe mode=stripeIntegrationMode pk=pk! options=options! style=style! paymentIntentMap=stripePaymentIntent! checkoutFormId="orderreview" checkoutButtonId="processButton"
       hooks=hooks! multiStepCheckout={"finalize":true} debug=true/>
  </#if>

  <@checkoutActionsMenu directLinks=true>
    <form type="post" action="<@pageUrl>processorder</@pageUrl>" name="${parameters.formNameValue}" id="${parameters.formNameValue}">
      <#if (parameters.checkoutpage)?has_content><#-- SCIPIO: use parameters map for checkout page, so request attributes are considered: requestParameters.checkoutpage -->
        <input type="hidden" name="checkoutpage" value="${parameters.checkoutpage}" /><#-- ${requestParameters.checkoutpage} -->
      </#if>
      <#if (requestAttributes.issuerId)?has_content>
        <input type="hidden" name="issuerId" value="${requestAttributes.issuerId}" />
      </#if>
      <@field type="submit" submitType="input-button" inline=true name="processButton" id="processButton" text=uiLabelMap.OrderSubmitOrder onClick="processOrder();" class="${styles.link_run_sys!} ${styles.action_add!} ${styles.action_importance_high!}" />
    </form>
  </@checkoutActionsMenu>
<#else>
  <@commonMsg type="error">${uiLabelMap.OrderErrorShoppingCartEmpty}.</@commonMsg>
</#if>
