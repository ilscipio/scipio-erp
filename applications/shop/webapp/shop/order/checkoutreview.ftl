<#--
Scipio Commerce
Copyright (C) Ilscipio GmbH

This file is part of Scipio Commerce. Scipio Commerce is free software: you
can redistribute it and modify it under the terms of the GNU Affero General
Public License, version 3, as published by the Free Software Foundation.
Scipio Commerce is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License
for more details. You should have received a copy of the license with this
work (file LICENSE). If not, see <https://www.gnu.org/licenses/agpl-3.0.html>.
A commercial license is available from Ilscipio GmbH.

SPDX-License-Identifier: AGPL-3.0-only
-->
<#include "component://shop/webapp/shop/order/ordercommon.ftl">

<@script>
    var clicked = 0;
    function processOrder() {
        <#-- SCIPIO: 4.0.0: the form is submitted by script; check required fields (terms) first -->
        var scpForm = document["${escapeVal(parameters.formNameValue, 'js')}"];
        if (scpForm && scpForm.reportValidity && !scpForm.reportValidity()) {
            return;
        }
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

  <#if "EXT_STRIPE" == (paymentMethodType.paymentMethodTypeId)!>
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
      <#-- SCIPIO: 4.0.0: pre-contract information, terms and the EU order button label (compliance component) -->
      <#assign scpOrderButtonText = uiLabelMap.OrderSubmitOrder>
      <#if Static["org.ofbiz.base.component.ComponentConfig"].isComponentEnabled("compliance")>
        <#import "component://compliance/templates/shop/complianceLib.ftl" as compliance>
        <@compliance.checkoutBlock/>
        <#assign scpOrderButtonText = compliance.orderButtonText()>
      </#if>
      <@field type="submit" submitType="input-button" inline=true name="processButton" id="processButton" text=scpOrderButtonText onClick="processOrder();" class="${styles.link_run_sys!} ${styles.action_add!} ${styles.action_importance_high!}" />
    </form>
  </@checkoutActionsMenu>
<#else>
  <@commonMsg type="error">${uiLabelMap.OrderErrorShoppingCartEmpty}.</@commonMsg>
</#if>
