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
            //window.location.replace("<@ofbizUrl>processorder</@ofbizUrl>");
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
  <@checkoutActionsMenu directLinks=true>
    <form type="post" action="<@ofbizUrl>processorder</@ofbizUrl>" name="${parameters.formNameValue}">
      <#if (parameters.checkoutpage)?has_content><#-- SCIPIO: use parameters map for checkout page, so request attributes are considered: requestParameters.checkoutpage -->
        <input type="hidden" name="checkoutpage" value="${parameters.checkoutpage}" /><#-- ${requestParameters.checkoutpage} -->
      </#if>
      <#if (requestAttributes.issuerId)?has_content>
        <input type="hidden" name="issuerId" value="${requestAttributes.issuerId}" />
      </#if>
      <@field type="submit" submitType="input-button" inline=true name="processButton" text=uiLabelMap.OrderSubmitOrder onClick="processOrder();" class="${styles.link_run_sys!} ${styles.action_add!} ${styles.action_importance_high!}" />
    </form>
  </@checkoutActionsMenu>
<#else>
  <@commonMsg type="error">${uiLabelMap.OrderErrorShoppingCartEmpty}.</@commonMsg>
</#if>
