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
