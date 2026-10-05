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

<#-- SCIPIO: DEPRECATED TEMPLATE -->

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

    function shippingMethodChanged(shippingMethod) {
        var submitToUri = "<@pageUrl>quickAnonProcessShipOptionsUpdateOrderItems</@pageUrl>?shipping_method=" + shippingMethod;
        jQuery.ajax({
            url: submitToUri,
            type: "POST",
            success: function(data) {
                document.getElementById("orderItemsSection").innerHTML = data;
            }
        });
    }
</@script>

<#if !isDemoStore?? || isDemoStore><@alert type="info">${uiLabelMap.OrderDemoFrontNote}.</@alert></#if>

<#if cart?? && (0 < cart.size())>
  <@render resource="component://shop/widget/OrderScreens.xml#quickAnonOrderHeader" />
  <br />
  <div id="orderItemsSection"><@render resource="component://shop/widget/OrderScreens.xml#orderitems" /></div>
  <form type="post" action="<@pageUrl>processorder</@pageUrl>" name="${parameters.formNameValue}">
    <@row>
      <@cell class="+${styles.text_right!}">
        <#if (parameters.checkoutpage)?has_content><#-- SCIPIO: use parameters map for checkout page, so request attributes are considered: requestParameters.checkoutpage -->
          <input type="hidden" name="checkoutpage" value="${parameters.checkoutpage}" /><#-- ${requestParameters.checkoutpage} -->
        </#if>
        <input type="button" id="submitOrderReview" name="processButton" value="${uiLabelMap.OrderSubmitOrder}" onclick="aroundSubmitOrder();" class="${styles.link_run_sys!} ${styles.action_add!} ${styles.action_importance_high!}" />
      </@cell>
    </@row>
  </form>
<#else>
  <@commonMsg type="error">${uiLabelMap.OrderErrorShoppingCartEmpty}.</@commonMsg>
</#if>
