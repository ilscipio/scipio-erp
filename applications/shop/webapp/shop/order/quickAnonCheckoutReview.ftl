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
<#include "ordercommon.ftl">

<#-- SCIPIO: DEPRECATED TEMPLATE -->

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

    function shippingMethodChanged(shippingMethod) {
        var submitToUri = "<@ofbizUrl>quickAnonProcessShipOptionsUpdateOrderItems</@ofbizUrl>?shipping_method=" + shippingMethod;
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
  <form type="post" action="<@ofbizUrl>processorder</@ofbizUrl>" name="${parameters.formNameValue}">
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
