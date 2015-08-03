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
<#--
<#assign title>
        ${uiLabelMap.CommonCreate}&nbsp;
        <#if shoppingCart.getOrderType() == "PURCHASE_ORDER">
            ${uiLabelMap.OrderPurchaseOrder}
        <#else>
        ${uiLabelMap.OrderOrder}
        </#if>
</#assign>-->


<@section>   
      <ul class="button-group">
      <#if shoppingCart.getOrderType() == "PURCHASE_ORDER">
        <#if shoppingCart.getOrderPartyId() == "_NA_" || (shoppingCart.size() = 0)>
          <li><a href="" class="${styles.button_default!} disabled">${uiLabelMap.OrderFinalizeOrder}</a></li>
        <#else>
          <li><a href="<@ofbizUrl>finalizeOrder?finalizeMode=purchase&amp;finalizeReqCustInfo=false&amp;finalizeReqShipInfo=false&amp;finalizeReqOptions=false&amp;finalizeReqPayInfo=false</@ofbizUrl>" class="${styles.button_default!}">${uiLabelMap.OrderFinalizeOrder}</a></li>
        </#if>
      <#else>
        <#if shoppingCart.size() = 0>
          <li><a href="" class="${styles.button_default!} disabled">${uiLabelMap.OrderQuickFinalizeOrder}</a></li>
          <li><a href="" class="${styles.button_default!} disabled">${uiLabelMap.OrderFinalizeOrderDefault}</a></li>
          <li><a href="" class="${styles.button_default!} disabled">${uiLabelMap.OrderFinalizeOrder}</a></li>
        <#else>
          <li><a href="<@ofbizUrl>quickcheckout</@ofbizUrl>" class="${styles.button_default!}">${uiLabelMap.OrderQuickFinalizeOrder}</a></li>
          <li><a href="<@ofbizUrl>finalizeOrder?finalizeMode=default</@ofbizUrl>" class="${styles.button_default!}">${uiLabelMap.OrderFinalizeOrderDefault}</a></li>
          <li><a href="<@ofbizUrl>finalizeOrder?finalizeMode=init</@ofbizUrl>" class="${styles.button_default!}">${uiLabelMap.OrderFinalizeOrder}</a></li>
        </#if>
      </#if>

      <#if (shoppingCart.size() > 0)>
        <li><a href="javascript:document.cartform.submit()" class="${styles.button_default!}">${uiLabelMap.OrderRecalculateOrder}</a></li>
        <li><a href="javascript:removeSelected();" class="${styles.button_default!}">${uiLabelMap.OrderRemoveSelected}</a></li>
      <#else>
        <li><a href="" class="${styles.button_default!} disabled">${uiLabelMap.OrderRecalculateOrder}</a></li>
        <li><a href="" class="${styles.button_default!} disabled">${uiLabelMap.OrderRemoveSelected}</a></li>
      </#if>
      <li><a href="<@ofbizUrl>emptycart</@ofbizUrl>" class="${styles.button_default!}">${uiLabelMap.OrderClearOrder}</a></li>
    </ul>
</@section>
