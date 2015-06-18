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
          <li><a href="" class="button tiny disabled">${uiLabelMap.OrderFinalizeOrder}</a></li>
        <#else>
          <li><a href="<@ofbizUrl>finalizeOrder?finalizeMode=purchase&amp;finalizeReqCustInfo=false&amp;finalizeReqShipInfo=false&amp;finalizeReqOptions=false&amp;finalizeReqPayInfo=false</@ofbizUrl>" class="button tiny">${uiLabelMap.OrderFinalizeOrder}</a></li>
        </#if>
      <#else>
        <#if shoppingCart.size() = 0>
          <li><a href="" class="button tiny disabled">${uiLabelMap.OrderQuickFinalizeOrder}</a></li>
          <li><a href="" class="button tiny disabled">${uiLabelMap.OrderFinalizeOrderDefault}</a></li>
          <li><a href="" class="button tiny disabled">${uiLabelMap.OrderFinalizeOrder}</a></li>
        <#else>
          <li><a href="<@ofbizUrl>quickcheckout</@ofbizUrl>" class="button tiny">${uiLabelMap.OrderQuickFinalizeOrder}</a></li>
          <li><a href="<@ofbizUrl>finalizeOrder?finalizeMode=default</@ofbizUrl>" class="button tiny">${uiLabelMap.OrderFinalizeOrderDefault}</a></li>
          <li><a href="<@ofbizUrl>finalizeOrder?finalizeMode=init</@ofbizUrl>" class="button tiny">${uiLabelMap.OrderFinalizeOrder}</a></li>
        </#if>
      </#if>

      <#if (shoppingCart.size() > 0)>
        <li><a href="javascript:document.cartform.submit()" class="button tiny">${uiLabelMap.OrderRecalculateOrder}</a></li>
        <li><a href="javascript:removeSelected();" class="button tiny">${uiLabelMap.OrderRemoveSelected}</a></li>
      <#else>
        <li><a href="" class="button tiny disabled">${uiLabelMap.OrderRecalculateOrder}</a></li>
        <li><a href="" class="button tiny disabled">${uiLabelMap.OrderRemoveSelected}</a></li>
      </#if>
      <li><a href="<@ofbizUrl>emptycart</@ofbizUrl>" class="button tiny">${uiLabelMap.OrderClearOrder}</a></li>
    </ul>
</@section>
