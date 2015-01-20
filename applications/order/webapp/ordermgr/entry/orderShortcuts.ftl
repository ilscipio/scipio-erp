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

<#assign shoppingCart = sessionAttributes.shoppingCart!>

<@section title="${uiLabelMap.OrderOrderShortcuts}">
        <ul class="button-group">
            <#if shoppingCart.getOrderType() == "PURCHASE_ORDER">
              <li><a href="<@ofbizUrl>RequirementsForSupplier</@ofbizUrl>" class="button tiny">${uiLabelMap.OrderRequirements}</a></li>
            </#if>
            <#if shoppingCart.getOrderType()?has_content && shoppingCart.items()?has_content>
              <li><a href="<@ofbizUrl>createQuoteFromCart?destroyCart=Y</@ofbizUrl>" class="button tiny">${uiLabelMap.OrderCreateQuoteFromCart}</a></li>
              <li><a href="<@ofbizUrl>FindQuoteForCart</@ofbizUrl>" class="button tiny">${uiLabelMap.OrderOrderQuotes}</a></li>
            </#if>
            <#if shoppingCart.getOrderType() == "SALES_ORDER">
              <li><a href="<@ofbizUrl>createCustRequestFromCart?destroyCart=Y</@ofbizUrl>" class="button tiny">${uiLabelMap.OrderCreateCustRequestFromCart}</a></li>
            </#if>
            <li><a href="/partymgr/control/findparty?${externalKeyParam!}" class="button tiny">${uiLabelMap.PartyFindParty}</a></li>
            <#if shoppingCart.getOrderType() == "SALES_ORDER">
              <li><a href="<@ofbizUrl>setCustomer</@ofbizUrl>" class="button tiny">${uiLabelMap.PartyCreateNewCustomer}</a></li>
            </#if>
            <li><a href="<@ofbizUrl>checkinits</@ofbizUrl>" class="button tiny">${uiLabelMap.PartyChangeParty}</a></li>
            <#if security.hasEntityPermission("CATALOG", "_CREATE", session)>
               <li><a href="/catalog/control/EditProduct?${externalKeyParam!}" target="catalog" class="button tiny">${uiLabelMap.ProductCreateNewProduct}</a></li>
            </#if>
            <li><a href="<@ofbizUrl>quickadd</@ofbizUrl>" class="button tiny">${uiLabelMap.OrderQuickAdd}</a></li>
            <#if shoppingLists??>
              <li><a href="<@ofbizUrl>viewPartyShoppingLists?partyId=${partyId}</@ofbizUrl>" class="button tiny">${uiLabelMap.PageTitleShoppingList}</a></li>
            </#if>
        </ul>
</@section>
