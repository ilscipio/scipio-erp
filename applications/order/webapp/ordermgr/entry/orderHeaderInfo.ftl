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
<#assign currencyUomId = shoppingCart.getCurrency()>
<#assign partyId = shoppingCart.getPartyId()>
<#assign partyMap = Static["org.ofbiz.party.party.PartyWorker"].getPartyOtherValues(request, partyId, "party", "person", "partyGroup")>
<#assign agreementId = shoppingCart.getAgreementId()!>
<#assign quoteId = shoppingCart.getQuoteId()!>

<#if shoppingCart?has_content>
    <#assign shoppingCartSize = shoppingCart.size()>
<#else>
    <#assign shoppingCartSize = 0>
</#if>

<@section title=uiLabelMap.OrderOrderHeaderInfo>
        <@row>
            <@cell>
              <form method="post" action="setOrderName" name="setCartOrderNameForm">
                  <@field type="input" id="orderName" name="orderName" size="12" maxlength="200" value=shoppingCart.getOrderName()?default('') label=uiLabelMap.OrderOrderName/>
                  <@field type="submit" text=uiLabelMap.CommonSet class="+${styles.link_run_session!} ${styles.action_update!}" />
              </form>
            </@cell>
        </@row>
            <#if shoppingCart.getOrderType() != "PURCHASE_ORDER">
                <@row>
                    <@cell>
                    <form method="post" action="setPoNumber" name="setCartPoNumberForm">
                        <@field type="input" id="correspondingPoId" name="correspondingPoId" size="12" value=shoppingCart.getPoNumber()?default('') label=uiLabelMap.OrderPONumber/>
                        <@field type="submit" text=uiLabelMap.CommonSet class="+${styles.link_run_session!} ${styles.action_update!}" />
                    </form>
                    </@cell>
                </@row>
            </#if>
            
        <@table type="fields"> <#-- orig: class="basic-table" -->
            <@tr>
                <@td class="${styles.grid_large!}3">${uiLabelMap.Party}</@td>
                <@td>
                  <a href="${customerDetailLink}${partyId}${externalKeyParam!}" target="partymgr" class="${styles.link_nav_info_id!}">${partyId}</a>
                  <#if partyMap.person??>
                    ${partyMap.person.firstName!}&nbsp;${partyMap.person.lastName!}
                  </#if>
                  <#if partyMap.partyGroup??>
                    ${partyMap.partyGroup.groupName!}
                  </#if>
              </@td>
            </@tr>
            <@tr>
                <@td class="${styles.grid_large!}3">${uiLabelMap.CommonCurrency}</@td>
                <@td>
                  ${currencyUomId}
                </@td>
            </@tr>
            <#if agreementId?has_content>
            <@tr>
                <@td class="${styles.grid_large!}3">${uiLabelMap.AccountingAgreement}</@td>
                <@td>${agreementId}</@td>
            </@tr>
            </#if>
            <#if quoteId?has_content>
            <@tr>
                <@td class="${styles.grid_large!}3">${uiLabelMap.OrderOrderQuote}</@td>
                <@td>${quoteId}</@td>
            </@tr>
            </#if>
            <@tr>
                <@td class="${styles.grid_large!}3">${uiLabelMap.CommonTotal}</@td>
                <@td><@ofbizCurrency amount=shoppingCart.getGrandTotal() isoCode=currencyUomId/></@td>
            </@tr>
        </@table>
</@section>
