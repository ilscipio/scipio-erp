<#--
This file is subject to the terms and conditions defined in the
files 'LICENSE' and 'NOTICE', which are part of this source
code package.
-->
<#include "ordercommon.ftl">

<#-- SCIPIO: Must use context or accessor
<#assign shoppingCart = sessionAttributes.shoppingCart!>-->
<#assign shoppingCart = getShoppingCart()!>
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
            
        <@table type="fields">
            <@tr>
                <@td class="${styles.grid_large!}3">${uiLabelMap.Party}</@td>
                <@td>
                  <a href="${customerDetailLink}${partyId}${rawString(externalKeyParam!)}" target="partymgr" class="${styles.link_nav_info_id!}">${partyId}</a>
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
