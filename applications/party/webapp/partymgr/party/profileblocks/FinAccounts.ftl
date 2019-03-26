<#--
This file is subject to the terms and conditions defined in the
files 'LICENSE' and 'NOTICE', which are part of this source
code package.
-->

<#-- SCIPIO: prefix to prevent duplicate IDs -->
<#assign accountBaseId = accountBaseId!ownedFinAccount.finAccountId!"">

<div style="border-bottom: 1px solid #ccc; margin-bottom: 20px">
    <p>
        <b>${uiLabelMap.AccountingAccountNumber}:</b> <a href="<@serverUrl>/accounting/control/EditFinAccount?finAccountId=${ownedFinAccount.finAccountId}${raw(externalKeyParam)}</@serverUrl>"
             class="${styles.link_nav_info_id!}">${ownedFinAccount.finAccountId}</a>
        <b>${uiLabelMap.AccountingAccountType}:</b> ${(ownedFinAccountType.description)!(uiLabelMap.CommonNA)}
        <b>${uiLabelMap.FormFieldTitle_finAccountName}:</b> ${ownedFinAccount.finAccountName!}
    </p>
    <p>
        <b>${uiLabelMap.AccountingCurrency}:</b> ${(accountCurrencyUom.description)!} [${ownedFinAccount.currencyUomId!}]
        <b>${uiLabelMap.AccountingDateOpened}:</b> ${ownedFinAccount.fromDate!}
        <b>${uiLabelMap.CommonStatus}:</b> ${(finAccountStatusItem.description)!"Active"}
        <#if ownedFinAccount.replenishLevel??>
            <b>${uiLabelMap.FormFieldTitle_replenishLevel}:</b> <@ofbizCurrency amount=ownedFinAccount.replenishLevel isoCode=ownedFinAccount.currencyUomId/>
        </#if>
    </p>

    <@table type="data-list" class="+${styles.table_spacing_tiny_hint!}" id=(accountBaseId+"-fa-transactions")>
        <@thead>
            <@tr class="header-row">
                <@th>${uiLabelMap.FormFieldTitle_transactionDate}</@th>
                <@th>${uiLabelMap.CommonId}</@th>
                <@th>${uiLabelMap.OrderItem}</@th>
                <@th>${uiLabelMap.AccountingPayment}</@th>
                <@th>${uiLabelMap.AccountingType}</@th>
                <@th>${uiLabelMap.AccountingAmount}</@th>
            </@tr>
        </@thead>
        <@tbody>
            <#list ownedFinAccountTransList as ownedFinAccountTrans>
                <#assign finAccountTransType = ownedFinAccountTrans.getRelatedOne('FinAccountTransType', false)>
                <#assign displayAmount = ownedFinAccountTrans.amount>
                <#if ownedFinAccountTrans.finAccountTransTypeId == 'WITHDRAWAL'>
                    <#assign displayAmount = -displayAmount>
                </#if>
                <@tr>
                    <@td>${ownedFinAccountTrans.transactionDate!}</@td>
                    <@td>${ownedFinAccountTrans.finAccountTransId}</@td>
                    <@td>${ownedFinAccountTrans.orderId!}:${ownedFinAccountTrans.orderItemSeqId!}</@td>
                    <@td>${ownedFinAccountTrans.paymentId!}</@td>
                    <@td>${finAccountTransType.description?default(ownedFinAccountTrans.finAccountTransTypeId)!}</@td>
                    <@td><@ofbizCurrency amount=displayAmount isoCode=ownedFinAccount.currencyUomId/></@td>
                </@tr>
            </#list>
        </@tbody>
        <@tfoot>
            <#--<@tr type="util"><@td colspan="6"><hr /></@td></@tr>-->
            <@tr>
                <@td colspan="5"><b>${uiLabelMap.FormFieldTitle_actualBalance}</b></@td>
                <@td><b><@ofbizCurrency amount=ownedFinAccount.actualBalance isoCode=ownedFinAccount.currencyUomId/></b></@td>
            </@tr>
        </@tfoot>
    </@table>
</div>

<#if ownedFinAccountAuthList?has_content>
    <div style="border-bottom: 1px solid #ccc; margin-bottom: 20px">
        <@table type="data-list" class="+${styles.table_spacing_tiny_hint!}" id=(accountBaseId+"-fa-authorizations")>
            <@thead>
                <@tr class="header-row">
                    <@th>${uiLabelMap.FormFieldTitle_authorizationDate}</@th>
                    <@th>${uiLabelMap.CommonId}</@th>
                    <@th>${uiLabelMap.CommonExpires}</@th>
                    <@th>${uiLabelMap.AccountingAmount}</@th>
                </@tr>
            </@thead>
            <@tbody>
                <#list ownedFinAccountAuthList as ownedFinAccountAuth>
                    <@tr>
                        <@td>${ownedFinAccountAuth.authorizationDate!}</@td>
                        <@td>${ownedFinAccountAuth.finAccountAuthId}</@td>
                        <@td>${ownedFinAccountAuth.thruDate!}</@td>
                        <@td><@ofbizCurrency amount=-ownedFinAccountAuth.amount isoCode=ownedFinAccount.currencyUomId/></@td>
                    </@tr>
                </#list>
            </@tbody>
            <@tfoot>
                <#--<@tr type="util"><@td colspan="4"><hr /></@td></@tr>-->
                <@tr>
                    <@td colspan="3"><b>${uiLabelMap.FormFieldTitle_actualBalance}</b></@td>
                    <@td><b><@ofbizCurrency amount=ownedFinAccount.actualBalance isoCode=ownedFinAccount.currencyUomId/></b></@td>
                </@tr>
                <@tr>
                    <@td colspan="3"><b>${uiLabelMap.FormFieldTitle_availableBalance}</b></@td>
                    <@td><b><@ofbizCurrency amount=ownedFinAccount.availableBalance isoCode=ownedFinAccount.currencyUomId/></b></@td>
                </@tr>
            </@tfoot>
        </@table>
    </div>
</#if>

