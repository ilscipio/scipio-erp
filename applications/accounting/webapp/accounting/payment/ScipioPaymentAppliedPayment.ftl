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
<#-- SCIPIO -->
<#if paymentList?has_content> <#-- FIXME: Ugly workaround, because variables set by entity-condition do not validate correctly on screen condition -->
<@section title=uiLabelMap.AccountingAppliedPayments>
    <@table type="data-complex" role="grid">
        <@thead>
            <@tr valign="bottom" class="header-row">
                <@th >${uiLabelMap.CommonInvoice}</@th>
                <#--<@th >${uiLabelMap.CommonSequenceNum}</@th>-->
                <@th >${uiLabelMap.CommonTo}</@th>
                <@th class="${styles.text_right!}">${uiLabelMap.AccountingAmountApplied}</@th>
            </@tr>
        </@thead>
        <#list paymentList as iApp>
            <#assign amountApplied = Static["org.ofbiz.accounting.payment.PaymentWorker"].getPaymentAppliedAmount(delegator, iApp.paymentApplicationId!0)/>
            <@tr>
                <@td><a href="<@pageUrl>invoiceOverview?invoiceId=${iApp.invoiceId!}</@pageUrl>">${iApp.invoiceId!}</a></@td>
                <#--<@td>${iApp.invoiceItemSeqId!}</@td>-->
                <#if iApp.billingAccountId?has_content>
                    <#assign billingAcct = iApp.getRelatedOne("BillingAccount", false)/>
                        <@td><a href="<@pageUrl>EditBillingAccount?billingAccountId=${invoice.billingAccountId!}</@pageUrl>">${billingAcct.get('description',locale)}</a></@td>
                    <#else>
                        <@td></@td>
                </#if>
                <@td class="${styles.text_right!}"><@ofbizCurrency isoCode=payment.currencyUomId amount=(iApp.amountApplied!)/></@td>
            </@tr>
        </#list>        
    </@table>
</@section>
</#if>