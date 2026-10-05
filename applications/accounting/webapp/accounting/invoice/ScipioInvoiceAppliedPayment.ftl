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
<@section title=uiLabelMap.AccountingAppliedPayments>
    <@table type="data-complex" role="grid">
        <@thead>
            <@tr valign="bottom" class="header-row">
                <@th >${uiLabelMap.CommonPayment}</@th>
                <#--<@th >${uiLabelMap.CommonSequenceNum}</@th>-->
                <@th>${uiLabelMap.CommonProduct}</@th>
                <@th >${uiLabelMap.CommonDescription}</@th>
                <@th class="${styles.text_right!}">${uiLabelMap.AccountingAmountApplied}</@th>
                <@th class="${styles.text_right!}">${uiLabelMap.CommonTotal}</@th>
            </@tr>
        </@thead>
        <#list invoiceApplications as iApp>
            <@tr>
                <@td><a href="<@pageUrl>paymentOverview?paymentId=${iApp.paymentId!}</@pageUrl>">${iApp.paymentId!}</a></@td>
                <#--<@td>${iApp.invoiceItemSeqId!}</@td>-->
                <@td>${iApp.productId!}</@td>
                <@td>${iApp.description!}</@td>
                <@td class="${styles.text_right!}"><@ofbizCurrency isoCode=invoice.currencyUomId amount=(iApp.amountApplied!)/></@td>
                <@td class="${styles.text_right!}"><@ofbizCurrency isoCode=invoice.currencyUomId amount=(iApp.total!)/></@td>
            </@tr>
        </#list>        
    </@table>
</@section>