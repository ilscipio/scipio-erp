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
<@section title=uiLabelMap.AccountingInvoiceItems>
    <@table type="data-complex" role="grid">
        <@thead>
            <@tr valign="bottom" class="header-row">
                <@th>${uiLabelMap.FormFieldTitle_invoiceItemSeqId!}</@th>
                <@th>${uiLabelMap.FormFieldTitle_invoiceItemTypeId!}</@th>
                <@th>${uiLabelMap.FormFieldTitle_productId!}</@th>
                <@th width="10%">${uiLabelMap.FormFieldTitle_orderId!}</@th>
                <@th width="20%" class="${styles.text_right!}">${uiLabelMap.FormFieldTitle_quantity!}</@th>
                <@th width="20%" class="${styles.text_right!}">${uiLabelMap.FormFieldTitle_amount!}</@th>
                <@th width="20%" class="${styles.text_right!}">${uiLabelMap.FormFieldTitle_total!}</@th>
            </@tr>
        </@thead>
        <#list invItemAndOrdItems as item>
            <#assign iTotal = (item.quantity!1 * item.amount!0)/>
            <#assign itemType = delegator.findOne("InvoiceItemType", {"invoiceItemTypeId" : item.invoiceItemTypeId!}, true)>
            <@tr>
                <@td><a href="<@pageUrl>invoiceOverview?invoiceId=${item.invoiceId}</@pageUrl>">${item.invoiceId!}</a></@td>
                <@td>${itemType.get("description",locale)!}</@td>
                <@td>${item.productId!}</@td>
                <@td>${item.orderId!}</@td>
                <@td class="${styles.text_right!}">${item.quantity!1}</@td>
                <@td class="${styles.text_right!}"><@ofbizCurrency isoCode=invoice.currencyUomId amount=(item.amount!)/></@td>
                <@td class="${styles.text_right!}"><strong><@ofbizCurrency isoCode=invoice.currencyUomId amount=(iTotal!)/></strong></@td>
            </@tr>
        </#list>
    </@table>
</@section>