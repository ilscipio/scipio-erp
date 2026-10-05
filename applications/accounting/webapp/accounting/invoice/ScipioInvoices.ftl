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
<#if invoices?has_content>
<@table type="fields">
    <@thead>
        <@tr valign="bottom" class="header-row">
            <@th>${uiLabelMap.AccountingInvoiceID!}</@th>
            <@th>${uiLabelMap.CommonType!}</@th>
            <#-- <@th>${uiLabelMap.CommonFrom!}</@th> --->
            <@th>${uiLabelMap.CommonDate!}</@th>
            <@th>${uiLabelMap.CommonTotal!}</@th>
            <@th>${uiLabelMap.FormFieldTitle_amountToApply!}</@th>
        </@tr>
    </@thead>
    <#list invoices as item>
        <#assign total = Static["org.ofbiz.accounting.invoice.InvoiceWorker"].getInvoiceTotal(delegator, item.invoiceId) />
        <#assign outstandingAmount = Static["org.ofbiz.accounting.invoice.InvoiceWorker"].getInvoiceNotApplied(delegator, item.invoiceId) />       
        <#assign itemType = item.getRelatedOne("InvoiceType", false)/>
        <@tr>
            <@td><a href="<@pageUrl>invoiceOverview?invoiceId=${item.invoiceId}</@pageUrl>">${item.invoiceId!}</a></@td>
            <@td>${itemType.get("description",locale)!}</@td>
            <#-- <@td>${item.partyIdFrom}</@td> -->
            <@td><#if item.dueDate?has_content><@formattedDateTime date=item.dueDate /></#if></@td>
            <@td><@ofbizCurrency isoCode=item.currencyUomId amount=total!0/></@td>              
            <@td><strong><@ofbizCurrency isoCode=item.currencyUomId amount=outstandingAmount!0/></strong></@td>
        </@tr>
    </#list>
</@table>
<#else>
  <@commonMsg type="result-norecord" />
</#if>
