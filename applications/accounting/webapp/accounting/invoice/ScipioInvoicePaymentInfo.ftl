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
<@section title=uiLabelMap.AccountingPaymentInformation>
  <@table type="fields">
        <@thead>
            <@tr valign="bottom" class="header-row">
                <@th>${uiLabelMap.CommonType!}</@th>
                <@th>${uiLabelMap.CommonDue!}</@th>
                <@th>${uiLabelMap.CommonAmount!}</@th>
                <@th>${uiLabelMap.CommonPaid!}</@th>
                <@th>${uiLabelMap.CommonOutStanding!}</@th>
            </@tr>
        </@thead>
        <#list invoicePaymentInfoList as item>
            <#if item.termTypeId?has_content>
              <#assign itemType = delegator.findOne("TermType", {"termTypeId":item.termTypeId!""}, false)!>
            <#else>
              <#assign itemType = {}>
            </#if>
            <@tr>
                <@td>${(itemType.get("description",locale))!}</@td>
                <@td><@formattedDateTime date=item.dueDate /></@td>
                
                <@td><@ofbizCurrency isoCode=invoice.currencyUomId amount=(item.amount!)/></@td>
                <@td><@ofbizCurrency isoCode=invoice.currencyUomId amount=(item.paidAmount!)/></@td>
                <@td><strong><@ofbizCurrency isoCode=invoice.currencyUomId amount=(item.outstandingAmount!)/></strong></@td>
            </@tr>
        </#list>
  </@table>
</@section>