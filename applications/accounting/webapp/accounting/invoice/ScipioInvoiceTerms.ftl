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
<@section title=uiLabelMap.AccountingAgreementItemTerms>
    <@table type="fields">
        <@thead>
            <@tr valign="bottom" class="header-row">
                <@th>${uiLabelMap.FormFieldTitle_termDays!}</@th>
                <@th>${uiLabelMap.FormFieldTitle_termTypeId!}</@th>
                <@th>${uiLabelMap.FormFieldTitle_termValue!}</@th>
                <@th>${uiLabelMap.FormFieldTitle_textData!}</@th>
                <@th>${uiLabelMap.FormFieldTitle_textValue!}</@th>
            </@tr>
        </@thead>
       <#list invoiceTerms as item>
            <#assign itemType = item.getRelatedOne("TermType", false)!/>
            <@tr>
                <@td>${item.termDays!}</@td>
                <@td>${(itemType.get("description",locale))!}</@td>
                <@td><@ofbizCurrency isoCode=item.uomId amount=(item.termValue!0)/></@td>
                <@td>${item.description!}</@td><#-- TODO: REVIEW: this field name was invalid, I can only guess: ${item.textData!} -->
                <@td>${item.textValue!}</@td>
            </@tr>
        </#list>

    </@table>
</@section>