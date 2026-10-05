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

<#macro menuContent menuArgs={}>
    <@menu args=menuArgs>
        <#if quote?? && quote.statusId == "QUO_APPROVED">
            <@menuitem type="link" href=makePageUrl("loadCartFromQuote?quoteId=" + quote.quoteId + "&amp;finalizeMode=init") class="+${styles.action_run_session!} ${styles.action_clear!}" text=uiLabelMap.OrderCreateOrder />
        </#if>
    </@menu>
</#macro>

<@section title=title!rawLabel(titleProperty)! menuContent=menuContent>
    <#if quote?has_content>
        <@table type="fields" class="${styles.table_basic!}" cellspacing="0">
            <#-- quote id -->
            <@tr>
                <@td scope="row" class="${styles.grid_large!}3">${uiLabelMap.OrderQuote} ${uiLabelMap.CommonNbr}</@td>
                <@td colspan="3">
                    ${quote.quoteId!}
                </@td>
            </@tr>
            <#-- quote name -->
            <@tr>
                <@td scope="row" class="${styles.grid_large!}3">${uiLabelMap.CommonName}</@td>
                <@td colspan="3">
                    ${quote.quoteName!}
                </@td>
            </@tr>
            <#-- quote description -->
            <@tr>
                <@td scope="row" class="${styles.grid_large!}3">${uiLabelMap.CommonDescription}</@td>
                <@td colspan="3">
                    ${quote.description!}
                </@td>
            </@tr>
            <#-- quote status -->
            <#assign status = quote.getRelatedOne("StatusItem", true)>
            <@tr>
                <@td scope="row" class="${styles.grid_large!}3">${uiLabelMap.CommonStatus}</@td>
                <@td colspan="3">
                    ${status.get("description",locale)}
                </@td>
            </@tr>
            <#-- issue date -->
            <@tr>
                <@td scope="row" class="${styles.grid_large!}3">${uiLabelMap.OrderOrderQuoteIssueDate}</@td>
                <@td colspan="3">
                    ${quote.issueDate!}
                </@td>
            </@tr>
            <#-- valid from date -->
            <@tr>
                <@td scope="row" class="${styles.grid_large!}3">${uiLabelMap.CommonValidFromDate}</@td>
                <@td colspan="3">
                    ${quote.validFromDate!}
                </@td>
            </@tr>
            <#-- valid thru date -->
            <@tr>
                <@td scope="row" class="${styles.grid_large!}3">${uiLabelMap.CommonValidThruDate}</@td>
                <@td colspan="3">
                    ${quote.validThruDate!}
                </@td>
            </@tr>
        </@table>
    </#if>
</@section>