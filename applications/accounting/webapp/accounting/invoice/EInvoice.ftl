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
<#-- E-invoice tab of an invoice: check, download, settings; for a received invoice: the original file. -->
<#if invoice?has_content>
<#if einvOutgoing!false>
    <@section title=uiLabelMap.EInvoiceCheckTitle>
        <@form method="get" action=makePageUrl("invoiceEInvoice") name="einvFormatForm" id="einvFormatForm">
            <@field type="hidden" name="invoiceId" value=invoice.invoiceId/>
            <@field type="select" name="formatId" label=uiLabelMap.EInvoiceFormat onChange="this.form.submit();">
                <#list einvFormats as format>
                    <option value="${format.enumId}"<#if format.enumId == einvFormatId> selected="selected"</#if>>${format.get("description", locale)!format.enumId}<#if format.enumId == (einvCustomerFormatId!"")> (${uiLabelMap.EInvoiceCustomerFormat})</#if></option>
                </#list>
            </@field>
            <@field type="submit" text=uiLabelMap.EInvoiceCheck class="+${styles.link_run_sys!} ${styles.action_verify!}"/>
        </@form>

        <#if einvValid!false>
            <@alert type="success">${getLabel("EInvoiceValid", "EInvoiceUiLabels", {"formatId": einvFormatId})}</@alert>
        <#else>
            <@alert type="error">
                ${getLabel("EInvoiceNotValid", "EInvoiceUiLabels", {"formatId": einvFormatId})}
                <ul>
                <#list (einvErrors![]) as message>
                    <li>${message}</li>
                </#list>
                </ul>
            </@alert>
        </#if>
        <#if einvWarnings?has_content>
            <@alert type="warning">
                ${uiLabelMap.EInvoiceWarnings}:
                <ul>
                <#list einvWarnings as message>
                    <li>${message}</li>
                </#list>
                </ul>
            </@alert>
        </#if>

        <@menu type="button">
            <#if einvValid!false>
                <@menuitem type="link" href=makePageUrl("einvoice?invoiceId=${invoice.invoiceId}&formatId=${einvFormatId}") text="${uiLabelMap.EInvoiceDownload} (${einvFormatId})" class="+${styles.action_run_sys!} ${styles.action_export!}"/>
                <@menuitem type="link" href=makePageUrl("sendPerEmail?invoiceId=${invoice.invoiceId}") text=uiLabelMap.CommonSendPerEmail class="+${styles.action_nav!} ${styles.action_send!}"/>
            </#if>
            <@menuitem type="link" href=makePageUrl("EditPartyEInvoicePref?partyId=${einvCustomerId}&invoiceId=${invoice.invoiceId}") text="${uiLabelMap.EInvoiceSettings}: ${einvCustomerId}" class="+${styles.action_nav!} ${styles.action_update!}"/>
            <@menuitem type="link" href=makePageUrl("EditPartyEInvoicePref?partyId=${einvOrganizationId}&invoiceId=${invoice.invoiceId}") text="${uiLabelMap.EInvoiceSettings}: ${einvOrganizationId}" class="+${styles.action_nav!} ${styles.action_update!}"/>
        </@menu>
    </@section>

    <#if einvArchived?has_content>
        <@section title=uiLabelMap.EInvoiceArchivedFiles>
            <@table type="data-list">
                <@thead>
                    <@tr>
                        <@th>${uiLabelMap.CommonDate}</@th>
                        <@th>${uiLabelMap.EInvoiceFormat}</@th>
                        <@th>${uiLabelMap.CommonName}</@th>
                        <@th></@th>
                    </@tr>
                </@thead>
                <#list einvArchived as doc>
                    <@tr>
                        <@td>${doc.fromDate!}</@td>
                        <@td>${doc.formatId!}</@td>
                        <@td>${doc.fileName!}</@td>
                        <@td><a href="<@pageUrl>einvoice?invoiceId=${invoice.invoiceId}&amp;formatId=${doc.formatId!}</@pageUrl>" class="${styles.link_run_sys!} ${styles.action_export!}">${uiLabelMap.EInvoiceDownload}</a></@td>
                    </@tr>
                </#list>
            </@table>
        </@section>
    </#if>
<#else>
    <@section title=uiLabelMap.EInvoiceOriginal>
        <#if einvHasOriginal!false>
            <p>${uiLabelMap.EInvoiceFormat}: ${einvReceivedSyntax!}</p>
            <@menu type="button">
                <@menuitem type="link" href=makePageUrl("einvoiceOriginal?invoiceId=${invoice.invoiceId}") text=uiLabelMap.EInvoiceDownload class="+${styles.action_run_sys!} ${styles.action_export!}"/>
                <@menuitem type="link" href=makePageUrl("einvoiceOriginalView?invoiceId=${invoice.invoiceId}") target="_blank" text=uiLabelMap.EInvoiceView class="+${styles.action_run_sys!} ${styles.action_view!}"/>
            </@menu>
        <#else>
            <@commonMsg type="info">${uiLabelMap.CommonNoRecordFound}</@commonMsg>
        </#if>
    </@section>
</#if>
</#if>
