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

<@form name="massInvoiceDownload" action=makePageUrl("massDownloadInvoices.pdf") method="POST">
    <@fields>
        <@field type="hidden" name="partyGroupId" value=myCompanyId! />
        <@field type="datetime" name="fromDate" label=getLabel("CommonFrom") />
        <@field type="datetime" name="thruDate" label=getLabel("CommonThru")/>
        <@field type="select" name="status" label=getLabel("CommonStatus")>
            <option value="">--</option>
            <#list invoiceStatuses as invoiceStatus>
                <option value="${invoiceStatus.statusId}">${invoiceStatus.description}</option>
            </#list>
        </@field>
        <@field type="select" name="type" label=getLabel("CommonType")>
            <option value="">--</option>
            <#list invoiceTypes as invoiceType>
                <option value="${invoiceType.invoiceTypeId}">${invoiceType.description}</option>
            </#list>
        </@field>
        <@field type="lookup" name="partyIdFrom" formName="massInvoiceDownload" id="partyIdFrom" fieldFormName="LookupPartyName" label=uiLabelMap.PartyPartyFrom />
        <@field type="lookup" name="partyIdTo" formName="massInvoiceDownload" id="partyIdTo" fieldFormName="LookupPartyName" label=uiLabelMap.PartyPartyTo />
        <@field type="submit" name="generate" label=getLabel("CommonDownload")/>
    </@fields>
</@form>