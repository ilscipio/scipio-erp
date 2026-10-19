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
<#-- Import of a received e-invoice (XRechnung / UBL / CII / ZUGFeRD / Factur-X) as a purchase invoice. -->
<@section title=uiLabelMap.EInvoiceImportTitle>
    <p>${uiLabelMap.EInvoiceImportHelp}</p>
    <@form method="post" action=makePageUrl("importEInvoice") name="EInvoiceImportForm" id="EInvoiceImportForm" enctype="multipart/form-data">
        <@field type="text" name="organizationPartyId" label=uiLabelMap.EInvoiceOrganization value=(parameters.organizationPartyId!defaultOrganizationPartyId!myCompanyId!"") required=true/>
        <@field type="lookup" name="supplierPartyId" formName="EInvoiceImportForm" id="supplierPartyId" fieldFormName="LookupPartyName"
            label=uiLabelMap.EInvoiceSupplier value=(parameters.supplierPartyId!"") tooltip=uiLabelMap.EInvoiceImportSupplierHelp/>
        <@field type="file" name="uploadedFile" label=uiLabelMap.EInvoiceFile required=true/>
        <@field type="submit" text=uiLabelMap.CommonImport class="+${styles.link_run_sys!} ${styles.action_import!}"/>
    </@form>
</@section>
