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
<#-- E-invoice settings of a customer, supplier or organization (PartyEInvoicePref). -->
<#if partyId?has_content>
<@section title=(raw(uiLabelMap.EInvoiceSettings) + ": " + raw(partyName!) + " [" + raw(partyId) + "]")>
    <@form method="post" action=makePageUrl("updatePartyEInvoicePref") name="EditPartyEInvoicePref" id="EditPartyEInvoicePref">
        <@field type="hidden" name="partyId" value=partyId/>
        <@field type="hidden" name="invoiceId" value=(parameters.invoiceId!"")/>
        <@field type="select" name="formatEnumId" label=uiLabelMap.EInvoiceFormat>
            <option value=""></option>
            <#list einvFormats as format>
                <option value="${format.enumId}"<#if format.enumId == (einvPref.formatEnumId)!""> selected="selected"</#if>>${format.get("description", locale)!format.enumId}</option>
            </#list>
        </@field>
        <@field type="select" name="deliveryChannelEnumId" label=uiLabelMap.EInvoiceDeliveryChannel>
            <option value=""></option>
            <#list einvChannels as channel>
                <option value="${channel.enumId}"<#if channel.enumId == (einvPref.deliveryChannelEnumId)!""> selected="selected"</#if>>${channel.get("description", locale)!channel.enumId}</option>
            </#list>
        </@field>
        <@field type="text" name="einvoiceEmail" label=uiLabelMap.EInvoiceEmailAddress value=(einvPref.einvoiceEmail)!""/>
        <@field type="text" name="buyerReference" label=uiLabelMap.EInvoiceBuyerReference value=(einvPref.buyerReference)!""/>
        <@field type="text" name="electronicAddress" label=uiLabelMap.EInvoiceElectronicAddress value=(einvPref.electronicAddress)!""/>
        <@field type="text" name="electronicAddressScheme" label=uiLabelMap.EInvoiceElectronicAddressScheme value=(einvPref.electronicAddressScheme)!""/>
        <@field type="text" name="contactName" label=uiLabelMap.EInvoiceContactName value=(einvPref.contactName)!""/>
        <@field type="text" name="contactPhone" label=uiLabelMap.EInvoiceContactPhone value=(einvPref.contactPhone)!""/>
        <@field type="text" name="contactEmail" label=uiLabelMap.EInvoiceContactEmail value=(einvPref.contactEmail)!""/>
        <#if einvEftAccounts?has_content>
            <@field type="select" name="paymentMethodId" label=uiLabelMap.EInvoicePaymentAccount>
                <option value=""></option>
                <#list einvEftAccounts as eft>
                    <option value="${eft.paymentMethodId}"<#if eft.paymentMethodId == (einvPref.paymentMethodId)!""> selected="selected"</#if>>${eft.label!eft.paymentMethodId}</option>
                </#list>
            </@field>
        </#if>
        <@field type="select" name="smallBusinessFlag" label=uiLabelMap.EInvoiceSmallBusiness tooltip=uiLabelMap.EInvoiceSmallBusinessHelp>
            <option value="N"<#if (einvPref.smallBusinessFlag)!"N" != "Y"> selected="selected"</#if>>${uiLabelMap.CommonN}</option>
            <option value="Y"<#if (einvPref.smallBusinessFlag)!"" == "Y"> selected="selected"</#if>>${uiLabelMap.CommonY}</option>
        </@field>
        <@field type="submit" text=uiLabelMap.CommonSave class="+${styles.link_run_sys!} ${styles.action_update!}"/>
    </@form>
    <#if parameters.invoiceId?has_content>
        <@menu type="button">
            <@menuitem type="link" href=makePageUrl("invoiceEInvoice?invoiceId=${parameters.invoiceId}") text=uiLabelMap.EInvoice class="+${styles.action_nav!} ${styles.action_view!}"/>
        </@menu>
    </#if>
</@section>
</#if>
