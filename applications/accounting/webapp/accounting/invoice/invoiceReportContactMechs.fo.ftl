<#--
This file is subject to the terms and conditions defined in the
files 'LICENSE' and 'NOTICE', which are part of this source
code package.
-->
<#escape x as x?xml>
<fo:block content-width="85mm" font-size="10pt" margin-top="45mm" padding-right="20mm">
    <fo:block-container height="5mm" font-size="6pt">
        <fo:block wrap-option="wrap">
            <#-- Return Address -->
            ${companyName}
        </fo:block>
    </fo:block-container>
    <fo:block>
    <#if billingAddress?has_content>
        <#assign billToPartyNameResult = runService("getPartyNameForDate", {"partyId":billToParty.partyId, "compareDate":invoice.invoiceDate, "userLogin":userLogin})/>
        <fo:block wrap-option="wrap">${billToPartyNameResult.fullName?default(billingAddress.toName)?default("Billing Name Not Found")}</fo:block>
        <#if billingAddress.attnName??>
            <fo:block wrap-option="wrap">${billingAddress.attnName}</fo:block>
        </#if>
            <fo:block wrap-option="wrap">${billingAddress.address1!}</fo:block>
        <#if billingAddress.address2??>
            <fo:block wrap-option="wrap">${billingAddress.address2}</fo:block>
        </#if>
        <fo:block wrap-option="wrap">${billingAddress.city!} ${billingAddress.stateProvinceGeoId!} ${billingAddress.postalCode!}</fo:block>
        <#if billToPartyTaxId?has_content>
            <fo:block wrap-option="wrap">${uiLabelMap.PartyTaxId}: ${billToPartyTaxId}</fo:block>
        </#if>
    <#else>
        <fo:block wrap-option="wrap">${uiLabelMap.AccountingNoGenBilAddressFound}</fo:block>
        <fo:block wrap-option="wrap">${billToParty.partyId}</fo:block>
    </#if>
    </fo:block>
</fo:block>
</#escape>