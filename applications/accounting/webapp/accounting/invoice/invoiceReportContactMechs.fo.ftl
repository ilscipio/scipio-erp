<#--
Licensed to the Apache Software Foundation (ASF) under one
or more contributor license agreements.  See the NOTICE file
distributed with this work for additional information
regarding copyright ownership.  The ASF licenses this file
to you under the Apache License, Version 2.0 (the
"License"); you may not use this file except in compliance
with the License.  You may obtain a copy of the License at

http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing,
software distributed under the License is distributed on an
"AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
KIND, either express or implied.  See the License for the
specific language governing permissions and limitations
under the License.
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
        <#assign billToPartyNameResult = dispatcher.runSync("getPartyNameForDate", {"partyId":billToParty.partyId, "compareDate":invoice.invoiceDate, "userLogin":userLogin})/>
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