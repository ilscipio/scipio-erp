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
<#if partyTaxAuthInfoAndDetailList??>
  <#-- SCIPIO: no such request in shop
    <#list partyTaxAuthInfoAndDetailList as partyTaxAuthInfoAndDetail>
        <div>
            <a href="<@pageUrl>deleteCustomerTaxAuthInfo?partyId=${partyId}&amp;taxAuthPartyId=${partyTaxAuthInfoAndDetail.taxAuthPartyId}&amp;taxAuthGeoId=${partyTaxAuthInfoAndDetail.taxAuthGeoId}&amp;fromDate=${partyTaxAuthInfoAndDetail.fromDate}</@pageUrl>" class="${styles.link_run_sys!} ${styles.action_remove!}">X</a>
            [${partyTaxAuthInfoAndDetail.geoCode}] ${partyTaxAuthInfoAndDetail.geoName} (${partyTaxAuthInfoAndDetail.groupName!}): ${uiLabelMap.PartyTaxId} [${partyTaxAuthInfoAndDetail.partyTaxId!(uiLabelMap.CommonNA)}], ${uiLabelMap.PartyTaxIsExempt} [${partyTaxAuthInfoAndDetail.isExempt!"N"}]
        </div>
    </#list>
  -->
    <@field type="select" label=uiLabelMap.PartyTaxAddInfo name="taxAuthPartyGeoIds">
      <option></option>
      <#list taxAuthorityAndDetailList as taxAuthorityAndDetail>
        <option value="${taxAuthorityAndDetail.taxAuthPartyId}::${taxAuthorityAndDetail.taxAuthGeoId}">[${taxAuthorityAndDetail.geoCode}] ${taxAuthorityAndDetail.geoName} (${taxAuthorityAndDetail.groupName!})</option>
      </#list>
    </@field>
    <@field type="input" label=uiLabelMap.CommonId name="partyTaxId" size="12" maxlength="40"/>
    <#if (productStore.showTaxIsExempt!"Y") == "Y">
        <@field type="checkbox" checkboxType="simple-standard" label=uiLabelMap.PartyTaxIsExempt name="isExempt" value="Y"/>
    <#else>
        <input type="hidden" name="isExempt" value="N"/>
    </#if>
</#if>
