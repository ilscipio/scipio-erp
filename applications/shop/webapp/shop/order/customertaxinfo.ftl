<#--
This file is subject to the terms and conditions defined in the
files 'LICENSE' and 'NOTICE', which are part of this source
code package.
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
