<#--
This file is subject to the terms and conditions defined in the
files 'LICENSE' and 'NOTICE', which are part of this source
code package.
-->
  <div>
    <#if postalAddress.toName?has_content><b>${uiLabelMap.PartyAddrToName}:</b> ${postalAddress.toName}<br /></#if>
    <#if postalAddress.attnName?has_content><b>${uiLabelMap.PartyAddrAttnName}:</b> ${postalAddress.attnName}<br /></#if>
    ${postalAddress.address1!}<br />
    <#if postalAddress.address2?has_content>${postalAddress.address2}<br /></#if>
    ${postalAddress.city!},
    <#if postalAddress.stateProvinceGeoId?has_content>
      <#assign stateProvince = postalAddress.getRelatedOne("StateProvinceGeo", true)>
      ${stateProvince.abbreviation!stateProvince.geoId}
    </#if>
    ${postalAddress.postalCode!}
    <#if postalAddress.countryGeoId?has_content><br />
      <#assign country = postalAddress.getRelatedOne("CountryGeo", true)>
      ${country.get("geoName", locale)?default(country.geoId)}
    </#if>
    </div>
    <#-- 
    <#if !postalAddress.countryGeoId?has_content>
    <#assign addr1 = postalAddress.address1!>
    <#if addr1?has_content && (addr1.indexOf(" ") > 0)>
      <#assign addressNum = addr1.substring(0, addr1.indexOf(" "))>
      <#assign addressOther = addr1.substring(addr1.indexOf(" ")+1)>
      <a target="_blank" href="${uiLabelMap.CommonLookupWhitepagesAddressLink}" class="${styles.link_nav!} ${styles.action_find!} ${styles.action_external!}">${uiLabelMap.CommonLookupWhitepages}</a>
    </#if>
    </#if>-->

