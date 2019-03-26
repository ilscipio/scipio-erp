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
    ${postalAddress.postalCode!} ${postalAddress.city!}
    <#if postalAddress.countryGeoId?has_content><br />
      <#assign country = postalAddress.getRelatedOne("CountryGeo", true)>
      ${country.get("geoName", locale)?default(country.geoId)}
    </#if>
  </div>
