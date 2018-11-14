<#--
This file is subject to the terms and conditions defined in the
files 'LICENSE' and 'NOTICE', which are part of this source
code package.
-->
<#escape x as x?xml>
  <fo:block>${postalAddress.address1!}</fo:block>
  <#if postalAddress.address2?has_content><fo:block>${postalAddress.address2!}</fo:block></#if>
  <fo:block>${postalAddress.city!}<#if postalAddress.stateProvinceGeoId?has_content>, ${postalAddress.stateProvinceGeoId}</#if> ${postalAddress.postalCode!}</fo:block>
  <#if postalAddress.countryGeoId?has_content>
    <fo:block>
      <#assign country = postalAddress.getRelatedOne("CountryGeo", true)>
      ${country.get("geoName", locale)?default(country.geoId)}
    </fo:block>
  </#if>
</#escape>

