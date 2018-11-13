<#--
This file is subject to the terms and conditions defined in the
files 'LICENSE' and 'NOTICE', which are part of this source
code package.
-->

<#if fulfillContactMech?has_content>

<#if "POSTAL_ADDRESS" == fulfillContactMech.contactMechTypeId>
  <#assign label = uiLabelMap.PartyAddressMailingShipping>
  <#assign postalAddress = fulfillContactMech.getRelatedOne("PostalAddress", true)!>
<#elseif "EMAIL_ADDRESS" == fulfillContactMech.contactMechTypeId>
  <#assign label = uiLabelMap.PartyToEmailAddress>
  <#assign emailAddress = fulfillContactMech.infoString!>
<#elseif "TELECOM_NUMBER" == fulfillContactMech.contactMechTypeId>
  <#assign label = uiLabelMap.PartyPhoneNumber>
  <#assign telecomNumber = fulfillContactMech.getRelatedOne("TelecomNumber", true)!>
</#if>

  <@section title=uiLabelMap.PartyContactInformation>
        <@table type="fields"> <#-- orig: class="basic-table" --> <#-- orig: cellspacing="0" -->
            <@tr>
                <@td align="right" valign="top" width="25%">
                    &nbsp;${label!uiLabelMap.PartyUnknown}
                </@td>
                <@td width="5%">&nbsp;</@td>
                <@td valign="top" width="70%">
                      <#if emailAddress?has_content>${emailAddress}</#if>

                      <#if postalAddress?has_content>
                        <#if postalAddress.toName?has_content><span>${uiLabelMap.PartyAddrToName}</span>&nbsp;${postalAddress.toName}<br /></#if>
                        <#if postalAddress.attnName?has_content><span>${uiLabelMap.PartyAddrAttnName}</span>&nbsp;${postalAddress.attnName}<br /></#if>
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
                             ${country.geoName!country.geoId}
                        </#if>
                      </#if>

                      <#if telecomNumber?has_content>
                        ${telecomNumber.countryCode!}
                        <#if telecomNumber.areaCode?has_content>${telecomNumber.areaCode!"000"}-</#if>${telecomNumber.contactNumber!"000-0000"}
                        <#--<#if (telecomNumber?has_content && !telecomNumber.countryCode?has_content) || telecomNumber.countryCode == "011">
                          <a target="_blank" href="${uiLabelMap.CommonLookupAnywhoLink}" class="${styles.link_nav!} ${styles.action_find!} ${styles.action_external!}">${uiLabelMap.CommonLookupAnywho}</a>
                          <a target="_blank" href="${uiLabelMap.CommonLookupWhitepagesTelNumberLink}" class="${styles.link_nav!} ${styles.action_find!} ${styles.action_external!}">${uiLabelMap.CommonLookupWhitepages}</a>
                        </#if>-->
                      </#if>
                    </@td>
            </@tr>
        </@table>
  </@section>
</#if>