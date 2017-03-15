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

  <#macro menuContent menuArgs={}>
    <@menu args=menuArgs>
        <#--if security.hasEntityPermission("PARTYMGR", "_CREATE", session) || userLogin.partyId == partyId>
          <@menuitem type="link" href=makeOfbizUrl("editcontactmech?partyId=${partyId}") text=uiLabelMap.CommonCreateNew class="+${styles.action_nav!} ${styles.action_add!}" />
        </#if-->
    </@menu>
  </#macro>
  <@section id="partyContactInfo" title=uiLabelMap.PartyContactInformation menuContent=menuContent>
      <#if contactMeches?has_content>
        <@table type="data-list"> <#-- orig: class="basic-table" --> <#-- orig: cellspacing="0" -->
          <@tr>
            <@th>${uiLabelMap.PartyContactType}</@th>
            <@th>${uiLabelMap.PartyContactInformation}</@th>
            <@th>${uiLabelMap.PartyContactSolicitingOk}</@th>
            <@th>&nbsp;</@th>
          </@tr>
          <#list contactMeches as contactMechMap>
            <#assign contactMech = contactMechMap.contactMech>
            <#assign partyContactMech = contactMechMap.partyContactMech>
            <@tr type="util"><@td colspan="4"><hr /></@td></@tr>
            <@tr>
              <@td class="label align-top">${contactMechMap.contactMechType.get("description",locale)}</@td>
              <@td>
                <#list contactMechMap.partyContactMechPurposes as partyContactMechPurpose>
                  <#assign contactMechPurposeType = partyContactMechPurpose.getRelatedOne("ContactMechPurposeType", true)>
                  <div>
                    <#if contactMechPurposeType?has_content>
                      <strong>${contactMechPurposeType.get("description",locale)}</strong>
                    <#else>
                      <strong>${uiLabelMap.PartyMechPurposeTypeNotFound}: "${partyContactMechPurpose.contactMechPurposeTypeId}"</strong>
                    </#if>
                    <#if partyContactMechPurpose.thruDate?has_content>
                      (${uiLabelMap.CommonExpire}: ${partyContactMechPurpose.thruDate})
                    </#if>
                  </div>
                </#list>
                <#if "POSTAL_ADDRESS" == contactMech.contactMechTypeId>
                  <#assign postalAddress = contactMechMap.postalAddress>
                  <#if postalAddress?has_content>
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
                      ${country.geoName!country.geoId}
                    </#if>
                  </div>
                  </#if>
                  <#-- 
                  <#if (postalAddress?has_content && !postalAddress.countryGeoId?has_content) || postalAddress.countryGeoId == "USA">
                    <#assign addr1 = postalAddress.address1!>
                    <#if addr1?has_content && (addr1.indexOf(" ") > 0)>
                      <#assign addressNum = addr1.substring(0, addr1.indexOf(" "))>
                      <#assign addressOther = addr1.substring(addr1.indexOf(" ")+1)>
                      <a target="_blank" href="${uiLabelMap.CommonLookupWhitepagesAddressLink}" class="${styles.link_nav!} ${styles.action_find!}">${uiLabelMap.CommonLookupWhitepages}</a>
                    </#if>
                  </#if> -->
                  <#if postalAddress.geoPointId?has_content>
                    <#if contactMechPurposeType?has_content>
                      <#assign popUptitle = contactMechPurposeType.get("description",locale) + uiLabelMap.CommonGeoLocation>
                    </#if>
                    <a href="javascript:popUp('<@ofbizUrl>geoLocation?geoPointId=${postalAddress.geoPointId}</@ofbizUrl>', '${popUptitle!}', '450', '550')" class="${styles.link_nav!} ${styles.action_find!}">${uiLabelMap.CommonGeoLocation}</a>
                  </#if>
                <#elseif "TELECOM_NUMBER" == contactMech.contactMechTypeId>
                  <#assign telecomNumber = contactMechMap.telecomNumber>
                  <div>
                    ${telecomNumber.countryCode!}
                    <#if telecomNumber.areaCode?has_content>${telecomNumber.areaCode!"000"}-</#if>${telecomNumber.contactNumber?default("000-0000")}
                    <#if partyContactMech.extension?has_content>${uiLabelMap.PartyContactExt}&nbsp;${partyContactMech.extension}</#if>
                    <#--
                    <#if (telecomNumber?has_content && !telecomNumber.countryCode?has_content) || telecomNumber.countryCode == "011">
                      <a target="_blank" href="${uiLabelMap.CommonLookupAnywhoLink}" class="${styles.link_nav!} ${styles.action_find!} ${styles.action_external!}">${uiLabelMap.CommonLookupAnywho}</a>
                      <a target="_blank" href="${uiLabelMap.CommonLookupWhitepagesTelNumberLink}" class="${styles.link_nav!} ${styles.action_find!} ${styles.action_external!}">${uiLabelMap.CommonLookupWhitepages}</a>
                    </#if>-->
                  </div>
                <#elseif "EMAIL_ADDRESS" == contactMech.contactMechTypeId>
                  <div>
                    ${contactMech.infoString!}
                    <#--a href="<@ofbizUrl>EditCommunicationEvent?partyIdFrom=${userLogin.partyId}&amp;partyIdTo=${partyId}&amp;communicationEventTypeId=EMAIL_COMMUNICATION&amp;contactMechIdTo=${contactMech.contactMechId}&amp;contactMechTypeId=EMAIL_ADDRESS<#if thisUserPrimaryEmail?has_content>&amp;contactMechIdFrom=${thisUserPrimaryEmail.contactMechId}</#if></@ofbizUrl>" class="${styles.link_nav!} ${styles.action_send!}">${uiLabelMap.CommonSendEmail}</a-->
                  </div>
                <#elseif "WEB_ADDRESS" == contactMech.contactMechTypeId>
                  <div>
                    ${contactMech.infoString!}
                    <#assign openAddress = contactMech.infoString?default("")>
                    <#if !openAddress?starts_with("http") && !openAddress?starts_with("HTTP")><#assign openAddress = "http://" + openAddress></#if>
                    <a target="_blank" href="${openAddress}" class="${styles.link_nav!} ${styles.action_find!} ${styles.action_external!}">${uiLabelMap.CommonOpenPageNewWindow}</a>
                  </div>
                <#else>
                  <div>${contactMech.infoString!}</div>
                </#if>
                <div>(${uiLabelMap.CommonUpdated}:&nbsp;${partyContactMech.fromDate})</div>
                <#if partyContactMech.thruDate?has_content><div><b>${uiLabelMap.PartyContactEffectiveThru}:&nbsp;${partyContactMech.thruDate}</b></div></#if>
                <#-- create cust request -->
                <#if custRequestTypes??>
                  <form name="createCustRequestForm" action="<@ofbizUrl>createCustRequest</@ofbizUrl>" method="post" onsubmit="javascript:submitFormDisableSubmits(this)">
                    <input type="hidden" name="partyId" value="${partyId}"/>
                    <input type="hidden" name="fromPartyId" value="${partyId}"/>
                    <input type="hidden" name="fulfillContactMechId" value="${contactMech.contactMechId}"/>
                    <select name="custRequestTypeId">
                      <#list custRequestTypes as type>
                        <option value="${type.custRequestTypeId}">${type.get("description", locale)}</option>
                      </#list>
                    </select>
                    <input type="submit" class="${styles.link_run_sys!} ${styles.action_add!}" value="${uiLabelMap.PartyCreateNewCustRequest}"/>
                  </form>
                </#if>
              </@td>
              <@td valign="top"><b>(${partyContactMech.allowSolicitation!})</b></@td>
              <@td class="button-col">
                <#--if security.hasEntityPermission("PARTYMGR", "_UPDATE", session) || userLogin.partyId == partyId>
                  <a href="<@ofbizUrl>editcontactmech?partyId=${partyId}&amp;contactMechId=${contactMech.contactMechId}</@ofbizUrl>" class="${styles.link_nav!} ${styles.action_update!}">${uiLabelMap.CommonUpdate}</a>
                </#if>
                <#if security.hasEntityPermission("PARTYMGR", "_DELETE", session) || userLogin.partyId == partyId>
                  <form name="partyDeleteContact" method="post" action="<@ofbizUrl>deleteContactMech</@ofbizUrl>" onsubmit="javascript:submitFormDisableSubmits(this)">
                    <input name="partyId" value="${partyId}" type="hidden"/>
                    <input name="contactMechId" value="${contactMech.contactMechId}" type="hidden"/>
                    <input type="submit" class="${styles.link_run_sys!} ${styles.action_terminate!}" value="${uiLabelMap.CommonExpire}"/>
                  </form>
                </#if-->
              </@td>
            </@tr>
          </#list>
        </@table>
      <#else>
        <@commonMsg type="result-norecord">${uiLabelMap.PartyNoContactInformation}</@commonMsg>
      </#if>
  </@section>
  