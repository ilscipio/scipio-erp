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

<#-- SCIPIO: 2017-09-29: this can be set to show non-interactive details info; and title -->
<#assign partyInfoViewOnly = partyInfoViewOnly!false>
<#assign partyInfoSimpleFuncOnly = partyInfoSimpleFuncOnly!false>
<#assign partyContactInfoTitle = partyContactInfoTitle!uiLabelMap.PartyContactInformation>
<#assign partyContactInfoUseSection = partyContactInfoUseSection!true>

  <#-- SCIPIO: Removed
  <#macro menuContent menuArgs={}>
    <@menu args=menuArgs>
    <#if security.hasEntityPermission("PARTYMGR", "_CREATE", session) || userLogin.partyId == partyId>
      <@menuitem type="link" href=makeOfbizUrl("editcontactmech?partyId=${partyId}") text=uiLabelMap.CommonNew class="+${styles.action_nav!} ${styles.action_add!}"/>
    </#if>
    </@menu>
  </#macro>-->
  <@section id="partyContactInfo" title=partyContactInfoTitle open=partyContactInfoUseSection close=partyContactInfoUseSection>
      <#if contactMeches?has_content>
        <@table type="data-complex"> <#-- orig: class="basic-table" --> <#-- orig: cellspacing="0" -->
          <@thead>
          <@tr>
            <@th>${uiLabelMap.PartyContactType}</@th>
            <@th>${uiLabelMap.PartyContactInformation}</@th>
            <@th>${uiLabelMap.PartyContactSolicitingOk}</@th>
            <@th>&nbsp;</@th>
          </@tr>
          </@thead>
          <@tbody>
          <#list contactMeches as contactMechMap>
            <#assign contactMech = contactMechMap.contactMech>
            <#assign partyContactMech = contactMechMap.partyContactMech>
          <#if (contactMechMap_index > 0)>
            <@tr type="util"><@td colspan="4"><hr /></@td></@tr>
          </#if>
            <@tr>
              <@td>${contactMechMap.contactMechType.get("description",locale)}</@td>
              <@td>
                <#list contactMechMap.partyContactMechPurposes as partyContactMechPurpose>
                  <#assign contactMechPurposeType = partyContactMechPurpose.getRelatedOne("ContactMechPurposeType", true)>
                  <div>
                    <#if contactMechPurposeType?has_content>
                      <b>${contactMechPurposeType.get("description",locale)}</b>
                    <#else>
                      <b>${uiLabelMap.PartyMechPurposeTypeNotFound}: "${partyContactMechPurpose.contactMechPurposeTypeId}"</b>
                    </#if>
                    <#if partyContactMechPurpose.thruDate?has_content>
                      (${uiLabelMap.CommonExpire}: ${partyContactMechPurpose.thruDate})
                    </#if>
                  </div>
                </#list>
                <#if "POSTAL_ADDRESS" == contactMech.contactMechTypeId>
                  <#if contactMechMap.postalAddress?has_content>
                    <#assign postalAddress = contactMechMap.postalAddress>
                    <#assign dummy = setContextField("postalAddress", postalAddress)>
                    <@render resource="component://party/widget/partymgr/PartyScreens.xml#postalAddressHtmlFormatter" />
                    <#if postalAddress.geoPointId?has_content>
                      <#if contactMechPurposeType?has_content>
                        <#assign popUptitle = contactMechPurposeType.get("description", locale) + uiLabelMap.CommonGeoLocation>
                      </#if>
                      <a href="javascript:popUp('<@ofbizUrl>GetPartyGeoLocation?geoPointId=${postalAddress.geoPointId}&partyId=${partyId}</@ofbizUrl>', '${popUptitle!}', '450', '550')" class="${styles.link_nav!} ${styles.action_select!}">${uiLabelMap.CommonGeoLocation}</a>
                    </#if>
                  </#if>
                <#elseif "TELECOM_NUMBER" == contactMech.contactMechTypeId>
                  <#if contactMechMap.telecomNumber?has_content>
                    <#assign telecomNumber = contactMechMap.telecomNumber>
                    <div>
                      ${telecomNumber.countryCode!}
                      <#if telecomNumber.areaCode?has_content>${telecomNumber.areaCode!"000"}-</#if><#if telecomNumber.contactNumber?has_content>${telecomNumber.contactNumber?default("000-0000")}</#if>
                      <#if partyContactMech.extension?has_content>${uiLabelMap.PartyContactExt}&nbsp;${partyContactMech.extension}</#if>
                        <#--<#if !telecomNumber.countryCode?has_content || telecomNumber.countryCode == "011">
                          <a target="_blank" href="${uiLabelMap.CommonLookupAnywhoLink}" class="${styles.link_nav!} ${styles.action_find!} ${styles.action_external!}">${uiLabelMap.CommonLookupAnywho}</a>
                          <a target="_blank" href="${uiLabelMap.CommonLookupWhitepagesTelNumberLink}" class="${styles.link_nav!} ${styles.action_find!} ${styles.action_external!}">${uiLabelMap.CommonLookupWhitepages}</a>
                        </#if>-->
                    </div>
                  </#if>
                <#elseif "EMAIL_ADDRESS" == contactMech.contactMechTypeId>
                  <div>
                    ${contactMech.infoString!}
                  <#if !partyInfoViewOnly && !partyInfoSimpleFuncOnly>
                    <#assign emailFormName = 'createEmail${contactMech.infoString?replace("&#64;","")?replace("&#x40;","")?replace(".","")?replace("@","")}'>
                    <form method="post" action="<@ofbizUrl>NewDraftCommunicationEvent</@ofbizUrl>" onsubmit="javascript:submitFormDisableSubmits(this)" name="${emailFormName}">
                      <#if userLogin.partyId?has_content>
                      <input name="partyIdFrom" value="${userLogin.partyId}" type="hidden"/>
                      </#if>
                      <input name="partyIdTo" value="${partyId}" type="hidden"/>
                      <input name="contactMechIdTo" value="${contactMech.contactMechId}" type="hidden"/>
                      <input name="my" value="My" type="hidden"/>
                      <input name="statusId" value="COM_PENDING" type="hidden"/>
                      <input name="communicationEventTypeId" value="EMAIL_COMMUNICATION" type="hidden"/>
                    </form><a class="${styles.link_run_sys!} ${styles.action_send!}" href="javascript:document['${emailFormName}'].submit()">${uiLabelMap.CommonSendEmail}</a>
                  </#if>
                  </div>
                <#elseif "WEB_ADDRESS" == contactMech.contactMechTypeId>
                  <div>
                    ${contactMech.infoString!}
                    <#assign openAddress = contactMech.infoString?default("")>
                    <#if !openAddress?starts_with("http") && !openAddress?starts_with("HTTP")><#assign openAddress = "http://" + openAddress></#if>
                    <a target="_blank" href="${openAddress}" class="${styles.link_nav!} ${styles.action_view!} ${styles.action_external!}">${uiLabelMap.CommonOpenPageNewWindow}</a>
                  </div>
                <#else>
                  <div>${contactMech.infoString!}</div>
                </#if>
                <div>(${uiLabelMap.CommonUpdated}:&nbsp;${partyContactMech.fromDate})</div>
                <#if partyContactMech.thruDate?has_content><div><b>${uiLabelMap.PartyContactEffectiveThru}:&nbsp;${partyContactMech.thruDate}</b></div></#if>
                <#-- create cust request -->
                <#if custRequestTypes?? && !partyInfoViewOnly && !partyInfoSimpleFuncOnly>
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
              <@td><b>(${partyContactMech.allowSolicitation!})</b></@td>
              <@td class="button-col">
                <#if (security.hasEntityPermission("PARTYMGR", "_UPDATE", session) || userLogin.partyId == partyId) && !partyInfoViewOnly>
                  <a href="<@ofbizUrl>editcontactmech?partyId=${partyId}&amp;contactMechId=${contactMech.contactMechId}</@ofbizUrl>" class="${styles.link_nav!} ${styles.action_update!}">${uiLabelMap.CommonUpdate}</a>
                </#if>
                <#if (security.hasEntityPermission("PARTYMGR", "_DELETE", session) || userLogin.partyId == partyId) && !partyInfoViewOnly>
                  <form name="partyDeleteContact" method="post" action="<@ofbizUrl>deleteContactMech</@ofbizUrl>" onsubmit="javascript:submitFormDisableSubmits(this)">
                    <input name="partyId" value="${partyId}" type="hidden"/>
                    <input name="contactMechId" value="${contactMech.contactMechId}" type="hidden"/>
                    <input type="submit" class="${styles.link_run_sys!} ${styles.action_terminate!}" value="${uiLabelMap.CommonExpire}"/>
                  </form>
                </#if>
              </@td>
            </@tr>
          </#list>
          </@tbody>
        </@table>
      <#else>
        <@commonMsg type="result-norecord">${uiLabelMap.PartyNoContactInformation}</@commonMsg>
      </#if>
  </@section>
