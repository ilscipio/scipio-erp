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
<#if !mechMap.contactMech??>
  <#-- When creating a new contact mech, first select the type, then actually create -->
  <#if !preContactMechTypeId?has_content>
    <@section title="${uiLabelMap.PartyCreateNewContact}">
      <form method="post" action="<@ofbizUrl>editcontactmech</@ofbizUrl>" name="createcontactmechform">
        <input type="hidden" name="partyId" value="${partyId}" />
        <@field type="generic" label="${uiLabelMap.PartySelectContactType}">
            <select name="preContactMechTypeId">
              <#list mechMap.contactMechTypes as contactMechType>
                <option value="${contactMechType.contactMechTypeId}">${contactMechType.get("description",locale)}</option>
              </#list>
            </select>
        </@field>
        <@field type="submitarea">
            <a href="javascript:document.createcontactmechform.submit()" class="${styles.button_default!}">${uiLabelMap.CommonCreate}</a>
        </@field>
      </form>
    </@section>
  </#if>
</#if>
<#if mechMap.contactMechTypeId?has_content>

  <#if !mechMap.contactMech?has_content>
    <#assign sectionTitle>${uiLabelMap.PartyCreateNewContact}</#assign>
  <#else>
    <#assign sectionTitle>${uiLabelMap.PartyEditContactInformation}</#assign>
  </#if>
  <@section title=sectionTitle>
  
    <div id="mech-purpose-types">
  <#if !mechMap.contactMech?has_content>
    <#if contactMechPurposeType??>
      <p>(${uiLabelMap.PartyMsgContactHavePurpose} <b>"${contactMechPurposeType.get("description",locale)!}"</b>)</p>
    </#if>
  </#if>
  
    <#-- FIXME: form inside table -->
  <#if !mechMap.contactMech?has_content>
      <form method="post" action="<@ofbizUrl>${mechMap.requestName}</@ofbizUrl>" name="editcontactmechform" id="editcontactmechform">
        <input type="hidden" name="DONE_PAGE" value="${donePage}" />
        <input type="hidden" name="contactMechTypeId" value="${mechMap.contactMechTypeId}" />
        <input type="hidden" name="partyId" value="${partyId}" />
        <#if cmNewPurposeTypeId?has_content><input type="hidden" name="contactMechPurposeTypeId" value="${cmNewPurposeTypeId}" /></#if>
        <#if preContactMechTypeId??><input type="hidden" name="preContactMechTypeId" value="${preContactMechTypeId}" /></#if>
        <#if contactMechPurposeTypeId??><input type="hidden" name="contactMechPurposeTypeId" value="${contactMechPurposeTypeId!}" /></#if>
        <#if paymentMethodId?has_content><input type='hidden' name='paymentMethodId' value='${paymentMethodId}' /></#if>
  <#else>  
      <#if mechMap.purposeTypes?has_content>
        <@tr>
          <@td>${uiLabelMap.PartyContactPurposes}</@td>
          <@td>
            <@table type="data-list" class="basic-table" cellspacing="0">
            <@tbody>
              <#if mechMap.partyContactMechPurposes?has_content>
                <#list mechMap.partyContactMechPurposes as partyContactMechPurpose>
                  <#assign contactMechPurposeType = partyContactMechPurpose.getRelatedOne("ContactMechPurposeType", true)>
                  <@tr>
                    <@td>
                      <#if contactMechPurposeType?has_content>
                        ${contactMechPurposeType.get("description",locale)}
                      <#else>
                        ${uiLabelMap.PartyPurposeTypeNotFound}: "${partyContactMechPurpose.contactMechPurposeTypeId}"
                      </#if>
                      (${uiLabelMap.CommonSince}:${partyContactMechPurpose.fromDate.toString()})
                      <#if partyContactMechPurpose.thruDate?has_content>(${uiLabelMap.CommonExpire}: ${partyContactMechPurpose.thruDate.toString()}</#if>
                    </@td>
                    <@td class="button-col">
                      <form name="deletePartyContactMechPurpose_${partyContactMechPurpose.contactMechPurposeTypeId}" method="post" action="<@ofbizUrl>deletePartyContactMechPurpose</@ofbizUrl>" >
                         <input type="hidden" name="partyId" value="${partyId}" />
                         <input type="hidden" name="contactMechId" value="${contactMechId}" />
                         <input type="hidden" name="contactMechPurposeTypeId" value="${partyContactMechPurpose.contactMechPurposeTypeId}" />
                         <input type="hidden" name="fromDate" value="${partyContactMechPurpose.fromDate.toString()}" />
                         <input type="hidden" name="DONE_PAGE" value="${donePage?replace("=","%3d")}" />
                         <input type="hidden" name="useValues" value="true" />
                         <a href="javascript:document.deletePartyContactMechPurpose_${partyContactMechPurpose.contactMechPurposeTypeId}.submit()" class="${styles.button_default!}">${uiLabelMap.CommonDelete}</a> 
                       </form>
                    </@td>
                  </@tr>
                </#list>
              </#if>
              </@tbody>
              <@tfoot>
              <@tr>
                  <@td class="button-col">
                <form method="post" action="<@ofbizUrl>createPartyContactMechPurpose</@ofbizUrl>" name="newpurposeform">
                  <input type="hidden" name="partyId" value="${partyId}" />
                  <input type="hidden" name="DONE_PAGE" value="${donePage}" />
                  <input type="hidden" name="useValues" value="true" />
                  <input type="hidden" name="contactMechId" value="${contactMechId!}" />
                    <select name="contactMechPurposeTypeId">
                      <option></option>
                      <#list mechMap.purposeTypes as contactMechPurposeType>
                        <option value="${contactMechPurposeType.contactMechPurposeTypeId}">${contactMechPurposeType.get("description",locale)}</option>
                      </#list>
                    </select>
                </form>
                  </@td>
                <@td><a href="javascript:document.newpurposeform.submit()" class="${styles.button_default!}">${uiLabelMap.PartyAddPurpose}</a></@td>
              </@tr>
            </@tfoot>
            </@table>
          </@td>
        </@tr>
      </#if>
      <form method="post" action="<@ofbizUrl>${mechMap.requestName}</@ofbizUrl>" name="editcontactmechform" id="editcontactmechform">
        <input type="hidden" name="contactMechId" value="${contactMechId}" />
        <input type="hidden" name="contactMechTypeId" value="${mechMap.contactMechTypeId}" />
        <input type="hidden" name="partyId" value="${partyId}" />
        <input type="hidden" name="DONE_PAGE" value="${donePage!}" />
  </#if>
  <#if "POSTAL_ADDRESS" = mechMap.contactMechTypeId!>
    <@tr>
      <@td>${uiLabelMap.PartyToName}</@td>
      <@td>
        <input type="text" size="50" maxlength="100" name="toName" value="${(mechMap.postalAddress.toName)?default(request.getParameter('toName')!)}" />
      </@td>
    </@tr>
    <@tr>
      <@td>${uiLabelMap.PartyAttentionName}</@td>
      <@td>
        <input type="text" size="50" maxlength="100" name="attnName" value="${(mechMap.postalAddress.attnName)?default(request.getParameter('attnName')!)}" />
      </@td>
    </@tr>
    <@tr>
      <@td>${uiLabelMap.PartyAddressLine1} *</@td>
      <@td>
        <input type="text" size="100" maxlength="255" name="address1" value="${(mechMap.postalAddress.address1)?default(request.getParameter('address1')!)}" />
      </@td>
    </@tr>
    <@tr>
      <@td>${uiLabelMap.PartyAddressLine2}</@td>
      <@td>
        <input type="text" size="100" maxlength="255" name="address2" value="${(mechMap.postalAddress.address2)?default(request.getParameter('address2')!)}" />
      </@td>
    </@tr>
    <@tr>
      <@td>${uiLabelMap.PartyCity} *</@td>
      <@td>
        <input type="text" size="50" maxlength="100" name="city" value="${(mechMap.postalAddress.city)?default(request.getParameter('city')!)}" />
      </@td>
    </@tr>
    <@tr>
      <@td>${uiLabelMap.PartyState}</@td>
      <@td>
        <select name="stateProvinceGeoId" id="editcontactmechform_stateProvinceGeoId">
        </select>
      </@td>
    </@tr>
    <@tr>
      <@td>${uiLabelMap.PartyZipCode} *</@td>
      <@td>
        <input type="text" size="30" maxlength="60" name="postalCode" value="${(mechMap.postalAddress.postalCode)?default(request.getParameter('postalCode')!)}" />
      </@td>
    </@tr>
    <@tr>   
      <@td>${uiLabelMap.CommonCountry}</@td>
      <@td>     
        <select name="countryGeoId" id="editcontactmechform_countryGeoId">
          ${screens.render("component://common/widget/CommonScreens.xml#countries")}        
          <#if (mechMap.postalAddress??) && (mechMap.postalAddress.countryGeoId??)>
            <#assign defaultCountryGeoId = mechMap.postalAddress.countryGeoId>
          <#else>
           <#assign defaultCountryGeoId = Static["org.ofbiz.base.util.UtilProperties"].getPropertyValue("general.properties", "country.geo.id.default")>
          </#if>
          <option selected="selected" value="${defaultCountryGeoId}">
            <#assign countryGeo = delegator.findOne("Geo",Static["org.ofbiz.base.util.UtilMisc"].toMap("geoId",defaultCountryGeoId), false)>
            ${countryGeo.get("geoName",locale)}
          </option>
        </select>
      </@td>
    </@tr>
    <#assign isUsps = Static["org.ofbiz.party.contact.ContactMechWorker"].isUspsAddress(mechMap.postalAddress)>
    <@tr>
      <@td>${uiLabelMap.PartyIsUsps}</@td>
      <@td><#if isUsps>${uiLabelMap.CommonY}<#else>${uiLabelMap.CommonN}</#if>
      </@td>
    </@tr>
  <#elseif "TELECOM_NUMBER" = mechMap.contactMechTypeId!>
    <@tr>
      <@td>${uiLabelMap.PartyPhoneNumber}</@td>
      <@td>
        <input type="text" size="4" maxlength="10" name="countryCode" value="${(mechMap.telecomNumber.countryCode)?default(request.getParameter('countryCode')!)}" />
        -&nbsp;<input type="text" size="4" maxlength="10" name="areaCode" value="${(mechMap.telecomNumber.areaCode)?default(request.getParameter('areaCode')!)}" />
        -&nbsp;<input type="text" size="15" maxlength="15" name="contactNumber" value="${(mechMap.telecomNumber.contactNumber)?default(request.getParameter('contactNumber')!)}" />
        &nbsp;${uiLabelMap.PartyContactExt}&nbsp;<input type="text" size="6" maxlength="10" name="extension" value="${(mechMap.partyContactMech.extension)?default(request.getParameter('extension')!)}" />
      </@td>
    </@tr>
    <@tr>
      <@td></@td>
      <@td>[${uiLabelMap.CommonCountryCode}] [${uiLabelMap.PartyAreaCode}] [${uiLabelMap.PartyContactNumber}] [${uiLabelMap.PartyContactExt}]</@td>
    </@tr>
  <#elseif "EMAIL_ADDRESS" = mechMap.contactMechTypeId!>
    <@tr>
      <@td>${mechMap.contactMechType.get("description",locale)}</@td>
      <@td>
        <input type="text" size="60" maxlength="255" name="emailAddress" value="${(mechMap.contactMech.infoString)?default(request.getParameter('emailAddress')!)}" />
      </@td>
    </@tr>
  <#else>
    <@tr>
      <@td>${mechMap.contactMechType.get("description",locale)}</@td>
      <@td>
        <input type="text" size="60" maxlength="255" name="infoString" value="${(mechMap.contactMech.infoString)!}" />
      </@td>
    </@tr>
  </#if>
  <@tr>
    <@td>${uiLabelMap.PartyContactAllowSolicitation}?</@td>
    <@td>
      <select name="allowSolicitation">
        <#if (((mechMap.partyContactMech.allowSolicitation)!"") == "Y")><option value="Y">${uiLabelMap.CommonY}</option></#if>
        <#if (((mechMap.partyContactMech.allowSolicitation)!"") == "N")><option value="N">${uiLabelMap.CommonN}</option></#if>
        <option></option>
        <option value="Y">${uiLabelMap.CommonY}</option>
        <option value="N">${uiLabelMap.CommonN}</option>
      </select>
    </@td>
  </@tr>
  </form>
  </div>
  
  <@menu type="button">
    <@menuitem type="link" ofbizHref="backHome" text="${uiLabelMap.CommonGoBack}" />
    <@menuitem type="link" href="javascript:document.editcontactmechform.submit()" text="${uiLabelMap.CommonSave}" />
  </@menu>
  </@section>
<#else>
  <@menu type="button">
    <@menuitem type="link" ofbizHref="backHome" text="${uiLabelMap.CommonGoBack}" />
  </@menu>
</#if>
