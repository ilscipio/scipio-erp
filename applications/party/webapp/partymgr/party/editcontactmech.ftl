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
            <@input type="select" name="preContactMechTypeId">
              <#list mechMap.contactMechTypes as contactMechType>
                <option value="${contactMechType.contactMechTypeId}">${contactMechType.get("description",locale)}</option>
              </#list>
            </@input>
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
        <@field type="generic" label="${uiLabelMap.PartyContactPurposes}">
          <@table type="data-list" cellspacing="0"> <#-- orig: class="basic-table" -->
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
                    <@input type="select" name="contactMechPurposeTypeId">
                      <option></option>
                      <#list mechMap.purposeTypes as contactMechPurposeType>
                        <option value="${contactMechPurposeType.contactMechPurposeTypeId}">${contactMechPurposeType.get("description",locale)}</option>
                      </#list>
                    </@input>
                  </form>
                </@td>
                <@td><a href="javascript:document.newpurposeform.submit()" class="${styles.button_default!}">${uiLabelMap.PartyAddPurpose}</a></@td>
              </@tr>
            </@tfoot>
          </@table>
        </@field>
      </#if>
      <form method="post" action="<@ofbizUrl>${mechMap.requestName}</@ofbizUrl>" name="editcontactmechform" id="editcontactmechform">
        <input type="hidden" name="contactMechId" value="${contactMechId}" />
        <input type="hidden" name="contactMechTypeId" value="${mechMap.contactMechTypeId}" />
        <input type="hidden" name="partyId" value="${partyId}" />
        <input type="hidden" name="DONE_PAGE" value="${donePage!}" />
  </#if>
  <#if "POSTAL_ADDRESS" = mechMap.contactMechTypeId!>
    <@field type="generic" label="${uiLabelMap.PartyToName}">
        <input type="text" size="50" maxlength="100" name="toName" value="${(mechMap.postalAddress.toName)?default(request.getParameter('toName')!)}" />
    </@field>
    <@field type="generic" label="${uiLabelMap.PartyAttentionName}">
        <input type="text" size="50" maxlength="100" name="attnName" value="${(mechMap.postalAddress.attnName)?default(request.getParameter('attnName')!)}" />
    </@field>
    <@field type="generic" label="${uiLabelMap.PartyAddressLine1} *">
        <input type="text" size="100" maxlength="255" name="address1" value="${(mechMap.postalAddress.address1)?default(request.getParameter('address1')!)}" />
    </@field>
    <@field type="generic" label="${uiLabelMap.PartyAddressLine2}">
        <input type="text" size="100" maxlength="255" name="address2" value="${(mechMap.postalAddress.address2)?default(request.getParameter('address2')!)}" />
    </@field>
    <@field type="generic" label="${uiLabelMap.PartyCity} *">
        <input type="text" size="50" maxlength="100" name="city" value="${(mechMap.postalAddress.city)?default(request.getParameter('city')!)}" />
    </@field>
    <@field type="generic" label="${uiLabelMap.PartyState}">
        <@input type="select" name="stateProvinceGeoId" id="editcontactmechform_stateProvinceGeoId">
        </@input>
    </@field>
    <@field type="generic" label="${uiLabelMap.PartyZipCode} *">
        <input type="text" size="30" maxlength="60" name="postalCode" value="${(mechMap.postalAddress.postalCode)?default(request.getParameter('postalCode')!)}" />
    </@field>
    <@field type="generic" label="${uiLabelMap.CommonCountry}">
        <@input type="select" name="countryGeoId" id="editcontactmechform_countryGeoId">
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
        </@input>
    </@field>
    <#assign isUsps = Static["org.ofbiz.party.contact.ContactMechWorker"].isUspsAddress(mechMap.postalAddress)>
    <@field type="display" label="${uiLabelMap.PartyIsUsps}">
        <#if isUsps>${uiLabelMap.CommonY}<#else>${uiLabelMap.CommonN}</#if>
    </@field>
  <#elseif "TELECOM_NUMBER" = mechMap.contactMechTypeId!>
    <@field type="generic" label="${uiLabelMap.PartyPhoneNumber}">
        <input type="text" size="4" maxlength="10" name="countryCode" value="${(mechMap.telecomNumber.countryCode)?default(request.getParameter('countryCode')!)}" />
        -&nbsp;<input type="text" size="4" maxlength="10" name="areaCode" value="${(mechMap.telecomNumber.areaCode)?default(request.getParameter('areaCode')!)}" />
        -&nbsp;<input type="text" size="15" maxlength="15" name="contactNumber" value="${(mechMap.telecomNumber.contactNumber)?default(request.getParameter('contactNumber')!)}" />
        &nbsp;${uiLabelMap.PartyContactExt}&nbsp;<input type="text" size="6" maxlength="10" name="extension" value="${(mechMap.partyContactMech.extension)?default(request.getParameter('extension')!)}" />
    </@field>
    <@field type="generic" label="">
        [${uiLabelMap.CommonCountryCode}] [${uiLabelMap.PartyAreaCode}] [${uiLabelMap.PartyContactNumber}] [${uiLabelMap.PartyContactExt}]
    </@field>
  <#elseif "EMAIL_ADDRESS" = mechMap.contactMechTypeId!>
    <@field type="generic" label="${mechMap.contactMechType.get('description',locale)}">
        <input type="text" size="60" maxlength="255" name="emailAddress" value="${(mechMap.contactMech.infoString)?default(request.getParameter('emailAddress')!)}" />
    </@field>
  <#else>
    <@field type="generic" label="${mechMap.contactMechType.get('description',locale)}">
        <input type="text" size="60" maxlength="255" name="infoString" value="${(mechMap.contactMech.infoString)!}" />
    </@field>
  </#if>
  <@field type="generic" label="${uiLabelMap.PartyContactAllowSolicitation}?">
      <@input type="select" name="allowSolicitation">
        <#if (((mechMap.partyContactMech.allowSolicitation)!"") == "Y")><option value="Y">${uiLabelMap.CommonY}</option></#if>
        <#if (((mechMap.partyContactMech.allowSolicitation)!"") == "N")><option value="N">${uiLabelMap.CommonN}</option></#if>
        <option></option>
        <option value="Y">${uiLabelMap.CommonY}</option>
        <option value="N">${uiLabelMap.CommonN}</option>
      </@input>
  </@field>
  </form>
  </div>
  
    <@menu type="button">
      <@menuitem type="link" href=makeOfbizUrl("backHome") text="${uiLabelMap.CommonGoBack}" />
      <@menuitem type="link" href="javascript:document.editcontactmechform.submit()" text="${uiLabelMap.CommonSave}" />
    </@menu>
  </@section>
<#else>
  <@menu type="button">
    <@menuitem type="link" href=makeOfbizUrl("backHome") text="${uiLabelMap.CommonGoBack}" />
  </@menu>
</#if>
