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
    <@section title=uiLabelMap.PartyCreateNewContact>
      <form method="post" action="<@ofbizUrl>editcontactmech</@ofbizUrl>" name="createcontactmechform">
        <input type="hidden" name="partyId" value="${partyId}" />
        <@field type="select" label=uiLabelMap.PartySelectContactType name="preContactMechTypeId">
              <#list mechMap.contactMechTypes as contactMechType>
                <option value="${contactMechType.contactMechTypeId}">${contactMechType.get("description",locale)}</option>
              </#list>
        </@field>
        <@field type="submit" submitType="link" href="javascript:document.createcontactmechform.submit()" class="+${styles.link_run_sys!} ${styles.action_add!}" text=uiLabelMap.CommonCreate />
      </form>
    </@section>
  </#if>
</#if>
<#if mechMap.contactMechTypeId?has_content>

  <#if !mechMap.contactMech?has_content>
    <#assign sectionTitle = uiLabelMap.PartyCreateNewContact />
  <#else>
    <#assign sectionTitle = uiLabelMap.PartyEditContactInformation />
  </#if>
  <@section title=sectionTitle>
  
    <div id="mech-purpose-types">
  <#if !mechMap.contactMech?has_content>
    <#if contactMechPurposeType??>
      <p>(${uiLabelMap.PartyMsgContactHavePurpose} <b>"${contactMechPurposeType.get("description",locale)!}"</b>)</p>
    </#if>
  </#if>
  
  <#if !mechMap.contactMech?has_content>
      <form method="post" action="<@ofbizUrl>${mechMap.requestName}</@ofbizUrl>" name="editcontactmechform" id="editcontactmechform">
        <input type="hidden" name="DONE_PAGE" value="${donePage}" />
        <input type="hidden" name="contactMechTypeId" value="${mechMap.contactMechTypeId}" />
        <input type="hidden" name="partyId" value="${partyId}" />
        <#if cmNewPurposeTypeId?has_content><input type="hidden" name="contactMechPurposeTypeId" value="${cmNewPurposeTypeId}" /></#if>
        <#if preContactMechTypeId??><input type="hidden" name="preContactMechTypeId" value="${preContactMechTypeId}" /></#if>
        <#if contactMechPurposeTypeId??><input type="hidden" name="contactMechPurposeTypeId" value="${contactMechPurposeTypeId!}" /></#if>
        <#if paymentMethodId?has_content><input type="hidden" name="paymentMethodId" value="${paymentMethodId}" /></#if>
  <#else>  
      <#if mechMap.purposeTypes?has_content>
        <@field type="generic" label=uiLabelMap.PartyContactPurposes>
          <@fields type="default-manual" ignoreParentField=true>
          <@table type="data-list"> <#-- orig: class="basic-table" --> <#-- orig: cellspacing="0" -->
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
                      <#-- SCIPIO: 2017-10-10: formerly this was: name="deletePartyContactMechPurpose_${partyContactMechPurpose.contactMechPurposeTypeId}"
                          but some states may cause duplicate purpose records, and then can't delete them, so must use index instead -->
                      <form name="deletePartyContactMechPurpose_${partyContactMechPurpose_index}" method="post" action="<@ofbizUrl>deletePartyContactMechPurpose</@ofbizUrl>" >
                         <input type="hidden" name="partyId" value="${partyId}" />
                         <input type="hidden" name="contactMechId" value="${contactMechId}" />
                         <input type="hidden" name="contactMechPurposeTypeId" value="${partyContactMechPurpose.contactMechPurposeTypeId}" />
                         <input type="hidden" name="fromDate" value="${partyContactMechPurpose.fromDate.toString()}" />
                         <input type="hidden" name="DONE_PAGE" value="${donePage?replace("=","%3d")}" />
                         <input type="hidden" name="useValues" value="true" />
                         <a href="javascript:document.deletePartyContactMechPurpose_${partyContactMechPurpose_index}.submit()" class="${styles.link_run_sys!} ${styles.action_remove!}">${uiLabelMap.CommonDelete}</a> 
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
                    <@field type="select" name="contactMechPurposeTypeId" widgetOnly=true>
                      <option></option>
                      <#list mechMap.purposeTypes as contactMechPurposeType>
                        <option value="${contactMechPurposeType.contactMechPurposeTypeId}">${contactMechPurposeType.get("description",locale)}</option>
                      </#list>
                    </@field>
                  </form>
                </@td>
                <@td><@field type="submit" submitType="link" href="javascript:document.newpurposeform.submit()" class="+${styles.link_run_sys!} ${styles.action_add!}" text=uiLabelMap.PartyAddPurpose /></@td>
              </@tr>
            </@tfoot>
          </@table>
          </@fields>
        </@field>
      </#if>
      <form method="post" action="<@ofbizUrl>${mechMap.requestName}</@ofbizUrl>" name="editcontactmechform" id="editcontactmechform">
        <input type="hidden" name="contactMechId" value="${contactMechId}" />
        <input type="hidden" name="contactMechTypeId" value="${mechMap.contactMechTypeId}" />
        <input type="hidden" name="partyId" value="${partyId}" />
        <input type="hidden" name="DONE_PAGE" value="${donePage!}" />
  </#if>
  <#if "POSTAL_ADDRESS" == (mechMap.contactMechTypeId!)>
    <@field type="input" label=uiLabelMap.PartyToName size="50" maxlength="100" name="toName" value=(mechMap.postalAddress.toName)?default(request.getParameter('toName')!) />
    <@field type="input" label=uiLabelMap.PartyAttentionName size="50" maxlength="100" name="attnName" value=(mechMap.postalAddress.attnName)?default(request.getParameter('attnName')!) />
    <@field type="input" label=uiLabelMap.PartyAddressLine1 required=true size="100" maxlength="255" name="address1" value=(mechMap.postalAddress.address1)?default(request.getParameter('address1')!) />
    <@field type="input" label=uiLabelMap.PartyAddressLine2 size="100" maxlength="255" name="address2" value=(mechMap.postalAddress.address2)?default(request.getParameter('address2')!) />
    <@field type="input" label=uiLabelMap.PartyCity required=true size="50" maxlength="100" name="city" value=(mechMap.postalAddress.city)?default(request.getParameter('city')!) />
    <@field type="select" label=uiLabelMap.PartyState name="stateProvinceGeoId" id="editcontactmechform_stateProvinceGeoId">
        <#if (mechMap.postalAddress.stateProvinceGeoId)?has_content>
          <option value="${mechMap.postalAddress.stateProvinceGeoId}">${mechMap.postalAddress.stateProvinceGeoId}</option>
        </#if>
    </@field>
    <@field type="input" label=uiLabelMap.PartyZipCode required=true size="30" maxlength="60" name="postalCode" value=(mechMap.postalAddress.postalCode)?default(request.getParameter('postalCode')!) />
    <@field type="select" label=uiLabelMap.CommonCountry name="countryGeoId" id="editcontactmechform_countryGeoId">
        <@render resource="component://common/widget/CommonScreens.xml#countries" ctxVars={"currentCountryGeoId":(mechMap.postalAddress.countryGeoId)!, "countriesPreselectFirst":true} />
    </@field>
    <#assign isUsps = Static["org.ofbiz.party.contact.ContactMechWorker"].isUspsAddress(mechMap.postalAddress)>
    <@field type="display" label=uiLabelMap.PartyIsUsps>
        <#if isUsps>${uiLabelMap.CommonY}<#else>${uiLabelMap.CommonN}</#if>
    </@field>
  <#elseif "TELECOM_NUMBER" == (mechMap.contactMechTypeId!)>
    <@field type="generic" label=uiLabelMap.PartyPhoneNumber>
        <@field type="input" inline=true size="4" maxlength="10" name="countryCode" value=(mechMap.telecomNumber.countryCode)?default(request.getParameter('countryCode')!) />
        -&nbsp;<@field type="input" inline=true size="4" maxlength="10" name="areaCode" value=(mechMap.telecomNumber.areaCode)?default(request.getParameter('areaCode')!) />
        -&nbsp;<@field type="input" inline=true size="15" maxlength="15" name="contactNumber" value=(mechMap.telecomNumber.contactNumber)?default(request.getParameter('contactNumber')!) />
        &nbsp;${uiLabelMap.PartyContactExt}&nbsp;<@field type="input" inline=true size="6" maxlength="10" name="extension" value=(mechMap.partyContactMech.extension)?default(request.getParameter('extension')!) />
    </@field>
    <@field type="display" label="">
        [${uiLabelMap.CommonCountryCode}] [${uiLabelMap.PartyAreaCode}] [${uiLabelMap.PartyContactNumber}] [${uiLabelMap.PartyContactExt}]
    </@field>
  <#elseif "EMAIL_ADDRESS" == (mechMap.contactMechTypeId!)>
    <@field type="input" label=(mechMap.contactMechType.get('description',locale)) size="60" maxlength="255" name="emailAddress" value=((mechMap.contactMech.infoString)!(request.getParameter('emailAddress')!)) />
  <#else>
    <@field type="input" label=(mechMap.contactMechType.get('description',locale)) size="60" maxlength="255" name="infoString" value=((mechMap.contactMech.infoString)!) />
  </#if>
  <@field type="select" label="${rawLabel('PartyContactAllowSolicitation')}?" name="allowSolicitation">
      <#if (((mechMap.partyContactMech.allowSolicitation)!"") == "Y")><option value="Y">${uiLabelMap.CommonY}</option></#if>
      <#if (((mechMap.partyContactMech.allowSolicitation)!"") == "N")><option value="N">${uiLabelMap.CommonN}</option></#if>
      <option></option>
      <option value="Y">${uiLabelMap.CommonY}</option>
      <option value="N">${uiLabelMap.CommonN}</option>
  </@field>
  </form>
  </div>
  
    <@field type="submitarea">
      <@field type="submit" submitType="link" href=makeOfbizUrl("backHome") text=uiLabelMap.CommonGoBack class="+${styles.link_nav_cancel!}" />
      <@field type="submit" submitType="link" href="javascript:document.editcontactmechform.submit()" text=uiLabelMap.CommonSave class="+${styles.link_run_sys!} ${styles.action_update!}" />
    </@field>
  </@section>
<#else>
    <@field type="submitarea">
      <@field type="submit" submitType="link" href=makeOfbizUrl("backHome") text=uiLabelMap.CommonGoBack class="+${styles.link_nav_cancel!}" />
    </@field>
</#if>
