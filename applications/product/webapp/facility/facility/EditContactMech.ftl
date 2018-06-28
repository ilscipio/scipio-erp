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
    <@menuitem type="link" href=makeOfbizUrl("authview/${donePage}?facilityId=${facilityId}") text=uiLabelMap.CommonGoBack class="+${styles.action_nav!} ${styles.action_cancel!}" />
  <#if (mechMap.contactMechTypeId)?has_content && (mechMap.contactMech)?has_content>
    <@menuitem type="link" href=makeOfbizUrl("EditContactMech?facilityId=${facilityId}") text=uiLabelMap.ProductNewContactMech class="+${styles.action_nav!} ${styles.action_add!}" />
  </#if>
  </@menu>
</#macro>
<@section menuContent=menuContent>

<#if !mechMap.facilityContactMech?? && mechMap.contactMech??>
  <@commonMsg type="error">${uiLabelMap.PartyContactInfoNotBelongToPartyOrExpired}</@commonMsg><#-- SCIPIO: clearer message -->
<#else>
  <#if !mechMap.contactMech??>
    <#-- When creating a new contact mech, first select the type, then actually create -->
    <#if !preContactMechTypeId?has_content>

    <form method="post" action="<@ofbizUrl>EditContactMech</@ofbizUrl>" name="createcontactmechform">
      <input type="hidden" name="facilityId" value="${facilityId}" />
      <input type="hidden" name="DONE_PAGE" value="${donePage!}" />
    <@row>
      <@cell columns=9>
        <@field type="select" label=uiLabelMap.PartySelectContactType name="preContactMechTypeId">
          <#list mechMap.contactMechTypes as contactMechType>
            <option value="${contactMechType.contactMechTypeId}">${contactMechType.get("description",locale)}</option>
          </#list>
        </@field>
      </@cell>
      <@cell columns=3>
        <@field type="submit" submitType="link" href="javascript:document.createcontactmechform.submit()" class="+${styles.link_run_sys!} ${styles.action_add!}" text=uiLabelMap.CommonCreate />
      </@cell>
    </@row>
    </form>
    </#if>
  </#if>

  <#if mechMap.contactMechTypeId?has_content>
    <#if !mechMap.contactMech?has_content>
      <#if contactMechPurposeType??>
        <div><span>(${uiLabelMap.PartyMsgContactHavePurpose}</span>"${contactMechPurposeType.get("description",locale)!}")</div>
      </#if>
    </#if>
      
    <#if !mechMap.contactMech?has_content>
        <form method="post" action="<@ofbizUrl>${mechMap.requestName}</@ofbizUrl>" name="editcontactmechform" id="editcontactmechform">
        <input type="hidden" name="DONE_PAGE" value="${donePage}" />
        <input type="hidden" name="contactMechTypeId" value="${mechMap.contactMechTypeId}" />
        <input type="hidden" name="facilityId" value="${facilityId}" />
        <#if preContactMechTypeId??><input type="hidden" name="preContactMechTypeId" value="${preContactMechTypeId}" /></#if>
        <#if contactMechPurposeTypeId??><input type="hidden" name="contactMechPurposeTypeId" value="${contactMechPurposeTypeId!}" /></#if>

        <#if paymentMethodId??><input type="hidden" name="paymentMethodId" value="${paymentMethodId}" /></#if>

        <@field type="select" label=uiLabelMap.PartyContactPurposes name="contactMechPurposeTypeId" required=true>
            <option></option>
            <#list mechMap.purposeTypes as contactMechPurposeType>
              <option value="${contactMechPurposeType.contactMechPurposeTypeId}">${contactMechPurposeType.get("description",locale)}</option>
             </#list>
        </@field>
    <#else>
        <#if mechMap.purposeTypes?has_content>
        <@field type="generic" label=uiLabelMap.PartyContactPurposes>
          <@fields type="default-manual-widgetonly" ignoreParentField=true>
            <@table type="data-list" autoAltRows=true> <#-- orig: class="basic-table" --> <#-- orig: cellspacing="0" -->
            <#if mechMap.facilityContactMechPurposes?has_content>
              <#list mechMap.facilityContactMechPurposes as facilityContactMechPurpose>
                <#assign contactMechPurposeType = facilityContactMechPurpose.getRelatedOne("ContactMechPurposeType", true)>
                <@tr valign="middle">
                  <@td>
                      <#if contactMechPurposeType?has_content>
                        <b>${contactMechPurposeType.get("description",locale)}</b>
                      <#else>
                        <b>${uiLabelMap.PartyMechPurposeTypeNotFound}: "${facilityContactMechPurpose.contactMechPurposeTypeId}"</b>
                      </#if>
                      (${uiLabelMap.CommonSince}: ${facilityContactMechPurpose.fromDate})
                      <#if facilityContactMechPurpose.thruDate?has_content>(${uiLabelMap.CommonExpires}: ${facilityContactMechPurpose.thruDate.toString()}</#if>
                      <a href="javascript:document.getElementById('deleteFacilityContactMechPurpose_${facilityContactMechPurpose_index}').submit();" class="${styles.link_run_sys!} ${styles.action_remove!}">${uiLabelMap.CommonDelete}</a>
                  
                    <form id="deleteFacilityContactMechPurpose_${facilityContactMechPurpose_index}" method="post" action="<@ofbizUrl>deleteFacilityContactMechPurpose</@ofbizUrl>">
                      <input type="hidden" name="facilityId" value="${facilityId!}" />
                      <input type="hidden" name="contactMechId" value="${contactMechId!}" />
                      <input type="hidden" name="contactMechPurposeTypeId" value="${(facilityContactMechPurpose.contactMechPurposeTypeId)!}" />
                      <input type="hidden" name="fromDate" value="${(facilityContactMechPurpose.fromDate)!}" />
                      <input type="hidden" name="DONE_PAGE" value="${donePage!}" />
                      <input type="hidden" name="useValues" value="true" />
                    </form>
                  </@td>
                </@tr>
              </#list>
              </#if>
            <@tfoot>
              <@tr>
                <@td>
                  <form method="post" action="<@ofbizUrl>createFacilityContactMechPurpose?DONE_PAGE=${donePage}&amp;useValues=true</@ofbizUrl>" name="newpurposeform">
                  <input type="hidden" name="facilityId" value="${facilityId}" />
                  <input type="hidden" name="contactMechId" value="${contactMechId!}" />
                    <@field type="select" name="contactMechPurposeTypeId">
                      <option></option>
                      <#list mechMap.purposeTypes as contactMechPurposeType>
                        <option value="${contactMechPurposeType.contactMechPurposeTypeId}">${contactMechPurposeType.get("description",locale)}</option>
                      </#list>
                    </@field>
                    &nbsp;<a href="javascript:document.newpurposeform.submit()" class="${styles.link_run_sys!} ${styles.action_add!}">${uiLabelMap.PartyAddPurpose}</a>
                  </form>
                </@td>
              </@tr>
              </@tfoot>
            </@table>
          </@fields>
        </@field>
        </#if>
        <form method="post" action="<@ofbizUrl>${mechMap.requestName}</@ofbizUrl>" name="editcontactmechform" id="editcontactmechform">
        <input type="hidden" name="contactMechId" value="${contactMechId}" />
        <input type="hidden" name="contactMechTypeId" value="${mechMap.contactMechTypeId}" />
        <input type="hidden" name="facilityId" value="${facilityId}" />
    </#if>

  <#if "POSTAL_ADDRESS" == (mechMap.contactMechTypeId!)>
    <@field type="input" label=uiLabelMap.PartyToName size="30" maxlength="60" name="toName" value=(mechMap.postalAddress.toName)!(request.getParameter('toName')!) />
    <@field type="input" label=uiLabelMap.PartyAttentionName size="30" maxlength="60" name="attnName" value=(mechMap.postalAddress.attnName)!(request.getParameter('attnName')!) />
    <@field type="input" label=uiLabelMap.PartyAddressLine1 required=true size="30" maxlength="30" name="address1" value=(mechMap.postalAddress.address1)!(request.getParameter('address1')!) />
    <@field type="input" label=uiLabelMap.PartyAddressLine2 size="30" maxlength="30" name="address2" value=(mechMap.postalAddress.address2)!(request.getParameter('address2')!) />
    <@field type="input" label=uiLabelMap.PartyCity required=true size="30" maxlength="30" name="city" value=(mechMap.postalAddress.city)!(request.getParameter('city')!) />

    <@field type="select" label=uiLabelMap.PartyState name="stateProvinceGeoId" id="editcontactmechform_stateProvinceGeoId">
        <#if (mechMap.postalAddress.stateProvinceGeoId)?has_content>
            <option value="${mechMap.postalAddress.stateProvinceGeoId}">${mechMap.postalAddress.stateProvinceGeoId}</option>
        </#if>
    </@field>
    <@field type="input" label=uiLabelMap.PartyZipCode required=true size="12" maxlength="10" name="postalCode" value=(mechMap.postalAddress.postalCode)!(request.getParameter('postalCode')!) />
    <@field type="select" label=uiLabelMap.CommonCountry name="countryGeoId" id="editcontactmechform_countryGeoId">
        <@render resource="component://common/widget/CommonScreens.xml#countries" ctxVars={"currentCountryGeoId":(mechMap.postalAddress.countryGeoId)!, "countriesPreselectFirst":true}/>
    </@field>
  <#elseif "TELECOM_NUMBER" == (mechMap.contactMechTypeId!)>
    <@field type="generic" label=uiLabelMap.PartyPhoneNumber>
        <@field type="input" inline=true size="4" maxlength="10" name="countryCode" value=(mechMap.telecomNumber.countryCode)!(request.getParameter('countryCode')!) />
        -&nbsp;<@field type="input" inline=true size="4" maxlength="10" name="areaCode" value=(mechMap.telecomNumber.areaCode)!(request.getParameter('areaCode')!) />
        -&nbsp;<@field type="input" inline=true size="15" maxlength="15" name="contactNumber" value=(mechMap.telecomNumber.contactNumber)!(request.getParameter('contactNumber')!) />
        &nbsp;ext&nbsp;<@field type="input" inline=true size="6" maxlength="10" name="extension" value=(mechMap.facilityContactMech.extension)!(request.getParameter('extension')!) />
    </@field>
    <@field type="display">
        [${uiLabelMap.CommonCountryCode}] [${uiLabelMap.PartyAreaCode}] [${uiLabelMap.PartyContactNumber}] [${uiLabelMap.PartyExtension}]
    </@field>
  <#elseif "EMAIL_ADDRESS" == (mechMap.contactMechTypeId!)>
    <@field type="input" label=(uiLabelMap.PartyEmailAddress!) required=true size="60" maxlength="255" name="emailAddress" value=(mechMap.contactMech.infoString)?default(request.getParameter('emailAddress')!) />
  <#else>
    <@field type="input" label=(mechMap.contactMechType.get('description',locale)!) required=true size="60" maxlength="255" name="infoString" value=((mechMap.contactMech.infoString)!) />
  </#if>
    <@field type="submit" submitType="link" href="javascript:document.editcontactmechform.submit()" class="+${styles.link_run_sys!} ${styles.action_update!}" text=uiLabelMap.CommonSave />
  </form>
  </#if>
</#if>

</@section>