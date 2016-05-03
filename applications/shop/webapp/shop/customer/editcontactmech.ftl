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
<#include "customercommon.ftl">

<#assign requireCreate = false>
<#if canNotView>
  <@commonMsg type="error-perm">${uiLabelMap.PartyContactInfoNotBelongToYou}.</@commonMsg>
  <@menu type="button">
    <@menuitem type="link" href=makeOfbizUrl("${donePage}") class="+${styles.action_nav!} ${styles.action_cancel!}" text=uiLabelMap.CommonGoBack />
  </@menu>
<#else>
  <#if !contactMech??>
    <#-- When creating a new contact mech, first select the type, then actually create -->
    <#if !requestParameters.preContactMechTypeId?? && !preContactMechTypeId??>
      <#assign requireCreate = true>
      <@section title=uiLabelMap.PartyCreateNewContactInfo>
        <form method="post" action="<@ofbizUrl>editcontactmechnosave?DONE_PAGE=${donePage}</@ofbizUrl>" name="createcontactmechform">
            <@field type="select" label="${uiLabelMap.PartySelectContactType}" name="preContactMechTypeId">
              <#list contactMechTypes as contactMechType>
                <option value="${contactMechType.contactMechTypeId}">${contactMechType.get("description",locale)}</option>
              </#list>
            </@field>
          <#-- Cato: make it part of menu further below, otherwise looks strange
            <@field type="submit" submitType="link" href="javascript:document.createcontactmechform.submit()" class="${styles.link_run_sys!} ${styles.action_add!}" text=uiLabelMap.CommonCreate />
          -->
        </form>
      <#--<@commonMsg type="error">ERROR: Contact information with ID "${contactMechId}" not found!</@commonMsg>-->
      </@section>
    </#if>
  </#if>

  <#if contactMechTypeId??>
  
<#macro menuContent menuArgs={}>
  <@menu args=menuArgs>
    <@menuitem type="link" href=makeOfbizUrl("${donePage}") class="+${styles.action_nav!} ${styles.action_cancel!}" text=uiLabelMap.CommonGoBack />
    <@menuitem type="link" href="javascript:document.editcontactmechform.submit()" class="+${styles.action_run_sys!} ${styles.action_update!}" text=uiLabelMap.CommonSave />
  </@menu>
</#macro>
<#assign sectionTitle><#if !contactMech??>${uiLabelMap.PartyCreateNewContactInfo}<#else><#-- Cato: duplicate: ${uiLabelMap.PartyEditContactInfo}--></#if></#assign>
<@section title=sectionTitle menuContent=menuContent menuLayoutGeneral="bottom">
    
    <#if contactMech??>
        <@field type="generic" label=uiLabelMap.PartyContactPurposes>
          <@fields type="default-manual-widgetonly" ignoreParentField=true>
            <@table type="data-complex">
              <#list (partyContactMechPurposes!) as partyContactMechPurpose>
                <#assign contactMechPurposeType = partyContactMechPurpose.getRelatedOne("ContactMechPurposeType", true) />
                <@tr>
                  <@td>
                    <#if contactMechPurposeType??>
                      ${contactMechPurposeType.get("description",locale)}
                    <#else>
                      ${uiLabelMap.PartyPurposeTypeNotFound}: "${partyContactMechPurpose.contactMechPurposeTypeId}"
                    </#if>
                     (${uiLabelMap.CommonSince}: ${partyContactMechPurpose.fromDate.toString()})
                    <#if partyContactMechPurpose.thruDate??>(${uiLabelMap.CommonExpires}:${partyContactMechPurpose.thruDate.toString()})</#if>
                  </@td>
                  <@td>
                      <form name="deletePartyContactMechPurpose_${partyContactMechPurpose.contactMechPurposeTypeId}" method="post" action="<@ofbizUrl>deletePartyContactMechPurpose?DONE_PAGE=${donePage}</@ofbizUrl>">
                          <input type="hidden" name="contactMechId" value="${contactMechId}"/>
                          <input type="hidden" name="contactMechPurposeTypeId" value="${partyContactMechPurpose.contactMechPurposeTypeId}"/>
                          <input type="hidden" name="fromDate" value="${partyContactMechPurpose.fromDate}"/>
                          <input type="hidden" name="useValues" value="true"/>
                          <@field type="submit" submitType="link" href="javascript:document.deletePartyContactMechPurpose_${partyContactMechPurpose.contactMechPurposeTypeId}.submit()" class="${styles.link_run_sys!} ${styles.action_remove!}" text=uiLabelMap.CommonDelete /></a>
                      </form> 
                  </@td>
                </@tr>
              </#list>
              <#if purposeTypes?has_content>
                <@tr>
                  <@td>
                    <form method="post" action="<@ofbizUrl>createPartyContactMechPurpose?DONE_PAGE=${donePage}</@ofbizUrl>" name="newpurposeform">
                      <input type="hidden" name="contactMechId" value="${contactMechId}"/>
                      <input type="hidden" name="useValues" value="true"/>
                        <@field type="select" name="contactMechPurposeTypeId">
                          <option></option>
                          <#list purposeTypes as contactMechPurposeType>
                            <option value="${contactMechPurposeType.contactMechPurposeTypeId}">${contactMechPurposeType.get("description",locale)}</option>
                          </#list>
                        </@field>
                    </form>
                  </@td>
                  <@td><@field type="submit" submitType="link" href="javascript:document.newpurposeform.submit()" class="${styles.link_run_sys!} ${styles.action_add!}" text=uiLabelMap.PartyAddPurpose /></@td>
                </@tr>
              </#if>
            </@table>
          </@fields>
        </@field>
    </#if>
    
  <form method="post" action="<@ofbizUrl>${reqName}?DONE_PAGE=${donePage}</@ofbizUrl>" name="editcontactmechform" id="editcontactmechform">
    
    <#if !contactMech??>
      <input type="hidden" name="contactMechTypeId" value="${contactMechTypeId}" />
      <#if contactMechPurposeType??>
        <p>(${uiLabelMap.PartyNewContactHavePurpose} "${contactMechPurposeType.get("description",locale)!}")</p>
      </#if>
      <#if cmNewPurposeTypeId?has_content><input type="hidden" name="contactMechPurposeTypeId" value="${cmNewPurposeTypeId}" /></#if>
      <#if preContactMechTypeId?has_content><input type="hidden" name="preContactMechTypeId" value="${preContactMechTypeId}" /></#if>
      <#if paymentMethodId?has_content><input type="hidden" name="paymentMethodId" value="${paymentMethodId}" /></#if>
    <#else>
      <input type="hidden" name="contactMechId" value="${contactMechId}" />
      <input type="hidden" name="contactMechTypeId" value="${contactMechTypeId}" />
    </#if>

    <#if contactMechTypeId == "POSTAL_ADDRESS">
      <#-- Cato: Delegated -->
        <@render resource="component://shop/widget/CustomerScreens.xml#postalAddressFields" 
            ctxVars={
              "pafFieldNamePrefix":"",
              "pafUseScripts":false, <#-- NOTE: false because another script takes care of it here for us --> 
              "pafFieldIdPrefix":"editcontactmechform_"
              }/>
    <#elseif contactMechTypeId == "TELECOM_NUMBER">
      <@field type="generic" label="${uiLabelMap.PartyPhoneNumber}">
          <@field type="input" inline=true size="4" maxlength="10" name="countryCode" value=(telecomNumberData.countryCode!) tooltip=uiLabelMap.CommonCountryCode />
          -&nbsp;<@field type="input" inline=true size="4" maxlength="10" name="areaCode" value=(telecomNumberData.areaCode!) tooltip=uiLabelMap.PartyAreaCode />
          -&nbsp;<@field type="input" inline=true size="15" maxlength="15" name="contactNumber" value=(telecomNumberData.contactNumber!) tooltip=uiLabelMap.PartyContactNumber />
          &nbsp;${uiLabelMap.PartyContactExt}&nbsp;<@field type="input" inline=true size="6" maxlength="10" name="extension" value=(partyContactMechData.extension!) tooltip=uiLabelMap.PartyExtension />
      </@field>
      <#-- Cato: use tooltips
      <@field type="display">
          [${uiLabelMap.CommonCountryCode}] [${uiLabelMap.PartyAreaCode}] [${uiLabelMap.PartyContactNumber}] [${uiLabelMap.PartyExtension}]
      </@field>
      -->
    <#elseif contactMechTypeId == "EMAIL_ADDRESS">
      <#assign fieldValue><#if tryEntity>${contactMech.infoString!}<#else>${requestParameters.emailAddress!}</#if></#assign>
      <@field type="input" label="${uiLabelMap.PartyEmailAddress}" required=true size="60" maxlength="255" name="emailAddress" value=fieldValue />
    <#else>
      <@field type="input" label=(contactMechType.get("description",locale)!) required=true size="60" maxlength="255" name="infoString" value=(contactMechData.infoString!) />
    </#if>
      <@field type="select" label="${uiLabelMap.PartyAllowSolicitation}?" name="allowSolicitation">
        <#if (((partyContactMechData.allowSolicitation)!"") == "Y")><option value="Y">${uiLabelMap.CommonYes}</option></#if>
        <#if (((partyContactMechData.allowSolicitation)!"") == "N")><option value="N">${uiLabelMap.CommonNo}</option></#if>
        <option></option>
        <option value="Y">${uiLabelMap.CommonYes}</option>
        <option value="N">${uiLabelMap.CommonNo}</option>
      </@field>
  </form>
</@section>

  <#else>    
    <@menu type="button">
      <@menuitem type="link" href=makeOfbizUrl("${donePage}") class="+${styles.action_nav!} ${styles.action_cancel!}" text=uiLabelMap.CommonGoBack />
    <#if requireCreate>
      <@menuitem type="link" href="javascript:document.createcontactmechform.submit()" class="+${styles.action_run_sys!} ${styles.action_add!}" text=uiLabelMap.CommonCreate />
    </#if>
    </@menu>
  </#if>
</#if>
