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
<#include "component://shop/webapp/shop/customer/customercommon.ftl">
<#include "component://setup/webapp/setup/common/common.ftl">

<#assign defaultParams = {
    "USER_ADDRESS_ALLOW_SOL": "Y",
    "USER_MOBILE_ALLOW_SOL": "Y",
    "USER_WORK_ALLOW_SOL": "Y",
    "USER_FAX_ALLOW_SOL": "Y",
    "USER_EMAIL_ALLOW_SOL": "Y",
    <#-- TODO: review defaults -->
    "roleTypeId": "OWNER",
    "partyRelationshipTypeId": "OWNER",
    <#-- NOTE: PRODUCT_STORE_ID is now concatenation of "[productStoreId]::[roleTypeId]"; if roleTypeId is omitted,
        assumed to be same as the party relationship to company roleTypeIdTo -->
    "PRODUCT_STORE_ID": rawString(productStoreId!),
    "USER_ADDR_PURPOSE": ["GENERAL_LOCATION", "SHIPPING_LOCATION"]
}>

<#assign paramMaps = getWizardFormFieldValueMaps({
    "record":userInfo!true,   
    "defaults":defaultParams
})>
<#assign params = paramMaps.values>
<#assign fixedParams = paramMaps.fixedValues>

<@script>
    <#if getUsername>    
         lastFocusedName = null;
         function setLastFocused(formElement) {
             lastFocusedName = formElement.name;
             document.write.lastFocusedName;
         }
         function clickUsername() {
             if (document.getElementById('UNUSEEMAIL').checked) {
                 if (lastFocusedName == "UNUSEEMAIL") {
                     jQuery('#PASSWORD').focus();
                 } else if (lastFocusedName == "PASSWORD") {
                     jQuery('#UNUSEEMAIL').focus();
                 } else {
                     jQuery('#PASSWORD').focus();
                 }
             }
         }
         function changeEmail() {
             if (document.getElementById('UNUSEEMAIL').checked) {
                 document.getElementById('USERNAME').value = jQuery('#USER_EMAIL').val();
             }
         }
         function setEmailUsername(noreset) {
             if (document.getElementById('UNUSEEMAIL') != null && document.getElementById('UNUSEEMAIL').checked) {
                 document.getElementById('USERNAME').value = jQuery('#USER_EMAIL').val();
                 <#-- don't disable, make the browser not submit the field: document.getElementById('USERNAME').disabled=true; -->
                 <#-- SCIPIO: ... but DO set disabled class so user sees as if was disabled -->
                 jQuery('#USERNAME').slideUp('slow');
             } else {
                 if (noreset !== true) { <#-- SCIPIO: extra check -->
                    document.getElementById('USERNAME').value='';
                 }
                 <#-- document.getElementById('USERNAME').disabled=false; -->
                 jQuery('#USERNAME').slideDown('slow');
             }
         }
    </#if>
    jQuery(document).ready(function() {        
        <#-- SCIPIO: do this also on page load -->
        setEmailUsername(true);
    });
</@script>

<@form method="post" action=makeOfbizUrl(target) id=submitFormId name=submitFormId validate=setupFormValidate>
    <@defaultWizardFormFields exclude=["userPartyId"] />
    <@field type="hidden" name="isCreateUser" value=(userParty??)?string("N","Y")/>
    <@field type="hidden" name="userPartyId" value=((userParty.partyId)!)/>    
    <#if userParty??>
        <@field type="display" name="userPartyId" label=uiLabelMap.PartyPartyId>
            <@setupExtAppLink uri=("/partymgr/control/viewprofile?partyId="+rawString(userParty.partyId!)) text=(userParty.partyId!)/><#t/>
        </@field>
    </#if>

    <#assign fieldsRequired = true>

    <#if !getUsername?has_content || (getUsername?has_content && getUsername)>    	      
        <#macro extraFieldContent args={}>	          
          <@field type="checkbox" checkboxType="simple-standard" name="UNUSEEMAIL" id="UNUSEEMAIL" value="on" onClick="setEmailUsername();" onFocus="setLastFocused(this);" label=uiLabelMap.SetupUseEmailAddress checked=((parameters.UNUSEEMAIL!) == "on")/>
        </#macro>
        <#assign fieldStyle = "">    	        
        <#if !userUserLogin?has_content>
           <#if ((parameters.UNUSEEMAIL!) == "on")>
              <#assign fieldStyle = "display:none;">
           </#if>
           <#-- We need to allow empty for any existing user even if has no login
           <#assign pwRequired = true>-->
           <#assign pwRequired = (!userParty??)> 
           <@field type="text" name="USERNAME" id="USERNAME" style=fieldStyle value=(params.USERNAME!) onFocus="clickUsername();" onchange="changeEmail();" label=uiLabelMap.CommonUsername required=pwRequired postWidgetContent=extraFieldContent />
        <#else>
           <@field type="display" value=(userUserLogin.userLoginId!) label=uiLabelMap.CommonUsername />
           <@field type="hidden" name="USERNAME" value=(params.userLoginId!)/>
        </#if>    	      
    </#if>

    <#if (!createAllowPassword?has_content) || createAllowPassword>
      <#-- We need to allow empty for any existing user even if has no login
      <#assign pwRequired = ((!userUserLogin.userLoginId?has_content)!true)>-->
      <#assign pwRequired = (!userParty??)>
      <@field type="password" name="PASSWORD" id="PASSWORD" onFocus="setLastFocused(this);" label=uiLabelMap.CommonPassword required=pwRequired />      
      <@field type="password" name="CONFIRM_PASSWORD" id="CONFIRM_PASSWORD" value="" maxlength="50" label=uiLabelMap.PartyRepeatPassword required=pwRequired />		      
    <#else>
      <@commonMsg type="info-important">${uiLabelMap.PartyReceivePasswordByEmail}.</@commonMsg>
    </#if>

    <@field type="generic" label=uiLabelMap.PartyRelationships>
        <@fields args={"type":"default-compact", "ignoreParentField":true}>            
          <@row>
            <@cell large=6>
            <@field type="select" name="roleTypeId" id="roleTypeId" label=getLabel('PartyPartyCurrentInTheRoleOf')
                required=true><#-- SCIPIO: DEV NOTE: DO NOT REMOVE REQUIRED FLAG, service just crash without it... -->
                <#list userPartyRoles as partyRole>
                    <#assign selected = (rawString(params.roleTypeId!) == rawString(partyRole.roleTypeId!))>
                    <option value="${partyRole.roleTypeId}"<#if selected> selected="selected"</#if>>${partyRole.get('description', locale)!partyRole.roleTypeId}</option>
                </#list>
            </@field>
            </@cell>
            <@cell large=6>
              <#if userParty?? && userPartyRole??>
                  <#-- WARNING: roleTypeId is highly confounded: it is
                      used for ProductStoreRole, PartyContactMech AND PartyRelationship,
                      so it is not trivial to change, and trying to update all the records can damage data,
                      so large warning required. -->
                  <#-- DEV NOTE: Cannot delete old PartyRole automatically by default because the PartyRole may be reused by more than
                      one related entity/function, included being manually depended upon by the client.
                      Even if could check the 78 database relations to PartyRole, it could still be needed
                      in a secondary purpose by the client for manual use. -->
                  <@alert type="warning">${uiLabelMap.SetupUserRoleChangeWarning}
                    <@setupExtAppLink uri=("/partymgr/control/viewroles?partyId="+rawString(userParty.partyId!)) text=uiLabelMap.PageTitleViewPartyRole class="${styles.link_nav!} ${styles.action_view!}"/>
                    <br/>
                    <@fields type="default-compact" fieldArgs={"inlineItems":false}>
                      <@field type="checkbox" name="relRoleChgUpdRec" value="true" altValue="false" label=uiLabelMap.SetupUpdateRecordsForNewRole currentValue=(params.relRoleChgUpdRec!) defaultValue="true"/>
                      <@field type="checkbox" name="relRoleChgUpdExpDelPrev" value="true" altValue="false" label=uiLabelMap.SetupUpdateExpiredRemovePreviousRole currentValue=(params.relRoleChgUpdExpDelPrev!) defaultValue="true"/>
                    </@fields>
                  </@alert>
              </#if>
            </@cell>
          </@row>
          <@row>
            <@cell large=6 last=true>
            <@field type="select" name="partyRelationshipTypeId" id="partyRelationshipTypeId" label=getLabel('SetupIsRelatedToOrgAs', '', {"orgPartyId":rawString(orgPartyId!)}) required=false>
                <option value=""<#if !rawString(params.partyRelationshipTypeId!)?has_content> selected="selected"</#if>>--</option>
                <#list userPartyRelationshipTypes as userPartyRelationshipType>
                    <#assign selected = (rawString(params.partyRelationshipTypeId!) == rawString(userPartyRelationshipType.partyRelationshipTypeId!))>
                    <option value="${userPartyRelationshipType.partyRelationshipTypeId}"<#if selected> selected="selected"</#if>>${userPartyRelationshipType.get('partyRelationshipName', locale)!userPartyRelationshipType.partyRelationshipTypeId}</option>
                </#list>
            </@field>
            </@cell>
          </@row>
            <#if userPartyRelationship??>
                <@field type="hidden" name="oldUserPartyRelationshipTypeId" value=userPartyRelationship.partyRelationshipTypeId />
                <@field type="hidden" name="oldUserPartyRelationshipRoleTypeIdTo" value=userPartyRelationship.roleTypeIdTo />
                <@field type="hidden" name="oldUserPartyRelationshipFromDate" value=userPartyRelationship.fromDate />
            </#if>
            <#if userPartyRole??>
                <@field type="hidden" name="oldUserPartyRoleId" value=userPartyRole.roleTypeId />
            </#if>
            
          <@row>
            <@cell large=6>
            <@field type="select" name="PRODUCT_STORE_ID" id="PRODUCT_STORE_ID" label=uiLabelMap.SetupAssociateUserToStore required=false>
                <#assign storeFound = false>
                <#assign rawStoreIdPram = rawString(params.PRODUCT_STORE_ID!)>
                <option value=""<#if !rawStoreIdPram?has_content> selected="selected"<#assign storeFound = true></#if>>--</option>
                <#list productStoreList as store>
                    <#assign selected = (rawStoreIdPram == rawString(store.productStoreId!))>
                    <#if selected>
                      <#assign storeFound = true>
                    </#if>
                    <#assign storeName = store.get('storeName', locale)!>
                    <option value="${store.productStoreId}"<#if selected> selected="selected"</#if>><#if storeName?has_content>${storeName} [${store.productStoreId}]<#else>${store.productStoreId}</#if></option>
                </#list>
                <#-- 
                    // REMOVED: this was code intended for handling roleTypeId as part of PRODUCT_STORE_ID param for 
                    // ProductStoreRoles to company stores that do not match the company relationship roleTypeId, 
                    // but it is too complicated to implement without causing additional issues.
                <#if !storeFound>
                    <#- - Special case: relation to store in a different roleTypeId - ->
                    <#assign storeRoleInfo = Static["com.ilscipio.scipio.setup.SetupDataUtil"].splitProductStoreRoleParamToValues(delegator, rawStoreIdPram, productStoreIdSet!)!>
                    <#if storeRoleInfo.valid && storeRoleInfo.roleType??>
                        <#assign store = storeRoleInfo.productStore>
                        <#assign storeName = store.get('storeName', locale)!>
                        <option value="${escapeVal(rawStoreIdPram, 'html')}" selected="selected"><#if storeName?has_content>${storeName} [${store.productStoreId}]<#else>${store.productStoreId}</#if><#rt/>
                            <#lt/> ${uiLabelMap.CommonAs} ${storeRoleInfo.roleType.get('description', locale)!storeRoleInfo.roleType.roleTypeId}</option>
                    </#if>
                </#if>
                -->
            </@field>
              <#if userProductStoreRole??>
                <@field type="hidden" name="oldUserPsrProductStoreId" value=userProductStoreRole.productStoreId/>
                <@field type="hidden" name="oldUserPsrRoleTypeId" value=userProductStoreRole.roleTypeId/>
                <#-- don't need to this one; it will be relooked-up using date filter server-side
                <@field type="hidden" name="oldUserPsrFromDate" value=userProductStoreRole.fromDate/>-->
              </#if>
            </@cell>
            <@cell large=6>
                <#if extraProductStoreRole??>
                  <@alert type="info">${uiLabelMap.SetupUserStoreRoleMultiInfo}
                    <@setupExtAppLink uri=(("/catalog/control/FindProductStoreRoles?productStoreId="+rawString(extraProductStoreRole.productStoreId)+"&partyId="+rawString(extraProductStoreRole.partyId))) text=uiLabelMap.PageTitleFindProductStoreRoles class="${styles.link_nav!} ${styles.action_view!}"/>
                  </@alert>
                <#elseif userProductStoreRole??>
                  <@alert type="info">${uiLabelMap.SetupUserStoreRoleAssocInfo}
                    <@setupExtAppLink uri=(("/catalog/control/FindProductStoreRoles?productStoreId="+rawString(userProductStoreRole.productStoreId)+"&partyId="+rawString(userProductStoreRole.partyId))) text=uiLabelMap.PageTitleFindProductStoreRoles class="${styles.link_nav!} ${styles.action_view!}"/>
                  </@alert>
                <#elseif userParty??>
                  <@alert type="info">${uiLabelMap.SetupUserStoreRoleAssocInfo}
                    <@setupExtAppLink uri=(("/catalog/control/FindProductStoreRoles?productStoreId="+rawString(productStoreId!)+"&partyId="+rawString(userParty.partyId))) text=uiLabelMap.PageTitleFindProductStoreRoles class="${styles.link_nav!} ${styles.action_view!}"/>
                  </@alert>
                </#if>
            </@cell>
          </@row>
        </@fields>
    </@field>

	<hr/> 

    <input type="hidden" name="emailProductStoreId" value="${productStoreId}"/>    
    <@personalTitleField name="USER_TITLE" label=uiLabelMap.CommonTitle personalTitle=(params.personalTitle!params.USER_TITLE!) /> 
    <@field type="input" name="USER_FIRST_NAME" id="USER_FIRST_NAME" value=(params.firstName!params.USER_FIRST_NAME!) label=uiLabelMap.PartyFirstName required=true />
    <@field type="input" name="USER_LAST_NAME" id="USER_LAST_NAME" value=(params.lastName!params.USER_LAST_NAME) label=uiLabelMap.PartyLastName required=true />

    <hr/>

    <#if userInfo??>
      <#assign addressManageUri = "/partymgr/control/viewprofile?partyId=${rawString(userPartyId!)}">
      <#assign fieldLabelDetail><@formattedContactMechPurposeDescs (generalAddressContactMechPurposes![]) ; description><b>${escapeVal(description, 'html')}</b><br/></@formattedContactMechPurposeDescs>
      </#assign>
    <#else>
      <#assign labelDetail = "">
      <#assign addressManageUri = "">
    </#if>
    <@field type="hidden" name="USE_ADDRESS" value="true"/>
    <#if userInfo?? && params.USER_ADDRESS_CONTACTMECHID?has_content>
        <@field type="hidden" name="USER_ADDRESS_CONTACTMECHID" value=(params.USER_ADDRESS_CONTACTMECHID!)/>
    </#if>
    <@field type="generic" label=uiLabelMap.PartyGeneralAddress labelDetail=fieldLabelDetail>
        <div id="setupUser-editMailShipAddr-area">
            <@fields args={"type":"default", "ignoreParentField":true}>
                <#-- Do not mark postal fields required if this is an existing user with no address; but
                    in all other cases mark as required -->
        	    <#assign postalFieldsRequired = fieldsRequired && !(userParty?? && !userInfo.USER_ADDRESS_CONTACTMECHID?has_content)>
                <@render resource="component://setup/widget/ProfileScreens.xml#postalAddressFields" 
                      ctxVars={
                        "pafFieldNamePrefix":"USER_",
                        "pafFieldIdPrefix":"EditUser_",
                        "pafUseScripts":true,
                        "pafFallbacks":({}),
                        "pafParams": params,                    
                        "pafFieldNameMap": {
                          "stateProvinceGeoId": "STATE",
                          "countryGeoId": "COUNTRY",
                          "address1": "ADDRESS1",
                          "address2": "ADDRESS2",
                          "city": "CITY",
                          "postalCode": "POSTAL_CODE"
                        },
                        "pafUseToAttnName":false,
                        "pafMarkRequired":postalFieldsRequired
                      }/>
                <@field type="hidden" name="USER_ADDRESS_ALLOW_SOL" value=(fixedParams.USER_ADDRESS_ALLOW_SOL!)/>
                <#-- Don't remove existing address (otherwise inconsistent with user creation) -->
                <@field type="hidden" name="USER_ADDRESS_UPDIGNEMPTY" value="true"/>
            </@fields>
            
            <#-- DEV NOTE: for users must allow toggle specific purposes because they're too generic and whole address
                form is useless without this. Replaces USER_ADDRESS_CREATEMISSINGPURPOSES below. -->
            <@field type="generic" label=uiLabelMap.CommonPurpose ignoreParentField=true fieldsType="default">
              <#list postalPurposeTypeList as purposeType>
                <@field type="checkbox" name="USER_ADDR_PURPOSE" inlineItems=true value=purposeType.contactMechPurposeTypeId label=(purposeType.get("description")!purposeType.contactMechPurposeTypeId)
                  checked=((params.USER_ADDR_PURPOSE![])?seq_contains(rawString(purposeType.contactMechPurposeTypeId)))/>
              </#list>
            </@field>

            <#if userParty??>
                <#assign addressNoticeParams = {"purposes":getContactMechPurposeDescs(locationPurposes)?join(", ")}>
                <#if !generalAddressContactMech??>
                    <@alert type="warning">${getLabel('SetupNoAddressDefinedNotice', '', addressNoticeParams)}</@alert>
                <#else>
                  <#if (generalAddressStandaloneCompleted!false) == false>
                    <#if (locationAddressesCompleted!false) == true>
                      <#-- 2018-10-31: Not really appropriate for user, because normal that many users will have multiple addresses
                      <@alert type="info">${getLabel('SetupSplitAddressPurposesNotice', '', addressNoticeParams)}
                        <@setupExtAppLink uri=addressManageUri text=uiLabelMap.PartyManager class="+${styles.link_nav} ${styles.action_update}"/>
                      </@alert>-->
                    <#else>
                      <@alert type="warning">${getLabel('SetupMissingAddressPurposesNotice', '', addressNoticeParams)}
                        <@setupExtAppLink uri=addressManageUri text=uiLabelMap.PartyManager class="+${styles.link_nav} ${styles.action_update}"/>
                      </@alert>
                      <#-- 2018-10-31: no need for this for user anymore since we have the checkboxes
                      <#-  - NOTE: this flag could be destructive, that's why it's false by default - ->
                      <@field type="checkbox" checkboxType="simple" name="USER_ADDRESS_CREATEMISSINGPURPOSES" label=uiLabelMap.SetupCreateMissingAddressPurposes
                        id="USER_ADDRESS_CREATEMISSINGPURPOSES_CHECK" value="true" altValue="false" currentValue=(params.USER_ADDRESS_CREATEMISSINGPURPOSES!"false")/>-->
                    </#if>
                  </#if>
                </#if>
            </#if>
        </div>
    </@field>

	<#if userInfo?? && params.USER_WORK_CONTACTMECHID?has_content>
        <@field type="hidden" name="USER_WORK_CONTACTMECHID" value=(params.USER_WORK_CONTACTMECHID!)/>
    </#if>
    <@telecomNumberField label=uiLabelMap.PartyContactWorkPhoneNumber params=params
        fieldNamePrefix="USER_WORK_" countryCodeName="COUNTRY" areaCodeName="AREA" contactNumberName="CONTACT" extensionName="EXT">
        <@fields type="default-compact" ignoreParentField=true>                    
            <@field type="hidden" name="USER_WORK_ALLOW_SOL" value=(fixedParams.USER_WORK_ALLOW_SOL!)/>        
        </@fields>
    </@telecomNumberField>

    <#if userInfo?? && params.USER_MOBILE_CONTACTMECHID?has_content>
        <@field type="hidden" name="USER_MOBILE_CONTACTMECHID" value=(params.USER_MOBILE_CONTACTMECHID!)/>
    </#if>
    <@telecomNumberField label=uiLabelMap.PartyContactMobilePhoneNumber params=params
        fieldNamePrefix="USER_MOBILE_" countryCodeName="COUNTRY" areaCodeName="AREA" contactNumberName="CONTACT" extensionName="EXT">
        <@fields type="default-compact" ignoreParentField=true>                    
            <@field type="hidden" name="USER_MOBILE_ALLOW_SOL" value=(fixedParams.USER_MOBILE_ALLOW_SOL!)/>            
        </@fields>
    </@telecomNumberField>

    <#if userInfo?? && params.USER_FAX_CONTACTMECHID?has_content>
        <@field type="hidden" name="USER_FAX_CONTACTMECHID" value=(params.USER_FAX_CONTACTMECHID!)/>
    </#if>
    <@telecomNumberField label=uiLabelMap.PartyContactFaxPhoneNumber params=params
        fieldNamePrefix="USER_FAX_" countryCodeName="COUNTRY" areaCodeName="AREA" contactNumberName="CONTACT" extensionName="EXT">
        <@fields type="default-compact" ignoreParentField=true>                    
            <@field type="hidden" name="USER_FAX_ALLOW_SOL" value=(fixedParams.USER_FAX_ALLOW_SOL!)/>       
        </@fields>
    </@telecomNumberField>

    <#if userInfo?? && params.USER_EMAIL_CONTACTMECHID?has_content>
      <@field type="hidden" name="USER_EMAIL_CONTACTMECHID" value=(params.USER_EMAIL_CONTACTMECHID!)/>
    </#if>
    <#-- Only require email field for new users and users with already an email (don't let remove it, otherwise contradicts the create requirement) -->
    <#assign emailRequired = ((!userParty??) || (userInfo.USER_EMAIL)?has_content)>
    <@field type="input" name="USER_EMAIL" id="USER_EMAIL" value=(params.USER_EMAIL!) onChange="changeEmail()" onkeyup="changeEmail()" label=uiLabelMap.PartyEmailAddress 
        required=emailRequired/><#-- SCIPIO: DEV NOTE: DO NOT REMOVE REQUIRED FLAG unless you edit the server-side service to make this optional, otherwise you mislead the admin! -->            
    <@field type="hidden" name="USER_EMAIL_ALLOW_SOL" value=(fixedParams.USER_EMAIL_ALLOW_SOL!)/>
    <@field type="hidden" name="USER_EMAIL_UPDIGNEMPTY" value="true"/><#-- Help ensure server-side never remove email on update -->
</@form>
