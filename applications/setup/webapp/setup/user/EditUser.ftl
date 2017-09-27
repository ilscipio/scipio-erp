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
        "addressAllowSol": "Y",
        "homeAllowSol": "Y",
        "workAllowSol": "Y",
        "faxAllowSol": "Y",
        "mobileAllowSol": "Y",
        "emailAllowSol": "Y",
        
        "productStoreId": "${productStoreId}"
}>

<#assign userPartyParamMaps = getWizardFormFieldValueMaps({
    "record":userParty!true, <#-- NOTE: must fallback with boolean true -->
    "defaults":defaultParams,
    "strictRecord":true <#-- TODO: REMOVE (debugging) -->
})>
<#assign userPartyParams = userPartyParamMaps.values>
<#assign fixedParams = userPartyParamMaps.fixedValues>

<#assign userPersonParamMaps = getWizardFormFieldValueMaps({
    "record":userPerson!true, <#-- NOTE: must fallback with boolean true -->    
    "strictRecord":true <#-- TODO: REMOVE (debugging) -->
})>
<#assign userPersonParams = userPersonParamMaps.values>

<#assign userPostalAddressParamMaps = getWizardFormFieldValueMaps({
    "record":userPostalAddress!true, <#-- NOTE: must fallback with boolean true -->    
    "strictRecord":true <#-- TODO: REMOVE (debugging) -->
})>
<#assign userPostalAddressParams = userPostalAddressParamMaps.values>

<#assign userEmailAddressParamMaps = getWizardFormFieldValueMaps({
    "record":userEmailAddress!true, <#-- NOTE: must fallback with boolean true -->    
    "strictRecord":true <#-- TODO: REMOVE (debugging) -->
})>
<#assign userEmailAddressParams = userEmailAddressParamMaps.values>

<#assign userTelecomNumberParamMaps = getWizardFormFieldValueMaps({
    "record":userTelecomNumber!true, <#-- NOTE: must fallback with boolean true -->    
    "strictRecord":true <#-- TODO: REMOVE (debugging) -->
})>
<#assign userTelecomNumberParams = userTelecomNumberParamMaps.values>

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
             if (document.getElementById('UNUSEEMAIL').checked) {
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
    

<@form method="post" action=makeOfbizUrl(target) id="NewUser" name="NewUser">
    <@defaultWizardFormFields/>
    
    <#-- 
    	<@field type="hidden" name="USER_ADDRESS_ALLOW_SOL" value=(fixedParams.addressAllowSol!) />
    <@field type="hidden" name="USER_HOME_ALLOW_SOL" value=(fixedParams.homeAllowSol!) />
    <@field type="hidden" name="USER_WORK_ALLOW_SOL" value=(fixedParams.workAllowSol!) />
    <@field type="hidden" name="USER_FAX_ALLOW_SOL" value=(fixedParams.faxAllowSol!) />
    <@field type="hidden" name="USER_MOBILE_ALLOW_SOL" value=(fixedParams.mobileAllowSol!) />
    <@field type="hidden" name="USER_EMAIL_ALLOW_SOL" value=(fixedParams.emailAllowSol!) />        
    <@field type="hidden" name="PRODUCT_STORE_ID" value=(fixedParams.productStoreId!) />
    
    -->
  
  	<@commonMsg type="info-important">${uiLabelMap.CommonFieldsMarkedAreRequired}</@commonMsg>

	<@row>
	  <@cell columns=6>
    	  <fieldset>
    	    <legend>${uiLabelMap.SetupAccountInformation}</legend>
    	    
    	    <@field type="input" name="USER_EMAIL" id="USER_EMAIL" value=(userEmailAddressParams.infoString!) onChange="changeEmail()" onkeyup="changeEmail()" label=uiLabelMap.PartyEmailAddress required=true />
    	
    	    <#if !getUsername?has_content || (getUsername?has_content && getUsername)>
    	      <#if parameters.preferredUsername?has_content>		        
    	        <input type="hidden" name="USERNAME" id="USERNAME" value="${userPartyParams.userLoginId!}"/>
    	        <@field type="text" name="showUserName" id="showUserName" value=(userPartyParams.userLoginId!) disabled="disabled" label=uiLabelMap.CommonUsername required=true />
    	      <#else>
    	        <#macro extraFieldContent args={}>	          
    	          <@field type="checkbox" checkboxType="simple-standard" name="UNUSEEMAIL" id="UNUSEEMAIL" value="on" onClick="setEmailUsername();" onFocus="setLastFocused(this);" label=uiLabelMap.SetupUseEmailAddress checked=((parameters.UNUSEEMAIL!) == "on")/>
    	        </#macro>
    	        <#assign fieldStyle = "">
    	        <#if ((parameters.UNUSEEMAIL!) == "on")>
    	          <#assign fieldStyle = "display:none;">
    	        </#if>
    	        <@field type="text" name="USERNAME" id="USERNAME" style=fieldStyle value=(userPartyParams.userLoginId!) onFocus="clickUsername();" onchange="changeEmail();" label=uiLabelMap.CommonUsername required=true postWidgetContent=extraFieldContent />
    	      </#if>
    	    </#if>
    	
    	    <#if !createAllowPassword?has_content || (createAllowPassword?has_content && createAllowPassword)>
    	      <@field type="password" name="PASSWORD" id="PASSWORD" onFocus="setLastFocused(this);" label=uiLabelMap.CommonPassword required=true />      
    	      <@field type="password" name="CONFIRM_PASSWORD" id="CONFIRM_PASSWORD" value="" maxlength="50" label=uiLabelMap.PartyRepeatPassword required=true />		      
    	    <#else>
    	      <@commonMsg type="info-important">${uiLabelMap.PartyReceivePasswordByEmail}.</@commonMsg>
    	    </#if>
    	  </fieldset>
	  </@cell>
	
	  <@cell columns=6>
    	  <fieldset>
    	    <legend>${uiLabelMap.PartyPersonalInformation}</legend>
    	    <input type="hidden" name="emailProductStoreId" value="${productStoreId}"/>    
    	    <@personalTitleField name="USER_TITLE" label=uiLabelMap.CommonTitle /> 
    	    <@field type="input" name="USER_FIRST_NAME" id="USER_FIRST_NAME" value=(userPersonParams.firstName!) label=uiLabelMap.PartyFirstName required=true />
    	    <@field type="input" name="USER_LAST_NAME" id="USER_LAST_NAME" value=(userPersonParams.lastName!) label=uiLabelMap.PartyLastName required=true />
    	  </fieldset>
	  </@cell>
	</@row>
	
	<@row>
	  <@cell columns=6>
    	  <fieldset>
    	    <legend>${uiLabelMap.CommonLocation}</legend>
    	    <@field type="input" name="USER_ADDRESS1" id="USER_ADDRESS1" value=(userPostalAddressParams.address1!) label=uiLabelMap.PartyAddressLine1 required=true />    
    	    <@field type="input" name="USER_ADDRESS2" id="USER_ADDRESS2" value=(userPostalAddressParams.address2!) label=uiLabelMap.PartyAddressLine2 />
    	    <@field type="input" name="USER_CITY" id="USER_CITY" value=(userPostalAddressParams.city!) label=uiLabelMap.PartyCity required=true />
    	    <@field type="input" name="USER_POSTAL_CODE" id="USER_POSTAL_CODE" value=(userPostalAddressParams.postalCode!) label=uiLabelMap.PartyZipCode required=true />  
    	    <@field type="select" name="USER_COUNTRY" id="NewUser_USER_COUNTRY" label=uiLabelMap.CommonCountry required=true>
    	        <@render resource="component://common/widget/CommonScreens.xml#countries" ctxVars={
    	            "currentCountryGeoId":userPostalAddressParams.contryGeoId!""
    	        }/>  
    	    </@field>
    	    <@field type="select" name="USER_STATE" id="NewUser_USER_STATE" required=true label=uiLabelMap.PartyState>
    	        <#-- Populated by JS -->
    	        <#if userPostalAddressParams.stateProvinceGeoId?has_content>
    	          <option value="${userPostalAddressParams.stateProvinceGeoId?html}">${userPostalAddressParams.stateProvinceGeoId?html}</option>
    	        </#if>
    	    </@field>
    	  </fieldset>
	  </@cell>
	
	  <@cell columns=6>
    	  <fieldset>    
    	    <legend>${getLabel("CommunicationEventType.description.PHONE_COMMUNICATION", "PartyEntityLabels")}</legend>
    	        <@field type="input" label=uiLabelMap.PartyHomePhone size="8" maxlength="25" name="USER_HOME_CONTACT" value=(userTelecomNumberParams.contactNumber!) tooltip=uiLabelMap.PartyContactNumber required=false/>
    	        <@field type="input" label=uiLabelMap.PartyMobilePhone size="8" maxlength="25" name="USER_MOBILE_CONTACT" value=(userTelecomNumberParams.contactNumber!) tooltip=uiLabelMap.PartyContactNumber required=false/>
    	        <@field type="input" label=uiLabelMap.PartyFaxNumber size="8" maxlength="25" name="USER_FAX_CONTACT" value=(userTelecomNumberParams.contactNumber!) tooltip=uiLabelMap.PartyContactNumber required=false/>
    	  </fieldset>
	  </@cell>
	</@row>
	
    <@field type="submit" title=uiLabelMap.CommonUpdate class="+${styles.link_run_sys} ${styles.action_update}"/>
</@form>

