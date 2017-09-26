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

<#macro menuContent menuArgs={}>
  <@menu args=menuArgs>    
    <@menuitem type="link" href="javascript:document.getElementById('NewUser').submit()" class="+${styles.action_run_sys!} ${styles.action_update!}" text=uiLabelMap.CommonSave/>
  </@menu>
</#macro>

<@section menuContent=menuContent menuLayoutGeneral="bottom"><#-- title=uiLabelMap.EcommerceRegister-->
  


<#-- SCIPIO: NOTE: fieldErrors should be kept in the time there is no javascript validation.
    To remove these, simply toggle this bool. 
    NOTE: these do work fairly decently however. -->
	<#assign useServerFieldErrors = useServerFieldErrors!true>
	
	<#macro fieldErrors args={} inlineArgs...>
	  <#if useServerFieldErrors && errorMessageList?has_content>
	    <#local fieldName = inlineArgs.fieldName!args.fieldName!>
	    <#assign fieldMessages = Static["org.ofbiz.base.util.MessageString"].getMessagesForField(fieldName, true, errorMessageList)>
	    <#if fieldMessages?has_content>
	    <@alert type="error">
	      <#list fieldMessages as errorMsg>
	        ${errorMsg}<#if errorMsg_has_next><br/></#if>
	      </#list>
	    </@alert>
	    </#if>
	  </#if>
	</#macro>
	<#macro fieldErrorsMulti args={} fieldNames...>
	  <#if useServerFieldErrors && errorMessageList?has_content>
	    <#assign fieldMessages = Static["org.ofbiz.base.util.MessageString"].getMessagesForField(fieldNames, true, errorMessageList)>
	    <#if fieldMessages?has_content>
	    <@alert type="error">
	      <#list fieldMessages as errorMsg>
	        ${errorMsg}<#if errorMsg_has_next><br/></#if>
	      </#list>
	    </@alert>
	    </#if>
	  </#if>
	</#macro>

	<form method="post" action="<@ofbizUrl>${target}</@ofbizUrl>" id="NewUser" name="NewUser">
		<@field type="hidden" name="USER_ADDRESS_ALLOW_SOL" value="Y"/>
	    <@field type="hidden" name="USER_HOME_ALLOW_SOL" value="Y"/>
	    <@field type="hidden" name="USER_WORK_ALLOW_SOL" value="Y"/>
	    <@field type="hidden" name="USER_FAX_ALLOW_SOL" value="Y"/>
	    <@field type="hidden" name="USER_MOBILE_ALLOW_SOL" value="Y"/>
	    <@field type="hidden" name="USER_EMAIL_ALLOW_SOL" value="Y"/>
	    
	    <@field type="hidden" name="productStoreId" value="${productStoreId}"/>
	  
	  	<@commonMsg type="info-important">${uiLabelMap.CommonFieldsMarkedAreRequired}</@commonMsg>
	
		<@row>
		  <@cell columns=6>
		  <fieldset>
		    <legend>${uiLabelMap.SetupAccountInformation}</legend>
		    <#macro extraFieldContent args={}>
		      <@fieldErrors fieldName="USER_EMAIL"/>
		      <@fields type="default-compact" ignoreParentField=true>
		        <@allowSolicitationField name="USER_EMAIL_ALLOW_SOL" />
		      </@fields>
		    </#macro>
		    <@field type="input" name="USER_EMAIL" id="USER_EMAIL" value=(parameters.USER_EMAIL!) 
		        onChange="changeEmail()" onkeyup="changeEmail()" label=uiLabelMap.PartyEmailAddress required=true 
		        />
		
		    <#if getUsername>
		      <#if parameters.preferredUsername?has_content>
		        <#macro extraFieldContent args={}>
		          <@fieldErrors fieldName="USERNAME"/>
		        </#macro>
		        <input type="hidden" name="USERNAME" id="USERNAME" value="${parameters.USERNAME!}"/>
		        <@field type="text" name="showUserName" id="showUserName" value=(parameters.USERNAME!) disabled="disabled" label=uiLabelMap.CommonUsername 
		            required=true postWidgetContent=extraFieldContent  />
		      <#else>
		        <#macro extraFieldContent args={}>
		          <@fieldErrors fieldName="USERNAME"/>
		          <@field type="checkbox" checkboxType="simple-standard" name="UNUSEEMAIL" id="UNUSEEMAIL" value="on" 
		            onClick="setEmailUsername();" onFocus="setLastFocused(this);" label=uiLabelMap.SetupUseEmailAddress 
		            checked=((parameters.UNUSEEMAIL!) == "on")/>
		        </#macro>
		        <#assign fieldStyle = "">
		        <#if ((parameters.UNUSEEMAIL!) == "on")>
		          <#assign fieldStyle = "display:none;">
		        </#if>
		        <@field type="text" name="USERNAME" id="USERNAME" style=fieldStyle value=(parameters.USERNAME!) onFocus="clickUsername();" onchange="changeEmail();" 
		            label=uiLabelMap.CommonUsername required=true postWidgetContent=extraFieldContent />
		      </#if>
		    </#if>
		
		    <#if createAllowPassword>
		      <#macro extraFieldContent args={}>
		        <@fieldErrors fieldName="PASSWORD"/>
		      </#macro>
		      <@field type="password" name="PASSWORD" id="PASSWORD" onFocus="setLastFocused(this);" 
		        label=uiLabelMap.CommonPassword required=true postWidgetContent=extraFieldContent />
		
		      <#macro extraFieldContent args={}>
		        <@fieldErrors fieldName="CONFIRM_PASSWORD"/>
		      </#macro>
		      <@field type="password" name="CONFIRM_PASSWORD" id="CONFIRM_PASSWORD" value="" maxlength="50" 
		        label=uiLabelMap.PartyRepeatPassword required=true postWidgetContent=extraFieldContent />
		
		      <#macro extraFieldContent args={}>
		        <@fieldErrors fieldName="PASSWORD_HINT"/>
		      </#macro>
		      <#-- <@field type="input" name="PASSWORD_HINT" id="PASSWORD_HINT" value=(parameters.PASSWORD_HINT!) 
		        maxlength="100"label=uiLabelMap.PartyPasswordHint postWidgetContent=extraFieldContent/> -->
		    <#else>
		      <@commonMsg type="info-important">${uiLabelMap.PartyReceivePasswordByEmail}.</@commonMsg>
		    </#if>
		  </fieldset>
		  </@cell>
		
		  <@cell columns=6>
		  <fieldset>
		    <legend>${uiLabelMap.PartyPersonalInformation}</legend>
		    <input type="hidden" name="emailProductStoreId" value="${productStoreId}"/>
		
		    <#macro extraFieldContent args={}>
		      <@fieldErrors fieldName="USER_TITLE"/>
		    </#macro>
		    <@personalTitleField name="USER_TITLE" label=uiLabelMap.CommonTitle postWidgetContent=extraFieldContent/> 
		    
		    <#macro extraFieldContent args={}>
		      <@fieldErrors fieldName="USER_FIRST_NAME"/>
		    </#macro>
		    <@field type="input" name="USER_FIRST_NAME" id="USER_FIRST_NAME" value=(parameters.USER_FIRST_NAME!) 
		        label=uiLabelMap.PartyFirstName required=true postWidgetContent=extraFieldContent />
		
		    <#macro extraFieldContent args={}>
		      <@fieldErrors fieldName="USER_MIDDLE_NAME"/>
		    </#macro>
		    <#-- <@field type="input" name="USER_MIDDLE_NAME" id="USER_MIDDLE_NAME" value=(parameters.USER_MIDDLE_NAME!) 
		        label=uiLabelMap.PartyMiddleInitial postWidgetContent=extraFieldContent/> -->
		
		    <#macro extraFieldContent args={}>
		      <@fieldErrors fieldName="USER_LAST_NAME"/>
		    </#macro>
		    <@field type="input" name="USER_LAST_NAME" id="USER_LAST_NAME" value=(parameters.USER_LAST_NAME!) 
		        label=uiLabelMap.PartyLastName required=true postWidgetContent=extraFieldContent />
		
		    <#macro extraFieldContent args={}>
		      <@fieldErrors fieldName="USER_SUFFIX"/>
		    </#macro>
		    <#-- <@field type="input" name="USER_SUFFIX" id="USER_SUFFIX" value=(parameters.USER_SUFFIX!) 
		        label=uiLabelMap.PartySuffix postWidgetContent=extraFieldContent containerClass="+${styles.field_extra!}"/> -->
		
		  </fieldset>
		  </@cell>
		</@row>
		
		<@row>
		  <@cell columns=6>
		  <fieldset>
		    <#-- SCIPIO: NOTE: This is used both as GENERAL_LOCATION and SHIPPING_LOCATION
		    <legend>${uiLabelMap.PartyShippingAddress}</legend>
		    <legend>${getLabel("ContactMechType.description.POSTAL_ADDRESS", "PartyEntityLabels")}</legend>-->
		    
		    <legend>${uiLabelMap.CommonLocation}</legend>
		    
		    <#macro extraFieldContent args={}>
		      <@fieldErrors fieldName="USER_ADDRESS1"/>
		    </#macro>
		    <@field type="input" name="USER_ADDRESS1" id="USER_ADDRESS1" value=(parameters.USER_ADDRESS1!) 
		        label=uiLabelMap.PartyAddressLine1 required=true postWidgetContent=extraFieldContent/>
		
		    <#macro extraFieldContent args={}>
		      <@fieldErrors fieldName="USER_ADDRESS2"/>
		    </#macro>
		    <@field type="input" name="USER_ADDRESS2" id="USER_ADDRESS2" value=(parameters.USER_ADDRESS2!) 
		        label=uiLabelMap.PartyAddressLine2 postWidgetContent=extraFieldContent/>
		
		    <#macro extraFieldContent args={}>
		      <@fieldErrors fieldName="USER_CITY"/>
		    </#macro>
		    <@field type="input" name="USER_CITY" id="USER_CITY" value=(parameters.USER_CITY!) 
		        label=uiLabelMap.PartyCity required=true postWidgetContent=extraFieldContent />
		
		    <#macro extraFieldContent args={}>
		      <@fieldErrors fieldName="USER_POSTAL_CODE"/>
		    </#macro>
		    <@field type="input" name="USER_POSTAL_CODE" id="USER_POSTAL_CODE" value=(parameters.USER_POSTAL_CODE!) 
		        label=uiLabelMap.PartyZipCode required=true postWidgetContent=extraFieldContent />
		  
		    <#macro extraFieldContent args={}>
		      <@fieldErrors fieldName="USER_COUNTRY"/>
		    </#macro>
		    <@field type="select" name="USER_COUNTRY" id="NewUser_USER_COUNTRY" label=uiLabelMap.CommonCountry 
		        required=true postWidgetContent=extraFieldContent>
		        <@render resource="component://common/widget/CommonScreens.xml#countries" ctxVars={
		            "currentCountryGeoId":parameters.USER_COUNTRY!""
		        }/>  
		    </@field>
		    
		    <#macro extraFieldContent args={}>
		      <@fieldErrors fieldName="USER_STATE"/>
		    </#macro>
		    <@field type="select" name="USER_STATE" id="NewUser_USER_STATE" required=true 
		        label=uiLabelMap.PartyState postWidgetContent=extraFieldContent>
		        <#-- Populated by JS -->
		        <#if parameters.USER_STATE?has_content>
		          <option value="${parameters.USER_STATE?html}">${parameters.USER_STATE?html}</option>
		        </#if>
		    </@field>
		
		    <#macro extraFieldContent args={}>
		      <@fieldErrors fieldName="USER_ADDRESS_ALLOW_SOL"/>
		    </#macro>
		    <#-- <@allowSolicitationField name="USER_ADDRESS_ALLOW_SOL" postWidgetContent=extraFieldContent containerClass="+${styles.field_extra!}"  /> -->
		
		  </fieldset>
		  </@cell>
		
		  <@cell columns=6>
		  <fieldset>    
		    <legend>${getLabel("CommunicationEventType.description.PHONE_COMMUNICATION", "PartyEntityLabels")}</legend>
		
		    <@field type="generic" label=uiLabelMap.PartyHomePhone tooltip=tooltip required=false>    
		        <@field type="input" inline=true size="8" maxlength="25" name="USER_HOME_CONTACT" value=(params["USER_HOME_CONTACT"])! tooltip=uiLabelMap.PartyContactNumber required=false/>
		    </@field>
		    <@field type="generic" label=uiLabelMap.PartyMobilePhone tooltip=tooltip required=false>    
		        <@field type="input" inline=true size="8" maxlength="25" name="USER_MOBILE_CONTACT" value=(params["USER_MOBILE_CONTACT"])! tooltip=uiLabelMap.PartyContactNumber required=false/>
		    </@field>
		    <@field type="generic" label=uiLabelMap.PartyFaxNumber tooltip=tooltip required=false>    
		        <@field type="input" inline=true size="8" maxlength="25" name="USER_FAX_CONTACT" value=(params["USER_FAX_CONTACT"])! tooltip=uiLabelMap.PartyContactNumber required=false/>
		    </@field>
		
		  </fieldset>
		  </@cell>
		</@row>
	
	</form>

</@section>