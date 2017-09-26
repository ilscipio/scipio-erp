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

<#-- SCIPIO: TODO?: Fields for business account (with party group) -->
<#-- SCIPIO: TODO?: Some of this is redundant with customerbasicfields.ftl - investigate -->

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
             document.getElementById('USERNAME').value = jQuery('#CUSTOMER_EMAIL').val();
         }
     }
     function setEmailUsername(noreset) {
         if (document.getElementById('UNUSEEMAIL').checked) {
             document.getElementById('USERNAME').value = jQuery('#CUSTOMER_EMAIL').val();
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
     function hideShowUsaStates() {
       <#-- SCIPIO: Don't do this here. if we ever do it it should be everywhere or nowhere. 
         var customerStateElement = document.getElementById('newuserform_stateProvinceGeoId');
         var customerCountryElement = document.getElementById('newuserform_countryGeoId');
         if (customerCountryElement.value == "USA" || customerCountryElement.value == "UMI") {
             customerStateElement.style.display = "block";
         } else {
             customerStateElement.style.display = "none";
         }
       -->
     }
   
</#if>

    jQuery(document).ready(function() {
        hideShowUsaStates();
        
        <#-- SCIPIO: do this also on page load -->
        setEmailUsername(true);
    });

</@script>

<#macro menuContent menuArgs={}>
  <@menu args=menuArgs>    
    <@menuitem type="link" href="javascript:document.getElementById('newuserform').submit()" class="+${styles.action_run_sys!} ${styles.action_update!}" text=uiLabelMap.CommonSave/>
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

<form method="post" action="<@ofbizUrl>createcustomer${previousParams}</@ofbizUrl>" id="newuserform" name="newuserform">
  
  <@commonMsg type="info-important">${uiLabelMap.CommonFieldsMarkedAreRequired}</@commonMsg>
<#-- 
        <field name="partyId"><hidden value="${partyId}"/></field>
        <field name="customerPartyId"><hidden value="CUST${partyId}"/></field>
        <field use-when="displayPassword!=null" name="USERNAME" title="${uiLabelMap.CommonUsername}"><text size="30" maxlength="250"/></field>
        <field use-when="displayPassword!=null" name="PASSWORD" title="${uiLabelMap.CommonPassword}"><password size="15" maxlength="250"/></field>
        <field use-when="displayPassword!=null" name="CONFIRM_PASSWORD" title="${uiLabelMap.CommonPassword}" tooltip="* ${uiLabelMap.CommonConfirm}"><password size="15" maxlength="250"/></field>
        <field name="USERNAME" title="${uiLabelMap.CommonUsername}" tooltip="* ${uiLabelMap.PartyTemporaryPassword}"><text size="30" maxlength="250"/></field>
        <field name="USER_ADDRESS_ALLOW_SOL"><hidden value="Y"/></field>
        <field name="USER_HOME_ALLOW_SOL"><hidden value="Y"/></field>
        <field name="USER_WORK_ALLOW_SOL"><hidden value="Y"/></field>
        <field name="USER_FAX_ALLOW_SOL"><hidden value="Y"/></field>
        <field name="USER_MOBILE_ALLOW_SOL"><hidden value="Y"/></field>
        <field name="USER_EMAIL_ALLOW_SOL"><hidden value="Y"/></field>
        
          <field name="USE_ADDRESS"><hidden value="${USE_ADDRESS}"/></field>
        <field name="require_email"><hidden value="${require_email}"/></field>
        <field name="USER_TITLE" title="${uiLabelMap.CommonTitle}"><text size="10" maxlength="30"/></field>
        <field name="USER_FIRST_NAME" title="${uiLabelMap.PartyFirstName}" required-field="true"><text size="30" maxlength="60"/></field>
        <field name="USER_MIDDLE_NAME" title="${uiLabelMap.PartyMiddleInitial}"><text size="4" maxlength="4"/></field>
        <field name="USER_LAST_NAME" title="${uiLabelMap.PartyLastName}" required-field="true"><text size="30" maxlength="60"/></field>
        <field name="USER_SUFFIX" title="${uiLabelMap.PartySuffix}"><text size="10" maxlength="30"/></field>
        <field name="ShippingAddressTitle" title="${uiLabelMap.PartyAddressMailingShipping}" title-area-style="group-label"><display description=" " also-hidden="false"/></field>
        <field name="USER_ADDRESS1" title="${uiLabelMap.CommonAddress1}" required-field="true"><text size="30" maxlength="60"/></field>
        <field name="USER_ADDRESS2" title="${uiLabelMap.CommonAddress2}"><text size="30" maxlength="60"/></field>
        <field name="USER_CITY" title="${uiLabelMap.CommonCity}" required-field="true"><text size="30" maxlength="60"/></field>
        <field name="USER_POSTAL_CODE" title="${uiLabelMap.CommonZipPostalCode}" required-field="true"><text size="10" maxlength="30"/></field>
        <field name="USER_COUNTRY" title="${uiLabelMap.CommonCountry}" required-field="true">
            <drop-down no-current-selected-key="${defaultCountryGeoId}">
                <entity-options entity-name="Geo" key-field-name="geoId" description="${geoName}"><!-- SCIPIO: no: ${geoId} -->
         <#--            <entity-constraint name="geoTypeId" value="COUNTRY"/>
                    <entity-order-by field-name="geoId"/>
                </entity-options>
            </drop-down>
        </field>
        <field name="USER_STATE" title="${uiLabelMap.CommonState}" required-field="false"><drop-down allow-empty="true"/></field>                       
        <field name="USER_ADDRESS_ALLOW_SOL" title="${uiLabelMap.PartyContactAllowAddressSolicitation}?">
            <drop-down allow-empty="true"><option key="Y" description="${uiLabelMap.CommonY}"/><option key="N" description="${uiLabelMap.CommonN}"/></drop-down>
        </field>
        <field name="HomePhoneTitle" title="${uiLabelMap.PartyHomePhone}" title-area-style="group-label" widget-style="tooltip"><display description="${uiLabelMap.PartyPhoneNumberRequired}" also-hidden="false"/></field>
        <field name="USER_HOME_COUNTRY" title="${uiLabelMap.CommonCountryCode}"><text size="4" maxlength="10"/></field>
        <field name="USER_HOME_AREA" title="${uiLabelMap.PartyAreaCode}"><text size="4" maxlength="10"/></field>
        <field name="USER_HOME_CONTACT" title="${uiLabelMap.PartyPhoneNumber}"><text size="15" maxlength="15"/></field>
        <field name="USER_HOME_EXT" title="${uiLabelMap.PartyContactExt}"><text size="6" maxlength="10"/></field>
        <field name="USER_HOME_ALLOW_SOL" title="${uiLabelMap.PartyContactAllowSolicitation}?"><drop-down allow-empty="true"><option key="Y" description="${uiLabelMap.CommonY}"/><option key="N" description="${uiLabelMap.CommonN}"/></drop-down></field>
        <field name="WorkPhoneTitle" title="${uiLabelMap.PartyContactWorkPhoneNumber}" title-area-style="group-label"><display description=" " also-hidden="false"/></field>
        <field name="USER_WORK_COUNTRY" title="${uiLabelMap.CommonCountryCode}"><text size="4" maxlength="10"/></field>
        <field name="USER_WORK_AREA" title="${uiLabelMap.PartyAreaCode}"><text size="4" maxlength="10"/></field>
        <field name="USER_WORK_CONTACT" title="${uiLabelMap.PartyPhoneNumber}"><text size="15" maxlength="15"/></field>
        <field name="USER_WORK_EXT" title="${uiLabelMap.PartyContactExt}"><text size="6" maxlength="10"/></field>
        <field name="USER_WORK_ALLOW_SOL" title="${uiLabelMap.PartyContactAllowSolicitation}?"><drop-down allow-empty="true"><option key="Y" description="${uiLabelMap.CommonY}"/><option key="N" description="${uiLabelMap.CommonN}"/></drop-down></field>
        <field name="FaxPhoneTitle" title="${uiLabelMap.PartyContactFaxPhoneNumber}" title-area-style="group-label"><display description=" " also-hidden="false"/></field>
        <field name="USER_FAX_COUNTRY" title="${uiLabelMap.CommonCountryCode}"><text size="4" maxlength="10"/></field>
        <field name="USER_FAX_AREA" title="${uiLabelMap.PartyAreaCode}"><text size="4" maxlength="10"/></field>
        <field name="USER_FAX_CONTACT" title="${uiLabelMap.PartyPhoneNumber}"><text size="15" maxlength="15"/></field>
        <field name="USER_FAX_EXT" title="${uiLabelMap.PartyContactExt}"><text size="6" maxlength="10"/></field>
        <field name="USER_FAX_ALLOW_SOL" title="${uiLabelMap.PartyContactAllowSolicitation}?"><drop-down allow-empty="true"><option key="Y" description="${uiLabelMap.CommonY}"/><option key="N" description="${uiLabelMap.CommonN}"/></drop-down></field>
        <field name="MobilePhoneTitle" title="${uiLabelMap.PartyContactMobilePhoneNumber}" title-area-style="group-label"><display description=" " also-hidden="false"/></field>
        <field name="USER_MOBILE_COUNTRY" title="${uiLabelMap.CommonCountryCode}"><text size="4" maxlength="10"/></field>
        <field name="USER_MOBILE_AREA" title="${uiLabelMap.PartyAreaCode}"><text size="4" maxlength="10"/></field>
        <field name="USER_MOBILE_CONTACT" title="${uiLabelMap.PartyPhoneNumber}"><text size="15" maxlength="15"/></field>
        <field name="USER_MOBILE_ALLOW_SOL" title="${uiLabelMap.PartyContactAllowSolicitation}?"><drop-down allow-empty="true"><option key="Y" description="${uiLabelMap.CommonY}"/><option key="N" description="${uiLabelMap.CommonN}"/></drop-down></field>
        <field name="EmailAddressTitle" title="${uiLabelMap.PartyEmailAddress}" title-area-style="group-label"><display description=" " also-hidden="false"/></field>
        <field use-when="require_email!=null" name="USER_EMAIL" title="${uiLabelMap.CommonEmail}" required-field="true"><text size="60" maxlength="250"/></field>
        <field use-when="require_email==null" name="USER_EMAIL" title="${uiLabelMap.CommonEmail}" required-field="true"><text size="60" maxlength="250"/></field>
        <field name="USER_EMAIL_ALLOW_SOL" title="${uiLabelMap.PartyContactAllowSolicitation}?"><drop-down allow-empty="true"><option key="Y" description="${uiLabelMap.CommonY}"/><option key="N" description="${uiLabelMap.CommonN}"/></drop-down></field>
        <!-- <field name="USER_ORDER_EMAIL" title="Order Emails (comma separated)" ><text size="60" maxlength="250"/></field> -->
        <#--  <field use-when="displayPassword==true" name="USERNAME" title="${uiLabelMap.CommonUsername}" required-field="true"><text size="30" maxlength="250"/></field>
        <field use-when="displayPassword==true" name="PASSWORD" title="${uiLabelMap.CommonPassword}" required-field="true"><password size="15" maxlength="250"/></field>
        <field use-when="displayPassword==true" name="CONFIRM_PASSWORD" title="${uiLabelMap.CommonPassword}" tooltip="* ${uiLabelMap.CommonConfirm}" required-field="true"><password size="15" maxlength="250"/></field>
        <field use-when="displayPassword!=true" name="USERNAME" title="${uiLabelMap.CommonUsername}" tooltip="* ${uiLabelMap.PartyTemporaryPassword}" required-field="true"><text size="30" maxlength="250"/></field>
        <!--<field name="RequiredNote" title=" "><display description="${uiLabelMap.PartyRequiredNote}" also-hidden="false"/></field> -->
        <#-- 
        <field name="PRODUCT_STORE_ID" title="Product Store" required-field="true">
            <drop-down>
                <entity-options entity-name="ProductStore" key-field-name="productStoreId" description="${storeName} (${productStoreId})">
                    <entity-order-by field-name="productStoreId"/>
                </entity-options>
            </drop-down>
        </field>
        -->

<@row>
  <@cell columns=6>
  <fieldset>
    <legend>${uiLabelMap.CommonAccountInformation}</legend>
    <#macro extraFieldContent args={}>
      <@fieldErrors fieldName="CUSTOMER_EMAIL"/>
      <@fields type="default-compact" ignoreParentField=true>
        <@allowSolicitationField name="CUSTOMER_EMAIL_ALLOW_SOL" />
      </@fields>
    </#macro>
    <@field type="input" name="CUSTOMER_EMAIL" id="CUSTOMER_EMAIL" value=(parameters.CUSTOMER_EMAIL!) 
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
            onClick="setEmailUsername();" onFocus="setLastFocused(this);" label=uiLabelMap.EcommerceUseEmailAddress 
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
      <@fieldErrors fieldName="CUSTOMER_ADDRESS1"/>
    </#macro>
    <@field type="input" name="CUSTOMER_ADDRESS1" id="CUSTOMER_ADDRESS1" value=(parameters.CUSTOMER_ADDRESS1!) 
        label=uiLabelMap.PartyAddressLine1 required=true postWidgetContent=extraFieldContent/>

    <#macro extraFieldContent args={}>
      <@fieldErrors fieldName="CUSTOMER_ADDRESS2"/>
    </#macro>
    <@field type="input" name="CUSTOMER_ADDRESS2" id="CUSTOMER_ADDRESS2" value=(parameters.CUSTOMER_ADDRESS2!) 
        label=uiLabelMap.PartyAddressLine2 postWidgetContent=extraFieldContent/>

    <#macro extraFieldContent args={}>
      <@fieldErrors fieldName="CUSTOMER_CITY"/>
    </#macro>
    <@field type="input" name="CUSTOMER_CITY" id="CUSTOMER_CITY" value=(parameters.CUSTOMER_CITY!) 
        label=uiLabelMap.PartyCity required=true postWidgetContent=extraFieldContent />

    <#macro extraFieldContent args={}>
      <@fieldErrors fieldName="CUSTOMER_POSTAL_CODE"/>
    </#macro>
    <@field type="input" name="CUSTOMER_POSTAL_CODE" id="CUSTOMER_POSTAL_CODE" value=(parameters.CUSTOMER_POSTAL_CODE!) 
        label=uiLabelMap.PartyZipCode required=true postWidgetContent=extraFieldContent />
  
    <#macro extraFieldContent args={}>
      <@fieldErrors fieldName="CUSTOMER_COUNTRY"/>
    </#macro>
    <@field type="select" name="CUSTOMER_COUNTRY" id="newuserform_countryGeoId" label=uiLabelMap.CommonCountry 
        required=true postWidgetContent=extraFieldContent>
        <@render resource="component://common/widget/CommonScreens.xml#countries" ctxVars={
            "currentCountryGeoId":parameters.CUSTOMER_COUNTRY!""
        }/>  
    </@field>
    
    <#macro extraFieldContent args={}>
      <@fieldErrors fieldName="CUSTOMER_STATE"/>
    </#macro>
    <@field type="select" name="CUSTOMER_STATE" id="newuserform_stateProvinceGeoId" required=true 
        label=uiLabelMap.PartyState postWidgetContent=extraFieldContent>
        <#-- Populated by JS -->
        <#if parameters.CUSTOMER_STATE?has_content>
          <option value="${parameters.CUSTOMER_STATE?html}">${parameters.CUSTOMER_STATE?html}</option>
        </#if>
    </@field>

    <#macro extraFieldContent args={}>
      <@fieldErrors fieldName="CUSTOMER_ADDRESS_ALLOW_SOL"/>
    </#macro>
    <#-- <@allowSolicitationField name="CUSTOMER_ADDRESS_ALLOW_SOL" postWidgetContent=extraFieldContent containerClass="+${styles.field_extra!}"  /> -->

  </fieldset>
  </@cell>

  <@cell columns=6>
  <fieldset>    
    <legend>${getLabel("CommunicationEventType.description.PHONE_COMMUNICATION", "PartyEntityLabels")}</legend>

    <@field type="generic" label=uiLabelMap.PartyHomePhone tooltip=tooltip required=false>    
        <@field type="input" inline=true size="8" maxlength="25" name="CUSTOMER_HOME_CONTACT" value=(params["CUSTOMER_HOME_CONTACT"])! tooltip=uiLabelMap.PartyContactNumber required=false/>
    </@field>
    <@field type="generic" label=uiLabelMap.PartyMobilePhone tooltip=tooltip required=false>    
        <@field type="input" inline=true size="8" maxlength="25" name="CUSTOMER_MOBILE_CONTACT" value=(params["CUSTOMER_MOBILE_CONTACT"])! tooltip=uiLabelMap.PartyContactNumber required=false/>
    </@field>
    <@field type="generic" label=uiLabelMap.PartyFaxNumber tooltip=tooltip required=false>    
        <@field type="input" inline=true size="8" maxlength="25" name="CUSTOMER_FAX_CONTACT" value=(params["CUSTOMER_FAX_CONTACT"])! tooltip=uiLabelMap.PartyContactNumber required=false/>
    </@field>

  </fieldset>
  </@cell>
</@row>

</form>

</@section>