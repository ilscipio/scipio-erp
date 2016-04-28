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
<#-- Cato: Duplicated (forcefully) from component://order/webapp/ordermgr/entry/checkoutshippingaddress.ftl -->

<#include "ordercommon.ftl">

<@script>
function submitForm(form, mode, value) {
    if (mode == "DN") {
        // done action; checkout
        form.action="<@ofbizUrl>checkoutoptions</@ofbizUrl>";
        form.submit();
    } else if (mode == "CS") {
        // continue shopping
        form.action="<@ofbizUrl>updateCheckoutOptions/showcart</@ofbizUrl>";
        form.submit();
    } else if (mode == "NA") {
        // new address
        form.action="<@ofbizUrl>updateCheckoutOptions/editcontactmech?preContactMechTypeId=POSTAL_ADDRESS&contactMechPurposeTypeId=SHIPPING_LOCATION&DONE_PAGE=checkoutshippingaddress</@ofbizUrl>";
        form.submit();
    } else if (mode == "EA") {
        // edit address
        form.action="<@ofbizUrl>updateCheckoutOptions/editcontactmech?DONE_PAGE=checkoutshippingaddress&contactMechId="+value+"</@ofbizUrl>";
        form.submit();
    }
}

function toggleBillingAccount(box) {
    var amountName = box.value + "_amount";
    box.checked = true;
    box.form.elements[amountName].disabled = false;

    for (var i = 0; i < box.form.elements[box.name].length; i++) {
        if (!box.form.elements[box.name][i].checked) {
            box.form.elements[box.form.elements[box.name][i].value + "_amount"].disabled = true;
        }
    }
}

function updateAddrVisibility() {
    updateNewAddrFormVisibility();
}

<#-- Cato: show new address form -->
function updateNewAddrFormVisibility() {
    var radio = jQuery('#newshipaddrfield .newshipaddrradioarea input');
    if (radio.length > 0) {
        if (radio.is(":checked")) {
            jQuery('#newshipaddrcontent').show();
            jQuery('#newshipaddrcontent').focus();
        } else {
            jQuery('#newshipaddrcontent').hide();
        }
    }
}

jQuery(document).ready(function() {
    <#-- FIXME: error message div for ajax failures -->
    jQuery("#newShipAddr_countryGeoId").change(function() {
        getAssociatedStateList('newShipAddr_countryGeoId', 'newShipAddr_stateProvinceGeoId', null, null);
    });
    getAssociatedStateList('newShipAddr_countryGeoId', 'newShipAddr_stateProvinceGeoId', null, null);
    
    <#-- Cato: Needed for page refreshes to work -->
    updateAddrVisibility();
    
    <#-- Cato: FIXME? Which will work currently depends on markup -->
    jQuery('input.addr-select-radio').change(updateAddrVisibility);
    jQuery('.addr-select-radio input').change(updateAddrVisibility);
});

</@script>

<#assign cart = shoppingCart!/>

<form method="post" name="checkoutInfoForm" id="checkoutInfoForm">

<#if !userLogin?has_content || userLogin.userLoginId == "anonymous">
<#-- Cato: New form to allow creation of a temp anon user (based on old/legacy anon checkout)
    at the same time the ship address form is submitted -->
<@section title=uiLabelMap.EcommerceYourNamePhoneAndEmail>
   <#-- Cato: this triggers the user creation (or update) before the main event -->
   <input type="hidden" name="createUpdateAnonUser" value="Y"/>

   <#-- Cato: NOTE: fields duplicated from old checkout custsettings.ftl -->

   <#-- Cato: WARN: this partyId is needed even if userLogin.partyId is already set -->
   <input type="hidden" name="partyId" value="${parameters.partyId!}"/>

   <@field type="select" name="personalTitle" label=uiLabelMap.CommonTitle>
      <#if requestParameters.personalTitle?has_content>
        <option>${parameters.personalTitle}</option>
        <option value="${parameters.personalTitle}">--</option>
      <#else>
        <option value="">${uiLabelMap.CommonSelectOne}</option>
      </#if>
      <option>${uiLabelMap.CommonTitleMr}</option>
      <option>${uiLabelMap.CommonTitleMrs}</option>
      <option>${uiLabelMap.CommonTitleMs}</option>
      <option>${uiLabelMap.CommonTitleDr}</option>
    </@field>
    <@field type="input" name="firstName" value=(parameters.firstName!) required=true label=uiLabelMap.PartyFirstName/>
    <@field type="input" name="middleName" value=(parameters.middleName!) label=uiLabelMap.PartyMiddleInitial/>
    <@field type="input" name="lastName" value=(parameters.lastName!) required=true label=uiLabelMap.PartyLastName/>
    <@field type="input" name="suffix" value=(parameters.suffix!) label=uiLabelMap.PartySuffix/>

    <input type="hidden" name="homePhoneContactMechId" value="${parameters.homePhoneContactMechId!}"/>
    <@field type="generic" label=uiLabelMap.PartyHomePhone required=true tooltip=false>
        <@field type="input" inline=true size="4" maxlength="10" name="homeCountryCode" value=(parameters.homeCountryCode!) required=true tooltip=uiLabelMap.CommonCountryCode/>
        -&nbsp;<@field type="input" inline=true size="4" maxlength="10" name="homeAreaCode" value=(parameters.homeAreaCode!) required=true tooltip=uiLabelMap.PartyAreaCode />
        -&nbsp;<@field type="input" inline=true size="15" maxlength="15" name="homeContactNumber" value=(parameters.homeContactNumber!) required=true tooltip=uiLabelMap.PartyContactNumber />
        &nbsp;${uiLabelMap.PartyContactExt}&nbsp;<@field type="input" inline=true size="6" maxlength="10" name="homeExt" value=(parameters.homeExt!) tooltip=uiLabelMap.PartyExtension />

      <@fields type="default-compact" ignoreParentField=true>
        <@field type="select" name="homeSol" label="${uiLabelMap.PartyAllowSolicitation}?">
          <#if (((parameters.homeSol)!"") == "Y")><option value="Y">${uiLabelMap.CommonY}</option></#if>
          <#if (((parameters.homeSol)!"") == "N")><option value="N">${uiLabelMap.CommonN}</option></#if>
          <option></option>
          <option value="Y">${uiLabelMap.CommonY}</option>
          <option value="N">${uiLabelMap.CommonN}</option>
        </@field>
      </@fields>
    </@field>

    <input type="hidden" name="workPhoneContactMechId" value="${parameters.workPhoneContactMechId!}"/>
    <@field type="generic" label=uiLabelMap.PartyBusinessPhone>
        <@field type="input" inline=true size="4" maxlength="10" name="workCountryCode" value=(parameters.workCountryCode!) tooltip=uiLabelMap.CommonCountryCode/>
        -&nbsp;<@field type="input" inline=true size="4" maxlength="10" name="workAreaCode" value=(parameters.workAreaCode!) tooltip=uiLabelMap.PartyAreaCode />
        -&nbsp;<@field type="input" inline=true size="15" maxlength="15" name="workContactNumber" value=(parameters.workContactNumber!) tooltip=uiLabelMap.PartyContactNumber />
        &nbsp;${uiLabelMap.PartyContactExt}&nbsp;<@field type="input" inline=true size="6" maxlength="10" name="workExt" value=(parameters.workExt!) tooltip=uiLabelMap.PartyExtension />

      <@fields type="default-compact" ignoreParentField=true>
        <@field type="select" name="workSol" label="${uiLabelMap.PartyAllowSolicitation}?">
          <#if (((parameters.workSol)!"") == "Y")><option value="Y">${uiLabelMap.CommonY}</option></#if>
          <#if (((parameters.workSol)!"") == "N")><option value="N">${uiLabelMap.CommonN}</option></#if>
          <option></option>
          <option value="Y">${uiLabelMap.CommonY}</option>
          <option value="N">${uiLabelMap.CommonN}</option>
        </@field>
      </@fields>
    </@field>

    <input type="hidden" name="emailContactMechId" value="${parameters.emailContactMechId!}"/>
    <@field type="input" name="emailAddress" value=(parameters.emailAddress!) required=true label=uiLabelMap.PartyEmailAddress/>

    <@field type="select" name="emailSol" label="${uiLabelMap.PartyAllowSolicitation}?">
      <#if (((parameters.emailSol)!"") == "Y")><option value="Y">${uiLabelMap.CommonY}</option></#if>
      <#if (((parameters.emailSol)!"") == "N")><option value="N">${uiLabelMap.CommonN}</option></#if>
      <option></option>
      <option value="Y">${uiLabelMap.CommonY}</option>
      <option value="N">${uiLabelMap.CommonN}</option>
    </@field>
</@section>
</#if>


<#-- Cato: if there are no shipping addresses available, always offer a form. If there are already addresses, offer a button to show the form. -->
<#assign showNewAddrForm = !shippingContactMechList?has_content || ((parameters.shipping_contact_mech_id!) == "_NEW_RECORD_")>
        
<#macro menuContent menuArgs={}>
  <@menu args=menuArgs>
    <@menuitem type="link" href=makeOfbizUrl("splitship") class="+${styles.action_nav!} ${styles.action_update!}" text=uiLabelMap.OrderSplitShipment />
    <#-- Cato: This old link becomes redundant with the addition of an inlined form (below). 
    <@menuitem type="link" href="javascript:submitForm(document.checkoutInfoForm, 'NA', '');" class="+${styles.action_nav!} ${styles.action_add!}" text=uiLabelMap.PartyAddNewAddress />
    -->
    <#-- Integrate this with radio button instead so more direct
    <#if shippingContactMechList?has_content>
      <@menuitem type="link" href="javascript:updateNewAddrFormVisibility();" class="+${styles.action_nav_local!} ${styles.action_add!}" text=uiLabelMap.PartyAddNewAddress />
    </#if>
    -->
  </@menu>
</#macro>
<@section title="${uiLabelMap.OrderWhereShallWeShipIt}?" menuContent=menuContent><#-- Cato: No numbers for multi-page checkouts, make checkout too rigid: 1)&nbsp;${uiLabelMap.OrderWhereShallWeShipIt}? -->
  <#if (cart.getShipGroupSize() > 1)>
    <@alert type="info">${uiLabelMap.OrderNOTEMultipleShipmentsExist}</@alert>
  </#if>

  <#--<fieldset>-->
    <input type="hidden" name="checkoutpage" value="shippingaddress"/>
    <@section>
        <#if shippingContactMechList?has_content>
          <#--<@tr type="util"><@td colspan="2"><hr /></@td></@tr>-->
          <#list shippingContactMechList as shippingContactMech>
            <#assign shippingAddress = shippingContactMech.getRelatedOne("PostalAddress", false)>
            <#if parameters.shipping_contact_mech_id?has_content>
              <#assign checkThisAddress = (shippingAddress.contactMechId == parameters.shipping_contact_mech_id)>
            <#else>
              <#assign checkThisAddress = (shippingContactMech_index == 0 && !cart.getShippingContactMechId()?has_content) || (cart.getShippingContactMechId()?default("") == shippingAddress.contactMechId)/>
              <#-- Cato: auto check if it's the only one -->
              <#assign checkThisAddress = checkThisAddress || (shippingContactMechList?size == 1)>
            </#if>
            <#assign postfixContent></#assign>
            <#assign labelContent>
                <#if shippingAddress.toName?has_content><b>${uiLabelMap.CommonTo}:</b>&nbsp;${shippingAddress.toName}<br /></#if>
                <#if shippingAddress.attnName?has_content><b>${uiLabelMap.PartyAddrAttnName}:</b>&nbsp;${shippingAddress.attnName}<br /></#if>
                <#if shippingAddress.address1?has_content>${shippingAddress.address1}<br /></#if>
                <#if shippingAddress.address2?has_content>${shippingAddress.address2}<br /></#if>
                <#if shippingAddress.city?has_content>${shippingAddress.city}</#if>
                <#if shippingAddress.stateProvinceGeoId?has_content><br />${shippingAddress.stateProvinceGeoId}</#if>
                <#if shippingAddress.postalCode?has_content><br />${shippingAddress.postalCode}</#if>
                <#if shippingAddress.countryGeoId?has_content><br />${shippingAddress.countryGeoId}</#if>
                <a href="javascript:submitForm(document.checkoutInfoForm, 'EA', '${shippingAddress.contactMechId}');" class="${styles.link_run_session!} ${styles.action_update!}">${uiLabelMap.CommonUpdate}</a>
            </#assign>
            <@checkoutInvField type="radio" name="shipping_contact_mech_id" value="${shippingAddress.contactMechId}" checked=checkThisAddress labelContent=labelContent postfixContent=postfixContent class="+addr-select-radio"/>
            <#--<@tr type="util"><@td colspan="2"><hr /></@td></@tr>-->
          </#list>
        </#if>

    <#macro newAddrFormContent args={} fieldsType="default">
    <@fields type=fieldsType ignoreParentField=true>
      <input type="hidden" name="new_ship_addr_prefix" value="newShipAddr_" />
      <input type="hidden" name="newShipAddr_partyId" value="${parameters.partyId!}" />
      <input type="hidden" name="newShipAddr_productStoreId" value="${productStoreId!}" />
      
      <#-- Cato: UNTESTED: If this is uncommented, this address takes over the SHIPPING_LOCATION purpose from the last address
           and saves as default in profile due to way createPostalAddressAndPurposes service written.
           It's nice that it sets as default in profile (maybe), but I'm not sure we want it to remove the SHIPPING_LOCATION
           purpose from the other address. With it commented, it simply adds another SHIPPING_LOCATION and does not set in
           profile.
      <input type="hidden" name="newShipAddr_setShippingPurpose" value="Y" />-->
      
      <#-- Cato: Fields copied from editcontactmech.ftl (+ newShipAddr_ prefix) -->

      <@field type="input" label="${uiLabelMap.PartyToName}" size="30" maxlength="60" name="newShipAddr_toName" value="" />
      <@field type="input" label="${uiLabelMap.PartyAttentionName}" size="30" maxlength="60" name="newShipAddr_attnName" value="" />
      <@field type="input" label="${uiLabelMap.PartyAddressLine1}" required=true size="30" maxlength="30" name="newShipAddr_address1" value="" />
      <@field type="input" label="${uiLabelMap.PartyAddressLine2}" size="30" maxlength="30" name="newShipAddr_address2" value="" />
      <@field type="input" label="${uiLabelMap.PartyCity}" required=true size="30" maxlength="30" name="newShipAddr_city" value="" />
      <@field type="select" label="${uiLabelMap.PartyState}" name="newShipAddr_stateProvinceGeoId" id="newShipAddr_stateProvinceGeoId">
      </@field>      
      <@field type="input" label="${uiLabelMap.PartyZipCode}" required=true size="12" maxlength="10" name="newShipAddr_postalCode" value="" />
      <@field type="select" label="${uiLabelMap.CommonCountry}" name="newShipAddr_countryGeoId" id="newShipAddr_countryGeoId">
          <@render resource="component://common/widget/CommonScreens.xml#countries" />        
          <#if (postalAddress??) && (postalAddress.countryGeoId??)>
            <#assign defaultCountryGeoId = postalAddress.countryGeoId>
          <#else>
            <#assign defaultCountryGeoId = getPropertyValue("general.properties", "country.geo.id.default")!"">
          </#if>
          <option selected="selected" value="${defaultCountryGeoId}">
          <#assign countryGeo = delegator.findOne("Geo",{"geoId":defaultCountryGeoId}, false)>
            ${countryGeo.get("geoName",locale)}
          </option>
      </@field>
      <@field type="select" label="${uiLabelMap.PartyAllowSolicitation}?" name="newShipAddr_allowSolicitation">
        <#if (((partyContactMechData.allowSolicitation)!"") == "Y")><option value="Y">${uiLabelMap.CommonY}</option></#if>
        <#if (((partyContactMechData.allowSolicitation)!"") == "N")><option value="N">${uiLabelMap.CommonN}</option></#if>
        <option></option>
        <option value="Y">${uiLabelMap.CommonY}</option>
        <option value="N">${uiLabelMap.CommonN}</option>
      </@field>
    </@fields>
    </#macro>

        <#-- Cato: _NEW_RECORD_ asks the checkout process to check for _NEW_RECORD_ and create a new contact mech if needed before the other events -->
        <#if shippingContactMechList?has_content>
          <#if parameters.shipping_contact_mech_id?has_content>
            <#assign checkThisAddress = (parameters.shipping_contact_mech_id == "_NEW_RECORD_")>
          <#else>
            <#assign checkThisAddress = (!shippingContactMechList?has_content)>
          </#if>
          <#macro newAddrFormContentAndTitle args={}>
            <label for="newshipaddrradio"><@heading relLevel=+1>${uiLabelMap.PartyAddNewAddress}</@heading></label>
            <div id="newshipaddrcontent"<#if !showNewAddrForm> style="display:none;"</#if>>
            <#-- Cato: Can pass this to change the look/markup: <@newAddrFormContent fieldsType="default-compact"... -->
            <@newAddrFormContent />
            </div>
          </#macro>
          <@checkoutInvField type="radio" name="shipping_contact_mech_id" value="_NEW_RECORD_" checked=checkThisAddress 
            labelContent=newAddrFormContentAndTitle widgetAreaClass="+newshipaddrradioarea" 
            id="newshipaddrradio" containerId="newshipaddrfield" 
            class="+addr-select-radio"/>
        <#else>
          <div id="newshipaddrcontent"<#if !showNewAddrForm> style="display:none;"</#if>>
            <@heading>${uiLabelMap.PartyAddNewAddress}</@heading>
            <input type="hidden" name="shipping_contact_mech_id" value="_NEW_RECORD_" />
            <@newAddrFormContent />
          </div>
        </#if>
    
    </@section>

    <#-- Party Tax Info -->
    <@section title=uiLabelMap.PartyTaxIdentification>
      <#-- Cato: NOTE: Can simply add this around to change the look:
      <@fields type="default-compact"> -->
        <@render resource="component://shop/widget/OrderScreens.xml#customertaxinfo" /> 
      <#--</@fields>-->
    </@section>

  <#if agreements?has_content>
    <@section title=uiLabelMap.AccountingAgreementInformation>
      <#-- Cato: for shop, use only select boxes, otherwise can't link to anything -->
      <#if false && agreements.size() != 1>
          <@field type="select" label=uiLabelMap.OrderSelectAgreement name="agreementId">
            <#list agreements as agreement>
              <option value="${agreement.agreementId!}">${agreement.agreementId} - ${agreement.description!}</option>
            </#list>
          </@field>
      <#else>
        <#list agreements as agreement>
          <#-- Cato: I don't know why this was the condition: checked=checkThisAddress -->
          <#assign labelContent>${agreement.description!} will be used for this order. 
            <@modal id="agreement_info_${agreement.agreementId!}" label="Click here for more details">
                <#-- Cato: I don't know what else to put here at current time -->
                ${agreement.description!}
            </@modal>
          </#assign>
          <@checkoutInvField type="radio" name="agreementId" value=(agreement.agreementId!) checked=(agreements?size == 1) labelContent=labelContent />
        </#list>
      </#if>
    </@section>
  </#if>

  <#--</fieldset>-->
</@section>

</form>

<@checkoutActionsMenu directLinks=false formName="checkoutInfoForm" />
