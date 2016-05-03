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

<@initItemSelectionWithNewFormScript itemFieldClass="addr-select-radio" 
    newItems=[{"fieldId":"newshipaddrradio", "contentId":"newshipaddrcontent"}] />

</@script>

<#assign cart = shoppingCart!/>

<form method="post" name="checkoutInfoForm" id="checkoutInfoForm">

<#if userIsAnon>
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
          <#if (((parameters.homeSol)!"") == "Y")><option value="Y">${uiLabelMap.CommonYes}</option></#if>
          <#if (((parameters.homeSol)!"") == "N")><option value="N">${uiLabelMap.CommonNo}</option></#if>
          <option></option>
          <option value="Y">${uiLabelMap.CommonYes}</option>
          <option value="N">${uiLabelMap.CommonNo}</option>
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
          <#if (((parameters.workSol)!"") == "Y")><option value="Y">${uiLabelMap.CommonYes}</option></#if>
          <#if (((parameters.workSol)!"") == "N")><option value="N">${uiLabelMap.CommonNo}</option></#if>
          <option></option>
          <option value="Y">${uiLabelMap.CommonYes}</option>
          <option value="N">${uiLabelMap.CommonNo}</option>
        </@field>
      </@fields>
    </@field>

    <input type="hidden" name="emailContactMechId" value="${parameters.emailContactMechId!}"/>
    <@field type="input" name="emailAddress" value=(parameters.emailAddress!) required=true label=uiLabelMap.PartyEmailAddress/>

    <@field type="select" name="emailSol" label="${uiLabelMap.PartyAllowSolicitation}?">
      <#if (((parameters.emailSol)!"") == "Y")><option value="Y">${uiLabelMap.CommonYes}</option></#if>
      <#if (((parameters.emailSol)!"") == "N")><option value="N">${uiLabelMap.CommonNo}</option></#if>
      <option></option>
      <option value="Y">${uiLabelMap.CommonYes}</option>
      <option value="N">${uiLabelMap.CommonNo}</option>
    </@field>
</@section>
</#if>


<#-- Cato: if there are no shipping addresses available, always offer a form. If there are already addresses, offer a button to show the form. -->
<#assign showNewAddrForm = !shippingContactMechList?has_content || ((parameters.shipping_contact_mech_id!) == "_NEW_")>
        
<#macro menuContent menuArgs={}>
  <@menu args=menuArgs>
    <#-- Cato: FIXME?: splitship currently will not work for anon users. 
        This is a limitation of combining the personal info into the ship address page.
        However, it is probably acceptable to require an account for more advanced shipping features. -->
    <#if userHasAccount>
      <@menuitem type="link" href=makeOfbizUrl("splitship") class="+${styles.action_nav!} ${styles.action_update!}" text=uiLabelMap.OrderSplitShipment />
    </#if>
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
          <#-- Use standard floats for this?... -->
          <@addressList>
          <#list shippingContactMechList as shippingContactMech>
            <@addressEntry>
            <#assign shippingAddress = shippingContactMech.getRelatedOne("PostalAddress", false)>
            <#if parameters.shipping_contact_mech_id?has_content>
              <#assign checkThisAddress = (shippingAddress.contactMechId == parameters.shipping_contact_mech_id)>
            <#else>
              <#assign checkThisAddress = (shippingContactMech_index == 0 && !cart.getShippingContactMechId()?has_content) || ((cart.getShippingContactMechId()!"") == shippingAddress.contactMechId)/>
              <#-- Cato: auto check if it's the only one -->
              <#assign checkThisAddress = checkThisAddress || (shippingContactMechList?size == 1)>
            </#if>
            <#assign postfixContent></#assign>
            <#assign labelContent>
              <@formattedAddress address=shippingAddress updateLink="javascript:submitForm(document.checkoutInfoForm, 'EA', '${shippingAddress.contactMechId}');" emphasis=true />
            </#assign>
            <@commonInvField type="radio" name="shipping_contact_mech_id" value="${shippingAddress.contactMechId}" checked=checkThisAddress labelContent=labelContent postfixContent=postfixContent class="+addr-select-radio"/>
            </@addressEntry>
          </#list>
          </@addressList>
        </#if>
    

    <#macro newAddrFormContent args={}>
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
      <@render resource="component://shop/widget/CustomerScreens.xml#postalAddressFields" 
            ctxVars={
                "pafFieldNamePrefix":"newShipAddr_",
                "pafUseScripts":true}/>
    </#macro>

        <#-- Cato: _NEW_ asks the checkout process to check for _NEW_ and create a new contact mech if needed before the other events -->
        <#if shippingContactMechList?has_content>
          <#if parameters.shipping_contact_mech_id?has_content>
            <#assign checkThisAddress = (parameters.shipping_contact_mech_id == "_NEW_")>
          <#else>
            <#assign checkThisAddress = (!shippingContactMechList?has_content)>
          </#if>
          <#macro newAddrFormFieldContent args={}>
            <label for="newshipaddrradio"><@heading relLevel=+1>${uiLabelMap.PartyAddNewAddress}</@heading></label>
            <#--
            <div id="newshipaddrcontent"<#if !showNewAddrForm> style="display:none;"</#if> class="new-item-selection-content">
              <@newAddrFormContent />
            </div>
            -->
          </#macro>
          <@addressList>
            <@addressEntry>
          <@commonInvField type="radio" name="shipping_contact_mech_id" value="_NEW_" checked=checkThisAddress 
             id="newshipaddrradio" class="+addr-select-radio" label=uiLabelMap.PartyAddNewAddress/><#--labelContent=newAddrFormFieldContent-->
            </@addressEntry>
          </@addressList>
            <div id="newshipaddrcontent"<#if !showNewAddrForm> style="display:none;"</#if> class="new-item-selection-content">
              <@newAddrFormContent />
            </div>
        <#else>
          <div id="newshipaddrcontent"<#if !showNewAddrForm> style="display:none;"</#if>>
            <#-- Cato: title is not needed; implied
            <@heading>${uiLabelMap.PartyAddNewAddress}</@heading>-->
            <input type="hidden" name="shipping_contact_mech_id" value="_NEW_" />
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

  <#-- Cato: NOTE: Agreements only show if they are defined in the data for the specific customer.
      e.g. there are none for anon user by default. -->
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
                <#-- Cato: TODO: This needs to go through the agreement terms. In stock data there is little text to show. -->
                ${agreement.description!}
            </@modal>
          </#assign>
          <@commonInvField type="radio" name="agreementId" value=(agreement.agreementId!) checked=(agreements?size == 1) labelContent=labelContent />
        </#list>
      </#if>
    </@section>
  </#if>

  <#--</fieldset>-->
</@section>

</form>

<@checkoutActionsMenu directLinks=false formName="checkoutInfoForm" />
