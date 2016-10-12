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
<#-- Scipio: Duplicated (forcefully) from component://order/webapp/ordermgr/entry/checkoutshippingaddress.ftl -->

<#-- Scipio: TODO?: Anon user fields for business account (with party group) -->


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

<@initItemSelectionWithContentFormScript itemFieldClass="addr-select-radio" 
    contentItems=[{"fieldId":"newshipaddrradio", "contentId":"newshipaddrcontent"}] />

</@script>

<#assign cart = shoppingCart!/>

<form method="post" name="checkoutInfoForm" id="checkoutInfoForm">

<#if userIsAnon>
<#-- Scipio: New form to allow creation of a temp anon user (based on old/legacy anon checkout)
    at the same time the ship address form is submitted -->
<@section title=uiLabelMap.EcommerceYourNamePhoneAndEmail>
  <#-- This makes it impossible to align with others below
  Instead, using @fields to get only widget part to limit columns
  <@row>
    <@cell columns=6>
  -->
  <@fields type="inherit-all" fieldArgs={"gridArgs":{"totalLarge":8}}>

   <#-- Scipio: this triggers the user creation (or update) before the main event -->
   <input type="hidden" name="createUpdateAnonUser" value="Y"/>

   <#-- Scipio: NOTE: fields duplicated from old checkout custsettings.ftl -->

   <#-- Scipio: WARN: this partyId is needed even if userLogin.partyId is already set! -->
   <input type="hidden" name="partyId" value="${parameters.partyId!}"/>

   <@render resource="component://shop/widget/CustomerScreens.xml#customerBasicFields" ctxVars={}/>

  </@fields>
  <#--
    </@cell>
  </@row>
  -->

</@section>
</#if>


<#-- Scipio: if there are no shipping addresses available, always offer a form. If there are already addresses, offer a button to show the form. -->
<#assign showNewAddrForm = !shippingContactMechList?has_content || ((parameters.shipping_contact_mech_id!) == "_NEW_")>
        
<#macro menuContent menuArgs={}>
  <@menu args=menuArgs>
    <#-- Scipio: FIXME?: splitship currently will not work for anon users. 
        This is a limitation of combining the personal info into the ship address page.
        However, it is probably acceptable to require an account for more advanced shipping features. -->
  <#-- TODO?: Implement split ship page and uncomment here
    <#if userHasAccount>
      <@menuitem type="link" href=makeOfbizUrl("splitship") class="+${styles.action_nav!} ${styles.action_update!}" text=uiLabelMap.OrderSplitShipment />
    </#if>
  -->
    <#-- Scipio: This old link becomes redundant with the addition of an inlined form (below). 
    <@menuitem type="link" href="javascript:submitForm(document.checkoutInfoForm, 'NA', '');" class="+${styles.action_nav!} ${styles.action_add!}" text=uiLabelMap.PartyAddNewAddress />
    -->
    <#-- Integrate this with radio button instead so more direct
    <#if shippingContactMechList?has_content>
      <@menuitem type="link" href="javascript:updateNewAddrFormVisibility();" class="+${styles.action_nav_local!} ${styles.action_add!}" text=uiLabelMap.PartyAddNewAddress />
    </#if>
    -->
  </@menu>
</#macro>
<@section title="${rawString(uiLabelMap.OrderWhereShallWeShipIt)}?" menuContent=menuContent><#-- Scipio: No numbers for multi-page checkouts, make checkout too rigid: 1) ${uiLabelMap.OrderWhereShallWeShipIt}? -->
  <#if (cart.getShipGroupSize() > 1)>
    <@alert type="info">${uiLabelMap.OrderNOTEMultipleShipmentsExist}</@alert>
  </#if>

  <#--<fieldset>-->
    <input type="hidden" name="checkoutpage" value="shippingaddress"/>
    <@section>
      <@addressList>
        <#if shippingContactMechList?has_content>
          <#-- Use standard floats for this?... -->

          <#list shippingContactMechList as shippingContactMech>
            <@addressEntry>
            <#assign shippingAddress = shippingContactMech.getRelatedOne("PostalAddress", false)>
            <#if parameters.shipping_contact_mech_id?has_content>
              <#assign checkThisAddress = (shippingAddress.contactMechId == parameters.shipping_contact_mech_id)>
            <#else>
              <#assign checkThisAddress = (shippingContactMech_index == 0 && !cart.getShippingContactMechId()?has_content) || ((cart.getShippingContactMechId()!"") == shippingAddress.contactMechId)/>
              <#-- Scipio: auto check if it's the only one -->
              <#assign checkThisAddress = checkThisAddress || (shippingContactMechList?size == 1)>
            </#if>
            <#assign postfixContent></#assign>
            <#assign labelContent>
              <@formattedAddress usePanel=true address=shippingAddress updateLink="javascript:submitForm(document.checkoutInfoForm, 'EA', '${shippingAddress.contactMechId}');" emphasis=true />
            </#assign>
            <@checkAddressInvField type="radio" name="shipping_contact_mech_id" value=shippingAddress.contactMechId checked=checkThisAddress labelContent=labelContent postfixContent=postfixContent class="+addr-select-radio"/>
            </@addressEntry>
          </#list>

        </#if>

        <#-- Scipio: _NEW_ asks the checkout process to check for _NEW_ and create a new contact mech if needed before the other events -->
        <#if shippingContactMechList?has_content>
          <#if parameters.shipping_contact_mech_id?has_content>
            <#assign checkThisAddress = (parameters.shipping_contact_mech_id == "_NEW_")>
          <#else>
            <#assign checkThisAddress = (!shippingContactMechList?has_content)>
          </#if>
          <@addressEntry ownLine=true>
            <#assign labelContent><span class="new-address-radio-label">${uiLabelMap.PartyAddNewAddress}</span></#assign>
            <@checkAddressInvField type="radio" name="shipping_contact_mech_id" value="_NEW_" checked=checkThisAddress 
               id="newshipaddrradio" class="+addr-select-radio" labelContent=labelContent/><#--label=uiLabelMap.PartyAddNewAddress labelContent=newAddrFormFieldContent-->
          </@addressEntry>
        </#if>
    
      </@addressList>
    
        <#macro newAddrFormContent args={}>
          <#-- This makes it impossible to align with others below
          Instead, using @fields to get only widget part to limit columns
          <@row>
            <@cell columns=6>
          -->
          <@fields type="inherit-all" fieldArgs={"gridArgs":{"totalLarge":8}}>
            <input type="hidden" name="newShipAddr_partyId" value="${parameters.partyId!}" />
            <input type="hidden" name="newShipAddr_productStoreId" value="${productStoreId!}" />
            <#-- Scipio: UNTESTED: If this is uncommented, this address takes over the SHIPPING_LOCATION purpose from the last address
                and saves as default in profile due to way createPostalAddressAndPurposes service written.
                It's nice that it sets as default in profile (maybe), but I'm not sure we want it to remove the SHIPPING_LOCATION
                purpose from the other address. With it commented, it simply adds another SHIPPING_LOCATION and does not set in
                profile.
            <input type="hidden" name="newShipAddr_setShippingPurpose" value="Y" />-->
          
            <#-- Scipio: Fields copied from editcontactmech.ftl (+ newShipAddr_ prefix) -->
            <@render resource="component://shop/widget/CustomerScreens.xml#postalAddressFields" 
                ctxVars={
                    "pafFieldNamePrefix":"newShipAddr_",
                    "pafUseScripts":true,
                    "pafParams":newShipAddrParams!parameters
                    } />
          </@fields>
          <#--
            </@cell>
          </@row>
          -->
        </#macro>
    
        <#if shippingContactMechList?has_content>
          <#macro newAddrFormFieldContent args={}>
            <#--
            <label for="newshipaddrradio"><@heading relLevel=+1>${uiLabelMap.PartyAddNewAddress}</@heading></label>
            -->
            <div id="newshipaddrcontent"<#if !showNewAddrForm> style="display:none;"</#if> class="new-item-selection-content">
              <@newAddrFormContent />
            </div>
          </#macro>
          <@newAddrFormFieldContent />
        <#else>
          <div id="newshipaddrcontent"<#if !showNewAddrForm> style="display:none;"</#if>>
            <input type="hidden" name="shipping_contact_mech_id" value="_NEW_" />
            <#-- Scipio: title is not needed; implied
            <@heading>${uiLabelMap.PartyAddNewAddress}</@heading>-->
            <@newAddrFormContent />
          </div>
        </#if>
    
    </@section>

  <#-- TODO: Uncomment this once template OK
  <#if userIsBusiness>-->
  <#if true>
    <#-- Party Tax Info -->
    <@section>
    <@row>
      <@cell columns=2>
        <label><strong>${uiLabelMap.PartyTaxIdentification}</strong></label>
      </@cell>
      <@cell columns=6 last=true>
        <@section>
        <#-- Scipio: NOTE: Can simply use or omit default-compact to change the look -->
        <@fields type="default-compact">
        <@render resource="component://shop/widget/OrderScreens.xml#customertaxinfo" /> 
        </@fields>
        </@section>
      </@cell>
    </@row>
    </@section>

  <#-- Scipio: NOTE: Agreements only show if they are defined in the data for the specific customer.
      e.g. there are none for anon user by default. -->
  <#if agreements?has_content>
    <@section>
    <@row>
      <@cell columns=2>
        <label><strong>${uiLabelMap.AccountingAgreementInformation}</strong></label>
      </@cell>
      <@cell columns=10>
        <@section>
          <@fields type="default-compact">
          <#-- Scipio: for shop, use only radios; can't link to anything with select -->
          <#if false && agreements.size() != 1>
              <@field type="select" label=uiLabelMap.OrderSelectAgreement name="agreementId">
                <#list agreements as agreement>
                  <option value="${agreement.agreementId!}">${agreement.agreementId} - ${agreement.description!}</option>
                </#list>
              </@field>
          <#else>
            <#-- not really needed currently, readd later maybe
            <@field type="generic" label=uiLabelMap.AccountingAgreement name="agreementId">-->
                <#list agreements as agreement>
                  <#-- Scipio: I don't know why this was the condition: checked=checkThisAddress -->
                  <#assign fieldLabel>${agreement.description!} will be used for this order. 
                    <@modal id="agreement_info_${rawString(agreement.agreementId!)}" label=uiLabelMap.CommonClickHereDetails>
                        <#-- Scipio: TODO: This needs to go through the agreement terms. In stock data there is little text to show. -->
                        ${agreement.description!}
                    </@modal>
                  </#assign>
                  <#-- FIXME?: wrapAsRaw is not ideal -->
                  <@field type="radio" inlineItems=false name="agreementId" value=(agreement.agreementId!) checked=(agreements?size == 1) label=wrapAsRaw(fieldLabel, 'html') />
                </#list>
            <#--
            </@field>-->
          </#if>
        </@fields>
        </@section>
      </@cell>
    </@row>
    </@section>
  </#if>

  </#if>

  <#--</fieldset>-->
</@section>

</form>

<@checkoutActionsMenu directLinks=false formName="checkoutInfoForm" />
