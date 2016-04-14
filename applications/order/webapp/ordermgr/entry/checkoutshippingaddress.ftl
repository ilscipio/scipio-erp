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

</@script>

<#assign cart = shoppingCart!/>

<@section title="1)&nbsp;${uiLabelMap.OrderWhereShallWeShipIt}?">
<form method="post" name="checkoutInfoForm">
  <#--<fieldset>-->
    <input type="hidden" name="checkoutpage" value="shippingaddress"/>
            <@table type="fields" class="+${styles.table_spacing_tiny_hint!}" width="100%"> <#-- orig: class="" --> <#-- orig: cellspacing="0" --> <#-- orig: cellpadding="1" --> <#-- orig: border="0" -->
              <@tr>
                <@td colspan="2">
                  <a href="<@ofbizUrl>splitship</@ofbizUrl>" class="${styles.link_nav!} ${styles.action_update!}">${uiLabelMap.OrderSplitShipment}</a>
                  <a href="javascript:submitForm(document.checkoutInfoForm, 'NA', '');" class="${styles.link_nav!} ${styles.action_add!}">${uiLabelMap.PartyAddNewAddress}</a>
                  <#if (cart.getShipGroupSize() > 1)>
                    <div class="${styles.text_color_alert!}">${uiLabelMap.OrderNOTEMultipleShipmentsExist}</div>
                  </#if>
                </@td>
              </@tr>
               <#if shippingContactMechList?has_content>
                 <@tr type="util"><@td colspan="2"><hr /></@td></@tr>
                 <#list shippingContactMechList as shippingContactMech>
                   <#assign shippingAddress = shippingContactMech.getRelatedOne("PostalAddress", false)>
                   <#assign checkThisAddress = (shippingContactMech_index == 0 && !cart.getShippingContactMechId()?has_content) || (cart.getShippingContactMechId()?default("") == shippingAddress.contactMechId)/>
                   <@tr>
                     <@td valign="top" width="1%" nowrap="nowrap">
                       <@field type="radio" inline=true name="shipping_contact_mech_id" value="${shippingAddress.contactMechId}" checked=checkThisAddress />
                     </@td>
                     <@td valign="top" width="99%" nowrap="nowrap">
                         <#if shippingAddress.toName?has_content><b>${uiLabelMap.CommonTo}:</b>&nbsp;${shippingAddress.toName}<br /></#if>
                         <#if shippingAddress.attnName?has_content><b>${uiLabelMap.PartyAddrAttnName}:</b>&nbsp;${shippingAddress.attnName}<br /></#if>
                         <#if shippingAddress.address1?has_content>${shippingAddress.address1}<br /></#if>
                         <#if shippingAddress.address2?has_content>${shippingAddress.address2}<br /></#if>
                         <#if shippingAddress.city?has_content>${shippingAddress.city}</#if>
                         <#if shippingAddress.stateProvinceGeoId?has_content><br />${shippingAddress.stateProvinceGeoId}</#if>
                         <#if shippingAddress.postalCode?has_content><br />${shippingAddress.postalCode}</#if>
                         <#if shippingAddress.countryGeoId?has_content><br />${shippingAddress.countryGeoId}</#if>
                         <a href="javascript:submitForm(document.checkoutInfoForm, 'EA', '${shippingAddress.contactMechId}');" class="${styles.link_run_session!} ${styles.action_update!}">${uiLabelMap.CommonUpdate}</a>
                       </@td>
                   </@tr>
                   <@tr type="util"><@td colspan="2"><hr /></@td></@tr>
                 </#list>
               </#if>
              </@table>
            <@section title=uiLabelMap.AccountingAgreementInformation>
               <@table type="fields" class="+${styles.table_spacing_tiny_hint!}"> <#-- orig: class="" --> <#-- orig: cellspacing="" -->
                 <#if agreements??>
                   <#if agreements.size() != 1>
                     <@tr>
                       <@td>&nbsp;</@td>
                       <@td align='left' valign='top' nowrap="nowrap">
                         <div class="tableheadtext">
                           ${uiLabelMap.OrderSelectAgreement}
                         </div>
                       </@td>
                       <@td>&nbsp;</@td>
                       <@td valign='middle'>
                         <div class="tabletext" valign="top">
                           <select name="agreementId">
                             <#list agreements as agreement>
                               <option value="${agreement.agreementId!}">${agreement.agreementId} - ${agreement.description!}</option>
                             </#list>
                           </select>
                         </div>
                       </@td>
                     </@tr>
                   <#else>
                   <@fields type="default-nolabels">
                     <#list agreements as agreement>
                        <@field type="radio" name="agreementId" value="${agreement.agreementId!}" checked=checkThisAddress label="${agreement.description!} will be used for this order." />
                     </#list>
                   </@fields>
                   </#if>
                 </#if>
               </@table>
            </@section>

            <#-- Party Tax Info -->
            <@section title=uiLabelMap.PartyTaxIdentification>
                <@render resource="component://order/widget/ordermgr/OrderEntryOrderScreens.xml#customertaxinfo" /> 
            </@section>

  <#--</fieldset>-->
</form>
</@section>

<@menu type="button">
  <@menuitem type="link" href="javascript:submitForm(document.checkoutInfoForm, 'CS', '');" text=uiLabelMap.OrderBacktoShoppingCart class="+${styles.action_nav!} ${styles.action_cancel!}" />
  <@menuitem type="link" href="javascript:submitForm(document.checkoutInfoForm, 'DN', '');" text=uiLabelMap.CommonNext class="+${styles.action_run_session!} ${styles.action_continue!}" />
</@menu>

