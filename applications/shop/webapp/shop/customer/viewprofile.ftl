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

<#if party??>
<#-- Main Heading -->

<p>
  ${uiLabelMap.CommonWelcome},<#--${uiLabelMap.PartyTheProfileOf}-->
  <#if person??>
    ${person.personalTitle!} ${person.firstName!} ${person.middleName!} ${person.lastName!} ${person.suffix!}
  <#else>
    ${uiLabelMap.PartyNewUser}
  </#if>
  !
</p>

<#-- Loyalty points -->
<#if monthsToInclude?? && totalSubRemainingAmount?? && totalOrders??>
  <@section title=uiLabelMap.EcommerceLoyaltyPoints>
    ${uiLabelMap.EcommerceYouHave} ${totalSubRemainingAmount} ${uiLabelMap.EcommercePointsFrom} ${totalOrders} ${uiLabelMap.EcommerceOrderInLast} ${monthsToInclude} ${uiLabelMap.EcommerceMonths}.
  </@section>
</#if>

<@menu type="button">
  <#-- SCIPIO: No store in the world will encourage to view expired records. Use direct link for testing. WARN: SHOW_OLD still implemented by screen.
  <#if showOld>
    <@menuitem type="link" href=makeOfbizUrl("viewprofile") class="+${styles.action_run_sys!} ${styles.action_hide!}" text=uiLabelMap.PartyHideOld />
  <#else>
    <@menuitem type="link" href=makeOfbizUrl("viewprofile?SHOW_OLD=true") class="+${styles.action_run_sys!} ${styles.action_show!}" text=uiLabelMap.PartyShowOld />
  </#if>-->
  <#if ((productStore.enableDigProdUpload)!) == "Y">
    <@menuitem type="link" href=makeOfbizUrl("digitalproductlist") class="${styles.link_nav!} ${styles.action_import!}" text=uiLabelMap.EcommerceDigitalProductUpload />
  </#if>
</@menu>
<#-- ============================================================= -->
<#-- SCIPIO: Language -->
<#assign dummy = setRequestAttribute("setLocalesTarget", "setSessionLocaleProfile")>
<@render resource="component://common/widget/CommonScreens.xml#listLocalesCompact" />
<#-- ============================================================= -->


<@tabs>
    <#-- Party Personal Info Tab -->
    <@tab title=uiLabelMap.PartyPersonalInformation>
        <#macro menuContent menuArgs={}>
            <@menu args=menuArgs>
                <#if person??>
                  <#assign itemText = uiLabelMap.CommonUpdate>
                <#else>
                  <#assign itemText = uiLabelMap.CommonCreate>
                </#if>
                <@menuitem type="link" href=makeOfbizUrl("editperson") text=itemText />
            </@menu>
        </#macro>
        <@section title=uiLabelMap.PartyPersonalInformation menuContent=menuContent>
            <#if person??>
              <#-- SCIPIO: This was a table, not illogical but will look better as fields -->
              <@field type="display" label=uiLabelMap.PartyName>${person.personalTitle!} ${person.firstName!} ${person.middleName!} ${person.lastName!} ${person.suffix!}</@field>
              <#if person.nickname?has_content><@field type="display" label=uiLabelMap.PartyNickName>${person.nickname}</@field></#if>
              <#if person.gender?has_content><@field type="display" label=uiLabelMap.PartyGender>${person.gender}</@field></#if>
              <#if person.birthDate??><@field type="display" label=uiLabelMap.PartyBirthDate>${person.birthDate.toString()}</@field></#if>
              <#if person.height??><@field type="display" label=uiLabelMap.PartyHeight>${person.height}</@field></#if>
              <#if person.weight??><@field type="display" label=uiLabelMap.PartyWeight>${person.weight}</@field></#if>
              <#if person.mothersMaidenName?has_content><@field type="display" label=uiLabelMap.PartyMaidenName>${person.mothersMaidenName}</@field></#if>
              <#if person.maritalStatus?has_content><@field type="display" label=uiLabelMap.PartyMaritalStatus>${person.maritalStatus}</@field></#if>
              <#if person.socialSecurityNumber?has_content><@field type="display" label=uiLabelMap.PartySocialSecurityNumber>${person.socialSecurityNumber}</@field></#if>
              <#if person.passportNumber?has_content><@field type="display" label=uiLabelMap.PartyPassportNumber>${person.passportNumber}</@field></#if>
              <#if person.passportExpireDate??><@field type="display" label=uiLabelMap.PartyPassportExpireDate>${person.passportExpireDate.toString()}</@field></#if>
              <#if person.totalYearsWorkExperience??><@field type="display" label=uiLabelMap.PartyYearsWork>${person.totalYearsWorkExperience}</@field></#if>
              <#if person.comments?has_content><@field type="display" label=uiLabelMap.CommonComments>${person.comments}</@field></#if>
            <#else>
              <@commonMsg type="result-norecord">${uiLabelMap.PartyPersonalInformationNotFound}</@commonMsg>
            </#if>
        </@section>
    </@tab>
    
    <#-- Account Info Tab -->
    <@tab title=uiLabelMap.EcommerceAccountInformation>
        <#macro menuContent menuArgs={}>
            <@menu args=menuArgs>
                <@menuitem type="link" href=makeOfbizUrl("changepassword") text=uiLabelMap.PartyChangePassword />
            </@menu>
        </#macro>
        <@section title=uiLabelMap.EcommerceAccountInformation menuContent=menuContent><#-- title="${rawLabel('CommonUsername')} & ${rawLabel('CommonPassword')}" -->
            <#-- SCIPIO: This was a table, not illogical but will look better as fields -->
              
              <@field type="display" label=uiLabelMap.AccountingAccountType>
                <#if userIsBusiness>
                  ${getLabel("DataCategory.categoryName.BUSINESS", "ContentEntityLabels")}
                <#else>
                  ${getLabel("DataCategory.categoryName.PERSONAL", "ContentEntityLabels")}
                </#if>
              </@field>
            <@field type="display" label=uiLabelMap.CommonUsername>${userLogin.userLoginId}</@field>
        </@section>
    </@tab>
    
    <#-- Party Contact Tab -->
    <@tab title=uiLabelMap.PartyContactInformation>
        <#macro menuContent menuArgs={}>
            <@menu args=menuArgs>
                <@menuitem type="link" href=makeOfbizUrl("editcontactmech") text=uiLabelMap.CommonCreateNew />
            </@menu>
        </#macro>
        <@section title=uiLabelMap.PartyContactInformation menuContent=menuContent>
          <#if partyContactMechValueMaps?has_content>
            <@table type="data-complex"> <#-- orig: width="100%" border="0" cellpadding="0" -->
              <@thead>
                <@tr valign="bottom">
                  <@th>${uiLabelMap.PartyContactType}</@th>
                  <@th>${uiLabelMap.CommonInformation}</@th>
                  <@th>${uiLabelMap.PartySolicitingOk}?</@th>
                  <@th></@th>
                </@tr>
              </@thead>
              <@tbody>
              <#list partyContactMechValueMaps as partyContactMechValueMap>
                <#assign contactMech = partyContactMechValueMap.contactMech! />
                <#assign contactMechType = partyContactMechValueMap.contactMechType! />
                <#assign partyContactMech = partyContactMechValueMap.partyContactMech! />
                  <@tr>
                    <@td>
                      ${contactMechType.get("description",locale)}
                    </@td>
                    <@td>
                      <#list partyContactMechValueMap.partyContactMechPurposes! as partyContactMechPurpose>
                        <#assign contactMechPurposeType = partyContactMechPurpose.getRelatedOne("ContactMechPurposeType", true)! />
                        <div>
                          <#if contactMechPurposeType?has_content>
                            <#if contactMechPurposeType.contactMechPurposeTypeId == "SHIPPING_LOCATION"><span style="white-space:nowrap;"></#if>
                            ${contactMechPurposeType.get("description",locale)}
                            <#if contactMechPurposeType.contactMechPurposeTypeId == "SHIPPING_LOCATION" && (((profiledefs.defaultShipAddr)!"") == contactMech.contactMechId)>
                              <span><strong>[${uiLabelMap.EcommerceIsDefault}]</strong></span>
                            </#if>
                            <#if contactMechPurposeType.contactMechPurposeTypeId == "SHIPPING_LOCATION"></span></#if>
                          <#else>
                            ${uiLabelMap.PartyPurposeTypeNotFound}: "${partyContactMechPurpose.contactMechPurposeTypeId}"
                          </#if>
                          <#if partyContactMechPurpose.thruDate??>${uiLabelMap.CommonExpire}:${partyContactMechPurpose.thruDate.toString()}</#if>
                        </div>
                      </#list>
                      <#if (contactMech.contactMechTypeId!) == "POSTAL_ADDRESS">
                        <#assign postalAddress = partyContactMechValueMap.postalAddress! />
                        <div>
                          <#if postalAddress?has_content>
                            <@formattedAddress address=postalAddress emphasis=false />
                            <#-- <#if (!postalAddress.countryGeoId?has_content || (postalAddress.countryGeoId!) == "USA")>
                              <#assign addr1 = postalAddress.address1!?string />
                              <#if (addr1?index_of(" ") > 0)>
                                <#assign addressNum = addr1?substring(0, addr1?index_of(" ")) />
                                <#assign addressOther = addr1?substring(addr1?index_of(" ")+1) />
                                <br/><a target="_blank" href="${uiLabelMap.CommonLookupWhitepagesAddressLink}" class="${styles.link_nav_inline!} ${styles.action_find!} ${styles.action_external!}">${uiLabelMap.CommonLookupWhitepages}</a>
                              </#if>
                            </#if> -->
                          <#else>
                            ${uiLabelMap.PartyPostalInformationNotFound}.
                          </#if>
                          </div>
                      <#elseif (contactMech.contactMechTypeId!) == "TELECOM_NUMBER">
                        <#assign telecomNumber = partyContactMechValueMap.telecomNumber!>
                        <div>
                        <#if telecomNumber??>
                          ${telecomNumber.countryCode!}
                          <#if telecomNumber.areaCode?has_content>${telecomNumber.areaCode}-</#if>${telecomNumber.contactNumber!}
                          <#if partyContactMech.extension?has_content>ext&nbsp;${partyContactMech.extension}</#if>
                          <#-- 
                          <#if (!telecomNumber.countryCode?has_content || telecomNumber.countryCode == "011")>
                            <a target="_blank" href="${uiLabelMap.CommonLookupAnywhoLink}" class="${styles.link_nav!} ${styles.action_find!} ${styles.action_external!}">${uiLabelMap.CommonLookupAnywho}</a>
                            <a target="_blank" href="${uiLabelMap.CommonLookupWhitepagesTelNumberLink}" class="${styles.link_nav!} ${styles.action_find!} ${styles.action_external!}">${uiLabelMap.CommonLookupWhitepages}</a>
                          </#if>-->
                        <#else>
                          ${uiLabelMap.PartyPhoneNumberInfoNotFound}.
                        </#if>
                        </div>
                      <#elseif (contactMech.contactMechTypeId!) == "EMAIL_ADDRESS">
                          <a href="mailto:${contactMech.infoString!}" class="${styles.link_run_sys_inline!} ${styles.action_send!} ${styles.action_external!}">${contactMech.infoString!}</a>
                      <#elseif (contactMech.contactMechTypeId!) == "WEB_ADDRESS">
                        <div>
                          ${contactMech.infoString}
                          <#assign openAddress = contactMech.infoString!?string />
                          <#if !openAddress?starts_with("http://") && !openAddress?starts_with("HTTP://") && !openAddress?starts_with("https://") && !openAddress?starts_with("HTTPS://")><#assign openAddress = "http://" + openAddress /></#if>
                          <a target="_blank" href="${openAddress}" class="${styles.link_nav!} ${styles.action_view!} ${styles.action_external!}">${uiLabelMap.CommonOpenNewWindow}</a>
                        </div>
                      <#else>
                        ${contactMech.infoString!}
                      </#if>
                      <#-- SCIPIO: needless information
                      <div>(${uiLabelMap.CommonUpdated}:&nbsp;${partyContactMech.fromDate.toString()})</div>-->
                      <#if partyContactMech.thruDate??><div>${uiLabelMap.CommonDelete}:&nbsp;${partyContactMech.thruDate.toString()}</div></#if>
                    </@td>
                    <@td>
                      <#if (partyContactMech.allowSolicitation!) == "Y">
                        ${uiLabelMap.CommonYes}
                      <#elseif (partyContactMech.allowSolicitation!) == "N">
                        ${uiLabelMap.CommonNo}
                      <#elseif !partyContactMech.allowSolicitation?has_content>
                        (${uiLabelMap.ProductNotSet})
                      <#else>
                        (${uiLabelMap.PartyUnknown})
                      </#if>
                    </@td>
                    <@td class="+${styles.text_right!}">
                      <@menu type="button">
                        <#-- SCIPIO: NOTE: this should list one non-expired SHIPPING_LOCATION at most. We also correct this to prevent showing button for expired ones. -->
                        <#list partyContactMechValueMap.partyContactMechPurposes! as partyContactMechPurpose>
                          <#assign contactMechPurposeType = partyContactMechPurpose.getRelatedOne("ContactMechPurposeType", true)! />
                          <#if !partyContactMechPurpose.thruDate?? && ((contactMechPurposeType.contactMechPurposeTypeId)!) == "SHIPPING_LOCATION" && (((profiledefs.defaultShipAddr)!"") != contactMech.contactMechId)>
                            <@menuitem type="generic"> 
                              <form name="defaultShippingAddressForm" style="display:inline;" method="post" action="<@ofbizUrl>setprofiledefault/viewprofile</@ofbizUrl>">
                                <input type="hidden" name="productStoreId" value="${productStoreId}" />
                                <input type="hidden" name="defaultShipAddr" value="${contactMech.contactMechId}" />
                                <input type="hidden" name="partyId" value="${party.partyId}" />
                                <input type="submit" style="display:inline;" value="${uiLabelMap.EcommerceSetDefault} ${uiLabelMap.OrderShippingAddress}" class="${styles.link_run_sys!} ${styles.action_updatestatus!}" />
                              </form>
                            </@menuitem>
                          </#if>
                        </#list>
                        <@menuitem type="link" href=makeOfbizUrl("editcontactmech?contactMechId=${contactMech.contactMechId}") class="+${styles.action_nav!} ${styles.action_update!}" text=uiLabelMap.CommonUpdate />
                        <@menuitem type="link" href="javascript:document.deleteContactMech_${contactMech.contactMechId}.submit()" class="+${styles.action_run_sys!} ${styles.action_terminate!}" text=uiLabelMap.CommonExpire>
                          <form name="deleteContactMech_${contactMech.contactMechId}" method="post" action="<@ofbizUrl>deleteContactMech</@ofbizUrl>">
                            <input type="hidden" name="contactMechId" value="${contactMech.contactMechId}"/>
                          </form>
                        </@menuitem>
                      </@menu>
                    </@td>
                  </@tr>
              </#list>
              </@tbody>
            </@table>
          <#else>
            <@commonMsg type="result-norecord">${uiLabelMap.PartyNoContactInformation}.</@commonMsg>
          </#if>
        </@section>
    </@tab>
    
    <#-- Payment Method Tab -->
    <@tab title=uiLabelMap.AccountingPaymentMethodInformation>
        <#macro menuContent menuArgs={}>
            <@menu args=menuArgs>
                <@menuitem type="link" href=makeOfbizUrl("editcreditcard") text=uiLabelMap.PartyCreateNewCreditCard />
                <@menuitem type="link" href=makeOfbizUrl("editgiftcard") text=uiLabelMap.PartyCreateNewGiftCard />
                <@menuitem type="link" href=makeOfbizUrl("editeftaccount") text=uiLabelMap.PartyCreateNewEftAccount />
            </@menu>
        </#macro>
        <@section title=uiLabelMap.AccountingPaymentMethodInformation menuContent=menuContent>
          <#if paymentMethodValueMaps?has_content>
              <@table type="data-complex"> <#-- orig: width="100%" cellpadding="2" cellspacing="0" border="0" -->
                <#list paymentMethodValueMaps as paymentMethodValueMap>
                  <#assign paymentMethod = paymentMethodValueMap.paymentMethod! />
                  <#assign creditCard = paymentMethodValueMap.creditCard! />
                  <#assign giftCard = paymentMethodValueMap.giftCard! />
                  <#assign eftAccount = paymentMethodValueMap.eftAccount! />
            <#-- SCIPIO: FIXME?: don't support listing any others yet -->
            <#if creditCard?has_content || giftCard?has_content || eftAccount?has_content>
                  <@tr>
                    <#assign updateLink = "">
                    <@td>
                    <#if (paymentMethod.paymentMethodTypeId!) == "CREDIT_CARD">
                        ${uiLabelMap.AccountingCreditCard}:
                        <@formattedCreditCardDetail creditCard=creditCard paymentMethod=paymentMethod />
                        <@formattedPayMethGeneralDetail paymentMethod=paymentMethod />
                        <#assign updateLink = makeOfbizUrl("editcreditcard?paymentMethodId=${paymentMethod.paymentMethodId}")>
                    <#elseif (paymentMethod.paymentMethodTypeId!) == "GIFT_CARD">
                        ${uiLabelMap.AccountingGiftCard}: 
                        <@formattedGiftCardDetail giftCard=giftCard paymentMethod=paymentMethod />
                        <@formattedPayMethGeneralDetail paymentMethod=paymentMethod />
                        <#assign updateLink = makeOfbizUrl("editgiftcard?paymentMethodId=${paymentMethod.paymentMethodId}")>
                    <#elseif (paymentMethod.paymentMethodTypeId!) == "EFT_ACCOUNT">
                        ${uiLabelMap.AccountingEFTAccount}: 
                        <@formattedEftAccountDetail eftAccount=eftAccount paymentMethod=paymentMethod />
                        <@formattedPayMethGeneralDetail paymentMethod=paymentMethod />
                      <#assign updateLink = makeOfbizUrl("editeftaccount?paymentMethodId=${paymentMethod.paymentMethodId}")>
                    </#if>
                        <#if ((profiledefs.defaultPayMeth)!"") == paymentMethod.paymentMethodId>
                          <br/><strong>[${uiLabelMap.EcommerceIsDefault}]</strong>
                        </#if>
                    </@td>
                    <@td class="+${styles.text_right!}">
                      <@menu type="button">
                        <#--<#if ((profiledefs.defaultPayMeth)!"") == paymentMethod.paymentMethodId>
                          <@menuitem type="text" class="+${styles.action_run_sys!} ${styles.action_updatestatus!}" text="[${rawLabel('EcommerceIsDefault')}]" />
                        <#else>-->
                        <#if ((profiledefs.defaultPayMeth)!"") != paymentMethod.paymentMethodId>
                          <@menuitem type="generic" class="+${styles.action_run_sys!} ${styles.action_updatestatus!}">
                            <form name="defaultPaymentMethodForm" method="post" action="<@ofbizUrl>setprofiledefault/viewprofile</@ofbizUrl>">
                              <input type="hidden" name="productStoreId" value="${productStoreId}" />
                              <input type="hidden" name="defaultPayMeth" value="${paymentMethod.paymentMethodId}" />
                              <input type="hidden" name="partyId" value="${party.partyId}" />
                              <input type="submit" value="${uiLabelMap.EcommerceSetDefault}" class="${styles.link_run_sys!} ${styles.action_updatestatus!}" />
                            </form>
                          </@menuitem>
                        </#if>
                        <#if updateLink?has_content>
                          <@menuitem type="link" href=updateLink  class="+${styles.action_nav!} ${styles.action_update!}" text=uiLabelMap.CommonUpdate />
                        </#if>
                        <@menuitem type="link" href=makeOfbizUrl("deletePaymentMethod/viewprofile?paymentMethodId=${paymentMethod.paymentMethodId}") class="+${styles.action_run_sys!} ${styles.action_terminate!}" text=uiLabelMap.CommonExpire />
                      </@menu>
                    </@td>
                  </@tr>
            </#if>
                </#list>
              </@table>
          <#else>
            <@commonMsg type="result-norecord">${uiLabelMap.AccountingNoPaymentMethodInformation}</@commonMsg>
          </#if>
        </@section>
    </@tab>
    
    <#-- Shipping Info Tab -->
    <@tab title=uiLabelMap.EcommerceDefaultShipmentMethod>
        <#macro menuContent menuArgs={}>
            <@menu args=menuArgs>
            <#-- SCIPIO: this isn't a link, it's a form submit, put it at bottom so not mistakable
              <#if profiledefs?has_content && profiledefs.defaultShipAddr?has_content && carrierShipMethods?has_content>
                <@menuitem type="link" href="javascript:document.setdefaultshipmeth.submit();" text=uiLabelMap.EcommerceSetDefault />
              </#if>
             -->
            </@menu>
        </#macro>
        <@section title=uiLabelMap.EcommerceDefaultShipmentMethod><#--menuContent=menuContent-->
          <form name="setdefaultshipmeth" action="<@ofbizUrl>setprofiledefault/viewprofile</@ofbizUrl>" method="post">
          <@fields type="default-compact">
            <input type="hidden" name="productStoreId" value="${productStoreId!}" />
            <input type="hidden" name="partyId" value="${(userLogin.partyId)!}" />
            <#if profiledefs?has_content && profiledefs.defaultShipAddr?has_content && carrierShipMethods?has_content>
              <#list carrierShipMethods as shipMeth>
                <#assign shippingMethod = rawString(shipMeth.shipmentMethodTypeId!) + "@" + rawString(shipMeth.partyId) />
                <#assign shippingMethodLabel><#if shipMeth.partyId != "_NA_">${shipMeth.partyId!}&nbsp;</#if>${shipMeth.get("description", locale)!}</#assign>
                <@field type="radio" name="defaultShipMeth" value=shippingMethod checked=((rawString(profiledefs.defaultShipMeth!)) == shippingMethod) label=wrapAsRaw(shippingMethodLabel, 'htmlmarkup') />
              </#list>
              <@field type="submit" text=uiLabelMap.EcommerceSetDefault />
            <#else>
              ${uiLabelMap.EcommerceDefaultShipmentMethodMsg} (<i class="${styles.icon!} ${styles.icon_prefix}pencil"></i> ${uiLabelMap.PartyContactInformation})
            </#if>
          </@fields>
          </form>
        </@section>
    </@tab>


</@tabs>

<#-- ============================================================= -->
<#-- SCIPIO: TODO?
<@section title=uiLabelMap.EcommerceFileManager>
    <@table type="fields"> <#-orig: width="100%" border="0" cellpadding="1"->
      <#if partyContent?has_content>
        <#list partyContent as contentRole>
        <#assign content = contentRole.getRelatedOne("Content", false) />
        <#assign contentType = content.getRelatedOne("ContentType", true) />
        <#assign mimeType = content.getRelatedOne("MimeType", true)! />
        <#assign status = content.getRelatedOne("StatusItem", true) />
          <@tr>
            <@td><a href="<@ofbizUrl>img?imgId=${content.dataResourceId!}</@ofbizUrl>" class="${link_nav_info_id!}">${content.contentId}</a></@td>
            <@td>${content.contentName!}</@td>
            <@td>${(contentType.get("description",locale))!}</@td>
            <@td>${(mimeType.description)!}</@td>
            <@td>${(status.get("description",locale))!}</@td>
            <@td>${contentRole.fromDate!}</@td>
            <@td class="+${styles.text_right!}">
              <form name="removeContent_${contentRole.contentId}" method="post" action="removePartyAsset">
                <input name="partyId" type="hidden" value="${userLogin.partyId}"/>
                <input name="contentId" type="hidden" value="${contentRole.contentId}"/>
                <input name="roleTypeId" type="hidden" value="${contentRole.roleTypeId}"/>
              </form>
              <a href="<@ofbizUrl>img?imgId=${content.dataResourceId!}</@ofbizUrl>" class="${styles.link_nav!} ${styles.action_view!}">${uiLabelMap.CommonView}</a>
              <a href="javascript:document.removeContent_${contentRole.contentId}.submit();" class="${styles.link_run_sys!} ${styles.action_remove!}">${uiLabelMap.CommonRemove}</a>
            </@td>
          </@tr>
        </#list>
      <#else>
         <@tr><@td>${uiLabelMap.EcommerceNoFiles}</@td></@tr>
      </#if>
    </@table>
    <div>&nbsp;</div>
    <@heading>${uiLabelMap.EcommerceUploadNewFile}</@heading>
    <div>
      <form method="post" enctype="multipart/form-data" action="<@ofbizUrl>uploadPartyContent</@ofbizUrl>">
      <div>
        <input type="hidden" name="partyId" value="${party.partyId}"/>
        <input type="hidden" name="dataCategoryId" value="PERSONAL"/>
        <input type="hidden" name="contentTypeId" value="DOCUMENT"/>
        <input type="hidden" name="statusId" value="CTNT_PUBLISHED"/>
        <input type="hidden" name="roleTypeId" value="OWNER"/>
        <input type="file" name="uploadedFile" size="50"/>
        <select name="partyContentTypeId">
          <option value="">${uiLabelMap.PartySelectPurpose}</option>
          <#list partyContentTypes as partyContentType>
            <option value="${partyContentType.partyContentTypeId}">${partyContentType.get("description", locale)?default(partyContentType.partyContentTypeId)}</option>
          </#list>
        </select>
        <select name="mimeTypeId">
          <option value="">${uiLabelMap.PartySelectMimeType}</option>
          <#list mimeTypes as mimeType>
            <option value="${mimeType.mimeTypeId}">${mimeType.get("description", locale)?default(mimeType.mimeTypeId)}</option>
          </#list>
        </select>
        <input type="submit" value="${uiLabelMap.CommonUpload}" class="${styles.link_run_sys!} ${styles.action_import!}"/>
        </div>
      </form>
    </div>
</@section>
-->
<#-- ============================================================= -->
<#-- SCIPIO: TODO?
<@section title=uiLabelMap.PartyContactLists>
    <@table type="data-complex"> <#-orig: width="100%" border="0" cellpadding="1" cellspacing="0"->
      <@tr>
        <@th>${uiLabelMap.EcommerceListName}</@th>
        <#-<@th>${uiLabelMap.OrderListType}</@th>->
        <@th>${uiLabelMap.CommonFromDate}</@th>
        <@th>${uiLabelMap.CommonThruDate}</@th>
        <@th>${uiLabelMap.CommonStatus}</@th>
        <@th>${uiLabelMap.CommonEmail}</@th>
        <@th>&nbsp;</@th>
        <@th>&nbsp;</@th>
      </@tr>
      <#list contactListPartyList as contactListParty>
      <#assign contactList = contactListParty.getRelatedOne("ContactList", false)! />
      <#assign statusItem = contactListParty.getRelatedOne("StatusItem", true)! />
      <#assign emailAddress = contactListParty.getRelatedOne("PreferredContactMech", true)! />
      <#-<#assign contactListType = contactList.getRelatedOne("ContactListType", true)/>->
      <@tr><@td colspan="7"></@td></@tr>
      <@tr>
        <@td>${contactList.contactListName!}<#if contactList.description?has_content>&nbsp;-&nbsp;${contactList.description}</#if></@td>
        <#-<@td>${contactListType.get("description",locale)!}</@td>->
        <@td>${contactListParty.fromDate!}</@td>
        <@td>${contactListParty.thruDate!}</@td>
        <@td>${(statusItem.get("description",locale))!}</@td>
        <@td>${emailAddress.infoString!}</@td>
        <@td>&nbsp;</@td>
        <@td>
          <#if ((contactListParty.statusId!) == "CLPT_ACCEPTED")>            
            <form method="post" action="<@ofbizUrl>updateContactListParty</@ofbizUrl>" name="clistRejectForm${contactListParty_index}">
            <div>
              <#assign productStoreId = Static["org.ofbiz.product.store.ProductStoreWorker"].getProductStoreId(request) />
              <input type="hidden" name="productStoreId" value="${productStoreId!}" />
              <input type="hidden" name="partyId" value="${party.partyId}"/>
              <input type="hidden" name="contactListId" value="${contactListParty.contactListId}"/>
              <input type="hidden" name="preferredContactMechId" value="${contactListParty.preferredContactMechId}"/>
              <input type="hidden" name="fromDate" value="${contactListParty.fromDate}"/>
              <input type="hidden" name="statusId" value="CLPT_REJECTED"/>
              <input type="submit" value="${uiLabelMap.EcommerceUnsubscribe}" class="${styles.link_run_sys!} ${styles.action_remove!}"/>
              </div>
            </form>
          <#elseif ((contactListParty.statusId!) == "CLPT_PENDING")>
            <form method="post" action="<@ofbizUrl>updateContactListParty</@ofbizUrl>" name="clistAcceptForm${contactListParty_index}">
            <div>
              <input type="hidden" name="partyId" value="${party.partyId}"/>
              <input type="hidden" name="contactListId" value="${contactListParty.contactListId}"/>
              <input type="hidden" name="preferredContactMechId" value="${contactListParty.preferredContactMechId}"/>
              <input type="hidden" name="fromDate" value="${contactListParty.fromDate}"/>
              <input type="hidden" name="statusId" value="CLPT_ACCEPTED"/>
              <input type="text" size="10" name="optInVerifyCode" value=""/>
              <input type="submit" value="${uiLabelMap.EcommerceVerifySubscription}" class="${styles.link_run_sys!} ${styles.action_update!}"/>
              </div>
            </form>
          <#elseif ((contactListParty.statusId!) == "CLPT_REJECTED")>
            <form method="post" action="<@ofbizUrl>updateContactListParty</@ofbizUrl>" name="clistPendForm${contactListParty_index}">
            <div>
              <input type="hidden" name="partyId" value="${party.partyId}"/>
              <input type="hidden" name="contactListId" value="${contactListParty.contactListId}"/>
              <input type="hidden" name="preferredContactMechId" value="${contactListParty.preferredContactMechId}"/>
              <input type="hidden" name="fromDate" value="${contactListParty.fromDate}"/>
              <input type="hidden" name="statusId" value="CLPT_PENDING"/>
              <input type="submit" value="${uiLabelMap.EcommerceSubscribe}" class="${styles.link_run_sys!} ${styles.action_add!}"/>
              </div>
            </form>
          </#if>
        </@td>
      </@tr>
      </#list>
    </@table>
    <div>
      <form method="post" action="<@ofbizUrl>createContactListParty</@ofbizUrl>" name="clistPendingForm">
        <div>
        <input type="hidden" name="partyId" value="${party.partyId}"/>
        <input type="hidden" name="statusId" value="CLPT_PENDING"/>
        <span class="tableheadtext">${uiLabelMap.EcommerceNewListSubscription}: </span>
        <select name="contactListId">
          <#list publicContactLists as publicContactList>
            <#-<#assign publicContactListType = publicContactList.getRelatedOne("ContactListType", true)>->
            <#assign publicContactMechType = publicContactList.getRelatedOne("ContactMechType", true)! />
            <option value="${publicContactList.contactListId}">${publicContactList.contactListName!} <#-${publicContactListType.get("description",locale)} -> <#if publicContactMechType?has_content>[${publicContactMechType.get("description",locale)}]</#if></option>
          </#list>
        </select>
        <select name="preferredContactMechId">
        <#-<option></option>->
          <#list partyAndContactMechList as partyAndContactMech>
            <option value="${partyAndContactMech.contactMechId}"><#if partyAndContactMech.infoString?has_content>${partyAndContactMech.infoString}<#elseif partyAndContactMech.tnContactNumber?has_content>${partyAndContactMech.tnCountryCode!}-${partyAndContactMech.tnAreaCode!}-${partyAndContactMech.tnContactNumber}<#elseif partyAndContactMech.paAddress1?has_content>${partyAndContactMech.paAddress1}, ${partyAndContactMech.paAddress2!}, ${partyAndContactMech.paCity!}, ${partyAndContactMech.paStateProvinceGeoId!}, ${partyAndContactMech.paPostalCode!}, ${partyAndContactMech.paPostalCodeExt!} ${partyAndContactMech.paCountryGeoId!}</#if></option>
          </#list>
        </select>
        <input type="submit" value="${uiLabelMap.EcommerceSubscribe}" class="${styles.link_run_sys!} ${styles.action_add!}"/>
        </div>
      </form>
    </div>
    <label>${uiLabelMap.EcommerceListNote}</label>
</@section>
-->
<#-- ============================================================= -->
<#-- SCIPIO: TODO?
<#if surveys?has_content>
  <@section title=uiLabelMap.EcommerceSurveys>
    <@table type="data-complex" width="100%" border="0" cellpadding="1">
      <#list surveys as surveyAppl>
        <#assign survey = surveyAppl.getRelatedOne("Survey", false) />
        <@tr>
          <@td>&nbsp;</@td>
          <@td>${survey.surveyName!}&nbsp;-&nbsp;${survey.description!}</@td>
          <@td>&nbsp;</@td>
          <@td>
            <#assign responses = Static["org.ofbiz.product.store.ProductStoreWorker"].checkSurveyResponse(request, survey.surveyId)?default(0)>
            <#if (responses < 1)>${uiLabelMap.EcommerceNotCompleted}<#else>${uiLabelMap.EcommerceCompleted}</#if>
          </@td>
          <#if (responses == 0 || (survey.allowMultiple!"N") == "Y")>
            <#assign surveyLabel = uiLabelMap.EcommerceTakeSurvey />
            <#if (responses > 0 && survey.allowUpdate?default("N") == "Y")>
              <#assign surveyLabel = uiLabelMap.EcommerceUpdateSurvey />
            </#if>
            <@td class="+${styles.text_right!}"><a href="<@ofbizUrl>takesurvey?productStoreSurveyId=${surveyAppl.productStoreSurveyId}</@ofbizUrl>" class="${styles.link_nav!}">${surveyLabel}</a></@td>
          <#else>
          &nbsp;
          </#if>
        </@tr>
      </#list>
    </@table>
  </@section>
</#if>
-->
<#-- ============================================================= -->
<#-- only 5 messages will show; edit the ViewProfile.groovy to change this number -->
<#-- SCIPIO: TODO? 
<@render resource="component://shop/widget/CustomerScreens.xml#messagelist-include" />
-->
<#-- SCIPIO: TODO? 
<@render resource="component://shop/widget/CustomerScreens.xml#FinAccountList-include" />
-->
<#-- Serialized Inventory Summary -->
<#-- SCIPIO: TODO? 
<@render resource="component://shop/widget/CustomerScreens.xml#SerializedInventorySummary" />
-->
<#-- Subscription Summary -->
<#-- SCIPIO: TODO? 
<@render resource="component://shop/widget/CustomerScreens.xml#SubscriptionSummary" />
-->
<#-- Reviews -->
<#-- SCIPIO: TODO? 
<@render resource="component://shop/widget/CustomerScreens.xml#showProductReviews" />
-->
<#else>
    <@commonMsg type="error">${uiLabelMap.PartyNoPartyForCurrentUserName}: ${(userLogin.userLoginId)!uiLabelMap.CommonNA}</@commonMsg>
</#if>
