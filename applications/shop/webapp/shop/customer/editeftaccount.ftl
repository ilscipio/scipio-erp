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

<#if canNotView>
  <@commonMsg type="error-perm">${uiLabelMap.AccountingEFTNotBelongToYou}.</@commonMsg>
  <@menu type="button">
    <@menuitem type="link" href=makeOfbizUrl("${donePage}") class="+${styles.action_nav!} ${styles.action_cancel!}" text=uiLabelMap.CommonGoBack />
  </@menu>
<#else>

<#macro menuContent menuArgs={}>
  <@menu args=menuArgs>
    <@menuitem type="link" href=makeOfbizUrl("${donePage}") class="+${styles.action_nav!} ${styles.action_cancel!}" text=uiLabelMap.CommonGoBack />
    <@menuitem type="link" href="javascript:document.editeftaccountform.submit()" class="+${styles.action_run_sys!} ${styles.action_update!}" text=uiLabelMap.CommonSave />
  </@menu>
</#macro>
<#assign sectionTitle>
  <#if !eftAccount??>
    ${uiLabelMap.AccountingAddNewEftAccount}
  <#else>
    ${uiLabelMap.PageTitleEditEFTAccount}
  </#if>
</#assign>
<@section title=sectionTitle menuContent=menuContent>

  <form method="post" action="<@ofbizUrl><#if !eftAccount??>createEftAccount?DONE_PAGE=${donePage}<#else>updateEftAccount?DONE_PAGE=${donePage}</#if></@ofbizUrl>" name="editeftaccountform">

    <#if eftAccount??>
      <input type="hidden" name="paymentMethodId" value="${paymentMethodId}" />
    </#if>

    <@field type="input" label="${uiLabelMap.AccountingNameOnAccount}" required=true size="30" maxlength="60" name="nameOnAccount" value=(eftAccountData.nameOnAccount!) />
    <@field type="input" label="${uiLabelMap.AccountingCompanyNameOnAccount}" size="30" maxlength="60" name="companyNameOnAccount" value=(eftAccountData.companyNameOnAccount!) />
    <@field type="input" label="${uiLabelMap.AccountingBankName}" required=true size="30" maxlength="60" name="bankName" value=(eftAccountData.bankName!) />
    <@field type="input" label="${uiLabelMap.AccountingRoutingNumber}" required=true size="10" maxlength="30" name="routingNumber" value=(eftAccountData.routingNumber!) />
    <@field type="select" label="${uiLabelMap.AccountingAccountType}" required=true name="accountType">
      <option>${eftAccountData.accountType!}</option>
      <option></option>
      <option>${uiLabelMap.CommonChecking}</option>
      <option>${uiLabelMap.CommonSavings}</option>
    </@field>
    <@field type="input" label="${uiLabelMap.AccountingAccountNumber}" required=true size="20" maxlength="40" name="accountNumber" value=(eftAccountData.accountNumber!) />
    <@field type="input" label="${uiLabelMap.CommonDescription}" size="30" maxlength="60" name="description" value=(paymentMethodData.description!) />

    <@field type="generic" label=uiLabelMap.PartyBillingAddress>
        <#-- Removed because is confusing, can add but would have to come back here with all data populated as before...
        <a href="<@ofbizUrl>editcontactmech</@ofbizUrl>" class="${styles.link_nav!} ${styles.action_add!}">
          [Create New Address]</a>&nbsp;&nbsp;
        -->
      <@fields type="default-manual" ignoreParentField=true>
        <@table type="data-complex"> <#-- orig: width="100%" border="0" cellpadding="1" -->
        <#if curPostalAddress??>
          <@tr>
            <@td align="right" valign="top" width="1%">
              <input type="radio" name="contactMechId" value="${curContactMechId}" checked="checked" />
            </@td>
            <@td valign="top" width="80%">
              <div><b>${uiLabelMap.PartyUseCurrentAddress}:</b></div>
              <#list curPartyContactMechPurposes as curPartyContactMechPurpose>
                <#assign curContactMechPurposeType = curPartyContactMechPurpose.getRelatedOne("ContactMechPurposeType", true)>
                <div>
                  <b>${curContactMechPurposeType.get("description",locale)!}</b>
                  <#if curPartyContactMechPurpose.thruDate??>
                    (${uiLabelMap.CommonExpire}:${curPartyContactMechPurpose.thruDate.toString()})
                  </#if>
                </div>
              </#list>
              <div>
                <#if curPostalAddress.toName??><b>${uiLabelMap.CommonTo}:</b> ${curPostalAddress.toName}<br /></#if>
                <#if curPostalAddress.attnName??><b>${uiLabelMap.PartyAddrAttnName}:</b> ${curPostalAddress.attnName}<br /></#if>
                ${curPostalAddress.address1!}<br />
                <#if curPostalAddress.address2??>${curPostalAddress.address2}<br /></#if>
                ${curPostalAddress.city}<#if curPostalAddress.stateProvinceGeoId?has_content>,&nbsp;${curPostalAddress.stateProvinceGeoId}</#if>&nbsp;${curPostalAddress.postalCode}
                <#if curPostalAddress.countryGeoId??><br />${curPostalAddress.countryGeoId}</#if>
              </div>
              <div>(${uiLabelMap.CommonUpdated}:&nbsp;${(curPartyContactMech.fromDate.toString())!})</div>
              <#if curPartyContactMech.thruDate??><div><b>${uiLabelMap.CommonDelete}:&nbsp;${curPartyContactMech.thruDate.toString()}</b></div></#if>
            </@td>
          </@tr>
        <#else>
           <#-- <@tr>
            <@td valign="top" colspan="2">${uiLabelMap.PartyNoBillingAddress}
            </@td>
          </@tr> -->
        </#if>
          <#-- is confusing
          <@tr>
            <@td valign="top" colspan="2"><b>${uiLabelMap.EcommerceMessage3}</b>
            </@td>
          </@tr>
          -->
          <#list postalAddressInfos as postalAddressInfo>
            <#assign contactMech = postalAddressInfo.contactMech>
            <#assign partyContactMechPurposes = postalAddressInfo.partyContactMechPurposes>
            <#assign postalAddress = postalAddressInfo.postalAddress>
            <#assign partyContactMech = postalAddressInfo.partyContactMech>
            <@tr>
              <@td align="right" valign="top" width="1%">
                <input type="radio" name="contactMechId" value="${contactMech.contactMechId}" />
              </@td>
              <@td valign="top" width="80%">
                <#list partyContactMechPurposes as partyContactMechPurpose>
                    <#assign contactMechPurposeType = partyContactMechPurpose.getRelatedOne("ContactMechPurposeType", true)>
                    <div>
                      <b>${contactMechPurposeType.get("description",locale)!}</b>
                      <#if partyContactMechPurpose.thruDate??>(${uiLabelMap.CommonExpire}:${partyContactMechPurpose.thruDate})</#if>
                    </div>
                </#list>
                <div>
                  <#if postalAddress.toName??><b>${uiLabelMap.CommonTo}:</b> ${postalAddress.toName}<br /></#if>
                  <#if postalAddress.attnName??><b>${uiLabelMap.PartyAddrAttnName}:</b> ${postalAddress.attnName}<br /></#if>
                  ${postalAddress.address1!}<br />
                  <#if postalAddress.address2??>${postalAddress.address2}<br /></#if>
                  ${postalAddress.city}<#if postalAddress.stateProvinceGeoId?has_content>,&nbsp;${postalAddress.stateProvinceGeoId}</#if>&nbsp;${postalAddress.postalCode}
                  <#if postalAddress.countryGeoId??><br />${postalAddress.countryGeoId}</#if>
                </div>
                <div>(${uiLabelMap.CommonUpdated}:&nbsp;${(partyContactMech.fromDate.toString())!})</div>
                <#if partyContactMech.thruDate??><div><b>${uiLabelMap.CommonDelete}:&nbsp;${partyContactMech.thruDate.toString()}</b></div></#if>
              </@td>
            </@tr>
          </#list>
          <#if !postalAddressInfos?has_content && !curContactMech??>
              <@tr type="meta"><@td colspan="2">${uiLabelMap.PartyNoContactInformation}.</@td></@tr>
          </#if>
        </@table>
      </@fields>
    </@field>
  </form>
  
  <@menuContent menuArgs={"type":"button"} />

</@section>

</#if>

