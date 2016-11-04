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

<#if !creditCard??>
  <#assign sectionTitle = uiLabelMap.AccountingAddNewCreditCard>
<#else>
  <#assign sectionTitle = uiLabelMap.AccountingEditCreditCard>
</#if>
<@section title=sectionTitle menuContent=menuContent>
    <#macro saveCancelMenu>
      <@menu type="button">
        <@menuitem type="link" href=makeOfbizUrl("${donePage}?partyId=${partyId}") text=uiLabelMap.CommonCancelDone class="+${styles.action_nav!} ${styles.action_cancel!}" />
        <@menuitem type="link" href="javascript:document.editcreditcardform.submit()" text=uiLabelMap.CommonSave class="+${styles.action_run_sys!} ${styles.action_update!}" />
      </@menu>
    </#macro>
    
    <#--<@saveCancelMenu />-->

    <#if !creditCard??>
      <form method="post" action="<@ofbizUrl>createCreditCard?DONE_PAGE=${donePage}</@ofbizUrl>" name="editcreditcardform">
    <#else>
      <form method="post" action="<@ofbizUrl>updateCreditCard?DONE_PAGE=${donePage}</@ofbizUrl>" name="editcreditcardform">
        <input type="hidden" name="paymentMethodId" value="${paymentMethodId}" />
    </#if>
        <input type="hidden" name="partyId" value="${partyId}"/>
        <@row>
          <@cell>
            <@render resource="component://accounting/widget/CommonScreens.xml#creditCardFields" />
          </@cell>
        </@row>

        <@field type="generic" label=uiLabelMap.AccountingBillingAddress>
          <@fields type="default-manual" ignoreParentField=true>
            <#-- Removed because is confusing, can add but would have to come back here with all data populated as before...
            <a href="<@ofbizUrl>editcontactmech</@ofbizUrl>" class="${styles.link_nav!} ${styles.action_add!}">
              [Create New Address]</a>&nbsp;&nbsp;
            -->
            <@table type="data-complex"> <#-- orig: cellspacing="0" -->
            <@tbody>
            <#assign hasCurrent = false>
            <#if curPostalAddress?has_content>
              <#assign hasCurrent = true>
              <@tr>
                <@td class="button-col">
                  <@field type="radio" inline=true name="contactMechId" value=curContactMechId checked=true />
                </@td>
                <@td>
                  <p><b>${uiLabelMap.PartyUseCurrentAddress}:</b></p>
                  <#list curPartyContactMechPurposes as curPartyContactMechPurpose>
                    <#assign curContactMechPurposeType = curPartyContactMechPurpose.getRelatedOne("ContactMechPurposeType", true)>
                    <div>
                      <b>${curContactMechPurposeType.get("description",locale)!}</b>
                      <#if curPartyContactMechPurpose.thruDate??>
                        (${uiLabelMap.CommonExpire}:${curPartyContactMechPurpose.thruDate.toString()})
                      </#if>
                    </div>
                  </#list>
                  <#if curPostalAddress.toName??><div><b>${uiLabelMap.CommonTo}:</b> ${curPostalAddress.toName}</div></#if>
                  <#if curPostalAddress.attnName??><div><b>${uiLabelMap.PartyAddrAttnName}:</b> ${curPostalAddress.attnName}</div></#if>
                  <#if curPostalAddress.address1??><div>${curPostalAddress.address1}</div></#if>
                  <#if curPostalAddress.address2??><div>${curPostalAddress.address2}</div></#if>
                  <div>${curPostalAddress.city!}<#if curPostalAddress.stateProvinceGeoId?has_content>,&nbsp;${curPostalAddress.stateProvinceGeoId!}</#if>&nbsp;${curPostalAddress.postalCode!}</div>
                  <#if curPostalAddress.countryGeoId??><div>${curPostalAddress.countryGeoId}</div></#if>
                  <div>(${uiLabelMap.CommonUpdated}:&nbsp;${(curPartyContactMech.fromDate.toString())!})</div>
                  <#if curPartyContactMech.thruDate??><div><b>${uiLabelMap.CommonDelete}:&nbsp;${curPartyContactMech.thruDate.toString()}</b></div></#if>
                </@td>
              </@tr>
            <#else>
               <#-- <@tr>
                <@td valign="top" colspan="2">
                  ${uiLabelMap.PartyBillingAddressNotSelected}
                </@td>
              </@tr> -->
            </#if>
              <#-- is confusing
              <@tr>
                <@td valign="top" colspan="2">
                  <b>Select a New Billing Address:</b>
                </@td>
              </@tr>
              -->
              <#list postalAddressInfos as postalAddressInfo>
                <#assign contactMech = postalAddressInfo.contactMech>
                <#assign partyContactMechPurposes = postalAddressInfo.partyContactMechPurposes>
                <#assign postalAddress = postalAddressInfo.postalAddress>
                <#assign partyContactMech = postalAddressInfo.partyContactMech>
                <@tr>
                  <@td class="button-col">
                    <@field type="radio" inline=true name="contactMechId" value=contactMech.contactMechId />
                  </@td>
                  <@td>
                    <#list partyContactMechPurposes as partyContactMechPurpose>
                      <#assign contactMechPurposeType = partyContactMechPurpose.getRelatedOne("ContactMechPurposeType", true)>
                      <div>
                        <b>${contactMechPurposeType.get("description",locale)!}</b>
                        <#if partyContactMechPurpose.thruDate??>(${uiLabelMap.CommonExpire}:${partyContactMechPurpose.thruDate})</#if>
                      </div>
                    </#list>
                    <#if postalAddress.toName??><div><b>${uiLabelMap.CommonTo}:</b> ${postalAddress.toName}</div></#if>
                    <#if postalAddress.attnName??><div><b>${uiLabelMap.PartyAddrAttnName}:</b> ${postalAddress.attnName}</div></#if>
                    <#if postalAddress.address1??><div>${postalAddress.address1}</div></#if>
                    <#if postalAddress.address2??><div>${postalAddress.address2}</div></#if>
                    <div>${postalAddress.city}<#if postalAddress.stateProvinceGeoId?has_content>,&nbsp;${postalAddress.stateProvinceGeoId}</#if>&nbsp;${postalAddress.postalCode!}</div>
                    <#if postalAddress.countryGeoId??><div>${postalAddress.countryGeoId}</div></#if>
                    <div>(${uiLabelMap.CommonUpdated}:&nbsp;${(partyContactMech.fromDate.toString())!})</div>
                    <#if partyContactMech.thruDate??><div><b>${uiLabelMap.CommonDelete}:&nbsp;${partyContactMech.thruDate.toString()}</b></div></#if>
                  </@td>
                </@tr>
              </#list>
              <#-- not yet supported in party manager
              <@tr>
                <@td align="right" valigh="top" width="1%">
                  <input type="radio" name="contactMechId" value="_NEW_" <#if !hasCurrent>checked="checked"</#if> />
                </@td>
                <@td valign="middle" width="80%">
                  ${uiLabelMap.PartyCreateNewBillingAddressCreditCard}.
                </@td>
              </@tr>
              -->
            </@tbody>
            </@table>
            <#if !postalAddressInfos?has_content && !curContactMech??>
              <@commonMsg type="result-norecord">${uiLabelMap.PartyNoContactInformation}.</@commonMsg>
            </#if>
          </@fields>
        </@field>
      </form>
      
      <@saveCancelMenu />

</@section>
