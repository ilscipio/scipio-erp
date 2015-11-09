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

<@section title="${uiLabelMap.PageTitleReturnHeader}">
        <#if returnHeader??>
            <form name="returnhead" method="post" action="<@ofbizUrl>updateReturn</@ofbizUrl>">
            <input type="hidden" name="returnId" value="${returnHeader.returnId}" />
            <input type="hidden" name="returnHeaderTypeId" value="CUSTOMER_RETURN"/>
            <input type="hidden" name="currentStatusId" value="${returnHeader.statusId!}" />
        <#else>
            <form name="returnhead" method="post" action="<@ofbizUrl>createReturn</@ofbizUrl>">
            <input type="hidden" name="returnHeaderTypeId" value="CUSTOMER_RETURN"/>
        </#if>

        <#if returnHeader??>
          <@field type="generic" label="${uiLabelMap.OrderReturnId}">
              ${returnHeader.returnId}
          </@field>
        </#if>
          <@field type="generic" label="${uiLabelMap.CommonCurrency}">
              <#if returnHeader??>
              ${returnHeader.currencyUomId!}
          <#else>
             <select name="currencyUomId">
                <#if (orderHeader?has_content) && (orderHeader.currencyUom?has_content)>
                  <option value="${orderHeader.currencyUom}" selected>${orderHeader.getRelatedOne("Uom", false).getString("description",locale)}</option>
                  <option value="${orderHeader.currencyUom}">---</option>
                <#elseif defaultCurrency?has_content>
                  <option value="${defaultCurrency.uomId}" selected>${defaultCurrency.getString("description")}</option>
                  <option value="${defaultCurrency.uomId}">---</option>
                </#if>
                <#if currencies?has_content>
                  <#list currencies as currency>
                    <option value="${currency.uomId}">${currency.get("description",locale)}</option>
                  </#list>
                </#if>
             </select>
          </#if>
          </@field>
          <@field type="generic" label="${uiLabelMap.OrderEntryDate}">
              <#if returnInfo.entryDate??>
                <#assign entryDate = returnInfo.get("entryDate").toString()>
              </#if>
              <@htmlTemplate.renderDateTimeField name="entryDate" event="" action="" value="${entryDate!}" className=""  title="Format: yyyy-MM-dd HH:mm:ss.SSS" size="25" maxlength="30" id="entryDate1" dateType="date" shortDateInput=false timeDropdownParamName="" defaultDateTimeString="" localizedIconTitle="" timeDropdown="" timeHourName="" classString="" hour1="" hour2="" timeMinutesName="" minutes="" isTwelveHour="" ampmName="" amSelected="" pmSelected="" compositeType="" formName=""/>
          </@field>
          <@field type="generic" label="${uiLabelMap.OrderReturnFromParty}">
              <@htmlTemplate.lookupField value='${returnInfo.fromPartyId!}' formName="returnhead" name="fromPartyId" id="fromPartyId" fieldFormName="LookupPartyName"/>
          </@field>
          <@field type="generic" label="${uiLabelMap.OrderReturnToFacility}">
              <select name='destinationFacilityId'>
                <#if currentFacility??>
                  <option value="${currentFacility.facilityId}">${currentFacility.facilityName?default(currentFacility.facilityId)}</option>
                  <option value="${currentFacility.facilityId}">---</option>
                </#if>
                <option value="">${uiLabelMap.FacilityNoFacility}</option>
                <#list facilityList as facility>
                  <option value="${facility.facilityId}" <#if (facilityList?size == 1)>selected="selected"</#if>>${facility.facilityName?default(facility.facilityId)}</option>
                </#list>
          </@field>
          <@field type="generic" label="${uiLabelMap.AccountingBillingAccount}">
              <#if billingAccountList?has_content>
                <select name='billingAccountId'>
                  <#if currentAccount??>
                    <option value="${currentAccount.billingAccountId}">${currentAccount.billingAccountId}: ${currentAccount.description!}</option>
                    <option value="${currentAccount.billingAccountId}">---</option>
                  </#if>
                  <option value="">${uiLabelMap.AccountingNewBillingAccount}</option>
                  <#list billingAccountList as ba>
                    <option value="${ba.billingAccountId}">${ba.billingAccountId}: ${ba.description!}</option>
                  </#list>
                </select>
              <#else>
                <input type='text' size='20' name='billingAccountId' />
              </#if>
          </@field>
          <@field type="generic" label="${uiLabelMap.FormFieldTitle_paymentMethodId}">
              <#if creditCardList?? || eftAccountList??>
                <select name='paymentMethodId'>
                  <#if currentCreditCard??>
                    <option value="${currentCreditCard.paymentMethodId}">CC:&nbsp;${Static["org.ofbiz.party.contact.ContactHelper"].formatCreditCard(currentCreditCard)}</option>
                  </#if>
                  <#if currentEftAccount??>
                    <option value="${currentEftAccount.paymentMethodId}">EFT:&nbsp;${currentEftAccount.nameOnAccount!}, ${currentEftAccount.accountNumber!}</option>
                  </#if>
                  <option value=""></option>
                  <#if creditCardList?has_content>
                    <#list creditCardList as creditCardPm>
                      <#assign creditCard = creditCardPm.getRelatedOne("CreditCard", false)>
                      <option value="${creditCard.paymentMethodId}">CC:&nbsp;${Static["org.ofbiz.party.contact.ContactHelper"].formatCreditCard(creditCard)}</option>
                    </#list>
                  </#if>
                  <#if eftAccountList?has_content>
                    <#list eftAccountList as eftAccount>
                      <option value="${eftAccount.paymentMethodId}">EFT:&nbsp;${eftAccount.nameOnAccount!}, ${eftAccount.accountNumber!}</option>
                    </#list>
                  </#if>
                </select>
              <#else>
                <input type='text' size='20' name='paymentMethodId' value="${(returnHeader.paymentMethodId)!}"/>
              </#if>
              <#if (returnHeader.fromPartyId)?has_content>
                <a href="/partymgr/control/editcreditcard?partyId=${returnHeader.fromPartyId}${StringUtil.wrapString(externalKeyParam)}" target="partymgr" class="${styles.link_action!}">${uiLabelMap.AccountingCreateNewCreditCard}</a>
              </#if>
          </@field>
          <@field type="generic" label="${uiLabelMap.OrderReturnNeedsAutoReceive}">
              <select name='needsInventoryReceive'>
                <#if needsInventoryReceive??>
                  <#if "Y" == needsInventoryReceive>
                    <option selected="selected" value="${needsInventoryReceive}">${uiLabelMap.CommonYes}</option>
                  <#elseif "N" == needsInventoryReceive>
                    <option selected="selected" value="${needsInventoryReceive}">${uiLabelMap.CommonNo}</option>
                  </#if>
                  <option value="${needsInventoryReceive}">---</option>
                </#if>
                <option value="Y">${uiLabelMap.CommonYes}</option>
                <option value="N">${uiLabelMap.CommonNo}</option>
              </select>
          </@field>
        <#if returnHeader?has_content>
          <@field type="generic" label="${uiLabelMap.CommonReturnStatus}">
              <select name="statusId">
                <#if currentStatus??>
                  <option value="${currentStatus.statusId}">${currentStatus.get("description",locale)}</option>
                  <option value="${currentStatus.statusId}">---</option>
                </#if>
                <#list returnStatus as status>
                  <option value="${status.statusIdTo}">${status.get("transitionName",locale)}</option>
                </#list>
              </select>
          </@field>
          <@field type="generic" label="${uiLabelMap.FormFieldTitle_createdBy}">
              ${returnHeader.createdBy?default("Unknown")}
          </@field>
          <@field type="generic" label="${uiLabelMap.OrderReturnFromAddress}">
              <#if (addressEditable)>
                <#list addresses as address >
                  <@displayAddress postalAddress = address.postalAddress editable = true/>
                </#list>
                <input type='radio' name="originContactMechId" value="" <#if (!postalAddressFrom?has_content)> checked="checked"</#if> />${uiLabelMap.CommonNoAddress}
              <#else>
                 <#if (postalAddressFrom?has_content)>
                   <@displayAddress postalAddress = postalAddressFrom editable = false />
                 <#else>
                   ${uiLabelMap.CommonNoAddress}
                 </#if>
              </#if>
          </@field>
          <@field type="generic" label="${uiLabelMap.OrderReturnToAddress}">
              <#if (postalAddressTo?has_content)>
                <@displayAddress postalAddress = postalAddressTo editable=false />
              </#if>
          </@field>
          <@field type="submitarea">
              <input type="submit" value="${uiLabelMap.CommonUpdate}"/>
          </@field>
        <#else>
          <input type="hidden" name="statusId" value="RETURN_REQUESTED" />
          <@field type="submitarea">
              <input type="submit" value="${uiLabelMap.CommonNew}"/>
          </@field>
        </#if>

        <#macro displayAddress postalAddress editable>
            <#if postalAddress?has_content>
                    <div>
                      <#if (editable)>
                        <input type='radio' name="originContactMechId" value="${postalAddress.contactMechId!}"
                          <#if ( postalAddressFrom?has_content && postalAddressFrom.contactMechId?default("") == postalAddress.contactMechId)>checked="checked"</#if> />
                      </#if>
                      <#if postalAddress.toName?has_content><span>${uiLabelMap.CommonTo}</span>&nbsp;${postalAddress.toName}<br /></#if>
                      <#if postalAddress.attnName?has_content><span>${uiLabelMap.CommonAttn}</span>&nbsp;${postalAddress.attnName}<br /></#if>
                      <#if postalAddress.address1?has_content>&nbsp;&nbsp;&nbsp;&nbsp;${postalAddress.address1}<br /></#if>
                      <#if postalAddress.address2?has_content>&nbsp;&nbsp;&nbsp;&nbsp;${postalAddress.address2}<br /></#if>
                      <#if postalAddress.city?has_content>&nbsp;&nbsp;&nbsp;&nbsp;${postalAddress.city}</#if>
                      <#if postalAddress.stateProvinceGeoId?has_content>&nbsp;${postalAddress.stateProvinceGeoId}</#if>
                      <#if postalAddress.postalCode?has_content>&nbsp;${postalAddress.postalCode}</#if>
                      <#if postalAddress.countryGeoId?has_content><br />&nbsp;&nbsp;&nbsp;&nbsp;${postalAddress.countryGeoId}</#if>
                    </div>
            </#if>
        </#macro>
</@section>