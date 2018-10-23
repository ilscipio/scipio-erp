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

<@section title=uiLabelMap.EcommerceMyAccount>
    <@section title=uiLabelMap.PartyContactInformation>
      <a class="${styles.link_nav!} ${styles.action_update!}" href="<@ofbizUrl>editProfile</@ofbizUrl>">${uiLabelMap.EcommerceEditProfile}</a>
      <label>${firstName!} ${lastName!}</label>
      <input type="hidden" id="updatedEmailContactMechId" name="emailContactMechId" value="${emailContactMechId!}" />
      <input type="hidden" id="updatedEmailAddress" name="updatedEmailAddress" value="${emailAddress!}" />
      <#if emailAddress??>
        <label>${emailAddress!}</label>
        <a href="mailto:${emailAddress!}" class="${styles.link_run_sys!} ${styles.action_send!} ${styles.action_external!}">(${uiLabelMap.PartySendEmail})</a>
      </#if>
      <div id="serverError_${emailContactMechId!}" class="errorMessage"></div>
    </@section>
    <#-- Manage Addresses -->
    <@section title=uiLabelMap.EcommerceAddressBook>
      <a class="${styles.link_nav!} ${styles.action_update!}" href="<@ofbizUrl>manageAddress</@ofbizUrl>">${uiLabelMap.EcommerceManageAddresses}</a>
      <@section title=uiLabelMap.EcommercePrimaryShippingAddress>
          <ul>
          <#if shipToContactMechId??>
            <li>${shipToAddress1!}</li>
            <#if shipToAddress2?has_content><li>${shipToAddress2!}</li></#if>
            <li>
              <ul>
                <li>
                  <#if shipToStateProvinceGeoId?has_content && shipToStateProvinceGeoId != "_NA_">
                    ${shipToStateProvinceGeoId}
                  </#if>
                  ${shipToCity!},
                  ${shipToPostalCode!}
                </li>
                <li>${shipToCountryGeoId!}</li>
              </ul>
            </li>
            <#if shipToTelecomNumber?has_content>
            <li>
              ${shipToTelecomNumber.countryCode!}-
              ${shipToTelecomNumber.areaCode!}-
              ${shipToTelecomNumber.contactNumber!}
              <#if shipToExtension??>-${shipToExtension!}</#if>
            </li>
            </#if>
          <#else>
            <li>${uiLabelMap.PartyPostalInformationNotFound}</li>
          </#if>
          </ul>
      </@section>
      <@section title=uiLabelMap.EcommercePrimaryBillingAddress>
          <ul>
          <#if billToContactMechId??>
            <li>${billToAddress1!}</li>
            <#if billToAddress2?has_content><li>${billToAddress2!}</li></#if>
            <li>
              <ul>
                <li>
                  <#if billToStateProvinceGeoId?has_content && billToStateProvinceGeoId != "_NA_">
                    ${billToStateProvinceGeoId}
                  </#if>
                  ${billToCity!},
                  ${billToPostalCode!}
                </li>
                <li>${billToCountryGeoId!}</li>
              </ul>
            </li>
            <#if billToTelecomNumber?has_content>
            <li>
              ${billToTelecomNumber.countryCode!}-
              ${billToTelecomNumber.areaCode!}-
              ${billToTelecomNumber.contactNumber!}
              <#if billToExtension??>-${billToExtension!}</#if>
            </li>
            </#if>
          <#else>
            <li>${uiLabelMap.PartyPostalInformationNotFound}</li>
          </#if>
          </ul>
      </@section>
    </@section>
</@section>