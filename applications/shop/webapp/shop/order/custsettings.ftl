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
<#include "ordercommon.ftl">

<#-- SCIPIO: DEPRECATED TEMPLATE -->

<@section title=uiLabelMap.EcommerceYourNamePhoneAndEmail>
<form id="editCustomerNamePhoneAndEmail" name="${parameters.formNameValue}" method="post" action="<@ofbizUrl>processCustomerSettings</@ofbizUrl>">
  <input type="hidden" name="partyId" value="${parameters.partyId!}"/>

    <@personalTitleField name="personalTitle" label=uiLabelMap.CommonTitle />

    <@field type="input" name="firstName" value=(parameters.firstName!) required=true label=uiLabelMap.PartyFirstName/>
    <@field type="input" name="middleName" value=(parameters.middleName!) label=uiLabelMap.PartyMiddleInitial/>
    <@field type="input" name="lastName" value=(parameters.lastName!) required=true label=uiLabelMap.PartyLastName/>
    <@field type="input" name="suffix" value=(parameters.suffix!) label=uiLabelMap.PartySuffix/>

    <input type="hidden" name="homePhoneContactMechId" value="${parameters.homePhoneContactMechId!}"/>
    <@telecomNumberField label=uiLabelMap.PartyHomePhone required=true 
        countryCodeName="homeCountryCode" areaCodeName="homeAreaCode" contactNumberName="homeContactNumber" extensionName="homeExt">
      <@fields type="default-compact" ignoreParentField=true>
        <@allowSolicitationField name="homeSol" allowSolicitation="" />
      </@fields>
    </@telecomNumberField>

    <input type="hidden" name="workPhoneContactMechId" value="${parameters.workPhoneContactMechId!}"/>
    <@telecomNumberField label=uiLabelMap.PartyBusinessPhone required=false 
        countryCodeName="workCountryCode" areaCodeName="workAreaCode" contactNumberName="workContactNumber" extensionName="workExt">
      <@fields type="default-compact" ignoreParentField=true>
        <@allowSolicitationField name="workSol" allowSolicitation="" />
      </@fields>
    </@telecomNumberField>

    <input type="hidden" name="emailContactMechId" value="${parameters.emailContactMechId!}"/>
    <@field type="generic" label=uiLabelMap.PartyEmailAddress required=true>
      <@field type="input" name="emailAddress" value=(parameters.emailAddress!) required=true />
      <@fields type="default-compact" ignoreParentField=true>
        <@allowSolicitationField name="emailSol" allowSolicitation="" />
      </@fields>
    </@field>

    <#--
    <@field type="submit" text=uiLabelMap.CommonContinue class="${styles.link_run_session!} ${styles.action_update!}"/>
    -->

</form>
</@section>

<@checkoutActionsMenu directLinks=true />




