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

<@section title=uiLabelMap.EcommerceYourNamePhoneAndEmail>
<form id="editCustomerNamePhoneAndEmail" name="${parameters.formNameValue}" method="post" action="<@ofbizUrl>processCustomerSettings</@ofbizUrl>">
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

    <#--
    <@field type="submit" text=uiLabelMap.CommonContinue class="${styles.link_run_session!} ${styles.action_update!}"/>
    -->

</form>
</@section>

<@checkoutActionsMenu directLinks=true />




