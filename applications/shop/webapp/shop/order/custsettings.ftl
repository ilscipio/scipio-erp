<#--
Scipio Commerce
Copyright (C) Ilscipio GmbH

This file is part of Scipio Commerce. Scipio Commerce is free software: you
can redistribute it and modify it under the terms of the GNU Affero General
Public License, version 3, as published by the Free Software Foundation.
Scipio Commerce is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License
for more details. You should have received a copy of the license with this
work (file LICENSE). If not, see <https://www.gnu.org/licenses/agpl-3.0.html>.
A commercial license is available from Ilscipio GmbH.

SPDX-License-Identifier: AGPL-3.0-only
-->
<#include "component://shop/webapp/shop/order/ordercommon.ftl">

<#-- SCIPIO: DEPRECATED TEMPLATE -->

<@section title=uiLabelMap.EcommerceYourNamePhoneAndEmail>
<form id="editCustomerNamePhoneAndEmail" name="${parameters.formNameValue}" method="post" action="<@pageUrl>processCustomerSettings</@pageUrl>">
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




