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

<#include "component://shop/webapp/shop/customer/customercommon.ftl">

<#-- SCIPIO: TODO?: Some of this is redundant with newcustomer.ftl - investigate -->

<#if !cbfParams??>
  <#assign cbfParams = parameters>
</#if>

<#-- SCIPIO: NOTE: fields duplicated from old checkout custsettings.ftl -->
<#-- FIXME: these only source from parameters map, need more fallbacks/sources... -->

    <@personalTitleField params=cbfParams name="${fieldNamePrefix}personalTitle" label=uiLabelMap.CommonTitle />

    <@field type="input" name="${fieldNamePrefix}firstName" value=(cbfParams["${fieldNamePrefix}firstName"]!) required=true label=uiLabelMap.PartyFirstName/>
    <@field type="input" name="${fieldNamePrefix}middleName" value=(cbfParams["${fieldNamePrefix}middleName"]!) label=uiLabelMap.PartyMiddleInitial/>
    <@field type="input" name="${fieldNamePrefix}lastName" value=(cbfParams["${fieldNamePrefix}lastName"]!) required=true label=uiLabelMap.PartyLastName/>
    <@field type="input" name="${fieldNamePrefix}suffix" value=(cbfParams["${fieldNamePrefix}suffix"]!) label=uiLabelMap.PartySuffix containerClass="+${styles.field_extra!}"/>

    <input type="hidden" name="${fieldNamePrefix}homePhoneContactMechId" value="${cbfParams["${fieldNamePrefix}homePhoneContactMechId"]!}"/>
    <@telecomNumberField label=uiLabelMap.PartyHomePhone required=true 
        countryCodeName="${fieldNamePrefix}homeCountryCode" areaCodeName="${fieldNamePrefix}homeAreaCode" contactNumberName="${fieldNamePrefix}homeContactNumber" extensionName="${fieldNamePrefix}homeExt">
      <@fields type="default-compact" ignoreParentField=true>
        <@allowSolicitationField params=cbfParams name="${fieldNamePrefix}homeSol" allowSolicitation="" containerClass="+${styles.field_extra!}" />
      </@fields>
    </@telecomNumberField>

    <input type="hidden" name="${fieldNamePrefix}workPhoneContactMechId" value="${cbfParams["${fieldNamePrefix}workPhoneContactMechId"]!}"/>
    <@telecomNumberField label=uiLabelMap.PartyBusinessPhone required=false containerClass="+${styles.field_extra!}"
        countryCodeName="${fieldNamePrefix}workCountryCode" areaCodeName="${fieldNamePrefix}workAreaCode" contactNumberName="${fieldNamePrefix}workContactNumber" extensionName="${fieldNamePrefix}workExt">
      <@fields type="default-compact" ignoreParentField=true>
        <@allowSolicitationField params=cbfParams name="${fieldNamePrefix}workSol" allowSolicitation="" />
      </@fields>
    </@telecomNumberField>

    <input type="hidden" name="${fieldNamePrefix}emailContactMechId" value="${cbfParams["${fieldNamePrefix}emailContactMechId"]!}"/>
    <@field type="generic" label=uiLabelMap.PartyEmailAddress required=true>
      <@fields type="default-manual-widgetonly">
        <@field type="input" name="${fieldNamePrefix}emailAddress" value=(cbfParams["${fieldNamePrefix}emailAddress"]!) required=true />
      </@fields>
      <@fields type="default-compact" ignoreParentField=true>
        <@allowSolicitationField params=cbfParams name="${fieldNamePrefix}emailSol" allowSolicitation="" />
      </@fields>
    </@field>

