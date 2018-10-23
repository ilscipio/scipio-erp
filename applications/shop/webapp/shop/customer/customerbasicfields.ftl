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

