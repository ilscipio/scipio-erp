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
<#include "customercommon.ftl">

<#-- Cato: migrated from editcontactmech.ftl -->

<#if parameters["${fieldNamePrefix}stateProvinceGeoId"]??>    
  <#assign defaultStateProvinceGeoId = parameters["${fieldNamePrefix}stateProvinceGeoId"]>
<#elseif (postalAddress??) && (postalAddress.stateProvinceGeoId??)>
  <#assign defaultStateProvinceGeoId = postalAddress.stateProvinceGeoId>
<#elseif (pafFallbacks.stateProvinceGeoId)??>
  <#assign defaultStateProvinceGeoId = pafFallbacks.stateProvinceGeoId>
<#else>
  <#assign defaultStateProvinceGeoId = "">
</#if>

<#if useScripts>

<@script>

jQuery(document).ready(function() {

    <#assign fieldNamePrefixJs = escapePart(fieldNamePrefix, 'js')>

    <#-- Cato: FIXME: error message div for ajax failures is missing 
        FIXME: initial selected value may currently not work -->
    var errorMsgContainerId = null;
    var containerId = null;
    jQuery("#${fieldNamePrefixJs}countryGeoId").change(function() {
        getAssociatedStateList('${fieldNamePrefixJs}countryGeoId', '${fieldNamePrefixJs}stateProvinceGeoId', errorMsgContainerId, containerId);
    });
    getAssociatedStateList('${fieldNamePrefixJs}countryGeoId', '${fieldNamePrefixJs}stateProvinceGeoId', errorMsgContainerId, containerId);
    
});

</@script>

</#if>

  <@field type="input" label="${uiLabelMap.PartyToName}" size="30" maxlength="60" name="${fieldNamePrefix}toName" value=(parameters["${fieldNamePrefix}toName"]!(postalAddressData.toName)!(pafFallbacks.toName)!) />
  <@field type="input" label="${uiLabelMap.PartyAttentionName}" size="30" maxlength="60" name="${fieldNamePrefix}attnName" value=(parameters["${fieldNamePrefix}attnName"]!(postalAddressData.attnName)!(pafFallbacks.attnName)!) />
  <@field type="input" label="${uiLabelMap.PartyAddressLine1}" required=true size="30" maxlength="30" name="${fieldNamePrefix}address1" value=(parameters["${fieldNamePrefix}address1"]!(postalAddressData.address1)!(pafFallbacks.address1)!) />
  <@field type="input" label="${uiLabelMap.PartyAddressLine2}" size="30" maxlength="30" name="${fieldNamePrefix}address2" value=(parameters["${fieldNamePrefix}address2"]!(postalAddressData.address2)!(pafFallbacks.address2)!) />
  <@field type="input" label="${uiLabelMap.PartyCity}" required=true size="30" maxlength="30" name="${fieldNamePrefix}city" value=(parameters["${fieldNamePrefix}city"]!(postalAddressData.city)!(pafFallbacks.city)!) />
  <@field type="select" label="${uiLabelMap.PartyState}" name="${fieldNamePrefix}stateProvinceGeoId" id="${fieldNamePrefix}stateProvinceGeoId">
    <#-- Cato: NOTE: This was empty in stock; supposed to load via JS; for now, put the current if this is empty -->
    <#if defaultStateProvinceGeoId?has_content>
      <option value="${defaultStateProvinceGeoId}">${defaultStateProvinceGeoId}</option>
    </#if>
  </@field>      
  <@field type="input" label="${uiLabelMap.PartyZipCode}" required=true size="12" maxlength="10" name="${fieldNamePrefix}postalCode" value=(parameters["${fieldNamePrefix}postalCode"]!(postalAddressData.postalCode)!(pafFallbacks.postalCode)!) />
  <@field type="select" label="${uiLabelMap.CommonCountry}" name="${fieldNamePrefix}countryGeoId" id="${fieldNamePrefix}countryGeoId">
      <#if parameters["${fieldNamePrefix}countryGeoId"]??>    
        <#assign defaultCountryGeoId = parameters["${fieldNamePrefix}countryGeoId"]>
      <#elseif (postalAddress??) && (postalAddress.countryGeoId??)>
        <#assign defaultCountryGeoId = postalAddress.countryGeoId>
      <#elseif (pafFallbacks.countryGeoId)??>
        <#assign defaultCountryGeoId = pafFallbacks.countryGeoId>
      <#else>
        <#assign defaultCountryGeoId = getPropertyValue("general.properties", "country.geo.id.default")!"">
      </#if>
      <option selected="selected" value="${defaultCountryGeoId}">
      <#assign countryGeo = delegator.findOne("Geo",{"geoId":defaultCountryGeoId}, false)>
        ${countryGeo.get("geoName",locale)}
      </option>
      <option></option>
      <@render resource="component://common/widget/CommonScreens.xml#countries" />   
  </@field>
    