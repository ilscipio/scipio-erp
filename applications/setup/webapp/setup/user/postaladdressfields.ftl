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
<#include "component://setup/webapp/setup/common/common.ftl">

<#-- SCIPIO: migrated from Shop and modified 
    TODO?: REINTEGRATE ADDITIONS BACK INTO SHOP TEMPLATE -->

<#if !pafParams??>
  <#assign pafParams = parameters>
</#if>

<#assign fieldNameMap = {
  <#-- defaults -->
  "toName": "toName",
  "attnName": "attnName",
  "stateProvinceGeoId": "stateProvinceGeoId",
  "countryGeoId": "countryGeoId",
  "address1": "address1",
  "address2": "address2",
  "city": "city",
  "postalCode": "postalCode"
} + (toSimpleMap(fieldNameMap!{}))>


<#if pafParams["${fieldNamePrefix}${fieldNameMap.stateProvinceGeoId}"]??>    
  <#assign defaultStateProvinceGeoId = pafParams["${fieldNamePrefix}${fieldNameMap.stateProvinceGeoId}"]>
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

    <#assign fieldIdPrefixJs = escapeVal(fieldIdPrefix, 'js')>

    <#-- SCIPIO: NOTE: the container IDs can be omitted because the js doesn't make proper use of them anyhow -->
    <#-- TODO?: getAssociatedStateList may be out of date compared to getDependentDropdownValues?  -->
    var errorMsgContainerId = null;
    var containerId = null;
    <#-- NOTE: 2017-10-09: MODIFIED from shop postaladdressfields.ftl to
        return the ajax result to the caller via the out argument.
        TODO: REVIEW: it's not ideal but it should work for time being. -->
    jQuery("#${fieldIdPrefixJs}countryGeoId").change(function(event, out) {
        var ajaxResult = getAssociatedStateList('${fieldIdPrefixJs}countryGeoId', '${fieldIdPrefixJs}stateProvinceGeoId', errorMsgContainerId, containerId);
        if (jQuery.type(out) === 'object') {
            out.ajaxResult = ajaxResult;
        }
    });
    getAssociatedStateList('${fieldIdPrefixJs}countryGeoId', '${fieldIdPrefixJs}stateProvinceGeoId', errorMsgContainerId, containerId);
    
});

</@script>

</#if>

<#if useToAttnName>
  <@field type="input" label=uiLabelMap.PartyToName size="30" maxlength="60" name="${fieldNamePrefix}${fieldNameMap.toName}" value=(pafParams["${fieldNamePrefix}${fieldNameMap.toName}"]!(postalAddressData.toName)!(pafFallbacks.toName)!) />
  <@field type="input" label=uiLabelMap.PartyAttentionName size="30" maxlength="60" name="${fieldNamePrefix}${fieldNameMap.attnName}" value=(pafParams["${fieldNamePrefix}${fieldNameMap.attnName}"]!(postalAddressData.attnName)!(pafFallbacks.attnName)!) containerClass="+${styles.field_extra!}"/>
</#if>
  <@field type="input" label=uiLabelMap.PartyAddressLine1 required=markRequired size="30" maxlength="30" name="${fieldNamePrefix}${fieldNameMap.address1}" value=(pafParams["${fieldNamePrefix}${fieldNameMap.address1}"]!(postalAddressData.address1)!(pafFallbacks.address1)!) />
  <@field type="input" label=uiLabelMap.PartyAddressLine2 size="30" maxlength="30" name="${fieldNamePrefix}${fieldNameMap.address2}" value=(pafParams["${fieldNamePrefix}${fieldNameMap.address2}"]!(postalAddressData.address2)!(pafFallbacks.address2)!) />
  <@field type="input" label=uiLabelMap.PartyCity required=markRequired size="30" maxlength="30" name="${fieldNamePrefix}${fieldNameMap.city}" value=(pafParams["${fieldNamePrefix}${fieldNameMap.city}"]!(postalAddressData.city)!(pafFallbacks.city)!) />    
  <@field type="input" label=uiLabelMap.PartyZipCode required=markRequired size="12" maxlength="10" name="${fieldNamePrefix}${fieldNameMap.postalCode}" value=(pafParams["${fieldNamePrefix}${fieldNameMap.postalCode}"]!(postalAddressData.postalCode)!(pafFallbacks.postalCode)!) />
  <@field type="select" label=uiLabelMap.CommonCountry name="${fieldNamePrefix}${fieldNameMap.countryGeoId}" id="${fieldIdPrefix}countryGeoId">
      <#if pafParams["${fieldNamePrefix}${fieldNameMap.countryGeoId}"]??>    
        <#assign currentCountryGeoId = pafParams["${fieldNamePrefix}${fieldNameMap.countryGeoId}"]>
      <#elseif (postalAddress??) && (postalAddress.countryGeoId??)>
        <#assign currentCountryGeoId = postalAddress.countryGeoId>
      <#elseif (pafFallbacks.countryGeoId)??>
        <#assign currentCountryGeoId = pafFallbacks.countryGeoId>
      <#else>
        <#-- redundant done
        <#assign currentCountryGeoId = getPropertyValue("general.properties", "country.geo.id.default")!"">-->
        <#assign currentCountryGeoId = "">
      </#if>
    <#-- SCIPIO: there's no reason for this; allow countries ftl to select the right one
      <option selected="selected" value="${currentCountryGeoId}">
      <#assign countryGeo = delegator.findOne("Geo",{"geoId":currentCountryGeoId}, false)>
        ${countryGeo.get("geoName",locale)}
      </option>
      <option></option>
    -->
      <@render resource="component://common/widget/CommonScreens.xml#countries" ctxVars={"currentCountryGeoId":currentCountryGeoId}/>   
  </@field>
  <@field type="select" label=uiLabelMap.PartyState name="${fieldNamePrefix}${fieldNameMap.stateProvinceGeoId}" id="${fieldIdPrefix}stateProvinceGeoId">
    <#-- SCIPIO: NOTE: This was empty in stock; supposed to load via JS; for now, put the current if this is empty -->
    <#if defaultStateProvinceGeoId?has_content>
      <option value="${defaultStateProvinceGeoId}">${defaultStateProvinceGeoId}</option>
    </#if>
  </@field>

