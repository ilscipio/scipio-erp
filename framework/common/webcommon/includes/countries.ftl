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
<#assign countries = Static["org.ofbiz.common.CommonWorkers"].getCountryList(delegator)>
<#assign countriesPreselect = countriesPreselect!true>
<#if countriesPreselect>
  <#assign countriesPreselectFirst = countriesPreselectFirst!false>
<#else>
  <#assign countriesPreselectFirst = false>
</#if>
<#assign countriesPreselectInline = countriesPreselect && !countriesPreselectFirst>
<#assign countriesUseDefault = countriesUseDefault!true>
<#assign selectedCountry = {}>
<#assign countryMarkup>
  <#list countries as country>
    <#-- SCIPIO: support currentCountryGeoId, and use has_content instead of ?? -->
    <#if countriesPreselectInline && currentCountryGeoId?has_content>
        <option value="${country.geoId}"${(country.geoId==currentCountryGeoId)?string(" selected=\"selected\"","")}>${country.get("geoName",locale)!country.geoId}</option>
    <#elseif !currentCountryGeoId?has_content && countriesUseDefault && defaultCountryGeoId?has_content><#-- no countriesPreselectInline here, if use default, always inline -->
        <option value="${country.geoId}"${(country.geoId==defaultCountryGeoId)?string(" selected=\"selected\"","")}>${country.get("geoName",locale)!country.geoId}</option>
    <#else>
        <option value="${country.geoId}">${country.get("geoName",locale)!country.geoId}</option>
    </#if>
    <#if currentCountryGeoId?has_content>
        <#if country.geoId==currentCountryGeoId>
          <#assign selectedCountry = country>
        </#if>
    <#elseif !currentCountryGeoId?has_content && countriesUseDefault && defaultCountryGeoId?has_content>
        <#if country.geoId==defaultCountryGeoId>
          <#assign selectedCountry = country>
        </#if>
    </#if>
  </#list>
</#assign>
<#if countriesPreselectFirst && currentCountryGeoId?has_content && selectedCountry?has_content>
        <option value="${selectedCountry.geoId}">${selectedCountry.get("geoName",locale)!selectedCountry.geoId}</option>
        <option value="${selectedCountry.geoId}">---</option>
</#if>
<#if (countriesAllowEmpty!false)>
        <option value=""<#if countriesPreselect && (currentCountryGeoId!) == "NONE"> selected="selected"</#if>></option>
</#if>
${countryMarkup}

