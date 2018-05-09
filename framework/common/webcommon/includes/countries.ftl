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
<#assign selectedOption = {}>
<#macro countryOptions optionList>
  <#if optionList?has_content>
  <#list optionList as option>
    <#if option.geoId?has_content>
      <#local optVal = option.geoId>
      <#if option.getModelEntity??>
        <#local optLabel = option.get("geoName", locale)!option.geoId>
      <#else>
        <#local optLabel = option.geoName!option.geoId>
      </#if>
    <#else>
      <#local optVal = option.value>
      <#local optLabel = option.label!option.value>
    </#if>
    <#-- SCIPIO: support currentCountryGeoId, and use has_content instead of ?? -->
    <#if countriesPreselectInline && currentCountryGeoId?has_content>
        <option value="${optVal}"<#if optVal==currentCountryGeoId> selected="selected"</#if>>${optLabel}</option>
    <#elseif countriesPreselect && !currentCountryGeoId?has_content && countriesUseDefault && defaultCountryGeoId?has_content><#-- no countriesPreselectInline here, if use default, always inline -->
        <option value="${optVal}"<#if optVal==defaultCountryGeoId> selected="selected"</#if>>${optLabel}</option>
    <#else>
        <option value="${optVal}">${optLabel}</option>
    </#if>
    <#if currentCountryGeoId?has_content>
        <#if optVal==currentCountryGeoId>
          <#assign selectedOption = {"optVal":optVal, "optLabel":optLabel}>
        </#if>
    <#elseif !currentCountryGeoId?has_content && countriesUseDefault && defaultCountryGeoId?has_content>
        <#if optVal==defaultCountryGeoId>
          <#assign selectedOption = {"optVal":optVal, "optLabel":optLabel}>
        </#if>
    </#if>
  </#list>
  </#if>
</#macro>
<#assign countryMarkup>
  <#if (countriesAllowEmpty!false)>
        <#-- SCIPIO: NOTE: we usually can't use actual empty value for this test, because of FTL empty vs null semantics when the current gets passed to this template... 
            caller has to detect and handle (e.g.: <@render ... ctxVars={"currentCountryGeoId":parameters.countryGeoId!"NONE"} />) -->
        <option value=""<#if countriesPreselect && currentCountryGeoId?? && currentCountryGeoId == (countriesEmptyValue!"NONE")> selected="selected"</#if>></option>
  </#if>
  <@countryOptions optionList=(countriesExtraPreOptions![]) />
  <@countryOptions optionList=countries />
  <@countryOptions optionList=(countriesExtraPostOptions![]) />
</#assign>
<#if countriesPreselectFirst && currentCountryGeoId?has_content && selectedOption?has_content>
        <option value="${selectedOption.optVal!}">${selectedOption.optLabel!}</option>
        <option value="${selectedOption.optVal!}">---</option>
</#if>
${countryMarkup}

