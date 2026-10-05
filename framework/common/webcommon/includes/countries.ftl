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
<#--
Changes to this file: Copyright (C) Ilscipio GmbH. The changes are licensed
under the GNU Affero General Public License, version 3, or a commercial
license from Ilscipio GmbH (file LICENSE). The original code stays under
the Apache License, version 2.0, as stated above.
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
<#assign hasOptGroups = false>
<#if (countriesExtraPreOptions?has_content || countriesExtraPostOptions?has_content) && useOptGroup!false>
    <#assign hasOptGroups = true>
</#if>
<#macro countryOptions optionList>
  <#if optionList?has_content>
  <#list optionList as option>
    <#if option.geoId?has_content>
      <#local optVal = option.geoId>
      <#local optLabel = option.get("geoName", "component://common/config/CommonEntityLabels.xml", locale)!option.geoId>
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
  <#if hasOptGroups>
      <optgroup <#if optGroupLabels?has_content>label="${optGroupLabels['preLabel']!}"</#if>>
  </#if>
  <@countryOptions optionList=(countriesExtraPreOptions![]) />
  <#if hasOptGroups>
      </optgroup>
      <optgroup <#if optGroupLabels?has_content>label="${optGroupLabels['mainLabel']!}"</#if>>
  </#if>
  <@countryOptions optionList=countries />
  <#if hasOptGroups>
      </optgroup>
      <optgroup <#if optGroupLabels?has_content>label="${optGroupLabels['postLabel']!}"</#if>>
  </#if>
  <@countryOptions optionList=(countriesExtraPostOptions![]) />
  <#if hasOptGroups>
      </optgroup>
  </#if>
</#assign>
<#if countriesPreselectFirst && currentCountryGeoId?has_content && selectedOption?has_content>
        <option value="${selectedOption.optVal!}">${selectedOption.optLabel!}</option>
        <option value="${selectedOption.optVal!}">---</option>
</#if>
${countryMarkup}

