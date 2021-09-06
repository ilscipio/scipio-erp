<#--
This file is subject to the terms and conditions defined in the
files 'LICENSE' and 'NOTICE', which are part of this source
code package.
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

