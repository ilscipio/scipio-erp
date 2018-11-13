<#--
This file is subject to the terms and conditions defined in
 file 'LICENSE', which is part of this source code package.
-->
<#assign states = Static["org.ofbiz.common.CommonWorkers"].getStateList(delegator)>
<#assign statesPreselect = statesPreselect!true>
<#if statesPreselect>
  <#assign statesPreselectFirst = statesPreselectFirst!false>
<#else>
  <#assign statesPreselectFirst = false>
</#if>
<#assign statesPreselectInline = statesPreselect && !statesPreselectFirst>
<#assign selectedOption = {}>
<#macro stateOptions optionList>
  <#if optionList?has_content>
  <#list optionList as option>
    <#if option.geoId?has_content>
      <#local optVal = option.geoId>
      <#if optionList.getModelEntity??>
        <#local optLabel = option.get("geoName", locale)!option.geoId>
      <#else>
        <#local optLabel = option.geoName!option.geoId>
      </#if>
    <#else>
      <#local optVal = option.value>
      <#local optLabel = option.label!option.value>
    </#if>
    <option value="${optVal}"<#if statesPreselectInline && currentStateProvinceGeoId?has_content && currentStateProvinceGeoId==optVal> selected="selected"</#if>>${optLabel}</option>
    <#if currentStateProvinceGeoId?has_content && currentStateProvinceGeoId==optVal>
      <#assign selectedOption = {"optVal":optVal, "optLabel":optLabel}>
    </#if>
  </#list>
  </#if>
</#macro>
<#assign statesMarkup>
  <#if (statesAllowEmpty!false)>
        <#-- SCIPIO: NOTE: we usually can't use actual empty value for this test, because of FTL empty vs null semantics when the current gets passed to this template... 
            caller has to detect and handle (e.g.: <@render ... ctxVars={"currentStateProvinceGeoId":parameters.stateProvinceGeoId!"NONE"} />) -->
        <option value=""<#if statesPreselect && currentStateProvinceGeoId?? && currentStateProvinceGeoId == (statesEmptyValue!"NONE")> selected="selected"</#if>></option>
  </#if>
  <@stateOptions optionList=(statesExtraPreOptions![]) />
  <@stateOptions optionList=states />
  <@stateOptions optionList=(statesExtraPostOptions![]) />
</#assign>
<#if statesPreselectFirst && currentStateProvinceGeoId?has_content && selectedOption?has_content>
        <option value="${selectedOption.optVal!}">${selectedOption.optLabel!}</option>
        <option value="${selectedOption.optVal!}">---</option>
</#if>
${statesMarkup}

<#-- Here is some alternate code to get states limited to a region
<#if requestParameters.CUSTOMER_COUNTRY??>
    <#assign stateAssocs = Static["org.ofbiz.common.CommonWorkers"].getAssociatedStateList(delegator,requestParameters.CUSTOMER_COUNTRY)>
<#else>
    <#assign stateAssocs = Static["org.ofbiz.common.CommonWorkers"].getAssociatedStateList(delegator,null)>
</#if>

<#list stateAssocs as stateAssoc>
    <#assign state = stateAssoc.getRelatedOne("AssocGeo", false)>
    <option value="${state.geoId}">${state.geoName!state.geoId}</option>
</#list>
-->
