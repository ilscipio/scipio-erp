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
<#assign states = Static["org.ofbiz.common.CommonWorkers"].getStateList(delegator)>
<#assign statesPreselect = statesPreselect!true>
<#if statesPreselect>
  <#assign statesPreselectFirst = statesPreselectFirst!false>
<#else>
  <#assign statesPreselectFirst = false>
</#if>
<#assign statesPreselectInline = statesPreselect && !statesPreselectFirst>
<#assign selectedState = {}>
<#assign statesMarkup>
  <#list states as state>
    <option value="${state.geoId}"<#if statesPreselectInline && (currentStateProvinceGeoId!) == state.geoId> selected="selected"</#if>>${state.geoName!state.geoId}</option>
    <#if (currentStateProvinceGeoId!) == state.geoId>
      <#assign selectedState = state>
    </#if>
  </#list>
</#assign>
<#if statesPreselectFirst && currentStateProvinceGeoId?has_content && selectedState?has_content>
        <option value="${selectedState.geoId}">${selectedState.get("geoName",locale)!selectedState.geoId}</option>
        <option value="${selectedState.geoId}">---</option>
</#if>
<#if (statesAllowEmpty!false)>
        <option value=""<#if statesPreselect && (currentStateProvinceGeoId!) == "NONE"> selected="selected"</#if>></option>
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
