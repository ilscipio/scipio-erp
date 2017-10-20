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


<#assign defaultParams = {
	<#--"productStoreId": productStoreId! // not avail/crash -->
}>

<#assign paramMaps = getWizardFormFieldValueMaps({
    "record":true, <#-- NOTE: must fallback with boolean true -->
    "defaults":defaultParams,
    "strictRecord":true <#-- TODO: REMOVE (debugging) -->
})>
<#assign params = paramMaps.values>
<#assign fixedParams = paramMaps.fixedValues>

<@alert type="warning">WARNING: WORK-IN-PROGRESS</@alert>

<@form method="post" action=makeOfbizUrl(target) id="NewAccounting" name="NewAccounting">
    <@defaultWizardFormFields/>
  
  	<@commonMsg type="info-important">${uiLabelMap.CommonFieldsMarkedAreRequired}</@commonMsg>    
	
	<@field type="general" label=uiLabelMap.SetupSelectAccountingModuleForSetup>
	   <@field type="select" name="userPartyId" id="setupAccounting-selectAccounting-select" class="+setupAccounting-selectAccounting-select" inline=true style="display:inline-block;">
            <#-- <option value="">[${uiLabelMap.SetupCreateNewUser}]</option> -->
            <option value="" disabled="disabled"></option>
            <#if accountingCompoments?has_content>
              <#list accountingCompoments as accountingCompoment>
                <option value="${accountingCompoment.getComponentName()}">[${accountingCompoment.getComponentName()}]</option>
              </#list>
            </#if>
        </@field>
    </@field>        
    
</@form>

