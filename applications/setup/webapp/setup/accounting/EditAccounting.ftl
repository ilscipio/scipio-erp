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
	
}>

<#assign paramMaps = getWizardFormFieldValueMaps({
    "record":true, <#-- NOTE: must fallback with boolean true -->
    "defaults":defaultParams,
    "strictRecord":true <#-- TODO: REMOVE (debugging) -->
})>
<#assign params = paramMaps.values>
<#assign fixedParams = paramMaps.fixedValues>

<@alert type="warning">WARNING: WORK-IN-PROGRESS</@alert>

<@form method="post" action=makeOfbizUrl(target) id=submitFormId name=submitFormId validate=setupFormValidate>
    <@defaultWizardFormFields exclude=["topGlAccountId"] />
    <@field type="hidden" name="isCreateGl" value=(topAccountGlId??)?string("N","Y")/> 
    
    <#assign fieldsRequired = true>
    
    <#if topGlAccount??>
        <@field type="display" name="topGlAccountId" value=(topGlAccount.glAccountId!) label=uiLabelMap.CommonId />
    <#else>
        <@field type="text" name="topGlAccountId" value=(params.topGlAccountId!) label=uiLabelMap.CommonId />    
    </#if>
    
    <@field type="text" name="accountCode" value=(params.accountCode!) label=uiLabelMap.CommonCode />
    <@field type="text" name="accountName" value=(params.accountName!) label=uiLabelMap.CommonName />
    
    <@field type="select" name="glAccountTypeId">
      <option value="" <#if params.glAccountTypeId?has_content> selected="selected"</#if>></option>
      <#list glAccountTypes as glAccountType>
        <#assign selected = (rawString(params.glAccountTypeId!) == (glAccountType.glAccountTypeId!))>
        <option value="${glAccountType.glAccountTypeId!}"<#if selected> selected="selected"</#if>>${glAccountType.description!}</option>
      </#list>
    </@field>
    
    <@field type="select" name="glAccountClassId">
      <option value="" <#if params.glAccountClassId?has_content> selected="selected"</#if>></option>
      <#list glAccountClasses as glAccountClass>
        <#assign selected = (rawString(params.glAccountClassId!) == (glAccountClass.glAccountClassId!))>
        <option value="${glAccountClass.glAccountClassId!}"<#if selected> selected="selected"</#if>>${glAccountClass.description!}</option>
      </#list>
    </@field>
    
    <@field type="select" name="glResourceTypeId">
      <option value="" <#if params.glResourceTypeId?has_content> selected="selected"</#if>></option>
      <#list glResourceTypes as glResourceType>
        <#assign selected = (rawString(params.glResourceTypeId!) == (glResourceType.glResourceTypeId!))>
        <option value="${glResourceType.glResourceTypeId!}"<#if selected> selected="selected"</#if>>${glResourceType.description!}</option>
      </#list>
    </@field>
    
    <@field type="textarea" name="description" cols="30" rows="3" value=(params.description!) required=false label=uiLabelMap.CommonDescription />

</@form>

