<#--
Scipio Commerce
Copyright (C) Ilscipio GmbH

This file is part of Scipio Commerce. Scipio Commerce is free software: you
can redistribute it and modify it under the terms of the GNU Affero General
Public License, version 3, as published by the Free Software Foundation.
Scipio Commerce is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License
for more details. You should have received a copy of the license with this
work (file LICENSE). If not, see <https://www.gnu.org/licenses/agpl-3.0.html>.
A commercial license is available from Ilscipio GmbH.

SPDX-License-Identifier: AGPL-3.0-only
-->
<#include "component://webtools/webapp/webtools/service/servicecommon.ftl">

<#macro demoDataServiceFields serviceParameters params={} exclude={}>
  <#list serviceParameters as serviceParameter>
      <#local rawName = raw(serviceParameter.name)>
      <#local fieldLabel>${serviceParameter.name} (<em>${serviceParameter.type}</em>)<#if defaultValStr?has_content> (${uiLabelMap.WebtoolsServiceDefault}: <em>${defaultValStr}</em>)</#if></#local>
      <#if rawName?has_content && rawName == "dataGeneratorProviderId">
           <@field type="select" label=wrapAsRaw(fieldLabel, 'htmlmarkup') name="dataGeneratorProviderId">
             <#list dataGeneratorProviders as dataGeneratorProvider>
                 <option value="${dataGeneratorProvider.dataGeneratorProviderId}" <#if "LOCAL" == dataGeneratorProvider.dataGeneratorProviderId>selected</#if>>${dataGeneratorProvider.dataGeneratorProviderName}</option>
             </#list>
         </@field>
    </#if>
  </#list>    

  <#list serviceParameters as serviceParameter>
    <#-- WARN: watch out for screen auto-escaping on serviceParameter -->
    <#local rawName = raw(serviceParameter.name)>
    <#if (rawName?has_content && rawName != "dataGeneratorProviderId")>
        <#if (exclude[rawName]!false) != true && (rawName?has_content && rawName != "dataGeneratorProvider")>
          <#local defaultValue = serviceParameter.defaultValue!>
          <#local defaultValStr = defaultValue?string><#-- NOTE: forced html escaping - do not pass to macro params -->
          <#local fieldLabel>${serviceParameter.name} (<em>${serviceParameter.type}</em>)<#if defaultValStr?has_content> (${uiLabelMap.WebtoolsServiceDefault}: <em>${defaultValStr}</em>)</#if></#local>
          <#local rawType = raw(serviceParameter.type)>
          <#local required = (serviceParameter.optional == "N")>
          <#local value = params[rawName]!serviceParameter.value!>
          <#if rawType == "Boolean" || rawType == "java.lang.Boolean">
            <#-- TERNARY select so that may pass null/empty - NOTE: do not physically preselect the default here
                You could have a checkbox for cases with only 2 values possible but it will just make it inconsistent with the
                cases that require null to be allowed. -->
            <#if value?has_content && !value?is_boolean>
              <#local value = value?boolean>
            </#if>
            <@field type="select" label=wrapAsRaw(fieldLabel, 'htmlmarkup') name=serviceParameter.name required=required>
              <#if !required>
                <option value=""<#if !value?has_content> selected="selected"</#if>><#if defaultValStr?has_content>(${defaultValStr})</#if></option>
              </#if>
                <option value="true"<#if value?is_boolean && value> selected="selected"</#if>>true</option>
                <option value="false"<#if (!value?has_content && required) || (value?is_boolean && !value)> selected="selected"</#if>>false</option>
            </@field>
          <#elseif rawType == "Timestamp" || rawType == "java.sql.Timestamp">
            <@field type="datetime" label=wrapAsRaw(fieldLabel, 'htmlmarkup') name=serviceParameter.name 
                value=value required=required placeholder=defaultValue/>      
          <#else>
            <@field type="input" label=wrapAsRaw(fieldLabel, 'htmlmarkup') size="20" name=serviceParameter.name 
                value=getServiceParamStrRepr(value, rawType) required=required placeholder=getServiceParamStrRepr(defaultValue, rawType)/>
          </#if>
        </#if>
    </#if>
  </#list>
</#macro>

<#assign sectionTitle = rawLabel('WebtoolsServiceName') + " - " + raw(parameters.SERVICE_NAME!)>
<@section title=sectionTitle>
    <form name="demoDataGeneratorForm" method="post" action="<@pageUrl>DemoDataGeneratorResult?_RUN_SYNC_=Y</@pageUrl>">
        
          <#-- SCIPIO: leave room for the label area because service parameter names can be long -->
          <@fields fieldArgs={"labelColumns":4}>
            <@demoDataServiceFields serviceParameters=(serviceParameters!)/>
          </@fields>
          
          <#assign serviceParameterNames = serviceParameterNames![]><#-- 2017-09-13: this is now set by ScheduleJob.groovy -->
          <#list scheduleOptions as scheduleOption>
             <#if !serviceParameterNames?seq_contains(scheduleOption.name)>
                <input type="hidden" name="${scheduleOption.name}" value="${scheduleOption.value}"/>
             </#if>
          </#list>
    
          <@field type="submit" text=uiLabelMap.CommonSubmit class="${styles.link_run_sys!} ${styles.action_begin!}" />
    </form>
</@section>