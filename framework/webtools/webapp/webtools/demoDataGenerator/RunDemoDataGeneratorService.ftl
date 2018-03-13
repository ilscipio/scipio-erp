<#include "component://webtools/webapp/webtools/service/servicecommon.ftl">

<#macro demoDataServiceFields serviceParameters params={} exclude={}>
  <#list serviceParameters as serviceParameter>
  	<#local rawName = rawString(serviceParameter.name)>
  	<#local fieldLabel>${serviceParameter.name} (<em>${serviceParameter.type}</em>)<#if defaultValStr?has_content> (${uiLabelMap.WebtoolsServiceDefault}: <em>${defaultValStr}</em>)</#if></#local>
  	<#if rawName?has_content && rawName == "dataGeneratorProvider">
      	 <@field type="select" label=wrapAsRaw(fieldLabel, 'htmlmarkup') name="dataGeneratorProvider">
		 	<option value="">---</option>
		 	<#list dataGeneratorProviders as dataGeneratorProvider>
		 		<option value="${dataGeneratorProvider!""}">${dataGeneratorProvider!""}</option>
		 	</#list>
		 </@field>
	</#if>
  </#list>	

  <#list serviceParameters as serviceParameter>
    <#-- WARN: watch out for screen auto-escaping on serviceParameter -->
    <#local rawName = rawString(serviceParameter.name)>
    <#if (rawName?has_content && rawName != "dataGeneratorProvider")>
	    <#if (exclude[rawName]!false) != true && (rawName?has_content && rawName != "dataGeneratorProvider")>
	      <#local defaultValue = serviceParameter.defaultValue!>
	      <#local defaultValStr = defaultValue?string><#-- NOTE: forced html escaping - do not pass to macro params -->
	      <#local fieldLabel>${serviceParameter.name} (<em>${serviceParameter.type}</em>)<#if defaultValStr?has_content> (${uiLabelMap.WebtoolsServiceDefault}: <em>${defaultValStr}</em>)</#if></#local>
	      <#local rawType = rawString(serviceParameter.type)>
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

<form name="demoDataGeneratorForm" method="post" action="<@ofbizUrl>DemoDataGeneratorResult?_RUN_SYNC_=Y</@ofbizUrl>">

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