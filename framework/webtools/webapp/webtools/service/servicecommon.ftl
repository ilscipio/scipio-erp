
<#-- FIXME: NEEDS DEEPER REVIEW (INCOMPLETE?) + PERHAPS GLOBAL UTILITY FOR STR REPR -->
<#function getServiceParamStrRepr value type>
  <#if isObjectType("string", value)>
    <#return rawString(value)>
  <#elseif isObjectType("complexobject", value)>
    <#-- COMPLEX BEAN-WRAPPED OBJECTS MUST USE toString() BEFORE ?string/rawString
        OTHERWISE CRASH (in rawString) OR DOUBLE-ESCAPE -->
    <#return rawString(value.toString())>
  <#else>
    <#return rawString(value)>
  </#if>
</#function>

<#macro serviceFields serviceParameters params={}>
    <#list serviceParameters as serviceParameter>
      <#-- WARN: watch out for screen auto-escaping on serviceParameter -->
      <#local defaultValue = serviceParameter.defaultValue!>
      <#local defaultValStr = defaultValue?string><#-- NOTE: forced html escaping - do not pass to macro params -->
      <#local fieldLabel>${serviceParameter.name} (<em>${serviceParameter.type}</em>)<#if defaultValStr?has_content> (${uiLabelMap.WebtoolsServiceDefault}: <em>${defaultValStr}</em>)</#if></#local>
      <#local rawType = rawString(serviceParameter.type)>
      <#local rawName = rawString(serviceParameter.name)>
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
    </#list>
</#macro>

<#-- This corresponds to: ServiceForms.xml#runService 
    TODO: REVIEW: parameter handling -->
<#macro serviceInitFields serviceName srvInput=true srvReadOnly=false params={}>
  <#if srvInput>
    <@field type="input" name="SERVICE_NAME" label=uiLabelMap.WebtoolsService value=serviceName readonly=srvReadOnly/>
  <#else>
    <@field type="hidden" name="SERVICE_NAME" value=serviceName/>
  </#if>
    <@field type="input" name="POOL_NAME" label=uiLabelMap.WebtoolsPool value=(params.POOL_NAME!)/>
    <@field type="select" name="_RUN_SYNC_" label=uiLabelMap.WebtoolsMode>
      <#local syncVal = rawString(params._RUN_SYNC_!)> 
      <option value="Y"<#if "Y" == syncVal> selected="selected"</#if>>Sync</option>
      <option value="ASYNC"<#if "ASYNC" == syncVal> selected="selected"</#if>>Async (${uiLabelMap.WebtoolsOneTimeExecNotPersisted})</option>
    </@field>
</#macro>
