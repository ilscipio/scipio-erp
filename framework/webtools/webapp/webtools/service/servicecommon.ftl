
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

<#macro serviceFields serviceParameters>
    <#list serviceParameters as serviceParameter>
      <#-- WARN: watch out for screen auto-escaping on serviceParameter -->
      <#local defaultValue = serviceParameter.defaultValue!>
      <#local defaultValStr = defaultValue?string><#-- NOTE: forced html escaping - do not pass to macro params -->
      <#local fieldLabel>${serviceParameter.name} (<em>${serviceParameter.type}</em>)<#if defaultValStr?has_content> (${uiLabelMap.WebtoolsServiceDefault}: <em>${defaultValStr}</em>)</#if></#local>
      <#local rawType = rawString(serviceParameter.type)>
      <#local required = (serviceParameter.optional == "N")>
      <#if rawType == "Boolean" || rawType == "java.lang.Boolean">
        <#-- TERNARY select so that may pass null/empty - NOTE: do not physically preselect the default here
            You could have a checkbox for cases with only 2 values possible but it will just make it inconsistent with the
            cases that require null to be allowed. -->
        <@field type="select" label=wrapAsRaw(fieldLabel, 'htmlmarkup') name=serviceParameter.name required=required>
          <#if !required>
            <option value=""><#if defaultValStr?has_content>(${defaultValStr})</#if></option>
          </#if>
            <option value="true">true</option>
            <option value="false"<#if serviceParameter.optional == "N"> selected="selected"</#if>>false</option>
        </@field>
      <#elseif rawType == "Timestamp" || rawType == "java.sql.Timestamp">
        <@field type="datetime" label=wrapAsRaw(fieldLabel, 'htmlmarkup') name=serviceParameter.name 
            value=(serviceParameter.value!) required=required placeholder=defaultValue/>
      <#else>
        <@field type="input" label=wrapAsRaw(fieldLabel, 'htmlmarkup') size="20" name=serviceParameter.name 
            value=getServiceParamStrRepr(serviceParameter.value!, rawType) required=required placeholder=getServiceParamStrRepr(defaultValue, rawType)/>
      </#if>
    </#list>
</#macro>
