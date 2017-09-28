<#-- Common setup macros, definitions, etc. -->

<#function makeSetupStepUrl name stepState=true>
  <#if stepState?is_boolean>
    <#local stepState = (setupStepStates[name])!{}>
  </#if>
  <#local paramStr = addParamsToStrUrlEnc("", stepState.stepParams!)>
  <#if paramStr?has_content>
    <#local paramStr = "?" + paramStr>
  </#if>
  <#return makeOfbizUrl("setup"+name?cap_first + paramStr)>
</#function>

<#macro defaultWizardFormFields stepName=true stepState=true exclude=[]>
  <#if stepName?is_boolean && stepName == true>
    <#local stepName = setupStep><#-- main context -->
  </#if>
  <#if !stepName?is_boolean && stepName?has_content>
    <@field type="hidden" name="scpSubmitSetupStep" value=stepName/>
  
    <#if stepState?is_boolean>
      <#local stepState = (setupStepStates[stepName])!{}>
    </#if>
    <#local stepParams = toSimpleMap(stepState.stepParams!{})>
    <#list stepParams?keys as paramName>
      <#if !exclude?seq_contains(rawString(paramName))>
        <@field type="hidden" name=paramName value=stepParams[rawString(paramName)]!/>
      </#if>
    </#list>
  </#if>
</#macro>

<#-- Form field value logic - emulates org.ofbiz.widget.model.ModelFormField#getEntry
TODO: in future supposed to be handled with a form library method #getAutoValue

values = user-configurable and hidden
fixedValues = special: params that were hardcoded to preset values in stock ofbizsetup (handling differently here)
-->
<#function getWizardFormFieldValueMaps args={}>
  <#local record = args.record!true>
  <#local useReqParams = args.useReqParams!0><#-- default true -->
  <#local defaults = args.defaults!{}><#-- default values (NOTE: this is not in form widgets) -->
  <#local strictRecord = args.strictRecord!false><#-- if false it converts the GenericValue to map copy to prevent crash on unknown fields -->
  
  <#local recordIsContext = false>
  <#if record?is_boolean>
    <#if record>
      <#local record = context>
      <#local recordIsContext = true>
    <#else>
      <#local record = {}>
    </#if>
  </#if>
  
  <#if strictRecord || recordIsContext>
    <#local record = toSimpleMap(record)>
  <#else>
    <#local record = copyMap(record)>
  </#if>
  <#local defaults = toSimpleMap(defaults)>
  
  <#if useReqParams?is_number>
    <#local useReqParams = useRequestParameters!"">
  </#if>
  <#if ((isError!false) == true && !(useReqParams?is_boolean && useReqParams == false))
        || (useReqParams?is_boolean && useReqParams == true)><#-- check if service error or forced parameters -->
    <#local values = toSimpleMap(parameters)>
    <#local fixedValues = defaults><#-- this is stock ofbizsetup behavior -->
  <#else>
    <#if recordIsContext && !(useReqParams?is_boolean && useReqParams == false)>
      <#-- SPECIAL: this allows to pre-fill some fields when accessing New/Create screen initially -->
      <#local values = toSimpleMap(parameters) + record>
    <#else>
      <#local values = record><#-- DON'T use defaults if record is present -->
    </#if>
    <#if !recordIsContext>
      <#-- SPECIAL: different from stock ofbizsetup: we keep the values from the record so we 
          don't corrupt existing records - only new ones should get the defaults -->
      <#local fixedValues = record>
    <#else>
      <#local fixedValues = defaults><#-- this is stock ofbizsetup behavior -->
    </#if>
  </#if>
  <#return {"values":values, "fixedValues":fixedValues}>
</#function>

<#-- copied from shop -->
<#macro formattedAddressBasic address emphasis=false abbrev=false verbose=true purposes=[] useToAttnName=true>
  <#list purposes as purpose>
    <#if emphasis><b></#if>${delegator.findOne("ContactMechPurposeType", {"contactMechPurposeTypeId":purpose}, true).get("description", locale)}<#if emphasis></b></#if><br/>
  </#list>
  <#-- NOTE: This must NEVER end with a <br/>. only use between elements. -->
  <#if useToAttnName>
    <#if address.toName?has_content>${uiLabelMap.CommonTo}&nbsp;${address.toName}<br /></#if>
    <#if address.attnName?has_content>${uiLabelMap.PartyAddrAttnName}:&nbsp;${address.attnName}<br /></#if>
  </#if>
  <#if address.address1?has_content>${address.address1}<br /></#if>
  <#if address.address2?has_content>${address.address2}<br /></#if>
  <#if address.city?has_content>${address.city}</#if><#rt>
  <#lt><#if address.stateProvinceGeoId?has_content><#if address.city?has_content>, <#else><br/></#if><#rt>
    <#if verbose>${(delegator.findOne("Geo", {"geoId", address.stateProvinceGeoId}, true).get("geoName", locale))!address.stateProvinceGeoId}<#else>${address.stateProvinceGeoId}</#if></#if><#lt>
  <#if address.postalCode?has_content><br />${address.postalCode}</#if>
  <#if address.countryGeoId?has_content><br /><#rt>
    <#if verbose>${(delegator.findOne("Geo", {"geoId", address.countryGeoId}, true).get("geoName", locale))!address.countryGeoId}<#else>${address.countryGeoId}</#if></#if><#lt>
</#macro>

<#-- copied from shop -->
<#macro telecomNumberField params=true label="" fieldNamePrefix="" required=false showExt=true tooltip=false inlineArgs...>
  <#if params?is_boolean>
    <#local params = parameters>
  </#if>
  <#local args = inlineArgs>
  <#local countryCodeName = fieldNamePrefix + (args.countryCodeName)!"countryCode">
  <#local areaCodeName = fieldNamePrefix + (args.areaCodeName)!"areaCode">
  <#local contactNumberName = fieldNamePrefix + (args.contactNumberName)!"contactNumber">
  <#local extensionName = fieldNamePrefix + (args.extensionName)!"extension">
  <@field type="generic" label=label tooltip=tooltip required=required args=args>
      <@field type="input" inline=true size="1" maxlength="10" name=countryCodeName value=((params[countryCodeName])!(args.countryCode)!) tooltip=uiLabelMap.CommonCountryCode required=required/>
      -&nbsp;<@field type="input" inline=true size="2" maxlength="10" name=areaCodeName value=((params[areaCodeName])!(args.areaCode)!) tooltip=uiLabelMap.PartyAreaCode required=required/>
      -&nbsp;<@field type="input" inline=true size="8" maxlength="15" name=contactNumberName value=((params[contactNumberName])!(args.contactNumber)!) tooltip=uiLabelMap.PartyContactNumber required=required/>
      <#if showExt>&nbsp;<span style="white-space: nowrap;">${uiLabelMap.PartyContactExt}&nbsp;<@field type="input" inline=true size="4" maxlength="10" name=extensionName 
        value=((params[extensionName])!(args.extension)!) tooltip=uiLabelMap.PartyExtension /></span></#if>
    <#nested>
  </@field>
</#macro>

<#-- copied from shop -->
<#macro allowSolicitationField params=true name="" inlineArgs...>
  <#if params?is_boolean>
    <#local params = parameters>
  </#if>
  <#local args = inlineArgs>
  <@field type="select" label="${rawLabel('PartyAllowSolicitation')}?" name=name args=args>
    <option></option><#-- NOTE: Empty must be allowed? -->
    <option value="Y"<#if (params[name]!(args.allowSolicitation)!) == "Y"> selected="selected"</#if>>${uiLabelMap.CommonYes}</option>
    <option value="N"<#if (params[name]!(args.allowSolicitation)!) == "N"> selected="selected"</#if>>${uiLabelMap.CommonNo}</option>
  </@field>
</#macro>
