<#-- Common setup macros, definitions, etc. -->

<#-- TODO: REVIEW: this validate flag is a workaround for the loss of validate going from
    form widget to link-triggered forms -->
<#assign setupFormValidate = true>

<#macro compress_single_line><#local captured><#nested></#local>${captured?replace("^\\s+|\\s+$|\\n|\\r", "", "rm")}</#macro>

<#function makeSetupStepUrl name stepState=true excludeParams=[] paramDelim="&amp;">
  <#if stepState?is_boolean>
    <#local stepState = (setupStepStates[name])!{}>
  </#if>
  <#local stepParams = toSimpleMap(stepState.stepParams!{})>
  <#if excludeParams?has_content>
    <#local stepParams = copyMap(stepParams, "e", excludeParams)>
  </#if>
  <#local paramStr = addParamsToStrUrlEnc("", stepParams, paramDelim)>
  <#if paramStr?has_content>
    <#local paramStr = "?" + paramStr>
  </#if>
  <#return makeOfbizUrl("setup"+name?cap_first + paramStr)>
</#function>

<#function makeSetupStepUri name stepState=true>
  <#if stepState?is_boolean>
    <#local stepState = (setupStepStates[name])!{}>
  </#if>
  <#return "setup"+name?cap_first>
</#function>

<#macro setupStepFields name stepState=true exclude=[]>
  <#if stepState?is_boolean>
    <#local stepState = (setupStepStates[name])!{}>
  </#if>
  <#local stepParams = toSimpleMap(stepState.stepParams!)>
  <#list stepParams?keys as paramName>
    <#if !exclude?seq_contains(paramName)>
      <@field type="hidden" name=paramName value=(stepParams[paramName]!)/>
    </#if>
  </#list>
</#macro>

<#macro setupExtAppLink uri text="" class="" target="_blank" extLoginKey=true>
  <a href="<@ofbizInterWebappUrl uri=uri extLoginKey=extLoginKey escapeAs='html'/>"<#t/>
    <#if class?has_content> class="${class}"</#if><#t/>
    <#if target?has_content> target="${target}"</#if><#t/>
    ><#if text?has_content>${escapeVal(text, 'htmlmarkup')}<#else><#nested></#if></a><#t/>
</#macro>

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
  <#local isErrorEff = args.isError!isError!false>
  <#local record = args.record!true>
  <#local useReqParams = args.useReqParams!0><#-- default true -->
  <#if useReqParams?is_number>
    <#local useReqParams = useRequestParameters!"">
  </#if>
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
  
  <#local isRecord = false>
  <#if (isErrorEff && !(useReqParams?is_boolean && useReqParams == false))
        || (useReqParams?is_boolean && useReqParams == true)><#-- check if service error or forced parameters -->
    <#local values = toSimpleMap(parameters)>
    <#local fixedValues = defaults><#-- this is stock ofbizsetup behavior -->
  <#else>
    <#if recordIsContext>
      <#-- when (record==context gotten from above), should be OK to use defaults (to prevent this, pass context explicitly as record) -->
      <#if !(useReqParams?is_boolean && useReqParams == false)>
        <#-- SPECIAL: this allows to pre-fill some fields from parameters when accessing New/Create screen initially -->
        <#local values = defaults + toSimpleMap(parameters) + record>
      <#else>
        <#local values = defaults + record>
      </#if>
    <#else>
      <#local values = record><#-- DON'T use defaults or parameters if real record is present (record!=context) -->
      <#local isRecord = true>
    </#if>
    <#if !recordIsContext>
      <#-- SPECIAL: different from stock ofbizsetup: we keep the values from the record so we 
          don't corrupt existing records - only new ones should get the defaults -->
      <#local fixedValues = record>
      <#local isRecord = true>
    <#else>
      <#local fixedValues = defaults><#-- this is stock ofbizsetup behavior -->
    </#if>
  </#if>
  <#return {"values":values, "fixedValues":fixedValues, "isRecord":isRecord}>
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
<#-- FIXME: only support one set of altNames - this will be solved in the future when getAutoValue is implemented everywhere. 
    Make map copies otherwise. -->
<#macro telecomNumberField params=true label="" fieldNamePrefix="" required=false showExt=true tooltip=false 
    useAltNames=false altNamePrefix="" altNames={"countryCodeName":"countryCode", "areaCodeName":"areaCode", "contactNumberName":"contactNumber", "extensionName":"extension"} 
    inlineArgs...>
  <#if params?is_boolean>
    <#local params = parameters>
  </#if>
  <#local args = inlineArgs>
  <#if useAltNames>
    <#local countryCodeName = altNamePrefix + (altNames.countryCodeName)!"countryCode">
    <#local areaCodeName = altNamePrefix + (altNames.areaCodeName)!"areaCode">
    <#local contactNumberName = altNamePrefix + (altNames.contactNumberName)!"contactNumber">
    <#local extensionName = altNamePrefix + (altNames.extensionName)!"extension">
  <#else>
    <#local countryCodeName = fieldNamePrefix + (args.countryCodeName)!"countryCode">
    <#local areaCodeName = fieldNamePrefix + (args.areaCodeName)!"areaCode">
    <#local contactNumberName = fieldNamePrefix + (args.contactNumberName)!"contactNumber">
    <#local extensionName = fieldNamePrefix + (args.extensionName)!"extension">
  </#if>
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

<#macro setupSubmitMenu submitFormId="" allowSkip="" isCreate=false submitVarSuffixJs="">
  <#if !allowSkip?is_boolean>
    <#local allowSkip = setupStepSkippable!false>
  </#if>
  <#local submitVarSuffixJs = ""><#-- WARN: no support duplicate -->
  <@menu type="button">
    <#if submitFormId?has_content && setupStep?has_content>
      <@menuitem type="link" href="javascript:setupControlMenu${submitVarSuffixJs}.submitSave();" text=uiLabelMap.CommonSave class="+${styles.action_run_sys!} ${styles.action_update!}"/>
      <@menuitem type="link" href="javascript:setupControlMenu${submitVarSuffixJs}.submitSaveContinue();" text=uiLabelMap.SetupSaveAndContinue class="+${styles.action_run_sys!} ${styles.action_continue!}"/>
    </#if>
    <@menuitem type="link" href=(nextAvailSetupStep?has_content?then(makeSetupStepUrl(nextAvailSetupStep), "")) text=uiLabelMap.SetupSkip class="+${styles.action_nav!} ${styles.action_view!}" 
        disabled=!(allowSkip && nextAvailSetupStep?has_content)/>
  </@menu>
  <@script>
    var setupControlMenu${submitVarSuffixJs} = {
        submitFormId: "${escapeVal(submitFormId, 'js')}",
    
        setSubmitFormId: function(id) { this.submitFormId = id; },
        getSubmitFormId: function() { return this.submitFormId; },
        getSubmitForm: function() {
            var id = this.getSubmitFormId();
            if (!id) return null;
            var form = jQuery('#'+id);
            if (!form.length) return null;
            return form;
        },
        
        setSetupContinue: function(value, form) {
            var field = jQuery('input[name=setupContinue]', form);
            if (field.length) {
                field.val(value);
            } else {
                field = jQuery('<input type="hidden" name="setupContinue" value=""/>');
                field.val(value);
                form.append(field);
            }
        },
        
        submitSave: function() {
            var form = this.getSubmitForm();
            if (form) {
                this.setSetupContinue('N', form);
                form.submit();
            }
        },
        
        submitSaveContinue: function() {
            var form = this.getSubmitForm();
            if (form) {
                this.setSetupContinue('Y', form);
                form.submit();
            }
        },
        
        submit: function() {
            var form = this.getSubmitForm();
            if (form) {
                form.submit();
            }
        }
    };
  </@script>
  <#-- old, wont' work here:
      <@field type="submit" text=uiLabelMap[(party??)?then('CommonUpdate', 'CommonCreate')] class="+${styles.link_run_sys} ${styles.action_update}"/>
    -->
</#macro>

<#macro setupSubmitBar submitFormId="" allowSkip="" isCreate=false>
  <@row>
    <@cell columns=6>
      <#nested>
    </@cell>
    <@cell columns=6 class="+${styles.text_right!}">
      <@setupSubmitMenu submitFormId=submitFormId allowSkip=allowSkip isCreate=isCreate/>
    </@cell>
  </@row>
</#macro>

<#function getContactMechPurposeDescs purposes>
  <#local res = []>
  <#list purposes as purpose>
    <#local purposeType = delegator.findOne("ContactMechPurposeType", {"contactMechPurposeTypeId":purpose}, true)!>
    <#local res = res + [rawString(purposeType.get("description", locale)!purposeType.contactMechPurposeTypeId!)]>
  </#list>
  <#return res>
</#function>

<#macro formattedContactMechPurposeDescs purposes=[]>
  <#list getContactMechPurposeDescs(purposes) as description>
    <#nested description>
  </#list>
</#macro>

<#macro postalAddressAsScript postalAddress>
    {
        "contactMechId": "${escapeVal(postalAddress.contactMechId!, 'js')}",
        "toName": "${escapeVal(postalAddress.toName!, 'js')}",
        "attnName": "${escapeVal(postalAddress.attnName!, 'js')}",
        "stateProvinceGeoId": "${escapeVal(postalAddress.stateProvinceGeoId!, 'js')}",
        "countryGeoId": "${escapeVal(postalAddress.countryGeoId!, 'js')}",
        "address1": "${escapeVal(postalAddress.address1!, 'js')}",
        "address2": "${escapeVal(postalAddress.address2!, 'js')}",
        "city": "${escapeVal(postalAddress.city!, 'js')}",
        "postalCode": "${escapeVal(postalAddress.postalCode!, 'js')}"
    }
</#macro>

