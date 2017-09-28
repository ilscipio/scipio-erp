<#include "component://setup/webapp/setup/common/common.ftl">

<#assign defaultParams = {
    <#-- DEV NOTE: I don't know why ofbizsetup forces these to Y by default, but keeping as-is for now -->
    "USER_ADDRESS_ALLOW_SOL": "Y",
    "USER_WORK_ALLOW_SOL": "Y",
    "USER_FAX_ALLOW_SOL": "Y",
    "USER_EMAIL_ALLOW_SOL": "Y"
}>
<#assign paramMaps = getWizardFormFieldValueMaps({
    "record":true,
    "defaults":defaultParams
})>
<#assign params = paramMaps.values>
<#assign fixedParams = paramMaps.fixedValues>

    <@form id="EditOrganization" name="EditOrganization" action=makeOfbizUrl(target) method="post">
        <@defaultWizardFormFields exclude=["orgPartyId", "partyId"]/><#-- these will conflict with SetupWorker -->
        
      <#if party??>
        <@field type="display" name="partyId" value=(params.partyId!) label=uiLabelMap.PartyPartyId />
        <@field type="hidden" name="partyId" value=(params.partyId!)/> 
      <#else>
        <@field type="input" name="partyId" value=(params.partyId!) label=uiLabelMap.PartyPartyId />
      </#if>

        <@field type="hidden" name="USE_ADDRESS" value=(USE_ADDRESS!)/>
        <@field type="hidden" name="require_email" value=(require_email!)/>
        
        <@field type="input" name="groupName" value=(params.groupName!) label=uiLabelMap.SetupOrganizationName required=true size="30" maxlength="60"/>
        
      <#-- TODO: REVIEW: current handling by viewprofile... -->
      <#if !party??>
        
        <@field type="generic" label=uiLabelMap.PartyAddressMailingShipping>
          <@fields args={"type":"default", "ignoreParentField":true}>
            <@render resource="component://setup/widget/ProfileScreens.xml#postalAddressFields" 
                  ctxVars={
                    "pafFieldNamePrefix":"USER_",
                    "pafFieldIdPrefix":"EditOrganization_",
                    "pafUseScripts":true,
                    "pafFallbacks":({}),
                    "pafParams":params,
                    "pafFieldNameMap": {
                      "stateProvinceGeoId": "STATE",
                      "countryGeoId": "COUNTRY",
                      "address1": "ADDRESS1",
                      "address2": "ADDRESS2",
                      "city": "CITY",
                      "postalCode": "POSTAL_CODE"
                    },
                    "pafUseToAttnName":false
                  }/>
            <#--
            <@field type="input" name="USER_ADDRESS1" value=(params.USER_ADDRESS1!) label=uiLabelMap.CommonAddress1 required=true size="30" maxlength="60"/>
            <@field type="input" name="USER_ADDRESS2" value=(params.USER_ADDRESS2!) label=uiLabelMap.CommonAddress2 size="30" maxlength="60"/>
            <@field type="input" name="USER_CITY" value=(params.USER_CITY!) label=uiLabelMap.CommonCity required=true size="30" maxlength="60"/>
            <@field type="select" name="USER_STATE" label=uiLabelMap.CommonState required=true>
            </@field>                    
            <@field name="USER_POSTAL_CODE" value=(params.USER_POSTAL_CODE!) label=uiLabelMap.CommonZipPostalCode required=true size="10" maxlength="30"/>
            <@field type="select" name="USER_COUNTRY" label=uiLabelMap.CommonCountry required=true>
                <drop-down no-current-selected-key="${defaultCountryGeoId}">
                    <entity-options entity-name="Geo" key-field-name="geoId" description="${geoId}: ${geoName}">
                        <entity-constraint name="geoTypeId" value="COUNTRY"/>
                        <entity-order-by field-name="geoId"/>
                    </entity-options>
                </drop-down>
            </@field>
            -->
            <@field type="hidden" name="USER_ADDRESS_ALLOW_SOL" value=(fixedParams.USER_ADDRESS_ALLOW_SOL!)/>
          </@fields>
        </@field>
        
        <@telecomNumberField label=uiLabelMap.PartyContactWorkPhoneNumber params=params
            fieldNamePrefix="USER_WORK_" countryCodeName="COUNTRY" areaCodeName="AREA" contactNumberName="CONTACT" extensionName="EXT">
          <@fields type="default-compact" ignoreParentField=true>
            <#--<@allowSolicitationField params=params name="USER_WORK_ALLOW_SOL" allowSolicitation="" containerClass="+${styles.field_extra!}" />-->
            <@field type="hidden" name="USER_WORK_ALLOW_SOL" value=(fixedParams.USER_WORK_ALLOW_SOL!)/>
          </@fields>
        </@telecomNumberField>
        <#--<@field type="generic" label=uiLabelMap.PartyContactWorkPhoneNumber>
            <@field type="input" name="USER_WORK_COUNTRY" value=(params.USER_WORK_COUNTRY!) label=uiLabelMap.CommonCountryCode  size="4" maxlength="10"/>
            <@field type="input" name="USER_WORK_AREA" value=(params.USER_WORK_AREA!) label=uiLabelMap.PartyAreaCode  size="4" maxlength="10"/>
            <@field type="input" name="USER_WORK_CONTACT" value=(params.USER_WORK_CONTACT!) label=uiLabelMap.PartyPhoneNumber  size="15" maxlength="15"/>
            <@field type="input" name="USER_WORK_EXT" value=(params.USER_WORK_EXT!) label=uiLabelMap.PartyContactExt  size="6" maxlength="10"/>
            <@field type="hidden" name="USER_WORK_ALLOW_SOL" value=(params.USER_WORK_ALLOW_SOL!)/>
        </@field>-->
        
        <@telecomNumberField label=uiLabelMap.PartyContactFaxPhoneNumber params=params
            fieldNamePrefix="USER_FAX_" countryCodeName="COUNTRY" areaCodeName="AREA" contactNumberName="CONTACT" extensionName="EXT">
          <@fields type="default-compact" ignoreParentField=true>
            <#--<@allowSolicitationField params=params name="USER_FAX_ALLOW_SOL" allowSolicitation="" containerClass="+${styles.field_extra!}" />-->
            <@field type="hidden" name="USER_FAX_ALLOW_SOL" value=(fixedParams.USER_FAX_ALLOW_SOL!)/>
          </@fields>
        </@telecomNumberField>
        <#--<@field type="generic" label=uiLabelMap.PartyContactFaxPhoneNumber>
            <@field type="input" name="USER_FAX_COUNTRY" value=(params.USER_FAX_COUNTRY!) label=uiLabelMap.CommonCountryCode size="4" maxlength="10"/>
            <@field type="input" name="USER_FAX_AREA" value=(params.USER_FAX_AREA!) label=uiLabelMap.PartyAreaCode size="4" maxlength="10"/>
            <@field type="input" name="USER_FAX_CONTACT" value=(params.USER_FAX_CONTACT!) label=uiLabelMap.PartyPhoneNumber size="15" maxlength="15"/>
            <@field type="input" name="USER_FAX_EXT" value=(params.USER_FAX_EXT!) label=uiLabelMap.PartyContactExt size="6" maxlength="10"/>
            <@field type="hidden" name="USER_FAX_ALLOW_SOL" value=(params.USER_FAX_ALLOW_SOL!)/>
        </@field>-->
        
        <@field type="input" name="USER_EMAIL" value=(params.USER_EMAIL!) label=uiLabelMap.CommonEmail required=true size="60" maxlength="250"/>
        <@field type="hidden" name="USER_EMAIL_ALLOW_SOL" value=(fixedParams.USER_EMAIL_ALLOW_SOL!)/>
        <#-- no point
        <@field type="generic" label=uiLabelMap.PartyEmailAddress>
            <@field type="input" name="USER_EMAIL" value=(params.USER_EMAIL!) label=uiLabelMap.CommonEmail required=true size="60" maxlength="250"/>
            <@field type="hidden" name="USER_EMAIL_ALLOW_SOL" value=(params.USER_EMAIL_ALLOW_SOL!)/>
        </@field>-->
        
      </#if>
        
        <@field type="submit" text=uiLabelMap[(party??)?then('CommonUpdate', 'CommonSave')] class="+${styles.link_run_sys} ${styles.action_update}"/>
    </@form>
