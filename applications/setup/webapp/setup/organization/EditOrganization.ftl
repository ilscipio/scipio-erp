<#include "component://setup/webapp/setup/common/common.ftl">

<#assign defaultParams = {
    <#-- DEV NOTE: I don't know why ofbizsetup forces these to Y by default, but keeping as-is for now -->
    "USER_ADDRESS_ALLOW_SOL": "Y",
    "USER_WORK_ALLOW_SOL": "Y",
    "USER_FAX_ALLOW_SOL": "Y",
    "USER_EMAIL_ALLOW_SOL": "Y"
} + toSimpleMap(defaultParams!{})>
<#assign paramMaps = getWizardFormFieldValueMaps({
    "record":organizationInfo!true, <#-- NOTE: organizationInfo is pre-prepared in data script -->
    "defaults":defaultParams,
    "strictRecord":false <#-- numerous extra fields not on party/partyGroup -->
})>
<#assign params = paramMaps.values>
<#assign fixedParams = paramMaps.fixedValues>

    <@form id=submitFormId action=makeOfbizUrl(target) method="post" validate=setupFormValidate>
        <@defaultWizardFormFields exclude=["orgPartyId", "partyId"]/><#-- these will conflict with SetupWorker -->
        <@field type="hidden" name="isCreateOrganization" value=(organizationInfo??)?string("N","Y")/>
        
        <#-- TODO: REVIEW: I don't know if we want to disable this or not yet
        <@field type="hidden" name="update_current_userlogin_party" value="false">-->
        
      <#if organizationInfo??>
        <@field type="display" name="partyId" label=uiLabelMap.PartyPartyId><#rt/>
            <@setupExtAppLink uri="/partymgr/control/viewprofile?partyId=${rawString(params.partyId!)}" text=(params.partyId!)/><#t/>
        </@field><#lt/>
        <@field type="hidden" name="partyId" value=(params.partyId!)/> 
      <#else>
        <@field type="input" name="partyId" value=(params.partyId!) label=uiLabelMap.PartyPartyId placeholder="Company"/>
      </#if>

        <@field type="input" name="groupName" value=(params.groupName!) label=uiLabelMap.SetupOrganizationName required=true size="30" maxlength="60"/>
        
      <#-- SPECIAL: we may have to relax the required fields for already-created organizations, or else cause trouble for people's configs-->
      <#-- UPDATE: leaving these required for now because most were removed and this causes inconsistency
      <#assign fieldsRequired = !(organizationInfo??)>-->
      <#assign fieldsRequired = true>
        
        <#if organizationInfo??>
          <#assign addressManageUri = "/partymgr/control/viewprofile?partyId=${rawString(params.partyId!)}">
          <#assign fieldLabelDetail><@formattedContactMechPurposeDescs (generalAddressContactMechPurposes![]) ; description><b>${escapeVal(description, 'html')}</b><br/></@formattedContactMechPurposeDescs>
          </#assign>
        <#else>
          <#assign labelDetail = "">
          <#assign addressManageUri = "">
        </#if>
      
        <@field type="generic" label=uiLabelMap.PartyGeneralAddress labelDetail=fieldLabelDetail>
          <#if organizationInfo??>
            <#-- NOT NEEDED: the ofbiz services automatically detect if address changed and
                avoids creating a new contactMechId if same
            <@field type="checkbox" checkboxType="simple" name="USE_ADDRESS" label=uiLabelMap.SetupUpdateAddress
                id="USE_ADDRESS_CHECK" value="true" altValue="false" currentValue=(params.USE_ADDRESS!"false")/>
            <@script>
              jQuery(document).ready(function() {
                  var setUseMailShipAddr = function(value, area) {
                      jQuery('input,select', area).prop('disabled', !value);
                  };
                  var updateUseMailShipAddr = function() {
                      var checked = jQuery('#USE_ADDRESS_CHECK').is(':checked');
                      var area = jQuery('#setupOrg-editMailShipAddr-area');
                      setUseMailShipAddr(checked, area);
                  };
                  updateUseMailShipAddr();
                  jQuery('#USE_ADDRESS_CHECK').change(updateUseMailShipAddr);
              });
            </@script>
             -->
            <#if params.USER_ADDRESS_CONTACTMECHID?has_content>
              <@field type="hidden" name="USER_ADDRESS_CONTACTMECHID" value=(params.USER_ADDRESS_CONTACTMECHID!)/>
            </#if>
          <#--
          <#else>
            <@field type="hidden" name="USE_ADDRESS" value=(USE_ADDRESS!"true")/>
          -->
          </#if>
          <@field type="hidden" name="USE_ADDRESS" value="true"/>
          
          <div id="setupOrg-editMailShipAddr-area">
            <@fields args={"type":"default", "ignoreParentField":true}>
              <@render resource="component://setup/widget/ProfileScreens.xml#postalAddressFields" 
                  ctxVars={
                    "pafFieldNamePrefix":"USER_",
                    "pafFieldIdPrefix":"EditOrganization_",
                    "pafUseScripts":true,
                    "pafFallbacks":({}),
                    "pafParams": params,
                    "pafFieldNameMap": {
                      "stateProvinceGeoId": "STATE",
                      "countryGeoId": "COUNTRY",
                      "address1": "ADDRESS1",
                      "address2": "ADDRESS2",
                      "city": "CITY",
                      "postalCode": "POSTAL_CODE"
                    },
                    "pafUseToAttnName":false,  <#-- (organizationInfo??) TODO: REVIEW -->
                    "pafMarkRequired":fieldsRequired
                  }/>
              <@field type="hidden" name="USER_ADDRESS_ALLOW_SOL" value=(fixedParams.USER_ADDRESS_ALLOW_SOL!)/>
            </@fields>
          
          <#if organizationInfo??>
            <#assign addressNoticeParams = {"purposes":getContactMechPurposeDescs(locationPurposes)?join(", ")}>
            <#if !generalAddressContactMech??>
                <@alert type="warning">${getLabel('SetupNoAddressDefinedNotice', '', addressNoticeParams)}</@alert>
            <#else>
              <#if (generalAddressStandaloneCompleted!false) == false>
                <#if (locationAddressesCompleted!false) == true>
                  <@alert type="info">${getLabel('SetupSplitAddressPurposesNotice', '', addressNoticeParams)}
                    <@setupExtAppLink uri=addressManageUri text=uiLabelMap.PartyManager class="+${styles.link_nav} ${styles.action_update}"/>
                  </@alert>
                <#else>
                  <@alert type="warning">${getLabel('SetupMissingAddressPurposesNotice', '', addressNoticeParams)}
                    <@setupExtAppLink uri=addressManageUri text=uiLabelMap.PartyManager class="+${styles.link_nav} ${styles.action_update}"/>
                  </@alert>
                  <#-- NOTE: this flag could be destructive, that's why it's false by default -->
                  <@field type="checkbox" checkboxType="simple" name="USER_ADDRESS_CREATEMISSINGPURPOSES" label=uiLabelMap.SetupCreateMissingAddressPurposes
                    id="USER_ADDRESS_CREATEMISSINGPURPOSES_CHECK" value="true" altValue="false" currentValue=(params.USER_ADDRESS_CREATEMISSINGPURPOSES!"false")/>
                </#if>
              </#if>
            </#if>
          </#if>
          </div>
        </@field>
        
        <#if organizationInfo?? && params.USER_WORK_CONTACTMECHID?has_content>
          <@field type="hidden" name="USER_WORK_CONTACTMECHID" value=(params.USER_WORK_CONTACTMECHID!)/>
        </#if>
        <@telecomNumberField label=uiLabelMap.PartyContactWorkPhoneNumber params=params
            fieldNamePrefix="USER_WORK_" countryCodeName="COUNTRY" areaCodeName="AREA" contactNumberName="CONTACT" extensionName="EXT">
          <@fields type="default-compact" ignoreParentField=true>
            <#--<@allowSolicitationField params=params name="USER_WORK_ALLOW_SOL" allowSolicitation="" containerClass="+${styles.field_extra!}" />-->
            <@field type="hidden" name="USER_WORK_ALLOW_SOL" value=(fixedParams.USER_WORK_ALLOW_SOL!)/>
          </@fields>
        </@telecomNumberField>
        
        <#if organizationInfo?? && params.USER_FAX_CONTACTMECHID?has_content>
          <@field type="hidden" name="USER_FAX_CONTACTMECHID" value=(params.USER_FAX_CONTACTMECHID!)/>
        </#if>
        <@telecomNumberField label=uiLabelMap.PartyContactFaxPhoneNumber params=params
            fieldNamePrefix="USER_FAX_" countryCodeName="COUNTRY" areaCodeName="AREA" contactNumberName="CONTACT" extensionName="EXT">
          <@fields type="default-compact" ignoreParentField=true>
            <#--<@allowSolicitationField params=params name="USER_FAX_ALLOW_SOL" allowSolicitation="" containerClass="+${styles.field_extra!}" />-->
            <@field type="hidden" name="USER_FAX_ALLOW_SOL" value=(fixedParams.USER_FAX_ALLOW_SOL!)/>
          </@fields>
        </@telecomNumberField>
        
        <#-- SCIPIO: NOTE: probably can get away with never requiring email -->
        <@field type="hidden" name="require_email" value="false"/>
        <#if params.USER_EMAIL_CONTACTMECHID?has_content>
          <@field type="hidden" name="USER_EMAIL_CONTACTMECHID" value=(params.USER_EMAIL_CONTACTMECHID!)/>
        </#if>
        <@field type="input" name="USER_EMAIL" value=(params.USER_EMAIL!) label=uiLabelMap.CommonEmail size="60" maxlength="250"/><#-- required=fieldsRequired  -->
        <@field type="hidden" name="USER_EMAIL_ALLOW_SOL" value=(fixedParams.USER_EMAIL_ALLOW_SOL!)/>

    </@form>
