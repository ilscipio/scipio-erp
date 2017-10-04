<#include "component://setup/webapp/setup/common/common.ftl">

<#assign defaultParams = {
    <#-- DEV NOTE: I don't know why ofbizsetup forces these to Y by default, but keeping as-is for now -->
    "USER_ADDRESS_ALLOW_SOL": "Y",
    "USER_WORK_ALLOW_SOL": "Y",
    "USER_FAX_ALLOW_SOL": "Y",
    "USER_EMAIL_ALLOW_SOL": "Y"
}>
<#assign paramMaps = getWizardFormFieldValueMaps({
    "record":partyGroup!true,
    "defaults":defaultParams,
    "strictRecord":false <#-- numerous extra fields not on party/partyGroup -->
})>
<#assign params = paramMaps.values>
<#assign fixedParams = paramMaps.fixedValues>

    <@form id=submitFormId action=makeOfbizUrl(target) method="post" validate=setupFormValidate>
        <@defaultWizardFormFields exclude=["orgPartyId", "partyId"]/><#-- these will conflict with SetupWorker -->
        <@field type="hidden" name="isCreateOrganization" value=(partyGroup??)?string("N","Y")/>
        
        <#-- TODO: REVIEW: I don't know if we want to disable this or not yet
        <@field type="hidden" name="update_current_userlogin_party" value="false">-->
        
      <#if party??>
        <@field type="display" name="partyId" label=uiLabelMap.PartyPartyId><#rt/>
            <@setupExtAppLink uri="/partymgr/control/viewprofile?partyId=${rawString(params.partyId!)}" text=(params.partyId!)/><#t/>
        </@field><#lt/>
        <@field type="hidden" name="partyId" value=(params.partyId!)/> 
      <#else>
        <@field type="input" name="partyId" value=(params.partyId!) label=uiLabelMap.PartyPartyId placeholder="Company"/>
      </#if>

        <@field type="input" name="groupName" value=(params.groupName!) label=uiLabelMap.SetupOrganizationName required=true size="30" maxlength="60"/>
        
      <#-- SPECIAL: we may have to relax the required fields for already-created organizations, or else cause trouble for people's configs-->
      <#assign fieldsRequired = !(party??)>
        
      <#-- TODO: THESE ARE NOT RECOGNIZED BY UPDATE EVENT YET - CREATE ONLY 
          IS NOT CLEAR HOW SAFE IT IS TO USE THESE FORMS TO UPDATE EXISTING ORGANIZATION -
          RISK OF MESSING UP RECORDS - TENTATIVE ONLY FOR NOW 
          
          THE UPDATE SERVICE MUST BE FINISHED BEFORE THESE CAN BE FILLED IN.
          -->
      <#--<#if !party??>-->
      
      <#if party??>
        <@alert type="warning">FIXME: Fields below this do not yet work for updating existing organization.</@alert>
      </#if>
      
        <#if partyId??>
          <#assign fieldLabelDetail>
            <#list (mailShipAddressContactMechPurposes![]) as purpose>
                <b>${delegator.findOne("ContactMechPurposeType", {"contactMechPurposeTypeId":purpose}, true).get("description", locale)}</b><br/>
            </#list>
          </#assign>
        <#else>
          <#assign labelDetail = "">
        </#if>
      
        <@field type="generic" label=uiLabelMap.PartyAddressMailingShipping labelDetail=fieldLabelDetail>
          <#if party??>
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
            <#-- NOTE: this flag could be destructive, that's why it's false by default -->
            <@field type="checkbox" checkboxType="simple" name="USER_ADDRESS_UPDATEROLES" label=uiLabelMap.SetupCreateMissingAddressPurposes
                id="USER_ADDRESS_UPDATEROLES_CHECK" value="true" altValue="false" currentValue=(params.USER_ADDRESS_UPDATEROLES!"false")/>
            <#if mailShipAddressContactMech??>
              <@field type="hidden" name="USER_ADDRESS_CONTACTMECHID" value=mailShipAddressContactMech.contactMechId/>
            </#if>
          <#else>
            <@field type="hidden" name="USE_ADDRESS" value=(USE_ADDRESS!"true")/>
          </#if>
          
          <div id="setupOrg-editMailShipAddr-area">
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
                    "pafUseToAttnName":false,
                    "pafMarkRequired":fieldsRequired
                  }/>
              <@field type="hidden" name="USER_ADDRESS_ALLOW_SOL" value=(fixedParams.USER_ADDRESS_ALLOW_SOL!)/>
            </@fields>
          </div>
          
          <#if partyId??>
            <#if !mailShipAddressContactMech??>
                <#-- TODO: localize -->
                <@alert type="warning">${uiLabelMap.SetupNoShipMailAddressDefinedForOrganizationNotice}</@alert>
            <#else>
              <#if (mailShipAddressStandaloneCompleted!false) == false>
                <#if (locationAddressesCompleted!false) == true>
                  <@alert type="info">${uiLabelMap.SetupSplitShipMailAddressPurposesForOrganizationNotice}
                    <@setupExtAppLink uri="/partymgr/control/viewprofile?partyId=${rawString(params.partyId!)}" text=uiLabelMap.PartyManager class="+${styles.link_nav} ${styles.action_update}"/>
                  </@alert>
                <#else>
                  <@alert type="warning">${uiLabelMap.SetupMissingShipMailAddressPurposesForOrganizationNotice}
                    <@setupExtAppLink uri="/partymgr/control/viewprofile?partyId=${rawString(params.partyId!)}" text=uiLabelMap.PartyManager class="+${styles.link_nav} ${styles.action_update}"/>
                  </@alert>
                </#if>
              </#if>
            </#if>
          </#if>
        </@field>
        
        <#if workPhoneContactMech??>
          <@field type="hidden" name="USER_WORK_CONTACTMECHID" value=workPhoneContactMech.contactMechId/>
        </#if>
        <@telecomNumberField label=uiLabelMap.PartyContactWorkPhoneNumber params=params
            fieldNamePrefix="USER_WORK_" countryCodeName="COUNTRY" areaCodeName="AREA" contactNumberName="CONTACT" extensionName="EXT">
          <@fields type="default-compact" ignoreParentField=true>
            <#--<@allowSolicitationField params=params name="USER_WORK_ALLOW_SOL" allowSolicitation="" containerClass="+${styles.field_extra!}" />-->
            <@field type="hidden" name="USER_WORK_ALLOW_SOL" value=(fixedParams.USER_WORK_ALLOW_SOL!)/>
          </@fields>
        </@telecomNumberField>
        
        <#if faxPhoneContactMech??>
          <@field type="hidden" name="USER_FAX_CONTACTMECHID" value=faxPhoneContactMech.contactMechId/>
        </#if>
        <@telecomNumberField label=uiLabelMap.PartyContactFaxPhoneNumber params=params
            fieldNamePrefix="USER_FAX_" countryCodeName="COUNTRY" areaCodeName="AREA" contactNumberName="CONTACT" extensionName="EXT">
          <@fields type="default-compact" ignoreParentField=true>
            <#--<@allowSolicitationField params=params name="USER_FAX_ALLOW_SOL" allowSolicitation="" containerClass="+${styles.field_extra!}" />-->
            <@field type="hidden" name="USER_FAX_ALLOW_SOL" value=(fixedParams.USER_FAX_ALLOW_SOL!)/>
          </@fields>
        </@telecomNumberField>
        
        <#-- SCIPIO: probably can get away with not requiring email
        <#if partyId??>
          <@field type="hidden" name="require_email" value=(require_email!"true")/>
        <#else>
          <@field type="hidden" name="require_email" value=(require_email!"false")/>
        </#if>-->
        <@field type="hidden" name="require_email" value="false"/>
        <#if primaryEmailContactMech??>
          <@field type="hidden" name="USER_EMAIL_CONTACTMECHID" value=primaryEmailContactMech.contactMechId/>
        </#if>
        <@field type="input" name="USER_EMAIL" value=(params.USER_EMAIL!) label=uiLabelMap.CommonEmail size="60" maxlength="250"/><#-- required=fieldsRequired  -->
        <@field type="hidden" name="USER_EMAIL_ALLOW_SOL" value=(fixedParams.USER_EMAIL_ALLOW_SOL!)/>
        <#-- no point
        <@field type="generic" label=uiLabelMap.PartyEmailAddress>
            <@field type="input" name="USER_EMAIL" value=(params.USER_EMAIL!) label=uiLabelMap.CommonEmail required=true size="60" maxlength="250"/>
            <@field type="hidden" name="USER_EMAIL_ALLOW_SOL" value=(params.USER_EMAIL_ALLOW_SOL!)/>
        </@field>-->
        
      <#--</#if>-->
    </@form>
