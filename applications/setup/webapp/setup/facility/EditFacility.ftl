<#include "component://setup/webapp/setup/common/common.ftl">

<#assign defaultParams = {
    "facilityTypeId": "WAREHOUSE",
    "ownerPartyId": partyId!,
    "defaultInventoryItemTypeId": "NON_SERIAL_INV_ITEM",
    "defaultWeightUomId": "WT_lb"
    <#--"partyId": partyId!-->
}>
<#assign paramMaps = getWizardFormFieldValueMaps({
    "record":facility!true, <#-- NOTE: must fallback with boolean true -->
    "defaults":defaultParams,
    "strictRecord":true <#-- TODO: REMOVE (debugging) -->
})>
<#assign params = paramMaps.values>
<#assign fixedParams = paramMaps.fixedValues>

    <@form id="EditFacility" action=makeOfbizUrl(target) method="post">
        <@defaultWizardFormFields exclude=["facilityId"]/>
        <@field type="hidden" name="isCreateFacility" value=(facility??)?string("N","Y")/>
        
      <#if facility??>
        <@field type="display" label=uiLabelMap.FormFieldTitle_facilityId tooltip=uiLabelMap.ProductNotModificationRecrationFacility value=(params.facilityId!)/>
        <@field type="hidden" name="facilityId" value=(params.facilityId!)/> 
      <#else>
        <#-- TODO: REVIEW: required=true -->
        <@field type="input" name="facilityId" label=uiLabelMap.FormFieldTitle_facilityId value=(params.facilityId!)/>
      </#if>

        <@field type="input" name="facilityName" value=(params.facilityName!) label=uiLabelMap.ProductName required=true size="30" maxlength="60"/>
        <@field type="input" name="description" value=(params.description!) label=uiLabelMap.SetupFacilityDescription size="60" maxlength="250"/>
        <@field type="input" name="defaultDaysToShip" value=(params.defaultDaysToShip!) label=uiLabelMap.ProductDefaultDaysToShip size="10" maxlength="20"/>
        
        <@field type="hidden" name="facilityTypeId" value=(fixedParams.facilityTypeId!)/>
        <@field type="hidden" name="ownerPartyId" value=(fixedParams.ownerPartyId!)/>
        <@field type="hidden" name="defaultInventoryItemTypeId" value=(fixedParams.defaultInventoryItemTypeId!)/>
        <@field type="hidden" name="defaultWeightUomId" value=(fixedParams.defaultWeightUomId!)/>
        
        <@field type="hidden" name="partyId" value=(partyId!)/>
        
        <#-- TODO: updating the facility location is not possible -->
        <@field type="generic" label=uiLabelMap.OrderAddress>
            <#if facility??>
              <#list (facilityContactMechs![]) as fcm>
                <#if rawString(fcm.contactMechTypeId!) == "POSTAL_ADDRESS">
                  <#assign postalAddress = delegator.findOne("PostalAddress", {"contactMechId":fcm.contactMechId}, false)!>
                  <#-- TODO: mechanism to edit these after create -->
                  <@formattedAddressBasic address=postalAddress purposes=(facilityContactMechPurposes[rawString(fcm.contactMechId)]!) emphasis=true useToAttnName=false/>
                <#else>
                  <#-- anything else? -->
                </#if>
              </#list>
            <#else>
              <#if partyPostalAddress?has_content>
                <@formattedAddressBasic address=partyPostalAddress useToAttnName=false emphasis=true />
                <@field type="hidden" name="postalAddressContactMechId" value=partyPostalAddress.contactMechId/>
              <#else>
                <#-- TODO: manual input form -->
                <@alert type="error">${uiLabelMap.SetupNoAddressOrganizationSetupIncomplete}</@alert>
              </#if>
            </#if>
        </@field>
        
        <@field type="submit" text=uiLabelMap[(facility??)?then('CommonUpdate', 'CommonCreate')] class="+${styles.link_run_sys} ${styles.action_update}" disabled=(!facilitySubmitOk)/>
    </@form>
