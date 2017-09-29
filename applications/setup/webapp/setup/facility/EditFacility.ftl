<#include "component://setup/webapp/setup/common/common.ftl">

<#assign defaultParams = {
    "facilityTypeId": "WAREHOUSE",
    "ownerPartyId": partyId!,
    "defaultInventoryItemTypeId": "NON_SERIAL_INV_ITEM",
    "defaultWeightUomId": defaultDefaultWeightUomId!
    <#--"partyId": partyId!-->
}>
<#assign paramMaps = getWizardFormFieldValueMaps({
    "record":facility!true, <#-- NOTE: must fallback with boolean true -->
    "defaults":defaultParams,
    "strictRecord":true <#-- TODO: REMOVE (debugging) -->
})>
<#assign params = paramMaps.values>
<#assign fixedParams = paramMaps.fixedValues>

    <@form id=submitFormId action=makeOfbizUrl(target) method="post" validate=setupFormValidate>
        <@defaultWizardFormFields exclude=["facilityId"]/>
        <@field type="hidden" name="isCreateFacility" value=(facility??)?string("N","Y")/>
        
      <#if facility??>
        <@field type="display" label=uiLabelMap.FormFieldTitle_facilityId tooltip=uiLabelMap.ProductNotModificationRecrationFacility><#rt/>
            <@setupExtAppLink uri="/facility/control/EditFacility?facilityId=${rawString(params.facilityId!)}" text=(params.facilityId!)/><#t/>
        </@field><#lt/>
        <@field type="hidden" name="facilityId" value=(params.facilityId!)/> 
      <#else>
        <#-- TODO: REVIEW: required=true -->
        <@field type="input" name="facilityId" label=uiLabelMap.FormFieldTitle_facilityId value=(params.facilityId!)/>
      </#if>

        <@field type="input" name="facilityName" value=(params.facilityName!) label=uiLabelMap.ProductName required=true size="30" maxlength="60"/>
        <@field type="input" name="description" value=(params.description!) label=uiLabelMap.SetupFacilityDescription size="60" maxlength="250"/>
        <@field type="input" name="defaultDaysToShip" value=(params.defaultDaysToShip!) label=uiLabelMap.ProductDefaultDaysToShip size="10" maxlength="20"/>
        <@field type="select" name="defaultWeightUomId" value=(params.defaultWeightUomId!) label=uiLabelMap.ProductFacilityDefaultWeightUnit size="10" maxlength="20">
          <option value="">${uiLabelMap.CommonNone}</option>
          <#list weightUomList as uom>
            <option value="${uom.uomId}"<#if (uom.uomId == (params.defaultWeightUomId!))> selected="selected"<#t/></#if><#rt/>
                <#lt/>>${uom.get("description",locale)!(uom.uomId)} (${uom.abbreviation!})</option>
          </#list>
        </@field>
        
        <@field type="hidden" name="facilityTypeId" value=(fixedParams.facilityTypeId!)/>
        <@field type="hidden" name="ownerPartyId" value=(fixedParams.ownerPartyId!)/>
        <@field type="hidden" name="defaultInventoryItemTypeId" value=(fixedParams.defaultInventoryItemTypeId!)/>
        <#--<@field type="hidden" name="defaultWeightUomId" value=(fixedParams.defaultWeightUomId!)/>-->
        
        <@field type="hidden" name="partyId" value=(partyId!)/>
        
        <#-- FIXME: this is fixed to the company address - the service won't accept anything else yet,
            so can't send manual -->
        <#-- TODO: updating the facility location is not possible -->
        <#if facility??>
          <#assign addressLabelDetail>(<@setupExtAppLink uri="/facility/control/settings?facilityId=${rawString(params.facilityId!)}" text=uiLabelMap.CommonManage/>)</#assign>
        <#else>
          <#assign addressLabelDetail = "">
        </#if>
        <@field type="generic" label=uiLabelMap.OrderAddress labelDetail=addressLabelDetail>
            <#if facility??>
              <#list (facilityContactMechs![]) as fcm>
                <#if rawString(fcm.contactMechTypeId!) == "POSTAL_ADDRESS">
                  <#assign postalAddress = delegator.findOne("PostalAddress", {"contactMechId":fcm.contactMechId}, false)!>
                  <#-- TODO?: inline mechanism to edit these after create? (have link below) -->
                  <div>
                  <@formattedAddressBasic address=postalAddress purposes=(facilityContactMechPurposes[rawString(fcm.contactMechId)]!) emphasis=true useToAttnName=false/>
                  </div>
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
    </@form>
