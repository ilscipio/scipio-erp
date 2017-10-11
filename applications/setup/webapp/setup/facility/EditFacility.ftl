<#include "component://setup/webapp/setup/common/common.ftl">

<#assign defaultParams = {
    "facilityTypeId": "WAREHOUSE",
    "ownerPartyId": partyId!,
    "defaultInventoryItemTypeId": "NON_SERIAL_INV_ITEM",
    "defaultWeightUomId": defaultDefaultWeightUomId!
    <#--"partyId": partyId!-->
} + toSimpleMap(defaultParams!{})>
<#assign paramMaps = getWizardFormFieldValueMaps({
    "record":facilityInfo!true, <#-- NOTE: must fallback with boolean true -->
    "defaults":defaultParams,
    "strictRecord":true <#-- TODO: REMOVE (debugging) -->
})>
<#assign params = paramMaps.values>
<#assign fixedParams = paramMaps.fixedValues>

<style type="text/css"><#-- FIXME -->
    .setup-addresslist { }
    .setup-addressentry { display:inline-block; width:20em; }
</style>

    <@form id=submitFormId action=makeOfbizUrl(target) method="post" validate=setupFormValidate>
        <@defaultWizardFormFields exclude=["facilityId"]/>
        <@field type="hidden" name="isCreateFacility" value=(facility??)?string("N","Y")/>
        
      <#if facilityInfo??>
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
        
        <#if facilityInfo??>
          <#assign addressManageUri = "/facility/control/settings?facilityId=${rawString(params.facilityId!)}">
          <#assign addressLabelDetail>
            <@formattedContactMechPurposeDescs (shipAddressContactMechPurposes![]) ; description><b>${escapeVal(description, 'html')}</b><br/></@formattedContactMechPurposeDescs>
            (<@setupExtAppLink uri=addressManageUri text=uiLabelMap.CommonManage/>)
          </#assign>
        <#else>
          <#assign addressLabelDetail = "">
          <#assign addressManageUri = "">
        </#if>
        <@field type="generic" label=uiLabelMap.PartyAddressMailingShipping labelDetail=addressLabelDetail>
          <@field type="hidden" name="useInputFacilityAddr" value="true"/><#-- don't use the stock ofbiz functionality, always use our form so consistent with org -->
        
          <#if facilityInfo??>
            <#if params.shipAddress_contactMechId?has_content>
              <@field type="hidden" name="shipAddress_contactMechId" value=(params.shipAddress_contactMechId!)/>
            </#if>
          </#if>
          <#-- NOTE: always requiring address for now -->
          <@field type="hidden" name="useFacilityAddr" value="true"/>

          <div id="setupFacility-editShipAddr-area">
            <@fields args={"type":"default", "ignoreParentField":true}>
              <@render resource="component://setup/widget/ProfileScreens.xml#postalAddressFields" 
                  ctxVars={
                    "pafFieldNamePrefix":"shipAddress_",
                    "pafFieldIdPrefix":"EditFacility_",
                    "pafUseScripts":true,
                    "pafFallbacks":{},
                    "pafParams": params,
                    "pafUseToAttnName":true,
                    "pafMarkRequired":true
                  }/>
            
          
          <#if srcPostalAddressList?has_content>
            <@script>
                var srcPostalAddressList = [
                  <#list (srcPostalAddressList![]) as postalAddress>
                    <@postalAddressAsScript postalAddress=postalAddress/><#if postalAddress?has_next>,</#if>
                  </#list>
                ];
                var setFacilityShipAddress = function(postalAddress) {
                    var form = jQuery('#${escapeVal(submitFormId, 'js')}');
                    <#assign prefix = "shipAddress_">
                    
                    jQuery('input[name=${prefix}toName]', form).val(postalAddress.toName || '').change();
                    jQuery('input[name=${prefix}attnName]', form).val(postalAddress.attnName || '').change();
                    jQuery('input[name=${prefix}address1]', form).val(postalAddress.address1 || '').change();
                    jQuery('input[name=${prefix}address2]', form).val(postalAddress.address2 || '').change();
                    jQuery('input[name=${prefix}city]', form).val(postalAddress.city || '').change();
                    jQuery('input[name=${prefix}postalCode]', form).val(postalAddress.postalCode || '').change();
                    
                    <#-- SPECIAL: using jQuery promise contruct to wait for this to return otherwise the state val will be squashed
                    jQuery('select[name=${prefix}countryGeoId]', form).val(postalAddress.countryGeoId || '').change();
                    jQuery('select[name=${prefix}stateProvinceGeoId]', form).val(postalAddress.stateProvinceGeoId || '').change();
                    -->
                    var countryElem = jQuery('select[name=${prefix}countryGeoId]', form);
                    countryElem.val(postalAddress.countryGeoId || '');
                    <#-- NOTE: for this to work the change handler in postaladdressfields.ftl has to output the ajaxResult (returned by triggerChange) - see postaladdressfields.ftl 
                        DEV NOTE: explicitly decided against using triggerChange + return value. -->
                    var countryOut = {};
                    countryElem.trigger('change', countryOut);
                    $.when(countryOut.ajaxResult).done(function() {
                        jQuery('select[name=${prefix}stateProvinceGeoId]', form).val(postalAddress.stateProvinceGeoId || '').change();
                    });
                };
            </@script>
            <@field type="generic" label=(rawLabel('CommonOr')+":")>
                <@modal id="setupFacility-selectShipAddr" class="+${styles.link_nav!} ${styles.action_show!}" label=uiLabelMap.SetupSelectAddress>
                  <div class="setup-addresslist">
                    <#list srcPostalAddressList as postalAddress>
                      <div class="setup-addressentry">
                        <@formattedAddressBasic address=postalAddress 
                            purposes=(srcContactMechPurposeMap[rawString(postalAddress.contactMechId)]!)
                            emphasis=true/><br/>
                        <a href="javascript:setFacilityShipAddress(srcPostalAddressList[${postalAddress?index}]);jQuery('#modal_setupFacility-selectShipAddr').foundation('reveal', 'close');void(0);"<#rt/> 
                            <#lt/> class="${styles.link_run_local!} ${styles.action_select!}">${uiLabelMap.CommonSelect}</a>
                      </div>
                    </#list>
                  </div>
                </@modal>
            </@field>
          </#if>
          
            </@fields>
            
          <#if facilityInfo??>
            <#assign addressNoticeParams = {"purposes":getContactMechPurposeDescs(locationPurposes)?join(", ")}>
            <#if !shipAddressContactMech??>
                <@alert type="warning">${getLabel('SetupNoAddressDefinedNotice', '', addressNoticeParams)}</@alert>
            <#else>
              <#if (shipAddressStandaloneCompleted!false) == false>
                <#if (locationAddressesCompleted!false) == true>
                  <@alert type="info">${getLabel('SetupSplitAddressPurposesNotice', '', addressNoticeParams)}
                    <@setupExtAppLink uri=addressManageUri text=uiLabelMap.ProductFacilityManager class="+${styles.link_nav} ${styles.action_update}"/>
                  </@alert>
                <#else>
                  <@alert type="warning">${getLabel('SetupMissingAddressPurposesNotice', '', addressNoticeParams)}
                    <@setupExtAppLink uri=addressManageUri text=uiLabelMap.ProductFacilityManager class="+${styles.link_nav} ${styles.action_update}"/>
                  </@alert>
                  <#-- NOTE: this flag could be destructive, that's why it's false by default -->
                  <@field type="checkbox" checkboxType="simple" name="createMissingShipAddressPurposes" label=uiLabelMap.SetupCreateMissingAddressPurposes
                      id="createMissingShipAddressPurposes_check" value="true" altValue="false" currentValue=(params.createMissingShipAddressPurposes!"false")/>
                </#if>
              </#if>
            </#if>
          </#if>
          
          </div>
        </@field>
    </@form>
