<#include "component://setup/webapp/setup/common/common.ftl">

<@script>
  jQuery(document).ready(function() {
    jQuery('#setupOrg-selectOrg-select').change(function() {
        var val = jQuery(this).val();
        if (val) {
            jQuery('#setupOrg-selectOrg-form').submit();
        } else {
            jQuery('#setupOrg-newOrg-form').submit();
        }
    });
  });
</@script>

  <@form method="get" action=makeOfbizUrl(selectOrgTarget) id="setupOrg-selectOrg-form">
    <#-- TODO: REVIEW
    <@defaultWizardFormFields exclude=[]/> -->
  
    <@field type="select" name="partyId" id="setupOrg-selectOrg-select" label=uiLabelMap.SetupOrganization>
        <option value="">[${uiLabelMap.SetupCreateNewOrganization}]</option>
        <option value="" disabled="disabled"></option>
        <#if parties?has_content>
          <#list parties as partyEntry>
            <#assign curPartyGroup = delegator.findOne("PartyGroup", {"partyId":partyEntry.partyId}, true)>
            <#assign selected = (rawString(partyEntry.partyId) == rawString(partyId!))>
            <option value="${partyEntry.partyId}"<#if selected> selected="selected"</#if>>${curPartyGroup.groupName} [${partyEntry.partyId}]</option>
          </#list>
        </#if>
    </@field>
  </@form>
  
  <@form method="get" action=makeOfbizUrl("setupOrganization") id="setupOrg-newOrg-form">
    <#-- TODO: REVIEW
    <@defaultWizardFormFields exclude=[]/> -->
    
    <@field type="hidden" name="newOrganization" value="Y"/>
  </@form>
  
  