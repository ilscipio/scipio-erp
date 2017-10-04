<#include "component://setup/webapp/setup/common/common.ftl">

<@script>
    function submitSelectOrganizationForm() {
        var val = jQuery('#setupOrg-selectOrg-select').val();
        if (val) {
            jQuery('#setupOrg-selectOrg-form').submit();
        } else {
            jQuery('#setupOrg-newOrg-form').submit();
        }
    }
    jQuery(document).ready(function() {
        <#-- 
        jQuery('#setupOrg-selectOrg-select').change(function() {
            submitSelectOrganizationForm();
        });
        -->
        jQuery('#setupOrg-selectOrg-submit').click(function() {
            submitSelectOrganizationForm();
        });
    });
</@script>

  <@form method="get" action=makeOfbizUrl(selectOrgTarget) id="setupOrg-selectOrg-form">
    <#-- TODO: REVIEW: may make a difference later
    <@defaultWizardFormFields exclude=[]/> -->
  
    <@field type="general" label=uiLabelMap.SetupSelectOrganizationForSetup>
        <#-- FIXME: submit doesn't align -->
        <@field type="select" name="partyId" id="setupOrg-selectOrg-select" inline=true>
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
        <@field type="submit" submitType="link" text=uiLabelMap.CommonSelect id="setupOrg-selectOrg-submit" inline=true/>
    </@field>
  </@form>
  
  <@form method="get" action=makeOfbizUrl("setupOrganization") id="setupOrg-newOrg-form">
    <#-- TODO: REVIEW: may make a difference later
    <@defaultWizardFormFields exclude=[]/> -->
    
    <@field type="hidden" name="newOrganization" value="Y"/>
  </@form>
  
  