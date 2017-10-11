<#include "component://setup/webapp/setup/common/common.ftl">

<#-- FIXME?: either move or find better solution, but @cell works badly here, need float or inline-block -->
<style type="text/css">
  .setupOrg-selectOrg-select, .setupOrg-selectOrg-submit-buttons {
    display:inline-block;
  }
  .setupOrg-selectOrg-select {
    margin-right:0.5em;
  }
  .setupOrg-selectOrg-select, .setupOrg-selectOrg-submit-buttons {
    vertical-align:top; <#-- firefox align issue hack -->
  }
</style>

<@script>
    jQuery(document).ready(function() {
        var submitSelectOrganizationForm = function(cntd) {
            var val = jQuery('#setupOrg-selectOrg-select').val();
            if (val) {
                if (cntd) {
                    jQuery('#setupOrg-selectContinueOrg-form input[name=partyId]').val(val);
                    jQuery('#setupOrg-selectContinueOrg-form').submit();
                } else {
                    jQuery('#setupOrg-selectOrg-form').submit();
                } 
            } else {
                jQuery('#setupOrg-newOrg-form').submit();
            }
        };

        jQuery('#setupOrg-selectOrg-submit').click(function() {
            submitSelectOrganizationForm(false);
        });
        jQuery('#setupOrg-selectOrg-submit-continue').click(function() {
            submitSelectOrganizationForm(true);
        });
    });
</@script>

  <@form method="get" action=makeOfbizUrl("setupOrganization") id="setupOrg-selectOrg-form">
    <#-- TODO: REVIEW: may make a difference later
    <@defaultWizardFormFields exclude=[]/> -->
    <#--<@field type="hidden" name="setupContinue" value="N"/> not needed yet-->
  
    <@field type="general" label=uiLabelMap.SetupSelectOrganizationForSetup>
        <#-- FIXME: submit doesn't align -->
        <@field type="select" name="partyId" id="setupOrg-selectOrg-select" class="+setupOrg-selectOrg-select" inline=true style="display:inline-block;">
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
        <@menu type="button" id="setupOrg-selectOrg-submit-buttons" class="+setupOrg-selectOrg-submit-buttons">
          <@menuitem type="link" id="setupOrg-selectOrg-submit" href="javascript:void(0);" text=uiLabelMap.CommonSelect class="+${styles.action_run_session!} ${styles.action_update!}"/>
          <@menuitem type="link" id="setupOrg-selectOrg-submit-continue" href="javascript:void(0);" text=uiLabelMap.SetupSelectAndContinue class="+${styles.action_run_session!} ${styles.action_continue!}"/>
        </@menu>
    </@field>
  </@form>
  
  <@form method="get" action=makeOfbizUrl("setupWizard") id="setupOrg-selectContinueOrg-form">
    <#-- TODO: REVIEW: may make a difference later
    <@defaultWizardFormFields exclude=[]/> -->
    <#--<@field type="hidden" name="setupContinue" value="Y"/> not needed yet-->
  
    <@field type="hidden" name="partyId" value=""/>
  </@form>
  
  <@form method="get" action=makeOfbizUrl("setupOrganization") id="setupOrg-newOrg-form">
    <#-- TODO: REVIEW: may make a difference later
    <@defaultWizardFormFields exclude=[]/> -->
    
    <@field type="hidden" name="newOrganization" value="Y"/>
  </@form>
  
  