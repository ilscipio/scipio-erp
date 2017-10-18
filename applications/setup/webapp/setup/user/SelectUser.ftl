<#include "component://setup/webapp/setup/common/common.ftl">

<#-- FIXME?: either move or find better solution, but @cell works badly here, need float or inline-block -->
<style type="text/css">
  .setupUser-selectUser-select, .setupUser-selectUser-submit-buttons {
    display:inline-block;
  }
  .setupUser-selectUser-select {
    margin-right:0.5em;
  }
  .setupUser-selectUser-select, .setupUser-selectUser-submit-buttons {
    vertical-align:top; <#-- firefox align issue hack -->
  }
</style>

<@script>
    jQuery(document).ready(function() {
        var submitSelectUserForm = function(cntd) {
            var val = jQuery('#setupUser-selectUser-select').val();
            if (val) {
                if (cntd) {
                    jQuery('#setupUser-selectContinueUser-form input[name=userPartyId]').val(val);
                    jQuery('#setupUser-selectContinueUser-form').submit();
                } else {
                    jQuery('#setupUser-selectUser-form').submit();
                } 
            } else {
                jQuery('#setupUser-newUser-form').submit();
            }
        };

        jQuery('#setupUser-selectUser-submit').click(function() {
            submitSelectUserForm(false);
        });
        jQuery('#setupUser-selectUser-submit-continue').click(function() {
            submitSelectUserForm(true);
        });
    });
</@script>

  <@form method="get" action=makeOfbizUrl("setupUser") id="setupUser-selectUser-form">
    <#-- TODO: REVIEW: may make a difference later -->
    <@defaultWizardFormFields exclude=[]/>
    <#--<@field type="hidden" name="setupContinue" value="N"/> not needed yet-->
  
    <@field type="general" label=uiLabelMap.SetupSelectUserForSetup>
        <#-- FIXME: submit doesn't align -->
        <@field type="select" name="userPartyId" id="setupUser-selectUser-select" class="+setupUser-selectUser-select" inline=true style="display:inline-block;">
            <option value="">[${uiLabelMap.SetupCreateNewUser}]</option>
            <option value="" disabled="disabled"></option>
            <#if parties?has_content>
              <#list parties as partyEntry>
                <#assign curParty = delegator.findOne("Party", {"partyId": partyEntry.partyIdTo}, true)>
                <#assign selected = (rawString(partyEntry.partyIdTo) == rawString(userPartyId!))>
                <option value="${curParty.partyId}"<#if selected> selected="selected"</#if>>[${curParty.partyId}]</option>
              </#list>
            </#if>
        </@field>
        <@menu type="button" id="setupUser-selectUser-submit-buttons" class="+setupUser-selectUser-submit-buttons">
          <@menuitem type="link" id="setupUser-selectUser-submit" href="javascript:void(0);" text=uiLabelMap.CommonSelect class="+${styles.action_run_session!} ${styles.action_update!}"/>
          <@menuitem type="link" id="setupUser-selectUser-submit-continue" href="javascript:void(0);" text=uiLabelMap.SetupSelectAndContinue class="+${styles.action_run_session!} ${styles.action_continue!}"/>
        </@menu>
    </@field>
  </@form>
  
  <@form method="get" action=makeOfbizUrl("setupWizard") id="setupUser-selectContinueUser-form">
    <#-- TODO: REVIEW: may make a difference later -->
    <@defaultWizardFormFields exclude=[]/>
    <#--<@field type="hidden" name="setupContinue" value="Y"/> not needed yet-->
  
    <@field type="hidden" name="userPartyId" value=""/>
  </@form>
  
  <@form method="get" action=makeOfbizUrl("setupUser") id="setupUser-newUser-form">
    <#-- TODO: REVIEW: may make a difference later -->
    <@defaultWizardFormFields exclude=[]/>    
    <@field type="hidden" name="newUser" value="Y"/>
  </@form>
  
  