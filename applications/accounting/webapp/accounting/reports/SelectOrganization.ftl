<style type="text/css">
  .acctg-selectOrg-select, .acctg-selectOrg-submit-buttons {
    display:inline-block;
  }
  .acctg-selectOrg-select {
    margin-right:0.5em;
  }
  .acctg-selectOrg-select, .acctg-selectOrg-submit-buttons {
    vertical-align:top; <#-- firefox align issue hack -->
  }
</style>

<@form method="get" action=makePageUrl("TransactionReports") id="acctg-selectOrg-form" name="acctg-selectOrg-form">    
  
    <@field type="generic">
        <#-- FIXME: submit doesn't align -->
        <@field type="select" name="organizationPartyId" inline=true style="display:inline-block;">            
            <option value="" disabled="disabled"></option>
            <#if parties?has_content>
              <#list parties as partyEntry>
                <#assign curPartyGroup = delegator.findOne("PartyGroup", {"partyId":partyEntry.partyId}, true)>
                <#assign selected = (rawString(partyEntry.partyId) == rawString(partyId!))>
                <option value="${partyEntry.partyId}"<#if selected> selected="selected"</#if>>${curPartyGroup.groupName} [${partyEntry.partyId}]</option>
              </#list>
            </#if>
        </@field>
        <@menu type="button" id="acctg-selectOrg-submit-buttons" class="+acctg-selectOrg-submit-buttons">
        	<@menuitem type="link" contentId="acctg-selectOrg-submit" href="javascript:document.forms['acctg-selectOrg-form'].submit();" text=uiLabelMap.CommonSelect class="+${styles.action_run_session!} ${styles.action_update!}"/>        	
       	</@menu>
        
	</@field>    
</@form>
  
  
  