<@form method="get" action=makeOfbizUrl("setupOrganization") id="setupOrg-selectOrg-form">    
  
    <@field type="generic" label=uiLabelMap.SetupSelectOrganizationForSetup>
        <#-- FIXME: submit doesn't align -->
        <@field type="select" name="partyId" inline=true style="display:inline-block;">            
            <option value="" disabled="disabled"></option>
            <#if parties?has_content>
              <#list parties as partyEntry>
                <#assign curPartyGroup = delegator.findOne("PartyGroup", {"partyId":partyEntry.partyId}, true)>
                <#assign selected = (rawString(partyEntry.partyId) == rawString(partyId!))>
                <option value="${partyEntry.partyId}"<#if selected> selected="selected"</#if>>${curPartyGroup.groupName} [${partyEntry.partyId}]</option>
              </#list>
            </#if>
        </@field>
        
        <@field type="button" text=uiLabelMap.CommonSelect class="+${styles.action_run_session!} ${styles.action_continue!}"/>
	</@field>    
</@form>
  
  
  