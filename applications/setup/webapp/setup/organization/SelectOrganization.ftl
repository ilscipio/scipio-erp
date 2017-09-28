
<#-- NOTE: sending to setupWizard instead of setupOrganization so it automatically goes to the next unfinished step -->
  <@form method="get" action=makeOfbizUrl(selectOrgTarget) id="setupOrg-selectOrg-form">
    <@field type="select" name="partyId" label=uiLabelMap.SetupOrganization onChange="jQuery('#setupOrg-selectOrg-form').submit(); void(0);">
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
  