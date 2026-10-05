<#--
Scipio Commerce
Copyright (C) Ilscipio GmbH

This file is part of Scipio Commerce. Scipio Commerce is free software: you
can redistribute it and modify it under the terms of the GNU Affero General
Public License, version 3, as published by the Free Software Foundation.
Scipio Commerce is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License
for more details. You should have received a copy of the license with this
work (file LICENSE). If not, see <https://www.gnu.org/licenses/agpl-3.0.html>.
A commercial license is available from Ilscipio GmbH.

SPDX-License-Identifier: AGPL-3.0-only
-->
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
                <#assign selected = (raw(partyEntry.partyId) == raw(partyId!))>
                <option value="${partyEntry.partyId}"<#if selected> selected="selected"</#if>>${curPartyGroup.groupName} [${partyEntry.partyId}]</option>
              </#list>
            </#if>
        </@field>
        <@menu type="button" id="acctg-selectOrg-submit-buttons" class="+acctg-selectOrg-submit-buttons">
            <@menuitem type="link" contentId="acctg-selectOrg-submit" href="javascript:document.forms['acctg-selectOrg-form'].submit();" text=uiLabelMap.CommonSelect class="+${styles.action_run_session!} ${styles.action_update!}"/>            
           </@menu>
        
    </@field>    
</@form>
  
  
  