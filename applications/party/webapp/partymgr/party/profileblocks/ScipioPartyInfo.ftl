<#-- SCIPIO -->
<@section title=uiLabelMap.CommonOverview>
    <@table type="fields">
        <#if partyContentList?has_content>
            <#list partyContentList as pc>
                <@tr>
                  <@td class="${styles.grid_large!}2">${uiLabelMap.FormFieldTitle_personalImage}
                  </@td>
                  <@td colspan="3"><@img src=makeOfbizInterWebappUrl('/content/control/stream?contentId=${pc.contentId!}') height="150px" width="100px" type="contain"/></@td>
                </@tr>
                <#break>
            </#list> 
        </#if>

        <#if lookupGroup?has_content && lookupParty.logoImageUrl?has_content>
                <@tr>
                  <@td class="${styles.grid_large!}2">${uiLabelMap.CommonOrganizationLogo}</@td>
                  <@td colspan="3"><@img src=(lookupParty.logoImageUrl!) height="150px" width="100px" type="contain"/></@td>
                </@tr>
        </#if>

        <@tr>
            <@td class="${styles.grid_large!}2">${uiLabelMap.PartyName}</@td>
            <@td colspan="3">
                    <#assign partyName>
                        <#if lookupPerson?has_content>
                          <#if lookupParty.salutation?has_content> ${lookupParty.salutation!}</#if>
                          <#if lookupParty.personalTitle?has_content> ${lookupParty.personalTitle!}</#if>
                          <#if lookupParty.firstName?has_content> ${lookupParty.firstName!}</#if>
                          <#if lookupParty.middleName?has_content> ${lookupParty.middleName!}</#if>
                          <#if lookupParty.lastName?has_content> ${lookupParty.lastName!}</#if>
                        </#if>
                        <#if lookupGroup?has_content>
                          <#if lookupParty.groupName?has_content> ${lookupParty.groupName!}</#if>
                        </#if>
                    </#assign>
                    <#if partyNameHistoryList?has_content>
                        <@modal id="modal_info_${parameters.partyId!}" label=wrapAsRaw(partyName, 'htmlmarkup')>
                            <#if partyNameHistoryList?has_content>
                              <@heading>${uiLabelMap.PartyHistoryWas}</@heading>
                              <ul class="no-bullet">
                                <#list partyNameHistoryList as pHistory>
                                  <li>"<#if lookupPerson?has_content>
                                              <#if pHistory.personalTitle?has_content> ${pHistory.personalTitle!}</#if>
                                              <#if pHistory.firstName?has_content> ${pHistory.firstName!}</#if>
                                              <#if pHistory.middleName?has_content> ${pHistory.middleName!}</#if>
                                              <#if pHistory.lastName?has_content> ${pHistory.lastName!}</#if>
                                       </#if>
                                       <#if lookupGroup?has_content>
                                            <#if pHistory.groupName?has_content> ${pHistory.groupName!}</#if>
                                       </#if>
                                       " <#if pHistory.changeDate?has_content>- <@formattedDateTime date=pHistory.changeDate defaultVal="0000-00-00 00:00:00"/></#if>
                                  </li>
                                </#list>
                              </ul>
                            </#if>
                        </@modal>
                    <#else>
                        ${partyName}
                    </#if>
                </@td>
        </@tr>

        <#if lookupGroup?has_content && lookupParty.tickerSymbol?has_content>
            <@tr>
              <@td class="${styles.grid_large!}2">${uiLabelMap.FormFieldTitle_tickerSymbol}
              </@td>
              <@td colspan="3">${lookupParty.tickerSymbol!}</@td>
            </@tr>    
        </#if>

        <#if lookupPerson?has_content && lookupParty.nickname?has_content>
            <@tr>
              <@td class="${styles.grid_large!}2">${uiLabelMap.PartyNickName!}
              </@td>
              <@td colspan="3">${lookupParty.nickname!}</@td>
            </@tr>    
        </#if>

        <#if lookupPerson?has_content && lookupParty.birthDate?has_content>
            <@tr>
              <@td class="${styles.grid_large!}2">${uiLabelMap.PartyBirthDate!}
              </@td>
              <@td colspan="3"><@formattedDate date=(lookupParty.birthDate!) /><#if lookupParty.deceasedDate?has_content> - <@formattedDate date=(lookupParty.deceasedDate!) /></#if></@td>
            </@tr>    
        </#if>

        <#if lookupPerson?has_content && lookupParty.gender?has_content>
            <@tr>
              <@td class="${styles.grid_large!}2">${uiLabelMap.PartyGender}
              </@td>
              <@td colspan="3">${lookupParty.gender!}</@td>
            </@tr>    
        </#if>

        <#if lookupParty.statusId?has_content>
            <#assign status = delegator.findOne("StatusItem", {"statusId" : lookupParty.statusId}, false)/>
            <@tr>
              <@td class="${styles.grid_large!}2">${uiLabelMap.CommonStatus}
              </@td>
              <@td colspan="3">${status.get("description",locale)!}</@td>
            </@tr>    
        </#if>

        <#if lookupParty.comments?has_content>
            <@tr>
              <@td class="${styles.grid_large!}2">${uiLabelMap.PartyComments}
              </@td>
              <@td colspan="3">${lookupParty.comments!}</@td>
            </@tr>    
        </#if>

            <@tr>
              <@td class="${styles.grid_large!}2">${uiLabelMap.PartyAvsString}
              </@td>
              <@td colspan="3">
                  <#if security.hasEntityPermission("PARTYMGR", "_UPDATE", session)>
                      <@modal id="modal_avsstring_${parameters.partyId}" label=((avsOverride.avsDeclineString)!uiLabelMap.CommonGlobal)>
                         <@heading>${uiLabelMap.PartyAvsOver}</@heading>
                         <form name="updateAvsOverride" method="post" action="<@ofbizUrl>updateAvsOverride</@ofbizUrl>">
                         <input type="hidden" name="partyId" value="${parameters.partyId}"/>
                        <@row>
                            <@cell columns=6>
                                <@field type="input" name="avsDeclineString" title=uiLabelMap.PartyAvsString size="60" maxlength="250"/>
                            </@cell>
                            <@cell columns=6>
                                <input type="submit" class="${styles.link_run_sys!} ${styles.action_update!}" value="${uiLabelMap.CommonSave}"/>
                            </@cell>
                        </@row>
                        </form>
                    </@modal>
                  
                  
                    <#if avsOverride??>
                      <a href="<@ofbizUrl>resetAvsOverride?partyId=${lookupParty.partyId}</@ofbizUrl>" class="${styles.action_reset!}">${uiLabelMap.CommonReset}</a>
                    </#if>
                <#else>
                    ${(avsOverride.avsDeclineString)!uiLabelMap.CommonGlobal}
                </#if>
            </@td>
            </@tr>    
        

    </@table>
</@section>