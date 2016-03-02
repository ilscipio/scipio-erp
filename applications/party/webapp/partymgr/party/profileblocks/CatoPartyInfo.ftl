<#-- CATO -->
<@section title="${uiLabelMap.CommonOverview}">
    <@table type="fields">
        <#if partyContentList?has_content>
            <#list partyContentList as pc>
                <@tr>
                  <@td class="${styles.grid_large!}2"><#--${uiLabelMap.FormFieldTitle_personalImage}-->
                  </@td>
                  <#--CATO: The inline styles should probably be replaced by the th and img-thumgnail classes for foundation/bootstrap -->
                  <@td colspan="3"><img src="<@ofbizInterWebappUrl>/content/control/stream?contentId=${pc.contentId!}</@ofbizInterWebappUrl>" style="max-width: 100%; height: auto" width="100"/></@td>
                </@tr>
                <#break>
            </#list> 
        </#if>

        <@tr>
            <@td class="${styles.grid_large!}2">${uiLabelMap.PartyName}</@td>
            <@td colspan="3">
                    <#assign partyName><#if lookupPerson.salutation?has_content> ${lookupPerson.salutation!}</#if>
                                          <#if lookupPerson.personalTitle?has_content> ${lookupPerson.personalTitle!}</#if>
                                          <#if lookupPerson.firstName?has_content> ${lookupPerson.firstName!}</#if>
                                          <#if lookupPerson.middleName?has_content> ${lookupPerson.middleName!}</#if>
                                          <#if lookupPerson.lastName?has_content> ${lookupPerson.lastName!}</#if></#assign>
                    <@modal id="modal_info_${parameters.partyId!}" label="${partyName!}">
                        <#if partyNameHistoryList?has_content>
                          <@heading>${uiLabelMap.PartyHistoryWas}</@heading>
                          <ul class="no-bullet">
                            <#list partyNameHistoryList as pHistory>
                              <li>"<#if pHistory.personalTitle?has_content> ${pHistory.personalTitle!}</#if>
                                          <#if pHistory.firstName?has_content> ${pHistory.firstName!}</#if>
                                          <#if pHistory.middleName?has_content> ${pHistory.middleName!}</#if>
                                          <#if pHistory.lastName?has_content> ${pHistory.lastName!}</#if>" <#if pHistory.changeDate?has_content>- <@formattedDateTime date=pHistory.changeDate defaultVal="0000-00-00 00:00:00"/></#if></li>
                            </#list>
                          </ul>
                        </#if>
                    </@modal>
                </@td>
        </@tr>

        <#if lookupPerson.nickname?has_content>
            <@tr>
              <@td class="${styles.grid_large!}2">${uiLabelMap.PartyNickName!}
              </@td>
              <@td colspan="3">${lookupPerson.nickname!}</@td>
            </@tr>    
        </#if>

        <#if lookupPerson.birthDate?has_content>
            <@tr>
              <@td class="${styles.grid_large!}2">${uiLabelMap.PartyBirthDate!}
              </@td>
              <@td colspan="3"><@formattedDate date=lookupPerson.birthDate! /><#if lookupPerson.deceasedDate?has_content> - <@formattedDate date=lookupPerson.deceasedDate! /></#if></@td>
            </@tr>    
        </#if>

        <#if lookupPerson.gender?has_content>
            <@tr>
              <@td class="${styles.grid_large!}2">${uiLabelMap.PartyGender}
              </@td>
              <@td colspan="3">${lookupPerson.gender!}</@td>
            </@tr>    
        </#if>

        <#if lookupPerson.statusId?has_content>
            <#assign status = party.getRelatedOne("StatusItem", false)/>
            <@tr>
              <@td class="${styles.grid_large!}2">${uiLabelMap.CommonStatus}
              </@td>
              <@td colspan="3">${status.get("description",locale)!}</@td>
            </@tr>    
        </#if>

        <#if lookupPerson.comments?has_content>
            <@tr>
              <@td class="${styles.grid_large!}2">${uiLabelMap.PartyComments}
              </@td>
              <@td colspan="3">${lookupPerson.comments!}</@td>
            </@tr>    
        </#if>

            <@tr>
              <@td class="${styles.grid_large!}2">${uiLabelMap.PartyAvsString}
              </@td>
              <@td colspan="3">
                  <#if security.hasEntityPermission("PARTYMGR", "_UPDATE", session)>
                      <@modal id="modal_avsstring_${parameters.partyId}" label="${(avsOverride.avsDeclineString)?default(uiLabelMap.CommonGlobal)}">
                         <form name="updateAvsOverride" method="post" action="<@ofbizUrl>updateAvsOverride</@ofbizUrl>">
                         <input type="hidden" name="partyId" value="${parameters.partyId}"/>
                        <@row>
                            <@cell columns=6>
                                <@field type="input" name="avsDeclineString" title="${uiLabelMap.PartyAvsString}" size="60" maxlength="250"/>
                            </@cell>
                            <@cell columns=6>
                                <input type="submit" class="${styles.link_run_sys!} ${styles.action_update!}" value="${uiLabelMap.CommonSave}"/>
                            </@cell>
                        </@row>
                        </form>
                    </@modal>
                  
                  
                    <#if avsOverride??>
                      <a href="<@ofbizUrl>resetAvsOverride?partyId=${party.partyId}</@ofbizUrl>" class="${styles.action_reset!}">${uiLabelMap.CommonReset}</a>
                    </#if>
                <#else>
                    ${(avsOverride.avsDeclineString)?default(uiLabelMap.CommonGlobal)}
                </#if>
            </@td>
            </@tr>    
        

    </@table>
</@section>