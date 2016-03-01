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
            <@td colspan="3"><#if lookupPerson.salutation?has_content> ${lookupPerson.salutation!}</#if><#if lookupPerson.personalTitle?has_content> ${lookupPerson.personalTitle!}</#if><#if lookupPerson.firstName?has_content> ${lookupPerson.firstName!}</#if><#if lookupPerson.middleName?has_content> ${lookupPerson.middleName!}</#if><#if lookupPerson.lastName?has_content> ${lookupPerson.lastName!}</#if></@td>
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

    </@table>
</@section>