<#if positionList?has_content>

    <@table type="data-complex" role="grid">
        <@thead>
            <@tr valign="bottom" class="header-row">
                <@th class="${styles.text_right!}" width="30%">${uiLabelMap.FormFieldTitle_position}</@th>
                <@th class="${styles.text_right!}" width="30%">${getLabel("RoleType.description.DEPARTMENT", "PartyEntityLabels")}</@th>
                <@th class="${styles.text_right!}" width="40%">${uiLabelMap.HumanResPartyQualification}</@th>
            </@tr>
        </@thead>
        <#list positionList as emplPos><#-- emplPos is JobRequisitionAndEmplPosition -->
            <#-- TODO: move to groovy -->
            <#assign emplPosType = emplPos.getRelatedOne("EmplPositionType", false)!>
     
            <#assign groupName = "">
            <#if (emplPos.partyId?has_content)>
              <#assign depParty = delegator.findOne("PartyGroup", {"partyId":emplPos.partyId}, false)!>
              <#if depParty?has_content>
                <#assign groupName = depParty.get("groupName", locale)!"">
              </#if>
            </#if>

            <#assign qualIds = []>
            <#if emplPos.qualification?has_content>
              <#-- only one supported for now -->
              <#assign qualIds = [emplPos.qualification]>
            </#if>

            <@tr>
                <@td class="${styles.text_right!}">
                  <#if (emplPos.emplPositionId)?has_content><a href="<@ofbizUrl>EditEmplPosition?emplPositionId=${emplPos.emplPositionId?html}</@ofbizUrl>" class="${styles.link_nav_inline!} ${styles.action_view!}"></#if>
                    ${(emplPosType.get("description", locale))!}
                  <#if (emplPos.emplPositionId)?has_content></a></#if>
                </@td>
                <@td class="${styles.text_right!}">${groupName}</@td>
                <@td class="${styles.text_right!}">   
                 <#list qualIds as qualId>
                    ${(delegator.findOne("PartyQualType", {"partyQualTypeId": qualId}, false).get("description", locale))!}<#if qualId_has_next>, </#if>
                  </#list>
                </@td>
            </@tr>
        </#list>
    </@table>

<#else>

    <@commonMsg type="result-norecord" />

</#if>