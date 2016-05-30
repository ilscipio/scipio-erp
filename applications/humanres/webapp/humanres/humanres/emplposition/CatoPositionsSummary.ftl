<#if positionList?has_content>

    <@table type="data-complex" role="grid">
        <@thead>
            <@tr valign="bottom" class="header-row">
                <@th class="${styles.text_right!}" width="30%">${uiLabelMap.FormFieldTitle_position}</@th>
                <@th class="${styles.text_right!}" width="30%">${getLabel("RoleType.description.DEPARTMENT", "PartyEntityLabels")}</@th>
                <@th class="${styles.text_right!}" width="40%">${uiLabelMap.HumanResPartyQualification}</@th>
            </@tr>
        </@thead>
        <#list positionList as emplPos>
            <#-- TODO: move to groovy -->
            <#assign emplPosType = emplPos.getRelatedOne("EmplPositionType", false)!>
     
            <#-- FIXME: how to get department? -->
            <#-- FIXME: cannot get qualifications from position because no link to job requisition -->
            
            <@tr>
                <@td class="${styles.text_right!}">
                  <#if (emplPos.emplPositionId)?has_content><a href="<@ofbizUrl>EditEmplPosition?emplPositionId=${emplPos.emplPositionId?html}</@ofbizUrl>" class="${styles.link_nav_inline!} ${styles.action_view!}"></#if>
                    ${(emplPosType.get("description", locale))!}
                  <#if (emplPos.emplPositionId)?has_content></a></#if>
                </@td>
                <@td class="${styles.text_right!}"></@td>
                <@td class="${styles.text_right!}">   
        
                </@td>
            </@tr>
        </#list>
    </@table>

<#else>

    <@commonMsg type="result-norecord" />

</#if>