<#-- SCIPIO -->

<#if emplAppList?has_content>

    <@table type="data-complex" role="grid">
        <@thead>
            <@tr valign="bottom" class="header-row">
                <@th class="${styles.text_right!}" width="30%">${uiLabelMap.FormFieldTitle_position}</@th>
                <@th class="${styles.text_right!}" width="30%">${uiLabelMap.CommonName}</@th>
                <@th class="${styles.text_right!}" width="40%">${uiLabelMap.HumanResPartyQualification}</@th>
            </@tr>
        </@thead>
        <#list emplAppList as emplApp>
            <#-- TODO: move to groovy -->
            <#assign emplPos = emplApp.getRelatedOne("EmplPosition", false)!>
            <#if (emplPos.emplPositionTypeId)?has_content>
              <#assign emplPosType = emplPos.getRelatedOne("EmplPositionType", false)!>
            <#else>
              <#assign emplPosType = {}>
            </#if>

            <#if emplApp.applyingPartyId?has_content>
              <#assign displayPartyNameResult = runService("getPartyNameForDate", {"partyId":emplApp.applyingPartyId, 
                "compareDate":emplApp.applicationDate!nowTimestamp, "userLogin":userLogin})/>
              <#assign qualifications = Static["org.ofbiz.entity.util.EntityUtil"].filterByDate(delegator.findByAnd("PartyQual", {"partyId": emplApp.applyingPartyId}, [], false)!)!>  
            <#else>
              <#assign displayPartyNameResult = {}>
              <#assign qualifications = []>
            </#if>

            <@tr>
                <@td class="${styles.text_right!}">
                  <#if (emplPos.emplPositionId)?has_content><a href="<@pageUrl>EditEmplPosition?emplPositionId=${emplPos.emplPositionId?html}</@pageUrl>" class="${styles.link_nav_inline!} ${styles.action_view!}"></#if>
                    ${(emplPosType.get("description", locale))!}
                  <#if (emplPos.emplPositionId)?has_content></a></#if>
                </@td>
                <@td class="${styles.text_right!}">
                  <#if emplApp.applyingPartyId?has_content><a href="<@pageUrl>FindEmploymentApps?applyingPartyId=${emplApp.applyingPartyId?html}&amp;noConditionFind=Y</@pageUrl>" class="${styles.link_nav_inline!} ${styles.action_view!}"></#if>
                    ${displayPartyNameResult.fullName!getLabel("OrderPartyNameNotFound", "OrderUiLabels")}</@td>
                  <#if emplApp.applyingPartyId?has_content></a></#if>
                <@td class="${styles.text_right!}">   
                  <#list qualifications as qual>
                    ${(qual.getRelatedOne("PartyQualType", false).get("description", locale))!}<#if qual_has_next>, </#if>
                  </#list>
                </@td>
            </@tr>
        </#list>
    </@table>

<#else>

    <@commonMsg type="result-norecord" />

</#if>
