<#if registrations?has_content>    
    <@section title=uiLabelMap.PartyRegistrations>
        <@paginate mode="content" url=makeOfbizUrl("main") viewIndex=viewIndex!0 listSize=listSize!0 viewSize=viewSize!1 layout="bottom">
            <@table type="data-list" role="grid" autoAltRows=true id="securityAlerts">
                <@thead>
                    <@tr valign="bottom" class="header-row">
                        <@th>${uiLabelMap.CommonDate}</@th>
                        <@th>${uiLabelMap.CommonPerson}</@th>
                        <@th>${uiLabelMap.CommonComments}</@th>
                    </@tr>
                </@thead>
                <@tbody>
                    <#list registrations as party>
                        <@tr>
                            <@td><#if party.createdDate?has_content>${party.createdDate?string('yyyy-MM-dd HH:mm')!}</#if></@td>
                            <@td><a href="<@ofbizInterWebappUrl>/partymgr/control/viewprofile?partyId=${party.partyId!}</@ofbizInterWebappUrl>" class="${styles.link_nav_info_id!}">${party.firstName!} ${party.lastName!}</a></@td>
                            <@td>${party.comments!""}</@td>
                        </@tr>
                    </#list>
                </@tbody>
            </@table>
        </@paginate>
    </@section>
<#else>
    <@commonMsg type="result-norecord"/>            
</#if>