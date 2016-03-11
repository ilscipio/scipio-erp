<#if lastCommunications?has_content>    
    <@section title=uiLabelMap.PartyLastCommunication>
        <@paginate mode="content" url=makeOfbizUrl("main") viewIndex=viewIndex!0 listSize=listSize!0 viewSize=viewSize!1 layout="bottom">
            <@table type="data-list" role="grid">
                <@thead>
                    <@tr valign="bottom" class="header-row">
                        <@th>${uiLabelMap.CommonDate}</@th>
                        <@th>${uiLabelMap.CommonFrom}</@th>
                        <@th>${uiLabelMap.CommonTo}</@th>
                        <@th>${uiLabelMap.PartySubject}</@th>
                        <@th>${uiLabelMap.CommonType}</@th>
                    </@tr>
                </@thead>
                <#list lastCommunications as lastCommunication>
                    <@tr>
                        <@td>${lastCommunication.date?string('yyyy-MM-dd HH:mm')!}</@td>
                        <@td><a href="<@ofbizUrl>viewprofile?partyId=${lastCommunication.partyIdFrom!}</@ofbizUrl>">${lastCommunication.fromPersonFullName!}</a></@td>                    
                        <@td><a href="<@ofbizUrl>viewprofile?partyId=${lastCommunication.partyIdTo!}</@ofbizUrl>">${lastCommunication.toPersonFullName!}</a></@td>
                        <@td><a href="<@ofbizUrl>ViewCommunicationEvent?communicationEventId=${lastCommunication.commEventId!}</@ofbizUrl>">${lastCommunication.subject!}</a></@td>
                        <@td>${lastCommunication.commEventType!}</@td>
                    </@tr>
                </#list>        
            </@table>
        </@paginate>
    </@section>
<#else>
    <@commonMsg type="result-norecord"/>            
</#if>