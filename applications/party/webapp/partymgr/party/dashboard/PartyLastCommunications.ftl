<#if lastCommunications?has_content>    
    <@section title=uiLabelMap.PartyLastCommunication>
        <@paginate mode="content" url=makePageUrl("main") viewIndex=viewIndex!0 listSize=listSize!0 viewSize=viewSize!1 layout="bottom">
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
                        <@td><a href="<@pageUrl>viewprofile?partyId=${lastCommunication.partyIdFrom!}</@pageUrl>">${lastCommunication.fromPersonFullName!}</a></@td>                    
                        <@td><a href="<@pageUrl>viewprofile?partyId=${lastCommunication.partyIdTo!}</@pageUrl>">${lastCommunication.toPersonFullName!}</a></@td>
                        <@td><a href="<@pageUrl>ViewCommunicationEvent?communicationEventId=${lastCommunication.commEventId!}</@pageUrl>">${lastCommunication.subject!}</a></@td>
                        <@td>${lastCommunication.commEventType!}</@td>
                    </@tr>
                </#list>        
            </@table>
        </@paginate>
    </@section>
<#else>
    <@commonMsg type="result-norecord"/>            
</#if>