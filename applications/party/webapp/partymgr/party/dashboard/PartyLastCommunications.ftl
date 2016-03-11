<#if lastCommunications?has_content>    
    <@section title=uiLabelMap.PartyLastCommunication>
        <@table type="data-complex" role="grid">
            <@thead>
                <@tr valign="bottom" class="header-row">
                    <@th>${uiLabelMap.CommonFrom}</@th>
                    <@th>${uiLabelMap.CommonTo}</@th>
                    <@th>${uiLabelMap.PartySubject}</@th>
                    <@th>${uiLabelMap.CommonType}</@th>
                    <@th>${uiLabelMap.CommonDate}</@th>
                </@tr>
            </@thead>
            <#list lastCommunications as lastCommunication>
                <@tr>
                    <@td><a href="<@ofbizUrl>viewprofile?partyId=${lastCommunication.partyIdFrom!}</@ofbizUrl>">${lastCommunication.fromPersonFullName!}</a></@td>                    
                    <@td><a href="<@ofbizUrl>viewprofile?partyId=${lastCommunication.partyIdTo!}</@ofbizUrl>">${lastCommunication.toPersonFullName!}</a></@td>
                    <@td><a href="<@ofbizUrl>ViewCommunicationEvent?communicationEventId=${lastCommunication.commEventId!}</@ofbizUrl>">${lastCommunication.subject!}</a></@td>
                    <@td>${lastCommunication.commEventType!}</@td>
                    <@td>${lastCommunication.date?string('yyyy-MM-dd HH:mm')!}</@td>
                </@tr>
            </#list>        
        </@table>
    </@section>
<#else>
    <@commonMsg type="result-norecord"/>            
</#if>