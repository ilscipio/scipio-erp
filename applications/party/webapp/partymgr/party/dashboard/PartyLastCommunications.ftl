<#if lastCommunications?has_content>    
    <@section title=uiLabelMap.PartyLastCommunication>
        <@table type="data-complex" role="grid">
            <@thead>
                <@tr valign="bottom" class="header-row">
                    <@th>${uiLabelMap.CommonFrom}</@th>
                    <@th>${uiLabelMap.CommonTo}</@th>
                    <@th>${uiLabelMap.CommonSubject}</@th>
                    <@th>${uiLabelMap.CommonType}</@th>
                    <@th>${uiLabelMap.CommonDate}</@th>
                </@tr>
            </@thead>
            <#list lastCommunications as lastCommunication>
                <@tr>
                    <@td>${lastCommunication.partyIdFrom!}</@td>                    
                    <@td>${lastCommunication.partyIdTo!}</@td>
                    <@td>${lastCommunication.subject!}</@td>
                    <@td>${lastCommunication.communicationEventTypeId!}</@td>
                    <@td>${lastCommunication.entryDate?string('yyyy-MM-dd HH:mm:ss')!}</@td>
                </@tr>
            </#list>        
        </@table>
    </@section>
<#else>
    <@commonMsg type="result-norecord"/>            
</#if>