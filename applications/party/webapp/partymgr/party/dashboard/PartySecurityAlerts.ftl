<#if securityAlerts?has_content>    
    <@section title=uiLabelMap.PartySecurityAlert>
        <@table type="data-complex" role="grid">
            <@thead>
                <@tr valign="bottom" class="header-row">
                    <@th>${uiLabelMap.CommonReason}</@th>
                    <@th>${uiLabelMap.CommonFrom}</@th>
                    <@th>${uiLabelMap.CommonRequest}</@th>
                    <@th>${uiLabelMap.PartyClientIP}</@th>
                    <@th>${uiLabelMap.CommonDate}</@th>
                </@tr>
            </@thead>
            <#list securityAlerts as securityAlert>
                <@tr>
                    <@td>
                        <#if securityAlert.enabled == 'N' && securityAlert.disabledDateTime?has_content>
                            Account locked
                        <#elseif securityAlert.successfulLogin == 'N'>
                            Login Failed
                        <#else>
                            Unknown
                        </#if> 
                    </@td>
                    
                    <@td>${securityAlert.userLoginId!}</@td>
                    <@td><a href="${securityAlert.requestUrl!}">${securityAlert.contentId?replace('.',' - ')!}</a></@td>
                    <@td>${securityAlert.serverIpAddress!}</@td>
                    <@td>${securityAlert.fromDate?string('yyyy-MM-dd HH:mm:ss')!}</@td>
                </@tr>
            </#list>        
        </@table>
    </@section>
<#else>
    <@commonMsg type="result-norecord"/>            
</#if>