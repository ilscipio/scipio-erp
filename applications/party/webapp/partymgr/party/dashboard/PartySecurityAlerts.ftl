<#if securityAlerts?has_content>    
    <@section title=uiLabelMap.PartySecurityAlert>
        <@paginate mode="content" url=makeOfbizUrl("main") viewIndex=viewIndex!0 listSize=listSize!0 viewSize=viewSize!1 layout="bottom">
            <@table type="data-list" role="grid" autoAltRows=true id="securityAlerts">
                <@thead>
                    <@tr valign="bottom" class="header-row">
                        <@th>${uiLabelMap.CommonDate}</@th>
                        <@th>${uiLabelMap.CommonReason}</@th>
                        <@th>${uiLabelMap.CommonFrom}</@th>
                        <@th>${uiLabelMap.CommonRequest}</@th>
                        <@th>${uiLabelMap.PartyClientIP}</@th>
                    </@tr>
                </@thead>
                <@tbody>
                    <#list securityAlerts as securityAlert>
                        <@tr>
                            <@td>${securityAlert.fromDate?string('yyyy-MM-dd HH:mm')!}</@td>
                            <@td>
                                <#if securityAlert.enabled == 'N' && securityAlert.disabledDateTime?has_content>
                                    ${uiLabelMap.PartyAccountLocked}
                                <#elseif securityAlert.successfulLogin == 'N'>
                                    ${uiLabelMap.PartyLoginFailed}
                                <#else>
                                    ${uiLabelMap.PartyUnknown}
                                </#if> 
                            </@td>
                            
                            <@td>${securityAlert.userLoginId!}</@td>
                            <@td><a href="${securityAlert.requestUrl!}">${securityAlert.contentId?replace('.',' - ')!}</a></@td>
                            <@td>${securityAlert.clientIpAddress!}</@td>
                        </@tr>
                    </#list>
                </@tbody>
            </@table>
            <script>
                $(document).ready(function() {        
                    var table = $('#securityAlerts').DataTable();
                } );
            </script>
        </@paginate>
    </@section>
<#else>
    <@commonMsg type="result-norecord"/>            
</#if>