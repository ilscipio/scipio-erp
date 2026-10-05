<#--
Scipio Commerce
Copyright (C) Ilscipio GmbH

This file is part of Scipio Commerce. Scipio Commerce is free software: you
can redistribute it and modify it under the terms of the GNU Affero General
Public License, version 3, as published by the Free Software Foundation.
Scipio Commerce is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License
for more details. You should have received a copy of the license with this
work (file LICENSE). If not, see <https://www.gnu.org/licenses/agpl-3.0.html>.
A commercial license is available from Ilscipio GmbH.

SPDX-License-Identifier: AGPL-3.0-only
-->
<#if securityAlerts?has_content>    
    <@section title=uiLabelMap.PartySecurityAlert>
        <@paginate mode="content" url=makePageUrl("main") viewIndex=viewIndex!0 listSize=listSize!0 viewSize=viewSize!1 layout="bottom">
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