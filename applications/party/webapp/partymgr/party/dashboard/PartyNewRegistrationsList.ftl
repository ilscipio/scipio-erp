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
<#if registrations?has_content>    
    <@section title=uiLabelMap.PartyRegistrations>
        <@paginate mode="content" url=makePageUrl("main") viewIndex=viewIndex!0 listSize=listSize!0 viewSize=viewSize!1 layout="bottom">
            <@table type="data-list" role="grid" autoAltRows=true id="securityAlerts">
                <@thead>
                    <@tr valign="bottom" class="header-row">
                        <@th>${uiLabelMap.CommonDate}</@th>
                        <@th>${uiLabelMap.CommonPerson}</@th>
                        <!--<@th>${uiLabelMap.CommonComments}</@th>-->
                    </@tr>
                </@thead>
                <@tbody>
                    <#list registrations as party>
                        <@tr>
                            <@td><#if party.createdDate?has_content>${party.createdDate?string('yyyy-MM-dd HH:mm')!}<#elseif party.createdStamp?has_content>
                            ${party.createdStamp?string('yyyy-MM-dd HH:mm')!}
                            </#if></@td>
                            <@td><#if party.firstName?has_content || party.lastName?has_content>
                                <a href="<@serverUrl>/partymgr/control/viewprofile?partyId=${party.partyId!}</@serverUrl>" class="${styles.link_nav_info_id!}">${party.firstName!} ${party.lastName!}</a>
                            <#else>
                                <a href="<@serverUrl>/partymgr/control/viewprofile?partyId=${party.partyId!}</@serverUrl>" class="${styles.link_nav_info_id!}">${party.partyId!}</a></#if></@td>
                            <#--<@td>${party.comments!""}</@td>-->
                        </@tr>
                    </#list>
                </@tbody>
            </@table>
        </@paginate>
    </@section>
<#else>
    <@commonMsg type="result-norecord"/>            
</#if>