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
<#if lastCommunications?has_content>    
    <@section>
        <@paginate mode="content" url=makePageUrl("main") viewIndex=viewIndex!0 listSize=listSize!0 viewSize=viewSize!1 layout="bottom">
            <@table type="data-list" role="grid">
                <@thead>
                    <@tr valign="bottom" class="header-row">
                        <@th>${uiLabelMap.PartySubject}</@th>
                        <!--<@th>${uiLabelMap.CommonFrom}</@th>-->
                        <@th>${uiLabelMap.CommonTo}</@th>
                        <@th>${uiLabelMap.CommonDate}</@th>
                        <@th>${uiLabelMap.CommonType}</@th>
                    </@tr>
                </@thead>
                <#list lastCommunications as lastCommunication>
                    <@tr>
                        <@td><a href="<@pageUrl>ViewCommunicationEvent?communicationEventId=${lastCommunication.commEventId!}</@pageUrl>">${lastCommunication.subject!}</a></@td>
                        <!--<@td><a href="<@pageUrl>viewprofile?partyId=${lastCommunication.partyIdFrom!}</@pageUrl>">${lastCommunication.fromPersonFullName!}</a></@td>-->
                        <@td><a href="<@pageUrl>viewprofile?partyId=${lastCommunication.partyIdTo!}</@pageUrl>">${lastCommunication.toPersonFullName!}</a></@td>
                        <@td>${lastCommunication.date?string('yyyy-MM-dd HH:mm')!}</@td>

                        <@td>${lastCommunication.commEventType!}</@td>
                    </@tr>
                </#list>        
            </@table>
        </@paginate>
    </@section>
<#else>
    <@commonMsg type="result-norecord"/>            
</#if>