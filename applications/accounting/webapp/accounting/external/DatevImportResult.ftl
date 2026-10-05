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
<#if stats?has_content>
    <#if fatalMessages?has_content>
        <@alert type="error">
            <ul>
                <#list fatalMessages as fatalMessage>
                    <li>${fatalMessage.message!}</li>
                </#list>
            </ul>
        </@alert>
    <#else>            
        <@table type="data-complex" role="grid">
            <@thead>
                <@tr>
                    <@th>Record</@th>
                    <@th>Level</@th>
                    <@th>Message</@th>
                </@tr>
            </@thead>        
            <#list stats as stat>
                <@tr>
                    <@td>${stat.position!}</@td>
                    <@td>${stat.level!}</@td>
                    <@td>${stat.message!}</@td>
                </@tr>
            </#list>
        </@table>
    </#if>
    
</#if>
