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
<#list dashboardGrid as grid>
    <div class="row">
        <#assign columnSize = Static["java.lang.Math"].round(dashboardColumns / columns) * Static["java.lang.Math"].round(12 / dashboardColumns) />        
        <#list grid as s>            
            <#assign totalColumnSize = totalColumnSize!0 + columnSize />            
            <div class="columns large-${columnSize}">                
                <#if s?has_content>                    
                    ${sections.render(s)}
                </#if>
            </div>
        </#list>
    </div>
</#list>
