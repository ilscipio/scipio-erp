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
<#assign chartType=chartType!"bar"/>    <#-- (line|bar|pie) default: line -->
<#assign library=chartLibrary!"chart"/>
<#assign datasets=(chartDatasets!1)?number />
<#if sales??>
  <#assign sales=rewrapMap(sales, "raw-simple")>
</#if>

<#if sales?has_content> 
    <#if chartType == "line" || chartType == "bar">        
        <@chart type=chartType title=chartTitle library=library xlabel=(xlabel!"") ylabel=(ylabel!"") label1=(label1!"") label2=(label2!"")>
            <#list sales as key, currData>
                <#if currData?has_content>
                   <@chartdata value=((currData.total)!0) value2=((currData.count)!0) title=currData.pos!/>
                </#if>
            </#list>
        </@chart>
    <#elseif chartType == "pie">
        <@commonMsg type="error">${uiLabelMap.CommonUnsupported}</@commonMsg>
    <#else>
        <@commonMsg type="error">${uiLabelMap.CommonUnsupported}</@commonMsg>
    </#if>
<#elseif sales??>
    <@commonMsg type="result-norecord"/>            
</#if>