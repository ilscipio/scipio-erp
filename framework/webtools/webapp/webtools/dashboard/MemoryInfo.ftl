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
<#assign chartType=chartType!"pie"/>    <#-- (line|bar|pie) default: line -->
<#assign library=chartLibrary!"chart"/>
<#assign datasets=(chartDatasets!1)?number />
<#assign memoryInfo=rewrapMap(memoryInfo!{}, "raw-simple")>

<#if memoryInfo?has_content>
    <@row><@cell columns=12>${uiLabelMap.WebtoolsMaxMemory}: ${maxMemoryMB} MB</@cell></@row>
    <@row><@cell columns=12>${uiLabelMap.WebtoolsTotalMemory}: ${totalMemoryMB} MB</@cell></@row>    
    <@row>
        <@cell columns=12>
            <#if chartType == "pie" || chartType == "bar">
                <@chart type=chartType library=library xlabel=(xlabel!"") ylabel=(ylabel!"") label1=(label1!"") label2=(label2!"") labelUom1="MB">
                    <#list mapKeys(memoryInfo) as key>        
                        <#assign currData = memoryInfo[key] />
                        <#if currData?has_content>
                            <#if datasets == 1>
                                <@chartdata value=(currData!0) title=key/>
                            </#if>                
                        </#if>
                    </#list>
                </@chart>
            <#elseif chartType == "line">
                <@commonMsg type="error">${uiLabelMap.CommonUnsupported}</@commonMsg>
            <#else>
                <@commonMsg type="error">${uiLabelMap.CommonUnsupported}</@commonMsg>
            </#if>
        </@cell>
    </@row>
<#else>
    <@commonMsg type="result-norecord"/>            
</#if>