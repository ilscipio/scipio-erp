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
<#assign totalMap=rewrapMap(totalMap!{}, "raw-simple")>

<#if totalMap?has_content> 
    <#if chartType == "line" || chartType == "bar">
        <#-- FIXME: I don't know how to pass currency symbol, Freemarker doesn't let me get the symbol as is, either it transforms to unicode (ie: dollar) or to html entity (ie: euro)  -->
        <@chart type=chartType library=library xlabel=(xlabel!"") ylabel=(ylabel!"") label1=(label1!"") label2=(label2!"") labelUom1=(currencyUomId!"") labelUom2=(currencyUomId!"")>
            <#list mapKeys(totalMap) as key>        
                <#assign currData = totalMap[key] />
                <#if currData?has_content>
                    <#if datasets == 1>
                        <@chartdata value=(currData['income']!0) title=key/>
                    <#elseif datasets == 2>                    
                        <@chartdata value=(currData['income']!0) value2=(currData['expense']!0) title=key/>
                    </#if>
                </#if>
            </#list>
        </@chart>
    <#elseif chartType == "pie">
        <@commonMsg type="error">${uiLabelMap.CommonUnsupported}</@commonMsg>
    <#else>
        <@commonMsg type="error">${uiLabelMap.CommonUnsupported}</@commonMsg>
    </#if>
<#else>
    <@commonMsg type="result-norecord"/>            
</#if>
