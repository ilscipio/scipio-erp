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
<#assign chartValue=chartValue!"total"/> <#-- (total|count) default: total -->
<#assign chartData=chartData!"week"/>
<#assign library=chartLibrary!"foundation"/>
<#assign xlabel=xlabel!""/>
<#assign ylabel=ylabel!""/>
<#assign label1=label1!""/>
<#assign label2=label2!""/>
<#-- <#assign chartDataMap={"day":dailyStats,"week":weeklyStats,"month":monthlyStats}/>
<#assign currData=chartDataMap[chartData]/> -->
<#assign currData=rewrapMap(orderStats!{}, "raw-simple")/>
<#assign fieldIdNum=fieldIdNum!0/>
<#-- OrderOrdersTotals -->

<@section title=title!"">
  <@chart type=chartType library=library xlabel=xlabel ylabel=ylabel label1=label1 label2=label2>
    <#if currData?has_content>
        <#list mapKeys(currData) as key>
          <#if chartType=="bar" || chartType=="line">
            <@chartdata value=((currData[key][chartValue])!0) value2=((currData[key].count)!0) title=key/>
          <#else>
            <@chartdata value=((currData[key][chartValue])!0) title=key/>
          </#if>
        </#list>
    </#if>  
  </@chart>
</@section>       