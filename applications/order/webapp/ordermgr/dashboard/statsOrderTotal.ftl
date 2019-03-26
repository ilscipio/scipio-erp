<#--
This file is subject to the terms and conditions defined in the
files 'LICENSE' and 'NOTICE', which are part of this source
code package.
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