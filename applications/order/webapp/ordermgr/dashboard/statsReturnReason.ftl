<#--
This file is subject to the terms and conditions defined in the
files 'LICENSE' and 'NOTICE', which are part of this source
code package.
-->
<#assign chartType=chartType!"pie"/>    <#-- (line|bar|pie) default: line -->
<#assign chartData=chartData!"month"/>
<#assign library=chartLibrary!"chart"/>
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
        <#list currData as obj>
          <#assign reasonId = obj["returnReasonId"]/>
          <#assign returnReasonDescr = returnReasonMap[reasonId] />
          <@chartdata value=(obj["totalQuantity"]) title=returnReasonDescr/>
        </#list>
    </#if>
  </@chart>
</@section>