<#--
Licensed to the Apache Software Foundation (ASF) under one
or more contributor license agreements.  See the NOTICE file
distributed with this work for additional information
regarding copyright ownership.  The ASF licenses this file
to you under the Apache License, Version 2.0 (the
"License"); you may not use this file except in compliance
with the License.  You may obtain a copy of the License at

http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing,
software distributed under the License is distributed on an
"AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
KIND, either express or implied.  See the License for the
specific language governing permissions and limitations
under the License.
-->
<#assign chartType=chartType!"bar"/>    <#-- (line|bar|pie) default: line -->
<#assign chartValue=chartValue!"total"/> <#-- (total|count) default: total -->
<#assign chartData=chartData!"week"/>
<#assign library=chartLibrary!"foundation"/>
<#assign chartDataMap={"day":dailyStats,"week":weeklyStats,"month":monthlyStats}/>
<#assign currData=chartDataMap[chartData]/>
<#assign fieldIdNum=fieldIdNum!0/>
<#-- OrderOrdersTotals -->

<#if title?has_content><@heading relLevel=1>${title!}</@heading></#if>
<#if ((currData.isEmpty())!true) == false>
  <@chart type=chartType library=library>
    <#list currData.keySet() as key>
      <#if chartType=="bar" || chartType=="line">
        <@chartdata value="${(currData[key][chartValue])!0}" value2="${(currData[key].pos)!0}" title="${key}"/>
      <#else>
        <@chartdata value="${(currData[key][chartValue])!0}" title="${key}"/>
      </#if>
    </#list>  
  </@chart>
<#else>
  <@resultMsg>${uiLabelMap.CommonNoRecordFound}.</@resultMsg>
</#if>
        