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
<#include 'calendarcommon.ftl'>

<#-- SCIPIO: FTL now includes the title -->
<@section title=uiLabelMap.WorkEffortUpComingEvents><#--title=uiLabelMap.WorkEffortCalendarUpComingEventsView-->

<#-- SCIPIO: have to pre-check if any content using this -->
  <#assign hasEvents = false>
  <#list days as day>
    <#assign workEfforts = day.calendarEntries>
    <#if workEfforts?has_content>
      <#assign hasEvents = true>
    </#if>
  </#list>

  <#if hasEvents>
    <@table type="data-list" autoAltRows="" responsive=true> <#-- orig: class="basic-table hover-bar" --> <#-- orig: cellspacing="0" -->
     <@thead>
      <@tr class="header-row">
        <@th width="20%">${uiLabelMap.CommonStartDateTime}</@th>
        <@th width="20%">${uiLabelMap.CommonEndDateTime}</@th>
        <@th width="15%">${uiLabelMap.CommonType}</@th>
        <@th width="45%">${uiLabelMap.WorkEffortName}</@th>
      </@tr>
      </@thead>
      <#list days as day>
        <#assign workEfforts = day.calendarEntries>
        <#if workEfforts?has_content>
          <#list workEfforts as calendarEntry>
            <#assign workEffort = calendarEntry.workEffort>
            <@tr>
              <@td><#if workEffort.actualStartDate??>${workEffort.actualStartDate}<#else>${workEffort.estimatedStartDate}</#if></@td>
              <@td><#if workEffort.actualCompletionDate??>${workEffort.actualCompletionDate}<#else>${workEffort.estimatedCompletionDate}</#if></@td>
              <@td>${workEffort.getRelatedOne("WorkEffortType", false).get("description",locale)}</@td>
              <@td class="button-col"><a href="<@ofbizUrl>EditWorkEffort?workEffortId=${workEffort.workEffortId}${addlParam!}</@ofbizUrl>">${workEffort.workEffortName}</a></@td>
            </@tr>
          </#list>
        </#if>
      </#list>
    </@table>
  <#else>
    <@commonMsg type="result-norecord">${uiLabelMap.WorkEffortNoEventsFound}.</@commonMsg>
  </#if>
</@section>