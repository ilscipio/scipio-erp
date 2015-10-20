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

  <#if days?has_content>
    <@table type="data-list" autoAltRows=true class="+hover-bar" cellspacing="0"> <#-- orig: class="basic-table hover-bar" -->
     <@thead>
      <@tr class="header-row">
        <@th>${uiLabelMap.CommonStartDateTime}</@th>
        <@th>${uiLabelMap.CommonEndDateTime}</@th>
        <@th>${uiLabelMap.CommonType}</@th>
        <@th>${uiLabelMap.WorkEffortName}</@th>
      </@tr>
      </@thead>
      <#list days as day>
        <#assign workEfforts = day.calendarEntries>
        <#if workEfforts?has_content>
          <@tr class="header-row"><@th colspan="4"><hr /></@th></@tr>
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
    <@resultMsg>${uiLabelMap.WorkEffortNoEventsFound}.</@resultMsg>
  </#if>