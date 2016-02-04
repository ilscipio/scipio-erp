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
<@section>
  <#if parameters.searchLabels??>
  <@table type="data-list" autoAltRows=true> <#-- orig: class="basic-table hover-bar" -->
    <@thead>
      <@tr class="header-row">
          <@th>${uiLabelMap.WebtoolsLabelManagerRow}</@th>
          <@th>${uiLabelMap.WebtoolsLabelManagerKey}</@th>
          <@th>${uiLabelMap.WebtoolsLabelManagerReferences}</@th>
      </@tr>
    </@thead>
    <#assign rowNumber = 1>
    <#list referencesList as reference>
      <#assign labelFound = 'N'>
      <#assign refNum = factory.getLabelReferenceFile(reference)>
      <#if (refNum > 0)>
        <@tr>
          <@td>${rowNumber}</@td>
          <@td>${reference}</@td>
          <@td align="center"><#if (refNum > 0)><a href="<@ofbizUrl>ViewReferences?sourceKey=${reference}</@ofbizUrl>">${refNum}</a><#else>${refNum}</#if></@td>
        </@tr>
        <#assign rowNumber = rowNumber + 1>
      </#if>
    </#list>
  </@table>
  </#if>
</@section>
