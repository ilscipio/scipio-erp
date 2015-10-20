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

<@table type="data-list" autoAltRows=true cellspacing="0" class="+hover-bar"> <#-- orig: class="basic-table hover-bar" -->
  <@thead>
    <@tr class="header-row">
        <#assign firstRecord = records[0]!/>
        <#list columnNames as columnName>
        <#assign class><#if firstRecord?? && firstRecord[columnName]?default("")?is_number>align-text</#if></#assign>
        <@th class=class>
            ${columnName}
        </@th>
        </#list>
    </@tr>
  </@thead>
    <#list records as record>
    <@tr valign="middle">
        <#list columnNames as columnName>
        <#assign columnValue = record[columnName]?default("")>
        <#assign class><#if columnValue?is_number>align-text</#if></#assign>
        <@td class=class>
            ${columnValue}
        </@td>
        </#list>
    </@tr>
    </#list>
</@table>
