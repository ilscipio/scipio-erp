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
<@heading>Connection Pool Status</@heading>

<#assign groups = delegator.getModelGroupReader().getGroupNames(delegator.getDelegatorName())!/>
<@table type="data-list" autoAltRows=true class="+light-grid ${styles.table_spacing_tiny_hint!}"> <#-- orig: class="basic-table light-grid hover-bar" --> <#-- orig: cellspacing="" -->
  <@thead>
    <@tr class="header-row">
        <@th>Helper Name</@th>
        <@th>Num Active</@th>
        <@th>Num Idle</@th>
        <@th>Num Total</@th>
        <@th>Max Active</@th>
        <@th>Max Idle</@th>
        <@th>Min Idle</@th>
        <@th>Min Evictable Idle Time</@th>
        <@th>Max Wait</@th>
    </@tr>
    </@thead>
    <#if (groups?has_content)>
        <#list groups as group>
            <#assign helper = delegator.getGroupHelperName(group)!/>
            <#if (helper?has_content)>
                <#assign dataSourceInfo = Static["org.ofbiz.entity.connection.DBCPConnectionFactory"].getDataSourceInfo(helper)!/>
                <#if (dataSourceInfo?has_content)>
                    <@tr>
                        <@td>${helper}</@td>
                        <@td>${dataSourceInfo.poolNumActive!}</@td>
                        <@td>${dataSourceInfo.poolNumIdle!}</@td>
                        <@td>${dataSourceInfo.poolNumTotal!}</@td>
                        <@td>${dataSourceInfo.poolMaxActive!}</@td>
                        <@td>${dataSourceInfo.poolMaxIdle!}</@td>
                        <@td>${dataSourceInfo.poolMinIdle!}</@td>
                        <@td>${dataSourceInfo.poolMinEvictableIdleTimeMillis!}</@td>
                        <@td>${dataSourceInfo.poolMaxWait!}</@td>
                    </@tr>
                </#if>
            </#if>
        </#list>
    </#if>
</@table>
