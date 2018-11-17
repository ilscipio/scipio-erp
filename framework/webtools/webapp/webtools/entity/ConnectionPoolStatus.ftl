<#--
This file is subject to the terms and conditions defined in the
files 'LICENSE' and 'NOTICE', which are part of this source
code package.
-->
<@heading>Connection Pool Status</@heading>

<#assign groups = delegator.getModelGroupReader().getGroupNames(delegator.getDelegatorName())!/>
<@table type="data-list" autoAltRows=true class="+light-grid ${styles.table_spacing_tiny_hint!}">
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
