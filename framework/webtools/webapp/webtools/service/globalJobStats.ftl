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
<@section title=title>
<@table id="global-job-stats-${jobType}" type="data-list" responsive=true>
    <@thead>
        <@tr>
            <@th>serviceName</@th>
            <@th width="8%">totalCalls</@th>
            <@th width="8%">totalRuntime</@th>
            <@th width="8%">minRuntime</@th>
            <@th width="8%">maxRuntime</@th>
            <@th width="8%">averageRuntime</@th>
            <@th width="8%">successCount</@th>
            <@th width="8%">failCount</@th>
            <@th width="8%">errorCount</@th>
            <@th width="8%">exceptionCount</@th>
        </@tr>
    </@thead>
    <@tbody>
        <#if jobList?has_content>
            <#list (jobList!) as job>
                <@tr>
                    <@td><#if job.serviceName?has_content><a href="<@pageUrl uri='ServiceList?sel_service_name='+raw(job.serviceName)/>">${job.serviceName}</a></#if></@td>
                    <@td width="8%">${job.totalCalls!}</@td>
                    <@td width="8%"><#if job.totalRuntime??>${UtilDateTime.formatDurationHMS(job.totalRuntime)}</#if></@td>
                    <@td width="8%"><#if job.minRuntime??>${UtilDateTime.formatDurationHMS(job.minRuntime)}</#if></@td>
                    <@td width="8%"><#if job.maxRuntime??>${UtilDateTime.formatDurationHMS(job.maxRuntime)}</#if></@td>
                    <@td width="8%"><#if job.averageRuntime??>${UtilDateTime.formatDurationHMS(job.averageRuntime)}</#if></@td>
                    <@td width="8%">${job.successCount!}</@td>
                    <@td width="8%">${job.failCount!}</@td>
                    <@td width="8%">${job.errorCount!}</@td>
                    <@td width="8%">${job.exceptionCount!}</@td>
                </@tr>
            </#list>
        <#--<#else> let datatables do it or it crashes
            <@tr><@td colspan="10">${getLabel('CommonNone', 'CommonUiLabels')}</@td></@tr>-->
        </#if>
    </@tbody>
</@table>
</@section>