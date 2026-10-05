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
<#-- SCIPIO: Manufacturing work center capacity and load planning view. -->
<#assign wr = wclResult!{}>
<#assign workCenters = wr.workCenters![]>
<#assign days = wr.days![]>
<#assign loadRows = wr.loadRows![]>
<#assign tasks = wr.tasks![]>

<@section title=uiLabelMap.ManufacturingWorkCenterLoad>

    <#-- Capacity / load matrix -->
    <#if workCenters?has_content && days?has_content>
        <@table type="data-list" role="grid">
            <@thead>
                <@tr class="header-row">
                    <@th>${uiLabelMap.ManufacturingWorkCenter}</@th>
                    <#list days as day>
                        <@th>${day?string('MM-dd')}</@th>
                    </#list>
                </@tr>
            </@thead>
            <@tbody>
                <#list workCenters as wc>
                    <@tr class=((wc.isLine!false)?then(styles.color_info!"", ""))>
                        <@td><#if wc.isLine!false><b>${uiLabelMap.ManufacturingProductionLine}:</b> </#if>${wc.fixedAssetName!wc.fixedAssetId!}</@td>
                        <#list days as day>
                            <#assign matchRow = false>
                            <#list loadRows as lr>
                                <#if lr.fixedAssetId == wc.fixedAssetId && lr.day == day>
                                    <#assign matchRow = lr>
                                </#if>
                            </#list>
                            <#if matchRow?is_hash>
                                <#assign pct = (matchRow.loadPercent!0)?number>
                                <#assign cellClass = (pct gt 100)?then(styles.text_color_alert!"", (pct gt 80)?then(styles.text_color_warning!"", ""))><@td class=cellClass>${matchRow.loadMinutes!0}/${matchRow.capacityMinutes!0} (${pct?round}%)</@td>
                            <#else>
                                <@td>-</@td>
                            </#if>
                        </#list>
                    </@tr>
                    <#if wc.isLine!false && wc.members?has_content>
                        <#list wc.members as member>
                            <@tr>
                                <@td class="${styles.text_indent!}">&nbsp;&nbsp;${member.fixedAssetName!member.fixedAssetId!}</@td>
                                <#list days as day>
                                    <@td>-</@td>
                                </#list>
                            </@tr>
                        </#list>
                    </#if>
                </#list>
            </@tbody>
        </@table>
    <#else>
        <@commonMsg type="result-norecord"/>
    </#if>

    <#-- Gantt -->
    <@section title=uiLabelMap.ManufacturingGantt>
        <#if days?has_content>
            <#assign rangeStartMs = days?first.time>
            <#assign rangeEndMs = days?last.time + 86400000>
            <#assign rangeSpanMs = rangeEndMs - rangeStartMs>
            <#list workCenters as wc>
                <div class="${styles.grid_row}">
                    <div class="${styles.grid_large}2 ${styles.grid_cell}"><#if wc.isLine!false>${uiLabelMap.ManufacturingProductionLine}: </#if>${wc.fixedAssetName!wc.fixedAssetId!}</div>
                    <div class="${styles.grid_large}10 ${styles.grid_cell}">
                        <div style="position:relative;height:2em;">
                            <#list tasks as task>
                                <#if task.workCenterFixedAssetId! == wc.fixedAssetId!>
                                    <#assign taskStartMs = (task.estimatedStartDate.time)!rangeStartMs>
                                    <#assign taskEndMs = (task.estimatedCompletionDate.time)!rangeEndMs>
                                    <#assign leftPct = ((taskStartMs - rangeStartMs) / rangeSpanMs * 100)?number>
                                    <#if leftPct < 0><#assign leftPct = 0></#if>
                                    <#assign widthPct = ((taskEndMs - taskStartMs) / rangeSpanMs * 100)?number>
                                    <#if (leftPct + widthPct) gt 100><#assign widthPct = 100 - leftPct></#if>
                                    <#if widthPct < 1><#assign widthPct = 1></#if>
                                    <a href="<@pageUrl>ShowProductionRun?productionRunId=${task.productionRunId!}</@pageUrl>"<#rt>
                                        <#lt> style="position:absolute;left:${leftPct?round}%;width:${widthPct?round}%;"<#rt>
                                        <#lt> class="${styles.link_nav} <#if task.currentStatusId! == 'PRUN_RUNNING'>${styles.color_info}<#else>${styles.color_success}</#if>"<#rt>
                                        <#lt> title="${task.productionRunName!task.workEffortName!}">${task.productionRunName!task.workEffortName!}</a>
                                </#if>
                            </#list>
                        </div>
                    </div>
                </div>
            </#list>
        </#if>
        <div>
            <span class="${styles.text_color_success}">&#9632;</span> ${uiLabelMap.ManufacturingLoadNormal}
            <span class="${styles.text_color_info}">&#9632;</span> ${uiLabelMap.ManufacturingRunning}
            <span class="${styles.text_color_warning}">&#9632;</span> ${uiLabelMap.ManufacturingLoadWarning}
            <span class="${styles.text_color_alert}">&#9632;</span> ${uiLabelMap.ManufacturingLoadOverload}
        </div>
    </@section>

    <#-- Task list -->
    <@section title=uiLabelMap.ManufacturingTaskList>
        <#if tasks?has_content>
            <@table type="data-list" role="grid">
                <@thead>
                    <@tr class="header-row">
                        <@th>${uiLabelMap.WorkEffortWorkEffort}</@th>
                        <@th>${uiLabelMap.ManufacturingWorkCenter}</@th>
                        <@th>${uiLabelMap.CommonProduct}</@th>
                        <@th>${uiLabelMap.CommonStatus}</@th>
                        <@th>${uiLabelMap.CommonFrom}</@th>
                        <@th>${uiLabelMap.CommonThru}</@th>
                        <@th>${uiLabelMap.ManufacturingLoadMinutes}</@th>
                        <@th>${uiLabelMap.CommonPriority}</@th>
                    </@tr>
                </@thead>
                <@tbody>
                    <#list tasks as task>
                        <@tr>
                            <@td><a href="<@pageUrl>ShowProductionRun?productionRunId=${task.productionRunId!}</@pageUrl>" class="${styles.link_nav_info_id}">${task.workEffortName!task.workEffortId!}</a></@td>
                            <@td>${task.fixedAssetId!}</@td>
                            <@td>${task.productId!}</@td>
                            <@td>${task.currentStatusId!}</@td>
                            <@td>${task.estimatedStartDate?string('yyyy-MM-dd HH:mm')!}</@td>
                            <@td>${task.estimatedCompletionDate?string('yyyy-MM-dd HH:mm')!}</@td>
                            <@td>${task.loadMinutes!0}</@td>
                            <@td>${task.priority!}</@td>
                        </@tr>
                    </#list>
                </@tbody>
            </@table>
        <#else>
            <@commonMsg type="result-norecord"/>
        </#if>
    </@section>

</@section>
