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
<#-- SCIPIO: Manufacturing capacity planning dashboard. -->
<#assign dr = dashboardResult!{}>
<#assign runCounts = dr.runCounts!{}>
<#assign createdScheduled = (runCounts.PRUN_CREATED!0) + (runCounts.PRUN_SCHEDULED!0)>
<#assign lateRuns = dr.lateRuns![]>
<#assign runningTasks = dr.runningTasks![]>
<#assign upcomingRuns = dr.upcomingRuns![]>
<#assign shortages = dr.shortages![]>
<#assign mrpProposals = dr.mrpProposals!{}>
<#assign mrpProposalsTotal = (mrpProposals.productionRuns!0) + (mrpProposals.purchases!0)>
<#assign workCenterLoad = dr.workCenterLoad![]>
<#assign lastMrpRun = dr.lastMrpRun!false>

<@section title=uiLabelMap.ManufacturingDashboard>

    <#-- KPI tiles -->
    <div class="${styles.grid_row}">
        <div class="${styles.grid_large}3 ${styles.grid_cell}">
            <a href="<@pageUrl>FindProductionRun</@pageUrl>" class="${styles.link_nav}">
                <h2>${(runCounts.total)!0}</h2>
                <div>${uiLabelMap.CommonTotal} ${uiLabelMap.ManufacturingProductionRuns}</div>
            </a>
        </div>
        <div class="${styles.grid_large}3 ${styles.grid_cell}">
            <a href="<@pageUrl>FindProductionRun</@pageUrl>" class="${styles.link_nav}">
                <h2>${createdScheduled}</h2>
                <div>${uiLabelMap.ManufacturingCreatedAndScheduled}</div>
            </a>
        </div>
        <div class="${styles.grid_large}3 ${styles.grid_cell}">
            <a href="<@pageUrl>FindProductionRun</@pageUrl>" class="${styles.link_nav}">
                <h2 class="${styles.text_color_info}">${(runCounts.PRUN_RUNNING)!0}</h2>
                <div>${uiLabelMap.ManufacturingRunning}</div>
            </a>
        </div>
        <div class="${styles.grid_large}3 ${styles.grid_cell}">
            <a href="<@pageUrl>FindProductionRun</@pageUrl>" class="${styles.link_nav}">
                <h2<#if lateRuns?size gt 0> class="${styles.text_color_alert}"</#if>>${lateRuns?size}</h2>
                <div>${uiLabelMap.ManufacturingLateRuns}</div>
            </a>
        </div>
    </div>
    <div class="${styles.grid_row}">
        <div class="${styles.grid_large}4 ${styles.grid_cell}">
            <a href="<@pageUrl>ShopFloor</@pageUrl>" class="${styles.link_nav}">
                <h2>${runningTasks?size}</h2>
                <div>${uiLabelMap.ManufacturingRunningTasks}</div>
            </a>
        </div>
        <div class="${styles.grid_large}4 ${styles.grid_cell}">
            <a href="<@pageUrl>MrpProposals</@pageUrl>" class="${styles.link_nav}">
                <h2>${mrpProposalsTotal}</h2>
                <div>${uiLabelMap.ManufacturingMrpProposals} (${uiLabelMap.ManufacturingProductionRuns}: ${(mrpProposals.productionRuns)!0}, ${uiLabelMap.ManufacturingPurchases}: ${(mrpProposals.purchases)!0})</div>
            </a>
        </div>
        <div class="${styles.grid_large}4 ${styles.grid_cell}">
            <a href="<@pageUrl>Dashboard</@pageUrl>#shortages" class="${styles.link_nav}">
                <h2<#if shortages?size gt 0> class="${styles.text_color_warning}"</#if>>${shortages?size}</h2>
                <div>${uiLabelMap.ManufacturingShortages}</div>
            </a>
        </div>
    </div>

    <#-- Late / upcoming runs -->
    <div class="${styles.grid_row}">
        <div class="${styles.grid_large}6 ${styles.grid_cell}">
            <@section title=uiLabelMap.ManufacturingLateRuns>
                <#if lateRuns?has_content>
                    <@table type="data-list" role="grid">
                        <@thead>
                            <@tr class="header-row">
                                <@th>${uiLabelMap.CommonId}</@th>
                                <@th>${uiLabelMap.CommonProduct}</@th>
                                <@th>${uiLabelMap.CommonStatus}</@th>
                                <@th>${uiLabelMap.CommonDate}</@th>
                                <@th>${uiLabelMap.CommonQuantity}</@th>
                                <@th>${uiLabelMap.ManufacturingDaysLate}</@th>
                            </@tr>
                        </@thead>
                        <@tbody>
                            <#list lateRuns as run>
                                <@tr>
                                    <@td><a href="<@pageUrl>ShowProductionRun?productionRunId=${run.workEffortId!}</@pageUrl>" class="${styles.link_nav_info_id}">${run.workEffortName!run.workEffortId!}</a></@td>
                                    <@td>${run.productId!}</@td>
                                    <@td>${run.currentStatusId!}</@td>
                                    <@td>${run.estimatedCompletionDate?string('yyyy-MM-dd HH:mm')!}</@td>
                                    <@td>${run.quantityProduced!0}/${run.quantityToProduce!0}</@td>
                                    <@td class="${styles.text_color_alert}">${run.daysLate!0}</@td>
                                </@tr>
                            </#list>
                        </@tbody>
                    </@table>
                <#else>
                    <@commonMsg type="result-norecord"/>
                </#if>
            </@section>
        </div>
        <div class="${styles.grid_large}6 ${styles.grid_cell}">
            <@section title=uiLabelMap.ManufacturingUpcomingRuns>
                <#if upcomingRuns?has_content>
                    <@table type="data-list" role="grid">
                        <@thead>
                            <@tr class="header-row">
                                <@th>${uiLabelMap.CommonId}</@th>
                                <@th>${uiLabelMap.CommonProduct}</@th>
                                <@th>${uiLabelMap.CommonStatus}</@th>
                                <@th>${uiLabelMap.CommonDate}</@th>
                                <@th>${uiLabelMap.CommonQuantity}</@th>
                            </@tr>
                        </@thead>
                        <@tbody>
                            <#list upcomingRuns as run>
                                <@tr>
                                    <@td><a href="<@pageUrl>ShowProductionRun?productionRunId=${run.workEffortId!}</@pageUrl>" class="${styles.link_nav_info_id}">${run.workEffortName!run.workEffortId!}</a></@td>
                                    <@td>${run.productId!}</@td>
                                    <@td>${run.currentStatusId!}</@td>
                                    <@td>${run.estimatedStartDate?string('yyyy-MM-dd HH:mm')!}</@td>
                                    <@td>${run.quantityToProduce!0}</@td>
                                </@tr>
                            </#list>
                        </@tbody>
                    </@table>
                <#else>
                    <@commonMsg type="result-norecord"/>
                </#if>
            </@section>
        </div>
    </div>

    <#-- Running tasks -->
    <@section title=uiLabelMap.ManufacturingRunningTasks>
        <#if runningTasks?has_content>
            <@table type="data-list" role="grid">
                <@thead>
                    <@tr class="header-row">
                        <@th>${uiLabelMap.WorkEffortWorkEffort}</@th>
                        <@th>${uiLabelMap.ManufacturingWorkCenter}</@th>
                        <@th>${uiLabelMap.CommonDate}</@th>
                        <@th>${uiLabelMap.CommonQuantity}</@th>
                    </@tr>
                </@thead>
                <@tbody>
                    <#list runningTasks as task>
                        <@tr>
                            <@td>${task.workEffortName!task.workEffortId!}</@td>
                            <@td><a href="<@pageUrl>ShopFloor?fixedAssetId=${task.fixedAssetId!}</@pageUrl>" class="${styles.link_nav_info_id}">${task.fixedAssetId!}</a></@td>
                            <@td>${task.actualStartDate?string('yyyy-MM-dd HH:mm')!}</@td>
                            <@td>${task.quantityProduced!0} / ${task.quantityRejected!0}</@td>
                        </@tr>
                    </#list>
                </@tbody>
            </@table>
        <#else>
            <@commonMsg type="result-norecord"/>
        </#if>
    </@section>

    <#-- Shortages -->
    <a name="shortages"></a>
    <@section title=uiLabelMap.ManufacturingShortages>
        <#if shortages?has_content>
            <@table type="data-list" role="grid">
                <@thead>
                    <@tr class="header-row">
                        <@th>${uiLabelMap.CommonProduct}</@th>
                        <@th>${uiLabelMap.ProductFacility}</@th>
                        <@th>${uiLabelMap.CommonQuantity} (${uiLabelMap.CommonNew})</@th>
                        <@th>${uiLabelMap.CommonAvailable}</@th>
                        <@th>${uiLabelMap.ManufacturingShortages}</@th>
                    </@tr>
                </@thead>
                <@tbody>
                    <#list shortages as shortage>
                        <@tr>
                            <@td><a href="<@pageUrl>EditProductBom?productId=${shortage.productId!}</@pageUrl>" class="${styles.link_nav_info_id}">${shortage.internalName!shortage.productId!}</a></@td>
                            <@td>${shortage.facilityId!}</@td>
                            <@td>${shortage.quantityNeeded!0}</@td>
                            <@td>${shortage.quantityAvailable!0}</@td>
                            <@td class="${styles.text_color_warning}">${shortage.shortage!0}</@td>
                        </@tr>
                    </#list>
                </@tbody>
            </@table>
        <#else>
            <@commonMsg type="result-norecord"/>
        </#if>
    </@section>

    <#-- Work center load -->
    <@section title=uiLabelMap.ManufacturingWorkCenterLoad>
        <#if workCenterLoad?has_content>
            <@table type="data-list" role="grid">
                <@thead>
                    <@tr class="header-row">
                        <@th>${uiLabelMap.ManufacturingWorkCenter}</@th>
                        <@th>${uiLabelMap.ManufacturingCapacityMinutes}</@th>
                        <@th>${uiLabelMap.ManufacturingLoadMinutes}</@th>
                        <@th>${uiLabelMap.ManufacturingLoadPercent}</@th>
                    </@tr>
                </@thead>
                <@tbody>
                    <#list workCenterLoad as wc>
                        <#assign loadPct = (wc.loadPercent!0)?number>
                        <#assign barPct = loadPct?round>
                        <#if barPct gt 100><#assign barPct = 100></#if>
                        <@tr>
                            <@td>${wc.fixedAssetName!wc.fixedAssetId!}</@td>
                            <@td>${wc.totalCapacityMinutes!0}</@td>
                            <@td>${wc.totalLoadMinutes!0}</@td>
                            <@td>
                                <div class="${styles.table_data_list}">
                                    <div style="width:${barPct}%;" class="<#if loadPct gt 100>${styles.color_warning}<#elseif loadPct gt 80>${styles.color_info}<#else>${styles.color_success}</#if>">&nbsp;</div>
                                </div>
                                <span<#if loadPct gt 100> class="${styles.text_color_alert}"<#elseif loadPct gt 80> class="${styles.text_color_warning}"</#if>>${loadPct?round}%</span>
                            </@td>
                        </@tr>
                    </#list>
                </@tbody>
            </@table>
        <#else>
            <@commonMsg type="result-norecord"/>
        </#if>
    </@section>

    <#-- Last MRP run -->
    <@section title=uiLabelMap.ManufacturingLastMrpRun>
        <#if lastMrpRun?has_content>
            <@row>
                <@cell columns=3>${uiLabelMap.ManufacturingMrpName}: ${lastMrpRun.mrpName!}</@cell>
                <@cell columns=3>${uiLabelMap.CommonStatus}: ${lastMrpRun.statusId!}</@cell>
                <@cell columns=3>${uiLabelMap.ManufacturingProductionRuns}: ${lastMrpRun.proposedProductionRuns!0}</@cell>
                <@cell columns=3>${uiLabelMap.ManufacturingPurchases}: ${lastMrpRun.proposedPurchases!0}</@cell>
            </@row>
            <a href="<@pageUrl>MrpRunDetail?mrpId=${lastMrpRun.mrpId!}</@pageUrl>" class="${styles.link_nav_info_id}">${uiLabelMap.CommonView}</a>
        <#else>
            <@commonMsg type="result-norecord"/>
        </#if>
        <a href="<@pageUrl>RunMrp</@pageUrl>" class="${styles.link_run_sys} ${styles.action_begin}">${uiLabelMap.ManufacturingRunMrp}</a>
    </@section>

    <#-- Quick links -->
    <@section title=uiLabelMap.ManufacturingQuickLinks>
        <a href="<@pageUrl>CreateProductionRun</@pageUrl>" class="${styles.link_nav} ${styles.action_add}">${uiLabelMap.ManufacturingCreateProductionRun}</a>
        <a href="<@pageUrl>ShopFloor</@pageUrl>" class="${styles.link_nav} ${styles.action_view}">${uiLabelMap.ManufacturingRunningTasks}</a>
        <a href="<@pageUrl>WorkCenterLoad</@pageUrl>" class="${styles.link_nav} ${styles.action_view}">${uiLabelMap.ManufacturingWorkCenterLoad}</a>
        <a href="<@pageUrl>MrpRuns</@pageUrl>" class="${styles.link_nav} ${styles.action_view}">${uiLabelMap.ManufacturingMrpReports}</a>
    </@section>

</@section>
