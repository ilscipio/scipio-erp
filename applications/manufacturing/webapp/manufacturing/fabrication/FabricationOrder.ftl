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
<#-- SCIPIO: Fabrication order totals panel: combined quantity, planned time, machines, and run statuses. -->
<#assign totals = fabOrderTotals!{}>
<#if totals?has_content>
    <div class="${styles.grid_row}">
        <div class="${styles.grid_large}3 ${styles.grid_cell}">
            <div>${uiLabelMap.ManufacturingRunCount}</div>
            <div><strong>${totals.runCount!0}</strong></div>
        </div>
        <div class="${styles.grid_large}3 ${styles.grid_cell}">
            <div>${uiLabelMap.ManufacturingTotalQuantity}</div>
            <div><strong>${totals.totalQuantity!0}</strong></div>
        </div>
        <div class="${styles.grid_large}3 ${styles.grid_cell}">
            <div>${uiLabelMap.ManufacturingPlannedMinutes}</div>
            <div><strong>${totals.plannedMinutes!0}</strong></div>
        </div>
        <div class="${styles.grid_large}3 ${styles.grid_cell}">
            <div>${uiLabelMap.CommonStatus}</div>
            <div><strong>${totals.derivedStatus!}</strong></div>
        </div>
    </div>
    <div class="${styles.grid_row}">
        <div class="${styles.grid_large}3 ${styles.grid_cell}">
            <div>${uiLabelMap.ManufacturingEarliestStart}</div>
            <div>${(totals.earliestStart)?string('yyyy-MM-dd HH:mm')!'-'}</div>
        </div>
        <div class="${styles.grid_large}3 ${styles.grid_cell}">
            <div>${uiLabelMap.ManufacturingLatestCompletion}</div>
            <div>${(totals.latestCompletion)?string('yyyy-MM-dd HH:mm')!'-'}</div>
        </div>
        <div class="${styles.grid_large}6 ${styles.grid_cell}">
            <div>${uiLabelMap.ManufacturingWorkCenters}</div>
            <div>
                <#assign workCenters = totals.workCenters![]>
                <#if workCenters?has_content>
                    <#list workCenters as wc>${wc}<#if wc_has_next>, </#if></#list>
                <#else>
                    -
                </#if>
            </div>
        </div>
    </div>
    <#assign statusCounts = totals.statusCounts!{}>
    <#if statusCounts?has_content>
        <div class="${styles.grid_row}">
            <div class="${styles.grid_large}12 ${styles.grid_cell}">
                <div>${uiLabelMap.CommonStatus}</div>
                <div>
                    <#list statusCounts.entrySet() as sc>${sc.key} (${sc.value})<#if sc_has_next>, </#if></#list>
                </div>
            </div>
        </div>
    </#if>
<#else>
    <@commonMsg type="result-norecord"/>
</#if>
