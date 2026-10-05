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
<#-- SCIPIO: quick work center picker: production lines first (as "Line: name"), then individual machines. -->
<#assign workCenters = workCenters![]>
<#assign lineWorkCenters = []>
<#assign machineWorkCenters = []>
<#list workCenters as wc>
    <#if wc.isLine!false>
        <#assign lineWorkCenters = lineWorkCenters + [wc]>
    <#else>
        <#assign machineWorkCenters = machineWorkCenters + [wc]>
    </#if>
</#list>
<#assign selectedFixedAssetId = ((parameters.fixedAssetId)!"")?string>
<#assign selectedIsLine = false>
<#list lineWorkCenters as wc>
    <#if ((wc.fixedAssetId)!"")?string == selectedFixedAssetId>
        <#assign selectedIsLine = true>
    </#if>
</#list>

<#if workCenters?has_content>
    <@form method="get" action=makePageUrl("ShopFloor")>
        <@field type="hidden" name="facilityId" value=(parameters.facilityId)!/>
        <@field type="select" name="fixedAssetId" label=uiLabelMap.ManufacturingWorkCenter events={"onchange":"this.form.submit();"}>
            <@field type="option" value="" text=""/>
            <#list lineWorkCenters as wc>
                <@field type="option" value=(wc.fixedAssetId)!"" text="${uiLabelMap.ManufacturingProductionLine}: ${(wc.fixedAssetName)!(wc.fixedAssetId)!}" selected=(((wc.fixedAssetId)!"")?string == (selectedFixedAssetId!""))/>
            </#list>
            <#list machineWorkCenters as wc>
                <@field type="option" value=(wc.fixedAssetId)!"" text="${(wc.fixedAssetName)!(wc.fixedAssetId)!}" selected=(((wc.fixedAssetId)!"")?string == (selectedFixedAssetId!""))/>
            </#list>
        </@field>
    </@form>
</#if>

<#if selectedIsLine>
    <@commonMsg type="info">${uiLabelMap.ManufacturingLineHelp}</@commonMsg>
</#if>

<#if tasks?has_content>
    <#list tasks as task>
        <@section title="${(task.productionRunName)!(task.productionRunId)!} - ${(task.workEffortName)!(task.workEffortId)!}">
            <@row>
                <@cell columns=4><b>${uiLabelMap.ProductProduct}:</b> ${(task.productName)!} [${(task.productId)!}]</@cell>
                <@cell columns=4><b>${uiLabelMap.CommonStatus}:</b> ${(task.currentStatusId)!}</@cell>
                <@cell columns=4><b>${uiLabelMap.ManufacturingWorkCenter}:</b> ${(task.currentFixedAssetName)!} [${(task.currentFixedAssetId)!}]</@cell>
            </@row>
            <@row>
                <@cell columns=3><b>${uiLabelMap.ManufacturingQuantityToProduce}:</b> ${(task.quantityToProduce)!}</@cell>
                <@cell columns=3><b>${uiLabelMap.ManufacturingQuantityProduced}:</b> ${(task.quantityProduced)!}</@cell>
                <@cell columns=3><b>${uiLabelMap.ManufacturingQuantityRejected}:</b> ${(task.quantityRejected)!}</@cell>
                <@cell columns=3><b>${uiLabelMap.CommonQuantity} ${uiLabelMap.ManufacturingProductionRun}:</b> ${(task.runQuantityProduced)!}</@cell>
            </@row>
            <@row>
                <@cell columns=3><b>${uiLabelMap.ManufacturingPlannedMinutes}:</b> ${(task.estimatedMinutes)!}</@cell>
                <@cell columns=3><b>${uiLabelMap.ManufacturingActualMinutes}:</b> ${(task.actualMinutes)!}</@cell>
                <@cell columns=3><b>${uiLabelMap.ManufacturingEstimatedStartDate}:</b> ${(task.estimatedStartDate)!}</@cell>
                <@cell columns=3><b>${uiLabelMap.ManufacturingActualStartDateTime}:</b> ${(task.actualStartDate)!}</@cell>
            </@row>

            <#if task.machineOptions?has_content>
                <@form method="post" action=makePageUrl("shopFloorSwitchMachine")>
                    <@field type="hidden" name="workEffortId" value=(task.workEffortId)!/>
                    <@field type="hidden" name="viewFixedAssetId" value=(parameters.fixedAssetId)!/>
                    <@field type="hidden" name="facilityId" value=(parameters.facilityId)!/>
                    <@field type="select" name="fixedAssetId" label=uiLabelMap.ManufacturingMachine inlineLabel=true>
                        <#list task.machineOptions as opt>
                            <@field type="option" value=(opt.fixedAssetId)!"" text="${(opt.fixedAssetName)!(opt.fixedAssetId)!}" selected=(((opt.fixedAssetId)!"")?string == (task.currentFixedAssetId)!"")/>
                        </#list>
                    </@field>
                    <@field type="submit" text=uiLabelMap.ManufacturingSwitchMachine class="${styles.link_run_sys!} ${styles.action_update!}"/>
                </@form>
            </#if>

            <#if task.components?has_content>
                <@table type="data-list" role="grid" autoAltRows=true>
                    <@thead>
                        <@tr valign="bottom" class="header-row">
                            <@th>${uiLabelMap.ProductProduct}</@th>
                            <@th>${uiLabelMap.ManufacturingQuantityToProduce}</@th>
                            <@th>${uiLabelMap.ManufacturingIssuedQuantity}</@th>
                        </@tr>
                    </@thead>
                    <@tbody>
                        <#list task.components as component>
                            <@tr>
                                <@td>${(component.internalName)!} [${(component.productId)!}]</@td>
                                <@td>${(component.estimatedQuantity)!}</@td>
                                <@td>${(component.issuedQuantity)!}</@td>
                            </@tr>
                        </#list>
                    </@tbody>
                </@table>
            </#if>

            <#if task.canStart!false>
                <@form method="post" action=makePageUrl("shopFloorStartTask")>
                    <@field type="hidden" name="productionRunId" value=(task.productionRunId)!/>
                    <@field type="hidden" name="workEffortId" value=(task.workEffortId)!/>
                    <@field type="hidden" name="statusId" value="PRUN_RUNNING"/>
                    <@field type="hidden" name="fixedAssetId" value=(parameters.fixedAssetId)!/>
                    <@field type="hidden" name="facilityId" value=(parameters.facilityId)!/>
                    <@field type="submit" text=uiLabelMap.ManufacturingStartProductionRunTask class="${styles.link_run_sys!} ${styles.action_begin!}"/>
                </@form>
            </#if>

            <#if task.canDeclare!false>
                <@form method="post" action=makePageUrl("shopFloorDeclareTask")>
                    <@field type="hidden" name="productionRunId" value=(task.productionRunId)!/>
                    <@field type="hidden" name="workEffortId" value=(task.workEffortId)!/>
                    <@field type="hidden" name="fixedAssetId" value=(parameters.fixedAssetId)!/>
                    <@field type="hidden" name="facilityId" value=(parameters.facilityId)!/>
                    <@field type="input" name="quantityProduced" label=uiLabelMap.ManufacturingQuantityProduced size=8/>
                    <@field type="input" name="quantityRejected" label=uiLabelMap.ManufacturingQuantityRejected size=8/>
                    <@field type="select" name="reasonEnumId" label=uiLabelMap.ManufacturingRejectReason allowEmpty=true>
                        <@field type="option" value="" text=""/>
                        <#list rejectReasons![] as reason>
                            <@field type="option" value=(reason.enumId)! text=(reason.description)!(reason.enumId)!/>
                        </#list>
                    </@field>
                    <@field type="input" name="setupMinutes" label=uiLabelMap.ManufacturingSetupMinutes size=6/>
                    <@field type="input" name="taskMinutes" label=uiLabelMap.ManufacturingTaskMinutes size=6/>
                    <@field type="textarea" name="comments" label=uiLabelMap.ManufacturingComments rows=2/>
                    <@field type="checkbox" name="issueRequiredComponents" value="Y" label=uiLabelMap.ManufacturingIssueComponentsBackflush/>
                    <@field type="submit" text=uiLabelMap.ManufacturingDeclareProductionRunTask class="${styles.link_run_sys!} ${styles.action_update!}"/>
                </@form>
            </#if>

            <#if task.canComplete!false>
                <@form method="post" action=makePageUrl("shopFloorCompleteTask")>
                    <@field type="hidden" name="productionRunId" value=(task.productionRunId)!/>
                    <@field type="hidden" name="workEffortId" value=(task.workEffortId)!/>
                    <@field type="hidden" name="statusId" value="PRUN_COMPLETED"/>
                    <@field type="hidden" name="fixedAssetId" value=(parameters.fixedAssetId)!/>
                    <@field type="hidden" name="facilityId" value=(parameters.facilityId)!/>
                    <@field type="checkbox" name="issueAllComponents" value="Y" label=uiLabelMap.ManufacturingIssueComponentsBackflush/>
                    <@field type="submit" text=uiLabelMap.ManufacturingCompleteProductionRunTask class="${styles.link_run_sys!} ${styles.action_complete!}"/>
                </@form>
            </#if>
        </@section>
    </#list>
<#else>
    <@commonMsg type="result-norecord"/>
</#if>
