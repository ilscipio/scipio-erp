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

<@section title=uiLabelMap.ManufacturingInputsConsumed>
<#if taskInfos?has_content>
<#list taskInfos as taskInfo>
  <#assign task = taskInfo.task>
  <@section title="${raw(task.workEffortName!)} [${raw(task.workEffortId)}]">
    <#if taskInfo.taskForm??>
    ${taskInfo.taskForm.renderFormString(context)}
    <#if taskInfo.replaceForm?? && taskInfo.inputRows?has_content>
    ${taskInfo.replaceForm.renderFormString(context)}
    </#if>
    <#else>
    <@commonMsg type="result-norecord"/>
    </#if>
  </@section>
</#list>
<#else>
    <@commonMsg type="result-norecord"/>
</#if>
</@section>

<@section title=uiLabelMap.ManufacturingOutputsProduced>
<#if outputs?has_content>
    <@table type="data-list" role="grid">
        <@thead>
            <@tr class="header-row">
                <@th>${uiLabelMap.ProductProductName}</@th>
                <@th>${uiLabelMap.ManufacturingTaskName}</@th>
                <@th>${uiLabelMap.ManufacturingPlannedQuantity}</@th>
                <@th>${uiLabelMap.ManufacturingQuantityProduced}</@th>
            </@tr>
        </@thead>
        <@tbody>
            <#list outputs as out>
                <@tr>
                    <@td>${out.internalName!out.productId!}</@td>
                    <@td><#if out.taskName?has_content>${out.taskName}<#else>${uiLabelMap.ManufacturingProductionRun}</#if></@td>
                    <@td>${out.plannedQuantity!0}</@td>
                    <@td>${out.producedQuantity!0}</@td>
                </@tr>
            </#list>
        </@tbody>
    </@table>
<#else>
    <@commonMsg type="result-norecord"/>
</#if>
</@section>
