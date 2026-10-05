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
<#-- SCIPIO: Barcode/QR scan screen - start, complete, or declare output of a production run task by scanning its code. -->

<p>${uiLabelMap.ManufacturingScanCodeHelp}</p>

<#if scanResult??>
    <@section title=uiLabelMap.ManufacturingProductionRun>
        <@row>
            <@cell columns=4><b>${uiLabelMap.ManufacturingProductionRunId}:</b> ${(scanResult.productionRunId)!}</@cell>
            <@cell columns=4><b>${uiLabelMap.ManufacturingTaskName}:</b> ${(scanResult.taskName)!} [${(scanResult.workEffortId)!}]</@cell>
            <@cell columns=4><b>${uiLabelMap.CommonStatus}:</b> ${(scanResult.statusId)!}</@cell>
        </@row>
        <@row>
            <@cell columns=6><b>${uiLabelMap.ProductProduct}:</b> ${(scanResult.productName)!} [${(scanResult.productId)!}]</@cell>
        </@row>
    </@section>
</#if>

<#if lastScans?has_content>
    <@section title=uiLabelMap.ManufacturingLastScans>
        <@table type="data-list" role="grid" autoAltRows=true>
            <@thead>
                <@tr valign="bottom" class="header-row">
                    <@th>${uiLabelMap.ManufacturingScanDate}</@th>
                    <@th>${uiLabelMap.ManufacturingScanAction}</@th>
                    <@th>${uiLabelMap.ManufacturingTaskName}</@th>
                    <@th>${uiLabelMap.CommonQuantity}</@th>
                    <@th>${uiLabelMap.ManufacturingLot}</@th>
                    <@th>${uiLabelMap.ManufacturingUser}</@th>
                    <@th>${uiLabelMap.ManufacturingComments}</@th>
                </@tr>
            </@thead>
            <@tbody>
                <#list lastScans as scan>
                    <@tr>
                        <@td>${(scan.scanDate)!}</@td>
                        <@td>${(scan.scanAction)!}</@td>
                        <@td>${(scan.workEffortId)!}</@td>
                        <@td>${(scan.quantity)!}</@td>
                        <@td>${(scan.lotId)!}</@td>
                        <@td>${(scan.userLoginId)!}</@td>
                        <@td>${(scan.comments)!}</@td>
                    </@tr>
                </#list>
            </@tbody>
        </@table>
    </@section>
</#if>
