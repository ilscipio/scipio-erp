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

<@section title=uiLabelMap.ManufacturingReservedLots>
<p>${uiLabelMap.ManufacturingReservationHelp}</p>
<#if reservationsForm??>
${reservationsForm.renderFormString(context)}
<#elseif reservations?has_content>
    <@table type="data-list" role="grid">
        <@thead>
            <@tr class="header-row">
                <@th>${uiLabelMap.ManufacturingTaskName}</@th>
                <@th>${uiLabelMap.ManufacturingLot}</@th>
                <@th>${uiLabelMap.ProductProductName}</@th>
                <@th>${uiLabelMap.ProductInventoryItemId}</@th>
                <@th>${uiLabelMap.ManufacturingReservedQuantity}</@th>
                <@th>${uiLabelMap.ManufacturingReservedDate}</@th>
            </@tr>
        </@thead>
        <@tbody>
            <#list reservations as reservation>
                <@tr>
                    <@td>${reservation.taskName!reservation.workEffortId!}</@td>
                    <@td>${reservation.lotId!}</@td>
                    <@td>${reservation.internalName!reservation.productId!}</@td>
                    <@td>${reservation.inventoryItemId!}</@td>
                    <@td>${reservation.quantityReserved!0} ${reservation.uomAbbreviation!}</@td>
                    <@td>${reservation.reservedDate!}</@td>
                </@tr>
            </#list>
        </@tbody>
    </@table>
<#else>
    <@commonMsg type="result-norecord"/>
</#if>
</@section>

<@section title=uiLabelMap.ManufacturingReserveALot>
<#if reserveLotForm??>
${reserveLotForm.renderFormString(context)}
</#if>
</@section>
