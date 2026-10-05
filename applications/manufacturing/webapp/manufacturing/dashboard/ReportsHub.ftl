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
<#-- SCIPIO: Manufacturing reports hub - one small GET form per report. -->

<@section title=uiLabelMap.ManufacturingReports>

    <@section title=uiLabelMap.ManufacturingMrpReports>
        <@row>
            <@cell columns=4>
                <@form name="MRPPRunsProductsByFeatureForm" method="get" action=makePageUrl("MRPPRunsProductsByFeature")>
                    <@fields>
                        <@field type="text" name="mrpName" label=uiLabelMap.ManufacturingMrpName/>
                        <@field type="submit" submitType="submit" text=uiLabelMap.ManufacturingMrpPRunsProductsByFeature/>
                    </@fields>
                </@form>
            </@cell>
            <@cell columns=4>
                <@form name="MRPPRunsComponentsByFeatureForm" method="get" action=makePageUrl("MRPPRunsComponentsByFeature")>
                    <@fields>
                        <@field type="text" name="mrpName" label=uiLabelMap.ManufacturingMrpName/>
                        <@field type="submit" submitType="submit" text=uiLabelMap.ManufacturingMrpPRunsComponentsByFeature/>
                    </@fields>
                </@form>
            </@cell>
            <@cell columns=4>
                <@form name="PRunsProductsStacksForm" method="get" action=makePageUrl("PRunsProductsStacks")>
                    <@fields>
                        <@field type="text" name="mrpName" label=uiLabelMap.ManufacturingMrpName/>
                        <@field type="submit" submitType="submit" text=uiLabelMap.ManufacturingPRunsProductsStacks/>
                    </@fields>
                </@form>
            </@cell>
        </@row>
    </@section>

    <@section title=uiLabelMap.ManufacturingShipmentReports>
        <@row>
            <@cell columns=4>
                <@form name="CuttingListReportForm" method="get" action=makePageUrl("CuttingListReport")>
                    <@fields>
                        <@field type="text" name="shipmentId" label=uiLabelMap.ProductShipmentId/>
                        <@field type="submit" submitType="submit" text=uiLabelMap.ManufacturingCuttingListReport/>
                    </@fields>
                </@form>
            </@cell>
            <@cell columns=4>
                <@form name="SPPRunsProductsByFeatureForm" method="get" action=makePageUrl("SPPRunsProductsByFeature")>
                    <@fields>
                        <@field type="text" name="shipmentId" label=uiLabelMap.ProductShipmentId/>
                        <@field type="submit" submitType="submit" text=uiLabelMap.ManufacturingSpPRunsProductsByFeature/>
                    </@fields>
                </@form>
            </@cell>
            <@cell columns=4>
                <@form name="SPPRunsComponentsByFeatureForm" method="get" action=makePageUrl("SPPRunsComponentsByFeature")>
                    <@fields>
                        <@field type="text" name="shipmentId" label=uiLabelMap.ProductShipmentId/>
                        <@field type="submit" submitType="submit" text=uiLabelMap.ManufacturingSpPRunsComponentsByFeature/>
                    </@fields>
                </@form>
            </@cell>
        </@row>
        <@row>
            <@cell columns=4>
                <@form name="PackageContentsAndOrderForm" method="get" action=makePageUrl("PackageContentsAndOrder")>
                    <@fields>
                        <@field type="text" name="shipmentId" label=uiLabelMap.ProductShipmentId/>
                        <@field type="submit" submitType="submit" text=uiLabelMap.ManufacturingPackageContentsAndOrder/>
                    </@fields>
                </@form>
            </@cell>
            <@cell columns=4>
                <@form name="PRunsProductsAndOrderForm" method="get" action=makePageUrl("PRunsProductsAndOrder")>
                    <@fields>
                        <@field type="text" name="shipmentId" label=uiLabelMap.ProductShipmentId/>
                        <@field type="submit" submitType="submit" text=uiLabelMap.ManufacturingPRunsProductsAndOrder/>
                    </@fields>
                </@form>
            </@cell>
            <@cell columns=4>
                <@form name="PRunsInfoAndOrderForm" method="get" action=makePageUrl("PRunsInfoAndOrder")>
                    <@fields>
                        <@field type="text" name="shipmentId" label=uiLabelMap.ProductShipmentId/>
                        <@field type="submit" submitType="submit" text=uiLabelMap.ManufacturingPRunsInfoAndOrder/>
                    </@fields>
                </@form>
            </@cell>
        </@row>
        <@row>
            <@cell columns=4>
                <@form name="ShipmentPlanStockReportForm" method="get" action=makePageUrl("ShipmentPlanStockReport")>
                    <@fields>
                        <@field type="text" name="shipmentId" label=uiLabelMap.ProductShipmentId/>
                        <@field type="submit" submitType="submit" text=uiLabelMap.ManufacturingShipmentPlanStockReport/>
                    </@fields>
                </@form>
            </@cell>
            <@cell columns=4>
                <@form name="ShipmentLabelForm" method="get" action=makePageUrl("ShipmentLabel")>
                    <@fields>
                        <@field type="text" name="shipmentId" label=uiLabelMap.ProductShipmentId/>
                        <@field type="submit" submitType="submit" text=uiLabelMap.ProductShippingLabel/>
                    </@fields>
                </@form>
            </@cell>
            <@cell columns=4>
                <@form name="ShipmentWorkEffortTasksForm" method="get" action=makePageUrl("ShipmentWorkEffortTasks")>
                    <@fields>
                        <@field type="text" name="shipmentId" label=uiLabelMap.ProductShipmentId/>
                        <@field type="submit" submitType="submit" text=uiLabelMap.ManufacturingShipmentWorkEffortTasks/>
                    </@fields>
                </@form>
            </@cell>
        </@row>
    </@section>

    <@section title=uiLabelMap.ManufacturingProductionRunDocuments>
        <@row>
            <@cell columns=4>
                <@form name="PrintProductionRunForm" method="get" action=makePageUrl("PrintProductionRun")>
                    <@fields>
                        <@field type="text" name="productionRunId" label=uiLabelMap.ManufacturingProductionRunId/>
                        <@field type="submit" submitType="submit" text=uiLabelMap.ManufacturingPrintProductionRunDoc/>
                    </@fields>
                </@form>
            </@cell>
            <@cell columns=4>
                <@form name="ProductionRunLabelsPdfForm" method="get" action=makePageUrl("ProductionRunLabelsPdf")>
                    <@fields>
                        <@field type="text" name="productionRunId" label=uiLabelMap.ManufacturingProductionRunId/>
                        <@field type="submit" submitType="submit" text=uiLabelMap.ManufacturingTaskLabels/>
                    </@fields>
                </@form>
            </@cell>
        </@row>
    </@section>

</@section>
