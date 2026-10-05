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
<#-- SCIPIO: Printable A6 label sheet for a production run - one label for the run, one per task, each with a scannable code. -->
<#escape x as x?xml>
<fo:root xmlns:fo="http://www.w3.org/1999/XSL/Format">
    <fo:layout-master-set>
        <fo:simple-page-master master-name="label" page-width="105mm" page-height="148mm"
                                margin-top="5mm" margin-bottom="5mm" margin-left="5mm" margin-right="5mm">
            <fo:region-body/>
        </fo:simple-page-master>
    </fo:layout-master-set>

<#if productionRun??>
    <fo:page-sequence master-reference="label">
        <fo:flow flow-name="xsl-region-body" font-family="Helvetica">
            <fo:block font-size="14pt" font-weight="bold" text-align="center">${uiLabelMap.ManufacturingProductionRun}</fo:block>
            <fo:block font-size="12pt" text-align="center" space-after="3mm">${(productionRun.workEffortName)!(productionRunId)!} [${productionRunId!}]</fo:block>
            <#if product??>
                <fo:block font-size="10pt" text-align="center" space-after="3mm">${(product.internalName)!} [${(productId)!}]</fo:block>
            </#if>
            <fo:block text-align="center" space-after="3mm">
                <fo:instream-foreign-object>
                    <barcode:barcode xmlns:barcode="http://barcode4j.krysalis.org/ns" message="${runScanCode}">
                        <barcode:code128>
                            <barcode:height>20mm</barcode:height>
                            <barcode:module-width>0.4mm</barcode:module-width>
                        </barcode:code128>
                        <barcode:human-readable>
                            <barcode:placement>bottom</barcode:placement>
                            <barcode:font-name>Helvetica</barcode:font-name>
                            <barcode:font-size>7pt</barcode:font-size>
                        </barcode:human-readable>
                    </barcode:barcode>
                </fo:instream-foreign-object>
            </fo:block>
            <fo:block font-size="8pt" text-align="center">${runScanCode}</fo:block>
        </fo:flow>
    </fo:page-sequence>

    <#list tasks![] as task>
        <#assign taskScanCode = "PRUN:" + productionRunId + ":" + task.workEffortId>
        <fo:page-sequence master-reference="label">
            <fo:flow flow-name="xsl-region-body" font-family="Helvetica">
                <fo:block font-size="12pt" font-weight="bold" text-align="center">${(productionRun.workEffortName)!(productionRunId)!} [${productionRunId!}]</fo:block>
                <fo:block font-size="11pt" text-align="center" space-after="2mm">${(task.workEffortName)!} [${(task.workEffortId)!}]</fo:block>
                <#if task.fixedAssetId?? && (workCenterNames[task.fixedAssetId])??>
                    <fo:block font-size="9pt" text-align="center">${uiLabelMap.ManufacturingWorkCenter}: ${workCenterNames[task.fixedAssetId]!} [${task.fixedAssetId}]</fo:block>
                </#if>
                <#if product??>
                    <fo:block font-size="9pt" text-align="center" space-after="2mm">${(product.internalName)!} [${(productId)!}] - ${uiLabelMap.CommonQuantity}: ${(productionRun.quantityToProduce)!}</fo:block>
                </#if>
                <fo:block text-align="center" space-after="2mm">
                    <fo:instream-foreign-object>
                        <barcode:barcode xmlns:barcode="http://barcode4j.krysalis.org/ns" message="${taskScanCode}">
                            <barcode:code128>
                                <barcode:height>20mm</barcode:height>
                                <barcode:module-width>0.4mm</barcode:module-width>
                            </barcode:code128>
                            <barcode:human-readable>
                                <barcode:placement>bottom</barcode:placement>
                                <barcode:font-name>Helvetica</barcode:font-name>
                                <barcode:font-size>7pt</barcode:font-size>
                            </barcode:human-readable>
                        </barcode:barcode>
                    </fo:instream-foreign-object>
                </fo:block>
                <fo:block font-size="8pt" text-align="center">${taskScanCode}</fo:block>
            </fo:flow>
        </fo:page-sequence>
    </#list>
<#else>
    <fo:page-sequence master-reference="label">
        <fo:flow flow-name="xsl-region-body" font-family="Helvetica">
            <fo:block>${uiLabelMap.ManufacturingFabricationOrderProductionRunNotFound!} ${(productionRunId)!}</fo:block>
        </fo:flow>
    </fo:page-sequence>
</#if>
</fo:root>
</#escape>
