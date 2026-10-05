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

<@section>
    <@table type="fields">
        <#-- measurements -->
        <#if product.productHeight?has_content>
            <@tr>
              <@td class="${styles.grid_large!}2">${uiLabelMap.ProductProductHeight}
              </@td>
                <@td colspan="3">${product.productHeight!""}
                                 <#if product.heightUomId?has_content>
                                    <#assign measurementUom = product.getRelatedOne("HeightUom", true)/>
                                    ${(measurementUom.get("abbreviation",locale))!}
                                 </#if>
                </@td>
            </@tr>
        </#if>

        <#if product.productWidth?has_content>
            <@tr>
              <@td class="${styles.grid_large!}2">${uiLabelMap.ProductProductWidth}
              </@td>
                <@td colspan="3">${product.productWidth!""}
                                 <#if product.widthUomId?has_content>
                                    <#assign measurementUom = product.getRelatedOne("WidthUom", true)/>
                                    ${(measurementUom.get("abbreviation",locale))!}
                                 </#if>
                </@td>
            </@tr>
        </#if>

        <#if product.productDepth?has_content>
            <@tr>
              <@td class="${styles.grid_large!}2">${uiLabelMap.ProductProductDepth}
              </@td>
                <@td colspan="3">${product.productDepth!""}
                                 <#if product.depthUomId?has_content>
                                    <#assign measurementUom = product.getRelatedOne("DepthUom", true)/>
                                    ${(measurementUom.get("abbreviation",locale))!}
                                 </#if>
                </@td>
            </@tr>
        </#if>

        <#if product.productDiameter?has_content>
            <@tr>
              <@td class="${styles.grid_large!}2">${uiLabelMap.ProductProductDiameter}
              </@td>
                <@td colspan="3">${product.productDiameter!""}
                                 <#if product.diameterUomId?has_content>
                                    <#assign measurementUom = product.getRelatedOne("DiameterUom", true)/>
                                    ${(measurementUom.get("abbreviation",locale))!}
                                 </#if>
                </@td>
            </@tr>
        </#if>

        <#if product.productWeight?has_content>
            <@tr>
              <@td class="${styles.grid_large!}2">${uiLabelMap.ProductProductWeight}
              </@td>
                <@td colspan="3">${product.productWeight!""}
                                 <#if product.weightUomId?has_content>
                                    <#assign measurementUom = product.getRelatedOne("WeightUom", true)/>
                                    ${(measurementUom.get("abbreviation",locale))!}
                                 </#if>
                </@td>
            </@tr>
        </#if>

        <#-- Shipping info
        <#if product.shippingHeight?has_content>
            <@tr>
              <@td class="${styles.grid_large!}2">${uiLabelMap.ProductShippingHeight}
              </@td>
                <@td colspan="3">${product.shippingHeight!""}
                                 <#if product.heightUomId?has_content>
                                    <#assign measurementUom = product.getRelatedOne("HeightUom", true)/>
                                    ${(measurementUom.get("abbreviation",locale))!}
                                 </#if>
                </@td>
            </@tr>
        </#if>

        <#if product.shippingWidth?has_content>
            <@tr>
              <@td class="${styles.grid_large!}2">${uiLabelMap.ProductShippingWidth}
              </@td>
                <@td colspan="3">${product.shippingWidth!""}
                                 <#if product.widthUomId?has_content>
                                    <#assign measurementUom = product.getRelatedOne("WidthUom", true)/>
                                    ${(measurementUom.get("abbreviation",locale))!}
                                 </#if>
                </@td>
            </@tr>
        </#if>

        <#if product.shippingDepth?has_content>
            <@tr>
              <@td class="${styles.grid_large!}2">${uiLabelMap.ProductShippingDepth}
              </@td>
                <@td colspan="3">${product.shippingDepth!""}
                                 <#if product.depthUomId?has_content>
                                    <#assign measurementUom = product.getRelatedOne("DepthUom", true)/>
                                    ${(measurementUom.get("abbreviation",locale))!}
                                 </#if>
                </@td>
            </@tr>
        </#if>


        <#if product.shippingDiameter?has_content>
            <@tr>
              <@td class="${styles.grid_large!}2">${uiLabelMap.ProductShippingDiameter}
              </@td>
                <@td colspan="3">${product.shippingDiameter!""}
                                 <#if product.diameterUomId?has_content>
                                    <#assign measurementUom = product.getRelatedOne("DiameterUom", true)/>
                                    ${(measurementUom.get("abbreviation",locale))!}
                                 </#if>
                </@td>
            </@tr>
        </#if>

        <#if product.shippingWeight?has_content>
            <@tr>
              <@td class="${styles.grid_large!}2">${uiLabelMap.ProductShippingWeight}
              </@td>
                <@td colspan="3">${product.shippingWeight!""}
                                 <#if product.weightUomId?has_content>
                                    <#assign measurementUom = product.getRelatedOne("WeightUom", true)/>
                                    ${(measurementUom.get("abbreviation",locale))!}
                                 </#if>
                </@td>
            </@tr>
        </#if>
        -->

    </@table>


</@section>