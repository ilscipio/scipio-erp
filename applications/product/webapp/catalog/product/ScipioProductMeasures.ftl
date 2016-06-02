<#--
Licensed to the Apache Software Foundation (ASF) under one
or more contributor license agreements.  See the NOTICE file
distributed with this work for additional information
regarding copyright ownership.  The ASF licenses this file
to you under the Apache License, Version 2.0 (the
"License"); you may not use this file except in compliance
with the License.  You may obtain a copy of the License at

http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing,
software distributed under the License is distributed on an
"AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
KIND, either express or implied.  See the License for the
specific language governing permissions and limitations
under the License.
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