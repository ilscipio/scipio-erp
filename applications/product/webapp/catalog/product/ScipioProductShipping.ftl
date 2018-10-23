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

        <#-- quantity and shipping -->
        <#if product.quantityIncluded?has_content>
            <@tr>
              <@td class="${styles.grid_large!}2">${uiLabelMap.ProductQuantityIncluded}
              </@td>
                <@td colspan="3">${product.quantityIncluded!""}
                                 <#if product.quantityUomId?has_content>
                                    <#assign measurementUom = product.getRelatedOne("QuantityUom", true)/>
                                    ${(measurementUom.get("abbreviation",locale))!}
                                 </#if>
                </@td>
            </@tr>
        </#if>

        <#if product.piecesIncluded?has_content>
            <@tr>
              <@td class="${styles.grid_large!}2">${uiLabelMap.ProductPiecesIncluded}
              </@td>
                <@td colspan="3">${product.piecesIncluded!""}
                </@td>
            </@tr>
        </#if>

        <#if product.inShippingBox?has_content && product.inShippingBox=="Y">   
            <@tr>
              <@td class="${styles.grid_large!}2">${uiLabelMap.ProductShippingBox}
              </@td>
              <@td colspan="3"><#if product.inShippingBox=="Y">${uiLabelMap.CommonYes}<#else>${uiLabelMap.CommonNo}</#if></@td>
            </@tr>
        </#if>

        <#if product.defaultShipmentBoxTypeId?has_content>
            <#assign productAttrDescription = product.getRelatedOne("DefaultShipmentBoxType", true) />
            <@tr>
              <@td class="${styles.grid_large!}2">${uiLabelMap.ProductDefaultShipmentBoxTypeId}
              </@td>
              <@td colspan="3">${(productAttrDescription.get("description",locale))?default(product.defaultShipmentBoxTypeId)!}</@td>
            </@tr>
        </#if>

        <#if product.chargeShipping?has_content && product.chargeShipping=="Y">   
            <@tr>
              <@td class="${styles.grid_large!}2">${uiLabelMap.ProductChargeShipping}
              </@td>
              <@td colspan="3"><#if product.chargeShipping=="Y">${uiLabelMap.CommonYes}<#else>${uiLabelMap.CommonNo}</#if></@td>
            </@tr>
        </#if>
    </@table>


</@section>