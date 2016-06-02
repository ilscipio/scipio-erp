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
        <#-- inventory -->
        <#if product.salesDiscWhenNotAvail?has_content && product.salesDiscWhenNotAvail=="Y">
            <@tr>
              <@td class="${styles.grid_large!}2">${uiLabelMap.ProductSalesDiscontinuationNotAvailable}
              </@td>
              <@td colspan="3"><#if product.salesDiscWhenNotAvail=="Y">${uiLabelMap.CommonYes}<#else>${uiLabelMap.CommonNo}</#if></@td>
            </@tr>
        </#if>

        <#if product.requirementMethodEnumId?has_content>
            <#assign productRequirement = product.getRelatedOne("RequirementMethodEnumeration", true) />
            <@tr>
              <@td class="${styles.grid_large!}2">${uiLabelMap.ProductRequirementMethodEnumId}
              </@td>
              <@td colspan="3">${(productRequirement.get("description",locale))?default(product.requirementMethodEnumId)!}</@td>
            </@tr>
        </#if>

        <#if product.lotIdFilledIn?has_content>
            <@tr>
              <@td class="${styles.grid_large!}2">${uiLabelMap.ProductLotId}
              </@td>
              <@td colspan="3">${product.lotIdFilledIn!""}</@td>
            </@tr>
        </#if>

        <#if product.inventoryMessage?has_content>
            <@tr>
              <@td class="${styles.grid_large!}2">${uiLabelMap.ProductInventoryMessage}
              </@td>
              <@td colspan="3">${product.inventoryMessage!""}</@td>
            </@tr>
        </#if>
    </@table>


</@section>