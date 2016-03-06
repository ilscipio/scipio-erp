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
        <#if product.requireAmount?has_content && product.requireAmount=="Y">   
            <@tr>
              <@td class="${styles.grid_large!}2">${uiLabelMap.ProductRequireAmount}
              </@td>
              <@td colspan="3"><#if product.requireAmount=="Y">${uiLabelMap.CommonYes}<#else>${uiLabelMap.CommonNo}</#if></@td>
            </@tr>
        </#if>

        <#if product.amountUomTypeId?has_content> 
            <#assign productAmount = product.getRelatedOne("AmountUomType", true) />
            <@tr>
              <@td class="${styles.grid_large!}2">${uiLabelMap.ProductAmountUomTypeId}
              </@td>
              <@td colspan="3">${(productAmount.get("description",locale))?default(product.amountUomTypeId)!}</@td>
            </@tr>
        </#if>
    </@table>


</@section>