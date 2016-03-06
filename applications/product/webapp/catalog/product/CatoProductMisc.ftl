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
        <#if product.returnable?has_content && product.returnable=="Y">   
            <@tr>
              <@td class="${styles.grid_large!}2">${uiLabelMap.ProductReturnable}
              </@td>
              <@td colspan="3"><#if product.returnable=="Y">${uiLabelMap.CommonYes}<#else>${uiLabelMap.CommonNo}</#if></@td>
            </@tr>
        </#if>

        <#-- marketing -->
        <#if product.includeInPromotions?has_content && product.includeInPromotions=="Y">   
            <@tr>
              <@td class="${styles.grid_large!}2">${uiLabelMap.ProductIncludePromotions}
              </@td>
              <@td colspan="3"><#if product.includeInPromotions=="Y">${uiLabelMap.CommonYes}<#else>${uiLabelMap.CommonNo}</#if></@td>
            </@tr>
        </#if>

        <#--
        <#if product.contentInfoText?has_content && product.contentInfoText=="Y">   
            <@tr>
              <@td class="${styles.grid_large!}2">${uiLabelMap.ProductContentInfoText}
              </@td>
              <@td colspan="3"><#if product.contentInfoText=="Y">${uiLabelMap.CommonYes}<#else>${uiLabelMap.CommonNo}</#if></@td>
            </@tr>
        </#if>-->

        <#-- tax -->
        <#if product.taxable?has_content && product.taxable=="Y">   
            <@tr>
              <@td class="${styles.grid_large!}2">${uiLabelMap.ProductTaxable}
              </@td>
              <@td colspan="3"><#if product.taxable=="Y">${uiLabelMap.CommonYes}<#else>${uiLabelMap.CommonNo}</#if></@td>
            </@tr>
        </#if>
    </@table>


</@section>