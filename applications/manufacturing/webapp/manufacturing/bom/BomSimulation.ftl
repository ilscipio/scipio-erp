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

<#if requestParameters.lookupFlag?default("N") == "Y">

<#if selectedFeatures?has_content>
  <#assign sectionTitle>${uiLabelMap.ManufacturingSelectedFeatures}</#assign>
  <#assign menuHtml>
    <@menu type="section" inlineItems=true></@menu>
  </#assign>
<#else>
  <#assign sectionTitle>${uiLabelMap.ManufacturingBomSimulation}</#assign>
  <#assign menuHtml>
    <@menu type="section" inlineItems=true></@menu>
  </#assign>
</#if>
  <@section title=sectionTitle menuHtml=menuHtml>
     <#if selectedFeatures?has_content>
       <#list selectedFeatures as selectedFeature>
         <p>${selectedFeature.productFeatureTypeId} = ${selectedFeature.description!} [${selectedFeature.productFeatureId}]</p>
       </#list>
     </#if>

      <@table type="data-list" autoAltRows=true class="${styles.table!}" cellspacing="0">
       <@thead>
        <@tr class="header-row">
          <@th width="10%">${uiLabelMap.ManufacturingProductLevel}</@th>
          <@th width="20%">${uiLabelMap.ProductProductId}</@th>
          <@th width="10%">${uiLabelMap.ManufacturingProductVirtual}</@th>
          <@th width="40%">${uiLabelMap.ProductProductName}</@th>
          <@th width="10%" align="right">${uiLabelMap.CommonQuantity}</@th>
          <@th width="10%" align="right">&nbsp;</@th>
        </@tr>
        </@thead>
        <#if tree?has_content>
          <#list tree as node>
            <@tr valign="middle">
              <@td>
              <@table type="generic" class="" cellspacing="1">
              <@tr>
              <@td>${node.depth}</@td>
              <#list 0..(node.depth) as level>
              <@td bgcolor="red">&nbsp;&nbsp;</@td>
              </#list>
              </@tr>
              </@table>
              </@td>
              <@td>
              <@table type="generic" class="" cellspacing="1">
              <@tr>
              <#list 0..(node.depth) as level>
              <@td>&nbsp;&nbsp;</@td>
              </#list>
              <@td>
                ${node.product.productId}
              </@td>
              </@tr>
              </@table>
              </@td>
              <@td>
                <#if node.product.isVirtual?default("N") == "Y">
                    ${node.product.isVirtual}
                </#if>
                ${(node.ruleApplied.ruleId)!}
              </@td>
              <@td>${node.product.internalName?default("&nbsp;")}</@td>
              <@td align="right">${node.quantity}</@td>
              <@td align="right"><a href="<@ofbizUrl>EditProductBom?productId=${(node.product.productId)!}&amp;productAssocTypeId=${(node.bomTypeId)!}</@ofbizUrl>" class="${styles.button_default!}">${uiLabelMap.CommonEdit}</a></@td>
            </@tr>
          </#list>
        <#else>
            <@tr type="meta">
              <@td colspan="6"><@resultMsg>${uiLabelMap.CommonNoElementFound}.</@resultMsg></@td>
            </@tr>
        </#if>
      </@table>
    
      <@table type="data-list" autoAltRows=true class="${styles.table!}" cellspacing="0">
       <@thead>
        <@tr class="header-row">
          <@th width="20%">${uiLabelMap.ProductProductId}</@th>
          <@th width="50%">${uiLabelMap.ProductProductName}</@th>
          <@th width="6%" align="right">${uiLabelMap.CommonQuantity}</@th>
          <@th width="6%" align="right">${uiLabelMap.ProductQoh}</@th>
          <@th width="6%" align="right">${uiLabelMap.ProductWeight}</@th>
          <@th width="6%" align="right">${uiLabelMap.FormFieldTitle_cost}</@th>
          <@th width="6%" align="right">${uiLabelMap.CommonTotalCost}</@th>
        </@tr>
        </@thead>
        <#if productsData?has_content>
          <#list productsData as productData>
            <#assign node = productData.node>
            <@tr valign="middle">
              <@td><a href="/catalog/control/EditProduct?productId=${node.product.productId}${StringUtil.wrapString(externalKeyParam)}" class="${styles.button_default!}">${node.product.productId}</a></@td>
              <@td>${node.product.internalName?default("&nbsp;")}</@td>
              <@td align="right">${node.quantity}</@td>
              <@td align="right">${productData.qoh!}</@td>
              <@td align="right">${node.product.productWeight!}</@td>
              <#if productData.unitCost?? && (productData.unitCost > 0)>
              <@td align="right">${productData.unitCost!}</@td>
              <#else>
              <@td align="right"><a href="/catalog/control/EditProductCosts?productId=${node.product.productId}${StringUtil.wrapString(externalKeyParam)}" class="${styles.button_default!}">NA</a></@td>
              </#if>
              <@td align="right">${productData.totalCost!}</@td>
            </@tr>
          </#list>
          <#--
          <#if grandTotalCost??>
          <@tr>
            <@td colspan="6" align="right">${grandTotalCost}</@td>
          </@tr>
          </#if>
          -->
        <#else>
          <@tr type="meta">
            <@td colspan="6"><@resultMsg>${uiLabelMap.CommonNoElementFound}.</@resultMsg></@td>
          </@tr>
        </#if>
      </@table>
  </@section>
</#if>
