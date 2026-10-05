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
<#--
Changes to this file: Copyright (C) Ilscipio GmbH. The changes are licensed
under the GNU Affero General Public License, version 3, or a commercial
license from Ilscipio GmbH (file LICENSE). The original code stays under
the Apache License, version 2.0, as stated above.
-->

<#if requestParameters.lookupFlag?default("N") == "Y">

<#if selectedFeatures?has_content>
  <#assign sectionTitle = uiLabelMap.ManufacturingSelectedFeatures/>
  <#macro menuContent menuArgs={}>
    <@menu args=menuArgs></@menu>
  </#macro>
<#else>
  <#assign sectionTitle = uiLabelMap.ManufacturingBomSimulation/>
  <#macro menuContent menuArgs={}>
    <@menu args=menuArgs></@menu>
  </#macro>
</#if>
  <@section title=sectionTitle menuContent=menuContent>
     <#if selectedFeatures?has_content>
       <#list selectedFeatures as selectedFeature>
         <p>${selectedFeature.productFeatureTypeId} = ${selectedFeature.description!} [${selectedFeature.productFeatureId}]</p>
       </#list>
     </#if>
    <@section title=getLabel("ContentTree", "ContentUiLabels")>
    <#if tree?has_content>
      <@table type="data-list" autoAltRows=true>
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
        <@tbody>
          <#list tree as node>
            <@tr valign="middle">
              <@td>${node.depth}</@td>
              <#-- SCIPIO: 4.0.0: indent by padding; a nested table with one empty cell per level drew a box per level -->
              <@td style="padding-left:${((node.depth!0) * 1.25 + 0.75)?string('0.00')}em;">${node.product.productId}</@td>
              <@td>
                <#if node.product.isVirtual?default("N") == "Y">
                    ${node.product.isVirtual}
                </#if>
                ${(node.ruleApplied.ruleId)!}
              </@td>
              <@td>${node.product.internalName?default("&nbsp;")}</@td>
              <@td align="right">${node.quantity}</@td>
              <@td align="right"><a href="<@pageUrl>EditProductBom?productId=${(node.product.productId)!}&amp;productAssocTypeId=${(node.bomTypeId)!}</@pageUrl>" class="${styles.link_nav_info!}">${uiLabelMap.CommonEdit}</a></@td>
            </@tr>
          </#list>
        </@tbody>
      </@table>
    <#else>
      <@commonMsg type="result-norecord">${uiLabelMap.CommonNoElementFound}.</@commonMsg>
    </#if>
    </@section>
    
    <@section title=uiLabelMap.ProductProducts>
    <#if productsData?has_content>
      <@table type="data-list" autoAltRows=true>
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
        <@tbody>
          <#list productsData as productData>
            <#assign node = productData.node>
            <@tr valign="middle">
              <@td><a href="<@serverUrl>/catalog/control/ViewProduct?productId=${node.product.productId}${raw(externalKeyParam)}</@serverUrl>" class="${styles.link_nav_info_id!} ${styles.action_update!}">${node.product.productId}</a></@td>
              <@td>${node.product.internalName?default("&nbsp;")}</@td>
              <@td align="right">${node.quantity}</@td>
              <@td align="right">${productData.qoh!}</@td>
              <@td align="right">${node.product.productWeight!}</@td>
              <#if productData.unitCost?? && (productData.unitCost > 0)>
              <@td align="right">${productData.unitCost!}</@td>
              <#else>
              <@td align="right"><a href="<@serverUrl>/catalog/control/EditProductCosts?productId=${node.product.productId}${raw(externalKeyParam)}</@serverUrl>" class="${styles.link_nav_info!}">NA</a></@td>
              </#if>
              <@td align="right">${productData.totalCost!}</@td>
            </@tr>
          </#list>
        </@tbody>
        <@tfoot>
          <#if grandTotalCost??>
          <@tr>
            <@td colspan="6" align="right"><b>${uiLabelMap.CommonTotalCost}</b></@td>
            <@td align="right"><b>${grandTotalCost}</b></@td>
          </@tr>
          </#if>
        </@tfoot>
      </@table>
    <#else>
      <@commonMsg type="result-norecord">${uiLabelMap.CommonNoElementFound}.</@commonMsg>
    </#if>
    </@section>

  </@section>
</#if>
