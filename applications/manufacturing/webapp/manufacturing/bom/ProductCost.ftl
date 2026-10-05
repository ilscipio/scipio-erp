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

<#assign curIsoCode = parameters.currencyUomId!"USD">
<@section title=uiLabelMap.PageTitleProductCost>
  <#if product?has_content>
    <@row>
      <@cell columns=6>
        <@field type="display" label=uiLabelMap.ProductProductId>${productId!} - ${product.internalName!}</@field>
      </@cell>
      <@cell columns=6 class="+${styles.text_right!}">
        <a href="<@pageUrl>ProductCost?productId=${productId!}&amp;recalculate=Y</@pageUrl>" class="${styles.link_run_sys!} ${styles.action_update!}">${uiLabelMap.ManufacturingRecalculate}</a>
      </@cell>
    </@row>

    <@row>
      <@cell columns=3><@field type="display" label=uiLabelMap.CommonTotalCost><@ofbizCurrency amount=(totalCost!0) isoCode=curIsoCode/></@field></@cell>
      <@cell columns=3><@field type="display" label=uiLabelMap.ManufacturingMaterialCost><@ofbizCurrency amount=(materialCost!0) isoCode=curIsoCode/></@field></@cell>
      <@cell columns=3><@field type="display" label=uiLabelMap.ManufacturingLaborCost><@ofbizCurrency amount=(laborCost!0) isoCode=curIsoCode/></@field></@cell>
      <@cell columns=3><@field type="display" label=uiLabelMap.ManufacturingOverheadCost><@ofbizCurrency amount=(overheadCost!0) isoCode=curIsoCode/></@field></@cell>
    </@row>
    <@row>
      <@cell columns=3><@field type="display" label=uiLabelMap.ManufacturingRoutingCost><@ofbizCurrency amount=(routingCost!0) isoCode=curIsoCode/></@field></@cell>
      <@cell columns=3><@field type="display" label=uiLabelMap.ManufacturingLastCalculatedDate><#if lastCalculatedDate??>${lastCalculatedDate?string('yyyy-MM-dd HH:mm')}</#if></@field></@cell>
    </@row>

    <@section title=uiLabelMap.ManufacturingCostSources>
      <#if costComponents?has_content>
        <@table type="data-complex">
          <@tr>
            <@td><@field type="display" label=uiLabelMap.ManufacturingMaterialCost><@ofbizCurrency amount=(materialCost!0) isoCode=curIsoCode/></@field></@td>
            <@td>${uiLabelMap.ManufacturingCostSourceMaterialDesc}</@td>
            <@td align="right"><a href="#productComponents" class="${styles.link_nav_info_id!}">${uiLabelMap.ProductComponents}</a></@td>
          </@tr>
          <@tr>
            <@td><@field type="display" label=uiLabelMap.ManufacturingLaborCost><@ofbizCurrency amount=(laborCost!0) isoCode=curIsoCode/></@field></@td>
            <@td>${uiLabelMap.ManufacturingCostSourceLaborMachineDesc}</@td>
            <@td align="right">
              <#if productRouting?has_content>
                <a href="<@pageUrl>EditRouting?workEffortId=${productRouting.workEffortId!}</@pageUrl>" class="${styles.link_nav_info_id!}">${uiLabelMap.ManufacturingRoutingTasks}</a>
              </#if>
            </@td>
          </@tr>
          <@tr>
            <@td><@field type="display" label=uiLabelMap.ManufacturingOverheadCost><@ofbizCurrency amount=(overheadCost!0) isoCode=curIsoCode/></@field></@td>
            <@td>${uiLabelMap.ManufacturingCostSourceOverheadDesc}</@td>
            <@td align="right"><a href="<@pageUrl>EditCostCalcs</@pageUrl>" class="${styles.link_nav_info_id!}">${uiLabelMap.PageTitleEditCostCalcs}</a></@td>
          </@tr>
          <@tr>
            <@td><@field type="display" label=uiLabelMap.ManufacturingRoutingCost><@ofbizCurrency amount=(routingCost!0) isoCode=curIsoCode/></@field></@td>
            <@td>${uiLabelMap.ManufacturingCostSourceRoutingDesc}</@td>
            <@td align="right"><a href="<@pageUrl>FindRouting?productId=${productId!}</@pageUrl>" class="${styles.link_nav_info_id!}">${uiLabelMap.PageTitleFindRouting}</a></@td>
          </@tr>
        </@table>
      <#else>
        <@alert type="info">
          <p>${uiLabelMap.ManufacturingCostSetupHintTitle}</p>
          <ol>
            <li><a href="#productComponents">${uiLabelMap.ManufacturingCostSetupHintStep1}</a></li>
            <li>
              <#if productRouting?has_content>
                <a href="<@pageUrl>EditRouting?workEffortId=${productRouting.workEffortId!}</@pageUrl>">${uiLabelMap.ManufacturingCostSetupHintStep2}</a>
              <#else>
                <a href="<@pageUrl>FindRouting?productId=${productId!}</@pageUrl>">${uiLabelMap.ManufacturingCostSetupHintStep2}</a>
              </#if>
            </li>
            <li><a href="<@pageUrl>EditCostCalcs</@pageUrl>">${uiLabelMap.ManufacturingCostSetupHintStep3}</a></li>
            <li>${uiLabelMap.ManufacturingCostSetupHintStep4}</li>
          </ol>
        </@alert>
      </#if>
    </@section>

    <@section title=uiLabelMap.ManufacturingCostComponents>
      <#if costComponents?has_content>
        <@table type="data-list" autoAltRows=true>
          <@thead>
            <@tr class="header-row">
              <@th>${uiLabelMap.CommonType}</@th>
              <@th>${uiLabelMap.CommonDescription}</@th>
              <@th align="right">${uiLabelMap.FormFieldTitle_cost}</@th>
            </@tr>
          </@thead>
          <@tbody>
            <#list costComponents as costComponent>
              <@tr>
                <@td>${costComponent.costComponentTypeId!}</@td>
                <@td>${costComponent.description!}</@td>
                <@td align="right"><@ofbizCurrency amount=(costComponent.cost!0) isoCode=curIsoCode/></@td>
              </@tr>
            </#list>
          </@tbody>
        </@table>
      <#else>
        <@commonMsg type="result-norecord"/>
      </#if>
    </@section>

    <@section title=uiLabelMap.ProductComponents id="productComponents">
      <#if components?has_content>
        <@table type="data-list" autoAltRows=true>
          <@thead>
            <@tr class="header-row">
              <@th>${uiLabelMap.ProductProductId}</@th>
              <@th align="right">${uiLabelMap.CommonQuantity}</@th>
              <@th align="right">${uiLabelMap.ManufacturingScrapFactor}</@th>
              <@th align="right">${uiLabelMap.FormFieldTitle_cost}</@th>
              <@th align="right">${uiLabelMap.ManufacturingLineCost}</@th>
            </@tr>
          </@thead>
          <@tbody>
            <#list components as comp>
              <@tr>
                <@td><a href="<@pageUrl>ProductCost?productId=${comp.productId!}</@pageUrl>" class="${styles.link_nav_info_id!}">${comp.productId!}</a>&nbsp;${comp.internalName!}</@td>
                <@td align="right">${comp.quantity!}</@td>
                <@td align="right">${comp.scrapFactor!}</@td>
                <@td align="right"><@ofbizCurrency amount=(comp.unitCost!0) isoCode=curIsoCode/></@td>
                <@td align="right"><@ofbizCurrency amount=(comp.lineCost!0) isoCode=curIsoCode/></@td>
              </@tr>
            </#list>
          </@tbody>
        </@table>
      <#else>
        <@commonMsg type="result-norecord"/>
      </#if>
    </@section>
  <#else>
    <@commonMsg type="result-norecord"/>
  </#if>
</@section>
