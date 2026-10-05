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
<#-- SCIPIO: Renders the product BOM structure (component explosion) set by EditProductBom.groovy as "productBomTree". -->
<#if productBomTree?has_content>
  <@table type="data-list" autoAltRows=true>
    <@thead>
      <@tr class="header-row">
        <@th width="10%">${uiLabelMap.ManufacturingProductLevel}</@th>
        <@th width="40%">${uiLabelMap.ProductProductId}</@th>
        <@th width="40%">${uiLabelMap.ProductProductName}</@th>
        <@th width="10%" align="right">${uiLabelMap.CommonQuantity}</@th>
      </@tr>
    </@thead>
    <@tbody>
      <#list productBomTree as node>
        <@tr valign="middle">
          <@td>${node.depth}</@td>
          <@td style="padding-left:${((node.depth!0) * 1.5)?string('0.0')}em;">
            <a href="<@pageUrl>EditProductBom?productId=${(node.product.productId)!}&amp;productAssocTypeId=${(node.bomTypeId)!}</@pageUrl>" class="${styles.link_nav_info_id!}">${node.product.productId}</a>
          </@td>
          <@td>${node.product.internalName?default("&nbsp;")}</@td>
          <@td align="right">${node.quantity}</@td>
        </@tr>
      </#list>
    </@tbody>
  </@table>
<#else>
  <p>${uiLabelMap.ManufacturingBomStructureNoComponents}</p>
</#if>
