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

<@section title=uiLabelMap.PageTitleProductWhereUsed>
  <#if product?has_content>
    <@row>
      <@cell columns=6>
        <@field type="display" label=uiLabelMap.ProductProductId>${productId!} - ${product.internalName!}</@field>
      </@cell>
      <@cell columns=6>
        <@field type="display" label=uiLabelMap.CommonQuantity>${count!0}</@field>
      </@cell>
    </@row>
    <#if whereUsed?has_content>
      <@table type="data-list" autoAltRows=true>
        <@thead>
          <@tr class="header-row">
            <@th>${uiLabelMap.ManufacturingProductLevel}</@th>
            <@th>${uiLabelMap.ProductProductId}</@th>
            <@th align="right">${uiLabelMap.CommonQuantity}</@th>
            <@th>${uiLabelMap.ManufacturingParentProduct}</@th>
          </@tr>
        </@thead>
        <@tbody>
          <#list whereUsed as node>
            <@tr>
              <@td style="padding-left:${((node.depth!0) * 1.5)?string('0.0')}em;">${node.depth!0}</@td>
              <@td>
                <a href="<@pageUrl>EditProductBom?productId=${node.productId!}</@pageUrl>" class="${styles.link_nav_info_id!}">${node.productId!}</a>&nbsp;${node.internalName!}
              </@td>
              <@td align="right">${node.quantity!}</@td>
              <@td>
                <#if node.parentProductId?has_content>
                  <a href="<@pageUrl>EditProductBom?productId=${node.parentProductId!}</@pageUrl>" class="${styles.link_nav_info_id!}">${node.parentProductId!}</a>
                </#if>
              </@td>
            </@tr>
          </#list>
        </@tbody>
      </@table>
    <#else>
      <@commonMsg type="result-norecord"/>
    </#if>
  <#else>
    <@commonMsg type="result-norecord"/>
  </#if>
</@section>
