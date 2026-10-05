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
<#include "component://shop/webapp/shop/customer/customercommon.ftl">

<@section title=uiLabelMap.ProductSerializedInventorySummary id="serialized-inventory-summary">
    <@table type="data-list">
        <@thead>
            <@tr class="header-row">
                <@th>${uiLabelMap.ProductInventoryItemId}</@th>
                <@th>${uiLabelMap.ProductProductName}</@th>
                <@th>${uiLabelMap.ProductSerialNumber}</@th>
                <@th>${uiLabelMap.ProductSoftIdentifier}</@th>
                <@th>${uiLabelMap.ProductActivationNumber}</@th>
                <@th>${uiLabelMap.ProductActivationNumber} ${uiLabelMap.CommonValidThruDate}</@th>
            </@tr>
        </@thead>
        <@tbody>
            <#list inventoryItemList as inventoryItem>
                <#assign product = inventoryItem.getRelatedOne('Product', false)!>
                <@tr>
                    <@td>${inventoryItem.inventoryItemId}</@td>
                    <@td>
                        <#if product?has_content>
                            <#if (product.isVariant!"N") == "Y">
                                <#assign product = Static['org.ofbiz.product.product.ProductWorker'].getParentProduct(product.productId, delegator)!>
                            </#if>
                            <#if product?has_content>
                                <#assign productName = Static['org.ofbiz.product.product.ProductContentWrapper'].getProductContentAsText(product, 'PRODUCT_NAME', request, "raw")!>
                                <a href="<@pageUrl>product?product_id=${product.productId}</@pageUrl>" class="${styles.link_nav_info_name!}">${productName!product.productId}</a>
                            </#if>
                        </#if>
                    </@td>
                    <@td>${inventoryItem.serialNumber!}</@td>
                    <@td>${inventoryItem.softIdentifier!}</@td>
                    <@td>${inventoryItem.activationNumber!}</@td>
                    <@td>${inventoryItem.activationValidThru!}</@td>
                </@tr>
            </#list>
        </@tbody>
    </@table>
</@section>

