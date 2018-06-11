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

<@section id="serialized-inventory-summary" title=uiLabelMap.ProductSerializedInventorySummary>

        <@table type="data-list" class="+${styles.table_spacing_tiny_hint!}" id="serialized-inventory"> <#-- orig: class="basic-table" --> <#-- orig: cellspacing="0" --> <#-- orig: cellpadding="2" -->
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
                        <@td><a href="<@ofbizInterWebappUrl>/facility/control/EditInventoryItem?inventoryItemId=${inventoryItem.inventoryItemId}&amp;externalLoginKey=${requestAttributes.externalLoginKey!}</@ofbizInterWebappUrl>" class="${styles.link_nav_info_id!}">${inventoryItem.inventoryItemId}</a></@td>
                        <@td>
                            <#if product?has_content>
                                <#if product.isVariant?default('N') == 'Y'>
                                    <#assign product = Static['org.ofbiz.product.product.ProductWorker'].getParentProduct(product.productId, delegator)!>
                                </#if>
                                <#if product?has_content>
                                    <#assign productName = Static['org.ofbiz.product.product.ProductContentWrapper'].getProductContentAsText(product, 'PRODUCT_NAME', request, "raw")!>
                                    <a href="<@ofbizInterWebappUrl>/catalog/control/ViewProduct?productId=${product.productId}&amp;externalLoginKey=${requestAttributes.externalLoginKey!}</@ofbizInterWebappUrl>">${productName!product.productId}</a>
                                </#if>
                            </#if>
                        </@td>
                        <@td>${inventoryItem.serialNumber!}</@td>
                        <@td>
                          ${inventoryItem.softIdentifier!}
                          <#if (inventoryItem.softIdentifier?has_content && inventoryItem.softIdentifier?matches("\\d+"))>
                            <#assign sid = Static["java.lang.Long"].decode(inventoryItem.softIdentifier)/>
                            (0x${Static["java.lang.Long"].toHexString(sid)})
                          </#if>
                        </@td>
                        <@td>${inventoryItem.activationNumber!}</@td>
                        <@td>${inventoryItem.activationValidThru!}</@td>
                    </@tr>
                </#list>
            </@tbody>
        </@table>

</@section>

