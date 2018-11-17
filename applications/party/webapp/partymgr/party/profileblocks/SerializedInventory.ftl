<#--
This file is subject to the terms and conditions defined in the
files 'LICENSE' and 'NOTICE', which are part of this source
code package.
-->

<@section id="serialized-inventory-summary" title=uiLabelMap.ProductSerializedInventorySummary>

        <@table type="data-list" class="+${styles.table_spacing_tiny_hint!}" id="serialized-inventory">
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

