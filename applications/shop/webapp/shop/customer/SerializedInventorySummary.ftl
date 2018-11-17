<#--
This file is subject to the terms and conditions defined in the
files 'LICENSE' and 'NOTICE', which are part of this source
code package.
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
                                <a href="<@ofbizUrl>product?product_id=${product.productId}</@ofbizUrl>" class="${styles.link_nav_info_name!}">${productName!product.productId}</a>
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

