<#--
This file is subject to the terms and conditions defined in the
files 'LICENSE' and 'NOTICE', which are part of this source
code package.
-->
<#include "component://shop/webapp/shop/customer/customercommon.ftl">

<@section title=uiLabelMap.ProductSubscriptions id="subscription-summary">
    <@table type="data-list">
        <@thead>
            <@tr class="header-row">
                <@th>${uiLabelMap.ProductSubscription} ${uiLabelMap.CommonId}</@th>
                <@th>${uiLabelMap.ProductSubscription} ${uiLabelMap.CommonType}</@th>
                <@th>${uiLabelMap.CommonDescription}</@th>
                <@th>${uiLabelMap.ProductProductName}</@th>
                <@th>${uiLabelMap.CommonFromDate}</@th>
                <@th>${uiLabelMap.CommonThruDate}</@th>
            </@tr>
            <#--<@tr type="util"><@td colspan="6"><hr /></@td></@tr>-->
        </@thead>
        <@tbody>
            <#list subscriptionList as subscription>
                <@tr>
                    <@td>${subscription.subscriptionId}</@td>
                    <@td>
                        <#assign subscriptionType = subscription.getRelatedOne('SubscriptionType', false)!>
                        ${(subscriptionType.description)?default(subscription.subscriptionTypeId!(uiLabelMap.CommonNA))}
                    </@td>
                    <@td>${subscription.description!}</@td>
                    <@td>
                        <#assign product = subscription.getRelatedOne('Product', false)!>
                        <#if product?has_content>
                            <#assign productName = Static['org.ofbiz.product.product.ProductContentWrapper'].getProductContentAsText(product, 'PRODUCT_NAME', request, "raw")!>
                            <a href="<@pageUrl>product?product_id=${product.productId}</@pageUrl>" class="${styles.link_nav_info_name!}">${productName!product.productId}</a>
                        </#if>
                    </@td>
                    <@td>${subscription.fromDate!}</@td>
                    <@td>${subscription.thruDate!}</@td>
                </@tr>
            </#list>
        </@tbody>
    </@table>
</@section>

