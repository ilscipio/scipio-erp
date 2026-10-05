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

