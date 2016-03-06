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

<@section title=uiLabelMap.OrderRequestItems>
        <@table type="data-list" autoAltRows=true> <#-- orig: class="basic-table" --> <#-- orig: cellspacing="0" -->
          <@thead>
            <@tr valign="bottom" class="header-row">
                <@th width="10%">${uiLabelMap.ProductItem}</@th>
                <@th width="35%">${uiLabelMap.OrderProduct}</@th>
                <@th width="10%" align="right">${uiLabelMap.ProductQuantity}</@th>
                <@th width="10%" align="right">${uiLabelMap.OrderAmount}</@th>
                <@th width="10%" align="right">${uiLabelMap.OrderRequestMaximumAmount}</@th>
                <@th width="5%" align="right">&nbsp;</@th>
            </@tr>
          </@thead>
          <@tbody>
            <#list requestItems as requestItem>
                <#if requestItem.productId??>
                    <#assign product = requestItem.getRelatedOne("Product", false)>
                </#if>
                <@tr valign="middle">
                    <@td valign="top">
                            <#if showRequestManagementLinks??>
                                <a href="<@ofbizUrl>EditRequestItem?custRequestId=${requestItem.custRequestId}&amp;custRequestItemSeqId=${requestItem.custRequestItemSeqId}</@ofbizUrl>" class="${styles.link_nav_info_id!}">${requestItem.custRequestItemSeqId}</a>
                            <#else>
                                ${requestItem.custRequestItemSeqId}
                            </#if>
                        </@td>
                    <@td valign="top">
                            ${(product.internalName)!}&nbsp;
                            <#if showRequestManagementLinks??>
                                <a href="<@ofbizInterWebappUrl>/catalog/control/ViewProduct?productId=${requestItem.productId!}</@ofbizInterWebappUrl>" class="${styles.link_nav_info_id!}">${requestItem.productId!}</a>
                            <#else>
                                <a href="<@ofbizUrl>product?product_id=${requestItem.productId!}</@ofbizUrl>" class="${styles.link_nav_info_id!}">${requestItem.productId!}</a>
                            </#if>
                        </@td>
                    <@td align="right" valign="top">${requestItem.quantity!}</@td>
                    <@td align="right" valign="top">${requestItem.selectedAmount!}</@td>
                    <@td align="right" valign="top"><@ofbizCurrency amount=requestItem.maximumAmount isoCode=request.maximumAmountUomId/></@td>
                </@tr>
            </#list>
          </@tbody>
        </@table>
</@section>