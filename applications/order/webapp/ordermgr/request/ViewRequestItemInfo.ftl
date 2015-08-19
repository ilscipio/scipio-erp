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

<@section title="${uiLabelMap.OrderRequestItems}">
        <table cellspacing="0" class="basic-table">
          <thead>
            <tr valign="bottom" class="header-row">
                <th width="10%">${uiLabelMap.ProductItem}</th>
                <th width="35%">${uiLabelMap.OrderProduct}</th>
                <th width="10%" align="right">${uiLabelMap.ProductQuantity}</th>
                <th width="10%" align="right">${uiLabelMap.OrderAmount}</th>
                <th width="10%" align="right">${uiLabelMap.OrderRequestMaximumAmount}</th>
                <th width="5%" align="right">&nbsp;</th>
            </tr>
            </thead>
            <#assign alt_row = false>
            <#list requestItems as requestItem>
                <#if requestItem.productId??>
                    <#assign product = requestItem.getRelatedOne("Product", false)>
                </#if>
                <tr valign="middle"<@dataRowClassStr alt=alt_row />>
                    <td valign="top">
                        <div>
                            <#if showRequestManagementLinks??>
                                <a href="<@ofbizUrl>EditRequestItem?custRequestId=${requestItem.custRequestId}&amp;custRequestItemSeqId=${requestItem.custRequestItemSeqId}</@ofbizUrl>" class="${styles.button_default!}">${requestItem.custRequestItemSeqId}</a>
                            <#else>
                                ${requestItem.custRequestItemSeqId}
                            </#if>
                        </div>
                    </td>
                    <td valign="top">
                        <div>
                            ${(product.internalName)!}&nbsp;
                            <#if showRequestManagementLinks??>
                                <a href="/catalog/control/EditProduct?productId=${requestItem.productId!}" class="${styles.button_default!}">${requestItem.productId!}</a>
                            <#else>
                                <a href="<@ofbizUrl>product?product_id=${requestItem.productId!}</@ofbizUrl>" class="${styles.button_default!}">${requestItem.productId!}</a>
                            </#if>
                        </div>
                    </td>
                    <td align="right" valign="top">${requestItem.quantity!}</td>
                    <td align="right" valign="top">${requestItem.selectedAmount!}</td>
                    <td align="right" valign="top"><@ofbizCurrency amount=requestItem.maximumAmount isoCode=request.maximumAmountUomId/></td>
                </tr>
                <#-- toggle the row color -->
                <#assign alt_row = !alt_row>
            </#list>
        </table>
    </@section>