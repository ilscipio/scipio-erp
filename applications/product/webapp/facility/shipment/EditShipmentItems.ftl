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

<#if shipment??>
<@section title="${uiLabelMap.PageTitleEditShipmentItems}">
        <@table type="data-complex" autoAltRows=false cellspacing="0"> <#-- orig: class="basic-table" -->
          <@thead>
            <@tr class="header-row">
                <@th>${uiLabelMap.ProductItem}</@th>
                <@th>&nbsp;</@th>
                <@th>&nbsp;</@th>
                <@th>${uiLabelMap.ProductQuantity}</@th>
                <@th>&nbsp;</@th>
                <@th>&nbsp;</@th>
                <@th>&nbsp;</@th>
            </@tr>
          </@thead>
        <#assign alt_row = false>
        <#list shipmentItemDatas as shipmentItemData>
            <#assign shipmentItem = shipmentItemData.shipmentItem>
            <#assign itemIssuances = shipmentItemData.itemIssuances>
            <#assign orderShipments = shipmentItemData.orderShipments>
            <#assign shipmentPackageContents = shipmentItemData.shipmentPackageContents>
            <#assign product = shipmentItemData.product!>
            <#assign totalQuantityPackaged = shipmentItemData.totalQuantityPackaged>
            <#assign totalQuantityToPackage = shipmentItemData.totalQuantityToPackage>
            <@tr valign="middle" alt=alt_row>
                <@td>${shipmentItem.shipmentItemSeqId}</@td>
                <@td colspan="2"><a href="/catalog/control/EditProduct?productId=${shipmentItem.productId!}" target="_blank" class="${styles.link_default!}">${shipmentItem.productId!} - ${(product.internalName)!}</a></@td>
                <@td>${shipmentItem.quantity?default("&nbsp;")}</@td>
                <@td colspan="2">${shipmentItem.shipmentContentDescription?default("&nbsp;")}</@td>
                <@td><a href="javascript:document.deleteShipmentItem${shipmentItemData_index}.submit();" class="${styles.button_default!}">${uiLabelMap.CommonDelete}</a></@td>
            </@tr>
            <form name="deleteShipmentItem${shipmentItemData_index}" method="post" action="<@ofbizUrl>deleteShipmentItem</@ofbizUrl>">
                <input type="hidden" name="shipmentId" value="${shipmentId}"/>
                <input type="hidden" name="shipmentItemSeqId" value="${shipmentItem.shipmentItemSeqId}"/>
            </form>
            <#list orderShipments as orderShipment>
                <@tr valign="middle" alt=alt_row>
                    <@td>&nbsp;</@td>
                    <@td><span>${uiLabelMap.ProductOrderItem}</span> <a href="/ordermgr/control/orderview?orderId=${orderShipment.orderId!}" class="${styles.button_default!}">${orderShipment.orderId!}</a> ${orderShipment.orderItemSeqId!}</@td>
                    <@td>&nbsp;</@td>
                    <@td>${orderShipment.quantity!}</@td>
                    <@td>&nbsp;</@td>
                    <@td>&nbsp;</@td>
                    <@td>&nbsp;<#-- don't allow a delete, need to implement a cancel issuance <a href="<@ofbizUrl>deleteShipmentItemIssuance?shipmentId=${shipmentId}&amp;itemIssuanceId=${itemIssuance.itemIssuanceId}</@ofbizUrl>" class="${styles.button_default!}">${uiLabelMap.CommonDelete}</a> --></@td>
                </@tr>
            </#list>
            <#list itemIssuances as itemIssuance>
                <@tr valign="middle" alt=alt_row>
                    <@td>&nbsp;</@td>
                    <@td><span>${uiLabelMap.ProductOrderItem}</span> <a href="/ordermgr/control/orderview?orderId=${itemIssuance.orderId!}" target="_blank" class="${styles.link_default!}">${itemIssuance.orderId!} -  ${itemIssuance.orderItemSeqId!}</a></@td>
                    <@td><span>${uiLabelMap.ProductInventory}</span> <a href="<@ofbizUrl>EditInventoryItem?inventoryItemId=${itemIssuance.inventoryItemId!}</@ofbizUrl>" target="_blank" class="${styles.link_default!}">${itemIssuance.inventoryItemId!}</a></@td>
                    <@td>${itemIssuance.quantity!}</@td>
                    <@td>${itemIssuance.issuedDateTime!}</@td>
                    <@td>${uiLabelMap.ProductFuturePartyRoleList}</@td>
                    <@td>&nbsp;<#-- don't allow a delete, need to implement a cancel issuance <a href="<@ofbizUrl>deleteShipmentItemIssuance?shipmentId=${shipmentId}&amp;itemIssuanceId=${itemIssuance.itemIssuanceId}</@ofbizUrl>" class="${styles.button_default!}">${uiLabelMap.CommonDelete}</a> --></@td>
                </@tr>
            </#list>
            <#list shipmentPackageContents as shipmentPackageContent>
                <@tr valign="middle" alt=alt_row>
                    <@td>&nbsp;</@td>
                    <@td colspan="2"><span>${uiLabelMap.ProductPackage}</span> ${shipmentPackageContent.shipmentPackageSeqId}</@td>
                    <@td>${shipmentPackageContent.quantity!}&nbsp;</@td>
                    <#if shipmentPackageContent.subProductId?has_content>
                    <@td><span>${uiLabelMap.ProductSubProduct}</span> ${shipmentPackageContent.subProductId}</@td>
                    <@td>${shipmentPackageContent.subProductQuantity!}</@td>
                    <#else>
                    <@td colspan="2">&nbsp;</@td>
                    </#if>
                    <@td><a href="javascript:document.deleteShipmentItemPackageContent${shipmentItemData_index}${shipmentPackageContent_index}.submit();" class="${styles.button_default!}">${uiLabelMap.CommonDelete}</a></@td>
                </@tr>
                <form name="deleteShipmentItemPackageContent${shipmentItemData_index}${shipmentPackageContent_index}" method="post" action="<@ofbizUrl>deleteShipmentItemPackageContent</@ofbizUrl>">
                    <input type="hidden" name="shipmentId" value="${shipmentId}"/>
                    <input type="hidden" name="shipmentItemSeqId" value="${shipmentPackageContent.shipmentItemSeqId}"/>
                    <input type="hidden" name="shipmentPackageSeqId" value="${shipmentPackageContent.shipmentPackageSeqId}"/>
                </form>
            </#list>
            <#if (totalQuantityToPackage > 0)>
                <@tr valign="middle" alt=alt_row>
                    <form action="<@ofbizUrl>createShipmentItemPackageContent</@ofbizUrl>" method="post" name="createShipmentPackageContentForm${shipmentItemData_index}">
                    <input type="hidden" name="shipmentId" value="${shipmentId}"/>
                    <input type="hidden" name="shipmentItemSeqId" value="${shipmentItem.shipmentItemSeqId}"/>
                    <@td>&nbsp;</@td>
                    <@td colspan="2"><span>${uiLabelMap.ProductAddToPackage}</span>
                        <@input type="select" name="shipmentPackageSeqId">
                            <#list shipmentPackages as shipmentPackage>
                                <option>${shipmentPackage.shipmentPackageSeqId}</option>
                            </#list>
                            <option value="New">${uiLabelMap.CommonNew}</option><!-- Warning: the "New" value cannot be translated because it is used in secas -->
                        </@input>
                    </@td>
                    <@td>
                            <input type="text" name="quantity" size="5" value="${totalQuantityToPackage}"/>
                            <a href="javascript:document.createShipmentPackageContentForm${shipmentItemData_index}.submit()" class="${styles.button_default!}">${uiLabelMap.CommonAdd}</a>
                    </@td>
                    <@td colspan="2">&nbsp;</@td>
                    <@td>&nbsp;</@td>
                    </form>
                </@tr>
            </#if>
            <#-- toggle the row color -->
            <#assign alt_row = !alt_row>
        </#list>
        <@tr>
            <form action="<@ofbizUrl>createShipmentItem</@ofbizUrl>" method="post" name="createShipmentItemForm">
                <input type="hidden" name="shipmentId" value="${shipmentId}"/>
                <@td>${uiLabelMap.ProductNewItem}</@td>
                <@td colspan="2"><span>${uiLabelMap.ProductProductId}</span> 
                  <@htmlTemplate.lookupField formName="createShipmentItemForm" name="productId" id="productId" fieldFormName="LookupProduct"/>
                </@td>
                <@td><span>${uiLabelMap.CommonQty}</span> <input type="text" name="quantity" size="5" value="0"/></@td>
                <@td colspan="2"><span>${uiLabelMap.ProductProductDescription}</span> <input name="shipmentContentDescription" size="30" maxlength="255"/></@td>
                <@td><a href="javascript:document.createShipmentItemForm.submit()" class="${styles.button_default!}">${uiLabelMap.CommonCreate}</a></@td>
            </form>
        </@tr>
        </@table>
</@section>
<#else>
<@section>
        <@alert type="error">${uiLabelMap.ProductShipmentNotFoundId} : [${shipmentId!}]</@alert>
</@section>
</#if>