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
<#--
<@section>
    <form name="additemsfromorder" action="<@ofbizUrl>AddItemsFromOrder</@ofbizUrl>">
        <input type="hidden" name="shipmentId" value="${shipmentId}"/>
        <@field type="lookup" value=(orderId!) label=uiLabelMap.ProductOrderId formName="additemsfromorder" name="orderId" id="orderId" fieldFormName="LookupOrderHeaderAndShipInfo"/>
        <@field type="text" label=uiLabelMap.ProductOrderShipGroupId size="20" name="shipGroupSeqId" value=(shipGroupSeqId!)/>
        <@field type="submit" text=uiLabelMap.CommonSelect class="${styles.link_run_sys!} ${styles.action_add!}"/>
    </form>
</@section>
-->
<@section>
    <#if orderId?has_content && !orderHeader??>
        <@commonMsg type="error">${getLabel('ProductErrorOrderIdNotFound', '', {"orderId":orderId!''})}</@commonMsg>
    </#if>
    <#-- <#if orderHeader??>
        <#if orderHeader.orderTypeId == "SALES_ORDER" && (shipment.shipmentTypeId!) != "SALES_SHIPMENT">
            <@heading class="+${styles.text_color_alert!}">${uiLabelMap.ProductWarningOrderType} ${(orderType.get("description",locale))?default(orderHeader.orderTypeId!)}, ${uiLabelMap.ProductNotSalesShipment}.</@heading>
        <#elseif orderHeader.orderTypeId == "PURCHASE_ORDER" && (shipment.shipmentTypeId!) != "PURCHASE_SHIPMENT" && (shipment.shipmentTypeId!) != "DROP_SHIPMENT">
            <@heading class="+${styles.text_color_alert!}">${uiLabelMap.ProductWarningOrderType} ${(orderType.get("description",locale))?default(orderHeader.orderTypeId!)}, ${uiLabelMap.ProductNotPurchaseShipment}.</@heading>
        <#else>
            <@heading>${uiLabelMap.ProductNoteOrderType} ${(orderType.get("description",locale))?default(orderHeader.orderTypeId!)}.</@heading>
            <@heading>${uiLabelMap.ProductShipmentType}: ${shipment.shipmentTypeId!}.</@heading>
        </#if>
        <#if (shipment.shipmentTypeId!) == "SALES_SHIPMENT">
            <@heading>${uiLabelMap.ProductOriginFacilityIs}: <#if originFacility??>${originFacility.facilityName!} [${originFacility.facilityId}]<#else><span class="${styles.text_color_alert!}">${uiLabelMap.ProductNotSet}</span></#if></@heading>
        <#elseif (shipment.shipmentTypeId!) == "PURCHASE_SHIPMENT">
            <@heading>${uiLabelMap.ProductDestinationFacilityIs}: <#if destinationFacility??>${destinationFacility.facilityName!} [${destinationFacility.facilityId}]<#else><span class="${styles.text_color_alert!}">${uiLabelMap.ProductNotSet}</span></#if></@heading>
        </#if>
        <#if "ORDER_APPROVED" == orderHeader.statusId || "ORDER_BACKORDERED" == orderHeader.statusId>
            <@heading>${uiLabelMap.ProductNoteOrderStatus} ${(orderHeaderStatus.get("description",locale))?default(orderHeader.statusId!)}.</@heading>
        <#elseif "ORDER_COMPLETED" == orderHeader.statusId>
            <@heading>${uiLabelMap.ProductNoteOrderStatus} ${(orderHeaderStatus.get("description",locale))?default(orderHeader.statusId!)}, ${uiLabelMap.ProductNoItemsLeft}.</@heading>
        <#else>
            <@heading class="+${styles.text_color_alert!}">${uiLabelMap.ProductWarningOrderStatus} ${(orderHeaderStatus.get("description",locale))?default(orderHeader.statusId!)}; ${uiLabelMap.ProductApprovedBeforeShipping}.</@heading>
        </#if>
    </#if> -->

    <#if orderItemDatas??>
        <#assign rowCount = 0>
        <#if isSalesOrder>
            <form action="<@ofbizUrl>issueOrderItemShipGrpInvResToShipment</@ofbizUrl>" method="post" name="selectAllForm">
        <#else>
            <form action="<@ofbizUrl>issueOrderItemToShipment</@ofbizUrl>" method="post" name="selectAllForm">
        </#if>
        <input type="hidden" name="shipmentId" value="${shipmentId}" />
        <input type="hidden" name="_useRowSubmit" value="Y" />
        <@table type="data-complex" class="+${styles.table_spacing_tiny_hint!}" autoAltRows=true> <#-- orig: class="basic-table hover-bar" --> <#-- orig: cellspacing="0" --> <#-- orig: cellpadding="2" -->
          <@thead>
            <@tr class="header-row">
                <@th>${uiLabelMap.ProductOrderId}<br />${uiLabelMap.ProductOrderShipGroupId}<br />${uiLabelMap.ProductOrderItem}</@th>
                <@th>${uiLabelMap.ProductProduct}</@th>
                <#if isSalesOrder>
                    <@th>${uiLabelMap.ProductItemsIssuedReserved}</@th>
                    <@th>${uiLabelMap.ProductIssuedReservedTotalOrdered}</@th>
                    <@th>${uiLabelMap.ProductReserved}</@th>
                    <@th>${uiLabelMap.ProductNotAvailable}</@th>
                <#else>
                    <@th>${uiLabelMap.ProductItemsIssued}</@th>
                    <@th>${uiLabelMap.ProductIssuedOrdered}</@th>
                </#if>
                <@th>${uiLabelMap.ProductIssue}</@th>
                <@th align="right">
                    <div>${uiLabelMap.CommonSubmit} ?</div>
                    <div>${uiLabelMap.CommonAll}<input type="checkbox" name="selectAll" value="${uiLabelMap.CommonY}" onclick="javascript:toggleAll(this, 'selectAllForm');highlightAllRows(this, 'orderItemData_tableRow_', 'selectAllForm');" /></div>
                </@th>
            </@tr>
            </@thead>
            <#list orderItemDatas! as orderItemData>
                <#assign orderItemAndShipGroupAssoc = orderItemData.orderItemAndShipGroupAssoc>
                <#assign product = orderItemData.product!>
                <#assign itemIssuances = orderItemData.itemIssuances>
                <#assign totalQuantityIssued = orderItemData.totalQuantityIssued>
                <#assign orderItemShipGrpInvResDatas = orderItemData.orderItemShipGrpInvResDatas!>
                <#assign totalQuantityReserved = orderItemData.totalQuantityReserved!>
                <#assign totalQuantityIssuedAndReserved = orderItemData.totalQuantityIssuedAndReserved!>
                <@tr id="orderItemData_tableRow_${rowCount}" valign="middle">
                    <@td>${orderItemAndShipGroupAssoc.orderId} / ${orderItemAndShipGroupAssoc.shipGroupSeqId} / ${orderItemAndShipGroupAssoc.orderItemSeqId}</@td>
                    <@td>${(product.internalName)!} [${orderItemAndShipGroupAssoc.productId!(uiLabelMap.CommonNA)}]</@td>
                    <@td>
                        <#if itemIssuances?has_content>
                            <#list itemIssuances as itemIssuance>
                                <div><b>[${itemIssuance.quantity!}]</b>${itemIssuance.shipmentId!}:${itemIssuance.shipmentItemSeqId!} ${uiLabelMap.CommonOn} [${(itemIssuance.issuedDateTime.toString())!}] ${uiLabelMap.CommonBy} [${(itemIssuance.issuedByUserLoginId)!}]</div>
                            </#list>
                        <#else>
                            <div>&nbsp;</div>
                        </#if>
                    </@td>
                    <@td>
                        <#if isSalesOrder>
                            [${totalQuantityIssued} + ${totalQuantityReserved} = ${totalQuantityIssuedAndReserved}]
                            <strong>
                                <#if (totalQuantityIssuedAndReserved > orderItemAndShipGroupAssoc.quantity)>&gt;<#else><#if (totalQuantityIssuedAndReserved < orderItemAndShipGroupAssoc.quantity)>&lt;<#else>=</#if></#if>
                                ${orderItemAndShipGroupAssoc.quantity}
                            </strong>
                        <#else>
                            ${totalQuantityIssued}
                            <strong>
                                <#if (totalQuantityIssued > orderItemAndShipGroupAssoc.quantity)>&gt;<#else><#if (totalQuantityIssued < orderItemAndShipGroupAssoc.quantity)>&lt;<#else>=</#if></#if>
                                ${orderItemAndShipGroupAssoc.quantity}
                            </strong>
                        </#if>
                    </@td>
                    <#if isSalesOrder>
                        <@td colspan=4>&nbsp;</@td>                        
                    <#else>
                        <#assign quantityNotIssued = orderItemAndShipGroupAssoc.quantity - totalQuantityIssued>
                        <#if (quantityNotIssued > 0)>
                            <@td>
                                <input type="hidden" name="shipmentId_o_${rowCount}" value="${shipmentId}"/>
                                <input type="hidden" name="orderId_o_${rowCount}" value="${orderItemAndShipGroupAssoc.orderId}"/>
                                <input type="hidden" name="shipGroupSeqId_o_${rowCount}" value="${orderItemAndShipGroupAssoc.shipGroupSeqId}"/>
                                <input type="hidden" name="orderItemSeqId_o_${rowCount}" value="${orderItemAndShipGroupAssoc.orderItemSeqId}"/>
                                <input type="text" size="5" name="quantity_o_${rowCount}" value="${quantityNotIssued}"/>
                            </@td>
                            <@td>
                              <input type="checkbox" name="_rowSubmit_o_${rowCount}" value="Y" onclick="javascript:checkToggle(this, 'selectAllForm');highlightRow(this,'orderItemData_tableRow_${rowCount}');" />
                            </@td>
                            <#assign rowCount = rowCount + 1>
                        <#else>
                            <@td>&nbsp;</@td>
                            <@td>&nbsp;</@td>
                        </#if>
                    </#if>
                </@tr>
                <#if isSalesOrder>
                    <#list orderItemShipGrpInvResDatas as orderItemShipGrpInvResData>
                        <#assign orderItemShipGrpInvRes = orderItemShipGrpInvResData.orderItemShipGrpInvRes>
                        <#assign inventoryItem = orderItemShipGrpInvResData.inventoryItem>
                        <#assign inventoryItemFacility = orderItemShipGrpInvResData.inventoryItemFacility>
                        <#assign availableQuantity = orderItemShipGrpInvRes.quantity - (orderItemShipGrpInvRes.quantityNotAvailable?default(0))>
                        <#if availableQuantity < 0>
                            <#assign availableQuantity = 0>
                        </#if>
                        <@tr id="orderItemData_tableRow_${rowCount}" groupLast=true>
                            <@td>&nbsp;</@td>
                            <@td>&nbsp;</@td>
                            <@td>
                                ${orderItemShipGrpInvRes.inventoryItemId}
                                <#if inventoryItem.facilityId?has_content>
                                    <span<#if originFacility?? && originFacility.facilityId != inventoryItem.facilityId> class="${styles.text_color_alert!}"</#if>>[${(inventoryItemFacility.facilityName)!inventoryItem.facilityId}]</span>
                                <#else>
                                    <span class="${styles.text_color_alert!}">[${uiLabelMap.ProductNoFacility}]</span>
                                </#if>
                            </@td>
                            <@td>&nbsp;</@td>
                            <@td>${orderItemShipGrpInvRes.quantity}</@td>
                            <@td>${orderItemShipGrpInvRes.quantityNotAvailable?default("&nbsp;")}</@td>
                            <#if originFacility?? && originFacility.facilityId == (inventoryItem.facilityId!)>
                                <@td>
                                    <input type="hidden" name="shipmentId_o_${rowCount}" value="${shipmentId}"/>
                                    <input type="hidden" name="orderId_o_${rowCount}" value="${orderItemShipGrpInvRes.orderId}"/>
                                    <input type="hidden" name="shipGroupSeqId_o_${rowCount}" value="${orderItemShipGrpInvRes.shipGroupSeqId}"/>
                                    <input type="hidden" name="orderItemSeqId_o_${rowCount}" value="${orderItemShipGrpInvRes.orderItemSeqId}"/>
                                    <input type="hidden" name="inventoryItemId_o_${rowCount}" value="${orderItemShipGrpInvRes.inventoryItemId}"/>
                                    <input type="text" size="5" name="quantity_o_${rowCount}" value="${(orderItemShipGrpInvResData.shipmentPlanQuantity)!availableQuantity}"/>
                                </@td>
                                <@td>
                                  <input type="checkbox" name="_rowSubmit_o_${rowCount}" value="Y" onclick="javascript:checkToggle(this, 'selectAllForm');highlightRow(this,'orderItemData_tableRow_${rowCount}');" />
                                </@td>
                                <#assign rowCount = rowCount + 1>
                            <#else>
                                <@td>${uiLabelMap.ProductNotOriginFacility}</@td>
                                <@td>&nbsp;</@td>
                            </#if>
                        </@tr>
                    </#list>
                </#if>
            </#list>
        </@table>
        <div align="right"><input type="submit" class="${styles.link_run_sys!} ${styles.action_add!}" value="${uiLabelMap.ProductIssueAll}"/></div>
        <input type="hidden" name="_rowCount" value="${rowCount}" />
        </form>
        <@script>selectAll('selectAllForm');</@script>
    </#if>
</@section>

<#else>
  <@section>
    <@commonMsg type="error">${uiLabelMap.ProductShipmentNotFoundId}: [${shipmentId!}]</@commonMsg>
  </@section>
</#if>