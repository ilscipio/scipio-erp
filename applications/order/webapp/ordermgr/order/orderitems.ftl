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

<#if orderHeader?has_content>
<@section title=uiLabelMap.OrderOrderItems>
            <@table type="data-complex" role="grid"> <#-- orig: class="basic-table" --> <#-- orig: cellspacing="0" -->
              <@thead>
                <@tr valign="bottom" class="header-row">
                    <@th width="30%">${uiLabelMap.ProductProduct}</@th>
                    <@th width="15%" class="${styles.text_right!}">${uiLabelMap.CommonStatus}</@th>
                    <@th width="10%">${uiLabelMap.OrderQuantity}</@th>
                    <@th width="20%" class="${styles.text_right!}">${uiLabelMap.OrderUnitList}</@th>
                    <@th width="15%" class="${styles.text_right!}">${uiLabelMap.OrderAdjustments}</@th>
                    <@th width="15%" class="${styles.text_right!}">${uiLabelMap.OrderSubTotal}</@th>
                </@tr>
                </@thead>
                <#if !orderItemList?has_content>
                    <@tr type="meta">
                        <@td colspan="6">
                            <@commonMsg type="error">${uiLabelMap.checkhelper_sales_order_lines_lookup_failed}</@commonMsg>
                        </@td>
                    </@tr>
                <#else>
                    <#assign itemClass = "2">
                    <#list orderItemList as orderItem>
                        <#if itemClass == "1"><#assign rowColor=styles.row_alt!><#else><#assign rowColor=styles.row_reg!></#if> 
                        <#assign orderItemContentWrapper = Static["org.ofbiz.order.order.OrderContentWrapper"].makeOrderContentWrapper(orderItem, request)>
                        <#assign orderItemShipGrpInvResList = orderReadHelper.getOrderItemShipGrpInvResList(orderItem)>
                        <#if orderHeader.orderTypeId == "SALES_ORDER"><#assign pickedQty = orderReadHelper.getItemPickedQuantityBd(orderItem)></#if>
                        <@tr class="${rowColor!}">
                            <#assign orderItemType = orderItem.getRelatedOne("OrderItemType", false)!>
                            <#assign productId = orderItem.productId!>
                            <#-- SCIPIO: This product lookup added by us, missing from upstream patch -->
                            <#assign product = orderItem.getRelatedOne("Product", false)!>
                            <#if productId?? && productId == "shoppingcart.CommentLine">
                                <@td> &gt;&gt; ${orderItem.itemDescription}</@td>
                            <#else>
                                <@td>
                                        <#if orderItem.supplierProductId?has_content>
                                            <a href="<@ofbizInterWebappUrl>/catalog/control/ViewProduct?productId=${productId}${rawString(externalKeyParam)}</@ofbizInterWebappUrl>">${orderItem.supplierProductId} - ${orderItem.itemDescription!}</a>
                                        <#elseif productId??>
                                            <a href="<@ofbizInterWebappUrl>/catalog/control/ViewProduct?productId=${productId}${rawString(externalKeyParam)}</@ofbizInterWebappUrl>">${orderItem.productId!(uiLabelMap.CommonNA)} - ${orderItem.itemDescription!}</a>
                                            <#if (product.salesDiscontinuationDate)?? && Static["org.ofbiz.base.util.UtilDateTime"].nowTimestamp().after(product.salesDiscontinuationDate)>
                                                <br />
                                                    ${uiLabelMap.OrderItemDiscontinued}: <@formattedDateTime date=product.salesDiscontinuationDate />
                                            </#if>
                                        <#elseif orderItemType??>
                                            <a href="<@ofbizInterWebappUrl>/catalog/control/ViewProduct?productId=${productId}${rawString(externalKeyParam)}</@ofbizInterWebappUrl>">${orderItemType.description} - ${orderItem.itemDescription!}</a>
                                        <#else>
                                            <a href="<@ofbizInterWebappUrl>/catalog/control/ViewProduct?productId=${productId}${rawString(externalKeyParam)}</@ofbizInterWebappUrl>">${orderItem.itemDescription!}</a>
                                        </#if>
                                        
                                        <#assign orderItemAttributes = orderItem.getRelated("OrderItemAttribute", null, null, false)/>
                                        <#if orderItemAttributes?has_content>
                                            <ul>
                                            <#list orderItemAttributes as orderItemAttribute>
                                                <li>
                                                    ${orderItemAttribute.attrName} : ${orderItemAttribute.attrValue}
                                                </li>
                                            </#list>
                                            </ul>
                                        </#if>
                                        <#-- Scipio: order by ProductContent.sequenceNum -->
                                        <#assign downloadContents = delegator.findByAnd("OrderItemAndProductContentInfo", {"orderId" : orderId, "orderItemSeqId" : orderItem.orderItemSeqId, "productContentTypeId" : "DIGITAL_DOWNLOAD", "statusId" : "ITEM_COMPLETED"}, ["sequenceNum ASC"], true)/>
                                        <#if downloadContents?has_content>
                                           <@modal id="${orderId}_${orderItem.orderItemSeqId}_downloads" label=uiLabelMap.ContentDownload class="${styles.link_nav!} ${styles.action_export!}">
                                              <@heading relLevel=+1>${getLabel("EcommerceDownloadsAvailableTitle", "EcommerceUiLabels")}</@heading>
                                              <ol>
                                              <#list downloadContents as downloadContent>
                                                    <li><a href="<@ofbizInterWebappUrl>/content/control/ViewSimpleContent?contentId=${downloadContent.contentId}${rawString(externalKeyParam)}</@ofbizInterWebappUrl>"<#rt/>
                                                        <#lt/> target="_blank" class="${styles.link_run_sys_inline!} ${styles.action_export!}">${downloadContent.contentName!downloadContent.contentId!}</a>
                                              </#list>
                                              </ol>
                                           </@modal>
                                        </#if>
                                </@td>
                            </#if>
                            <#if productId?? && productId == "shoppingcart.CommentLine">
                                <@td colspan="6"> &gt;&gt; ${orderItem.itemDescription}</@td>
                            <#else>
                                <#-- now show status details per line item -->
                                <#assign currentItemStatus = orderItem.getRelatedOne("StatusItem", false)>
                                <@td class="${styles.text_right!}">
                                    <#assign productItemStatus>
                                         <#if ("ITEM_CREATED" == (currentItemStatus.statusId) && "ORDER_APPROVED" == (orderHeader.statusId)) && security.hasEntityPermission("ORDERMGR", "_UPDATE", session)>                                       
                                                    <a href="javascript:document.OrderApproveOrderItem_${orderItem.orderItemSeqId!""}.submit()" class="${styles.link_run_sys!} ${styles.action_update!}">${uiLabelMap.OrderApproveOrder}</a>
                                                    <form name="OrderApproveOrderItem_${orderItem.orderItemSeqId!""}" method="post" action="<@ofbizUrl>changeOrderItemStatus</@ofbizUrl>">
                                                        <input type="hidden" name="statusId" value="ITEM_APPROVED"/>
                                                        <input type="hidden" name="orderId" value="${orderId!}"/>
                                                        <input type="hidden" name="orderItemSeqId" value="${orderItem.orderItemSeqId!}"/>
                                                    </form>
                                                    <br/>
                                                </#if>
                                                <#assign orderItemStatuses = orderReadHelper.getOrderItemStatuses(orderItem)>
                                                <#list orderItemStatuses as orderItemStatus>
                                                    <#assign loopStatusItem = orderItemStatus.getRelatedOne("StatusItem", false)>
                                                    <#if orderItemStatus.statusDatetime?has_content><@formattedDateTime date=orderItemStatus.statusDatetime />&nbsp;&nbsp;</#if>${loopStatusItem.get("description",locale)?default(orderItemStatus.statusId)}
                                                    <br/>
                                                </#list> 
                                        <#assign returns = orderItem.getRelated("ReturnItem", null, null, false)!>
                                        <#if returns?has_content>
                                            <#list returns as returnItem>
                                                <#assign returnHeader = returnItem.getRelatedOne("ReturnHeader", false)>
                                                <#if returnHeader.statusId != "RETURN_CANCELLED">
                                                    <font color="red">${uiLabelMap.OrderReturned}</font>
                                                    ${uiLabelMap.CommonNbr}<a href="<@ofbizUrl>returnMain?returnId=${returnItem.returnId}</@ofbizUrl>">${returnItem.returnId}</a>
                                                </#if>
                                            </#list>
                                        </#if>
                                    </#assign>
                                    <#if productItemStatus?has_content>
                                        <@modal id="${productId}_st" label=(currentItemStatus.get('description',locale)!(currentItemStatus.statusId))>${productItemStatus!}</@modal>
                                    <#else>
                                        ${currentItemStatus.get('description',locale)?default(currentItemStatus.statusId)}
                                    </#if>
                                       
                                   
                                   
                                </@td>
                                <#-- QUANTITY -->
                                <@td>
                                                    <#assign shippedQuantity = orderReadHelper.getItemShippedQuantity(orderItem)>
                                                    <#assign shipmentReceipts = delegator.findByAnd("ShipmentReceipt", {"orderId" : orderHeader.getString("orderId"), "orderItemSeqId" : orderItem.orderItemSeqId}, null, false)/>
                                                    <#assign totalReceived = 0.0>
                                                    <#if shipmentReceipts?? && shipmentReceipts?has_content>
                                                        <#list shipmentReceipts as shipmentReceipt>
                                                            <#if shipmentReceipt.quantityAccepted?? && shipmentReceipt.quantityAccepted?has_content>
                                                                <#assign  quantityAccepted = shipmentReceipt.quantityAccepted>
                                                                <#assign totalReceived = quantityAccepted + totalReceived>
                                                            </#if>
                                                            <#if shipmentReceipt.quantityRejected?? && shipmentReceipt.quantityRejected?has_content>
                                                                <#assign  quantityRejected = shipmentReceipt.quantityRejected>
                                                                <#assign totalReceived = quantityRejected + totalReceived>
                                                            </#if>
                                                        </#list>
                                                    </#if>
                                                    <#if product.productTypeId == "SERVICE" && currentItemStatus.statusId == "ITEM_COMPLETED">
                                                        <#assign shippedQuantity = orderItem.quantity?default(0)/>
                                                        <#assign totalReceived = orderItem.quantity?default(0)>
                                                    </#if>
                                                    <#if orderHeader.orderTypeId == "PURCHASE_ORDER">
                                                        <#assign remainingQuantity = ((orderItem.quantity?default(0) - orderItem.cancelQuantity?default(0)) - totalReceived?double)>
                                                    <#else>
                                                        <#assign remainingQuantity = ((orderItem.quantity?default(0) - orderItem.cancelQuantity?default(0)) - shippedQuantity?double)>
                                                    </#if>
                                                    <#-- to compute shortfall amount, sum up the orderItemShipGrpInvRes.quantityNotAvailable -->
                                                    <#assign shortfalledQuantity = 0/>
                                                    <#list orderItemShipGrpInvResList as orderItemShipGrpInvRes>
                                                        <#if (orderItemShipGrpInvRes.quantityNotAvailable?has_content && orderItemShipGrpInvRes.quantityNotAvailable > 0)>
                                                            <#assign shortfalledQuantity = shortfalledQuantity + orderItemShipGrpInvRes.quantityNotAvailable/>
                                                        </#if>
                                                    </#list>
                                        <@modal id="${productId}_q" label=(orderItem.quantity!0)?string.number>    
                                            <@table type="data-complex"> <#-- orig: class="" -->
                                                <@tr valign="top">
                                                    <@td><b>${uiLabelMap.OrderOrdered}</b></@td>
                                                    <@td>${orderItem.quantity?default(0)?string.number}</@td>
                                                    <@td><b>${uiLabelMap.OrderShipRequest}</b></@td>
                                                    <@td>${orderReadHelper.getItemReservedQuantity(orderItem)}</@td>
                                                </@tr>
                                                <@tr valign="top">
                                                    <@td><b>${uiLabelMap.OrderCancelled}</b></@td>
                                                    <@td>${orderItem.cancelQuantity?default(0)?string.number}</@td>
                                                    <#if orderHeader.orderTypeId == "SALES_ORDER">
                                                        <#if pickedQty gt 0 && orderHeader.statusId == "ORDER_APPROVED">
                                                            <@td><font color="red"><b>${uiLabelMap.OrderQtyPicked}</b></font></@td>
                                                            <@td><font color="red">${pickedQty?default(0)?string.number}</font></@td>
                                                        <#else>
                                                            <@td><b>${uiLabelMap.OrderQtyPicked}</b></@td>
                                                            <@td>${pickedQty?default(0)?string.number}</@td>
                                                        </#if>
                                                    <#else>
                                                        <@td>&nbsp;</@td>
                                                        <@td>&nbsp;</@td>
                                                    </#if>
                                                </@tr>
                                                <@tr valign="top">
                                                    <@td><b>${uiLabelMap.OrderRemaining}</b></@td>
                                                    <@td>${remainingQuantity}</@td>
                                                    <#if orderHeader.orderTypeId == "PURCHASE_ORDER">
                                                        <@td><b>${uiLabelMap.OrderPlannedInReceive}</b></@td>
                                                        <@td>${totalReceived}</@td>
                                                    <#else>
                                                        <@td><b>${uiLabelMap.OrderQtyShipped}</b></@td>
                                                        <@td>${shippedQuantity}</@td>
                                                    </#if>
                                                </@tr>
                                                <@tr valign="top">
                                                    <@td><b>${uiLabelMap.OrderShortfalled}</b></@td>
                                                    <@td>${shortfalledQuantity}</@td>
                                                    <@td><b>${uiLabelMap.OrderOutstanding}</b></@td>
                                                    <@td>
                                                        <#-- Make sure digital goods without shipments don't always remainn "outstanding": if item is completed, it must have no outstanding quantity.  -->
                                                        <#if (orderItem.statusId?has_content) && (orderItem.statusId == "ITEM_COMPLETED")>
                                                            0
                                                        <#elseif orderHeader.orderTypeId == "PURCHASE_ORDER">
                                                            ${(orderItem.quantity?default(0) - orderItem.cancelQuantity?default(0)) - totalReceived?double}
                                                        <#elseif orderHeader.orderTypeId == "SALES_ORDER">
                                                            ${(orderItem.quantity?default(0) - orderItem.cancelQuantity?default(0)) - shippedQuantity?double}
                                                        </#if>
                                                    </@td>
                                                </@tr>
                                                <@tr valign="top">
                                                    <@td><b>${uiLabelMap.OrderInvoiced}</b></@td>
                                                    <@td>${orderReadHelper.getOrderItemInvoicedQuantity(orderItem)}</@td>
                                                    <@td><b>${uiLabelMap.OrderReturned}</b></@td>
                                                    <@td>${returnQuantityMap.get(orderItem.orderItemSeqId)!0}</@td>
                                                </@tr>
                                            </@table>
                                        </@modal>
                                      
                                    <#if productId?has_content>
                                        <#assign product = orderItem.getRelatedOne("Product", true)>
                                    </#if>
                                    <#if productId??>
                                        <#-- INVENTORY -->
                                        <#if (orderHeader.statusId != "ORDER_COMPLETED") && availableToPromiseMap?? && quantityOnHandMap?? && availableToPromiseMap.get(productId)?? && quantityOnHandMap.get(productId)??>
                                            <#assign quantityToProduce = 0>
                                            <#assign atpQuantity = availableToPromiseMap.get(productId)?default(0)>
                                            <#assign qohQuantity = quantityOnHandMap.get(productId)?default(0)>
                                            <#assign mktgPkgATP = mktgPkgATPMap.get(productId)?default(0)>
                                            <#assign mktgPkgQOH = mktgPkgQOHMap.get(productId)?default(0)>
                                            <#assign requiredQuantity = requiredProductQuantityMap.get(productId)?default(0)>
                                            <#assign onOrderQuantity = onOrderProductQuantityMap.get(productId)?default(0)>
                                            <#assign inProductionQuantity = productionProductQuantityMap.get(productId)?default(0)>
                                            <#assign unplannedQuantity = requiredQuantity - qohQuantity - inProductionQuantity - onOrderQuantity - mktgPkgQOH>
                                            <#if unplannedQuantity < 0><#assign unplannedQuantity = 0></#if>
                                            &nbsp;
                                            <@modal id="${productId}_i" label=uiLabelMap.ProductInventory>
                                                    <@table type="fields"> <#-- orig: cellspacing="0" --> <#-- orig: cellpadding="0" --> <#-- orig: border="0" -->
                                                        <@tr>
                                                            <@td>${uiLabelMap.OrderRequiredForSO}</@td>
                                                            <@td style="padding-left: 15px; text-align: left;">${requiredQuantity}</@td>
                                                        </@tr>
                                                        <#if availableToPromiseByFacilityMap?? && quantityOnHandByFacilityMap?? && quantityOnHandByFacilityMap.get(productId)?? && availableToPromiseByFacilityMap.get(productId)??>
                                                            <#assign atpQuantityByFacility = availableToPromiseByFacilityMap.get(productId)?default(0)>
                                                            <#assign qohQuantityByFacility = quantityOnHandByFacilityMap.get(productId)?default(0)>
                                                            <@tr>
                                                                <@td>
                                                                    ${uiLabelMap.ProductInInventory} [${facility.facilityName!}] ${uiLabelMap.ProductQoh}
                                                                </@td>
                                                                <@td style="padding-left: 15px; text-align: left;">
                                                                    ${qohQuantityByFacility} (${uiLabelMap.ProductAtp}: ${atpQuantityByFacility})
                                                                </@td>
                                                            </@tr>
                                                        </#if>
                                                        <@tr>
                                                            <@td>
                                                                ${uiLabelMap.ProductInInventory} [${uiLabelMap.CommonAll} ${uiLabelMap.ProductFacilities}] ${uiLabelMap.ProductQoh}
                                                            </@td>
                                                            <@td style="padding-left: 15px; text-align: left;">
                                                                ${qohQuantity} (${uiLabelMap.ProductAtp}: ${atpQuantity})
                                                            </@td>
                                                        </@tr>
                                                        <#if (product?has_content) && (product.productTypeId?has_content) && Static["org.ofbiz.entity.util.EntityTypeUtil"].hasParentType(delegator, "ProductType", "productTypeId", product.productTypeId, "parentTypeId", "MARKETING_PKG")>
                                                            <@tr>
                                                                <@td>${uiLabelMap.ProductMarketingPackageQOH}</@td>
                                                                <@td style="padding-left: 15px; text-align: left;">
                                                                    ${mktgPkgQOH} (${uiLabelMap.ProductAtp}: ${mktgPkgATP})
                                                                </@td>
                                                            </@tr>
                                                        </#if>
                                                        <@tr>
                                                            <@td>${uiLabelMap.OrderOnOrder}</@td>
                                                            <@td style="padding-left: 15px; text-align: left;">${onOrderQuantity}</@td>
                                                        </@tr>
                                                        <@tr>
                                                            <@td>${uiLabelMap.OrderInProduction}</@td>
                                                            <@td style="padding-left: 15px; text-align: left;">${inProductionQuantity}</@td>
                                                        </@tr>
                                                        <@tr>
                                                            <@td>${uiLabelMap.OrderUnplanned}</@td>
                                                            <@td style="padding-left: 15px; text-align: left;">${unplannedQuantity}</@td>
                                                        </@tr>
                                                    </@table>
                                                    <a href="<@ofbizInterWebappUrl>/catalog/control/EditProductInventoryItems?productId=${productId}&amp;showAllFacilities=Y${rawString(externalKeyParam)}</@ofbizInterWebappUrl>" 
                                                        class="${styles.link_run_sys!} ${styles.action_view!}" target="_blank">${uiLabelMap.ProductInventory}</a>
                                            </@modal>
                                        </#if>
                                    </#if>
                                </@td>
                                <@td class="${styles.text_right!}" valign="top" nowrap="nowrap">
                                    <@ofbizCurrency amount=orderItem.unitPrice isoCode=currencyUomId/>
                                    / <@ofbizCurrency amount=orderItem.unitListPrice isoCode=currencyUomId/>
                                </@td>
                                <@td class="${styles.text_right!}" valign="top" nowrap="nowrap">
                                    <#assign modalLabel><@ofbizCurrency amount=Static["org.ofbiz.order.order.OrderReadHelper"].getOrderItemAdjustmentsTotal(orderItem, orderAdjustments, true, false, false) isoCode=currencyUomId/></#assign>
                                    <@modal id="${productId}_adj" label=modalLabel>
                                        <@table type="data-complex" class="+grid"> <#-- orig: class="grid" -->
                                            <@thead>
                                                <@tr>
                                                <@th width="70%" colspan="2">${uiLabelMap.OrderAdjustments}</@th>
                                                <@th></@th>
                                                <@th></@th>
                                                <@th width="10%" class="${styles.text_right!}">${uiLabelMap.OrderSubTotal}</@th>
                                                <@th></@th>
                                                <@th></@th>
                                                </@tr>
                                            </@thead>
                                            <#-- ADJUSTMENTS -->
                        <#-- show info from workeffort -->
                        <#assign workOrderItemFulfillments = orderItem.getRelated("WorkOrderItemFulfillment", null, null, false)!>
                        <#if workOrderItemFulfillments?has_content>
                            <#list workOrderItemFulfillments as workOrderItemFulfillment>
                                <#assign workEffort = workOrderItemFulfillment.getRelatedOne("WorkEffort", true)>
                                <@tr class="${rowColor!}">
                                    <@td>&nbsp;</@td>
                                    <@td colspan="6">
                                        <#if orderItem.orderItemTypeId != "RENTAL_ORDER_ITEM">
                                            ${uiLabelMap.ManufacturingProductionRun}
                                            <a href="<@ofbizInterWebappUrl>/manufacturing/control/ShowProductionRun?productionRunId=${workEffort.workEffortId}${rawString(externalKeyParam)}</@ofbizInterWebappUrl>" class="${styles.link_nav_info_id!}">${workEffort.workEffortId}</a>
                                            ${uiLabelMap.OrderCurrentStatus}
                                            ${(delegator.findOne("StatusItem", {"statusId":workEffort.getString("currentStatusId")}, true).get("description",locale))!}
                                        <#else>
                                            ${uiLabelMap.CommonFrom}
                                            : <#if workEffort.estimatedStartDate?has_content><@formattedDate date=workEffort.estimatedStartDate /></#if> ${uiLabelMap.CommonTo}
                                            : <#if workEffort.estimatedCompletionDate?has_content><@formattedDate date=workEffort.estimatedCompletionDate /></#if> ${uiLabelMap.OrderNumberOfPersons}
                                            : ${workEffort.reservPersons!""}
                                        </#if>
                                    </@td>
                                </@tr>
                                <#break><#-- need only the first one -->
                            </#list>
                        </#if>
                        <#-- show linked order lines -->
                        <#assign linkedOrderItemsTo = delegator.findByAnd("OrderItemAssoc", {"orderId":orderItem.getString("orderId"), "orderItemSeqId":orderItem.getString("orderItemSeqId")}, null, false)>
                        <#assign linkedOrderItemsFrom = delegator.findByAnd("OrderItemAssoc", {"toOrderId":orderItem.getString("orderId"), "toOrderItemSeqId":orderItem.getString("orderItemSeqId")}, null, false)>
                        <#if linkedOrderItemsTo?has_content>
                            <#list linkedOrderItemsTo as linkedOrderItem>
                                <#assign linkedOrderId = linkedOrderItem.toOrderId>
                                <#assign linkedOrderItemSeqId = linkedOrderItem.toOrderItemSeqId>
                                <#assign linkedOrderItemValue = linkedOrderItem.getRelatedOne("ToOrderItem", false)>
                                <#assign linkedOrderItemValueStatus = linkedOrderItemValue.getRelatedOne("StatusItem", false)>
                                <#assign description = linkedOrderItem.getRelatedOne("OrderItemAssocType", false).getString("description")/>
                                <@tr class="${rowColor!}">
                                    <@td>&nbsp;</@td>
                                    <@td colspan="6">
                                        ${uiLabelMap.OrderLinkedToOrderItem}&nbsp;(${description!})
                                        <a href="<@ofbizInterWebappUrl>/ordermgr/control/orderview?orderId=${linkedOrderId}</@ofbizInterWebappUrl>" class="${styles.link_nav_info_id!}">${linkedOrderId}/${linkedOrderItemSeqId}</a>&nbsp;${linkedOrderItemValueStatus.description!}
                                    </@td>
                                </@tr>
                            </#list>
                        </#if>
                        <#if linkedOrderItemsFrom?has_content>
                            <#list linkedOrderItemsFrom as linkedOrderItem>
                                <#assign linkedOrderId = linkedOrderItem.orderId>
                                <#assign linkedOrderItemSeqId = linkedOrderItem.orderItemSeqId>
                                <#assign linkedOrderItemValue = linkedOrderItem.getRelatedOne("FromOrderItem", false)>
                                <#assign linkedOrderItemValueStatus = linkedOrderItemValue.getRelatedOne("StatusItem", false)>
                                <#assign description = linkedOrderItem.getRelatedOne("OrderItemAssocType", false).getString("description")/>
                                <@tr class="${rowColor!}">
                                    <@td>&nbsp;</@td>
                                    <@td colspan="6">
                                        ${uiLabelMap.OrderLinkedFromOrderItem}&nbsp;(${description!})
                                        <a href="<@ofbizInterWebappUrl>/ordermgr/control/orderview?orderId=${linkedOrderId}</@ofbizInterWebappUrl>" class="${styles.link_nav_info_id!}">${linkedOrderId}/${linkedOrderItemSeqId}</a>&nbsp;${linkedOrderItemValueStatus.description!}
                                    </@td>
                                </@tr>
                            </#list>
                        </#if>
                        <#-- show linked requirements -->
                        <#assign linkedRequirements = orderItem.getRelated("OrderRequirementCommitment", null, null, false)!>
                        <#if linkedRequirements?has_content>
                            <#list linkedRequirements as linkedRequirement>
                                <@tr class="${rowColor!}">
                                    <@td>&nbsp;</@td>
                                    <@td colspan="6">
                                        ${uiLabelMap.OrderLinkedToRequirement}&nbsp;
                                        <a href="<@ofbizUrl>EditRequirement?requirementId=${linkedRequirement.requirementId}</@ofbizUrl>" class="${styles.link_nav_info_id!}">${linkedRequirement.requirementId}</a>&nbsp;
                                    </@td>
                                </@tr>
                            </#list>
                        </#if>
                        <#-- show linked quote -->
                        <#assign linkedQuote = orderItem.getRelatedOne("QuoteItem", true)!>
                        <#if linkedQuote?has_content>
                            <@tr class="${rowColor!}">
                                <@td>&nbsp;</@td>
                                <@td colspan="6">
                                    ${uiLabelMap.OrderLinkedToQuote}&nbsp;
                                    <a href="<@ofbizUrl>EditQuoteItem?quoteId=${linkedQuote.quoteId}&amp;quoteItemSeqId=${linkedQuote.quoteItemSeqId}</@ofbizUrl>" class="${styles.link_nav_info_id!}">${linkedQuote.quoteId}-${linkedQuote.quoteItemSeqId}</a>&nbsp;
                                </@td>
                            </@tr>
                        </#if>
                        <#-- now show adjustment details per line item -->
                        <#assign orderItemAdjustments = Static["org.ofbiz.order.order.OrderReadHelper"].getOrderItemAdjustmentList(orderItem, orderAdjustments)>
                        <#if orderItemAdjustments?? && orderItemAdjustments?has_content>
                            <#list orderItemAdjustments as orderItemAdjustment>
                                <#assign adjustmentType = orderItemAdjustment.getRelatedOne("OrderAdjustmentType", true)>
                                <@tr class="${rowColor!}">
                                    <@td colspan="2">
                                        ${uiLabelMap.OrderAdjustment}&nbsp;${adjustmentType.get("description",locale)}
                                        ${orderItemAdjustment.get("description",locale)!}
                                        <#if orderItemAdjustment.comments?has_content>
                                            (${orderItemAdjustment.comments!""})
                                        </#if>
                                        <#if orderItemAdjustment.productPromoId?has_content>
                                            <a href="<@ofbizInterWebappUrl>/catalog/control/EditProductPromo?productPromoId=${orderItemAdjustment.productPromoId}${rawString(externalKeyParam)}</@ofbizInterWebappUrl>" class="${styles.link_nav_info_name!}">${orderItemAdjustment.getRelatedOne("ProductPromo", false).getString("promoName")}</a>
                                        </#if>
                                        <#if orderItemAdjustment.orderAdjustmentTypeId == "SALES_TAX">
                                            <#if orderItemAdjustment.primaryGeoId?has_content>
                                                <#assign primaryGeo = orderItemAdjustment.getRelatedOne("PrimaryGeo", true)/>
                                                <#if primaryGeo.geoName?has_content>
                                                    ${uiLabelMap.OrderJurisdiction}&nbsp;${primaryGeo.geoName} [${primaryGeo.abbreviation!}]
                                                </#if>
                                                <#if orderItemAdjustment.secondaryGeoId?has_content>
                                                    <#assign secondaryGeo = orderItemAdjustment.getRelatedOne("SecondaryGeo", true)/>
                                                    ${uiLabelMap.CommonIn}&nbsp;${secondaryGeo.geoName} [${secondaryGeo.abbreviation!}])
                                                </#if>
                                            </#if>
                                            <#if orderItemAdjustment.sourcePercentage??>
                                                ${uiLabelMap.OrderRate}&nbsp;${orderItemAdjustment.sourcePercentage?string("0.######")}
                                            </#if>
                                            <#if orderItemAdjustment.customerReferenceId?has_content>
                                                ${uiLabelMap.OrderCustomerTaxId}&nbsp;${orderItemAdjustment.customerReferenceId}
                                            </#if>
                                            <#if orderItemAdjustment.exemptAmount??>
                                                ${uiLabelMap.OrderExemptAmount}&nbsp;${orderItemAdjustment.exemptAmount}
                                            </#if>
                                        </#if>
                                    </@td>
                                    <@td>&nbsp;</@td>
                                    <@td>&nbsp;</@td>
                                                        <@td class="${styles.text_right!}">
                                        <@ofbizCurrency amount=Static["org.ofbiz.order.order.OrderReadHelper"].calcItemAdjustment(orderItemAdjustment, orderItem) isoCode=currencyUomId/>
                                    </@td>
                                                        <@td colspan="2"></@td>
                                </@tr>
                            </#list>
                        </#if>
                        <#-- now show price info per line item -->
                        <#assign orderItemPriceInfos = orderReadHelper.getOrderItemPriceInfos(orderItem)>
                        <#if orderItemPriceInfos?? && orderItemPriceInfos?has_content>
                            <@tr class="${rowColor!}">
                                <@td colspan="6">&nbsp;</@td>
                            </@tr>
                            <#list orderItemPriceInfos as orderItemPriceInfo>
                                <@tr class="${rowColor!}">
                                    <@td colspan="2">
                                        ${uiLabelMap.ProductPriceRuleNameId}&nbsp;[${orderItemPriceInfo.productPriceRuleId!}:${orderItemPriceInfo.productPriceActionSeqId!}]
                                        ${orderItemPriceInfo.description!}
                                    </@td>
                                    <@td>&nbsp;</@td>
                                    <@td class="${styles.text_right!}">
                                        <@ofbizCurrency amount=orderItemPriceInfo.modifyAmount isoCode=currencyUomId/>
                                    </@td>
                                    <@td colspan="3">&nbsp;</@td>
                                </@tr>
                            </#list>
                        </#if>
                        <#-- now show survey information per line item -->
                        <#assign orderItemSurveyResponses = Static["org.ofbiz.order.order.OrderReadHelper"].getOrderItemSurveyResponse(orderItem)>
                        <#if orderItemSurveyResponses?? && orderItemSurveyResponses?has_content>
                            <#list orderItemSurveyResponses as survey>
                                <@tr class="${rowColor!}">
                                    <@td colspan="2">
                                        ${uiLabelMap.CommonSurveys}&nbsp;
                                        <a href="<@ofbizInterWebappUrl>/content/control/ViewSurveyResponses?surveyResponseId=${survey.surveyResponseId}&amp;surveyId=${survey.surveyId}${rawString(externalKeyParam)}</@ofbizInterWebappUrl>" class="${styles.link_nav_info_id!}">${survey.surveyId}</a>
                                    </@td>
                                    <@td colspan="5">&nbsp;</@td>
                                </@tr>
                            </#list>
                        </#if>
                        <#-- display the ship estimated/before/after dates -->
                        <#if orderItem.estimatedShipDate??>
                            <@tr class="${rowColor!}">
                                <@td colspan="2">
                                    ${uiLabelMap.OrderEstimatedShipDate}&nbsp;<@formattedDate date=orderItem.estimatedShipDate />
                                </@td>
                                <@td colspan="5">&nbsp;</@td>
                            </@tr>
                        </#if>
                        <#if orderItem.estimatedDeliveryDate??>
                            <@tr class="${rowColor!}">
                                <@td colspan="2">
                                    ${uiLabelMap.OrderOrderQuoteEstimatedDeliveryDate}&nbsp;<@formattedDate date=orderItem.estimatedDeliveryDate />
                                </@td>
                                <@td colspan="5">&nbsp;</@td>
                            </@tr>
                        </#if>
                        <#if orderItem.shipAfterDate??>
                            <@tr class="${rowColor!}">
                                <@td colspan="2">
                                    ${uiLabelMap.OrderShipAfterDate}&nbsp;<@formattedDate date=orderItem.shipAfterDate />
                                </@td>
                                <@td colspan="5">&nbsp;</@td>
                            </@tr>
                        </#if>
                        <#if orderItem.shipBeforeDate??>
                            <@tr class="${rowColor!}">
                                <@td colspan="2">
                                    ${uiLabelMap.OrderShipBeforeDate}&nbsp;<@formattedDate date=orderItem.shipBeforeDate />
                                </@td>
                                <@td colspan="5">&nbsp;</@td>
                            </@tr>
                        </#if>
                        <#-- now show ship group info per line item -->
                        <#assign orderItemShipGroupAssocs = orderItem.getRelated("OrderItemShipGroupAssoc", null, null, false)!>
                        <#if orderItemShipGroupAssocs?has_content>
                            <#list orderItemShipGroupAssocs as shipGroupAssoc>
                                <#assign shipGroup = shipGroupAssoc.getRelatedOne("OrderItemShipGroup", false)>
                                <#assign shipGroupAddress = shipGroup.getRelatedOne("PostalAddress", false)!>
                                <@tr class="${rowColor!}">
                                    <@td colspan="2">
                                        ${uiLabelMap.OrderShipGroup}&nbsp;[${shipGroup.shipGroupSeqId}]
                                        ${shipGroupAddress.address1?default("${uiLabelMap.OrderNotShipped}")}
                                    </@td>
                                    <@td align="center">
                                        ${shipGroupAssoc.quantity?string.number}&nbsp;
                                    </@td>
                                    <@td colspan="4">&nbsp;</@td>
                                </@tr>
                            </#list>
                        </#if>
                        <#-- now show inventory reservation info per line item -->
                        <#if orderItemShipGrpInvResList?? && orderItemShipGrpInvResList?has_content>
                            <#list orderItemShipGrpInvResList as orderItemShipGrpInvRes>
                                <@tr class="${rowColor!}">
                                    <@td colspan="2">
                                        ${uiLabelMap.CommonInventory}&nbsp;
                                        <a href="<@ofbizInterWebappUrl>/facility/control/EditInventoryItem?inventoryItemId=${orderItemShipGrpInvRes.inventoryItemId}${rawString(externalKeyParam)}</@ofbizInterWebappUrl>" class="${styles.link_nav_info_id!}">${orderItemShipGrpInvRes.inventoryItemId}</a>
                                        ${uiLabelMap.OrderShipGroup}&nbsp;${orderItemShipGrpInvRes.shipGroupSeqId}
                                    </@td>
                                    <@td align="center">
                                        ${orderItemShipGrpInvRes.quantity?string.number}&nbsp;
                                    </@td>
                                    <@td>
                                        <#if (orderItemShipGrpInvRes.quantityNotAvailable?has_content && orderItemShipGrpInvRes.quantityNotAvailable > 0)>
                                                                
                                                [${orderItemShipGrpInvRes.quantityNotAvailable?string.number}&nbsp;${uiLabelMap.OrderBackOrdered}]
                                                                
                                                <#--<a href="<@ofbizUrl>balanceInventoryItems?inventoryItemId=${orderItemShipGrpInvRes.inventoryItemId}&amp;orderId=${orderId}&amp;priorityOrderId=${orderId}&amp;priorityOrderItemSeqId=${orderItemShipGrpInvRes.orderItemSeqId}</@ofbizUrl>" style="font-size: xx-small;">Raise Priority</a> -->
                                        </#if>
                                        &nbsp;
                                    </@td>
                                    <@td colspan="3"></@td>
                                </@tr>
                            </#list>
                        </#if>
                        <#-- now show planned shipment info per line item -->
                        <#assign orderShipments = orderItem.getRelated("OrderShipment", null, null, false)!>
                        <#if orderShipments?has_content>
                            <#list orderShipments as orderShipment>
                                <@tr class="${rowColor!}">
                                    <@td colspan="2">
                                        ${uiLabelMap.OrderPlannedInShipment}&nbsp;<a target="facility" href="<@ofbizInterWebappUrl>/facility/control/EditShipment?shipmentId=${orderShipment.shipmentId}${rawString(externalKeyParam)}</@ofbizInterWebappUrl>" class="${styles.link_nav_info_id!}">${orderShipment.shipmentId}</a>: ${orderShipment.shipmentItemSeqId}
                                    </@td>
                                    <@td align="center">
                                        ${orderShipment.quantity?string.number}&nbsp;
                                    </@td>
                                    <@td colspan="4">&nbsp;</@td>
                                </@tr>
                            </#list>
                        </#if>
                        <#-- now show item issuances (shipment) per line item -->
                        <#assign itemIssuances = itemIssuancesPerItem.get(orderItem.get("orderItemSeqId"))!>
                        <#if itemIssuances?has_content>
                            <#list itemIssuances as itemIssuance>
                            <@tr class="${rowColor!}">
                                <@td colspan="2">
                                    <#if itemIssuance.shipmentId?has_content>
                                        ${uiLabelMap.OrderIssuedToShipmentItem}&nbsp;
                                        <a target="facility" href="<@ofbizInterWebappUrl>/facility/control/EditShipment?shipmentId=${itemIssuance.shipmentId}${rawString(externalKeyParam)}</@ofbizInterWebappUrl>" class="${styles.link_nav_info_id!}">${itemIssuance.shipmentId}</a>: ${itemIssuance.shipmentItemSeqId!}
                                    <#else>
                                        ${uiLabelMap.OrderIssuedWithoutShipment}
                                    </#if>
                                </@td>
                                <@td align="center">
                                    ${itemIssuance.quantity?default(0) - itemIssuance.cancelQuantity?default(0)}&nbsp;
                                </@td>
                                <@td colspan="4">&nbsp;</@td>
                            </@tr>
                            </#list>
                        </#if>
                        <#-- now show item issuances (inventory item) per line item -->
                        <#if itemIssuances?has_content>
                            <#list itemIssuances as itemIssuance>
                                <@tr class="${rowColor!}">
                                    <@td colspan="2">
                                        <#if itemIssuance.inventoryItemId?has_content>
                                            <#assign inventoryItem = itemIssuance.getRelatedOne("InventoryItem", false)/>
                                            ${uiLabelMap.CommonInventory}
                                            <a href="<@ofbizInterWebappUrl>/facility/control/EditInventoryItem?inventoryItemId=${itemIssuance.inventoryItemId}${rawString(externalKeyParam)}</@ofbizInterWebappUrl>" class="${styles.link_nav_info_id!}">${itemIssuance.inventoryItemId}</a>
                                            ${uiLabelMap.OrderShipGroup}&nbsp;${itemIssuance.shipGroupSeqId!}
                                            <#if (inventoryItem.serialNumber?has_content)>
                                                <br />
                                                ${uiLabelMap.ProductSerialNumber}&nbsp;${inventoryItem.serialNumber}&nbsp;
                                            </#if>
                                        </#if>
                                    </@td>
                                    <@td align="center">
                                        ${itemIssuance.quantity?default(0) - itemIssuance.cancelQuantity?default(0)}
                                    </@td>
                                    <@td colspan="4">&nbsp;</@td>
                                </@tr>
                            </#list>
                        </#if>
                        <#-- now show shipment receipts per line item -->
                        <#assign shipmentReceipts = orderItem.getRelated("ShipmentReceipt", null, null, false)!>
                        <#if shipmentReceipts?has_content>
                            <#list shipmentReceipts as shipmentReceipt>
                                <@tr class="${rowColor!}">
                                    <@td colspan="2">
                                        <#if shipmentReceipt.shipmentId?has_content>
                                            ${uiLabelMap.OrderShipmentReceived}&nbsp;
                                            <a target="facility" href="<@ofbizInterWebappUrl>/facility/control/EditShipment?shipmentId=${shipmentReceipt.shipmentId}${rawString(externalKeyParam)}</@ofbizInterWebappUrl>" class="${styles.link_nav_info_id!}">${shipmentReceipt.shipmentId}</a>: ${shipmentReceipt.shipmentItemSeqId!}
                                        </#if>
                                        &nbsp;<#if shipmentReceipt.datetimeReceived?has_content><@formattedDateTime date=shipmentReceipt.datetimeReceived /></#if>&nbsp;
                                        ${uiLabelMap.CommonInventory}&nbsp;
                                        <a href="<@ofbizInterWebappUrl>/facility/control/EditInventoryItem?inventoryItemId=${shipmentReceipt.inventoryItemId}${rawString(externalKeyParam)}</@ofbizInterWebappUrl>" class="${styles.link_nav_info_id!}">${shipmentReceipt.inventoryItemId}</a>
                                    </@td>
                                    <@td align="center">
                                        ${shipmentReceipt.quantityAccepted?string.number}&nbsp;/&nbsp;${shipmentReceipt.quantityRejected?default(0)?string.number}
                                    </@td>
                                    <@td colspan="4">&nbsp;</@td>
                                </@tr>
                            </#list>
                        </#if>
                                        </@table>
                                    </@modal>
                                    
                                </@td>
                                <@td class="${styles.text_right!}" valign="top" nowrap="nowrap">
                                    <#if orderItem.statusId != "ITEM_CANCELLED">
                                        <@ofbizCurrency amount=Static["org.ofbiz.order.order.OrderReadHelper"].getOrderItemSubTotal(orderItem, orderAdjustments) isoCode=currencyUomId/>
                                    <#else>
                                        <@ofbizCurrency amount=0.00 isoCode=currencyUomId/>
                                    </#if>
                                </@td>
                            </#if>
                        </@tr>
                        
                        <#-- New in Ofbiz 14.12 -->
                        <#if orderItem.comments?has_content>
                          <@tr class="${rowColor!}">
                            <@td>&nbsp;</@td>
                            <@td colspan="4">
                              <@section>
                                  <@table type="data-list"> <#-- orig: class="basic-table" --> <#-- orig: cellspacing="0" -->
                                    <@tr>
                                      <@td class="${styles.text_right!}" nowrap="nowrap">
                                        ${uiLabelMap.CommonComments}
                                      </@td>
                                      <@td>
                                        ${uiLabelMap.CommonCurrent}:&nbsp;${orderItem.comments}
                                        <#assign orderItemSeqId = orderItem.orderItemSeqId!>
                                        <#if comments?has_content>
                                          <hr/>
                                          <#list comments as comm>
                                            <#if comm.orderItemSeqId?has_content && orderItemSeqId?has_content && comm.orderItemSeqId == orderItemSeqId>
                                              <#if comm.changeComments?has_content>
                                                <div>
                                                ${(comm.changeComments)!}
                                                &nbsp;
                                                <#if comm.changeDatetime?has_content><@formattedDateTime date=comm.changeDatetime defaultVal="0000-00-00 00:00:00" /></#if> &nbsp; ${uiLabelMap.CommonBy} - [${(comm.changeUserLogin)!}]
                                                </div>
                                              </#if>
                                            </#if>
                                          </#list>
                                        </#if>
                                      </@td>
                                    </@tr>
                                  </@table>
                              </@section>
                            </@td>
                            <@td></@td>
                          </@tr>
                        </#if>                        
                        <#if itemClass == "2">
                            <#assign itemClass = "1">
                        <#else>
                            <#assign itemClass = "2">
                        </#if>
                    </#list>
                </#if>
                <#list orderHeaderAdjustments as orderHeaderAdjustment>
                    <#assign adjustmentType = orderHeaderAdjustment.getRelatedOne("OrderAdjustmentType", false)>
                    <#assign adjustmentAmount = Static["org.ofbiz.order.order.OrderReadHelper"].calcOrderAdjustment(orderHeaderAdjustment, orderSubTotal)>
                    <#if adjustmentAmount != 0>
                        <@tr>
                            <@td colspan="5">
                                <#if orderHeaderAdjustment.comments?has_content>${orderHeaderAdjustment.comments} - </#if>
                                <#if orderHeaderAdjustment.description?has_content>${orderHeaderAdjustment.description} - </#if>
                                ${adjustmentType.get("description", locale)}
                            </@td>
                            <@td class="${styles.text_right!}" nowrap="nowrap">
                                <@ofbizCurrency amount=adjustmentAmount isoCode=currencyUomId/>
                            </@td>
                        </@tr>
                    </#if>
                </#list>
                
                
                
                <#-- subtotal -->
                <@tr>
                    <@td colspan="5"></@td>
                    <@td colspan="1"><hr /></@td>
                </@tr>
                <@tr>
                    <@td colspan="5" class="${styles.text_right!}">
                        ${uiLabelMap.OrderItemsSubTotal}
                    </@td>
                    <@td nowrap="nowrap" class="${styles.text_right!}">
                        <@ofbizCurrency amount=orderSubTotal isoCode=currencyUomId/>
                    </@td>
                </@tr>
                <#-- other adjustments -->
                <@tr>
                    <@td colspan="5" class="${styles.text_right!}">
                        ${uiLabelMap.OrderTotalOtherOrderAdjustments}
                    </@td>
                    <@td nowrap="nowrap" class="${styles.text_right!}">
                        <@ofbizCurrency amount=otherAdjAmount isoCode=currencyUomId/>
                    </@td>
                </@tr>
                <#-- shipping adjustments -->
                <@tr>
                    <@td colspan="5" class="${styles.text_right!}">
                        ${uiLabelMap.OrderTotalShippingAndHandling}
                    </@td>
                    <@td nowrap="nowrap" class="${styles.text_right!}">
                        <@ofbizCurrency amount=shippingAmount isoCode=currencyUomId/>
                    </@td>
                </@tr>
                <#-- tax adjustments -->
                <@tr>
                    <@td colspan="5" class="${styles.text_right!}">
                         ${uiLabelMap.OrderTotalSalesTax}
                    </@td>
                    <@td nowrap="nowrap" class="${styles.text_right!}">
                        <@ofbizCurrency amount=taxAmount isoCode=currencyUomId/>
                    </@td>
                </@tr>
                <#-- grand total -->
                <@tr>
                    <@td colspan="5"></@td>
                    <@td colspan="1"><hr /></@td>
                </@tr>
                <@tr>
                    <@td colspan="5" class="${styles.text_right!}">
                        <strong>${uiLabelMap.OrderTotalDue}</strong>
                    </@td>
                    <@td nowrap="nowrap" class="${styles.text_right!}">
                        <@ofbizCurrency amount=grandTotal isoCode=currencyUomId/>
                    </@td>
                </@tr>
            </@table>
        </@section>
</#if>
