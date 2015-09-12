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
<@section title="${uiLabelMap.OrderOrderItems}">
            <@table type="data-complex" class="basic-table" cellspacing="0" role="grid">
              <@thead>
                <@tr valign="bottom" class="header-row">
                    <@th width="35%">${uiLabelMap.ProductProduct}</@th>
                    <@th width="10%" class="text-right">${uiLabelMap.CommonStatus}</@th>
                    <@th width="10%">${uiLabelMap.OrderQuantity}</@th>
                    <@th width="10%" class="text-right">${uiLabelMap.OrderUnitList}</@th>
                    <@th width="10%" class="text-right">${uiLabelMap.OrderAdjustments}</@th>
                    <@th width="10%" class="text-right">${uiLabelMap.OrderSubTotal}</@th>
                    <@th width="15%">&nbsp;</@th>
                </@tr>
                </@thead>
                <#if !orderItemList?has_content>
                    <@tr metaRow=true>
                        <@td colspan="7">
                            <@alert type="error">${uiLabelMap.checkhelper_sales_order_lines_lookup_failed}</@alert>
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
                            <#if productId?? && productId == "shoppingcart.CommentLine">
                                <@td> &gt;&gt; ${orderItem.itemDescription}</@td>
                            <#else>
                                <@td>
                                        <strong>
                                        <#if orderItem.supplierProductId?has_content>
                                            ${orderItem.supplierProductId} - ${orderItem.itemDescription!}
                                        <#elseif productId??>
                                            ${orderItem.productId?default("N/A")} - ${orderItem.itemDescription!}
                                            <#if (product.salesDiscontinuationDate)?? && Static["org.ofbiz.base.util.UtilDateTime"].nowTimestamp().after(product.salesDiscontinuationDate)>
                                                <br />
                                                    ${uiLabelMap.OrderItemDiscontinued}: ${Static["org.ofbiz.base.util.UtilFormatOut"].formatDateTime(product.salesDiscontinuationDate, "", locale, timeZone)!}
                                            </#if>
                                        <#elseif orderItemType??>
                                            ${orderItemType.description} - ${orderItem.itemDescription!}
                                        <#else>
                                            ${orderItem.itemDescription!}
                                        </#if>
                                        </strong>
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
                                </@td>
                            </#if>
                            <#if productId?? && productId == "shoppingcart.CommentLine">
                                <@td colspan="7"> &gt;&gt; ${orderItem.itemDescription}</@td>
                            <#else>
                                <#-- now show status details per line item -->
                                <#assign currentItemStatus = orderItem.getRelatedOne("StatusItem", false)>
                                <@td class="text-right">
                                    <@modal id="${productId}_st" label="${currentItemStatus.get('description',locale)?default(currentItemStatus.statusId)}">
                                   
                                            <#if ("ITEM_CREATED" == (currentItemStatus.statusId) && "ORDER_APPROVED" == (orderHeader.statusId)) && security.hasEntityPermission("ORDERMGR", "_UPDATE", session)>
                                                
                                                    <a href="javascript:document.OrderApproveOrderItem_${orderItem.orderItemSeqId?default("")}.submit()" class="${styles.button_default!}">${uiLabelMap.OrderApproveOrder}</a>
                                                    <form name="OrderApproveOrderItem_${orderItem.orderItemSeqId?default("")}" method="post" action="<@ofbizUrl>changeOrderItemStatus</@ofbizUrl>">
                                                        <input type="hidden" name="statusId" value="ITEM_APPROVED"/>
                                                        <input type="hidden" name="orderId" value="${orderId!}"/>
                                                        <input type="hidden" name="orderItemSeqId" value="${orderItem.orderItemSeqId!}"/>
                                                    </form>
                                                <br/>
                                            </#if>
                                            <#assign orderItemStatuses = orderReadHelper.getOrderItemStatuses(orderItem)>
                                            <#list orderItemStatuses as orderItemStatus>
                                                
                                                <#assign loopStatusItem = orderItemStatus.getRelatedOne("StatusItem", false)>
                                                <#if orderItemStatus.statusDatetime?has_content>${Static["org.ofbiz.base.util.UtilFormatOut"].formatDateTime(orderItemStatus.statusDatetime, "", locale, timeZone)!}&nbsp;&nbsp;</#if>${loopStatusItem.get("description",locale)?default(orderItemStatus.statusId)}
                                                <br/>
                                            </#list>
                                        
                                    <#assign returns = orderItem.getRelated("ReturnItem", null, null, false)!>
                                    <#if returns?has_content>
                                        <#list returns as returnItem>
                                            <#assign returnHeader = returnItem.getRelatedOne("ReturnHeader", false)>
                                            <#if returnHeader.statusId != "RETURN_CANCELLED">
                                                <font color="red">${uiLabelMap.OrderReturned}</font>
                                                ${uiLabelMap.CommonNbr}<a href="<@ofbizUrl>returnMain?returnId=${returnItem.returnId}</@ofbizUrl>" class="">${returnItem.returnId}</a>
                                            </#if>
                                        </#list>
                                    </#if>
                                   </@modal>
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
                                        <@modal id="${productId}_q" label="${orderItem.quantity?default(0)?string.number}">    
                                            <@table type="data-complex" class="">
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
                                                    <@td>${returnQuantityMap.get(orderItem.orderItemSeqId)?default(0)}</@td>
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
                                            <@modal id="${productId}_i" label="${uiLabelMap.ProductInventory}">
                                                    <@table type="fields" cellspacing="0" cellpadding="0" border="0">
                                                        <@tr>
                                                            <@td style="text-align: right; padding-bottom: 10px;">
                                                                <a class=""
                                                                   href="/catalog/control/EditProductInventoryItems?productId=${productId}&amp;showAllFacilities=Y${StringUtil.wrapString(externalKeyParam)}"
                                                                   target="_blank">${uiLabelMap.ProductInventory}</a>
                                                            </@td>
                                                            <@td>&nbsp;</@td>
                                                        </@tr>
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
                                                    <a class="close-reveal-modal">&#215;</a>
                                            </@modal>
                                        </#if>
                                    </#if>
                                </@td>
                                <@td class="text-right" valign="top" nowrap="nowrap">
                                    <@ofbizCurrency amount=orderItem.unitPrice isoCode=currencyUomId/>
                                    / <@ofbizCurrency amount=orderItem.unitListPrice isoCode=currencyUomId/>
                                </@td>
                                <@td class="text-right" valign="top" nowrap="nowrap">
                                    <#assign modallabel><@ofbizCurrency amount=Static["org.ofbiz.order.order.OrderReadHelper"].getOrderItemAdjustmentsTotal(orderItem, orderAdjustments, true, false, false) isoCode=currencyUomId/></#assign>
                                    <@modal id="${productId}_adj" label=modallabel>
                                        <@table type="data-complex" class="grid">
                                            <@thead>
                                                <@tr>
                                                <@th width="70%" colspan="2">${uiLabelMap.OrderAdjustments}</@th>
                                                <@th></@th>
                                                <@th></@th>
                                                <@th width="10%" class="text-right">${uiLabelMap.OrderSubTotal}</@th>
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
                                            <a href="/manufacturing/control/ShowProductionRun?productionRunId=${workEffort.workEffortId}${StringUtil.wrapString(externalKeyParam)}"
                                                                    class="">${workEffort.workEffortId}</a>
                                            ${uiLabelMap.OrderCurrentStatus}
                                            ${(delegator.findOne("StatusItem", Static["org.ofbiz.base.util.UtilMisc"].toMap("statusId", workEffort.getString("currentStatusId")), true).get("description",locale))!}
                                        <#else>
                                            ${uiLabelMap.CommonFrom}
                                            : <#if workEffort.estimatedStartDate?has_content>${Static["org.ofbiz.base.util.UtilFormatOut"].formatDate(workEffort.estimatedStartDate, "", locale, timeZone)!}</#if> ${uiLabelMap.CommonTo}
                                            : <#if workEffort.estimatedCompletionDate?has_content>${Static["org.ofbiz.base.util.UtilFormatOut"].formatDate(workEffort.estimatedCompletionDate, "", locale, timeZone)!}</#if> ${uiLabelMap.OrderNumberOfPersons}
                                            : ${workEffort.reservPersons?default("")}
                                        </#if>
                                    </@td>
                                </@tr>
                                <#break><#-- need only the first one -->
                            </#list>
                        </#if>
                        <#-- show linked order lines -->
                        <#assign linkedOrderItemsTo = delegator.findByAnd("OrderItemAssoc", Static["org.ofbiz.base.util.UtilMisc"].toMap("orderId", orderItem.getString("orderId"), "orderItemSeqId", orderItem.getString("orderItemSeqId")), null, false)>
                        <#assign linkedOrderItemsFrom = delegator.findByAnd("OrderItemAssoc", Static["org.ofbiz.base.util.UtilMisc"].toMap("toOrderId", orderItem.getString("orderId"), "toOrderItemSeqId", orderItem.getString("orderItemSeqId")), null, false)>
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
                                        <a href="/ordermgr/control/orderview?orderId=${linkedOrderId}"
                                                               class="">${linkedOrderId}/${linkedOrderItemSeqId}</a>&nbsp;${linkedOrderItemValueStatus.description!}
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
                                        <a href="/ordermgr/control/orderview?orderId=${linkedOrderId}"
                                                               class="">${linkedOrderId}/${linkedOrderItemSeqId}</a>&nbsp;${linkedOrderItemValueStatus.description!}
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
                                        <a href="<@ofbizUrl>EditRequirement?requirementId=${linkedRequirement.requirementId}</@ofbizUrl>"
                                                               class="">${linkedRequirement.requirementId}</a>&nbsp;
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
                                    <a href="<@ofbizUrl>EditQuoteItem?quoteId=${linkedQuote.quoteId}&amp;quoteItemSeqId=${linkedQuote.quoteItemSeqId}</@ofbizUrl>"
                                                           class="">${linkedQuote.quoteId}-${linkedQuote.quoteItemSeqId}</a>&nbsp;
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
                                        ${StringUtil.wrapString(orderItemAdjustment.get("description",locale)!)}
                                        <#if orderItemAdjustment.comments?has_content>
                                            (${orderItemAdjustment.comments?default("")})
                                        </#if>
                                        <#if orderItemAdjustment.productPromoId?has_content>
                                            <a href="/catalog/control/EditProductPromo?productPromoId=${orderItemAdjustment.productPromoId}${StringUtil.wrapString(externalKeyParam)}"
                                               >${orderItemAdjustment.getRelatedOne("ProductPromo", false).getString("promoName")}</a>
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
                                                        <@td class="text-right">
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
                                <@td colspan="7">&nbsp;</@td>
                            </@tr>
                            <#list orderItemPriceInfos as orderItemPriceInfo>
                                                    <@tr class="${rowColor!}">
                                                        <@td colspan="2">
                                                            ${uiLabelMap.ProductPriceRuleNameId}&nbsp;[${orderItemPriceInfo.productPriceRuleId!}:${orderItemPriceInfo.productPriceActionSeqId!}]
                                        ${orderItemPriceInfo.description!}
                                    </@td>
                                    <@td>&nbsp;</@td>
                                                        <@td class="text-right">
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
                                        <a href="/content/control/ViewSurveyResponses?surveyResponseId=${survey.surveyResponseId}&amp;surveyId=${survey.surveyId}${StringUtil.wrapString(externalKeyParam)}"
                                                               class="">${survey.surveyId}</a>
                                    </@td>
                                    <@td colspan="5">&nbsp;</@td>
                                </@tr>
                            </#list>
                        </#if>
                        <#-- display the ship estimated/before/after dates -->
                        <#if orderItem.estimatedShipDate??>
                                                <@tr class="${rowColor!}">
                                                    <@td colspan="2">
                                                        ${uiLabelMap.OrderEstimatedShipDate}&nbsp;${Static["org.ofbiz.base.util.UtilFormatOut"].formatDate(orderItem.estimatedShipDate, "", locale, timeZone)!}
                                </@td>
                                <@td colspan="5">&nbsp;</@td>
                            </@tr>
                        </#if>
                        <#if orderItem.estimatedDeliveryDate??>
                                                <@tr class="${rowColor!}">
                                                    <@td colspan="2">
                                                        ${uiLabelMap.OrderOrderQuoteEstimatedDeliveryDate}&nbsp;${Static["org.ofbiz.base.util.UtilFormatOut"].formatDate(orderItem.estimatedDeliveryDate, "", locale, timeZone)!}
                                </@td>
                                <@td colspan="5">&nbsp;</@td>
                            </@tr>
                        </#if>
                        <#if orderItem.shipAfterDate??>
                                                <@tr class="${rowColor!}">
                                                    <@td colspan="2">
                                                        ${uiLabelMap.OrderShipAfterDate}&nbsp;${Static["org.ofbiz.base.util.UtilFormatOut"].formatDate(orderItem.shipAfterDate, "", locale, timeZone)!}
                                </@td>
                                <@td colspan="5">&nbsp;</@td>
                            </@tr>
                        </#if>
                        <#if orderItem.shipBeforeDate??>
                                                <@tr class="${rowColor!}">
                                                    <@td colspan="2">
                                                        ${uiLabelMap.OrderShipBeforeDate}&nbsp;${Static["org.ofbiz.base.util.UtilFormatOut"].formatDate(orderItem.shipBeforeDate, "", locale, timeZone)!}
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
                                        <a href="/facility/control/EditInventoryItem?inventoryItemId=${orderItemShipGrpInvRes.inventoryItemId}${StringUtil.wrapString(externalKeyParam)}"
                                                               class="">${orderItemShipGrpInvRes.inventoryItemId}</a>
                                                            ${uiLabelMap.OrderShipGroup}&nbsp;${orderItemShipGrpInvRes.shipGroupSeqId}
                                    </@td>
                                    <@td align="center">
                                        ${orderItemShipGrpInvRes.quantity?string.number}&nbsp;
                                    </@td>
                                    <@td>
                                        <#if (orderItemShipGrpInvRes.quantityNotAvailable?has_content && orderItemShipGrpInvRes.quantityNotAvailable > 0)>
                                                                
                                                [${orderItemShipGrpInvRes.quantityNotAvailable?string.number}&nbsp;${uiLabelMap.OrderBackOrdered}]
                                                                
                                                                <#--<a href="<@ofbizUrl>balanceInventoryItems?inventoryItemId=${orderItemShipGrpInvRes.inventoryItemId}&amp;orderId=${orderId}&amp;priorityOrderId=${orderId}&amp;priorityOrderItemSeqId=${orderItemShipGrpInvRes.orderItemSeqId}</@ofbizUrl>" class="" style="font-size: xx-small;">Raise Priority</a> -->
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
                                                            ${uiLabelMap.OrderPlannedInShipment}&nbsp;<a
target="facility"
                                            href="/facility/control/ViewShipment?shipmentId=${orderShipment.shipmentId}${StringUtil.wrapString(externalKeyParam)}"
                                                                class="">${orderShipment.shipmentId}</a>: ${orderShipment.shipmentItemSeqId}
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
                                        <a target="facility"
                                           href="/facility/control/ViewShipment?shipmentId=${itemIssuance.shipmentId}${StringUtil.wrapString(externalKeyParam)}"
                                                               class="">${itemIssuance.shipmentId}</a>: ${itemIssuance.shipmentItemSeqId!}
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
                                            <a href="/facility/control/EditInventoryItem?inventoryItemId=${itemIssuance.inventoryItemId}${StringUtil.wrapString(externalKeyParam)}"
                                                                   class="">${itemIssuance.inventoryItemId}</a>
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
                                            <a target="facility"
                                               href="/facility/control/ViewShipment?shipmentId=${shipmentReceipt.shipmentId}${StringUtil.wrapString(externalKeyParam)}"
                                                                   class="">${shipmentReceipt.shipmentId}</a>:${shipmentReceipt.shipmentItemSeqId!}
                                        </#if>
                                        &nbsp;<#if shipmentReceipt.datetimeReceived?has_content>${Static["org.ofbiz.base.util.UtilFormatOut"].formatDateTime(shipmentReceipt.datetimeReceived, "", locale, timeZone)!}</#if>&nbsp;
                                                            ${uiLabelMap.CommonInventory}&nbsp;
                                        <a href="/facility/control/EditInventoryItem?inventoryItemId=${shipmentReceipt.inventoryItemId}${StringUtil.wrapString(externalKeyParam)}"
                                                               class="">${shipmentReceipt.inventoryItemId}</a>
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
                                <@td class="text-right" valign="top" nowrap="nowrap">
                                    <#if orderItem.statusId != "ITEM_CANCELLED">
                                        <@ofbizCurrency amount=Static["org.ofbiz.order.order.OrderReadHelper"].getOrderItemSubTotal(orderItem, orderAdjustments) isoCode=currencyUomId/>
                                    <#else>
                                        <@ofbizCurrency amount=0.00 isoCode=currencyUomId/>
                                    </#if>
                                </@td>
                                <@td>
                                    <@menu type="button">
                                        <#assign downloadContents = delegator.findByAnd("OrderItemAndProductContentInfo", {"orderId" : orderId, "orderItemSeqId" : orderItem.orderItemSeqId, "productContentTypeId" : "DIGITAL_DOWNLOAD", "statusId" : "ITEM_COMPLETED"})/>
                                        <#if downloadContents?has_content>
                                            <#list downloadContents as downloadContent>
                                                <@menuitem type="link" href="/content/control/ViewSimpleContent?contentId=${downloadContent.contentId}" text="${uiLabelMap.ContentDownload}" target="_blank" />
                                            </#list>
                                        </#if>
                                        <@menuitem type="link" href="/catalog/control/EditProduct?productId=${productId}${StringUtil.wrapString(externalKeyParam)}" text="${uiLabelMap.ProductCatalog}" target="_blank" />
                                        <@menuitem type="link" href="/ecommerce/control/product?product_id=${productId}" text="${uiLabelMap.OrderEcommerce}" target="_blank" />
                                        <#if orderItemContentWrapper.get("IMAGE_URL")?has_content>
                                          <@menuitem type="link" ofbizHref="viewimage?orderId=${orderId}&amp;orderItemSeqId=${orderItem.orderItemSeqId}&amp;orderContentTypeId=IMAGE_URL" text="${uiLabelMap.OrderViewImage}" target="_orderImage" />
                                        </#if>
                                      </@menu>
                                </@td>
                            </#if>
                        </@tr>
                        
                        <#-- New in Ofbiz 14.12 -->
                        <#if orderItem.comments?has_content>
                          <@tr class="${rowColor!}">
                            <@td>&nbsp;</@td>
                            <@td colspan="4">
                              <@section>
                                  <@table type="data-list" class="basic-table" cellspacing="0">
                                    <@tr>
                                      <@td class="text-right" nowrap="nowrap">
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
                                                <#if comm.changeDatetime?has_content>${Static["org.ofbiz.base.util.UtilFormatOut"].formatDateTime(comm.changeDatetime, "", locale, timeZone)?default("0000-00-00 00:00:00")}</#if>  &nbsp;  ${uiLabelMap.CommonBy} -  [${(comm.changeUserLogin)!}]
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
                            <@td class="text-right" nowrap="nowrap">
                                <@ofbizCurrency amount=adjustmentAmount isoCode=currencyUomId/>
                            </@td>
                            <@td>&nbsp;</@td>
                        </@tr>
                    </#if>
                </#list>
                
                
                
                <#-- subtotal -->
                <@tr>
                    <@td colspan="5"></@td>
                    <@td colspan="1"><hr /></@td>
                    <@td colspan="1"></@td>
                </@tr>
                <@tr>
                    <@td colspan="5" class="text-right">
                        ${uiLabelMap.OrderItemsSubTotal}
                    </@td>
                    <@td nowrap="nowrap" class="text-right">
                        <@ofbizCurrency amount=orderSubTotal isoCode=currencyUomId/>
                    </@td>
                    <@td>&nbsp;</@td>
                </@tr>
                <#-- other adjustments -->
                <@tr>
                    <@td colspan="5" class="text-right">
                        ${uiLabelMap.OrderTotalOtherOrderAdjustments}
                    </@td>
                    <@td nowrap="nowrap" class="text-right">
                        <@ofbizCurrency amount=otherAdjAmount isoCode=currencyUomId/>
                    </@td>
                    <@td>&nbsp;</@td>
                </@tr>
                <#-- shipping adjustments -->
                <@tr>
                    <@td colspan="5" class="text-right">
                            ${uiLabelMap.OrderTotalShippingAndHandling}
                    </@td>
                    <@td nowrap="nowrap" class="text-right">
                        <@ofbizCurrency amount=shippingAmount isoCode=currencyUomId/>
                    </@td>
                    <@td>&nbsp;</@td>
                </@tr>
                <#-- tax adjustments -->
                <@tr>
                    <@td colspan="5" class="text-right">
                             ${uiLabelMap.OrderTotalSalesTax}
                    </@td>
                    <@td nowrap="nowrap" class="text-right">
                        <@ofbizCurrency amount=taxAmount isoCode=currencyUomId/>
                    </@td>
                    <@td>&nbsp;</@td>
                </@tr>
                <#-- grand total -->
                <@tr>
                    <@td colspan="5"></@td>
                    <@td colspan="1"><hr /></@td>
                    <@td colspan="1"></@td>
                </@tr>
                <@tr>
                    <@td colspan="5" class="text-right">
                            <strong>${uiLabelMap.OrderTotalDue}</strong>
                    </@td>
                    <@td nowrap="nowrap" class="text-right">
                        <@ofbizCurrency amount=grandTotal isoCode=currencyUomId/>
                            </strong>
                    </@td>
                    <@td>&nbsp;</@td>
                </@tr>
            </@table>
        </@section>
</#if>
