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

<#-- price change rules -->
<#assign allowPriceChange = false/>
<#if (orderHeader.orderTypeId == 'PURCHASE_ORDER' || security.hasEntityPermission("ORDERMGR", "_SALES_PRICEMOD", session))>
    <#assign allowPriceChange = true/>
</#if>

<@section title="${uiLabelMap.OrderOrderItems}">
        <ul class="button-group">
          <#if security.hasEntityPermission("ORDERMGR", "_UPDATE", session)>
              <#if orderHeader?has_content && orderHeader.statusId != "ORDER_CANCELLED" && orderHeader.statusId != "ORDER_COMPLETED">
                  <li><a href="javascript:document.updateItemInfo.action='<@ofbizUrl>cancelSelectedOrderItems</@ofbizUrl>';document.updateItemInfo.submit()" class="button tiny">${uiLabelMap.OrderCancelSelectedItems}</a></li>
                  <li><a href="javascript:document.updateItemInfo.action='<@ofbizUrl>cancelOrderItem</@ofbizUrl>';document.updateItemInfo.submit()" class="button tiny">${uiLabelMap.OrderCancelAllItems}</a></li>
                  <li><a href="<@ofbizUrl>orderview?${paramString}</@ofbizUrl>" class="button tiny">${uiLabelMap.OrderViewOrder}</a></li>
              </#if>
          </#if>
        </ul>
        <#if !orderItemList?has_content>
            <span class="alert">${uiLabelMap.checkhelper_sales_order_lines_lookup_failed}</span>
        <#else>
            <form name="updateItemInfo" method="post" action="<@ofbizUrl>updateOrderItems</@ofbizUrl>">
            <input type="hidden" name="orderId" value="${orderId}"/>
            <input type="hidden" name="orderItemSeqId" value=""/>
            <input type="hidden" name="shipGroupSeqId" value=""/>
            <#if (orderHeader.orderTypeId == 'PURCHASE_ORDER')>
              <input type="hidden" name="supplierPartyId" value="${partyId}"/>
              <input type="hidden" name="orderTypeId" value="PURCHASE_ORDER"/>
            </#if>
            <table class="basic-table order-items" cellspacing="0">
              <thead>
                <tr class="header-row">
                    <th width="30%" style="border-bottom:none;">${uiLabelMap.ProductProduct}</th>
                    <th width="30%" style="border-bottom:none;">${uiLabelMap.CommonStatus}</th>
                    <th width="5%" style="border-bottom:none;" class="align-text">${uiLabelMap.OrderQuantity}</th>
                    <th width="10%" style="border-bottom:none;" class="align-text">${uiLabelMap.OrderUnitPrice}</th>
                    <th width="10%" style="border-bottom:none;" class="align-text">${uiLabelMap.OrderAdjustments}</th>
                    <th width="10%" style="border-bottom:none;" class="align-text">${uiLabelMap.OrderSubTotal}</th>
                    <th width="2%" style="border-bottom:none;">&nbsp;</th>
                    <th width="3%" style="border-bottom:none;">&nbsp;</th>
                </tr>
                </thead>
                <#list orderItemList as orderItem>
                    <#if orderItem.productId??> <#-- a null product may come from a quote -->
                      <#assign orderItemContentWrapper = Static["org.ofbiz.order.order.OrderContentWrapper"].makeOrderContentWrapper(orderItem, request)>
                      
                      <tr>
                          <#assign orderItemType = orderItem.getRelatedOne("OrderItemType", false)!>
                          <#assign productId = orderItem.productId!>
                          <#if productId?? && productId == "shoppingcart.CommentLine">
                              <td colspan="8" valign="top">
                                  &gt;&gt; ${orderItem.itemDescription}
                              </td>
                          <#else>
                              <td valign="top">
                                  <div>
                                      <#if orderHeader.statusId = "ORDER_CANCELLED" || orderHeader.statusId = "ORDER_COMPLETED">
                                      <#if productId??>
                                      ${orderItem.productId?default("N/A")} - ${orderItem.itemDescription!}
                                      <#elseif orderItemType??>
                                      ${orderItemType.description} - ${orderItem.itemDescription!}
                                      <#else>
                                      ${orderItem.itemDescription!}
                                      </#if>
                                      <#else>
                                      <#if productId??>
                                      <#assign orderItemName = orderItem.productId?default("N/A")/>
                                      <#elseif orderItemType??>
                                      <#assign orderItemName = orderItemType.description/>
                                      </#if>
                                      <p>${uiLabelMap.ProductProduct}&nbsp;${orderItemName}</p>
                                      <#if productId??>
                                          <#assign product = orderItem.getRelatedOne("Product", true)>
                                          <#if product.salesDiscontinuationDate?? && Static["org.ofbiz.base.util.UtilDateTime"].nowTimestamp().after(product.salesDiscontinuationDate)>
                                              <span class="alert">${uiLabelMap.OrderItemDiscontinued}: ${product.salesDiscontinuationDate}</span>
                                          </#if>
                                      </#if>
                                      ${uiLabelMap.CommonDescription}<br />
                                      <input type="text" size="20" name="idm_${orderItem.orderItemSeqId}" value="${orderItem.itemDescription!}"/>
                                      </#if>
                                  </div>
                                  <#if productId??>
                                  <div>
                                      <a href="/catalog/control/EditProduct?productId=${productId}" class="button tiny" target="_blank">${uiLabelMap.ProductCatalog}</a>
                                      <a href="/ecommerce/control/product?product_id=${productId}" class="button tiny" target="_blank">${uiLabelMap.OrderEcommerce}</a>
                                      <#if orderItemContentWrapper.get("IMAGE_URL")?has_content>
                                      <a href="<@ofbizUrl>viewimage?orderId=${orderId}&amp;orderItemSeqId=${orderItem.orderItemSeqId}&amp;orderContentTypeId=IMAGE_URL</@ofbizUrl>" target="_orderImage" class="button tiny">${uiLabelMap.OrderViewImage}</a>
                                      </#if>
                                  </div>
                                  </#if>
                              </td>

                              <#-- now show status details per line item -->
                              <#assign currentItemStatus = orderItem.getRelatedOne("StatusItem", false)>
                              <td>
                                  <@modal id="${productId}_st" label="${currentItemStatus.get('description',locale)?default(currentItemStatus.statusId)}">
                                   
                                            <#if ("ITEM_CREATED" == (currentItemStatus.statusId) && "ORDER_APPROVED" == (orderHeader.statusId)) && security.hasEntityPermission("ORDERMGR", "_UPDATE", session)>
                                                
                                                    <a href="javascript:document.OrderApproveOrderItem_${orderItem.orderItemSeqId?default("")}.submit()" class="button tiny">${uiLabelMap.OrderApproveOrder}</a>
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
                              </td>
                              <td class="align-text" valign="top" nowrap="nowrap">
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
                                <@modal id="${productId}_q" label="${orderItem.quantity?default(0)?string.number}">    
                                            <table>
                                                <tr valign="top">
                                                    
                                                    <td><b>${uiLabelMap.OrderOrdered}</b></td>
                                                    <td>${orderItem.quantity?default(0)?string.number}</td>
                                                    <td><b>${uiLabelMap.OrderShipRequest}</b></td>
                                                    <td>${orderReadHelper.getItemReservedQuantity(orderItem)}</td>
                                                </tr>
                                                <tr valign="top">
                                                    <td><b>${uiLabelMap.OrderCancelled}</b></td>
                                                    <td>${orderItem.cancelQuantity?default(0)?string.number}</td>
                                                </tr>
                                                <tr valign="top">
                                                    <td><b>${uiLabelMap.OrderRemaining}</b></td>
                                                    <td>${remainingQuantity}</td>
                                                    <#if orderHeader.orderTypeId == "PURCHASE_ORDER">
                                                        <td><b>${uiLabelMap.OrderPlannedInReceive}</b></td>
                                                        <td>${totalReceived}</td>
                                                    <#else>
                                                        <td><b>${uiLabelMap.OrderQtyShipped}</b></td>
                                                        <td>${shippedQuantity}</td>
                                                    </#if>
                                                </tr>
                                                <tr valign="top">
                                                    <td><b>${uiLabelMap.OrderOutstanding}</b></td>
                                                    <td>
                                                        <#-- Make sure digital goods without shipments don't always remainn "outstanding": if item is completed, it must have no outstanding quantity.  -->
                                                        <#if (orderItem.statusId?has_content) && (orderItem.statusId == "ITEM_COMPLETED")>
                                                            0
                                                        <#elseif orderHeader.orderTypeId == "PURCHASE_ORDER">
                                                            ${(orderItem.quantity?default(0) - orderItem.cancelQuantity?default(0)) - totalReceived?double}
                                                        <#elseif orderHeader.orderTypeId == "SALES_ORDER">
                                                            ${(orderItem.quantity?default(0) - orderItem.cancelQuantity?default(0)) - shippedQuantity?double}
                                                        </#if>
                                                    </td>
                                                </tr>
                                                <tr valign="top">
                                                    <td><b>${uiLabelMap.OrderInvoiced}</b></td>
                                                    <td>${orderReadHelper.getOrderItemInvoicedQuantity(orderItem)}</td>
                                                    <td><b>${uiLabelMap.OrderReturned}</b></td>
                                                    <td>${returnQuantityMap.get(orderItem.orderItemSeqId)?default(0)}</td>
                                                </tr>
                                            </table>
                                        </@modal>
                                  
                              </td>
                              <td class="align-text" valign="top" nowrap="nowrap">
                                  <#-- check for permission to modify price -->
                                  <#if (allowPriceChange)>
                                      <input type="text" size="8" name="ipm_${orderItem.orderItemSeqId}" value="<@ofbizAmount amount=orderItem.unitPrice/>"/>
                                      &nbsp;<input type="checkbox" name="opm_${orderItem.orderItemSeqId}" value="Y"/>
                                  <#else>
                                      <div><@ofbizCurrency amount=orderItem.unitPrice isoCode=currencyUomId/> / <@ofbizCurrency amount=orderItem.unitListPrice isoCode=currencyUomId/></div>
                                  </#if>
                              </td>
                              <td class="align-text" valign="top" nowrap="nowrap">
                                  <@ofbizCurrency amount=Static["org.ofbiz.order.order.OrderReadHelper"].getOrderItemAdjustmentsTotal(orderItem, orderAdjustments, true, false, false) isoCode=currencyUomId/>
                              </td>
                              <td class="align-text" valign="top" nowrap="nowrap">
                                  <#if orderItem.statusId != "ITEM_CANCELLED">
                                  <@ofbizCurrency amount=Static["org.ofbiz.order.order.OrderReadHelper"].getOrderItemSubTotal(orderItem, orderAdjustments) isoCode=currencyUomId/>
                                  <#else>
                                  <@ofbizCurrency amount=0.00 isoCode=currencyUomId/>
                                  </#if>
                              </td>
                              <td>&nbsp;</td>
                          </#if>
                      </tr>

                      <#-- now update/cancel reason and comment field -->
                      <#if orderItem.statusId != "ITEM_CANCELLED" && orderItem.statusId != "ITEM_COMPLETED" && ("Y" != orderItem.isPromo!)>
                        <tr><td colspan="8">${uiLabelMap.OrderReturnReason}
                            <select name="irm_${orderItem.orderItemSeqId}">
                              <option value="">&nbsp;</option>
                              <#list orderItemChangeReasons as reason>
                                <option value="${reason.enumId}">${reason.get("description",locale)?default(reason.enumId)}</option>
                              </#list>
                            </select>
                            ${uiLabelMap.CommonComments}
                            <input type="text" name="icm_${orderItem.orderItemSeqId}" value="" size="30" maxlength="60"/>
                            <#if (orderHeader.orderTypeId == 'PURCHASE_ORDER')>
                              ${uiLabelMap.OrderEstimatedShipDate}
                              <@htmlTemplate.renderDateTimeField name="isdm_${orderItem.orderItemSeqId}" value="${orderItem.estimatedShipDate!}" event="" action="" className=""  title="Format: yyyy-MM-dd HH:mm:ss.SSS" size="25" maxlength="30" id="isdm_${orderItem.orderItemSeqId}" dateType="date" shortDateInput=false timeDropdownParamName="" defaultDateTimeString="" localizedIconTitle="" timeDropdown="" timeHourName="" classString="" hour1="" hour2="" timeMinutesName="" minutes="" isTwelveHour="" ampmName="" amSelected="" pmSelected="" compositeType="" formName=""/>
                              ${uiLabelMap.OrderOrderQuoteEstimatedDeliveryDate}
                              <@htmlTemplate.renderDateTimeField name="iddm_${orderItem.orderItemSeqId}" value="${orderItem.estimatedDeliveryDate!}" event="" action="" className=""  title="Format: yyyy-MM-dd HH:mm:ss.SSS" size="25" maxlength="30" id="iddm_${orderItem.orderItemSeqId}" dateType="date" shortDateInput=false timeDropdownParamName="" defaultDateTimeString="" localizedIconTitle="" timeDropdown="" timeHourName="" classString="" hour1="" hour2="" timeMinutesName="" minutes="" isTwelveHour="" ampmName="" amSelected="" pmSelected="" compositeType="" formName=""/>
                            </#if>
                            </td>
                        </tr>
                      </#if>
                      <#-- now show adjustment details per line item -->
                      <#assign orderItemAdjustments = Static["org.ofbiz.order.order.OrderReadHelper"].getOrderItemAdjustmentList(orderItem, orderAdjustments)>
                      <#if orderItemAdjustments?? && orderItemAdjustments?has_content>
                          <#list orderItemAdjustments as orderItemAdjustment>
                              <#assign adjustmentType = orderItemAdjustment.getRelatedOne("OrderAdjustmentType", true)>
                              <tr>
                                  <td class="align-text" colspan="2">
                                      ${uiLabelMap.OrderAdjustment}&nbsp;${adjustmentType.get("description",locale)}&nbsp;
                                      ${orderItemAdjustment.get("description",locale)!} (${orderItemAdjustment.comments?default("")})

                                      <#if orderItemAdjustment.orderAdjustmentTypeId == "SALES_TAX">
                                      <#if orderItemAdjustment.primaryGeoId?has_content>
                                      <#assign primaryGeo = orderItemAdjustment.getRelatedOne("PrimaryGeo", true)/>
                                      ${uiLabelMap.OrderJurisdiction}&nbsp;${primaryGeo.geoName} [${primaryGeo.abbreviation!}]
                                      <#if orderItemAdjustment.secondaryGeoId?has_content>
                                      <#assign secondaryGeo = orderItemAdjustment.getRelatedOne("SecondaryGeo", true)/>
                                      (${uiLabelMap.CommonIn}&nbsp;${secondaryGeo.geoName} [${secondaryGeo.abbreviation!}])
                                      </#if>
                                      </#if>
                                      <#if orderItemAdjustment.sourcePercentage??>Rate&nbsp;${orderItemAdjustment.sourcePercentage}</#if>
                                      <#if orderItemAdjustment.customerReferenceId?has_content>Customer Tax ID&nbsp;${orderItemAdjustment.customerReferenceId}</#if>
                                      <#if orderItemAdjustment.exemptAmount??>Exempt Amount&nbsp;${orderItemAdjustment.exemptAmount}</#if>
                                      </#if>
                                  </td>
                                  <td>&nbsp;</td>
                                  <td>&nbsp;</td>
                                  <td class="align-text">
                                      <@ofbizCurrency amount=Static["org.ofbiz.order.order.OrderReadHelper"].calcItemAdjustment(orderItemAdjustment, orderItem) isoCode=currencyUomId/>
                                  </td>
                                  <td colspan="3">&nbsp;</td>
                              </tr>
                          </#list>
                      </#if>

                      <#-- now show ship group info per line item -->
                      <#assign orderItemShipGroupAssocs = orderItem.getRelated("OrderItemShipGroupAssoc", null, null, false)!>
                      <#if orderItemShipGroupAssocs?has_content>
                          <tr><td colspan="8">&nbsp;</td></tr>
                          <#list orderItemShipGroupAssocs as shipGroupAssoc>
                                <#assign shipGroupQty = shipGroupAssoc.quantity - shipGroupAssoc.cancelQuantity?default(0)>
                              <#assign shipGroup = shipGroupAssoc.getRelatedOne("OrderItemShipGroup", false)>
                              <#assign shipGroupAddress = shipGroup.getRelatedOne("PostalAddress", false)!>
                              <#assign itemStatusOkay = (orderItem.statusId != "ITEM_CANCELLED" && orderItem.statusId != "ITEM_COMPLETED" && (shipGroupAssoc.cancelQuantity?default(0) < shipGroupAssoc.quantity?default(0)) && ("Y" != orderItem.isPromo!))>
                              <#assign itemSelectable = (security.hasEntityPermission("ORDERMGR", "_ADMIN", session) && itemStatusOkay) || (security.hasEntityPermission("ORDERMGR", "_UPDATE", session) && itemStatusOkay && orderHeader.statusId != "ORDER_SENT")>
                              <tr>
                                  <td class="align-text" colspan="2">
                                      ${uiLabelMap.OrderShipGroup}&nbsp;[${shipGroup.shipGroupSeqId}] ${shipGroupAddress.address1?default("${uiLabelMap.OrderNotShipped}")}
                                  </td>
                                  <td align="center">
                                      <input type="text" name="iqm_${shipGroupAssoc.orderItemSeqId}:${shipGroupAssoc.shipGroupSeqId}" size="6" value="${shipGroupQty?string.number}"/>
                                      <#if itemSelectable>
                                          <input type="checkbox" name="selectedItem" value="${orderItem.orderItemSeqId}" />
                                      </#if>
                                  </td>
                                  <td colspan="4">&nbsp;</td>
                                  <td>
                                      <#if itemSelectable>
                                          <a href="javascript:document.updateItemInfo.action='<@ofbizUrl>cancelOrderItem</@ofbizUrl>';document.updateItemInfo.orderItemSeqId.value='${orderItem.orderItemSeqId}';document.updateItemInfo.shipGroupSeqId.value='${shipGroup.shipGroupSeqId}';document.updateItemInfo.submit()" class="button tiny">${uiLabelMap.CommonCancel}</a>
                                      <#else>
                                          &nbsp;
                                      </#if>
                                  </td>
                              </tr>
                          </#list>
                      </#if>
                    </#if>
                </#list>
                <tr>
                    <td colspan="7">&nbsp;</td>
                    <td>
                        <input type="submit" value="${uiLabelMap.OrderUpdateItems}" class="button tiny"/>
                    </td>
                </tr>
                
            </table>
            </form>
        </#if>
        <#list orderHeaderAdjustments as orderHeaderAdjustment>
            <#assign adjustmentType = orderHeaderAdjustment.getRelatedOne("OrderAdjustmentType", false)>
            <#assign adjustmentAmount = Static["org.ofbiz.order.order.OrderReadHelper"].calcOrderAdjustment(orderHeaderAdjustment, orderSubTotal)>
            <#assign orderAdjustmentId = orderHeaderAdjustment.get("orderAdjustmentId")>
            <#assign productPromoCodeId = ''>
            <#if adjustmentType.get("orderAdjustmentTypeId") == "PROMOTION_ADJUSTMENT" && orderHeaderAdjustment.get("productPromoId")?has_content>
                <#assign productPromo = orderHeaderAdjustment.getRelatedOne("ProductPromo", false)>
                <#assign productPromoCodes = delegator.findByAnd("ProductPromoCode", {"productPromoId":productPromo.productPromoId}, null, false)>
                <#assign orderProductPromoCode = ''>
                <#list productPromoCodes as productPromoCode>
                    <#if !(orderProductPromoCode?has_content)>
                        <#assign orderProductPromoCode = delegator.findOne("OrderProductPromoCode", {"productPromoCodeId":productPromoCode.productPromoCodeId, "orderId":orderHeaderAdjustment.orderId}, false)!>
                    </#if>
                </#list>
                <#if orderProductPromoCode?has_content>
                    <#assign productPromoCodeId = orderProductPromoCode.get("productPromoCodeId")>
                </#if>
            </#if>
            <#if adjustmentAmount != 0>
                <form name="updateOrderAdjustmentForm${orderAdjustmentId}" method="post" action="<@ofbizUrl>updateOrderAdjustment</@ofbizUrl>">
                    <input type="hidden" name="orderAdjustmentId" value="${orderAdjustmentId!}"/>
                    <input type="hidden" name="orderId" value="${orderId!}"/>
                    <table class="basic-table" cellspacing="0">
                        <tr>
                            <td class="align-text" width="55%">
                                ${adjustmentType.get("description",locale)}&nbsp;${orderHeaderAdjustment.comments!}
                            </td>
                            <td nowrap="nowrap" width="30%">
                                <#if (allowPriceChange)>
                                    <input type="text" name="description" value="${orderHeaderAdjustment.get("description")!}" size="30" maxlength="60"/>
                                <#else>
                                    ${orderHeaderAdjustment.get("description")!}
                                </#if>
                            </td>
                            <td nowrap="nowrap" width="15%">
                                <#if (allowPriceChange)>
                                    <input type="text" name="amount" size="6" value="<@ofbizAmount amount=adjustmentAmount/>"/>
                                    <input class="smallSubmit" type="submit" value="${uiLabelMap.CommonUpdate}"/>
                                    <a href="javascript:document.deleteOrderAdjustment${orderAdjustmentId}.submit();" class="button tiny">${uiLabelMap.CommonDelete}</a>
                                <#else>
                                    <@ofbizAmount amount=adjustmentAmount/>
                                </#if>
                            </td>
                        </tr>
                    </table>
                </form>
                <form name="deleteOrderAdjustment${orderAdjustmentId}" method="post" action="<@ofbizUrl>deleteOrderAdjustment</@ofbizUrl>">
                    <input type="hidden" name="orderAdjustmentId" value="${orderAdjustmentId!}"/>
                    <input type="hidden" name="orderId" value="${orderId!}"/>
                    <#if adjustmentType.get("orderAdjustmentTypeId") == "PROMOTION_ADJUSTMENT">
                        <input type="hidden" name="productPromoCodeId" value="${productPromoCodeId!}"/>
                    </#if>
                </form>
            </#if>
        </#list>

        <#-- add new adjustment -->
        <#if security.hasEntityPermission("ORDERMGR", "_UPDATE", session) && orderHeader.statusId != "ORDER_COMPLETED" && orderHeader.statusId != "ORDER_CANCELLED" && orderHeader.statusId != "ORDER_REJECTED">
            <form name="addAdjustmentForm" method="post" action="<@ofbizUrl>createOrderAdjustment</@ofbizUrl>">
                <input type="hidden" name="comments" value="Added manually by [${userLogin.userLoginId}]"/>
                <input type="hidden" name="orderId" value="${orderId!}"/>
                <table class="basic-table" cellspacing="0">
                    <tr>
                        <td class="align-text" width="55%">
                            ${uiLabelMap.OrderAdjustment}&nbsp;
                            <select name="orderAdjustmentTypeId">
                                <#list orderAdjustmentTypes as type>
                                <option value="${type.orderAdjustmentTypeId}">${type.get("description",locale)?default(type.orderAdjustmentTypeId)}</option>
                                </#list>
                            </select>
                            <select name="shipGroupSeqId">
                                <option value="_NA_"></option>
                                <#list shipGroups as shipGroup>
                                <option value="${shipGroup.shipGroupSeqId}">${uiLabelMap.OrderShipGroup} ${shipGroup.shipGroupSeqId}</option>
                                </#list>
                            </select>
                        </td>
                        <td width="30%"><input type="text" name="description" value="" size="30" maxlength="60"/></td>
                        <td width="15%">
                            <input type="text" name="amount" size="6" value="<@ofbizAmount amount=0.00/>"/>
                            <input class="smallSubmit" type="submit" value="${uiLabelMap.CommonAdd}"/>
                        </td>
                    </tr>
                </table>
            </form>
        </#if>

        <#-- subtotal -->
        <table class="basic-table" cellspacing="0">
            <tr class="align-text">
              <td width="80%">${uiLabelMap.OrderItemsSubTotal}</td>
              <td width="10%" nowrap="nowrap"><@ofbizCurrency amount=orderSubTotal isoCode=currencyUomId/></td>
              <td width="10%" colspan="2">&nbsp;</td>
            </tr>

            <#-- other adjustments -->
            <tr class="align-text">
              <td>${uiLabelMap.OrderTotalOtherOrderAdjustments}</td>
              <td nowrap="nowrap"><@ofbizCurrency amount=otherAdjAmount isoCode=currencyUomId/></td>
              <td colspan="2">&nbsp;</td>
            </tr>

            <#-- shipping adjustments -->
            <tr class="align-text">
              <td>${uiLabelMap.OrderTotalShippingAndHandling}</td>
              <td nowrap="nowrap"><@ofbizCurrency amount=shippingAmount isoCode=currencyUomId/></td>
              <td colspan="2">&nbsp;</td>
            </tr>

            <#-- tax adjustments -->
            <tr class="align-text">
              <td>${uiLabelMap.OrderTotalSalesTax}</td>
              <td nowrap="nowrap"><@ofbizCurrency amount=taxAmount isoCode=currencyUomId/></td>
              <td colspan="2">&nbsp;</td>
            </tr>

            <#-- grand total -->
            <tr class="align-text">
              <td>${uiLabelMap.OrderTotalDue}</td>
              <td nowrap="nowrap"><@ofbizCurrency amount=grandTotal isoCode=currencyUomId/></td>
              <td colspan="2">&nbsp;</td>
            </tr>
        </table>
    </@section>
</#if>
