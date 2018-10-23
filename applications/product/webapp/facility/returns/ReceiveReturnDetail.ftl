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

<@section menuContent=menuContent>
    <#-- Receiving Results -->
    <#if receivedItems?has_content>
        <@section>
            <p>${uiLabelMap.ProductReceiptForReturn} ${uiLabelMap.CommonNbr}<a href="<@ofbizInterWebappUrl>/ordermgr/control/returnMain?returnId=${returnHeader.returnId}${externalKeyParam!}</@ofbizInterWebappUrl>" class="${styles.link_nav_info_id!}">${returnHeader.returnId}</a></p>
            <#if "RETURN_RECEIVED" == returnHeader.getString("statusId")>
                <@commonMsg type="result">${uiLabelMap.ProductReturnCompletelyReceived}</@commonMsg>
            </#if>
     
            <@table type="data-list"> <#-- orig: class="basic-table" --> <#-- orig: cellspacing="0" -->
                <@thead>
                    <@tr class="header-row">
                        <@th>${uiLabelMap.ProductReceipt}</@th>
                        <@th>${uiLabelMap.CommonDate}</@th>
                        <@th>${uiLabelMap.CommonReturn}</@th>
                        <@th>${uiLabelMap.ProductLine}</@th>
                        <@th>${uiLabelMap.ProductProductId}</@th>
                        <@th>${uiLabelMap.ProductPerUnitPrice}</@th>
                        <@th>${uiLabelMap.ProductReceived}</@th>
                    </@tr>
                </@thead>
                <#list receivedItems as item>
                    <@tr>
                        <@td>${item.receiptId}</@td>
                        <@td>${item.getString("datetimeReceived").toString()}</@td>
                        <@td>${item.returnId}</@td>
                        <@td>${item.returnItemSeqId}</@td>
                        <@td>${item.productId?default("Not Found")}</@td>
                        <@td>${item.unitCost?default(0)?string("##0.00")}</@td>
                        <@td>${item.quantityAccepted?string.number}</@td>
                    </@tr>
                </#list>
          </@table>
          </@section>
    </#if>

    <#-- Multi-Item Return Receiving -->
    <#if returnHeader?has_content>
        <@section>
            <form method="post" action="<@ofbizUrl>receiveReturnedProduct</@ofbizUrl>" name="selectAllForm">
                <@fields type="default-manual-widgetonly">
                    <#-- general request fields -->
                    <input type="hidden" name="facilityId" value="${requestParameters.facilityId!}" />
                    <input type="hidden" name="returnId" value="${requestParameters.returnId!}" />
                    <input type="hidden" name="_useRowSubmit" value="Y" />
                    <#assign now = Static["org.ofbiz.base.util.UtilDateTime"].nowTimestamp().toString()>
                    <#assign rowCount = 0>
                    <#if !returnItems?? || returnItems?size == 0>
                        <@commonMsg type="result-norecord">${uiLabelMap.ProductNoItemsToReceive}</@commonMsg>
                    <#else>
                        <@table type="data-complex"> <#-- orig: class="basic-table" --> <#-- orig: cellspacing="0" -->
                            <@tr>
                                <@td>
                                    <@heading>
                                        ${uiLabelMap.ProductReceiveReturn} <a href="<@ofbizInterWebappUrl>/ordermgr/control/returnMain?returnId=${returnHeader.returnId}${externalKeyParam!}</@ofbizInterWebappUrl>" class="${styles.link_nav_info_id!}">#${returnHeader.returnId}</a>
                                        <#if parameters.shipmentId?has_content>${uiLabelMap.ProductShipmentId} <a href="<@ofbizUrl>EditShipment?shipmentId=${parameters.shipmentId}</@ofbizUrl>" class="${styles.link_nav_info_id!}">${parameters.shipmentId}</a></#if>
                                    </@heading>
                                </@td>
                                <@td align="right">
                                    ${uiLabelMap.ProductSelectAll}&nbsp;
                                    <input type="checkbox" name="selectAll" value="Y" onclick="javascript:toggleAll(this, 'selectAllForm');" />
                                </@td>
                            </@tr>
                            <#list returnItems as returnItem>
                                <#assign defaultQuantity = returnItem.returnQuantity - receivedQuantities[returnItem.returnItemSeqId]?double>
                                <#assign orderItem = returnItem.getRelatedOne("OrderItem", false)!>
                                <#if (orderItem?has_content && 0 < defaultQuantity)>
                                    <#assign orderItemType = (orderItem.getRelatedOne("OrderItemType", false))!>
                                    <input type="hidden" name="returnId_o_${rowCount}" value="${returnItem.returnId}" />
                                    <input type="hidden" name="returnItemSeqId_o_${rowCount}" value="${returnItem.returnItemSeqId}" />
                                    <input type="hidden" name="shipmentId_o_${rowCount}" value="${parameters.shipmentId!}" />
                                    <input type="hidden" name="facilityId_o_${rowCount}" value="${requestParameters.facilityId!}" />
                                    <input type="hidden" name="datetimeReceived_o_${rowCount}" value="${now}" />
                                    <input type="hidden" name="quantityRejected_o_${rowCount}" value="0" />
                                    <input type="hidden" name="comments_o_${rowCount}" value="${uiLabelMap.OrderReturnedItemRaNumber} ${returnItem.returnId}" />
            
                                    <#assign unitCost = Static["org.ofbiz.order.order.OrderReturnServices"].getReturnItemInitialCost(delegator, returnItem.returnId, returnItem.returnItemSeqId)/>
                                    <@tr type="util">
                                        <@td colspan="2"><hr/></@td>
                                    </@tr>
                                    <@tr>
                                        <@td>
                                            <@table type="fields"> <#-- orig: class="basic-table" --> <#-- orig: cellspacing="0" -->
                                                <@tr>
                                                    <#assign productId = "">
                                                    <#if orderItem.productId??>
                                                        <#assign product = orderItem.getRelatedOne("Product", false)>
                                                        <#assign productId = product.productId>
                                                        <#assign serializedInv = product.getRelated("InventoryItem", {"inventoryItemTypeId":"SERIALIZED_INV_ITEM"}, null, false)>
                                                        <input type="hidden" name="productId_o_${rowCount}" value="${product.productId}" />
                                                        <@td width="45%">
                                                            ${returnItem.returnItemSeqId}:&nbsp;<a href="<@ofbizInterWebappUrl>/catalog/control/ViewProduct?productId=${product.productId}${externalKeyParam!}</@ofbizInterWebappUrl>" target="catalog" class="${styles.link_nav_info_idname!}">${product.productId}&nbsp;-&nbsp;${product.internalName!}</a> : ${product.description!}
                                                            <#if serializedInv?has_content><font color="red">**${uiLabelMap.ProductSerializedInventoryFound}**</font></#if>
                                                        </@td>
                                                    <#elseif orderItem?has_content>
                                                        <@td width="45%">
                                                            ${returnItem.returnItemSeqId}:&nbsp;<b>${orderItemType.get("description",locale)}</b> : ${orderItem.itemDescription!}&nbsp;&nbsp;
                                                            <input type="text" size="12" name="productId_o_${rowCount}" />
                                                            <a href="<@ofbizInterWebappUrl>/catalog/control/ViewProduct?${rawString(externalKeyParam)}</@ofbizInterWebappUrl>" target="catalog" class="${styles.link_nav!} ${styles.action_add!}">${uiLabelMap.ProductCreateProduct}</a>
                                                        </@td>
                                                    <#else>
                                                        <@td width="45%">
                                                            ${returnItem.returnItemSeqId}:&nbsp;${returnItem.get("description",locale)!}
                                                        </@td>
                                                    </#if>
                                                    <@td>&nbsp;</@td>
                        
                                                    <#-- location(s) -->
                                                    <@td align="right">${uiLabelMap.ProductLocation}</@td>
                                                    <@td align="right">
                                                        <#assign facilityLocations = (product.getRelated("ProductFacilityLocation", {"facilityId":facilityId}, null, false))!>
                                                        <#if facilityLocations?has_content>
                                                            <select name="locationSeqId_o_${rowCount}">
                                                            <#list facilityLocations as productFacilityLocation>
                                                                <#assign facility = productFacilityLocation.getRelatedOne("Facility", true)>
                                                                <#assign facilityLocation = productFacilityLocation.getRelatedOne("FacilityLocation", false)!>
                                                                <#assign facilityLocationTypeEnum = (facilityLocation.getRelatedOne("TypeEnumeration", true))!>
                                                                <option value="${productFacilityLocation.locationSeqId}"><#if facilityLocation??>${facilityLocation.areaId!}:${facilityLocation.aisleId!}:${facilityLocation.sectionId!}:${facilityLocation.levelId!}:${facilityLocation.positionId!}</#if><#if facilityLocationTypeEnum??>(${facilityLocationTypeEnum.get("description",locale)})</#if>[${productFacilityLocation.locationSeqId}]</option>
                                                            </#list>
                                                            <option value="">${uiLabelMap.ProductNoLocation}</option>
                                                            </select>
                                                        <#else>
                                                            <span>
                                                                <#if parameters.facilityId??>
                                                                    <#assign LookupFacilityLocationView="LookupFacilityLocation?facilityId=${facilityId}">
                                                                <#else>
                                                                    <#assign LookupFacilityLocationView="LookupFacilityLocation">
                                                                </#if>
                                                                <@field type="lookup" formName="selectAllForm" name="locationSeqId_o_${rowCount}" id="locationSeqId_o_${rowCount}" fieldFormName=LookupFacilityLocationView/>
                                                            </span>
                                                        </#if>
                                                    </@td>
                        
                                                    <@td align="right" nowrap="nowrap">${uiLabelMap.ProductQtyReceived}</@td>
                                                    <@td align="right">
                                                        <input type="text" name="quantityAccepted_o_${rowCount}" size="6" value="${defaultQuantity?string.number}" />
                                                    </@td>
                                                </@tr>
                                                <@tr>
                                                    <@td width='10%'>
                                                        <select name="inventoryItemTypeId_o_${rowCount}" size="1" id="inventoryItemTypeId_o_${rowCount}" onchange="javascript:setInventoryItemStatus(this,${rowCount});">
                                                            <#list inventoryItemTypes as nextInventoryItemType>
                                                                <option value="${nextInventoryItemType.inventoryItemTypeId}"
                                                                <#if (facility.defaultInventoryItemTypeId?has_content) && (nextInventoryItemType.inventoryItemTypeId == facility.defaultInventoryItemTypeId)>
                                                                    selected="selected"
                                                                </#if>
                                                                >${nextInventoryItemType.get("description",locale)?default(nextInventoryItemType.inventoryItemTypeId)}</option>
                                                            </#list>
                                                      </select>
                                                    </@td>
                                                    <@td width="35%">
                                                        <span>${uiLabelMap.ProductInitialInventoryItemStatus}:</span>&nbsp;&nbsp;
                                                        <select name="statusId_o_${rowCount}" size="1" id="statusId_o_${rowCount}">
                                                            <option value="INV_RETURNED">${uiLabelMap.ProductReturned}</option>
                                                            <option value="INV_AVAILABLE">${uiLabelMap.ProductAvailable}</option>
                                                            <option value="INV_NS_DEFECTIVE" <#if returnItem.returnReasonId?default("") == "RTN_DEFECTIVE_ITEM">Selected</#if>>${uiLabelMap.ProductDefective}</option>
                                                        </select>
                                                    </@td>
                                                    <#if serializedInv?has_content>
                                                        <@td align="right">${uiLabelMap.ProductExistingInventoryItem}</@td>
                                                        <@td align="right">
                                                            <select name="inventoryItemId_o_${rowCount}">
                                                                <#list serializedInv as inventoryItem>
                                                                    <option>${inventoryItem.inventoryItemId}</option>
                                                                </#list>
                                                            </select>
                                                        </@td>
                                                    <#else>
                                                        <@td colspan="2">&nbsp;</@td>
                                                    </#if>
                                                    <@td align="right" nowrap="nowrap">${uiLabelMap.ProductPerUnitPrice}</@td>
                                                    <@td align="right">
                                                        <input type="text" name="unitCost_o_${rowCount}" size="6" value="${unitCost?default(0)?string("##0.00")}" />
                                                    </@td>
                                                </@tr>
                                            </@table>
                                        </@td>
                                        <@td align="right">
                                            <input type="checkbox" name="_rowSubmit_o_${rowCount}" value="Y" onclick="javascript:checkToggle(this, 'selectAllForm');" />
                                        </@td>
                                    </@tr>
                                    <#assign rowCount = rowCount + 1>
                                </#if>
                            </#list>
                            <@tr type="util">
                                <@td colspan="2">
                                    <hr/>
                                </@td>
                            </@tr>
                            <#if rowCount == 0>
                                <@tr>
                                    <@td colspan="2">${uiLabelMap.ProductNoItemsReturn} #${returnHeader.returnId} ${uiLabelMap.ProductToReceive}.</@td>
                                </@tr>
                                <@tr>
                                    <@td colspan="2" align="right">
                                        <a href="<@ofbizUrl>ReceiveInventory?facilityId=${requestParameters.facilityId!}</@ofbizUrl>" class="${styles.link_nav_cancel!}">${uiLabelMap.ProductReturnToReceiving}</a>
                                    </@td>
                                </@tr>
                            <#else>
                                <@tr>
                                    <@td colspan="2" align="right">
                                        <a href="javascript:document.selectAllForm.submit();" class="${styles.link_run_sys!} ${styles.action_receive!}">${uiLabelMap.ProductReceiveSelectedProduct}</a>
                                    </@td>
                                </@tr>
                            </#if>
                        </@table>
                    </#if>
                    <input type="hidden" name="_rowCount" value="${rowCount}" />
                </@fields>
            </form>
            <@script>selectAll('selectAllForm');</@script>
        </@section>
    </#if>
</@section>
<@script>
    function setInventoryItemStatus(selection,index) {
        var statusId = "statusId_o_" + index;
        jObjectStatusId = jQuery("#" + statusId);
        jQuery.ajax({
            url: 'UpdatedInventoryItemStatus',
            data: {inventoryItemType: selection.value, inventoryItemStatus: jObjectStatusId.val()},
            type: "POST",
            success: function(data){jObjectStatusId.html(data);}
        });
    }
</@script>
