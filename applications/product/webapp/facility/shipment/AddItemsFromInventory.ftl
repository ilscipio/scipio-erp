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

<@section title="${rawString(uiLabelMap.ProductIssueInventoryItemsToShipment)}: [${rawString(shipmentId!)}]">
  <@fields type="default-manual">
    <@table type="data-list" class="+${styles.table_spacing_tiny_hint!}"> <#-- orig: class="basic-table hover-bar" --> <#-- orig: cellspacing="0" --> <#-- orig: cellpadding="2" -->
     <@thead>
      <@tr class="header-row">
        <@th>${uiLabelMap.CommonReturn} ${uiLabelMap.CommonDescription}</@th>
        <@th>${uiLabelMap.ProductProduct}</@th>
        <@th>${uiLabelMap.OrderReturnQty}</@th>
        <@th>${uiLabelMap.ProductShipmentQty}</@th>
        <@th>${uiLabelMap.ProductTotIssuedQuantity}</@th>
        <@th></@th>
        <@th>${uiLabelMap.CommonQty} ${uiLabelMap.CommonNot} ${uiLabelMap.ManufacturingIssuedQuantity}</@th>
        <@th>${uiLabelMap.ProductInventoryItemId} ${uiLabelMap.CommonQty} ${uiLabelMap.CommonSubmit}</@th>
      </@tr>
      </@thead>
      <#list items as item>
        <@tr>
          <@td><a href="<@ofbizInterWebappUrl>/ordermgr/control/returnMain?returnId=${item.returnId}</@ofbizInterWebappUrl>" class="${styles.link_nav_info_id!}">${item.returnId}</a> [${item.returnItemSeqId}]</@td>
          <@td><a href="<@ofbizInterWebappUrl>/catalog/control/EditProductInventoryItems?productId=${item.productId}</@ofbizInterWebappUrl>" class="${styles.link_nav_info_id!}">${item.productId}</a> ${item.internalName!}</@td>
          <@td>${item.returnQuantity}</@td>
          <@td>${item.shipmentItemQty}</@td>
          <@td>${item.totalQtyIssued}</@td>
          <@td>
            <#if item.issuedItems?has_content>
              <#list item.issuedItems as issuedItem>
                <div><a href="<@ofbizInterWebappUrl>/facility/control/EditInventoryItem?inventoryItemId=${issuedItem.inventoryItemId}</@ofbizInterWebappUrl>" class="${styles.link_nav_info_id!}">${issuedItem.inventoryItemId}</a> ${issuedItem.quantity}</div>
              </#list>
            </#if>
          </@td>
          <@td>${item.qtyStillNeedToBeIssued}</@td>
          <#if (item.shipmentItemQty > item.totalQtyIssued)>
            <@td>
                <form name="issueInventoryItemToShipment_${item_index}" action="<@ofbizUrl>issueInventoryItemToShipment</@ofbizUrl>" method="post">
                  <input type="hidden" name="shipmentId" value="${shipmentId}"/>
                  <input type="hidden" name="shipmentItemSeqId" value="${item.shipmentItemSeqId}"/>
                  <input type="hidden" name="totalIssuedQty" value="${item.totalQtyIssued}"/>
                  <@field type="lookup" formName="issueInventoryItemToShipment_${item_index}" name="inventoryItemId" id="inventoryItemId" fieldFormName="LookupInventoryItem?orderId=${item.orderId}&amp;partyId=${item.partyId}&amp;productId=${item.productId}"/>
                  <@field type="input" size="5" name="quantity"/>
                  <@field type="submit" value=uiLabelMap.CommonSubmit class="${styles.link_run_sys!} ${styles.action_update!}"/>
                </form>
            </@td>
          </#if>
        </@tr>
      </#list>
    </@table>
  </@fields>
</@section>
