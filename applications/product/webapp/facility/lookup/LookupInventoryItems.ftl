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

<@table type="data-complex"> <#-- orig: class="basic-table hover-bar" --> <#-- orig: cellspacing="0" -->
 <@thead>
  <@tr class="header-row-2">
    <@th>${uiLabelMap.ProductInventoryItemId}</@th>
    <@th>${uiLabelMap.ProductFacilityId}</@th>
    <@th>${uiLabelMap.ProductLocationSeqId}</@th>
    <@th>${uiLabelMap.ProductQoh}</@th>
    <@th>${uiLabelMap.ProductAtp}</@th>
    <@th>${uiLabelMap.FormFieldTitle_unitCost}</@th>
  </@tr>
  </@thead>
  <#if (inventoryItemsForPo?? && inventoryItemsForPo?has_content)>
    <@tr class="header-row-2"><@td colspan="6">&nbsp;${uiLabelMap.ProductInventoryItemsFor} ${uiLabelMap.ProductPurchaseOrder} - ${orderId}</@td></@tr>
    <#list inventoryItemsForPo as inventoryItem>
      <@tr>
        <@td><a class="${styles.link_nav_info_id!}" href="javascript:set_value('${inventoryItem.inventoryItemId}')">${inventoryItem.inventoryItemId}</a></@td>
        <@td>${inventoryItem.facilityId!}</@td>
        <@td>${inventoryItem.locationSeqId!}</@td>
        <@td>${inventoryItem.quantityOnHandTotal!}</@td>
        <@td>${inventoryItem.availableToPromiseTotal!}</@td>
        <@td>${inventoryItem.unitCost!}</@td>
      </@tr>
    </#list>
  </#if>
  <#if (inventoryItemsForSupplier?? && inventoryItemsForSupplier?has_content)>
    <@tr class="header-row-2"><@td colspan="6"><span class="label centered">&nbsp;${uiLabelMap.ProductInventoryItemsFor} ${uiLabelMap.ProductSupplier} - ${partyId}</span></@td></@tr>
    <#list inventoryItemsForSupplier as inventoryItem>
      <@tr>
        <@td><a class="${styles.link_nav_info_id!}" href="javascript:set_value('${inventoryItem.inventoryItemId}')">${inventoryItem.inventoryItemId}</a></@td>
        <@td>${inventoryItem.facilityId!}</@td>
        <@td>${inventoryItem.locationSeqId!}</@td>
        <@td>${inventoryItem.quantityOnHandTotal!}</@td>
        <@td>${inventoryItem.availableToPromiseTotal!}</@td>
        <@td>${inventoryItem.unitCost!}</@td>
      </@tr>
    </#list>
  </#if>
  <#if (inventoryItemsForProduct?? && inventoryItemsForProduct?has_content)>
    <@tr class="header-row-2"><@td colspan="6">&nbsp;${uiLabelMap.ProductInventoryItemsFor} ${uiLabelMap.ProductProduct} - ${internalName!} [${productId}]</@td></@tr>
    <#list inventoryItemsForProduct as inventoryItem>
      <@tr>
        <@td><a class="${styles.link_nav_info_id!}" href="javascript:set_value('${inventoryItem.inventoryItemId}')">${inventoryItem.inventoryItemId}</a></@td>
        <@td>${inventoryItem.facilityId!}</@td>
        <@td>${inventoryItem.locationSeqId!}</@td>
        <@td>${inventoryItem.quantityOnHandTotal!}</@td>
        <@td>${inventoryItem.availableToPromiseTotal!}</@td>
        <@td>${inventoryItem.unitCost!}</@td>
      </@tr>
    </#list>
  </#if>
  <#if !(inventoryItemsForPo?? && inventoryItemsForPo?has_content) && !(inventoryItemsForSupplier?? && inventoryItemsForSupplier?has_content) && !(inventoryItemsForProduct?? && inventoryItemsForProduct?has_content)>
    <@tr><@td>${uiLabelMap.CommonNo} ${uiLabelMap.ProductInventoryItems} ${uiLabelMap.ProductAvailable}.</@td></@tr>
  </#if>
  <#if inventoryItem?has_content && internalName?has_content>
    <@tr><@td>${inventoryItem.inventoryItemId} - ${internalName}</@td></@tr>
  </#if>
</@table>
