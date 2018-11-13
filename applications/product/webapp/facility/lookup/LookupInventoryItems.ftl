<#--
This file is subject to the terms and conditions defined in
 file 'LICENSE', which is part of this source code package.
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
