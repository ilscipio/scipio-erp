        <input type="hidden" name="inventoryTransferId" value="${inventoryTransferId!}" />
        <input type="hidden" name="inventoryItemId" value="${inventoryItemId!}" />
        <input type="hidden" name="facilityId" value="${facilityId!}" />            
        <input type="hidden" name="locationSeqId" value="${(inventoryItem.locationSeqId)!}" />
        <@field type="display" label=uiLabelMap.ProductInventoryItemId>
            ${inventoryItemId!}
        </@field>
         <@field type="display" label=uiLabelMap.ProductInventoryItemTypeId>
        <#if inventoryItemType??>
            ${(inventoryItemType.get("description",locale))!}
        </#if>
        </@field>
        <@field type="display" label=uiLabelMap.ProductProductId>
            <#if inventoryItem?? && (inventoryItem.productId)??>
                <a href="<@ofbizInterWebappUrl>/catalog/control/ViewProduct?productId=${(inventoryItem.productId)!}</@ofbizInterWebappUrl>" class="${styles.link_nav_info_id!}">${(inventoryItem.productId)!}</a>
            </#if>
        </@field>
        <@field type="display" label=uiLabelMap.CommonStatus>
            ${(inventoryStatus.get("description",locale))!("--")}
        </@field>
    
        <@field type="display" label=uiLabelMap.ProductComments>
            ${(inventoryItem.comments)!("--")}
        </@field>
    
        <@field type="display" label=uiLabelMap.ProductSerialAtpQoh>
            <#if inventoryItem?? && inventoryItem.inventoryItemTypeId.equals("NON_SERIAL_INV_ITEM")>
                    ${(inventoryItem.availableToPromiseTotal)!}&nbsp;
                    /&nbsp;${(inventoryItem.quantityOnHandTotal)!}
            <#elseif inventoryItem?? && inventoryItem.inventoryItemTypeId.equals("SERIALIZED_INV_ITEM")>
                ${(inventoryItem.serialNumber)!}
            <#elseif inventoryItem??>
                <@commonMsg type="error">${uiLabelMap.ProductErrorType} ${(inventoryItem.inventoryItemTypeId)!} ${uiLabelMap.ProductUnknownSpecifyType}.</@commonMsg>
            </#if>
        </@field>