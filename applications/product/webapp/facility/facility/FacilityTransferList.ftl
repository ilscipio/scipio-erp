<#if fromTransfers?has_content>
    <#assign transferList = fromTransfers />
<#elseif toTransfers?has_content>
    <#assign transferList = toTransfers />
</#if>
<#if transferList?has_content>
    <@section menuContent=menuContent title=uiLabelMap.ProductInventoryTransfers>
        <form action="<@ofbizUrl>UpdateInventoryTransfer</@ofbizUrl>" method="post">
        <input type="hidden" name="facilityId" value="${facilityId!}" />        
        
            <@table type="data-list" autoAltRows=true scrollable=true responsive=true fixedColumnsLeft=1> <#-- orig: class="basic-table hover-bar" --> <#-- orig: cellspacing="0" -->
                <#-- Header Begins -->
                <@thead>
                    <@tr class="header-row-2">
                        <@th>${uiLabelMap.ProductInventoryTransfer}</@th>
                        <@th>${uiLabelMap.ProductInventoryItemId}</@th>
                        <@th>${uiLabelMap.FormFieldTitle_facilityName}</@th>                       
                        <@th>${uiLabelMap.ProductId}</@th>
                        <@th>${uiLabelMap.ProductProductName}</@th>
                        <@th>${uiLabelMap.CommonStatus}</@th>
                        <@th>${uiLabelMap.ProductSerialNumber}</@th>
                        <@th>${uiLabelMap.ProductLocationSeqIdTo}</@th>
                        <@th>${uiLabelMap.ProductAtpQoh}</@th>
                        <@th>${uiLabelMap.CommonSendDate}</@th>
                        <@th>${uiLabelMap.FormFieldTitle_receiveDate}</@th>                                
                        <@th>${uiLabelMap.CommonUpdate}</@th>                        
                    </@tr>
                </@thead>
                <#-- Header Ends-->
                <#list fromTransfers as transfer>
                    <#assign inventoryItem = delegator.findOne("InventoryItem", { "inventoryItemId" : transfer.inventoryItemId }, false) />
                    <#assign facility = delegator.findOne("Facility", { "facilityId" : inventoryItem.facilityId }, true) />
                    <#assign product = delegator.findOne("Product", { "productId" : inventoryItem.productId}, false)!>
                    <#assign statusItem = delegator.findOne("StatusItem", { "statusId" : transfer.statusId }, true) />
                    <@tr>
                        <@td><a href="<@ofbizUrl>TransferInventoryItem?inventoryItemId=${transfer.inventoryItemId}&inventoryTransferId=${transfer.inventoryTransferId}&facilityId=${transfer.facilityId}</@ofbizUrl>">${transfer.inventoryTransferId}</a></@td>
                        <@td><a href="<@ofbizUrl>EditFacilityInventoryItems?inventoryItemId=${transfer.inventoryItemId}</@ofbizUrl>">${transfer.inventoryItemId}</a></@td>
                        <@td><a href="<@ofbizUrl>EditFacility?facilityId=${transfer.facilityId}</@ofbizUrl>">${facility.facilityName!}</a></@td>                                                 
                        <@td><a href="<@ofbizInterWebappUrl>/catalog/control/ViewProduct?productId=${product.productId}</@ofbizInterWebappUrl>">${product.productId}</a></@td>
                        <@td>${product.productName!}</@td>
                        <@td><#if statusItem?has_content>${statusItem.description!}</#if></@td>                      
                        <@td>${inventoryItem.serialNumber!}</@td>
                        <@td>${transfer.locationSeqIdTo!}</@td>
                        <@td>${inventoryItem.availableToPromiseTotal!}/${inventoryItem.quantityOnHandTotal!}</@td>
                        <@td>${transfer.sendDate!}</@td>
                        <@td>${transfer.receiveDate!}</@td>
                        <@td><a href=<@ofbizUrl>TransferInventoryItem?inventoryItemId=${transfer.inventoryItemId}&inventoryTransferId=${transfer.inventoryTransferId}&facilityId=${transfer.facilityId}</@ofbizUrl>")>${uiLabelMap.CommonUpdate}</a></@td>
                    </@tr>
                </#list>
            </@table>    
        </form>
    </@section>
</#if>