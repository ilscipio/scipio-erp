<#if fromTransfers?has_content>
    <#assign transferList = fromTransfers />
<#elseif toTransfers?has_content>
    <#assign transferList = toTransfers />
</#if>
<#if transferList?has_content>
    <@section menuContent=menuContent title=uiLabelMap.ProductInventoryTransfers>
        <form action="<@pageUrl>UpdateInventoryTransfer</@pageUrl>" method="post">
        <input type="hidden" name="facilityId" value="${facilityId!}" />        
        
            <@table type="data-list" autoAltRows=true scrollable=true responsive=true fixedColumnsLeft=1>
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
                        <@td><a href="<@pageUrl>TransferInventoryItem?inventoryItemId=${transfer.inventoryItemId}&inventoryTransferId=${transfer.inventoryTransferId}&facilityId=${transfer.facilityId}</@pageUrl>">${transfer.inventoryTransferId}</a></@td>
                        <@td><a href="<@pageUrl>EditFacilityInventoryItems?inventoryItemId=${transfer.inventoryItemId}</@pageUrl>">${transfer.inventoryItemId}</a></@td>
                        <@td><a href="<@pageUrl>EditFacility?facilityId=${transfer.facilityId}</@pageUrl>">${facility.facilityName!}</a></@td>                                                 
                        <@td><a href="<@serverUrl>/catalog/control/ViewProduct?productId=${product.productId}</@serverUrl>">${product.productId}</a></@td>
                        <@td>${product.productName!}</@td>
                        <@td><#if statusItem?has_content>${statusItem.description!}</#if></@td>                      
                        <@td>${inventoryItem.serialNumber!}</@td>
                        <@td>${transfer.locationSeqIdTo!}</@td>
                        <@td>${inventoryItem.availableToPromiseTotal!}/${inventoryItem.quantityOnHandTotal!}</@td>
                        <@td>${transfer.sendDate!}</@td>
                        <@td>${transfer.receiveDate!}</@td>
                        <@td><a href=<@pageUrl>TransferInventoryItem?inventoryItemId=${transfer.inventoryItemId}&inventoryTransferId=${transfer.inventoryTransferId}&facilityId=${transfer.facilityId}</@pageUrl>")>${uiLabelMap.CommonUpdate}</a></@td>
                    </@tr>
                </#list>
            </@table>    
        </form>
    </@section>
</#if>