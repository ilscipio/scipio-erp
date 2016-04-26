<#if fromTransfers?has_content>
    <#assign transferList = fromTransfers />
<#elseif toTransfers?has_content>
    <#assign transferList = toTransfers />
</#if>

<@section menuContent=menuContent>
    <#if transferList?has_content>
        <form action="<@ofbizUrl>UpdateInventoryTransfer</@ofbizUrl>" method="post">
        <input type="hidden" name="facilityId" value="${facilityId!}" />        
        
            <@table type="data-list" autoAltRows=true scrollable=true> <#-- orig: class="basic-table hover-bar" --> <#-- orig: cellspacing="0" -->
                <#-- Header Begins -->
                <@thead>
                    <@tr class="header-row-2">
                        <@th>${uiLabelMap.ProductInventoryTransfer}</@th>
                        <@th>${uiLabelMap.ProductInventoryItemId}</@th>
                        <@th>${uiLabelMap.ProductFacilityId}</@th>
                        <@th>${uiLabelMap.FormFieldTitle_facilityName}</@th>
                        <@th>${uiLabelMap.ProductLocationSeqIdFrom}</@th>
                        <@th>${uiLabelMap.ProductId}</@th>
                        <@th>${uiLabelMap.ProductProductName}</@th>
                        <@th>${uiLabelMap.ProductSerialNumber}</@th>
                        <@th>${uiLabelMap.ProductLocationSeqIdTo}</@th>
                        <@th>${uiLabelMap.ProductAtpQoh}</@th>
                        <@th>${uiLabelMap.CommonSendDate}</@th>
                        <@th>${uiLabelMap.CommonReceiveDate}</@th>
                        <@th>${uiLabelMap.CommonStatus}</@th>
        
                        <#-- <@th>${uiLabelMap.CommonUpdate}</@th>
                        <@th>${uiLabelMap.CommonDelete}</@th> -->
                    </@tr>
                </@thead>
                <#-- Header Ends-->
                
                    <#list fromTransfers as transfer>
                        <#assign inventoryItem = delegator.findOne("InventoryItem", { "inventoryItemId" : transfer.inventoryItemId }, false) />
                        <#assign facility = delegator.findOne("Facility", { "facilityId" : inventoryItem.facilityId }, true) />
                        <#assign product = delegator.findOne("Product", { "productId" : inventoryItem.productId}, false)!>
                        <#assign statusItem = delegator.findOne("StatusItem", { "statusId" : transfer.statusId }, true) />
                        <@tr>
                            <@td>${transfer.inventoryTransferId}</@td>
                            <@td>${transfer.inventoryItemId}</@td>
                            <@td>${transfer.facilityId}</@td>
                            <@td>${facility.facilityName!}</@td>
                            <@td>${transfer.locationSeqId!}</@td>                            
                            <@td><a href="<@ofbizUrl>${product.productId}</@ofbizUrl>">${product.productId}</a></@td>
                            <@td>${product.productName!}</@td>                        
                            <@td>${inventoryItem.serialNumber!}</@td>
                            <@td>${transfer.locationSeqIdTo!}</@td>
                            <@td>${inventoryItem.availableToPromiseTotal!}/${inventoryItem.quantityOnHandTotal!}</@td>
                            <@td>${transfer.sendDate!}</@td>
                            <@td>${transfer.receiveDate!}</@td>
                            <@td></@td>
                            <@td><#if statusItem?has_content>${statusItem.description!}</#if></@td>
                        </@tr>
                    </#list>
                
            </@table>    
        </form>
    </#if>
</@section>