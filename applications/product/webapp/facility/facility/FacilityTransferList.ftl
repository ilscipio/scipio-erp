<#if fromTransfers?has_content>
    <#assign transferList = fromTransfers />
<#elseif toTransfers?has_content>
    <#assign transferList = toTransfers />
</#if>

<@section menuContent=menuContent>
    <#if transferList?has_content>
        <form action="<@ofbizUrl>UpdateFacilityLocation</@ofbizUrl>" method="post">
        <input type="hidden" name="facilityId" value="${facilityId!}" />        
        
            <@table type="data-list" autoAltRows=true responsive=true scrollable=true> <#-- orig: class="basic-table hover-bar" --> <#-- orig: cellspacing="0" -->
                <#-- Header Begins -->
                <@thead>
                    <@tr class="header-row-2">
                        <@th>${uiLabelMap.ProductInventoryTransfer}</@th>
                        <@th>${uiLabelMap.ProductInventoryItemId}</@th>
                        <@th>${uiLabelMap.ProductFacilityId}</@th>
                        <@th>${uiLabelMap.ProductFacilityName}</@th>
                        <@th>${uiLabelMap.ProductLocationSeqIdTo}</@th>
                        <@th>${uiLabelMap.ProductId}</@th>
                        <@th>${uiLabelMap.ProductProductName}</@th>
                        <@th>${uiLabelMap.ProductSerialNumber}</@th>
                        <@th>${uiLabelMap.ProductLocationSeqIdFrom}</@th>
                        <@th>${uiLabelMap.ProductAtpQoh}</@th>
                        <@th>${uiLabelMap.CommonSendDate}</@th>
                        <@th>${uiLabelMap.CommonReceiveDate}</@th>
                        <@th>${uiLabelMap.CommonStatus}</@th>
        
                        <#-- <@th>${uiLabelMap.CommonUpdate}</@th>
                        <@th>${uiLabelMap.CommonDelete}</@th> -->
                    </@tr>
                </@thead>
                <#-- Header Ends-->
                <@tbody>
                    <#list fromTransfers as transfer>
                        <#assign inventoryItem = delegator.findOne("InventoryItem", { "inventoryItemId" : transfer.inventoryItemId }, false) />
                        <#assign product = delegator.findOne("Product", { "productId" : transfer.productId}, false)!>
                        <#assign statusItem = delegator.findOne("StatusItem", { "statusId" : inventoryItem.statusId }, true) />
                        <@tr>
                            <@td>${transfer.inventoryTransferId}</@td>
                            <@td>${transfer.inventoryItemId}</@td>
                            <@td>${transfer.facilityId}</@td>
                            <@td>${transfer.facilityName!}</@td>
                            <@td>${transfer.locationSeqId}</@td>
                            <@td><a href="<@ofbizUrl>${product.productId}</@ofbizUrl>">${product.productId}</a></@td>
                            <@td>${product.productName!}</@td>S                            
                            <@td>${inventoryItem.serialName!}</@td>
                            <@td>${inventoryItem.availableToPromiseTotal!}/${inventoryItem.quantityOnHandTotal!}</@td>
                            <@td>${inventoryItem.sendDate}</@td>
                            <@td>${inventoryItem.recieveDate}</@td>
                            <@td>${inventoryItem.sendDate}</@td>
                            <@td>${statusItem.description}</@td>
                        </@tr>
                    </#list>
                </@tbody>
            </@table>    
        </form>
    </#if>
</@section>