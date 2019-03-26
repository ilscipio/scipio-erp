<@section menuContent=menuContent>
    <#if invalidProductId??>
        <@commonMsg type="error">${invalidProductId}</@commonMsg>
    </#if>    
    <@section title=uiLabelMap.ProductReceiveItem>
        <form name="receiveInventoryItem" method="post" action="<@pageUrl>ReceiveInventory</@pageUrl>">
            <input type="hidden" name="facilityId" value="${requestParameters.facilityId!}"/>
            <#-- <input type="hidden" name="initialSelected" value="Y"/> -->
            <@field type="lookup" label=uiLabelMap.ProductPurchaseOrderNumber tooltip=uiLabelMap.ProductLeaveSingleProductReceiving value=(requestParameters.purchaseOrderId!) formName="receiveInventoryItem" name="purchaseOrderId" id="purchaseOrderId" fieldFormName="LookupPurchaseOrderHeaderAndShipInfo"/>
            <@field type="lookup" label=uiLabelMap.ProductProductId tooltip=uiLabelMap.ProductLeaveEntirePoReceiving value=(requestParameters.productId!) formName="receiveInventoryItem" name="productId" id="productId" fieldFormName="LookupProduct"/>
            <@field type="submit" submitType="link" href="javascript:document.receiveInventoryItem.submit();" class="+${styles.link_run_sys!} ${styles.action_receive!}" text=uiLabelMap.ProductReceiveProduct />
        </form>
    </@section>
</@section>