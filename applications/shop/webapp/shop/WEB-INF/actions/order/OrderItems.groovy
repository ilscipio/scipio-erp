/**
 * SCIPIO: Specific to orderitems template.
 */

// SCIPIO: Each item may have downloadable files; make them available here (by productId)
productDownloads = [:];
orderItems = context.orderItems;
if (orderItems) {
    for (orderItem in orderItems) {
        downloadProductContentAndInfoList = from("ProductContentAndInfo").where("productId", orderItem.productId, "productContentTypeId", "DIGITAL_DOWNLOAD").orderBy("sequenceNum ASC").cache(true).queryList();
        if (downloadProductContentAndInfoList) {
            productDownloads[orderItem.productId] = downloadProductContentAndInfoList;
        }
    }
}
context.productDownloads = productDownloads;

// SCIPIO: OrderItemAttributes and ProductConfigWrappers
// Only do this if it's not a persisted order...
cart = context.cart;
if (cart != null && !(context.orderHeader?.orderId)) {
    orderItemAttrMap = cart.makeAllOrderItemAttributesByOrderItemSeqId();
    context.orderItemAttrMap = orderItemAttrMap;
    orderItemProdCfgMap = cart.getProductConfigWrappersByOrderItemSeqId();
    context.orderItemProdCfgMap = orderItemProdCfgMap;
}
