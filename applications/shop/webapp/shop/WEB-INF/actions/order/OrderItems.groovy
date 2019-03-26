/**
 * SCIPIO: Specific to orderitems template.
 */

import org.ofbiz.order.order.OrderReadHelper;

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
orderHeader = context.orderHeader;
if (orderHeader?.orderId) {
    orh = context.localOrderReadHelper;
    if (orh == null) {
        orh = new OrderReadHelper(dispatcher, context.locale, orderHeader);
    }
    context.localOrderReadHelper = orh;
    orderItemProdCfgMap = orh.getProductConfigWrappersByOrderItemSeqId(orderItems);
    context.orderItemProdCfgMap = orderItemProdCfgMap;
} else {
    // Only do this if it's not a persisted order...
    cart = context.cart;
    if (cart != null) {
        orderItemAttrMap = cart.makeAllOrderItemAttributesByOrderItemSeqId();
        context.orderItemAttrMap = orderItemAttrMap;
        orderItemProdCfgMap = cart.getProductConfigWrappersByOrderItemSeqId();
        context.orderItemProdCfgMap = orderItemProdCfgMap;
        orderItemSurvResMap = cart.makeAllOrderItemSurveyResponsesByOrderItemSeqId();
        context.orderItemSurvResMap = orderItemSurvResMap;
    }
}
