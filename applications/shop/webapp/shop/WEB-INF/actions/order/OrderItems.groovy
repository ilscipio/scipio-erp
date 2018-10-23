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


