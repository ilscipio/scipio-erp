salesOrderItemFacts = null;

if (parameters.salesChannel) {
    salesOrderItemFacts = delegator.findByAnd("SalesOrderItemFact", UtilMisc.toMap("saleChannel", parameters.salesChannel), null, false);
} else {
    salesOrderItemFacts = delegator.findList("SalesOrderItemFact", null, null, null, null, false);
}

context.salesOrderItemFacts = salesOrderItemFacts;

for (item in salesOrderItemFacts) {
    Debug.log("item ================> " + item);
}

//totalRow = 0;
//countOfRow = 0;
//if (salesOrderItemFacts) {
//    totalRow = salesOrderItemFacts.size();
//}
//    
//
//                    
//    if (countOfRow == totalRow - 1) return false;
//    
//    salesOrderItemFact = salesOrderItemFacts.get(countOfRow);
//    
//    row["orderId"] = salesOrderItemFact.getString("orderId");
//    row["orderItemSeqId"] = salesOrderItemFact.getString("orderItemSeqId");
//    row["orderStatus"] = salesOrderItemFact.getString("orderStatus");
//    row["quantity"] = salesOrderItemFact.getString("quantity");
//    row["saleChannel"] = salesOrderItemFact.getString("saleChannel");
//    row["extNetAmount"] = salesOrderItemFact.getString("extNetAmount");