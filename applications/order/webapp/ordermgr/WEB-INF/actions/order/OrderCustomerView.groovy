import org.ofbiz.base.util.*;
import org.ofbiz.entity.*;
import org.ofbiz.base.util.cache.UtilCache
import org.ofbiz.entity.condition.EntityCondition
import org.ofbiz.entity.condition.EntityOperator
import org.ofbiz.entity.model.DynamicViewEntity
import org.ofbiz.entity.model.ModelKeyMap
import org.ofbiz.order.order.OrderReadHelper;

orderReadHelper = context.orderReadHelper;
if(!context.orderHeader) {
    if (context.orderId) {
        context.orderHeader = from('OrderHeader').where('orderId', context.orderId).cache(true).queryFirst();
    }
}
if(context.orderHeader){
    if(!orderReadHelper){
        orderReadHelper = new OrderReadHelper(dispatcher, context.locale, context.orderHeader);
    }
    if(!context.displayParty){
        if ("PURCHASE_ORDER".equals(orderType)) {
            displayParty = orderReadHelper.getSupplierAgent();
        } else {
            displayParty = orderReadHelper.getPlacingParty();
        }
    }

    customerId = displayParty.partyId;
    emailAddress = orderReadHelper.getOrderEmailString();




    //SQL magic
    List<String> orderIds = orderReadHelper.getAllCustomerOrderIdsFromOrderEmail();
    Integer orderCount = orderIds.size();
    Integer returnCount = 0;
    BigDecimal orderItemValue = BigDecimal.ZERO;
    BigDecimal returnItemValue = BigDecimal.ZERO;
    Long orderItemCount  = Long.valueOf(0);
    Long returnItemCount = Long.valueOf(0);


    DynamicViewEntity itemEntity = new DynamicViewEntity();
    itemEntity.addMemberEntity("OI", "OrderItem");
    itemEntity.addAlias("OI", "orderId", null, null, null, true, null);
    itemEntity.addAlias("OI", "orderItemCount", "quantity", null, null, false, "count");
    itemEntity.addAlias("OI", "orderItemValue", "unitPrice", null, null, false, "sum");



    expr = EntityCondition.makeCondition("orderId", EntityOperator.IN, orderIds);

    try{
        customerOrderStats = delegator.findListIteratorByCondition(itemEntity,expr,null,null,null,null);
        if (customerOrderStats != null) {
            while (n = customerOrderStats.next()) {
                BigDecimal nv = n.getBigDecimal("orderItemValue");
                orderItemValue = orderItemValue.add(nv);
                orderItemCount += n.getLong("orderItemCount");
            }
        }
    }catch(Exception e){
        e;
    }

    DynamicViewEntity retEntity = new DynamicViewEntity();
    retEntity.addMemberEntity("OI", "ReturnItem");
    retEntity.addAlias("OI", "orderId", null, null, null, true, null);
    retEntity.addAlias("OI", "returnId", null, null, null, true, null);
    retEntity.addAlias("OI", "returnTypeId", null, null, null, true, null);
    retEntity.addAlias("OI", "returnItemCount", "returnQuantity", null, null, false, "count");
    retEntity.addAlias("OI", "returnItemValue", "returnPrice", null, null, false, "sum");

    try{
        returnStats = delegator.findListIteratorByCondition(retEntity,EntityCondition.makeCondition("orderId", EntityOperator.IN, orderIds),null,null,null,null);
        if (returnStats != null) {
            returnStatsList = returnStats.getCompleteList()
            returnCount = returnStatsList.size();
            for(GenericValue n : returnStatsList){
                BigDecimal nv = n.getBigDecimal("returnItemValue");
                returnItemValue = returnItemValue.add(nv);
                returnItemCount += n.getLong("returnItemCount");
            }
        }
    }catch(Exception e){
        e;
    }


    context.orderCount = orderCount;
    context.orderItemValue = orderItemValue;
    context.orderItemCount = orderItemCount;
    context.returnCount = returnCount;
    context.returnItemValue = returnItemValue;
    context.returnItemCount = returnItemCount;
    if(BigDecimal.ZERO.compareTo(returnItemCount) == 0){
        context.returnItemRatio = BigDecimal.ZERO;
    }else{
        context.returnItemRatio =  (returnItemCount / orderItemCount).setScale(2,BigDecimal.ROUND_HALF_UP);
    }
    //context.customerSupportStats = customerSupportStats;


}