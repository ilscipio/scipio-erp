import org.ofbiz.base.util.*;
import org.ofbiz.entity.*;
import org.ofbiz.base.util.cache.UtilCache
import org.ofbiz.entity.condition.EntityCondition
import org.ofbiz.entity.condition.EntityOperator
import org.ofbiz.entity.model.DynamicViewEntity
import org.ofbiz.entity.model.ModelKeyMap
import org.ofbiz.order.order.OrderReadHelper

import java.sql.Timestamp;

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
        if ("PURCHASE_ORDER".equals(context.orderHeader.orderTypeId)) {
            displayParty = orderReadHelper.getSupplierAgent();
        } else {
            displayParty = orderReadHelper.getPlacingParty();
        }
    }

    customerId = displayParty.partyId;
    emailAddress = orderReadHelper.getOrderEmailString();


    //SQL magic
    List<String> orderIds = orderReadHelper.getAllCustomerOrderIdsFromOrderEmail(UtilMisc.toList("ORDER_CANCELLED","ORDER_REJECTED"));
    Integer orderCount = orderIds.size();
    Integer returnCount = 0;
    BigDecimal orderItemValue = BigDecimal.ZERO;
    BigDecimal returnItemValue = BigDecimal.ZERO;
    BigDecimal orderItemCount  = BigDecimal.ZERO;
    BigDecimal returnItemCount = BigDecimal.ZERO;
    int rfmRecency = 0;


    DynamicViewEntity itemEntity = new DynamicViewEntity();
    itemEntity.addMemberEntity("OI", "OrderItem");
    itemEntity.addAlias("OI", "orderId", null, null, null, true, null);
    itemEntity.addAlias("OI", "statusId", null, null, null, true, null);
    itemEntity.addAlias("OI", "orderItemCount", "quantity", null, null, false, "sum");
    itemEntity.addAlias("OI", "orderItemValue", "unitPrice", null, null, false, "sum");
    exprListStatus = []
    exprListStatus.add(EntityCondition.makeCondition("orderId", EntityOperator.IN, orderIds));
    exprListStatus.add(EntityCondition.makeCondition("statusId", EntityOperator.IN, ["ITEM_COMPLETED","ITEM_APPROVED"]));

    EntityCondition andCond = EntityCondition.makeCondition(exprListStatus, EntityOperator.AND);

    try{
        customerOrderStats = delegator.findListIteratorByCondition(itemEntity,andCond,null,null,null,null);
        Timestamp lastOrderDate;

        if (customerOrderStats != null) {
            int cIndex = 0;
            orderStatsList = customerOrderStats.getCompleteList()
            for(GenericValue n : orderStatsList){
                if(cIndex==0){
                    GenericValue o = delegator.findOne("OrderHeader", UtilMisc.toMap("orderId", n.getString("orderId")), true);
                    lastOrderDate = o.getTimestamp("orderDate");
                    rfmRecency = ( System.currentTimeMillis() - lastOrderDate.getTime()) / (1000 * 60 * 60 * 24);

                }
                BigDecimal nv = n.getBigDecimal("orderItemValue");
                orderItemValue = orderItemValue.add(nv);
                orderItemCount = orderItemCount.add(n.getBigDecimal("orderItemCount"));
                cIndex+=1;
            }
        }
    }catch(Exception e){
        e;
    }

    DynamicViewEntity retEntity = new DynamicViewEntity();
    retEntity.addMemberEntity("OI", "ReturnItem");
    retEntity.addAlias("OI", "orderId", null, null, null, true, null);
    retEntity.addAlias("OI", "returnId", null, null, null, true, null);
    retEntity.addAlias("OI", "statusId", null, null, null, true, null);
    retEntity.addAlias("OI", "returnTypeId", null, null, null, true, null);
    retEntity.addAlias("OI", "returnItemCount", "returnQuantity", null, null, false, "sum");
    retEntity.addAlias("OI", "returnItemValue", "returnPrice", null, null, false, "sum");

    try{

        exprListStatus = []
        exprListStatus.add(EntityCondition.makeCondition("orderId", EntityOperator.IN, orderIds));
        exprListStatus.add(EntityCondition.makeCondition("statusId", EntityOperator.NOT_EQUAL, "RETURN_CANCELLED"));

        returnStats = delegator.findListIteratorByCondition(retEntity,EntityCondition.makeCondition(exprListStatus, EntityOperator.AND),null,null,null,null);
        if (returnStats != null) {
            returnStatsList = returnStats.getCompleteList()
            returnCount = returnStatsList.size();
            for(GenericValue n : returnStatsList){
                BigDecimal nv = n.getBigDecimal("returnItemValue");
                returnItemValue = returnItemValue.add(nv);
                returnItemCount = returnItemCount.add(n.getBigDecimal("returnItemCount"));
            }
        }
    }catch(Exception e){
        e;
    }



    context.orderCount = orderCount;
    context.orderItemValue = orderItemValue;
    context.orderItemCount = orderItemCount.setScale(0,BigDecimal.ROUND_HALF_UP);
    context.returnCount = returnCount;
    context.returnItemValue = returnItemValue;
    context.returnItemCount = returnItemCount.setScale(0,BigDecimal.ROUND_HALF_UP);
    if(BigDecimal.ZERO.compareTo(returnItemCount) == 0){
        context.returnItemRatio = BigDecimal.ZERO;
    }else{
        context.returnItemRatio =  (returnItemCount / orderItemCount).setScale(2,BigDecimal.ROUND_HALF_UP);
    }
    context.rfmRecency=rfmRecency;
    rfmFrequency = orderCount;
    context.rfmFrequency=rfmFrequency;
    rfmMonetary = orderItemValue.subtract(returnItemValue);
    context.rfmMonetary = rfmMonetary;

    int rfmRecencyScore = 0;
    int rfmFrequencyScore = 0;
    int rfmMonetaryScore = 0;

    if(rfmRecency <= UtilProperties.getPropertyAsInteger("order","order.rfm.recency.1",30)){
        rfmRecencyScore = 1;
    }else if(rfmRecency <= UtilProperties.getPropertyAsInteger("order","order.rfm.recency.2",90)){
        rfmRecencyScore = 2;
    }else if(rfmRecency <= UtilProperties.getPropertyAsInteger("order","order.rfm.recency.3",375)){
        rfmRecencyScore = 3;
    }else{
        rfmRecencyScore = 4;
    }

    if(rfmFrequency >= UtilProperties.getPropertyAsInteger("order","order.rfm.frequency.1",50)){
        rfmFrequencyScore = 1;
    }else if(rfmFrequency >= UtilProperties.getPropertyAsInteger("order","order.rfm.frequency.2",30)){
        rfmFrequencyScore = 2;
    }else if(rfmFrequency >= UtilProperties.getPropertyAsInteger("order","order.rfm.frequency.3",20)){
        rfmFrequencyScore = 3;
    }else{
        rfmFrequencyScore = 4;
    }

    if(rfmMonetary.compareTo(new BigDecimal(UtilProperties.getPropertyAsInteger("order","order.rfm.monetary.1",250))) == 1){
        rfmMonetaryScore = 1;
    }else if(rfmMonetary.compareTo(new BigDecimal(UtilProperties.getPropertyAsInteger("order","order.rfm.monetary.2",100))) == 1){
        rfmMonetaryScore = 2;
    }else if(rfmMonetary.compareTo(new BigDecimal(UtilProperties.getPropertyAsInteger("order","order.rfm.monetary.3",50))) == 1){
        rfmMonetaryScore = 3;
    }else{
        rfmMonetaryScore = 4;
    }

    context.rfmRecencyScore=rfmRecencyScore;
    context.rfmFrequencyScore=rfmFrequencyScore;
    context.rfmMonetaryScore=rfmMonetaryScore;
}