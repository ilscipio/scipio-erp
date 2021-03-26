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
    if (request.getAttribute("orderId")) {
        context.orderHeader = from('OrderHeader').where('orderId', request.getAttribute("orderId")).cache(true).queryFirst();
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
    Map<String,Object> orderStats = orderReadHelper.getCustomerOrderMktgStats(orderIds,true,["ITEM_COMPLETED","ITEM_APPROVED"],["RETURN_CANCELLED"]);

    context.orderCount = orderStats.orderCount;
    context.orderItemValue = orderStats.orderItemValue;
    context.orderItemCount = orderStats.orderItemCount;
    context.returnCount = orderStats.returnCount;
    context.returnItemValue = orderStats.returnItemValue;
    context.returnItemCount = orderStats.returnItemCount;
    context.returnItemRatio = orderStats.returnItemRatio;
    context.rfmRecency=orderStats.rfmRecency;
    context.rfmFrequency=orderStats.rfmFrequency;
    context.rfmMonetary = orderStats.rfmMonetary;
    context.rfmRecencyScore=orderStats.rfmRecencyScore;
    context.rfmFrequencyScore=orderStats.rfmFrequencyScore;
    context.rfmMonetaryScore=orderStats.rfmMonetaryScore;
}