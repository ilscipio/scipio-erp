import java.sql.Timestamp

import org.ofbiz.base.util.Debug
import org.ofbiz.base.util.UtilDateTime
import org.ofbiz.base.util.UtilMisc
import org.ofbiz.base.util.UtilProperties
import org.ofbiz.base.util.UtilRandom
import org.ofbiz.entity.*
import org.ofbiz.entity.condition.EntityCondition
import org.ofbiz.entity.condition.EntityJoinOperator
import org.ofbiz.entity.condition.EntityOperator
import org.ofbiz.entity.util.*
import org.ofbiz.service.ServiceUtil

import com.ilscipio.scipio.ce.demoSuite.dataGenerator.service.DataGeneratorGroovyBaseScript;

public class TrackingCodeOrderData extends DataGeneratorGroovyBaseScript {
    private static final String module = "TrackingCodeOrderData.groovy";
    final String DEFAULT_WEBAPP_NAME = "shop";

    TrackingCodeOrderData() {
        Debug.logInfo("-=-=-=- DEMO DATA CREATION SERVICE - TRACKING ORDER DATA-=-=-=-", module);
    }

    public void init() {
        trackingCodeTypeList = delegator.findAll("TrackingCodeType",  true);
        conditionList = EntityCondition.makeCondition(
                EntityCondition.makeCondition("orderDate", EntityOperator.GREATER_THAN_EQUAL_TO, context.minDate),
                EntityJoinOperator.AND,
                EntityCondition.makeCondition("orderDate", EntityOperator.LESS_THAN, context.maxDate));
        orderHeaderList = from("OrderHeader").where(conditionList).queryList();
        context.orderHeaderList = orderHeaderList;
        context.trackingCodeTypeList = trackingCodeTypeList;
    }

    List prepareData(int index) throws Exception {
        List<GenericValue> toBeStored = new LinkedList<GenericValue>();
        if (context.trackingCodeTypeList && context.orderHeaderList) {
            trackingCodeType =  context.trackingCodeTypeList.get(UtilRandom.random(context.trackingCodeTypeList));
            orderHeader = context.orderHeaderList.get(UtilRandom.random(orderHeaderList));
            trackingCodeList = delegator.findByAnd("TrackingCode", null, null, false);
            trackingCode = trackingCodeList.get(UtilRandom.random(trackingCodeList));

            GenericValue trackingCodeOrder = delegator.makeValue("TrackingCodeOrder",
                    UtilMisc.toMap("trackingCodeId", trackingCode.trackingCodeId, "orderId", orderHeader.orderId, "trackingCodeTypeId", trackingCodeType.trackingCodeTypeId));
            toBeStored.add(trackingCodeOrder);
        }
        return toBeStored;
    }
}