import org.ofbiz.entity.condition.EntityCondition
import org.ofbiz.entity.condition.EntityJoinOperator
import org.ofbiz.entity.condition.EntityOperator
import org.ofbiz.entity.util.EntityQuery
import org.ofbiz.minilang.method.entityops.EntityOperation

availableCustRequestTypes = [
    "RF_BUGFIX",
    "RF_FEATURE",
    "RF_INFO",
    "RF_PROPOSAL",
    "RF_SUPPORT"
];

custRequestTypes = EntityQuery.use(delegator).from("CustRequestType").where(EntityCondition.makeCondition("custRequestTypeId", EntityOperator.IN, availableCustRequestTypes)).queryList();
//custRequestStatuses = EntityQuery.use(delegator).from("CustRequestStatus").queryList();

context.custRequestTypes = custRequestTypes;
//context.custRequestStatuses = custRequestStatuses;