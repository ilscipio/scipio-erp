/**
 * SCIPIO: SETUP interactive Fiscal Periods data prep.
 */

import org.ofbiz.base.util.*;
import org.ofbiz.entity.GenericValue
import org.ofbiz.entity.condition.*;
import org.ofbiz.entity.util.*;
 
final module = "EditFiscalPeriods.groovy";

orgPartyId = context.orgPartyId;

EntityConditionList conds = EntityCondition.makeCondition([
    EntityCondition.makeCondition("parentPeriodId", EntityOperator.EQUALS, null),
    EntityCondition.makeCondition("organizationPartyId", EntityOperator.EQUALS, orgPartyId),
    EntityCondition.makeCondition([
           EntityCondition.makeCondition("isClosed", EntityOperator.EQUALS, "N"),
           EntityCondition.makeCondition("isClosed", EntityOperator.NOT_EQUAL, null)
    ],
    EntityOperator.OR)],
    EntityOperator.AND
);

List<GenericValue> timePeriods = EntityQuery.use(delegator).from("CustomTimePeriod").where(conds).filterByDate().queryList();
for (GenericValue timePeriod in timePeriods) {
    Debug.log("time period id =====>  " + timePeriod.customTimePeriodId);    
}

context.timePeriods = timePeriods;