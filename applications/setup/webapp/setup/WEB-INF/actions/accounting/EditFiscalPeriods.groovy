/**
 * SCIPIO: SETUP interactive Fiscal Periods data prep.
 */

import org.ofbiz.base.util.*;
import org.ofbiz.entity.GenericValue
import org.ofbiz.entity.condition.*;
import org.ofbiz.entity.util.*;

import com.ilscipio.scipio.treeMenu.jsTree.JsTreeHelper
 
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

List<GenericValue> timePeriods = EntityQuery.use(delegator).select("customTimePeriodId").from("CustomTimePeriod").where(conds).queryList();

treeMenuHelper = new JsTreeHelper();
treeMenuData = [];
for (customTimePeriod in timePeriods) {
    result = dispatcher.runSync("buildCustomPeriodTree", ["customTimePeriodId": customTimePeriod.customTimePeriodId]);
    if (result?.treeList) {
        treeMenuData = treeMenuData + result.treeList;
    }
}
treeMenuHelper.addAll(treeMenuData);
context.treeMenuData = treeMenuHelper;


for (GenericValue timePeriod in timePeriods) {
    Debug.log("time period id =====>  " + timePeriod.customTimePeriodId);    
}