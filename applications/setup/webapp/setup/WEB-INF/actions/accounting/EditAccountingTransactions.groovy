import org.ofbiz.entity.condition.EntityCondition
import org.ofbiz.entity.condition.EntityJoinOperator
import org.ofbiz.entity.util.EntityQuery

// TODO: Add this to condition: where("parentTypeId IS NULL OR parentTypeId = ''")
context.acctgTransTypes = EntityQuery.use(delegator).from("AcctgTransType").cache(true).queryList();

context.datevDataCategories = EntityQuery.use(delegator).from("DatevDataCategory").cache(true).queryList();