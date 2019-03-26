import org.ofbiz.base.util.UtilMisc
import org.ofbiz.entity.condition.EntityCondition
import org.ofbiz.entity.condition.EntityJoinOperator
import org.ofbiz.entity.util.EntityQuery

acctgTypeConds = UtilMisc.toList(
        EntityCondition.makeCondition("parentTypeId", EntityJoinOperator.EQUALS, null),
        EntityCondition.makeCondition("parentTypeId", EntityJoinOperator.EQUALS, ""));
    
context.acctgParentTransTypes = EntityQuery.use(delegator).from("AcctgTransType").cache(true).where(acctgTypeConds, EntityJoinOperator.OR).queryList();
context.acctgParentEntryTransTypes = EntityQuery.use(delegator).from("AcctgTransEntryType").cache(true).where(acctgTypeConds, EntityJoinOperator.OR).queryList();

context.datevDataCategories = EntityQuery.use(delegator).from("DatevDataCategory").cache(true).queryList();