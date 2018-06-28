import org.ofbiz.entity.condition.EntityCondition
import org.ofbiz.entity.condition.EntityOperator
import org.ofbiz.entity.util.EntityQuery

context.accountingGLs = EntityQuery.use(delegator).from("GlAccount").where(EntityCondition.makeCondition("parentGlAccountId", EntityOperator.EQUALS, null)).queryList();