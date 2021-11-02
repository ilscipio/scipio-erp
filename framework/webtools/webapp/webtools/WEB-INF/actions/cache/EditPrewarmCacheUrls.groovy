import org.ofbiz.entity.GenericValue
import org.ofbiz.entity.util.*;
import org.ofbiz.entity.condition.*;

List<GenericValue> websites = EntityQuery.use(delegator).select("webSiteId").from("WebSite").where(EntityCondition.makeCondition("productStoreId", EntityJoinOperator.NOT_EQUAL, null)).orderBy("siteName").queryList()
context.websites = websites;