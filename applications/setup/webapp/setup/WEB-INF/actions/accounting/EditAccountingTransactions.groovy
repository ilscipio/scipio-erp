import org.ofbiz.entity.util.EntityQuery

context.datevDataCategories = EntityQuery.use(delegator).from("DatevDataCategory").cache(true).queryList();