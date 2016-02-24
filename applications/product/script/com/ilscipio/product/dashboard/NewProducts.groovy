import javolution.util.FastList

import org.ofbiz.base.util.Debug
import org.ofbiz.base.util.UtilDateTime
import org.ofbiz.entity.condition.EntityCondition
import org.ofbiz.entity.condition.EntityOperator


dateIntervals = UtilDateTime.getPeriodInterval("month", null, locale, timeZone);

Debug.log("dateItervals =========> " + dateIntervals);

List introductionDateAndExprs = FastList.newInstance();
introductionDateAndExprs.add(EntityCondition.makeCondition("introductionDate", EntityOperator.GREATER_THAN_EQUAL_TO, dateIntervals["dateBegin"]));
introductionDateAndExprs.add(EntityCondition.makeCondition("introductionDate", EntityOperator.LESS_THAN, dateIntervals["dateEnd"]));

newProducts = from("Product").where(introductionDateAndExprs).queryList();


newProducts.each { product ->
    Debug.log("new product id ===========> " + product.productId);
} 

context.products = newProducts;