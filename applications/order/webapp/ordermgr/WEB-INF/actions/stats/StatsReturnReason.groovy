import org.ofbiz.entity.model.DynamicViewEntity
import org.ofbiz.entity.model.ModelViewEntity

import java.math.BigDecimal
import java.math.RoundingMode;
import java.util.*;
import java.sql.Timestamp;
import org.ofbiz.entity.*;
import org.ofbiz.entity.condition.*;
import org.ofbiz.entity.util.*;
import org.ofbiz.base.util.*;

import com.ibm.icu.text.SimpleDateFormat;
import org.ofbiz.base.util.cache.UtilCache;


contentCache = UtilCache.getOrCreateUtilCache("stats.return.reason", 0, 0, 60000, true);
ArrayList processResult() {
    iCount = (context.chartIntervalCount != null) ? context.chartIntervalCount : 1;
    String iScope = context.chartIntervalScope != null ? context.chartIntervalScope : "month"; //day|month|year
    if (iCount <= 0)
        iCount = UtilDateTime.getIntervalDefaultCount(iScope);
    fromDateTimestamp = UtilDateTime.getTimeStampFromIntervalScope(iScope, iCount);
    dateIntervals = UtilDateTime.getPeriodIntervalAndFormatter(iScope, fromDateTimestamp, context.locale, context.timeZone);

    List<EntityCondition> curConditions = UtilMisc.toList(
            EntityCondition.makeCondition("statusId", EntityOperator.NOT_EQUAL, "RETURN_CANCELLED"),
            EntityCondition.makeCondition("lastUpdatedStamp", EntityOperator.GREATER_THAN_EQUAL_TO, dateIntervals.getDateBegin()),
            EntityCondition.makeCondition("lastUpdatedStamp", EntityOperator.LESS_THAN_EQUAL_TO, dateIntervals.getDateEnd())
    );


    EntityCondition ecl = EntityCondition.makeCondition(curConditions, EntityOperator.AND);
    List<GenericValue> returns = from("ReturnItemStats").select("returnReasonId","totalReturnValue","totalQuantity").where(ecl).queryList();

    return returns;
}

List reasonlist = delegator.findList("ReturnReason",null, UtilMisc.toSet("returnReasonId","description"), UtilMisc.toList("sequenceId"),null,true);
def reasonMap = [:];
for(GenericValue reason: reasonlist){
    reasonMap.put(reason.get("returnReasonId"),reason.get("description"));
}

context.orderStats = processResult();
context.returnReasonMap = reasonMap;