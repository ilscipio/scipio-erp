import org.ofbiz.security.Security;

import org.ofbiz.webapp.stats.*;
import javolution.util.FastList;
import javolution.util.FastMap;
import java.util.Date;
import java.text.SimpleDateFormat;
import java.sql.Timestamp;
import org.ofbiz.entity.*;
import org.ofbiz.entity.condition.*;
import org.ofbiz.entity.util.*;
import org.ofbiz.base.util.*;
import org.ofbiz.base.util.Debug;
import org.ofbiz.entity.model.DynamicViewEntity;
import org.ofbiz.entity.model.ModelViewEntity.ComplexAlias;
import org.ofbiz.entity.model.ModelViewEntity.ComplexAliasField;


delegator = request.getAttribute("delegator");

// Time definition
//startDate = UtilDateTime.getDayStart(nowTimestamp, timeZone, locale).; // Today (Begin)
//startDate = UtilDateTime.getWeekStart(nowTimestamp, timeZone, locale); // This week (Begin)
startDate = UtilDateTime.getMonthStart(nowTimestamp, timeZone, locale); // This month (Begin)
//startDate = UtilDateTime.getMonthStart(nowTimestamp,0,-1,timeZone, locale);  // Last Month (Begin)
//startDate = UtilDateTime.getYearStart(nowTimestamp, timeZone, locale);  // This Year (Begin)

//endDate = UtilDateTime.getDayEnd(startDate, timeZone, locale);	// Today (End)
//endDate = UtilDateTime.getWeekEnd(startDate, timeZone, locale); // This week (End)
endDate = UtilDateTime.getMonthEnd(startDate, timeZone, locale); // This month (End)
// endDate = UtilDateTime.getMonthEnd(startDate, timeZone, locale); // Last month (End)
//endDate = UtilDateTime.getYearEnd(startDate, timeZone, locale); // This Year (End)


Date toDate(Timestamp timestamp) {
	long milliseconds = timestamp.getTime() + (timestamp.getNanos() / 1000000);
	return new Date(milliseconds);
}

Map processResult(List resultList) {
	Map dateMap = new TreeMap<Date, Object>();
	Map resultMap = [:];
    def startD = toDate(startDate);
	def endD = toDate(endDate);
	
	// create new ordered Map of all upcoming dates
	startD.upto(endD){
		Map newMap = [:];
		newMap.put("count", 0);
		dateMap.put(it,newMap);
	}
	
	// Update map with our values
	resultList.each {
		Map p = it;
		String year = p.get("year");
		String month = p.get("month");
		String day = p.get("day");
		Map newMap = [:];
		newMap.put("count", p.get("contentIdCount"));
		String date = year + "/" + month + "/" + day;
		SimpleDateFormat formatter = new SimpleDateFormat("yyyy/MM/dd");
		dateMap.put(formatter.parse(date),newMap);
    }
	
	// prepare Map for freemarker
	dateMap.each{k, v ->
		resultMap.put(((Date)k).format("dd-MM-yyyy"),v);
	}
	
    return resultMap;
}

List getDataFromDB(String yearFunction, String monthFunction, String dayFunction){
	DynamicViewEntity dve = new DynamicViewEntity();
					dve.addMemberEntity("SH", "ServerHit");
					dve.addAlias("SH", "contentId", null, null, null, Boolean.TRUE, null);
					dve.addAlias("SH", "hitStartDateTime", null, null, null, Boolean.TRUE, null);
					dve.addAlias("SH", "hitTypeId", null, null, null, Boolean.TRUE, null);
					dve.addAlias("SH", "referrerUrl", null, null, null, Boolean.FALSE, null);
					
					// Split date in order to group at a later stage
					dve.addAlias("SH", "year", "hitStartDateTime",  null, null, Boolean.TRUE, yearFunction);
					dve.addAlias("SH", "month", "hitStartDateTime",  null, null, Boolean.TRUE, monthFunction);
					dve.addAlias("SH", "day", "hitStartDateTime",  null, null, Boolean.TRUE, dayFunction);
					dve.addAlias("SH", "contentIdCount", "contentId", null, null, null, "count");
	
	// Daily results
	EntityCondition ecl = EntityCondition.makeCondition([
							EntityCondition.makeCondition("hitTypeId", EntityOperator.EQUALS, "REQUEST"),
							EntityCondition.makeCondition("hitStartDateTime", EntityOperator.GREATER_THAN_EQUAL_TO, startDate),
							EntityCondition.makeCondition("hitStartDateTime", EntityOperator.LESS_THAN_EQUAL_TO, endDate)],
						EntityOperator.AND);
	return select("contentIdCount","year","month","day").from(dve).where(ecl).queryList();
}

dataList = [];
try{
	// For most SQL Databases use this
	dataList = getDataFromDB("extract-year","extract-month","extract-day");
}catch(Exception e){
	//Fallback for derby
	dataList = getDataFromDB("year","month","day");
}

context.chartData = processResult(dataList);