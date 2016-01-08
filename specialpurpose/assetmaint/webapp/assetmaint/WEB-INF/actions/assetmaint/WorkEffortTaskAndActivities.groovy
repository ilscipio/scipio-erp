import javolution.util.FastMap

import org.ofbiz.base.util.Debug
import org.ofbiz.base.util.UtilMisc

service = context.workEffortService;
Map workEffortMap = FastMap.newInstance();
if (service == "getWorkEffortAssignedTasks" || service == "getWorkEffortCompletedTasks") {	 
	workEffortMap = dispatcher.runSync(service, UtilMisc.toMap("createdPeriod", context.createdPeriod));	
	Debug.log("workEffortMap =====> " + workEffortMap);
	context.tasks = workEffortMap.get("tasks");	
} else if (service == "getWorkEffortAssignedActivities" || service == "getWorkEffortCompletedActivities") {
	workEffortMap = dispatcher.runSync(service, UtilMisc.toMap("createdPeriod", context.createdPeriod));
	context.activities = workEffortMap.get("activities");
}