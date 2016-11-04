import javolution.util.FastList
import javolution.util.FastMap

import org.ofbiz.base.util.Debug
import org.ofbiz.base.util.UtilMisc
import org.ofbiz.entity.util.EntityUtil

service = context.workEffortService;
List workEffortList = FastList.newInstance();
if (service == "getWorkEffortAssignedTasks" || service == "getWorkEffortCompletedTasks") {     
    workEffortMap = dispatcher.runSync(service, UtilMisc.toMap("createdPeriod", context.createdPeriod, "userLogin", context.userLogin));    
    workEffortList = workEffortMap.get("tasks");
} else if (service == "getWorkEffortAssignedActivities" || service == "getWorkEffortCompletedActivities") {
    workEffortMap = dispatcher.runSync(service, UtilMisc.toMap("createdPeriod", context.createdPeriod, "userLogin", context.userLogin));
    workEffortList = workEffortMap.get("activities");    
}

result = FastList.newInstance();
workEffortList.each { workEffortPartyAssignment ->
    Map workEffort = FastMap.newInstance();
    workEffort.put("workEffortId", workEffortPartyAssignment.workEffortId);
    workEffort.put("createdDate", workEffortPartyAssignment.createdDate);
    workEffort.put("partyId", workEffortPartyAssignment.partyId);
    statusItem = delegator.findOne("StatusItem", ["statusId" : workEffortPartyAssignment.currentStatusId], true);
    if (statusItem)
        workEffort.put("statusDescription", statusItem.description);
    workEffortFixedAssetAssign = EntityUtil.getFirst(delegator.findByAnd("WorkEffortFixedAssetAssign", ["workEffortId" : workEffort.workEffortId, "fromDate" : workEffort.createdDate], null, true));
    fixedAsset = workEffortFixedAssetAssign?.getRelatedOne("FixedAsset", true);
    if (fixedAsset) {
        workEffort.put("assetName", fixedAsset.fixedAssetName);
        fixedAssetType = fixedAsset.getRelatedOne("FixedAssetType", true);
        if (fixedAssetType)
            workEffort.put("maintenanceType", fixedAssetType.description);
    }
    result.add(workEffort);
}
context.workEfforts = result;