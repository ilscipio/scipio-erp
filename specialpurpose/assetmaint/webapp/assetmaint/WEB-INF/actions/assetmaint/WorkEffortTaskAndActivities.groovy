/*
 * Scipio Commerce
 * Copyright (C) Ilscipio GmbH
 *
 * This file is part of Scipio Commerce. Scipio Commerce is free software: you
 * can redistribute it and modify it under the terms of the GNU Affero General
 * Public License, version 3, as published by the Free Software Foundation.
 * Scipio Commerce is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 * FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License
 * for more details. You should have received a copy of the license with this
 * work (file LICENSE). If not, see <https://www.gnu.org/licenses/agpl-3.0.html>.
 * A commercial license is available from Ilscipio GmbH.
 *
 * SPDX-License-Identifier: AGPL-3.0-only
 */

import org.ofbiz.base.util.Debug
import org.ofbiz.base.util.UtilMisc
import org.ofbiz.entity.util.EntityUtil

service = context.workEffortService;
List workEffortList = [];
if (service == "getWorkEffortAssignedTasks" || service == "getWorkEffortCompletedTasks") {     
    workEffortMap = dispatcher.runSync(service, UtilMisc.toMap("createdPeriod", context.createdPeriod, "userLogin", context.userLogin));    
    workEffortList = workEffortMap.get("tasks");
} else if (service == "getWorkEffortAssignedActivities" || service == "getWorkEffortCompletedActivities") {
    workEffortMap = dispatcher.runSync(service, UtilMisc.toMap("createdPeriod", context.createdPeriod, "userLogin", context.userLogin));
    workEffortList = workEffortMap.get("activities");    
}

result = [];
workEffortList.each { workEffortPartyAssignment ->
    Map workEffort = [:];
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