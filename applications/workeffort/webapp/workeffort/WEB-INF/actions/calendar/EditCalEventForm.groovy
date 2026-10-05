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
/**
 * SCIPIO: data prep to filter statuses (and/or other)
 */

import java.util.*;

import org.ofbiz.base.util.*;
import org.ofbiz.entity.*;
import org.ofbiz.entity.util.*;
import org.ofbiz.entity.condition.*;

final module = "EditCalEventForm.groovy";

workEffort = context.workEffort;
statusTypeIds = context.statusTypeIds ?: [];

statusItemList = [];

currentStatusItem = null;

calEventMayCancel = false;
calEventSingleStatus = false;


condList = [];
if (statusTypeIds) {
    for(statusTypeId in statusTypeIds) {
        condList.add(EntityCondition.makeCondition("statusTypeId", statusTypeId));
    }
}
statusTypeCond = EntityCondition.makeCondition(condList, EntityOperator.OR);

if (workEffort?.currentStatusId) {
    // WARN: this code does not restrict the statusTypeId (as was done in stock
    // editCalEvent form); so we allow user to mark task closed from calendar...
    
    statusValidChangeList = from("StatusValidChange").where("statusId", workEffort.currentStatusId).cache().queryList();
    
    currentStatusItem = workEffort.getRelatedOne("CurrentStatusItem", false);
    
    condList = [];
    condList.add(EntityCondition.makeCondition("statusId", workEffort.currentStatusId)); // NOTE: must be here AND below
    if (statusValidChangeList) {
        for (statusValidChange in statusValidChangeList) {
            condList.add(EntityCondition.makeCondition("statusId", statusValidChange.statusIdTo));
        }
    }
    cond = EntityCondition.makeCondition(condList, EntityOperator.OR);
    
    cond = EntityCondition.makeCondition(cond, EntityOperator.AND, statusTypeCond);
    
    // current status (bypass statusTypeCond, but NOTE: previous one is also needed!)
    cond = EntityCondition.makeCondition(cond, EntityOperator.OR, 
        EntityCondition.makeCondition("statusId", workEffort.currentStatusId));
    
    //Debug.logError("TEST: " + cond, module);
    statusItemList = from("StatusItem").where(cond).orderBy("sequenceId").cache().queryList();
} else {
    statusItemList = from("StatusItem").where(statusTypeCond).orderBy("sequenceId").cache().queryList();
}

if (statusItemList == null) {
    statusItemList = [];
}

if (workEffort && statusItemList && (currentStatusItem?.statusId != "CAL_CANCELLED")) {
    for(statusItem in statusItemList) {
        if (statusItem.statusId == "CAL_CANCELLED") {
            calEventMayCancel = true;
            break;
        }
    }
}

if (workEffort && currentStatusItem && statusItemList.size() == 1 && currentStatusItem.statusId == statusItemList[0].statusId) {
    calEventSingleStatus = true;
}

context.statusItemList = statusItemList;
context.currentStatusItem = currentStatusItem;
context.calEventMayCancel = calEventMayCancel;

 