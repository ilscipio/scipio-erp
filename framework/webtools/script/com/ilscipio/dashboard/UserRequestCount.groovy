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

import org.ofbiz.base.util.*
import org.ofbiz.base.util.UtilDateTime.TimeInterval
import org.ofbiz.entity.*
import org.ofbiz.entity.condition.*
import org.ofbiz.entity.util.*
import org.ofbiz.base.util.cache.UtilCache

final module = "UserRequestCount.groovy";

contentCache = UtilCache.getOrCreateUtilCache("dashboard.webtools", 0, 0, 60000, true);
cacheId = "webtools_user_requestcount";

Map<Date, Map<String, BigDecimal>> processResults() {
    int iCount = context.chartIntervalCount != null ? Integer.parseInt(context.chartIntervalCount) : 6;
    String iScope = context.chartIntervalScope != null ? context.chartIntervalScope : "hour"; //day|week|month|year
    
    iCount = UtilDateTime.getIntervalDefaultCount(iScope);
    fromDateTimestamp = UtilDateTime.getTimeStampFromIntervalScope(iScope, iCount);
    
    TimeInterval dateIntervals = UtilDateTime.getPeriodIntervalAndFormatter(iScope, fromDateTimestamp, context.locale, context.timeZone);
    
    Map<Date, Long> totalRequests = [:];
    for (int i = 0; i <= iCount; i++) {
        List serverHitDateAndExprs = [];
        serverHitDateAndExprs.add(EntityCondition.makeCondition("hitStartDateTime", EntityOperator.GREATER_THAN_EQUAL_TO, dateIntervals.getDateBegin()));
        serverHitDateAndExprs.add(EntityCondition.makeCondition("hitStartDateTime", EntityOperator.LESS_THAN, dateIntervals.getDateEnd()));
        serverHitDateAndExprs.add(EntityCondition.makeCondition("hitTypeId", EntityOperator.EQUALS, "REQUEST"));
        
        serverRequestHits = from("ServerHit").select("visitId").where(serverHitDateAndExprs).queryCount();
        totalRequests.put(dateIntervals.getDateFormatter().format(dateIntervals.getDateBegin()), serverRequestHits);
        dateIntervals = UtilDateTime.getPeriodIntervalAndFormatter(iScope, 1, dateIntervals.getDateEnd(), context.locale, context.timeZone);
    }
    return totalRequests;
}

Map cacheMap = [:];
// TODO: Not sure wether I should leave or not the result to be cached, for now let's not cache it
if (contentCache.get(cacheId)==null || "Y".equals(parameters.forceRefresh)) {
    Debug.logInfo("building totalMap", module);
    cacheMap = processResults();
    contentCache.put(cacheId, cacheMap);
    Debug.logInfo("finished totalMap; adding to cache", module);
} else {
    cacheMap = contentCache.get(cacheId);
    Debug.logInfo("taking totalMap from cache", module);
}
context.userRequestCount = cacheMap;
