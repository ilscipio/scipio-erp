/*
 * Licensed to the Apache Software Foundation (ASF) under one
 * or more contributor license agreements.  See the NOTICE file
 * distributed with this work for additional information
 * regarding copyright ownership.  The ASF licenses this file
 * to you under the Apache License, Version 2.0 (the
 * "License"); you may not use this file except in compliance
 * with the License.  You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 * KIND, either express or implied.  See the License for the
 * specific language governing permissions and limitations
 * under the License.
 */

import java.util.*;
import java.sql.Timestamp
import java.text.*;

import org.ofbiz.base.util.*;

final module = "Days.groovy";

String startParam = parameters.startTime;
// SCIPIO: some screens pass start instead of startTime
if (!startParam) {
    startParam = parameters.start;
}
// SCIPIO: we must now also accept a startDate in yyyy-MM-dd format
if (!startParam) {
    String startDate = parameters.startDate;
    if (startDate) {
        DateFormat df = UtilDateTime.toDateFormat(UtilDateTime.DATE_FORMAT, timeZone, locale);
        try {
            java.util.Date parsedDate = df.parse(startDate);
            // return it back to a long time as string to it'll fit in the code below
            startParam = String.valueOf(parsedDate.getTime());
        } catch(ParseException e) {
            Debug.logError("Invalid date (will use now time instead): " + startDate, module);
        }
    }
}

Timestamp start = null;
if (UtilValidate.isNotEmpty(startParam)) {
    // SCIPIO: catch invalid dates
    try {
        start = new Timestamp(Long.parseLong(startParam));
    } catch (NumberFormatException e) {
        Debug.logError("Invalid long time (will use now time instead): " + startParam, module);
    }
}
if (start == null) {
    start = UtilDateTime.getDayStart(nowTimestamp, timeZone, locale);
} else {
    start = UtilDateTime.getDayStart(start, timeZone, locale);
}
Timestamp prev = UtilDateTime.getDayStart(start, -1, timeZone, locale);
context.prevMillis = new Long(prev.getTime()).toString();
Timestamp next = UtilDateTime.getDayStart(start, 1, timeZone, locale);
context.nextMillis = new Long(next.getTime()).toString();
// SCIPIO: bad types lead to crashes, so swap this around
//Map serviceCtx = dispatcher.getDispatchContext().makeValidContext("getWorkEffortEventsByPeriod", "IN", parameters);
//serviceCtx.putAll(UtilMisc.toMap("userLogin", userLogin, "start", start, "numPeriods", 24, "periodType", Calendar.HOUR, "locale", locale, "timeZone", timeZone));
Map serviceCtx = [:];
serviceCtx.putAll(parameters);
serviceCtx.putAll(UtilMisc.toMap("userLogin", userLogin, "start", start, "numPeriods", 24, "periodType", Calendar.HOUR, "locale", locale, "timeZone", timeZone));
serviceCtx = dispatcher.getDispatchContext().makeValidContext("getWorkEffortEventsByPeriod", "IN", serviceCtx);
if (context.entityExprList) {
    serviceCtx.entityExprList = entityExprList;
}
Map result = runService('getWorkEffortEventsByPeriod', serviceCtx);
context.put("periods", result.get("periods"));
context.put("maxConcurrentEntries", result.get("maxConcurrentEntries"));
context.put("start", start);
context.put("prev", prev);
context.put("next", next);
