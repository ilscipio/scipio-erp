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
 * This is based on the new SCIPIO getServerRequests services. Sample implementation only
 */
import java.sql.Timestamp;
import org.ofbiz.base.util.*;

Map findDataMap = [:];
Timestamp nowTimestamp = UtilDateTime.nowTimestamp();
switch (context.chartIntervalScope ?: "day") {
    case "hour":
        begin = UtilDateTime.getHourStart(nowTimestamp, 0, timeZone, locale);
        break;

    case "day":
        begin = UtilDateTime.getDayStart(nowTimestamp, 0, timeZone, locale);
        break;

    case "week":
        begin = UtilDateTime.getWeekStart(nowTimestamp, 0, timeZone, locale);
        break;

    case "month":
        begin = UtilDateTime.getMonthStart(nowTimestamp, 0, timeZone, locale);
        break;

    case "year":
        begin = UtilDateTime.getYearStart(nowTimestamp, 0, timeZone, locale);
        break;

    default:
        begin = UtilDateTime.getDayStart(nowTimestamp, 0, timeZone, locale);
}

try {
    //findDataMap = dispatcher.runSync("getServerRequests", UtilMisc.toMap("fromDate",begin,"thruDate",nowTimestamp,"dateInterval",context.chartIntervalScope?context.chartIntervalScope:"day","userLogin",userLogin));
    findDataMap = dispatcher.runSync("getSavedHitBinLiveData", ["fromDate": begin, "useCache": false, "maxRequests": context.maxRequestsEntries, "userLogin": userLogin]);
    chartData = findDataMap.requests;
    context.chartData = chartData;
} catch(Exception e) {
    Debug.logError(e, "Cannot fetch request data", module); // NOTE: Because call is switched this shouldn't happen anymore...
}

