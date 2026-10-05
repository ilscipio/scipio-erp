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
import org.ofbiz.base.util.UtilDateTime
import org.ofbiz.entity.condition.EntityCondition
import org.ofbiz.entity.condition.EntityOperator


//dateIntervals = UtilDateTime.getPeriodInterval("month", null, locale, timeZone);
int iCount = context.chartIntervalCount != null ? Integer.parseInt(context.chartIntervalCount) : 6;
String iScope = context.chartIntervalScope != null ? context.chartIntervalScope : "month"; //day|week|month|year

iCount = UtilDateTime.getIntervalDefaultCount(iScope);
fromDateTimestamp = UtilDateTime.getTimeStampFromIntervalScope(iScope, iCount);
dateIntervals = UtilDateTime.getPeriodIntervalAndFormatter(iScope, fromDateTimestamp, context.locale, context.timeZone);

List introductionDateAndExprs = [];
introductionDateAndExprs.add(EntityCondition.makeCondition("introductionDate", EntityOperator.GREATER_THAN_EQUAL_TO, dateIntervals.getDateBegin()));
introductionDateAndExprs.add(EntityCondition.makeCondition("introductionDate", EntityOperator.LESS_THAN, dateIntervals.getDateEnd()));

newProducts = from("Product").where(introductionDateAndExprs).queryList();
newProducts.each { product ->
    dateIntervals = UtilDateTime.getPeriodIntervalAndFormatter(iScope, 1, fromDateTimestamp, context.locale, context.timeZone);
} 

context.products = newProducts;