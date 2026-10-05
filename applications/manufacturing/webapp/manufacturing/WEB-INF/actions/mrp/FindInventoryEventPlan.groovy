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
/*
 * Changes to this file: Copyright (C) Ilscipio GmbH. The changes are licensed
 * under the GNU Affero General Public License, version 3, or a commercial
 * license from Ilscipio GmbH (file LICENSE). The original code stays under
 * the Apache License, version 2.0, as stated above.
 */

import org.ofbiz.base.util.ObjectType;
import org.ofbiz.entity.condition.EntityOperator;
import org.ofbiz.entity.condition.EntityCondition;
import org.ofbiz.entity.util.EntityUtilProperties;

productId = parameters.productId;
mrpId = parameters.mrpId;
facilityId = parameters.facilityId;

// SCIPIO: MRP runs for the mrpId filter dropdown, newest first; newest is the default selection
mrpRunList = from("MrpRun").orderBy("-startDate").queryList();
context.mrpRunList = mrpRunList;
context.defaultMrpId = mrpRunList ? mrpRunList[0].mrpId : null;

// SCIPIO: facilities for the facilityId filter dropdown
context.facilityList = from("Facility").orderBy("facilityName").queryList();

// get the lookup flag; a filter passed on the URL (e.g. a link from MrpRunDetail) also triggers the search
lookupFlag = parameters.lookupFlag;
if (!lookupFlag && (productId || mrpId || facilityId)) {
    lookupFlag = "Y";
}
context.showResults = (lookupFlag ? true : false);

// blank param list
paramList = "";
inventoryList = [];

if (lookupFlag) {
    paramList = paramList + "&lookupFlag=" + lookupFlag;
    andExprs = [];

    //define main condition
    mainCond = null;

    // now do the filtering

    eventDate = parameters.eventDate;
    if (eventDate?.length() > 8) {
    eventDate = eventDate.trim();
    if (eventDate.length() < 14) eventDate = eventDate + " " + "00:00:00.000";
    paramList = paramList + "&eventDate=" + eventDate;
        andExprs.add(EntityCondition.makeCondition("eventDate", EntityOperator.GREATER_THAN, ObjectType.simpleTypeConvert(eventDate, "Timestamp", null, null)));
    }

    if (productId) {
        paramList = paramList + "&productId=" + productId;
        andExprs.add(EntityCondition.makeCondition("productId", EntityOperator.EQUALS, productId));
    }
    if (mrpId) {
        paramList = paramList + "&mrpId=" + mrpId;
        andExprs.add(EntityCondition.makeCondition("mrpId", EntityOperator.EQUALS, mrpId));
    }
    if (facilityId) {
        paramList = paramList + "&facilityId=" + facilityId;
        andExprs.add(EntityCondition.makeCondition("facilityId", EntityOperator.EQUALS, facilityId));
    }
    andExprs.add(EntityCondition.makeCondition("mrpEventTypeId", EntityOperator.NOT_EQUAL, "INITIAL_QOH"));
    andExprs.add(EntityCondition.makeCondition("mrpEventTypeId", EntityOperator.NOT_EQUAL, "ERROR"));
    andExprs.add(EntityCondition.makeCondition("mrpEventTypeId", EntityOperator.NOT_EQUAL, "REQUIRED_MRP"));

    mainCond = EntityCondition.makeCondition(andExprs, EntityOperator.AND);

    if ( mainCond) {
    // do the lookup
        inventoryList = from("MrpEvent").where(mainCond).orderBy("productId", "eventDate").queryList();
    }

    context.inventoryList = inventoryList;
}
context.paramList = paramList;

// set the page parameters
viewIndex = Integer.valueOf(parameters.VIEW_INDEX  ?: 0);
viewSize = Integer.valueOf(parameters.VIEW_SIZE ?: EntityUtilProperties.getPropertyValue("widget", "widget.form.defaultViewSize", "20", delegator));
listSize = 0;
if (inventoryList)
    listSize = inventoryList.size();

lowIndex = viewIndex * viewSize;
highIndex = (viewIndex + 1) * viewSize;
if (listSize < highIndex)
    highIndex = listSize;
if ( highIndex < 1 )
    highIndex = 0;
context.viewIndex = viewIndex;
context.listSize = listSize;
context.highIndex = highIndex;
context.lowIndex = lowIndex;
context.viewSize = viewSize;

