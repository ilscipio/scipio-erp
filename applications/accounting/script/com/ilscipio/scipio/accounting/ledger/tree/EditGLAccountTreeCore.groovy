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
 * SCIPIO: CORE interactive glAccount tree data prep.
 */

import javax.servlet.*;
import javax.servlet.http.*;

import org.ofbiz.base.util.*;
import org.ofbiz.entity.*;
import org.ofbiz.entity.condition.EntityCondition
import org.ofbiz.entity.condition.EntityOperator
import org.ofbiz.entity.util.EntityQuery
import org.ofbiz.entity.util.EntityUtil

import com.ilscipio.scipio.treeMenu.jsTree.JsTreeHelper;

final String module = "EditGLAccountTreeCore.groovy";

context.organizationPartyId = context.orgPartyId;
topGlAccountId = context.topGlAccountId;
topGlAccountIds = [];
if (topGlAccountId) {
    topGlAccountIds.add(topGlAccountId);
} else {
    topGlAccounts = EntityQuery.use(delegator).select("glAccountId").from("GlAccount").where(EntityCondition.makeCondition("parentGlAccountId", EntityOperator.EQUALS, null)).queryList();
    topGlAccountIds.addAll(EntityUtil.getFieldListFromEntityList(topGlAccounts, "glAccountId", true));
}

isEventError = context.egltIsEventError;
if (isEventError == null) isEventError = context.isError;
if (isEventError == null) isEventError = false;
context.egltIsEventError = isEventError;

getSetStringParam = { paramName ->
    def value = context[paramName];
    if (value == null) value = parameters[paramName] as String;
    context[paramName] = value;
    return value;
};
targetNodePath = getSetStringParam("egltTargetNodePath");
newTargetNodePath = getSetStringParam("egltNewTargetNodePath");
submittedFormId = getSetStringParam("egltSubmittedFormId");

treeMenuHelper = new JsTreeHelper();
treeMenuData = [];
for (glAccountId in topGlAccountIds) {
    result = dispatcher.runSync("buildGlAccountTree", ["glAccountId": glAccountId]);
    if (result?.treeList) {
        treeMenuData = treeMenuData + result.treeList;
    }
}
treeMenuHelper.addAll(treeMenuData);
context.treeMenuData = treeMenuHelper;

context.treeOptionsFixedParams=["orgPartyId": context.orgPartyId, "topGlAccountId" : context.topGlAccountId];