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
package com.ilscipio.scipio.accounting.ledger;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.GeneralException;
import org.ofbiz.base.util.UtilGenerics;
import org.ofbiz.base.util.UtilMisc;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.GenericEntityException;
import org.ofbiz.entity.GenericValue;
import org.ofbiz.entity.condition.EntityCondition;
import org.ofbiz.entity.condition.EntityOperator;
import org.ofbiz.entity.util.EntityQuery;
import org.ofbiz.service.DispatchContext;
import org.ofbiz.service.ServiceUtil;

import com.ilscipio.scipio.treeMenu.TreeDataItem;
import com.ilscipio.scipio.treeMenu.jsTree.JsTreeDataItem;
import com.ilscipio.scipio.treeMenu.jsTree.JsTreeDataItem.JsTreeDataItemState;

public class GeneralLedgerServices {

    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());

    public static Map<String, Object> buildGlAccountTree(DispatchContext dctx, Map<String, ? extends Object> context) {
        Map<String, Object> result = ServiceUtil.returnSuccess();
        Delegator delegator = dctx.getDelegator();
        //LocalDispatcher dispatcher = dctx.getDispatcher();
        //Locale locale = (Locale) context.get("locale");
        String library = (String) context.get("library");
        String mode = (String) context.get("mode");
        String glAccountId = (String) context.get("glAccountId");

        Map<String, Object> state = UtilGenerics.checkMap(context.get("state"));

        boolean includeGlAccountData = Boolean.TRUE.equals(context.get("includeGlAccountData"));
        boolean includeEmptyTop = Boolean.TRUE.equals(context.get("includeEmptyTop"));

        List<TreeDataItem> resultList = new ArrayList<>();
        if (mode.equals("full")) {
            try {
                GenericValue topGlAccount = delegator.findOne("GlAccount", true, UtilMisc.toMap("glAccountId", glAccountId));
                List<GenericValue> childGlAccounts = topGlAccount.getRelated("ChildGlAccount", null, UtilMisc.toList("accountCode"), false);
                resultList.add(convertToTreeDataItem(topGlAccount, library, state, includeGlAccountData, !childGlAccounts.isEmpty()));
                if ((childGlAccounts.isEmpty() && includeEmptyTop) || !childGlAccounts.isEmpty()) {
                    try {
                        resultList.addAll(childGlAccountTree(childGlAccounts, library, includeGlAccountData));
                    } catch (GeneralException e) {
                        return ServiceUtil.returnError(e.getMessage());
                    }
                }
            } catch (GenericEntityException e) {
                return ServiceUtil.returnError(e.getMessage());
            }
        }
        result.put("treeList", resultList);

        return result;

    }

    private static TreeDataItem convertToTreeDataItem(GenericValue glAccount, String library, boolean includeGlAccountData, boolean isParent) {
        return convertToTreeDataItem(glAccount, library, null, includeGlAccountData, isParent);
    }

    private static TreeDataItem convertToTreeDataItem(GenericValue glAccount, String library, Map<String, Object> state, boolean includeGlAccountData, boolean isParent) {
        String nodeId = "glAccountId_" + glAccount.getString("glAccountId");
        String parentNodeId = null;
        if (UtilValidate.isNotEmpty(glAccount.getString("parentGlAccountId")))
            parentNodeId = "glAccountId_" + glAccount.getString("parentGlAccountId");
        Map<String, Object> effState = UtilMisc.toMap("opened", false, "selected", false);
        if (state != null) {
            effState.putAll(state);
        }
        if (library.equals("jsTree")) {
            String glAccountId = glAccount.getString("glAccountId");
            String accountName = glAccount.getString("accountName");
            JsTreeDataItem dataItem = null;
            dataItem = new JsTreeDataItem(nodeId, glAccountId, (UtilValidate.isNotEmpty(accountName)) ? accountName : glAccountId, "jstree-folder",
                    new JsTreeDataItemState(effState), parentNodeId);
            dataItem.setType("glAccount");
            if (includeGlAccountData) {
                dataItem.put("glAccountEntity", glAccount);
            }
            dataItem.put("isParent", isParent);
            return dataItem;
        }

        return null;
    }

    private static List<TreeDataItem> childGlAccountTree(List<GenericValue> childGlAccounts, String library, boolean includeGlAccountData) throws GeneralException {
        List<TreeDataItem> childTreeDataItems = new ArrayList<>();
        return childGlAccountTree(childGlAccounts, library, includeGlAccountData, childTreeDataItems);
    }

    private static List<TreeDataItem> childGlAccountTree(List<GenericValue> childGlAccounts, String library, boolean includeGlAccountData, List<TreeDataItem> childTreeDataItems)
            throws GeneralException {
        for (GenericValue childGlAccount : childGlAccounts) {
            List<GenericValue> glAccounts = childGlAccount.getRelated("ChildGlAccount", null, UtilMisc.toList("accountCode"), false);
            childTreeDataItems.add(convertToTreeDataItem(childGlAccount, library, includeGlAccountData, !glAccounts.isEmpty()));
            if (UtilValidate.isNotEmpty(glAccounts)) {
                childTreeDataItems.addAll(childGlAccountTree(glAccounts, library, includeGlAccountData));
            }
        }
        return childTreeDataItems;
    }

    public static Map<String, Object> getGlAccountAndAssocs(DispatchContext dctx, Map<String, ? extends Object> context) {
        Map<String, Object> result = ServiceUtil.returnSuccess();

        Delegator delegator = dctx.getDelegator();
        String glAccountId = (String) context.get("glAccountId");
        Boolean useCache = (Boolean) context.get("useCache");

        GenericValue glAccount = null;
        GenericValue parentGlAccount = null;
        Map<String, Object> glAccountMap = new HashMap<>();
        try {
            glAccount = EntityQuery.use(delegator).cache(useCache).from("GlAccount").where(EntityCondition.makeCondition("glAccountId", EntityOperator.EQUALS, glAccountId))
                    .queryOne();
            if (UtilValidate.isNotEmpty(glAccount)) {
                parentGlAccount = glAccount.getRelatedOne("ParentGlAccount", false);
                glAccountMap.putAll(glAccount.getAllFields());
                StringBuilder parentGlAccountDesc = new StringBuilder();
                if (UtilValidate.isNotEmpty(parentGlAccount)) {
                    parentGlAccountDesc.append("[");
                    parentGlAccountDesc.append(parentGlAccount.getString("glAccountId"));
                    parentGlAccountDesc.append("]");
                    if (UtilValidate.isNotEmpty(parentGlAccount.getString("accountName"))) {
                        parentGlAccountDesc.append(" " + parentGlAccount.getString("accountName"));
                    } else if (UtilValidate.isNotEmpty(parentGlAccount.getString("accountCode"))) {
                        parentGlAccountDesc.append(" " + parentGlAccount.getString("accountCode"));
                    }
                } else {
                    parentGlAccountDesc.append("_Not Applicable_");
                }
                glAccountMap.put("parentGlAccountDesc", parentGlAccountDesc.toString());
            }

        } catch (GenericEntityException e) {
            Debug.logError(e,"GlAccount [ " + glAccountId + "] could not be retrieved", module);
            result = ServiceUtil.returnFailure("GlAccount [ " + glAccountId + "] could not be retrieved"); // TODO: localize
        }
        result.put("glAccount", glAccountMap);
        return result;
    }

}
