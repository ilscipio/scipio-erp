/*******************************************************************************
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
 *******************************************************************************/

package com.ilscipio.scipio.accounting.period;

import java.util.ArrayList;
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

public class PeriodServices {

    public static String module = PeriodServices.class.getName();
    public static final String resource = "AccountingUiLabels";

    /**
     * SCIPIO: Builds a custom period tree
     * 
     * @param dctx
     * @param context
     * @return
     */
    // FIXME: Move this to a generic service (as well as buildGlAccountTree)
    public static Map<String, Object> buildCustomPeriodTree(DispatchContext dctx, Map<String, ? extends Object> context) {
        Map<String, Object> result = ServiceUtil.returnSuccess();
        Delegator delegator = dctx.getDelegator();
        String library = (String) context.get("library");
        String mode = (String) context.get("mode");
        String customTimePeriodId = (String) context.get("customTimePeriodId");

        Map<String, Object> state = UtilGenerics.checkMap(context.get("state"));

        boolean includeTimePeriodData = Boolean.TRUE.equals(context.get("includeTimePeriodData"));
        boolean includeEmptyTop = Boolean.TRUE.equals(context.get("includeEmptyTop"));

        List<TreeDataItem> resultList = new ArrayList<>();
        if (mode.equals("full")) {
            try {
                GenericValue topCustomTimePeriod = delegator.findOne("CustomTimePeriod", true, UtilMisc.toMap("customTimePeriodId", customTimePeriodId));
                List<GenericValue> childTimePeriods = topCustomTimePeriod.getRelated("ChildCustomTimePeriod", null, UtilMisc.toList("periodNum"), false);
                resultList.add(convertToTreeDataItem(topCustomTimePeriod, library, state, includeTimePeriodData, !childTimePeriods.isEmpty()));
                if ((childTimePeriods.isEmpty() && includeEmptyTop) || !childTimePeriods.isEmpty()) {
                    try {
                        resultList.addAll(childTimePeriodTree(childTimePeriods, library, includeTimePeriodData));
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

    private static TreeDataItem convertToTreeDataItem(GenericValue customTimePeriod, String library, boolean includeTimePeriodData, boolean isParent) {
        return convertToTreeDataItem(customTimePeriod, library, null, includeTimePeriodData, isParent);
    }

    private static TreeDataItem convertToTreeDataItem(GenericValue customTimePeriod, String library, Map<String, Object> state, boolean includeCustomTimePeriodData,
            boolean isParent) {
        String nodeId = "customTimePeriodId_" + customTimePeriod.getString("customTimePeriodId");
        String parentNodeId = null;
        if (UtilValidate.isNotEmpty(customTimePeriod.getString("parentPeriodId")))
            parentNodeId = "customTimePeriodId_" + customTimePeriod.getString("parentPeriodId");
        Map<String, Object> effState = UtilMisc.toMap("opened", false, "selected", false);
        if (state != null) {
            effState.putAll(state);
        }
        if (library.equals("jsTree")) {
            String customTimePeriodId = customTimePeriod.getString("customTimePeriodId");
            String periodName = customTimePeriod.getString("periodName");
            String nodeText = (UtilValidate.isNotEmpty(periodName)) ? periodName : customTimePeriodId;
            GenericValue periodType = null;
            try {
                periodType = customTimePeriod.getRelatedOne("PeriodType", true);
            } catch (GenericEntityException e) {
                Debug.logWarning("Can't retreive periodType [" + customTimePeriod.getString("periodTypeId") + "]", module);
            }
            if (UtilValidate.isNotEmpty(periodType)) {
                nodeText = nodeText.concat(" [" + periodType.getString("description") + "]");
            }
            JsTreeDataItem dataItem = null;
            dataItem = new JsTreeDataItem(nodeId, customTimePeriodId, nodeText, "jstree-folder", new JsTreeDataItemState(effState), parentNodeId);
            dataItem.setType("timePeriod");
            if (includeCustomTimePeriodData) {
                dataItem.put("customTimePeriodEntity", customTimePeriod);
            }
            dataItem.put("isParent", isParent);
            return dataItem;
        }

        return null;
    }

    private static List<TreeDataItem> childTimePeriodTree(List<GenericValue> childTimePeriods, String library, boolean includeTimePeriodData) throws GeneralException {
        List<TreeDataItem> childTreeDataItems = UtilMisc.newList();
        return childTimePeriodTree(childTimePeriods, library, includeTimePeriodData, childTreeDataItems);
    }

    private static List<TreeDataItem> childTimePeriodTree(List<GenericValue> childTimePeriods, String library, boolean includeTimePeriodData, List<TreeDataItem> childTreeDataItems)
            throws GeneralException {
        for (GenericValue childTimePeriod : childTimePeriods) {
            List<GenericValue> timePeriods = childTimePeriod.getRelated("ChildCustomTimePeriod", null, UtilMisc.toList("periodNum"), false);
            childTreeDataItems.add(convertToTreeDataItem(childTimePeriod, library, includeTimePeriodData, !timePeriods.isEmpty()));
            if (UtilValidate.isNotEmpty(timePeriods)) {
                childTreeDataItems.addAll(childTimePeriodTree(timePeriods, library, includeTimePeriodData));
            }
        }
        return childTreeDataItems;
    }

    public static Map<String, Object> getTimePeriod(DispatchContext dctx, Map<String, ? extends Object> context) {
        Map<String, Object> result = ServiceUtil.returnSuccess();

        Delegator delegator = dctx.getDelegator();
        String customTimePeriodId = (String) context.get("customTimePeriodId");
        Boolean useCache = (Boolean) context.get("useCache");

        GenericValue timePeriod = null;
        GenericValue parentPeriod = null;
        Map<String, Object> timePeriodMap = UtilMisc.newMap();
        try {
            timePeriod = EntityQuery.use(delegator).cache(useCache).from("CustomTimePeriod")
                    .where(EntityCondition.makeCondition("customTimePeriodId", EntityOperator.EQUALS, customTimePeriodId)).queryOne();
            if (UtilValidate.isNotEmpty(timePeriod)) {
                parentPeriod = timePeriod.getRelatedOne("ParentCustomTimePeriod", false);
                timePeriodMap.putAll(timePeriod.getAllFields());
                StringBuilder parentTimePeriodDesc = new StringBuilder();
                if (UtilValidate.isNotEmpty(parentPeriod)) {
                    parentTimePeriodDesc.append("[");
                    parentTimePeriodDesc.append(parentPeriod.getString("customTimePeriodId"));
                    parentTimePeriodDesc.append("]");
                    if (UtilValidate.isNotEmpty(parentPeriod.getString("periodName"))) {
                        parentTimePeriodDesc.append(" " + parentPeriod.getString("periodName"));
                    } else if (UtilValidate.isNotEmpty(parentPeriod.getString("periodNum"))) {
                        parentTimePeriodDesc.append(" " + parentPeriod.getString("periodNum"));
                    }
                } else {
                    parentTimePeriodDesc.append("_Not Applicable_");
                }
                timePeriodMap.put("parentPeriodDesc", parentTimePeriodDesc.toString());
            }

        } catch (GenericEntityException e) {
            Debug.logError("TimePeriod [ " + customTimePeriodId + "] couldn't be found. " + e.getMessage(), module);
        }

        result.put("timePeriod", timePeriodMap);

        return result;
    }
}
