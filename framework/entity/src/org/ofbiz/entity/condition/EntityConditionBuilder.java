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

package org.ofbiz.entity.condition;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.Locale;
import java.util.Map;

import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.UtilGenerics;
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.GenericModelException;
import org.ofbiz.entity.config.model.Datasource;
import org.ofbiz.entity.model.ModelEntity;

import groovy.util.BuilderSupport;

public class EntityConditionBuilder extends BuilderSupport {
    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());

    @SuppressWarnings("serial")
    private static class ConditionHolder extends EntityCondition {
        protected EntityCondition condition;

        protected ConditionHolder(EntityCondition condition) {
            this.condition = condition;
        }

        public Object asType(Class clz) {
            Debug.logInfo("asType(%s): %s", module, clz, condition);
            if (clz == EntityCondition.class) {
                return condition;
            }
            return this;
        }

        public EntityCondition build() {
            return condition;
        }

        public boolean isEmpty() {
            return condition.isEmpty();
        }

        public String makeWhereString(ModelEntity modelEntity, List<EntityConditionParam> entityConditionParams, Datasource datasourceInfo) {
            return condition.makeWhereString(modelEntity, entityConditionParams, datasourceInfo);
        }

        public void checkCondition(ModelEntity modelEntity) throws GenericModelException {
            condition.checkCondition(modelEntity);
        }

        public boolean mapMatches(Delegator delegator, Map<String, ? extends Object> map) {
            return condition.mapMatches(delegator, map);
        }

        public EntityCondition freeze() {
            return condition.freeze();
        }

        public int hashCode() {
            return condition.hashCode();
        }
        public boolean equals(Object obj) {
            return condition.equals(obj);
        }

    }

    @Override
    protected Object createNode(Object methodName) {
        String operatorName = ((String)methodName).toLowerCase(Locale.getDefault());
        EntityJoinOperator operator = EntityOperator.lookupJoin(operatorName);
        List<EntityCondition> condList = new ArrayList<>(); // SCIPIO: switched to ArrayList
        return new ConditionHolder(EntityCondition.makeCondition(condList, operator));
    }

    @Override
    protected Object createNode(Object methodName, Object objArg) {
        Object node = createNode(methodName);
        setParent(node, objArg);
        return node;
    }

    @Override
    protected Object createNode(Object methodName, Map mapArg) {
        Map<String, Object> fieldValueMap = UtilGenerics.checkMap(mapArg);
        String operatorName = ((String)methodName).toLowerCase(Locale.getDefault());
        EntityComparisonOperator<String, Object> operator = EntityOperator.lookupComparison(operatorName);
        List<EntityCondition> conditionList = new ArrayList<>(fieldValueMap.size()); // SCIPIO: switched to ArrayList
        for (Map.Entry<String, Object> entry : fieldValueMap.entrySet()) {
            conditionList.add(EntityCondition.makeCondition(entry.getKey(), operator, entry.getValue()));
        }
        if (conditionList.size() == 1) {
            return new ConditionHolder(conditionList.get(0));
        }
        return new ConditionHolder(EntityCondition.makeCondition(conditionList));
    }

    @Override
    protected Object createNode(Object methodName, Map mapArg, Object objArg) {
        return null;
    }

    @Override
    protected void setParent(Object parent, Object child) {
        ConditionHolder holder = (ConditionHolder) parent;
        EntityConditionList<EntityCondition> parentConList = UtilGenerics.cast(holder.condition);
        Iterator<EntityCondition> iterator = parentConList.getConditionIterator();
        List<EntityCondition> tempList = new ArrayList<>(); // SCIPIO: switched to ArrayList
        while (iterator.hasNext()) {
            tempList.add(iterator.next());
        }
        if (child instanceof EntityCondition) {
            tempList.add((EntityCondition)child);
        } else if (child instanceof ConditionHolder) {
            tempList.add(((ConditionHolder)child).condition);
        } else {
            tempList.addAll(UtilGenerics.<EntityCondition>checkList(child));
        }
        holder.condition = EntityCondition.makeCondition(tempList, parentConList.getOperator());
    }

}
