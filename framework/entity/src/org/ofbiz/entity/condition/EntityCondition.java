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

import java.sql.Timestamp;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.List;
import java.util.Map;

import org.ofbiz.base.lang.IsEmpty;
import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.EntityFieldNotFoundException;
import org.ofbiz.entity.GenericEntity;
import org.ofbiz.entity.GenericModelException;
import org.ofbiz.entity.config.model.Datasource;
import org.ofbiz.entity.model.ModelEntity;
import org.ofbiz.entity.util.EntityUtil;

/**
 * <p>Represents the conditions to be used to constrain a query.</p>
 * <p>An EntityCondition can represent various type of constraints, including:</p>
 * <ul>
 *  <li>EntityConditionList: a list of EntityConditions, combined with the operator specified
 *  <li>EntityExpr: for simple expressions or expressions that combine EntityConditions
 *  <li>EntityFieldMap: a map of fields where the field (key) equals the value, combined with the operator specified
 * </ul>
 * These can be used in various combinations using the EntityConditionList and EntityExpr objects.
 *
 */
@SuppressWarnings("serial")
public abstract class EntityCondition extends EntityConditionBase implements IsEmpty {

    public static <L,R,LL,RR> EntityExpr makeCondition(L lhs, EntityComparisonOperator<LL,RR> operator, R rhs) {
        return new EntityExpr(lhs, operator, rhs);
    }

    public static <R> EntityExpr makeCondition(String fieldName, R value) {
        return new EntityExpr(fieldName, EntityOperator.EQUALS, value);
    }

    /**
     * Helper to make an OR condition for all the possible values of a field.
     *
     * <p>SCIPIO: 3.0.0: Added.</p>
     */
    public static EntityCondition makeFieldCondition(String fieldName, Collection<?> values) {
        List<EntityCondition> condList = new ArrayList<>(values.size());
        for (Object value : values) {
            condList.add(makeCondition(fieldName, value));
        }
        return makeCondition(condList, EntityOperator.OR);
    }

    /**
     * Helper to make an OR condition for all the possible values of a field.
     *
     * <p>SCIPIO: 3.0.0: Added.</p>
     */
    public static EntityCondition makeFieldCondition(String fieldName, Object... values) {
        return makeFieldCondition(fieldName, Arrays.asList(values));
    }

    public static EntityExpr makeCondition(EntityCondition lhs, EntityJoinOperator operator, EntityCondition rhs) {
        return new EntityExpr(lhs, operator, rhs);
    }

    @SafeVarargs
    public static <T extends EntityCondition> EntityConditionList<T> makeCondition(EntityJoinOperator operator, T... conditionList) {
        return new EntityConditionList<>(Arrays.<T>asList(conditionList), operator);
    }

    @SafeVarargs
    public static <T extends EntityCondition> EntityConditionList<T> makeCondition(T... conditionList) {
        return new EntityConditionList<>(Arrays.<T>asList(conditionList), EntityOperator.AND);
    }

    public static <T extends EntityCondition> EntityConditionList<T> makeCondition(List<T> conditionList, EntityJoinOperator operator) {
        return new EntityConditionList<>(conditionList, operator);
    }

    public static <T extends EntityCondition> EntityConditionList<T> makeCondition(List<T> conditionList) {
        return new EntityConditionList<>(conditionList, EntityOperator.AND);
    }

    public static <L,R> EntityFieldMap makeCondition(Map<String, ? extends Object> fieldMap, EntityComparisonOperator<L,R> compOp, EntityJoinOperator joinOp) {
        return new EntityFieldMap(fieldMap, compOp, joinOp);
    }

    public static EntityFieldMap makeCondition(Map<String, ? extends Object> fieldMap, EntityJoinOperator joinOp) {
        return new EntityFieldMap(fieldMap, EntityOperator.EQUALS, joinOp);
    }

    public static EntityFieldMap makeCondition(Map<String, ? extends Object> fieldMap) {
        return new EntityFieldMap(fieldMap, EntityOperator.EQUALS, EntityOperator.AND);
    }

    public static <L,R> EntityFieldMap makeCondition(EntityComparisonOperator<L,R> compOp, EntityJoinOperator joinOp, Object... keysValues) {
        return new EntityFieldMap(compOp, joinOp, keysValues);
    }

    public static EntityFieldMap makeCondition(EntityJoinOperator joinOp, Object... keysValues) {
        return new EntityFieldMap(EntityOperator.EQUALS, joinOp, keysValues);
    }

    public static EntityFieldMap makeConditionMap(Object... keysValues) {
        return new EntityFieldMap(EntityOperator.EQUALS, EntityOperator.AND, keysValues);
    }

    public static EntityDateFilterCondition makeConditionDate(String fromDateName, String thruDateName) {
        return new EntityDateFilterCondition(fromDateName, thruDateName);
    }

    public static List<EntityCondition> makeConditionDateList(List<String> filterByFieldNames) { // SCIPIO
        List<EntityCondition> conditions = new ArrayList<>();
        for (int i = 0; i < filterByFieldNames.size();) {
            String fromDateFieldName = filterByFieldNames.get(i++);
            String thruDateFieldName = filterByFieldNames.get(i++);
            conditions.add(new EntityDateFilterCondition(fromDateFieldName, thruDateFieldName));
        }
        return conditions;
    }

    public static List<EntityCondition> makeConditionDateList(Timestamp filterByDateMoment, List<String> filterByFieldNames) { // SCIPIO
        List<EntityCondition> conditions = new ArrayList<>();
        for (int i = 0; i < filterByFieldNames.size();) {
            String fromDateFieldName = filterByFieldNames.get(i++);
            String thruDateFieldName = filterByFieldNames.get(i++);
            if (filterByDateMoment == null) {
                conditions.add(EntityUtil.getFilterByDateExpr(fromDateFieldName, thruDateFieldName));
            } else {
                conditions.add(EntityUtil.getFilterByDateExpr(filterByDateMoment, fromDateFieldName, thruDateFieldName));
            }
        }
        return conditions;
    }

    /**
     * Makes a date range condition with specific range start and end dates on a single field.
     * <p>SCIPIO: 2.1.0: Added.</p>
     * @param dateFieldName The name of the field containing the entity's date
     * @param rangeStart    The start of the range to filter against
     * @param rangeEnd      The end of the range to filter against
     * @return EntityCondition representing the date range filter
     */
    public static EntityCondition makeDateRangeCondition(String dateFieldName, Timestamp rangeStart, Timestamp rangeEnd) {
        EntityCondition cond = null;
        if (rangeStart != null) {
            cond = EntityCondition.makeCondition(dateFieldName, EntityOperator.GREATER_THAN_EQUAL_TO, rangeStart);
        }
        if (rangeEnd != null) {
            EntityCondition orderThruDateCond = EntityCondition.makeCondition(dateFieldName,
                    EntityOperator.LESS_THAN, rangeEnd);
            cond = (cond != null) ? EntityCondition.makeCondition(cond, orderThruDateCond) : orderThruDateCond;
        }
        return cond;
    }

    /**
     * Combines both conditions using {@link #makeCondition(EntityCondition, EntityJoinOperator, EntityCondition)}}
     * only if both conditions are non-null, otherwise returns the non-null one.
     * <p>SCIPIO: 2.1.0: Added.</p>
     */
    public static EntityCondition combine(EntityCondition lhs, EntityJoinOperator operator, EntityCondition rhs) {
        if (lhs == null) {
            return rhs;
        } else if (rhs == null) {
            return lhs;
        } else {
            return makeCondition(lhs, operator, rhs);
        }
    }

    /**
     * Combines both conditions using {@link #makeCondition(EntityCondition, EntityJoinOperator, EntityCondition)}}
     * only if both conditions are non-null, otherwise returns the non-null one.
     * <p>SCIPIO: 2.1.0: Added.</p>
     */
    public static EntityCondition combine(EntityCondition lhs, EntityCondition rhs) {
        if (lhs == null) {
            return rhs;
        } else if (rhs == null) {
            return lhs;
        } else {
            return makeCondition(lhs, EntityOperator.AND, rhs);
        }
    }

    public static EntityWhereString makeConditionWhere(String sqlString) {
        return new EntityWhereString(sqlString);
    }

    /** Combines conditions using AND but skips any null (SCIPIO). */
    public static EntityCondition append(EntityCondition... conditions) {
        EntityCondition cond = conditions[0];
        for(int i = 1; i < conditions.length; i++) {
            if (conditions[i] != null) {
                cond = (cond != null) ? makeCondition(cond, EntityOperator.AND, conditions[i]) : conditions[i];
            }
        }
        return cond;
    }

    /** Combines conditions using AND but skips any null (SCIPIO). */
    public static EntityCondition append(EntityCondition first, EntityCondition second) {
        if (first != null) {
            if (second != null) {
                return EntityCondition.makeCondition(first, EntityOperator.AND, second);
            } else {
                return first;
            }
        } else {
            return second;
        }
    }

    @Override
    public String toString() {
        return makeWhereString(null, new ArrayList<EntityConditionParam>(), null);
    }

    public void accept(EntityConditionVisitor visitor) {
        throw new IllegalArgumentException(getClass().getName() + ".accept not implemented");
    }

    abstract public String makeWhereString(ModelEntity modelEntity, List<EntityConditionParam> entityConditionParams, Datasource datasourceInfo);

    abstract public void checkCondition(ModelEntity modelEntity) throws GenericModelException;

    public boolean entityMatches(GenericEntity entity) {
        return mapMatches(entity.getDelegator(), entity);
    }

    public Boolean eval(GenericEntity entity) {
        return eval(entity.getDelegator(), entity);
    }

    public Boolean eval(Delegator delegator, Map<String, ? extends Object> map) {
        return mapMatches(delegator, map) ? Boolean.TRUE : Boolean.FALSE;
    }

    abstract public boolean mapMatches(Delegator delegator, Map<String, ? extends Object> map);

    abstract public EntityCondition freeze();

    public void visit(EntityConditionVisitor visitor) {
        throw new IllegalArgumentException(getClass().getName() + ".visit not implemented");
    }
}
