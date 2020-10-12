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

package org.ofbiz.entity.util;

import java.io.Serializable;
import java.sql.Timestamp;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Set;
import java.util.function.Supplier;

import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.NamedElement;
import org.ofbiz.base.util.UtilDateTime;
import org.ofbiz.base.util.UtilGenerics;
import org.ofbiz.base.util.UtilMisc;
import org.ofbiz.base.util.UtilProperties;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.base.util.collections.PagedList;
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.EntityFieldNotFoundException;
import org.ofbiz.entity.GenericEntity;
import org.ofbiz.entity.GenericEntityException;
import org.ofbiz.entity.GenericPK;
import org.ofbiz.entity.GenericValue;
import org.ofbiz.entity.condition.EntityCondition;
import org.ofbiz.entity.condition.EntityDateFilterCondition;
import org.ofbiz.entity.condition.OrderByList;
import org.ofbiz.entity.model.ModelEntity;
import org.ofbiz.entity.model.ModelField;

/**
 * Helper methods when dealing with Entities, especially ones that follow certain conventions
 */
public final class EntityUtil {

    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());

    /**
     * SCIPIO: A list of entities names which are typically dangerous to update, remove or import.
     * Includes:
     * <ul>
     *   <li>EntityKeyStore - Database-specific entity field encryption keys</li>
     * </ul>
     * First added 2018-09-17.
     */
    private static final Set<String> unsafeEntitiesForUpdate = UtilMisc.unmodifiableHashSet("EntityKeyStore");

    private EntityUtil() {}

    @SafeVarargs
    public static <V> Map<String, V> makeFields(V... args) {
        Map<String, V> fields = new HashMap<String, V>();
        if (args != null) {
            for (int i = 0; i < args.length;) {
                if (!(args[i] instanceof String)) throw new IllegalArgumentException("Key(" + i + "), with value(" + args[i] + ") is not a String.");
                String key = (String) args[i];
                i++;
                if (!(args[i] instanceof Comparable<?>)) throw new IllegalArgumentException("Value(" + i + "), with value(" + args[i] + ") does not implement Comparable.");
                if (!(args[i] instanceof Serializable)) throw new IllegalArgumentException("Value(" + i + "), with value(" + args[i] + ") does not implement Serializable.");
                fields.put(key, args[i]);
                i++;
            }
        }
        return fields;
    }


    public static GenericValue getFirst(Collection<GenericValue> values) {
        if (UtilValidate.isNotEmpty(values)) {
            return (values instanceof List) ? UtilGenerics.<List<GenericValue>>cast(values).get(0) : values.iterator().next(); // SCIPIO: optimized
        } else {
            return null;
        }
    }

    public static GenericValue getFirst(List<GenericValue> values) {
        if (UtilValidate.isNotEmpty(values)) {
            return values.get(0);
        } else {
            return null;
        }
    }

    public static GenericValue getOnly(Collection<GenericValue> values) {
        if (UtilValidate.isNotEmpty(values)) {
            Iterator<GenericValue> it = values.iterator();
            GenericValue result = it.next();
            if (it.hasNext()) {
                throw new IllegalArgumentException("Passed List had more than one value.");
            }
            return result;
        } else {
            return null;
        }
    }

    public static GenericValue getOnly(List<GenericValue> values) {
        if (UtilValidate.isNotEmpty(values)) {
            if (values.size() == 1) {
                return values.get(0);
            } else {
                throw new IllegalArgumentException("Passed List had more than one value.");
            }
        } else {
            return null;
        }
    }

    public static GenericPK getFirstPk(Collection<GenericValue> values) { // SCIPIO
        if (UtilValidate.isNotEmpty(values)) {
            return (values instanceof List) ? UtilGenerics.<List<GenericValue>>cast(values).get(0).getPrimaryKey() : values.iterator().next().getPrimaryKey(); // SCIPIO: optimized
        } else {
            return null;
        }
    }

    public static GenericPK getFirstPk(List<GenericValue> values) { // SCIPIO
        if (UtilValidate.isNotEmpty(values)) {
            return values.get(0).getPrimaryKey();
        } else {
            return null;
        }
    }

    public static EntityCondition getFilterByDateExpr() {
        return EntityCondition.makeConditionDate("fromDate", "thruDate");
    }

    public static EntityCondition getFilterByDateExpr(String fromDateName, String thruDateName) {
        return EntityCondition.makeConditionDate(fromDateName, thruDateName);
    }

    public static EntityCondition getFilterByDateExpr(java.util.Date moment) {
        return EntityDateFilterCondition.makeCondition(new java.sql.Timestamp(moment.getTime()), "fromDate", "thruDate");
    }

    public static EntityCondition getFilterByDateExpr(java.sql.Timestamp moment) {
        return EntityDateFilterCondition.makeCondition(moment, "fromDate", "thruDate");
    }

    public static EntityCondition getFilterByDateExpr(java.sql.Timestamp moment, String fromDateName, String thruDateName) {
        return EntityDateFilterCondition.makeCondition(moment, fromDateName, thruDateName);
    }

    /**
     * Returns the values that are currently active.
     * <p>
     * SCIPIO: 2018-09-29: This method no longer throws exception if the date field names
     * are invalid for the entity; instead a detailed error is logged. This is an extremely
     * easy error to make, and otherwise can cause needless critical failures on small errors
     * during upgrades.
     *
     *@param datedValues GenericValue's that have "fromDate" and "thruDate" fields
     *@return List of GenericValue's that are currently active
     */
    public static <T extends GenericEntity> List<T> filterByDate(List<T> datedValues) {
        return filterByDate(datedValues, UtilDateTime.nowTimestamp(), null, null, true);
    }

    /**
     * Returns the values that are currently active.
     * <p>
     * SCIPIO: 2018-09-29: This method no longer throws exception if the date field names
     * are invalid for the entity; instead a detailed error is logged. This is an extremely
     * easy error to make, and otherwise can cause needless critical failures on small errors
     * during upgrades.
     *
     *@param datedValues GenericValue's that have "fromDate" and "thruDate" fields
     *@param allAreSame Specifies whether all values in the List are of the same entity; this can help speed things up a fair amount since we only have to see if the from and thru date fields are valid once
     *@return List of GenericValue's that are currently active
     */
    public static <T extends GenericEntity> List<T> filterByDate(List<T> datedValues, boolean allAreSame) {
        return filterByDate(datedValues, UtilDateTime.nowTimestamp(), null, null, allAreSame);
    }

    /**
     * Returns the values that are active at the moment.
     * <p>
     * SCIPIO: 2018-09-29: This method no longer throws exception if the date field names
     * are invalid for the entity; instead a detailed error is logged. This is an extremely
     * easy error to make, and otherwise can cause needless critical failures on small errors
     * during upgrades.
     *
     *@param datedValues GenericValue's that have "fromDate" and "thruDate" fields
     *@param moment the moment in question
     *@return List of GenericValue's that are active at the moment
     */
    public static <T extends GenericEntity> List<T> filterByDate(List<T> datedValues, java.util.Date moment) {
        return filterByDate(datedValues, new java.sql.Timestamp(moment.getTime()), null, null, true);
    }

    /**
     * Returns the values that are active at the moment.
     * <p>
     * SCIPIO: 2018-09-29: This method no longer throws exception if the date field names
     * are invalid for the entity; instead a detailed error is logged. This is an extremely
     * easy error to make, and otherwise can cause needless critical failures on small errors
     * during upgrades.
     *
     *@param datedValues GenericValue's that have "fromDate" and "thruDate" fields
     *@param moment the moment in question
     *@return List of GenericValue's that are active at the moment
     */
    public static <T extends GenericEntity> List<T> filterByDate(List<T> datedValues, java.sql.Timestamp moment) {
        return filterByDate(datedValues, moment, null, null, true);
    }

    /**
     * Returns the values that are active at the moment.
     * <p>
     * SCIPIO: 2018-09-29: This method no longer throws exception if the date field names
     * are invalid for the entity; instead a detailed error is logged. This is an extremely
     * easy error to make, and otherwise can cause needless critical failures on small errors
     * during upgrades.
     *
     *@param datedValues GenericValue's that have "fromDate" and "thruDate" fields
     *@param moment the moment in question
     *@param allAreSame Specifies whether all values in the List are of the same entity; this can help speed things up a fair amount since we only have to see if the from and thru date fields are valid once
     *@return List of GenericValue's that are active at the moment
     */
    public static <T extends GenericEntity> List<T> filterByDate(List<T> datedValues, java.sql.Timestamp moment, String fromDateName, String thruDateName, boolean allAreSame) {
        if (datedValues == null) return null;
        if (moment == null) return datedValues;
        if (fromDateName == null) fromDateName = "fromDate";
        if (thruDateName == null) thruDateName = "thruDate";

        List<T> result = new ArrayList<>(datedValues.size()); // SCIPIO: switched to ArrayList
        Iterator<T> iter = datedValues.iterator();

        if (allAreSame) {
            ModelField fromDateField = null;
            ModelField thruDateField = null;

            if (iter.hasNext()) {
                T datedValue = iter.next();

                /**
                 * SCIPIO: 2018-09-29: When filterByDate is used on an entity without fromDate/thruDate, we will
                 * log as an error instead of throwing exception and crashing the system.
                 * This is because due to entitymodel changes it's extremely common to accidentally add
                 * a .filterByDate() call, so at least this way this error will not cause significant damage.
                 * Since in 90% of cases the bugfix is simply to remove the call, this is a fairly safe way to
                 * address the issue.
                 */
                fromDateField = datedValue.getModelEntity().getField(fromDateName);
                // SCIPIO: EntityFieldNotFoundException
                //if (fromDateField == null) throw new EntityFieldNotFoundException("\"" + fromDateName + "\" is not a field of " + datedValue.getEntityName());
                if (fromDateField == null) { // SCIPIO: 2018-09-29
                    Exception e = new EntityFieldNotFoundException("\"" + fromDateName + "\" is not a field of " + datedValue.getEntityName());
                    Debug.logError(e, "Query error: " + e.getMessage() + "; skipping date filter", module);
                    return new ArrayList<>(datedValues);
                }
                thruDateField = datedValue.getModelEntity().getField(thruDateName);
                // SCIPIO: EntityFieldNotFoundException
                //if (thruDateField == null) throw new EntityFieldNotFoundException("\"" + thruDateName + "\" is not a field of " + datedValue.getEntityName());
                if (thruDateField == null) { // SCIPIO: 2018-09-29
                    Exception e = new EntityFieldNotFoundException("\"" + thruDateName + "\" is not a field of " + datedValue.getEntityName());
                    Debug.logError(e, "Query error: " + e.getMessage() + "; skipping date filter", module);
                    return new ArrayList<>(datedValues);
                }

                java.sql.Timestamp fromDate = (java.sql.Timestamp) datedValue.dangerousGetNoCheckButFast(fromDateField);
                java.sql.Timestamp thruDate = (java.sql.Timestamp) datedValue.dangerousGetNoCheckButFast(thruDateField);

                if ((thruDate == null || thruDate.after(moment)) && (fromDate == null || fromDate.before(moment) || fromDate.equals(moment))) {
                    result.add(datedValue);
                }// else not active at moment
            }
            while (iter.hasNext()) {
                T datedValue = iter.next();
                java.sql.Timestamp fromDate = (java.sql.Timestamp) datedValue.dangerousGetNoCheckButFast(fromDateField);
                java.sql.Timestamp thruDate = (java.sql.Timestamp) datedValue.dangerousGetNoCheckButFast(thruDateField);

                if ((thruDate == null || thruDate.after(moment)) && (fromDate == null || fromDate.before(moment) || fromDate.equals(moment))) {
                    result.add(datedValue);
                }// else not active at moment
            }
        } else {
            // if not all values are known to be of the same entity, must check each one...
            while (iter.hasNext()) {
                T datedValue = iter.next();
                try {
                    java.sql.Timestamp fromDate = datedValue.getTimestamp(fromDateName);
                    java.sql.Timestamp thruDate = datedValue.getTimestamp(thruDateName);

                    if ((thruDate == null || thruDate.after(moment)) && (fromDate == null || fromDate.before(moment) || fromDate.equals(moment))) {
                        result.add(datedValue);
                    }// else not active at moment
                } catch(EntityFieldNotFoundException e) { // SCIPIO: 2018-09-29
                    Debug.logError(e, "Query error: " + e.getMessage() + "; skipping date filter", module);
                    result.add(datedValue);
                }
            }
        }

        return result;
    }

    public static boolean isValueActive(GenericValue datedValue, java.sql.Timestamp moment) {
        return isValueActive(datedValue, moment, "fromDate", "thruDate");
    }

    public static boolean isValueActive(GenericValue datedValue, java.sql.Timestamp moment, String fromDateName, String thruDateName) {
        java.sql.Timestamp fromDate = datedValue.getTimestamp(fromDateName);
        java.sql.Timestamp thruDate = datedValue.getTimestamp(thruDateName);

        if ((thruDate == null || thruDate.after(moment)) && (fromDate == null || fromDate.before(moment) || fromDate.equals(moment))) {
            return true;
        } else {
            // else not active at moment
            return false;
        }
    }

    /**
     *returns the values that match the values in fields
     *
     *@param values List of GenericValues
     *@param fields the field-name/value pairs that must match
     *@return List of GenericValue's that match the values in fields
     */
    public static <T extends GenericEntity> List<T> filterByAnd(List<T> values, Map<String, ? extends Object> fields) {
        if (values == null) return null;

        List<T> result = null;
        if (UtilValidate.isEmpty(fields)) {
            result = new ArrayList<>(values); // SCIPIO: switched to ArrayList
            //result.addAll(values);
        } else {
            result = new ArrayList<>(values.size()); // SCIPIO: switched to ArrayList
            for (T value: values) {
                if (value.matchesFields(fields)) {
                    result.add(value);
                }// else did not match
            }
        }
        return result;
    }

    /**
     *returns the values that match all of the exprs in list
     *
     *@param values List of GenericValues
     *@param exprs the expressions that must validate to true
     *@return List of GenericValue's that match the values in fields
     */
    public static <T extends GenericEntity> List<T> filterByAnd(List<T> values, List<? extends EntityCondition> exprs) {
        if (values == null) return null;
        if (UtilValidate.isEmpty(exprs)) {
            // no constraints... oh well
            return values;
        }

        List<T> result = new ArrayList<>(values.size()); // SCIPIO: switched to ArrayList
        for (T value: values) {
            boolean include = true;

            for (EntityCondition condition: exprs) {
                include = condition.entityMatches(value);
                if (!include) break;
            }
            if (include) {
                result.add(value);
            }
        }
        return result;
    }

    /**
     *returns the values that match any of the exprs in list
     *
     *@param values List of GenericValues
     *@param exprs the expressions that must validate to true
     *@return List of GenericValue's that match the values in fields
     */
    public static <T extends GenericEntity> List<T> filterByOr(List<T> values, List<? extends EntityCondition> exprs) {
        if (values == null || UtilValidate.isEmpty(exprs)) {
            return values;
        }

        List<T> result = new ArrayList<>(values.size()); // SCIPIO: switched to ArrayList
        for (T value: values) {
            boolean include = false;

            for (EntityCondition condition: exprs) {
                include = condition.entityMatches(value);
                if (include) break;
            }
            if (include) {
                result.add(value);
            }
        }
        return result;
    }

    /**
     *returns the values in the order specified after with localized value
     *
     *@param values List of GenericValues
     *@param orderBy The fields of the named entity to order the query by;
     *      optionally add a " ASC" for ascending or " DESC" for descending
     *@param locale Locale use to retrieve localized value
     *@return List of GenericValue's in the proper order
     */
    public static <T extends GenericEntity> List<T> localizedOrderBy(Collection<T> values, List<String> orderBy, Locale locale) {
        if (values == null) return null;
        if (values.isEmpty()) return new ArrayList<T>();
        //force check entity label before order by
        List<T> localizedValues = new ArrayList<T>();
        for (T value : values) {
            @SuppressWarnings("unchecked")
            T newValue = (T) value.clone();
            for (String orderByField : orderBy) {
                if (orderByField.endsWith(" DESC")) {
                    orderByField= orderByField.substring(0, orderByField.length() - 5);
                } else if (orderByField.endsWith(" ASC")) {
                    orderByField= orderByField.substring(0, orderByField.length() - 4);
                } else if (orderByField.startsWith("-")
                        || orderByField.startsWith("+")) {
                    orderByField= orderByField.substring(1, orderByField.length());
                }
                newValue.put(orderByField, value.get(orderByField, locale));
            }
            localizedValues.add(newValue);
        }
        return orderBy(localizedValues, orderBy);
    }

    /**
     *returns the values in the order specified
     *
     *@param values List of GenericValues
     *@param orderBy The fields of the named entity to order the query by;
     *      optionally add a " ASC" for ascending or " DESC" for descending
     *@return List of GenericValue's in the proper order
     */
    public static <T extends GenericEntity> List<T> orderBy(Collection<T> values, List<String> orderBy) {
        if (values == null) return null;
        if (values.isEmpty()) return new ArrayList<T>();
        if (UtilValidate.isEmpty(orderBy)) {
            // SCIPIO: Optimized
            //List<T> newList = new ArrayList<T>();
            //newList.addAll(values);
            //return newList;
            return new ArrayList<T>(values);
        }

        // SCIPIO: Optimized
        //List<T> result = new ArrayList<T>();
        //result.addAll(values);
        List<T> result = new ArrayList<T>(values);
        if (Debug.verboseOn()) Debug.logVerbose("Sorting " + values.size() + " values, orderBy=" + orderBy.toString(), module);
        Collections.sort(result, new OrderByList(orderBy));
        return result;
    }

    /**
     * @deprecated use {@link #getRelated(String, Map, List, boolean)}
     */
    @Deprecated
    public static List<GenericValue> getRelated(String relationName, List<GenericValue> values) throws GenericEntityException {
        Debug.logWarning("deprecated EntityUtil method, please replace as suggested in API Java Doc: getRelated(String, List)", module); // SCIPIO: fixed message (removed stack trace, not that useful for us)
        return getRelated(relationName, null, values, false);
    }

    public static List<GenericValue> getRelated(String relationName, Map<String, ? extends Object> fields, List<GenericValue> values, boolean useCache) throws GenericEntityException {
        if (values == null) return null;

        List<GenericValue> result = new ArrayList<>(values.size()); // SCIPIO: switched to ArrayList
        for (GenericValue value: values) {
            result.addAll(value.getRelated(relationName, fields, null, useCache));
        }
        return result;
    }

    public static <T extends GenericEntity> List<T> filterByCondition(List<T> values, EntityCondition condition) {
        if (values == null) return null;

        List<T> result = new ArrayList<>(values.size()); // SCIPIO: switched to ArrayList
        for (T value: values) {
            if (condition.entityMatches(value)) {
                result.add(value);
            }
        }
        return result;
    }

    public static <T extends GenericEntity> List<T> filterOutByCondition(List<T> values, EntityCondition condition) {
        if (values == null) return null;

        List<T> result = new ArrayList<>(values.size()); // SCIPIO: switched to ArrayList
        for (T value: values) {
            if (!condition.entityMatches(value)) {
                result.add(value);
            }
        }
        return result;
    }

    public static List<GenericValue> findDatedInclusionEntity(Delegator delegator, String entityName, Map<String, ? extends Object> search) throws GenericEntityException {
        return findDatedInclusionEntity(delegator, entityName, search, UtilDateTime.nowTimestamp());
    }

    public static List<GenericValue> findDatedInclusionEntity(Delegator delegator, String entityName, Map<String, ? extends Object> search, Timestamp now) throws GenericEntityException {
        EntityCondition searchCondition = EntityCondition.makeCondition(UtilMisc.toList(
                EntityCondition.makeCondition(search), EntityUtil.getFilterByDateExpr(now)));
        return EntityQuery.use(delegator).from(entityName).where(searchCondition).orderBy("-fromDate").queryList();
    }

    public static GenericValue newDatedInclusionEntity(Delegator delegator, String entityName, Map<String, ? extends Object> search) throws GenericEntityException {
        return newDatedInclusionEntity(delegator, entityName, search, UtilDateTime.nowTimestamp());
    }

    public static GenericValue newDatedInclusionEntity(Delegator delegator, String entityName, Map<String, ? extends Object> find, Timestamp now) throws GenericEntityException {
        Map<String, Object> search;
        List<GenericValue> entities = findDatedInclusionEntity(delegator, entityName, find, now);
        if (UtilValidate.isNotEmpty(entities)) {
            search = null;
            for (GenericValue entity: entities) {
                if (now.equals(entity.get("fromDate"))) {
                    search = new HashMap<String, Object>();
                    for (Map.Entry<String, ? super Object> entry: entity.getPrimaryKey().entrySet()) {
                        search.put(entry.getKey(), entry.getValue());
                    }
                    entity.remove("thruDate");
                } else {
                    entity.set("thruDate",now);
                }
                entity.store();
            }
            if (search == null) {
                search = new HashMap<String, Object>();
                search.putAll(EntityUtil.getFirst(entities));
            }
        } else {
            /* why is this being done? leaving out for now...
            search = new HashMap(search);
            */
            search = new HashMap<String, Object>();
            search.putAll(find);
        }
        if (now.equals(search.get("fromDate"))) {
            return EntityUtil.getOnly(EntityQuery.use(delegator).from(entityName).where(search).queryList());
        } else {
            search.put("fromDate",now);
            search.remove("thruDate");
            return delegator.makeValue(entityName, search);
        }
    }

    public static void delDatedInclusionEntity(Delegator delegator, String entityName, Map<String, ? extends Object> search) throws GenericEntityException {
        delDatedInclusionEntity(delegator, entityName, search, UtilDateTime.nowTimestamp());
    }

    public static void delDatedInclusionEntity(Delegator delegator, String entityName, Map<String, ? extends Object> search, Timestamp now) throws GenericEntityException {
        List<GenericValue> entities = findDatedInclusionEntity(delegator, entityName, search, now);
        for (GenericValue entity: entities) {
            entity.set("thruDate",now);
            entity.store();
        }
    }

    public static <T> List<T> getFieldListFromEntityList(List<GenericValue> genericValueList, String fieldName, boolean distinct) {
        if (genericValueList == null || fieldName == null) {
            return null;
        }
        List<T> fieldList = new ArrayList<>(genericValueList.size()); // SCIPIO: switched to ArrayList
        Set<T> distinctSet = null;
        if (distinct) {
            distinctSet = new HashSet<T>();
        }

        for (GenericValue value: genericValueList) {
            T fieldValue = UtilGenerics.<T>cast(value.get(fieldName));
            if (fieldValue != null) {
                if (distinct) {
                    if (!distinctSet.contains(fieldValue)) {
                        fieldList.add(fieldValue);
                        distinctSet.add(fieldValue);
                    }
                } else {
                    fieldList.add(fieldValue);
                }
            }
        }

        return fieldList;
    }

    public static <T> List<T> getFieldListFromEntityListIterator(EntityListIterator genericValueEli, String fieldName, boolean distinct) {
        if (genericValueEli == null || fieldName == null) {
            return null;
        }
        List<T> fieldList = new ArrayList<>(); // SCIPIO: switched to ArrayList
        Set<T> distinctSet = null;
        if (distinct) {
            distinctSet = new HashSet<T>();
        }

        GenericValue value = null;
        while ((value = genericValueEli.next()) != null) {
            T fieldValue = UtilGenerics.<T>cast(value.get(fieldName));
            if (fieldValue != null) {
                if (distinct) {
                    if (!distinctSet.contains(fieldValue)) {
                        fieldList.add(fieldValue);
                        distinctSet.add(fieldValue);
                    }
                } else {
                    fieldList.add(fieldValue);
                }
            }
        }

        return fieldList;
    }

    /**
     * Returns <code>true</code> if multi-tenant has been enabled.
     * <p>Multi-tenant features are enabled by setting the <code>multitenant</code>
     * property in <code>general.properties</code> to "Y".</p>
     */
    public static boolean isMultiTenantEnabled() {
        return "Y".equalsIgnoreCase(UtilProperties.getPropertyValue("general", "multitenant"));
    }

    /**
     * @param viewIndex
     * @param viewSize
     * @return the calculated start index based on viewIndex and viewSize
     * @see EntityUtil#getPagedList
     */
    public static int getStartIndexFromViewIndex(int viewIndex, int viewSize) {
        if (viewIndex == 0) {
            return 1;
        }
        return (viewIndex * viewSize) + 1;
    }

    /**
     * @param iter EntityListIterator
     * @param viewIndex
     * @param viewSize
     * @return PagedList object with a subset of data items from EntityListIterator based on viewIndex and viewSize
     * @throws GenericEntityException
     * @see org.ofbiz.entity.util.EntityListIterator
     */
    public static PagedList<GenericValue> getPagedList(EntityListIterator iter, int viewIndex, int viewSize) throws GenericEntityException {
        int startIndex = getStartIndexFromViewIndex(viewIndex, viewSize);
        int endIndex = (startIndex + viewSize) - 1;

        List<GenericValue> dataItems = iter.getPartialList(startIndex, viewSize);
        if (dataItems.size() < viewIndex) {
            endIndex = (endIndex - viewSize) + dataItems.size();
        }

        int size = iter.getResultsSizeAfterPartialList();
        if (endIndex > size) {
            endIndex = size;
        }

        return new PagedList<GenericValue>(startIndex, endIndex, size, viewIndex, viewSize, dataItems);
    }

    /**
     * SCIPIO: Type conversion of Strings that resemble "Y" or "N" values to Boolean
     * @deprecated use {@link org.ofbiz.base.util.UtilMisc#booleanValueIndicator(Object)} instead.
     */
    @Deprecated
    public static Boolean toBoolean(String value) {
        if ("Y".equalsIgnoreCase(value)) {
            return true;
        } else if ("N".equalsIgnoreCase(value)) {
            return false;
        } else {
            return null;
        }
    }

    /**
     * SCIPIO: Type conversion of Strings that resemble "Y" or "N" values to Boolean
     * @deprecated use {@link org.ofbiz.base.util.UtilMisc#booleanValueIndicator(Object, boolean)} instead.
     */
    @Deprecated
    public static boolean toBoolean(String indicator, boolean defaultVal) {
        Boolean result = toBoolean(indicator);
        return result != null ? result : defaultVal;
    }

    /**
     * SCIPIO: A list of entities names which are typically dangerous to update, remove or import.
     * Includes:
     * <ul>
     *   <li>EntityKeyStore - Database-specific entity field encryption keys</li>
     * </ul>
     * First added 2018-09-17.
     */
    public static Set<String> getUnsafeEntitiesForUpdate(Delegator delegator) {
        return unsafeEntitiesForUpdate;
    }

    /*
     * SCIPIO: COMPLETELY DEPRECATED METHODS
     * These were removed all the way back in ofbiz 15 and should never be used anymore.
     * They are moved down here in Scipio to avoid maintenance issues.
     */

    /**
     * @deprecated use {@link #getRelated(String, Map, List, boolean)}
     */
    @Deprecated
    public static List<GenericValue> getRelatedCache(String relationName, List<GenericValue> values) throws GenericEntityException {
        Debug.logWarning("Scipio: highly deprecated EntityUtil method called, please replace call: getRelatedCache(String, List)", module);
        return getRelated(relationName, null, values, true);
    }

    /**
     * @deprecated use {@link #getRelated(String, Map, List, boolean)}
     */
    @Deprecated
    public static List<GenericValue> getRelatedByAnd(String relationName, Map<String, ? extends Object> fields, List<GenericValue> values) throws GenericEntityException {
        Debug.logWarning("Scipio: highly deprecated EntityUtil method called, please replace call: getRelatedByAnd(String, Map, List)", module);
        return getRelated(relationName, fields, values, false);
    }

    /**
     * @deprecated use {@link #getRelated(String, Map, List, boolean)}
     */
    @Deprecated
    public static List<GenericValue> getRelatedByAndCache(String relationName, Map<String, ? extends Object> fields, List<GenericValue> values) throws GenericEntityException {
        Debug.logWarning("Scipio: highly deprecated EntityUtil method called, please replace call: getRelatedByAndCache(String, Map, List)", module);
        return getRelated(relationName, fields, values, true);
    }
    
    /**
     * SCIPIO: An utility method used to filter a list of mixed entities by entityName
     * 
     * @param originalList
     * @param entityName
     * @return
     */
    public static List<GenericValue> filterByEntityName(List<GenericValue> originalList, String entityName) {
        List<GenericValue> filteredList = UtilMisc.newList();
        for (GenericValue value : originalList ) {
            if (value.getEntityName().equals(entityName)) {
                filteredList.add(value);
            }
        }
        return filteredList;
    }

    /**
     * SCIPIO: Returns a list where the values having null for the given field are moved to the beginning.
     */
    public static <T extends GenericEntity> List<T> getNullsFirst(Collection<T> values, String fieldName) {
        if (values == null) return null;
        if (values.isEmpty()) return new ArrayList<T>();
        if (UtilValidate.isEmpty(fieldName)) {
            return new ArrayList<T>(values);
        }
        List<T> result = new ArrayList<T>(values.size());
        List<T> nonNullValues = new ArrayList<T>(values.size());
        for(T value : values) {
            if (value.get(fieldName) != null) {
                nonNullValues.add(value);
            } else {
                result.add(value);
            }
        }
        result.addAll(nonNullValues);
        return result;
    }

    /**
     * SCIPIO: Returns a list where the values having null for the given field are moved to the end.
     */
    public static <T extends GenericEntity> List<T> getNullsLast(Collection<T> values, String fieldName) {
        if (values == null) return null;
        if (values.isEmpty()) return new ArrayList<T>();
        if (UtilValidate.isEmpty(fieldName)) {
            return new ArrayList<T>(values);
        }
        List<T> result = new ArrayList<T>(values.size());
        List<T> nullValues = new ArrayList<T>(values.size());
        for(T value : values) {
            if (value.get(fieldName) != null) {
                result.add(value);
            } else {
                nullValues.add(value);
            }
        }
        result.addAll(nullValues);
        return result;
    }

    /**
     * SCIPIO: For each record extracts a shortPk string that maps to it.
     */
    public static <M extends Map<String, Object>> Map<String, M> makeShortPkRecordMap(Collection<? extends M> records,
                                                                                      Collection<String> pkFieldNames,
                                                                                      Map<String, M> outMap) throws IllegalArgumentException {
        return UtilMisc.makeShortKeyRecordMap(records, pkFieldNames, "::", outMap);
    }

    /**
     * SCIPIO: For each record extracts a shortPk string that maps to it.
     */
    public static <M extends Map<String, Object>> Map<String, M> makeShortPkRecordMap(Collection<? extends M> records,
                                                                                      Collection<String> pkFieldNames) throws IllegalArgumentException {
        return makeShortPkRecordMap(records, pkFieldNames, new LinkedHashMap<>());
    }

    public static String getShortPk(Map<String, ?> record, Collection<?> pkFields) { // SCIPIO
        if (pkFields.size() == 1) {
            return String.valueOf(record.get(NamedElement.getName(UtilMisc.first(pkFields))));
        } else if (pkFields.size() <= 0) {
            return null;
        }
        StringBuilder sb = new StringBuilder();
        for(Object fieldNameObj : pkFields) {
            if (sb.length() > 0) {
                sb.append("::");
            }
            sb.append(record.get(NamedElement.getName(fieldNameObj)));
        }
        return sb.toString();
    }

    public static String getShortPk(Map<String, ?> pk, ModelEntity model) { // SCIPIO
        return getShortPk(pk, model.getPkFields());
    }

    public static <M extends Map<String, Object>> M getPk(Object out, Map<String, ?> record, Collection<?> pkFields, boolean nonNull) { // SCIPIO
        M outMap = null;
        for(Object fieldNameObj : pkFields) {
            String fieldName = NamedElement.getName(fieldNameObj);
            Object value = record.get(fieldName);
            if (!nonNull || value != null) {
                if (outMap == null) {
                    if (out instanceof Map) {
                        outMap = UtilGenerics.cast(out);
                    } else if (out instanceof Supplier) {
                        outMap = UtilGenerics.<Supplier<M>>cast(out).get();
                    } else {
                        outMap = UtilGenerics.cast(new LinkedHashMap<>());
                    }
                }
                outMap.put(fieldName, value);
            }
        }
        return outMap;
    }

    public static <M extends Map<String, Object>> M getPk(M out, Map<String, ?> record, ModelEntity model, boolean nonNull) { // SCIPIO
        return getPk(out, record, model.getPkFields(), nonNull);
    }

    public static <M extends Map<String, Object>> M getPk(Map<String, ?> record, Collection<?> pkFields, boolean nonNull) { // SCIPIO
        return getPk(null, record, pkFields, nonNull);
    }

    public static <M extends Map<String, Object>> M getPk(Map<String, ?> record, ModelEntity model, boolean nonNull) { // SCIPIO
        return getPk(null, record, model.getPkFields(), nonNull);
    }

    public static boolean hasPk(Map<String, ?> record, Collection<?> pkFields) { // SCIPIO
        if (pkFields.size() == 1) {
            return (record.get(NamedElement.getName(UtilMisc.first(pkFields))) != null);
        } else if (pkFields.size() <= 0) {
            return false;
        }
        for(Object fieldNameObj : pkFields) {
            if (record.get(NamedElement.getName(fieldNameObj)) == null) {
                return false;
            }
        }
        return true;
    }

    public static boolean hasPk(Map<String, ?> pk, ModelEntity model) { // SCIPIO
        return hasPk(pk, model.getPkFields());
    }
}
