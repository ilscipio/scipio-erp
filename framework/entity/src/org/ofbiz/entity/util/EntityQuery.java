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

import java.sql.Timestamp;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Date;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.UtilGenerics;
import org.ofbiz.base.util.UtilMisc;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.base.util.collections.PagedList;
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.EntityFieldNotFoundException;
import org.ofbiz.entity.GenericEntityException;
import org.ofbiz.entity.GenericPK;
import org.ofbiz.entity.GenericValue;
import org.ofbiz.entity.condition.EntityCondition;
import org.ofbiz.entity.condition.EntityJoinOperator;
import org.ofbiz.entity.model.DynamicViewEntity;
import org.ofbiz.entity.model.ModelEntity;

/**
 * Used to setup various options for and subsequently execute entity queries.
 *
 * All methods to set options modify the EntityQuery instance then return this modified object to allow method call chaining. It is
 * important to note that this object is not immutable and is modified internally, and returning EntityQuery is just a
 * self reference for convenience.
 *
 * After a query the object can be further modified and then used to perform another query if desired.
 */
public class EntityQuery {

    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());

    private Delegator delegator;
    private String entityName = null;
    private DynamicViewEntity dynamicViewEntity = null;
    private boolean useCache = false;
    private EntityCondition whereEntityCondition = null;
    private Set<String> fieldsToSelect = null;
    private List<String> orderBy = null;
    private Integer resultSetType = EntityFindOptions.TYPE_FORWARD_ONLY;
    private Integer fetchSize = null;
    private Integer maxRows = null;
    private Boolean distinct = null;
    private EntityCondition havingEntityCondition = null;
    private boolean filterByDate = false;
    private Timestamp filterByDateMoment;
    private List<String> filterByFieldNames = null;
    private boolean searchPkOnly = false;
    private Map<String, Object> fieldMap = null;



    /** Construct an EntityQuery object for use against the specified Delegator
     * @param delegator The delegator instance to use for the query
     */
    public static EntityQuery use(Delegator delegator) {
        return new EntityQuery(delegator);
    }

    /** Construct an EntityQuery object for use against the specified Delegator
     * @param delegator The delegator instance to use for the query
     */
    public EntityQuery(Delegator delegator) {
        this.delegator = delegator;
    }

    /** Set the fields to be returned when the query is executed.
     *
     * Note that the select methods are not additive, if a subsequent
     * call is made to select then the existing fields for selection
     * will be replaced.
     * @param fieldsToSelect - A Set of Strings containing the field names to be selected
     * @return this EntityQuery object, to enable chaining
     */
    public EntityQuery select(Set<String> fieldsToSelect) {
        this.fieldsToSelect = fieldsToSelect;
        return this;
    }

    /** Set the fields to be returned when the query is executed.
     *
     * Note that the select methods are not additive, if a subsequent
     * call is made to select then the existing fields for selection
     * will be replaced.
     * @param fields - Strings containing the field names to be selected
     * @return this EntityQuery object, to enable chaining
     */
    public EntityQuery select(String...fields) {
        this.fieldsToSelect = UtilMisc.toSetArray(fields);
        return this;
    }

    /** Set the entity to query against
     * @param entityName - The name of the entity to query against
     * @return this EntityQuery object, to enable chaining
     */
    public EntityQuery from(String entityName) {
        this.entityName = entityName;
        this.dynamicViewEntity = null;
        return this;
    }

    /** Set the entity to query against
     * @param dynamicViewEntity - The DynamicViewEntity object to query against
     * @return this EntityQuery object, to enable chaining
     */
    public EntityQuery from(DynamicViewEntity dynamicViewEntity) {
        this.dynamicViewEntity  = dynamicViewEntity;
        this.entityName = null;
        return this;
    }

    /** Set the EntityCondition to be used as the WHERE clause for the query
     *
     * NOTE: Each successive call to any of the where(...) methods will replace the currently set condition for the query.
     * @param entityCondition - An EntityCondition object to be used as the where clause for this query
     * @return this EntityQuery object, to enable chaining
     */
    public EntityQuery where(EntityCondition entityCondition) {
        this.whereEntityCondition = entityCondition;
        return this;
    }

    /** Set a Map of field name/values to be ANDed together as the WHERE clause for the query
     *
     * NOTE: Each successive call to any of the where(...) methods will replace the currently set condition for the query.
     * @param fieldMap - A Map of field names/values to be ANDed together as the where clause for the query
     * @return this EntityQuery object, to enable chaining
     */
    public EntityQuery where(Map<String, Object> fieldMap) {
        this.fieldMap = fieldMap;
        return this;
    }

    /** Set a series of field name/values to be ANDed together as the WHERE clause for the query
     *
     * NOTE: Each successive call to any of the where(...) methods will replace the currently set condition for the query.
     * @param fields - A series of field names/values to be ANDed together as the where clause for the query
     * @return this EntityQuery object, to enable chaining
     */
    public EntityQuery where(Object...fields) {
        // SCIPIO: This needlessly complicates internals and is done later
        //this.whereEntityCondition = EntityCondition.makeCondition(UtilMisc.toMap(fields));
        this.fieldMap = UtilMisc.toMap(fields);
        return this;
    }

    /** Set a series of EntityConditions to be ANDed together as the WHERE clause for the query
     *
     * NOTE: Each successive call to any of the where(...) methods will replace the currently set condition for the query.
     * @param entityCondition - A series of EntityConditions to be ANDed together as the where clause for the query
     * @return this EntityQuery object, to enable chaining
     */
    public EntityQuery where(EntityCondition...entityCondition) {
        this.whereEntityCondition = EntityCondition.makeCondition(Arrays.asList(entityCondition));
        return this;
    }

    /** Set a list of EntityCondition objects to be ANDed together as the WHERE clause for the query
     *
     * NOTE: Each successive call to any of the where(...) methods will replace the currently set condition for the query.
     * @param andConditions - A list of EntityCondition objects to be ANDed together as the WHERE clause for the query
     * @return this EntityQuery object, to enable chaining
     */
    public <T extends EntityCondition> EntityQuery where(List<T> andConditions) {
        this.whereEntityCondition = EntityCondition.makeCondition(andConditions);
        return this;
    }

    /** Set a list of EntityCondition objects to be combined together with given operator as the WHERE clause for the query
     *
     * NOTE: Each successive call to any of the where(...) methods will replace the currently set condition for the query.
     * <p>
     * SCIPIO: New, added 2018-05-17.
     * @param conditions - A list of EntityCondition objects to be combined together as the WHERE clause for the query
     * @param operator - The join operator
     * @return this EntityQuery object, to enable chaining
     */
    public <T extends EntityCondition> EntityQuery where(List<T> conditions, EntityJoinOperator operator) {
        this.whereEntityCondition = EntityCondition.makeCondition(conditions, operator);
        return this;
    }

    /** Set the EntityCondition to be used as the HAVING clause for the query.
     *
     * NOTE: Each successive call to any of the having(...) methods will replace the currently set condition for the query.
     * @param entityCondition - The EntityCondition object that specifies how to constrain
     *            this query after any groupings are done (if this is a view
     *            entity with group-by aliases)
     * @return this EntityQuery object, to enable chaining
     */
    public EntityQuery having(EntityCondition entityCondition) {
        this.havingEntityCondition = entityCondition;
        return this;
    }

    /** The fields of the named entity to order the resultset by; optionally add a " ASC" for ascending or " DESC" for descending
     *
     * NOTE: Each successive call to any of the orderBy(...) methods will replace the currently set orderBy fields for the query.
     * @param orderBy - The fields of the named entity to order the resultset by
     * @return this EntityQuery object, to enable chaining
     */
    public EntityQuery orderBy(List<String> orderBy) {
        this.orderBy = orderBy;
        return this;
    }

    /** The fields of the named entity to order the resultset by; optionally add a " ASC" for ascending or " DESC" for descending
     *
     * NOTE: Each successive call to any of the orderBy(...) methods will replace the currently set orderBy fields for the query.
     * @param fields - The fields of the named entity to order the resultset by
     * @return this EntityQuery object, to enable chaining
     */
    public EntityQuery orderBy(String...fields) {
        this.orderBy = Arrays.asList(fields);
        return this;
    }

    /** Indicate that the ResultSet object's cursor may move only forward (this is the default behavior)
     *
     * @return this EntityQuery object, to enable chaining
     */
    public EntityQuery cursorForwardOnly() {
        this.resultSetType = EntityFindOptions.TYPE_FORWARD_ONLY;
        return this;
    }

    /** Indicate that the ResultSet object's cursor is scrollable but generally sensitive to changes to the data that underlies the ResultSet.
     *
     * @return this EntityQuery object, to enable chaining
     */
    public EntityQuery cursorScrollSensitive() {
        this.resultSetType = EntityFindOptions.TYPE_SCROLL_SENSITIVE;
        return this;
    }

    /** Indicate that the ResultSet object's cursor is scrollable but generally not sensitive to changes to the data that underlies the ResultSet.
     *
     * @return this EntityQuery object, to enable chaining
     */
    public EntityQuery cursorScrollInsensitive() {
        this.resultSetType = EntityFindOptions.TYPE_SCROLL_INSENSITIVE;
        return this;
    }

    /** Specifies the fetch size for this query. -1 will fall back to datasource settings.
     * SCIPIO: now accepts null, which clears it (default).
     *
     * @param fetchSize - The fetch size for this query
     * @return this EntityQuery object, to enable chaining
     */
    public EntityQuery fetchSize(Integer fetchSize) {
        this.fetchSize = fetchSize;
        return this;
    }

    /** Specifies the max number of rows to return, 0 means all rows.
     * SCIPIO: now accepts null, which clears it (default).
     *
     * @param maxRows - the max number of rows to return
     * @return this EntityQuery object, to enable chaining
     */
    public EntityQuery maxRows(Integer maxRows) {
        this.maxRows = maxRows;
        return this;
    }

    /** Specifies that the values returned should be filtered to remove duplicate values.
     *
     * @return this EntityQuery object, to enable chaining
     */
    public EntityQuery distinct() {
        this.distinct = true;
        return this;
    }

    /** Specifies whether the values returned should be filtered to remove duplicate values.
     * SCIPIO: now accepts null, which clears it (default).
     *
     * @param distinct - boolean indicating whether the values returned should be filtered to remove duplicate values
     * @return this EntityQuery object, to enable chaining
     */
    public EntityQuery distinct(Boolean distinct) {
        this.distinct = distinct;
        return this;
    }

    /** Specifies whether results should be read from the cache (or written to the cache if the results have not yet been cached)
     *
     * @return this EntityQuery object, to enable chaining
     */
    public EntityQuery cache() {
        this.useCache = true;
        return this;
    }

    /** Specifies whether results should be read from the cache (or written to the cache if the results have not yet been cached)
     * SCIPIO: Now supports null which is same as false.
     *
     * @param useCache - boolean to indicate if the cache should be used or not
     * @return this EntityQuery object, to enable chaining
     */
    public EntityQuery cache(Boolean useCache) {
        this.useCache = (useCache != null) ? useCache : false;
        return this;
    }

    /** Specifies whether the query should return only values that are currently active using from/thruDate fields.
     * <p>
     * SCIPIO: 2018-09-29: This method no longer throws exception if the date field names
     * are invalid for the entity; instead a detailed error is logged. This is an extremely
     * easy error to make, and otherwise can cause needless critical failures on small errors
     * during upgrades.
     *
     * @return this EntityQuery object, to enable chaining
     */
    public EntityQuery filterByDate() {
        this.filterByDate  = true;
        this.filterByDateMoment = null;
        this.filterByFieldNames = null;
        return this;
    }

    /** Specifies whether the query should return only values that are currently active using from/thruDate fields (SCIPIO).
     * <p>
     * SCIPIO: 2018-09-29: This method no longer throws exception if the date field names
     * are invalid for the entity; instead a detailed error is logged. This is an extremely
     * easy error to make, and otherwise can cause needless critical failures on small errors
     * during upgrades.
     *
     * @return this EntityQuery object, to enable chaining
     */
    public EntityQuery filterByDate(QueryDateFilter filter) {
        if (filter != null) {
            this.filterByDate = filter.isEnabled();
            this.filterByDateMoment = filter.getMoment();
            this.filterByFieldNames = filter.getFieldNames();
        } else {
            this.filterByDate = false;
            this.filterByDateMoment = null;
            this.filterByFieldNames = null;
        }
        return this;
    }

    /** Specifies whether the query should return only values that are active during the specified moment using from/thruDate fields.
     * <p>
     * SCIPIO: 2018-09-29: This method no longer throws exception if the date field names
     * are invalid for the entity; instead a detailed error is logged. This is an extremely
     * easy error to make, and otherwise can cause needless critical failures on small errors
     * during upgrades.
     *
     * @param moment - Timestamp representing the moment in time that the values should be active during
     * @return this EntityQuery object, to enable chaining
     */
    public EntityQuery filterByDate(Timestamp moment) {
        if (moment != null) {
            this.filterByDate = true;
            this.filterByDateMoment = moment;
            this.filterByFieldNames = null;
        } else {
            // Maintain existing behavior exhibited by EntityUtil.filterByDate(moment) when moment is null and perform no date filtering
            this.filterByDate = false;
            this.filterByDateMoment = null;
            this.filterByFieldNames = null;
        }
        return this;
    }

    /** Specifies whether the query should return only values that are active during the specified moment using from/thruDate fields.
     * <p>
     * SCIPIO: 2018-09-29: This method no longer throws exception if the date field names
     * are invalid for the entity; instead a detailed error is logged. This is an extremely
     * easy error to make, and otherwise can cause needless critical failures on small errors
     * during upgrades.
     *
     * @param moment - Date representing the moment in time that the values should be active during
     * @return this EntityQuery object, to enable chaining
     */
    public EntityQuery filterByDate(Date moment) {
        this.filterByDate(new java.sql.Timestamp(moment.getTime()));
        return this;
    }

    /** Specifies whether the query should return only values that are currently active using the specified from/thru field name pairs.
     * <p>
     * SCIPIO: 2018-09-29: This method no longer throws exception if the date field names
     * are invalid for the entity; instead a detailed error is logged. This is an extremely
     * easy error to make, and otherwise can cause needless critical failures on small errors
     * during upgrades.
     *
     * @param filterByFieldName - String pairs representing the from/thru date field names e.g. "fromDate", "thruDate", "contactFromDate", "contactThruDate"
     * @return this EntityQuery object, to enable chaining
     */
    public EntityQuery filterByDate(String... filterByFieldName) {
        return this.filterByDate(null, filterByFieldName);
    }

    /** Specifies whether the query should return only values that are active during the specified moment using the specified from/thru field name pairs.
     * <p>
     * SCIPIO: 2018-09-29: This method no longer throws exception if the date field names
     * are invalid for the entity; instead a detailed error is logged. This is an extremely
     * easy error to make, and otherwise can cause needless critical failures on small errors
     * during upgrades.
     *
     * @param moment - Timestamp representing the moment in time that the values should be active during
     * @param filterByFieldName - String pairs representing the from/thru date field names e.g. "fromDate", "thruDate", "contactFromDate", "contactThruDate"
     * @return this EntityQuery object, to enable chaining
     */
    public EntityQuery filterByDate(Timestamp moment, String... filterByFieldName) {
        this.filterByDate  = true;
        this.filterByDateMoment = moment;
        if (filterByFieldName.length % 2 != 0) {
            throw new IllegalArgumentException("You must pass an even sized array to this method, each pair should represent a from date field name and a thru date field name");
        }
        this.filterByFieldNames = Arrays.asList(filterByFieldName);
        return this;
    }

    /** SCIPIO: Specifies whether the query should return only values that are active during the specified moment using from/thruDate fields,
     * using the "now" timestamp (current time), with explicit boolean toggle.
     * Added 2018-10-19.
     * <p>
     * SCIPIO: 2018-09-29: This method no longer throws exception if the date field names
     * are invalid for the entity; instead a detailed error is logged. This is an extremely
     * easy error to make, and otherwise can cause needless critical failures on small errors
     * during upgrades.
     *
     * @param enable - Enable or disable the filter-by-date filter.
     * @return this EntityQuery object, to enable chaining
     */
    public EntityQuery filterByDate(boolean enable) {
        this.filterByDate = enable;
        this.filterByDateMoment = null;
        this.filterByFieldNames = null;
        return this;
    }

    /** SCIPIO: Specifies whether the query should return only values that are active during the specified moment using from/thruDate fields,
     * using the specified moment if non-null OR, if null, using the "now" timestamp (current time),
     * with explicit boolean toggle.
     * Added 2017-11-27.
     * <p>
     * SCIPIO: 2018-09-29: This method no longer throws exception if the date field names
     * are invalid for the entity; instead a detailed error is logged. This is an extremely
     * easy error to make, and otherwise can cause needless critical failures on small errors
     * during upgrades.
     *
     * @param moment - Timestamp representing the moment in time that the values should be active during
     * @return this EntityQuery object, to enable chaining
     */
    public EntityQuery filterByDate(boolean enable, Timestamp moment) {
        this.filterByDate = enable;
        this.filterByDateMoment = (enable) ? moment : null;
        this.filterByFieldNames = null;
        return this;
    }

    /** SCIPIO: Specifies whether the query should return only values that are active during the specified moment using from/thruDate fields,
     * using the specified moment if non-null OR, if null, using the "now" timestamp (current time),
     * with explicit boolean toggle.
     * Added 2017-11-27.
     * <p>
     * SCIPIO: 2018-09-29: This method no longer throws exception if the date field names
     * are invalid for the entity; instead a detailed error is logged. This is an extremely
     * easy error to make, and otherwise can cause needless critical failures on small errors
     * during upgrades.
     *
     * @param moment - Date representing the moment in time that the values should be active during
     * @return this EntityQuery object, to enable chaining
     */
    public EntityQuery filterByDate(boolean enable, Date moment) {
        return this.filterByDate(enable, new java.sql.Timestamp(moment.getTime()));
    }

    /** SCIPIO: Specifies whether the query should return only values that are currently active using the specified from/thru field name pairs,
     * using the specified moment if non-null OR, if null, using the "now" timestamp (current time),
     * with explicit boolean toggle.
     * Added 2017-11-27.
     * <p>
     * SCIPIO: 2018-09-29: This method no longer throws exception if the date field names
     * are invalid for the entity; instead a detailed error is logged. This is an extremely
     * easy error to make, and otherwise can cause needless critical failures on small errors
     * during upgrades.
     *
     * @param enable Enable or disable the filter-by-date filter.
     * @param filterByFieldName - String pairs representing the from/thru date field names e.g. "fromDate", "thruDate", "contactFromDate", "contactThruDate"
     * @return this EntityQuery object, to enable chaining
     */
    public EntityQuery filterByDate(boolean enable, String... filterByFieldName) {
        return this.filterByDate(enable, (Timestamp) null, filterByFieldName);
    }

    /** SCIPIO: Specifies whether the query should return only values that are active during the specified moment using the specified from/thru field name pairs,
     * using the specified moment if non-null OR, if null, using the "now" timestamp (current time),
     * with explicit boolean toggle.
     * Added 2017-11-27.
     * <p>
     * SCIPIO: 2018-09-29: This method no longer throws exception if the date field names
     * are invalid for the entity; instead a detailed error is logged. This is an extremely
     * easy error to make, and otherwise can cause needless critical failures on small errors
     * during upgrades.
     *
     * @param enable Enable or disable the filter-by-date filter.
     * @param moment - Timestamp representing the moment in time that the values should be active during
     * @param filterByFieldName - String pairs representing the from/thru date field names e.g. "fromDate", "thruDate", "contactFromDate", "contactThruDate"
     * @return this EntityQuery object, to enable chaining
     */
    public EntityQuery filterByDate(boolean enable, Timestamp moment, String... filterByFieldName) {
        if (enable) {
            // SCIPIO: NOTE: this stock method interprets null as "now".
            this.filterByDate(moment, filterByFieldName);
        } else {
            this.filterByDate = false;
            this.filterByDateMoment = null;
            this.filterByFieldNames = null;
        }
        return this;
    }

    /** Executes the EntityQuery and returns a list of results
     *
     * @return Returns a List of GenericValues representing the results of the query
     */
    public List<GenericValue> queryList() throws GenericEntityException {
        return query(null);
    }

    /** Executes the EntityQuery and returns an EntityListIterator representing the result of the query.
     *
     * NOTE:  THAT THIS MUST BE CLOSED (preferably in a finally block) WHEN YOU
     *        ARE DONE WITH IT, AND DON'T LEAVE IT OPEN TOO LONG BEACUSE IT
     *        WILL MAINTAIN A DATABASE CONNECTION.
     *
     * @return Returns an EntityListIterator representing the result of the query
     */
    public EntityListIterator queryIterator() throws GenericEntityException {
        if (useCache) {
            Debug.logWarning("Call to iterator() with cache, ignoring cache" + toLogAppend(), module); // SCIPIO: Improved logging
        }
        if (dynamicViewEntity == null) {
            return delegator.find(entityName, makeWhereCondition(false), havingEntityCondition, fieldsToSelect, orderBy, makeEntityFindOptions());
        } else {
            return delegator.findListIteratorByCondition(dynamicViewEntity, makeWhereCondition(false), havingEntityCondition, fieldsToSelect, orderBy, makeEntityFindOptions());
        }
    }

    /** Executes the EntityQuery and returns the first result
     *
     * @return GenericValue representing the first result record from the query
     */
    public GenericValue queryFirst() throws GenericEntityException {
        EntityFindOptions efo = makeEntityFindOptions();
        // Only limit results when the query isn't filtering by date in memory against a cached result
        if (!this.useCache && !this.filterByDate) {
            efo.setMaxRows(1);
        }
        GenericValue result =  EntityUtil.getFirst(query(efo));
        return result;
    }

    /** Executes the EntityQuery and a single result record
     * <p>SCIPIO: NOTE: This method has been modified to primarily use {@link Delegator#findOne(String, Map, boolean)}
     * for the majority of lookups which lookup using {@link #where(Object...)} and {@link #where(Map)}
     * (but currently not {@link #where(EntityCondition)}). For non-dynamic entities: orderBy, filterByDate and most of the
     * find options are almost meaningless and should usually not be set, but if set may trigger legacy use of
     * {@link #queryList()}. If you need the original behavior, {@link #queryOneFromList()} is now available.</p>
     * TODO: Apply findOne to more cases
     *
     * @return GenericValue representing the only result record from the query
     */
    public GenericValue queryOne() throws GenericEntityException {
        this.searchPkOnly = true;
        // SCIPIO: This behavior caused the PK entity cache to be unused, which is unfortunate because
        // it's simpler and more efficient for PK queries, so instead, use the Delegator PK lookup method wherever possible
        //GenericValue result =  EntityUtil.getOnly(queryList());
        GenericValue result;
        if (dynamicViewEntity == null && !filterByDate && orderBy == null && !hasEntityFindOptions() &&
                whereEntityCondition == null && havingEntityCondition == null && fieldMap != null) {
            //if (Debug.verboseOn()) {
            //    Debug.logVerbose("queryOne: using findOne() implementation" + toLogAppend(), module);
            //}
            // TODO: REMOVE: in the future this PK check can be removed for performance reasons,
            //  is here due to ofbiz flaw in makeWhereCondition we don't want
            GenericPK pk = GenericPK.create(delegator.getModelEntity(entityName));
            pk.setPKFields(fieldMap);
            if (pk.size() < fieldMap.size()) {
                String msg = "queryOne(): Passed primary key is not a valid primary key for entity [" + entityName +
                        "], truncating: " + fieldMap.keySet();
                Debug.logError(new IllegalArgumentException(msg), msg, module);
                fieldMap = pk;
            }
            result = delegator.findOne(entityName, fieldMap, useCache);
            if (result != null && fieldsToSelect != null) {
                result = result.select(fieldsToSelect);
            }
        } else {
            if (Debug.verboseOn()) {
                Debug.logVerbose("queryOne: using queryList() implementation" + toLogAppend(), module);
            }
            result = EntityUtil.getOnly(queryList());
        }
        return result;
    }

    /** Executes the EntityQuery and a single result record, using {@link #queryList()} (SCIPIO).
     * <p>SCIPIO: NOTE: This is simply the old implementation of {@link #queryOne()} in case needed in specific cases.</p>
     *
     * @return GenericValue representing the only result record from the query
     */
    public GenericValue queryOneFromList() throws GenericEntityException {
        this.searchPkOnly = true;
        GenericValue result = EntityUtil.getOnly(queryList());
        return result;
    }

    /** Executes the EntityQuery and returns the result count
     *
     * If the query generates more than a single result then an exception is thrown
     *
     * @return GenericValue representing the only result record from the query
     */
    public long queryCount() throws GenericEntityException {
        if (dynamicViewEntity != null) {
            try (EntityListIterator iterator = queryIterator()) {
                return iterator.getResultsSizeAfterPartialList();
            }
        }
        return delegator.findCountByCondition(entityName, makeWhereCondition(false), havingEntityCondition, makeEntityFindOptions());
    }

    private List<GenericValue> query(EntityFindOptions efo) throws GenericEntityException {
        EntityFindOptions findOptions = null;
        if (efo == null) {
            findOptions = makeEntityFindOptions();
        } else {
            findOptions = efo;
        }
        List<GenericValue> result = null;
        if (dynamicViewEntity == null) {
            result = delegator.findList(entityName, makeWhereCondition(useCache), fieldsToSelect, orderBy, findOptions, useCache);
        } else {
            try (EntityListIterator it = queryIterator()) {
                result = it.getCompleteList();
            }
        }
        if (filterByDate && useCache) {
            try {
                return EntityUtil.filterByCondition(result, this.makeDateCondition());
            } catch(EntityFieldNotFoundException e) { // SCIPIO
                //Debug.logError(e, "Query error: " + e.getMessage() + toLogAppend(), module); // already logged
            }
        }
        return result;
    }

    private EntityFindOptions makeEntityFindOptions() {
        EntityFindOptions findOptions = new EntityFindOptions();
        if (resultSetType != null) {
            findOptions.setResultSetType(resultSetType);
        }
        if (fetchSize != null) {
            findOptions.setFetchSize(fetchSize);
        }
        if (maxRows != null) {
            findOptions.setMaxRows(maxRows);
        }
        if (distinct != null) {
            findOptions.setDistinct(distinct);
        }
        return findOptions;
    }

    private boolean hasEntityFindOptions() { // SCIPIO
        return (resultSetType != EntityFindOptions.TYPE_FORWARD_ONLY) || (fetchSize != null) || (maxRows != null) || (distinct != null);
    }

    private EntityCondition makeWhereCondition(boolean usingCache) {
        if (whereEntityCondition == null && fieldMap != null) {
            if (this.searchPkOnly) {
                //Resolve if the map contains a sub map parameters, use a containsKeys to avoid error when a GenericValue is given as map
                @SuppressWarnings("unchecked")
                Map<String, Object> parameters = fieldMap.containsKey("parameters") ? (Map<String, Object>) fieldMap.get("parameters") : null;
                // SCIPIO: The parameters map behavior may need to be deprecated due to security concerns, so for now warn:
                if (parameters != null) {
                    Debug.logWarning("EntityQuery.makeWhereCondition for queryOne(): using implicit parameters map for search PK" +
                            " (did you pass context to where()?); deprecated for security" + toLogAppend(), module);
                }
                GenericPK pk = GenericPK.create(delegator.getModelEntity(entityName));
                pk.setPKFields(parameters);
                pk.setPKFields(fieldMap);
                this.whereEntityCondition = EntityCondition.makeCondition(pk.getPrimaryKey());
            } else {
                this.whereEntityCondition = EntityCondition.makeCondition(fieldMap);
            }
        }
        // we don't use the useCache field here because not all queries will actually use the cache, e.g. findCountByCondition never uses the cache
        if (filterByDate && !usingCache) {
            try {
                if (whereEntityCondition != null) {
                    return EntityCondition.makeCondition(whereEntityCondition, this.makeDateCondition());
                } else {
                    return this.makeDateCondition();
                }
            } catch(EntityFieldNotFoundException e) { // SCIPIO
                //Debug.logError(e, "Query error: " + e.getMessage() + "; skipping date filter" + toLogAppend(), module); // already logged
            }
        }
        return whereEntityCondition;
    }

    private EntityCondition makeDateCondition() {
        List<EntityCondition> conditions = new ArrayList<>();
        if (UtilValidate.isEmpty(this.filterByFieldNames)) {
            this.filterByDate(filterByDateMoment, "fromDate", "thruDate");
        }

        for (int i = 0; i < this.filterByFieldNames.size();) {
            String fromDateFieldName = this.filterByFieldNames.get(i++);
            String thruDateFieldName = this.filterByFieldNames.get(i++);

            try { // SCIPIO
                checkEntityDateFields(delegator, entityName, fromDateFieldName, thruDateFieldName);
            } catch(EntityFieldNotFoundException e) {
                Debug.logError(e, "Query error: " + e.getMessage() + "; skipping date filter" + toLogAppend(), module); // SCIPIO: Improved logging
                continue;
            }

            if (filterByDateMoment == null) {
                conditions.add(EntityUtil.getFilterByDateExpr(fromDateFieldName, thruDateFieldName));
            } else {
                conditions.add(EntityUtil.getFilterByDateExpr(this.filterByDateMoment, fromDateFieldName, thruDateFieldName));
            }
        }

        if (conditions.isEmpty()) { // SCIPIO
            throw new EntityFieldNotFoundException("No date filters could be produced for entity '"
                    + entityName + " using field names: " + filterByFieldNames);
        }

        return EntityCondition.makeCondition(conditions);
    }

    /**
     * SCIPIO: 2018-09-29: When filterByDate is used on an entity without fromDate/thruDate, we will
     * log as an error instead of throwing exception and crashing the system.
     * This is because due to entitymodel changes it's extremely common to accidentally add
     * a .filterByDate() call, so at least this way this error will not cause significant damage.
     * Since in 90% of cases the bugfix is simply to remove the call, this is a fairly safe way to
     * address the issue.
     */
    private static void checkEntityDateFields(Delegator delegator, String entityName, String fromDateFieldName, String thruDateFieldName) throws EntityFieldNotFoundException {
        ModelEntity entityModel = delegator.getModelEntity(entityName);
        if (entityModel != null) { // If null, let regular call crash itself
            if (!entityModel.isField(fromDateFieldName)) {
                throw new EntityFieldNotFoundException("\"" + fromDateFieldName + "\" is not a field of "
                        + entityName);
            } else if (!entityModel.isField(thruDateFieldName)) {
                throw new EntityFieldNotFoundException("\"" + thruDateFieldName + "\" is not a field of "
                        + entityName);
            }
        }
    }

    public <T> List<T> getFieldList(final String fieldName) throws GenericEntityException {
        select(fieldName);
        try (EntityListIterator genericValueEli = queryIterator()) {
            if (Boolean.TRUE.equals(this.distinct)) {
                Set<T> distinctSet = new LinkedHashSet<T>(); // SCIPIO: fixed to LinkedHashSet from HashSet to preserve result order
                GenericValue value = null;
                while ((value = genericValueEli.next()) != null) {
                    T fieldValue = UtilGenerics.<T>cast(value.get(fieldName));
                    if (fieldValue != null) {
                        distinctSet.add(fieldValue);
                    }
                }
                return new ArrayList<T>(distinctSet);
            }
            else {
                List<T> fieldList = new ArrayList<T>(); // SCIPIO: switched to ArrayList
                GenericValue value = null;
                while ((value = genericValueEli.next()) != null) {
                    T fieldValue = UtilGenerics.<T>cast(value.get(fieldName));
                    if (fieldValue != null) {
                        fieldList.add(fieldValue);
                    }
                }
                return fieldList;
            }
        }
    }

    public <T> Set<T> getFieldSet(final String fieldName) throws GenericEntityException { // SCIPIO
        select(fieldName);
        try (EntityListIterator genericValueEli = queryIterator()) {
            if (Boolean.TRUE.equals(this.distinct)) {
                Set<T> distinctSet = new LinkedHashSet<T>(); // SCIPIO: fixed to LinkedHashSet from HashSet to preserve result order
                GenericValue value = null;
                while ((value = genericValueEli.next()) != null) {
                    T fieldValue = UtilGenerics.<T>cast(value.get(fieldName));
                    if (fieldValue != null) {
                        distinctSet.add(fieldValue);
                    }
                }
                return distinctSet;
            } else {
                Set<T> fieldList = new LinkedHashSet<T>();
                GenericValue value = null;
                while ((value = genericValueEli.next()) != null) {
                    T fieldValue = UtilGenerics.<T>cast(value.get(fieldName));
                    if (fieldValue != null) {
                        fieldList.add(fieldValue);
                    }
                }
                return fieldList;
            }
        }
    }

    public <T, C extends Collection<T>> C getFieldCollection(final String fieldName, final C collection) throws GenericEntityException { // SCIPIO
        select(fieldName);
        try (EntityListIterator genericValueEli = queryIterator()) {
            if (Boolean.TRUE.equals(this.distinct)) {
                Set<T> distinctSet = new LinkedHashSet<T>();
                GenericValue value = null;
                while ((value = genericValueEli.next()) != null) {
                    T fieldValue = UtilGenerics.<T>cast(value.get(fieldName));
                    if (fieldValue != null) {
                        distinctSet.add(fieldValue);
                    }
                }
                collection.addAll(distinctSet);
            } else {
                GenericValue value = null;
                while ((value = genericValueEli.next()) != null) {
                    T fieldValue = UtilGenerics.<T>cast(value.get(fieldName));
                    if (fieldValue != null) {
                        collection.add(fieldValue);
                    }
                }
            }
        }
        return collection;
    }

    /**
     * Query paged list.
     * @param viewIndex
     * @param viewSize
     * @return PagedList object with a subset of data items
     * @throws GenericEntityException
     * @see EntityUtil#getPagedList
     */
    public PagedList<GenericValue> queryPagedList(int viewIndex, int viewSize) throws GenericEntityException {
        try (EntityListIterator genericValueEli = queryIterator()) {
            return EntityUtil.getPagedList(genericValueEli, viewIndex, viewSize);
        }
    }

    /** SCIPIO: Executes the EntityQuery and returns a list of results; returns null if GenericEntityException.
     * NOTE: Unchecked exceptions representing programming errors may still be thrown.
     *
     * @return Returns a List of GenericValues representing the results of the query
     */
    public List<GenericValue> queryListSafe() {
        try {
            return queryList();
        } catch (GenericEntityException e) {
            Debug.logError(e, "Error in queryList: " + e.getMessage() + toLogAppend(), module);
            return null;
        }
    }

    /** SCIPIO: Executes the EntityQuery and returns an EntityListIterator representing the result of the query; returns null if GenericEntityException.
     * NOTE: Unchecked exceptions representing programming errors may still be thrown.
     *
     * NOTE:  THAT THIS MUST BE CLOSED (preferably in a finally block) WHEN YOU
     *        ARE DONE WITH IT, AND DON'T LEAVE IT OPEN TOO LONG BEACUSE IT
     *        WILL MAINTAIN A DATABASE CONNECTION.
     *
     * @return Returns an EntityListIterator representing the result of the query
     */
    public EntityListIterator queryIteratorSafe() {
        try {
            return queryIterator();
        } catch (GenericEntityException e) {
            Debug.logError(e, "Error in queryIterator: " + e.getMessage() + toLogAppend(), module);
            return null;
        }
    }

    /** SCIPIO: Executes the EntityQuery and returns the first result; returns null if GenericEntityException.
     * NOTE: Unchecked exceptions representing programming errors may still be thrown.
     *
     * @return GenericValue representing the first result record from the query
     */
    public GenericValue queryFirstSafe() {
        try {
            return queryFirst();
        } catch (GenericEntityException e) {
            Debug.logError(e, "Error in queryFirst: " + e.getMessage() + toLogAppend(), module);
            return null;
        }
    }

    /** SCIPIO: Executes the EntityQuery and a single result record; returns null if GenericEntityException.
     * NOTE: Unchecked exceptions representing programming errors may still be thrown.
     *
     * @return GenericValue representing the only result record from the query
     */
    public GenericValue queryOneSafe() {
        try {
            return queryOne();
        } catch (GenericEntityException e) {
            Debug.logError(e, "Error in queryOne: " + e.getMessage() + toLogAppend(), module);
            return null;
        }
    }

    /** SCIPIO: Executes the EntityQuery and a single result record; returns null if GenericEntityException.
     * NOTE: Unchecked exceptions representing programming errors may still be thrown.
     *
     * @return GenericValue representing the only result record from the query
     */
    public GenericValue queryOneFromListSafe() {
        try {
            return queryOneFromList();
        } catch (GenericEntityException e) {
            Debug.logError(e, "Error in queryOneFromList: " + e.getMessage() + toLogAppend(), module);
            return null;
        }
    }

    /** SCIPIO: Executes the EntityQuery and returns the result count; returns null if GenericEntityException.
     * NOTE: Unchecked exceptions representing programming errors may still be thrown.
     *
     * If the query generates more than a single result then zero is returned.
     *
     * @return GenericValue representing the only result record from the query
     */
    public long queryCountSafe() {
        try {
            return queryCount();
        } catch (GenericEntityException e) {
            Debug.logError(e, "Error in queryCount: " + e.getMessage() + toLogAppend(), module);
            return 0;
        }
    }

    public <T> List<T> getFieldListSafe(String fieldName) { // SCIPIO
        try {
            return getFieldList(fieldName);
        } catch (GenericEntityException e) {
            Debug.logError(e, "Error in getFieldList(): " + e.getMessage() + toLogAppend(), module);
            return null;
        }
    }

    public <T> Set<T> getFieldSetSafe(String fieldName) { // SCIPIO
        try {
            return getFieldSet(fieldName);
        } catch (GenericEntityException e) {
            Debug.logError(e, "Error in getFieldList(): " + e.getMessage() + toLogAppend(), module);
            return null;
        }
    }

    public <T, C extends Collection<T>> C getFieldCollectionSafe(String fieldName, C collection) { // SCIPIO
        try {
            return getFieldCollection(fieldName, collection);
        } catch (GenericEntityException e) {
            Debug.logError(e, "Error in getFieldList(): " + e.getMessage() + toLogAppend(), module);
            return null;
        }
    }

    /**
     * SCIPIO: Query paged list; returns null if GenericEntityException.
     * NOTE: Unchecked exceptions representing programming errors may still be thrown.
     * @param viewIndex
     * @param viewSize
     * @return PagedList object with a subset of data items
     * @throws GenericEntityException
     * @see EntityUtil#getPagedList
     */
    public PagedList<GenericValue> queryPagedListSafe(int viewIndex, int viewSize) {
        try {
            return queryPagedList(viewIndex, viewSize);
        } catch (GenericEntityException e) {
            Debug.logError(e, "Error in queryPagedList(): " + e.getMessage() + toLogAppend(), module);
            return null;
        }
    }

    @Override
    public String toString() { // SCIPIO: Debugging help
        return "{entityName='" + entityName + '\'' +
                ", dynamicViewEntity=" + dynamicViewEntity +
                ", useCache=" + useCache +
                ", whereEntityCondition=" + whereEntityCondition +
                ", fieldsToSelect=" + fieldsToSelect +
                ", orderBy=" + orderBy +
                ", resultSetType=" + resultSetType +
                ", fetchSize=" + fetchSize +
                ", maxRows=" + maxRows +
                ", distinct=" + distinct +
                ", havingEntityCondition=" + havingEntityCondition +
                ", filterByDate=" + filterByDate +
                ", filterByDateMoment=" + filterByDateMoment +
                ", filterByFieldNames=" + filterByFieldNames +
                //", searchPkOnly=" + searchPkOnly +
                ", fieldMap=" + fieldMap +
                '}';
    }

    protected String toLogAppend() { // SCIPIO: Debugging help
        return " - " + this;
    }

}
