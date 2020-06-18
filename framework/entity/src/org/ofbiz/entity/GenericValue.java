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

package org.ofbiz.entity;

import java.util.Arrays;
import java.util.Collection;
import java.util.List;
import java.util.Map;
import java.util.Objects;

import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.UtilMisc;
import org.ofbiz.entity.model.ModelEntity;


/**
 * Generic Entity Value Object - Handles persistence for any defined entity.
 *
 */
@SuppressWarnings("serial")
public class GenericValue extends GenericEntity {

    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());

    public static final GenericValue NULL_VALUE = new NullGenericValue();

    /** Creates new GenericValue */
    public static GenericValue create(ModelEntity modelEntity) {
        GenericValue newValue = new GenericValue();
        newValue.init(modelEntity);
        return newValue;
    }

    /** Creates new GenericValue from existing Map */
    public static GenericValue create(Delegator delegator, ModelEntity modelEntity, Map<String, ? extends Object> fields) {
        GenericValue newValue = new GenericValue();
        newValue.init(delegator, modelEntity, fields);
        return newValue;
    }

    /** Creates new GenericValue from existing Map */
    public static GenericValue create(Delegator delegator, ModelEntity modelEntity, Object singlePkValue) {
        GenericValue newValue = new GenericValue();
        newValue.init(delegator, modelEntity, singlePkValue);
        return newValue;
    }

    /** Creates new GenericValue from existing GenericValue */
    public static GenericValue create(GenericValue value) {
        GenericValue newValue = new GenericValue();
        newValue.init(value);
        return newValue;
    }

    /** Creates new GenericValue from existing GenericValue */
    public static GenericValue create(GenericPK primaryKey) {
        GenericValue newValue = new GenericValue();
        newValue.init(primaryKey);
        return newValue;
    }

    /** SCIPIO: Creates new GenericValue partially from fields from existing GenericValue with new-to-existing field name mappings, but treated as a "new" instance (not a "copy");
     * source fields are assumed to already be correct/same types as those on the new value (no type checks).<p>
     * NOTE: Instance members other than "fields" are treated as a "new" value, not copied from the passed value; this is half-way between
     * copy constructor and construction from map. Added 2018-10-22. */
    public static GenericValue createAsFieldSubset(Delegator delegator, ModelEntity modelEntity, GenericValue sourceFieldsValue, Map<String, String> newToExistingFieldMap) {
        GenericValue newValue = new GenericValue();
        newValue.initAsFieldSubset(delegator, modelEntity, sourceFieldsValue, newToExistingFieldMap);
        return newValue;
    }

    /** SCIPIO: Creates new GenericValue partially from fields from existing GenericValue with new-to-existing field name mappings, but treated as a "new" instance (not a "copy");
     * source fields are assumed to already be correct/same types as those on the new value (no type checks).<p>
     * NOTE: Instance members other than "fields" are treated as a "new" value, not copied from the passed value; this is half-way between
     * copy constructor and construction from map. Added 2018-10-22. */
    public static GenericValue createAsFieldSubset(Delegator delegator, ModelEntity modelEntity, GenericValue sourceFieldsValue, Collection<String> fieldNames) {
        GenericValue newValue = new GenericValue();
        newValue.initAsFieldSubset(delegator, modelEntity, sourceFieldsValue, fieldNames);
        return newValue;
    }

    public GenericValue create() throws GenericEntityException {
        return this.getDelegator().create(this);
    }

    public void store() throws GenericEntityException {
        this.getDelegator().store(this);
    }

    public void remove() throws GenericEntityException {
        this.getDelegator().removeValue(this);
    }

    public void refresh() throws GenericEntityException {
        this.getDelegator().refresh(this);
    }

    public void refreshFromCache() throws GenericEntityException {
        this.getDelegator().refreshFromCache(this);
    }

    /** Get the named Related Entity for the GenericValue from the persistent store (no entity cache)
     * <p>SCIPIO: 2019-02-14: UN-deprecated this method, because it's useful and does no harm; this overload simply does not use the entity cache.
     *@param relationName String containing the relation name which is the combination of relation.title and relation.rel-entity-name as specified in the entity XML definition file
     *@return List of GenericValue instances as specified in the relation definition
     */
    public List<GenericValue> getRelated(String relationName) throws GenericEntityException {
        return this.getDelegator().getRelated(relationName, null, null, this, false);
    }

    /** SCIPIO: Get the named Related Entity for the GenericValue from the persistent store and filter it
     * <p>SCIPIO: Added 2019-02-14.
     *@param relationName String containing the relation name which is the combination of relation.title and relation.rel-entity-name as specified in the entity XML definition file
     *@param fields the fields that must equal in order to keep
     *@return List of GenericValue instances as specified in the relation definition
     */
    public List<GenericValue> getRelated(String relationName, Map<String, ? extends Object> fields) throws GenericEntityException {
        return this.getDelegator().getRelated(relationName, fields, null, this, false);
    }

    /** Get the named Related Entity for the GenericValue from the persistent store
     *@param relationName String containing the relation name which is the combination of relation.title and relation.rel-entity-name as specified in the entity XML definition file
     * @param orderBy The fields of the named entity to order the query by; may be null;
     *      optionally add a " ASC" for ascending or " DESC" for descending
     *@return List of GenericValue instances as specified in the relation definition
     *@deprecated use {@link #getRelated(String, Map, List, boolean)}
     */
    @Deprecated
    public List<GenericValue> getRelated(String relationName, List<String> orderBy) throws GenericEntityException {
        Debug.logWarning("Scipio: Deprecated GenericValue method invoked" + getCallerInfoLogStr(), module);
        return this.getDelegator().getRelated(relationName, null, orderBy, this, false);
    }

    /** Get the named Related Entity for the GenericValue from the persistent store (no entity cache)
     * <p>SCIPIO: 2019-02-14: UN-deprecated this method, because it's useful and does no harm; this overload simply does not use the entity cache.
     *@param relationName String containing the relation name which is the combination of relation.title and relation.rel-entity-name as specified in the entity XML definition file
     * @param byAndFields the fields that must equal in order to keep; may be null
     * @param orderBy The fields of the named entity to order the query by; may be null;
     *      optionally add a " ASC" for ascending or " DESC" for descending
     *@return List of GenericValue instances as specified in the relation definition
     */
    public List<GenericValue> getRelated(String relationName, Map<String, ? extends Object> byAndFields, List<String> orderBy) throws GenericEntityException {
        return this.getDelegator().getRelated(relationName, byAndFields, orderBy, this, false);
    }

    /** Get the named Related Entity for the GenericValue from the persistent store
     *@param relationName String containing the relation name which is the combination of relation.title and relation.rel-entity-name as specified in the entity XML definition file
     * @param byAndFields the fields that must equal in order to keep; may be null
     * @param orderBy The fields of the named entity to order the query by; may be null;
     *      optionally add a " ASC" for ascending or " DESC" for descending
     * @param useCache Whether to cache the results
     *@return List of GenericValue instances as specified in the relation definition
     */
    public List<GenericValue> getRelated(String relationName, Map<String, ? extends Object> byAndFields, List<String> orderBy, boolean useCache) throws GenericEntityException {
        return this.getDelegator().getRelated(relationName, byAndFields, orderBy, this, useCache);
    }

    /** SCIPIO: Get the named Related Entity for the GenericValue from the persistent store
     *@param relationName String containing the relation name which is the combination of relation.title and relation.rel-entity-name as specified in the entity XML definition file
     * @param useCache Whether to cache the results
     *@return List of GenericValue instances as specified in the relation definition
     */
    public List<GenericValue> getRelated(String relationName, boolean useCache) throws GenericEntityException {
        return this.getDelegator().getRelated(relationName, null, null, this, useCache);
    }

    /**
     * Get the named Related Entity for the GenericValue from the persistent store across another Relation.
     * Helps to get related Values in a multi-to-multi relationship.
     * @param relationNameOne String containing the relation name which is the
     *      combination of relation.title and relation.rel-entity-name as
     *      specified in the entity XML definition file, for first relation
     * @param relationNameTwo String containing the relation name for second relation
     * @param orderBy The fields of the named entity to order the query by; may be null;
     *      optionally add a " ASC" for ascending or " DESC" for descending
     * @return List of GenericValue instances as specified in the relation definition
     */
    public List<GenericValue> getRelatedMulti(String relationNameOne, String relationNameTwo, List<String> orderBy) throws GenericEntityException {
        return this.getDelegator().getMultiRelation(this, relationNameOne, relationNameTwo, orderBy);
    }

    /**
     * Get the named Related Entity for the GenericValue from the persistent store across another Relation.
     * Helps to get related Values in a multi-to-multi relationship.
     * @param relationNameOne String containing the relation name which is the
     *      combination of relation.title and relation.rel-entity-name as
     *      specified in the entity XML definition file, for first relation
     * @param relationNameTwo String containing the relation name for second relation
     * @return List of GenericValue instances as specified in the relation definition
     */
    public List<GenericValue> getRelatedMulti(String relationNameOne, String relationNameTwo) throws GenericEntityException {
        return this.getDelegator().getMultiRelation(this, relationNameOne, relationNameTwo, null);
    }

    /** Get the named Related Entity for the GenericValue from the persistent store, bypassing cache (no entity cache)
     * <p>SCIPIO: 2019-02-14: UN-deprecated this method, because it's useful and does no harm; this overload simply does not use the entity cache.
     *@param relationName String containing the relation name which is the combination of relation.title and relation.rel-entity-name as specified in the entity XML definition file
     *@return List of GenericValue instances as specified in the relation definition
     */
    public GenericValue getRelatedOne(String relationName) throws GenericEntityException {
        return this.getDelegator().getRelatedOne(relationName, this, false);
    }

    /** Get the named Related Entity for the GenericValue from the persistent store
     *@param relationName String containing the relation name which is the combination of relation.title and relation.rel-entity-name as specified in the entity XML definition file
     *@param useCache Whether to cache the results
     *@return The single related GenericValue instance
     */
    public GenericValue getRelatedOne(String relationName, boolean useCache) throws GenericEntityException {
        return this.getDelegator().getRelatedOne(relationName, this, useCache);
    }

    /** Remove the named Related Entity for the GenericValue from the persistent store
     *@param relationName String containing the relation name which is the combination of relation.title and relation.rel-entity-name as specified in the entity XML definition file
     */
    public void removeRelated(String relationName) throws GenericEntityException {
        this.getDelegator().removeRelated(relationName, this);
    }

    /** Get a dummy primary key for the named Related Entity for the GenericValue
     * @param relationName String containing the relation name which is the
     *      combination of relation.title and relation.rel-entity-name as
     *      specified in the entity XML definition file
     * @return GenericPK containing a possibly incomplete PrimaryKey object representing the related entity or entities
     */
    public GenericPK getRelatedDummyPK(String relationName) throws GenericEntityException {
        return this.getDelegator().getRelatedDummyPK(relationName, null, this);
    }

    /** Get a dummy primary key for the named Related Entity for the GenericValue
     * @param relationName String containing the relation name which is the
     *      combination of relation.title and relation.rel-entity-name as
     *      specified in the entity XML definition file
     * @param byAndFields the fields that must equal in order to keep; may be null
     * @return GenericPK containing a possibly incomplete PrimaryKey object representing the related entity or entities
     */
    public GenericPK getRelatedDummyPK(String relationName, Map<String, ? extends Object> byAndFields) throws GenericEntityException {
        return this.getDelegator().getRelatedDummyPK(relationName, byAndFields, this);
    }

    /**
     * SCIPIO: Extracts a member entity value from the given view-entity value, straight from its fields in memory,
     * for the given view-entity entity alias OR entity name. The view-entity must map all fields of the member,
     * otherwise an exception is thrown. Attempts to return null for non-matched optional view-links (optional joins), best-effort,
     * dependent on the view-entity definition (see below).
     * <p>
     * This is an alias for: <code>extractViewMember(entityAliasOrName, false, true)</code>
     * <p>
     * This method can be used to avoid re-querying the database needlessly for the individual entities after a view-entity lookup,
     * without needing or using the entity cache.
     * <p>
     * NOTE: This method does NOT populate the system-generated fields: lastUpdatedStamp, lastUpdatedTxStamp, createdStamp, createdTxStamp.
     * In most cases this does not cause any issues.
     * <p>
     * <strong>WARN:</strong> See {@link #extractViewMember(String, boolean, boolean)} description for complications due to optional
     * view links (optional joins); the view-entity definition must be written to support it.
     * <p>
     * Added 2018-10-22.
     * @see #extractViewMember(String, boolean, boolean)
     */
    public GenericValue extractViewMember(String entityAliasOrName) throws GenericEntityException {
        return this.getDelegator().extractViewMember(this, entityAliasOrName, false, true);
    }

    /**
     * SCIPIO: Extracts a member entity value from the given view-entity value, straight from its fields in memory,
     * for the given view-entity entity alias OR entity name.
     * <p>
     * This method can be used to avoid re-querying the database needlessly for the individual entities after a view-entity lookup,
     * without needing or using the entity cache.
     * <p>
     * NOTE: This method does NOT populate the system-generated fields: lastUpdatedStamp, lastUpdatedTxStamp, createdStamp, createdTxStamp.
     * In most cases this does not cause any issues.
     * <p>
     * If allowPartial is false and the target entity fields cannot be fully populated from the view-entity fields, throws
     * an exception; if true, fields are partially populated. It is up to the caller to ensure that the original 
     * view-entity definition aliases all the fields needed to populate the member.
     * <p>
     * If nullForAbsentOptViewLink is true, the method will <em>attempt</em> to return null for optional view-links
     * that did not match any records.
     * <strong>WARN:</strong> This optional view-link check is a best-effort attempt and depends on the view-entity definition; 
     * it may only work properly if the view-link aliases the primary key for the optional entity redundantly to a second alias,
     * which will be null if the optional entity was not found. The most popular strategy is this: if your view-entity has rel-optional="true", 
     * you can use "alias-all" with a field prefix on the optional entity and <em>not</em> "exclude" the PK fields.
     * The PK field from the referring non-optional entity will still be populated but the prefixed one will not, allowing this check to work properly.
     * <p>
     * NOTE: Passing the entity name (instead of entity alias) is only supported if it is aliased only once in the view-entity.
     * <p>
     * Added 2018-10-22.
     */
    public GenericValue extractViewMember(String entityAliasOrName, boolean allowPartial, boolean nullForAbsentOptViewLink) throws GenericEntityException {
        return this.getDelegator().extractViewMember(this, entityAliasOrName, allowPartial, nullForAbsentOptViewLink);
    }

    /** Returns a GenericValue copy containing only the selected fields (SCIPIO). NOTE: Currently Observable is not preserved. */
    public GenericValue select(Collection<String> fields) {
        return (GenericValue) super.select(fields);
    }

    /** Returns a GenericValue copy containing only the selected fields (SCIPIO). NOTE: Currently Observable is not preserved. */
    public GenericValue select(String... fields) {
        return (GenericValue) super.select(fields);
    }

    @Override
    public int hashCode() {
        return Objects.hashCode(super.hashCode());
    }

    @Override
    public boolean equals(Object obj) {
        if (obj instanceof GenericValue) {
            return super.equals(obj);
        }
        return false;
    }

    /** Clones this GenericValue, this is a shallow clone and uses the default shallow HashMap clone
     *  @return Object that is a clone of this GenericValue
     */
    @Override
    public Object clone() {
        return GenericValue.create(this);
    }

    @Override
    protected GenericValue newValue() { // SCIPIO
        return new GenericValue();
    }

    protected static class NullGenericValue extends GenericValue implements NULL {
        @Override
        public String getEntityName() {
            return "[null-entity-value]";
        }
        @Override
        public String toString() {
            return "[null-entity-value]";
        }
    }

    public static String getStackTraceAsString() {
        return Arrays.toString(Thread.currentThread().getStackTrace());
    }

    /*
     * SCIPIO: COMPLETELY DEPRECATED METHODS
     * These were removed all the way back in ofbiz 15 and should never be used anymore.
     * They are moved down here in Scipio to avoid maintenance issues.
     * The main reason these are going away is due to replacement with EntityQuery which is clearer.
     */

    /** Get the named Related Entity for the GenericValue from the persistent
     * store, looking first in the global generic cache (for the moment this isn't true, is same as EmbeddedCache variant)
     * <p>SCIPIO: 2019-02-14: UN-deprecated this method, because it's useful and does no harm.
     *@param relationName String containing the relation name which is the combination of relation.title and relation.rel-entity-name as specified in the entity XML definition file
     *@return List of GenericValue instances as specified in the relation definition
     */
    public List<GenericValue> getRelatedCache(String relationName) throws GenericEntityException {
        return this.getDelegator().getRelated(relationName, null, null, this, true);
    }

    /** SCIPIO: Get the named Related Entity for the GenericValue from the persistent
     *  store and filter it, looking first in the global generic cache (for the moment this isn't true, is same as EmbeddedCache variant)
     * <p>SCIPIO: Added 2019-02-14.
     *@param relationName String containing the relation name which is the combination of relation.title and relation.rel-entity-name as specified in the entity XML definition file
     *@param fields the fields that must equal in order to keep
     *@return List of GenericValue instances as specified in the relation definition
     */
    public List<GenericValue> getRelatedCache(String relationName, Map<String, ? extends Object> fields) throws GenericEntityException {
        return this.getDelegator().getRelated(relationName, fields, null, this, true);
    }
    
    /** Get the named Related Entity for the GenericValue from the persistent
     *  store, looking first in the global generic cache (for the moment this isn't true, is same as EmbeddedCache variant) (no entity cache)
     * <p>SCIPIO: 2019-02-14: UN-deprecated this method, because it's useful and does no harm.
     * @param relationName String containing the relation name which is the combination of relation.title and relation.rel-entity-name as specified in the entity XML definition file
     * @param byAndFields the fields that must equal in order to keep; may be null
     * @param orderBy The fields of the named entity to order the query by; may be null;
     *      optionally add a " ASC" for ascending or " DESC" for descending
     *@return List of GenericValue instances as specified in the relation definition
     */
    public List<GenericValue> getRelatedCache(String relationName, Map<String, ? extends Object> byAndFields, List<String> orderBy) throws GenericEntityException {
        return this.getDelegator().getRelated(relationName, byAndFields, orderBy, this, true);
    }

    /** Get the named Related Entity for the GenericValue from the persistent
     * store, looking first in the global generic cache (for the moment this isn't true, is same as EmbeddedCache variant)
     * @param relationName String containing the relation name which is the combination of relation.title and relation.rel-entity-name as specified in the entity XML definition file
     * @param orderBy The fields of the named entity to order the query by; may be null;
     *      optionally add a " ASC" for ascending or " DESC" for descending
     * @return List of GenericValue instances as specified in the relation definition
     * @deprecated use {@link #getRelated(String, Map, List, boolean)}
     */
    @Deprecated
    public List<GenericValue> getRelatedCache(String relationName, List<String> orderBy) throws GenericEntityException {
        Debug.logWarning("Scipio: Deprecated GenericValue method invoked", module);
        return this.getDelegator().getRelated(relationName, null, orderBy, this, true);
    }

    /** Get the named Related Entity for the GenericValue from the persistent
     *  store, looking first in the global generic cache (for the moment this isn't true, is same as EmbeddedCache variant)
     * <p>SCIPIO: 2019-02-14: UN-deprecated this method, because it's useful and does no harm.
     *@param relationName String containing the relation name which is the combination of relation.title and relation.rel-entity-name as specified in the entity XML definition file
     *@return List of GenericValue instances as specified in the relation definition
     */
    public GenericValue getRelatedOneCache(String relationName) throws GenericEntityException {
        return this.getDelegator().getRelatedOne(relationName, this, true);
    }

    /** Get the named Related Entity for the GenericValue from the persistent store and filter it
     * @param relationName String containing the relation name which is the combination of relation.title and relation.rel-entity-name as specified in the entity XML definition file
     * @param fields the fields that must equal in order to keep
     * @return List of GenericValue instances as specified in the relation definition
     * @deprecated use {@link #getRelated(String, Map, List, boolean)}
     */
    @Deprecated
    public List<GenericValue> getRelatedByAnd(String relationName, Map<String, ? extends Object> fields) throws GenericEntityException {
        Debug.logWarning("Scipio: Deprecated GenericValue method invoked", module);
        return this.getDelegator().getRelated(relationName, fields, null, this, false);
    }

    /** Get the named Related Entity for the GenericValue from the persistent
     * store and filter it, looking first in the global generic cache (for the moment this isn't true, is same as EmbeddedCache variant)
     * @param relationName String containing the relation name which is the combination of relation.title and relation.rel-entity-name as specified in the entity XML definition file
     * @param fields the fields that must equal in order to keep
     * @return List of GenericValue instances as specified in the relation definition
     * @deprecated use {@link #getRelated(String, Map, List, boolean)}
     */
    @Deprecated
    public List<GenericValue> getRelatedByAndCache(String relationName, Map<String, ? extends Object> fields) throws GenericEntityException {
        Debug.logWarning("Scipio: Deprecated GenericValue method invoked", module);
        return this.getDelegator().getRelated(relationName, fields, null, this, true);
    }

    /** Get the named Related Entity for the GenericValue from the persistent store and order it
     * @param relationName String containing the relation name which is the combination of relation.title and relation.rel-entity-name as specified in the entity XML definition file
     * @param orderBy the order that they should be returned
     * @return List of GenericValue instances as specified in the relation definition
     * @deprecated use {@link #getRelated(String, Map, List, boolean)}
     */
    @Deprecated
    public List<GenericValue> getRelatedOrderBy(String relationName, List<String> orderBy) throws GenericEntityException {
        Debug.logWarning("Scipio: Deprecated GenericValue method invoked" + getCallerInfoLogStr(), module);
        return this.getDelegator().getRelated(relationName, null, orderBy, this, false);
    }

    /** Get the named Related Entity for the GenericValue from the persistent
     * store and order it, looking first in the global generic cache (for the moment this isn't true, is same as EmbeddedCache variant)
     * @param relationName String containing the relation name which is the combination of relation.title and relation.rel-entity-name as specified in the entity XML definition file
     * @param orderBy the order that they should be returned
     * @return List of GenericValue instances as specified in the relation definition
     * @deprecated use {@link #getRelated(String, Map, List, boolean)}
     */
    @Deprecated
    public List<GenericValue> getRelatedOrderByCache(String relationName, List<String> orderBy) throws GenericEntityException {
        Debug.logWarning("Scipio: Deprecated GenericValue method invoked" + getCallerInfoLogStr(), module);
        return this.getDelegator().getRelated(relationName, null, orderBy, this, true);
    }

    private static String getCallerInfoLogStr() { // SCIPIO
        StackTraceElement ste = Thread.currentThread().getStackTrace()[2];
        return " (" + Debug.formatCallerShortInfo(ste) + ", called from: " + Debug.getCallerShortInfo(UtilMisc.toList(GenericValue.class.getName())) + ")";
    }
}
