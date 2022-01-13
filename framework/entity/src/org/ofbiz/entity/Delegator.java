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
package org.ofbiz.entity;

import java.net.URL;
import java.util.Collection;
import java.util.List;
import java.util.Map;
import java.util.Set;

import javax.servlet.ServletContext;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpSession;
import javax.xml.parsers.ParserConfigurationException;

import org.ofbiz.entity.cache.Cache;
import org.ofbiz.entity.condition.EntityCondition;
import org.ofbiz.entity.datasource.GenericHelper;
import org.ofbiz.entity.datasource.GenericHelperInfo;
import org.ofbiz.entity.eca.EntityEcaHandler;
import org.ofbiz.entity.model.DynamicViewEntity;
import org.ofbiz.entity.model.ModelEntity;
import org.ofbiz.entity.model.ModelField;
import org.ofbiz.entity.model.ModelFieldType;
import org.ofbiz.entity.model.ModelFieldTypeReader;
import org.ofbiz.entity.model.ModelGroupReader;
import org.ofbiz.entity.model.ModelReader;
import org.ofbiz.entity.model.ModelViewEntity;
import org.ofbiz.entity.util.DistributedCacheClear;
import org.ofbiz.entity.util.EntityCrypto;
import org.ofbiz.entity.util.EntityFindOptions;
import org.ofbiz.entity.util.EntityListIterator;
import org.ofbiz.entity.util.EntityQuery;
import org.ofbiz.entity.util.EntityQuerySafe;
import org.ofbiz.entity.util.EntityStoreOptions;
import org.ofbiz.entity.util.SequenceUtil;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.xml.sax.SAXException;

public interface Delegator {

    enum OperationType {INSERT, UPDATE, DELETE}

    /**
     * Returns the named delegator from central factory.
     * <p>SCIPIO: 2.1.0: Added convenience facade method.</p>
     */
    static Delegator delegator(String delegatorName) {
        return DelegatorFactory.getDelegator(delegatorName);
    }

    /**
     * Returns the default delegator from central factory.
     * <p>SCIPIO: 2.1.0: Added convenience facade method.</p>
     */
    static Delegator defaultDelegator() {
        return DelegatorFactory.getDefaultDelegator();
    }

    /**
     * Returns the delegator from map context, or the default delegator.
     * <p>NOTE: This explicitly does not check the "request" key for HttpServletRequest because in every case
     * "request" is set, "delegator" should always be set by the system, otherwise it is considered a (system) error.
     * This is an abstracted accessor method.</p>
     * <p>SCIPIO: 2.1.0: Added convenience facade method.</p>
     */
    static Delegator delegator(Map<String, ?> context) {
        Delegator delegator;
        if (context != null) {
            delegator = (Delegator) context.get("delegator");
            if (delegator != null) {
                return delegator;
            }
        }
        return getDefaultDelegator();
    }

    /**
     * Returns the most specific delegator from request attributes, session attributes, servlet context attributes
     * or the default delegator.
     * <p>SCIPIO: 2.1.0: Added convenience facade method.</p>
     */
    static Delegator delegator(HttpServletRequest request) {
        Delegator delegator;
        if (request != null) {
            delegator = (Delegator) request.getAttribute("delegator");
            if (delegator != null) {
                return delegator;
            }
            HttpSession session = request.getSession(false);
            if (session != null) {
                delegator = (Delegator) session.getAttribute("delegator");
                if (delegator != null) {
                    return delegator;
                }
            }
            ServletContext servletContext = request.getServletContext();
            delegator = (Delegator) servletContext.getAttribute("delegator");
            if (delegator != null) {
                return delegator;
            }
        }
        return getDefaultDelegator();
    }

    /**
     * Returns the named delegator from central factory.
     * @deprecated Use {@link #delegator(String)}
     * <p>SCIPIO: 2.1.0: Added convenience facade method.</p>
     */
    @Deprecated
    static Delegator getDelegator(String delegatorName) {
        return delegator(delegatorName);
    }

    /**
     * Returns the default delegator from central factory.
     * @deprecated Use {@link #defaultDelegator()}
     * <p>SCIPIO: 2.1.0: Added convenience facade method.</p>
     */
    @Deprecated
    static Delegator getDefaultDelegator() {
        return defaultDelegator();
    }


    void clearAllCacheLinesByDummyPK(Collection<GenericPK> dummyPKs);

    void clearAllCacheLinesByValue(Collection<GenericValue> values);

    /**
     * This method is a shortcut to completely clear all entity engine caches.
     * For performance reasons this should not be called very often.
     */
    void clearAllCaches();

    void clearAllCaches(boolean distribute);

    /**
     * Remove a CACHED Generic Entity from the cache by its primary key, does
     * NOT check to see if the passed GenericPK is a complete primary key. Also
     * tries to clear the corresponding all cache entry.
     *
     * @param primaryKey
     *            The primary key to clear by.
     */
    void clearCacheLine(GenericPK primaryKey);

    void clearCacheLine(GenericPK primaryKey, boolean distribute);

    /**
     * Remove a CACHED GenericValue from as many caches as it can. Automatically
     * tries to remove entries from the all cache, the by primary key cache, and
     * the by and cache. This is the ONLY method that tries to clear
     * automatically from the by and cache.
     *
     * @param value
     *            The GenericValue to clear by.
     */
    void clearCacheLine(GenericValue value);

    void clearCacheLine(GenericValue value, boolean distribute);

    /**
     * Remove all CACHED Generic Entity (List) from the cache
     *
     * @param entityName
     *            The Name of the Entity as defined in the entity XML file
     */
    void clearCacheLine(String entityName);

    /**
     * Remove a CACHED Generic Entity (List) from the cache, either a PK, ByAnd,
     * or All
     *
     * @param entityName
     *            The Name of the Entity as defined in the entity XML file
     * @param fields
     *            The fields of the named entity to query by with their
     *            corresponding values
     */
    void clearCacheLine(String entityName, Map<String, ? extends Object> fields);

    /**
     * Remove a CACHED Generic Entity (List) from the cache, either a PK, ByAnd,
     * or All
     *
     * @param entityName
     *            The Name of the Entity as defined in the entity XML file
     * @param fields
     *            The fields of the named entity to query by with their
     *            corresponding values
     */
    void clearCacheLine(String entityName, Object... fields);

    void clearCacheLineByCondition(String entityName, EntityCondition condition);

    void clearCacheLineByCondition(String entityName, EntityCondition condition, boolean distribute);

    /**
     * Remove a CACHED Generic Entity from the cache by its primary key. Checks
     * to see if the passed GenericPK is a complete primary key, if it is then
     * the cache line will be removed from the primaryKeyCache; if it is NOT a
     * complete primary key it will remove the cache line from the andCache. If
     * the fields map is empty, then the allCache for the entity will be
     * cleared.
     *
     * @param dummyPK
     *            The dummy primary key to clear by.
     */
    void clearCacheLineFlexible(GenericEntity dummyPK);

    void clearCacheLineFlexible(GenericEntity dummyPK, boolean distribute);

    Delegator cloneDelegator();

    Delegator cloneDelegator(String delegatorName);

    /**
     * Creates a Entity in the form of a GenericValue and write it to the
     * datasource
     *
     * @param primaryKey
     *            The GenericPK to create a value in the datasource from
     * @return GenericValue instance containing the new instance
     */
    GenericValue create(GenericPK primaryKey) throws GenericEntityException;

    /**
     * Creates a Entity in the form of a GenericValue and write it to the
     * datasource
     *
     * @param value
     *            The GenericValue to create a value in the datasource from
     * @return GenericValue instance containing the new instance
     */
    GenericValue create(GenericValue value) throws GenericEntityException;

    /**
     * Creates a Entity in the form of a GenericValue and write it to the
     * database
     *
     * @return GenericValue instance containing the new instance
     */
    GenericValue create(String entityName, Map<String, ? extends Object> fields) throws GenericEntityException;

    /**
     * Creates a Entity in the form of a GenericValue and write it to the
     * database
     *
     * @return GenericValue instance containing the new instance
     */
    GenericValue create(String entityName, Object... fields) throws GenericEntityException;

    /**
     * Creates or stores an Entity
     *
     * @param value
     *            The GenericValue instance containing the new or existing
     *            instance
     * @return GenericValue instance containing the new or updated instance
     */
    GenericValue createOrStore(GenericValue value) throws GenericEntityException;

    /**
     * Sets the sequenced ID (for entity with one primary key field ONLY), and
     * then does a create in the database as normal. The reason to do it this
     * way is that it will retry and fix the sequence if somehow the sequencer
     * is in a bad state and returning a value that already exists.
     *
     * @param value
     *            The GenericValue to create a value in the datasource from
     * @return GenericValue instance containing the new instance
     */
    GenericValue createSetNextSeqId(GenericValue value) throws GenericEntityException;

    /**
     * Creates a Entity in the form of a GenericValue and write it to the
     * database
     *
     * @return GenericValue instance containing the new instance
     */
    GenericValue createSingle(String entityName, Object singlePkValue) throws GenericEntityException;

    @Deprecated
    Object decryptFieldValue(String entityName, String encValue) throws EntityCryptoException;

    Object decryptFieldValue(String entityName, ModelField.EncryptMethod encryptMethod, String encValue) throws EntityCryptoException;

    Object encryptFieldValue(String entityName, ModelField.EncryptMethod encryptMethod, Object fieldValue) throws EntityCryptoException;

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
    GenericValue extractViewMember(GenericValue viewValue, String entityAliasOrName, boolean allowPartial, boolean nullForAbsentOptViewLink) throws GenericEntityException;

    /**
     * Finds GenericValues by the conditions specified in the EntityCondition
     * object, the the EntityCondition javadoc for more details.
     *
     * @param entityName
     *            The name of the Entity as defined in the entity XML file
     * @param whereEntityCondition
     *            The EntityCondition object that specifies how to constrain
     *            this query before any groupings are done (if this is a view
     *            entity with group-by aliases)
     * @param havingEntityCondition
     *            The EntityCondition object that specifies how to constrain
     *            this query after any groupings are done (if this is a view
     *            entity with group-by aliases)
     * @param fieldsToSelect
     *            The fields of the named entity to get from the database; if
     *            empty or null all fields will be retreived
     * @param orderBy
     *            The fields of the named entity to order the query by;
     *            optionally add a " ASC" for ascending or " DESC" for
     *            descending
     * @param findOptions
     *            An instance of EntityFindOptions that specifies advanced query
     *            options. See the EntityFindOptions JavaDoc for more details.
     * @return EntityListIterator representing the result of the query: NOTE
     *         THAT THIS MUST BE CLOSED (preferably in a finally block) WHEN YOU
     *         ARE DONE WITH IT, AND DON'T LEAVE IT OPEN TOO LONG BEACUSE IT
     *         WILL MAINTAIN A DATABASE CONNECTION.
     */
    EntityListIterator find(String entityName, EntityCondition whereEntityCondition, EntityCondition havingEntityCondition, Set<String> fieldsToSelect, List<String> orderBy, EntityFindOptions findOptions) throws GenericEntityException;

    /**
     * Finds all Generic entities
     *
     * @param entityName
     *            The Name of the Entity as defined in the entity XML file
     * @param useCache
     *            Whether to cache the results
     * @return List containing all Generic entities
     */
    List<GenericValue> findAll(String entityName, boolean useCache) throws GenericEntityException;

    /**
     * Finds Generic Entity records by all of the specified fields (ie: combined
     * using AND), looking first in the cache; uses orderBy for lookup, but only
     * keys results on the entityName and fields
     *
     * @param entityName
     *            The Name of the Entity as defined in the entity XML file
     * @param fields
     *            The fields of the named entity to query by with their
     *            corresponding values
     * @param orderBy
     *            The fields of the named entity to order the query by;
     *            optionally add a " ASC" for ascending or " DESC" for
     *            descending
     * @param useCache
     *            Whether to cache the results
     * @return List of GenericValue instances that match the query
     */
    List<GenericValue> findByAnd(String entityName, Map<String, ? extends Object> fields, List<String> orderBy, boolean useCache) throws GenericEntityException;

    /**
     * Find a Generic Entity by its Primary Key and only returns the values
     * requested by the passed keys (names).
     *
     * @param primaryKey
     *            The primary key to find by.
     * @param keys
     *            The keys, or names, of the values to retrieve; only these
     *            values will be retrieved
     * @return The GenericValue corresponding to the primaryKey
     */
    GenericValue findByPrimaryKeyPartial(GenericPK primaryKey, Set<String> keys) throws GenericEntityException;

    /**
     * Gets the hit count of GenericValues for the given EntityCondition objects.
     *
     * @param entityName
     * @param whereEntityCondition
     * @param havingEntityCondition
     * @param findOptions
     * @return long value with hit count
     * @throws GenericEntityException
     */
    long findCountByCondition(String entityName, EntityCondition whereEntityCondition, EntityCondition havingEntityCondition, EntityFindOptions findOptions) throws GenericEntityException;

    /**
     * Finds GenericValues by the conditions specified in the EntityCondition
     * object, the the EntityCondition javadoc for more details.
     *
     * @param entityName
     *            The name of the Entity as defined in the entity XML file
     * @param entityCondition
     *            The EntityCondition object that specifies how to constrain
     *            this query before any groupings are done (if this is a view
     *            entity with group-by aliases)
     * @param fieldsToSelect
     *            The fields of the named entity to get from the database; if
     *            empty or null all fields will be retrieved
     * @param orderBy
     *            The fields of the named entity to order the query by;
     *            optionally add a " ASC" for ascending or " DESC" for
     *            descending
     * @param findOptions
     *            An instance of EntityFindOptions that specifies advanced query
     *            options. See the EntityFindOptions JavaDoc for more details.
     * @return List of GenericValue objects representing the result
     */
    List<GenericValue> findList(String entityName, EntityCondition entityCondition, Set<String> fieldsToSelect, List<String> orderBy, EntityFindOptions findOptions, boolean useCache) throws GenericEntityException;

    /**
     * Finds GenericValues by the conditions specified in the EntityCondition
     * object, the the EntityCondition javadoc for more details.
     *
     * @param dynamicViewEntity
     *            The DynamicViewEntity to use for the entity model for this
     *            query; generally created on the fly for limited use
     * @param whereEntityCondition
     *            The EntityCondition object that specifies how to constrain
     *            this query before any groupings are done (if this is a view
     *            entity with group-by aliases)
     * @param havingEntityCondition
     *            The EntityCondition object that specifies how to constrain
     *            this query after any groupings are done (if this is a view
     *            entity with group-by aliases)
     * @param fieldsToSelect
     *            The fields of the named entity to get from the database; if
     *            empty or null all fields will be retreived
     * @param orderBy
     *            The fields of the named entity to order the query by;
     *            optionally add a " ASC" for ascending or " DESC" for
     *            descending
     * @param findOptions
     *            An instance of EntityFindOptions that specifies advanced query
     *            options. See the EntityFindOptions JavaDoc for more details.
     * @return EntityListIterator representing the result of the query: NOTE
     *         THAT THIS MUST BE CLOSED WHEN YOU ARE DONE WITH IT, AND DON'T
     *         LEAVE IT OPEN TOO LONG BEACUSE IT WILL MAINTAIN A DATABASE
     *         CONNECTION.
     */
    EntityListIterator findListIteratorByCondition(DynamicViewEntity dynamicViewEntity, EntityCondition whereEntityCondition, EntityCondition havingEntityCondition, Collection<String> fieldsToSelect, List<String> orderBy, EntityFindOptions findOptions) throws GenericEntityException;

    /**
     * Find a Generic Entity by its primary key.
     *
     * @param entityName The Name of the Entity as defined in the entity XML file
     * @param useCache Retrieve the Generic Entity from the cache when <code>true</code>
     * @param fields The fields of the named entity to query by with their corresponding values
     * @return The Generic Entity corresponding to the primary key
     * @throws GenericEntityException
     */
    GenericValue findOne(String entityName, boolean useCache, Object... fields) throws GenericEntityException;

    /**
     * Find a Generic Entity by its Primary Key
     *
     * @param entityName
     *            The Name of the Entity as defined in the entity XML file
     * @param fields
     *            The fields of the named entity to query by with their
     *            corresponding values
     * @return The GenericValue corresponding to the primaryKey
     */
    GenericValue findOne(String entityName, Map<String, ? extends Object> fields, boolean useCache) throws GenericEntityException;

    Cache getCache();

    String getCurrentSessionIdentifier();

    String getCurrentUserIdentifier();

    String getDelegatorName();

    String getDelegatorBaseName();

    String getDelegatorTenantId();

    <T> EntityEcaHandler<T> getEntityEcaHandler();

    /**
     * Gets a field type instance by name from the helper that corresponds to
     * the specified entity
     *
     * @param entity
     *            The entity
     * @param type
     *            The name of the type
     * @return ModelFieldType instance for the named type from the helper that
     *         corresponds to the specified entity
     */
    ModelFieldType getEntityFieldType(ModelEntity entity, String type) throws GenericEntityException;

    /**
     * Gets field type names from the helper that corresponds to the specified
     * entity
     *
     * @param entity
     *            The entity
     * @return Collection of field type names from the helper that corresponds
     *         to the specified entity
     */
    Collection<String> getEntityFieldTypeNames(ModelEntity entity) throws GenericEntityException;

    /**
     * Gets the helper name that corresponds to this delegator and the specified
     * entityName
     *
     * @param entityName
     *            The name of the entity to get the helper for
     * @return String with the helper name that corresponds to this delegator
     *         and the specified entityName
     */
    String getEntityGroupName(String entityName);

    /**
     * Gets the an instance of helper that corresponds to this delegator and the
     * specified entity
     *
     * @param entity
     *            The entity to get the helper for
     * @return GenericHelper that corresponds to this delegator and the
     *         specified entity
     */
    GenericHelper getEntityHelper(ModelEntity entity) throws GenericEntityException;

    /**
     * Gets the an instance of helper that corresponds to this delegator and the
     * specified entityName
     *
     * @param entityName
     *            The name of the entity to get the helper for
     * @return GenericHelper that corresponds to this delegator and the
     *         specified entityName
     */
    GenericHelper getEntityHelper(String entityName) throws GenericEntityException;

    /**
     * Gets the helper name that corresponds to this delegator and the specified
     * entity
     *
     * @param entity
     *            The entity to get the helper for
     * @return String with the helper name that corresponds to this delegator
     *         and the specified entity
     */
    String getEntityHelperName(ModelEntity entity);

    /**
     * Gets the helper name that corresponds to this delegator and the specified
     * entityName
     *
     * @param entityName
     *            The name of the entity to get the helper name for
     * @return String with the helper name that corresponds to this delegator
     *         and the specified entityName
     */
    String getEntityHelperName(String entityName);

    GenericValue getFromPrimaryKeyCache(GenericPK primaryKey);

    /**
     * Gets the helper name that corresponds to this delegator and the specified
     * entityName
     *
     * @param groupName
     *            The name of the group to get the helper name for
     * @return String with the helper name that corresponds to this delegator
     *         and the specified entityName
     */
    String getGroupHelperName(String groupName);

    GenericHelperInfo getGroupHelperInfo(String entityGroupName);

    /**
     * Gets the instance of ModelEntity that corresponds to this delegator and
     * the specified entityName
     *
     * @param entityName
     *            The name of the entity to get
     * @return ModelEntity that corresponds to this delegator and the specified
     *         entityName
     */
    ModelEntity getModelEntity(String entityName);

    /**
     * Gets the instance of ModelViewEntity that corresponds to this delegator and
     * the specified entityName
     * <p>SCIPIO: 2.1.0: Added as helper.</p>
     *
     * @param viewEntityName
     *            The name of the view-entity to get
     * @return ModelViewEntity that corresponds to this delegator and the specified
     *         entityName
     */
    default ModelViewEntity getModelViewEntity(String viewEntityName) {
        return (ModelViewEntity) getModelEntity(viewEntityName);
    }

    /**
     * Gets the instance of ModelViewEntity that corresponds to this delegator and
     * the specified entityName and returns a new editable DynamicViewEntity.
     * <p>SCIPIO: 2.1.0: Added as helper.</p>
     *
     * @param viewEntityName
     *            The name of the view-entity to get
     * @return DynamicViewEntity that corresponds to this delegator and the specified
     *         entityName
     */
    default DynamicViewEntity makeDynamicViewEntity(String viewEntityName) {
        return new DynamicViewEntity(getModelViewEntity(viewEntityName));
    }

    /**
     * Makes a new empty DynamicViewEntity.
     * <p>SCIPIO: 2.1.0: Added as helper.</p>
     *
     * @return a new emtpy DynamicViewEntity
     */
    default DynamicViewEntity makeDynamicViewEntity() {
        return new DynamicViewEntity();
    }

    /**
     * Gets a Map of entity name and entity model pairs that are in the named
     * group
     *
     * @param groupName The name of the group
     * @return Map of entityName String keys and ModelEntity instance values
     */
    Map<String, ModelEntity> getModelEntityMapByGroup(String groupName) throws GenericEntityException;

    ModelFieldTypeReader getModelFieldTypeReader(ModelEntity entity);

    /**
     * Gets the instance of ModelGroupReader that corresponds to this delegator
     *
     * @return ModelGroupReader that corresponds to this delegator
     */
    ModelGroupReader getModelGroupReader();

    /**
     * Gets the instance of ModelReader that corresponds to this delegator
     *
     * @return ModelReader that corresponds to this delegator
     */
    ModelReader getModelReader();

    /**
     * Get the named Related Entity for the GenericValue from the persistent
     * store across another Relation. Helps to get related Values in a
     * multi-to-multi relationship.
     *
     * @param relationNameOne
     *            String containing the relation name which is the combination
     *            of relation.title and relation.rel-entity-name as specified in
     *            the entity XML definition file, for first relation
     * @param relationNameTwo
     *            String containing the relation name for second relation
     * @param value
     *            GenericValue instance containing the entity
     * @param orderBy
     *            The fields of the named entity to order the query by; may be
     *            null; optionally add a " ASC" for ascending or " DESC" for
     *            descending
     * @return List of GenericValue instances as specified in the relation
     *         definition
     */
    List<GenericValue> getMultiRelation(GenericValue value, String relationNameOne, String relationNameTwo, List<String> orderBy) throws GenericEntityException;

    /**
     * Get the next guaranteed unique seq id from the sequence with the given
     * sequence name; if the named sequence doesn't exist, it will be created
     *
     * @param seqName
     *            The name of the sequence to get the next seq id from
     * @return String with the next sequenced id for the given sequence name
     */
    String getNextSeqId(String seqName);

    /**
     * Get the next guaranteed unique seq id from the sequence with the given
     * sequence name; if the named sequence doesn't exist, it will be created
     *
     * @param seqName
     *            The name of the sequence to get the next seq id from
     * @param staggerMax
     *            The maximum amount to stagger the sequenced ID, if 1 the
     *            sequence will be incremented by 1, otherwise the current
     *            sequence ID will be incremented by a value between 1 and
     *            staggerMax
     * @return Long with the next seq id for the given sequence name
     */
    String getNextSeqId(String seqName, long staggerMax);

    /**
     * Get the next guaranteed unique seq id from the sequence with the given
     * sequence name; if the named sequence doesn't exist, it will be created
     *
     * @param seqName
     *            The name of the sequence to get the next seq id from
     * @return Long with the next sequenced id for the given sequence name
     */
    Long getNextSeqIdLong(String seqName);

    /**
     * Get the next guaranteed unique seq id from the sequence with the given
     * sequence name; if the named sequence doesn't exist, it will be created
     *
     * @param seqName
     *            The name of the sequence to get the next seq id from
     * @param staggerMax
     *            The maximum amount to stagger the sequenced ID, if 1 the
     *            sequence will be incremented by 1, otherwise the current
     *            sequence ID will be incremented by a value between 1 and
     *            staggerMax
     * @return Long with the next seq id for the given sequence name
     */
    Long getNextSeqIdLong(String seqName, long staggerMax);

    /**
     * Gets the name of the server configuration that corresponds to this
     * delegator
     *
     * @return server configuration name
     */
    String getOriginalDelegatorName();

    /**
     * Get the named Related Entity for the GenericValue from the persistent
     * store
     *
     * @param relationName
     *            String containing the relation name which is the combination
     *            of relation.title and relation.rel-entity-name as specified in
     *            the entity XML definition file
     * @param byAndFields
     *            the fields that must equal in order to keep; may be null
     * @param orderBy
     *            The fields of the named entity to order the query by; may be
     *            null; optionally add a " ASC" for ascending or " DESC" for
     *            descending
     * @param value
     *            GenericValue instance containing the entity
     * @param useCache
     *            Whether to cache the results
     * @return List of GenericValue instances as specified in the relation
     *         definition
     */
    List<GenericValue> getRelated(String relationName, Map<String, ? extends Object> byAndFields, List<String> orderBy, GenericValue value, boolean useCache) throws GenericEntityException;

    /**
     * Get a dummy primary key for the named Related Entity for the GenericValue.
     *
     * @param relationName
     *            String containing the relation name which is the combination
     *            of relation.title and relation.rel-entity-name as specified in
     *            the entity XML definition file
     * @param byAndFields
     *            the fields that must equal in order to keep; may be null
     * @param value
     *            GenericValue instance containing the entity
     * @return GenericPK containing a possibly incomplete PrimaryKey object
     *         representing the related entity or entities
     */
    GenericPK getRelatedDummyPK(String relationName, Map<String, ? extends Object> byAndFields, GenericValue value) throws GenericEntityException;

    /**
     * Get related entity where relation is of type one, uses findByPrimaryKey
     *
     * @param relationName
     *            String containing the relation name which is the combination
     *            of relation.title and relation.rel-entity-name as specified in
     *            the entity XML definition file
     * @param value
     *            GenericValue instance containing the entity
     * @param useCache
     *            Whether to cache the results
     * @return GenericValue that is the related entity
     * @throws IllegalArgumentException
     *             if the list found has more than one item
     */
    GenericValue getRelatedOne(String relationName, GenericValue value, boolean useCache) throws GenericEntityException;

    void initEntityEcaHandler();

    void initDistributedCacheClear();

    GenericPK makePK(Element element);

    /** Creates a Primary Key in the form of a GenericPK without persisting it */
    GenericPK makePK(String entityName);

    /** Creates a Primary Key in the form of a GenericPK without persisting it */
    GenericPK makePK(String entityName, Map<String, ? extends Object> fields);

    /** Creates a Primary Key in the form of a GenericPK without persisting it */
    GenericPK makePK(String entityName, Object... fields);

    /** Creates a Primary Key in the form of a GenericPK without persisting it */
    GenericPK makePKSingle(String entityName, Object singlePkValue);

    Delegator makeTestDelegator(String delegatorName);

    /**
     * Creates a Entity in the form of a GenericValue without persisting it;
     * only valid fields will be pulled from the fields Map
     */
    GenericValue makeValidValue(String entityName, Map<String, ? extends Object> fields);

    /**
     * Creates a Entity in the form of a GenericValue without persisting it;
     * only valid fields will be pulled from the fields Map
     */
    GenericValue makeValidValue(String entityName, Object... fields);

    GenericValue makeValue(Element element);

    /** Creates a Entity in the form of a GenericValue without persisting it */
    GenericValue makeValue(String entityName);

    /** Creates a Entity in the form of a GenericValue without persisting it */
    GenericValue makeValue(String entityName, Map<String, ? extends Object> fields);

    /** Creates a Entity in the form of a GenericValue without persisting it */
    GenericValue makeValue(String entityName, Object... fields);

    List<GenericValue> makeValues(Document document);

    /** Creates a Entity in the form of a GenericValue without persisting it */
    GenericValue makeValueSingle(String entityName, Object singlePkValue);

    void putAllInPrimaryKeyCache(List<GenericValue> values);

    void putInPrimaryKeyCache(GenericPK primaryKey, GenericValue value);

    // ======= XML Related Methods ========
    List<GenericValue> readXmlDocument(URL url) throws SAXException, ParserConfigurationException, java.io.IOException;

    /**
     * Refresh the Entity for the GenericValue from the persistent store
     *
     * @param value
     *            GenericValue instance containing the entity to refresh
     */
    void refresh(GenericValue value) throws GenericEntityException;

    /**
     * Refresh the Entity for the GenericValue from the cache
     *
     * @param value
     *            GenericValue instance containing the entity to refresh
     */
    void refreshFromCache(GenericValue value) throws GenericEntityException;

    /** Refreshes the ID sequencer clearing all cached bank values. */
    void refreshSequencer();

    /**
     * <p>Remove the Entities from the List from the persistent store.</p>
     * <p>The List contains GenericEntity objects, can be either GenericPK or
     * GenericValue. </p>
     * <p>If a certain entity contains a complete primary key, the entity in
     * the datasource corresponding to that primary key will be removed, this
     * is like a removeByPrimary Key.</p>
     * <p>On the other hand, if a certain entity is an incomplete or non
     * primary key, if will behave like the removeByAnd method. </p>
     * <p>These updates all happen in one transaction, so they will either
     * all succeed or all fail, if the data source supports transactions.</p>
     *
     * @param dummyPKs
     *            Collection of GenericEntity instances containing the entities
     *            or by and fields to remove
     * @return int representing number of rows effected by this operation
     */
    int removeAll(List<? extends GenericEntity> dummyPKs) throws GenericEntityException;

    int removeAll(String entityName) throws GenericEntityException;

    /**
     * Removes/deletes Generic Entity records found by all of the specified
     * fields (ie: combined using AND)
     *
     * @param entityName
     *            The Name of the Entity as defined in the entity XML file
     * @param fields
     *            The fields of the named entity to query by with their
     *            corresponding values
     * @return int representing number of rows effected by this operation
     */
    int removeByAnd(String entityName, Map<String, ? extends Object> fields) throws GenericEntityException;

    /**
     * Removes/deletes Generic Entity records found by all of the specified
     * fields (ie: combined using AND)
     *
     * @param entityName
     *            The Name of the Entity as defined in the entity XML file
     * @param fields
     *            The fields of the named entity to query by with their
     *            corresponding values
     * @return int representing number of rows effected by this operation
     */
    int removeByAnd(String entityName, Object... fields) throws GenericEntityException;

    /**
     * Removes/deletes Generic Entity records found by the condition
     *
     * @param entityName
     *            The Name of the Entity as defined in the entity XML file
     * @param condition
     *            The condition used to restrict the removing
     * @return int representing number of rows effected by this operation
     */
    int removeByCondition(String entityName, EntityCondition condition) throws GenericEntityException;

    /**
     * Remove a Generic Entity corresponding to the primaryKey
     *
     * @param primaryKey
     *            The primary key of the entity to remove.
     * @return int representing number of rows effected by this operation
     */
    int removeByPrimaryKey(GenericPK primaryKey) throws GenericEntityException;

    /**
     * Remove the named Related Entity for the GenericValue from the persistent
     * store
     *
     * @param relationName
     *            String containing the relation name which is the combination
     *            of relation.title and relation.rel-entity-name as specified in
     *            the entity XML definition file
     * @param value
     *            GenericValue instance containing the entity
     * @return int representing number of rows effected by this operation
     */
    int removeRelated(String relationName, GenericValue value) throws GenericEntityException;

    /**
     * Remove a Generic Value from the database
     *
     * @param value
     *            The GenericValue object of the entity to remove.
     * @return int representing number of rows effected by this operation
     */
    int removeValue(GenericValue value) throws GenericEntityException;

    void rollback();

    void setDistributedCacheClear(DistributedCacheClear distributedCacheClear);

    void setEntityCrypto(EntityCrypto crypto);

    <T> void setEntityEcaHandler(EntityEcaHandler<T> entityEcaHandler);

    /**
     * Look at existing values for a sub-entity with a sequenced secondary ID,
     * and get the highest plus 1
     */
    void setNextSubSeqId(GenericValue value, String seqFieldName, int numericPadding, int incrementBy);

    /**
     * Allows you to pass a SequenceUtil class (possibly one that overrides the
     * getNextSeqId method); if null is passed will effectively refresh the
     * sequencer.
     */
    void setSequencer(SequenceUtil sequencer);

    /**
     * Store the Entity from the GenericValue to the persistent store
     *
     * @param value
     *            GenericValue instance containing the entity
     * @return int representing number of rows effected by this operation
     */
    int store(GenericValue value) throws GenericEntityException;

    /**
     * <p>Store the Entities from the List GenericValue instances to the persistent
     * store.</p>
     * <p>This is different than the normal store method in that the
     * store method only does an update, while the storeAll method checks to see
     * if each entity exists, then either does an insert or an update as
     * appropriate.</p>
     * <p>These updates all happen in one transaction, so they
     * will either all succeed or all fail, if the data source supports
     * transactions. This is just like to othersToStore feature of the
     * GenericEntity on a create or store.</p>
     *
     * @param values
     *            List of GenericValue instances containing the entities to
     *            store (SCIPIO: now Collection)
     * @return int representing number of rows effected by this operation
     */
    int storeAll(Collection<GenericValue> values) throws GenericEntityException;

    /**
     * <p>Store the Entities from the List GenericValue instances to the persistent
     * store.</p>
     * <p>This is different than the normal store method in that the
     * store method only does an update, while the storeAll method checks to see
     * if each entity exists, then either does an insert or an update as
     * appropriate.</p>
     * <p>These updates all happen in one transaction, so they
     * will either all succeed or all fail, if the data source supports
     * transactions. This is just like to othersToStore feature of the
     * GenericEntity on a create or store.</p>
     *
     * @param storeOptions
     *            An instance of EntityStoreOptions that specifies advanced store
     *            options or null for default values.
     *            See the EntityStoreOptions JavaDoc for more details.
     * @param values
     *            List of GenericValue instances containing the entities to
     *            store (SCIPIO: now Collection)
     * @return int representing number of rows effected by this operation
     */
    int storeAll(Collection<GenericValue> values, EntityStoreOptions storeOptions) throws GenericEntityException;

    /**
     * Store a group of values.
     *
     * @param entityName
     *            The name of the Entity as defined in the entity XML file
     * @param fieldsToSet
     *            The fields of the named entity to set in the database
     * @param condition
     *            The condition that restricts the list of stored values
     * @return int representing number of rows effected by this operation
     * @throws GenericEntityException
     */
    int storeByCondition(String entityName, Map<String, ? extends Object> fieldsToSet, EntityCondition condition) throws GenericEntityException;

    /**
     * Get use of Distributed Cache Clear mechanism status
     * @return boolean true if this delegator uses a Distributed Cache Clear mechanism
     */
    boolean useDistributedCacheClear();

    DistributedCacheClear getDistributedCacheClear(); // SCIPIO: moved here from GenericDelegator for reuse

    /**
     * SCIPIO: Helper method to determine if the entity exists by name in the system.
     * Never throws exceptions.
     */
    default boolean isEntity(String entityName) {
        return (getModelReader().getModelEntityNoCheck(entityName) != null);
    }

    /*
     * SCIPIO: EntityQuery alias methods
     * The abstracted methods query(), from(...) and select(...) delegate to either xxxUnsafe when called from java, groovy, xml, etc.,
     * or xxxSafe when invoked from FreeMarker *.ftl templates; switched by {@link org.ofbiz.entity.ftl.DelegatorWrapperModel#get(String)}).
     */

    /**
     * SCIPIO: Returns a new {@link org.ofbiz.entity.util.EntityQuery}(Safe) for this delegator, equivalent to:
     * <code>EntityQuery(Safe).use(delegator)</code>; this is an alias for {@link #queryUnsafe()} for most languages,
     * <strong>except</strong> for FreeMarker templates (*.ftl) for which {@link #querySafe()} is invoked instead.
     */
    default EntityQuery query() {
        return EntityQuery.use(this);
    }

    /**
     * SCIPIO: Returns a new {@link org.ofbiz.entity.util.EntityQuery} for this delegator, equivalent to:
     * <code>EntityQuery.use(delegator)</code>, whose query methods may throw GenericEntityException.
     */
    default EntityQuery queryUnsafe() {
        return EntityQuery.use(this);
    }

    /**
     * SCIPIO: Returns a new {@link org.ofbiz.entity.util.EntityQuerySafe} for this delegator, equivalent to:
     * <code>EntityQuerySafe.use(delegator)</code>, whose query methods do not throw GenericEntityException.
     */
    default EntityQuerySafe querySafe() {
        return EntityQuerySafe.use(this);
    }

    /**
     * SCIPIO: Returns a new {@link org.ofbiz.entity.util.EntityQuery}(Safe) for this delegator, set up to query the specified entity,
     * equivalent to: <code>EntityQuery(Safe).use(delegator).from(entityName)</code>; this is an alias for {@link #fromUnsafe} for
     * most languages, <strong>except</strong> for FreeMarker templates* (*.ftl) for which {@link #fromSafe} is invoked instead.
     */
    default EntityQuery from(String entityName) {
        return query().from(entityName);
    }

    /**
     * SCIPIO: Returns a new {@link org.ofbiz.entity.util.EntityQuery}(Safe) for this delegator, set up to query the specified entity,
     * equivalent to: <code>EntityQuery(Safe).use(delegator).from(dynamicViewEntity)</code>; this is an alias for {@link #fromUnsafe} for
     * most languages, <strong>except</strong> for FreeMarker templates* (*.ftl) for which {@link #fromSafe} is invoked instead.
     */
    default EntityQuery from(DynamicViewEntity dynamicViewEntity) {
        return query().from(dynamicViewEntity);
    }

    /**
     * SCIPIO: Returns a new {@link org.ofbiz.entity.util.EntityQuery}(Safe) for this delegator, set up to select the specified fields,
     * equivalent to: <code>EntityQuery(Safe).use(delegator).select(fieldsToSelect)</code>; this is an alias for {@link #selectUnsafe} for
     * most languages, <strong>except</strong> for FreeMarker templates* (*.ftl) for which {@link #selectSafe} is invoked instead.
     */
    default EntityQuery select(Set<String> fieldsToSelect) {
        return query().select(fieldsToSelect);
    }

    /**
     * SCIPIO: Returns a new {@link org.ofbiz.entity.util.EntityQuery}(Safe) for this delegator, set up to select the specified fields,
     * equivalent to: <code>EntityQuery(Safe).use(delegator).select(fieldsToSelect)</code>; this is an alias for {@link #selectUnsafe} for
     * most languages, <strong>except</strong> for FreeMarker templates* (*.ftl) for which {@link #selectSafe} is invoked instead.
     */
    default EntityQuery select(String... fieldsToSelect) {
        return query().select(fieldsToSelect);
    }

    /**
     * SCIPIO: Returns a new {@link org.ofbiz.entity.util.EntityQuery} for this delegator, set up to query the specified entity,
     * equivalent to: <code>EntityQuery.use(delegator).from(entityName)</code>, whose query methods may throw GenericEntityException.
     */
    default EntityQuery fromUnsafe(String entityName) {
        return queryUnsafe().from(entityName);
    }

    /**
     * SCIPIO: Returns a new {@link org.ofbiz.entity.util.EntityQuery} for this delegator, set up to query the specified entity,
     * equivalent to: <code>EntityQuery.use(delegator).from(dynamicViewEntity)</code>, whose query methods may throw GenericEntityException.
     */
    default EntityQuery fromUnsafe(DynamicViewEntity dynamicViewEntity) {
        return queryUnsafe().from(dynamicViewEntity);
    }

    /**
     * SCIPIO: Returns a new {@link org.ofbiz.entity.util.EntityQuery} for this delegator, set up to select the specified fields,
     * equivalent to: <code>EntityQuery.use(delegator).select(fieldsToSelect)</code>, whose query methods may throw GenericEntityException.
     */
    default EntityQuery selectUnsafe(Set<String> fieldsToSelect) {
        return queryUnsafe().select(fieldsToSelect);
    }

    /**
     * SCIPIO: Returns a new {@link org.ofbiz.entity.util.EntityQuery} for this delegator, set up to select the specified fields,
     * equivalent to: <code>EntityQuery.use(delegator).select(fieldsToSelect)</code>, whose query methods may throw GenericEntityException.
     */
    default EntityQuery selectUnsafe(String... fieldsToSelect) {
        return queryUnsafe().select(fieldsToSelect);
    }

    /**
     * SCIPIO: Returns a new {@link org.ofbiz.entity.util.EntityQuerySafe} for this delegator, set up to query the specified entity,
     * equivalent to: <code>EntityQuerySafe.use(delegator).from(entityName)</code>, whose query methods do not throw GenericEntityException.
     */
    default EntityQuery fromSafe(String entityName) {
        return querySafe().from(entityName);
    }

    /**
     * SCIPIO: Returns a new {@link org.ofbiz.entity.util.EntityQuerySafe} for this delegator, set up to query the specified entity,
     * equivalent to: <code>EntityQuerySafe.use(delegator).from(dynamicViewEntity)</code>, whose query methods do not throw GenericEntityException.
     */
    default EntityQuery fromSafe(DynamicViewEntity dynamicViewEntity) {
        return querySafe().from(dynamicViewEntity);
    }

    /**
     * SCIPIO: Returns a new {@link org.ofbiz.entity.util.EntityQuerySafe} for this delegator, set up to select the specified fields,
     * equivalent to: <code>EntityQuerySafe.use(delegator).select(fieldsToSelect)</code>, whose query methods do not throw GenericEntityException.
     */
    default EntityQuery selectSafe(Set<String> fieldsToSelect) {
        return querySafe().select(fieldsToSelect);
    }

    /**
     * SCIPIO: Returns a new {@link org.ofbiz.entity.util.EntityQuerySafe} for this delegator, set up to select the specified fields,
     * equivalent to: <code>EntityQuerySafe.use(delegator).select(fieldsToSelect)</code>, whose query methods do not throw GenericEntityException.
     */
    default EntityQuery selectSafe(String... fieldsToSelect) {
        return querySafe().select(fieldsToSelect);
    }
}
