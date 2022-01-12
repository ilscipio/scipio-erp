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

import java.io.IOException;
import java.io.PrintWriter;
import java.io.Serializable;
import java.math.BigDecimal;
import java.nio.ByteBuffer;
import java.sql.Blob;
import java.sql.SQLException;
import java.util.*;
import java.util.concurrent.ConcurrentHashMap;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.ilscipio.scipio.ce.util.collections.ScipioMap;
import org.ofbiz.base.crypto.HashCrypt;
import org.ofbiz.base.lang.JSON;
import org.ofbiz.base.util.*;
import org.ofbiz.base.util.Base64;
import org.ofbiz.base.util.Observable;
import org.ofbiz.base.util.Observer;
import org.ofbiz.base.util.collections.LocalizedMap;
import org.ofbiz.entity.condition.EntityCondition;
import org.ofbiz.entity.condition.EntityFieldMap;
import org.ofbiz.entity.jdbc.SqlJdbcUtil;
import org.ofbiz.entity.model.ModelEntity;
import org.ofbiz.entity.model.ModelField;
import org.ofbiz.entity.model.ModelFieldType;
import org.ofbiz.entity.model.ModelKeyMap;
import org.ofbiz.entity.model.ModelRelation;
import org.ofbiz.entity.model.ModelViewEntity;
import org.ofbiz.entity.model.ModelViewEntity.ModelAlias;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
/**
 * Generic Entity Value Object - Handles persistence for any defined entity.
 * <p>Note that this class extends <code>Observable</code> to achieve change notification for
 * <code>Observer</code>s. Whenever a field changes the name of the field will be passed to
 * the <code>notifyObservers()</code> method, and through that to the <code>update()</code> method of each
 * <code>Observer</code>.</p>
 * <p>This class is not thread-safe. If an instance of this class is shared between threads,
 * then it should be made immutable by calling the <code>setImmutable()</code> method.</p>
 *
 */
@SuppressWarnings("serial")
public class GenericEntity implements ScipioMap<String, Object>, LocalizedMap<Object>, Serializable, Comparable<GenericEntity>, Cloneable {

    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());
    public static final GenericEntity NULL_ENTITY = new NullGenericEntity();
    public static final NullField NULL_FIELD = new NullField();

    // Do not restore observers during deserialization. Instead, client code must add observers.
    private transient Observable observable = new Observable();

    /** Name of the GenericDelegator, used to re-get the GenericDelegator when deserialized */
    private String delegatorName = null;

    /** Reference to an instance of GenericDelegator used to do some basic operations on this entity value. If null various methods in this class will fail. This is automatically set by the GenericDelegator for all GenericValue objects instantiated through it. You may set this manually for objects you instantiate manually, but it is optional. */
    private transient Delegator internalDelegator = null;

    /** A Map containing the original field values from the database.
     */
    private Map<String, Object> originalDbValues = null;

    /** Contains the fields for this entity. Note that this should always be a
     *  HashMap to allow for two things: non-synchronized reads (synchronized
     *  writes are done through synchronized setters) and being able to store
     *  null values. Null values are important because with them we can distinguish
     *  between desiring to set a value to null and desiring to not modify the
     *  current value on an update.
     */
    private Map<String, Object> fields = new HashMap<>();

    /** Contains the entityName of this entity, necessary for efficiency when creating EJBs */
    private String entityName = null;

    /** Contains the ModelEntity instance that represents the definition of this entity, not to be serialized */
    private transient ModelEntity modelEntity = null;

    private boolean generateHashCode = true;
    private int cachedHashCode = 0;

    /** Used to specify whether or not this representation of the entity can be changed; generally cleared when this object comes from a cache */
    private boolean mutable = true;

    /** This is an internal field used to specify that a value has come from a sync process and that the auto-stamps should not be over-written */
    private boolean isFromEntitySync = false;

    /**
     * Parsed json objects by field name.
     * <p>SCIPIO: 2.1.0: Added.</p>
     */
    private transient Map<String, Object> jsonCache;

    /** Creates new GenericEntity - Should never be used, prefer the other options. */
    protected GenericEntity() { }

    /** Creates new GenericEntity */
    public static GenericEntity createGenericEntity(ModelEntity modelEntity) {
        if (modelEntity == null) {
            throw new IllegalArgumentException("Cannot create a GenericEntity with a null modelEntity parameter");
        }

        GenericEntity newEntity = new GenericEntity();
        newEntity.init(modelEntity);
        return newEntity;
    }

    /** Creates new GenericEntity from existing Map */
    public static GenericEntity createGenericEntity(Delegator delegator, ModelEntity modelEntity, Map<String, ? extends Object> fields) {
        if (modelEntity == null) {
            throw new IllegalArgumentException("Cannot create a GenericEntity with a null modelEntity parameter");
        }

        GenericEntity newEntity = new GenericEntity();
        newEntity.init(delegator, modelEntity, fields);
        return newEntity;
    }

    /** Copy Factory Method: Creates new GenericEntity from existing GenericEntity */
    public static GenericEntity createGenericEntity(GenericEntity value) {
        if (value == null) {
            throw new IllegalArgumentException("Cannot create a GenericEntity with a null value parameter");
        }

        GenericEntity newEntity = new GenericEntity();
        newEntity.init(value);
        return newEntity;
    }

    protected void assertIsMutable() {
        if (!this.mutable) {
            String msg = "This object has been flagged as immutable (unchangeable), probably because it came from an Entity Engine cache. Cannot modify an immutable entity object. Use the clone method to create a mutable copy of this object.";
            IllegalStateException toBeThrown = new IllegalStateException(msg);
            Debug.logError(toBeThrown, module);
            throw toBeThrown;
        }
    }

    private Observable getObservable() {
        if (this.observable == null) {
            this.observable = new Observable();
        }
        return this.observable;
    }

    /** Creates new GenericEntity */
    protected void init(ModelEntity modelEntity) {
        assertIsMutable();
        if (modelEntity == null) {
            throw new IllegalArgumentException("Cannot create a GenericEntity with a null modelEntity parameter");
        }
        this.modelEntity = modelEntity;
        this.entityName = modelEntity.getEntityName();
        this.observable = new Observable();

        // check some things
        if (this.entityName == null) {
            throw new IllegalArgumentException("Cannot create a GenericEntity with a null entityName in the modelEntity parameter");
        }
    }

    /** Creates new GenericEntity from existing Map */
    protected void init(Delegator delegator, ModelEntity modelEntity, Map<String, ? extends Object> fields, Object fieldNames) {
        assertIsMutable();
        if (modelEntity == null) {
            throw new IllegalArgumentException("Cannot create a GenericEntity with a null modelEntity parameter");
        }
        this.modelEntity = modelEntity;
        this.entityName = modelEntity.getEntityName();
        this.delegatorName = delegator.getDelegatorName();
        this.internalDelegator = delegator;
        this.observable = new Observable();
        if (fields instanceof GenericEntity) { // SCIPIO
            fields = ((GenericEntity) fields).fields;
        }
        if (fieldNames != null) { // SCIPIO
            if (fieldNames instanceof Iterable) {
                for (String fieldName : UtilGenerics.<Iterable<String>>cast(fieldNames)) {
                    Object value = fields.get(fieldName);
                    // TODO: REVIEW: 2020-10-11: Always set the key otherwise modifies existing behavior like #getPrimaryKey
                    //if (value != null) {
                    set(fieldName, value);
                    //}
                }
            } else if (fieldNames instanceof Map) {
                for (Map.Entry<String, String> fieldNameMap : UtilGenerics.<Map<String, String>>cast(fieldNames).entrySet()) {
                    if (fieldNameMap.getValue() != null) {
                        Object value = fields.get(fieldNameMap.getValue());
                        //if (value != null) {
                        set(fieldNameMap.getKey(), value);
                        //}
                    }
                }
            } else {
                throw new IllegalArgumentException("Invalid initialization field names");
            }
        } else {
            setFields(fields);
        }

        // check some things
        if (this.entityName == null) {
            throw new IllegalArgumentException("Cannot create a GenericEntity with a null entityName in the modelEntity parameter");
        }
    }

    protected void initFields() {

    }

    /** Creates new GenericEntity from existing Map */
    protected void init(Delegator delegator, ModelEntity modelEntity, Map<String, ? extends Object> fields) {
        init(delegator, modelEntity, fields, null); // SCIPIO: delegating
    }

    /** Creates new GenericEntity from existing Map */
    protected void init(Delegator delegator, ModelEntity modelEntity, Object singlePkValue) {
        assertIsMutable();
        if (modelEntity == null) {
            throw new IllegalArgumentException("Cannot create a GenericEntity with a null modelEntity parameter");
        }
        if (modelEntity.getPksSize() != 1) {
            throw new IllegalArgumentException("Cannot create a GenericEntity with more than one primary key field");
        }
        this.modelEntity = modelEntity;
        this.entityName = modelEntity.getEntityName();
        this.delegatorName = delegator.getDelegatorName();
        this.internalDelegator = delegator;
        this.observable = new Observable();
        set(modelEntity.getOnlyPk().getName(), singlePkValue);

        // check some things
        if (this.entityName == null) {
            throw new IllegalArgumentException("Cannot create a GenericEntity with a null entityName in the modelEntity parameter");
        }
    }

    /** Copy Constructor: Creates new GenericEntity from existing GenericEntity. SCIPIO: Now supports fieldNames subset, newValue to prevent observable. */
    protected void init(GenericEntity value, Collection<String> fieldNames, boolean newValue) {
        assertIsMutable();
        // check some things
        if (value.entityName == null) {
            throw new IllegalArgumentException("Cannot create a GenericEntity with a null entityName in the modelEntity parameter");
        }
        this.entityName = value.getEntityName();
        // NOTE: could call getModelEntity to insure we have a value, just in case the value passed in has been serialized, but might as well leave it null to keep the object light if it isn't there
        this.modelEntity = value.modelEntity;
        if (value.fields != null) {
            if (fieldNames != null) {
                for(String fieldName : fieldNames) {
                    this.fields.put(fieldName, value.fields.get(fieldName));
                }
            } else {
                this.fields.putAll(value.fields);
            }
        }
        this.delegatorName = value.delegatorName;
        this.internalDelegator = value.internalDelegator;
        this.observable = newValue ? new Observable() : new Observable(value.observable);
    }

    /** Copy Constructor: Creates new GenericEntity from existing GenericEntity. */
    protected void init(GenericEntity value) {
        init(value, null, false); // SCIPIO: Refactored
    }

    /** SCIPIO: Creates new GenericEntity partially from fields from existing GenericEntity with new-to-existing field name mappings, but treated as a "new" instance (not a "copy");
     * source fields are assumed to already be correct/same types as those on the new value (no type checks).
     * <p>This is similar to {@link #init(Delegator, ModelEntity, Map, Object)} except the source fields are not re-checked for type.</p>
     * <p>NOTE: Instance members other than "fields" are treated as a "new" value, not copied from the passed value; this is half-way between
     * copy constructor and construction from map. Added 2018-10-22.</p> */
    protected void initAsFieldSubset(Delegator delegator, ModelEntity modelEntity, Map<String, Object> fields, Object fieldNames) {
        assertIsMutable();
        if (modelEntity == null) {
            throw new IllegalArgumentException("Cannot create a GenericEntity with a null modelEntity parameter");
        }
        this.modelEntity = modelEntity;
        this.entityName = modelEntity.getEntityName();
        this.delegatorName = delegator.getDelegatorName();
        this.internalDelegator = delegator;
        this.observable = new Observable();
        if (fields instanceof GenericEntity) {
            fields = ((GenericEntity) fields).fields;
        }
        if (fieldNames != null) {
            if (fieldNames instanceof Iterable) {
                for(String fieldName : UtilGenerics.<Iterable<String>>cast(fieldNames)) {
                    Object value = fields.get(fieldName);
                    // TODO: REVIEW: 2020-10-11: Always set the key otherwise modifies existing behavior like #getPrimaryKey
                    //if (value != null) {
                    this.fields.put(fieldName, value);
                    //}
                }
            } else if (fieldNames instanceof Map) {
                for(Map.Entry<String, String> fieldNameMap : UtilGenerics.<Map<String, String>>cast(fieldNames).entrySet()) {
                    if (fieldNameMap.getValue() != null) {
                        Object value = fields.get(fieldNameMap.getValue());
                        //if (value != null) {
                        this.fields.put(fieldNameMap.getKey(), value);
                        //}
                    }
                }
            } else {
                throw new IllegalArgumentException("Invalid initialization field names");
            }
        } else {
            this.fields.putAll(fields);
        }

        // check some things
        if (this.entityName == null) {
            throw new IllegalArgumentException("Cannot create a GenericEntity with a null entityName in the modelEntity parameter");
        }
    }

    public void reset() {
        assertIsMutable();
        // from GenericEntity
        this.delegatorName = null;
        this.internalDelegator = null;
        this.originalDbValues = null;
        this.fields = new HashMap<>();
        this.entityName = null;
        this.modelEntity = null;
        this.generateHashCode = true;
        this.cachedHashCode = 0;
        this.mutable = true;
        this.isFromEntitySync = false;
        this.observable = new Observable();
    }

    public void refreshFromValue(GenericEntity newValue) throws GenericEntityException {
        assertIsMutable();
        if (newValue == null) {
            throw new GenericEntityException("Could not refresh value, new value not found for: " + this);
        }
        GenericPK thisPK = this.getPrimaryKey();
        GenericPK newPK = newValue.getPrimaryKey();
        if (!thisPK.equals(newPK)) {
            throw new GenericEntityException("Could not refresh value, new value did not have the same primary key; this PK=" + thisPK + ", new value PK=" + newPK);
        }
        this.fields = new HashMap<>(newValue.fields);
        this.setDelegator(newValue.getDelegator());
        this.generateHashCode = newValue.generateHashCode;
        this.cachedHashCode = newValue.cachedHashCode;
        this.observable = new Observable(newValue.observable);
    }

    /**
     *
     * @deprecated Use hasChanged()
     */
    public boolean isModified() {
        return this.hasChanged();
    }

    /**
     * Flags this object as being synchronized with the data source.
     * The entity engine will call this method immediately after
     * populating this object with data from the data source.
     */
    public void synchronizedWithDatasource() {
        assertIsMutable();
        this.originalDbValues = Collections.unmodifiableMap(getAllFields());
        this.clearChanged();
    }

    /**
     * Flags this object as being removed from the data source.
     * The entity engine will call this method immediately after
     * removing this value from the data source. Once this method is
     * called, the object is immutable.
     */
    public void removedFromDatasource() {
        assertIsMutable();
        this.clearChanged();
        this.setImmutable();
    }

    public boolean isMutable() {
        return this.mutable;
    }

    public void setImmutable() {
        if (this.mutable) {
            this.mutable = false;
            this.fields = Collections.unmodifiableMap(this.fields);
        }
    }

    /**
     * @return Returns the isFromEntitySync.
     */
    public boolean getIsFromEntitySync() {
        return this.isFromEntitySync;
    }

    /**
     * @param isFromEntitySync The isFromEntitySync to set.
     */
    public void setIsFromEntitySync(boolean isFromEntitySync) {
        assertIsMutable();
        this.isFromEntitySync = isFromEntitySync;
    }

    public String getEntityName() {
        return entityName;
    }

    public ModelEntity getModelEntity() {
        if (modelEntity == null) {
            if (entityName != null) {
                modelEntity = this.getDelegator().getModelEntity(entityName);
            }
            if (modelEntity == null) {
                throw new IllegalStateException("[GenericEntity.getModelEntity] could not find modelEntity for entityName " + entityName);
            }
        }
        return modelEntity;
    }

    /** Get the GenericDelegator instance that created this value object and that is responsible for it.
     *@return GenericDelegator object
     */
    public Delegator getDelegator() {
        if (internalDelegator == null) {
            if (delegatorName == null) {
                delegatorName = "default";
            }
            internalDelegator = DelegatorFactory.getDelegator(delegatorName);
            if (internalDelegator == null) {
                throw new IllegalStateException("[GenericEntity.getDelegator] could not find delegator with name " + delegatorName);
            }
        }
        return internalDelegator;
    }

    /** Set the GenericDelegator instance that created this value object and that is responsible for it. */
    public void setDelegator(Delegator internalDelegator) {
        assertIsMutable();
        if (internalDelegator == null) {
            return;
        }
        this.delegatorName = internalDelegator.getDelegatorName();
        this.internalDelegator = internalDelegator;
    }

    public Object get(String name) {
        if (getModelEntity().getField(name) == null) {
            // SCIPIO: 2018-09-29: Throw more helpful EntityFieldNotFoundException instead
            //throw new IllegalArgumentException("The field name (or key) [" + name + "] is not valid for entity [" + this.getEntityName() + "].");
            throw new EntityFieldNotFoundException("The field name (or key) [" + name + "] is not valid for entity [" + this.getEntityName() + "].");
        }
        return fields.get(name);
    }

    public boolean hasModelField(String name) { // SCIPIO
        return getModelEntity().getField(name) != null;
    }

    /** Returns true if the entity contains all of the primary key fields, but NO others. */
    public boolean isPrimaryKey() {
        return isPrimaryKey(false);
    }
    public boolean isPrimaryKey(boolean requireValue) {
        TreeSet<String> fieldKeys = new TreeSet<>(this.fields.keySet());
        for (ModelField curPk: this.getModelEntity().getPkFieldsUnmodifiable()) {
            String fieldName = curPk.getName();
            if (requireValue) {
                if (this.fields.get(fieldName) == null) {
                    return false;
                }
            } else {
                if (!this.fields.containsKey(fieldName)) {
                    return false;
                }
            }
            fieldKeys.remove(fieldName);
        }
        if (!fieldKeys.isEmpty()) {
            return false;
        }
        return true;
    }

    /** Returns true if the entity contains all of the primary key fields. */
    public boolean containsPrimaryKey() {
        return containsPrimaryKey(false);
    }
    public boolean containsPrimaryKey(boolean requireValue) {
        for (ModelField curPk: this.getModelEntity().getPkFieldsUnmodifiable()) {
            String fieldName = curPk.getName();
            if (requireValue) {
                if (this.fields.get(fieldName) == null) {
                    return false;
                }
            } else {
                if (!this.fields.containsKey(fieldName)) {
                    return false;
                }
            }
        }
        return true;
    }

    /**
     * Returns a string of primary key field values delimited by "::" (none for single field primary keys).
     * New alias for {@link #getPkShortValueString()}.
     */
    public String getShortPk() { // SCIPIO: Renamed from getPkShortValueString
        try { // SCIPIO: Try single-PK
            ModelField onlyPk = this.getModelEntity().getOnlyPk();
            Object pkValue = this.get(onlyPk.getName());
            return (pkValue != null) ? pkValue.toString() : "null"; // "null" for legacy behavior
        } catch(IllegalArgumentException e) {
        }
        StringBuilder sb = new StringBuilder();
        for (ModelField curPk: this.getModelEntity().getPkFields()) {
            if (sb.length() > 0) {
                sb.append("::");
            }
            sb.append(this.get(curPk.getName()));
        }
        return sb.toString();
    }

    /**
     * Returns a string of primary key field values delimited by "::" (none for single field primary keys).
     * Legacy alias for {@link #getShortPk()}.
     */
    public String getPkShortValueString() {
        return getShortPk();
    }

    /** Sets the named field to the passed value, even if the value is null
     * @param name The field name to set
     * @param value The value to set
     */
    public void set(String name, Object value) {
        set(name, value, true);
    }

    /** Sets the named field to the passed value. If value is null, it is only
     *  set if the setIfNull parameter is true. This is useful because an update
     *  will only set values that are included in the HashMap and will store null
     *  values in the HashMap to the datastore. If a value is not in the HashMap,
     *  it will be left unmodified in the datastore.
     * @param name The field name to set
     * @param value The value to set
     * @param setIfNull Specifies whether or not to set the value if it is null
     */
    public Object set(String name, Object value, boolean setIfNull) {
        assertIsMutable();
        clearJsonCache(name); // SCIPIO: 2.1.0: Added
        ModelField modelField = getModelEntity().getField(name);
        if (modelField == null) {
            // SCIPIO: 2018-09-29: Throw more helpful EntityFieldNotFoundException instead
            //throw new IllegalArgumentException("[GenericEntity.set] \"" + name + "\" is not a field of " + entityName + ", must be one of: " + getModelEntity().fieldNameString());
            throw new EntityFieldNotFoundException("[GenericEntity.set] \"" + name + "\" is not a field of " + entityName + ", must be one of: " + getModelEntity().fieldNameString());
        }
        if (value != null || setIfNull) {
            ModelFieldType type = null;
            try {
                type = getDelegator().getEntityFieldType(getModelEntity(), modelField.getType());
            } catch (IllegalStateException | GenericEntityException e) {
                Debug.logWarning(e, module);
            }
            if (type == null) {
                throw new IllegalArgumentException("Type " + modelField.getType() + " not found for entity [" + this.getEntityName() + "]; probably because there is no datasource (helper) setup for the entity group that this entity is in: [" + this.getDelegator().getEntityGroupName(this.getEntityName()) + "]");
            }

            if (value instanceof Boolean) {
                // if this is a Boolean check to see if we should convert from an indicator or just leave as is
                try {
                    int fieldType = SqlJdbcUtil.getType(type.getJavaType());
                    if (fieldType != 10) {
                        value = (Boolean) value ? "Y" : "N";
                    }
                } catch (GenericNotImplementedException e) {
                    throw new IllegalArgumentException(e.getMessage());
                }
            } else if (value != null && !(value instanceof NULL)) {
                // make sure the type matches the field Java type
                if (value instanceof TimeDuration) {
                    try {
                        value = ObjectType.simpleTypeConvert(value, type.getJavaType(), null, null);
                    } catch (GeneralException e) {
                        Debug.logError(e, module);
                    }
                } else if ((value instanceof String) && "byte[]".equals(type.getJavaType())) {
                    value = ((String) value).getBytes(UtilIO.getUtf8());
                }
                if (!ObjectType.instanceOf(value, type.getJavaType())) {
                    if (!("java.sql.Blob".equals(type.getJavaType()) && (value instanceof byte[] || ObjectType.instanceOf(value, ByteBuffer.class)))) {
                        String errMsg = "In entity field [" + this.getEntityName() + "." + name + "] set the value passed in [" + value.getClass().getName() + "] is not compatible with the Java type of the field [" + type.getJavaType() + "]";
                        // eventually we should do this, but for now we'll do a "soft" failure: throw new IllegalArgumentException(errMsg);
                        Debug.logWarning(new Exception("Location of database type warning"), "=-=-=-=-=-=-=-=-= Database type warning GenericEntity.set =-=-=-=-=-=-=-=-= " + errMsg, module);
                    }
                }
            }
            Object old = fields.put(name, value);

            generateHashCode = true;
            this.setChanged();
            this.notifyObservers(name);
            return old;
        }
        return fields.get(name);
    }

    public void dangerousSetNoCheckButFast(ModelField modelField, Object value) {
        assertIsMutable();
        if (modelField == null) {
            throw new IllegalArgumentException("Cannot set field with a null modelField");
        }
        generateHashCode = true;
        this.fields.put(modelField.getName(), value);
        this.setChanged();
        this.notifyObservers(modelField.getName());
    }

    public Object dangerousGetNoCheckButFast(ModelField modelField) {
        if (modelField == null) {
            throw new IllegalArgumentException("Cannot get field with a null modelField");
        }
        return this.fields.get(modelField.getName());
    }

    /** Sets the named field to the passed value, converting the value from a String to the corrent type using <code>Type.valueOf()</code>
     * @param name The field name to set
     * @param value The String value to convert and set
     */
    public void setString(String name, String value) {
        if (value == null) {
            set(name, null);
            return;
        }

        boolean isNullString = false;
        if ("null".equals(value) || "[null-field]".equals(value)) { // keep [null-field] but it'not used now
            // count this as a null too, but only for numbers and stuff, not for Strings
            isNullString = true;
        }

        ModelField field = getModelEntity().getField(name);
        if (field == null)
         {
            set(name, value); // this will get an error in the set() method...
        }

        ModelFieldType type = null;
        try {
            if (field != null) {
                type = getDelegator().getEntityFieldType(getModelEntity(), field.getType());
            }
        } catch (IllegalStateException | GenericEntityException e) {
            Debug.logWarning(e, module);
        }
        if (type == null) {
            throw new IllegalArgumentException("Type " + field.getType() + " not found");
        }
        String fieldType = type.getJavaType();

        try {
            switch (SqlJdbcUtil.getType(fieldType)) {
            case 1:
                set(name, value);
                break;

            case 2:
                set(name, isNullString ? null : java.sql.Timestamp.valueOf(value));
                break;

            case 3:
                set(name, isNullString ? null : java.sql.Time.valueOf(value));
                break;

            case 4:
                set(name, isNullString ? null : java.sql.Date.valueOf(value));
                break;

            case 5:
                set(name, isNullString ? null : Integer.valueOf(value));
                break;

            case 6:
                set(name, isNullString ? null : Long.valueOf(value));
                break;

            case 7:
                set(name, isNullString ? null : Float.valueOf(value));
                break;

            case 8:
                set(name, isNullString ? null : Double.valueOf(value));
                break;

            case 9: // BigDecimal
                set(name, isNullString ? null : new BigDecimal(value));
                break;

            case 10:
                set(name, isNullString ? null : Boolean.valueOf(value));
                break;

            case 11: // Object
                set(name, value);
                break;

            case 12: // java.sql.Blob
                // TODO: any better way to handle Blob from String?
                set(name, value);
                break;

            case 13: // java.sql.Clob
                // TODO: any better way to handle Clob from String?
                set(name, value);
                break;

            case 14: // java.util.Date
                set(name, UtilDateTime.toDate(value));
                break;

            case 15: // java.util.Collection
                // TODO: how to convert from String to Collection? ie what should the default behavior be?
                set(name, value);
                break;
            }
        } catch (GenericNotImplementedException ex) {
            throw new IllegalArgumentException(ex.getMessage());
        }
    }

    /** Sets a field with an array of bytes, wrapping them automatically for easy use.
     * @param name The field name to set
     * @param bytes The byte array to be wrapped and set
     */
    public void setBytes(String name, byte[] bytes) {
        this.set(name, bytes);
    }

    public void setNextSeqId() {
        List<String> pkFieldNameList = this.modelEntity.getPkFieldNames();
        if (pkFieldNameList.size() != 1) {
            throw new IllegalArgumentException("Cannot setNextSeqId for entity [" + this.getEntityName() + "] that does not have a single primary key field, instead has [" + pkFieldNameList.size() + "]");
        }

        String pkFieldName = pkFieldNameList.get(0);
        if (this.get(pkFieldName) != null) {
            // don't throw exception, too much of a pain and usually intended: throw new IllegalArgumentException("Cannot setNextSeqId, pk field [" + pkFieldName + "] of entity [" + this.getEntityName() + "] already has a value [" + this.get(pkFieldName) + "]");
        }

        String sequencedValue = this.getDelegator().getNextSeqId(this.getEntityName());
        this.set(pkFieldName, sequencedValue);
    }

    public Boolean getBoolean(String name) {
        Object obj = get(name);

        if (obj == null) {
            return null;
        }
        if (obj instanceof Boolean) {
            return (Boolean) obj;
        } else if (obj instanceof String) {
            String value = (String) obj;

            if ("Y".equalsIgnoreCase(value) || "T".equalsIgnoreCase(value)) {
                return Boolean.TRUE;
            } else if ("N".equalsIgnoreCase(value) || "F".equalsIgnoreCase(value)) {
                return Boolean.FALSE;
            } else {
                throw new IllegalArgumentException("getBoolean could not map the String '" + value + "' to Boolean type");
            }
        } else {
            throw new IllegalArgumentException("getBoolean could not map the object '" + obj.toString() + "' to Boolean type, unknown object type: " + obj.getClass().getName());
        }
    }

    public Boolean getBoolean(String name, Boolean defaultValue) { // SCIPIO
        Boolean value = getBoolean(name);
        return (value != null) ? value : defaultValue;
    }

    /** Returns the specified field as a <code>TimeDuration</code> instance.
     * The field's Java data type can be either <code>String</code> or
     * <code>Number</code>. Invalid Java data types will throw
     * <code>IllegalArgumentException</code>.
     *
     * @param name The name of the desired field
     * @return A <code>TimeDuration</code> instance or <code>null</code>
     */
    public TimeDuration getDuration(String name) {
        Object obj = get(name);
        if (obj == null) {
            return null;
        }
        try {
            Number number = (Number) obj;
            return TimeDuration.fromNumber(number);
        } catch (Exception e) {}
        try {
            String duration = (String) obj;
            return TimeDuration.parseDuration(duration);
        } catch (Exception e) {}
        throw new IllegalArgumentException("getDuration could not map the object '" + obj.toString() + "' to TimeDuration type, incompatible object type: " + obj.getClass().getName());
    }

    /**
     * Returns the named field as string, or null.
     */
    public String getString(String name) {
        Object object = get(name);
        return object == null ? null : object.toString();
    }

    /**
     * Returns the named field as string in the given locale, or null.
     * <p>SCIPIO: 2019-03-07: Added overload.</p>
     */
    public String getString(String name, Locale locale) {
        Object object = get(name, locale);
        return object == null ? null : object.toString();
    }

    /**
     * Returns the named field as string in the given locale, or null.
     * <p>SCIPIO: 2019-03-07: Added overload.</p>
     */
    public String getString(String name, String resource, Locale locale) {
        Object object = get(name, resource, locale);
        return object == null ? null : object.toString();
    }

    /**
     * Returns the named field as string, or empty string if unset or null.
     * <p>SCIPIO: 2.1.0: Added helper.</p>
     */
    public String getStringOrEmpty(String name) {
        Object object = get(name);
        return object == null ? "" : object.toString();
    }

    /**
     * Returns the named field as string in the given locale, or empty string if unset or null.
     * <p>SCIPIO: 2.1.0: Added helper.</p>
     */
    public String getStringOrEmpty(String name, Locale locale) {
        Object object = get(name, locale);
        return object == null ? "" : object.toString();
    }

    /**
     * Returns the named field as string in the given locale, or empty string if unset or null.
     * <p>SCIPIO: 2.1.0: Added helper.</p>
     */
    public String getStringOrEmpty(String name, String resource, Locale locale) {
        Object object = get(name, resource, locale);
        return object == null ? "" : object.toString();
    }

    public java.sql.Timestamp getTimestamp(String name) {
        return (java.sql.Timestamp) get(name);
    }

    public java.sql.Time getTime(String name) {
        return (java.sql.Time) get(name);
    }

    public java.sql.Date getDate(String name) {
        return (java.sql.Date) get(name);
    }

    public Integer getInteger(String name) {
        return (Integer) get(name);
    }

    public Long getLong(String name) {
        return (Long) get(name);
    }

    public Float getFloat(String name) {
        return (Float) get(name);
    }

    public Double getDouble(String name) {
        // this "hack" is needed for now until the Double/BigDecimal issues are all resolved
        Object value = get(name);
        if (value instanceof BigDecimal) {
            return ((BigDecimal) value).doubleValue();
        }
        return (Double) value;
    }

    public BigDecimal getBigDecimal(String name) {
        // this "hack" is needed for now until the Double/BigDecimal issues are all resolved
        // NOTE: for things to generally work properly BigDecimal should really be used as the java-type in the field type def XML files
        Object value = get(name);
        if (value instanceof Double) {
            return new BigDecimal((Double) value);
        }
        return (BigDecimal) value;
    }

    public Number getNumber(String name) { // SCIPIO
        return (Number) get(name);
    }

    /**
     * Interprets the given field as a JSON object, or null if the field is null/empty.
     * <p>SCIPIO: 2.1.0: Added.</p>
     */
    public JSON getJson(String name) {
        String jsonString = getString(name);
        return (jsonString != null) ? JSON.from(jsonString) : null;
    }

    /**
     * Converts the given Java object or JSON wrapper to a JSON string and writes it to the field.
     * <p>SCIPIO: 2.1.0: Added.</p>
     */
    public void setJson(String name, Object object) {
        if (object instanceof String) {
            ;
        } else if (object instanceof JSON) {
            object = object.toString();
        } else if (object != null) {
            try {
                object = JSON.toString(object);
            } catch(Exception e) {
                throw new IllegalArgumentException("Could not convert field [" + name + "] of entity " +
                        getEntityName() + " from Java type [" + object.getClass() + "] to JSON string", e);
            }
        }
        set(name, object);
    }

    /**
     * Interprets the named field as a JSON object and evaluates it to a Java type, or null if the field is null/empty,
     * with support for caching of parsed json to/from {@link #jsonCache} for read-only objects.
     * <p>SCIPIO: 2.1.0: Added.</p>
     * @param name the field name
     * @param targetType the target type, either <code>Map.class</code> or <code>List.class</code>
     * @param returnDefault if true, when result is null, either a new collection (when useJsonCache false) or empty
     *                      collection returned (when useJsonCache true); if false returns null
     * @param useJsonCache when true, caches and returns read-only objects; when false, returns original modifiable full copies
     */
    public <T> T getJsonObject(String name, Class<?> targetType, boolean returnDefault, boolean useJsonCache) {
        T jsonObject = null;
        Map<String, Object> jsonCache = null;
        if (useJsonCache) {
            jsonCache = this.jsonCache;
            if (jsonCache != null) {
                jsonObject = UtilGenerics.cast(jsonCache.get(name));
                if (jsonObject != null) {
                    return jsonObject;
                }
            }
        }
        String jsonString = getString(name);
        if (jsonString != null) {
            try {
                jsonObject = JSON.toObject(jsonString, targetType);
            } catch (IOException e) {
                throw new IllegalArgumentException("Could not convert field [" + name + "] of entity " + getEntityName() +
                        " from JSON to Java type [" + targetType + "]", e);
            }
        }
        if (useJsonCache) {
            if (jsonObject != null) { // NOTE: We don't need any null-flag objects because get() empty string is fast
                jsonObject = UtilMisc.unmodifiableGeneric(jsonObject);
                // NOTE: Due to threading cached results may be lost due to replacing the cache, but this is minor
                Map<String, Object> jsonCacheNew = (jsonCache != null) ? new HashMap<>(jsonCache) : new HashMap<>();
                jsonCacheNew.put(name, jsonObject);
                this.jsonCache = Collections.unmodifiableMap(jsonCacheNew);
            } else {
                if (returnDefault) {
                    jsonObject = UtilMisc.emptyGeneric(targetType);
                }
            }
        } else {
            if (jsonObject == null) {
                if (returnDefault) {
                    jsonObject = makeJsonObject(targetType);
                }
            }
        }
        return jsonObject;
    }

    /**
     * Creates an empty json collection, normally <code>Map.class</code> or <code>List.class</code>.
     * <p>SCIPIO: 2.1.0: Added.</p>
     */
    public <T> T makeJsonObject(Class<?> targetType, Object initialValues) {
        if (Map.class.isAssignableFrom(targetType)) {
            return UtilGenerics.cast(makeJsonMap(UtilGenerics.cast(initialValues)));
        } else if (List.class.isAssignableFrom(targetType)) {
            return UtilGenerics.cast(makeJsonList(UtilGenerics.cast(initialValues)));
        } else if (Set.class.isAssignableFrom(targetType)) {
            return UtilGenerics.cast(makeJsonSet(UtilGenerics.cast(initialValues)));
        } else if (Collection.class.isAssignableFrom(targetType)) {
            return UtilGenerics.cast(makeJsonList(UtilGenerics.cast(initialValues)));
        } else {
            throw new IllegalArgumentException("Unsupported json collection type: " + targetType);
        }
    }

    /**
     * Creates an empty json collection, normally <code>Map.class</code> or <code>List.class</code>.
     * <p>SCIPIO: 2.1.0: Added.</p>
     */
    public <T> T makeJsonObject(Class<?> targetType) {
        return makeJsonObject(targetType, null);
    }

    /**
     * Interprets the named json field as a read-only map.
     * <p>SCIPIO: 2.1.0: Added.</p>
     * @param name the field name
     * @param returnDefault if true, when result is null, either a new collection (when useJsonCache false) or empty
     *                      collection returned (when useJsonCache true); if false returns null
     * @param useJsonCache when true, caches and returns read-only objects; when false, returns original modifiable full copies
     */
    public <K, V> Map<K, V> getJsonMap(String name, boolean returnDefault, boolean useJsonCache) {
        return getJsonObject(name, Map.class, returnDefault, useJsonCache);
    }

    /**
     * Interprets the named json field as a read-only map, or empty map if null, with local json caching.
     * <p>SCIPIO: 2.1.0: Added.</p>
     * @param name the field name
     */
    public <K, V> Map<K, V> getJsonMap(String name) {
        return getJsonMap(name, true, true);
    }

    /**
     * Interprets the named json field as a map copy, or new map if null, with no caching.
     * <p>SCIPIO: 2.1.0: Added.</p>
     * @param name the field name
     * @param returnDefault if true, when result is null, returns a new collection; if false returns null
     */
    public <K, V> Map<K, V> getJsonMapCopy(String name, boolean returnDefault) {
        return getJsonMap(name, returnDefault, false);
    }

    /**
     * Returns a new json map.
     * <p>SCIPIO: 2.1.0: Added.</p>
     */
    public <K, V> Map<K, V> makeJsonMap(Map<? extends K, ? extends V> initialValues) {
        return (initialValues != null) ? new LinkedHashMap<>(initialValues) : new LinkedHashMap<>();
    }

    /**
     * Returns a new json map.
     * <p>SCIPIO: 2.1.0: Added.</p>
     */
    public <K, V> Map<K, V> makeJsonMap() {
        return makeJsonMap(null);
    }

    /**
     * Interprets the named json field as a read-only list.
     * <p>SCIPIO: 2.1.0: Added.</p>
     * @param name the field name
     * @param returnDefault if true, when result is null, either a new collection (when useJsonCache false) or empty
     *                      collection returned (when useJsonCache true); if false returns null
     * @param useJsonCache when true, caches and returns read-only objects; when false, returns original modifiable full copies
     */
    public <T> List<T> getJsonList(String name, boolean returnDefault, boolean useJsonCache) {
        return getJsonObject(name, List.class, returnDefault, useJsonCache);
    }

    /**
     * Interprets the named json field as a read-only list, or empty list if null, with local json caching.
     * <p>SCIPIO: 2.1.0: Added.</p>
     * @param name the field name
     */
    public <T> List<T> getJsonList(String name) {
        return getJsonList(name, true, true);
    }

    /**
     * Interprets the named json field as a list copy, or new list if null, with no caching.
     * <p>SCIPIO: 2.1.0: Added.</p>
     * @param name the field name
     * @param returnDefault if true, when result is null, returns a new collection; if false returns null
     */
    public <T> List<T> getJsonListCopy(String name, boolean returnDefault) {
        return getJsonList(name, returnDefault, false);
    }

    /**
     * Returns a new json list.
     * <p>SCIPIO: 2.1.0: Added.</p>
     */
    public <E> List<E> makeJsonList(Collection<? extends E> initialValues) {
        return (initialValues != null) ? new ArrayList<>(initialValues) : new ArrayList<>();
    }

    /**
     * Returns a new json list.
     * <p>SCIPIO: 2.1.0: Added.</p>
     */
    public <E> List<E> makeJsonList() {
        return makeJsonList(null);
    }

    /**
     * Interprets the named json field as a read-only set.
     * <p>SCIPIO: 2.1.0: Added.</p>
     * @param name the field name
     * @param returnDefault if true, when result is null, either a new collection (when useJsonCache false) or empty
     *                      collection returned (when useJsonCache true); if false returns null
     * @param useJsonCache when true, caches and returns read-only objects; when false, returns original modifiable full copies
     */
    public <T> Set<T> getJsonSet(String name, boolean returnDefault, boolean useJsonCache) {
        return getJsonObject(name, Set.class, returnDefault, useJsonCache);
    }

    /**
     * Interprets the named json field as a read-only set, or empty set if null, with local json caching.
     * <p>SCIPIO: 2.1.0: Added.</p>
     * @param name the field name
     */
    public <T> Set<T> getJsonSet(String name) {
        return getJsonSet(name, true, true);
    }

    /**
     * Interprets the named json field as a set copy, or new set if null, with no caching.
     * <p>SCIPIO: 2.1.0: Added.</p>
     * @param name the field name
     * @param returnDefault if true, when result is null, returns a new collection; if false returns null
     */
    public <T> Set<T> getJsonSetCopy(String name, boolean returnDefault) {
        return getJsonSet(name, returnDefault, false);
    }

    /**
     * Returns a new json set.
     * <p>SCIPIO: 2.1.0: Added.</p>
     */
    public <E> Set<E> makeJsonSet(Collection<? extends E> initialValues) {
        return (initialValues != null) ? new LinkedHashSet<>(initialValues) : new LinkedHashSet<>();
    }

    /**
     * Returns a new json set.
     * <p>SCIPIO: 2.1.0: Added.</p>
     */
    public <E> Set<E> makeJsonSet() {
        return makeJsonSet(null);
    }

    /**
     * Interprets the standard entityJson field as a read-only map.
     * <p>SCIPIO: 2.1.0: Added.</p>
     * @param returnDefault if true, when result is null, either a new collection (when useJsonCache false) or empty
     *                      collection returned (when useJsonCache true); if false returns null
     * @param useJsonCache when true, caches and returns read-only objects; when false, returns original modifiable full copies
     */
    public <K, V> Map<K, V> getEntityJsonMap(boolean returnDefault, boolean useJsonCache) {
        return getJsonMap(ModelEntity.ENTITY_JSON_FIELD, returnDefault, useJsonCache);
    }

    /**
     * Interprets the named json field as a read-only map, or empty map if null, with local json caching.
     * <p>SCIPIO: 2.1.0: Added.</p>
     */
    public <K, V> Map<K, V> getEntityJsonMap() {
        return getEntityJsonMap(true, true);
    }

    /**
     * Interprets the standard entityJson field as a map copy, or new map if null, with no caching.
     * <p>SCIPIO: 2.1.0: Added.</p>
     * @param returnDefault if true, when result is null, returns a new collection; if false returns null
     */
    public <K, V> Map<K, V> getEntityJsonMapCopy(boolean returnDefault) {
        return getEntityJsonMap(returnDefault, false);
    }

    /**
     * Converts the given map to JSON string and writes it to the standard entityJson field.
     * <p>SCIPIO: 2.1.0: Added.</p>
     */
    public void setEntityJsonMap(Map<String, ?> jsonMap) {
        setJson(ModelEntity.ENTITY_JSON_FIELD, jsonMap);
    }

    /**
     * Returns a new json map for the standard entityJson field.
     * <p>SCIPIO: 2.1.0: Added.</p>
     */
    public <K, V> Map<K, V> makeEntityJsonMap(Map<? extends K, ? extends V> initialValues) {
        return makeJsonMap(initialValues);
    }

    /**
     * Returns a new json map for the standard entityJson field.
     * <p>SCIPIO: 2.1.0: Added.</p>
     */
    public <K, V> Map<K, V> makeEntityJsonMap() {
        return makeEntityJsonMap(null);
    }

    /**
     * Clears last json Map/List cache.
     * <p>SCIPIO: 2.1.0: Added.</p>
     */
    public void clearJsonCache() {
        this.jsonCache = null;
    }

    /**
     * Clears last json Map/List cache for the field name.
     * <p>NOTE: Currently forces a full clear.</p>
     * <p>SCIPIO: 2.1.0: Added.</p>
     */
    public void clearJsonCache(String name) {
        // TODO: REVIEW
        //Map<String, Object> jsonCache = this.jsonCache;
        //if (jsonCache != null) {
        //    jsonCache.remove(name);
        //}
        clearJsonCache();
    }

    @SuppressWarnings("deprecation")
    public byte[] getBytes(String name) {
        Object value = get(name);
        if (value == null) {
            return null;
        }
        if (value instanceof Blob) {
            try {
                Blob valueBlob = (Blob) value;
                return valueBlob.getBytes(1, (int) valueBlob.length());
            } catch (SQLException e) {
                String errMsg = "Error getting byte[] from Blob: " + e.toString();
                Debug.logError(e, errMsg, module);
                return null;
            }
        }
        if (value instanceof byte[]) {
            return (byte[]) value;
        }
        if (value instanceof org.ofbiz.entity.util.ByteWrapper) {
            // NOTE DEJ20071022: the use of ByteWrapper is not recommended and is deprecated, only old data should be stored that way
            Debug.logWarning("Found a ByteWrapper object in the database for field [" + this.getEntityName() + "." + name + "]; converting to byte[] and returning, but note that you need to update your database to unwrap these objects for future compatibility", module);
            org.ofbiz.entity.util.ByteWrapper wrapper = (org.ofbiz.entity.util.ByteWrapper) value;
            return wrapper.getBytes();
        }
        // uh-oh, this shouldn't happen...
        throw new IllegalArgumentException("In call to getBytes the value is not a supported type, should be byte[] or ByteWrapper, is: " + value.getClass().getName());
    }

    /** Checks a resource bundle for a value for this field using the entity name, the field name
     *    and a composite of the Primary Key field values as a key. If no value is found in the
     *    resource then the field value is returned. Uses the default-resource-name from the entity
     *    definition as the resource name. To specify a resource name manually, use the other getResource method.
     *
     *  So, the key in the resource bundle (properties file) should be as follows:
     *    &lt;entity-name&gt;.&lt;field-name&gt;.&lt;pk-field-value-1&gt;.&lt;pk-field-value-2&gt;...&lt;pk-field-value-n&gt;
     *  For example:
     *    ProductType.description.FINISHED_GOOD
     *
     * @param name The name of the field on the entity
     * @param locale The locale to use when finding the ResourceBundle, if null uses the default
     *    locale for the current instance of Java
     * @return If the corresponding resource is found and contains a key as described above, then that
     *    property value is returned; otherwise returns the field value
     */
    public Object get(String name, Locale locale) {
        return get(name, null, locale);
    }

    /** Same as the getResource method that does not take resource name, but instead allows manually
     *    specifying the resource name. In general you should use the other method for more consistent
     *    naming and use of the corresponding properties files.
     * @param name The name of the field on the entity
     * @param resource The name of the resource to get the value from; if null defaults to the
     *    default-resource-name on the entity definition, if specified there
     * @param locale The locale to use when finding the ResourceBundle, if null uses the default
     *    locale for the current instance of Java
     * @return If the specified resource is found and contains a key as described above, then that
     *    property value is returned; otherwise returns the field value
     */
    public Object get(String name, String resource, Locale locale) {
        Object fieldValue = get(name);
        // In case of view entity first try to retrieve with View field names
        ModelEntity modelEntityToUse = this.getModelEntity();
        Object resourceValue = get(this.getModelEntity(), modelEntityToUse, name, resource, locale);
        if (resourceValue == null) {
            if (modelEntityToUse instanceof ModelViewEntity) {
                //  now try to retrieve with the field heading from the real entity linked to the view
                ModelViewEntity modelViewEntity = (ModelViewEntity) modelEntityToUse;
                Iterator<ModelAlias> it = modelViewEntity.getAliasesIterator();
                while (it.hasNext()) {
                    ModelAlias modelAlias = it.next();
                    if (modelAlias.getName().equalsIgnoreCase(name)) {
                        modelEntityToUse = modelViewEntity.getMemberModelEntity(modelAlias.getEntityAlias());
                        name = modelAlias.getField();
                        break;
                    }
                }
                resourceValue = get(this.getModelEntity(), modelEntityToUse, name, resource, locale);
                if (resourceValue == null) {
                    return fieldValue;
                }
                return resourceValue;
            }
            return fieldValue;
        }
        return resourceValue;
    }

    /**
     * call by the previous method to be able to read with View entityName and entity Field and after for real entity
     * @param modelEntity the modelEntity, for a view it's the ViewEntity
     * @param modelEntityToUse, same as before except if it's a second call for a view, and so it's the real modelEntity
     * @return null or resourceValue
     */
    private Object get(ModelEntity modelEntity, ModelEntity modelEntityToUse, String name, String resource, Locale locale) {
        if (UtilValidate.isEmpty(resource)) {
            resource = modelEntityToUse.getDefaultResourceName();
            // still empty? return null
            if (UtilValidate.isEmpty(resource)) {
                return null;
            }
        }
        if (UtilProperties.isPropertiesResourceNotFound(resource, locale, false)) {
            // Properties do not exist for this resource+locale combination
            return null;
        }
        ResourceBundle bundle = null;
        try {
            bundle = UtilProperties.getResourceBundle(resource, locale);
        } catch (IllegalArgumentException e) {
            bundle = null;
        }
        if (bundle == null) {
            return null;
        }

        StringBuilder keyBuffer = new StringBuilder();
        // start with the Entity Name
        keyBuffer.append(modelEntityToUse.getEntityName());
        // next add the Field Name
        keyBuffer.append('.');
        keyBuffer.append(name);
        // finish off by adding the values of all PK fields
        if (modelEntity instanceof ModelViewEntity){
            // retrieve pkNames of realEntity
            ModelViewEntity modelViewEntity = (ModelViewEntity) modelEntity;
            List<String> pkNamesToUse = new ArrayList<>(modelEntityToUse.getPksSize()); // SCIPIO: switched to ArrayList
            // iterate on realEntity for pkField
            Iterator<ModelField> iter = modelEntityToUse.getPksIterator();
            while (iter != null && iter.hasNext()) {
                ModelField curField = iter.next();
                String pkName = null;
                Iterator<ModelAlias> iterAlias = modelViewEntity.getAliasesIterator();
                //search aliasName for pkField of realEntity
                while (iterAlias != null && iterAlias.hasNext()) {
                    ModelAlias aliasField = iterAlias.next();
                    if (aliasField.getField().equals(curField.getName())){
                        ModelEntity memberModelEntity = modelViewEntity.getMemberModelEntity(aliasField.getEntityAlias());
                        if (memberModelEntity.getEntityName().equals(modelEntityToUse.getEntityName())) {
                            pkName = aliasField.getName();
                            break;
                        }
                    }
                }
                if (pkName == null) {
                    pkName = curField.getName();
                }
                pkNamesToUse.add(pkName);
            }
            // read value with modelEntity name of pkNames
            for (String pkName : pkNamesToUse) {
                if (this.containsKey(pkName)) {
                    keyBuffer.append('.');
                    keyBuffer.append(this.get(pkName));
                }
            }
        } else {
            Iterator<ModelField> iter = modelEntity.getPksIterator();
            while (iter != null && iter.hasNext()) {
                ModelField curField = iter.next();
                keyBuffer.append('.');
                keyBuffer.append(this.get(curField.getName()));
            }
        }

        String bundleKey = keyBuffer.toString();

        Object resourceValue = null;
        try {
            resourceValue = bundle.getObject(bundleKey);
        } catch (MissingResourceException e) {
            return null;
        }
        return resourceValue;
    }

    /** Returns the entity value's primary key as a GenericPK, alias for {@link #getPrimaryKey()} (SCIPIO). */
    public GenericPK getPk() {
        return getPrimaryKey();
    }

    /** Returns the entity value's primary key as a GenericPK. */
    public GenericPK getPrimaryKey() {
        // SCIPIO: Unnecessary overhead
        //return GenericPK.create(this.getDelegator(), getModelEntity(), getFields(getModelEntity().getPkFieldNames()));
        return GenericPK.create(this.getDelegator(), getModelEntity(), fields, getModelEntity().getPkFieldNames());
    }

    /** go through the pks and for each one see if there is an entry in fields to set */
    public void setPKFields(Map<? extends Object, ? extends Object> fields) {
        setAllFields(fields, true, null, Boolean.TRUE);
    }

    /** go through the pks and for each one see if there is an entry in fields to set */
    public void setPKFields(Map<? extends Object, ? extends Object> fields, boolean setIfEmpty) {
        setAllFields(fields, setIfEmpty, null, Boolean.TRUE);
    }

    /** go through the non-pks and for each one see if there is an entry in fields to set */
    public void setNonPKFields(Map<? extends Object, ? extends Object> fields) {
        setAllFields(fields, true, null, Boolean.FALSE);
    }

    /** go through the non-pks and for each one see if there is an entry in fields to set */
    public void setNonPKFields(Map<? extends Object, ? extends Object> fields, boolean setIfEmpty) {
        setAllFields(fields, setIfEmpty, null, Boolean.FALSE);
    }

    /** Intelligently sets fields on this entity from the Map of fields passed in (SCIPIO). */
    public void setAllFields(Map<? extends Object, ? extends Object> fields) {
        setAllFields(fields, true, null, null);
    }

    /** Intelligently sets fields on this entity from the Map of fields passed in (SCIPIO). */
    public void setAllFields(Map<? extends Object, ? extends Object> fields, boolean setIfEmpty) {
        setAllFields(fields, setIfEmpty, null, null);
    }
    
    /** Intelligently sets fields on this entity from the Map of fields passed in
     * @param fields The fields Map to get the values from
     * @param setIfEmpty Used to specify whether empty/null values in the field Map should over-write non-empty values in this entity
     * @param namePrefix If not null or empty will be pre-pended to each field name (upper-casing the first letter of the field name first), and that will be used as the fields Map lookup name instead of the field-name
     * @param pks If null, set all values, if TRUE just set PKs, if FALSE just set non-PKs
     */
    public void setAllFields(Map<? extends Object, ? extends Object> fields, boolean setIfEmpty, String namePrefix, Boolean pks) {
        if (fields == null) {
            return;
        }
        Iterator<ModelField> iter = null;
        if (pks != null) {
            if (pks) {
                iter = this.getModelEntity().getPksIterator();
            } else {
                iter = this.getModelEntity().getNopksIterator();
            }
        } else {
            iter = this.getModelEntity().getFieldsIterator();
        }

        while (iter != null && iter.hasNext()) {
            ModelField curField = iter.next();
            String fieldName = curField.getName();
            String sourceFieldName = null;
            if (UtilValidate.isNotEmpty(namePrefix)) {
                sourceFieldName = namePrefix + Character.toUpperCase(fieldName.charAt(0)) + fieldName.substring(1);
            } else {
                sourceFieldName = curField.getName();
            }

            if (fields.containsKey(sourceFieldName)) {
                Object field = fields.get(sourceFieldName);

                if (setIfEmpty) {
                    // if empty string, set to null
                    if (field != null && field instanceof String && ((String) field).length() == 0) {
                        this.set(curField.getName(), null);
                    } else {
                        this.set(curField.getName(), field);
                    }
                } else {
                    // okay, only set if not empty...
                    if (field != null) {
                        // if it's a String then we need to check length, otherwise set it because it's not null
                        if (field instanceof String) {
                            String fieldStr = (String) field;

                            if (fieldStr.length() > 0) {
                                this.set(curField.getName(), fieldStr);
                            }
                        } else {
                            this.set(curField.getName(), field);
                        }
                    }
                }
            }
        }
    }

    /** Returns keys of entity fields
     * @return java.util.Collection
     */
    public Collection<String> getAllKeys() {
        return fields.keySet();
    }

    /** Returns key/value pairs of entity fields
     * @return java.util.Map
     */
    public Map<String, Object> getAllFields() {
        return new HashMap<>(this.fields);
    }

    /** Used by clients to specify exactly the fields they are interested in
     * @param keysofFields the name of the fields the client is interested in
     * @return java.util.Map
     */
    public Map<String, Object> getFields(Collection<String> keysofFields) {
        if (keysofFields == null) {
            return null;
        }
        Map<String, Object> aMap = new HashMap<>();

        for (String aKey: keysofFields) {
            aMap.put(aKey, this.fields.get(aKey));
        }
        return aMap;
    }

    /** SCIPIO: Used by clients to specify exactly the fields they are NOT interested in
     * Added 2017-12-15.
     * @param excludeKeysofFields the name of the fields the client is NOT interested in
     * @return java.util.Map
     */
    public Map<String, Object> getFieldsExclude(Collection<String> excludeKeysofFields) {
        Map<String, Object> aMap = new HashMap<String, Object>();
        if (excludeKeysofFields == null) excludeKeysofFields = Collections.emptySet();
        for (String aKey: getAllKeys()) {
            if (!excludeKeysofFields.contains(aKey)) {
                aMap.put(aKey, this.fields.get(aKey));
            }
        }
        return aMap;
    }

    /** Used by clients to update particular fields in the entity
     * @param keyValuePairs java.util.Map
     */
    public void setFields(Map<? extends String, ? extends Object> keyValuePairs) {
        if (keyValuePairs == null) {
            return;
        }
        // this could be implement with Map.putAll, but we'll leave it like this for the extra features it has
        for (Map.Entry<? extends String, ? extends Object> anEntry: keyValuePairs.entrySet()) {
            this.set(anEntry.getKey(), anEntry.getValue(), true);
        }
    }

    /** Sets the named fields to the passed values, even if the value is null (SCIPIO)
     * @param keyValuePairs Name/value pairs of values to set
     */
    public void setFields(Object... keyValuePairs) {
        // ArrayOutOfBounds is clear enough
        //if (keyValuePairs.length % 2 == 1) {
        //    throw new IllegalArgumentException("You must pass an even sized array to the setPairs method (size = " + keyValuePairs.length + ")");
        //}
        for (int i = 0; i < keyValuePairs.length;) {
            set((String) keyValuePairs[i++], keyValuePairs[i++]);
        }
    }

    public boolean matchesFields(Map<String, ? extends Object> keyValuePairs) {
        if (fields == null) {
            return true;
        }
        if (UtilValidate.isEmpty(keyValuePairs)) {
            return true;
        }
        for (Map.Entry<String, ? extends Object> anEntry: keyValuePairs.entrySet()) {
            if (!UtilValidate.areEqual(anEntry.getValue(), this.fields.get(anEntry.getKey()))) {
                return false;
            }
        }
        return true;
    }

    public GenericPK getRelatedOnePk(ModelRelation relation) { // SCIPIO
        ModelEntity relModelEntity = getDelegator().getModelEntity(relation.getRelEntityName());
        if (relModelEntity == null) {
            throw new IllegalStateException("[GenericEntity.getRelatedPk] could not find modelEntity for entityName " + relation.getRelEntityName());
        }
        GenericPK pk = GenericPK.createAsFieldSubset(getDelegator(), relModelEntity, this.fields, relation.getRelFieldToFieldNameMap());
        return (pk.size() == relModelEntity.getPksSize()) ? pk : null;
    }

    public GenericPK getRelatedOnePk(String relationName) { // SCIPIO
        return getRelatedOnePk(getModelEntity().getRelation(relationName));
    }

    public <C extends Collection<M>, M extends Map<String, Object>> C getRelatedOnePksForEntity(C out, String relatedEntityName) { // SCIPIO
        Iterator<ModelRelation> it = getModelEntity().getRelationsIterator();
        while(it.hasNext()) {
            ModelRelation relation = it.next();
            if (relatedEntityName.equals(relation.getRelEntityName()) && relation.getType().startsWith("one")) {
                GenericPK pk = getRelatedOnePk(relation);
                if (pk != null) {
                    out.add(UtilGenerics.cast(pk));
                }
            }
        }
        return out;
    }

    /** Used to indicate if locking is enabled for this entity
     * @return True if locking is enabled
     */
    public boolean lockEnabled() {
        return getModelEntity().lock();
    }

    // ======= XML Related Methods ========
    public static Document makeXmlDocument(Collection<GenericValue> values) {
        Document document = UtilXml.makeEmptyXmlDocument("entity-engine-xml");

        if (document == null) {
            return null;
        }

        addToXmlDocument(values, document);
        return document;
    }

    public static int addToXmlDocument(Collection<GenericValue> values, Document document) {
        return addToXmlElement(values, document, document.getDocumentElement());
    }

    public static int addToXmlElement(Collection<GenericValue> values, Document document, Element element) {
        if (values == null) {
            return 0;
        }
        if (document == null) {
            return 0;
        }

        int numberAdded = 0;

        for (GenericValue value: values) {
            Element valueElement = value.makeXmlElement(document);

            element.appendChild(valueElement);
            numberAdded++;
        }
        return numberAdded;
    }

    /** Makes an XML Element object with an attribute for each field of the entity
     *@param document The XML Document that the new Element will be part of
     *@return org.w3c.dom.Element object representing this generic entity
     */
    public Element makeXmlElement(Document document) {
        return makeXmlElement(document, null);
    }

    /** Makes an XML Element object with an attribute for each field of the entity
     *@param document The XML Document that the new Element will be part of
     *@param prefix A prefix to put in front of the entity name in the tag name
     *@return org.w3c.dom.Element object representing this generic entity
     */
    public Element makeXmlElement(Document document, String prefix) {
        Element element = null;

        if (prefix == null) {
            prefix = "";
        }
        if (document != null) {
            element = document.createElement(prefix + this.getEntityName());
        }
        if (element == null) {
            return null;
        }

        Iterator<ModelField> modelFields = this.getModelEntity().getFieldsIterator();
        while (modelFields.hasNext()) {
            ModelField modelField = modelFields.next();
            String name = modelField.getName();
            String value = this.getString(name);

            if (value != null) {
                if (value.indexOf('\n') >= 0 || value.indexOf('\r') >= 0) {
                    UtilXml.addChildElementCDATAValue(element, name, value, document);
                } else {
                    element.setAttribute(name, value);
                }
            }
        }

        return element;
    }

    /** Writes XML text with an attribute or CDATA element for each field of the entity
     *@param writer A PrintWriter to write to
     *@param prefix A prefix to put in front of the entity name in the tag name
     */
    public void writeXmlText(PrintWriter writer, String prefix) {
        writeXmlText(writer, prefix, null);
    }

    /** Writes XML text with an attribute or CDATA element for each field of the entity
     * SCIPIO: 2017-05-30: modified to support alwaysCdataFields.
     *@param writer A PrintWriter to write to
     *@param prefix A prefix to put in front of the entity name in the tag name
     *@param alwaysCdataFields Names of fields which should always be printed as CDATA blocks
     */
    public void writeXmlText(PrintWriter writer, String prefix, java.util.Set<String> alwaysCdataFields) {
        int indent = 4;
        StringBuilder indentStrBuf = new StringBuilder();
        for (int i = 0; i < indent; i++) {
            indentStrBuf.append(' ');
        }
        String indentString = indentStrBuf.toString();

        if (prefix == null) {
            prefix = "";
        }

        writer.print(indentString);
        writer.print('<');
        writer.print(prefix);
        writer.print(this.getEntityName());

        // write attributes immediately and if a CDATA element is needed, put those in a Map for now
        Map<String, String> cdataMap = new HashMap<>();

        Iterator<ModelField> modelFields = this.getModelEntity().getFieldsIterator();
        while (modelFields.hasNext()) {
            ModelField modelField = modelFields.next();
            String name = modelField.getName();

            String type = modelField.getType();
            if (type != null && ("blob".equals(type) || "byte-array".equals(type) || "object".equals(type))) { // SCIPIO: 2017-07-06: added export byte-array and object as base64; not just blob, otherwise invalid output for other two
                Object obj = get(name);
                boolean b1 = obj instanceof byte [];
                if (b1) {
                    byte [] binData = (byte [])obj;
                    String strData = new String(Base64.base64Encode(binData), UtilIO.getUtf8());
                    cdataMap.put(name, strData);
                } else {
                    Debug.logWarning("Field:" + name + " is not of type 'byte[]'. obj: " + obj, module);
                }
            } else {
                String valueStr = this.getString(name);

                if (valueStr != null) {
                    StringBuilder value = new StringBuilder(valueStr);
                    boolean needsCdata = false;

                    // SCIPIO: 2017-05-30: can now force specific fields to always print as cdata
                    if (alwaysCdataFields != null && alwaysCdataFields.contains(name)) {
                        needsCdata = true;
                    } else {
                        // check each character, if line-feed or carriage-return is found set needsCdata to true; also look for invalid characters
                        for (int i = 0; i < value.length(); i++) {
                            char curChar = value.charAt(i);
                            /*
                             * Some common character for these invalid values, have seen these are mostly from MS Word, but may be part of some standard:
                             * 5 = ... 18 = apostrophe 19 = left quotation mark 20 = right quotation mark 22 = – 23 = - 25 = tm
                             */

                            switch (curChar) {
                            case '\'':
                                value.replace(i, i+1, "&apos;");
                                break;
                            case '"':
                                value.replace(i, i+1, "&quot;");
                                break;
                            case '&':
                                value.replace(i, i+1, "&amp;");
                                break;
                            case '<':
                                value.replace(i, i+1, "&lt;");
                                break;
                            case '>':
                                value.replace(i, i+1, "&gt;");
                                break;
                            case 0xA: // newline, \n
                                needsCdata = true;
                                break;
                            case 0xD: // carriage return, \r
                                needsCdata = true;
                                break;
                            case 0x9: // tab
                                // do nothing, just catch here so it doesn't get into the default
                                break;
                            case 0x5: // elipses (...)
                                value.replace(i, i+1, "...");
                                break;
                            case 0x12: // apostrophe
                                value.replace(i, i+1, "&apos;");
                                break;
                            case 0x13: // left quote
                                value.replace(i, i+1, "&quot;");
                                break;
                            case 0x14: // right quote
                                value.replace(i, i+1, "&quot;");
                                break;
                            case 0x16: // big(?) dash -
                                value.replace(i, i+1, "-");
                                break;
                            case 0x17: // dash -
                                value.replace(i, i+1, "-");
                                break;
                            case 0x19: // tm
                                value.replace(i, i+1, "tm");
                                break;
                            default:
                                if (curChar < 0x20) {
                                    // if it is less that 0x20 at this point it is invalid because the only valid values < 0x20 are 0x9, 0xA, 0xD as caught above
                                    Debug.logInfo("Removing invalid character [" + curChar + "] numeric value [" + (int) curChar + "] for field " + name + " of entity with PK: " + this.getPrimaryKey().toString(), module);
                                    value.deleteCharAt(i);
                                } else if (curChar > 0x7F) {
                                    // Replace each char which is out of the ASCII range with a XML entity
                                    String replacement = "&#" + (int) curChar + ";";
                                    if (Debug.verboseOn()) {
                                        Debug.logVerbose("Entity: " + this.getEntityName() + ", PK: " + this.getPrimaryKey().toString() + " -> char [" + curChar + "] replaced with [" + replacement + "]", module);
                                    }
                                    value.replace(i, i+1, replacement);
                                }
                            }
                        }
                    }

                    if (needsCdata) {
                        // use valueStr instead of the escaped value, not needed or wanted in a CDATA block
                        cdataMap.put(name, valueStr);
                    } else {
                        writer.print(' ');
                        writer.print(name);
                        writer.print("=\"");
                        // encode the value...
                        writer.print(value.toString());
                        writer.print("\"");
                    }
                }
            }
        }

        if (cdataMap.size() == 0) {
            writer.println("/>");
        } else {
            writer.println('>');

            for (Map.Entry<String, String> entry: cdataMap.entrySet()) {
                writer.print(indentString);
                writer.print(indentString);
                writer.print('<');
                writer.print(entry.getKey());
                writer.print("><![CDATA[");
                writer.print(entry.getValue());
                writer.print("]]></");
                writer.print(entry.getKey());
                writer.println('>');
            }

            // don't forget to close the entity.
            writer.print(indentString);
            writer.print("</");
            writer.print(this.getEntityName());
            writer.println(">");
        }
    }

    /** Determines the equality of two GenericEntity objects, overrides the default equals
     *@param  obj  The object (GenericEntity) to compare this two
     *@return      boolean stating if the two objects are equal
     */
    @Override
    public boolean equals(Object obj) {
        if (obj == this) {
            return true;
        }
        if (obj instanceof GenericEntity) {
            GenericEntity that = (GenericEntity) obj;
            return this.entityName.equals(that.entityName) && this.fields.equals(that.fields);
        }
        return false;
    }

    /** Creates a hashCode for the entity, using the default String hashCode and Map hashCode, overrides the default hashCode
     *@return    Hashcode corresponding to this entity
     */
    @Override
    public int hashCode() {
        // divide both by two (shift to right one bit) to maintain scale and add together
        if (generateHashCode) {
            cachedHashCode = 0;
            if (getEntityName() != null) {
                cachedHashCode += getEntityName().hashCode() >> 1;
            }
            cachedHashCode += fields.hashCode() >> 1;
            generateHashCode = false;
        }
        return cachedHashCode;
    }

    /**
     * Creates a String for the entity, overrides the default toString
     * This method is secure, it will not display encrypted fields
     *
     *@return String corresponding to this entity
     */
    @Override
    public String toString() {
        StringBuilder theString = new StringBuilder();
        theString.append("[GenericEntity:");
        theString.append(getEntityName());
        theString.append(']');

        // SCIPIO: 2018-10-02: This sorting was highly counter-productive to readability; instead show fields 
        // in the order defined in entitymodel.xml, most importantly so that the primary key shows up first
        //for (String curKey: new TreeSet<>(fields.keySet())) {
        //    Object curValue = fields.get(curKey);
        //    ModelField field = this.getModelEntity().getField(curKey);
        for (ModelField field : this.getModelEntity().getFieldsUnmodifiable()) {
            String curKey = field.getName();
            Object curValue = fields.get(curKey);
            if (field.getEncryptMethod().isEncrypted() && curValue instanceof String) {
                String encryptField = (String) curValue;
                // the encryptField may not actually be UTF8, it could be any
                // random encoding; just treat it as a series of raw bytes.
                // This won't give the same output as the value stored in the
                // database, but should be good enough for printing
                curValue = HashCrypt.cryptBytes(null, null, encryptField.getBytes(UtilIO.getUtf8()));
            }
            theString.append('[');
            theString.append(curKey);
            theString.append(',');
            theString.append(curValue);
            theString.append('(');
            theString.append(curValue != null ? curValue.getClass().getName() : "");
            theString.append(')');
            theString.append(']');
        }
        return theString.toString();
    }

    /**
     * Creates a String for the entity, overrides the default toString
     * This method is NOT secure, it WILL display encrypted fields
     *
     *@return String corresponding to this entity
     */
    public String toStringInsecure() {
        StringBuilder theString = new StringBuilder();
        theString.append("[GenericEntity:");
        theString.append(getEntityName());
        theString.append(']');

        for (String curKey: new TreeSet<>(fields.keySet())) {
            Object curValue = fields.get(curKey);
            theString.append('[');
            theString.append(curKey);
            theString.append(',');
            theString.append(curValue);
            theString.append('(');
            theString.append(curValue != null ? curValue.getClass().getName() : "");
            theString.append(')');
            theString.append(']');
        }
        return theString.toString();
    }

    protected int compareToFields(GenericEntity that, String name) {
        Comparable<Object> thisVal = UtilGenerics.cast(this.fields.get(name));
        Object thatVal = that.fields.get(name);

        if (thisVal == null) {
            if (thatVal == null) {
                return 0;
            // if thisVal is null, but thatVal is not, return 1 to put this earlier in the list
            }
            return 1;
        }
        // if thatVal is null, put the other earlier in the list
        if (thatVal == null) {
            return  -1;
        }
        return thisVal.compareTo(thatVal);
    }

    /** Compares this GenericEntity to the passed object
     *@param that Object to compare this to
     *@return int representing the result of the comparison (-1,0, or 1)
     */
    public int compareTo(GenericEntity that) {
        // if null, it will push to the beginning
        if (that == null) {
            return -1;
        }

        int tempResult = this.entityName.compareTo(that.entityName);

        // if they did not match, we know the order, otherwise compare the primary keys
        if (tempResult != 0) {
            return tempResult;
        }

        // both have same entityName, should be the same so let's compare PKs
        Iterator<ModelField> pkIter = getModelEntity().getPksIterator();
        while (pkIter.hasNext()) {
            ModelField curField = pkIter.next();
            tempResult = compareToFields(that, curField.getName());
            if (tempResult != 0) {
                return tempResult;
            }
        }

        // okay, if we got here it means the primaryKeys are exactly the SAME, so compare the rest of the fields
        Iterator<ModelField> nopkIter = getModelEntity().getNopksIterator();
        while (nopkIter.hasNext()) {
            ModelField curField = nopkIter.next();
            if (!curField.getIsAutoCreatedInternal()) {
                tempResult = compareToFields(that, curField.getName());
                if (tempResult != 0) {
                    return tempResult;
                }
            }
        }

        // if we got here it means the two are exactly the same, so return tempResult, which should be 0
        return tempResult;
    }

    /**
     * Returns true if all fields are null or unset (SCIPIO).
     */
    public boolean isAllFieldsNull(boolean includeInternal) {
        if (includeInternal) {
            for(ModelField field : getModelEntity().getFields()) {
                if (get(field.getName()) != null) {
                    return false;
                }
            }
        } else {
            for(ModelField field : getModelEntity().getFields()) {
                if (field.getIsAutoCreatedInternal()) {
                    continue;
                }
                if (get(field.getName()) != null) {
                    return false;
                }
            }
        }
        return true;
    }

    /**
     * Returns true if all (non-internal) pk fields are null or unset (SCIPIO).
     */
    public boolean isPkFieldsNull(boolean includeInternal) {
        if (includeInternal) {
            for(ModelField field : getModelEntity().getPkFields()) {
                if (get(field.getName()) != null) {
                    return false;
                }
            }
        } else {
            for(ModelField field : getModelEntity().getPkFields()) {
                if (field.getIsAutoCreatedInternal()) {
                    continue;
                }
                if (get(field.getName()) != null) {
                    return false;
                }
            }
        }
        return true;
    }

    /**
     * Returns true if all (non-internal) non-pk fields are null or unset (SCIPIO).
     */
    public boolean isNonPkFieldsNull(boolean includeInternal) {
        if (includeInternal) {
            for(ModelField field : getModelEntity().getNoPkFields()) {
                if (get(field.getName()) != null) {
                    return false;
                }
            }
        } else {
            for(ModelField field : getModelEntity().getNoPkFields()) {
                if (field.getIsAutoCreatedInternal()) {
                    continue;
                }
                if (get(field.getName()) != null) {
                    return false;
                }
            }
        }
        return true;
    }

    /** Clones this GenericEntity, this is a shallow clone and uses the default shallow HashMap clone
     *  @return Object that is a clone of this GenericEntity
     */
    @Override
    public Object clone() {
        // SCIPIO: use newGeneric to make this correct in the base implementation
        //GenericEntity newEntity = new GenericEntity();
        GenericEntity newEntity = newValue();
        newEntity.init(this);

        newEntity.setDelegator(internalDelegator);
        return newEntity;
    }

    /** Returns a new instance of GenericEntity or subclass for clone() and other purposes. */
    protected GenericEntity newValue() {
        return new GenericEntity();
    }

    // ---- Methods added to implement the Map interface: ----

    public Object remove(Object key) {
        return this.fields.remove(key);
    }

    public boolean containsKey(Object key) {
        return this.fields.containsKey(key);
    }

    public java.util.Set<Map.Entry<String, Object>> entrySet() {
        return Collections.unmodifiableMap(this.fields).entrySet();
    }

    public Object put(String key, Object value) {
        return this.set(key, value, true);
    }

    public void putAll(java.util.Map<? extends String, ? extends Object> map) {
        this.setFields(map);
    }

    public void clear() {
        this.fields.clear();
    }

    public Object get(Object key) {
        return this.get((String) key);
    }

    public java.util.Set<String> keySet() {
        return Collections.unmodifiableSet(this.fields.keySet());
    }

    public boolean isEmpty() {
        return this.fields.isEmpty();
    }

    public java.util.Collection<Object> values() {
        return Collections.unmodifiableMap(this.fields).values();
    }

    public boolean containsValue(Object value) {
        return this.fields.containsValue(value);
    }

    public int size() {
        return this.fields.size();
    }

    public boolean matches(EntityCondition condition) {
        return condition.entityMatches(this);
    }

    public void addObserver(Observer observer) {
        getObservable().addObserver(observer);
    }

    public void clearChanged() {
        getObservable().clearChanged();
    }

    public void deleteObserver(Observer observer) {
        getObservable().deleteObserver(observer);
    }

    public void deleteObservers() {
        getObservable().deleteObservers();
    }

    public boolean hasChanged() {
        return getObservable().hasChanged();
    }

    public void notifyObservers() {
        getObservable().notifyObservers();
    }

    public void notifyObservers(Object arg) {
        getObservable().notifyObservers(arg);
    }

    public void setChanged() {
        getObservable().setChanged();
    }

    public boolean originalDbValuesAvailable() {
        return this.originalDbValues != null ? true : false;
    }

    public Object getOriginalDbValue(String name) {
        if (getModelEntity().getField(name) == null) {
            // SCIPIO: 2018-09-29: Throw more helpful EntityFieldNotFoundException instead
            //throw new IllegalArgumentException("[GenericEntity.get] \"" + name + "\" is not a field of " + getEntityName());
            throw new EntityFieldNotFoundException("[GenericEntity.get] \"" + name + "\" is not a field of " + getEntityName());
        }
        if (originalDbValues == null) {
            return null;
        }
        return originalDbValues.get(name);
    }

    /** Returns a GenericEntity copy containing only the selected fields (SCIPIO). NOTE: Currently Observable is not preserved. */
    public GenericEntity select(Collection<String> fields) {
        GenericEntity entity = newValue();
        entity.init(this, fields, true); // TODO: REVIEW: using newValue==true for now to prevent unexpected Observer issues
        return entity;
    }

    /** Returns a GenericEntity copy containing only the selected fields (SCIPIO). NOTE: Currently Observable is not preserved. */
    public GenericEntity select(String... fields) {
        return select(Arrays.asList(fields));
    }

    /**
     * Checks to see if all foreign key records exist in the database. Will create a dummy value for
     * those missing when specified.
     *
     * @param insertDummy Create a dummy record using the provided fields
     * @return true if all FKs exist (or when all missing are created)
     * @throws GenericEntityException
     */
    public boolean checkFks(boolean insertDummy) throws GenericEntityException {
        ModelEntity model = this.getModelEntity();
        Iterator<ModelRelation> relItr = model.getRelationsIterator();
        while (relItr.hasNext()) {
            ModelRelation relation = relItr.next();
            if ("one".equalsIgnoreCase(relation.getType())) {
                // see if the related value exists
                Map<String, Object> fields = new HashMap<>();
                for (ModelKeyMap keyMap : relation.getKeyMaps()) {
                    fields.put(keyMap.getRelFieldName(), this.get(keyMap.getFieldName()));
                }
                EntityFieldMap ecl = EntityCondition.makeCondition(fields);
                long count = this.getDelegator().findCountByCondition(relation.getRelEntityName(), ecl, null, null);
                if (count == 0) {
                    if (insertDummy) {
                        // create the new related value (dummy)
                        GenericValue newValue = this.getDelegator().makeValue(relation.getRelEntityName());
                        boolean allFieldsSet = true;
                        for (ModelKeyMap mkm : relation.getKeyMaps()) {
                            if (this.get(mkm.getFieldName()) != null) {
                                newValue.set(mkm.getRelFieldName(), this.get(mkm.getFieldName()));
                                if (Debug.infoOn()) {
                                    Debug.logInfo("Set [" + mkm.getRelFieldName() + "] to - " + this.get(mkm.getFieldName()), module);
                                }
                            } else {
                                allFieldsSet = false;
                            }
                        }
                        if (allFieldsSet) {
                            if (Debug.infoOn()) {
                                Debug.logInfo("Creating place holder value : " + newValue, module);
                            }

                            // inherit create and update times from this value in order to make this not seem like new/fresh data
                            newValue.put(ModelEntity.CREATE_STAMP_FIELD, this.get(ModelEntity.CREATE_STAMP_FIELD));
                            newValue.put(ModelEntity.CREATE_STAMP_TX_FIELD, this.get(ModelEntity.CREATE_STAMP_TX_FIELD));
                            newValue.put(ModelEntity.STAMP_FIELD, this.get(ModelEntity.STAMP_FIELD));
                            newValue.put(ModelEntity.STAMP_TX_FIELD, this.get(ModelEntity.STAMP_TX_FIELD));
                            // set isFromEntitySync so that create/update stamp fields set above will be preserved
                            newValue.setIsFromEntitySync(true);
                            // check the FKs for the newly created entity
                            newValue.checkFks(true);
                            newValue.create();
                        }
                    } else {
                        return false;
                    }
                }
            }
        }
        return true;
    }

    /** Copies all fields preserving the field order defined by the entity (SCIPIO). If includeNull null, only copies nulls if containsKey. */
    public <M extends Map<String, Object>> M copyAllFields(M out, Boolean includeNull, Collection<String> exclude) {
        return copyFields(out, getModelEntity().getAllFieldNames(), includeNull, exclude);
    }

    /** Copies all fields preserving the field order defined by the entity (SCIPIO). If includeNull null, only copies nulls if containsKey. */
    public <M extends Map<String, Object>> M copyAllFields(M out, Boolean includeNull) {
        return copyFields(out, getModelEntity().getAllFieldNames(), includeNull, null);
    }

    /** Copies pk fields preserving the field order defined by the entity (SCIPIO). If includeNull null, only copies nulls if containsKey. */
    public <M extends Map<String, Object>> M copyPkFields(M out, Boolean includeNull, Collection<String> exclude) {
        return copyFields(out, getModelEntity().getPkFieldNames(), includeNull, exclude);
    }

    /** Copies pk fields preserving the field order defined by the entity (SCIPIO). If includeNull null, only copies nulls if containsKey. */
    public <M extends Map<String, Object>> M copyPkFields(M out, Boolean includeNull) {
        return copyFields(out, getModelEntity().getPkFieldNames(), includeNull, null);
    }

    /** Copies non-pk fields preserving the field order defined by the entity (SCIPIO). If includeNull null, only copies nulls if containsKey. */
    public <M extends Map<String, Object>> M copyNonPkFields(M out, Boolean includeNull, Collection<String> exclude) {
        return copyFields(out, getModelEntity().getNoPkFieldNames(), includeNull, exclude);
    }

    /** Copies non-pk fields preserving the field order defined by the entity (SCIPIO). If includeNull null, only copies nulls if containsKey. */
    public <M extends Map<String, Object>> M copyNonPkFields(M out, Boolean includeNull) {
        return copyFields(out, getModelEntity().getNoPkFieldNames(), includeNull, null);
    }

    /** Copies specified fields preserving the field order defined by the entity (SCIPIO). If includeNull null, only copies nulls if containsKey. */
    public <M extends Map<String, Object>> M copyFields(M out, Collection<String> fieldNames, Boolean includeNull, Collection<String> exclude) {
        if (exclude == null) {
            exclude = Collections.emptySet();
        }
        for (String fieldName : fieldNames) {
            Object value = fields.get(fieldName);
            if ((Boolean.TRUE.equals(includeNull) || value != null || (includeNull == null && fields.containsKey(fieldName))) && !exclude.contains(fieldName)) {
                out.put(fieldName, value);
            }
        }
        return out;
    }

    /** Copies specified fields preserving the field order defined by the entity (SCIPIO). If includeNull null, only copies nulls if containsKey. */
    public <M extends Map<String, Object>> M copyFields(M out, Collection<String> fieldNames, Boolean includeNull) {
        return copyFields(out, fieldNames, includeNull, null);
    }

    public static interface NULL {
    }

    public static class NullGenericEntity extends GenericEntity implements NULL {
        protected NullGenericEntity() {
            this.setImmutable();
        }

        @Override
        public String getEntityName() {
            return "[null-entity]";
        }
        @Override
        public String toString() {
            return "[null-entity]";
        }
    }

    public static class NullField implements NULL, Comparable<NullField> {
        protected NullField() { }

        @Override
        public String toString() {
            return "[null-field]";
        }

        @Override
        public int hashCode() {
            return 42;
        }

        @Override
        public boolean equals(Object o) {
            return this == o;
        }

        public int compareTo(NullField other) {
            return equals(other) ? 0 : -1;
        }
    }
}
