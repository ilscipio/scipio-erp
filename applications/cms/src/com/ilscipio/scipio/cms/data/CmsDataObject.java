package com.ilscipio.scipio.cms.data;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.UtilMisc;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.GenericEntityException;
import org.ofbiz.entity.GenericValue;
import org.ofbiz.entity.condition.EntityCondition;
import org.ofbiz.entity.model.ModelEntity;

import com.ilscipio.scipio.cms.CmsException;
import com.ilscipio.scipio.cms.CmsNameException;
import com.ilscipio.scipio.cms.CmsUtil;

/**
 * A CmsObject subclass that represents an entity and encapsulates a
 * GenericValue object. The name and package given in the XML entity descriptor
 * needs to match the package and class name of the class wrapping it.
 */
public abstract class CmsDataObject extends CmsObject implements CmsEntityReadable {

    private static final long serialVersionUID = -5083909231662111225L;

    public static final String module = CmsDataObject.class.getName();

    /*
     * This is the default "active" bool value when no flag specified in value.
     * <p>
     * (The default value specified in new records is recorded elsewhere)
     */
    public static final boolean MAPPING_ACTIVE_DEFAULT = false;
    
    protected GenericValue entity;
    // IMPORTANT: these flags help prevent endless loops when used in conjunction with PreloadWorker
    protected boolean preloaded = false; 
    protected boolean mutable = true; // 2016-12: NOTE: CANNOT rely on entity.mutable anymore, because of the GenericValue-accepting constructor
    
    /**
     * Makes data object from already-looked-up entity, used as given.
     */
    protected CmsDataObject(GenericValue entity) {
        if (CmsUtil.verboseOn()) {
            if (entity == null) {
                throw new IllegalArgumentException("Null entity in " + this.getEntityName() + " data object constructor");
            }
            if (!entity.getEntityName().equals(this.getEntityName())) {
                throw new IllegalArgumentException("The passed GenericValue is not of the right entity "
                        + "type. Expected: " + this.getClass().getName() + ", provided: " + entity.getEntityName());
            }
        }
        this.entity = entity;
    }

    /**
     * Creates NEW data object, NOT persisted.
     */
    public CmsDataObject(Delegator delegator, Map<String, ?> fields) {
        // 2017-04-11: ALWAYS copy the fields so the verification can now modify them in-place
        Map<String, Object> flds = new HashMap<>(fields);
        verifyNewFields(delegator, flds, true);
        this.entity = makeValue(delegator, this.getEntityName(), flds);
    }
    
    /**
     * Copy constructor.
     */
    public CmsDataObject(CmsDataObject other, Map<String, Object> copyArgs) {
        this.entity = copyValue(other.getEntity());
        
        if (this.entity.getModelEntity().isField("createdBy")) {
            // Store the name of the person who created the copy (which is NOT the same as the person who
            // created the original version)
            String copyCreatorId = (String) copyArgs.get("copyCreatorId");
            if (UtilValidate.isNotEmpty(copyCreatorId)) {
                this.entity.set("createdBy", copyCreatorId);
            }
        }
    }
    
    /**
     * UPDATES data object, NOT persisted. setIfEmpty should usually be true.
     * <p>
     * DEV WARNING: 2017-11: ONLY setIfEmpty=true IS PROPERLY TESTED EVERYWHERE.
     * setIfEmpty=false IS ONLY FOR SPECIAL CASES AND CONVENIENCE CASES!
     * THE CORE LOGIC IS ALWAYS setIfEmpty=true.
     * USING setIfEmpty=false IN WRONG PLACES CAN CAUSE EXTREMELY DIFFICULT
     * ERRORS TO TRACK DOWN.
     * <p>
     * Analogous to the {@link #CmsTemplate(Delegator, Map)} constructor
     * that performs the create operation.
     * @param setIfEmpty if true, updates field as long as the key is present (containsKey);
     *                   if false, key must be present and value non-empty for update (isNotEmpty)
     */
    public void update(Map<String, ?> fields, boolean setIfEmpty) {
     // 2017-04-11: ALWAYS copy the fields so the verification can now modify them in-place
        Map<String, Object> flds = new HashMap<>(fields);
        verifyNewFields(getDelegator(), flds, false);
        entity.setNonPKFields(flds, setIfEmpty); // NOTE: setIfEmpty must be TRUE
    }
    
    /**
     * UPDATES data object, NOT persisted. ALL fields present keys are overridden even 
     * if null or empty.
     */
    public final void update(Map<String, ?> fields) {
        update(fields, true);
    }

    /**
     * Invokes the copy constructor to make an IN-MEMORY copy, not committed until
     * {@link CmsDataObject#store} is called.
     * <p>
     * The copy options are not standardized, sometimes they are simple field names.
     */
    public CmsDataObject copy(Map<String, Object> copyArgs) throws CmsException {
        throw new UnsupportedOperationException();
        // old code (reflection removed):
        //return getWorkerInst().makeCopy(this, copyArgs);
//        GenericValue valueCopy = getDelegator().makeValue(
//                entity.getEntityName());
//        
//        valueCopy.setNonPKFields(entity.getAllFields());
//        // no such thing in current schema
//        //if (valueCopy.getModelEntity().isField("uuid")) {
//        //    valueCopy.set("uuid", UUID.randomUUID().toString());
//        //} 
//        
//        CmsDataObject doCopy = null;        
//        try {
//            getDelegator().createSetNextSeqId(valueCopy);
//            doCopy = this.getClass().getConstructor(GenericValue.class)
//                    .newInstance(valueCopy);
//        } catch (Exception e) {
//            throw new CmsException(
//                    "Could not create copy of data object. Entity name: "
//                            + entity.getEntityName(), e);
//        }
//        return doCopy;
    }
    
    /**
     * 2016: Loads ALL this object's content into the current instance.
     * <p>
     * WARN: IMPORTANT: AFTER THIS CALL, 
     * NO FURTHER CALLS ARE ALLOWED TO MODIFY THE INSTANCE IN MEMORY.
     * Essential for thread safety!!!
     */
    @Override
    public void preload(PreloadWorker preloadWorker) {
        super.preload(preloadWorker);
        
        this.preloaded = true;
        if (preloadWorker.isImmutable()) {
            this.mutable = false;
        }
        
        // make the entity immutable and anything else required for it
        preloadWorker.preloadEntity(this.entity);

        if (CmsUtil.verboseOn()) {
            Debug.logInfo("Cms: Running preload for " + getEntityName() + " with id '" + getId() + "'", module);
        }
    }
    
    @Override
    public boolean isPreloaded() {
        return this.preloaded;
    }
    
    @Override
    public boolean isImmutable() {
        // CANNOT rely on the entity's flag, must use our own
        //return !entity.isMutable();
        return !this.mutable;
    }
    
    @Override
    public Delegator getDelegator() {
        return entity.getDelegator();
    }
    
    public String getEntityName() {
        return getEntityName(this.getClass());
    }
    
    public static String getEntityName(Class<? extends CmsDataObject> dataObjectClass) {
        return dataObjectClass.getSimpleName();
    }
    

    /**
     * Returns the GenericValue entity encapsulated by this data object.
     * 
     * @return entity object
     */
    public GenericValue getEntity() {
        return entity;
    }
    
    public boolean hasId() {
        return getId() != null;
    }
    
    /**
     * Returns true if entity was modified.
     */
    public boolean hasChanged() {
        return entity.hasChanged();
    }
    
    public boolean hasChangedOrNoId() {
        return hasChanged() || !hasId();
    }
    
    public boolean isEntityPersisted() {
        if (!entity.containsPrimaryKey()) {
            return false;
        }
        try {
            // NOTE: this check is what delegator does in createOrStore
            GenericValue checkValue = getDelegator().findOne(entity.getEntityName(), entity.getPrimaryKey(), false);
            return checkValue != null;
        } catch (GenericEntityException e) {
            throw new CmsDataException("Failed to check if entity " + getEntityName() + " with id '" + getId() + "' is persisted");
        }
    }

    /**
     * Sets the entity this data object encapsulates.
     * 2016: package-private; do not allow this in general.
     */
    void setEntity(GenericValue entity) {
        this.entity = entity;
    }

    /**
     * Optional callback upon create or update; throws exception if anything bad.
     */
    protected void verifyNewFields(Delegator delegator, Map<String, Object> fields, boolean isNew) throws CmsException {
    }
    
    protected void verifyNamePresent(Delegator delegator, Map<String, ?> fields, boolean isNew, String nameField) throws CmsException {
        if ((isNew || fields.containsKey(nameField)) && UtilValidate.isEmpty((String) fields.get(nameField))) {
            throw new CmsNameException("Missing name (" + nameField + ")"); // TODO: localize
        }
    }
    
    protected void verifyGroupingPresent(Delegator delegator, Map<String, ?> fields, boolean isNew, String groupingField) throws CmsException {
        if ((isNew || fields.containsKey(groupingField)) && UtilValidate.isEmpty((String) fields.get(groupingField))) {
            throw new CmsNameException("Missing " + groupingField); // TODO: localize
        }
    }
    
    /**
     * Reusable name uniqueness verification logic.
     * <p>
     * If groupingNullSignificant is true, it means a value of null for the grouping field is treated 
     * as if it was any other grouping value, and it's always part of the lookup fields.
     * If groupingNullSignificant is false, it means when grouping value is null, it is excluded from
     * lookup fields, so the method will then deny a lookup for the name if there is any record
     * having the name, regardless of its grouping value.
     * <p>
     * groupingNullSignificant is implied/forced to be true if groupingOptional is false.
     */
    protected void verifyUniqueName(Delegator delegator, Map<String, ?> fields, boolean isNew, 
            String nameField, boolean nameOptional, String groupingField, boolean groupingOptional, boolean groupingNullSignificant) throws CmsException {
        if (!groupingOptional) {
            groupingNullSignificant = true;
        }
        
        // check for basic name presence
        if (nameOptional) {
            if (UtilValidate.isEmpty((String) fields.get(nameField))) {
                return;
            }
        } else {
            verifyNamePresent(delegator, fields, isNew, nameField);
            // if we're not updating the name or the grouping field, we can stop now
            if (!isNew && (!fields.containsKey(nameField) && (groupingField == null || !fields.containsKey(groupingField)))) {
                return;
            }
        }
        
        if (groupingOptional) {
            ;
        } else {
            verifyGroupingPresent(delegator, fields, isNew, groupingField);
        }
        
        Map<String, Object> lookupFields = new HashMap<>();
        String nameVal;
        if (fields.containsKey(nameField) || isNew) {
            nameVal = (String) fields.get(nameField);
        } else {
            nameVal = this.getEntity().getString(nameField);
        }
        if (UtilValidate.isEmpty(nameVal)) {
            nameVal = null;
        }
        
        lookupFields.put(nameField, nameVal);
        
        String groupVal;
        if (groupingField == null) {
            groupVal = null;
        } else {
            if (fields.containsKey(groupingField) || isNew) {
                groupVal = (String) fields.get(groupingField);
            } else {
                groupVal = this.getEntity().getString(groupingField);
            }
            if (UtilValidate.isEmpty(groupVal)) {
                groupVal = null;
            }
            // THE WAY THIS WORKS: when grouping null NOT significant, if the group is not set, we only allow the name if there are no
            // other records having the name, whether they have group or not 
            // (instead of only checking for records that have null group)
            // when grouping null is significant, it acts exactly like a groupVal of its own,
            // so it is always part of the lookup fields.
            if (groupingNullSignificant || groupVal != null) {
                lookupFields.put(groupingField, groupVal);
            }
        }
        
        List<GenericValue> existingList;
        try {
            existingList = delegator.findByAnd(getEntityName(), lookupFields, null, false);
        } catch (GenericEntityException e) {
            throw new CmsException(e);
        }
        if (existingList.size() >= 1) {
            GenericValue existing = existingList.get(0);
            if (existingList.size() >= 2 || isNew || !(this.getId().equals(getId(existing)))) {
                if (groupingField != null) {
                    // TODO: localize
                    throw new CmsNameException("Name (" + nameField + ") already in use: " + nameVal + " with " + groupingField + ": " + existing.getString(groupingField));
                } else {
                    throw new CmsNameException("Name (" + nameField + ") already in use: " + nameVal);
                }
            }
        }
    }
    
    public static void trimFields(Map<String, ? super String> fields, String... fieldNames) {
        for(String name : fieldNames) {
            if (fields.containsKey(name)) {
                String value = (String) fields.get(name);
                if (value != null) {
                    value = value.trim();
                    if (value.isEmpty()) {
                        value = null;
                    }
                }
                fields.put(name, value);
            }
        }
    }
    
    // Common fields
    // Note: Not all types will support all of these; simply don't use those not supported
    // (bad interface, but ignore since these classes meant for internal use only)
    
    /**
     * Returns the identifier for this entity.
     * 
     * @return Id
     */
    public String getId() {
        return getId(entity);
    }
    
    public static String getId(GenericValue entity) {
        // 2016: special case: don't use the combo if avoidable because it may return string "null"
        if (entity.getModelEntity().getPkFields().size() == 1) {
            return entity.getString(entity.getModelEntity().getPkFields().get(0).getName());
        } else {
            String res = entity.getPkShortValueString();
            if (UtilValidate.isEmpty(res) || "null".equals(res)) {
                return null;
            } else {
                return res;
            }
        }
    }
    
//    public String getUuid() {
//        if (this.entity != null && this.entity.getModelEntity().isField("uuid")) {
//          return this.entity.getString("uuid");
//        } else {
//          return null;  
//        }        
//    }

    
    /**
     * This checks if the field is non-empty and returns it, or if not, then it looks
     * into localized defaults.
     */
    public static String getStoredOrLocalizedField(GenericValue entity, String fieldName, Locale locale) {
        String value = entity.getString(fieldName);
        if (UtilValidate.isNotEmpty(value)) {
            return value;
        } else {
            return (String) entity.get(fieldName, locale);
        }
    }
    
    /**
     * This checks if the field is localized and if so returns the localization, or if not,
     * returns the stored field.
     */
    public static String getLocalizedOrStoredField(GenericValue entity, String fieldName, Locale locale) {
        return (String) entity.get(fieldName, locale);
    }
    
    
    // Operations
    

    /**
     * Creates or stores the current entity. If no PK is set, creates one.
     * <p>
     * The calling code must be aware that the PK is a determining factor of how this behaves, and here
     * the PK should never be generated with setNextSeqId beforehand if we are trying to create
     * a new value. Or in other words the combination of Delegator.setNextSeqId+Delegator.createOrStore
     * is a huge no-no, at least for the Ofbiz version this was running on today.
     * For us, just avoid setNextSeqId entirely - the value it creates may already be out of date anyway.
     */
    protected void createWithNewIdIfNoneOrStore() throws CmsException {
        GenericValue value;
        Delegator d = getDelegator();
        
        if (entity.containsPrimaryKey()) {
            try {
                value = d.createOrStore(entity);
            } catch(GenericEntityException e) {
                throw new CmsDataException("Entity " + entity.getEntityName() 
                    + " could not be stored (ID: " + entity.getPkShortValueString() + "): " + e.getMessage(), e);
            }
        } else {
            try {
                value = d.createSetNextSeqId(entity);
            } catch(GenericEntityException e) {
                throw new CmsDataException("Entity " + entity.getEntityName() 
                    + " could not be created (with new ID): " + e.getMessage(), e);
            }    
        }

        if (value != null) {
            this.entity = value;
        }
    }

    /**
     * Creates or stores the current entity. If no PK is set, creates one.
     * <p>
     * The calling code must be aware that the PK is a determining factor of how this behaves, and here
     * the PK should never be generated with setNextSeqId beforehand if we are trying to create
     * a new value. Or in other words the combination of Delegator.setNextSeqId+Delegator.createOrStore
     * is a huge no-no, at least for the Ofbiz version this was running on today.
     * For us, just avoid setNextSeqId entirely - the value it creates may already be out of date anyway.
     * @return true for successful store, otherwise false (NOTE: 2016: don't use this bool, meaningless)
     */
    public void store() throws CmsException {
        createWithNewIdIfNoneOrStore();
    }
    
    /**
     * Removes the entity from the database.
     * 
     * @return number of rows affected (in any entity)
     */
    public int remove() throws CmsException {
        int rowsAffected = 0;
        try {
            rowsAffected += getDelegator().removeValue(entity);
        } catch (GenericEntityException e) {
            throw makeRemoveException(e);
        }
        return rowsAffected;
    }
    
    protected CmsException makeRemoveException(Exception e) {
        return new CmsDataException(getEntityName() + " could not be removed "
                + "(are there still other records that depend on this one?): " + e.getMessage()); // TODO: localize
    }

    public static int removeAll(Collection<? extends CmsDataObject> dataObjects) throws CmsException {
        int rowsAffected = 0;
        
        if (dataObjects != null) {
            for(CmsDataObject dataObject : dataObjects) {
                rowsAffected += dataObject.remove();
            }
        }
        
        return rowsAffected;
    }
    
    /**
     * Returns the last modification of the entity record as date.
     * 
     * @return modification date
     */
    public Date getLastModified() {
        return entity.getTimestamp("createdStamp");
    }

    public void setFields(Map<String, ? extends Object> fields) {
        entity.setFields(fields);
    }
    
    /**
     * Creates an empty value of an entity with the given fields preset.
     */
    protected static GenericValue makeValue(Delegator delegator, String entityName, Map<String, ?> fields) {
        GenericValue gv;
        if (fields != null) {
            gv = delegator.makeValidValue(entityName, fields);
        } else {
            gv = delegator.makeValue(entityName);  
        }
        // NEVER do this - always use createSetNextSeqId
        //gv.setNextSeqId();
        return gv;
    }
    
    /**
     * Copy value, without PK.
     */
    protected static GenericValue copyValue(GenericValue otherEntity) {
        GenericValue gv = otherEntity.getDelegator().makeValue(otherEntity.getEntityName());
        gv.setNonPKFields(otherEntity);
        return gv;
    }

    public void fieldsToMap(Map<String, Object> out) {
        out.putAll(entity);
    }
    
    public static boolean nonEmptyOrDefault(Boolean val, boolean defaultVal) {
        return val != null ? val : defaultVal;
    }
    
    public static <T> T nonEmptyOrDefault(T val, T defaultVal) {
        return UtilValidate.isNotEmpty(val) ? val : defaultVal;
    }
    
    public static boolean isActiveLogical(Boolean active) {
        return (active != null) ? active : MAPPING_ACTIVE_DEFAULT;
    }
    
    public static <T extends CmsDataObject> Map<String, T> makeIdDataObjectMap(Iterable<? extends T> dataObjects) {
        Map<String, T> map = new HashMap<>();
        if (dataObjects != null) {
            for(T dataObject : dataObjects) {
                map.put(dataObject.getId(), dataObject);
            }
        }
        return map;
    }
    
    public static List<GenericValue> getEntityValues(Collection<? extends CmsDataObject> dataObjects) {
        List<GenericValue> res = new ArrayList<>(dataObjects.size());
        for(CmsDataObject dataObj : dataObjects) {
            res.add(dataObj.getEntity());
        }
        return res;
    }
    
    /**
     * Returns a type-appropriate DataObjectWorker for this instance.
     * <p>
     * Each concrete subclass should override this method to provide a specific
     * factory type. This default implementation is less safe and constructs needlessly.
     */
    public abstract DataObjectWorker<?> getWorkerInst();
    
//    /**
//     * Returns a rudimentary DataObjectWorker for the given class, implemented using reflection
//     * and older-style code.
//     * @deprecated This is a <em>compatibility</em> and <em>transition</em> method only!
//     * CmsDataObject classes should extend DataObjectWorker with a non-reflective 
//     * implementation which then gets returned by {@link CmsDataObject#getWorkerInst()} as well
//     * as by a zero-parameter <code>getWorker()</code> method in its top file.
//     */
//    @Deprecated
//    public static <T extends CmsDataObject> DataObjectWorker<T> getWorker(final Class<T> dataObjectClass) {
//        return new ReflexiveDataObjectWorker<T>(dataObjectClass);
//    }
    
    
    
    /**
     * Data Object Factory and delegator-like worker, where each subclass represents a 
     * single data object type (primarily, though operations may involve related).
     * <p>
     * 2016: Every CmsDataObject subclass should be getting one of these at some point.
     * <p>
     * In addition, these can be passed to find operations and operations in general
     * to allow subclassing behavior in very specific circumstances.
     * <p>
     * TODO: 2016: ALL OF THE STATIC FIND OPERATIONS IN THE CLASS ABOVE
     * can be moved into this worker...
     */
    public static abstract class DataObjectWorker<T extends CmsDataObject> extends ObjectWorker<T> {
        
        // TODO: REVIEW: I have no idea if syncObj is still needed, but this is easy, so just do it...
        // this simply ensures each Class<?> will get a single distinct object globally (through worker)
        protected static final Map<Class<?>, Object> dataObjectClassSyncObjMap = new ConcurrentHashMap<>();
        
        protected final Object syncObj;
        protected final Class<T> dataObjectClass;
        
        protected DataObjectWorker(Class<T> dataObjectClass) {
            this.dataObjectClass = dataObjectClass;
            dataObjectClassSyncObjMap.putIfAbsent(dataObjectClass, new Object());
            this.syncObj = dataObjectClassSyncObjMap.get(dataObjectClass);
        }
  
        /*
         * Basic type and data model info methods
         */
        
        public Class<T> getDataObjectClass() {
            return dataObjectClass;
        }
        
        public String getEntityName() {
            return getDataObjectClass().getSimpleName();
        }
        
        public ModelEntity getModelEntity(Delegator delegator) {
            return delegator.getModelEntity(getEntityName());
        }
        
        /**
         * Returns candidate PK field names, which may or may not have any kind of 
         * actual constraints in the entity schema. The default implementation simply
         * returns the official entity PK.
         */
        public List<String> getLogicalPkFieldNames(Delegator delegator) {
            return getModelEntity(delegator).getPkFieldNames();
        }
        
        /**
         * Returns names of candidate PK fields that (despite terminology) may accept
         * empty values.
         * <p>
         * Must be a subset of {@link #getLogicalPkFieldNames()}.
         */
        public List<String> getLogicalPkFieldsAllowedEmptyNames(Delegator delegator) {
            return Collections.emptyList();
        }

        /**
         * Returns an application-level sync object that <em>may</em> (?) currently be used 
         * for data locking, to enforce the logical (candidate) keys for our mapping entities, 
         * for entities that used constraint-less PK IDs rather than traditional constraints.
         * @deprecated insufficient solution for complex deployments. 
         * TODO?: 2016: Investigate need and possibilities for database (row-level?) locking
         * instead of this; should be removed entirely at some point...
         */
        @Deprecated
        public Object getDataObjectOpsSyncObject() {
            return syncObj;
        }
        
        
        /*
         * Core factory methods
         */

        /**
         * Central, primary factory method: returns a data object instance from a GenericValue.
         * <p>
         * Likely to be called during find operations
         * <p>
         * NOTE: 2016: this should slowly start taking the place of the old Reflection API calls.
         */
        public abstract T makeFromValue(GenericValue value) throws CmsException;
        
        public abstract T makeFromFields(Delegator delegator, Map<String, ?> fields) throws CmsException;

        // this turns out to be inferior to simply having copy() on the instance due to typing.
//        /**
//         * Invokes the copy constructor to make an IN-MEMORY copy, not committed until
//         * {@link CmsDataObject#store} is called.
//         * <p>
//         * The copy options are not standardized, sometimes they are simple field names.
//         */
//        public abstract T makeCopy(T other, Map<String, Object> copyArgs) throws CmsException;
        
        /*
         * Find operations.
         * 
         * NOTE: most of these will produce data object instances using 
         * the method makeFromValue(Generic) above.
         */

        public T findById(Delegator delegator, String id, boolean useCache) throws CmsException {
            return findOne(delegator, UtilMisc.toMap(getModelEntity(delegator).getFirstPkFieldName(), id), useCache);
        }
        
        public T findByIdAlways(Delegator delegator, String id, boolean useCache) throws CmsException {
            T result = findById(delegator, id, useCache);
            if (result == null) {
                throw new CmsException("No value found for entity " + getEntityName() + " using primary key '" + id + "'");
            }
            return result;
        }


        /**
         * Finds all entities matching a given set of fields and wraps them in data
         * objects of the given class.
         * 
         * @return List of data objects of the given class
         * @throws CmsException 
         */
        public List<T> findAll(Delegator delegator, Map<String, ?> fields, List<String> orderBy, boolean cache) throws CmsException {
            final String entityName = getEntityName();
            List<GenericValue> genericValues;
            try {
                genericValues = delegator.findByAnd(entityName, fields, orderBy, isUseDbCacheStatic(cache));
            } catch (GenericEntityException e) {
                throw new CmsException("Entity could not be found. Entity: "
                        + entityName + " Fields: "
                        + fields.toString(), e);
            }
            List<T> dataObjects = new ArrayList<>(genericValues.size());
            for (GenericValue gv : genericValues) {
                try {
                    dataObjects.add(makeFromValue(gv));
                } catch (Exception e) {
                    Debug.logError(e, "Internal error: Error creating memory instance for entity " + entityName, module);
                }
            }
            return dataObjects;
        }
        
        /**
         * Finds all entities matching a given set of conditions and wraps them in data
         * objects of the given class.
         * 
         * @return List of data objects of the given class
         * @throws CmsException 
         */
        public List<T> findAll(Delegator delegator, EntityCondition whereCondition, List<String> orderBy, boolean cache) throws CmsException {
            final String entityName = getEntityName();
            List<GenericValue> genericValues;
            try {
                genericValues = delegator.findList(entityName, whereCondition, null, orderBy, null, isUseDbCacheStatic(cache));
            } catch (GenericEntityException e) {
                throw new CmsException("Entity could not be found. Entity: "
                        + entityName + " Condition: "
                        + (whereCondition != null ? whereCondition.toString() : "(none)"), e);
            }
            List<T> dataObjects = new ArrayList<>(genericValues.size());
            for (GenericValue gv : genericValues) {
                try {
                    dataObjects.add(makeFromValue(gv));
                } catch (Exception e) {
                    Debug.logError(e, "Internal error: Error creating memory instance for entity " + entityName, module);
                }
            }
            return dataObjects;
        }  
        
        /**
         * Finds all entities matching a given set of fields and wraps them in data
         * objects of the given class.
         * <p>
         * 2016: NOTE: I removed a webSiteId check that seems like it doesn't belong here, and
         * changed the lone caller instead.
         * 
         * @param dataObjectClass
         * @return List of data objects of the given class
         * @throws CmsException 
         */
        public List<T> findAll(Delegator delegator, List<String> orderBy, boolean cache) throws CmsException {
            final String entityName = getEntityName();
            List<GenericValue> genericValues;
            try {
                // caller should handle this:
                // EntityCondition.makeCondition("webSiteId", EntityOperator.NOT_EQUAL, null)
                genericValues = delegator.findList(entityName, null, 
                        null, orderBy, null, isUseDbCacheStatic(cache));
            } catch (GenericEntityException e) {
                throw new CmsException("Entity could not be found. Entity: "
                        + entityName + " Fields: ", e);
            }
            List<T> dataObjects = new ArrayList<>(genericValues.size());
            for (GenericValue gv : genericValues) {
                try {
                    dataObjects.add(makeFromValue(gv));
                } catch (Exception e) {
                    Debug.logError(e, "Cms: Error creating memory instance for entity " + entityName, module);
                }
            }
            return dataObjects;
        }

        public List<T> findAll(Delegator delegator, boolean cache) throws CmsException {
            return findAll(delegator, null, cache);
        }
        
        /**
         * This is to help find/detect DB errors (important since not enforced in schema).
         */
        public static enum SingleFindMode {
            FIRST_OF_MANY, // Default
            CANDIDATE_KEY_PERMISSIVE, // Log warning if many found
            CANDIDATE_KEY_STRICT // Exception if many found
        }
        
        public T findFirst(Delegator delegator, Map<String, ?> fields, boolean cache) throws CmsException {
            return findFirst(delegator, fields, cache, null);
        }
        
        /**
         * Finds the first (and usually only) entity matching the fields and wraps
         * it into a data object of the given class.
         * 
         * @param fields
         * @param dataObjectClass
         * @return Data object of the given class
         * @throws CmsException 
         */
        public T findFirst(Delegator delegator, Map<String, ?> fields, boolean cache, SingleFindMode findMode) throws CmsException {
            final String entityName = getEntityName();
            List<GenericValue> genericValues;
            T dataObject = null;
            try {
                genericValues = delegator.findByAnd(entityName, fields, null, isUseDbCacheStatic(cache));
            } catch (GenericEntityException e) {
                throw new CmsException("Entity could not be found. Entity: "
                        + entityName + " Fields: "
                        + fields.toString(), e);
            }
            if (genericValues.size() > 0) {
                if (genericValues.size() > 1) {
                    if (findMode == SingleFindMode.CANDIDATE_KEY_STRICT) {
                        throw new CmsDataException("Found duplicate candidate key for entity " + entityName +
                                ": Fields: " + fields.toString());
                    } else if (findMode == SingleFindMode.CANDIDATE_KEY_PERMISSIVE) {
                        Debug.logWarning("CMS: Found duplicate candidate key for entity " + entityName +
                                ": Fields: " + fields.toString(), module);
                    }
                }
                
                try {
                    dataObject = makeFromValue(genericValues.get(0));
                } catch (Exception e) {
                    Debug.logError(e, "Internal error: Error creating memory instance for entity " + entityName, module);
                }
            }
            return dataObject;
        }
        
        public T findFirst(Delegator delegator, EntityCondition whereCondition, List<String> orderBy, boolean cache) throws CmsException {
            return findFirst(delegator, whereCondition, orderBy, cache, null);
        }
        
        /**
         * Finds the first (and usually only) entity matching the fields and wraps
         * it into a data object of the given class.
         * 
         * @return Data object of the given class
         * @throws CmsException 
         */
        public T findFirst(Delegator delegator, EntityCondition whereCondition,
                List<String> orderBy, boolean cache, SingleFindMode findMode) throws CmsException {
            final String entityName = getEntityName();
            List<GenericValue> genericValues;
            T dataObject = null;
            try {
                genericValues = delegator.findList(entityName, 
                        whereCondition, null, orderBy, null, isUseDbCacheStatic(cache));
            } catch (GenericEntityException e) {
                throw new CmsException("Entity could not be found. Entity: " + entityName + " Condition: "
                        + (whereCondition != null ? whereCondition.toString() : "(none)"), e);
            }
            if (genericValues.size() > 0) {
                if (genericValues.size() > 1) {
                    if (findMode == SingleFindMode.CANDIDATE_KEY_STRICT) {
                        throw new CmsException("Found duplicate candidate key for entity " + entityName +
                                ": Search condition: " + (whereCondition != null ? whereCondition.toString() : "(none)"));
                    } else if (findMode == SingleFindMode.CANDIDATE_KEY_PERMISSIVE) {
                        Debug.logWarning("CMS: Found duplicate candidate key for entity " + entityName +
                                ": Search condition: " + (whereCondition != null ? whereCondition.toString() : "(none)"), module);
                    }
                }
                
                try {
                    dataObject = makeFromValue(genericValues.get(0));
                } catch (Exception e) {
                    Debug.logError(e, "Internal error: Error creating memory instance for entity " + entityName, module);
                }
            }
            return dataObject;
        }

        public T findOne(Delegator delegator, Map<String, ?> fields, boolean useCache) throws CmsException {
            final String entityName = getEntityName();
            GenericValue genericValue = null;
            T dataObject = null;
            try {
                genericValue = delegator.findOne(entityName, fields, isUseDbCacheStatic(useCache));
            } catch (GenericEntityException e) {
                throw new CmsException("Entity could not be found. Entity: " + entityName + " Fields: "
                        + fields.toString(), e);
            }
            if (genericValue != null) {
                try {
                    dataObject = makeFromValue(genericValue);
                } catch (Exception e) {
                    Debug.logError(e, "Cms: Internal error: Error creating memory instance for entity " + entityName, module);
                }
            }
            return dataObject;
        }
        
        public T findByCandidateKey(Delegator delegator, Map<String, ?> candidateKey, boolean useCache) throws CmsException {
            return findFirst(delegator, candidateKey, useCache, SingleFindMode.CANDIDATE_KEY_PERMISSIVE);
        }
        
        
        /*
         * Modification (create, update and delete) operations.
         * <p>
         * NOTE: 2016: These take place somewhat outside the CmsDataObject class abstractions;
         * however, in the case of delete and even create, it is frequently technically necessary
         * or more appropriate to apply these operations without doing a separate lookup;
         * these provide the next-best abstraction in these cases.
         */
        
        public T createDataObject(Delegator delegator, Map<String, ?> fields) throws CmsException {
            T dataObj = makeFromFields(delegator, fields);
            dataObj.store();
            return dataObj;
        }
        
        public T updateDataObject(Delegator delegator, Map<String, ?> fields) throws CmsException {
            Map<String, Object> pkFields = extractPkFields(delegator, fields);
            T dataObj = findOne(delegator, pkFields, false);
            if (dataObj == null) {
                throw new CmsException("Could not find " + getEntityName() + " entity to update with pk fields: " + pkFields.toString());
            }
            dataObj.update(fields);
            dataObj.store();
            return dataObj;
        }
        
        public T createOrUpdateDataObject(Delegator delegator, Map<String, ?> fields) throws CmsException {
            // depressing trick to check if pk present
            GenericValue tempPkValue = delegator.makeValidValue(getEntityName(), fields);
            if (tempPkValue.containsPrimaryKey()) {
                return updateDataObject(delegator, fields);
            } else {
                return createDataObject(delegator, fields);
            }
        }
        
        public void deleteDataObject(Delegator delegator, Map<String, ?> fields) throws CmsException {
            T dataObj = findOne(delegator, extractPkFields(delegator, fields), false);
            dataObj.remove();
        }
        
        public void deleteDataObject(Delegator delegator, String idField) throws CmsException {
            T dataObj = findById(delegator, idField, false);
            dataObj.remove();
        }
        
        protected Map<String, Object> extractPkFields(Delegator delegator, Map<String, ?> fields) {
            ModelEntity modelEntity = getModelEntity(delegator);
            Map<String, Object> pkFields = new HashMap<>();
            for(String name : modelEntity.getPkFieldNames()) {
                pkFields.put(name, fields.get(name));
            }
            return pkFields;
        }
        
        protected Map<String, Object> hasPkFields(Delegator delegator, Map<String, ?> fields) {
            ModelEntity modelEntity = getModelEntity(delegator);
            Map<String, Object> pkFields = new HashMap<>();
            for(String name : modelEntity.getPkFieldNames()) {
                pkFields.put(name, fields.get(name));
            }
            return pkFields;
        }
        
        
        /*
         * Cache operations
         */
        
        @Override
        public void clearEntityCaches(Delegator delegator) throws CmsException {
            delegator.clearCacheLine(getEntityName());
        }
        
    }
    
//    /**
//     * Old-style reflexive-invoking worker. 
//     * @deprecated limits factory potential and frustrates debugging.
//     */
//    @Deprecated
//    public static class ReflexiveDataObjectWorker<T extends CmsDataObject> extends DataObjectWorker<T> {
//   
//        public ReflexiveDataObjectWorker(Class<T> dataObjectClass) {
//            super(dataObjectClass);
//        }
//
//        @Override
//        public T makeFromValue(GenericValue value) throws CmsException {
//            try {
//                return getDataObjectClass().getConstructor(GenericValue.class).newInstance(value);
//            } catch (Exception e) {
//                throw new CmsException("Error creating memory instance for entity " + getEntityName(), e);
//            }
//        }
//
//        @Override
//        public T makeFromFields(Delegator delegator, Map<String, ?> fields) throws CmsException {
//            try {
//                return getDataObjectClass().getConstructor(Delegator.class, Map.class).newInstance(delegator, fields);
//            } catch (Exception e) {
//                throw new CmsException("Error creating memory instance for entity " + getEntityName(), e);
//            }
//        }
//    }
}
