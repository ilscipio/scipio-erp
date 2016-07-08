package com.ilscipio.scipio.cms.data;

import java.io.Serializable;
import java.util.Date;
import java.util.List;
import java.util.Map;

import javolution.util.FastList;
import javolution.util.FastMap;

import org.ofbiz.base.util.UtilMisc;
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.DelegatorFactory;
import org.ofbiz.entity.GenericEntityException;
import org.ofbiz.entity.GenericValue;
import org.ofbiz.entity.condition.EntityCondition;
import org.ofbiz.entity.condition.EntityOperator;
import org.ofbiz.entity.model.ModelEntity;

import com.ilscipio.scipio.cms.CmsException;

/**
 * A CmsDataObject subclass represents an entity and encapsulates a
 * GenericValue object. The name and package given in the XML entity descriptor
 * needs to match the package and class name of the class wrapping it.
 * 
 */
public abstract class CmsDataObject implements Serializable {

	private static final long serialVersionUID = -463201732423987596L;
    private static Delegator delegator;
    protected GenericValue entity;

	public static final String module = CmsDataObject.class.getName();

	public CmsDataObject(Map<String, ?> fields) {
	        entity = makeValue(this.getClass().getSimpleName(), fields);
	}
	 
    public CmsDataObject(String primaryKey) {
        String entityName = this.getClass().getSimpleName();
        String pkName = getDelegator().getModelEntity(entityName)
                .getPkFieldNames().get(0);
        Map<String, ?> fields = UtilMisc.toMap(pkName, primaryKey);

        CmsDataObject pdo = CmsDataObject.findFirst(fields, this.getClass());

        if (pdo != null) {
            entity = pdo.getEntity();
        } else {
            throw new CmsException(
                    "No entity with the given key could be retrieved. Key: "
                            + primaryKey, module);
        }
    }
  

    public CmsDataObject(GenericValue entity) {
        if (!entity.getEntityName().equals(this.getClass().getSimpleName())) {
            throw new IllegalArgumentException(
                    "The passed GenericValue is not of the right entity type. Expected: "
                            + this.getClass().getName() + " Provided: "
                            + entity.getEntityName());
        }
        this.entity = entity;
    }
    
    /**
     * Returns the identifier for this entity.
     * 
     * @return Id
     */
    public String getId() {
        return entity.getPkShortValueString();
    }
    
    /**
     * Returns the GenericValue entity encapsulated by this data object.
     * 
     * @return entity object
     */
    public GenericValue getEntity() {
        return entity;
    }

    /**
     * Sets the entity this data object encapsulates.
     * 
     * @param entity
     */
    public void setEntity(GenericValue entity) {
        this.entity = entity;
    }

    /**
     * Stores the current entity to the database.
     * 
     * @return true for successful store, otherwise false
     * @throws GenericEntityException
     */
    public boolean store() throws GenericEntityException {
        GenericValue value;
        if (entity.containsPrimaryKey()) {
            value = getDelegator().createOrStore(entity);
        } else {
            value = getDelegator().createSetNextSeqId(entity);
        }

        if (value != null) {
            this.entity = value;
            return true;
        } else {
            return false;
        }
    }

    /**
     * Removes the entity from the database.
     * 
     * @return True if delete was successful, otherwise false
     * @throws GenericEntityException
     */
    public boolean remove() {
        int rowsAffected = 0;
        try {
            rowsAffected = getDelegator().removeValue(entity);
        } catch (GenericEntityException e) {
            throw new CmsException("Entity could not be removed.", e, module);
        }
        return rowsAffected > 0 ? true : false;
    }

    /**
     * Returns the last modification of the entity record as date.
     * 
     * @return modification date
     */
    public Date getLastModified() {
        return entity.getTimestamp("createdStamp");
    }

    /**
     * Makes a copy of the data object.
     * 
     * @return copy, needs to cast
     */
    public CmsDataObject copy() {
        GenericValue valueCopy = getDelegator().makeValue(
                entity.getEntityName());
        valueCopy.setNextSeqId();
        valueCopy.setNonPKFields(entity.getAllFields());
        CmsDataObject doCopy = null;
        try {
            getDelegator().createOrStore(valueCopy);
            doCopy = this.getClass().getConstructor(GenericValue.class)
                    .newInstance(valueCopy);
        } catch (Exception e) {
            throw new CmsException(
                    "Could not create copy of data object. Entity name: "
                            + entity.getEntityName(), e, module);
        }
        return doCopy;
    }

    /**
     * Returns the delegator instance of this class.
     * 
     * @return Delegator instance
     */
    protected static Delegator getDelegator() {
        if (delegator == null) {
            delegator = DelegatorFactory.getDelegator("default");
        }
        return delegator;
    }
    
    /**
     * Creates an empty value of an entity with the given fields preset.
     * <p>
     * FIXME: This method preemptively sets a sequence ID for the
     * new value. This may (???) result in wasted sequence IDs in
     * read-only queries that use CmsDataObject. It's done here
     * temporarily to avoid breaking existing code that expects
     * there to always be a unique ID; ideally, should be done right 
     * before storing.
     * 
     * @param entityName
     * @param fields
     * 
     * @return
     */
    private static GenericValue makeValue(String entityName,
            Map<String, ?> fields) {
        GenericValue gv = null;
        //try {
        gv = getDelegator().makeValue(entityName,
                    getSanitizedFields(entityName, fields));
        gv.setNextSeqId();
        //} catch (GenericEntityException e) {
        //    throw new CmsException(
        //            "Could not create new value: " + entityName, e, module);
        //}
        return gv;
    }

    /**
     * Finds all entities matching a given set of fields and wraps them in data
     * objects of the given class.
     * 
     * @param fields
     * @param dataObjectClass
     * @return List of data objects of the given class
     */
    public static <T extends CmsDataObject> List<T> findAll(
            Map<String, ?> fields, Class<T> dataObjectClass, List orderBy) {
        List<GenericValue> genericValues;
        List<T> dataObjects = new FastList<T>();
        try {        	
            genericValues = getDelegator().findByAnd(dataObjectClass.getSimpleName(), fields, null, true);
        } catch (GenericEntityException e) {
            throw new CmsException("Entity could not be found. Entity: "
                    + dataObjectClass.getSimpleName() + " Fields: "
                    + fields.toString(), e, module);
        }
        for (GenericValue gv : genericValues) {
            try {
                dataObjects.add(dataObjectClass.getConstructor(
                        GenericValue.class).newInstance(gv));
            } catch (Exception e) {
                // yeah, yeah, I know. I'm a naughty boy.
            }
        }
        return dataObjects;
    }
    
    /**
     * Finds all entities matching a given set of conditions and wraps them in data
     * objects of the given class.
     * 
     * @param fields
     * @param dataObjectClass
     * @return List of data objects of the given class
     */
    public static <T extends CmsDataObject> List<T> findAll(EntityCondition whereCondition, 
            Class<T> dataObjectClass, List orderBy) {
        List<GenericValue> genericValues;
        List<T> dataObjects = new FastList<T>();
        try {
            // FIXME: Should this have boolean cache as true???
            genericValues = getDelegator().findList(
                    dataObjectClass.getSimpleName(), whereCondition, null, orderBy, null, true);
        } catch (GenericEntityException e) {
            throw new CmsException("Entity could not be found. Entity: "
                    + dataObjectClass.getSimpleName() + " Condition: "
                    + (whereCondition != null ? whereCondition.toString() : "(none)"), e, module);
        }
        for (GenericValue gv : genericValues) {
            try {
                dataObjects.add(dataObjectClass.getConstructor(
                        GenericValue.class).newInstance(gv));
            } catch (Exception e) {
                // yeah, yeah, I know. I'm a naughty boy.
            }
        }
        return dataObjects;
    }    


    /**
     * Finds the first (and usually only) entity matching the fields and wraps
     * it into a data object of the given class.
     * 
     * @param fields
     * @param dataObjectClass
     * @return Data object of the given class
     */
    public static <T extends CmsDataObject> T findFirst(
            Map<String, ?> fields, Class<T> dataObjectClass) {
        List<GenericValue> genericValues;
        T dataObject = null;
        try {
            // TODO should this be findByAndCache?!
            genericValues = getDelegator().findByAndCache(
                    dataObjectClass.getSimpleName(), fields);
        } catch (GenericEntityException e) {
            throw new CmsException("Entity could not be found. Entity: "
                    + dataObjectClass.getSimpleName() + " Fields: "
                    + fields.toString(), e, module);
        }
        if (genericValues.size() > 0) {
            try {
                dataObject = dataObjectClass.getConstructor(GenericValue.class)
                        .newInstance(genericValues.get(0));
            } catch (Exception e) {
                // FIXME yeah, yeah, I know. I'm a naughty boy.
            }
        }
        return dataObject;
    }
    
    /**
     * Finds the first (and usually only) entity matching the fields and wraps
     * it into a data object of the given class.
     * 
     * @param fields
     * @param dataObjectClass
     * @return Data object of the given class
     */
    public static <T extends CmsDataObject> T findFirst(EntityCondition whereCondition,
            List orderBy, Class<T> dataObjectClass) {
        List<GenericValue> genericValues;
        T dataObject = null;
        try {
            // FIXME: Should this have boolean cache as true???
            genericValues = getDelegator().findList(dataObjectClass.getSimpleName(), 
                    whereCondition, null, orderBy, null, true);
        } catch (GenericEntityException e) {
            throw new CmsException("Entity could not be found. Entity: "
                    + dataObjectClass.getSimpleName() + " Condition: "
                    + (whereCondition != null ? whereCondition.toString() : "(none)"), e, module);
        }
        if (genericValues.size() > 0) {
            try {
                dataObject = dataObjectClass.getConstructor(GenericValue.class)
                        .newInstance(genericValues.get(0));
            } catch (Exception e) {
            }
        }
        return dataObject;
    }
    
    
    /**
     * Finds all entities matching a given set of fields and wraps them in data
     * objects of the given class.
     * 
     * TODO: Make it less dependend upon the PCMSPage and PCMSAsset entity
     * 
     * @param dataObjectClass
     * @return List of data objects of the given class
     */
    public static <T extends CmsDataObject> List<T> findAll(Class<T> dataObjectClass,List orderBy) {
        List<GenericValue> genericValues;
        List<T> dataObjects = new FastList<T>();
        try {
            genericValues = getDelegator().findList(dataObjectClass.getSimpleName(), EntityCondition.makeCondition("webSiteId",EntityOperator.NOT_EQUAL,null), null, orderBy, null, true);
        } catch (GenericEntityException e) {
            throw new CmsException("Entity could not be found. Entity: "
                    + dataObjectClass.getSimpleName() + " Fields: ", e, module);
        }
        for (GenericValue gv : genericValues) {
            try {
                dataObjects.add(dataObjectClass.getConstructor(
                        GenericValue.class).newInstance(gv));
            } catch (Exception e) {
                // yeah, yeah, I know. I'm a naughty boy.
            }
        }
        return dataObjects;
    }

    /**
     * Returns a new CmsDataObjectCache for the given type.
     * 
     * @param <T>
     * @return cache instance
     */
    protected static <T extends CmsDataObject> CmsDataObjectCache<T> getNewCache(
            int expiration) {
        return new CmsDataObjectCache<T>(expiration);
    }

    /**
     * Returns a new CmsDataObjectCache for the given type.
     * 
     * @param <T>
     * @return cache instance
     */
    protected static <T extends CmsDataObject> CmsDataObjectCache<T> getNewCache() {
        return new CmsDataObjectCache<T>();
    }

    /**
     * Copies the fields in the entity over to the map.
     * 
     * @param fields
     *            Map containing entity fields (and others)
     * 
     * @return
     */
    private static Map<String, Object> getSanitizedFields(String entityName,
            Map<String, ?> fields) {
        ModelEntity me = getDelegator().getModelEntity(entityName);
        Map<String, Object> sanitizedFields = FastMap
                .<String, Object> newInstance();
        // Copy relevant fields
        for (String name : me.getAllFieldNames()) {
            sanitizedFields.put(name, fields.get(name));
        }
        return sanitizedFields;
    }
}
