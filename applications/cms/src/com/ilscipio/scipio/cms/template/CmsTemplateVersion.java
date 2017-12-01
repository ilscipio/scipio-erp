package com.ilscipio.scipio.cms.template;

import java.io.Serializable;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.ofbiz.base.util.UtilMisc;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.GenericEntityException;
import org.ofbiz.entity.GenericValue;

import com.ilscipio.scipio.cms.CmsException;
import com.ilscipio.scipio.cms.CmsUtil;
import com.ilscipio.scipio.cms.data.CmsDataObject;

public abstract class CmsTemplateVersion extends CmsTemplate {

    private static final long serialVersionUID = 2461746703166121074L;

    public static final String module = CmsTemplateVersion.class.getName();
    
    protected enum VersionStatus {
        ACTIVE,
        INACTIVE,
        UNKNOWN // Not all calling code needs to know status; affect only when necessary.
    }
    
    private VersionStatus nextVersionStatus = VersionStatus.UNKNOWN;

    protected CmsTemplateVersion(GenericValue entity) {
        super(entity);
    }

    public CmsTemplateVersion(Delegator delegator, Map<String, ?> fields) {
        super(delegator, fields);
    }
    
    @Override    
    public void update(Map<String, ?> fields) {
        super.update(fields);
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
        this.getTemplate();
    }

    @Override
    public String getName() {
        return getTemplate().getName();
    }
    
    /**
     * Marks version as inactive (only applied upon store).
     */
    public void setAsInactiveVersion() {
        nextVersionStatus = VersionStatus.INACTIVE;
    }
    
    /**
     * Marks version as active (only applied upon store).
     */
    public void setAsActiveVersion() {
        nextVersionStatus = VersionStatus.ACTIVE;
    }
    
    /** 
     * Commits the template version and any other necessary data.
     * <p> 
     * @see org.CmsDataObject.cms.data.CmsDataObject#store()
     */
    @Override
    public void store() throws CmsException {
        super.store();
        storeVersionStatus();
    }

    protected void storeVersionStatus() {
        // Then, modify active status if need be (if UNKNOWN, then we don't care - avoid extra queries this way)
        // This must be done AFTER the template version is stored.
        if (nextVersionStatus == VersionStatus.ACTIVE) {
            getActiveVersionWorkerInst().createOrUpdateRecord(getDelegator(), getTemplateId(), getVersionId());
            
            getTemplate().updateStoreLocalActiveContent(getTemplateContentId());
        } else if (nextVersionStatus == VersionStatus.INACTIVE) {
            getActiveVersionWorkerInst().removeRecord(getDelegator(), getTemplateId(), getVersionId());
            
            getTemplate().updateStoreLocalActiveContent(null);
        } else {
            // always update in case contentId changed on the active version without actual status change
            getTemplate().updateStoreLocalActiveContent(null);
        }
    }
    
    protected abstract ActiveVersionWorker<?, ?> getActiveVersionWorkerInst();
    
    protected abstract String getTemplateId();
    
    public abstract CmsVersionedComplexTemplate<?, ?> getTemplate();

    public String getVersionId() {
        return getId();
    }

    @Override
    public int remove() {
        // Remove active status (always run this, even if not active, just in case)
        return getActiveVersionWorkerInst().removeRecord(getDelegator(), getTemplateId(), getVersionId()) + 
                super.remove();
    }
    
    public static <T extends CmsTemplateVersion> boolean areSameVersion(T first, T second) {
        if (first != null && second != null) {
            String firstVersionId = first.getVersionId();
            if (firstVersionId != null)  {
                return firstVersionId.equals(second.getVersionId());
            } else {
                return false;
            }
        } else {
            return false;
        }
    }
    
    public <T extends CmsTemplateVersion> boolean isSameVersion(T other) {
        return areSameVersion(this, other);
    }
    
    /**
     * Determines if this is an active version.
     * <p>
     * knownInfo is merely an optimization. May be null. May be set by caller to avoid
     * excess queries in loops (could encapsulate this in an iterator later).
     * 
     * @param knownInfo
     * @return
     */
    public boolean isActiveVersion(ExtendedInfo knownInfo) {
        if (knownInfo != null && knownInfo.getActiveVersionId() != null) {
            return knownInfo.getActiveVersionId().equals(getVersionId());
        } else {
            return isSameVersion(getTemplate().getActiveVersion());
        }
    }  
    
    public boolean isActiveVersion() {
        return isActiveVersion(null);
    }
    
    public boolean isLastVersion(ExtendedInfo knownInfo) {
        if (knownInfo != null && knownInfo.getLastVersionId() != null) {
            return knownInfo.getLastVersionId().equals(getVersionId());
        } else {
            return isSameVersion(getTemplate().getLastVersion());
        }
    }
    
    public boolean isLastVersion() {
        return isLastVersion(null);
    }
    
    public boolean isFirstVersion(ExtendedInfo knownInfo) {
        if (knownInfo != null && knownInfo.getFirstVersionId() != null) {
            return knownInfo.getFirstVersionId().equals(getVersionId());
        } else {
            return isSameVersion(getTemplate().getFirstVersion());
        }
    }
   
    public boolean isFirstVersion() {
        return isFirstVersion(null);
    }
    
    Date getOriginalVersionDate() {
        return (Date) entity.get("origVersionDate");
    }
    
    public Date getVersionDate() {
        Date result = getOriginalVersionDate();
        if (UtilValidate.isEmpty(result)) {
            result = getLastModified();
        }
        return result;
    }
    
    public String getCreatedBy() {
        return entity.getString("createdBy");
    }
    
    public void setCreatedBy(String createdBy) {
        entity.set("createdBy", createdBy);
    }
    
    public String getCreatorName() {
        try {
            return CmsUtil.getPersonDisplayName(getDelegator(), getCreatedBy());
        } catch (GenericEntityException e) {
            throw new CmsException(
                "Could not retrieve user for page template version " + getVersionId(), e);
        }  
    }
    
    void putRawDataIntoMap(Map<String, Object> in) {
        in.putAll(entity);
    }
    
    Map<String, Object> getRawDataAsMap() {
        Map<String, Object> result = new HashMap<>();
        putRawDataIntoMap(result);
        return result;
    }
    
    /**
     * Puts this template versions's fields into a map resembling its entity.
     * 
     * @param in 
     * @param minimalInfoOnly   if true, returns only necessary/raw data (entity contents);
     *                          otherwise, returns a template-friendly map
     * @param knownInfo         for optimization purposes; may be null
     */
    public void putIntoMap(Map<String, Object> in, boolean minimalInfoOnly, ExtendedInfo knownInfo) {
        putRawDataIntoMap(in);
        // Special overrides
        in.put("versionId", getVersionId()); // Some instances may have special IDs
        in.put("versionDate", getVersionDate());
        
        if (!minimalInfoOnly) {
            //ExtendedInfo knownInfoLoc = (knownInfo != null ? knownInfo : new ExtendedInfo());
            in.put("isActive", isActiveVersion(knownInfo));
            in.put("isLast", isLastVersion(knownInfo));
            in.put("isFirst", isFirstVersion(knownInfo));
        }
    }
    
    public void putIntoMap(Map<String, Object> in) {
        putIntoMap(in, false, null);
    }
    
    public Map<String, Object> getAsMap(boolean minimalInfoOnly, ExtendedInfo knownInfo) {
        Map<String, Object> result = new HashMap<>();
        putIntoMap(result, minimalInfoOnly, knownInfo);
        return result;
    }
    
    public Map<String, Object> getAsMap() {
        return getAsMap(false, null);
    }
    
    @SuppressWarnings("serial")
    public static class ExtendedInfo implements Serializable {
        private String activeVersionId = null;
        private String lastVersionId = null;
        private String firstVersionId = null;

        public String getActiveVersionId() {
            return activeVersionId;
        }
        public void setActiveVersionId(String activeVersionId) {
            this.activeVersionId = activeVersionId;
        }
        public void setActiveVersion(CmsTemplateVersion version) {
            String verId = null;
            if (version != null) {
                verId = version.getVersionId();
            }
            setActiveVersionId(verId);
        }
        
        public String getLastVersionId() {
            return lastVersionId;
        }
        public void setLastVersionId(String lastVersionId) {
            this.lastVersionId = lastVersionId;
        }
        public void setLastVersion(CmsTemplateVersion version) {
            String verId = null;
            if (version != null) {
                verId = version.getVersionId();
            }
            setLastVersionId(verId);
        }
        
        public String getFirstVersionId() {
            return firstVersionId;
        }
        public void setFirstVersionId(String firstVersionId) {
            this.firstVersionId = firstVersionId;
        }
        public void setFirstVersion(CmsTemplateVersion version) {
            String verId = null;
            if (version != null) {
                verId = version.getVersionId();
            }
            setFirstVersionId(verId);
        }
    }
    
    /**
     * Worker class to help manipulate the CmsXxxVersionState entities procedurally and
     * internally within CmsXxxVersion code (CmsXxxVersionState should be abstracted by 
     * CmsXxxVersion class).
     * <p>
     * NOTE: This is also used for some non-template e.g. CmsPage/Version, so this is
     * kept separate from XxxTemplateWorker.
     */
    public static abstract class ActiveVersionWorker<T extends CmsDataObject, S extends CmsDataObject> {
    
        protected abstract String getStateEntityName();
    
        protected abstract String getRecordIdFieldName();
        
        public GenericValue makeRecord(Delegator delegator, String templateId, String versionId) {
            GenericValue activeRecord = delegator.makeValue(getStateEntityName());
            activeRecord.set(getRecordIdFieldName(), templateId);
            activeRecord.set("versionId", versionId);
            activeRecord.set("versionStateId", "CMS_VER_ACTIVE");
            return activeRecord;
        }
        
        public void updateRecord(Delegator delegator, GenericValue activeRecord, String versionId) {
            activeRecord.set("versionId", versionId);
        }
        
        public GenericValue createOrUpdateRecord(Delegator delegator, String templateId, String versionId) throws CmsException {
            GenericValue activeRecord = getRecord(delegator, templateId);
            if (activeRecord == null) {
                activeRecord = makeRecord(delegator, templateId, versionId);
            } else {
                updateRecord(delegator, activeRecord, versionId);
            }
            try {
                return delegator.createOrStore(activeRecord);
            } catch (GenericEntityException e) {
                throw new CmsException(e);
            }
        }
                    
        public GenericValue getRecord(Delegator delegator, String templateId) throws CmsException {
            try {
                return delegator.findOne(getStateEntityName(), 
                        UtilMisc.toMap(getRecordIdFieldName(), templateId, "versionStateId", "CMS_VER_ACTIVE"), false);
            } catch (GenericEntityException e) {
                throw new CmsException(e);
            }
        }
        
        public String getRecordVersionId(Delegator delegator, String templateId) throws CmsException {
            GenericValue record = getRecord(delegator, templateId);
            if (record != null) {
                return record.getString("versionId");
            }
            return null;
        }
        
        public GenericValue getRecord(Delegator delegator, String templateId, String versionId) throws CmsException {
            GenericValue result = null;
            List<GenericValue> activeRecords;
            try {
                activeRecords = delegator.findByAnd(getStateEntityName(), 
                        UtilMisc.toMap(getRecordIdFieldName(), templateId, "versionId", versionId, "versionStateId", "CMS_VER_ACTIVE"), null, false);
            } catch (GenericEntityException e) {
                throw new CmsException(e);
            }
            if (UtilValidate.isNotEmpty(activeRecords)) {
                return activeRecords.get(0);
            }
            return result;
        }
        
        /**
         * Makes the active template version procedurally for a given page template inactive. 
         * If none, causes no issue.
         */
        public int removeRecord(Delegator delegator, String templateId) throws CmsException {
            try {
                return delegator.removeByAnd(getStateEntityName(), UtilMisc.toMap(getRecordIdFieldName(), templateId, "versionStateId", "CMS_VER_ACTIVE"));
            } catch (GenericEntityException e) {
                throw new CmsException(e);
            }
        }
        
        /**
         * Makes the given template version inactive. If already inactive, causes no issue.
         */
        public int removeRecord(Delegator delegator, String templateId, String versionId) throws CmsException {
            try {
                return delegator.removeByAnd(getStateEntityName(), 
                        UtilMisc.toMap(getRecordIdFieldName(), templateId, "versionId", versionId, "versionStateId", "CMS_VER_ACTIVE"));
            } catch (GenericEntityException e) {
                throw new CmsException(e);
            }
        }
    }
}
