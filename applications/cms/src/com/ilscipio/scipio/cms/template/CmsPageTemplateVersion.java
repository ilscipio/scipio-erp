package com.ilscipio.scipio.cms.template;

import java.util.Date;
import java.util.List;
import java.util.Map;

import javolution.util.FastList;
import javolution.util.FastMap;

import org.ofbiz.base.util.UtilMisc;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.GenericEntityException;
import org.ofbiz.entity.GenericValue;
import com.ilscipio.scipio.cms.CmsException;
import com.ilscipio.scipio.cms.CmsUtil;
import com.ilscipio.scipio.cms.data.CmsDataObject;

/**
 * CmsPageTemplateVersion - Represents a version of a page template.
 * <p>
 * Not thread-safe (keep local).
 * 
 */
public class CmsPageTemplateVersion extends CmsDataObject {

    public static final String module = CmsPageTemplateVersion.class.getName();
    private static final long serialVersionUID = 6296253358684857671L;

    private enum VersionStatus {
        ACTIVE,
        INACTIVE,
        UNKNOWN // Not all calling code needs to know status; affect only when necessary.
    }
    
    
    private CmsPageTemplate pageTemplate; // Mutable - set on first get if necessary.
    private VersionStatus nextVersionStatus = VersionStatus.UNKNOWN;
    private boolean legacyVersion = false; // True if this instance represents a legacy template version
    
    
    // Constructors
    
    /**
     * Loads an existing template version by ID.
     * 
     * @param primaryKey
     */
    public CmsPageTemplateVersion(String primaryKey) {
        super(primaryKey);
        this.pageTemplate = null;
    }

    
    /**
     * Creates a new template version with the given fields (see CmsPageTemplateVersion entity for available fields).
     * 
     * @param fields
     */
    public CmsPageTemplateVersion(Map<String, ?> fields) {
        super(fields);
        this.pageTemplate = null;
    }
    
    /**
     * Creates a new template version with the given fields (see CmsPageTemplateVersion entity for available fields)
     * for the given page. If page template ID is passed in fields, must match given page template instance's.
     * 
     * @param fields
     * @param pageTemplate
     */
    public CmsPageTemplateVersion(Map<String, ?> fields, CmsPageTemplate pageTemplate) {
        this(fields, pageTemplate, false);
    }
    
    /**
     * Creates a new template version with the given fields (see CmsPageTemplateVersion entity for available fields)
     * for the given page. If page template ID is passed in fields, must match given page template instance's.
     * This instance can be created to designate that this is a "legacy" page template body instance.
     * 
     * @param fields
     * @param pageTemplate
     * @param legacyVersion
     */
    public CmsPageTemplateVersion(Map<String, ?> fields, CmsPageTemplate pageTemplate, boolean legacyVersion) {
        super(checkPageTemplateId(fields, pageTemplate));
        this.legacyVersion = legacyVersion;
        if (this.legacyVersion) {
            nextVersionStatus = VersionStatus.ACTIVE; // Initial legacy version is made active
        }
        this.pageTemplate = pageTemplate;
        this.setPageTemplateId(pageTemplate.getId());
    }

    private static Map<String, ?> checkPageTemplateId(Map<String, ?> fields, CmsPageTemplate pageTemplate) {
        String fieldsTmpId = getPageTemplateId(fields);
        if (UtilValidate.isNotEmpty(fieldsTmpId)) {
            if (!fieldsTmpId.equals(pageTemplate.getId())) {
                throw new CmsException("Trying to create a page template version instance " +
                    "with a template ID in fields (" + fieldsTmpId + ") that is different from " +
                    "page template instance ID (" + pageTemplate.getId() + ")", module);
            }
        }
        return fields;
    }
    
    /**
     * Loads an existing page template version from the given entity.
     * 
     * @param entity
     */
    public CmsPageTemplateVersion(GenericValue entity) {
        this(entity, null);
    }
    
    private CmsPageTemplateVersion(GenericValue entity, CmsPageTemplate pageTemplate) {
        super(entity);
        this.pageTemplate = pageTemplate; // Ability to specify pageTemplate here is almost merely an internal optimization
    }
    
    
    // Factory methods
    // Note: There may be some query redundancy in the legacy template verifications here, but
    // not worth addressing.
    // Note 2: Even though versionId is a primary key, we always ask for pageTemplateId alongside
    // because of legacy templates and other possibilities.
    
    private static CmsPageTemplateVersion findSpecificTemplateVersion(Delegator delegator, 
            String pageTemplateId, String versionId, CmsPageTemplate pageTemplate) {
        CmsPageTemplateVersion result = null;
        try {

        List<GenericValue> specificVersions = delegator.findByAnd("CmsPageTemplateVersion", 
                UtilMisc.toMap("pageTemplateId", pageTemplateId, "versionId", versionId));
        
        if (UtilValidate.isNotEmpty(specificVersions)) {
            result = new CmsPageTemplateVersion(specificVersions.get(0), pageTemplate);
        }
            
        } catch (GenericEntityException e) {
            throw new CmsException(
                    "Could not retrieve active page template version. Page template ID: " + pageTemplateId, e, module);
        }
        return result;
    }
    
    private static CmsPageTemplate getNewOrSpecifiedTemplate(String pageTemplateId, 
            CmsPageTemplate pageTemplate) throws CmsException {
        return (pageTemplate == null ? new CmsPageTemplate(pageTemplateId) : pageTemplate);
    }
    
    public static CmsPageTemplateVersion findSpecificTemplateVersion(Delegator delegator, 
            CmsPageTemplate pageTemplate, String versionId) {
        return findSpecificTemplateVersion(delegator, pageTemplate.getId(), versionId, pageTemplate);
    }
    
    public static CmsPageTemplateVersion findSpecificTemplateVersion(Delegator delegator, 
            String pageTemplateId, String versionId) {
        return findSpecificTemplateVersion(delegator, pageTemplateId, versionId, null);
    }
    
    private static CmsPageTemplateVersion findActiveTemplateVersion(Delegator delegator, 
            String pageTemplateId, CmsPageTemplate pageTemplate) {
        CmsPageTemplateVersion result = null;
        try {
            GenericValue activeRecord = new ActiveVersionRecordWorker(delegator).getRecord(pageTemplateId);
            if (activeRecord != null) {
                GenericValue templateValue = activeRecord.getRelatedOne("CmsPageTemplateVersion");
                if (templateValue != null) {
                    result = new CmsPageTemplateVersion(templateValue, pageTemplate);
                }
            }
            else {
                // If none found, there's a chance it's because this is a legacy template
                CmsPageTemplate pageTmp = getNewOrSpecifiedTemplate(pageTemplateId, pageTemplate);
                if (pageTmp.isLegacyTemplate()) {
                    result = pageTmp.getLegacyTemplateAsVersion();
                }
            }
            
        } catch (GenericEntityException e) {
            throw new CmsException(
                    "Could not retrieve active page template version. Page template ID: " + pageTemplateId, e, module);
        }
        return result;
    }
    
    public static CmsPageTemplateVersion findActiveTemplateVersion(Delegator delegator, 
            CmsPageTemplate pageTemplate) {
        return findActiveTemplateVersion(delegator, pageTemplate.getId(), pageTemplate);
    }
    
    public static CmsPageTemplateVersion findActiveTemplateVersion(Delegator delegator, 
            String pageTemplateId) {
        return findActiveTemplateVersion(delegator, pageTemplateId, null);
    }
    
    
    /**
     * Finds all non-legacy template versions.
     * <p>
     * This one must be used in legacy template checks.
     * 
     * @param delegator
     * @param pageTemplateId
     * @param pageTemplate
     * @return
     */
    static List<CmsPageTemplateVersion> findAllNonLegacyTemplateVersions(Delegator delegator, 
            String pageTemplateId, CmsPageTemplate pageTemplate) {
        List<CmsPageTemplateVersion> result = FastList.newInstance();
        try {
            
            List<GenericValue> versionValues = delegator.findByAnd("CmsPageTemplateVersion", 
                    UtilMisc.toMap("pageTemplateId", pageTemplateId), UtilMisc.toList("createdStamp DESC"));
            
            for(GenericValue versionValue : versionValues) {
                result.add(new CmsPageTemplateVersion(versionValue, pageTemplate));
            }
            
        } catch (GenericEntityException e) {
            throw new CmsException(
                    "Could not retrieve one or more page template versions. Page template ID: " + pageTemplateId, e, module);
        }
        return result;
    }
    
    /**
     * Finds all template versions. Orders results from newest to oldest.
     * <p>
     * Note: Doesn't order using origVersionDate. As long as everything else is done in
     * order, it shouldn't be an issue.
     * 
     * @param delegator
     * @param pageTemplateId
     * @param pageTemplate
     * @return
     */
    private static List<CmsPageTemplateVersion> findAllTemplateVersions(Delegator delegator, 
            String pageTemplateId, CmsPageTemplate pageTemplate) {
        List<CmsPageTemplateVersion> result = findAllNonLegacyTemplateVersions(delegator, pageTemplateId,
                pageTemplate);
        
        // If none, there's a chance it's because this is a legacy template
        if (UtilValidate.isEmpty(result)) {
            CmsPageTemplate pageTmp = getNewOrSpecifiedTemplate(pageTemplateId, pageTemplate);
            if (pageTmp.isLegacyTemplate()) {
                result.add(pageTmp.getLegacyTemplateAsVersion());
            }
        }
        return result;
    }

    public static List<CmsPageTemplateVersion> findAllTemplateVersions(Delegator delegator, 
            CmsPageTemplate pageTemplate) {
        return findAllTemplateVersions(delegator, pageTemplate.getId(), pageTemplate);
    }
    
    public static List<CmsPageTemplateVersion> findAllTemplateVersions(Delegator delegator, 
            String pageTemplateId) {
        return findAllTemplateVersions(delegator, pageTemplateId, null);
    }
    
    
    private static CmsPageTemplateVersion findLastTemplateVersion(Delegator delegator, 
            String pageTemplateId, CmsPageTemplate pageTemplate) {
        List<CmsPageTemplateVersion> allVersions = findAllTemplateVersions(delegator, pageTemplateId, pageTemplate);
        if (UtilValidate.isNotEmpty(allVersions)) {
            return allVersions.get(0);
        }
        else {
            return null;
        }
    }
    
    public static CmsPageTemplateVersion findLastTemplateVersion(Delegator delegator, 
            CmsPageTemplate pageTemplate) {
        return findLastTemplateVersion(delegator, pageTemplate.getId(), pageTemplate);
    }
    
    public static CmsPageTemplateVersion findLastTemplateVersion(Delegator delegator, 
            String pageTemplateId) {
        return findLastTemplateVersion(delegator, pageTemplateId, null);
    }
    
    
    private static CmsPageTemplateVersion findFirstTemplateVersion(Delegator delegator, 
            String pageTemplateId, CmsPageTemplate pageTemplate) {
        List<CmsPageTemplateVersion> allVersions = findAllTemplateVersions(delegator, pageTemplateId, pageTemplate);
        if (UtilValidate.isNotEmpty(allVersions)) {
            return allVersions.get(allVersions.size() - 1);
        }
        else {
            return null;
        }
    }
    
    public static CmsPageTemplateVersion findFirstTemplateVersion(Delegator delegator, 
            CmsPageTemplate pageTemplate) {
        return findLastTemplateVersion(delegator, pageTemplate.getId(), pageTemplate);
    }
    
    public static CmsPageTemplateVersion findFirstTemplateVersion(Delegator delegator, 
            String pageTemplateId) {
        return findFirstTemplateVersion(delegator, pageTemplateId, null);
    }
    
    
    // Getters and operational methods
    
    /**
     * Returns true if this template version was derived from a legacy page template (without versioning).
     * 
     * @return
     */
    public boolean isLegacyVersion() {
        return this.legacyVersion;
    }
    
    public CmsPageTemplate getPageTemplate() {
        if (pageTemplate != null) {
            return pageTemplate;
        }
        else {
            pageTemplate = new CmsPageTemplate(getPageTemplateId());
            return pageTemplate;
        }
    }
    
    public String getPageTemplateId() {
        return entity.getString("pageTemplateId");
    }
    
    public static String getPageTemplateId(Map<String, ?> fields) {
        return (String) fields.get("pageTemplateId");
    }
    
    private void setPageTemplateId(String pageTemplateId) {
        entity.set("pageTemplateId", pageTemplateId);
    }
    
    @Override
    public String getId() {
            return super.getId();
    }
    
    public String getVersionId() {
        return getId();
    }
    
    public String getVersionComment() {
        return entity.getString("versionComment");
    }
    
    /**
     * Sets version comment.
     * <p>
     * Note: Is only public because it may be desirable to be able to edit past comments (i.e., like SVN).
     * 
     * @param versionComment
     */
    public void setVersionComment(String versionComment) {
        entity.set("versionComment", versionComment);
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
                "Could not retrieve user for page template version " + getVersionId(), e,
                module);
        }  
    }
    
    public String getTemplateBody() {
        return entity.getString("templateBody");
    }
    
    /**
     * Sets template body for given version. Generally deprecated and should not be used
     * except for backwards-compatibility.
     */
    void setTemplateBody(String templateBody) {
        entity.set("templateBody", templateBody);
    }
    
    
    public static boolean areSameVersion(CmsPageTemplateVersion first, CmsPageTemplateVersion second) {
        if (first != null && second != null) {
            String firstVersionId = first.getVersionId();
            if (firstVersionId != null)  {
                return firstVersionId.equals(second.getVersionId());
            }
            else {
                return false;
            }
        }
        else {
            return false;
        }
    }
    
    public boolean isSameVersion(CmsPageTemplateVersion other) {
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
        }
        else {
            return isSameVersion(getPageTemplate().getActiveVersion());
        }
    }  
    
    public boolean isActiveVersion() {
        return isActiveVersion(null);
    }
    
    public boolean isLastVersion(ExtendedInfo knownInfo) {
        if (knownInfo != null && knownInfo.getLastVersionId() != null) {
            return knownInfo.getLastVersionId().equals(getVersionId());
        }
        else {
            return isSameVersion(getPageTemplate().getLastVersion());
        }
    }
    
    public boolean isLastVersion() {
        return isLastVersion(null);
    }
    
    public boolean isFirstVersion(ExtendedInfo knownInfo) {
        if (knownInfo != null && knownInfo.getFirstVersionId() != null) {
            return knownInfo.getFirstVersionId().equals(getVersionId());
        }
        else {
            return isSameVersion(getPageTemplate().getFirstVersion());
        }
    }
   
    public boolean isFirstVersion() {
        return isFirstVersion(null);
    }
    
    
    void putRawDataIntoMap(Map<String, Object> in) {
        in.putAll(entity);
    }
    
    Map<String, Object> getRawDataAsMap() {
        Map<String, Object> result = FastMap.newInstance();
        putRawDataIntoMap(result);
        return result;
    }
    
    public static class ExtendedInfo {

        private String activeVersionId = null;
        private String lastVersionId = null;
        private String firstVersionId = null;

        public String getActiveVersionId() {
            return activeVersionId;
        }
        public void setActiveVersionId(String activeVersionId) {
            this.activeVersionId = activeVersionId;
        }
        public void setActiveVersion(CmsPageTemplateVersion version) {
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
        public void setLastVersion(CmsPageTemplateVersion version) {
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
        public void setFirstVersion(CmsPageTemplateVersion version) {
            String verId = null;
            if (version != null) {
                verId = version.getVersionId();
            }
            setFirstVersionId(verId);
        }
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
        //if (isLegacyVersion()) {
        //in.put("versionComment", "First"); // TODO: Localize // Not Needed here
        //}
        in.put("versionDate", getVersionDate());
        
        if (!minimalInfoOnly) {
            ExtendedInfo knownInfoLoc = (knownInfo != null ? knownInfo : new ExtendedInfo());
            
            in.put("isActive", isActiveVersion(knownInfo));
            in.put("isLast", isLastVersion(knownInfo));
            in.put("isFirst", isFirstVersion(knownInfo));
        }
    }
    
    public void putIntoMap(Map<String, Object> in) {
        putIntoMap(in, false, null);
    }
    
    public Map<String, Object> getAsMap(boolean minimalInfoOnly, ExtendedInfo knownInfo) {
        Map<String, Object> result = FastMap.newInstance();
        putIntoMap(result, minimalInfoOnly, knownInfo);
        return result;
    }
    
    public Map<String, Object> getAsMap() {
        return getAsMap(false, null);
    }
    
    
    /** 
     * Commits the template version and any other necessary data.
     * <p>
     * Currently also triggers legacy template migration and active status record keeping.
     * 
     * @see org.CmsDataObject.cms.data.CmsDataObject#store()
     */
    @Override
    public boolean store() throws GenericEntityException {
        
        // First, store legacy version if applicable
        boolean legacyResult = getPageTemplate().commitLegacyTemplateMigration(isLegacyVersion());
        
        // Then store self
        boolean selfStoreResult = super.store();
        this.legacyVersion = false; // No longer a legacy version once committed
        
        // Then, modify active status if need be (if UNKNOWN, then we don't care - avoid extra queries this way)
        // This must be done AFTER the template version is stored.
        boolean statusResult = true;
        if (nextVersionStatus == VersionStatus.ACTIVE) {
            try {
                ActiveVersionRecordWorker recordWorker = new ActiveVersionRecordWorker(entity.getDelegator());
                GenericValue resValue = recordWorker.createOrUpdateRecord(getPageTemplateId(), getVersionId());
                statusResult = (resValue != null);
            }
            catch(GenericEntityException e) {
                throw new CmsException("Entity could not be stored.", e, module);
            }
        }
        else if (nextVersionStatus == VersionStatus.INACTIVE) {
            try {
                unsetActiveStatusRecord();
            }
            catch(GenericEntityException e) {
                throw new CmsException("Entity could not be stored.", e, module);
            }
        }
        
        return legacyResult && selfStoreResult && statusResult;
    }
    
    @Override
    public boolean remove() {
        try {
            // Remove active status (always run this, even if not active, just in case)
            unsetActiveStatusRecord();
        }
        catch(GenericEntityException e) {
            throw new CmsException("Active page template version entity value could not be removed.", e, module);
        }
        return super.remove();
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
    
    private void unsetActiveStatusRecord() throws GenericEntityException {
        new ActiveVersionRecordWorker(entity.getDelegator()).unsetRecord(getPageTemplateId(), getVersionId());
    }
    
    
    // Helper classes and methods
    
    /**
     * Removes all template versions for a given page template procedurally. If none, causes no issue.
     * 
     * @param delegator
     * @param pageTemplateId
     * @throws GenericEntityException
     */
    static void removeAllTemplateVersions(Delegator delegator, 
            String pageTemplateId) throws GenericEntityException {
        
        new ActiveVersionRecordWorker(delegator).unsetRecord(pageTemplateId);
        delegator.removeByAnd("CmsPageTemplateVersion", UtilMisc.toMap("pageTemplateId", pageTemplateId));
    }
    

    /**
     * Worker class to help manipulate the CmsPageActiveTmpRecord entity procedurally and
     * internally within CmsPageTemplateVersion code (CmsPageActiveTmpRecord should be abstracted by 
     * CmsPageTemplateVersion).
     * 
     */
    private static class ActiveVersionRecordWorker {
        private Delegator delegator;
        ActiveVersionRecordWorker(Delegator delegator) {
            this.delegator = delegator;
        }
        
        GenericValue makeRecord(String pageTemplateId, String versionId) {
            GenericValue activeRecord = delegator.makeValue("CmsPageActiveTmpRecord");
            activeRecord.set("pageTemplateId", pageTemplateId);
            activeRecord.set("versionId", versionId);
            return activeRecord;
        }
        
        void updateRecord(GenericValue activeRecord, String versionId) {
            activeRecord.set("versionId", versionId);
        }
        
        GenericValue createOrUpdateRecord(String pageTemplateId, String versionId) throws GenericEntityException {
            GenericValue activeRecord = getRecord(pageTemplateId);
            if (activeRecord == null) {
                activeRecord = makeRecord(pageTemplateId, versionId);
            }
            else {
                updateRecord(activeRecord, versionId);
            }
            return delegator.createOrStore(activeRecord);
        }
                    
        GenericValue getRecord(String pageTemplateId) throws GenericEntityException {
            return delegator.findOne("CmsPageActiveTmpRecord", 
                    UtilMisc.toMap("pageTemplateId", pageTemplateId), false);
        }
        
        GenericValue getRecord(String pageTemplateId, 
                String versionId) throws GenericEntityException {
            GenericValue result = null;
            List<GenericValue> activeRecords = delegator.findByAnd("CmsPageActiveTmpRecord", 
                    UtilMisc.toMap("pageTemplateId", pageTemplateId, "versionId", versionId));
            if (UtilValidate.isNotEmpty(activeRecords)) {
                return activeRecords.get(0);
            }
            return result;
        }
        
        /**
         * Makes the active template version procedurally for a given page template inactive. 
         * If none, causes no issue.
         * 
         * @param delegator
         * @param pageTemplateId
         * @throws GenericEntityException
         */
        void unsetRecord(String pageTemplateId) throws GenericEntityException {
            delegator.removeByAnd("CmsPageActiveTmpRecord", UtilMisc.toMap("pageTemplateId", pageTemplateId));
        }
        
        /**
         * Makes the given template version inactive. If already inactive, causes no issue.
         * 
         * @param delegator
         * @param pageTemplateId
         * @param versionId
         * @throws GenericEntityException
         */
        void unsetRecord(String pageTemplateId, 
                String versionId) throws GenericEntityException {
            delegator.removeByAnd("CmsPageActiveTmpRecord", 
                    UtilMisc.toMap("pageTemplateId", pageTemplateId, "versionId", versionId));
        }
    }

}
