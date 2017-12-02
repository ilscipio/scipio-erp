package com.ilscipio.scipio.cms.template;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.UtilMisc;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.GenericEntityException;
import org.ofbiz.entity.GenericValue;
import org.ofbiz.entity.condition.EntityCondition;
import org.ofbiz.entity.condition.EntityOperator;

import com.ilscipio.scipio.ce.util.Optional;
import com.ilscipio.scipio.cms.CmsException;
import com.ilscipio.scipio.cms.data.CmsDataException;

/**
 * Versioned complex template.
 * <p>
 * This is the COMMON CODE shared between Page and Asset Templates (new 2016).
 */
public abstract class CmsVersionedComplexTemplate<T extends CmsVersionedComplexTemplate<T, V>, V extends CmsTemplateVersion> extends CmsComplexTemplate {

    private static final long serialVersionUID = 1294180234678880297L;

    public static final String module = CmsVersionedComplexTemplate.class.getName();
    
    // NOTE: these must be Optionals (emulated) for caching/thread safety reasons
    protected Optional<V> activeVersion; // NOTE: this is only temporarily cached in live render but then gets discarded by the subclasses
    protected Optional<V> lastVersion; // NOTE: not cached in live render
    
    // 2016: REMOVED - the parent class's tmplBodySrc is reused instead
    //private TemplateBodySource nextDisplayVersionTmplBodySrc = null; // This is set if the calling code wants to update the "current" template body for compatibility purposes    
    
    protected CmsVersionedComplexTemplate(GenericValue entity) {
        super(entity);
    }

    public CmsVersionedComplexTemplate(Delegator delegator, Map<String, ?> fields) {
        super(delegator, fields);
    }
    
    protected CmsVersionedComplexTemplate(CmsVersionedComplexTemplate<T, V> other, Map<String, Object> copyArgs) {
        super(other, copyArgs);
        // currently not calling anything here:
        //getVersionCopy...
    }
    
    @Override    
    public void update(Map<String, ?> fields, boolean setIfEmpty) {
        super.update(fields, setIfEmpty);
    }
    
    /** 
     * Creates in-memory copy of the parent template WITHOUT any versions.
     * Caller must create or copy a version.
     *
     * @see com.ilscipio.scipio.cms.data.CmsDataObject#copy(java.util.Map)
     */
    @SuppressWarnings("unchecked")
    @Override
    public CmsVersionedComplexTemplate<T, V> copy(Map<String, Object> copyArgs) throws CmsException {
        return (CmsVersionedComplexTemplate<T, V>) super.copy(copyArgs);
    }
    
    /**
     * Same as {@link #copy(Map)}, but also creates an in-memory copy of the requested
     * or last version and sets it as instance field, which can be gotten 
     * from the returned template using {@link #getLastVersion()}.
     * Caller must store.
     */
    @SuppressWarnings("unchecked")
    public CmsVersionedComplexTemplate<T, V> copyWithVersion(Map<String, Object> copyArgs) throws CmsException {
        CmsVersionedComplexTemplate<T, V> newTemplate = (CmsVersionedComplexTemplate<T, V>) super.copy(copyArgs);
        V newVersion = copyOtherVersion((T) this, copyArgs);
        newTemplate.setLastVersion(newVersion);
        return newTemplate;
    }
    
    /**
     * Override: Versioned templates must do nothing for this, we copy an explicit versionId instead.
     */
    @Override
    protected TemplateBodySource getTemplateBodySourceCopy(CmsTemplate other, Map<String, Object> copyArgs) {
        // DO NOTHING
        return null;
    }

    /**
     * Creates a new version for this page. Caller stores.
     * 
     * @param fields the entity fields to initialize the new version to (optional)
     * @return
     */
    public abstract V createNewVersion(Map<String, ?> fields);
    
    public V createAndStoreNewVersion(Map<String, ?> fields, boolean setAsActive) {
        V newVer = createNewVersion(fields);
        if (setAsActive) {
            newVer.setAsActiveVersion();
        }
        newVer.store();
        return newVer;
    }

    /**
     * Creates an in-memory copy of the specified (in copyArgs) or last version 
     * from the <code>other</code> instance, the result then associated to <code>this</code> instance, 
     * using any config in copyArgs.
     */
    protected V copyOtherVersion(T other, Map<String, Object> copyArgs) {
        return copyOtherVersion(getVersionForCopyAndVerify(copyArgs), copyArgs);
    }
    
    @SuppressWarnings("unchecked")
    protected V copyOtherVersion(V otherVersion, Map<String, Object> copyArgs) {
        CmsTemplateVersion newVersion = otherVersion.copy(copyArgs, this);
        
        // redundant?
        //// Copy the original version date from the last template
        //newVersion.setOriginalVersionDate(otherVersion.getVersionDate());
        
        return (V) newVersion;
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
        if (!hasLocalActiveTemplateBodySource()) {
            preloadWorker.preload(this.getActiveVersion());
        }
        //PreloadWorker.preloadImmutable(this.getLastVersion()); // NO
    }

    /**
     * Returns true if the template entity has a local optimized
     * reference to a template body such as activeContentId,
     * so that getActiveVersion() needs not be called in live renders.
     * <p>
     * Subclasses may override to control.
     */
    protected boolean hasLocalActiveTemplateBodySource() {
        return false;
    }
    
    /**
     * Stores the template record itself (and active version if any was set).
     * NOTE: 2017-11: this also stores <code>this.lastVersion</code>, for the copy case,
     * but ONLY if it was not already fully persisted.
     * TODO: REVIEW: could this always call store on this.lastVersion and this.activeVersion?
     *  
     *  (non-Javadoc)
     * @see org.CmsDataObject.cms.data.CmsDataObject#store()
     */
    @Override
    public void store() throws CmsException {
        super.store(); // Store self
        
        TemplateBodySource tmplBodySrc = this.tmplBodySrc;
        if (tmplBodySrc != null && tmplBodySrc.isDefined()) {
            CmsTemplateVersion activeVersion = getActiveVersion();
            if (activeVersion != null) {
                activeVersion.setTemplateBodySource(tmplBodySrc);
                activeVersion.store();
            } else {
                // 2016: WE WILL CREATE a new version if and only 
                // if the template has no versions attached to it yet;
                // this provides legacy straight storage without real versioning.
                if (isNewPageTemplate()) {
                    createStoreAutoFirstVersion();
                } else {
                    // 2016: NOTE: this merely preserves the previous behavior; could change to save to latest?
                    Debug.logWarning("Versioned template " + getId() + " tried to store a template body "
                            + "for its active version, but there was no active version - body discarded", module);
                }
            }
            this.tmplBodySrc = null;
        }
        
        
        if (lastVersion != null && lastVersion.isPresent()) {
            V lastVer = lastVersion.get();
            // TODO: REVIEW: could remove this condition, and also store activeVersion?
            // unclear if will cause problems anywhere.
            // For now this detects copy only.
            if (lastVer.getId() == null || lastVer.getTemplateId() == null) { 
                lastVer.store();
            }
        }
        
        // re-store ourselves with activeContentId if supported and changed 
        this.updateStoreLocalActiveContent(null);
    }
    
    protected void createStoreAutoFirstVersion() {
        V newVer = createNewVersion(UtilMisc.toMap());
        
        newVer.setTemplateBodySource(tmplBodySrc);
        if (getVerComTemplateWorkerInst().isFirstVersionActive()) {
            newVer.setAsActiveVersion();
        }
        String createdBy = entity.getString("lastUpdatedBy");
        if (UtilValidate.isEmpty(createdBy)) {
            createdBy = entity.getString("createdBy");
        }
        if (UtilValidate.isNotEmpty(createdBy)) {
            newVer.setCreatedBy(createdBy);
        }
        
        newVer.store();
    }
    
    /**
     * Explicitly Stores only the template record. For special circumstances.
     */
    @Override
    protected void storeSelfOnly() throws CmsException {
        super.storeSelfOnly();
    }
    
    @Override
    protected void storeTemplateBodySource() {
        // DO NOT STORE for this class - instead the active template version will be updated, if any (see store())
        //super.storeTemplateBodySource();
    }

    /**
     * Updates and stores local template activeContentId, if supported and needed by
     * the template entity. Subclasses should override if needed.
     * If activeContentId is non-null use it, otherwise re-lookup automatically.
     */
    protected void updateStoreLocalActiveContent(String activeContentId) {
        ;
    }
    
    /**
     * Removes the asset template from the database.
     * 
     * @return True if delete was successful, otherwise false
     * @throws GenericEntityException
     */
    @Override
    public int remove() {
        preventIfImmutable();
        int rowsAffected = 0;
        try {
            // delete the active record and all the versions
            rowsAffected += getVerComTemplateWorkerInst().removeAllTemplateVersions(getDelegator(), getId());
        } catch (GenericEntityException e) {
            throw makeRemoveException(e);
        }
        return rowsAffected + super.remove();
    }
    
    public V getLastVersion() {
        preventIfImmutable();
        
        Optional<V> lastVersion = this.lastVersion;
        if (lastVersion == null) {
            lastVersion = Optional.ofNullable(getVerComTemplateWorkerInst().findLastTemplateVersion(entity.getDelegator(), getTemplate()));
            this.lastVersion = lastVersion;
        }
        return lastVersion.orElse(null);
    }
    
    void setLastVersion(V version) {
        this.lastVersion = Optional.ofNullable(version);
    }
    
    public V getFirstVersion() {
        preventIfImmutable();
        return getVerComTemplateWorkerInst().findFirstTemplateVersion(entity.getDelegator(), getTemplate());
    }
    
    public V getActiveVersion() {
        Optional<V> activeVersion = this.activeVersion;
        if (activeVersion == null) {
            activeVersion = Optional.ofNullable(readActiveVersion());
            this.activeVersion = activeVersion;
        }
        return activeVersion.orElse(null);
    }
    
    void setActiveVersion(V version) {
        this.activeVersion = Optional.ofNullable(version);
    }
    
    protected V readActiveVersion() { // skips cache
        return getVerComTemplateWorkerInst().findActiveTemplateVersion(entity.getDelegator(), getTemplate());
    }
    
    
    public V getVersion(String versionId) {
        preventIfImmutable();
        return getVerComTemplateWorkerInst().findSpecificTemplateVersion(entity.getDelegator(), getTemplate(), versionId);
    }
    
    /**
     * Gets version indicated in the copy args and verifies OK for copy.
     * Does NOT creates any copies.
     */
    public V getVersionForCopyAndVerify(Map<String, Object> copyArgs) {
        String versionId = (String) copyArgs.get("copyVersionId");
        V version;
        if (UtilValidate.isNotEmpty(versionId)) {
            version = getVersion(versionId);
            if (version == null) throw new CmsDataException("cannot find template version '" + versionId + "' in template '" + getId() + "' for copy");
        } else {
            version = getLastVersion();
            if (version == null) throw new CmsDataException("source template '" + getId() + "' has no last template version - cannot create copy");
        }
        return version;
    }
    
    public List<V> getAllVersions() {
        preventIfImmutable();
        return getVerComTemplateWorkerInst().findAllTemplateVersions(entity.getDelegator(), getTemplate());
    }
    
    @SuppressWarnings("unchecked")
    protected T getTemplate() {
        // workaround for casting
        return (T) this;
    }
    
    public void putTemplateFieldsIntoMap(Map<String, Object> out) {
        out.putAll(entity);
        // Special overrides - the active body
        getTemplateBodySource().toFields(out);
    }
    
    private boolean hasTemplateVersions() {
        preventIfImmutable();
        List<V> allVersions = getVerComTemplateWorkerInst().findAllTemplateVersions(
                entity.getDelegator(), this.getId(), getTemplate());
        return UtilValidate.isNotEmpty(allVersions);
    }
    
    boolean isNewPageTemplate() {
        // For now, use a heuristic to determine if this is a new page template
        return !hasTemplateVersions();
    }

    /**
     * Sets the body on the display version. Generally deprecated and should not be used
     * except for backward-compatibility.
     */
    @Override
    public void setTemplateBodySource(Map<String, ?> fields) {
        // nothing special; reuse this.tmplBodySrc for caching until storage.
        super.setTemplateBodySource(fields);
    }

    /**
     * Returns the template body of the version currently set for display.
     * <p>
     * Preserves backwards-compatibility with templates that had no versioning.
     * 
     * @return
     */
    @Override
    public TemplateBodySource getTemplateBodySource() {
        TemplateBodySource tmplBodySrc = this.tmplBodySrc;
        if (tmplBodySrc == null) {
            CmsTemplateVersion activeVersion = getActiveVersion();
            if (activeVersion != null) {
                activeVersion.getTemplateBody();
                tmplBodySrc = activeVersion.getTemplateBodySource();
            } else {
                tmplBodySrc = TemplateBodySource.getUndefined();
            }
            this.tmplBodySrc = tmplBodySrc;
        }
        return tmplBodySrc;
    }

    protected abstract VerComTemplateWorker<T, V> getVerComTemplateWorkerInst();
    
    public static abstract class VerComTemplateWorker<T extends CmsVersionedComplexTemplate<T, V>, V extends CmsTemplateVersion> {

        protected abstract DataObjectWorker<T> getDataObjectWorker();
        
        // TODO: there is redundancy here, need cleanup later...
        
        protected abstract V createVersion(GenericValue value, T template);
        
        protected abstract T createTemplate(Delegator delegator, Map<String, ?> fields);

        protected abstract T getTemplate(Delegator delegator, String pageTemplateId);
        
        protected abstract CmsTemplateVersion.ActiveVersionWorker<T, V> getActiveVersionWorker();
        
        protected abstract String getVersionEntityName();
        
        protected abstract String getTemplateIdFieldName();
        
        protected abstract boolean isFirstVersionActive();
        
        /**
         * Creates and stores a new template and any other required related entity values from the given fields.
         */
        public T createAndStoreNewTemplate(Delegator delegator, Map<String, ?> fields) {
            // First create template record itself
            Map<String, Object> tmpFields = new HashMap<>();
            tmpFields.put("webSiteId", fields.get("webSiteId"));
            tmpFields.put("templateName", fields.get("templateName"));
            tmpFields.put("createdBy", fields.get("createdBy"));
            tmpFields.put("lastUpdatedBy", fields.get("lastUpdatedBy"));
            
            T newTmp = createTemplate(delegator, tmpFields);
            
            newTmp.store();
            
            // Then, add first version
            Map<String, Object> verFields = new HashMap<>();
            verFields.put("pageTemplateId", newTmp.getId());
            verFields.put("templateBody", fields.get("templateBody"));
            verFields.put("templateLocation", fields.get("templateLocation"));
            verFields.put("templateSource", fields.get("templateSource"));
            verFields.put("createdBy", fields.get("createdBy"));
            verFields.put("versionComment", "First"); // TODO: Localize
            
            newTmp.createAndStoreNewVersion(verFields, isFirstVersionActive());

            return newTmp;
        }

        /**
         * Returns the template with the given name within a website.
         * 
         * @param templateName
         * @param webSiteId
         * @return template
         */
        public T findByName(Delegator delegator, String templateName, String webSiteId, boolean useCache) {
            return getDataObjectWorker().findFirst(delegator, 
                    UtilMisc.toMap("templateName", templateName, "webSiteId", webSiteId), useCache);
        }
        
        /**
         * Returns all (active) asset templates of a website.
         * 
         * @param webSiteId
         * @return
         */
        public List<T> findByWebSiteId(Delegator delegator, String webSiteId, boolean useCache) {
            return getDataObjectWorker().findAll(delegator, EntityCondition.makeCondition("webSiteId", EntityOperator.EQUALS, webSiteId),
                    UtilMisc.toList("templateName ASC"), useCache);
        }
        
        /**
         * Returns the active template version with the given name within a website.
         * 
         * @param templateName
         * @param webSiteId
         * @return template
         */
        public V findActiveVersion(Delegator delegator, String templateName,
                String webSiteId, boolean useCache) {
            // TODO: optimize with view-entity
            T asset = findByName(delegator, templateName, webSiteId, useCache);
            if (asset != null) {
                return asset.getActiveVersion();
            }
            return null;
        }
        
        /**
         * Returns the active template version with the given name within a website.
         * 
         * @param templateName
         * @param webSiteId
         * @return template
         */
        public V findLatestVersion(Delegator delegator, String templateName,
                String webSiteId, boolean useCache) {
            // TODO: optimize with view-entity
            T asset = findByName(delegator, templateName, webSiteId, useCache);
            if (asset != null) {
                return asset.getLastVersion();
            }
            return null;
        }

        // Factory methods    
        
        private V findSpecificTemplateVersion(Delegator delegator, String templateId, String versionId, T template) {
            V result = null;
            try {
                List<GenericValue> specificVersions = delegator.findByAnd(getVersionEntityName(), 
                        UtilMisc.toMap(getTemplateIdFieldName(), templateId, "versionId", versionId), null, false);
                
                if (UtilValidate.isNotEmpty(specificVersions)) {
                    result = createVersion(specificVersions.get(0), template);
                }
            } catch (GenericEntityException e) {
                throw new CmsException(
                        "Could not retrieve active template version. " + getTemplateIdFieldName() + ": " + templateId, e);
            }
            return result;
        }
        
        T getNewOrSpecifiedTemplate(Delegator delegator, String templateId, T template) throws CmsException {
            return (template == null ? getTemplate(delegator, templateId) : template);
        }
        
        public V findSpecificTemplateVersion(Delegator delegator, T template, String versionId) {
            return findSpecificTemplateVersion(delegator, template.getId(), versionId, template);
        }
        
        public V findSpecificTemplateVersion(Delegator delegator, String templateId, String versionId) {
            return findSpecificTemplateVersion(delegator, templateId, versionId, null);
        }
        
        private V findActiveTemplateVersion(Delegator delegator, 
                String pageTemplateId, T pageTemplate) {
            V result = null;
            try {
                GenericValue activeRecord = getActiveVersionWorker().getRecord(delegator, pageTemplateId);
                if (activeRecord != null) {
                    GenericValue templateValue = activeRecord.getRelatedOne(getVersionEntityName(), false);
                    if (templateValue != null) {
                        result = createVersion(templateValue, pageTemplate);
                    }
                }
            } catch (GenericEntityException e) {
                throw new CmsException("Could not retrieve active page template version. " + getTemplateIdFieldName() + ": " + pageTemplateId, e);
            }
            return result;
        }
        
        public V findActiveTemplateVersion(Delegator delegator, T template) {
            return findActiveTemplateVersion(delegator, template.getId(), template);
        }
        
        public V findActiveTemplateVersion(Delegator delegator, String templateId) {
            return findActiveTemplateVersion(delegator, templateId, null);
        }
        
        /**
         * Finds all template versions
         * 
         * @param delegator
         * @param templateId
         * @param template
         * @return
         */
        List<V> findAllTemplateVersions(Delegator delegator, String templateId, T template) {
            List<V> result = new ArrayList<>();
            try {
                List<GenericValue> versionValues = delegator.findByAnd(getVersionEntityName(), 
                        UtilMisc.toMap(getTemplateIdFieldName(), templateId), UtilMisc.toList("createdStamp DESC"), false);
                
                for(GenericValue versionValue : versionValues) {
                    result.add(createVersion(versionValue, template));
                }
            } catch (GenericEntityException e) {
                throw new CmsException(
                        "Could not retrieve one or more page template versions. " + getTemplateIdFieldName() + ": " + templateId, e);
            }
            return result;
        }
    
        public List<V> findAllTemplateVersions(Delegator delegator, T template) {
            return findAllTemplateVersions(delegator, template.getId(), template);
        }
        
        public List<V> findAllTemplateVersions(Delegator delegator, String templateId) {
            return findAllTemplateVersions(delegator, templateId, null);
        }
        
        private V findLastTemplateVersion(Delegator delegator, String templateId, T template) {
            List<V> allVersions = findAllTemplateVersions(delegator, templateId, template);
            if (UtilValidate.isNotEmpty(allVersions)) {
                return allVersions.get(0);
            } else {
                return null;
            }
        }
        
        public V findLastTemplateVersion(Delegator delegator, T template) {
            return findLastTemplateVersion(delegator, template.getId(), template);
        }
        
        public V findLastTemplateVersion(Delegator delegator, String templateId) {
            return findLastTemplateVersion(delegator, templateId, null);
        }
        
        
        private V findFirstTemplateVersion(Delegator delegator, String templateId, T template) {
            List<V> allVersions = findAllTemplateVersions(delegator, templateId, template);
            if (UtilValidate.isNotEmpty(allVersions)) {
                return allVersions.get(allVersions.size() - 1);
            } else {
                return null;
            }
        }
        
        public V findFirstTemplateVersion(Delegator delegator, T template) {
            return findLastTemplateVersion(delegator, template.getId(), template);
        }
        
        public V findFirstTemplateVersion(Delegator delegator, String templateId) {
            return findFirstTemplateVersion(delegator, templateId, null);
        }
        
        /**
         * Removes all template versions for a given page template procedurally. If none, causes no issue.
         * 
         * @param delegator
         * @param pageTemplateId
         * @throws GenericEntityException
         */
        public int removeAllTemplateVersions(Delegator delegator, String templateId) throws GenericEntityException {
            return getActiveVersionWorker().removeRecord(delegator, templateId) + 
                    delegator.removeByAnd(getVersionEntityName(), UtilMisc.toMap(getTemplateIdFieldName(), templateId));
        }
    }

}
