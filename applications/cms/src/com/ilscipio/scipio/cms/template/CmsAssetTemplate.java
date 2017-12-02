package com.ilscipio.scipio.cms.template;

import java.io.Writer;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;

import javax.servlet.http.HttpServletRequest;

import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.UtilMisc;
import org.ofbiz.base.util.UtilProperties;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.base.util.collections.MapStack;
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.GenericEntityException;
import org.ofbiz.entity.GenericValue;

import com.ilscipio.scipio.ce.util.Optional;
import com.ilscipio.scipio.cms.CmsException;
import com.ilscipio.scipio.cms.CmsUtil;
import com.ilscipio.scipio.cms.content.CmsPage;
import com.ilscipio.scipio.cms.content.CmsPageContent;
import com.ilscipio.scipio.cms.content.CmsPageContext;
import com.ilscipio.scipio.cms.control.CmsControlUtil;
import com.ilscipio.scipio.cms.data.CmsDataObject;
import com.ilscipio.scipio.cms.data.CmsEntityVisit;
import com.ilscipio.scipio.cms.data.CmsEntityVisit.CmsEntityVisitor;
import com.ilscipio.scipio.cms.data.CmsEntityVisit.VisitRelation;
import com.ilscipio.scipio.cms.data.CmsEntityVisit.VisitRelations;
import com.ilscipio.scipio.cms.data.CmsMajorObject;
import com.ilscipio.scipio.cms.data.CmsObjectCache;
import com.ilscipio.scipio.cms.data.CmsObjectCache.CacheEntry;
import com.ilscipio.scipio.cms.template.CmsScriptTemplate.CmsScriptTemplateAssoc;
import com.ilscipio.scipio.cms.template.CmsTemplateVersion.ActiveVersionWorker;

import freemarker.core.Environment;

/**
 * 2016: Asset template 
 * NOTE: most code is now shared with page template.
 */
public class CmsAssetTemplate extends CmsMasterComplexTemplate<CmsAssetTemplate, CmsAssetTemplateVersion> 
    implements CmsRenderTemplate, CmsMajorObject  {
    
    private static final long serialVersionUID = -423202960558153897L;
    
    public static final String module = CmsAssetTemplate.class.getName();
    
    // default is TRUE
    private static final boolean firstVersionActive = UtilProperties.getPropertyAsBoolean("cms.properties", "asset.template.firstVersionActive", true);
    
    private static final CmsObjectCache<CmsAssetTemplate> idCache = CmsObjectCache.getGlobalCache("cms.template.asset.id");
    private static final CmsObjectCache<CmsAssetTemplate> nameCache = CmsObjectCache.getGlobalCache("cms.template.asset.name");
    
    
    protected final AssetTemplateRenderer renderer = new AssetTemplateRenderer(this); // 2016: dedicated renderer object
    protected CmsAssetTemplateAssoc assoc; // 2016: backreference to association
    
    protected CmsAssetTemplate(GenericValue entity, CmsAssetTemplateAssoc assoc) {
        super(entity);
        this.assoc = assoc;
    }
    
    protected CmsAssetTemplate(GenericValue entity) {
        super(entity);
    }

    public CmsAssetTemplate(Delegator delegator, Map<String, ?> fields) {
        super(delegator, fields);
    }
    
    protected CmsAssetTemplate(Delegator delegator, Map<String, ?> fields, CmsAssetTemplateAssoc assoc) {
        super(delegator, fields);
        this.assoc = assoc;
    }
    
    protected CmsAssetTemplate(CmsAssetTemplate other, Map<String, Object> copyArgs) {
        super(other, copyArgs);
    }
    
    @Override    
    public void update(Map<String, ?> fields, boolean setIfEmpty) {
        // this just updates the entity for now, plus saves tmplBodySrc...
        // version to be handled separate? (like page?)
        super.update(fields, setIfEmpty);
    }
    
    @Override
    public CmsAssetTemplate copy(Map<String, Object> copyArgs) throws CmsException {
        return new CmsAssetTemplate(this, copyArgs);
    }
    
    @Override
    public CmsAssetTemplate copyWithVersion(Map<String, Object> copyArgs) throws CmsException {
        return (CmsAssetTemplate) super.copyWithVersion(copyArgs);
    }

    @Override
    public CmsAssetTemplateVersion createNewVersion(Map<String, ?> fields) {
        return new CmsAssetTemplateVersion(getDelegator(), fields, this);
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
        preloadWorker.preload(this.renderer);
        // NOTE: un-intuitively, we must do the preload for the assoc, because instances store us
        // rather than the assoc itself!
        preloadWorker.preload(this.assoc);
        
        // SPECIAL CASE: To prevent memory wastage, don't keep original template body
        // source in memory because Freemarker template will already contain it.
        // NOTE: this works in conjunction with activeContentId which also evades the need
        // for needless cached Version instance
        this.tmplBodySrc = TemplateBodySource.getUndefined();
        
        // SPECIAL CASE: discard the active version reference, because here the renderer
        // already loaded the template
        this.activeVersion = Optional.empty();
    }

    @Override
    protected void verifyNewFields(Delegator delegator, Map<String, Object> fields, boolean isNew) throws CmsException {
        // NOTE: in theory, could want groupingNullSignificant=false here because it makes the duplicate name check more aggressive...
        verifyUniqueName(delegator, fields, isNew, "templateName", false, "webSiteId", true, true);
    }
    
    public void setWebSiteId(String webSiteId) { // legacy webSiteId field
        entity.setString("webSiteId", webSiteId);
    }

    public String getWebSiteId() { // legacy webSiteId field
        return entity.getString("webSiteId");
    }
    
    @Override
    public Map<String, Object> getDescriptor(Locale locale) {
        preventIfImmutable(); // WARN: currently dangerous if called from rendering!
        
        Map<String, Object> descriptor = super.getDescriptor(locale);
        descriptor.put("importName", getImportName());
        descriptor.put("contentTypeId", getContentTypeId());
        if (assoc != null) {
            descriptor.put("assoc", assoc.getDescriptor(locale));
        }
        return descriptor;
    }

    public CmsAssetTemplateAssoc getAssoc() {
        return assoc;
    }
    
    public Long getInputPosition() {
        return getAssoc().getInputPosition();
    }

    public String getImportName() {
        return getAssoc().getImportName();
    }

    public String getAssocId() { // 2016: new
        return getAssoc().getAssocId();
    }
    
    public String getContentTypeId() {
        return entity.getString("contentTypeId");
    }
    
    @Override
    public void store() throws CmsException {
        super.store();
    }

    @Override
    public int remove() throws CmsException {
        int rowsAffected = 0;
        
        try {
            // SPECIAL: we have to remove the active reference, otherwise deps will prevent delete
            setActiveTemplateContentId(null);
            entity.store();
            
            // delete CmsPageTemplateAssetAssoc
            List<GenericValue> pageAssetAssoc = entity.getRelated("CmsPageTemplateAssetAssoc", null, null, false);
            for (GenericValue pageAsset : pageAssetAssoc) {
                pageAsset.remove();
                rowsAffected += 1;
            }
        } catch (GenericEntityException e) {
            throw makeRemoveException(e);
        }
        
        return rowsAffected + super.remove();
    }
    
    public static abstract class CmsAssetTemplateAssoc extends CmsDataObject {

        private static final long serialVersionUID = -1882593413864645246L;

        protected CmsAssetTemplate assetTemplate;
        
        public CmsAssetTemplateAssoc(Delegator delegator, Map<String, ?> fields, CmsAssetTemplate assetTemplate) {
            super(delegator, fields);
            this.assetTemplate = assetTemplate;
        }

        protected CmsAssetTemplateAssoc(GenericValue entity) {
            super(entity);
        }
        
        protected CmsAssetTemplateAssoc(CmsAssetTemplateAssoc other, Map<String, Object> copyArgs) {
            super(other, copyArgs);
        }
        
        @Override    
        public void update(Map<String, ?> fields, boolean setIfEmpty) {
            // here, must ignore scriptTemplateId - set at creation and should never change
            if (fields.containsKey("scriptTemplateId") && UtilValidate.isNotEmpty(getAssetTemplateId())) {
                fields = new HashMap<String, Object>(fields);
                fields.remove("scriptTemplateId");
            }
            super.update(fields, setIfEmpty);
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
            // DO NOT do this way because the preload will (un-intuitively) come from CmsAssetTemplate to us
            //PreloadWorker.makeImmutable(this.getAssetTemplate());
        }
        
        @Override
        public void store() throws CmsException {
            if (assetTemplate != null) {
                assetTemplate.store();
                if (UtilValidate.isEmpty(getAssetTemplateId())) {
                    setAssetTemplateId(assetTemplate.getId());
                } else {
                    if (!getAssetTemplateId().equals(assetTemplate.getId())) {
                        throw new CmsException("Error: trying to change the assetTemplateId of "
                                + "existing asset association '" + getId() + "'; cannot be changed once after creation");
                    }
                }
            }
            super.store();
        }

        public String getAssocId() {
            return getId();
        }
        
        public String getAssetTemplateId() {
            return entity.getString("assetTemplateId");
        }
        
        private void setAssetTemplateId(String assetTemplateId) {
            entity.setString("assetTemplateId", assetTemplateId);
        }
        
        public CmsAssetTemplate getAssetTemplate() {
            CmsAssetTemplate assetTemplate = this.assetTemplate;
            final CmsAssetTemplateAssoc assoc = this;
            if (assetTemplate == null) {
                assetTemplate = new AssetTemplateWorker() { // extend the asset template worker to pass the assoc to CmsAssetTemplate constructor
                    @Override
                    public CmsAssetTemplate makeFromValue(GenericValue value) throws CmsException {
                        return new CmsAssetTemplate(value, assoc);
                    }

                    @Override
                    public CmsAssetTemplate makeFromFields(Delegator delegator, Map<String, ?> fields)
                            throws CmsException {
                        return new CmsAssetTemplate(delegator, fields, assoc);
                    }
                    
                }.findByIdAlways(getDelegator(), getAssetTemplateId(), false);
                this.assetTemplate = assetTemplate;
            }
            return assetTemplate;
        }
        
        public Long getInputPosition() {
            return entity.getLong("inputPosition") != null ? entity.getLong("inputPosition") : 0L;
        }
        
        public String getImportName() {
            return entity.getString("importName");
        }
        
        public String getDisplayName() {
            return entity.getString("displayName");
        }
        
        @Override
        public Map<String, Object> getDescriptor(Locale locale) {
            preventIfImmutable(); // WARN: currently dangerous if called from rendering!
            
            Map<String, Object> descriptor = super.getDescriptor(locale);
            descriptor.put("importName", getImportName());
            descriptor.put("displayName", getDisplayName());
            descriptor.put("inputPosition", getInputPosition());
            descriptor.put("id", getId());
            descriptor.put("assetTemplateId", getAssetTemplateId());
            return descriptor;
        }

        protected abstract void clearTemplate();
        
        protected abstract void setTemplate(CmsDataObject template);
        
        protected abstract boolean hasTemplate();
    }

    @Override
    protected CmsAssetTemplateScriptAssoc.AssetTemplateScriptAssocWorker getTemplateScriptAssocWorker() {
        return CmsAssetTemplateScriptAssoc.getWorker();
    }
    
    public static class CmsAssetTemplateScriptAssoc extends CmsTemplateScriptAssoc {

        private static final long serialVersionUID = -7223454711555662977L;

        protected CmsAssetTemplateScriptAssoc(GenericValue entity) {
            super(entity);
        }
        
        public CmsAssetTemplateScriptAssoc(Delegator delegator, Map<String, ?> fields, CmsScriptTemplate scriptTemplate) {
            super(delegator, fields, scriptTemplate);
        }

        protected CmsAssetTemplateScriptAssoc(CmsAssetTemplateScriptAssoc other, Map<String, Object> copyArgs) {
            super(other, copyArgs);
            // NOTE: don't bother clearing out the ID fields here, caller should handle
        }
        
        @Override    
        public void update(Map<String, ?> fields, boolean setIfEmpty) {
            super.update(fields, setIfEmpty);
        }
        
        @Override
        public CmsScriptTemplateAssoc copy(Map<String, Object> copyArgs) {
            return new CmsAssetTemplateScriptAssoc(this, copyArgs);
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
        }
        
        @Override
        protected void clearTemplate() {
            entity.set("assetTemplateId", null);
        }
        
        @Override
        protected void setTemplate(CmsDataObject template) {
            if (!(template instanceof CmsAssetTemplate)) throw new CmsException("CmsAssetTemplateScriptAssoc requires a CmsAssetTemplate, got: " 
                    + (template != null ? template.getClass().getName() : null));
            entity.set("assetTemplateId", template.getId());
        }
        
        @Override
        protected boolean hasTemplate() {
            return (entity.get("assetTemplateId") != null);
        }

        @Override
        public AssetTemplateScriptAssocWorker getWorkerInst() {
            return AssetTemplateScriptAssocWorker.worker;
        }
        
        public static AssetTemplateScriptAssocWorker getWorker() {
            return AssetTemplateScriptAssocWorker.worker;
        }

        public static class AssetTemplateScriptAssocWorker extends TemplateScriptAssocWorker<CmsAssetTemplateScriptAssoc> {
            private static final AssetTemplateScriptAssocWorker worker = new AssetTemplateScriptAssocWorker();
            
            protected AssetTemplateScriptAssocWorker() {
                super(CmsAssetTemplateScriptAssoc.class);
            }

            @Override
            public CmsAssetTemplateScriptAssoc makeFromValue(GenericValue value) throws CmsException {
                return new CmsAssetTemplateScriptAssoc(value);
            }

            @Override
            public CmsAssetTemplateScriptAssoc makeFromFields(Delegator delegator, Map<String, ?> fields) throws CmsException {
                return new CmsAssetTemplateScriptAssoc(delegator, fields, null);
            }

            @Override
            protected CmsAssetTemplateScriptAssoc makeFromFields(Delegator delegator, Map<String, ?> fields,
                    CmsScriptTemplate scriptTemplate) throws CmsException {
                return new CmsAssetTemplateScriptAssoc(delegator, fields, scriptTemplate);
            }
        }
    }
    
 
    // Helpers
    

    @Override
    public AssetTemplateRenderer getRenderer() {
        return renderer;
    }
    
    /**
     * Dedicated asset renderer object.
     */
    @SuppressWarnings("serial")
    public static class AssetTemplateRenderer extends CmsRenderTemplate.TemplateRenderer<CmsAssetTemplate> {

        public AssetTemplateRenderer(CmsAssetTemplate template) {
            super(template);
        }

        public static class AtRenderArgs extends RenderArgs {
            public AtRenderArgs() {
                super();
            }

            public AtRenderArgs(Environment env, MapStack<String> context, CmsPageContent content,
                    CmsPageContext pageContext, boolean shareScope) {
                super(env, context, content, pageContext, shareScope);
            }

            public AtRenderArgs(Environment env, MapStack<String> context, CmsPageContent content,
                    CmsPageContext pageContext, Map<String, Object> earlyCtxVars, Map<String, Object> ovrdCtxVars,
                    boolean shareScope) {
                super(env, context, content, pageContext, earlyCtxVars, ovrdCtxVars, shareScope);
            }

            public AtRenderArgs(Writer out, Environment env, MapStack<String> context, CmsPageContent content,
                    CmsPageContext pageContext, Map<String, Object> earlyCtxVars, Map<String, Object> ovrdCtxVars,
                    boolean skipSystemCtx, boolean skipExtraCommonCtx, boolean shareScope) {
                super(out, env, context, content, pageContext, earlyCtxVars, ovrdCtxVars, skipSystemCtx, skipExtraCommonCtx, shareScope);
            }

            public AtRenderArgs(Writer out, MapStack<String> context, CmsPageContent content,
                    CmsPageContext pageContext, boolean shareScope) {
                super(out, context, content, pageContext, shareScope);
            }

            public AtRenderArgs(Writer out, MapStack<String> context, CmsPageContent content,
                    CmsPageContext pageContext, Map<String, Object> earlyCtxVars, Map<String, Object> ovrdCtxVars, boolean shareScope, boolean newCmsCtx) {
                super(out, context, content, pageContext, earlyCtxVars, ovrdCtxVars, shareScope);
                setNewCmsCtx(newCmsCtx);
            }
        }
        
        /** 
         * Renders the asset.
         * <p>
         * Overrides caller to push and pop context, and prevent system context from re-populating.
         */
        @Override
        public Object processAndRender(RenderArgs renderArgs) throws CmsException {
            if (renderArgs.isNewCmsCtx()) {
                if (renderArgs.getPageContext() == null) {
                    renderArgs.setPageContext(CmsPageContext.makeFromGenericRequestContext(renderArgs.getContext()));
                    if (renderArgs.getContent() == null) {
                        renderArgs.setContent(new CmsPageContent((CmsPage) null));
                    }
                }
                
                // here assume context has everything except cms stuff
                renderArgs.setSkipSystemCtx(false);
                renderArgs.setSystemCtxCmsOnly(true);
                renderArgs.setSystemCtxNoPush(true); // don't push system ctx, if it's used
                renderArgs.setSkipExtraCommonCtx(true);
            } else {
                // NO SYSTEM context for assets in usual invocations - the system context should be already populated by the page earlier.
                renderArgs.setSkipSystemCtx(true);
                renderArgs.setSkipExtraCommonCtx(true);
            }
            return super.processAndRender(renderArgs);
        }

        @Override
        protected List<CmsScriptTemplate> getSortedScriptTemplates() {
            return template.getSortedScriptTemplates();
        }

    }

    @Override
    public AssetTemplateWorker getWorkerInst() {
        return AssetTemplateWorker.worker;
    }
    
    public static AssetTemplateWorker getWorker() {
        return AssetTemplateWorker.worker;
    }

    public static class AssetTemplateWorker extends DataObjectWorker<CmsAssetTemplate> {
        private static final AssetTemplateWorker worker = new AssetTemplateWorker();
        
        protected AssetTemplateWorker() {
            super(CmsAssetTemplate.class);
        }

        @Override
        public CmsAssetTemplate makeFromValue(GenericValue value) throws CmsException {
            return new CmsAssetTemplate(value);
        }

        @Override
        public CmsAssetTemplate makeFromFields(Delegator delegator, Map<String, ?> fields) throws CmsException {
            return new CmsAssetTemplate(delegator, fields);
        }
        
        @Override
        public CmsAssetTemplate findById(Delegator delegator, String id, boolean useCache) throws CmsException {
            return findById(delegator, id, useCache, null);
        }
        
        public CmsAssetTemplate findById(Delegator delegator, String id, boolean useCache, HttpServletRequest request) throws CmsException {
            boolean useGlobalCache = isUseGlobalObjCacheStatic(useCache);
            CmsObjectCache<CmsAssetTemplate> cache = null;
            if (useGlobalCache) {
                cache = idCache;
            }
            
            String key = delegator.getDelegatorName() + "::" + id;
            CmsAssetTemplate asset = null;
            CacheEntry<CmsAssetTemplate> assetEntry = null;
            
            if (useGlobalCache) {
                assetEntry = cache.getEntry(key);
            }

            if (assetEntry == null) {
                if (CmsUtil.verboseOn()) {
                    Debug.logInfo("Cms: Retrieving asset template from database: id: " + id + CmsControlUtil.getReqLogIdDelimStr(request), module);
                }
                asset = findOne(delegator, UtilMisc.toMap("assetTemplateId", id), 
                        isUseDbCacheStatic(useCache));

                if (useGlobalCache) {
                    cache.put(key, asset);
                }
            } else {
                if (assetEntry.hasValue()) {
                    if (CmsUtil.verboseOn()) {
                        Debug.logVerbose("Cms: Retrieving asset template from cache: id: " + id + CmsControlUtil.getReqLogIdDelimStr(request), module);
                    }
                    asset = assetEntry.getValue();
                }
            }

            return asset;
        }
        
        public CmsAssetTemplate findByName(Delegator delegator, String name, String webSiteId, boolean webSiteIdOptional, boolean useCache) throws CmsException {
            return findByName(delegator, name, webSiteId, webSiteIdOptional, useCache, null);
        }
        
        /**
         * Finds by name and optional webSiteId. 
         * NOTE: if no webSiteId passed, it preferentially returns the records having no webSiteId.
         */
        public CmsAssetTemplate findByName(Delegator delegator, String name, String webSiteId, boolean webSiteIdOptional, boolean useCache, HttpServletRequest request) throws CmsException {
            boolean useGlobalCache = isUseGlobalObjCacheStatic(useCache);
            CmsObjectCache<CmsAssetTemplate> cache = null;
            if (useGlobalCache) {
                cache = nameCache;
            }
            if (webSiteId != null && webSiteId.isEmpty()) {
                webSiteId = null;
            }
            String key = delegator.getDelegatorName() + "::" + name + "::" + (webSiteId != null ? webSiteId : (webSiteIdOptional ? "_OPT_" : ""));
            CmsAssetTemplate asset = null;
            CacheEntry<CmsAssetTemplate> assetEntry = null;
            
            if (useGlobalCache) {
                assetEntry = cache.getEntry(key);
            }

            if (assetEntry == null) {
                if (CmsUtil.verboseOn()) {
                    Debug.logInfo("Cms: Retrieving asset template from database: name: " + name + CmsControlUtil.getReqLogIdDelimStr(request), module);
                }
                Map<String, Object> fields = UtilMisc.toMap("templateName", name);
                if (!webSiteIdOptional || webSiteId != null) {
                    fields.put("webSiteId", webSiteId);
                }
                // NOTE: always null webSiteIds first - this matters
                List<CmsAssetTemplate> assets = findAll(delegator, fields, UtilMisc.toList("webSiteId"), isUseDbCacheStatic(useCache));
                if (assets.size() > 0) {
                    asset = assets.get(0);
                }
                if (assets.size() > 1) {
                    if (!webSiteIdOptional || webSiteId != null) {
                        Debug.logError("Cms: Multiple asset templates with name '" + name + "' and webSiteId '" + webSiteId + "' found; using first found (id: " + asset.getId() + ")", module);
                    } else if (asset.getWebSiteId() != null) {
                        // if lookup by name only, it's usually because we expected only one result,
                        // either one with webSiteId null (no log warning) or only one webSiteId
                        Debug.logWarning("Cms: Multiple asset templates with name '" + name + "' and having a webSiteId found; using first found (id: " + asset.getId() + ", webSiteId: " + asset.getWebSiteId() + ")", module);
                    }
                }
                
                if (useGlobalCache) {
                    cache.put(key, asset);
                }
            } else {
                if (assetEntry.hasValue()) {
                    if (CmsUtil.verboseOn()) {
                        Debug.logVerbose("Cms: Retrieving asset template from cache: name: " + name + CmsControlUtil.getReqLogIdDelimStr(request), module);
                    }
                    asset = assetEntry.getValue();
                }
            }

            return asset;
        }
    }
    
    @Override
    protected AssetVerComTemplateWorker getVerComTemplateWorkerInst() {
        return AssetVerComTemplateWorker.templateWorker;
    }
    
    protected static AssetVerComTemplateWorker getVerComTemplateWorker() {
        return AssetVerComTemplateWorker.templateWorker;
    }
    
    public static class AssetVerComTemplateWorker extends VerComTemplateWorker<CmsAssetTemplate, CmsAssetTemplateVersion> {
        
        protected static final AssetVerComTemplateWorker templateWorker = new AssetVerComTemplateWorker();
        
        @Override
        protected DataObjectWorker<CmsAssetTemplate> getDataObjectWorker() {
            return CmsAssetTemplate.getWorker();
        }
        
        @Override
        protected CmsAssetTemplateVersion createVersion(GenericValue value, CmsAssetTemplate template) {
            return new CmsAssetTemplateVersion(value, template);
        }

        @Override
        protected CmsAssetTemplate getTemplate(Delegator delegator, String templateId) {
            return getDataObjectWorker().findByIdAlways(delegator, templateId, false);
        }

        @Override
        protected ActiveVersionWorker<CmsAssetTemplate, CmsAssetTemplateVersion> getActiveVersionWorker() {
            return CmsAssetTemplateVersion.AssetTemplateActiveVersionWorker.activeVersionWorker;
        }

        @Override
        protected String getVersionEntityName() {
            return "CmsAssetTemplateVersion";
        }

        @Override
        protected String getTemplateIdFieldName() {
            return "assetTemplateId";
        }

        @Override
        protected CmsAssetTemplate createTemplate(Delegator delegator, Map<String, ?> fields) {
            return new CmsAssetTemplate(delegator, fields);
        }

        @Override
        protected boolean isFirstVersionActive() {
            return firstVersionActive;
        }
    }
    
    @Override
    public void acceptEntityDepsVisitor(CmsEntityVisitor visitor, GenericValue relValue, VisitRelation relValueRelation, CmsMajorObject majorDataObj) throws Exception {
        CmsEntityVisit.acceptRelatedEntityDepsVisitor(visitor, VisitRelPlan.visitRelations, this.getEntity(), relValueRelation, relValue, this);
    }
    
    public static class VisitRelPlan extends VisitRelations.BuildPlan {
        public static final VisitRelPlan INSTANCE = new VisitRelPlan("CmsAssetTemplate");
        static final VisitRelations visitRelations = INSTANCE.buildSafe();
        public VisitRelPlan(String majorEntityName) { super(majorEntityName); }
        @Override public VisitRelations.Builder planDefinition(Delegator delegator) throws Exception {
            return newBuilder(delegator)
                .entity("CmsAssetTemplate")
                    .relation("CmsAssetTemplateScriptAssoc")
                    .self()
                    .relation("CmsAssetTemplateVersion")
                    .recall("ASSET_SCRASSOC")
                    .relation("CmsAttributeTemplate")
                .entity("CmsAssetTemplateVersion")
                    .self()
                    .relation("CmsAssetTemplateVersionState")
                .entity("CmsAssetTemplateScriptAssoc")
                    .relationMajor("CmsScriptTemplate")
                    .selfStash("ASSET_SCRASSOC"); 
        }
    }

}
