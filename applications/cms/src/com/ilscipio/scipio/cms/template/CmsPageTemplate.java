package com.ilscipio.scipio.cms.template;

import java.io.Writer;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Optional;
import java.util.Set;

import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.UtilGenerics;
import org.ofbiz.base.util.UtilMisc;
import org.ofbiz.base.util.UtilProperties;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.base.util.collections.MapStack;
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.GenericEntityException;
import org.ofbiz.entity.GenericValue;

import com.ilscipio.scipio.cms.CmsException;
import com.ilscipio.scipio.cms.content.CmsPage;
import com.ilscipio.scipio.cms.content.CmsPageContent;
import com.ilscipio.scipio.cms.content.CmsPageContext;
import com.ilscipio.scipio.cms.data.CmsDataObject;
import com.ilscipio.scipio.cms.data.CmsEntityVisit;
import com.ilscipio.scipio.cms.data.CmsEntityVisit.CmsEntityVisitor;
import com.ilscipio.scipio.cms.data.CmsEntityVisit.VisitRelation;
import com.ilscipio.scipio.cms.data.CmsEntityVisit.VisitRelations;
import com.ilscipio.scipio.cms.data.CmsMajorObject;
import com.ilscipio.scipio.cms.template.CmsTemplateVersion.ActiveVersionWorker;

import freemarker.core.Environment;

/**
 * The page template determines the presentation of a page. It includes a
 * FreeMarker template and attribute definitions. Page templates also have zero
 * or more asset templates attached which can be embedded into page.
 * 
 */
public class CmsPageTemplate extends CmsMasterComplexTemplate<CmsPageTemplate, CmsPageTemplateVersion> implements CmsRenderTemplate, CmsMajorObject  {

    private static final long serialVersionUID = 9154893402134263580L;
    
    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());
    
    // default is TRUE
    private static final boolean firstVersionActive = UtilProperties.getPropertyAsBoolean("cms", "page.template.firstVersionActive", true);

    /**
     * A map of asset templates (assocs) with their importName as key.
     * <p>
     * WARN: because we store CmsAssetTemplate and not CmsAssetTemplateAssoc, this has implications
     * for preloadContent().
     */
    private Map<String, CmsAssetTemplate> assetTemplatesByImportName;
    
    protected final PageTemplateRenderer renderer = new PageTemplateRenderer(this); // 2016: dedicated renderer object
    
    /**
     * Constructs new CmsPageTemplate from the given entity.
     *
     * @param entity
     * @param isNewTemplate set to true if entity represents a new, partially-constructed
     *        template; set to false if entity represents an existing template in the data source.
     */
    private CmsPageTemplate(GenericValue entity, boolean isNewTemplate) {
        super(entity);
    }
    
    protected CmsPageTemplate(GenericValue entity) {
        this(entity, false);
    }
    
    public CmsPageTemplate(Delegator delegator, Map<String, ?> fields) {
        super(delegator, fields);
    }
    
    protected CmsPageTemplate(CmsPageTemplate other, Map<String, Object> copyArgs) {
        super(other, copyArgs); // NOTE: super invokes getInitialVersionCopy
        
        // copy asset templates - this stores them in-memory, they will get committed during store()
        this.assetTemplatesByImportName = copyAssetTemplates(getAssetTemplatesByImportName(), copyArgs, other);
    }

    @Override    
    public void update(Map<String, ?> fields, boolean setIfEmpty) {
        super.update(fields, setIfEmpty);
    }
    
    /**
     * Copies this page template and the lates version.
     */
    @Override
    public CmsPageTemplate copy(Map<String, Object> copyArgs) {
        CmsPageTemplate newTemplate = new CmsPageTemplate(this, copyArgs);
        copyInitialVersionToTemplateCopy(newTemplate, copyArgs);
        return newTemplate;
    }
    
    protected static Map<String, CmsAssetTemplate> copyAssetTemplates(Map<String, CmsAssetTemplate> srcAssetTemplates, Map<String, Object> copyArgs, CmsPageTemplate other) {
        Map<String, CmsAssetTemplate> assetTemplates = new LinkedHashMap<>();
        for (Map.Entry<String, CmsAssetTemplate> entry : srcAssetTemplates.entrySet()) {
            CmsPageTemplateAssetAssoc assoc = (CmsPageTemplateAssetAssoc) entry.getValue().getAssoc();
            if (assoc == null) {
                Debug.logError("internal error: unexpected null CmsPageTemplateAssetAssoc in-memory for page template '" + other.getId() + "'", module);
            } else {
                assoc = assoc.copy(copyArgs);
                assoc.clearTemplate(); // pageTemplateId will be fixed-up on store()
                assetTemplates.put(entry.getKey(), new CmsAssetTemplate(entry.getValue().getEntity(), assoc));
            }
        }
        return assetTemplates;
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
        this.assetTemplatesByImportName = preloadWorker.preloadDeep(this.getAssetTemplatesByImportName());
        
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


    @Override
    public Map<String, Object> getDescriptor(Locale locale) {
        preventIfImmutable(); // WARN: currently dangerous if called from rendering!
        
        Map<String, Object> descriptor = super.getDescriptor(locale);
        // TODO: We currently get the "ACTIVE" asset templates as descriptors for CmsEditor; it's not clear
        // whether conceptually this should be the "ACTIVE" or "LATEST" templates being used here, though in
        // practice it may make no difference - establish this once and for all.
        List<CmsAssetTemplate> assetTemplates = getSortedAssetTemplates();
        List<Map<String, Object>> atDescriptors = new ArrayList<>();
        for (CmsAssetTemplate at : assetTemplates) {
            atDescriptors.add(at.getDescriptor(locale));
        }
        descriptor.put("assets", atDescriptors);
        return descriptor;
    }
    
    @Override
    public CmsPageTemplateVersion createNewVersion(Map<String, ?> fields) {
        return new CmsPageTemplateVersion(getDelegator(), fields, this);
    }
    
    
    /**
     * Returns a map of asset templates linked to this page template. The map
     * key is the import name under which this asset is available in the
     * freemarker source code of this page template.
     * 
     * @return Map of asset templates
     */
    protected Map<String, CmsAssetTemplate> readAssetTemplates() {
        Map<String, CmsAssetTemplate> result = new LinkedHashMap<>();
        try {
            // NOTE: nulls first corresponds to default value 0
            List<GenericValue> assocEntities = entity.getRelated("CmsPageTemplateAssetAssoc", null, 
                    UtilMisc.toList("inputPosition ASC NULLS FIRST"), false);
            for (GenericValue assoce : assocEntities) {
                CmsPageTemplateAssetAssoc asset = CmsPageTemplateAssetAssoc.getWorker().makeFromValue(assoce);
                if (result.containsKey(asset.getImportName())) {
                    Debug.logError("Cms: Duplicate CmsPageTemplateAssetAssoc.importName ('" + asset.getImportName() + 
                            "') detected for page template " + getId(), module);
                }
                result.put(asset.getImportName(), asset.getAssetTemplate());
            }
        } catch (GenericEntityException e) {
            throw new CmsException("Attribute templates could not be retrieved for page template: " + getName(), e);
        }
        return result;
    }
    
    /**
     * Returns a map of asset templates linked to this page template. The map
     * key is the import name under which this asset is available in the
     * freemarker source code of this page template.
     * 
     * @return Map of asset templates
     */
    public Map<String, CmsAssetTemplate> getAssetTemplatesByImportName() {
        Map<String, CmsAssetTemplate> assetTemplates = this.assetTemplatesByImportName;
        if (assetTemplates == null) {
            assetTemplates = readAssetTemplates();
            this.assetTemplatesByImportName = assetTemplates;
        }
        return assetTemplates;
    }
    
    public CmsAssetTemplate getAssetTemplateByImportName(String importName) {
        return getAssetTemplatesByImportName().get(importName);
    }
    
    /**
     * Gets local asset by ID.
     * NOTE: This uses a slow iteration to lookup by ID, and is used live.
     * However, referencing assets by ID is discouraged in template anyways, because
     * IDs are generated and unreadable.
     */
    public CmsAssetTemplate getAssetTemplateById(String id) {
        for(CmsAssetTemplate assetTemplate : getAssetTemplatesByImportName().values()) {
            if (id.equals(assetTemplate.getId())) {
                return assetTemplate; 
            }
        }
        return null;
    }
    
    public List<CmsAssetTemplate> getSortedAssetTemplates() {
        List<CmsAssetTemplate> assetTemplates = new ArrayList<>(getAssetTemplatesByImportName().values());
        Collections.<CmsAssetTemplate> sort(assetTemplates, new Comparator<CmsAssetTemplate>() {
            @Override
            public int compare(CmsAssetTemplate first, CmsAssetTemplate second) {
                return first.getInputPosition().compareTo(second.getInputPosition());
            }
        });
        return assetTemplates;
    }
    
    public List<CmsAssetTemplate> getAssetTemplates() {
        return new ArrayList<>(getAssetTemplatesByImportName().values());
    }

    public void store(boolean forceStoreAssetAssoc) throws CmsException {
        super.store();
        
        // 2017-11: we'll now store the linked asset template associations IF they were not
        // yet persisted OR entity has changed OR if forceStoreAssetAssoc (off by default)
        if (this.assetTemplatesByImportName != null) {
            for(CmsAssetTemplate asset : this.assetTemplatesByImportName.values()) {
                CmsPageTemplateAssetAssoc assoc = (CmsPageTemplateAssetAssoc) asset.getAssoc();
                if (assoc != null) {
                    if (forceStoreAssetAssoc || assoc.hasChangedOrNoId()) {
                        assoc.ensureTemplate(this);
                        assoc.store();
                    }
                }
            }
        }
    }
    
    @Override
    public void store() throws CmsException {
        store(false);
    }

    /**
     * Removes the page template from the database.
     * 
     * @return True if delete was successful, otherwise false
     */
    @Override
    public int remove() {
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
        // delete the rest (see CmsVersionedComplexTemplate.remove())
        return rowsAffected + super.remove();
    }
    
    public void setWebSiteId(String webSiteId) { // legacy webSiteId field
        entity.setString("webSiteId", webSiteId);
    }

    public String getWebSiteId() { // legacy webSiteId field
        return entity.getString("webSiteId");
    }

    public static Map<String, Object> getTemplateAsMap(Delegator delegator, String pageTemplateId) {
        CmsPageTemplate pageTmp = CmsPageTemplate.getWorker().findByIdAlways(delegator, pageTemplateId, false);
        Map<String, Object> result = new HashMap<>();
        pageTmp.putTemplateFieldsIntoMap(result);
        return result;
    }
    
    public static class CmsPageTemplateAssetAssoc extends CmsAssetTemplate.CmsAssetTemplateAssoc {

        private static final long serialVersionUID = 5839674451619496850L;

        public CmsPageTemplateAssetAssoc(Delegator delegator, Map<String, ?> fields, CmsAssetTemplate assetTemplate) {
            super(delegator, checkFields(fields, true), assetTemplate);
        }

        protected CmsPageTemplateAssetAssoc(GenericValue entity) {
            super(entity);
        }

        protected CmsPageTemplateAssetAssoc(CmsPageTemplateAssetAssoc other, Map<String, Object> copyArgs) {
            super(other, copyArgs);
        }
        
        @Override    
        public void update(Map<String, ?> fields, boolean setIfEmpty) {
            super.update(fields, setIfEmpty);
        }
        
        @Override
        public CmsPageTemplateAssetAssoc copy(Map<String, Object> copyArgs) throws CmsException {
            return new CmsPageTemplateAssetAssoc(this, copyArgs);
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
        
        protected static <T> Map<String, T> checkFields(Map<String, T> fields, boolean isNew) throws CmsException {
            if (isNew || fields.containsKey("inputPosition")) {
                if (UtilValidate.isEmpty((Long) fields.get("inputPosition"))) {
                    UtilGenerics.<String, Object> checkMap(fields).put("inputPosition", 0L);
                }
            }
            return fields;
        }
        
        @Override
        protected void clearTemplate() {
            this.entity.set("pageTemplateId", null);
        }
        
        @Override
        protected void setTemplate(CmsDataObject template) {
            if (!(template instanceof CmsPageTemplate)) throw new CmsException("CmsPageTemplateAssocAssoc requires a CmsPageTemplate, got: " 
                    + (template != null ? template.getClass().getName() : null));
            entity.set("pageTemplateId", template.getId());
        }
        
        @Override
        protected boolean hasTemplate() {
            return (this.entity.getString("pageTemplateId") != null);
        }
        
        
        protected void ensureTemplate(CmsPageTemplate template) {
            String curTemplateId = this.entity.getString("pageTemplateId");
            if (curTemplateId == null) {
                this.entity.set("pageTemplateId", template.getId());
            } else {
                if (!curTemplateId.equals(template.getId())) {
                    Debug.logWarning("Cms: Page template '" + template.getId()
                            + "' had in-memory CmsPageTemplateAssetAssoc association"
                            + " pointing to another template ('" + curTemplateId + "'); correcting", module);
                    this.entity.set("pageTemplateId", template.getId());
                }
            }
        }
        
        @Override
        public PageAssetTemplateAssocWorker getWorkerInst() {
            return PageAssetTemplateAssocWorker.worker;
        }
        
        public static PageAssetTemplateAssocWorker getWorker() {
            return PageAssetTemplateAssocWorker.worker;
        }

        public static class PageAssetTemplateAssocWorker extends DataObjectWorker<CmsPageTemplateAssetAssoc> {
            private static final PageAssetTemplateAssocWorker worker = new PageAssetTemplateAssocWorker();
            
            protected PageAssetTemplateAssocWorker() {
                super(CmsPageTemplateAssetAssoc.class);
            }

            @Override
            public CmsPageTemplateAssetAssoc makeFromValue(GenericValue value) throws CmsException {
                return new CmsPageTemplateAssetAssoc(value);
            }

            @Override
            public CmsPageTemplateAssetAssoc makeFromFields(Delegator delegator, Map<String, ?> fields) throws CmsException {
                return new CmsPageTemplateAssetAssoc(delegator, fields, null);
            }
            
            /**
             * Adds or updates a link to an asset template to this page template. Fields will be filtered.
             */
            public void createUpdatePageTemplateAssetAssoc(Delegator delegator, Map<String, ?> fields) throws CmsException {
                try {
                    String pageAssetTemplateAssocId = (String) fields.get("pageAssetTemplateAssocId");
                    if (UtilValidate.isNotEmpty(pageAssetTemplateAssocId)) {
                        CmsPageTemplateAssetAssoc assoc = this.findByIdAlways(delegator, pageAssetTemplateAssocId, false);
                        assoc.update(fields);
                        assoc.store();
                    } else {
                        CmsPageTemplateAssetAssoc assoc = makeFromFields(delegator, fields);
                        assoc.store();
                    }
                } catch (Exception e) {
                    throw new CmsException(String.format(
                            "Could not add or update asset template to page template. Page Template: %s Asset Template: %s: ", fields.get("pageTemplateId"),
                            fields.get("assetTemplateId")) + e.getMessage(), e);
                }
            }
        }
    }
    
    
    @Override
    protected CmsPageTemplateScriptAssoc.PageTemplateScriptAssocWorker getTemplateScriptAssocWorker() {
        return CmsPageTemplateScriptAssoc.getWorker();
    }
    
    public static class CmsPageTemplateScriptAssoc extends CmsTemplateScriptAssoc {

        private static final long serialVersionUID = -7223454711555662977L;

        protected CmsPageTemplateScriptAssoc(GenericValue entity) {
            super(entity);
        }
        
        public CmsPageTemplateScriptAssoc(Delegator delegator, Map<String, ?> fields, CmsScriptTemplate scriptTemplate) {
            super(delegator, fields, scriptTemplate);
        }

        protected CmsPageTemplateScriptAssoc(CmsPageTemplateScriptAssoc other, Map<String, Object> copyArgs) {
            super(other, copyArgs);
            // NOTE: don't bother clearing out the ID fields here, caller should handle
        }
        
        @Override    
        public void update(Map<String, ?> fields, boolean setIfEmpty) {
            super.update(fields, setIfEmpty);
        }
        
        @Override
        public CmsPageTemplateScriptAssoc copy(Map<String, Object> copyArgs) throws CmsException {
            return new CmsPageTemplateScriptAssoc(this, copyArgs);
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
            entity.set("pageTemplateId", null);
        }
        
        @Override
        protected void setTemplate(CmsDataObject template) {
            if (!(template instanceof CmsPageTemplate)) throw new CmsException("CmsPageTemplateScriptAssoc requires a CmsPageTemplate, got: " 
                    + (template != null ? template.getClass().getName() : null));
            entity.set("pageTemplateId", template.getId());
        }
        
        @Override
        protected boolean hasTemplate() {
            return (entity.get("pageTemplateId") != null);
        }
        
        @Override
        public PageTemplateScriptAssocWorker getWorkerInst() {
            return PageTemplateScriptAssocWorker.worker;
        }
        
        public static PageTemplateScriptAssocWorker getWorker() {
            return PageTemplateScriptAssocWorker.worker;
        }

        public static class PageTemplateScriptAssocWorker extends TemplateScriptAssocWorker<CmsPageTemplateScriptAssoc> {
            private static final PageTemplateScriptAssocWorker worker = new PageTemplateScriptAssocWorker();
            
            protected PageTemplateScriptAssocWorker() {
                super(CmsPageTemplateScriptAssoc.class);
            }

            @Override
            public CmsPageTemplateScriptAssoc makeFromValue(GenericValue value) throws CmsException {
                return new CmsPageTemplateScriptAssoc(value);
            }

            @Override
            public CmsPageTemplateScriptAssoc makeFromFields(Delegator delegator, Map<String, ?> fields) throws CmsException {
                return new CmsPageTemplateScriptAssoc(delegator, fields, null);
            }

            @Override
            protected CmsPageTemplateScriptAssoc makeFromFields(Delegator delegator, Map<String, ?> fields,
                    CmsScriptTemplate scriptTemplate) throws CmsException {
                return new CmsPageTemplateScriptAssoc(delegator, fields, scriptTemplate);
            }
        }
    }
    
    

    // Helpers
    
    @Override
    public PageTemplateRenderer getRenderer() {
        return renderer;
    }
    
    /**
     * Dedicated page renderer object.
     */
    public static class PageTemplateRenderer extends CmsRenderTemplate.TemplateRenderer<CmsPageTemplate> {

        private static final long serialVersionUID = 1761709448851808935L;

        public PageTemplateRenderer(CmsPageTemplate template) {
            super(template);
        }

        public static class PtRenderArgs extends RenderArgs {
            private boolean runPageScripts = false;
            
            public PtRenderArgs() {
                super();
            }

            public PtRenderArgs(Environment env, MapStack<String> context, CmsPageContent content,
                    CmsPageContext pageContext, boolean shareScope) {
                super(env, context, content, pageContext, shareScope);
            }

            public PtRenderArgs(Environment env, MapStack<String> context, CmsPageContent content,
                    CmsPageContext pageContext, Map<String, Object> earlyCtxVars, Map<String, Object> ovrdCtxVars,
                    boolean shareScope) {
                super(env, context, content, pageContext, earlyCtxVars, ovrdCtxVars, shareScope);
            }

            public PtRenderArgs(Writer out, Environment env, MapStack<String> context, CmsPageContent content,
                    CmsPageContext pageContext, Map<String, Object> earlyCtxVars, Map<String, Object> ovrdCtxVars,
                    boolean skipSystemCtx, boolean skipExtraCommonCtx, boolean shareScope) {
                super(out, env, context, content, pageContext, earlyCtxVars, ovrdCtxVars, skipSystemCtx, skipExtraCommonCtx, shareScope);
            }

            public PtRenderArgs(Writer out, MapStack<String> context, CmsPageContent content,
                    CmsPageContext pageContext, boolean shareScope) {
                super(out, context, content, pageContext, shareScope);
            }

            public PtRenderArgs(Writer out, MapStack<String> context, CmsPageContent content,
                    CmsPageContext pageContext, Map<String, Object> earlyCtxVars, Map<String, Object> ovrdCtxVars, boolean shareScope) {
                super(out, context, content, pageContext, earlyCtxVars, ovrdCtxVars, shareScope);
            }

            public boolean isRunPageScripts() {
                return runPageScripts;
            }
            public void setRunPageScripts(boolean runPageScripts) {
                this.runPageScripts = runPageScripts;
            }
        }
        
        @Override
        public Object processAndRender(RenderArgs renderArgs) throws CmsException {
            return super.processAndRender(renderArgs);
        }
        
        @Override
        protected List<CmsScriptTemplate> getSortedScriptTemplates() {
            return template.getSortedScriptTemplates();
        }
        
        @Override
        protected void populateScriptsAndContent(RenderArgs renderArgs, Set<String> contextSkipNames) {
            if (renderArgs instanceof PtRenderArgs) {
                // SPECIAL: run scripts associated to the CmsPage - before everything else
                if (((PtRenderArgs) renderArgs).isRunPageScripts()) {
                    CmsPage page = renderArgs.getPage();
                    if (page != null) {
                        populateScripts(page.getSortedScriptTemplates(), renderArgs, contextSkipNames);
                    }
                }
            }
            super.populateScriptsAndContent(renderArgs, contextSkipNames);
        }

    }
    
    @Override
    public PageTemplateWorker getWorkerInst() {
        return PageTemplateWorker.worker;
    }
    
    public static PageTemplateWorker getWorker() {
        return PageTemplateWorker.worker;
    }

    public static class PageTemplateWorker extends DataObjectWorker<CmsPageTemplate> {
        private static final PageTemplateWorker worker = new PageTemplateWorker();
        
        protected PageTemplateWorker() {
            super(CmsPageTemplate.class);
        }

        @Override
        public CmsPageTemplate makeFromValue(GenericValue value) throws CmsException {
            return new CmsPageTemplate(value);
        }

        @Override
        public CmsPageTemplate makeFromFields(Delegator delegator, Map<String, ?> fields) throws CmsException {
            return new CmsPageTemplate(delegator, fields);
        }
    }
    
    @Override
    protected PageVerComTemplateWorker getVerComTemplateWorkerInst() {
        return PageVerComTemplateWorker.templateWorker;
    }
    
    protected static PageVerComTemplateWorker getVerComTemplateWorker() {
        return PageVerComTemplateWorker.templateWorker;
    }
    
    public static class PageVerComTemplateWorker extends VerComTemplateWorker<CmsPageTemplate, CmsPageTemplateVersion> {

        protected static final PageVerComTemplateWorker templateWorker = new PageVerComTemplateWorker();
        
        @Override
        protected DataObjectWorker<CmsPageTemplate> getDataObjectWorker() {
            return CmsPageTemplate.getWorker();
        }
        
        @Override
        protected CmsPageTemplateVersion createVersion(GenericValue value, CmsPageTemplate template) {
            return new CmsPageTemplateVersion(value, template);
        }

        @Override
        protected CmsPageTemplate getTemplate(Delegator delegator, String templateId) {
            return getDataObjectWorker().findByIdAlways(delegator, templateId, false);
        }

        @Override
        protected ActiveVersionWorker<CmsPageTemplate, CmsPageTemplateVersion> getActiveVersionWorker() {
            return CmsPageTemplateVersion.CmsPageTemplateActiveVersionWorker.activeVersionWorker;
        }

        @Override
        protected String getVersionEntityName() {
            return "CmsPageTemplateVersion";
        }

        @Override
        protected String getTemplateIdFieldName() {
            return "pageTemplateId";
        }

        @Override
        protected CmsPageTemplate createTemplate(Delegator delegator, Map<String, ?> fields) {
            return new CmsPageTemplate(delegator, fields);
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
        public static final VisitRelPlan INSTANCE = new VisitRelPlan("CmsPageTemplate");
        static final VisitRelations visitRelations = INSTANCE.buildSafe();
        public VisitRelPlan(String majorEntityName) { super(majorEntityName); }
        @Override public VisitRelations.Builder planDefinition(Delegator delegator) throws Exception {
            return newBuilder(delegator)
                .entity("CmsPageTemplate")
                    .relation("CmsPageTemplateScriptAssoc")
                    .relation("CmsPageTemplateAssetAssoc")
                    .self()
                    .relation("CmsPageTemplateVersion")
                    .recall("TMPL_SCRASSOC")
                    .recall("TMPL_ASSETASSOC")
                    .relation("CmsAttributeTemplate")
                .entity("CmsPageTemplateVersion")
                    .self()
                    .relation("CmsPageTemplateVersionState")
                .entity("CmsPageTemplateScriptAssoc")
                    .relationMajor("CmsScriptTemplate")
                    .selfStash("TMPL_SCRASSOC")
                .entity("CmsPageTemplateAssetAssoc")
                    .relationMajor("CmsAssetTemplate")
                    .selfStash("TMPL_ASSETASSOC");
        }
    }
}
