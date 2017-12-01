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
import java.util.Set;

import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.UtilMisc;
import org.ofbiz.base.util.UtilProperties;
import org.ofbiz.base.util.collections.MapStack;
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.GenericEntityException;
import org.ofbiz.entity.GenericValue;

import com.ilscipio.scipio.ce.util.Optional;
import com.ilscipio.scipio.cms.CmsException;
import com.ilscipio.scipio.cms.CmsInputException;
import com.ilscipio.scipio.cms.content.CmsPage;
import com.ilscipio.scipio.cms.content.CmsPageContent;
import com.ilscipio.scipio.cms.content.CmsPageContext;
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
    
    public static final String module = CmsPageTemplate.class.getName();
    
    // default is TRUE
    private static final boolean firstVersionActive = UtilProperties.getPropertyAsBoolean("cms.properties", "page.template.firstVersionActive", true);

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
    
    /**
     * Adds a link to an asset template to this page template.
     */
    public void addAssetTemplate(CmsAssetTemplate assetTemplate, String importName) {
        addAssetTemplate(assetTemplate.getId(), importName, 0L);
    }

    /**
     * Adds a link to an asset template to this page template.
     */
    public void addAssetTemplate(CmsAssetTemplate assetTemplate, String importName, Long inputPosition) {
        addAssetTemplate(assetTemplate.getId(), importName, inputPosition);
    }

    /**
     * Adds a link to an asset template to this page template.
     */
    public void addAssetTemplate(String assetTemplateId, String importName) {
        addAssetTemplate(assetTemplateId, importName, 0L);
    }

    /**
     * Adds a link to an asset template to this page template.
     */
    public void addAssetTemplate(String assetTemplateId, String importName, Long inputPosition) {
        addAssetTemplate(UtilMisc.toMap("assetTemplateId", assetTemplateId, "importName", importName, "inputPosition", inputPosition));
    }
    
    /**
     * Adds a link to an asset template to this page template. Fields will be filtered.
     */
    public void addAssetTemplate(Map<String, ?> fields) {
        if (fields.containsKey("pageAssetTemplateAssocId")) {
            throw new IllegalArgumentException("addAssetTemplate should not get pageAssetTemplateAssocId");
        }
        addUpdateAssetTemplate(fields);
    }
    
    /**
     * Adds or updates a link to an asset template to this page template. Fields will be filtered.
     */
    public void addUpdateAssetTemplate(Map<String, ?> fields) {
        try {
            if (fields.containsKey("pageAssetTemplateAssocId")) {
                String pageAssetTemplateAssocId = (String) fields.get("pageAssetTemplateAssocId");
                GenericValue assetAssoc = getDelegator().findOne("CmsPageTemplateAssetAssoc", 
                        UtilMisc.toMap("pageAssetTemplateAssocId", pageAssetTemplateAssocId), false);
                if (assetAssoc == null) {
                    throw new CmsInputException("CmsPageTemplateAssetAssoc not found for pageAssetTemplateAssocId: " + pageAssetTemplateAssocId);
                }
                assetAssoc.setNonPKFields(fields, false);
                assetAssoc.store();
            } else {
                Map<String, Object> flds = new HashMap<>(fields);

                flds.put("pageTemplateId", this.getId());
                if (flds.get("inputPosition") == null) {
                    flds.put("inputPosition", 0L);
                }
                GenericValue assetAssoc = getDelegator().makeValidValue("CmsPageTemplateAssetAssoc", flds);
                getDelegator().createSetNextSeqId(assetAssoc);
            }

            // reset assets
            assetTemplatesByImportName = null;
        } catch (GenericEntityException e) {
            throw new CmsException(String.format(
                    "Could not add or update asset template to page template. Page Template: %s Asset Template: %s: ", this.getName(),
                    fields.get("assetTemplateId")) + e.getMessage(), e);
        }
    }

    /**
     * Copies this page template including all linked products and the latest
     * version.
     */
    @Override
    public CmsPageTemplate copy() {
        return copy(null);
    }
    
    /**
     * Copies this page template including all linked products and the latest
     * version.
     */
    public CmsPageTemplate copy(String copyCreatorId) {
        // copy the template itself
        CmsPageTemplate templateCopy = (CmsPageTemplate) super.copy();
        // copy asset templates
        Map<String, CmsAssetTemplate> assetTemplateEntries = getAssetTemplatesByImportName();
        for (String name : assetTemplateEntries.keySet()) {
            CmsAssetTemplate assetTemplate = assetTemplateEntries.get(name);
            templateCopy.addAssetTemplate(assetTemplate, name, assetTemplate.getInputPosition());
        }
        
        // This happens because of a weird constructor invocation in the base classes that bypasses this class's constructor logic        
        CmsPageTemplateVersion lastVer = getLastVersion();
        
        if (lastVer != null) {
            Map<String, Object> fields = lastVer.getRawDataAsMap();
            fields.put("pageTemplateId", templateCopy.getId());
            fields.put("versionId", null);
            // Copy the original version date from the last template
            fields.put("origVersionDate", lastVer.getVersionDate());
            templateCopy.createAndStoreNewVersion(fields, firstVersionActive);
        }

        
        // Store the name of the person who created the copy (which is NOT the same as the person who
        // created the original version)
        if (copyCreatorId != null) {
            templateCopy.setCreatedBy(copyCreatorId);
        }
        
        // Do a store because in most cases something will have changed after the self-copy
        templateCopy.store();
        
        return templateCopy;
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
    
    public CmsAssetTemplate getAssetTemplateById(String id) {
        // FIXME: slow!
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
            super(delegator, fields, assetTemplate);
        }

        protected CmsPageTemplateAssetAssoc(GenericValue entity) {
            super(entity);
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
