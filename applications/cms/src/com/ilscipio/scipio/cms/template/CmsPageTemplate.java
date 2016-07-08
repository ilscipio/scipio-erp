package com.ilscipio.scipio.cms.template;

import java.io.IOException;
import java.io.Writer;
import java.util.Collections;
import java.util.Comparator;
import java.util.Date;
import java.util.List;
import java.util.Map;

import javolution.util.FastList;
import javolution.util.FastMap;

import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.UtilMisc;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.base.util.template.FreeMarkerWorker;
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.GenericEntityException;
import org.ofbiz.entity.GenericValue;
import org.ofbiz.widget.model.HtmlWidget.ExtendedWrapper;

import com.ilscipio.scipio.cms.CmsException;
import com.ilscipio.scipio.cms.content.CmsPageContent;
import com.ilscipio.scipio.cms.content.CmsPageContext;
import com.ilscipio.scipio.cms.data.CmsDataObject;

import freemarker.core.Environment;
import freemarker.template.Configuration;
import freemarker.template.TemplateDirectiveBody;
import freemarker.template.TemplateDirectiveModel;
import freemarker.template.TemplateException;
import freemarker.template.TemplateModel;
import freemarker.template.utility.DeepUnwrap;

/**
 * The page template determines the presentation of a page. It includes a
 * FreeMarker template and attribute definitions. Page templates also have zero
 * or more asset templates attached which can be embedded into page.
 * 
 */
public class CmsPageTemplate extends CmsTemplate {

    private boolean legacyTemplate = false; // True if a template created without versioning
    private String nextDisplayVersionBody = null; // This is set if the calling code wants to update the "current" template body for compatibility purposes
    private CmsPageTemplateVersion legacyTemplateVersion = null; // Set if a legacy template must be transferred to versioning upon first store
    
    // TODO: For the time being, always set the first version as active to preserve
    // backwards-compatibility with legacy template behaviour.
    private static final boolean IS_FIRST_VERSION_ACTIVE = true;
    
    static {
        fmTemplateLoader = new CmsTemplateLoader<CmsPageTemplate>();
        fmConfig = FreeMarkerWorker.makeConfiguration(new ExtendedWrapper(FreeMarkerWorker.version));
        fmConfig.setTemplateLoader(fmTemplateLoader);
    }
    

    public CmsPageTemplate(Map<String, ?> fields) {
        super(fields);
    }
    
    /**
     * A FreeMarker directive that inserts the content of an asset. The
     * directive is called with the import name of the asset. The asset is
     * evaluated as FreeMarker template with the content of the respective
     * variable as template model.
     * 
     * The method is called as follows: <@asset name="assetImportName" />
     * 
     */
    public static class AssetLoadDirective implements TemplateDirectiveModel {

        private CmsPageTemplate pageTemplate;
        private CmsPageContent pageContent;
        private CmsPageContext pageContext;
        private static final long serialVersionUID = 397081686734287491L;

        public static final String module = CmsPageTemplate.class.getName();

        public AssetLoadDirective(CmsPageTemplate template, CmsPageContent content, CmsPageContext context) {
            super();
            pageTemplate = template;
            pageContent = content;
            pageContext = context;
        }

        @SuppressWarnings({ "rawtypes", "unchecked" })
        @Override
        public void execute(Environment env, Map paramsUntyped, TemplateModel[] loopVars, TemplateDirectiveBody body)
                throws TemplateException, IOException {
            Map<String, TemplateModel> params = (Map<String, TemplateModel>) paramsUntyped;
            Writer out = env.getOut();
            String output = null;
            // first, check if import name was given
            if (!params.containsKey("name")) {
                if (pageContext.isPreview()) {
                    output = "<b>Asset include failed:</b> the name of the asset must be given as parameter \"name\"";
                } else {
                    Debug.logError("Asset include failed: the name of the asset must be given as parameter \"name\"", module);
                    throw new CmsException("Asset include failed: the name of the asset must be given as parameter \"name\"",
                            module);
                }
                // the name parameter is there, so let's try to render the asset
            } else {
                try {
                    // extract the asset name from freemarker template model
                    
                    String assetName = (String) DeepUnwrap.unwrap(params.get("name"));
                    // prepare content for asset
                    if(assetName != null || assetName != ""){
                    CmsAssetTemplate assetTemplate = pageTemplate.getActiveAssetTemplates().get(assetName);
                    CmsPageContent assetContent = pageContent.getAssetContent(assetName);
                    // render asset
                    output = assetTemplate.process(assetContent, pageContext);
                    }
                } catch (Exception e) {
                    // something went wrong, create an output if in preview mode
                    // or an exception if live
                    if (pageContext.isPreview()) {
                        output = "<b>Asset rendering failed:</b> please check asset name and"
                                + " validity of asset template. Please check with your supervisor if all variables have been set correctly. Error: " + e.getMessage();
                    } else {
                        Debug.logError(e, "An error occured while rendering an asset. Template: " + pageTemplate.getName(),
                                module);
                        throw new CmsException("An error occured while rendering an asset. Template: " + pageTemplate.getName(),
                                e, module);
                    }
                }
            }
            out.write(output);
        }
    }

    
    /**
     * Creates and stores a new template and any other required related entity values from the given fields.
     * 
     * @param delegator
     * @param fields
     * @return
     */
    public static CmsPageTemplate createStoreNewTemplate(Delegator delegator, Map<String, ?> fields) {
        // First create template record itself
        Map<String, Object> tmpFields = FastMap.newInstance();
        tmpFields.put("webSiteId", fields.get("webSiteId"));
        tmpFields.put("templateName", fields.get("templateName"));
        tmpFields.put("createdBy", fields.get("createdBy"));
        tmpFields.put("lastUpdatedBy", fields.get("lastUpdatedBy"));
        
        CmsPageTemplate newTmp = new CmsPageTemplate(tmpFields);
        
        try {
            newTmp.store();
        }
        catch(GenericEntityException e) {
            throw new CmsException("Entity could not be stored.", e, module);
        }
        
        // Then, add first version
        Map<String, Object> verFields = FastMap.newInstance();
        verFields.put("pageTemplateId", newTmp.getId());
        verFields.put("templateBody", fields.get("templateBody"));
        verFields.put("createdBy", fields.get("createdBy"));
        verFields.put("versionComment", "First"); // TODO: Localize
        
        newTmp.createStoreNewVersion(verFields, IS_FIRST_VERSION_ACTIVE);

        return newTmp;
    }
    
    

    /**
     * Adds a link to an asset template to this page template.
     * 
     * @param assetTemplateId
     * @param importName
     */
    public void addAssetTemplate(CmsAssetTemplate assetTemplate, String importName) {
        addAssetTemplate(assetTemplate.getId(), importName, 0L);
    }

    /**
     * Adds a link to an asset template to this page template.
     * 
     * @param assetTemplateId
     * @param importName
     * @param inputPositon
     */
    public void addAssetTemplate(CmsAssetTemplate assetTemplate, String importName, Long inputPosition) {
        addAssetTemplate(assetTemplate.getId(), importName, inputPosition);
    }

    /**
     * Adds a link to an asset template to this page template.
     * 
     * @param assetTemplateId
     * @param importName
     */
    public void addAssetTemplate(String assetTemplateId, String importName) {
        addAssetTemplate(assetTemplateId, importName, 0L);
    }

    /**
     * Adds a link to an asset template to this page template.
     * 
     * @param assetTemplateId
     * @param importName
     */
    public void addAssetTemplate(String assetTemplateId, String importName, Long inputPosition) {
        try {
            GenericValue assetAssoc = entity.getDelegator().makeValue("CmsPageAssetTemplateAssoc", "pageTemplateId",
                    this.getId(), "assetTemplateId", assetTemplateId, "importName", importName, "inputPosition", inputPosition);
            assetAssoc.setNextSeqId();
            entity.getDelegator().create(assetAssoc);

            // reset assets
            resetCachedAssetTemplates();
        } catch (GenericEntityException e) {
            throw new CmsException(String.format(
                    "Could not add asset template to page template. Page Tempalte: %s Asset Template: %s", this.getName(),
                    assetTemplateId), e, module);
        }
    }
    
    private void resetCachedAssetTemplates() {
        activeAssetTemplates = null;
        latestAssetTemplates = null;
        linkedAssetTemplates = null;
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
        Map<String, CmsAssetTemplate> assetTemplateEntries = getLinkedAssetTemplates();
        for (String name : assetTemplateEntries.keySet()) {
            CmsAssetTemplate assetTemplate = assetTemplateEntries.get(name);
            templateCopy.addAssetTemplate(assetTemplate, name, assetTemplate.getPosition());
        }
        
        // copy latest version
        try {
            if (this.isLegacyTemplate()) {
                // If it's a legacy template, do a legacy migration for the new one
                
                // Make sure the new template is marked as a legacy template for migration (should be already)
                templateCopy.legacyTemplate = true;
                
                // Trigger a store on the legacy version, which triggers the migration
                // Here the original createdBy field from the legacy template will get copied
                // transitively to the new version. We explicitly set the createdBy on the template copy
                // itself after this.
                // Here we also specify a specific original version date so this info is retained.
                Map<String, Object> overrideFields = UtilMisc.<String, Object> toMap("origVersionDate", this.getLastUpdatedDate());
                templateCopy.overrideGetLegacyTemplateAsVersion(false, overrideFields);
                templateCopy.commitLegacyTemplateMigration(false);
                
            }
            else {
                // Make sure the new template is NOT marked as a legacy template for migration
                // This happens because of a weird constructor invocation in the base classes that bypasses this class's constructor logic
                templateCopy.legacyTemplate = false;
                templateCopy.legacyTemplateVersion = null;
                
                CmsPageTemplateVersion lastVer = getLastVersion();
                
                if (lastVer != null) {
                    Map<String, Object> fields = lastVer.getRawDataAsMap();
                    fields.put("pageTemplateId", templateCopy.getId());
                    fields.put("versionId", null);
                    // Copy the original version date from the last template
                    fields.put("origVersionDate", lastVer.getVersionDate());
                    templateCopy.createStoreNewVersion(fields, IS_FIRST_VERSION_ACTIVE);
                }
            }
        }
        catch (GenericEntityException e) {
            throw new CmsException("Entity could not be stored.", e, module);
        }
        
        // In case we had
        // TODO: Remove this later (maybe?)
        // Since this is a new copy, no need to keep the old legacy body for safety, so clear it.
        // We do this in all cases because we could be copying a non-legacy template that was a legacy
        // one at some point.
        templateCopy.cleanupLegacyTemplate();
        
        
        // Store the name of the person who created the copy (which is NOT the same as the person who
        // created the original version)
        if (copyCreatorId != null) {
            templateCopy.setCreatedBy(copyCreatorId);
        }
        
        // Do a store because in most cases something will have changed after the self-copy
        try {
            templateCopy.store();
        }
        catch (GenericEntityException e) {
            throw new CmsException("Entity could not be stored.", e, module);
        }
        
        return templateCopy;
    }

    public Map<String, Object> getDescriptor() {
        Map<String, Object> descriptor = super.getDescriptor();
        // TODO: We currently get the "ACTIVE" asset templates as descriptors for CmsEditor; it's not clear
        // whether conceptually this should be the "ACTIVE" or "LATEST" templates being used here, though in
        // practice it may make no difference - establish this once and for all.
        List<CmsAssetTemplate> assetTemplates = new FastList<CmsAssetTemplate>(getActiveAssetTemplates().values());
        Collections.<CmsAssetTemplate> sort(assetTemplates, new Comparator<CmsAssetTemplate>() {
            @Override
            public int compare(CmsAssetTemplate first, CmsAssetTemplate second) {
                return first.getPosition().compareTo(second.getPosition());
            }

        });
        List<Map<String, Object>> atDescriptors = FastList.<Map<String, Object>> newInstance();
        for (CmsAssetTemplate at : assetTemplates) {
            atDescriptors.add(at.getDescriptor());
        }
        descriptor.put("assets", atDescriptors);
        return descriptor;
    }

    @Override
    protected CmsPageContent setDirectives(CmsPageContent content, CmsPageContext context) {
        content.put("asset", new AssetLoadDirective(this, content, context));
        content.put("link_to", new PageLinkDirective(context));
        return content;
    }


    

    /**
     * A map of asset templates with their importName as key.
     */
    private Map<String, CmsAssetTemplate> activeAssetTemplates;
    private Map<String, CmsAssetTemplate> latestAssetTemplates;
    private Map<String, CmsAssetTemplate> linkedAssetTemplates;
    
    private Map<String, CmsGroovyTemplate> groovyTemplates;

    /**
     * Constructs new CmsPageTemplate from the given entity.
     *
     * @param entity
     * @param isNewTemplate set to true if entity represents a new, partially-constructed
     *        template; set to false if entity represents an existing template in the data source.
     */
    private CmsPageTemplate(GenericValue entity, boolean isNewTemplate) {
        super(entity);
        if (!isNewTemplate) {
            verifyLegacyTemplate();
        }
    }
    
    public CmsPageTemplate(GenericValue entity) {
        this(entity, false);
    }
    

    public CmsPageTemplate(String primaryKey) {
        super(primaryKey);
        verifyLegacyTemplate();
    }
    
    public static Map<String, Object> getPageTemplateAsMap(Delegator delegator, String pageTemplateId) {
        CmsPageTemplate pageTmp = new CmsPageTemplate(pageTemplateId);
        Map<String, Object> result = FastMap.newInstance();
        pageTmp.putTemplateFieldsIntoMap(result);
        return result;
    }
    
    
    private boolean hasTemplateVersions() {
        List<CmsPageTemplateVersion> allVersions = CmsPageTemplateVersion.findAllNonLegacyTemplateVersions(
                entity.getDelegator(), this.getId(), this);
        return UtilValidate.isNotEmpty(allVersions);
    }
    
    private void verifyLegacyTemplate() {
        this.legacyTemplate = !hasTemplateVersions();
    }

    boolean isLegacyTemplate() {
        return legacyTemplate;
    }
    
    boolean isNewPageTemplate() {
        // For now, use a heuristic to determine if this is a new page template
        return !hasTemplateVersions() && !isLegacyTemplate();
    }
    
    CmsPageTemplateVersion getLegacyTemplateAsVersion() {
        if (legacyTemplateVersion == null) {
            return overrideGetLegacyTemplateAsVersion(true, null);
        }
        else {
            return legacyTemplateVersion;
        }
    }
    
    private CmsPageTemplateVersion overrideGetLegacyTemplateAsVersion(boolean preserveOriginalDate, 
            Map<String, ?> overrideFields) {
        
        Map<String, Object> fields = FastMap.newInstance();
        fields.put("templateBody", getLegacyBody());
        fields.put("versionComment", "First"); // TODO: Localize
        fields.put("pageTemplateId", getId());
        fields.put("createdBy", getCreatedBy());
        
        if (preserveOriginalDate) {
            // Here we force an original version date because the createdStamp/lastUpdatedStamp
            // won't reflect the last time the legacy template was edited.
            fields.put("origVersionDate", getLastUpdatedDate());
        }
        
        if (overrideFields != null) {
            fields.putAll(overrideFields);
        }
        
        // Always buffer this
        legacyTemplateVersion = new CmsPageTemplateVersion(fields, this, true);
        
        return legacyTemplateVersion;
    }
    

    
    
    
    private Date getLastUpdatedDate() {
        return (Date) entity.get("lastUpdatedStamp");
    }
    
    private void prepareLegacyTemplateMigration() {
        if (isLegacyTemplate() && legacyTemplateVersion == null) {
            getLegacyTemplateAsVersion();
        }
    }
    
    boolean commitLegacyTemplateMigration(boolean legacyVersionStoreInProgress) throws GenericEntityException {
        boolean result = true;
        if (isLegacyTemplate()) {
            CmsPageTemplateVersion toMigrate = getLegacyTemplateAsVersion();
            legacyTemplate = false; // Important - must set before call; otherwise, endless loop!
            legacyTemplateVersion = null;
            if (!legacyVersionStoreInProgress) {
                result = toMigrate.store();
            }
            // TODO: For now, don't clean up the legacy template - keep the legacy templateBody just in case.
            //cleanupLegacyTemplate();
            //this.store();
        }
        return result;
    }
    
    boolean cleanupLegacyTemplate() {
        boolean result = true;
        setLegacyBody(""); // Clear the legacy body          
        return result;
    }
    

    /**
     * Creates a new version for this page. Caller stores.
     * <p>
     * Note: If creating a new version on a legacy template (no versioning), the template
     * is converted by CmsPageTemplateVersion.store().
     * 
     * @param fields the entity fields to initialize the new version to (optional)
     * @return
     */
    public CmsPageTemplateVersion createNewVersion(Map<String, ?> fields) {
        prepareLegacyTemplateMigration(); // Must do this before the first version to preserve version ID order
        return new CmsPageTemplateVersion(fields, this);
    }
    
    public CmsPageTemplateVersion createStoreNewVersion(Map<String, ?> fields, boolean setAsActive) {
        CmsPageTemplateVersion newVer = createNewVersion(fields);
        if (setAsActive) {
            newVer.setAsActiveVersion();
        }
        try {
            newVer.store();
        }
        catch(GenericEntityException e) {
            throw new CmsException("Entity could not be stored.", e, module);
        }
        return newVer;
    }
    
    
    
    /** 
     * Returns the page template body of the currently active version or
     * the legacy template body if a legacy template.
     * 
     * @see org.CmsTemplate.cms.template.CmsTemplate#getBody()
     */
    @Override
    public String getBody() {
        return getDisplayVersionBody();
    }
    
    /** 
     * Sets the body on the active template version only. Generally deprecated for this class.
     * 
     * @see org.CmsTemplate.cms.template.CmsTemplate#setBody(java.lang.String)
     */
    @Override
    public void setBody(String body) {
        setDisplayVersionBody(body);
    }

    /**
     * Returns template body for legacy templates that were created without the versioning system.
     * 
     * @return
     */
    public String getLegacyBody() {
        return super.getBody();
    }
    
    public void setLegacyBody(String body) {
        super.setBody(body);
    }
    
    /**
     * Returns the template body of the version currently set for display.
     * <p>
     * Preserves backwards-compatibility with templates that had no versioning.
     * 
     * @return
     */
    public String getDisplayVersionBody() {
        String result = null;
        
        // Try active version first (will return legacy version if need be)
        CmsPageTemplateVersion activeVersion = getActiveVersion();
        
        if (activeVersion != null) {
            result = activeVersion.getTemplateBody();
        }
        
        return result;
    }
    
    /**
     * Sets the body on the display version. Generally deprecated and should not be used
     * except for backward-compatibility.
     */
    public void setDisplayVersionBody(String templateBody) {
        
        if (isLegacyTemplate()) {
            setLegacyBody(templateBody);
        }
        else {
            /* Do this only upon store
            CmsPageTemplateVersion activeVersion = getActiveVersion();
            
            // Note: For legacy templates, there would never be an inactive version.
            if (activeVersion != null) {
                activeVersion.setTemplateBody(templateBody);
            }
            */
            this.nextDisplayVersionBody = templateBody;
        }
    }
    
    
    public CmsPageTemplateVersion getLastVersion() {
        if (isLegacyTemplate()) {
            return getLegacyTemplateAsVersion();
        }
        else {
            return CmsPageTemplateVersion.findLastTemplateVersion(entity.getDelegator(), this);
        }
    }
    
    public CmsPageTemplateVersion getFirstVersion() {
        if (isLegacyTemplate()) {
            return getLegacyTemplateAsVersion();
        }
        else {
            return CmsPageTemplateVersion.findFirstTemplateVersion(entity.getDelegator(), this);
        }
    }
    
    public CmsPageTemplateVersion getLastOrNewVersion() {
        CmsPageTemplateVersion result = getLastVersion();
        if (result == null) {
            result = new CmsPageTemplateVersion(FastMap.<String, Object> newInstance(), this);
        }
        return result;
    }
    
    public CmsPageTemplateVersion getActiveVersion() {
        if (isLegacyTemplate()) {
            return getLegacyTemplateAsVersion();
        }
        else {
            return CmsPageTemplateVersion.findActiveTemplateVersion(entity.getDelegator(), this);
        }
    }
    
    public CmsPageTemplateVersion getActiveOrNewVersion() {
        CmsPageTemplateVersion result = getActiveVersion();
        if (result == null) {
            result = new CmsPageTemplateVersion(FastMap.<String, Object> newInstance(), this);
        }
        return result;
    }
    
    public CmsPageTemplateVersion getVersion(String versionId) {
            return CmsPageTemplateVersion.findSpecificTemplateVersion(entity.getDelegator(), this, versionId);
    }
    
    public List<CmsPageTemplateVersion> getAllVersions() {
        if (isLegacyTemplate()) {
            List<CmsPageTemplateVersion> result = FastList.newInstance();
            result.add(getLegacyTemplateAsVersion());
            return result;
        }
        else {
            return CmsPageTemplateVersion.findAllTemplateVersions(entity.getDelegator(), this);
        }
    }
    
    public void putTemplateFieldsIntoMap(Map<String, Object> in) {
        in.putAll(entity);
        // Special overrides
        
        // Legacy body for legacy code
        in.put("templateBody", getDisplayVersionBody());
    }
    
    public void updateTemplateFields(Map<String, ?> fields) {
        entity.setAllFields(fields, false, null, false);
    }
    
    
    private enum AssetTemplateStatus {
        ACTIVE,
        LATEST,
        LINKED
    }
    
    /**
     * Returns a map of asset templates linked to this page template of the specified status. The map
     * key is the import name under which this asset is available in the
     * freemarker source code of this page template.
     * 
     * @return Map of asset templates
     */
    private Map<String, CmsAssetTemplate> getAssetTemplatesWithStatus(AssetTemplateStatus assetTmpStatus) {
        Map<String, CmsAssetTemplate> result = new FastMap<String, CmsAssetTemplate>();

        try {
            // TODO: Ordering of assets
            List<GenericValue> assocEntities = entity.getRelated("CmsPageAssetTemplateAssoc");
            CmsAssetTemplate asset;
            for (GenericValue assoce : assocEntities) {
                GenericValue asse = assoce.getRelatedOne(CmsAssetTemplate.class.getSimpleName());

                asset = new CmsAssetTemplate(asse);
                
                if (assetTmpStatus == AssetTemplateStatus.ACTIVE) {
                    // New filter: the CmsAssetTemplate values associated to the CmsPageTemplate instance
                    // in the DB typically use IDs of old/inactive versions of asset templates (known schema limitation); here must
                    // use an extra lookup to make sure we return the active asset template version.
                    asset = asset.getActiveVersionOfSelf();
                }
                else if (assetTmpStatus == AssetTemplateStatus.LATEST) {
                    asset = asset.getLatestVersionOfSelf();
                }
                else {
                    // AssetTemplateType.LINKED
                }

                asset.setPosition(assoce.getLong("inputPosition") != null ? assoce.getLong("inputPosition") : 0L);
                asset.setImportName(assoce.getString("importName") != null ? assoce.getString("importName") : null);
                result.put(assoce.getString("importName"), asset);
            }
        } catch (GenericEntityException e) {
            throw new CmsException("Attribute templates could not be retrieved for page template: " + getName(), e, module);
        }
        return result;
    }
    
    /**
     * Returns a map of active (not necessarily latest) asset templates linked to this page template. The map
     * key is the import name under which this asset is available in the
     * freemarker source code of this page template.
     * 
     * @return Map of asset templates
     */
    public Map<String, CmsAssetTemplate> getActiveAssetTemplates() {
        if (activeAssetTemplates == null) {
            activeAssetTemplates = getAssetTemplatesWithStatus(AssetTemplateStatus.ACTIVE);
        }
        return activeAssetTemplates;
    }
    
    /**
     * Returns a map of latest (not necessarily active) asset templates linked to this page template. The map
     * key is the import name under which this asset is available in the
     * freemarker source code of this page template.
     * 
     * @return Map of asset templates
     */
    public Map<String, CmsAssetTemplate> getLatestAssetTemplates() {
        if (latestAssetTemplates == null) {
            latestAssetTemplates = getAssetTemplatesWithStatus(AssetTemplateStatus.LATEST);
        }
        return latestAssetTemplates;
    }
    
    /**
     * Returns a map of the directly-linked asset templates linked to this page template. The map
     * key is the import name under which this asset is available in the
     * freemarker source code of this page template.
     * 
     * @return Map of asset templates
     */
    public Map<String, CmsAssetTemplate> getLinkedAssetTemplates() {
        if (linkedAssetTemplates == null) {
            linkedAssetTemplates = getAssetTemplatesWithStatus(AssetTemplateStatus.LINKED);
        }
        return linkedAssetTemplates;
    }
    
    
    /**
     * Returns a map of groovy templates linked to this page template. The map
     * key is the import name under which this asset is available in the
     * freemarker source code of this page template.
     * 
     * 
     * @return Map of groovy templates
     */

    public Map<String, CmsGroovyTemplate> getGroovyTemplates() {
        if (groovyTemplates == null) {
            groovyTemplates = new FastMap<String, CmsGroovyTemplate>();
            try {
                // TODO: Ordering of assets
                List<GenericValue> assocEntities = entity.getRelated("CmsPageTemplateGroovyAssoc",UtilMisc.toList("inputPosition"));
                CmsGroovyTemplate groovy;
                for (GenericValue assoce : assocEntities) {
                    GenericValue asse = assoce.getRelatedOne(CmsGroovyTemplate.class.getSimpleName());

                    groovy = new CmsGroovyTemplate(asse);

                    groovy.setInputPosition(assoce.getLong("inputPosition") != null ? assoce.getLong("inputPosition") : 0L);
                    groovy.setTemplateName(asse.getString("templateName") != null ? asse.getString("templateName") : null);
                    groovyTemplates.put(asse.getString("templateName"), groovy);
                }
            } catch (GenericEntityException e) {
                throw new CmsException("Attribute templates could not be retrieved for page template: " + getName(), e, module);
            }
        }
        return groovyTemplates;
    }

    
    
    /**
     * Explicitly Stores only the template record. For special circumstances.
     * 
     * @return
     * @throws GenericEntityException
     */
    boolean storeSelfOnly() throws GenericEntityException {
        return super.store();
    }
    
    /**
     * Stores the page template record itself. For the time being, this does not store any
     * related entities. 
     *  
     *  (non-Javadoc)
     * @see org.CmsDataObject.cms.data.CmsDataObject#store()
     */
    @Override
    public boolean store() throws GenericEntityException {
        // Store self
        boolean selfStoreRes = super.store();
        
        // If a "legacy" body store was requested, do it now
        boolean legBodyRes = true;
        if (nextDisplayVersionBody != null) {
            CmsPageTemplateVersion activeVersion = getActiveVersion();
            // Note: For legacy templates, there would never be an inactive version.
            if (activeVersion != null) {
                activeVersion.setTemplateBody(nextDisplayVersionBody);
                try {
                    legBodyRes = activeVersion.store();
                }
                catch(GenericEntityException e) {
                    throw new CmsException("Entity could not be stored.", e, module);
                }
            }
            nextDisplayVersionBody = null;
        }
        return selfStoreRes && legBodyRes;
    }

    /**
     * Removes the page template from the database.
     * 
     * @return True if delete was successful, otherwise false
     * @throws GenericEntityException
     */
    @Override
    public boolean remove() {
        int rowsAffected = 0;
        try {
            // delete template versions
            CmsPageTemplateVersion.removeAllTemplateVersions(entity.getDelegator(), getId());
            
            // delete CmsPageAssetTemplateAssoc
            List<GenericValue> pageAssetAssoc = entity.getRelated("CmsPageAssetTemplateAssoc");
            for (GenericValue pageAsset : pageAssetAssoc) {
                pageAsset.remove();
            }
            // delete CmsAttributeTemplate
            List<GenericValue> attributesTemplate = entity.getRelated("CmsAttributeTemplate");
            for (GenericValue attributeTemplate : attributesTemplate) {
                attributeTemplate.remove();
            }
            rowsAffected = getDelegator().removeValue(entity, true);
        } catch (GenericEntityException e) {
            throw new CmsException("Entity could not be removed.", e, module);
        }
        return rowsAffected > 0 ? true : false;
    }

    /**
     * Returns the template with the given name within a website.
     * 
     * @param templateName
     * @param webSiteId
     * @return template
     */
    public static CmsPageTemplate findByName(String templateName, String webSiteId) {
        return (CmsPageTemplate) CmsDataObject.findFirst(UtilMisc.toMap("templateName", templateName, "webSiteId", webSiteId),
                CmsPageTemplate.class);
    }

    /**
     * Returns all page templates of a website.
     * 
     * @param webSiteId
     * @return
     */
    public static List<CmsPageTemplate> findByWebSiteId(String webSiteId) {
        return CmsDataObject.<CmsPageTemplate> findAll(UtilMisc.toMap("webSiteId", webSiteId), CmsPageTemplate.class,UtilMisc.toList("templateName ASC"));
    }
}
