package com.ilscipio.scipio.cms.content;

import java.io.IOException;
import java.io.Serializable;
import java.io.Writer;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.ListIterator;
import java.util.Locale;
import java.util.Map;
import java.util.Set;

import javax.servlet.http.HttpServletRequest;

import org.ofbiz.base.lang.JSON;
import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.UtilMisc;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.base.util.collections.MapStack;
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.GenericEntityException;
import org.ofbiz.entity.GenericValue;
import org.ofbiz.entity.condition.EntityCondition;
import org.ofbiz.entity.condition.EntityOperator;
import org.ofbiz.service.LocalDispatcher;

import com.ilscipio.scipio.ce.util.Optional;
import com.ilscipio.scipio.cms.CmsException;
import com.ilscipio.scipio.cms.CmsUtil;
import com.ilscipio.scipio.cms.control.CmsControlUtil;
import com.ilscipio.scipio.cms.control.CmsProcessMapping;
import com.ilscipio.scipio.cms.control.CmsViewMapping;
import com.ilscipio.scipio.cms.data.CmsDataException;
import com.ilscipio.scipio.cms.data.CmsDataObject;
import com.ilscipio.scipio.cms.data.CmsDataUtil;
import com.ilscipio.scipio.cms.data.CmsEntityVisit;
import com.ilscipio.scipio.cms.data.CmsEntityVisit.CmsEntityVisitor;
import com.ilscipio.scipio.cms.data.CmsEntityVisit.VisitRelation;
import com.ilscipio.scipio.cms.data.CmsEntityVisit.VisitRelations;
import com.ilscipio.scipio.cms.data.CmsMajorObject;
import com.ilscipio.scipio.cms.data.CmsObjectCache;
import com.ilscipio.scipio.cms.data.CmsObjectCache.CacheEntry;
import com.ilscipio.scipio.cms.template.CmsComplexTemplate;
import com.ilscipio.scipio.cms.template.CmsMasterComplexTemplate;
import com.ilscipio.scipio.cms.template.CmsMasterComplexTemplate.CmsTemplateScriptAssoc;
import com.ilscipio.scipio.cms.template.CmsPageTemplate;
import com.ilscipio.scipio.cms.template.CmsPageTemplate.PageTemplateRenderer.PtRenderArgs;
import com.ilscipio.scipio.cms.template.CmsScriptTemplate;

/**
 * Represents a CMS page.
 * <p>
 * 2016: IMPORTANT: If you add any cached fields, you MUST update the {@link #preloadContent} method.
 */
public class CmsPage extends CmsDataObject implements CmsMajorObject {
    
    private static final long serialVersionUID = -6442528536238200118L;

    public static final String module = CmsPage.class.getName();

    /**
     * NOTE: 2016: page cache is almost completely covered by CmsProcess/ViewMapping; 
     * the only exception is for the defaultCmsPageId in CmsScreenViewHandler.
     * Leaving this one in place because of potential uses.
     */
    private static final CmsObjectCache<CmsPage> idCache = CmsObjectCache.getGlobalCache("cms.content.page.id");
    
    public static final UserRole DEFAULT_USER_ROLE = UserRole.CMS_VISITOR;
    
    // NOTE: 2016: Optional is required for thread safety (preload)
    private Optional<CmsPageTemplate> template = null;
    private CmsPageContent activeContentModel = null; // don't need Optional because once get called, can't be null
    private Map<String, Map<String, ?>> products = null;
    
    private Optional<CmsPageVersion> lastVersion = null; // NOTE: NOT cached when live.
    private Optional<CmsPageVersion> activeVersion = null; // NOTE: NOT cached when live.
    
    private String activeVersionId = null; // NOTE: NOT cached when live. // 2016: this is no longer stored on CmsPage. NOTE: empty string "" means cache checked OR unset active upon store().
    
    private Set<String> candidateWebSiteIds = null; // NOTE: NOT cached when live.
    
    /**
     * 2017-11-29: new pages will have their primary process mapping set active false,
     * then first publish operation will then toggle it to true.
     */
    static final String newPagePrimaryProcessMappingActive = "N";
    
    /**
     * 2016: new backpointers to process mappings that act as "primary" ones for this page.
     * NOTE: 2016-11: currently there should usually be only one per webSiteId (sourceWebSiteId).
     * <p>
     * WARN: 2016-12: due to timings, it is now possible that the CmsProcessMapping that caused us to render
     * the page does not appear in the list here. we must accept this otherwise there is no way
     * to safely exploit the CmsPage.pageId global cache.
     */
    private Map<String, List<CmsProcessMapping>> primaryProcessMappingsByWebSiteId = null;
    
    protected final PageRenderer renderer = new PageRenderer(this);
    
    /**
     * A list of script templates (assocs) sorted by inputPosition.
     * <p>
     * WARN: because we store CmsScriptTemplate and not CmsScriptTemplateAssoc, this has implications
     * for preloadContent().
     * <p>
     * NOTE: for CmsPage we can't extend CmsMasterComplexTemplate so we compose using its helpers instead,
     * works out okay.
     */
    protected List<CmsScriptTemplate> sortedScriptTemplates;
    
    /**
     * This constructor creates a new CmsPage instance from a prefilled
     * GenericValue. It should be ensured that the GenericValue is valid, no
     * further validation takes place.
     * NOTE: this is called through java reflection.
     * 
     * @param entity GenericValue of an CmsPage entity
     */
    protected CmsPage(GenericValue entity) {
        super(entity);
    }
    
    /**
     * Creates a new CmsPage from a field map.
     * NOTE: 2016: this does not perform create or store operation.
    */
    public CmsPage(Delegator delegator, Map<String, ?> fields) {
        super(delegator, fields);
        // TODO: REVIEW: 2017-11-29: this was in duplicate with createAndStoreWithPrimaryProcessMapping,
        // I don't think it should be here and hopefully nothing was relying on this
        //this.setPrimaryProcessMappingFields(fields); 
    } 
    
    protected CmsPage(CmsPage other, Map<String, Object> copyArgs) {
        super(other, copyArgs);
        this.sortedScriptTemplates = CmsMasterComplexTemplate.copyScriptTemplateAssocs(other.getSortedScriptTemplates(), copyArgs);
        // future? current code for this is invalid (must not store)
//        Map<String, Map<String, ?>> productEntries = other.getProducts();
//        for (String name : productEntries.keySet()) {
//            Map<String, ?> product = productEntries.get(name);
//            this.addProduct((String) product.get("productId"), name);
//        }
    }
    
    public static CmsPage createAndStore(Delegator delegator, Map<String, ?> fields) {
        CmsPage page = new CmsPage(delegator, fields);
        page.store();
        return page;
    }
    
    public static CmsPage createAndStoreWithPrimaryProcessMapping(Delegator delegator, Map<String, ?> fields) {
        CmsPage page = new CmsPage(delegator, fields);  
        page.setPrimaryProcessMappingFields(fields, true); 
        page.store();
        return page;
    }
    
    @Override    
    public void update(Map<String, ?> fields, boolean setIfEmpty) {
        super.update(fields, setIfEmpty);
        this.setPrimaryProcessMappingFields(fields, setIfEmpty); 
    }
    
    /**
     * Copies this page including all linked products and the latest or requested version.
     * Caller must call store().
     * <p>
     * NOTE: the page is not published immediately - so both the active flag
     * on primary process mapping is N and there is no active version decided.
     */
    @Override
    public CmsPage copy(Map<String, Object> copyArgs) {
        CmsPage newPage = new CmsPage(this, copyArgs);
        // NOTE: the service puts primaryPath and webSiteId in the copyArgs
        newPage.setPrimaryProcessMappingFields(copyArgs, true);
        copyInitialVersionToPageCopy(newPage, copyArgs);
        return newPage;
    }

    /**
     * Copies the version specified in copyArgs from <code>this</code> to newPage and assigns it
     * as its lastVersion member (which will get picked up by {@link #store()} later).
     */
    protected void copyInitialVersionToPageCopy(CmsPage newPage, Map<String, Object> copyArgs) {
        // NOTE: here <code>this</code> is page we're copying from
        CmsPageVersion newVersion = newPage.copyOtherVersion(this, copyArgs);
        // NOTE: for new pages we do NOT set active version, user must verify
        // operation is good before publishing
        //newVersion.setActive...
        newPage.setLastVersion(newVersion);
    }
    
    /**
     * Creates an in-memory copy of the specified (in copyArgs) or last version 
     * from the <code>other</code> instance, the result then associated to <code>this</code> instance, 
     * using any config in copyArgs.
     * NOTE: this swaps <code>this</code> (because it was originally in the copy constructor).
     */
    protected CmsPageVersion copyOtherVersion(CmsPage other, Map<String, Object> copyArgs) {
        return copyOtherVersion(other.getVersionForCopyAndVerify(copyArgs), copyArgs);
    }
    
    protected CmsPageVersion copyOtherVersion(CmsPageVersion otherVersion, Map<String, Object> copyArgs) {
        CmsPageVersion newVersion = otherVersion.copy(copyArgs, this);
        
        // redundant?
        //// Copy the original version date from the last template
        //newVersion.setOriginalVersionDate(otherVersion.getVersionDate());
        
        return newVersion;
    }
    
    /**
     * 2016: Loads ALL this page's content and products into the current instance.
     * <p>
     * WARN: IMPORTANT: AFTER THIS CALL, 
     * NO FURTHER CALLS ARE ALLOWED TO MODIFY THE INSTANCE IN MEMORY
     * (EVEN if the instance is not physically made immutable!).
     * Essential for thread safety!!!
     */
    @Override
    public void preload(PreloadWorker preloadWorker) {
        super.preload(preloadWorker);
        this.products = preloadWorker.transformContainer(getProducts());
        // NOTE: active version is NOT cached when live - we build content model from it and then ditch it
        //preloadWorker.preload(getActiveVersion());
        preloadWorker.preload(getActiveContentModel(false));
        preloadWorker.preload(getTemplate());
        this.primaryProcessMappingsByWebSiteId = preloadWorker.preloadDeepListMap(getPrimaryProcessMappingsByWebSiteIdInternal());
        this.sortedScriptTemplates = preloadWorker.preloadDeep(this.getSortedScriptTemplates());
    }
    
    @Override
    protected void verifyNewFields(Delegator delegator, Map<String, Object> fields, boolean isNew) throws CmsException {
        // FIXME?: this will NOT verify the name across its multiple possible primary process mappings
        // it currently is only verifying against CmsPage.webSiteId
        // this may be an issue in the future
        verifyUniqueName(delegator, fields, isNew, "pageName", true, "webSiteId", false, true);
    }
    
    @Override
    public void store() throws CmsException {
        preventIfImmutable();
        super.store();

        if (lastVersion != null && lastVersion.isPresent()) {
            CmsPageVersion lastVer = lastVersion.get();
            // TODO: REVIEW: could remove this condition, and also store activeVersion?
            // unclear if will cause problems anywhere.
            // For now this detects copy and change only.
            if (lastVer.hasChangedOrNoId() || lastVer.getEntityPageId() == null) { 
                lastVer.store();
            }
        }
        
        // update active version record
        if (this.activeVersionId != null) {
            if (UtilValidate.isNotEmpty(this.activeVersionId)) {
                CmsPageVersion.activeVersionWorker.createOrUpdateRecord(getDelegator(), getId(), this.activeVersionId);
            } else {
                // remove the record
                CmsPageVersion.activeVersionWorker.removeRecord(getDelegator(), getId());
            }
        }
        
        if (UtilValidate.isNotEmpty(this.primaryProcessMappingsByWebSiteId)) {
            for(List<CmsProcessMapping> mappingList : this.primaryProcessMappingsByWebSiteId.values()) {
                for(CmsProcessMapping mapping : mappingList) {
                    // NOTE: must update any missing references here post-store, 
                    // because the page and the whole mapping could be completely new and have no IDs whatsoever
                    String pageId = this.getId();
                    if (!pageId.equals(mapping.getPrimaryForPageId())) {
                        mapping.setPrimaryForPageId(pageId);
                    }
                    mapping.store();
                }
            }
        }
        
        // needed for copy operation
        CmsMasterComplexTemplate.checkStoreScriptTemplateAssocs(this, this.sortedScriptTemplates);
    }

    /**
     * Gets version indicated in the copy args and verifies OK for copy.
     * Does NOT creates any copies.
     */
    public CmsPageVersion getVersionForCopyAndVerify(Map<String, Object> copyArgs) {
        String versionId = (String) copyArgs.get("copyVersionId");
        CmsPageVersion version;
        if ("ACTIVE".equals(versionId)) {
            version = getActiveVersion();
            if (version == null) throw new CmsDataException("Source page '" + getId() + "' has no active page version - cannot create copy");
        } else if ("LATEST".equals(versionId) || UtilValidate.isEmpty(versionId)) {
            version = getLastVersion();
            if (version == null) throw new CmsDataException("Source page '" + getId() + "' has no last page version - cannot create copy");
        } else {
            version = getVersion(versionId);
            if (version == null) throw new CmsDataException("Cannot find template version '" + versionId + "' in page '" + getId() + "' - cannot create copy");
        }
        return version;
    }
    
    /**
     * Adds the product to this page. It will be available in templates under
     * the given import name.
     * 
     * @param product The product to be added as GenericValue instance
     * @param importName The name this product should be available as
     */
    public void addProduct(GenericValue product, String importName) {
        preventIfImmutable();
        
        addProduct(product.getString("productId"), importName);
    }

    /**
     * Adds the product to this page. It will be available in templates under
     * the given import name.
     * 
     * @param productId The id of the product to be added
     * @param importName The name this product should be available as
     */
    public void addProduct(String productId, String importName) {
        preventIfImmutable();
        
        try {
            GenericValue productAssoc = entity.getDelegator().makeValue("CmsPageProductAssoc", "pageId", this.getId(),
                    "productId", productId, "importName", importName);
            // FIXME: this is bad
            productAssoc.setNextSeqId();
            entity.getDelegator().create(productAssoc);

            // reset products
            products = null;
        } catch (GenericEntityException e) {
            throw new CmsException(
                    String.format("Could not add product. Page: %s Product: %s", this.getName(), productId), e);
        }
    }

    /**
     * Adds a new content version to this page. This version is not live at this
     * point but can be activated using {@link #setActiveVersion(String)}.
     * 
     * @param content
     *            Page content as map with fields, field values and asset
     *            content as embedded field -> value maps.
     * @throws IOException 
     */
    public CmsPageVersion addVersion(Map<String, ?> content) throws IOException {
        preventIfImmutable();
        
        return addVersion(JSON.from(content).toString());
    }

    /**
     * Adds a new version directly from JSON.
     */
    public CmsPageVersion addVersion(String jsonContent) {
        preventIfImmutable();
        if (CmsUtil.verboseOn())
            Debug.logInfo("addVersion : " + jsonContent, module);
        return new CmsPageVersion(getDelegator(), UtilMisc.toMap("pageId", this.getId(), "content", jsonContent), this);
    }

    public boolean isActive() {
        return getActiveVersionId() != null ? true : false;
    }
    
    public String getActiveVersionId(boolean cacheActiveVersionId) {
        String activeVersionId = this.activeVersionId;
        if (activeVersionId == null) {
            activeVersionId = CmsPageVersion.activeVersionWorker.getRecordVersionId(getDelegator(), getId());
            if (activeVersionId == null) {
                activeVersionId = "";
            }
            if (cacheActiveVersionId) {
                // NOTE: we should NOT cache active version id in live render - waste of space
                preventIfImmutable();
                this.activeVersionId = activeVersionId;
            }
        }
        return activeVersionId.length() > 0 ? activeVersionId : null; // empty string as cache query event record, but MUST return as null
    }
    
    public String getActiveVersionId() {
        return getActiveVersionId(true);
    }
    
    /**
     * Returns the page version currently active for this page.
     */
    protected CmsPageVersion getActiveVersion(boolean cacheActiveVersion) {
        Optional<CmsPageVersion> activeVersion = this.activeVersion;
        if (activeVersion == null) {
            String versionId = getActiveVersionId(cacheActiveVersion);
            activeVersion = Optional.ofNullable(versionId != null ? CmsPageVersion.getWorker().findByIdAlways(getDelegator(), versionId, false) : null);
            if (cacheActiveVersion) {
                // NOTE: we should NOT cache active version in live render - waste of space - we make a CmsPageContent from it and ditch it
                preventIfImmutable();
                this.activeVersion = activeVersion;
            }
        }
        return activeVersion.orElse(null);
    }
    
    public CmsPageVersion getActiveVersion() {
        return getActiveVersion(true);
    }
    
    /**
     * Returns the page version currently active for this page or a new BLANK version if none.
     * The BLANK must not be used for anything other than live render.
     */
    @Deprecated
    public CmsPageVersion getActiveOrNewVersion(boolean cacheActiveVersion) {
        CmsPageVersion version = getActiveVersion(cacheActiveVersion);
        if (version == null) {
            version = new CmsPageVersion(getDelegator(), new HashMap<String, Object>(), this);
        }
        return version;
    }    



    /**
     * Returns the content as map.
     * 
     * @return page content
     */
    @SuppressWarnings("unchecked")
    public Map<String, ?> getContent() {
        Map<String, ?> content = null;
        try {
            CmsPageVersion activeVersion = getActiveVersion();
            content = (Map<String, ?>) JSON.from(activeVersion.getEntity()).toObject(Map.class);
        } catch (IOException e) {
            Debug.logError(e, "Unable to read JSON-formatted content from cms page '" + getId() + "'", module);
        }
        return content != null ? content : new HashMap<String, Object>();
    }

    /**
     * Retrieves the content model of this page. This method returns the content
     * of the current live version.
     * WARN: 2016: the result of this may be immutable; to have writable version you will
     * to create a copy.
     */
    public CmsPageContent getActiveContentModel() {
        return getActiveContentModel(true);
    }
    
    protected CmsPageContent getActiveContentModel(boolean cacheActiveVersion) {
        return getContentModel(null, null, cacheActiveVersion);
    }

    public CmsPageContent getContentModel(CmsPageContext context, String versionId) {
        return getContentModel(context, versionId, true);
    }
    
    /**
     * Retrieves the content model of this page. If the context specifies this
     * call to be a preview call the last version or given version (as parameter
     * "version") is returned. Otherwise, the current live content is returned.
     * The content is preprocessed according to the given page call context.
     * <p>
     * 2016: explicit versionId new, for preview mode; this shouldn't check request parameters
     */
    protected CmsPageContent getContentModel(CmsPageContext context, String versionId, boolean cacheActiveVersion) {
        // Initialize contentModel in any case
        CmsPageContent activeContentModel = this.activeContentModel;
        if (activeContentModel == null) {
            activeContentModel = new CmsPageContent(getActiveOrNewVersion(cacheActiveVersion).getContent(), this);
            this.activeContentModel = activeContentModel;
        }

        CmsPageContent cm = null;
        /*
         * Set content model for this render to last version or given version if
         * page view is in preview mode.
         */
        if (context != null && context.isPreview()) {
            CmsPageVersion previewVersion;
            if (UtilValidate.isNotEmpty(versionId)) {
                previewVersion = this.getVersion(versionId);
            } else {
                previewVersion = getActiveVersion();
            }

            if (previewVersion != null){
                cm = new CmsPageContent(previewVersion.getContent(), this);
            }
        }

        return cm != null ? cm : activeContentModel;
    }
    
    /**
     * 2017: returns script templates associated to page definition.
     * This is an extra layer of optional scripts that run before the template scripts.
     */
    public List<CmsScriptTemplate> getScriptTemplates() {
        return getSortedScriptTemplates();
    }
    
    public List<CmsScriptTemplate> getSortedScriptTemplates() {
        List<CmsScriptTemplate> sortedScriptTemplates = this.sortedScriptTemplates;
        if (sortedScriptTemplates == null) {
            try {
                sortedScriptTemplates = CmsMasterComplexTemplate.readSortedScriptTemplates(entity, getTemplateScriptAssocWorker());
            } catch (Exception e) {
                throw new CmsException("Script templates could not be retrieved for template: " + getName(), e);
            }
            this.sortedScriptTemplates = sortedScriptTemplates;
        }
        return sortedScriptTemplates;
    }
    
    /**
     * Returns a descriptor of this page as map. The descriptor contains the
     * metadata of this page but not the content. The following values are
     * included:
     * <dl>
     * <dt>id</dt>
     * <dd>Unique page id</dd>
     * <dt>path</dt>
     * <dd>The path of this page</dd>
     * <dt>webSiteId</dt>
     * <dd>The id of the website the page belongs to</dd>
     * <dt>templatePageId</dt>
     * <dd>The unique id of the template associated with this page</dd>
     * <dt>products</dt>
     * <dd>List of products as maps containing the <em>importName</em>,
     * <em>productId</em>, and <em>name</em></dd>
     * <dt>versions</dt>
     * <dd>List of page versions as maps containing the <em>id</em>,
     * <em>date</em>, <em>comment</em>, and <em>active</em> status</dd>
     * </dl>
     * <p>
     * 2017: Requires a webSiteId.
     */
    public Map<String, Object> getDescriptor(String webSiteId, Locale locale) {
        preventIfImmutable(); // WARN: currently dangerous if called from rendering!
        
        Map<String, Object> descriptor = super.getDescriptor(locale);
        
        populateBasicDescriptorFields(descriptor, webSiteId, locale);
        
        Map<String, Map<String, ?>> products = getProducts();
        List<Map<String, ?>> productList = new ArrayList<>();
        Map<String, ?> product = null;
        for (String productName : products.keySet()) {
            product = products.get(productName);
            productList.add(UtilMisc.toMap("importName", productName, "productId", product.get("productId"), "name",
                    product.get("internalName")));
        }
        descriptor.put("products", productList);
        
        List<Map<String, ?>> versionList = new ArrayList<>();
        //ToDo: Check if yyyy-MM-dd'T'HH:mm:ss.SSSZ is more suitable
        SimpleDateFormat isoDate = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss.SSS"); 
        for (CmsPageVersion version : getVersions()) {
            String date="";
            if(version.getLastModified()!=null){
                date = isoDate.format(version.getLastModified());
            }
            versionList.add(UtilMisc.toMap("id", version.getId(), "date", date, "comment", version.getVersionComment(), "createdBy",version.getCreatedByName(),"active",
                    new Boolean(version.getId().equals(getActiveVersionId()))));
        }
        descriptor.put("versions", versionList);
        
        descriptor.put("candidateWebSiteIds", new ArrayList<>(getCandidateWebSiteIds()));
        
        return descriptor;
    }
    
    @Deprecated
    @Override
    public Map<String, Object> getDescriptor(Locale locale) {
        throw new UnsupportedOperationException("CmsPage getDescriptor must now be called with a webSiteId");
    }
    
    public Map<String, Object> getShortDescriptor(String webSiteId, Locale locale) {
        preventIfImmutable(); // WARN: currently dangerous if called from rendering!
        
        Map<String, Object> descriptor = new HashMap<>();
        populateBasicDescriptorFields(descriptor, webSiteId, locale);
        return descriptor;
    }
    
    protected void populateBasicDescriptorFields(Map<String, Object> descriptor, String webSiteId, Locale locale) {
        preventIfImmutable(); // WARN: currently dangerous if called from rendering!
        
        descriptor.putAll(UtilMisc.toMap("id", getId(), 
                "name", getName(), 
                "primaryPath", getPrimaryPath(webSiteId), 
                "primaryPathExpanded", getPrimaryPathExpanded(webSiteId),
                "primaryTargetPath", getPrimaryTargetPath(webSiteId),
                "path", getPrimaryPath(webSiteId), // TODO: DEPRECATED: REMOVE
                "webSiteId", webSiteId,
                "defaultWebSiteId", getWebSiteId(),
                "pageTemplateId", getTemplate().getId(), 
                "primaryMappingCount", getPrimaryProcessMappingsListCopy().size(),
                "status", isActive(),
                "description", getDescription(locale)));
    }
    
    /**
     * Returns identification info for page for use in system logs and errors.
     */
    public String getLogIdRepr() {
        return "[page ID: " + getId() + "]"; // don't show this, too heavy and will lead to confusion: getPrimaryPath()
    }
    
    static String getLogIdRepr(String id, String primaryPath) {
        return "[page ID: " + id + (primaryPath != null ? ("; primary path: " + primaryPath ) : "") + "]";
    }

    /**
     * Returns the most current page version.
     * 
     * @return Page version
     */
    public CmsPageVersion getLastVersion() {
        preventIfImmutable();
        
        Optional<CmsPageVersion> lastVersion = this.lastVersion;
        if (lastVersion == null) {
            CmsPageVersion version = CmsPageVersion.getWorker().findLast(getDelegator(), this.getId(), false);
            lastVersion = Optional.ofNullable(version);
            this.lastVersion = lastVersion;
        }
        return lastVersion.orElse(null);
    }
    
    void setLastVersion(CmsPageVersion version) {
        preventIfImmutable();
        
        this.lastVersion = Optional.ofNullable(version);
    }
    
    /**
     * Returns the most current page version or new.
     * 
     * @return Page version
     */
    public CmsPageVersion getLastVersionOrNewVersion() {
        CmsPageVersion version = getLastVersion();
        if (version == null) {
            version = new CmsPageVersion(getDelegator(), new HashMap<String, Object>(), this);
        }
        return version;
    }

    /**
     * Returns the name of the page.
     * 
     * @return Page name
     */
    public String getName() {
        return entity.getString("pageName");
    }
    
    public String getDescription() {
        return CmsComplexTemplate.getDescription(entity, null);
    }
    
    public String getDescription(Locale locale) {
        return CmsComplexTemplate.getDescription(entity, locale);
    }

    /**
     * Returns true if this page is linked to the webSiteId through primary process mappings
     * or the legacy webSiteId field.
     */
    public boolean isLinkedToWebSiteId(String webSiteId) {
        return (webSiteId.equals(getWebSiteId()) || getPrimaryProcessMapping(webSiteId) != null);
    }
    
    /**
     * 2016: Returns the primary page path stored in this page's primary CmsProcessMapping record.
     * <p>
     * NOTE: 2016: UPDATE: Path will no longer to be stored on CmsPage entity.
     * For functional purposes, is fully covered by CmsProcessMapping.
     * For UI and organization purposes, CmsProcessMapping.primaryForPageId
     * is used for getting this simple path.
     */
    public String getPrimaryPath(String webSiteId) {
        //return entity.getString("pagePath");
        CmsProcessMapping primaryProcessMapping = getPrimaryProcessMapping(webSiteId);
        if (primaryProcessMapping != null) {
            return primaryProcessMapping.getSourcePath();
        } else {
            return null;
        }
    }
    
    public String getPrimaryPathExpanded(String webSiteId) {
        //return entity.getString("pagePath");
        CmsProcessMapping primaryProcessMapping = getPrimaryProcessMapping(webSiteId);
        if (primaryProcessMapping != null) {
            return primaryProcessMapping.getSourcePathExpanded();
        } else {
            return null;
        }
    }    

    public String getPrimaryTargetPath(String webSiteId) {
        //return entity.getString("pagePath");
        CmsProcessMapping primaryProcessMapping = getPrimaryProcessMapping(webSiteId);
        if (primaryProcessMapping != null) {
            return primaryProcessMapping.getTargetPath();
        } else {
            return null;
        }
    }
    
    public Map<String, ?> getProduct(String name) {
        return getProducts().get(name);
    }

    public Map<String, Map<String, ?>> getProducts() {
        Map<String, Map<String, ?>> products = this.products;
        if (products == null) {
            products = new HashMap<>();
            try {
                List<GenericValue> assocEntities = entity.getRelated("CmsPageProductAssoc", null, null, false);
                GenericValue asse;
                String importName;
                for (GenericValue assoce : assocEntities) {
                    asse = assoce.getRelatedOne("Product", false);
                    importName = assoce.getString("importName") == null ? asse.getPkShortValueString() : assoce
                            .getString("importName");
                    products.put(importName, asse);
                }
            } catch (Exception e) {
                throw new CmsException("Products could not be retrieved for page: " + this.getName(), e);
            }
            this.products = products;
        }
        return products;
    }

    /**
     * Returns the template for this page.
     */
    public CmsPageTemplate getTemplate() {
        Optional<CmsPageTemplate> template = this.template;
        if (template == null) {
//            GenericValue templateEntity;
//            try {
//                templateEntity = entity.getRelatedOne(CmsPageTemplate.class.getSimpleName(), false);
//            } catch (GenericEntityException e) {
//                throw new CmsException("Error when retrieving page template. Page: " + getName() + " Template ID: "
//                        + entity.getString("pageTemplateId"), e);
//            }
//            template = new CmsPageTemplate(templateEntity);
            template = Optional.ofNullable(CmsPageTemplate.getWorker().findByIdAlways(getDelegator(), getPageTemplateId(), false));
            this.template = template;
        }
        return template.orElse(null);
    }

    /**
     * Returns a specific page version for this page.
     * 
     * @param versionId
     * @return
     */
    public CmsPageVersion getVersion(String versionId) {
        return CmsPageVersion.getWorker().find(getDelegator(), this.getId(), versionId, false);
    }

    public List<CmsPageVersion> getVersions() {
        preventIfImmutable();
        
        return CmsPageVersion.getWorker().findAll(getDelegator(), this.getId(), false);
    }

    /**
     * Returns the highest-priority primary process mapping webSiteId associated to this
     * page.
     * NOTE: This is mainly for compatibility and any situation where getting only one
     * mapping is sufficient.
     * NOTE: 2017-02-14: in current supported configurations, this should not return null,
     * while getWebSiteId may return null.
     * @see #getWebSiteId
     */
    public String getPrimaryWebSiteId() {
        //return entity.getString("pagePath");
        List<CmsProcessMapping> primaryProcessMappings = getPrimaryProcessMappingsByPrio();
        return !primaryProcessMappings.isEmpty() ? primaryProcessMappings.get(0).getSourceWebSiteId() : null;
    }
    
    /**
     * Returns LEGACY webSiteId to which the page belongs, for organizational purposes.
     * NOTE: 2016: NOT used in live renders or functionally.
     * @see #getPrimaryWebSiteId
     */
    public String getWebSiteId() {
        return entity.getString("webSiteId");
    }
    
    public String getPageTemplateId() {
        return entity.getString("pageTemplateId");
    }
    
    /**
     * Removes the page from the database.
     * 
     * @return True if delete was successful, otherwise false
     * @throws GenericEntityException
     */
    @Override
    public int remove() {
        return remove(true);
    }
    
    public int remove(boolean removeRelatedOrphaned) throws CmsException {
        int rowsAffected = 0;
        try {
            Delegator delegator = getDelegator();
            
            // delete CmsPageSpecialMapping
            rowsAffected += delegator.removeByAnd("CmsPageSpecialMapping", UtilMisc.toMap("pageId", this.getId()));
            
            // delete CmsPageAuthorization
            List<GenericValue> pageAuthorizations = entity.getRelated("CmsPageAuthorization", null, null, false);
            for (GenericValue pageAuthorization : pageAuthorizations) {
                pageAuthorization.remove();
                rowsAffected += 1;
            }
            
            // delete CmsPageProductAssoc
            List<GenericValue> pageProducts = entity.getRelated("CmsPageProductAssoc", null, null, false);
            for (GenericValue pageProduct : pageProducts) {
                pageProduct.remove();
                rowsAffected += 1;
            }
            
            // delete the active active
            rowsAffected += CmsPageVersion.activeVersionWorker.removeRecord(delegator, getId());
            
            // delete CmsPageVersion
            rowsAffected += getDelegator().removeByAnd("CmsPageVersion", "pageId", getId());
            
            // delete all primary process mappings
            rowsAffected += removeAll(this.getPrimaryProcessMappingsListCopy());
            
            // NOTE: 2016: do NOT delete non-primary process mappings here because they
            // can be complex and involve multiple pages so the result may surprise user too much
            
            // delete all view mappings (pretty much harmless)
            rowsAffected += removeAll(CmsViewMapping.getWorker().findAll(delegator, UtilMisc.toMap("pageId", getId()), null, false));
            
            // remove script associations (and scripts themselves IF not standalone)
            rowsAffected += CmsMasterComplexTemplate.removeScriptTemplates(entity, getTemplateScriptAssocWorker());
            
        } catch (GenericEntityException e) {
            throw makeRemoveException(e);
        }
        return super.remove() + rowsAffected;
    }

    public int removeIfOrphan() throws CmsException {
        int removed = 0;
        if (this.isOrphan()) {
            removed += remove(true);
        }
        return removed;
    }
    
    public boolean isOrphan() throws CmsException {
        // 2016: for local cms, CmsPage is NEVER orphan, so just return false but leave the code
        // for future use
//        Delegator delegator = getDelegator();
//        boolean isOrphan = false;
//        try {
//            List<GenericValue> parentViewMappings = delegator.findByAnd("CmsViewMapping", 
//                    UtilMisc.toMap("pageId", getId()), null, false);
//            if (UtilValidate.isEmpty(parentViewMappings)) {
//                List<GenericValue> parentProcessViewMappings = delegator.findByAnd("CmsProcessViewMapping", 
//                        UtilMisc.toMap("pageId", getId()), null, false);
//                if (UtilValidate.isEmpty(parentProcessViewMappings)) {
//                    List<GenericValue> parentProcessMappings = delegator.findByAnd("CmsProcessMapping", 
//                            UtilMisc.toMap("pageId", getId()), null, false);
//                    if (UtilValidate.isEmpty(parentProcessMappings)) {
//                        isOrphan = true;
//                    }
//                }
//            }
//        }
//        catch(GenericEntityException e) {
//            throw new CmsException("Could not determine if CMS page " + getId() + " is orphan", e);
//        }
//        return isOrphan;
        return false;
    }
    
    public void removeProduct(String productId) {
        removeProducts(productId);
    }

    public void removeProducts() {
        removeProducts(null);
    }

    private void removeProducts(String productId) {
        try {
            List<GenericValue> assocEntities = entity.getRelated("CmsPageProductAssoc", null, null, false);
            for (GenericValue asse : assocEntities) {
                if (productId == null || asse.getString("productId").equals(productId)) {
                    asse.remove();
                }
            }
        } catch (GenericEntityException e) {
            new CmsException("Could not remove product links from page.", e);
        }
    }

    public void removeUserAuthorization(String userId) {
        try {
            getDelegator().removeByAnd("CmsPageAuthorization", "pageId", this.getId(), "userId", userId);
        } catch (GenericEntityException e) {
            throw new CmsException(String.format("Could not remove user authorization. Page: %s UserId: %s", this.getName(),
                    userId), e);
        }
    }
    
    public void removeGroupAuthorization(String groupId) {
        try {
            getDelegator().removeByAnd("CmsPageAuthorization", "pageId", this.getId(), "groupId", groupId);
        } catch (GenericEntityException e) {
            throw new CmsException(String.format("Could not remove group authorization. Page: %s UserId: %s", this.getName(),
                    groupId), e);
        }
    }    

    /**
     * Sets the version with the given version id as live version.
     * 
     * @param versionId
     */
    public void setActiveVersion(String versionId) {
        this.activeVersionId = (versionId != null) ? versionId : "";
    }

    /**
     * Sets the content of a page as map.
     * 
     * @param content
     * @throws IOException 
     */
    public void setContent(Map<String, ?> content) throws IOException {
        setContent(JSON.from(content).toString());
    }

    /**
     * Sets the content of a page as CmsPageContent object.
     * 
     * @param content
     */
    public void setContent(CmsPageContent content) {
        try{
            JSON json = JSON.from(content);
            Debug.logInfo(json.toString(), module);
            
            setContent(json.toString());
        } catch(Exception e) {
            Debug.logError(e, module);
        }
    }

    /**
     * Sets the content of the page as json string.
     * 
     * @param jsonContent
     */
    public void setContent(String jsonContent) {
        addVersion(jsonContent);
    }

    /**
     * Sets the name of the page.
     * 
     * @param name
     */
    public void setName(String name) {
        entity.setString("pageName", name);
    }

    /**
     * Sets id of pageTemplate to which the page belongs.
     * 
     * @param pageTemplate
     *            id
     */
    public void setPageTemplateId(String pageTemplateId) {
        entity.setString("pageTemplateId", pageTemplateId);
    }

    /**
     * 2016: Sets the primary page path on this page's associated CmsProcessMapping record. 
     * The path is normalized and any trailing slashs are removed.
     * <p>
     * FIXME: setIfEmpty IS NOT CURRENTLY HONORED HERE
     * 
     * @param path
     */
    public void setPrimaryProcessMappingFields(Map<String, ?> fields, boolean setIfEmpty) {
        String webSiteId = (String) fields.get("webSiteId");
  
        // bare minimum fields needed
        // NOTE: for simplicity we require webSiteId in all update cases even if changing only path.
        if (UtilValidate.isNotEmpty(webSiteId)) { 
            CmsProcessMapping primaryProcessMapping = getPrimaryProcessMapping(webSiteId);
            
            // 2016: FIXME?: this next check was not in original code I wrote.
            // but is required if we are changing the webSiteId at same time as path otherwise we produce multiple primary mappings.
            // basically this check may prevent proper expression of the full schema.
            String primaryWebSiteIdOverride = (String) fields.get("primaryWebSiteIdOverride");
            if (primaryProcessMapping == null && "Y".equals(primaryWebSiteIdOverride)) {
                List<CmsProcessMapping> mappings = this.getPrimaryProcessMappingsListCopy();
                if (mappings.size() > 0) {
                    if (mappings.size() == 1) {
                        primaryProcessMapping = mappings.get(0);
                        Debug.logInfo("Cms: Changing the sourceWebSiteId of sole primary process mapping for page '" + getId() + 
                                "' from '" + primaryProcessMapping.getSourceWebSiteId() + "' to '" + webSiteId + 
                                "' (simplified primary process mappings update mode)", module);
                    } else {
                        // can't correct automatically without producing extreme confusion and risk worsening state of data
                        throw new CmsException("Trying to change the sourceWebSiteId of a primary process mapping for page '" + getId() + "' "
                                + "to webSiteId '" + webSiteId + "' (simplified primary process mappings mode), but found multiple primary mappings; "
                                + "ambiguous; cannot select a record for update as the result would be unpredictable. Please fix schema manually by "
                                + "removing all primary process mappings for this page except one. This state may be a result of bad seed"
                                + "data or concurrency issues.");
                    }
                }
            }
            
            Map<String, Object> processFields = new HashMap<>();
            if (fields.containsKey("primaryPath")) {
                // we can't allow setting an empty primaryPath during updates
                if (UtilValidate.isNotEmpty((String) fields.get("primaryPath"))) {
                    processFields.put("sourcePath", fields.get("primaryPath"));
                } else {
                    Debug.logWarning("Cms: Attempted setPrimaryProcessMappingFields for page '" + getLogIdRepr() 
                        + "' with explicitly empty primaryPath (sourcePath) - not allowed - calling code should be fixed"
                        + " to either not set primaryPath or to re-send its previous value (or a different value)", module);
                }
            }
            if (fields.containsKey("primaryPathFromContextRoot")) {
                processFields.put("sourceFromContextRoot", fields.get("primaryPathFromContextRoot"));
            }
            if (fields.containsKey("primaryTargetPath")) {
                processFields.put("targetPath", fields.get("primaryTargetPath"));
            }
            if (fields.containsKey("active")) {
                processFields.put("active", fields.get("active"));
            }
            if (fields.containsKey("webSiteId")) {
                processFields.put("sourceWebSiteId", fields.get("webSiteId"));
            }
            if (primaryProcessMapping == null) {
                String primaryPath = (String) processFields.get("sourcePath");
                if (UtilValidate.isNotEmpty(primaryPath)) {
                    if (getId() != null) {
                        Debug.logInfo("Cms: Page '" + getId() + "' does not have a primary process mapping; creating a new one to "
                                + "store primary path " + primaryPath, module);
                    }
                    primaryProcessMapping = CmsProcessMapping.createPrimaryProcessMapping(getDelegator(), processFields);
                    this.addPrimaryProcessMapping(primaryProcessMapping);
                } else {
                    // TODO: REVIEW: should this throw an exception instead of warning?
                    // This situation is supported by the schema, just not the UI, so maybe
                    // it makes sense if the UI enforces alone, as this could return in the future...
                    // (it could also be enforced by the service)
                    if (getId() != null) {
                        Debug.logWarning("Cms: Invoked add new primary process mapping for page '" + getId() 
                            + "' but primaryPath field was empty - skipping primary process mapping creation (primaryPath/sourcePath cannot be empty)", module);
                    } else {
                        Debug.logWarning("Cms: Creating a new page with empty primaryPath field"
                                + " - skipping primary process mapping creation (primaryPath/sourcePath cannot be empty)", module);
                    }
                }
            } else {
                primaryProcessMapping.setPrimaryProcessMappingFields(processFields, false);
            }
        }
    }

    
    /**
     * Sets the template of this page. The page has to be stored to persist this
     * change.
     * 
     * @param template
     */
    public void setTemplate(CmsPageTemplate template) {
        entity.set("templateId", template.getId());
        this.template = Optional.ofNullable(template);
    }

    public void setUserAuthorization(String userId, String roleTypeId) {
        setUserAuthorization(userId, Enum.valueOf(UserRole.class, roleTypeId));
    }

    public void setUserAuthorization(String userId, UserRole role) {
        removeUserAuthorization(userId);
        try {
            GenericValue auth = getDelegator().makeValue("CmsPageAuthorization", "pageId", this.getId(), "userId", userId,
                    "roleTypeId", role.toString());
            auth.setNextSeqId();
            auth.create();
        } catch (GenericEntityException e) {
            throw new CmsException(String.format("Could not set user authorization. Page: %s UserId: %s Role: %s",
                    this.getName(), userId, role.toString()), e);
        }
    }
    
    
    public void setGroupAuthorization(String groupId, String roleTypeId) {
        setGroupAuthorization(groupId, Enum.valueOf(UserRole.class, roleTypeId));
    }
    
    public void setGroupAuthorization(String groupId, UserRole role) {
        removeGroupAuthorization(groupId);
        try {
            GenericValue auth = getDelegator().makeValue("CmsPageAuthorization", "pageId", this.getId(), "groupId", groupId,
                    "roleTypeId", role.toString());
            auth.setNextSeqId();
            auth.create();
        } catch (GenericEntityException e) {
            throw new CmsException(String.format("Could not set group authorization. Page: %s UserId: %s Role: %s",
                    this.getName(), groupId, role.toString()), e);
        }
    }


    protected Map<String, List<CmsProcessMapping>> getPrimaryProcessMappingsByWebSiteIdInternal() {
        Map<String, List<CmsProcessMapping>> primaryProcessMappingsByWebSiteId = this.primaryProcessMappingsByWebSiteId;
        if (primaryProcessMappingsByWebSiteId == null) {
            final CmsPage primaryForPage = this;
            List<CmsProcessMapping> primaryProcessMappingsList = new CmsProcessMapping.ProcessMappingWorker() {
                @Override
                public CmsProcessMapping makeFromValue(GenericValue value) throws CmsException {
                    return new CmsProcessMapping(value, primaryForPage);
                }
            }.findAll(getDelegator(), UtilMisc.toMap("primaryForPageId", getId()), null, false);
            primaryProcessMappingsByWebSiteId = CmsProcessMapping.makeProcessMappingsByWebSiteIdMap(primaryProcessMappingsList);
            this.primaryProcessMappingsByWebSiteId = primaryProcessMappingsByWebSiteId;
        }
        return primaryProcessMappingsByWebSiteId;
    }
    
    protected List<CmsProcessMapping> getPrimaryProcessMappingsInternal(String webSiteId) {
        return getPrimaryProcessMappingsByWebSiteIdInternal().get(webSiteId);
    }
    
    public Set<String> getPrimaryProcessMappingsWebSiteIds() {
        return Collections.unmodifiableSet(getPrimaryProcessMappingsByWebSiteIdInternal().keySet());
    }
    
    public List<CmsProcessMapping> getPrimaryProcessMappingsListCopy() {
        List<CmsProcessMapping> mappings = new ArrayList<>();
        for(List<CmsProcessMapping> webSiteMappings : getPrimaryProcessMappingsByWebSiteIdInternal().values()) {
            mappings.addAll(webSiteMappings);
        }
        return mappings;
    }

    public List<CmsProcessMapping> getPrimaryProcessMappingsByPrio() {
        List<CmsProcessMapping> mappings = getPrimaryProcessMappingsListCopy();
        mappings = CmsProcessMapping.sortProcessMappingsByActive(mappings);
        mappings = CmsProcessMapping.sortProcessMappingsSpecWebSiteFirst(mappings, getWebSiteId());
        return mappings;
    }
    
    public List<CmsProcessMapping> getPrimaryProcessMappings(String webSiteId) {
       List<CmsProcessMapping> primaryProcessMappings = getPrimaryProcessMappingsInternal(webSiteId);
       if (primaryProcessMappings != null) {
           return Collections.unmodifiableList(primaryProcessMappings);
       } else {
           return Collections.emptyList();
       }
    }
    
    public CmsProcessMapping getPrimaryProcessMapping(String webSiteId) {
        List<CmsProcessMapping> primaryProcessMappings = getPrimaryProcessMappingsInternal(webSiteId);
        if (primaryProcessMappings == null || primaryProcessMappings.isEmpty()) {
            if (CmsUtil.verboseOn() && getId() != null) {
                Debug.logInfo("Cms: Page " + getId() + " has no primary process mappings for webSiteId '" + webSiteId + "'", module);
            }
            return null;
        } else if (primaryProcessMappings.size() == 1) {
            return primaryProcessMappings.get(0);
        } else {
            // NOTE: this case should be impossible with the addition of CmsPageSpecialMapping entity, but leave it here anyway
            // what do we do? try to find one that matches our webSiteId - if we have one...
            for(CmsProcessMapping mapping : primaryProcessMappings) {
                if (mapping.getSourceWebSiteId() != null && mapping.getSourceWebSiteId().equals(this.getWebSiteId())) {
                    Debug.logInfo("Cms: Page " + getId() + " has multiple primary process mappings; returning "
                            + "one (" + mapping.getId() + ") matching the page's main webSiteId (" + mapping.getSourceWebSiteId() + ")", module);
                    return mapping;
                }
            }
            CmsProcessMapping mapping = primaryProcessMappings.get(0);
            Debug.logInfo("Cms: Page " + getId() + " has multiple primary process mappings, and none seem preferable; returning the first "
                    + "one (" + mapping.getId() + ")", module);
            return mapping;
        }
    }

    // TODO: REVIEW: DO NOT DO THIS ANYMORE FOR NOW;
    // it negatively impacts the CmsPage.pageId global cache because it prevents reusing it.
    // it will probably all work out without doing this.
//    /**
//     * 2016: this is used internally during preload to ensure the CmsProcessMapping
//     * references match up with this page's backreferences. These could end up out of sync
//     * otherwise due to the multiple DB queries done, and want the preloaded instances
//     * to be consistent.
//     */
//    public boolean checkAddPrimaryProcessMapping(CmsProcessMapping processMapping) {
//        if (getId().equals(processMapping.getPrimaryForPageId())) {
//            addPrimaryProcessMapping(processMapping);
//        }
//        return false;
//    }
    
    protected boolean addPrimaryProcessMapping(CmsProcessMapping newMapping) {
        String sourceWebSiteId = newMapping.getSourceWebSiteId();
        List<CmsProcessMapping> primaryProcessMappings = getPrimaryProcessMappingsInternal(sourceWebSiteId);
        String processMappingId = newMapping.getId();
        if (UtilValidate.isNotEmpty(processMappingId)) {
            ListIterator<CmsProcessMapping> it = primaryProcessMappings.listIterator();
            while(it.hasNext()) {
                CmsProcessMapping mapping = it.next();
                if (processMappingId.equals(mapping.getId())) {
                    it.set(newMapping);
                    return true;
                }
            }
        }
        // this should usually only happen on initial page creation...
        if (primaryProcessMappings == null) {
            primaryProcessMappings = new ArrayList<>();
        }
        primaryProcessMappings.add(newMapping); 
        this.primaryProcessMappingsByWebSiteId.put(sourceWebSiteId, primaryProcessMappings);
        return false;
    }


    public static String normalizePath(String path) {
        // 2016: KEEP case-sensitive and use newer norm method
        //return StringUtils.lowerCase(StringUtils.stripEnd(path, "/ "));
        return CmsControlUtil.normalizeContextRootRequestPath(path);
    }

    /**
     * 2016: Returns all the web site IDs potentially referencing this page through process and view mappings,
     * in order of most important to least relevant (loose heuristic).
     * <p>
     * NOTE: this does not include CmsPage.webSiteId (since it is dubious), but does 
     * include the webSiteId on its primary process mapping.
     */
    public Set<String> getCandidateWebSiteIds() {
        preventIfImmutable();
        
        Set<String> webSiteIds = this.candidateWebSiteIds;
        if (candidateWebSiteIds == null) {
            Delegator delegator = this.getDelegator();
            String pageId = this.getId();
            
            webSiteIds = new LinkedHashSet<String>();
            
            // primary mappings get priority
            List<CmsProcessMapping> prioPrimaryProcessMappings = getPrimaryProcessMappingsByPrio();
            webSiteIds.addAll(CmsProcessMapping.getWebSiteIdsFromMappings(prioPrimaryProcessMappings));
    
            // other mappings
            addWebSiteIdsFromMappings(delegator, pageId, "Y", webSiteIds);
            addWebSiteIdsFromMappings(delegator, pageId, null, webSiteIds);
            addWebSiteIdsFromMappings(delegator, pageId, "N", webSiteIds);
            this.candidateWebSiteIds = webSiteIds;
        }
        return webSiteIds;
    }
    
    private static void addWebSiteIdsFromMappings(Delegator delegator, String pageId, String active, Collection<String> webSiteIds) {
        Map<String, Object> fields = new HashMap<String, Object>();
        fields.put("pageId", pageId);
        fields.put("active", active);
        
        List<String> orderBy = new ArrayList<>();
        orderBy.add("lastUpdatedStamp DESC");
        
        try {
            List<GenericValue> values = delegator.findByAnd("CmsViewMapping", fields, orderBy, false);
            
            if (values != null) {
                for(GenericValue value : values) {
                    String webSiteId = value.getString("webSiteId");
                    if (webSiteId != null && !webSiteId.isEmpty()) {
                        webSiteIds.add(webSiteId);
                    }
                }
            }
            
        } catch (Exception e) {
            ;
        }

        // WARN: It is technically possible for this to include the root of wildcard page matches that allow root match,
        // but it's not really important
        
        try {
            // WARN: approximation of live behavior
            EntityCondition cond = EntityCondition.makeCondition(
                    EntityCondition.makeCondition(EntityCondition.makeCondition("processPrimaryForPageId", pageId),
                        EntityOperator.OR,    
                        EntityCondition.makeCondition(
                                EntityCondition.makeCondition("pageId", pageId),
                                EntityOperator.OR,
                                EntityCondition.makeCondition(
                                        EntityCondition.makeCondition("pageId", null),
                                        EntityOperator.AND,
                                        EntityCondition.makeCondition("processPageId", pageId)
                                        )
                                )
                        ),
                    EntityOperator.AND,
                    EntityCondition.makeCondition("active", active));

            List<GenericValue> values = delegator.findList("CmsProcessAndViewMapping", cond, null, orderBy, null, false);
            
            if (values != null) {
                for(GenericValue value : values) {
                    String webSiteId = value.getString("sourceWebSiteId");
                    if (webSiteId != null && !webSiteId.isEmpty()) {
                        webSiteIds.add(webSiteId);
                    }
                }
            }
            
        } catch (Exception e) {
            ;
        }

        /* this does not define a real mapping on its own
        try {
            List<GenericValue> values = delegator.findByAnd("CmsProcessMapping", fields, orderBy, false);
            
            if (values != null) {
                for(GenericValue value : values) {
                    String webSiteId = value.getString("sourceWebSiteId");
                    if (webSiteId != null && !webSiteId.isEmpty()) {
                        webSiteIds.add(webSiteId);
                    }
                }
            }
            
        } catch (Exception e) {
            ;
        }
        */
    }
    
    
    // 2016: left for reference but doesn't work for local cms invoke
//    /**
//     * Builds a list of candidate WebSiteIds that may currently map to the given path, in descending
//     * order of probable importance.
//     * <p>
//     * cmsPages are candidate pages assumed to be related to path (are a prefix of); may be gotten using
//     * {@link #getCandidatePagesForPath}.
//     * <p>
//     * WARN: this is an estimation of live behavior only. Currently not guaranteed
//     * to match live behavior. Exact, active mappings have priority. 
//     * Is complicated by wildcard mappings and priorities
//     * related to active status. All other things equal, it will tend to prioritize
//     * last-updated records.
//     */
//    public static List<String> getPageCandidateWebSiteIds(Delegator delegator, String path, List<GenericValue> cmsPages) {
//        Set<String> webSiteIds = new LinkedHashSet<String>();
//        
//        if (delegator != null && path != null && path.length() > 0) {
//            List<String> exactPageIds = new ArrayList<>();
//            List<String> wildcardPageIds = new ArrayList<>();
//            
//            for(GenericValue cmsPage : cmsPages) {
//                String cmsPagePath = cmsPage.getString("cmsPageReqPath");
//                if (cmsPagePath.length() >= path.length()) {
//                    exactPageIds.add(cmsPage.getString("pageId"));
//                } else {
//                    wildcardPageIds.add(cmsPage.getString("pageId"));
//                }
//            }
//            
//            // exact page matches first
//            // add active first, then non-marked, then N
//            // WARN: It is technically possible for this to include the root of wildcard page matches that allow root match,
//            // but it's not really important
//            for(String pageId : exactPageIds) {
//                addWebSiteIdsFromMappings(delegator, pageId, "Y", webSiteIds);
//            }
//            for(String pageId : exactPageIds) {
//                addWebSiteIdsFromMappings(delegator, pageId, null, webSiteIds);
//            }
//            for(String pageId : exactPageIds) {
//                addWebSiteIdsFromMappings(delegator, pageId, "N", webSiteIds);
//            }
//            
//            // wildcard page matches last
//            // add active first, then non-marked, then N
//            // CMS: 2016: not applicable for local renders
////            for(String pageId : wildcardPageIds) {
////                addWebSiteIdsFromWildcardMappings(delegator, pageId, "Y", webSiteIds);
////            }
////            for(String pageId : wildcardPageIds) {
////                addWebSiteIdsFromWildcardMappings(delegator, pageId, null, webSiteIds);
////            }
////            for(String pageId : wildcardPageIds) {
////                addWebSiteIdsFromWildcardMappings(delegator, pageId, "N", webSiteIds);
////            }
//        }
//        
//        return new ArrayList<String>(webSiteIds);
//    }
    
    // 2016: not appropriate unless rewritten
//    /**
//     * Finds all the CmsPages that *could* be related to the given path or potentially
//     * lead to a mapping to the given path.
//     */
//    public static List<GenericValue> getCandidatePagesForPath(Delegator delegator, String path, boolean exactOnly) {
//        List<GenericValue> res = null;
//        if (delegator != null && path != null) {
//            List<String> orderBy = new ArrayList<>();
//            orderBy.add("lastUpdatedStamp DESC");
//            try {
//                EntityCondition cond = EntityCondition.makeCondition("cmsPageReqPath", path);
//                
//                if (!exactOnly) {
//                    // each sub-path of path is a possible reference...
//                    List<String> subPaths = PathUtil.makeAllRequestPathPrefixes(path);
//                    for(String subPath : subPaths) {
//                        if (subPath.length() < path.length()) {
//                            cond = EntityCondition.makeCondition(cond, EntityOperator.OR,
//                                    EntityCondition.makeCondition("cmsPageReqPath", subPath)
//                            );
//                        }
//                    }
//                }
//                
//                res = delegator.findList("CmsPage", cond, null, orderBy, null, false);
//                
//                // reorder the result by path length, descending
//                Collections.sort(res, new Comparator<GenericValue>() {
//                    @Override
//                    public int compare(GenericValue o1, GenericValue o2) {
//                        return ((Integer) o2.getString("cmsPageReqPath").length()).compareTo(o1.getString("cmsPageReqPath").length());
//                    }
//                });
//
//            } catch (Exception e) {
//                ;
//            }
//        }
//        
//        if (res == null) {
//            res = new ArrayList<>();
//        }
//        return res;
//    }
    
    // 2016: not appropriate unless rewritten
//    /**
//     * Gets the first page ID for the exact path.
//     * <p>
//     * WARN: on its own, this is not guaranteed to return the page you expect!
//     */
//    public static String getPageIdForPath(Delegator delegator, String path) {
//        String pageId = null;
//        
//        if (delegator != null && path != null) {
//            Map<String, Object> fields = new HashMap<String, Object>();
//            fields.put("cmsPageReqPath", path);
//            List<String> orderBy = new ArrayList<>();
//            orderBy.add("lastUpdatedStamp DESC");
//            try {
//                List<GenericValue> values = delegator.findByAnd("CmsPage", fields, orderBy, false);
//                
//                if (values != null && !values.isEmpty()) { 
//                    pageId = values.get(0).getString("pageId");
//                }
//                
//            } catch (Exception e) {
//                ;
//            }
//        }
//        
//        return pageId;
//    }
    

    /**
     * Helper class to identify if a page authorization is dealing with a user authorization
     * or a group authorization.
     *
     */
    private enum PageAuthPartyType {
        // Note: the name "party" in this type does not signify a "Party" entity type
        USER ("userId"),
        GROUP ("groupId");
        
        private final String fieldName;
        private PageAuthPartyType(String fieldName) {
            this.fieldName = fieldName;
        }
        
        public EntityCondition makeNullCond() { // This null
            return EntityCondition.makeCondition(fieldName, EntityOperator.EQUALS, null);
        }
        public EntityCondition makeAllOthersNullCond() { // All null except for this
            List<EntityCondition> conds = new ArrayList<>();
            for(PageAuthPartyType type : PageAuthPartyType.values()) {
                if (type != this) {
                    conds.add(type.makeNullCond());
                }
            }
            return EntityCondition.makeCondition(conds, EntityOperator.AND);            
        }
        
        public EntityCondition makeNonNullCond() { // This non-null
            return EntityCondition.makeCondition(fieldName, EntityOperator.NOT_EQUAL, null);
        }
        public EntityCondition makeExclusiveNonNullCond() { // All null except this, non-null
            return EntityCondition.makeCondition(
                    makeAllOthersNullCond(), 
                    EntityOperator.AND,
                    this.makeNonNullCond()
                );
        }
        
        public EntityCondition makeIdCond(String id) { // This equals ID
            return EntityCondition.makeCondition(fieldName, EntityOperator.EQUALS, id);
        }
        public EntityCondition makeExclusiveIdCond(String id) { // All null except this, equals ID
            return EntityCondition.makeCondition(
                    makeAllOthersNullCond(), 
                    EntityOperator.AND,
                    this.makeIdCond(id)
                );
        } 
    }
    
    /**
     * Retrieves the users authorized to edit this page by user role.
     * 
     * @param role
     *            The role the users should have on the page
     * @return List of users
     */
    public List<GenericValue> getAuthorizedUsers(UserRole role) {
        return findUsersByPageRole(getDelegator(), this, role, false);
    }
    
    /**
     * Retrieves the users authorized to edit this page by user role.
     * 
     * @param role
     *            The role the users should have on the page
     * @return List of users
     */
    public List<GenericValue> getAuthorizedGroups(UserRole role) {
        return findGroupsByPageRole(getDelegator(), this, role, false);
    }    
    
    /**
     * Gets user's authorization.
     * <p>
     * Note: Delegator and dispatcher needed here; can't have
     * these as instance variables because they're not serializable and instances of this class *could* find
     * their way into session attributes.
     * 
     * @param userId
     * @param delegator
     * @param dispatcher
     * @return
     */
    public UserRole getUserAuthorization(String userId, Delegator delegator, LocalDispatcher dispatcher) {
        return findPageRoleForUser(this, userId, delegator, dispatcher);
    }
    
    /**
     * Returns the most senior role that the user is assigned for the given page.
     * <p>
     * A user can be part of many groups and also be named individually in the authorization
     * for a given page.
     * 
     * @param page
     * @param userId
     * @param delegator
     * @param dispatcher
     * @return
     */
    public static UserRole findPageRoleForUser(CmsPage page, String userId, 
            Delegator delegator, LocalDispatcher dispatcher) {
        UserRole role;
        List<GenericValue> auths = null;
        try {
            // New in security groups: 
            // We find all the auths for which the user is explicitly specified or
            // the auths for which one of his security groups is specified.
            
            // Condition to find specific users
            EntityCondition userCond = makeUserCond(userId, delegator, dispatcher);
                
            // Condition to find user security groups
            EntityCondition userSecGroupsCond = makeUserSecGroupsCond(userId, delegator, dispatcher);
            
            // Combined user & group condition
            EntityCondition userOrGroupsCond;
            if (userSecGroupsCond != null) {
                userOrGroupsCond = EntityCondition.makeCondition(userSecGroupsCond, EntityOperator.OR, userCond);
            } else {
                userOrGroupsCond = userCond;
            }
            
            // Page condition
            EntityCondition whereCond = EntityCondition.makeCondition(
                    EntityCondition.makeCondition("pageId", EntityOperator.EQUALS, page.getId()),
                    EntityOperator.AND,
                    userOrGroupsCond);
            
            auths = delegator.findList("CmsPageAuthorization", whereCond, null, null, null, false);
        } catch (GenericEntityException e) {
            throw new CmsException("Could not retrieve page role for user. Page: " + page.getName() + " User: " + userId, e);
        }
        if (UtilValidate.isNotEmpty(auths)) {
            // Here, the user might have multiple permissions per page due to overlapping groups and individual rights.
            // Return the role that gives him the most privilege.
            role = UserRole.findMostSeniorRole(auths, "roleTypeId");
        } else {
            role = DEFAULT_USER_ROLE;
        }
        return role;
    }
    
    private static EntityCondition makeUserCond(String userId, Delegator delegator, LocalDispatcher dispatcher) {
        return PageAuthPartyType.USER.makeExclusiveIdCond(userId);
    }
    
    private static EntityCondition makeUserSecGroupsCond(String userId, Delegator delegator, LocalDispatcher dispatcher) {
        // Get all the user's groups
        List<GenericValue> userSecGroups = CmsDataUtil.findUserLoginSecurityGroupByUserLoginId(delegator, userId);
        
        List<EntityCondition> userSecGroupsConds = new ArrayList<>();
        for(GenericValue userSecGroup : userSecGroups) {
            String groupId = userSecGroup.getString("groupId");
            userSecGroupsConds.add(PageAuthPartyType.GROUP.makeIdCond(groupId));
        }
        
        EntityCondition userSecGroupsCond = null;
        if (!userSecGroupsConds.isEmpty()) {
            // Make an OR condition to select all rows containing any one of the user's groups
            userSecGroupsCond = EntityCondition.makeCondition(userSecGroupsConds, EntityOperator.OR);
            
            // When making this condition, make sure it is exclusive; all other ID identifiers (userId, etc.) must be null for integrity/consistency
            userSecGroupsCond = EntityCondition.makeCondition(userSecGroupsCond, EntityOperator.AND,
                    PageAuthPartyType.GROUP.makeAllOthersNullCond()
            );
        }

        return userSecGroupsCond;
    }
    

    /**
     * Finds users currently authorized for the given page with the given role - only those 
     * named individually in the page authorization. Does not attempt to get all users from all allowed groups.
     * 
     * @param page
     * @param role
     * @return
     */
    public static List<GenericValue> findUsersByPageRole(Delegator delegator, CmsPage page, UserRole role, boolean useCache) {
        List<GenericValue> users = null;
        try {
            List<EntityCondition> conds = new ArrayList<>();
            conds.add(EntityCondition.makeCondition("pageId", page.getId()));
            conds.add(EntityCondition.makeCondition("roleTypeId", role.toString()));
            conds.add(PageAuthPartyType.USER.makeExclusiveNonNullCond());
            EntityCondition whereCond = EntityCondition.makeCondition(conds, EntityOperator.AND);
            users = delegator.findList("CmsPageAuthorization", whereCond, null, null, null, useCache);
        } catch (GenericEntityException e) {
            throw new CmsException("Could not retrieve users with page role. Page: " + page.getName() + " Role:"
                    + role.toString(), e);
        }
        return users;
    }
    
    public static enum UserRole {
        CMS_ADMIN(4), 
        CMS_EDITOR(3), 
        CMS_SUPERVISOR(2), 
        CMS_VISITOR(1);
        
        private final int seniorityRank; // Higher -> more senior
        UserRole(int seniorityRank) {
            this.seniorityRank = seniorityRank;
        }
        
        public boolean isMoreSeniorThan(UserRole other) {
            if (other == null) {
                return true;
            } else {
                return this.seniorityRank > other.seniorityRank;
            }
        }
        
        public static UserRole findMostSeniorRole(Iterable<UserRole> roles) {
            UserRole result = null;
            if (roles != null) {
                for(UserRole role : roles) {
                    if (role != null) {
                        if (role.isMoreSeniorThan(result)) {
                            result = role;
                        }
                    }
                }
            }
            return result;
        }
        
        public static <T extends Map<String, Object>> UserRole findMostSeniorRole(Iterable<T> auths, String roleFieldName) {
            List<UserRole> roles = new ArrayList<>();
            if (auths != null) {
                for (T auth : auths) {
                    UserRole roleToCheck = Enum.valueOf(UserRole.class, (String) auth.get(roleFieldName));
                    roles.add(roleToCheck);
                }
            }
            return UserRole.findMostSeniorRole(roles);
        }
    }
    
    /**
     * Finds security groups currently authorized for the given page with the given role.
     * 
     * @param page
     * @param role
     * @return
     */
    public static List<GenericValue> findGroupsByPageRole(Delegator delegator, CmsPage page, UserRole role, boolean useCache) {
        List<GenericValue> groups = null;
        try {
            List<EntityCondition> conds = new ArrayList<>();
            conds.add(EntityCondition.makeCondition("pageId", page.getId()));
            conds.add(EntityCondition.makeCondition("roleTypeId", role.toString()));
            conds.add(PageAuthPartyType.GROUP.makeExclusiveNonNullCond());
            EntityCondition whereCond = EntityCondition.makeCondition(conds, EntityOperator.AND);
            groups = delegator.findList("CmsPageAuthorization", whereCond, null, null, null, useCache);
        } catch (GenericEntityException e) {
            throw new CmsException("Could not retrieve users with page role. Page: " + page.getName() + " Role:"
                    + role.toString(), e);
        }
        return groups;
    }
    
    protected CmsPageScriptAssoc.PageScriptAssocWorker getTemplateScriptAssocWorker() {
        return CmsPageScriptAssoc.getWorker();
    }
    
    public static class CmsPageScriptAssoc extends CmsTemplateScriptAssoc {

        private static final long serialVersionUID = -7223454711555662977L;

        protected CmsPageScriptAssoc(GenericValue entity) {
            super(entity);
        }
        
        public CmsPageScriptAssoc(Delegator delegator, Map<String, ?> fields, CmsScriptTemplate scriptTemplate) {
            super(delegator, fields, scriptTemplate);
        }
        
        protected CmsPageScriptAssoc(CmsPageScriptAssoc other, Map<String, Object> copyArgs) {
            super(other, copyArgs);
            // NOTE: don't bother clearing out the ID fields here, caller should handle
        }

        @Override    
        public void update(Map<String, ?> fields, boolean setIfEmpty) {
            super.update(fields, setIfEmpty);
        }
        
        @Override
        public CmsPageScriptAssoc copy(Map<String, Object> copyArgs) throws CmsException {
            return new CmsPageScriptAssoc(this, copyArgs);
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
            entity.set("pageId", null);
        }

        @Override
        protected void setTemplate(CmsDataObject template) {
            if (!(template instanceof CmsPage)) throw new CmsException("CmsPageScriptAssoc requires a CmsPage, got: " 
                    + (template != null ? template.getClass().getName() : null));
            entity.set("pageId", template.getId());
        }

        @Override
        protected boolean hasTemplate() {
            return (entity.get("pageId") != null);
        }
        
        @Override
        public PageScriptAssocWorker getWorkerInst() {
            return PageScriptAssocWorker.worker;
        }
        
        public static PageScriptAssocWorker getWorker() {
            return PageScriptAssocWorker.worker;
        }

        public static class PageScriptAssocWorker extends TemplateScriptAssocWorker<CmsPageScriptAssoc> {
            private static final PageScriptAssocWorker worker = new PageScriptAssocWorker();
            
            protected PageScriptAssocWorker() {
                super(CmsPageScriptAssoc.class);
            }

            @Override
            public CmsPageScriptAssoc makeFromValue(GenericValue value) throws CmsException {
                return new CmsPageScriptAssoc(value);
            }

            @Override
            public CmsPageScriptAssoc makeFromFields(Delegator delegator, Map<String, ?> fields) throws CmsException {
                return new CmsPageScriptAssoc(delegator, fields, null);
            }

            @Override
            protected CmsPageScriptAssoc makeFromFields(Delegator delegator, Map<String, ?> fields,
                    CmsScriptTemplate scriptTemplate) throws CmsException {
                return new CmsPageScriptAssoc(delegator, fields, scriptTemplate);
            }
        }
    }
    
    public PageRenderer getRenderer() {
        return renderer;
    }
    
    /**
     * Dedicated renderer object.
     * <p>
     * NOTE: is not an inner class but nested class for consistency with same pattern
     * in templates (where inner is problematic).
     */
    @SuppressWarnings("serial")
    public static class PageRenderer implements Serializable {
        
        protected final CmsPage page;

        public PageRenderer(CmsPage page) {
            super();
            this.page = page;
        }
        
        /**
         * Renders the page to given writer.
         * <p>
         * NOTE: 2016: this creates a deep copy of content model so as not to affect
         * the page instance.
         * 
         * @param context
         * @return
         */
        public void processAndRender(Writer out, CmsPageContext pageContext, String versionId) {
            CmsPageContent content = page.getContentModel(pageContext, versionId);
            if (content != null) {
                content = content.clone(); // 2016: CLONE because content member on CmsPage instance may be immutable
            }
            MapStack<String> context = MapStack.create();
            boolean shareScope = true; // we don't need to push it here...
            PtRenderArgs renderArgs = new PtRenderArgs(out, context, content, pageContext, shareScope);
            renderArgs.setRunPageScripts(true);
            page.getTemplate().getRenderer().processAndRender(renderArgs);
        }
        
    }
    
    
    @Override
    public PageWorker getWorkerInst() {
        return PageWorker.worker;
    }
    
    public static PageWorker getWorker() {
        return PageWorker.worker;
    }

    public static class PageWorker extends DataObjectWorker<CmsPage> {
        private static final PageWorker worker = new PageWorker();
        
        protected PageWorker() {
            super(CmsPage.class);
        }

        @Override
        public CmsPage makeFromValue(GenericValue value) throws CmsException {
            return new CmsPage(value);
        }

        @Override
        public CmsPage makeFromFields(Delegator delegator, Map<String, ?> fields) throws CmsException {
            return new CmsPage(delegator, fields);
        }
        
        public CmsPage findById(Delegator delegator, String pageId, boolean useCache) throws CmsException {
            return findById(delegator, pageId, useCache, null);
        }

        /**
         * 2016: Finds page by ID.
         * <p>
         * NOTE: this is the only place where a page cache could still be useful.
         * 
         * @param request OPTIONAL request, used for logging
         */
        public CmsPage findById(Delegator delegator, String pageId, boolean useCache, HttpServletRequest request) throws CmsException {
            boolean useGlobalCache = isUseGlobalObjCacheStatic(useCache);
            CmsObjectCache<CmsPage> cache = null;
            if (useGlobalCache) {
                cache = idCache;
            }
            
            String key = delegator.getDelegatorName() + "::" + pageId;
            CmsPage page = null;
            CacheEntry<CmsPage> pageEntry = null;
            
            if (useGlobalCache) {
                pageEntry = cache.getEntry(key);
            }

            if (pageEntry == null) {
                if (CmsUtil.verboseOn()) {
                    Debug.logInfo("Cms: Retrieving page from database: id: " + pageId + CmsControlUtil.getReqLogIdDelimStr(request), module);
                }
                page = findOne(delegator, UtilMisc.toMap("pageId", pageId), 
                        isUseDbCacheStatic(useCache));

                if (useGlobalCache) {
                    cache.put(key, page);
                }
            } else {
                if (pageEntry.hasValue()) {
                    if (CmsUtil.verboseOn()) {
                        Debug.logVerbose("Cms: Retrieving page from cache: id: " + pageId + CmsControlUtil.getReqLogIdDelimStr(request), module);
                    }
                    page = pageEntry.getValue();
                }
            }

            return page;
        }
        
        /**
         * 2016: Finds page by ID or throws exception.
         */
        public CmsPage findByIdAlways(Delegator delegator, String pageId, boolean useCache) throws CmsException {
            CmsPage page = findById(delegator, pageId, useCache);
            if (page == null) {
                throw new CmsDataException("CmsPage for pageId '" + pageId + "' not found");
            }
            return page;
        }

        /**
         * Returns the page with the given primary path which should be unique within one
         * webSiteId.
         * <p>
         * NOTE: 2016: primary path cached removed, not used in rendering and detrimental to backend.
         */
        public CmsPage findByPath(Delegator delegator, String path, String webSiteId, boolean useCache) {
            path = normalizePath(path);
            Debug.logInfo("Cms: Retrieving page from database by primary path: " + path + " webSiteId: " + webSiteId, module);
            List<EntityCondition> condList = new ArrayList<>();
            condList.add(EntityCondition.makeCondition("sourceWebSiteId", webSiteId));
            condList.add(EntityCondition.makeCondition("sourcePath", path));
            // this is now variable
            //condList.add(EntityCondition.makeCondition("sourceFromContextRoot", "Y"));
            condList.add(EntityCondition.makeCondition("primaryForPageId", EntityOperator.NOT_EQUAL, null));
            
            CmsProcessMapping mapping = CmsProcessMapping.getWorker().findFirst(delegator, 
                    EntityCondition.makeCondition(condList, EntityOperator.AND), null, useCache);
            if (mapping != null) {
                return mapping.getPrimaryForPage();
            }
            return null;
        }
        
        /**
         * Returns all pages, with ordering.
         * 
         * @return List of pages
         */
        @Override
        public List<CmsPage> findAll(Delegator delegator, boolean useCache) {
            return findAll(delegator, getPagesOrderBy(), useCache);
        }
        
        /**
         * Returns all pages that are assigned a (any) website.
         * 
         * @return List of pages
         */
        public List<CmsPage> findAllWithWebsite(Delegator delegator, boolean useCache) {
            return findAll(delegator, 
                    EntityCondition.makeCondition("webSiteId", EntityOperator.NOT_EQUAL, null), // 2016: this used to be in findAll, but doesn't belong there
                    getPagesOrderBy(), useCache);
        }
        
        /**
         * Returns all pages of a given website.
         * 
         * @param webSiteId
         *            Id of the website
         * @return List of pages
         */
        public List<CmsPage> findByWebSiteId(Delegator delegator, String webSiteId, boolean useCache) {
            return findAll(delegator, UtilMisc.toMap("webSiteId", webSiteId), getPagesOrderBy(), useCache);
        }
        
        public List<String> getPagesOrderBy() {
            return UtilMisc.toList("pageName ASC");
        }

        @Override
        public void clearMemoryCaches() {
            idCache.clear();
        }
    }

    @Override
    public void acceptEntityDepsVisitor(CmsEntityVisitor visitor, GenericValue relValue, VisitRelation relValueRelation, CmsMajorObject majorDataObj) throws Exception {
        CmsEntityVisit.acceptRelatedEntityDepsVisitor(visitor, VisitRelPlan.visitRelations, this.getEntity(), relValueRelation, relValue, this);
    }
    
    public static class VisitRelPlan extends VisitRelations.BuildPlan {
        public static final VisitRelPlan INSTANCE = new VisitRelPlan("CmsPage");
        static final VisitRelations visitRelations = INSTANCE.buildSafe();
        public VisitRelPlan(String majorEntityName) { super(majorEntityName); }
        @Override public VisitRelations.Builder planDefinition(Delegator delegator) throws Exception {
            return newBuilder(delegator)
                .entity("CmsPage")
                    .relationMajor("CmsPageTemplate")
                    .relation("CmsPageScriptAssoc")
                    .self()
                    .relation("CmsPageVersion")
                    .recall("PAGE_SCR_ASSOC")
                    .relation("CmsPageProductAssoc")
                    .relation("CmsPageAuthorization")
                    .relation("CmsPageSpecialMapping") // this treats the Primary process mapping as a non-major relation
                    .relationMajor("CmsProcessMapping") // this treats all remaining (non-primary) process mappings as major relations
                    .relationMajor("CmsViewMapping")
                .entity("CmsPageVersion")  
                    .self()
                    .relation("CmsPageVersionState")
                .entity("CmsPageSpecialMapping")    // NOTE: reversed self() order here avoids having to create special case conditions
                    .relationMajorNoFilter("CmsProcessMapping") // SPECIAL: in this case we want to cross even if includeMajorDeps==false, so prevent filter
                    .self()
                .entity("CmsPageScriptAssoc")    // NOTE: reversed self() order here avoids having to create special case conditions
                    .relationMajor("CmsScriptTemplate")
                    .selfStash("PAGE_SCR_ASSOC");
        }
    }

}
