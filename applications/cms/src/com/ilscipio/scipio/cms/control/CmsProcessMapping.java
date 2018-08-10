package com.ilscipio.scipio.cms.control;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;

import javax.servlet.http.HttpServletRequest;

import org.apache.commons.lang.StringUtils;
import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.UtilGenerics;
import org.ofbiz.base.util.UtilMisc;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.base.util.cache.UtilCache;
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.GenericEntityException;
import org.ofbiz.entity.GenericValue;
import org.ofbiz.entity.condition.EntityCondition;
import org.ofbiz.entity.condition.EntityOperator;
import org.ofbiz.webapp.control.RequestLinkUtil;

import com.ilscipio.scipio.ce.util.PathUtil;
import com.ilscipio.scipio.cms.CmsException;
import com.ilscipio.scipio.cms.CmsUtil;
import com.ilscipio.scipio.cms.content.CmsPage;
import com.ilscipio.scipio.cms.data.CmsDataException;
import com.ilscipio.scipio.cms.data.CmsDataException.CmsUniqueDataException;
import com.ilscipio.scipio.cms.data.CmsEntityVisit;
import com.ilscipio.scipio.cms.data.CmsEntityVisit.CmsEntityVisitor;
import com.ilscipio.scipio.cms.data.CmsEntityVisit.VisitRelation;
import com.ilscipio.scipio.cms.data.CmsEntityVisit.VisitRelations;
import com.ilscipio.scipio.cms.data.CmsMajorObject;
import com.ilscipio.scipio.cms.data.CmsObjectCache;
import com.ilscipio.scipio.cms.data.CmsObjectCache.CacheEntry;
import com.ilscipio.scipio.cms.data.CmsObjectCache.SimpleCacheEntry;

/**
 * Wraps and represents a CmsProcessMapping entity value.
 */
public class CmsProcessMapping extends CmsControlDataObject implements CmsMajorObject  {

    private static final long serialVersionUID = 716726095952003354L;

    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());    
    
    private static final CmsObjectCache<CmsProcessMapping> pathCache = CmsObjectCache.getGlobalCache("cms.control.processMapping.path"); 
    private static final UtilCache<String, Set<String>> rootControlUriCache = CmsObjectCache.getGenericGlobalCache("cms.control.processMapping.rootctrluri"); 
    
    public static final String SOURCE_FROM_CONTEXT_ROOT_DEFAULT = "D"; // NOTE: this in an entity indicator type - single char
    
    public static final boolean defaultMatchAnyTargetPath = false;
    
    // CMS: 2016: not applicable to local renders
    //public static final boolean defaultPageFromPathWildcard = false;
    
    public static final boolean defaultRequireExtraPathInfo = false;
    
    static final boolean USE_LIVE_PRIMARY_PAGE_ID_CACHE = true;
    
    // global defaults for "primary" process mappings
    static final String primaryTargetPathDefault = "/cmsPagePlainNoAuth"; // NOTE: this is relative! /control may get prefixed, depended on webapp config (web.xml)
    
    // NOTE: I'm leaving this to "Y" for the general case even though "createPage" will
    // specify "N" for this (and override this)
    static final String primaryActiveDefault = "Y"; // currently must be hardcoded to "Y", otherwise nothing would work

    // Cached process view mappings.
    private List<CmsProcessViewMapping> processViewMappings = null;
    // Home-made cache. Don't need UtilCache for this.
    private final Map<String, CacheEntry<CmsProcessViewMapping>> viewsPathAndNameCache = 
            new ConcurrentHashMap<String, CacheEntry<CmsProcessViewMapping>>();
    
    // NOTE: 2016: Optional is required for thread safety (preload)
    private Optional<CmsPage> defaultPage = null;
    
    /**
     * WARN: 2016-12: in live rendering, it is possible that the page linked here does not contain ourselves
     * as a primary process mapping, due to changes by backend and timing of loading the entities.
     * In addition, if it's there, it will not be "this" instance but another copy of it.
     * It is done this way so that the page can be cached globally; otherwise we would have to 
     * modify it after loading to point back to us, which would force us to disable page caching.
     */
    private Optional<CmsPage> primaryForPage = null;
    
    protected CmsProcessMapping(GenericValue entity) {
        super(entity);
    }
    
    public CmsProcessMapping(GenericValue entity, CmsPage primaryForPage) {
        super(entity);
        if (primaryForPage != null && CmsUtil.verboseOn()) {
            if (!primaryForPage.getId().equals(this.getPrimaryForPageId())) {
                throw new CmsDataException("Cms: Illegal state: initialized a "
                        + "primary process mapping with a page that does not match the mapping's primaryForPageId field");
            }
        }
        this.primaryForPage = Optional.ofNullable(primaryForPage);
    }

    public CmsProcessMapping(Delegator delegator, Map<String, ?> fields) {
        super(delegator, checkFields(fields, true));
    }
    
    protected CmsProcessMapping(CmsProcessMapping other, Map<String, Object> copyArgs) {
        super(other, copyArgs);
    }
    
    @Override    
    public void update(Map<String, ?> fields, boolean setIfEmpty) {
        super.update(checkFields(fields, false), setIfEmpty);
    }
    
    @Override
    public CmsProcessMapping copy(Map<String, Object> copyArgs) throws CmsException {
        return new CmsProcessMapping(this, copyArgs);
    }
    
    protected static <T> Map<String, T> checkFields(Map<String, T> fields, boolean isNew) {
        if (isNew || fields.containsKey("sourceFromContextRoot")) {
            ensureSourceFromContextRoot(fields);
        }
        return fields;
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
        // TODO: REVIEW: currently we load the primary page through the CmsPage.pageId cache,
        // and pass true here;
        // HOWEVER this means the page cannot be modified after load, which means the
        // page's primary mappings may be out of sync and may not necessarily contain
        // "our" instance.
        // for now this will probably not cause any issue, but subject to change in future.
        preloadWorker.preload(this.getPrimaryForPage(USE_LIVE_PRIMARY_PAGE_ID_CACHE));
        preloadWorker.preload(this.getPage(USE_LIVE_PRIMARY_PAGE_ID_CACHE));
        this.processViewMappings = preloadWorker.preloadDeep(this.getProcessViewMappings());
        // NOTE: here can't make viewsPathAndNameCache immutable
    }
    
    public void setPrimaryProcessMappingFields(Map<String, ?> fields, boolean useDefaults) {
        //entity.setString("pagePath", normalizePath(path));
        Map<String, Object> setFields = getPrimaryProcessMappingFields(getDelegator(), fields, useDefaults);
        if (!setFields.isEmpty()) {
            entity.setNonPKFields(setFields);
        }
    }
    
    static Map<String, Object> getPrimaryProcessMappingFields(Delegator delegator, Map<String, ?> fields, boolean useDefaults) {
        Map<String, Object> setFields = new HashMap<>();
        
        if (fields.containsKey("sourcePath") || useDefaults) {
            String sourcePath = (String) fields.get("sourcePath");
            if (UtilValidate.isEmpty(sourcePath)) {
                sourcePath = null;
            } else {
                sourcePath = CmsPage.normalizePath(sourcePath);
            }
            setFields.put("sourcePath", sourcePath);
        }
        
        if (fields.containsKey("targetPath") || useDefaults) {
            String targetPath = (String) fields.get("targetPath");
            if (UtilValidate.isEmpty(targetPath)) {
                targetPath = primaryTargetPathDefault;
            } else {
                targetPath = CmsPage.normalizePath(targetPath);
            }
            setFields.put("targetPath", targetPath);
        }
        
        String webSiteId = null;
        if (fields.containsKey("sourceWebSiteId") || useDefaults) {
            webSiteId = (String) fields.get("sourceWebSiteId");
            if (UtilValidate.isEmpty(webSiteId)) {
                webSiteId = null;
            }
            setFields.put("sourceWebSiteId", webSiteId);
        }
        
        if (fields.containsKey("sourceFromContextRoot") || useDefaults) {
            String sourceFromContextRoot = (String) fields.get("sourceFromContextRoot");
            if (UtilValidate.isEmpty(fields.get("sourceFromContextRoot"))) {
                // NOTE: 2017-12-05: it's very unclear whether this flag is a good idea,
                // so we leave it configurable.
                CmsWebSiteConfig webSiteConfig = CmsWebSiteInfo.getWebSiteConfigOrDefault(webSiteId);
                if (webSiteConfig.isApplyPrimaryPathFromContextRootDefaultAtStorage()) {
                    sourceFromContextRoot = webSiteConfig.getPrimaryPathFromContextRootDefault() ? "Y" : "N";
                // already done afterward
                //} else {
                //    sourceFromContextRoot = SOURCE_FROM_CONTEXT_ROOT_DEFAULT;
                }
            }
            setFields.put("sourceFromContextRoot", sourceFromContextRoot);
        }
        
        if (fields.containsKey("primaryForPageId") || useDefaults) {
            String primaryForPageId = (String) fields.get("primaryForPageId");
            if (UtilValidate.isEmpty(primaryForPageId)) {
                primaryForPageId = null;
            }
            setFields.put("primaryForPageId", primaryForPageId);
        }
        
        if (fields.containsKey("active") || useDefaults) {
            String active = (String) fields.get("active");
            if (UtilValidate.isEmpty(active)) {
                active = primaryActiveDefault;
            }
            setFields.put("active", active);
        }
        
        if (fields.containsKey("indexable") || useDefaults) {
            Object indexable = fields.get("indexable");
            if (UtilValidate.isEmpty(indexable)) {
                indexable = null;
            }
            setFields.put("indexable", indexable);
        }
        return setFields;
    }
    
    public static CmsProcessMapping createPrimaryProcessMapping(Delegator delegator, Map<String, ?> fields) {
        Map<String, Object> setFields = getPrimaryProcessMappingFields(delegator, fields, true);
        CmsProcessMapping processMapping = new CmsProcessMapping(delegator, setFields);

        // 2017-11-30: We should be OK setting this to true. I don't see a reason why
        // this will ever needs N for primary process mapping. The 
        // CmsProcessMapping.active should be enough.
        //Boolean viewMappingActive = processMapping.getActive();
        Boolean viewMappingActive = Boolean.TRUE;
        
        // create single view mapping
        // processMappingId="10000" targetViewName="CmsPage" pageId="10000" active="Y"
        CmsProcessViewMapping processViewMapping = new CmsProcessViewMapping(delegator,
                UtilMisc.toMap("targetViewName", "CmsPage", "pageId", processMapping.getPrimaryForPageId(),
                        "active", viewMappingActive), processMapping);
        processMapping.addProcessViewMapping(processViewMapping);
        return processMapping;
    }
    
    protected void addProcessViewMapping(CmsProcessViewMapping processViewMapping) {
        List<CmsProcessViewMapping> processViewMappings = this.processViewMappings;
        if (processViewMappings == null) {
            processViewMappings = new ArrayList<>();
        }
        processViewMappings.add(processViewMapping);
        this.processViewMappings = processViewMappings;
    }
    
    
    /**
     * Verifies if our current fields are unique using
     * sourceWebSiteId+sourcePath+sourceFromContextRoot as candidate key,
     * throws exception if not.
     * BEST-EFFORT, not fully guaranteed because:
     * 1) no db locking
     * 2) when sourceFromContextRoot=N we don't bother looking up the prefix from web.xml
     * <p>
     * NOTE: this is now enforced by db unique index, but no harm in making
     * clearer error message.
     */
    public void verifyUniquePath() throws CmsException {
        List<CmsProcessMapping> others = getWorker().findAll(getDelegator(), 
                UtilMisc.toMap("sourceWebSiteId", getSourceWebSiteId(), 
                        "sourcePath", getSourcePath(), 
                        "sourceFromContextRoot", getSourceFromContextRootStr()), null, false);
        if (others == null || others.size() <= 0) {
            return;
        } else if (others.size() == 1) {
            if (others.get(0).getId().equals(this.getId())) {
                return;
            }
        }
        throw new CmsUniqueDataException("Duplicate mapping path detected: " + getLogPathRepr()); // TODO: localize
    }
    
    public String getLogPathRepr() {
        return "[sourceWebSiteId: " + getSourceWebSiteId() + ", sourcePath: " + getSourcePath()
        + ", sourceFromContextRoot: " + getSourceFromContextRoot() + "]";
    }
    
    @Override
    public void store() throws CmsException {
        String id = getId();
        String primaryForPageId = getPrimaryForPageId();
        
        // 2016: this field cannot be null anymore
        if (UtilValidate.isEmpty(getSourceFromContextRootStr())) { // use string method here, not the other one! other returns null for D.
            throw new CmsDataException("Trying to " + "create or update CmsProcessMapping" + (id != null ? " '" + id + "'" : "") + " but"
                    + " sourceFromContextRoot field is empty - required (for default, use special value " + SOURCE_FROM_CONTEXT_ROOT_DEFAULT + ")");
        }
        // NOTE: unique path is also enforced by DB if it supports unique indexes
        try {
            verifyUniquePath();
        } catch(CmsUniqueDataException e) {
            throw new CmsUniqueDataException("Cannot create or update CmsProcessMapping" + (id != null ? " '" + id + "'" : "") 
                    + " with website and path " + getLogPathRepr() 
                    + "; mapping/path combination already present in system" + (id != null ? " (processMappingId: " + id + ")" : ""));
        }
        
        // for primary mappings: read CmsPageSpecialMapping.
        // if this process mapping is not a new record and the sourceWebSiteId has changed,
        // the CmsPageSpecialMapping record must be deleted and recreated after.
        GenericValue pagePrimMappingEntity = null;
        if (UtilValidate.isNotEmpty(primaryForPageId) && UtilValidate.isNotEmpty(id)) {
            try {
                List<GenericValue> pagePrimMappingValues = getDelegator().findByAnd("CmsPageSpecialMapping", 
                        UtilMisc.toMap("processMappingId", id, "mappingTypeId", "CMS_PGSPCMAP_PRIMARY"), null, false);
                if (pagePrimMappingValues.size() >= 1) {
                    if (pagePrimMappingValues.size() > 1) {
                        // trying to delete automatically will probably fail, so just abort with a clear message now
                        // it should be impossible for our java code to create such a situation, so it's out of our hands
                        throw new CmsDataException("Cms: Invalid or corrupt data detected: Primary process mapping '" + id + "' has multiple CmsPageSpecialMapping"
                                + " entity values of type CMS_PGSPCMAP_PRIMARY! It must have exactly one, for page '" + primaryForPageId + "'"
                                + "; you will have to correct this situation manually (likely cause: bad seed data or manual data edit)");
                    }
                    pagePrimMappingEntity = pagePrimMappingValues.get(0);
                    if (!pagePrimMappingEntity.getString("webSiteId").equals(this.getSourceWebSiteId()) || 
                        !pagePrimMappingEntity.getString("pageId").equals(primaryForPageId)) {
                        
                        // we do not expect the pageId to change. our java code should never produce this.
                        // but in this case we can safely fix it.
                        if (!pagePrimMappingEntity.getString("pageId").equals(primaryForPageId)) {
                            Debug.logWarning("Cms: Invalid or corrupt data detected: Primary process mapping '" + id + "' was associated "
                                    + " to a CmsPageSpecialMapping CMS_PGSPCMAP_PRIMARY type record pointing to the wrong pageId (detected: " + pagePrimMappingEntity.getString("pageId") + ", "
                                    + "expected: " + primaryForPageId + "); the error will be automatically corrected", module);
                        }

                        if (CmsUtil.verboseOn()) {
                            Debug.logInfo("Cms: Primary process mapping '" + id + "': existing CmsPageSpecialMapping CMS_PGSPCMAP_PRIMARY type record"
                                    + " needs to be updated; removing existing record (pk change)", module);
                        }
                        getDelegator().removeValue(pagePrimMappingEntity);
                        pagePrimMappingEntity = null;
                    }
                } else {
                    if (this.isEntityPersisted()) {
                        // this shouldn't happen, but we can try to correct 
                        Debug.logWarning("Cms: Primary process mapping '" + id + "' was missing a corresponding "
                                + " CmsPageSpecialMapping CMS_PGSPCMAP_PRIMARY type record; will attempt to automatically correct"
                                + " (otherwise will fail with duplicate primary key when creating CmsPageSpecialMapping)", module);
                    }
                }
            } catch(GenericEntityException e) {
                throw new CmsDataException("Error reading or removing existing CmsPageSpecialMapping CMS_PGSPCMAP_PRIMARY type record for primary CmsProcessMapping '" 
                        + id + "': " + e.getMessage(), e);
            }
        }
        
        super.store();
        id = getId();
        primaryForPageId = getPrimaryForPageId();
        
        // create new CmsPageSpecialMapping if applicable and needed
        if (UtilValidate.isNotEmpty(primaryForPageId) && pagePrimMappingEntity == null) {
            if (CmsUtil.verboseOn()) {
                Debug.logInfo("Cms: Primary process mapping '" + id + "': creating new CmsPageSpecialMapping CMS_PGSPCMAP_PRIMARY type record for " + getLogPathRepr(), module);
            }
            try {
                GenericValue value = getDelegator().makeValue("CmsPageSpecialMapping", 
                        UtilMisc.toMap("processMappingId", id, "pageId", primaryForPageId, "webSiteId", getSourceWebSiteId(), "mappingTypeId", "CMS_PGSPCMAP_PRIMARY"));
                value.create();
            } catch(GenericEntityException e) {
                throw new CmsDataException("Error creating CmsPageSpecialMapping CMS_PGSPCMAP_PRIMARY type record for primary CmsProcessMapping '" + id + "': " + e.getMessage()
                        + " (Was there already a primary mapping for this page and website? There can be at most one primary mapping per page per website)", e);
            }
        }
        
        // store process view mappings if any were added or queried
        if (UtilValidate.isNotEmpty(this.processViewMappings)) {
            for(CmsProcessViewMapping mapping : this.processViewMappings) {
                // NOTE: check and update any missing essential IDs (when create)
                if (UtilValidate.isEmpty(mapping.getParentProcessMappingId())) {
                    mapping.setParentProcessMappingId(id);
                }
                // IF this is a primary process mapping, make sure set CmsProcessViewMapping.pageId to same as primaryForPageId
                if (UtilValidate.isNotEmpty(primaryForPageId) && !primaryForPageId.equals(mapping.getPageId())) {
                    mapping.setPageId(primaryForPageId);
                }
                mapping.store();
            }
        }
        clearCachedProcessViewMappings();
    }

    @Override
    public int remove() throws CmsException {
        return remove(true);
    }
    
    public int remove(boolean removeRelatedOrphaned) throws CmsException {
        int rowsAffected = 0;
        
        CmsPage page = null;
        if (removeRelatedOrphaned) {
            page = getPage(false);
        }
        
        try {
            Delegator delegator = getDelegator();
            
            String primaryForPageId = getPrimaryForPageId();
            if (UtilValidate.isNotEmpty(primaryForPageId)) {
                rowsAffected += getDelegator().removeByAnd("CmsPageSpecialMapping", UtilMisc.toMap("processMappingId", this.getId()));
            }
            
            // Can't do it this way; may have pages
            // Remove all associated process view mappings
            //getDelegator().removeByAnd("CmsProcessViewMapping", 
            //        UtilMisc.toMap("processMappingId", getId()));
            List<CmsProcessViewMapping> processViewMappings = getProcessViewMappings(false);
            if (UtilValidate.isNotEmpty(processViewMappings)) {
                for(CmsProcessViewMapping processViewMapping : processViewMappings) {
                    rowsAffected += processViewMapping.remove(removeRelatedOrphaned);
                }
            }
            
            clearCachedProcessViewMappings();
            
            rowsAffected += delegator.removeValue(entity);
        }
        catch(GenericEntityException e) {
            throw makeRemoveException(e);
        }
        
        if (removeRelatedOrphaned) {
            if (page != null) {
                rowsAffected += page.removeIfOrphan();
            }
        }
        
        return rowsAffected;
    }

    /**
     * internal page lookup method. this is used to avoid duplicate CmsPage references across
     * related CmsProcess(View)Mapping.
     */
    protected CmsPage findPageById(String pageId, boolean useCache) {
        Optional<CmsPage> page = this.primaryForPage;
        if (page != null && page.isPresent() && pageId.equals(page.get().getId())) {
            return page.get();
        }
        // NOTE: there could be a reason related to preload to skip this part, but it currently should not happen
        if (true) {
            page = this.defaultPage;
            if (page != null && page.isPresent() && pageId.equals(page.get().getId())) {
                return page.get();
            }
        }
        return CmsPage.getWorker().findById(getDelegator(), pageId, useCache);
    }
    
    public Boolean getActive() {
        return entity.getBoolean("active");
    }
    
    public boolean isActiveLogical() {
        return isActiveLogical(getActive());
    }
    
    public void setActive(Boolean active) {
        entity.set("active", active);
    }
    
    // CMS: 2016: not applicable to local renders
//    public Boolean getPageFromPathWildcard() {
//        return getBooleanField("pageFromPathWildcard");
//    }
//    
//    public void setPageFromPathWildcard(Boolean pageFromPathWildcard) {
//        setBooleanField("pageFromPathWildcard", pageFromPathWildcard);
//    }
    
    public Boolean getRequireExtraPathInfo() {
        return entity.getBoolean("requireExtraPathInfo");
    }
    
    public void setRequireExtraPathInfo(Boolean requireExtraPathInfo) {
        entity.set("requireExtraPathInfo", requireExtraPathInfo);
    }
    
    public String getSourcePathExpanded(String defaultSourceServletPath, boolean defaultSourceFromContextRoot) {
        if (isSourceFromContextRootLogical(defaultSourceFromContextRoot)) {
            return getSourcePath();
        } else {
            return PathUtil.concatPaths(defaultSourceServletPath, getSourcePath());
        }
    }
    
    /**
     * 2016: automatically (re-)lookups up website config.
     * <p>
     * NOTE: live code should use {@link #getSourcePathExpanded(String, boolean)}.
     */
    public String getSourcePathExpanded() { // 
        String webSiteId = getSourceWebSiteId();
        String sourcePath = getSourcePath();
        if (UtilValidate.isEmpty(webSiteId)) {
            Debug.logWarning("Cms: sourceWebSiteId is null on CmsProcessMapping " + getId() + "; cannot determine expanded path", module);
            return sourcePath;
        }
        Boolean sourceFromContextRoot = getSourceFromContextRoot();
        if (Boolean.TRUE.equals(sourceFromContextRoot)) {
            return sourcePath;
        }
        CmsWebSiteConfig webSiteConfig = CmsWebSiteInfo.getWebSiteInfo(webSiteId).getWebSiteConfig();
        if (sourceFromContextRoot == null) {
            sourceFromContextRoot = webSiteConfig.getDefaultSourceFromContextRoot();
        }
        if (Boolean.TRUE.equals(sourceFromContextRoot)) {
            return sourcePath;
        } else {
            try {
                String prefix = webSiteConfig.getDefaultSourceServletPath();
                if (UtilValidate.isEmpty(prefix) || "/".equals(prefix)) {
                    return sourcePath;
                } else {
                    return PathUtil.concatPaths(prefix, sourcePath);
                }
            } catch (Exception e) {
                throw new CmsException(e);
            }
        }
    }
    
    public String getTargetPath(HttpServletRequest request) {
        String queryString = request.getQueryString();
        StringBuffer targetPath = new StringBuffer(getTargetPath());
        if (StringUtils.isNotEmpty(queryString)) {
          if (targetPath.indexOf("?") > -1) { 
              targetPath.append("&");
          } else {
              targetPath.append("?");              
          }
          targetPath.append(queryString);
        }
        return targetPath.toString();
    }    
    
    public String getForwardPath() {
        return entity.getString("forwardPath");
    }
    
    public String getForwardPathRequestUri(HttpServletRequest request, 
            String defaultForwardServletPath, String defaultTargetServletPath, boolean defaultForwardExtraPathInfo) {
        String forwardPath = getForwardPath();
        if (UtilValidate.isNotEmpty(forwardPath)) {
            if (!isForwardFromContextRootLogical(defaultForwardExtraPathInfo)) {
                // Must append the default context root here
                forwardPath = PathUtil.concatPaths(defaultForwardServletPath, forwardPath);
            }
            return CmsControlUtil.normalizeContextRootRequestPath(forwardPath);
        } else {
            String targetPath = getTargetPath();
            if (UtilValidate.isNotEmpty(targetPath)) {
                return CmsControlUtil.normalizeContextRootRequestPath(PathUtil.concatPaths(defaultTargetServletPath, targetPath));
            } else {
            
                return null;
            }
        }
    }    
 
    
    public Boolean getForwardExtraPathInfo() {
        return entity.getBoolean("forwardExtraPathInfo");
    }
    
    public boolean isForwardExtraPathInfoLogical(boolean defaultForwardExtraPathInfo) {
        return isForwardExtraPathInfoLogical(getForwardExtraPathInfo(), defaultForwardExtraPathInfo);
    }
    
    public static boolean isForwardExtraPathInfoLogical(Boolean forwardFromContextRoot, boolean defaultForwardExtraPathInfo) {
        return Boolean.TRUE.equals(forwardFromContextRoot) || 
                (forwardFromContextRoot == null && defaultForwardExtraPathInfo);
    }
    
    
    public Boolean getForwardFromContextRoot() {
        return entity.getBoolean("forwardFromContextRoot");
    }
    
    public boolean isForwardFromContextRootLogical(boolean defaultForwardExtraPathInfo) {
        return isForwardFromContextRootLogical(getForwardFromContextRoot(), defaultForwardExtraPathInfo);
    }

    public static boolean isForwardFromContextRootLogical(Boolean forwardFromContextRoot, boolean defaultForwardExtraPathInfo) {
        return (forwardFromContextRoot != null) ? forwardFromContextRoot : defaultForwardExtraPathInfo;
    }
 

    public String getTargetServletPath() {
        return entity.getString("targetServletPath");
    }
    
    public Boolean getMatchAnyTargetPath() {
        return entity.getBoolean("matchAnyTargetPath");
    }
    
    
    
    public String getTargetPath() {
        return entity.getString("targetPath");
    }
    
    public String getSourcePath() {
        return entity.getString("sourcePath");
    }
    
    public Boolean getSourceFromContextRoot() {
        String res = getSourceFromContextRootStr();
        if ("Y".equals(res)) {
            return Boolean.TRUE;
        } else if ("N".equals(res)) {
            return Boolean.FALSE;
        } else {
            // NOTE: this covers special "D" default value
            return null;
        }
    }
    
    public String getSourceFromContextRootStr() {
        return entity.getString("sourceFromContextRoot");
    }
    
    public boolean isSourceFromContextRootLogical(boolean defaultSourceFromContextRoot) {
        return isSourceFromContextRootLogical(getSourceFromContextRoot(), defaultSourceFromContextRoot);
    }
    
    public static boolean isSourceFromContextRootLogical(Boolean sourceFromContextRoot, boolean defaultSourceFromContextRoot) {
        return (sourceFromContextRoot != null) ? sourceFromContextRoot : defaultSourceFromContextRoot;
    }
    
    public static EntityCondition makeSourceFromContextRootCondition(boolean defaultSourceFromContextRoot) {
        if (defaultSourceFromContextRoot) {
            return EntityCondition.makeCondition(EntityCondition.makeCondition("sourceFromContextRoot", SOURCE_FROM_CONTEXT_ROOT_DEFAULT),
                    EntityOperator.OR,
                    EntityCondition.makeCondition("sourceFromContextRoot", "Y")
                    );
        } else {
            return EntityCondition.makeCondition("sourceFromContextRoot", "Y");
        }
    }
    
    public static EntityCondition makeNotSourceFromContextRootCondition(boolean defaultSourceFromContextRoot) {
        if (defaultSourceFromContextRoot) {
            return EntityCondition.makeCondition("sourceFromContextRoot", "N");
        } else {
            return EntityCondition.makeCondition(EntityCondition.makeCondition("sourceFromContextRoot", SOURCE_FROM_CONTEXT_ROOT_DEFAULT),
                    EntityOperator.OR,
                    EntityCondition.makeCondition("sourceFromContextRoot", "N")
                    );
        }
    }
    
    public String getSourceWebSiteId() {
        return entity.getString("sourceWebSiteId");
    }    

    public String getPageId() {
        return entity.getString("pageId");
    }    
    
    public String getPrimaryForPageId() { // 2016: new
        return entity.getString("primaryForPageId");
    }        
    
    public String getLogIdRepr() {
        return "[mapping id: " + getId() + "; webSiteId: " + getSourceWebSiteId() + 
                "; source path: " + getSourcePath() + "]";
    }

    public void setForwardPath(String forwardPath) {
        entity.setString("forwardPath", forwardPath);
    }
    
    public void setForwardFromContextRoot(Boolean forwardFromContextRoot) {
        entity.set("forwardFromContextRoot", forwardFromContextRoot);
    }
    
    public void setForwardExtraPathInfo(Boolean forwardExtraPathInfo) {
        entity.set("forwardExtraPathInfo", forwardExtraPathInfo);
    }
    
    public void setTargetServletPath(String targetServletPath) {
        entity.setString("targetServletPath", targetServletPath);
    }
    
    public void setMatchAnyTargetPath(Boolean matchAnyTargetPath) {
        entity.set("matchAnyTargetPath", matchAnyTargetPath);
    }
    
    public void setTargetPath(String targetPath) {
        entity.setString("targetPath", targetPath);
    }
    
    public void setSourcePath(String sourcePath) {
        entity.setString("sourcePath", sourcePath);
    }

    public void setSourceFromContextRoot(Boolean sourceFromContextRoot) {
        entity.set("sourceFromContextRoot", sourceFromContextRoot != null ? sourceFromContextRoot : SOURCE_FROM_CONTEXT_ROOT_DEFAULT);
    }
    
    public static void ensureSourceFromContextRoot(Map<String, ?> fields) {
        if (UtilValidate.isEmpty((String) fields.get("sourceFromContextRoot"))) {
            UtilGenerics.<String, Object> checkMap(fields).put("sourceFromContextRoot", SOURCE_FROM_CONTEXT_ROOT_DEFAULT);
        }
    }
    
    public void setSourceWebSiteId(String sourceWebSiteId) {
        entity.setString("sourceWebSiteId", sourceWebSiteId);
    }    

    public void setPageId(String pageId) {
        entity.setString("pageId", pageId);
        this.defaultPage = null;
    }        
    
    public void setPrimaryForPageId(String primaryForPageId) { // 2016: new
        entity.setString("primaryForPageId", primaryForPageId);
        this.primaryForPage = null;
    }        
    
    public CmsPage getPage() throws CmsException {
        return getPage(false);
    } 
    
    public CmsPage getPage(boolean useCache) throws CmsException {
        Optional<CmsPage> page = this.defaultPage;
        if (page == null) {
            String pageId = getPageId();
            if (UtilValidate.isNotEmpty(pageId)) {
                page = Optional.ofNullable(findPageById(pageId, useCache));
                // do NOT do this, for same reasons as below
//                // ENSURE the page has us as backreference to us if we are its primary
//                if (page.isPresent()) {
//                    page.get().checkAddPrimaryProcessMapping(this);
//                }
            } else {
                page = Optional.empty();
            }
            this.defaultPage = page;
        }
        return page.orElse(null); 
    }
    
    public CmsPage getPrimaryForPage() throws CmsException {
        return getPrimaryForPage(false);
    } 
    
    public CmsPage getPrimaryForPage(boolean useCache) throws CmsException {
        Optional<CmsPage> page = this.primaryForPage;
        if (page == null) {
            String pageId = getPrimaryForPageId();
            if (UtilValidate.isNotEmpty(pageId)) {
                page = Optional.ofNullable(findPageById(pageId, useCache));
                // do NOT do this, because it prevents us from exploiting the pageId global cache,
                // and it will probably be okay if we don't do it; it just means an extra
                // CmsProcessMapping instance in memory, but that is less costly than extra CmsPage instances
//                // ENSURE the page has us as backreference to us if we are its primary
//                if (page.isPresent()) {
//                    page.get().checkAddPrimaryProcessMapping(this);
//                }
            } else {
                page = Optional.empty();
            }
            this.primaryForPage = page;
        }
        return page.orElse(null); 
    }

    public Boolean getIndexable() {
        return entity.getBoolean("indexable");
    }
    
    public boolean isIndexableLogical(boolean mappingsIndexableDefault) {
        Boolean indexable = getIndexable();
        return (indexable != null) ? indexable : mappingsIndexableDefault;
    }

    public void setIndexable(Boolean indexable) {
        entity.set("indexable", indexable);
    }

    
    private static CmsObjectCache<CmsProcessMapping> getPathCache() {
        return pathCache;
    }

    
    public static String getExtraPathInfo(String requestPath, String sourcePath) {
        if (requestPath.startsWith(sourcePath)) {
            return requestPath.substring(sourcePath.length());
        } else {
            return null;
        } 
    }
    

    public static String getRequestPath(HttpServletRequest request) {
        return CmsProcessMapping.getMappingPath(request, false, true);
    }
    
    public static String getMappingPath(HttpServletRequest req) {
       return getMappingPath(req, true, false);
    }
    
    public static String getMappingPath(HttpServletRequest req, boolean withQueryString, boolean normalize) {
        // Cms: I think matching against contextPath will be counter-productive for users
        //String contextPath = req.getContextPath();   // /mywebapp
        String servletPath = req.getServletPath();   // /servlet/MyServlet
        String pathInfo = req.getPathInfo();         // /a/b;c=123
        String queryString = req.getQueryString();          // d=789

        // Reconstruct original requesting URL
        // Cms: contextPath may be root (/)
        //String path = contextPath+servletPath;  
        //String path = PathUtil.concatPaths(contextPath, servletPath);
        String path = UtilValidate.isNotEmpty(servletPath) ? servletPath : "/"; // Always start with a "/" for us
        
        if (UtilValidate.isNotEmpty(pathInfo)) {
            path = PathUtil.concatPaths(path, pathInfo);
        }        
        
        if (normalize) {
            path = CmsControlUtil.normalizeContextRootRequestPath(path);
        }
        
        if (withQueryString && UtilValidate.isNotEmpty(queryString)) {
            path += "?" + queryString;
        }        
        // Cms: Let's stay case-sensitive like Ofbiz is, for now...
        // path = path.toLowerCase();  
        // Cms: We can't do this...
        //if (path.endsWith(".js") || path.endsWith(".png") || path.endsWith(".css")
        //        || path.endsWith(".jpeg") || path.endsWith(".gif") || path.endsWith(".jpg")) {
        //    return null;
        //} else {
        //    return path;
        //}
        return path;
    }
    
    @Override
    public Map<String, Object> getDescriptor(Locale locale) {
        Map<String, Object> map = super.getDescriptor(locale);
        map.putAll(UtilMisc.toMap(
                "id", getId(), 
                "sourcePath", getSourcePath(), 
                "sourceWebSiteId", getSourceWebSiteId(),
                "targetPath", getTargetPath(),                
                "pageId", getPageId()
        ));
        return map;
    }

    public List<CmsProcessViewMapping> getProcessViewMappings() throws CmsException {
        return getProcessViewMappings(false);
    }
    
    public List<CmsProcessViewMapping> getProcessViewMappings(boolean useCache) throws CmsException {
        List<CmsProcessViewMapping> processViewMappings = this.processViewMappings;
        if (processViewMappings == null) {
            processViewMappings = CmsProcessViewMapping.getWorker().findByProcess(getDelegator(), this.getId(), useCache);
            // we MUST update them all immediately to point back to us... otherwise preload will explode
            for(CmsProcessViewMapping processViewMapping : processViewMappings) {
                processViewMapping.setParentProcessMappingRefOnly(this);
            }
            this.processViewMappings = processViewMappings;
        }
        return processViewMappings;
    }
    
    public CmsProcessViewMapping getProcessViewMapping(String requestServletPath, String requestPath, String viewName, 
            String defaultTargetServletPath, Boolean defaultMatchAnyTargetPath) throws CmsException {
        return getProcessViewMapping(requestServletPath, requestPath, viewName, defaultTargetServletPath, 
                defaultMatchAnyTargetPath, false);
    }
    
    public CmsProcessViewMapping getProcessViewMapping(String requestServletPath, String requestPath, String viewName, 
            String defaultTargetServletPath, Boolean defaultMatchAnyTargetPath, boolean useCache) throws CmsException {
        // Previously:
        //return CmsProcessViewMapping.findByProcessAndView(getDelegator(), this, requestServletPath, requestPath, viewName, 
        //        defaultTargetServletPath, defaultMatchAnyTargetPath);
        
        // NOTE: passing true here instead of useCache, because this local cache is really of no consequence (perf only),
        // it's only the db lookup further down that could affect correctness
        boolean useLocalCache = isUseLocalObjCache(true); 
        
        Map<String, CacheEntry<CmsProcessViewMapping>> cache = this.viewsPathAndNameCache;
        String key = requestPath + "::" + viewName;

        // Note: No complicated sync required because doesn't matter if we run filter twice in multi threads
        CacheEntry<CmsProcessViewMapping> cacheEntry = null;
        if (useLocalCache) {
            cacheEntry = cache.get(key);
        }
        
        CmsProcessViewMapping viewMapping = null;
        
        // NOTE: 2016: viewsPathAndNameCache is a ConcurrentHashMap, 
        // there is no real need to synchronize on it here, as two stores of same value
        // will be the same during rendering (esp. since the viewMappings list itself is also cached).
        // NOTE: 2016: also notable here, is that the source viewMappings list now also gets cached to help ensure consistency.
        
        if (cacheEntry == null) {
            List<CmsProcessViewMapping> viewMappings = getProcessViewMappings(useCache);
            
            viewMapping = CmsProcessViewMapping.findBestProcessViewMatch(this, viewMappings, requestServletPath, requestPath, 
                    viewName, defaultTargetServletPath, defaultMatchAnyTargetPath);
            
            if (viewMapping != null) {
                // Important: Must set this before setting in cache!
                viewMapping.setParentProcessMappingRefOnly(this);
            }
            
            if (useLocalCache) {
                cache.put(key, new SimpleCacheEntry<CmsProcessViewMapping>(viewMapping));
            }
        } else {
            if (cacheEntry.hasValue()) {
                viewMapping = cacheEntry.getValue();
            }
        }

        return viewMapping;
    }
    
    public static Map<String, List<CmsProcessMapping>> makeProcessMappingsByWebSiteIdMap(List<CmsProcessMapping> primaryProcessMappingsList) {
        Map<String, List<CmsProcessMapping>> primaryProcessMappingsByWebSiteId = new HashMap<>();
        for(CmsProcessMapping primaryProcessMapping : primaryProcessMappingsList) {
            String sourceWebSiteId = primaryProcessMapping.getSourceWebSiteId();
            List<CmsProcessMapping> primaryProcessMappings = primaryProcessMappingsByWebSiteId.get(sourceWebSiteId);
            if (primaryProcessMappings == null) {
                primaryProcessMappings = new ArrayList<>();
            }
            primaryProcessMappings.add(primaryProcessMapping);
            primaryProcessMappingsByWebSiteId.put(sourceWebSiteId, primaryProcessMappings);
        }
        return primaryProcessMappingsByWebSiteId;
    }
    
    protected void clearCachedProcessViewMappings() {
        this.processViewMappings = null;
        this.viewsPathAndNameCache.clear();
    }

    public static EntityCondition makeSourceMatchesRequestPathCond(String sourcePathFieldName, String requestPath) { // Assumes requestPath normalized.
        // The only exact solution I could find was to break down the request path into all possible matching source paths.
        // i.e. if you have /first/second/third, then generate compares for:
        // /
        // /first
        // /first/second
        // /first/second/third

        List<String> subPaths = PathUtil.makeAllRequestPathPrefixes(requestPath);
        
        List<EntityCondition> condList = new ArrayList<>();
        for (String subPath : subPaths) {
            condList.add(EntityCondition.makeCondition(sourcePathFieldName, EntityOperator.EQUALS, subPath));
        }
        return EntityCondition.makeCondition(condList, EntityOperator.OR);
    }
    
    /**
     * Checks if process source path matches request path (from servlet context root). Assumes all normalized. 
     * <p>
     * Also see {@link #findByRequest} (implementations must match!).
     * 
     * @see #findByRequest
     */
    public static boolean sourceMatchesRequestPath(String sourcePath, String requestPath) {
        //return StringUtils.equals(sourcePath, requestPath);
        return PathUtil.isPathPrefixOf(requestPath, sourcePath);
    }
    
    /**
     * Checks if process/view target path matches request path (from servlet/controller root). Assumes all normalized. 
     */
    public static boolean targetMatchesRequestPath(String targetPath, String requestPath) {
        //return StringUtils.equals(targetPath, requestPath);
        return PathUtil.isPathPrefixOf(requestPath, targetPath);
    }
    
    /**
     * Similar to targetMatchesRequestPath but doesn't work with path but simple control URI name.
     */
    public static boolean targetMatchesControllerRequestUri(String targetPath, String requestUri) {
        if (targetPath.startsWith("/")) {
            targetPath = targetPath.substring(1);
        }
        return PathUtil.isPathPrefixOf(requestUri, targetPath);
    }

    public static String targetPathToRequestUri(String targetPath) {
        return PathUtil.ensureNoDelims(targetPath);
    }
    
    /**
     * Returns active URIs normalized from webapp context root.
     * TODO?: locale is currently ignored. might be involved in future.
     */
    public static List<String> getWebsiteActiveIndexableUris(Delegator delegator, String webSiteId, Locale contentLocale, boolean useCache) {
        List<CmsProcessMapping> mappingList = CmsProcessMapping.getWorker().findByWebSiteId(delegator, webSiteId, useCache);
        List<String> uriList = new ArrayList<>(mappingList.size());
        
        CmsWebSiteConfig webSiteConfig = CmsWebSiteInfo.getWebSiteConfigOrDefault(webSiteId);
        
        boolean defaultSourceFromContextRoot = webSiteConfig.getDefaultSourceFromContextRoot();
        String defaultSourceServletPath = webSiteConfig.getDefaultSourceServletPath();
        boolean defaultIsIndexable = webSiteConfig.getMappingsIndexableDefault();
        
        for(CmsProcessMapping mapping : mappingList) {
            if (!mapping.isActiveLogical()) continue;
            if (!mapping.isIndexableLogical(defaultIsIndexable)) continue;
            String uri = mapping.getSourcePathExpanded(defaultSourceServletPath, defaultSourceFromContextRoot);
            if (uri != null) {
                uriList.add(uri);
            }
        }
        return uriList;
    }

    public static List<CmsProcessMapping> sortProcessMappingsByActive(List<CmsProcessMapping> mappings) {
        List<CmsProcessMapping> activeMappings = new ArrayList<>();
        List<CmsProcessMapping> nullactiveMappings = new ArrayList<>();
        List<CmsProcessMapping> inactiveMappings = new ArrayList<>();
        for(CmsProcessMapping mapping : mappings) {
            Boolean active = mapping.getActive();
            if (Boolean.TRUE.equals(active)) {
                activeMappings.add(mapping);
            } else if (Boolean.FALSE.equals(active)) {
                inactiveMappings.add(mapping);
            } else {
                nullactiveMappings.add(mapping);
            }
        }
        List<CmsProcessMapping> res = new ArrayList<>(mappings.size());
        res.addAll(activeMappings);
        res.addAll(nullactiveMappings);
        res.addAll(inactiveMappings);
        return res;
    }
    
    public static List<CmsProcessMapping> sortProcessMappingsSpecWebSiteFirst(List<CmsProcessMapping> mappings, String webSiteId) {
        List<CmsProcessMapping> match = new ArrayList<>();
        List<CmsProcessMapping> nonMatch = new ArrayList<>();
        for(CmsProcessMapping mapping : mappings) {
            if (webSiteId != null && webSiteId.equals(mapping.getSourceWebSiteId())) {
                match.add(mapping);
            } else {
                nonMatch.add(mapping);
            }
        }
        List<CmsProcessMapping> res = new ArrayList<>(mappings.size());
        res.addAll(match);
        res.addAll(nonMatch);
        return res;
    }
    
    public static Set<String> getWebSiteIdsFromMappings(List<CmsProcessMapping> mappings) {
        Set<String> webSiteIds = new LinkedHashSet<>();
        for(CmsProcessMapping mapping : mappings) {
            String webSiteId = mapping.getSourceWebSiteId();
            if (UtilValidate.isNotEmpty(webSiteId)) {
                webSiteIds.add(webSiteId);
            }
        }
        return webSiteIds;
    }
    
    @Override
    public ProcessMappingWorker getWorkerInst() {
        return ProcessMappingWorker.worker;
    }
    
    public static ProcessMappingWorker getWorker() {
        return ProcessMappingWorker.worker;
    }
    
    public static class ProcessMappingWorker extends ControlDataObjectWorker<CmsProcessMapping> {

        private static final ProcessMappingWorker worker = new ProcessMappingWorker();
        
        protected ProcessMappingWorker() {
            super(CmsProcessMapping.class);
        }

        // FIXME? These could potentially also include sourceFromContextRoot... but hard to communicate in CMS apps
        private static final List<String> logicalPkFieldNames = Collections.unmodifiableList(Arrays.asList(new String[] { 
                "sourceWebSiteId", "sourcePath" 
        }));
        
        @Override
        public List<String> getLogicalPkFieldNames(Delegator delegator) {
            return logicalPkFieldNames;
        }

        @Override
        public String getControlPageIdFieldName() {
            return "pageId";
        }
        
        @Override
        public CmsProcessMapping makeFromValue(GenericValue value) throws CmsException {
            return new CmsProcessMapping(value);
        }

        @Override
        public CmsProcessMapping makeFromFields(Delegator delegator, Map<String, ?> fields) throws CmsException {
            return new CmsProcessMapping(delegator, fields);
        }
        
        public List<CmsProcessMapping> findByWebSiteId(Delegator delegator, String webSiteId, boolean useCache) throws CmsException {
            return findAll(delegator, UtilMisc.toMap("sourceWebSiteId", webSiteId), 
                    UtilMisc.toList("sourcePath ASC"), isUseDbCacheStatic(useCache));
        }
        
        public static class FindByRequestResult {
            public final CmsProcessMapping mapping;
            public final String requestPath;
            public final String extraPathInfo;
            
            protected FindByRequestResult(CmsProcessMapping mapping,
                    String requestPath, String extraPathInfo) {
                this.mapping = mapping;
                this.requestPath = requestPath;
                this.extraPathInfo = extraPathInfo;
            }
        }
        
        /**
         * Returns the mapping with the given path which should be unique within one
         * webSiteId.
         * <p>
         * 2016: support previewPath
         * <p>
         * NOTE: if preview mode, caller should also pass cache false.
         * 
         * @param request OPTIONAL request, used for logging
         */
        public FindByRequestResult findByRequestPath(Delegator delegator, 
                String path, String webSiteId, String defaultSourceServletPath, boolean defaultSourceFromContextRoot, 
                boolean preview, boolean useCache, HttpServletRequest request) throws CmsException {        

            boolean useGlobalCache = isUseGlobalObjCacheStatic(useCache);
            CmsObjectCache<CmsProcessMapping> cache = null;
            if (useGlobalCache) {
                cache = getPathCache();
            }
            
            // NOTE: We assume that defaultSourceServletPath and defaultSourceFromContextRoot are static
            // for a server execution, so we don't have to make them a key in the lookup. However logically they would be part of it.
            String key = delegator.getDelegatorName() + "::" + webSiteId + "::" + path;
            if (CmsUtil.verboseOn()) {
                Debug.logInfo("Cms: Finding process mapping: " + key + CmsControlUtil.getReqLogIdDelimStr(request), module);
            }
            
            CmsProcessMapping mapping = null;
            CacheEntry<CmsProcessMapping> mappingEntry = null;
            if (useGlobalCache) {
                mappingEntry = cache.getEntry(key);
            }
            
            if (mappingEntry == null) {
                if (CmsUtil.verboseOn()) {
                    Debug.logInfo("Cms: Retrieving process mapping from database: " + key + CmsControlUtil.getReqLogIdDelimStr(request), module);
                }
                
                // Note: This lookup has to match sourceMatchesRequestPath implementation!
                List<EntityCondition> condList = new ArrayList<>();
                condList.add(EntityCondition.makeCondition("sourceWebSiteId", webSiteId));
                
                EntityCondition contextSourcePathCond = EntityCondition.makeCondition(
                        makeSourceMatchesRequestPathCond("sourcePath", path),
                        EntityOperator.AND,
                        makeSourceFromContextRootCondition(defaultSourceFromContextRoot));
                
                // 2013-08-13: We have a new case here: source matches can be done with default source servlet implied
                // so user not forced to write it for every mapping.
                if (sourceMatchesRequestPath(defaultSourceServletPath, path)) {
                    
                    // Strip the servlet path from path
                    String pathFromServlet = path.substring(defaultSourceServletPath.length());
                    pathFromServlet = PathUtil.ensureStartDelim(pathFromServlet);
                    
                    EntityCondition servletSourcePathCond = EntityCondition.makeCondition(
                            makeSourceMatchesRequestPathCond("sourcePath", pathFromServlet),
                            EntityOperator.AND,
                            makeNotSourceFromContextRootCondition(defaultSourceFromContextRoot));
                    
                    condList.add(EntityCondition.makeCondition(contextSourcePathCond,
                            EntityOperator.OR,
                            servletSourcePathCond));
                } else {
                    condList.add(contextSourcePathCond);
                }

                List<CmsProcessMapping> mappings = CmsProcessMapping.getWorker().findAll(delegator, 
                        EntityCondition.makeCondition(condList, EntityOperator.AND), null,
                        isUseDbCacheStatic(useCache));
                
                if (UtilValidate.isNotEmpty(mappings)) {
                    // Use the mapping which is the most specific, i.e. the longest
                    // Note: I couldn't find a good universal way to specify orderBy string length in entity-based query, 
                    // so do it in code (logging useful anyway).
                    
                    Iterator<CmsProcessMapping> mappingIt = mappings.iterator();
                    mapping = mappingIt.next();
                    
                    String longestSourcePath = mapping.getSourcePathExpanded(defaultSourceServletPath, defaultSourceFromContextRoot);
                    boolean longestIsSourceFromContextRoot = mapping.isSourceFromContextRootLogical(defaultSourceFromContextRoot);
                    
                    while(mappingIt.hasNext()) {
                        CmsProcessMapping otherMapping = mappingIt.next();
                        
                        String otherSourcePath = otherMapping.getSourcePathExpanded(defaultSourceServletPath, defaultSourceFromContextRoot);
                        boolean otherIsSourceFromContextRoot = otherMapping.isSourceFromContextRootLogical(defaultSourceFromContextRoot);
                        
                        // Note: Slight priority to context root paths
                        if ((otherSourcePath.length() > longestSourcePath.length()) ||
                            (otherSourcePath.length() == longestSourcePath.length() && 
                             !longestIsSourceFromContextRoot && otherIsSourceFromContextRoot)) {
                            longestSourcePath = otherSourcePath;
                            longestIsSourceFromContextRoot = otherIsSourceFromContextRoot;
                            mapping = otherMapping;
                        }
                    }
                    
                    if (CmsUtil.verboseOn()) {
                        if (mappings.size() == 1) {
                            Debug.logInfo("Cms: Found single process mapping matching request: " + 
                                    mapping.getLogIdRepr() + CmsControlUtil.getReqLogIdDelimStr(request), module);
                        } else {
                            Debug.logInfo("Cms: Found " + mappings.size() + " process mappings matching request; using most specific: " + 
                                    mapping.getLogIdRepr() + CmsControlUtil.getReqLogIdDelimStr(request), module);
                        }
                    }
                }
                
                if (useGlobalCache) {
                    cache.put(key, mapping);
                }
            } else {
                if (mappingEntry.hasValue()) {
                    if (CmsUtil.verboseOn()) {
                        Debug.logVerbose("Cms: Retrieving mapping from cache: " + key, module);
                    }
                    mapping = mappingEntry.getValue();
                }
            }

            return new FindByRequestResult(mapping, path, 
                    mapping != null ? getExtraPathInfo(path, mapping.getSourcePath()) : null);
        }
        
        @Override
        public void clearMemoryCaches() {
            //CmsProcessViewMapping.clearCaches();
            getPathCache().clear();
        }
        
        /**
         * WARN: the caller must make sure to always pass the same controlServletPath, defaultSourceServletPath, defaultSourceFromContextRoot
         * values every time (CmsProcessFilter must cache these and never change) - this is optimization only.
         */
        public Set<String> getRequestUrisUnderControl(Delegator delegator, String webSiteId, 
                String controlServletPath, String defaultSourceServletPath, boolean defaultSourceFromContextRoot, boolean useCache) {
            String key = delegator.getDelegatorName() + "::" + webSiteId;
            
            Set<String> uris = null;
            if (useCache && rootControlUriCache != null) {
                uris = rootControlUriCache.get(key);
            }
            if (uris == null) {
                uris = new HashSet<>();
                for(CmsProcessMapping mapping : getMappingsUnderControl(delegator, webSiteId, controlServletPath, defaultSourceServletPath, defaultSourceFromContextRoot, useCache)) {
                    String sourceFromContextRoot = mapping.getSourceFromContextRootStr();
                    String sourcePath = mapping.getSourcePath();
                    
                    if ("Y".equals(sourceFromContextRoot) || (defaultSourceFromContextRoot && "D".equals(sourceFromContextRoot))) {
                        // sanity check
                        if (sourcePath.startsWith(controlServletPath + "/")) {
                            sourcePath = sourcePath.substring(controlServletPath.length());
                            uris.add(RequestLinkUtil.getFirstPathElem(sourcePath));
                            
                        } else {
                            Debug.logError("Cms: CmsProcessMapping.sourcePath was expected to start with '" + controlServletPath + "/' but does not: " + sourcePath, module);
                        }
                    } else { // assumes (N or (!defaultSourceFromContextRoot && D)) and path starts with control
                        if (!sourcePath.startsWith("/")) sourcePath = "/" + sourcePath;
                        uris.add(RequestLinkUtil.getFirstPathElem(sourcePath));
                    }
                }
                
                if (useCache && rootControlUriCache != null) {
                    rootControlUriCache.put(key, uris);
                }
            }
            return uris;
        }
        
        /**
         * Returns all process mapping source paths that are mapped (one way or another) to fall
         * under the /control path.
         * ONLY used if the ControlServlet is not mapped to root.
         * Highly depends on the sourceFromContextRoot flag.
         * Added 2017-11-15.
         */
        public List<CmsProcessMapping> getMappingsUnderControl(Delegator delegator, String webSiteId, 
                String controlServletPath, String defaultSourceServletPath, boolean defaultSourceFromContextRoot, boolean useCache) {
            if (controlServletPath == null || controlServletPath.length() < 1) return Collections.emptyList();
            
            EntityCondition cond = EntityCondition.makeCondition(EntityCondition.makeCondition("sourceWebSiteId", webSiteId),
                    EntityOperator.AND, 
                    makeSourcePathStartsWithControlCond(controlServletPath, defaultSourceServletPath, defaultSourceFromContextRoot));
            
            return CmsProcessMapping.getWorker().findAll(delegator, cond, null, isUseDbCacheStatic(useCache));
        }
        
        /**
         * Matches all CmsProcessMappings that fall under the control servlet path.
         */
        public EntityCondition makeSourcePathStartsWithControlCond(String controlServletPath, String defaultSourceServletPath, boolean defaultSourceFromContextRoot) {
            EntityCondition rootCond;
            if (defaultSourceFromContextRoot) {
                rootCond = EntityCondition.makeCondition(EntityCondition.makeCondition("sourceFromContextRoot", "Y"),
                        EntityOperator.OR,
                        EntityCondition.makeCondition("sourceFromContextRoot", "D"));
            } else {
                rootCond = EntityCondition.makeCondition("sourceFromContextRoot", "Y");
            }
            rootCond = EntityCondition.makeCondition(rootCond,
                    EntityOperator.AND,
                    EntityCondition.makeCondition("sourcePath", EntityOperator.LIKE, controlServletPath + "/%"));
            
            boolean defaultPathIsControl = (controlServletPath.equals(defaultSourceServletPath) || (defaultSourceServletPath.startsWith(controlServletPath + "/")));
            if (defaultPathIsControl) {
                EntityCondition defaultCond;
                if (defaultSourceFromContextRoot) {
                    defaultCond = EntityCondition.makeCondition("sourceFromContextRoot", "N");
                } else {
                    defaultCond = EntityCondition.makeCondition(EntityCondition.makeCondition("sourceFromContextRoot", "N"),
                            EntityOperator.OR,
                            EntityCondition.makeCondition("sourceFromContextRoot", "D"));
                }
                return EntityCondition.makeCondition(defaultCond, EntityOperator.OR, rootCond);
            } else {
                return rootCond;
            }
        }

    }
    
    @Override
    public void acceptEntityDepsVisitor(CmsEntityVisitor visitor, GenericValue relValue, VisitRelation relValueRelation, CmsMajorObject majorDataObj) throws Exception {
        // SPECIAL: if we're coming from CmsPage, then don't link back to any pages, because we'll be going in endless loops
        // and we're not really interested in any other pages when we're doing that
        VisitRelations relations;
        if (relValueRelation != null && ("CmsPage".equals(relValueRelation.getEntityName()) || "CmsPageSpecialMapping".equals(relValueRelation.getEntityName()))) {
            relations = VisitRelNoPagePlan.visitRelations;
        } else {
            relations = VisitRelPlan.visitRelations;
        }
        CmsEntityVisit.acceptRelatedEntityDepsVisitor(visitor, relations, this.getEntity(), relValueRelation, relValue, this);
    }
    
    public static class VisitRelPlan extends VisitRelations.BuildPlan {
        public static final VisitRelPlan INSTANCE = new VisitRelPlan("CmsProcessMapping");
        static final VisitRelations visitRelations = INSTANCE.buildSafe();
        public VisitRelPlan(String majorEntityName) { super(majorEntityName); }
        @Override public VisitRelations.Builder planDefinition(Delegator delegator) throws Exception {
            return newBuilder(delegator)
                    .entity("CmsProcessMapping")
                        .self()
                        .relation("CmsProcessViewMapping")
                        .relationMajor("CmsPage")
                        .relationMajor("PrimaryCmsPage")
                    .entity("CmsProcessViewMapping")    
                        .self()
                        .relationMajor("CmsPage");
        }
    }
    
    public static class VisitRelNoPagePlan extends VisitRelations.BuildPlan {
        public static final VisitRelPlan INSTANCE = new VisitRelPlan("CmsProcessMapping");
        static final VisitRelations visitRelations = INSTANCE.buildSafe();
        public VisitRelNoPagePlan(String majorEntityName) { super(majorEntityName); }
        @Override public VisitRelations.Builder planDefinition(Delegator delegator) throws Exception {
            return VisitRelPlan.INSTANCE.planDefinition(delegator)
                .removeAllRelationsOfRelEntityName("CmsPage");
        }
    }
}
