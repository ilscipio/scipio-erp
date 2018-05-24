package com.ilscipio.scipio.cms.control;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Locale;
import java.util.Map;

import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.UtilMisc;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.GenericValue;

import com.ilscipio.scipio.ce.util.Optional;
import com.ilscipio.scipio.cms.CmsException;
import com.ilscipio.scipio.cms.content.CmsPage;

/**
 * Wraps and represents a CmsProcessViewMapping entity value.
 */
public class CmsProcessViewMapping extends CmsControlDataObject {

    private static final long serialVersionUID = -7866463524435797534L;

    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());
    
    // No longer needed; moved caching to CmsProcessMapping
    //private static final CmsObjectCache<CmsProcessViewMapping> pathAndNameCache = getNewMappingCache();
    
    // NOTE: 2016: Optional is needed (?) for thread safety (preload)
    private Optional<CmsProcessMapping> parentProcessMapping = null;   
    private Optional<CmsPage> page = null;
    
    protected CmsProcessViewMapping(GenericValue entity) {
        super(entity);
    }

    protected CmsProcessViewMapping(Delegator delegator, Map<String, ?> fields, CmsProcessMapping parentProcessMapping) {
        super(delegator, fields);
        if (parentProcessMapping != null) {
            setParentProcessMapping(parentProcessMapping);
        }
    }
    
    protected CmsProcessViewMapping(CmsProcessViewMapping other, Map<String, Object> copyArgs) {
        super(other, copyArgs);
    }
    
    @Override    
    public void update(Map<String, ?> fields, boolean setIfEmpty) {
        super.update(fields, setIfEmpty);
    }
    
    @Override
    public CmsProcessViewMapping copy(Map<String, Object> copyArgs) throws CmsException {
        return new CmsProcessViewMapping(this, copyArgs);
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
        preloadWorker.preload(this.getPage(CmsProcessMapping.USE_LIVE_PRIMARY_PAGE_ID_CACHE));
        // NOTE: don't do parent process mapping
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
        
        rowsAffected += super.remove();
        
        if (removeRelatedOrphaned) {
            if (page != null) {
                rowsAffected += page.removeIfOrphan();
            }
        }
        
        return rowsAffected;
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
    
    public String getTargetServletPath() {
        return entity.getString("targetServletPath");
    }
 
    public Boolean getMatchAnyTargetPath() {
        return entity.getBoolean("matchAnyTargetPath");
    }
    
    /*
    public static EntityCondition makeMatchAnyTargetPathCond() {
        return EntityCondition.makeCondition("matchAnyTargetPath", "Y");
    }
    
    public static EntityCondition makeNotMatchAnyTargetPathCond() {
        return EntityCondition.makeCondition(
                EntityCondition.makeCondition("matchAnyTargetPath", null),
                EntityOperator.OR,
                EntityCondition.makeCondition("matchAnyTargetPath", "N")
                );
    }
    */
    
    public String getTargetPath() {
        return entity.getString("targetPath"); 
    }

    public String getTargetViewName() {
        return entity.getString("targetViewName"); 
    }    
    
    public String getPageId() {
        return entity.getString("pageId"); 
    }
    
    public String getParentProcessMappingId() {
        return entity.getString("processMappingId");
    }

    public CmsProcessMapping getParentProcessMapping() throws CmsException {
        Optional<CmsProcessMapping> parentProcessMapping = this.parentProcessMapping;
        if (parentProcessMapping == null) {
            String parentProcMappingId = getParentProcessMappingId();
            if (UtilValidate.isNotEmpty(parentProcMappingId)) {
                parentProcessMapping = Optional.ofNullable(CmsProcessMapping.getWorker().findOne(getDelegator(),
                        UtilMisc.toMap("processMappingId", parentProcMappingId), 
                        false));
            } else {
                Debug.logError("Cms: CmsProcessViewMapping '" + getId() + "' is missing reference to parent CmsProcessMapping", module);
                parentProcessMapping = Optional.empty();
            }
            this.parentProcessMapping = parentProcessMapping;
        }
        return parentProcessMapping.orElse(null);
    }
    
    public CmsPage getPage() throws CmsException {
        return getPage(false);
    } 
    
    public CmsPage getPage(boolean useCache) throws CmsException {
        Optional<CmsPage> page = this.page;
        if (page == null) {
            String pageId = getPageId();
            if (UtilValidate.isNotEmpty(pageId)) {
                // here, use the explicit pageId
                // OPTIMIZATION: check if parent already has a reference to our page (avoid double instances)
                CmsProcessMapping parent = getParentProcessMapping();
                if (parent != null) {
                    page = Optional.ofNullable(parent.findPageById(pageId, useCache));
                } else {
                    // NOTE: this case should not happen...
                    page = Optional.ofNullable(CmsPage.getWorker().findById(getDelegator(), pageId, useCache));
                }
            } else {
                // here, we have no explicit pageId and we must check the default pageId on parent
                CmsProcessMapping parent = getParentProcessMapping();
                if (parent != null) {
                    page = Optional.ofNullable(parent.getPage(useCache));
                } else {
                    page = Optional.empty();
                }
            }
            this.page = page;
        }
        return page.orElse(null);
    }
    
    public String getLogIdRepr() {
        return "[view mapping ID: " + getId() + "; target view name: " + getTargetViewName() + "]";
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
    
    
    public void setTargetViewName(String targetViewName) {
        entity.setString("targetViewName", targetViewName); 
    }  
    
    public void setPageId(String pageId) {
        entity.setString("pageId", pageId); 
        this.page = null;
    }  
    
    public void setParentProcessMappingId(String processMappingId) {
        entity.setString("processMappingId", processMappingId);
        this.parentProcessMapping = null;
    }
    
    protected void setParentProcessMappingRefOnly(CmsProcessMapping processMapping) {
        this.parentProcessMapping = Optional.ofNullable(processMapping);
    }
    
    public void setParentProcessMapping(CmsProcessMapping processMapping) {
        setParentProcessMappingId(processMapping != null ? processMapping.getId() : null);
        setParentProcessMappingRefOnly(processMapping);
    }

    // Moved to CmsProcessMapping and out of date
//    /**
//     * Finds process view mapping for given mapping, servlet and request. Assumes paths normalized.
//     * @throws CmsException 
//     */
//    static CmsProcessViewMapping findByProcessAndView(Delegator delegator, CmsProcessMapping processMapping, String requestServletPath,
//            String requestPath, String viewName, String defaultTargetServletPath, boolean defaultMatchAnyTargetPath, boolean useCache) throws CmsException {
//        
//        CmsObjectCache<CmsProcessViewMapping> cache = getPathAndNameCache();
//        String key = delegator.getDelegatorName() + "::" + processMapping.getId() + "::" + requestPath + "::" + viewName;
//        
//        Debug.logInfo("Cms: Finding process view mapping: " + key, module);
//        CmsProcessViewMapping viewMapping = cache.get(key);
//        
//        if (viewMapping == null) {
//            Debug.logInfo("Cms: Retrieving process view mapping from database: " + key, module);
//            
//            List<EntityCondition> condList;
//            
//            // Find all mappings with the specific view name (have to implement path filter check manually)
//            // Note: It is valid that there may be more than one, for different targetPaths
//            condList = new ArrayList<>();
//            condList.add(EntityCondition.makeCondition("processMappingId", processMapping.getId()));
//            condList.add(EntityCondition.makeCondition("targetViewName", viewName));
//            List<CmsProcessViewMapping> viewMappings = CmsDataObject.<CmsProcessViewMapping> findAll(delegator, 
//                    EntityCondition.makeCondition(condList, EntityOperator.AND), 
//                    CmsProcessViewMapping.class, null, useProcessViewDbCache);
//            
//            viewMapping = findBestProcessViewMatch(processMapping, viewMappings, requestServletPath, requestPath, 
//                    viewName, defaultTargetServletPath, defaultMatchAnyTargetPath);
//            
//            if (viewMapping != null) {
//                // Important: Must set this before setting in cache!
//                viewMapping.setParentProcessMappingShallow(processMapping);
//            }
//            cache.put(key, viewMapping);
//        } else {
//            Debug.logVerbose("Cms: Retrieving process view mapping from cache: " + key, module);
//        }
//
//        return viewMapping;
//    }
    
    
    public static CmsProcessViewMapping findBestProcessViewMatch(CmsProcessMapping processMapping,
            List<CmsProcessViewMapping> viewMappings, 
            String requestServletPath, String requestPath, String viewName, 
            String defaultTargetServletPath, boolean defaultMatchAnyTargetPath) throws CmsException {

        CmsProcessViewMapping viewMapping = null;

        if (UtilValidate.isNotEmpty(viewMappings)) {
            
            String parentTargetPath = processMapping.getTargetPath();
            
            final boolean processMappingMatchesReq = UtilValidate.isNotEmpty(parentTargetPath) &&
                    CmsProcessMapping.targetMatchesRequestPath(
                            CmsControlUtil.normalizeServletRootRequestPathNoNull(processMapping.getTargetPath()), 
                            requestPath);
            
            String parentOrDefTargetServletPath = nonEmptyOrDefault(processMapping.getTargetServletPath(), defaultTargetServletPath);
            Boolean parentOrDefMatchAnyTargetPath = nonEmptyOrDefault(processMapping.getMatchAnyTargetPath(), defaultMatchAnyTargetPath);
            
 
            // Triage by targetPath and matchAnyTargetPath
            // Try to give priority to more specific mappings.
            List<CmsProcessViewMapping> explPathMappings = new ArrayList<>();
            List<CmsProcessViewMapping> parentPathMappings = new ArrayList<>();
            List<CmsProcessViewMapping> matchAnyMappings = new ArrayList<>();
            
            for(CmsProcessViewMapping currMapping : viewMappings) {
                
                if (!viewName.equals(currMapping.getTargetViewName())) {
                    continue;
                }
                
                // First, must filter out those for which servlet path doesn't match
                
                // FIXME?: Here I didn't give priority to mappings with explicit target servlet path
                // (only for target path). I'm not sure it really matters.
                
                String explTargetServletPath = nonEmptyOrDefault(currMapping.getTargetServletPath(), parentOrDefTargetServletPath); 
                if (requestServletPath.equals(explTargetServletPath)) {        
                        
                    // Filter results by target path matching
                    if (nonEmptyOrDefault(currMapping.getMatchAnyTargetPath(), defaultMatchAnyTargetPath)) {
                        matchAnyMappings.add(currMapping);
                    } else {
                        String explTargetPath = currMapping.getTargetPath();
                        if (UtilValidate.isNotEmpty(explTargetPath)) { // Explicit target
                            if (CmsProcessMapping.targetMatchesRequestPath(explTargetPath, requestPath)) {
                                explPathMappings.add(currMapping);
                            }
                        } else {
                            if (parentOrDefMatchAnyTargetPath) {
                                // Note: Explicit target above overrides matchAnyTargetPath set in parent,
                                // but matchAnyTargetPath in child overrides all
                                matchAnyMappings.add(currMapping); 
                            } else if (processMappingMatchesReq) { // Default parent target
                                parentPathMappings.add(currMapping);
                            }
                        }
                    }
                }
            }
            
            if (explPathMappings.size() > 1 || parentPathMappings.size() > 1 || matchAnyMappings.size() > 1) {
                Debug.logWarning("Cms: There are multiple process view mappings of the same type corresponding" +
                        "to " + (processMapping.getId() + "::" + requestPath + "::" + viewName) + "; using first found only for given type", module);
            }
            
            // Return the first mapping found, most specific type first w.r.t. targetPath
            if (explPathMappings.size() > 0) {
                viewMapping = explPathMappings.get(0);
            } else if (parentPathMappings.size() > 0) {
                viewMapping = parentPathMappings.get(0);
            } else if (matchAnyMappings.size() > 0) {
                viewMapping = matchAnyMappings.get(0);
            }
        }

        return viewMapping;
    }
    
    @Override
    public Map<String, Object> getDescriptor(Locale locale) {
        Map<String, Object> map = super.getDescriptor(locale);
        map.putAll(UtilMisc.toMap(
                "id", getId(),
                "targetPath", getTargetPath(),                
                "targetViewName", getTargetViewName(),
                "pageId", getPageId(),
                "processMappingId", getParentProcessMappingId()
        ));
        return map;
    }  
    
//    private static CmsObjectCache<CmsProcessViewMapping> getPathAndNameCache() {
//        return pathAndNameCache;
//    }
    
    // 2016: TODO: REVIEW: useful for UI?
//    /**
//     * Best-effort attempt to check if a process view mapping will apply to a given controller
//     * request URI. Independent of view-name.
//     * <p>
//     * Logical should follow from findBestProcessViewMatch.
//     * <p>
//     * WARNING: Currently does not check targetServletPath or controller mount-point.
//     */
//    public static boolean appliesToControllerRequest(String reqUri, CmsProcessMappingInfo pmInfo,
//            CmsProcessViewMappingInfo pvmInfo) {
//        
//        // Bit of a kludge
//        final boolean defaultMatchAnyTargetPath = CmsProcessMapping.defaultMatchAnyTargetPath;
//        
//        if (nonEmptyOrDefault(pvmInfo.getMatchAnyTargetPath(), defaultMatchAnyTargetPath)) {
//            return true;
//        }
//        
//        String specTargetPath = pvmInfo.getTargetPath();
//        if (specTargetPath != null && specTargetPath.length() > 0) {
//            return CmsProcessMapping.targetMatchesControllerRequestUri(specTargetPath, reqUri);
//        }
//        
//        Boolean parentOrDefMatchAnyTargetPath = nonEmptyOrDefault(pmInfo.getMatchAnyTargetPath(), defaultMatchAnyTargetPath);
//        if (parentOrDefMatchAnyTargetPath) {
//            return true;
//        }
//        
//        String defTargetPath = pmInfo.getTargetPath();
//        if (defTargetPath != null && defTargetPath.length() > 0) {
//            return CmsProcessMapping.targetMatchesControllerRequestUri(defTargetPath, reqUri);
//        }
//        
//        return false;
//    }
    

    
    @Override
    public ProcessViewMappingWorker getWorkerInst() {
        return ProcessViewMappingWorker.worker;
    }
    
    public static ProcessViewMappingWorker getWorker() {
        return ProcessViewMappingWorker.worker;
    }
    
    public static class ProcessViewMappingWorker extends ControlDataObjectWorker<CmsProcessViewMapping> {
        
        private static final ProcessViewMappingWorker worker = new ProcessViewMappingWorker();
        
        // FIXME: These could potentially also include other fields and much more complicated check for
        // logical PK fields... but hard to communicate in CMS apps and it depends on various things. 
        // For example matchAnyTargetPath and the defaults inherited from the parent process mapping could play into this.
        // The most basic logic PK is {"processMappingId", "targetViewName"}, but to allow some flexibility
        // in UI I'm adding targetPath as a quick fix for now. But this is a very dumb check and still restricts schema usage.
        private static final List<String> logicalPkFieldNames = Collections.unmodifiableList(Arrays.asList(new String[] { 
                "processMappingId", "targetViewName", "targetPath" 
        }));
        private static final List<String> logicalPkFieldsAllowedEmptyNames = Collections.unmodifiableList(Arrays.asList(new String[] { 
                "targetPath" 
        }));
        
        protected ProcessViewMappingWorker() {
            super(CmsProcessViewMapping.class);
        }

        @Override
        public List<String> getLogicalPkFieldNames(Delegator delegator) {
            return logicalPkFieldNames;
        }

        @Override
        public List<String> getLogicalPkFieldsAllowedEmptyNames(Delegator delegator) {
            return logicalPkFieldsAllowedEmptyNames;
        }

        @Override
        public Class<CmsProcessViewMapping> getDataObjectClass() {
            return CmsProcessViewMapping.class;
        }

        @Override
        public String getControlPageIdFieldName() {
            return "pageId";
        }
        
        @Override
        public CmsProcessViewMapping makeFromValue(GenericValue value) throws CmsException {
            return new CmsProcessViewMapping(value);
        }

        @Override
        public CmsProcessViewMapping makeFromFields(Delegator delegator, Map<String, ?> fields) throws CmsException {
            return new CmsProcessViewMapping(delegator, fields, null);
        }
        
        public List<CmsProcessViewMapping> findByProcess(Delegator delegator, String processMappingId, boolean useCache) throws CmsException {
            return findAll(delegator, UtilMisc.toMap("processMappingId", processMappingId), 
                    UtilMisc.toList("targetPath ASC", "targetViewName ASC"), isUseDbCacheStatic(useCache));
        }
        
        @Override
        public List<CmsProcessViewMapping> findByWebSiteId(Delegator delegator, String webSiteId, boolean useCache)
                throws CmsException {
            // TODO
            throw new UnsupportedOperationException();
        }

        @Override
        public void clearMemoryCaches() {
            // None for now
            //getPathAndNameCache().clear();
        }

    }
}
