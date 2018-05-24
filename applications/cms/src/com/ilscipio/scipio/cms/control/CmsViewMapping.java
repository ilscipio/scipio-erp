package com.ilscipio.scipio.cms.control;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Map;

import javax.servlet.http.HttpServletRequest;

import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.UtilGenerics;
import org.ofbiz.base.util.UtilMisc;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.GenericValue;
import org.ofbiz.entity.condition.EntityCondition;
import org.ofbiz.entity.condition.EntityOperator;

import com.ilscipio.scipio.ce.util.Optional;
import com.ilscipio.scipio.cms.CmsException;
import com.ilscipio.scipio.cms.CmsUtil;
import com.ilscipio.scipio.cms.content.CmsPage;
import com.ilscipio.scipio.cms.data.CmsDataException;
import com.ilscipio.scipio.cms.data.CmsEntityVisit;
import com.ilscipio.scipio.cms.data.CmsEntityVisit.CmsEntityVisitor;
import com.ilscipio.scipio.cms.data.CmsEntityVisit.VisitRelation;
import com.ilscipio.scipio.cms.data.CmsEntityVisit.VisitRelations;
import com.ilscipio.scipio.cms.data.CmsMajorObject;
import com.ilscipio.scipio.cms.data.CmsObjectCache;
import com.ilscipio.scipio.cms.data.CmsObjectCache.CacheEntry;

/**
 * Wraps and represents a CmsViewMapping entity value.
 */
public class CmsViewMapping extends CmsControlDataObject implements CmsMajorObject  {

    private static final long serialVersionUID = -5208044951376304256L;

    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());
    
    public static final String TARGET_SERVLET_PATH_DEFAULT = "DEFAULT";
    
    private static final CmsObjectCache<CmsViewMapping> nameCache = CmsObjectCache.getGlobalCache("cms.control.viewMapping.name");
    
    protected static final String ACTIVE_INITIAL_VALUE = "Y";
    
    private Optional<CmsPage> page = null; // NOTE: 2016: Optional is required for thread safety (preload)

    protected CmsViewMapping(GenericValue entity) {
        super(entity);
    }
    
    public CmsViewMapping(Delegator delegator, Map<String, ?> fields) {
        super(delegator, checkFields(fields, true));
    }
    
    protected CmsViewMapping(CmsViewMapping other, Map<String, Object> copyArgs) {
        super(other, copyArgs);
    }
    
    @Override    
    public void update(Map<String, ?> fields, boolean setIfEmpty) {
        super.update(checkFields(fields, false), setIfEmpty);
    }
    
    @Override
    public CmsViewMapping copy(Map<String, Object> copyArgs) throws CmsException {
        return new CmsViewMapping(this, copyArgs);
    }   

    protected static <T> Map<String, T> checkFields(Map<String, T> fields, boolean isNew) {
        if (isNew || fields.containsKey("targetServletPath")) {
            ensureTargetServletPath(fields);
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
        // NOTE: not using page cache here, for consistency with CmsProcess(View)Mapping - see CmsProcessMapping.preloadContent
        preloadWorker.preload(this.getPage(CmsProcessMapping.USE_LIVE_PRIMARY_PAGE_ID_CACHE)); 
    }
    
    @Override
    public int remove() throws CmsException {
        return remove(true);
    }
    
    /**
     * Removes this view mapping. If requested, currently also removes the related page,
     * but only if it has been orphaned.
     */
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
    
    @Override
    public void store() throws CmsException {
        // 2016: this field cannot be null anymore
        if (UtilValidate.isEmpty(getTargetServletPath())) {
            String id = getId();
            String descStr;
            if (UtilValidate.isNotEmpty(id)) {
                descStr = "update CmsViewMapping '" + id + "'";
            } else {
                descStr = "create CmsViewMapping";
            }
            throw new CmsDataException("Trying to " + descStr + " but"
                    + " targetServletPath field is empty - required (for default, use special value " + TARGET_SERVLET_PATH_DEFAULT + ")");
        }
        super.store();
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
    
    public String getWebSiteId() {
        return entity.getString("webSiteId");
    }
    
    public String getTargetServletPath() {
        return entity.getString("targetServletPath");
    }
    
    public String getTargetViewName() {
        return entity.getString("targetViewName");
    }
    
    public String getPageId() {
        return entity.getString("pageId");
    }
    
    // 2016: REMOVED
//    /**
//     * Gets the optional target path filter.
//     */
//    public String getTargetPath() {
//        return entity.getString("targetPath"); 
//    }
    
    public String getLogIdRepr() {
        return makeLogIdRepr(getId(), getTargetViewName(), getWebSiteId());
    }
    
    static String makeLogIdRepr(String targetViewName, String webSiteId) {
        return "[target view name: " + targetViewName + ", web site: " + webSiteId + "]";
    }
    
    static String makeLogIdRepr(String id, String targetViewName, String webSiteId) {
        return "[view mapping: " + id + ", target view name: " + targetViewName + ", web site: " + webSiteId + "]";
    }
    
    public void setWebSiteId(String webSiteId) {
        entity.setString("webSiteId", webSiteId);
    }
    
    public void setTargetServletPath(String targetServletPath) {
        entity.setString("targetServletPath", UtilValidate.isNotEmpty(targetServletPath) ? targetServletPath : TARGET_SERVLET_PATH_DEFAULT);
    }
    
    public static void ensureTargetServletPath(Map<String, ?> fields) {
        if (UtilValidate.isEmpty((String) fields.get("targetServletPath"))) {
            UtilGenerics.<String, Object> checkMap(fields).put("targetServletPath", TARGET_SERVLET_PATH_DEFAULT);
        }
    }
    
    public void setTargetViewName(String targetViewName) {
        entity.setString("targetViewName", targetViewName);
    }
    
    public void setPageId(String pageId) {
        entity.setString("pageId", pageId);
        this.page = null;
    }
    
    // 2016: REMOVED
//    public void setTargetPath(String targetPath) {
//        entity.setString("targetPath", targetPath);
//    }
    
    public CmsPage getPage() throws CmsException {
        return getPage(false);
    } 
    
    public CmsPage getPage(boolean useCache) throws CmsException {
        Optional<CmsPage> page = this.page;
        if (page == null) {
            String pageId = getPageId();
            if (UtilValidate.isNotEmpty(pageId)) {
                page = Optional.ofNullable(CmsPage.getWorker().findById(getDelegator(), pageId, useCache));
            } else {
                page = Optional.empty();
            }
            this.page = page;
        }
        return page.orElse(null);
    }
    
    private static CmsObjectCache<CmsViewMapping> getNameCache() {
        return nameCache;
    }     
    
    /**
     * Finds view for for website, (control) servlet and view name. Assumes paths normalized.
     * <p>
     * NOTE: 2016: we do not use requestPath here anymore; see entitymodel. 
     * 
     * @param request OPTIONAL request, used for logging
     */
    public static CmsViewMapping findByView(Delegator delegator, String webSiteId, String requestServletPath, 
            String viewName, String defaultTargetServletPath, boolean useCache, HttpServletRequest request) throws CmsException {
        
        boolean useGlobalCache = isUseGlobalObjCacheStatic(useCache);
        CmsObjectCache<CmsViewMapping> cache = null;
        if (useGlobalCache) {
            cache = getNameCache();
        }
        
        // NOTE: the key should never need to contain defaultTargetServletPath because it is
        // fixed per webSiteId for a given server execution (web.xml param)
        String key = delegator.getDelegatorName() + "::" + webSiteId + "::" + viewName + "::" + requestServletPath;
        
        CmsViewMapping viewMapping = null;
        CacheEntry<CmsViewMapping> viewMappingEntry = null;
        
        if (useGlobalCache) {
            viewMappingEntry = cache.getEntry(key);
        }
        
        if (viewMappingEntry == null) {
            if (CmsUtil.verboseOn()) {
                Debug.logInfo("Cms: Retrieving view mapping from database: " + makeLogIdRepr(viewName, webSiteId), module);
            }
            
            // Get everything matching webSiteId and viewName and then filter path matching manually

            List<EntityCondition> condList = new ArrayList<>();
            condList.add(EntityCondition.makeCondition("webSiteId", webSiteId));
            if (requestServletPath.equals(defaultTargetServletPath)) {
                // Default matched, so include DEFAULT
                condList.add(EntityCondition.makeCondition(
                        EntityCondition.makeCondition("targetServletPath", requestServletPath),
                        EntityOperator.OR,
                        EntityCondition.makeCondition("targetServletPath", TARGET_SERVLET_PATH_DEFAULT)
                        ));
            } else {
                // Default didn't match; must match exact
                condList.add(EntityCondition.makeCondition("targetServletPath", requestServletPath));
            }
            condList.add(EntityCondition.makeCondition("targetViewName", viewName));

            List<CmsViewMapping> viewMappings = CmsViewMapping.getWorker().findAll(delegator,
                    EntityCondition.makeCondition(condList, EntityOperator.AND),
                    null, isUseDbCacheStatic(useCache));

            // Iterate view mapping trying to get the most specialized matching using precedence.
            // 2016: this is very simple because we now only have to check precedence for 
            // targetServletPath vs defaultTargetServletPath and we're good.
            // In addition, there is a DB UNIQUE index so duplicates will never happen
            if (viewMappings.size() == 1) {
                viewMapping = viewMappings.get(0);
            } else if (viewMappings.size() > 1) {
                if (viewMappings.size() > 2) {
                    // we should have at most one matching targetServletPath and one
                    // with targetServletPath DEFAULT, so > 2 case
                    // should be prevented by the unique index... as well as the UI...
                    Debug.logWarning("Cms: Unexpected number (" + viewMappings.size() 
                            + ") of view mappings returned for [webSiteId: " + webSiteId
                            + ", targetServletPath: " + requestServletPath + ", targetViewName: " + viewName + "];"
                            + " there may be a database integrity problem" + CmsControlUtil.getReqLogIdDelimStr(request), module);
                }
                // return the first mapping with non-DEFAULT targetServletPath.
                for(CmsViewMapping mapping : viewMappings) {
                    if (!TARGET_SERVLET_PATH_DEFAULT.equals(mapping.getTargetServletPath())) {
                        viewMapping = mapping;
                        break;
                    }
                }
                if (viewMapping == null) {
                    viewMapping = viewMappings.get(0);
                }
            }
            
            if (CmsUtil.verboseOn()) {
                if (viewMapping != null) {
                    Debug.logInfo("Cms: Found view mapping: " + viewMapping.getLogIdRepr() + CmsControlUtil.getReqLogIdDelimStr(request), module);
                } else {
                    Debug.logInfo("Cms: No view mapping found" + CmsControlUtil.getReqLogIdDelimStr(request), module);
                }
            }
            
            // 2016: we have re-removed the requestPath/targetPath matching because it's useless here
//            // Iterate view mapping trying to get the most specialized matching using precedence:
//            // 1) matches targetPath + targetServletPath
//            // 2) matches targetPath + defaultTargetServletPath
//            // 3) matches any path + targetServletPath
//            // 4) matches any path + defaultTargetServletPath
//
//            List<CmsViewMapping> explBothMappings = new ArrayList<>();
//            List<CmsViewMapping> explPathMappings = new ArrayList<>();
//            List<CmsViewMapping> explServMappings = new ArrayList<>();
//            List<CmsViewMapping> implBothMappings = new ArrayList<>();
//
//            if (UtilValidate.isNotEmpty(viewMappings)) {
//                for(CmsViewMapping mapping : viewMappings) {
//                    String explTargetPath = mapping.getTargetPath();
//                    String explTargetServletPath = mapping.getTargetServletPath();
//
//                    if (UtilValidate.isNotEmpty(explTargetPath)) {
//                        if (CmsProcessMapping.targetMatchesRequestPath(explTargetPath, requestPath)) {
//                            if (UtilValidate.isNotEmpty(explTargetServletPath)) {
//                                // Note: we already checked whether requestServletPath matched during the lookup
//                                explBothMappings.add(mapping);
//                            }
//                            else {
//                                explPathMappings.add(mapping);
//                            }
//                        }
//                    }
//                    else {
//                        if (UtilValidate.isNotEmpty(explTargetServletPath)) {
//                            explServMappings.add(mapping);
//                        }
//                        else {
//                            implBothMappings.add(mapping);
//                        }
//                    }
//
//                }
//
//                if (explBothMappings.size() > 1 || explPathMappings.size() > 1 || explServMappings.size() > 1 || implBothMappings.size() > 1) {
//                    Debug.logWarning("Cms: There are multiple view mappings of the same type corresponding" +
//                            " to " + key + "; using first found only for given type" + CmsControlUtil.getReqLogIdDelimStr(request), module);
//                }
//
//                // Return the first mapping found, most specific type first
//                if (explBothMappings.size() > 0) {
//                    viewMapping = explBothMappings.get(0);
//                }
//                else if (explPathMappings.size() > 0) {
//                    viewMapping = explPathMappings.get(0);
//                }
//                else if (explServMappings.size() > 0) {
//                    viewMapping = explServMappings.get(0);
//                }
//                else if (implBothMappings.size() > 0) {
//                    viewMapping = implBothMappings.get(0);
//                }
//
//                if (CmsUtil.verboseOn()) {
//                  if (viewMapping != null) {
//                      Debug.logInfo("Cms: Found view mapping: " + viewMapping.getLogIdRepr() + CmsControlUtil.getReqLogIdDelimStr(request), module);
//                  }
//                  else {
//                      Debug.logInfo("Cms: No view mapping found" + CmsControlUtil.getReqLogIdDelimStr(request), module);
//                  }
//              }
//            }

            // OLD-Old lookup (faster but didn't allow extra requestPath restriction)
//            // Find one with an explicit targetServletPath (should match exactly)
//            viewMapping = CmsDataObject.<CmsViewMapping> findFirst(UtilMisc.toMap("webSiteId", webSiteId, 
//                    "targetServletPath", requestServletPath,
//                    "targetViewName", viewName), 
//                    CmsViewMapping.class, SingleFindMode.CANDIDATE_KEY_PERMISSIVE);
//            
//            if (viewMapping == null) {
//                if (requestServletPath.equals(defaultTargetServletPath)) {
//                    
//                    // Find one with empty target servlet path (uses default)
//                    List<EntityCondition> condList = new ArrayList<>();
//                    condList.add(EntityCondition.makeCondition("webSiteId", webSiteId));
//                    condList.add(EntityCondition.makeCondition("targetServletPath", null));
//                    condList.add(EntityCondition.makeCondition("targetViewName", viewName));
//                    
//                    viewMapping = CmsDataObject.<CmsViewMapping> findFirst(
//                            EntityCondition.makeCondition(condList, EntityOperator.AND), null,
//                            CmsViewMapping.class, SingleFindMode.CANDIDATE_KEY_PERMISSIVE);
//                    
//                    if (CmsUtil.verboseOn()) {
//                        if (viewMapping != null) {
//                            Debug.logInfo("Cms: Found view mapping using default target servlet path: " + viewMapping.getLogIdRepr() + CmsControlUtil.getReqLogIdDelimStr(request), module);
//                        }
//                        else {
//                            Debug.logInfo("Cms: No view mapping with explicit targetServletPath found; request servlet path" +
//                                  "matches default, but no view mapping with default target servlet path found" + CmsControlUtil.getReqLogIdDelimStr(request), module);
//                        }
//                    }
//                }
//                else {
//                    if (CmsUtil.verboseOn()) {
//                        Debug.logInfo("Cms: No view mapping with explicit targetServletPath found, and default target path (" +
//                                defaultTargetServletPath + ") does not match current request servlet path (" + requestServletPath + ")" + CmsControlUtil.getReqLogIdDelimStr(request), module);
//                    }
//                }
//                
//                
//            }
//            else {
//                if (CmsUtil.verboseOn()) {
//                    Debug.logInfo("Cms: Found view mapping with explicit targetServletPath: " + viewMapping.getLogIdRepr() + CmsControlUtil.getReqLogIdDelimStr(request), module);
//                }
//            }
            
            if (useGlobalCache) {
                cache.put(key, viewMapping);
            }
        } else {
            if (viewMappingEntry.hasValue()) {
                if (CmsUtil.verboseOn()) {
                    Debug.logVerbose("Cms: Retrieving view mapping from cache: " + makeLogIdRepr(viewName, webSiteId) + CmsControlUtil.getReqLogIdDelimStr(request), module);
                }
                viewMapping = viewMappingEntry.getValue();
            }
        }

        return viewMapping;
    }


    @Override
    public ViewMappingWorker getWorkerInst() {
        return ViewMappingWorker.worker;
    }
    
    public static ViewMappingWorker getWorker() {
        return ViewMappingWorker.worker;
    }
    
    public static class ViewMappingWorker extends ControlDataObjectWorker<CmsViewMapping> {

        private static final ViewMappingWorker worker = new ViewMappingWorker();
        
        // FIXME? These could potentially also include other fields... but hard to communicate in CMS apps
        private static final List<String> logicalPkFieldNames = Collections.unmodifiableList(Arrays.asList(new String[] { 
                "webSiteId", "targetViewName" 
        }));
        
        protected ViewMappingWorker() {
            super(CmsViewMapping.class);
        }
        
        @Override
        public List<String> getLogicalPkFieldNames(Delegator delegator) {
            return logicalPkFieldNames;
        }

        @Override
        public String getControlPageIdFieldName() {
            return "pageId";
        }

        @Override
        public CmsViewMapping makeFromValue(GenericValue value) throws CmsException {
            return new CmsViewMapping(value);
        }

        @Override
        public CmsViewMapping makeFromFields(Delegator delegator, Map<String, ?> fields) throws CmsException {
            return new CmsViewMapping(delegator, fields);
        }

        @Override
        public List<CmsViewMapping> findByWebSiteId(Delegator delegator, String webSiteId, boolean useCache)
                throws CmsException {
            return findAll(delegator, UtilMisc.toMap("webSiteId", webSiteId),
                    UtilMisc.toList("targetViewName ASC"), isUseDbCacheStatic(useCache));
        }

        @Override
        public void clearMemoryCaches() {
            getNameCache().clear();
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
        public static final VisitRelPlan INSTANCE = new VisitRelPlan("CmsViewMapping");
        static final VisitRelations visitRelations = INSTANCE.buildSafe();
        public VisitRelPlan(String majorEntityName) { super(majorEntityName); }
        @Override public VisitRelations.Builder planDefinition(Delegator delegator) throws Exception {
            return newBuilder(delegator)
                .entity("CmsViewMapping")
                    .self()
                    .relationMajor("CmsPage");
        }
    }
    
    public static class VisitRelNoPagePlan extends VisitRelations.BuildPlan {
        public static final VisitRelPlan INSTANCE = new VisitRelPlan("CmsViewMapping");
        static final VisitRelations visitRelations = INSTANCE.buildSafe();
        public VisitRelNoPagePlan(String majorEntityName) { super(majorEntityName); }
        @Override public VisitRelations.Builder planDefinition(Delegator delegator) throws Exception {
            return VisitRelPlan.INSTANCE.planDefinition(delegator)
                .removeAllRelationsOfRelEntityName("CmsPage");
        }
    }
}
