package com.ilscipio.scipio.cms.data;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TreeMap;
import java.util.TreeSet;

import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.UtilMisc;
import org.ofbiz.content.data.SpecDataResEntityInfo;
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.GenericEntityException;
import org.ofbiz.entity.model.ModelEntity;
import org.ofbiz.entity.model.ModelReader;
import org.ofbiz.entity.model.ModelRelation;
import org.ofbiz.entity.model.ModelViewEntity;

import com.ilscipio.scipio.cms.CmsUtil;

/**
 * CMS entity meta info.
 * <p>
 * See {@link #getCmsModelEntities} for ordering info.
 */
public class CmsEntityInfo {

    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());
    
    public static final String CMS_ENTITY_BASE_PKG = "com.ilscipio.scipio.cms";
    public static final String CMS_ENTITY_BASE_PKG_PREFIX = CMS_ENTITY_BASE_PKG + ".";
    
    /**
     * Default preferred CMS entity order, mainly for data export, but anything that lists the entities
     * should follow this so the order is the same everywhere.
     * <p>
     * NOTE: This does not contain ofbiz entities like Content, DataResource or ElectronicText, and
     * should not contain any view entities.
     * <p>
     * DEV NOTE: This list may become error-prone to maintain, so for this reason the code does a bunch of error
     * checks; enable debug mode and check logs to debug.
     * <p>
     * Last updated: 2017-05-31, based on DependencyGraph results.
     */
    static final List<String> cmsEntityNamesPrefOrderDefault = UtilMisc.unmodifiableArrayList(
            "CmsScriptTemplate",
            
            "CmsAssetTemplate", "CmsAssetTemplateScriptAssoc", 
            "CmsAssetTemplateVersion", "CmsAssetTemplateVersionState",
            
            "CmsPageTemplate", "CmsPageTemplateAssetAssoc", "CmsPageTemplateScriptAssoc", 
            "CmsPageTemplateVersion", "CmsPageTemplateVersionState",
            
            "CmsAttributeTemplate",
            
            "CmsPage", "CmsPageAuthorization", "CmsPageProductAssoc", "CmsPageScriptAssoc",
            "CmsPageVersion", "CmsPageVersionState",
            
            "CmsProcessMapping", "CmsPageSpecialMapping", "CmsProcessViewMapping", "CmsViewMapping"
    );
    
    /**
     * Major entity names, see {@link #getMajorCmsEntityNames()} for info.
     */
    static final Set<String> majorCmsEntityNames = Collections.unmodifiableSet(new LinkedHashSet<>(Arrays.asList(new String[] {
            "CmsScriptTemplate", "CmsAssetTemplate", "CmsPageTemplate", "CmsPage",
            "CmsProcessMapping", "CmsViewMapping"
    })));
    
    static final Set<String> extCmsEntityNames = Collections.unmodifiableSet(new LinkedHashSet<>(Arrays.asList(new String[] {
            "Content", "DataResource", "ElectronicText"
    })));
    
    /**
     * Special entity names that are treated as special cases; these may not actually exist.
     */
    static final Set<String> specialCmsEntityNames = Collections.unmodifiableSet(new LinkedHashSet<>(Arrays.asList(new String[] {
            "CmsMedia", "CmsMediaVariants"
    })));
    
    static final Set<String> specialMajorCmsEntityNames = Collections.unmodifiableSet(new LinkedHashSet<>(Arrays.asList(new String[] {
            "CmsMedia"
    })));

    static final Map<String, Set<String>> specialCmsEntityNamesByPkg;
    static {
        Map<String, Set<String>> map = new LinkedHashMap<>();
        map.put("com.ilscipio.scipio.cms.media", Collections.unmodifiableSet(new LinkedHashSet<>(Arrays.asList(new String[] {
                "CmsMedia", "CmsMediaVariants"
        }))));
        specialCmsEntityNamesByPkg = Collections.unmodifiableMap(map);
    }
    
    // NOTE: special fast cache pattern (read-only cache)
    private static Map<ModelEntity, Set<String>> cmsContentIdFieldNamesCache = Collections.unmodifiableMap(new HashMap<ModelEntity, Set<String>>());
    private static Map<ModelEntity, List<ModelRelation>> cmsContentModelRelationsCache = Collections.unmodifiableMap(new HashMap<ModelEntity, List<ModelRelation>>());
    
    // doesn't depend on model reader
    protected static final Map<String, Set<String>> entityCdataFields = Collections.unmodifiableMap(makeEntityCdataFieldsMap());

    private static final CmsEntityInfo INSTANCE;
    static {
        // DEV NOTE: keep this after all other statics
        CmsEntityInfo inst;
        Delegator delegator = null;
        try {
            delegator = EntityInfoUtil.getDefaultDelegatorAlways();
            inst = new CmsEntityInfo(delegator, getCmsEntityNamesPrefOrderDefault(), 
                    OrderMode.Explicit.INSTANCE, true);
        } catch(Throwable t) {
            Debug.logError(t, "Could not build information for default CmsEntityInfo instance", module);
            inst = new CmsEntityInfo(delegator, true);
        }
        INSTANCE = inst;
    }
    
    protected final OrderMode orderMode;
    protected final Set<ModelEntity> cmsModelEntities;
    protected final Set<String> cmsEntityNames;
    protected final Map<String, Set<ModelEntity>> cmsModelEntitiesByPkg;
    protected final Map<String, Set<String>> cmsEntityNamesByPkg;
    
    protected final Set<String> combinedCmsEntityNames;
    protected final Map<String, Set<String>> combinedCmsEntityNamesByPkg;
    protected final Set<String> combinedMajorCmsEntityNames;
    
    protected final ModelReader modelReader;

    /******************************************************/
    /* Special Types */
    /******************************************************/
    
    public static abstract class OrderMode {
        public abstract <T> Set<T> makeSet();
        public abstract <K, V> Map<K, V> makeMap();
        
        public <T> Set<T> makeEntitySet() { return makeSet(); }
        public <K, V> Map<K, V> makeEntityPkgMap() { return makeMap(); }
        
        public static class None extends OrderMode {
            public static final None INSTANCE = new None();
            private None() {}
            @Override
            public <T> Set<T> makeSet() { return new HashSet<>(); }
            @Override
            public <K, V> Map<K, V> makeMap() { return new HashMap<>(); }
        }
        public static class Explicit extends OrderMode {
            public static final Explicit INSTANCE = new Explicit();
            private Explicit() {}
            @Override
            public <T> Set<T> makeSet() { return new LinkedHashSet<>(); }
            @Override
            public <K, V> Map<K, V> makeMap() { return new LinkedHashMap<>(); }
            
            @Override
            public <K, V> Map<K, V> makeEntityPkgMap() {
                // TODO/FIXME: We're making TreeMaps for these because we don't support explicit order for them yet
                return new TreeMap<>();
            }
        }
        public static class Natural extends OrderMode {
            public static final Natural INSTANCE = new Natural();
            private Natural() {}
            @Override
            public <T> Set<T> makeSet() { return new TreeSet<>(); }
            @Override
            public <K, V> Map<K, V> makeMap() { return new TreeMap<>(); }
        }
    }
    
    /******************************************************/
    /* Constructors */
    /******************************************************/

    /**
     * Fetches instance for delegator.
     * The collections are ordered and based on the preferred and dependency-check order.
     */
    public static CmsEntityInfo getInst(Delegator delegator) {
        return INSTANCE; // TODO: review if new instance or caches needed
    }
    
//    private static CmsEntityInfo getInst(ModelReader reader) {
//        return INSTANCE; // TODO: review if new instance or caches needed
//    }
    
    protected CmsEntityInfo(Delegator delegator, boolean empty) { // limited mitigation in case of unexpected init fail
        this.modelReader = (delegator != null) ? delegator.getModelReader() : null;
        this.orderMode = OrderMode.Explicit.INSTANCE;
        this.cmsModelEntities = Collections.emptySet();
        this.cmsEntityNames = Collections.emptySet();
        this.cmsModelEntitiesByPkg = Collections.emptyMap();
        this.cmsEntityNamesByPkg = Collections.emptyMap();
        this.combinedCmsEntityNames = Collections.emptySet();
        this.combinedCmsEntityNamesByPkg = Collections.emptyMap();
        this.combinedMajorCmsEntityNames = Collections.emptySet();
    }
    
    /**
     * Regular constructor, with ordering of collections by preferred/dependency order and read-only.
     * WARN: readOnly not guaranteed to make nested collections unmodifiable.
     */
    public CmsEntityInfo(Delegator delegator) {
        this(delegator, getCmsEntityNamesPrefOrderDefault(delegator), OrderMode.Explicit.INSTANCE, true);
    }
    
    /**
     * Full constructor.
     * WARN: readOnly not guaranteed to make nested collections unmodifiable.
     */
    public CmsEntityInfo(Delegator delegator, List<String> cmsEntityNamesPrefOrder, OrderMode orderMode, boolean readOnly) {
        this(delegator.getModelReader(), cmsEntityNamesPrefOrder, OrderMode.Explicit.INSTANCE, readOnly);
    }
    
    /**
     * Full constructor.
     * WARN: readOnly not guaranteed to make nested collections unmodifiable.
     * <p>
     * NOTE: this used to build sets using TreeSet to they naturally order using ModelEntity.compareTo,
     * which was the xmldsdump.groovy script behavior, but this is now causing issues so now everything is a
     * LinkedHashSet and bases on the preferred order instead.
     */
    private CmsEntityInfo(ModelReader reader, List<String> cmsEntityNamesPrefOrder, OrderMode orderMode, boolean readOnly) {
        this.modelReader = reader;
        this.orderMode = orderMode;
        Set<ModelEntity> cmsModelEntities = orderMode.makeEntitySet();
        Set<String> cmsEntityNames = orderMode.makeEntitySet();
        Map<String, Set<ModelEntity>> cmsModelEntitiesByPkg = orderMode.makeEntityPkgMap();
        Map<String, Set<String>> cmsEntityNamesByPkg = orderMode.makeEntityPkgMap();
        try {
            buildCmsModelEntityInfoCollections(reader, cmsEntityNamesPrefOrder, orderMode, cmsModelEntities, cmsEntityNames, cmsModelEntitiesByPkg, cmsEntityNamesByPkg);
        } catch(Throwable t) {
            Debug.logError(t, "Could not build CMS entity information", module);
        }
        this.cmsModelEntities = readOnly ? Collections.unmodifiableSet(cmsModelEntities) : cmsModelEntities;
        this.cmsEntityNames = readOnly ? Collections.unmodifiableSet(cmsEntityNames) : cmsEntityNames;
        this.cmsModelEntitiesByPkg = readOnly ? Collections.unmodifiableMap(cmsModelEntitiesByPkg) : cmsModelEntitiesByPkg;
        this.cmsEntityNamesByPkg = readOnly ? Collections.unmodifiableMap(cmsEntityNamesByPkg) : cmsEntityNamesByPkg;
        
        // FIXME: order is being imposed here
        Set<String> combinedCmsEntityNames = orderMode.makeEntitySet();
        combinedCmsEntityNames.addAll(cmsEntityNames);
        combinedCmsEntityNames.addAll(specialCmsEntityNames);
        this.combinedCmsEntityNames = readOnly ? Collections.unmodifiableSet(combinedCmsEntityNames) : combinedCmsEntityNames;

        // FIXME: order is being imposed here
        Map<String, Set<String>> combinedCmsEntityNamesByPkg = orderMode.makeEntityPkgMap();
        combinedCmsEntityNamesByPkg.putAll(cmsEntityNamesByPkg);
        combinedCmsEntityNamesByPkg.putAll(specialCmsEntityNamesByPkg);
        this.combinedCmsEntityNamesByPkg = readOnly ? Collections.unmodifiableMap(combinedCmsEntityNamesByPkg) : combinedCmsEntityNamesByPkg;
        
        // FIXME: order is being imposed here
        Set<String> combinedMajorCmsEntityNames = orderMode.makeEntitySet();
        combinedMajorCmsEntityNames.addAll(majorCmsEntityNames);
        combinedMajorCmsEntityNames.addAll(specialMajorCmsEntityNames);
        this.combinedMajorCmsEntityNames = readOnly ? Collections.unmodifiableSet(combinedMajorCmsEntityNames) : combinedMajorCmsEntityNames;
    }
    
    /******************************************************/
    /* Static Info/Getters */
    /******************************************************/

    public static List<String> getCmsEntityNamesPrefOrderDefault(Delegator delegator) {
        return cmsEntityNamesPrefOrderDefault;
    }
    
    public static List<String> getCmsEntityNamesPrefOrderDefault() {
        return cmsEntityNamesPrefOrderDefault;
    }
    
    public static Set<String> getMajorCmsEntityNamesStatic() {
        return majorCmsEntityNames;
    }
    
    /******************************************************/
    /* Getters/Meta Information */
    /******************************************************/
   
    public String getCmsEntityBasePkg() { // (for easier access from other langs)
        return CMS_ENTITY_BASE_PKG;
    }

    public String getCmsEntityBasePkgPrefix() { // (for easier access from other langs)
        return CMS_ENTITY_BASE_PKG_PREFIX;
    }

    public Map<String, Set<String>> getEntityCdataFields() {
        return entityCdataFields;
    }

    public OrderMode getOrderMode() {
        return orderMode;
    }
    
    /**
     * Returns the CMS model entities set.
     * If this instance is pref-ordering - such as all instances returned by the {@link #getInst} methods,
     * this set is ordered (LinkedHashSet) and follows the preferred entity ordering determined
     * mostly by {@link #getCmsEntityNamesPrefOrder}, but with further enforced dependency checking.
     */
    public Set<ModelEntity> getCmsModelEntities() {
        return cmsModelEntities;
    }

    /**
     * Returns the entity names set.
     * Ordering behaves the same as {@link #getCmsModelEntities}.
     */
    public Set<String> getCmsEntityNames() {
        return cmsEntityNames;
    }

    /**
     * Returns the map of partial and full CMS sub-packages to the model entities they contain.
     * Ordering of the entity names behaves the same as {@link #getCmsModelEntities},
     * BUT the package keys are done using natural ordering instead of explicit (FIXME: can't override order).
     */
    public Map<String, Set<ModelEntity>> getCmsModelEntitiesByPkg() {
        return cmsModelEntitiesByPkg;
    }

    /**
     * Returns the map of partial and full CMS sub-packages to the entity names they contain.
     * Ordering behaves the same as {@link #getCmsEntityNames},
     * BUT the package keys are done using natural ordering instead of explicit (FIXME: can't override order).
     */
    public Map<String, Set<String>> getCmsEntityNamesByPkg() {
        return cmsEntityNamesByPkg;
    }
    
    /**
     * Returns entity names for full package name, or null if no such package registered.
     */
    public Set<String> getCmsEntityNamesForPkg(String pkgName) {
        return cmsEntityNamesByPkg.get(pkgName);
    }
    
    /**
     * Returns external/extra/ofbiz entity names used by CMS, such as Content, DataResource, etc.
     */
    public Set<String> getExtCmsEntityNames() {
        return extCmsEntityNames;
    }
    
    /**
     * Returns the special entity names set; e.g. CmsMedia "fake" entity name.
     * Ordering behaves the same as {@link #getCmsModelEntities}.
     */
    public Set<String> getSpecialCmsEntityNames() {
        return specialCmsEntityNames;
    }
    
    public Map<String, Set<String>> getSpecialCmsEntityNamesByPkg() {
        return specialCmsEntityNamesByPkg;
    }
    
    public Set<String> getSpecialMajorCmsEntityNames() {
        return specialMajorCmsEntityNames;
    }
    
    /**
     * Returns {@link #getCmsEntityNames()} + {@link #getSpecialCmsEntityNames()}.
     */
    public Set<String> getCombinedCmsEntityNames() {
        return combinedCmsEntityNames;
    }

    /**
     * Returns {@link #getCmsEntityNamesByPkg()} + {@link #getSpecialCmsEntityNamesByPkg()}.
     */
    public Map<String, Set<String>> getCombinedCmsEntityNamesByPkg() {
        return combinedCmsEntityNamesByPkg;
    }
    
    /**
     * Returns {@link #getMajorCmsEntityNames()} + {@link #getSpecialMajorCmsEntityNames()}.
     */
    public Set<String> getCombinedMajorCmsEntityNames() {
        return combinedMajorCmsEntityNames;
    }

    public Set<String> copyCmsEntityNamesNoFilter(Collection<String> pkgNames, Collection<String> addEntityNames, Collection<String> removeEntityNames) {
        Set<String> res = new HashSet<>();
        if (pkgNames != null) {
            for(String pkgName : pkgNames) {
                Set<String> entityNames = cmsEntityNamesByPkg.get(pkgName);
                if (entityNames != null) {
                    res.addAll(entityNames);
                }
            }
        }
        if (addEntityNames != null) res.addAll(addEntityNames);
        if (removeEntityNames != null) res.removeAll(removeEntityNames);
        return res;
    }
    
    /**
     * Same as {@link #getCmsEntityNamesForPkg} but creates a copy and returns empty set instead of null,
     * and supports multiple package names (names concatenated). The combined sets are reordered if applicable.
     * Extra entity names can be specified to be added or removed.
     * WARN: DON'T USE OUTSIDE OF PRESET BUILDING FOR NOW
     */
    public Set<String> copyCmsEntityNames(Collection<String> pkgNames, Collection<String> addEntityNames, Collection<String> removeEntityNames) {
        return filterCombinedCmsEntityNames(copyCmsEntityNamesNoFilter(pkgNames, addEntityNames, removeEntityNames));
    }
    
    public Set<String> copyCmsEntityNames(Collection<String> pkgNames, Collection<String> addEntityNames) {
        return copyCmsEntityNames(pkgNames, addEntityNames, null);
    }
    
    public Set<String> copyCmsEntityNames(Collection<String> pkgNames) {
        return copyCmsEntityNames(pkgNames, null, null);
    }
    
    public Set<String> copyCmsEntityNames(String pkgName) {
        Set<String> res = getOrderMode().makeEntitySet();
        Set<String> entityNames = cmsEntityNamesByPkg.get(pkgName);
        if (entityNames != null) {
            res.addAll(entityNames);
        }
        return res; // no reorder needed in this case
    }
    
    /**
     * Returns set of "major" entities' names, entities that represent high-level 
     * objects or concepts or demarcations in CMS, and whose CmsDataObject class 
     * implements {@link CmsMajorObject}.
     */
    public Set<String> getMajorCmsEntityNames() {
        return majorCmsEntityNames;
    }
    
    public boolean isMajorCmsEntity(String entityName) {
        return majorCmsEntityNames.contains(entityName);
    }
    
    /******************************************************/
    /* Instance Helpers and Checks */
    /******************************************************/

    /**
     * Filters and optionally orders the given entity names using the same order this instance is using for {@link #getCmsEntityNames()},
     * and eliminates any names not recognized as part of those. It only orders if the given outEntityNames (returned)
     * is LinkedHashSet or equiv.
     * NOTE: will eliminate Content/DataResource/ElectronicText.
     */
    public Set<String> filterCmsEntityNames(Collection<String> names, Set<String> outEntityNames) {
        if (names == null) return outEntityNames;
        outEntityNames.addAll(getCmsEntityNames());
        outEntityNames.retainAll(names);
        return outEntityNames;
    }
    
    /**
     * Same as {@link #filterCmsEntityNames} but uses <code>getOrderMode().<String>makeEntitySet()</code>
     * for the out; ordering not guaranteed.
     */
    public Set<String> filterCmsEntityNames(Collection<String> names) {
        return filterCmsEntityNames(names, getOrderMode().<String>makeEntitySet());
    }
    
    /**
     * Filters AND orders the given entity names using the same order this instance is using for {@link #getCmsEntityNames()},
     * and eliminates any names not recognized as part of those. 
     * The returned collection is a LinkedHashSet to preserve order.
     * NOTE: will eliminate Content/DataResource/ElectronicText.
     */
    public Set<String> filterOrderCmsEntityNames(Collection<String> names) {
        return filterCmsEntityNames(names, OrderMode.Explicit.INSTANCE.<String>makeEntitySet());
    }
    
    /**
     * Similar to {@link #filterCmsEntityNames(Collection, Set)} but returns only Major entity names,
     * in other words entities corresponding to the data object classes that implement {@link CmsMajorObject}.
     */
    public Set<String> filterMajorCmsEntityNames(Collection<String> names, Set<String> outEntityNames) {
        if (names == null) return outEntityNames;
        outEntityNames.addAll(getMajorCmsEntityNames());
        outEntityNames.retainAll(names);
        return outEntityNames;
    }
    
    public Set<String> filterMajorCmsEntityNames(Collection<String> names) {
        return filterMajorCmsEntityNames(names, getOrderMode().<String>makeEntitySet());
    }
    
    public Set<String> filterOrderMajorCmsEntityNames(Collection<String> names) {
        return filterMajorCmsEntityNames(names, OrderMode.Explicit.INSTANCE.<String>makeEntitySet());
    }
    
    public Set<String> filterSpecialCmsEntityNames(Collection<String> names, Set<String> outEntityNames) {
        if (names == null) return outEntityNames;
        outEntityNames.addAll(getSpecialCmsEntityNames());
        outEntityNames.retainAll(names);
        return outEntityNames;
    }
    
    public Set<String> filterSpecialCmsEntityNames(Collection<String> names) {
        return filterSpecialCmsEntityNames(names, getOrderMode().<String>makeEntitySet());
    }
    
    public Set<String> filterCombinedCmsEntityNames(Collection<String> names, Set<String> outEntityNames) {
        if (names == null) return outEntityNames;
        outEntityNames.addAll(getCombinedCmsEntityNames());
        outEntityNames.retainAll(names);
        return outEntityNames;
    }
    
    public Set<String> filterCombinedCmsEntityNames(Collection<String> names) {
        return filterCombinedCmsEntityNames(names, getOrderMode().<String>makeEntitySet());
    }
    
    /**
     * Determines the special contentId reference field names for the given CMS entity.
     */
    public Set<String> getCmsContentIdFieldNames(ModelEntity modelEntity) {
        return getCmsContentIdFieldNamesStatic(modelEntity);
    }
    
    public List<ModelRelation> getCmsContentModelRelations(ModelEntity modelEntity) {
        return getCmsContentModelRelationsStatic(modelEntity);
    }
    
    public boolean containsCmsModelEntityInstance(ModelEntity modelEntity) {
        return cmsModelEntities.contains(modelEntity);
    }
    
    public boolean isCmsModelEntity(ModelEntity modelEntity) {
        return cmsModelEntities.contains(modelEntity) || isCmsModelEntityStatic(modelEntity);
    }
    
    /******************************************************/
    /* Static Helpers */
    /******************************************************/
    
    /**
     * Determines the special contentId reference field names for the given CMS entity.
     * Uses a fast homemade cache so there will be no issue calling this on thousands of records (e.g. for data export).
     */
    public static Set<String> getCmsContentIdFieldNamesStatic(ModelEntity modelEntity) {
        Set<String> fieldNames = cmsContentIdFieldNamesCache.get(modelEntity);
        if (fieldNames == null && modelEntity != null) {
            // NOTE: this slow check is within the cache check so no perf issue, plus avoids
            // overloading the cache with non-CMS modelEntity instances
            if (!isCmsModelEntityStatic(modelEntity)) {
                return Collections.<String>emptySet();
            }
            // NOTE: should be no harm including one-nofk here...
            fieldNames = Collections.unmodifiableSet(findContentIdFieldNamesStatic(modelEntity));
            // duplicate the whole homemade immutable cache, for best read perf + thread safety
            // NOTE: no need to synchronize, if we create a couple duplicate lists it's not an issue.
            Map<ModelEntity, Set<String>> cache = new HashMap<>(cmsContentIdFieldNamesCache);
            cache.put(modelEntity, fieldNames);
            // the unmodifiableMap inner "final" should give thread safety as long as no further changes (hence the copy)
            cmsContentIdFieldNamesCache = Collections.unmodifiableMap(cache); 
        }
        return fieldNames;
    }
    
    /**
     * Determines the contentId fields on any entity.
     * WARN: does not check that modelEntity is a CMS entity.
     */
    public static Set<String> findContentIdFieldNamesStatic(ModelEntity modelEntity) {
        return EntityInfoUtil.getRelationFieldNames(modelEntity, "Content", true, true, false);
    }
    
    public static List<ModelRelation> getCmsContentModelRelationsStatic(ModelEntity modelEntity) {
        List<ModelRelation> fieldNames = cmsContentModelRelationsCache.get(modelEntity);
        if (fieldNames == null && modelEntity != null) {
            if (!isCmsModelEntityStatic(modelEntity)) {
                return Collections.<ModelRelation>emptyList();
            }
            fieldNames = Collections.unmodifiableList(findCmsContentModelRelationsStatic(modelEntity));
            Map<ModelEntity, List<ModelRelation>> cache = new HashMap<>(cmsContentModelRelationsCache);
            cache.put(modelEntity, fieldNames);
            cmsContentModelRelationsCache = Collections.unmodifiableMap(cache); 
        }
        return fieldNames;
    }
    
    public static List<ModelRelation> findCmsContentModelRelationsStatic(ModelEntity modelEntity) {
        return EntityInfoUtil.findModelRelationsByRelEntityName(modelEntity, "Content", true, true, false);
    }
    
    /**
     * Figures out if the given entity is a CMS entity.
     */
    public static boolean isCmsModelEntityStatic(ModelEntity modelEntity) {
        return (modelEntity.getPackageName() != null && modelEntity.getPackageName().startsWith(CMS_ENTITY_BASE_PKG_PREFIX));
    }
    
    /**
     * Makes a map of sets of all the entity field names we want to always print out as CDATA blocks in XML output.
     */
    public static Map<String, Set<String>> makeEntityCdataFieldsMap() {
        Map<String, Set<String>> map = new HashMap<>();
        map.put("ElectronicText", new HashSet<>(Arrays.asList(new String[] { "textData" })));
        for(SpecDataResEntityInfo entityInfo : SpecDataResEntityInfo.getEntityInfoList()) {
            map.put(entityInfo.getEntityName(), new HashSet<>(Arrays.asList(new String[] { entityInfo.getDataFieldName() })));
        }
        return map;
    }

    /**
     * Determines a preferred-ordering set of all CMS model entities, with dependencies optionally enforced, such that
     * least-dependent appears first and most-dependent appears last.
     * Does NOT contain ofbiz entities like Content, DataResource or ElectronicText, nor view entities.
     */
    public static Collection<ModelEntity> findCmsModelEntitiesWithPrefOrder(ModelReader reader, List<String> cmsEntityNamesPrefOrder) {
        return findModelEntitiesWithPrefOrder(reader, cmsEntityNamesPrefOrder,
                false, Arrays.asList(new String[] { CMS_ENTITY_BASE_PKG_PREFIX }), 
                "Cms: ", " - please verify the preferred entity order (see " + module + ")", true, true, true);
    }
    
    /**
     * Determines a preferred-ordering set of all relevant model entities, based on a combination of 
     * a list of preferred names and a filter on all entities,
     * with dependencies optionally enforced, such that least-dependent appears first and most-dependent appears last.
     * <p>
     * TODO?: move to EntityInfoUtil later
     */
    public static Collection<ModelEntity> findModelEntitiesWithPrefOrder(ModelReader reader, 
            Collection<String> entityNamesPrefOrder, boolean allowViews, Collection<String> allowedPkgPrefixes, 
            String logPrefix, String logErrorSuffix, boolean depCheck, boolean prefChecks, boolean prefOrderCheck) {
        // First, get the explicitly-named CMS entities in the order we prefer
        LinkedHashSet<String> candidateEntityNames = new LinkedHashSet<>(entityNamesPrefOrder);
    
        // Sanity check
        if (prefChecks && candidateEntityNames.size() != entityNamesPrefOrder.size()) {
            Debug.logError(logPrefix + "EntityInfo: Preferred entity order contains duplicate names" + logErrorSuffix, module);
        }
        
        // Second, add anything we missed at the end (if any, sorted by name)
        try {
            Set<String> foundNames = new TreeSet<>(); // NOTE: the extra values can only be ordered by name at best...
            for(ModelEntity modelEntity : reader.getEntityCache().values()) {
                if ((!allowViews && modelEntity instanceof ModelViewEntity) || !EntityInfoUtil.hasPackagePrefix(modelEntity, allowedPkgPrefixes)) {
                    continue;
                }
                String name = modelEntity.getEntityName();
                if (!candidateEntityNames.contains(name)) {
                    if (prefChecks) {
                        Debug.logWarning(logPrefix + "EntityInfo: Found entity that is not part of the "
                            + "preferred entity order: " + modelEntity.getEntityName() + logErrorSuffix, module);
                    }
                    foundNames.add(name);
                }
            }
            candidateEntityNames.addAll(foundNames);
        } catch (GenericEntityException e) {
            Debug.logError(e, logPrefix + "EntityInfo: Entity engine error reading global entity list: " + e.getMessage() + logErrorSuffix, module);
        }
        
        // Third, sanity checks
        LinkedHashSet<ModelEntity> candidateModelEntities = new LinkedHashSet<>();
        for(String name : candidateEntityNames) {
            try {
                ModelEntity modelEntity = reader.getModelEntity(name);
                if (prefChecks) {
                    if (!allowViews && modelEntity instanceof ModelViewEntity) {
                        Debug.logError(logPrefix + "EntityInfo: Encountered name (" + name + ") of view-entity in preferred entity order"
                                + "; must only include physical entities" + logErrorSuffix, module);
                        continue;
                    }
                    if (!EntityInfoUtil.hasPackagePrefix(modelEntity, allowedPkgPrefixes)) {
                        Debug.logError(logPrefix + "EntityInfo: Encountered name (" + name + ") of entity in preferred entity order"
                                + " that doesn't match the allowed package prefixes" + logErrorSuffix, module);
                        continue;
                    }
                }
                candidateModelEntities.add(modelEntity);
            } catch(GenericEntityException e) {
                Debug.logError(e, logPrefix + "EntityInfo: Encountered entity name not found as models in the system: "
                        + name + logErrorSuffix, module);
            }
        }
        if (candidateModelEntities.size() != candidateEntityNames.size()) {
            // filter out the invalid names
            candidateEntityNames = new LinkedHashSet<>();
            EntityInfoUtil.getEntityNames(candidateModelEntities, candidateEntityNames);
        } 
        
        if (depCheck) {
            // Four, enforce dependencies by running the dep graph ago
            // DEV NOTE: this ensures entitymodel corrections won't break everything too easily.
            // It could be omitted but I needed to write all this to generate the initial preferred order anyway,
            // so this will help deal with changes.
            try {
                List<String> resolvedEntityNames = EntityInfoUtil.makeEntityNameDependencyOrder(candidateModelEntities, candidateEntityNames,
                        CmsUtil.verboseOn(), logPrefix, logErrorSuffix);
                List<String> candidateEntityNamesList = new ArrayList<String>(candidateEntityNames); // needed for equals
                boolean orderChanged = !resolvedEntityNames.equals(candidateEntityNamesList);
                if (prefOrderCheck && orderChanged) {
                    Debug.logWarning(logPrefix + "EntityInfo: Preferred entity order does not match the resolved dependencies order"
                            + "\nPreferred order:\n" + candidateEntityNamesList.toString()
                            + "\nComputed order:\n" + resolvedEntityNames.toString()
                            + "\n" + logErrorSuffix, module);
                    
                    candidateEntityNames = new LinkedHashSet<>(resolvedEntityNames);
                    candidateModelEntities = new LinkedHashSet<>();
                    EntityInfoUtil.getModelEntitiesSafe(reader, candidateEntityNames, candidateModelEntities);
                    if (candidateModelEntities.size() != candidateEntityNames.size()) {
                        Debug.logError(logPrefix + "EntityInfo: Encountered entity names not found as models in the system" + logErrorSuffix, module);
                        // filter out the invalid names
                        candidateEntityNames = new LinkedHashSet<>();
                        EntityInfoUtil.getEntityNames(candidateModelEntities, candidateEntityNames);
                    }
                }
            } catch(Throwable t) {
                Debug.logError(t, logPrefix + "EntityInfo: Could not resolve entity dependencies. Cause: "
                        + t.getMessage() + logErrorSuffix, module);
            }
        }
        return candidateModelEntities;
    }
    
    public static void buildCmsModelEntityInfoCollections(ModelReader reader, List<String> cmsEntityNamesPrefOrder, OrderMode orderMode,
            Collection<ModelEntity> modelEntities, Collection<String> allCmsEntityNames,
            Map<String, Set<ModelEntity>> modelEntitiesByPkg, Map<String, Set<String>> cmsEntityNamesByPkg) {
        for(ModelEntity modelEntity : findCmsModelEntitiesWithPrefOrder(reader, cmsEntityNamesPrefOrder)) {
            String name = modelEntity.getEntityName();
            String pkgName = modelEntity.getPackageName();
            if (pkgName != null && pkgName.startsWith(CMS_ENTITY_BASE_PKG_PREFIX)) {
                allCmsEntityNames.add(name);
                String subPkgRel = pkgName.substring(CMS_ENTITY_BASE_PKG_PREFIX.length());
                String[] subPkgs = EntityInfoUtil.generateSubPackageNames(subPkgRel);
                for(String subPkg : subPkgs) {
                    subPkg = CMS_ENTITY_BASE_PKG_PREFIX + subPkg;
                    Set<String> nameList = cmsEntityNamesByPkg.get(subPkg);
                    if (nameList == null) {
                        nameList = orderMode.makeEntitySet();
                        cmsEntityNamesByPkg.put(subPkg, nameList);
                    }
                    nameList.add(modelEntity.getEntityName());
                    
                    modelEntities.add(modelEntity);
                    Set<ModelEntity> modelEntityList = modelEntitiesByPkg.get(subPkg);
                    if (modelEntityList == null) {
                        modelEntityList = orderMode.makeEntitySet();
                        modelEntitiesByPkg.put(subPkg, modelEntityList);
                    }
                    modelEntityList.add(modelEntity);
                }
            } else {
                Debug.logError("Cms: EntityInfo: Unexpected package name (" + pkgName + ") for entity "+ name, module);
            }
        }

    }

}
