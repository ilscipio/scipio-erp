package com.ilscipio.scipio.product.category;

import java.sql.Timestamp;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Set;

import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.GeneralException;
import org.ofbiz.base.util.UtilMisc;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.GenericEntityException;
import org.ofbiz.entity.GenericValue;
import org.ofbiz.entity.condition.EntityCondition;
import org.ofbiz.entity.util.EntityListIterator;
import org.ofbiz.entity.util.EntityQuery;
import org.ofbiz.service.LocalDispatcher;

import com.ilscipio.scipio.product.category.CatalogTraversalException.StopCatalogTraversalException;
import com.ilscipio.scipio.product.category.CatalogVisitor.AbstractCatalogVisitor;
import com.ilscipio.scipio.product.seo.SeoCatalogTraverser;

/**
 * SCIPIO: Factors out the boilerplate code for traversing catalog categories.
 * Everything is overridable, plus supports a separate CatalogVisitor for the 
 * very core calls only.
 * <p>
 * Class is NOT thread-safe or serializable - keep out of session attributes.
 * <p>
 * DEV NOTE: do not put specific or stateful functionality in this class.
 * I made a special {@link SeoCatalogTraverser} subclass for that, 
 * because this one is may be used in many places (not just SEO).
 * <p>
 * TODO: LOCALIZE thrown errors
 */
public class CatalogTraverser extends AbstractCatalogVisitor {

    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());
    
    /**
     * Expected max category depth (optimization), intentionally exaggerated (probably no one has categories
     * deeper than a dozen or so).
     */
    public static final int MAX_CATEGORY_DEPTH_PERF = 128;
    
    protected static final String logPrefix = ""; // "CategoryTraverser: "
    
    private final CatalogVisitor visitor;
    private final Delegator delegator;
    private final LocalDispatcher dispatcher;
    /**
     * Basic traversal config options, prevents constructor overload. Always non-null.
     */
    protected TraversalConfig travConfig;
    
    protected Set<String> seenCategoryIds = null;
    protected Set<String> seenProductIds = null;

    /**
     * Composed visitor constructor, with explicit visitor.
     */
    public CatalogTraverser(CatalogVisitor visitor, Delegator delegator, LocalDispatcher dispatcher, TraversalConfig travConfig) {
        this.visitor = (visitor != null) ? visitor : this;
        this.delegator = delegator;
        this.dispatcher = dispatcher;
        this.travConfig = (travConfig != null) ? travConfig : newTravConfig();
    }
    
    /**
     * Abstract instance constructor, with the traverser itself as the visitor.
     */
    public CatalogTraverser(Delegator delegator, LocalDispatcher dispatcher, TraversalConfig travConfig) {
        this(null, delegator, dispatcher, travConfig);
    }

    public static class TraversalConfig {
        private boolean useCache = false;
        
        private boolean doCategory = true;
        private boolean doProduct = true;
        private boolean filterByDate = true;
        private Timestamp moment = null;
        
        private boolean preventDupCategoryAny = false;
        private boolean preventDupCategoryVisit = false;
        private boolean preventDupCategoryTraversal = false;
        private boolean preventDupProductVisit = false;
        
        public boolean isUseCache() {
            return useCache;
        }
        
        public TraversalConfig setUseCache(boolean useCache) {
            this.useCache = useCache;
            return this;
        }
        
        public TraversalConfig setDoTypes(Collection<String> types) {
            if (types == null) types = Collections.emptyList();
            boolean doAll = types.contains("all");
            this.doProduct = doAll || types.contains("product");
            this.doCategory = doAll || types.contains("category");
            return this;
        }
        
        public boolean isDoCategory() {
            return doCategory;
        }
        
        public TraversalConfig setDoCategory(boolean doCategory) {
            this.doCategory = doCategory;
            return this;
        }
        
        public boolean isDoProduct() {
            return doProduct;
        }
        
        public TraversalConfig setDoProduct(boolean doProduct) {
            this.doProduct = doProduct;
            return this;
        }
        
        /**
         * If true, all association queries filter by date to exclude expired.
         * Default for this class is true.
         * @see #getMoment
         */
        public boolean isFilterByDate() {
            return filterByDate;
        }
        
        public TraversalConfig setFilterByDate(boolean filterByDate) {
            this.filterByDate = filterByDate;
            return this;
        }
        
        /**
         * The moment used for queries that use filter-by-date.
         * Default for this class is null.
         * <p>
         * NOTE: When null and {@link #isFilterByDate} is true, means "use now timestamp"; in this case
         * each query gets its own timestamp, whereas when moment set, all queries get the same timestamp.
         * @see #isFilterByDate
         */
        public Timestamp getMoment() {
            return moment;
        }
        
        public TraversalConfig setMoment(Timestamp moment) {
            this.moment = moment;
            return this;
        }
        
        /**
         * Uses an in-memory category and product list to try to prevent duplicate visit calls.
         * WARN: should not be enabled when traversing huge numbers of products.
         */
        public boolean isPreventDupCategoryVisit() {
            return preventDupCategoryVisit;
        }
        
        public TraversalConfig setPreventDupCategoryVisit(boolean preventDupCategoryVisit) {
            this.preventDupCategoryVisit = preventDupCategoryVisit;
            this.preventDupCategoryAny = (this.preventDupCategoryVisit || this.preventDupCategoryTraversal);
            return this;
        }
        
        /**
         * Uses an in-memory category and product list to try to prevent duplicate traversal calls.
         * WARN: should not be enabled when traversing huge numbers of products.
         */
        public boolean isPreventDupCategoryTraversal() {
            return preventDupCategoryTraversal;
        }
        
        public TraversalConfig setPreventDupCategoryTraversal(boolean preventDupCategoryTraversal) {
            this.preventDupCategoryTraversal = preventDupCategoryTraversal;
            this.preventDupCategoryAny = (this.preventDupCategoryVisit || this.preventDupCategoryTraversal);
            return this;
        }
        
        /**
         * Uses an in-memory category and product list to try to prevent duplicate visit calls.
         * WARN: should not be enabled when traversing huge numbers of products.
         */
        public boolean isPreventDupProductVisit() {
            return preventDupProductVisit;
        }
        
        public TraversalConfig setPreventDupProductVisit(boolean preventDupProductVisit) {
            this.preventDupProductVisit = preventDupProductVisit;
            return this;
        }
        
        
        /**
         * Convenience method that sets all the other preventDup* booleans to true or false;
         * if true, attempt to prevent all category and product duplicates.
         */
        public TraversalConfig setPreventDupAll(boolean preventDupAll) {
            setPreventDupCategoryVisit(preventDupAll);
            setPreventDupCategoryTraversal(preventDupAll);
            setPreventDupProductVisit(preventDupAll);
            return this;
        }

        /**
         * Returns true if any of the following are set: 
         * isPreventDupCategoryVisit, isPreventDupCategoryTraversal.
         */
        public boolean isPreventDupCategoryAny() {
            return preventDupCategoryAny;
        }
        
        /**
         * Returns true if any of the following are set: 
         * isPreventDupProductVisit.
         */
        public boolean isPreventDupProductAny() {
            return preventDupProductVisit;
        }
    }
    
    /**
     * Factory method, subclasses must override if they extend TraversalConfig.
     */
    public TraversalConfig newTravConfig() {
        return new TraversalConfig();
    }
    
    /**
     * Gets traversal config.
     * NOTE: subclass can override for return type.
     */
    public TraversalConfig getTravConfig() {
        return travConfig;
    }

    public void setTravConfig(TraversalConfig travConfig) {
        // hackish type check, but didn't want massive generics for now
        if (!this.travConfig.getClass().isAssignableFrom(travConfig.getClass())) {
            throw new IllegalArgumentException("tried to assign traversal config of wrong" 
                    +" instance type; must be " + this.travConfig.getClass().getName() + " or subtype");
        }
        this.travConfig = travConfig;
    }

    /**
     * Resets all stateful fields for a new iteration.
     * <p>
     * NOTE: it is the subclasses and client code that decides when this gets called.
     */
    public void reset() throws GeneralException {
        resetDupVisitRecords();
    }
    
    public void resetDupVisitRecords() {
        this.seenCategoryIds = (travConfig.isPreventDupCategoryAny()) ? new HashSet<String>() : null;
        this.seenProductIds = (travConfig.isPreventDupProductAny()) ? new HashSet<String>() : null;
    }
    
    public CatalogVisitor getVisitor() {
        return visitor;
    }
    
    public Delegator getDelegator() {
        return delegator;
    }
    
    public LocalDispatcher getDispatcher() {
        return dispatcher;
    }
    
    public boolean isUseCache() {
        return travConfig.isUseCache();
    }
    
    protected boolean isDoCategory(GenericValue productCategory) {
        return travConfig.isDoCategory();
    }
    
    protected boolean isDoProducts(GenericValue productCategory) {
        return travConfig.isDoProduct();
    }
 
    /**
     * Gives additional info about the category, product, and traversal to the catalog visitor.
     * <p>
     * WARN: This are edited in-place by the traverser, so if the visitor needs to store these
     * values, it must create copies for itself, before returning from the visiting methods
     * ({@link CatalogVisitor#visitCategory} and others).
     */
    public class TraversalState {
        protected List<GenericValue> trailCategories;
        protected int physicalDepth;
        
        /**
         * Main constructor.
         * See {@link CatalogTraverser#newTraversalState} methods for initial values given.
         */
        public TraversalState(List<GenericValue> trailCategories, int physicalDepth) {
            this.trailCategories = trailCategories;
            this.physicalDepth = physicalDepth;
        }
        
        /**
         * Copy constructor.
         */
        public TraversalState(TraversalState other, boolean deepCopy) {
            if (deepCopy) {
                // NOTE: don't deep-copy the GenericValue
                this.trailCategories = other.getTrailCategoriesCopyForTraversal(); 
            } else {
                this.trailCategories = other.trailCategories;
            }
            this.physicalDepth = other.physicalDepth;
        }

        /**
         * Copy/clone method. 
         * Child classes must override.
         */
        public TraversalState copy(boolean deepCopy) {
            return new TraversalState(this, deepCopy);
        }
        
        /**
         * List of ProductCategory values indicating the current category path being visited, 
         * not including the visited category (or product - obviously) itself.
         * WARN: after 
         */
        public List<GenericValue> getTrailCategories() {
            return trailCategories;
        }
        
        /**
         * Same as {@link #getTrailCategories()} but returns a list copy optimized for traversal.
         */
        public List<GenericValue> getTrailCategoriesCopyForTraversal() {
            return newCategoryTrailList(trailCategories);
        }

        /**
         * Same as {@link #getTrailCategories()} but returns a list copy for general purposes.
         */
        public List<GenericValue> getTrailCategoriesCopy() {
            return new ArrayList<>(trailCategories);
        }

        public int getPhysicalDepth() {
            return physicalDepth;
        }
        
        /**
         * Increase depth by the given category, in-place.
         */
        protected void pushCategory(GenericValue productCategory) {
            getTrailCategories().add(productCategory);
            physicalDepth++;
        }
        
        /**
         * Decrease depth by the given category, in-place.
         */
        protected void popCategory(GenericValue productCategory) {
            getTrailCategories().remove(getTrailCategories().size() - 1);
            physicalDepth--;
        }
        
        protected void setTrailCategories(List<GenericValue> trailCategories) {
            this.trailCategories = trailCategories;
        }

        protected void setPhysicalDepth(int physicalDepth) {
            this.physicalDepth = physicalDepth;
        }
    }
    
    protected final TraversalState newTraversalState(int physicalDepth) {
        return newTraversalState(this.<GenericValue>newCategoryTrailList(), physicalDepth);
    }
    
    protected final TraversalState newTraversalState() {
        return newTraversalState(this.<GenericValue>newCategoryTrailList(), 0);
    }
    
    /**
     * Primary TraversalState factory method - may be overridden to return subclass instance.
     */
    protected TraversalState newTraversalState(List<GenericValue> trailCategories, int physicalDepth) {
        return new TraversalState(trailCategories, physicalDepth);
    }
    
    public <T> List<T> newCategoryTrailList(Collection<? extends T> initialValues) {
        // NOTE: don't want to use new ArrayList<>(initialValues) constructor because loses the capacity
        List<T> list = newCategoryTrailList();
        if (initialValues != null && initialValues.size() > 0) {
            list.addAll(initialValues);
        }
        return list;
    }
    
    public <T> List<T> newCategoryTrailList() {
        return new ArrayList<T>(MAX_CATEGORY_DEPTH_PERF);
    }
    
    /**
     * Simply calls the {@link CatalogVisitor#visitCategory} and {@link CatalogVisitor#visitProduct} method for every single
     * product in the system, bypassing catalog/category rollup, for simplistic operations only.
     */
    public boolean traverseAllInSystem() throws GeneralException {
        if (travConfig.isDoCategory()) {
            boolean cnt = traverseAllCategoriesInSystem();
            if (!cnt) return cnt;
        }
        if (travConfig.isDoProduct()) {
            boolean cnt = traverseAllProductsInSystem();
            if (!cnt) return cnt;
        }
        return true;
    }
    
    /**
     * Simply calls the {@link CatalogVisitor#visitCategory} method for every single
     * product in the system, bypassing catalog/category rollup, for simplistic operations only.
     */
    public boolean traverseAllCategoriesInSystem() throws GeneralException {
        TraversalState state = newTraversalState();
        EntityListIterator productCategoryIt = null;
        try {
            productCategoryIt = EntityQuery.use(delegator).from("ProductCategory").cache(isUseCache()).queryIterator();
            GenericValue productCategory;
            while ((productCategory = productCategoryIt.next()) != null) {
                visitor.visitCategory(productCategory, state);
            }
            return true;
        } catch(StopCatalogTraversalException e) {
            ; // not an error - just stop
            return false;
        } finally {
            if (productCategoryIt != null) {
                try {
                    productCategoryIt.close();
                } catch(Throwable t) {
                    Debug.logError(t, module);
                }
            }
        }
    }

    /**
     * Simply calls the {@link CatalogVisitor#visitProduct} method for every single
     * product in the system, bypassing catalog/category rollup, for simplistic operations only.
     */
    public boolean traverseAllProductsInSystem() throws GeneralException {
        TraversalState state = newTraversalState();
        EntityListIterator productIt = null;
        try {
            productIt = EntityQuery.use(delegator).from("Product").cache(isUseCache()).queryIterator();
            GenericValue product;
            while ((product = productIt.next()) != null) {
                // NOTE: doChildProducts = false, because already counted in our global query here
                visitor.visitProduct(product, state);
            }
            return true;
        } catch(StopCatalogTraversalException e) {
            ; // not an error - just stop
            return false;
        } finally {
            if (productIt != null) {
                try {
                    productIt.close();
                } catch(Throwable t) {
                    Debug.logError(t, module);
                }
            }
        }
    }
    
    /**
     * Traverse ProductStore categories using depth-first search algorithm.
     * Usually categoryAssocList should be ProdCatalogCategory, but supports starting in middle
     */
    public boolean traverseProductStoreDfs(GenericValue productStore) throws GeneralException {
        List<GenericValue> prodCatalogList = queryProductStoreCatalogList(productStore);
        try {
            for(GenericValue prodCatalog : prodCatalogList) {
                List<GenericValue> prodCatalogCategoryList = queryProdCatalogCategoryList(prodCatalog);
                traverseCategoriesDepthFirstImpl(prodCatalogCategoryList, CategoryRefType.CATALOG_ASSOC.getResolver(), newTraversalState());
            }
            return true;
        } catch(StopCatalogTraversalException e) {
            ; // not an error - just stop
            return false;
        }
    }
    
    /**
     * Traverse ProdCatalog categories using depth-first search algorithm.
     */
    public boolean traverseCatalogsDepthFirst(List<GenericValue> prodCatalogList) throws GeneralException {
        try {
            for(GenericValue prodCatalog : prodCatalogList) {
                List<GenericValue> prodCatalogCategoryList = queryProdCatalogCategoryList(prodCatalog);
                traverseCategoriesDepthFirstImpl(prodCatalogCategoryList, CategoryRefType.CATALOG_ASSOC.getResolver(), newTraversalState());
            }
            return true;
        } catch(StopCatalogTraversalException e) {
            ; // not an error - just stop
            return false;
        }
    }
    
    /**
     * Traverse categories using depth-first search algorithm.
     * @param categoryAssocList list of ProdCatalogCategory, ProductCategoryRollup, ProductCategory, or a view-entity derived from these
     * @return true if went through entire category tree; false if stopped prematurely at some point.
     */
    public boolean traverseCategoriesDepthFirst(List<GenericValue> categoryOrAssocList) throws GeneralException {
        return traverseCategoriesDepthFirst(categoryOrAssocList, null);
    }
    
    /**
     * Traverse categories using depth-first search algorithm.
     * @param categoryAssocList list of ProdCatalogCategory, ProductCategoryRollup, ProductCategory, or a view-entity derived from these
     * @param categoryDepth if non-null, overrides the physical category depth the algorithm should assume; 
     *                      [TODO: NOT IMPLEMENTED] if null, determines the depth automatically (extra step);
     *                      if -1, don't calculate or use physicalDepth at all (optimize out)
     * @return true if went through entire category tree; false if stopped prematurely at some point.
     */
    public boolean traverseCategoriesDepthFirst(List<GenericValue> categoryOrAssocList, Integer physicalDepth) throws GeneralException {
        if (UtilValidate.isEmpty(categoryOrAssocList)) return true;
        GenericValue firstCategoryAssoc = categoryOrAssocList.get(0);
        CategoryRefType categoryRefType = CategoryRefType.fromEntity(firstCategoryAssoc);
        if (categoryRefType == null) throw new GenericEntityException("entity value cannot be mapped to a CategoryRefType");
        if (physicalDepth == null) {
            if (categoryRefType.isAlwaysPhysicalDepthZero()) {
                physicalDepth = 0;
            } else {
                Debug.logWarning(getLogMsgPrefix()+"traverseCategoriesDepthFirst: We are beginning with non-catalog-category (assoc) list"
                        + " but caller failed to specify initial physicalDepth - caller should pass explicit or -1 (using -1). First category in list: " + firstCategoryAssoc, module);
                physicalDepth = -1;
            }
        }
        try {
            traverseCategoriesDepthFirstImpl(categoryOrAssocList, categoryRefType.getResolver(), newTraversalState());
            return true;
        } catch(StopCatalogTraversalException e) {
            ; // not an error - just stop
            return false;
        }
    }
    
    /**
     * Core DFS category traversal algorithm.
     * <p>
     * DEV NOTE: The entries in categoryAssocList must all already be in the entity type matching
     * the listEntryResolver, done by caller, for optimization.
     */
    protected void traverseCategoriesDepthFirstImpl(List<GenericValue> categoryAssocList, CategoryRefType.Resolver categoryAssocResolver, TraversalState state) throws GeneralException {
        for (GenericValue categoryAssoc : categoryAssocList) {
            String productCategoryId = categoryAssoc.getString("productCategoryId");
            // duplicate category traversal and/or visit prevention, if enabled
            boolean categorySeen = false;
            if (travConfig.isPreventDupCategoryAny()) {
                // NOTE: productCategoryId is practically always this field name no matter the entity
                categorySeen = this.seenCategoryIds.contains(productCategoryId);
            }

            if ((travConfig.isPreventDupCategoryTraversal() && categorySeen) || !isApplicableCategoryAssoc(categoryAssoc)) {
                continue;
            }
            
            GenericValue productCategory = categoryAssocResolver.getProductCategoryStrict(categoryAssoc, isUseCache());
            if (productCategory == null) {
                Debug.logError(getLogMsgPrefix()+"Error: Could not get related ProductCategory for: " + categoryAssoc, module);
                continue;
            }
            
            // visit category itself (before recursive call)
            if (isDoCategory(productCategory) && !(travConfig.isPreventDupCategoryVisit() && categorySeen)) {
                visitor.visitCategory(productCategory, state);
                if (travConfig.isPreventDupCategoryAny()) {
                    this.seenCategoryIds.add(productCategoryId);
                }
            }
            
            // push category
            visitor.pushCategory(productCategory, state);
            state.pushCategory(productCategory);
            
            // visit products (before recursive call)
            queryAndVisitCategoryProducts(productCategory, state);
            
            // recurse into sub categories
            List<GenericValue> childProductCategoryRollups = queryCategorySubCategoryList(productCategory);
            if (childProductCategoryRollups.size() > 0) {
                traverseCategoriesDepthFirstImpl(childProductCategoryRollups, CategoryRefType.CATEGORY_ASSOC.getResolver(), state);
            }
            
            // pop category
            state.popCategory(productCategory);
            visitor.popCategory(productCategory, state);
        }
    }
    
    protected void registerSeenCategoryIds(Collection<String> categoryIds) {
        if (this.seenCategoryIds == null) return;
        this.seenCategoryIds.addAll(categoryIds);
    }
    
    protected void registerSeenCategoryId(String categoryId) {
        if (this.seenCategoryIds == null) return;
        this.seenCategoryIds.add(categoryId);
    }

    protected Set<String> getSeenCategoryIds() {
        return seenCategoryIds;
    }
    
    protected void registerSeenProductIds(Collection<String> productIds) {
        if (this.seenProductIds == null) return;
        this.seenProductIds.addAll(productIds);
    }
    
    protected void registerSeenProductId(String productId) {
        if (this.seenProductIds == null) return;
        this.seenProductIds.add(productId);
    }

    protected Set<String> getSeenProductIds() {
        return seenProductIds;
    }

    protected void queryAndVisitCategoryProducts(GenericValue productCategory, TraversalState state) throws GeneralException {
        // products
        if (isDoProducts(productCategory)) {
            List<GenericValue> productCategoryMembers = queryCategoryProductList(productCategory);
            if (UtilValidate.isNotEmpty(productCategoryMembers)) {
                for (GenericValue productCategoryMember : productCategoryMembers) {
                    String productId = productCategoryMember.getString("productId");
                    // duplicate product visit prevention, if enabled
                    if (travConfig.isPreventDupProductVisit()) {
                        if (this.seenProductIds.contains(productId)) {
                            continue;
                        }
                    }
                    
                    GenericValue product = productCategoryMember.getRelatedOne("Product", isUseCache());
                    // product is always one level down
                    visitor.visitProduct(product, state);
                    
                    if (travConfig.isPreventDupProductVisit()) {
                        this.seenProductIds.add(productId);
                    }
                }
            }
        }
    }
    
    
    // GENERAL QUERIES (overridable)

    public List<GenericValue> queryProductStoreCatalogList(GenericValue productStore) throws GenericEntityException {
        return filterProductStoreCatalogList(EntityQuery.use(delegator).from("ProductStoreCatalog")
                .where(makeProductStoreCatalogCond(productStore.getString("productStoreId")))
                .filterByDate(travConfig.isFilterByDate(), travConfig.getMoment()).orderBy("sequenceNum").cache(isUseCache()).queryList());
    }
    
    
    public EntityCondition makeProductStoreCatalogCond(String productStoreId) {
        return EntityCondition.makeCondition("productStoreId", productStoreId);
    }

    /**
     * Can be overridden to filter ProductStoreCatalogs after store query.
     */
    public List<GenericValue> filterProductStoreCatalogList(List<GenericValue> productStoreCatalogList) {
        return productStoreCatalogList;
    }

    /**
     * NOTE: prodCatalog may be ProdCatalog or ProductStoreCatalog (just read prodCatalogId).
     */
    public List<GenericValue> queryProdCatalogCategoryList(GenericValue prodCatalog) throws GenericEntityException {
        return filterProdCatalogCategoryList(EntityQuery.use(delegator).from("ProdCatalogCategory")
                .where(makeProdCatalogCategoryCond(prodCatalog.getString("prodCatalogId")))
                .filterByDate(travConfig.isFilterByDate(), travConfig.getMoment()).orderBy("sequenceNum").cache(isUseCache()).queryList());
    }
    
    
    public EntityCondition makeProdCatalogCategoryCond(String prodCatalogId) {
        return EntityCondition.makeCondition("prodCatalogId", prodCatalogId);
    }
    
    /**
     * Can be overridden to filter ProdCatalogCategory after store query.
     */
    public List<GenericValue> filterProdCatalogCategoryList(List<GenericValue> prodCatalogCategoryList) {
        return prodCatalogCategoryList;
    }
    
    /**
     * Can be used to filter ProdCatalogCategory and ProductCategoryRollup during iteration.
     */
    public boolean isApplicableCategoryAssoc(GenericValue categoryOrAssoc) {
        return true;
    }

    public List<GenericValue> queryCategorySubCategoryList(GenericValue productCategory) throws GenericEntityException {
        return EntityQuery.use(getDelegator()).from("ProductCategoryRollup")
                .where("parentProductCategoryId", productCategory.getString("productCategoryId")).filterByDate(travConfig.isFilterByDate(), travConfig.getMoment())
                .orderBy("sequenceNum").cache(isUseCache()).queryList();
    }
    
    public List<GenericValue> queryCategoryProductList(GenericValue productCategory) throws GenericEntityException {
        return EntityQuery.use(getDelegator()).from("ProductCategoryMember")
                .where("productCategoryId", productCategory.getString("productCategoryId")).filterByDate(travConfig.isFilterByDate(), travConfig.getMoment())
                .orderBy("sequenceNum").cache(isUseCache()).queryList();
    }
    
    
    /**
     * A simple helper that gets a list of either ProdCatalog or ProductStoreCatalog based on which
     * method arguments are empty or not.
     * <p>
     * Explicit prodCatalogId[List] gets priority over all.
     * <p>
     * Result can be passed to {@link #traverseCatalogsDepthFirst(List)}.
     * <p>
     * @param returnProdCatalogEntityOnly if false, may return ProductStoreCatalog instead of ProdCatalog
     */
    public List<GenericValue> getTargetCatalogList(String prodCatalogId, Collection<String> prodCatalogIdList, 
            String productStoreId, String webSiteId, boolean returnProdCatalogEntityOnly) throws GeneralException {
        if (UtilValidate.isNotEmpty(prodCatalogId) || UtilValidate.isNotEmpty(prodCatalogIdList)) {
            List<GenericValue> prodCatalogList = new LinkedList<>();
            if (UtilValidate.isNotEmpty(prodCatalogId)) {
                GenericValue prodCatalog = delegator.findOne("ProdCatalog", UtilMisc.toMap("prodCatalogId", prodCatalogId), isUseCache());
                if (prodCatalog == null) throw new GeneralException("catalog '" + prodCatalogId + " not found");
                prodCatalogList.add(prodCatalog);
            }
            if (UtilValidate.isNotEmpty(prodCatalogIdList)) {
                for(String catId : prodCatalogIdList) {
                    GenericValue prodCatalog = delegator.findOne("ProdCatalog", UtilMisc.toMap("prodCatalogId", catId), isUseCache());
                    if (prodCatalog == null) throw new GeneralException("catalog '" + catId + " not found");
                    prodCatalogList.add(prodCatalog);
                }
            }
            return prodCatalogList;
        } else {
            if (UtilValidate.isEmpty(productStoreId)) {
                if (UtilValidate.isEmpty(webSiteId)) {
                    throw new GeneralException("missing webSiteId, productStoreId or prodCatalogId");
                }
                GenericValue webSite = delegator.findOne("WebSite", UtilMisc.toMap("webSiteId", webSiteId), isUseCache());
                if (webSite == null) throw new GeneralException("website '" + webSiteId + "' not found");
                productStoreId = webSite.getString("productStoreId");
                if (UtilValidate.isEmpty(productStoreId)) throw new GeneralException("website '" + webSiteId + "' has no product store");
            }
            GenericValue productStore = delegator.findOne("ProductStore", UtilMisc.toMap("productStoreId", productStoreId), isUseCache());
            if (productStore == null) throw new GeneralException("product store '" + productStoreId + " not found");
            
            List<GenericValue> productStoreCatalogList = EntityQuery.use(delegator).from("ProductStoreCatalog").where("productStoreId", productStoreId)
                    .filterByDate().orderBy("sequenceNum").cache(isUseCache()).queryList();
            
            if (returnProdCatalogEntityOnly && productStoreCatalogList.size() > 0) {
                List<GenericValue> prodCatalogList = new LinkedList<>();
                for(GenericValue productStoreCatalog : productStoreCatalogList) {
                    GenericValue prodCatalog = delegator.findOne("ProdCatalog", UtilMisc.toMap("prodCatalogId", productStoreCatalog.get("prodCatalogId")), isUseCache());
                    if (prodCatalog == null) throw new GenericEntityException("schema error: catalog '" + productStoreCatalog.get("prodCatalogId") + " not found");
                    prodCatalogList.add(prodCatalog);
                }
                return prodCatalogList;
            } else {
                return productStoreCatalogList;
            }
        }
    }

    protected String getLogMsgPrefix() {
        return logPrefix;
    }
    
    protected String getLogErrorPrefix() {
        return logPrefix+"Error traversing categories: ";
    }
    
    protected String getLogErrorMsg(Throwable t) {
        return getLogErrorPrefix() + t.getMessage();
    }
}
