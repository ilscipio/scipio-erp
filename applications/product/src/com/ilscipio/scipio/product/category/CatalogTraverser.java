package com.ilscipio.scipio.product.category;

import java.io.Closeable;
import java.sql.Timestamp;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashSet;
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
import org.ofbiz.entity.util.EntityUtil;
import org.ofbiz.product.catalog.CatalogWorker;
import org.ofbiz.product.category.CategoryWorker;
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

    // These are the "last" webSiteId and productStoreId queried and are updated anywhere a store or website is queried - can also be set manually by caller
    // NOTE: some of these IDs are set to empty string internally to indicate "not found"
    protected String webSiteId = null;
    protected GenericValue webSite = null;
    protected String productStoreId = null; // if none, this is set to empty string internally
    protected GenericValue productStore = null;
    protected String prodCatalogId = null;
    protected GenericValue prodCatalog = null;
    protected String viewAllowCategoryId = null; // if none, this is set to empty string internally
    protected GenericValue viewAllowCategory = null;

    // NOTE: There can be modified by the traverser instance or anyone; if overriding configuration, be careful to preserve it as appropriate using try/finally
    protected boolean doCategory;
    protected boolean doProduct;

    // DEV NOTE: If adding fields, beware of copy constructor below, needed for extension

    /**
     * Composed visitor constructor, with explicit visitor.
     */
    public CatalogTraverser(CatalogVisitor visitor, Delegator delegator, LocalDispatcher dispatcher, TraversalConfig travConfig) {
        this.visitor = (visitor != null) ? visitor : this;
        this.delegator = delegator;
        this.dispatcher = dispatcher;
        this.travConfig = (travConfig != null) ? travConfig : newTravConfig();
        this.doCategory = travConfig.isDoCategory();
        this.doProduct = travConfig.isDoProduct();
    }

    /**
     * Abstract instance constructor, with the traverser itself as the visitor.
     */
    public CatalogTraverser(Delegator delegator, LocalDispatcher dispatcher, TraversalConfig travConfig) {
        this(null, delegator, dispatcher, travConfig);
    }

    /**
     * Copy constructor.
     */
    public CatalogTraverser(CatalogTraverser other) {
        this.visitor = other.visitor;
        this.delegator = other.delegator;
        this.dispatcher = other.dispatcher;
        this.travConfig = other.travConfig;
        this.seenCategoryIds = (other.seenCategoryIds != null) ? new HashSet<>(other.seenCategoryIds) : null;
        this.seenProductIds = (other.seenProductIds != null) ? new HashSet<>(other.seenProductIds) : null;
        this.webSiteId = other.webSiteId;
        this.webSite = other.webSite;
        this.productStoreId = other.productStoreId;
        this.productStore = other.productStore;
        this.prodCatalogId = other.prodCatalogId;
        this.prodCatalog = other.prodCatalog;
        this.viewAllowCategoryId = other.viewAllowCategoryId;
        this.viewAllowCategory = other.viewAllowCategory;
        this.doCategory = other.doCategory;
        this.doProduct = other.doProduct;
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

        private List<CatalogFilter> filters = new ArrayList<>();

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


        public List<CatalogFilter> getFilters() {
            return filters;
        }

        public TraversalConfig addFilters(Collection<CatalogFilter> filters) {
            if (filters == null) { return this; }
            this.filters.addAll(filters);
            return this;
        }

        public TraversalConfig addFilter(CatalogFilter filter) {
            if (filter == null) { return this; }
            this.filters.add(filter);
            return this;
        }

        public TraversalConfig removeFilters(Collection<CatalogFilter> filters) {
            if (filters == null) { return this; }
            this.filters.removeAll(filters);
            return this;
        }

        public TraversalConfig removeFilter(CatalogFilter filter) {
            if (filter == null) { return this; }
            this.filters.remove(filter);
            return this;
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
        return isDoCategory();
    }

    protected boolean isDoCategory() {
        return doCategory;
    }

    /**
     * Sets the current state of whether to lookup categories.
     * WARN: The initial value is provided by the traversal config and this overrides it; use try/finally to restore if important.
     */
    public void setDoCategory(boolean doCategory) {
        this.doCategory = doCategory;
    }

    protected boolean isDoProduct(GenericValue productCategory) {
        return isDoProduct();
    }

    public boolean isDoProduct() {
        return doProduct;
    }

    /**
     * Sets the current state of whether to lookup products.
     * WARN: The initial value is provided by the traversal config and this overrides it; use try/finally to restore if important.
     */
    public void setDoProduct(boolean doProduct) {
        this.doProduct = doProduct;
    }

    public DoStateHandler doCategoryOnlySection() {
        DoStateHandler handler = getDoStateHandler();
        setDoProduct(false);
        return handler;
    }

    public DoStateHandler doProductOnlySection() {
        DoStateHandler handler = getDoStateHandler();
        setDoCategory(false);
        return handler;
    }

    protected DoStateHandler getDoStateHandler() { return new DoStateHandler(doCategory, doProduct); }

    /**
     * This can be used in a try-with-resources block around the traverse* calls to produce only products or only categories.
     */
    public class DoStateHandler implements Closeable {
        private boolean doCategoryPrev;
        private boolean doProductPrev;

        protected DoStateHandler(boolean doCategoryPrev, boolean doProductPrev) {
            this.doCategoryPrev = doCategoryPrev;
            this.doProductPrev = doProductPrev;
        }

        @Override
        public void close() {
            setDoCategory(doCategoryPrev);
            setDoProduct(doProductPrev);
        }
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

        public CatalogTraverser getTarverser() {
            return CatalogTraverser.this;
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

        /**
         * WARNING: May be null depending on caller's setup and usage.
         */
        public String getWebSiteId() {
            return CatalogTraverser.this.getWebSiteId();
        }

        /**
         * WARNING: May be null depending on caller's setup and usage.
         */
        public GenericValue getWebSite() {
            return CatalogTraverser.this.getWebSite();
        }

        /**
         * WARNING: May be null depending on caller's setup and usage.
         */
        public String getProductStoreId() {
            return CatalogTraverser.this.getProductStoreId();
        }

        /**
         * WARNING: May be null depending on caller's setup and usage.
         */
        public GenericValue getProductStore() {
            return CatalogTraverser.this.getProductStore();
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
        return new ArrayList<>(MAX_CATEGORY_DEPTH_PERF);
    }

    public boolean useProduct(GenericValue product, CatalogTraverser.TraversalState state) throws GeneralException {
        for(CatalogFilter filter : getTravConfig().getFilters()) {
            if (!filter.filterProduct(product, state)) {
                notifyProductFiltered(product, state);
                return false;
            }
        }
        return true;
    }

    protected void notifyProductFiltered(GenericValue product, CatalogTraverser.TraversalState state) throws GeneralException {
    }

    public boolean useCategory(GenericValue productCategory, CatalogTraverser.TraversalState state) throws GeneralException {
        for(CatalogFilter filter : getTravConfig().getFilters()) {
            if (!filter.filterCategory(productCategory, state)) {
                notifyCategoryFiltered(productCategory, state);
                return false;
            }
        }
        return true;
    }

    protected void notifyCategoryFiltered(GenericValue product, CatalogTraverser.TraversalState state) throws GeneralException {
    }

    /*
     * Current store/website/catalog setters
     * It is up to the caller to set as appropriate.
     */

    /**
     * Initialize the current product store. NOTE: This is not automatically done by the traversal methods below.
     */
    public void setProductStoreAndWebSite(String productStoreId, String webSiteId) {
        setWebSite(webSiteId);
        setProductStore(productStoreId);
    }

    public void setWebSite(String webSiteId) {
        this.webSiteId = webSiteId;
        this.webSite = null;
    }

    public void setWebSite(GenericValue webSite) {
        this.webSite = webSite;
        this.webSiteId = (webSite != null) ? webSite.getString("webSiteId") : null;
    }

    public void setProductStore(String productStoreId) {
        this.productStoreId = productStoreId;
        this.productStore = null;
    }

    public void setProductStore(GenericValue productStore) {
        this.productStore = productStore;
        this.productStoreId = (productStore != null) ? productStore.getString("productStoreId") : null;
    }

    public void setProdCatalog(String prodCatalogId) {
        this.prodCatalogId = prodCatalogId;
        this.prodCatalog = null;
        setViewAllowCategoryId(null); // reset
    }

    public void setProdCatalog(GenericValue prodCatalog) {
        this.prodCatalog = prodCatalog;
        this.prodCatalogId = (prodCatalog != null) ? prodCatalog.getString("prodCatalogId") : null;
        setViewAllowCategoryId(null); // reset
    }

    /*
     * Current store/website/catalog getters
     */

    /**
     * WARNING: May be null depending on caller's setup and usage.
     */
    public String getWebSiteId() {
        return webSiteId;
    }

    /**
     * WARNING: May be null depending on caller's setup and usage.
     */
    public GenericValue getWebSite() {
        if (webSite == null && UtilValidate.isNotEmpty(webSiteId)) {
            webSite = getDelegator().from("WebSite").where("webSiteId", webSiteId).queryOneSafe();
        }
        return webSite;
    }

    /**
     * WARNING: May be null depending on caller's setup and usage.
     */
    public String getProductStoreId() {
        String productStoreId = this.productStoreId;
        if (UtilValidate.isNotEmpty(productStoreId)) {
            return productStoreId;
        }
        if (productStoreId == null) {
            GenericValue webSite = getWebSite();
            if (webSite != null) {
                productStoreId = webSite.getString("productStoreId");
                if (productStoreId == null) {
                    productStoreId = ""; // none or could not determine
                }
            }
        }
        return UtilValidate.isNotEmpty(productStoreId) ? productStoreId : null;
    }

    /**
     * WARNING: May be null depending on caller's setup and usage.
     */
    public GenericValue getProductStore() {
        GenericValue productStore = this.productStore;
        if (productStore == null) {
            String productStoreId = getProductStoreId();
            if (productStoreId != null) {
                productStore = getDelegator().from("ProductStore").where("productStoreId", productStoreId).cache(isUseCache()).queryOneSafe();
            }
        }
        return productStore;
    }

    /**
     * WARNING: May be null depending on caller's setup and usage.
     */
    public String getProdCatalogId() {
        return prodCatalogId;
    }

    /**
     * WARNING: May be null depending on caller's setup and usage.
     */
    public GenericValue getProdCatalog() {
        if (prodCatalog == null && UtilValidate.isNotEmpty(prodCatalogId)) {
            prodCatalog = getDelegator().from("ProdCatalog").where("prodCatalogId", prodCatalogId).cache(isUseCache()).queryOneSafe();
        }
        return prodCatalog;
    }

    /**
     * This may be needed by callers/implementers for traverse* methods that don't set a catalog themselves.
     */
    public void setProdCatalogFromProductStore() {
        String productStoreId = getProductStoreId();
        String prodCatalogId = null;
        if (productStoreId != null) {
            GenericValue productStoreCatalog = null;
            try {
                productStoreCatalog = queryProductStoreCatalogMain(productStoreId);
            } catch (GenericEntityException e) {
                Debug.logError(e, module);
            }
            if (productStoreCatalog != null) {
                prodCatalogId = productStoreCatalog.getString("prodCatalogId");
            }
        }
        setProdCatalog(prodCatalogId);
    }

    /*
     * Traversal methods
     * NOTE: Generally caller is responsible to set the appropriate ProductStore/WebSite/ProdCatalog, except
     * for cases where the traversal iterates the catalogs (so it must set them).
     */

    /**
     * Simply calls the {@link CatalogVisitor#visitCategory} and {@link CatalogVisitor#visitProduct} method for every single
     * product in the system, bypassing catalog/category rollup; for crude operations.
     */
    public boolean traverseAllCategoriesAndProductsInSystem() throws GeneralException {
        if (isDoCategory()) {
            boolean cnt = traverseAllCategoriesInSystem();
            if (!cnt) return cnt;
        }
        if (isDoProduct()) {
            boolean cnt = traverseAllProductsInSystem();
            if (!cnt) return cnt;
        }
        return true;
    }

    /**
     * Simply calls the {@link CatalogVisitor#visitCategory} method for every single
     * product in the system, bypassing catalog/category rollup; for crude operations.
     */
    public boolean traverseAllCategoriesInSystem() throws GeneralException {
        TraversalState state = newTraversalState();
        try (EntityListIterator productCategoryIt = delegator.from("ProductCategory").cache(isUseCache()).queryIterator()) {
            GenericValue productCategory;
            while ((productCategory = productCategoryIt.next()) != null) {
                if (useCategory(productCategory, state)) {
                    visitor.visitCategory(productCategory, state);
                }
            }
            return true;
        } catch(StopCatalogTraversalException e) {
            ; // not an error - just stop
            return false;
        }
    }

    /**
     * Simply calls the {@link CatalogVisitor#visitProduct} method for every single
     * product in the system, bypassing catalog/category rollup; for crude operations.
     */
    public boolean traverseAllProductsInSystem() throws GeneralException {
        TraversalState state = newTraversalState();
        try (EntityListIterator productIt = delegator.from("Product").cache(isUseCache()).queryIterator()) {
            GenericValue product;
            while ((product = productIt.next()) != null) {
                // NOTE: doChildProducts = false, because already counted in our global query here
                if (useProduct(product, state)) {
                    visitor.visitProduct(product, state);
                }
            }
            return true;
        } catch(StopCatalogTraversalException e) {
            ; // not an error - just stop
            return false;
        }
    }

    /**
     * Traverse ProductStore categories using depth-first search algorithm using the current ProductStore (must already be set);
     * automatically sets the current ProdCatalog.
     * Usually categoryAssocList should be ProdCatalogCategory, but supports starting in middle
     */
    public boolean traverseStoreCatalogsDepthFirst() throws GeneralException {
        GenericValue productStore = getProductStore();
        if (productStore == null) {
            throw new IllegalStateException("ProductStore not set on CatalogTraverser");
        }
        List<GenericValue> prodCatalogList = queryProductStoreCatalogList(productStore);
        try {
            for(GenericValue prodCatalog : prodCatalogList) {
                setProdCatalog(prodCatalog);
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
     * Traverse ProdCatalog categories of the given product store and website using depth-first search algorithm;
     * convenience method that looks up the catalogs from the given IDs;
     * automatically sets the current ProdCatalog.
     * <p>
     * NOTE: 2019-10-03: This should be preferred over the overload that takes only prodCatalogList as this provides more information.
     */
    public boolean traverseCatalogsDepthFirst(String prodCatalogId, Collection<String> prodCatalogIdList, boolean returnProdCatalogEntityOnly) throws GeneralException {
        return traverseCatalogsDepthFirst(getTargetCatalogList(prodCatalogId, prodCatalogIdList, returnProdCatalogEntityOnly));
    }

    /**
     * Traverse ProdCatalog categories using depth-first search algorithm;
     * automatically sets the current ProdCatalog.
     * <p>
     * WARNING: This does not set the current product store or website, you may want another overload, otherwise you may have to set them manually
     * for filters to have access to them.
     */
    public boolean traverseCatalogsDepthFirst(List<GenericValue> prodCatalogList) throws GeneralException {
        try {
            for(GenericValue prodCatalog : prodCatalogList) {
                setProdCatalog(prodCatalog);
                List<GenericValue> prodCatalogCategoryList = queryProdCatalogCategoryList(prodCatalog);
                traverseCategoriesDepthFirstImpl(prodCatalogCategoryList, CategoryRefType.CATALOG_ASSOC.getResolver(), newTraversalState());
            }
            return true;
        } catch(StopCatalogTraversalException e) {
            return false; // NOTE: not an error - just stop
        }
    }

    /**
     * Traverse categories using depth-first search algorithm.
     * @param categoryOrAssocList list of ProdCatalogCategory, ProductCategoryRollup, ProductCategory, or a view-entity derived from these
     * @return true if went through entire category tree; false if stopped prematurely at some point.
     */
    public boolean traverseCategoriesDepthFirst(List<GenericValue> categoryOrAssocList) throws GeneralException {
        return traverseCategoriesDepthFirst(categoryOrAssocList, null);
    }

    /**
     * Traverse categories using depth-first search algorithm; does not set current store or website.
     * @param categoryOrAssocList list of ProdCatalogCategory, ProductCategoryRollup, ProductCategory, or a view-entity derived from these
     * @param physicalDepth if non-null, overrides the physical category depth the algorithm should assume;
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
                if (useCategory(productCategory, state)) {
                    visitor.visitCategory(productCategory, state);
                }
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

    /**
     * WARNING: May be null depending on caller's setup and usage.
     */
    public String getViewAllowCategoryId() {
        String viewAllowCategoryId = this.viewAllowCategoryId;
        if (UtilValidate.isNotEmpty(viewAllowCategoryId)) {
            return viewAllowCategoryId;
        }
        if (viewAllowCategoryId == null) {
            String prodCatalogId = getProdCatalogId();
            if (prodCatalogId != null) {
                viewAllowCategoryId = CatalogWorker.getCatalogViewAllowCategoryId(getDelegator(), prodCatalogId);
            }
            if (viewAllowCategoryId == null) {
                viewAllowCategoryId = ""; // found nothing
            }
            this.viewAllowCategoryId = viewAllowCategoryId;
        }
        return UtilValidate.isNotEmpty(viewAllowCategoryId) ? viewAllowCategoryId : null;
    }

    private void setViewAllowCategoryId(String viewAllowCategoryId) {
        this.viewAllowCategoryId = viewAllowCategoryId;
        this.viewAllowCategory = null;
    }

    /**
     * WARNING: May be null depending on caller's setup and usage.
     */
    public GenericValue getViewAllowCategory() {
        String viewAllowCategoryId = getViewAllowCategoryId();
        if (viewAllowCategory == null && viewAllowCategoryId != null) {
            viewAllowCategory = getDelegator().from("ProductCategory").where("productCategoryId", viewAllowCategoryId).queryOneSafe();
        }
        return viewAllowCategory;
    }

    private void setViewAllowCategory(GenericValue viewAllowCategory) {
        this.viewAllowCategory = viewAllowCategory;
        this.viewAllowCategoryId = (viewAllowCategory != null) ? viewAllowCategory.getString("productCategoryId") : null;
    }

    /**
     * Checks if the view-product is in the view-allow category for the current product.
     * NOTE: This is only a helper, not used in the abstract implementation! Subclasses and configurations must specify.
     */
    public boolean isViewAllowProduct(GenericValue product) {
        String viewAllowCategoryId = getViewAllowCategoryId();
        try {
            return (viewAllowCategoryId != null) ? isViewAllowProduct(product, viewAllowCategoryId) : true; // default true
        } catch (GenericEntityException e) {
            Debug.logError(e, module);
            return false; // return false here enough default true because otherwise errors could have security implications
        }
    }

    protected boolean isViewAllowProduct(GenericValue product, String viewAllowCategoryId) throws GenericEntityException { // May be overridden in case different behavior needed
        return CategoryWorker.isProductInCategory(getDelegator(), product.getString("productId"), viewAllowCategoryId);
    }

    protected void queryAndVisitCategoryProducts(GenericValue productCategory, TraversalState state) throws GeneralException {
        // products
        if (isDoProduct(productCategory)) {
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
                    if (useProduct(product, state)) {
                        visitor.visitProduct(product, state);
                    }

                    if (travConfig.isPreventDupProductVisit()) {
                        this.seenProductIds.add(productId);
                    }
                }
            }
        }
    }


    // GENERAL QUERIES (overridable)

    public List<GenericValue> queryProductStoreCatalogList(String productStoreId) throws GenericEntityException {
        return filterProductStoreCatalogList(delegator.from("ProductStoreCatalog")
                .where(makeProductStoreCatalogCond(productStoreId))
                .filterByDate(travConfig.isFilterByDate(), travConfig.getMoment()).orderBy("sequenceNum").cache(isUseCache()).queryList());
    }

    public final List<GenericValue> queryProductStoreCatalogList(GenericValue productStore) throws GenericEntityException {
        return queryProductStoreCatalogList(productStore.getString("productStoreId"));
    }

    public GenericValue queryProductStoreCatalogMain(String productStoreId) throws GenericEntityException {
        List<GenericValue> catalogs = queryProductStoreCatalogList(productStoreId);
        return EntityUtil.getFirst(catalogs);
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
        return filterProdCatalogCategoryList(delegator.from("ProdCatalogCategory")
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
    protected List<GenericValue> getTargetCatalogList(String prodCatalogId, Collection<String> prodCatalogIdList, boolean returnProdCatalogEntityOnly) throws GeneralException {
        if (UtilValidate.isNotEmpty(prodCatalogId) || UtilValidate.isNotEmpty(prodCatalogIdList)) {
            List<GenericValue> prodCatalogList = new ArrayList<>();
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
            String productStoreId = getProductStoreId();
            GenericValue productStore = delegator.findOne("ProductStore", UtilMisc.toMap("productStoreId", productStoreId), isUseCache());
            if (productStore == null) throw new GeneralException("product store '" + productStoreId + " not found");

            List<GenericValue> productStoreCatalogList = delegator.from("ProductStoreCatalog").where("productStoreId", productStoreId)
                    .filterByDate().orderBy("sequenceNum").cache(isUseCache()).queryList();

            if (returnProdCatalogEntityOnly && productStoreCatalogList.size() > 0) {
                List<GenericValue> prodCatalogList = new ArrayList<>();
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
