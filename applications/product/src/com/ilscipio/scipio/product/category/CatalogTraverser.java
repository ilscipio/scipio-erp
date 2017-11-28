package com.ilscipio.scipio.product.category;

import java.sql.Timestamp;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.GeneralException;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.GenericEntityException;
import org.ofbiz.entity.GenericValue;
import org.ofbiz.entity.condition.EntityCondition;
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
 * This base class is stateless - subclasses should decide if/how to manage state.
 * <p>
 * Class is NOT thread-safe or serializable - keep out of session attributes.
 * <p>
 * DEV NOTE: do not put specific or stateful functionality in this class.
 * I made a special {@link SeoCatalogTraverser} subclass for that, 
 * because this one is may be used in many places (not just SEO).
 * <p>
 * TODO: missing filterByDate options and moment to use.
 */
public class CatalogTraverser extends AbstractCatalogVisitor {

    public static final String module = CatalogTraverser.class.getName();
    
    /**
     * Expected max category depth (optimization), intentionally exaggerated (probably no one has categories
     * deeper than a dozen or so).
     */
    public static final int MAX_CATEGORY_DEPTH_PERF = 128;
    
    protected static final String logPrefix = ""; // "CategoryTraverser: "
    
    private final CatalogVisitor visitor;
    private final Delegator delegator;
    private final LocalDispatcher dispatcher;
    private final boolean useCache;
    
    private final boolean doCategory;
    private final boolean doProduct;
    private final boolean filterByDate;
    private final Timestamp moment;

    /**
     * Composed visitor constructor, with explicit visitor.
     */
    public CatalogTraverser(CatalogVisitor visitor, Delegator delegator, LocalDispatcher dispatcher,
            boolean useCache, boolean doCategory, boolean doProduct, boolean filterByDate, Timestamp moment) {
        this.visitor = (visitor != null) ? visitor : this;
        this.delegator = delegator;
        this.dispatcher = dispatcher;
        this.useCache = useCache;
        this.doCategory = doCategory;
        this.doProduct = doProduct;
        this.filterByDate = filterByDate;
        this.moment = moment;
    }
    
    /**
     * Composed visitor constructor, with explicit visitor.
     */
    public CatalogTraverser(CatalogVisitor visitor, Delegator delegator, LocalDispatcher dispatcher,
            boolean useCache) {
        this(visitor, delegator, dispatcher, useCache, true, true, true, null);
    }
    
    /**
     * Abstract instance constructor, with the traverser itself as the visitor.
     */
    public CatalogTraverser(Delegator delegator, LocalDispatcher dispatcher,
            boolean useCache, boolean doCategory, boolean doProduct, boolean filterByDate, Timestamp moment) {
        this(null, delegator, dispatcher, useCache, doCategory, doProduct, filterByDate, moment);
    }
    
    /**
     * Abstract instance constructor, with the traverser itself as the visitor.
     */
    public CatalogTraverser(Delegator delegator, LocalDispatcher dispatcher,
            boolean useCache) {
        this(null, delegator, dispatcher, useCache, true, true, true, null);
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
    
    protected <T> List<T> newCategoryTrailList(Collection<? extends T> initialValues) {
        // NOTE: don't want to use new ArrayList<>(initialValues) constructor because loses the capacity
        List<T> list = newCategoryTrailList();
        if (initialValues != null && initialValues.size() > 0) {
            list.addAll(initialValues);
        }
        return list;
    }
    
    protected <T> List<T> newCategoryTrailList() {
        return new ArrayList<T>(MAX_CATEGORY_DEPTH_PERF);
    }
    
    /**
     * Traverse ProductStore categories depth-first.
     * Usually categoryAssocList should be ProdCatalogCategory, but supports starting in middle
     */
    public boolean traverseProductStoreDepthFirst(GenericValue productStore) throws GeneralException {
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
     * Traverse ProdCatalog categories depth-first.
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
     * Traverse categories depth-first.
     * @param categoryAssocList list of ProdCatalogCategory, ProductCategoryRollup, ProductCategory, or a view-entity derived from these
     * @return true if went through entire category tree; false if stopped prematurely at some point.
     */
    public boolean traverseCategoriesDepthFirst(List<GenericValue> categoryOrAssocList) throws GeneralException {
        return traverseCategoriesDepthFirst(categoryOrAssocList, null);
    }
    
    /**
     * Traverse categories depth-first.
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
    
    protected void traverseCategoriesDepthFirstImpl(List<GenericValue> categoryAssocList, CategoryRefType.Resolver listEntryResolver, TraversalState state) throws GeneralException {
        for (GenericValue categoryAssoc : categoryAssocList) {
            if (!isApplicableCategoryAssoc(categoryAssoc)) {
                continue;
            }
            GenericValue productCategory = listEntryResolver.getProductCategoryStrict(categoryAssoc, isUseCache());
            if (productCategory == null) {
                Debug.logError(getLogMsgPrefix()+"Error: Could not get related ProductCategory for: " + categoryAssoc, module);
                continue;
            }
            
            // visit category itself (before recursive call)
            if (isDoCategory(productCategory)) {
                visitor.visitCategory(productCategory, state);
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
    
    protected void queryAndVisitCategoryProducts(GenericValue productCategory, TraversalState state) throws GeneralException {
        // products
        if (isDoProduct(productCategory)) {
            List<GenericValue> productCategoryMembers = queryCategoryProductList(productCategory);
            if (UtilValidate.isNotEmpty(productCategoryMembers)) {
                for (GenericValue productCategoryMember : productCategoryMembers) {
                    GenericValue product = productCategoryMember.getRelatedOne("Product", isUseCache());
                    // product is always one level down
                    visitor.visitProduct(product, state);
                }
            }
        }
    }
    
    protected boolean isDoCategory(GenericValue productCategory) {
        return isDoCategory();
    }

    protected boolean isDoProduct(GenericValue productCategory) {
        return isDoProduct();
    }
    
    // GENERAL QUERIES (overridable)

    protected List<GenericValue> queryProductStoreCatalogList(GenericValue productStore) throws GenericEntityException {
        return filterProductStoreCatalogList(EntityQuery.use(delegator).from("ProductStoreCatalog")
                .where(makeProductStoreCatalogCond(productStore.getString("productStoreId")))
                .filterByDate(isFilterByDate(), getMoment()).orderBy("sequenceNum").cache(isUseCache()).queryList());
    }
    
    
    protected EntityCondition makeProductStoreCatalogCond(String productStoreId) {
        return EntityCondition.makeCondition("productStoreId", productStoreId);
    }

    /**
     * Can be overridden to filter ProductStoreCatalogs after store query.
     */
    protected List<GenericValue> filterProductStoreCatalogList(List<GenericValue> productStoreCatalogList) {
        return productStoreCatalogList;
    }

    protected List<GenericValue> queryProdCatalogCategoryList(GenericValue prodCatalog) throws GenericEntityException {
        return filterProdCatalogCategoryList(EntityQuery.use(delegator).from("ProdCatalogCategory")
                .where(makeProdCatalogCategoryCond(prodCatalog.getString("prodCatalogId")))
                .filterByDate(isFilterByDate(), getMoment()).orderBy("sequenceNum").cache(isUseCache()).queryList());
    }
    
    
    protected EntityCondition makeProdCatalogCategoryCond(String prodCatalogId) {
        return EntityCondition.makeCondition("prodCatalogId", prodCatalogId);
    }
    
    /**
     * Can be overridden to filter ProdCatalogCategory after store query.
     */
    protected List<GenericValue> filterProdCatalogCategoryList(List<GenericValue> prodCatalogCategoryList) {
        return prodCatalogCategoryList;
    }
    
    /**
     * Can be used to filter ProdCatalogCategory and ProductCategoryRollup during iteration.
     */
    protected boolean isApplicableCategoryAssoc(GenericValue categoryOrAssoc) {
        return true;
    }

    protected List<GenericValue> queryCategorySubCategoryList(GenericValue productCategory) throws GenericEntityException {
        return EntityQuery.use(getDelegator()).from("ProductCategoryRollup")
                .where("parentProductCategoryId", productCategory.getString("productCategoryId")).filterByDate(isFilterByDate(), getMoment())
                .orderBy("sequenceNum").cache(isUseCache()).queryList();
    }
    
    protected List<GenericValue> queryCategoryProductList(GenericValue productCategory) throws GenericEntityException {
        return EntityQuery.use(getDelegator()).from("ProductCategoryMember")
                .where("productCategoryId", productCategory.getString("productCategoryId")).filterByDate(isFilterByDate(), getMoment())
                .orderBy("sequenceNum").cache(isUseCache()).queryList();
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
        return useCache;
    }

    public boolean isDoCategory() {
        return doCategory;
    }

    public boolean isDoProduct() {
        return doProduct;
    }

    public boolean isFilterByDate() {
        return filterByDate;
    }
    
    /**
     * NOTE: if null and {@link #isFilterByDate} is true, this means "use now timestamp".
     */
    public Timestamp getMoment() {
        return moment;
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
