package com.ilscipio.scipio.product.category;

import java.sql.Timestamp;
import java.util.ArrayList;
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
 * Everything is overridable, plus supports a separate CategoryVisitor for the 
 * very core calls only.
 * <p>
 * This base class is STATELESS - subclasses should decide if/how to manage state.
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
    
    public CatalogTraverser(Delegator delegator, LocalDispatcher dispatcher,
            boolean useCache) {
        this(null, delegator, dispatcher, useCache, true, true, true, null);
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
                List<GenericValue> trailCategories = newCategoryTrailList();
                traverseCategoriesDepthFirstImpl(prodCatalogCategoryList, CategoryRefType.CATALOG_ASSOC.getResolver(), trailCategories, 0);
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
                List<GenericValue> trailCategories = newCategoryTrailList();
                traverseCategoriesDepthFirstImpl(prodCatalogCategoryList, CategoryRefType.CATALOG_ASSOC.getResolver(), trailCategories, 0);
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
            List<GenericValue> trailCategories = newCategoryTrailList();
            traverseCategoriesDepthFirstImpl(categoryOrAssocList, categoryRefType.getResolver(), trailCategories, physicalDepth);
            return true;
        } catch(StopCatalogTraversalException e) {
            ; // not an error - just stop
            return false;
        }
    }
    
    protected void traverseCategoriesDepthFirstImpl(List<GenericValue> categoryAssocList, CategoryRefType.Resolver listEntryResolver, List<GenericValue> trailCategories, int physicalDepth) throws GeneralException {
        for (GenericValue categoryAssoc : categoryAssocList) {
            if (!isApplicableCategoryAssoc(categoryAssoc)) {
                continue;
            }
            GenericValue productCategory = listEntryResolver.getProductCategoryStrict(categoryAssoc, isUseCache());
            if (productCategory == null) {
                Debug.logError(getLogMsgPrefix()+"Error: Could not get related ProductCategory for: " + categoryAssoc, module);
                continue;
            }
            
            // self + push + products
            visitCategoryLocal(productCategory, trailCategories, physicalDepth);
            pushCategoryLocal(productCategory, trailCategories, physicalDepth);
            visitProductsLocal(productCategory, trailCategories, physicalDepth);
            
            // child cats (recursive)
            List<GenericValue> childProductCategoryRollups = queryCategorySubCategoryList(productCategory);
            if (UtilValidate.isNotEmpty(childProductCategoryRollups)) {
                traverseCategoriesDepthFirstImpl(childProductCategoryRollups, CategoryRefType.CATEGORY_ASSOC.getResolver(), trailCategories, physicalDepth + 1);
            }
            
            // pop
            popCategoryLocal(productCategory, trailCategories, physicalDepth);
        }
    }

    protected final void visitCategoryLocal(GenericValue productCategory, List<GenericValue> trailCategories, int physicalDepth) throws GeneralException {
        // self
        if (isDoCategory(productCategory)) {
            visitor.visitCategory(productCategory, trailCategories, physicalDepth);
        }
    }
    
    protected final void visitProductsLocal(GenericValue productCategory, List<GenericValue> trailCategories, int physicalDepth) throws GeneralException {
        // products
        if (isDoProduct(productCategory)) {
            List<GenericValue> productCategoryMembers = queryCategoryProductList(productCategory);
            if (UtilValidate.isNotEmpty(productCategoryMembers)) {
                for (GenericValue productCategoryMember : productCategoryMembers) {
                    GenericValue product = productCategoryMember.getRelatedOne("Product", isUseCache());
                    // product is always one level down
                    visitor.visitProduct(product, trailCategories, physicalDepth + 1);
                }
            }
        }
    }
    
    protected final void pushCategoryLocal(GenericValue productCategory, List<GenericValue> trailCategories, int physicalDepth) throws GeneralException {
        visitor.pushCategory(productCategory, trailCategories, physicalDepth);
        trailCategories.add(productCategory);
    }

    protected final void popCategoryLocal(GenericValue productCategory, List<GenericValue> trailCategories, int physicalDepth) throws GeneralException {
        trailCategories.remove(trailCategories.size() - 1);
        visitor.popCategory(productCategory, trailCategories, physicalDepth);
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

    protected <T> List<T> newCategoryTrailList() {
        return new ArrayList<T>(MAX_CATEGORY_DEPTH_PERF);
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
