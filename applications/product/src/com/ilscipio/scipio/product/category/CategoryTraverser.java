package com.ilscipio.scipio.product.category;

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

import com.ilscipio.scipio.product.category.CategoryTraversalException.StopCategoryTraversalException;
import com.ilscipio.scipio.product.category.CategoryVisitor.CommonCategoryVisitor;

/**
 * SCIPIO: Factors out the boilerplate code for traversing catalog categories.
 * Everything is overridable, plus supports a separate CategoryVisitor for the 
 * very core calls only.
 * <p>
 * TODO: I had this idea too late, so should go back on SEO URL/sitemap generation
 * and refactor them to use this (allows more options).
 */
public class CategoryTraverser extends CommonCategoryVisitor {

    public static final String module = CategoryTraverser.class.getName();
    
    protected static final String logPrefix = ""; // "CategoryTraverser: "
    /**
     * Expected max depth (optimization), grossly exaggerated.
     */
    public static final int MAX_DEPTH_PERF = 256;
    
    private final CategoryVisitor visitor;
    private final Delegator delegator;
    private final LocalDispatcher dispatcher;
    private final boolean useCache;
    
    private final boolean doCategory;
    private final boolean doProduct;

    /**
     * Composed visitor constructor, with explicit visitor.
     */
    public CategoryTraverser(CategoryVisitor visitor, Delegator delegator, LocalDispatcher dispatcher,
            boolean useCache, boolean doCategory, boolean doProduct) {
        this.visitor = visitor;
        this.delegator = delegator;
        this.dispatcher = dispatcher;
        this.useCache = useCache;
        this.doCategory = doCategory;
        this.doProduct = doProduct;
    }
    
    public CategoryTraverser(CategoryVisitor visitor, Delegator delegator, LocalDispatcher dispatcher,
            boolean useCache) {
        this(visitor, delegator, dispatcher, useCache, true, true);
    }
    
    /**
     * Abstract instance constructor, with the traverser itself as the visitor.
     */
    public CategoryTraverser(Delegator delegator, LocalDispatcher dispatcher,
            boolean useCache, boolean doCategory, boolean doProduct) {
        this.visitor = this;
        this.delegator = delegator;
        this.dispatcher = dispatcher;
        this.useCache = useCache;
        this.doCategory = doCategory;
        this.doProduct = doProduct;
    }
    
    public CategoryTraverser(Delegator delegator, LocalDispatcher dispatcher,
            boolean useCache) {
        this(delegator, dispatcher, useCache, true, true);
    }
    
    protected <T> List<T> newCategoryTrailList() {
        return new ArrayList<T>(MAX_DEPTH_PERF);
    }
    
    /**
     * Traverse ProductStore categories depth-first.
     * Usually categoryAssocList should be ProdCatalogCategory, but supports starting in middle
     */
    public boolean traverseProductStoreCategoriesDepthFirst(GenericValue productStore) throws GeneralException {
        List<GenericValue> prodCatalogList = EntityQuery.use(delegator).from("ProductStoreCatalog")
                .where(makeProductStoreCatalogCond(productStore.getString("productStoreId")))
                .filterByDate().orderBy("sequenceNum").cache(isUseCache()).queryList();
        prodCatalogList = reorderProductStoreCatalogs(prodCatalogList);
        
        try {
            for(GenericValue prodCatalog : prodCatalogList) {
                List<GenericValue> prodCatalogCategoryList = EntityQuery.use(delegator).from("ProdCatalogCategory")
                        .where(makeProdCatalogCategoryCond(prodCatalog.getString("prodCatalogId")))
                        .filterByDate().orderBy("sequenceNum").cache(isUseCache()).queryList();
                prodCatalogCategoryList = reorderProdCatalogCategories(prodCatalogCategoryList);
                List<GenericValue> trailCategories = newCategoryTrailList();
                traverseCategoriesDepthFirstImpl(prodCatalogCategoryList, CategoryRefType.CATALOG_ASSOC.getResolver(), trailCategories, 0);
            }
            return true;
        } catch(StopCategoryTraversalException e) {
            ; // not an error - just stop
            return false;
        }
    }
    
    protected EntityCondition makeProductStoreCatalogCond(String productStoreId) {
        return EntityCondition.makeCondition("productStoreId", productStoreId);
    }
    
    protected EntityCondition makeProdCatalogCategoryCond(String prodCatalogId) {
        return EntityCondition.makeCondition("prodCatalogId", prodCatalogId);
    }
    
    /**
     * Let's use makeXxxCond methods instead of this.
     */
    @Deprecated
    protected boolean isApplicableProdCatalogCategory(GenericValue prodCatalogCategory) {
        return true;
    }
    
    /**
     * If applicable, reorder by the prodCatalogIds in the config, so that order won't change randomly.
     */
    protected List<GenericValue> reorderProductStoreCatalogs(List<GenericValue> productStoreCatalogList) {
        return productStoreCatalogList;
    }
    
    /**
     * If applicable, reorder by the prodCatalogCategoryTypeIds in the config, so that order won't change randomly.
     */
    protected List<GenericValue> reorderProdCatalogCategories(List<GenericValue> prodCatalogCategoryList) {
        return prodCatalogCategoryList;
    }
    
    /**
     * Traverse categories depth-first.
     * Usually categoryAssocList should be ProdCatalogCategory, but supports starting in middle
     * using ProductCategoryRollup.
     * @param categoryAssocList
     * @return true if went through entire category tree; false if stopped prematurely at some point.
     */
    public boolean traverseCategoriesDepthFirst(List<GenericValue> categoryOrAssocList) throws GeneralException {
        return traverseCategoriesDepthFirst(categoryOrAssocList, null);
    }
    
    /**
     * Traverse categories depth-first.
     * Usually categoryAssocList should be ProdCatalogCategory, but supports starting in middle
     * using ProductCategoryRollup, in which case physicalDepth may be set to 
     * @param categoryAssocList
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
        } catch(StopCategoryTraversalException e) {
            ; // not an error - just stop
            return false;
        }
    }
    
    protected void traverseCategoriesDepthFirstImpl(List<GenericValue> categoryAssocList, CategoryRefType.Resolver listEntryResolver, List<GenericValue> trailCategories, int physicalDepth) throws GeneralException {
        for (GenericValue categoryAssoc : categoryAssocList) {
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
            List<GenericValue> childProductCategoryRollups = querySubCategories(productCategory);
            if (UtilValidate.isNotEmpty(childProductCategoryRollups)) {
                traverseCategoriesDepthFirstImpl(categoryAssocList, CategoryRefType.CATEGORY_ASSOC.getResolver(), trailCategories, physicalDepth + 1);
            }
            
            // pop
            popCategoryLocal(productCategory, trailCategories, physicalDepth);
        }
    }

    protected void visitCategoryLocal(GenericValue productCategory, List<GenericValue> trailCategories, int physicalDepth) throws GeneralException {
        // self
        if (isDoCategory(productCategory)) {
            visitor.visitCategory(productCategory, trailCategories, physicalDepth);
        }
    }
    
    protected void visitProductsLocal(GenericValue productCategory, List<GenericValue> trailCategories, int physicalDepth) throws GeneralException {
        // products
        if (isDoProduct(productCategory)) {
            List<GenericValue> productCategoryMembers = queryCategoryProducts(productCategory);
            if (UtilValidate.isNotEmpty(productCategoryMembers)) {
                for (GenericValue productCategoryMember : productCategoryMembers) {
                    GenericValue product = productCategoryMember.getRelatedOne("Product", isUseCache());
                    // product is always one level down
                    visitor.visitProduct(product, trailCategories, physicalDepth + 1);
                }
            }
        }
    }
    
    protected void pushCategoryLocal(GenericValue productCategory, List<GenericValue> trailCategories, int physicalDepth) throws GeneralException {
        visitor.pushCategory(productCategory, trailCategories, physicalDepth);
        trailCategories.add(productCategory);
    }

    protected void popCategoryLocal(GenericValue productCategory, List<GenericValue> trailCategories, int physicalDepth) throws GeneralException {
        trailCategories.remove(trailCategories.size() - 1);
        visitor.popCategory(productCategory, trailCategories, physicalDepth);
    }

    protected boolean isDoCategory(GenericValue productCategory) {
        return isDoCategory();
    }

    protected boolean isDoProduct(GenericValue productCategory) {
        return isDoProduct();
    }
    
    protected List<GenericValue> querySubCategories(GenericValue productCategory) throws GenericEntityException {
        return EntityQuery.use(getDelegator()).from("ProductCategoryRollup")
                .where("parentProductCategoryId", productCategory.getString("productCategoryId")).filterByDate()
                .orderBy("sequenceNum").cache(isUseCache()).queryList();
    }
    
    protected List<GenericValue> queryCategoryProducts(GenericValue productCategory) throws GenericEntityException {
        return EntityQuery.use(getDelegator()).from("ProductCategoryMember")
                .where("productCategoryId", productCategory.getString("productCategoryId")).filterByDate()
                .orderBy("sequenceNum").cache(isUseCache()).queryList();
    }

    public CategoryVisitor getVisitor() {
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
