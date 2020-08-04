package com.ilscipio.scipio.product.product;

import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.GeneralException;
import org.ofbiz.base.util.UtilMisc;
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.GenericEntityException;
import org.ofbiz.entity.GenericValue;
import org.ofbiz.service.DispatchContext;
import org.ofbiz.service.LocalDispatcher;

import java.sql.Timestamp;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

/**
 * Caches supplied product-related data from db, mainly used by <code>SolrProductIndexer</code> (SCIPIO).
 * Primarily optimized for solr indexing.
 * <p>
 * WARN: This class intentionally ignores differences in "moment" parameters! Currently it assumes moments passed are never non-null
 * and will always be the "now" timestamp when passed.
 * </p>
 * NOT thread-safe (meant for local worker caching).
 * TODO: REVIEW: delegator/inheritance complications for extension; may be fixed up in future. For now *Src methods are provided for overriding underlying logic.
 *  It is recommended that overriding classes create both * and *Src methods as shown below.
 */
public class ProductDataCache extends ProductDataReader {
    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());

    // TODO: REVIEW: delegation caused complications due to method reuse, to be improved in future; problematic design for extensions
    //protected final ProductDataReader reader;
    // FIXME: LinkedHashMap had to be removed for thread safety but it helped implement oldest-item removal, so this is currently flawed
    protected Map<String, StoreData> storeCache = new ConcurrentHashMap<>();
    protected Map<String, CatalogData> catalogCache = new ConcurrentHashMap<>();
    protected Map<String, CategoryData> categoryCache = new ConcurrentHashMap<>();
    protected Map<String, ProductData> productCache = new ConcurrentHashMap<>();
    protected Integer maxCacheStores;
    protected Integer maxCacheCatalogs;
    protected Integer maxCacheCategories;
    protected Integer maxCacheProducts;

    //public ProductDataCache(ProductDataReader reader) {
    //    this.reader = reader;
    //}

    public ProductDataCache() {
        //this.reader = new ProductDataReader();
    }

    public Integer getMaxCacheStores() {
        return maxCacheStores;
    }

    public ProductDataCache setMaxCacheStores(Integer maxCacheStores) {
        this.maxCacheStores = sanitizeMaxCacheSize(maxCacheStores);
        return this;
    }

    public Integer getMaxCacheCatalogs() {
        return maxCacheCatalogs;
    }

    public ProductDataCache setMaxCacheCatalogs(Integer maxCacheCatalogs) {
        this.maxCacheCatalogs = sanitizeMaxCacheSize(maxCacheCatalogs);
        return this;
    }

    public Integer getMaxCacheCategories() {
        return maxCacheCategories;
    }

    public ProductDataCache setMaxCacheCategories(Integer maxCacheCategories) {
        this.maxCacheCategories = sanitizeMaxCacheSize(maxCacheCategories);
        return this;
    }

    public Integer getMaxCacheProducts() {
        return maxCacheProducts;
    }

    public ProductDataCache setMaxCacheProducts(Integer maxCacheProducts) {
        this.maxCacheProducts = sanitizeMaxCacheSize(maxCacheProducts);
        return this;
    }

    protected Integer sanitizeMaxCacheSize(Integer maxCacheSize) {
        return (maxCacheSize != null && maxCacheSize > 0) ? maxCacheSize : null;
    }

    public String getLogCacheStats() {
        return "[stores: " + getMaxCacheSizeString(storeCache, maxCacheStores) +
                ", catalogs: " + getMaxCacheSizeString(catalogCache, maxCacheCatalogs) +
                ", categories: " + getMaxCacheSizeString(categoryCache, maxCacheCategories) +
                ", products: " + getMaxCacheSizeString(productCache, maxCacheProducts) + "]";
    }

    private String getMaxCacheSizeString(Map<String, ? extends DataCache> storeCache, Integer maxSize) {
        if (maxSize != null) {
            return storeCache.size() + "/" + maxSize;
        } else {
            return storeCache.size() + "";
        }
    }

    protected <C extends DataCache> void updateCache(Map<String, C> cache, String key, C value, Integer maxEntries) {
        if (cache.containsKey(key)) { // once added, never removed and never need to re-create (fine if assume thread-unsafe)
            return;
        }
        if (maxEntries != null && cache.size() >= maxEntries) {
            Iterator<Map.Entry<String, C>> it = cache.entrySet().iterator();
            int numToRemove = (cache.size() - maxEntries + 1);
            for (int i = 0; i < numToRemove && it.hasNext(); i++) {
                Map.Entry<String, C> entry = it.next();
                it.remove();
                //if (Debug.infoOn()) {
                //    Debug.logInfo("updateCache: Removing product '" + entry.getKey() + "' from cache (" + (i + 1) + "/" + numToRemove + ")", module);
                //}
            }
        }
        cache.put(key, value);
    }

    public static class DataCache {
    }

    protected ProductData getProductData(String productId) {
        if (maxCacheProducts == null) {
            return new ProductData(productId);
        }
        ProductData data = productCache.get(productId);
        if (data == null) {
            // NOTE: written assuming bad productId don't really happen
            data = new ProductData(productId);
        }
        return data;
    }

    public static class ProductData extends DataCache {
        protected final String productId;
        protected GenericValue product;
        protected List<GenericValue> productAssocFrom;
        protected List<GenericValue> productAssocTo;

        public ProductData(String productId) {
            this.productId = productId;
        }
    }

    @Override
    public GenericValue getProduct(DispatchContext dctx, String productId, boolean useCache) throws GenericEntityException {
        ProductData data = getProductData(productId);
        if (data.product != null) {
            return data.product;
        }
        GenericValue product = getProductSrc(dctx, productId, useCache);
        product.setImmutable();
        data.product = product;
        updateCache(productCache, productId, data, maxCacheProducts);
        return product;
    }

    @Override
    public GenericValue getProduct(DispatchContext dctx, Map<String, ?> pkFields, boolean useCache) throws GenericEntityException {
        return getProduct(dctx, (String) pkFields.get("productId"), useCache); // TODO: REVIEW: simpler for now
    }

    protected GenericValue getProductSrc(DispatchContext dctx, String productId, boolean useCache) throws GenericEntityException {
        return super.getProduct(dctx, productId, useCache);
    }

    protected GenericValue getProductSrc(DispatchContext dctx, Map<String, ?> pkFields, boolean useCache) throws GenericEntityException {
        return super.getProduct(dctx, pkFields, useCache);
    }

    @Override
    public List<GenericValue> getProductAssocFrom(DispatchContext dctx, String productId, Timestamp moment, boolean useCache) throws GenericEntityException {
        // NOTE: For now here we ignore differences in moment post-caching
        ProductData data = getProductData(productId);
        if (data.productAssocFrom != null) {
            return data.productAssocFrom;
        }
        List<GenericValue> productAssocFrom = getProductAssocFromSrc(dctx, productId, moment, useCache);
        data.productAssocFrom = Collections.unmodifiableList(productAssocFrom);
        updateCache(productCache, productId, data, maxCacheProducts);
        return productAssocFrom;
    }

    protected List<GenericValue> getProductAssocFromSrc(DispatchContext dctx, String productId, Timestamp moment, boolean useCache) throws GenericEntityException {
        return super.getProductAssocFrom(dctx, productId, moment, useCache);
    }

    @Override
    public List<GenericValue> getProductAssocTo(DispatchContext dctx, String productId, Timestamp moment, boolean useCache) throws GenericEntityException {
        ProductData data = getProductData(productId);
        if (data.productAssocTo != null) {
            return data.productAssocTo;
        }
        List<GenericValue> productAssocTo = getProductAssocToSrc(dctx, productId, moment, useCache);
        data.productAssocTo = Collections.unmodifiableList(productAssocTo);
        updateCache(productCache, productId, data, maxCacheProducts);
        return productAssocTo;
    }

    protected List<GenericValue> getProductAssocToSrc(DispatchContext dctx, String productId, Timestamp moment, boolean useCache) throws GenericEntityException {
        return super.getProductAssocTo(dctx, productId, moment, useCache);
    }

    protected CategoryData getCategoryData(String productCategoryId) {
        CategoryData data = categoryCache.get(productCategoryId);
        if (data == null) {
            // NOTE: written assuming bad productCategoryId don't really happen
            data = new CategoryData(productCategoryId);
        }
        return data;
    }

    public static class CategoryData extends DataCache {
        protected final String productCategoryId;
        protected GenericValue productCategory;
        /* TODO: REVIEW: not yet necessary (for solr performance) - getCategoryRollupTrails only needs to call this once per category
        protected List<GenericValue> productCategoryRollups;
         */
        protected List<List<String>> rollupTrails;
        protected List<String> catalogIds;

        public CategoryData(String productCategoryId) {
            this.productCategoryId = productCategoryId;
        }
    }

    @Override
    public List<String> getCatalogIdsByCategoryId(DispatchContext dctx, String productCategoryId, Timestamp moment, boolean useCache) throws GeneralException {
        CategoryData data = getCategoryData(productCategoryId);
        if (data.catalogIds != null) {
            return data.catalogIds;
        }
        List<String> catalogIds = getCatalogIdsByCategoryIdSrc(dctx, productCategoryId, moment, useCache);
        data.catalogIds = Collections.unmodifiableList(catalogIds);
        updateCache(categoryCache, productCategoryId, data, maxCacheCategories);
        return catalogIds;
    }

    protected List<String> getCatalogIdsByCategoryIdSrc(DispatchContext dctx, String productCategoryId, Timestamp moment, boolean useCache) throws GeneralException {
        return super.getCatalogIdsByCategoryId(dctx, productCategoryId, moment, useCache);
    }

    @Override
    public List<GenericValue> getCategoryRollups(DispatchContext dctx, String productCategoryId, Timestamp moment, boolean ordered, boolean useCache) throws GeneralException {
        /* TODO: REVIEW: not yet necessary (for solr performance) - getCategoryRollupTrails only needs to call this once per category
        CategoryData data = getCategoryData(productCategoryId);
        if (data.productCategoryRollups != null) {
            return data.productCategoryRollups;
        }
        List<GenericValue> productCategoryRollups = getCategoryRollupsSrc(dctx, productCategoryId, moment, ordered, useCache);
        data.productCategoryRollups = Collections.unmodifiableList(productCategoryRollups);
        updateCache(categoryCache, productCategoryId, data, maxCacheCategories);
        return productCategoryRollups;
         */
        return getCategoryRollupsSrc(dctx, productCategoryId, moment, ordered, useCache);
    }

    protected List<GenericValue> getCategoryRollupsSrc(DispatchContext dctx, String productCategoryId, Timestamp moment, boolean ordered, boolean useCache) throws GeneralException {
        return super.getCategoryRollups(dctx, productCategoryId, moment, ordered, useCache);
    }

    @Override
    public List<List<String>> getCategoryRollupTrails(DispatchContext dctx, String productCategoryId, Timestamp moment, boolean ordered, boolean useCache)  throws GeneralException {
        CategoryData data = getCategoryData(productCategoryId);
        if (data.rollupTrails != null) {
            return data.rollupTrails;
        }
        ordered = true; // force for caching correctness purposes
        // Based on CategoryWorker#getCategoryRollupTrails
        List<List<String>> trails = new ArrayList<>();
        List<GenericValue> productCategoryRollups = getCategoryRollups(dctx, productCategoryId, moment, ordered, useCache);
        if (productCategoryRollups != null) {
            for(GenericValue productCategoryRollup : productCategoryRollups) {
                String parentProductCategoryId = productCategoryRollup.getString("parentProductCategoryId");
                List<List<String>> parentTrails = getCategoryRollupTrails(dctx, parentProductCategoryId, moment, ordered, useCache);
                for (List<String> trail : parentTrails) {
                    // WARN: here MUST NOT modify the parent trail in-place for speed because it may be cached
                    trails.add(UtilMisc.copyExtendList(trail, productCategoryId));
                }
            }
        }
        if (trails.isEmpty()) {
            List<String> trail = new ArrayList<>(1);
            trail.add(productCategoryId);
            trails.add(trail);
        }
        data.rollupTrails = Collections.unmodifiableList(trails);
        updateCache(categoryCache, productCategoryId, data, maxCacheCategories);
        return trails;
    }

    protected CatalogData getCatalogData(String prodCatalogId) {
        CatalogData data = catalogCache.get(prodCatalogId);
        if (data == null) {
            // NOTE: written assuming bad productCategoryId don't really happen
            data = new CatalogData(prodCatalogId);
        }
        return data;
    }

    public static class CatalogData extends DataCache {
        protected final String prodCatalogId;
        protected GenericValue prodCatalog;
        protected List<GenericValue> productStoreCatalogs;

        public CatalogData(String prodCatalogId) {
            this.prodCatalogId = prodCatalogId;
        }
    }

    @Override
    public List<GenericValue> getProductStoreCatalogsForCatalogId(DispatchContext dctx, String catalogId, Timestamp moment, boolean ordered, boolean useCache) throws GeneralException {
        CatalogData data = getCatalogData(catalogId);
        if (data.productStoreCatalogs != null) {
            return data.productStoreCatalogs;
        }
        ordered = true; // force for caching correctness purposes
        List<GenericValue> productStoreCatalogs = getProductStoreCatalogsForCatalogIdSrc(dctx, catalogId, moment, ordered, useCache);
        data.productStoreCatalogs = Collections.unmodifiableList(productStoreCatalogs);
        updateCache(catalogCache, catalogId, data, maxCacheCatalogs);
        return productStoreCatalogs;
    }

    protected List<GenericValue> getProductStoreCatalogsForCatalogIdSrc(DispatchContext dctx, String catalogId, Timestamp moment, boolean ordered, boolean useCache) throws GeneralException {
        return super.getProductStoreCatalogsForCatalogId(dctx, catalogId, moment, ordered, useCache);
    }

    @Override
    public List<GenericValue> getProductStoresForCatalogIds(DispatchContext dctx, Collection<String> catalogIds, Timestamp moment, boolean ordered, boolean useCache) throws GeneralException {
        return getProductStoresForCatalogIdsSrc(dctx, catalogIds, moment, ordered, useCache);
    }

    protected List<GenericValue> getProductStoresForCatalogIdsSrc(DispatchContext dctx, Collection<String> catalogIds, Timestamp moment, boolean ordered, boolean useCache) throws GeneralException {
        // SPECIAL: use super so invokes the cache above
        //return reader.getProductStoresForCatalogIds(dctx, catalogIds, moment, ordered, useCache);
        return super.getProductStoresForCatalogIds(dctx, catalogIds, moment, ordered, useCache);
    }

    protected StoreData getStoreData(String productStoreId) {
        StoreData data = storeCache.get(productStoreId);
        if (data == null) {
            data = new StoreData(productStoreId);
        }
        return data;
    }

    public static class StoreData extends DataCache {
        protected final String productStoreId;
        protected GenericValue productStore;

        public StoreData(String productStoreId) {
            this.productStoreId = productStoreId;
        }
    }

    @Override
    public GenericValue getProductStore(DispatchContext dctx, String productStoreId, boolean useCache) throws GenericEntityException {
        StoreData data = getStoreData(productStoreId);
        if (data.productStore != null) {
            return data.productStore;
        }
        GenericValue productStore = getProductStoreSrc(dctx, productStoreId, useCache);
        productStore.setImmutable();
        data.productStore = productStore;
        updateCache(storeCache, productStoreId, data, maxCacheStores);
        return productStore;
    }

    protected GenericValue getProductStoreSrc(DispatchContext dctx, String productStoreId, boolean useCache) throws GenericEntityException {
        return super.getProductStore(dctx, productStoreId, useCache);
    }

}
