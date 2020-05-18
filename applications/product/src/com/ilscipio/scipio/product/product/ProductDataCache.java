package com.ilscipio.scipio.product.product;

import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.GeneralException;
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.GenericEntityException;
import org.ofbiz.entity.GenericValue;
import org.ofbiz.product.config.ProductConfigWrapper;
import org.ofbiz.product.product.ProductContentWrapper;
import org.ofbiz.service.DispatchContext;

import java.io.IOException;
import java.sql.Timestamp;
import java.util.Collection;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Set;
import java.util.function.Function;

/**
 * Caches supplied product-related data from db, mainly used by <code>SolrProductIndexer</code> (SCIPIO).
 * Primarily optimized for solr indexing.
 * WARN: This class intentionally ignores differences in "moment" parameters!
 */
public class ProductDataCache extends ProductDataReader {
    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());

    protected final ProductDataReader reader;
    protected Map<String, StoreData> storeCache = new LinkedHashMap<>();
    protected Map<String, CatalogData> catalogCache = new LinkedHashMap<>();
    protected Map<String, CategoryData> categoryCache = new LinkedHashMap<>();
    protected Map<String, ProductData> productCache = new LinkedHashMap<>();
    protected Integer maxProducts;

    public ProductDataCache(ProductDataReader reader) {
        this.reader = reader;
    }

    public ProductDataCache() {
        this.reader = new ProductDataReader();
    }

    public Integer getMaxProducts() {
        return maxProducts;
    }

    public ProductDataCache setMaxProducts(Integer maxProducts) {
        this.maxProducts = maxProducts;
        return this;
    }

    protected StoreData getStoreData(String productStoreId) {
        StoreData data = storeCache.get(productStoreId);
        if (data == null) {
            data = new StoreData(productStoreId);
            // TODO: REVIEW: written assuming bad productCategoryId don't really happen so misses are recorded
            storeCache.put(productStoreId, data);
        }
        return data;
    }

    public static class StoreData {
        protected final String productStoreId;
        protected GenericValue productStore;

        public StoreData(String productStoreId) {
            this.productStoreId = productStoreId;
        }
    }

    protected CatalogData getCatalogData(String prodCatalogId) {
        CatalogData data = catalogCache.get(prodCatalogId);
        if (data == null) {
            // NOTE: written assuming bad productCategoryId don't really happen
            data = new CatalogData(prodCatalogId);
            catalogCache.put(prodCatalogId, data);
        }
        return data;
    }

    public static class CatalogData {
        protected final String prodCatalogId;
        protected GenericValue prodCatalog;
        protected List<GenericValue> productStoreCatalogs;

        public CatalogData(String prodCatalogId) {
            this.prodCatalogId = prodCatalogId;
        }
    }

    protected CategoryData getCategoryData(String productCategoryId) {
        CategoryData data = categoryCache.get(productCategoryId);
        if (data == null) {
            // NOTE: written assuming bad productCategoryId don't really happen
            data = new CategoryData(productCategoryId);
            categoryCache.put(productCategoryId, data);
        }
        return data;
    }

    public static class CategoryData {
        protected final String productCategoryId;
        protected GenericValue productCategory;
        protected List<List<String>> rollupTrails;
        protected List<String> catalogIds;

        public CategoryData(String productCategoryId) {
            this.productCategoryId = productCategoryId;
        }
    }

    protected ProductData getProductData(String productId) {
        if (maxProducts == null) {
            return new ProductData(productId);
        }
        ProductData data = productCache.get(productId);
        if (data == null) {
            // NOTE: written assuming bad productId don't really happen
            data = new ProductData(productId);
            if (productCache.size() >= maxProducts) {
                Iterator<Map.Entry<String, ProductData>> it = productCache.entrySet().iterator();
                for (int i = 0; i < (productCache.size() - maxProducts + 1); i++) {
                    it.remove();
                }
            }
            productCache.put(productId, data);
        }
        return data;
    }

    public static class ProductData {
        protected final String productId;
        protected GenericValue product;
        protected List<GenericValue> productAssocFrom;
        protected List<GenericValue> productAssocTo;

        public ProductData(String productId) {
            this.productId = productId;
        }
    }

    /*
     * *****************************************************************
     * ProductDataReader Overrides
     * *****************************************************************
     */

    @Override
    public List<GenericValue> getProductAssocFrom(DispatchContext dctx, String productId, Timestamp moment, boolean useCache) throws GenericEntityException {
        ProductData data = getProductData(productId);
        if (data.productAssocFrom != null) {
            return data.productAssocFrom;
        }
        List<GenericValue> productAssocFrom = reader.getProductAssocFrom(dctx, productId, moment, useCache);
        data.productAssocFrom = productAssocFrom;
        // NOTE: Here we ignore differences in moment!
        return productAssocFrom;
    }

    @Override
    public List<GenericValue> getProductAssocTo(DispatchContext dctx, String productId, Timestamp moment, boolean useCache) throws GenericEntityException {
        ProductData data = getProductData(productId);
        if (data.productAssocTo != null) {
            return data.productAssocTo;
        }
        List<GenericValue> productAssocTo = reader.getProductAssocTo(dctx, productId, moment, useCache);
        data.productAssocTo = productAssocTo;
        return productAssocTo;
    }

    @Override
    public List<List<String>> getCategoryRollupTrails(DispatchContext dctx, String productCategoryId, Timestamp moment, boolean ordered, boolean useCache) {
        CategoryData data = getCategoryData(productCategoryId);
        if (data.rollupTrails != null) {
            return data.rollupTrails;
        }
        ordered = true; // force for caching correctness purposes
        List<List<String>> rollupTrails = reader.getCategoryRollupTrails(dctx, productCategoryId, moment, ordered, useCache);
        data.rollupTrails = rollupTrails;
        return rollupTrails;
    }

    @Override
    public List<GenericValue> getProductStoreCatalogsForCatalogId(DispatchContext dctx, String catalogId, Timestamp moment, boolean ordered, boolean useCache) {
        CatalogData data = getCatalogData(catalogId);
        if (data.productStoreCatalogs != null) {
            return data.productStoreCatalogs;
        }
        ordered = true; // force for caching correctness purposes
        List<GenericValue> productStoreCatalogs = reader.getProductStoreCatalogsForCatalogId(dctx, catalogId, moment, ordered, useCache);
        data.productStoreCatalogs = productStoreCatalogs;
        return productStoreCatalogs;
    }

    @Override
    public GenericValue getProductStore(DispatchContext dctx, String productStoreId, boolean useCache) throws GenericEntityException {
        StoreData data = getStoreData(productStoreId);
        if (data.productStore != null) {
            return data.productStore;
        }
        GenericValue productStore = reader.getProductStore(dctx, productStoreId, useCache);
        data.productStore = productStore;
        return productStore;
    }

    @Override
    public List<String> getCatalogIdsByCategoryId(DispatchContext dctx, String productCategoryId, Timestamp moment, boolean useCache) {
        CategoryData data = getCategoryData(productCategoryId);
        if (data.catalogIds != null) {
            return data.catalogIds;
        }
        List<String> catalogIds = reader.getCatalogIdsByCategoryId(dctx, productCategoryId, moment, useCache);
        data.catalogIds = catalogIds;
        return catalogIds;
    }

    @Override
    public Set<String> getOwnCategoryIdsForProduct(DispatchContext dctx, String productId, Timestamp moment, boolean ordered, boolean useCache) throws GenericEntityException {
        return reader.getOwnCategoryIdsForProduct(dctx, productId, moment, ordered, useCache);
    }

    @Override
    public Set<String> getAssocCategoryIdsForProduct(DispatchContext dctx, String productId, List<GenericValue> assocToVariant, Timestamp moment, boolean ordered, boolean useCache) throws GenericEntityException {
        return reader.getAssocCategoryIdsForProduct(dctx, productId, assocToVariant, moment, ordered, useCache);
    }

    @Override
    public Map<String, Object> getProductStandardPrices(DispatchContext dctx, Map<String, Object> context, GenericValue userLogin, GenericValue product, GenericValue productStore, String currencyUomId, Locale priceLocale, boolean useCache) throws GeneralException {
        return reader.getProductStandardPrices(dctx, context, userLogin, product, productStore, currencyUomId, priceLocale, useCache);
    }

    @Override
    public ProductConfigWrapper getConfigurableProductStartingPrices(DispatchContext dctx, Map<String, Object> context, GenericValue userLogin, GenericValue product, GenericValue productStore, String currencyUomId, Locale priceLocale, boolean useCache) throws GeneralException {
        return reader.getConfigurableProductStartingPrices(dctx, context, userLogin, product, productStore, currencyUomId, priceLocale, useCache);
    }

    @Override
    public <C extends Collection<String>> C getProductKeywords(C outKeywords, Delegator delegator, boolean useCache, String... productIds) throws GeneralException {
        return reader.getProductKeywords(outKeywords, delegator, useCache, productIds);
    }

    @Override
    public String getContentText(DispatchContext dctx, GenericValue targetContent, GenericValue product, GenericValue productContent, Locale locale, boolean useCache) throws GeneralException, IOException {
        return reader.getContentText(dctx, targetContent, product, productContent, locale, useCache);
    }

    @Override
    public Map<String, String> getLocalizedContentStringMap(DispatchContext dctx, GenericValue product, String productContentTypeId, Collection<Locale> locales, Locale defaultLocale, Function<Locale, String> langCodeFn, String generalKey, List<ProductContentWrapper> pcwList, Timestamp moment, boolean useCache) throws GeneralException, IOException {
        return reader.getLocalizedContentStringMap(dctx, product, productContentTypeId, locales, defaultLocale, langCodeFn, generalKey, pcwList, moment, useCache);
    }

    @Override
    public void refineLocalizedContentValues(Map<String, String> contentMap, Collection<Locale> locales, Locale defaultLocale, Function<Locale, String> langCodeFn, String generalKey) {
        reader.refineLocalizedContentValues(contentMap, locales, defaultLocale, langCodeFn, generalKey);
    }

    @Override
    public void getProductContentForLocales(Map<String, String> contentMap, DispatchContext dctx, GenericValue product, String productContentTypeId, Collection<Locale> locales, Locale defaultLocale, Function<Locale, String> langCodeFn, String generalKey, Timestamp moment, boolean useCache) throws GeneralException, IOException {
        reader.getProductContentForLocales(contentMap, dctx, product, productContentTypeId, locales, defaultLocale, langCodeFn, generalKey, moment, useCache);
    }

    @Override
    public Map<String, ProductContentWrapper> getProductContentWrappersForLocales(DispatchContext dctx, GenericValue product, Collection<Locale> locales, Locale defaultLocale, Function<Locale, String> langCodeFn, boolean useCache) throws GeneralException {
        return reader.getProductContentWrappersForLocales(dctx, product, locales, defaultLocale, langCodeFn, useCache);
    }

    @Override
    public List<GenericValue> getProductStoresForCatalogIds(DispatchContext dctx, Collection<String> catalogIds, Timestamp moment, boolean ordered, boolean useCache) {
        return reader.getProductStoresForCatalogIds(dctx, catalogIds, moment, ordered, useCache);
    }

    @Override
    public List<GenericValue> getProdCatalogCategoryByCategoryId(DispatchContext dctx, String productCategoryId, Timestamp moment, boolean useCache) {
        return reader.getProdCatalogCategoryByCategoryId(dctx, productCategoryId, moment, useCache);
    }
}
