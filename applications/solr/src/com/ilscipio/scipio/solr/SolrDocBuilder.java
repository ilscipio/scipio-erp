package com.ilscipio.scipio.solr;

import com.ilscipio.scipio.product.product.ProductDataCache;
import com.ilscipio.scipio.product.product.ProductDataReader;
import org.apache.solr.common.SolrInputDocument;
import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.GeneralException;
import org.ofbiz.base.util.UtilDateTime;
import org.ofbiz.base.util.UtilGenerics;
import org.ofbiz.base.util.UtilMisc;
import org.ofbiz.base.util.UtilProperties;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.GenericEntity;
import org.ofbiz.entity.GenericEntityException;
import org.ofbiz.entity.GenericPK;
import org.ofbiz.entity.GenericValue;
import org.ofbiz.entity.util.EntityUtil;
import org.ofbiz.product.config.ProductConfigWrapper;
import org.ofbiz.product.product.ProductContentWrapper;
import org.ofbiz.product.product.ProductWorker;
import org.ofbiz.product.store.ProductStoreWorker;
import org.ofbiz.service.DispatchContext;
import org.ofbiz.service.LocalDispatcher;
import org.ofbiz.service.ServiceUtil;

import java.math.BigDecimal;
import java.math.RoundingMode;
import java.sql.Timestamp;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Set;
import java.util.function.Function;

/**
 * Loads product data for indexing, based on the old <code>SolrProductUtil#getProductContent</code> method.(SCIPIO).
 * <p>Client code may override to customize any behavior, but be aware future changes/extensions are anticipated.</p>
 * <p>This is a local worker: not thread-safe, not serializable.</p>
 * <p>Main method: {@link #makeProductMapDoc}</p>
 */
public class SolrDocBuilder {
    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());

    protected static final Factory DEFAULT_FACTORY = new Factory();
    protected static final Factory CONFIGURED_FACTORY = readConfiguredFactory();
    protected static final boolean USE_INDEXER_CACHE = UtilProperties.getPropertyAsBoolean(SolrUtil.solrConfigName, "solr.index.rebuild.indexerCache.enable", true);
    protected static final int INDEXER_CACHE_MAX_PRODUCTS = UtilProperties.getPropertyAsInteger(SolrUtil.solrConfigName, "solr.index.rebuild.indexerCache.productCacheSize",
            UtilProperties.getPropertyAsInteger(SolrUtil.solrConfigName, "solr.index.rebuild.record.buffer.size", 1000));

    private final DispatchContext dctx;
    private final Map<String, Object> context;
    private final GenericValue userLogin;
    private final ProductDataReader productDataReader;
    private final boolean useEntityCache;
    private volatile long startTime = System.currentTimeMillis();

    protected SolrDocBuilder(DispatchContext dctx, Map<String, Object> context, ProductDataReader productDataReader) {
        this.dctx = dctx;
        context = Collections.unmodifiableMap(new HashMap<>(context)); // TODO: REVIEW: precaution
        this.context = context;
        this.userLogin = (GenericValue) context.get("userLogin");
        this.productDataReader = productDataReader;
        this.useEntityCache = isUseEntityCache(context);
    }

    protected SolrDocBuilder(DispatchContext dctx, Map<String, Object> context) {
        this(dctx, context, DEFAULT_FACTORY.getProductDataReader(dctx, context));
    }

    /*
     * *************************************************************
     * Factory
     * *************************************************************
     */

    public static SolrDocBuilder getInstance(DispatchContext dctx, Map<String, ?> serviceContext) {
        return getFactory(dctx).getIndexer(dctx, UtilGenerics.cast(serviceContext));
    }

    public static Factory getFactory(DispatchContext dctx) {
        return CONFIGURED_FACTORY;
    }

    protected static Factory readConfiguredFactory() {
        String factoryClassName = UtilProperties.getPropertyValue(SolrUtil.solrConfigName, "solr.index.indexer.factoryClass", Factory.class.getName());
        try {
            Class<Factory> factoryClass = UtilGenerics.cast(SolrDocBuilder.class.getClassLoader().loadClass(factoryClassName));
            return factoryClass.getConstructor().newInstance();
        } catch (Exception e) {
            Debug.logError(e, "Error loading indexer cache from " + SolrUtil.solrConfigName + "#solr.index.indexer.factoryClass", module);
            return DEFAULT_FACTORY;
        }
    }

    public static class Factory {
        public SolrDocBuilder getIndexer(DispatchContext dctx, Map<String, Object> serviceContext) {
            return new SolrDocBuilder(dctx, serviceContext, getProductDataReader(dctx, serviceContext));
        }

        // helpers methods
        protected ProductDataReader getProductDataReader(DispatchContext dctx, Map<String, Object> serviceContext) {
            return USE_INDEXER_CACHE ? new ProductDataCache().setMaxCacheProducts(INDEXER_CACHE_MAX_PRODUCTS) : new ProductDataReader();
        }
    }

    /*
     * *************************************************************
     * General getters/setters
     * *************************************************************
     */

    public DispatchContext getDctx() {
        return dctx;
    }

    /** Returns the map context passed to the indexer, normally derived from the service context received by rebuildSolrIndex and updateToSolr (eca). */
    public Map<String, Object> getContext() {
        return context;
    }

    public Delegator getDelegator() {
        return getDctx().getDelegator();
    }

    public LocalDispatcher getDispatcher() {
        return getDctx().getDispatcher();
    }

    public GenericValue getUserLogin() {
        return userLogin;
    }

    /** Returns the solr core intended for use - should be derived from {@link #getContext()} and often null. */
    public String getCore() {
        return (String) getContext().get("core");
    }

    /** NOTE: This will practically always be false due to risks enabling during Solr ECAs. */
    public boolean isUseEntityCache() {
        return useEntityCache;
    }

    public static boolean isUseEntityCache(Map<String, ?> serviceContext) {
        return Boolean.TRUE.equals(serviceContext.get("useCache"));
    }

    public long getStartTime() {
        return startTime;
    }

    /** Returns the main source of product/category/catalog data (may be caching). */
    public ProductDataReader getProductData() {
        return productDataReader;
    }

    public boolean isUseIndexerCache() {
        return USE_INDEXER_CACHE;
    }

    /*
     * *************************************************************
     * General data helpers
     * *************************************************************
     */

    public Function<Locale, String> getLangCodeFn() {
        return SolrLocaleUtil::getLangCode;
    }

    public String getDefaultCurrency(GenericValue productStore) {
        return SolrProductUtil.getConfiguredDefaultCurrency(getDelegator(), productStore);
    }

    public Locale getBuilderLocale() {
        Locale locale = (Locale) getContext().get("locale");
        return (locale != null) ? locale : Locale.getDefault();
    }

    protected BigDecimal scaleCurrency(Object amount) {
        if (amount == null) {
            return null;
        }
        BigDecimal bdamount = (amount instanceof BigDecimal) ? (BigDecimal) amount : new BigDecimal(amount.toString());
        return bdamount.setScale(2, RoundingMode.HALF_UP);
    }

    protected void addLocalizedContentStringMapToDoc(Map<String, Object> doc, String keyPrefix, String defaultKey, Map<String, String> contentMap,
                                                     Function<Locale, String> langCodeFn, String generalKey) {
        if (contentMap == null) {
            return;
        }
        for (Map.Entry<String, String> entry : contentMap.entrySet()) {
            if (generalKey.equals(entry.getKey())) {
                if (defaultKey != null) {
                    doc.put(defaultKey, entry.getValue());
                }
            } else {
                doc.put(keyPrefix + entry.getKey(), entry.getValue());
            }
        }
    }

    /**
     * FIXME: This is a WORKAROUND replacement following the removal of
     * {@code
     *  <copyField source="title_i18n_*" dest="alphaTitleSort_*"/>
     * }
     * in the solr schema.
     * There are 2 problems:
     * 1) this code should be done by solr, e.g. using existing or custom field processor: https://wiki.apache.org/solr/UpdateRequestProcessor
     * 2) we should not store strings for missing languages at all anymore, it causes
     *    unnecessary storage space taken and may give wrong-language processing issues (e.g. deutsch rules applied to english).
     *    The reason this is being done at indexing is limitations in query sort; it could be
     *    considered an optimization to do it an indexing (at expense of correct-language),
     *    but this was done as a workaround.
     */
    protected void addAlphaLocalizedContentStringMapToDoc(Map<String, Object> doc, String alphaKeyPrefix, String alphaDefaultKey,
                                                       String keyPrefix, String defaultKey, Map<String, String> contentMap,
                                                       List<Locale> locales, Locale defaultLocale, Function<Locale, String> langCodeFn, String generalKey) {
        if (contentMap == null) {
            return;
        }
        String generalValue = null;
        if (contentMap.containsKey(generalKey)) {
            generalValue = contentMap.get(generalKey);
            doc.put(alphaDefaultKey, generalValue);
        }
        // fill in ALL the locales
        for(Locale locale : locales) {
            String locStr = langCodeFn.apply(locale);
            String value = contentMap.get(keyPrefix + locStr);
            if (UtilValidate.isEmpty(value)) {
                value = generalValue;
                // 2017-09-14: no longer needed because general and target lang are [same?]
                //if (UtilValidate.isEmpty(value)) {
                //    // if there's nothing else, check entry for sys default lang; even though
                //    // this is sure to be the wrong language, it's better than nothing for sorting...
                //    value = contentMap.get(keyPrefix + defaultLocale);
                //}
            }
            doc.put(alphaKeyPrefix + locStr, value);
        }
    }

    public String getLogStatsShort() {
        ProductDataReader data = getProductData();
        if (data instanceof ProductDataCache) {
            return ((ProductDataCache) data).getLogCacheStats();
        }
        return null;
    }

    /*
     * *************************************************************
     * Pre-indexing callbacks
     * *************************************************************
     */

    public ExpandProductResult makeExpandProductResult() {
        return new ExpandProductResult();
    }

    public static class ExpandProductResult {
        protected int numFailures = 0;
        protected Map<String, Object> errorResult;

        public ExpandProductResult() {}

        public int getNumFailures() {
            return numFailures;
        }

        public ExpandProductResult setNumFailures(int numFailures) {
            this.numFailures = numFailures;
            return this;
        }

        public ExpandProductResult addFailure(String msg) {
            this.numFailures++;
            return this;
        }

        public Map<String, Object> getErrorResult() {
            return errorResult;
        }

        public ExpandProductResult setErrorResult(Map<String, Object> errorResult) {
            this.errorResult = errorResult;
            return this;
        }

        public boolean isError() {
            return ServiceUtil.isError(errorResult);
        }
    }

    public ExpandProductResult expandProductsForIndexing(Iterable<ProductIndexer.ProductEntry> products, Map<String, ProductIndexer.ProductEntry> expandedProducts) {
        ExpandProductResult expandResult = makeExpandProductResult();
        for(ProductIndexer.ProductEntry entry : products) {
            try {
                expandProductForIndexing(entry.getShortPk(), entry, expandedProducts, expandResult);
            } catch(Exception e) {
                Debug.logError(e, "expandProductsForIndexing: " + e.getMessage(), module);
                return makeExpandProductResult().setErrorResult(ServiceUtil.returnError(e.getMessage()));
            }
        }
        return expandResult;
    }

    /** Returns true if there's any chance the product should expand for indexing. */
    public boolean isExpandProductForIndexing(String productId, ProductIndexer.ProductEntry pur,
                                              Map<String, ProductIndexer.ProductEntry> expandedProducts, ExpandProductResult expandResult) {
        if (pur.isExplicitRemove()) {
            // Here the Product will already have been removed so we can't determine its
            // variants; but that's ok because it makes no sense for virtual to have been
            // removed without its variants removed, which should have come through an ECA
            // either in the same transaction or another before it.
            return false;
        }
        return pur.isUpdateRelatedProducts();
    }

    public int expandProductForIndexing(String productId, ProductIndexer.ProductEntry pur, Map<String, ProductIndexer.ProductEntry> expandedProducts,
                                        ExpandProductResult expandResult) throws GeneralException {
        int numProducts = 1;
        expandedProducts.put(productId, pur);
        if (isExpandProductForIndexing(productId, pur, expandedProducts, expandResult)) {
            GenericValue product = lookupProductForIndexing(productId, pur);
            if (product != null) {
                Timestamp moment = UtilDateTime.nowTimestamp();
                Collection<String> relatedProductIds = getRelatedProductIdsForIndexing(productId, product, pur, expandedProducts, moment, expandResult);
                if (relatedProductIds != null) {
                    for (String relatedProductId : relatedProductIds) {
                        // NOTE: we crush any prior entry for same product; chronological last update request in transaction has priority
                        // re-add the key to LinkedHashMap keep a readable order in log
                        expandedProducts.remove(relatedProductId);
                        expandedProducts.put(relatedProductId, getProductIndexer().makeEntry(pur, relatedProductId, pur.getEntryTime()));
                        numProducts++;
                    }
                }
            } else {
                if (pur.isExplicitAdd()) {
                    Debug.logError("expandProductForIndexing: Product not found for productId: " + productId, module);
                    expandedProducts.remove(productId);
                    expandResult.addFailure("Product not found for productId: " + productId);
                } else {
                    // Treated as removal
                }
            }
        }
        return numProducts;
    }

    /** NOTE: this gets the instance passed by the callers if possible - we don't do this during the main indexing run to prevent bad caching e.g. from entities. */
    public GenericValue lookupProductForIndexing(String productId, ProductIndexer.ProductEntry pur) throws GeneralException {
        GenericValue product = pur.getProductValue();
        if (product != null) { // NOTE: Likely to be null
            // NOTE: this is only used for the isVirtual/isVariant flag: it is re-looked up by productId in updateToSolrCoreMultiAddImp
            return product;
        } else {
            return getProductData().getProduct(getDctx(), productId, false);
        }
    }

    public Collection<String> getRelatedProductIdsForIndexing(String productId, GenericValue product, ProductIndexer.ProductEntry pur,
                                                              Map<String, ProductIndexer.ProductEntry> expandedProducts, Timestamp moment,
                                                              ExpandProductResult expandResult) throws GeneralException {
        Collection<String> relatedProductIds = null;
        if (hasChildProductIdsForIndexing(productId, product, pur, expandedProducts, moment, expandResult)) {
            Collection<String> variantProductIds = getChildProductIdsForIndexing(productId, product, pur, expandedProducts, moment, expandResult);
            if (relatedProductIds == null) {
                relatedProductIds = variantProductIds;
            } else {
                relatedProductIds.addAll(variantProductIds);
            }
        }
        if (hasParentProductIdsForIndexing(productId, product, pur, expandedProducts, moment, expandResult)) {
            Collection<String> virtualProductIds = getParentProductIdsForIndexing(productId, product, pur, expandedProducts, moment, expandResult);
            if (relatedProductIds == null) {
                relatedProductIds = virtualProductIds;
            } else {
                relatedProductIds.addAll(virtualProductIds);
            }
        }
        return relatedProductIds;
    }

    public boolean hasChildProductIdsForIndexing(String productId, GenericValue product, ProductIndexer.ProductEntry pur,
                                                 Map<String, ProductIndexer.ProductEntry> expandedProducts, Timestamp moment, ExpandProductResult expandResult) throws GeneralException {
        return pur.isUpdateVariants() && Boolean.TRUE.equals(product.getBoolean("isVirtual"));
    }

    public Collection<String> getChildProductIdsForIndexing(String productId, GenericValue product, ProductIndexer.ProductEntry pur,
                                                            Map<String, ProductIndexer.ProductEntry> expandedProducts, Timestamp moment, ExpandProductResult expandResult) throws GeneralException {
        if (pur.isUpdateVariantsDeep()) {
            return getProductData().getVariantProductIdsDeepDfs(getDctx(), productId, moment, false);
        } else {
            return getProductData().getVariantProductIds(getDctx(), productId, moment, false);
        }
    }

    public boolean hasParentProductIdsForIndexing(String productId, GenericValue product, ProductIndexer.ProductEntry pur,
                                                  Map<String, ProductIndexer.ProductEntry> expandedProducts, Timestamp moment, ExpandProductResult expandResult) throws GeneralException {
        return pur.isUpdateVirtual() && Boolean.TRUE.equals(product.getBoolean("isVariant"));
    }

    public Collection<String> getParentProductIdsForIndexing(String productId, GenericValue product, ProductIndexer.ProductEntry pur,
                                                             Map<String, ProductIndexer.ProductEntry> expandedProducts, Timestamp moment, ExpandProductResult expandResult) throws GeneralException {
        if (pur.isUpdateVirtualDeep()) {
            return getProductData().getVirtualProductIdsDeepDfs(getDctx(), productId, moment, false);
        } else {
            //return ProductWorker.getVirtualProductIds(dctx.getDelegator(), dctx.getDispatcher(),
            //        productId, orderBy, maxPerLevel, moment, false);
            return getProductData().getVirtualProductIds(getDctx(), productId, moment, false);
        }
    }

    public static boolean isProductInstance(Map<String, Object> srcInstance) {
        return (srcInstance instanceof GenericValue && "Product".equals(((GenericValue) srcInstance).getEntityName()));
    }

    public static GenericValue asProductInstanceOrNull(Map<String, Object> srcInstance) {
        return isProductInstance(srcInstance) ? (GenericValue) srcInstance : null;
    }

    /*
     * *************************************************************
     * General document handling and building
     * *************************************************************
     */

    public String getDocId(Object doc) {
        if (doc instanceof SolrInputDocument) {
            return getDocId((SolrInputDocument) doc);
        } else if (doc instanceof EntityIndexer.DocEntry) {
            return ((EntityIndexer.DocEntry) doc).getShortPk();
        } else if (doc instanceof EntityIndexer.Entry) {
            return ((EntityIndexer.Entry) doc).getShortPk();
        } else if (doc instanceof Map) {
            return getDocId(UtilGenerics.<Map<String, Object>>cast(doc));
        } else {
            throw new IllegalArgumentException("Invalid document type or value: " + (doc != null ? doc.getClass().getName() : "null"));
        }
    }

    public String getDocId(Map<String, Object> doc) {
        return (String) doc.get("id");
    }

    public String getDocId(SolrInputDocument doc) {
        return (String) doc.getFieldValue("id");
    }

    public SolrInputDocument asSolrDoc(Object doc) {
        if (doc instanceof SolrInputDocument) {
            return (SolrInputDocument) doc;
        } else if (doc instanceof EntityIndexer.DocEntry) {
            return makeSolrDoc(((EntityIndexer.DocEntry) doc).getDoc());
        } else if (doc instanceof Map) {
            return makeSolrDoc(UtilGenerics.<Map<String, Object>>cast(doc));
        } else {
            throw new IllegalArgumentException("invalid document");
        }
    }

    /**
     * Generates a Solr schema product from the fields of the solrProductAttributes service interface. Previously in SolrProductUtil.
     * NEW: 2020-05-15: This method now does almost nothing because the "fields" map simplifies everything significantly.
     */
    public SolrInputDocument makeSolrDoc(Map<String, Object> docMap) {
        SolrInputDocument doc = new SolrInputDocument();
        return addSolrDocFields(doc, docMap);
    }

    public SolrInputDocument addSolrDocFields(SolrInputDocument doc, Map<String, Object> docMap) {
        for(Map.Entry<String, Object> entry : docMap.entrySet()) {
            doc.addField(entry.getKey(), entry.getValue());
        }
        return doc;
    }

    /*
     * *************************************************************
     * Product data processing and document building
     * *************************************************************
     */

    /**
     * Generates a document in map form of product content that may be passed to the {@link SolrProductSearch#commitAddToSolr} or {@link #makeSolrDoc(Map)} method/service.
     * <p>
     * NOTE: Prefer <code>fields</code> over <code>targetCtx</code>, which is old method and requires needless patching to make work.
     * For <code>fields</code>, the field names are the final Solr schema field names; for <code>targetCtx</code>, they are
     * intermediate names found in services.xml.
     * <p>
     * <b>WARNING:</b> DO NOT USE ENTITY CACHE HERE FOR ANYTHING! It is an error and you will end up with serious problems (ECAs and entity cache don't mix)!
     * Do not call any utilities that use the entity cache! If you need them, you must patch them to not use entity cache.
     * The useCache parameter here will always be false for the foreseeable future.
     * <p>
     * <b>WARNING:</b> You should use the provided nowTimestamp for filter-by-date operations.
     * <p>
     * @param productDocBuilder the product builder
     * @param productEntry request source information, usually from ECAs - may be null
     * @return a document map representing the product - can then be passed to {@link #makeSolrDoc(Map)}
     */
    public Map<String, Object> makeProductMapDoc(ProductDocBuilder productDocBuilder, ProductIndexer.ProductEntry productEntry,
                                                 List<SolrDocBuilder.ProductFilter> productFilters) throws GeneralException {
        if (!ProductFilter.allowProduct(getDctx(), productFilters, productDocBuilder.getProduct(), productDocBuilder, productEntry)) {
            if (SolrUtil.verboseOn()) {
                Debug.logInfo("makeProductMapDoc: Filtered out product '" + productDocBuilder.getProductId() + "'", module);
            }
            return null;
        }
        if (SolrUtil.verboseOn()) {
            Debug.logInfo("makeProductMapDoc: Getting product content for product '" + productDocBuilder.getProductId() + "'", module);
        }
        return productDocBuilder.populateDoc(makeEmptyProductMapDoc());
    }

    public Map<String, Object> makeProductMapDoc(Object product, Timestamp moment) throws GeneralException {
        return makeProductMapDoc(makeProductDocBuilder(product, moment), null, null);
    }

    public Map<String, Object> makeProductMapDoc(Object product) throws GeneralException {
        return makeProductMapDoc(product, UtilDateTime.nowTimestamp());
    }

    /** Makes empty map for new document, can be overridden. NOTE: 2020-07: Now LinkedHashMap to preserve order since SolrInputDocument._fields is also one. */
    protected Map<String, Object> makeEmptyProductMapDoc() {
        //return new HashMap<>();
        return new LinkedHashMap<>();
    }

    public GenericValue asProductValue(Object product) throws GeneralException {
        if (product instanceof GenericValue || product == null) {
            return (GenericValue) product;
        } else if (product instanceof String) {
            return getProductData().getProduct(getDctx(), (String) product, false);
        } else if (product instanceof GenericPK) {
            return getProductData().getProduct(getDctx(), (GenericPK) product, false);
        } else if (product instanceof Map) {
            String id = (String) UtilGenerics.<Map<String, ?>>cast(product).get("id");
            if (UtilValidate.isEmpty(id)) {
                id = (String) UtilGenerics.<Map<String, ?>>cast(product).get("productId");
                if (UtilValidate.isEmpty(id)) {
                    throw new IllegalArgumentException("Could not determine an ID from product document or instance: " + product);
                }
            }
            return getProductData().getProduct(getDctx(), id, false);
        } else {
            throw new IllegalArgumentException("Invalid product value type: " + product.getClass().getName());
        }
    }

    public ProductIndexer getProductIndexer() {
        return ProductIndexer.getDefault();
    }

    /**
     * If the given docValue is not already a ProductDocEntry, tries to create one, in other words auto-converting/normalizing.
     */
    public ProductIndexer.ProductDocEntry asDocEntry(Object docValue, List<SolrDocBuilder.ProductFilter> productFilters, Timestamp moment) throws GeneralException {
        ProductIndexer entityIndexer = getProductIndexer();
        if (docValue instanceof ProductIndexer.ProductDocEntry) {
            ProductIndexer.ProductDocEntry docEntry = (ProductIndexer.ProductDocEntry) docValue;
            if (!ProductFilter.allowProduct(getDctx(), productFilters, docEntry.getDoc(), docEntry.getData())) {
                return null;
            }
            return docEntry;
        } else if (docValue instanceof EntityIndexer.DocEntry) {
            EntityIndexer.DocEntry docEntry = (EntityIndexer.DocEntry) docValue;
            ProductDocBuilder data = (ProductDocBuilder) docEntry.getData();
            if (data == null) {
                data = makeProductDocBuilder(docEntry.getPk(), moment);
            }
            if (!ProductFilter.allowProduct(getDctx(), productFilters, docEntry.getDoc(), data)) {
                return null;
            }
            return entityIndexer.makeDocEntry(docEntry.getPk(), docEntry.getDoc(), docEntry.getData());
            //} else if (docValue instanceof ProductEntry) { // TODO: REVIEW: what needed this?
            //    return makeDocEntry((ProductEntry) docValue, null, null);
        }
        GenericPK pk;
        Map<String, Object> doc;
        SolrDocBuilder.ProductDocBuilder data;
        ProductIndexer.ProductEntry entry = null;
        if (docValue instanceof ProductIndexer.ProductEntry) {
            entry = (ProductIndexer.ProductEntry) docValue;
            pk = entry.getPk();
            docValue = getProductData().getProduct(getDctx(), pk, false);
            if (docValue == null) {
                throw new GenericEntityException("Could not find Product " + pk);
            }
            data = makeProductDocBuilder(docValue, moment);
            doc = makeProductMapDoc(data, entry, productFilters);
        } else if (docValue instanceof GenericValue) {
            pk = ((GenericValue) docValue).getPrimaryKey();
            data = makeProductDocBuilder(docValue, moment);
            doc = makeProductMapDoc(data, entry, productFilters);
        } else if (docValue instanceof GenericPK) {
            pk = (GenericPK) docValue;
            docValue = getProductData().getProduct(getDctx(), pk, false);
            if (docValue == null) {
                throw new GenericEntityException("Could not find Product " + pk);
            }
            data = makeProductDocBuilder(docValue, moment);
            doc = makeProductMapDoc(data, entry, productFilters);
        } else if (docValue instanceof GenericEntity) {
            throw new IllegalArgumentException("Unsupported document type: " + docValue.getClass().getName());
        } else if (docValue instanceof Map) { // solr doc
            doc = UtilGenerics.cast(docValue);
            pk = GenericPK.create(getDelegator(), getDelegator().getModelEntity(entityIndexer.getEntityName()), doc.get("id"));
            data = makeProductDocBuilder(pk, moment);
            if (!ProductFilter.allowProduct(getDctx(), productFilters, doc, data)) {
                return null;
            }
        } else {
            throw new IllegalArgumentException("Unsupported document type: " + docValue.getClass().getName());
        }
        if (doc == null) { // filtered or otherwise null
            return null;
        }
        return (entry != null) ? entityIndexer.makeDocEntry(entry, doc, data) : entityIndexer.makeDocEntry(pk, doc, data);
    }

    public interface ProductFilter {
        /**
         * Returns true if product should be considered for indexing - using product entity as input.
         * NOTE: You must implement both overrides.
         * NOTE: productEntry is frequently null.
         */
        boolean allowProduct(DispatchContext dctx, GenericValue product, ProductDocBuilder productDocBuilder, ProductIndexer.ProductEntry productEntry) throws GeneralException;

        /**
         * Returns true if product should be considered for indexing - using solr document as input.
         * NOTE: You must implement both overrides.
         * NOTE: productEntry is frequently null.
         */
        boolean allowProduct(DispatchContext dctx, Map<String, Object> doc, ProductDocBuilder productDocBuilder) throws GeneralException;

        static boolean allowProduct(DispatchContext dctx, Collection<? extends ProductFilter> productFilters, GenericValue product, ProductDocBuilder productDocBuilder,
                                    ProductIndexer.ProductEntry productEntry) throws GeneralException {
            if (productFilters != null) {
                for(ProductFilter productFilter : productFilters) {
                    if (!productFilter.allowProduct(dctx, product, productDocBuilder, productEntry)) {
                        return false;
                    }
                }
            }
            return true;
        }

        static boolean allowProduct(DispatchContext dctx, Collection<? extends ProductFilter> productFilters, Map<String, Object> doc, ProductDocBuilder productDocBuilder) throws GeneralException {
            if (productFilters != null) {
                for(ProductFilter productFilter : productFilters) {
                    if (!productFilter.allowProduct(dctx, doc, productDocBuilder)) {
                        return false;
                    }
                }
            }
            return true;
        }
    }

    public ProductFilter makeStoreProductFilter(Collection<String> includeMainStoreIds, Collection<String> includeAnyStoreIds) {
        if (includeMainStoreIds != null) {
            return new MainStoreProductFilter(includeMainStoreIds);
        } else if (includeAnyStoreIds != null) {
            return new AnyStoreProductFilter(includeAnyStoreIds);
        } else {
            return null;
        }
    }

    public static abstract class StoreProductFilter implements ProductFilter {
        protected final Collection<String> includeStoreIds;

        public StoreProductFilter(Collection<String> includeStoreIds) {
            this.includeStoreIds = includeStoreIds;
        }

        public Collection<String> getIncludeStoreIds() {
            return includeStoreIds;
        }

        public boolean isIncludeStoreId(String productStoreId) {
            return getIncludeStoreIds().contains(productStoreId);
        }
    }

    public static class AnyStoreProductFilter extends StoreProductFilter {
        public AnyStoreProductFilter(Collection<String> includeStoreIds) {
            super(includeStoreIds);
        }

        @Override
        public boolean allowProduct(DispatchContext dctx, GenericValue product, ProductDocBuilder productDocBuilder, ProductIndexer.ProductEntry productEntry) throws GeneralException {
            Collection<String> productStoreIds = productDocBuilder.getProductStoreIds();
            if (productStoreIds != null) {
                for(String productStoreId : productStoreIds) {
                    if (isIncludeStoreId(productStoreId)) {
                        return true;
                    }
                }
            }
            return false;
        }

        @Override
        public boolean allowProduct(DispatchContext dctx, Map<String, Object> doc, ProductDocBuilder productDocBuilder) throws GeneralException {
            Collection<String> productStoreIds = UtilGenerics.cast(doc.get("productStore"));
            if (productStoreIds != null) {
                for(String productStoreId : productStoreIds) {
                    if (isIncludeStoreId(productStoreId)) {
                        return true;
                    }
                }
            }
            return false;
        }
    }

    public static class MainStoreProductFilter extends StoreProductFilter {
        public MainStoreProductFilter(Collection<String> includeStoreIds) {
            super(includeStoreIds);
        }

        @Override
        public boolean allowProduct(DispatchContext dctx, GenericValue product, ProductDocBuilder productDocBuilder, ProductIndexer.ProductEntry productEntry) throws GeneralException {
            return isIncludeStoreId(productDocBuilder.getProductStoreId());
        }

        @Override
        public boolean allowProduct(DispatchContext dctx, Map<String, Object> doc, ProductDocBuilder productDocBuilder) throws GeneralException {
            return isIncludeStoreId(UtilMisc.getFirst(UtilGenerics.cast(doc.get("productStore"))));
        }
    }

    /** Creates a new product reader instance. */
    public ProductDocBuilder makeProductDocBuilder(Object product, Timestamp moment) {
        if (product instanceof GenericValue) {
            GenericValue productValue = (GenericValue) product;
            if (!"Product".equals(productValue.getEntityName())) {
                throw new IllegalArgumentException("Not a Product value");
            }
            return makeProductDocBuilder(productValue.getString("productId"), productValue, moment);
        } else if (product instanceof GenericPK || product instanceof EntityIndexer.Entry || product instanceof EntityIndexer.DocEntry) {
            GenericPK productPk;
            if (product instanceof EntityIndexer.Entry) {
                productPk = ((EntityIndexer.Entry) product).getPk();
            } else if (product instanceof EntityIndexer.DocEntry) {
                productPk = ((EntityIndexer.DocEntry) product).getPk();
            } else {
                productPk = (GenericPK) product;
            }
            if (!"Product".equals(productPk.getEntityName())) {
                throw new IllegalArgumentException("Not a Product value");
            }
            return makeProductDocBuilder(productPk.getString("productId"), null, moment);
        } else if (product instanceof String) {
            return makeProductDocBuilder((String) product, null, moment);
        } else {
            throw new IllegalArgumentException("Invalid product primary key or value");
        }
    }

    /** Creates a new product reader instance for doc map generation. Client code should override. */
    public ProductDocBuilder makeProductDocBuilder(String productId, GenericValue product, Timestamp moment) {
        return new ProductDocBuilder(productId, product, moment);
    }

    /**
     * Records and caches product info during call to {@link #makeProductMapDoc}.
     * <p>Intended for client code to subclass and override any method as needed - many factoring points provided.</p>
     * <p></p>Written to minimize entity lookups.</p>
     * <p>NOTE: These use mutable pattern to prevent multiple reads automatically and without effort.
     * Client code is not obliged to follow these patterns for additional code added.</p>
     * <p>NOTE: Further caching is done underneath by {@link ProductDataCache} from {@link #getProductData()}.</p>
     */
    public class ProductDocBuilder {
        protected final String productId;
        protected GenericValue product;
        protected Timestamp moment;
        protected String parentProductId; // empty string means already looked up

        protected List<GenericValue> productAssocFrom;
        protected List<GenericValue> productAssocTo;
        protected List<GenericValue> productAssocToVariant;
        protected List<GenericValue> productAssocFromVariant;

        protected List<GenericValue> productCategoryMembers;
        protected Set<String> ownCategoryIds;
        protected Set<String> assocCategoryIds;
        protected Set<String> categoryIds;

        protected Set<String> ownCategoryTrails;
        protected Set<String> assocCategoryTrails;
        protected Set<String> categoryTrails;

        protected Set<String> catalogIds;
        protected List<GenericValue> productStores;
        protected Map<String, GenericValue> productStoreAndCatalogAssoc;
        protected Set<String> productStoreIds;
        protected GenericValue productStore;
        protected boolean productStoreChecked = false;

        protected Set<String> relatedCatalogIds;
        protected Set<String> relatedCategoryIds;
        protected Set<String> relatedTrails;

        protected Set<String> featureSet;

        protected String internalName;
        protected String productTypeId;
        protected Boolean digital;
        protected Boolean physical;
        protected Boolean requireAmount;

        protected String currencyUomId;
        protected Map<String, Object> stdPriceMap;
        protected String stdPriceMapProductStoreId; // compatibility
        protected String stdPriceMapCurrencyUomId; // compatibility
        protected ProductConfigWrapper cfgPriceWrapper;
        protected String cfgPriceWrapperProductStoreId; // compatibility
        protected String cfgPriceWrapperCurrencyUomId; // compatibility
        protected Map<String, Map<String, Object>> storeStdPriceMaps;
        protected Map<String, ProductConfigWrapper> storeCfgPriceWrappers;
        protected Map<String, Map<String, Object>> storePriceFields;

        protected Boolean useVariantStockCalcForTotal;
        protected Map<String, BigDecimal> productStoreInventories;

        protected List<Locale> locales;
        protected Locale defaultLocale;
        protected Map<String, ProductContentWrapper> pcwMap;
        protected List<ProductContentWrapper> pcwList;
        protected List<GenericValue> productContent;
        protected Map<String, String> titleLocaleMap;
        protected Map<String, String> descriptionLocaleMap;
        protected Map<String, String> longDescriptionLocaleMap;
        protected Set<String> keywords;
        protected List<String> legacyPriceFieldNames;

        protected ProductDocBuilder(String productId, GenericValue product, Timestamp moment) {
            this.productId = (productId != null) ? productId : product.getString("productId");
            this.product = product; // may be null
            this.moment = (moment != null) ? moment : UtilDateTime.nowTimestamp();
        }

        /*
         * *************************************************************
         * Product map document production - populateDoc
         * *************************************************************
         */

        /** Produces a solr document in the form of a map. Trivially converted to SolrInputDocument later using {@link #makeSolrDoc(Map)}. */
        public Map<String, Object> populateDoc(Map<String, Object> doc) throws GeneralException {
            populateDocMeta(doc);
            populateDocType(doc);
            populateDocPrice(doc);
            populateDocInventory(doc);
            populateDocContent(doc);
            populateDocSort(doc);
            populateDocCustom(doc);
            return doc;
        }

        public void populateDocMeta(Map<String, Object> doc) throws GeneralException {
            doc.put("id", getProductId());
            doc.put("productId", getProductId());
            doc.put("productStore", getProductStoreIds());
            doc.put("cat", getCategoryTrails());
            doc.put("ownCat_ss", getOwnCategoryTrails());
            doc.put("catalog", getCatalogIds());
            putCategorySequenceNums(doc, "catSeq_", "_i");
            checkStoresAndCatalogs();
        }

        public void checkStoresAndCatalogs() throws GeneralException {
            List<GenericValue> productStores = getProductStores();
            Collection<String> catalogIds = getCatalogIds();
            if (productStores.isEmpty()) {
                if (catalogIds.isEmpty()) {
                    Debug.logInfo("Could not determine store for product '" + getProductId() + "'; no catalogs", module);
                } else {
                    Debug.logInfo("Could not determine store for product '" + getProductId() + "' from catalogs: " + catalogIds, module);
                }
            } else {
                if (SolrUtil.verboseOn()) {
                    if (UtilValidate.isNotEmpty(getRelatedCatalogIds())) {
                        Debug.logInfo("Determined store(s) for product '" + getProductId() + "' indirectly (" + getProductStoreIds() + ") from related catalogs: " + catalogIds, module);
                    } else {
                        Debug.logInfo("Determined store(s) for product '" + getProductId() + "' directly (" + getProductStoreIds() + ") from catalogs: " + catalogIds, module);
                    }
                }
            }
        }

        public void populateDocType(Map<String, Object> doc) throws GeneralException {
            populateDocTypeBasic(doc);
            populateDocTypeFeatures(doc);
        }

        public void populateDocTypeBasic(Map<String, Object> doc) throws GeneralException {
            String productTypeId = getProductTypeId();
            if (productTypeId != null) {
                doc.put("productTypeId", productTypeId);
            }
            if (isVirtual()) {
                doc.put("isVirtual", true);
            }
            if (isVariant()) {
                doc.put("isVariant", true);
            }
            if (isDigital()) {
                doc.put("isDigital", true);
            }
            if (isPhysical()) {
                doc.put("isPhysical", true);
            }
            if (isRequireAmount()) {
                doc.put("requireAmount_b", true);
            }
            GenericValue product = getProduct();
            doc.put("listed_b", !Boolean.FALSE.equals(product.getBoolean("listed")));
            doc.put("searchable_b", !Boolean.FALSE.equals(product.getBoolean("searchable")));
        }

        public void populateDocTypeFeatures(Map<String, Object> doc) throws GeneralException {
            Collection<String> featureSet = getFeatureSet();
            if (featureSet != null) {
                doc.put("features", featureSet);
            }
        }

        public void populateDocPrice(Map<String, Object> doc) throws GeneralException {
            if (isConfigurableProduct()) {
                populateDocPriceConfigurable(doc);
            } else {
                populateDocPriceStandard(doc);
            }
        }

        public void populateDocPriceStandard(Map<String, Object> doc) throws GeneralException {
            populateDocPriceStandardField(doc, getStdPriceMap(), getStdPriceMapCurrencyUomId(), "");
            for(Map.Entry<String, Map<String, Object>> entry : getStoreStdPriceMaps().entrySet()) {
                populateDocPriceStandardField(doc, entry.getValue(), getStoreCurrencyUomId(entry.getKey()), entry.getKey());
            }
        }

        protected String getMainPriceFieldName(String baseName, String fieldGroup) {
            if (UtilValidate.isNotEmpty(fieldGroup)) {
                return baseName + "_" + fieldGroup + "_c";
            } else {
                return baseName + "_c";
            }
        }

        protected String getFloatPriceFieldName(String baseName, String fieldGroup) {
            if (UtilValidate.isNotEmpty(fieldGroup)) {
                return baseName + "_" + fieldGroup + "_pf";
            } else if (isLegacyPriceField(baseName)) {
                return baseName;
            } else if (isSharedFloatPriceField(baseName)) {
                return baseName + "_pf";
            } else {
                return null;
            }
        }

        protected boolean isLegacyPriceField(String baseName) {
            return getLegacyPriceFieldNames().contains(baseName);
        }

        public List<String> getLegacyPriceFieldNames() {
            List<String> staticPriceFieldNames = this.legacyPriceFieldNames;
            if (staticPriceFieldNames == null) {
                staticPriceFieldNames = determineLegacyPriceFieldNames();
                this.legacyPriceFieldNames = staticPriceFieldNames;
            }
            return staticPriceFieldNames;
        }

        protected List<String> determineLegacyPriceFieldNames() {
            return List.of("defaultPrice", "listPrice");
        }

        /**
         * Currently returns false; handled by legacy price fields, but may be overridden easily.
         */
        protected boolean isSharedFloatPriceField(String baseName) {
            return false; // TODO: REVIEW: these are disabled by default for now, but may want them (see above)
        }

        protected <T> T getMainPriceField(Map<String, Object> doc, String baseName, String fieldGroup) {
            String fieldName = getMainPriceFieldName(baseName, fieldGroup);
            return (fieldName != null) ? UtilGenerics.cast(doc.get(fieldName)) : null;
        }

        protected <T> T getFloatPriceField(Map<String, Object> doc, String baseName, String fieldGroup) {
            String fieldName = getFloatPriceFieldName(baseName, fieldGroup);
            return (fieldName != null) ? UtilGenerics.cast(doc.get(fieldName)) : null;
        }

        protected void setMainPriceField(Map<String, Object> doc, String baseName, String fieldGroup, BigDecimal value, String currencyUomId) {
            String fieldName = getMainPriceFieldName(baseName, fieldGroup);
            if (fieldName != null) {
                doc.put(fieldName, value + "," + currencyUomId);
            }
        }

        protected void setMainPriceField(Map<String, Object> doc, String baseName, String fieldGroup, Object value) {
            String fieldName = getMainPriceFieldName(baseName, fieldGroup);
            if (fieldName != null) {
                doc.put(fieldName, value);
            }
        }

        protected void setFloatPriceField(Map<String, Object> doc, String baseName, String fieldGroup, BigDecimal value, String currencyUomId) {
            String fieldName = getFloatPriceFieldName(baseName, fieldGroup);
            if (fieldName != null) {
                doc.put(fieldName, value.toString());
            }
        }

        protected void setFloatPriceField(Map<String, Object> doc, String baseName, String fieldGroup, Object value) {
            String fieldName = getFloatPriceFieldName(baseName, fieldGroup);
            if (fieldName != null) {
                doc.put(fieldName, value instanceof String ? value : value.toString());
            }
        }

        protected void setPriceFields(Map<String, Object> doc, String baseName, String fieldGroup, BigDecimal value, String currencyUomId) {
            setMainPriceField(doc, baseName, fieldGroup, value, currencyUomId);
            setFloatPriceField(doc, baseName, fieldGroup, value, currencyUomId);
        }

        protected void populateDocPriceStandardField(Map<String, Object> doc, Map<String, Object> priceMap,
                                                     String currencyUomId, String fieldGroup) throws GeneralException {
            BigDecimal defaultPrice = scaleCurrency(priceMap.get("defaultPrice"));
            if (defaultPrice != null) {
                setPriceFields(doc, "defaultPrice", fieldGroup, defaultPrice, currencyUomId);
            }

            BigDecimal listPrice = scaleCurrency(priceMap.get("listPrice"));
            if (listPrice != null) {
                setPriceFields(doc, "listPrice", fieldGroup, listPrice, currencyUomId);
            }

            BigDecimal promoPrice = scaleCurrency(priceMap.get("promoPrice"));
            if (promoPrice != null) {
                setPriceFields(doc, "promoPrice", fieldGroup, promoPrice, currencyUomId);
            }

            BigDecimal specialPromoPrice = scaleCurrency(priceMap.get("specialPromoPrice"));
            if (specialPromoPrice != null) {
                setPriceFields(doc, "specialPromoPrice", fieldGroup, specialPromoPrice, currencyUomId);
            }

            BigDecimal competitivePrice = scaleCurrency(priceMap.get("competitivePrice"));
            if (competitivePrice != null) {
                setPriceFields(doc, "competitivePrice", fieldGroup, competitivePrice, currencyUomId);
            }

            BigDecimal averageCost = scaleCurrency(priceMap.get("averageCost"));
            if (averageCost != null) {
                setPriceFields(doc, "averageCost", fieldGroup, averageCost, currencyUomId);
            }

            BigDecimal basePrice = scaleCurrency(priceMap.get("basePrice"));
            if (basePrice != null) {
                setPriceFields(doc, "basePrice", fieldGroup, basePrice, currencyUomId);
            }

            BigDecimal price = scaleCurrency(priceMap.get("price"));
            if (price != null) {
                setPriceFields(doc, "price", fieldGroup, price, currencyUomId);
            }
        }

        public void populateDocPriceConfigurable(Map<String, Object> doc) throws GeneralException {
            populateDocPriceConfigurableField(doc, getCfgPriceWrapper(), getCfgPriceWrapperCurrencyUomId(), "");
            for(Map.Entry<String, ProductConfigWrapper> entry : getStoreCfgPriceWrappers().entrySet()) {
                populateDocPriceConfigurableField(doc, entry.getValue(), getStoreCurrencyUomId(entry.getKey()), entry.getKey());
            }
        }

        protected void populateDocPriceConfigurableField(Map<String, Object> doc, ProductConfigWrapper pcw,
                                                         String currencyUomId, String fieldGroup) throws GeneralException {
            BigDecimal defaultPrice = scaleCurrency(pcw.getTotalPrice());
            if (defaultPrice != null) {
                setPriceFields(doc, "defaultPrice", fieldGroup, defaultPrice, currencyUomId);
            }

            BigDecimal listPrice = scaleCurrency(pcw.getTotalListPrice());
            // 2017-08-22: listPrice is NEVER null here - getTotalListPrice returns 0 if there was no list price - and
            // this creates 0$ list prices we can't validate in queries; this logic requires an extra check + ofbiz patch
            //if (listPrice != null) {
            if (listPrice != null && ((listPrice.compareTo(BigDecimal.ZERO) != 0) || pcw.hasOriginalListPrice())) {
                setPriceFields(doc, "listPrice", fieldGroup, listPrice, currencyUomId);
            }
        }

        public void populateDocInventory(Map<String, Object> doc) throws GeneralException {
            populateDocInventoryStock(doc);
            populateDocInventoryStatus(doc);
        }

        public void populateDocInventoryStock(Map<String, Object> doc) throws GeneralException {
            // WARN: here the total (inStock) behavior for variants is determined by the first store found only!
            for (Map.Entry<String, BigDecimal> entry : getProductStoreInventories().entrySet()) {
                if ("_total_".equals(entry.getKey())) {
                    doc.put("inStock", entry.getValue().toBigInteger().intValue());
                } else {
                    String fieldName = makeStoreStockFieldName(entry.getKey());
                    if (doc.containsKey(fieldName)) {
                        Debug.logError("Solr: DATA ERROR - DUPLICATE PRODUCT STORE storeStock_ VARIABLE DETECTED (" + fieldName
                                + ", for productStoreId '" + entry.getKey() + "') - productStoreId clash - Solr cannot index data for this store!"
                                + " This means that your system contains two ProductStores that have productStoreIds"
                                + " too similar so they cannot be uniquely represented in the Solr schema field names."
                                + " You will need to change the ID of one of the ProductStores and reindex using rebuildSolrIndex.", module);
                    } else {
                        int value = entry.getValue().toBigInteger().intValue();
                        doc.put(fieldName, value);
                    }
                }
            }
        }

        public String makeStoreStockFieldName(String productStoreId) {
            // TODO: change to _i (clients can override again for backward support)
            return "storeStock_" + SolrExprUtil.escapeFieldNamePart(productStoreId) + "_pi";
        }

        public void populateDocInventoryStatus(Map<String, Object> doc) throws GeneralException {
            Timestamp salesDiscDate = getSalesDiscDate();
            if (salesDiscDate != null) {
                doc.put("salesDiscDate_dt", salesDiscDate);
            }
        }

        public void populateDocContent(Map<String, Object> doc) throws GeneralException {
            populateDocContentInternal(doc);
            populateDocContentI18n(doc);
            populateDocContentKeywords(doc);
            populateDocContentImages(doc);
        }

        public void populateDocContentInternal(Map<String, Object> doc) throws GeneralException {
            String internalName = getInternalName();
            if (internalName != null) {
                doc.put("internalName", internalName);
            }
        }

        public void populateDocContentImages(Map<String, Object> doc) throws GeneralException {
            String smallImage = getSmallImage();
            if (smallImage != null) {
                doc.put("smallImage", smallImage);
            }
            String mediumImage = getMediumImage();
            if (mediumImage != null) {
                doc.put("mediumImage", mediumImage);
            }
            String largeImage = getLargeImage();
            if (largeImage != null) {
                doc.put("largeImage", largeImage);
            }
        }

        public void populateDocContentI18n(Map<String, Object> doc) throws GeneralException {
            addLocalizedContentStringMapToDoc(doc, "title_i18n_", "title_i18n_"+SolrLocaleUtil.I18N_GENERAL, getTitleLocaleMap(), getLangCodeFn(), SolrLocaleUtil.I18N_GENERAL);
            addLocalizedContentStringMapToDoc(doc, "description_i18n_", "description_i18n_"+SolrLocaleUtil.I18N_GENERAL, getDescriptionLocaleMap(), getLangCodeFn(), SolrLocaleUtil.I18N_GENERAL);
            addLocalizedContentStringMapToDoc(doc, "longdescription_i18n_", "longdescription_i18n_"+SolrLocaleUtil.I18N_GENERAL, getLongDescriptionLocaleMap(), getLangCodeFn(), SolrLocaleUtil.I18N_GENERAL);
        }

        public void populateDocContentKeywords(Map<String, Object> doc) throws GeneralException {
            Collection<String> keywords = getKeywords();
            if (UtilValidate.isNotEmpty(keywords)) {
                String keywordsString = String.join(" ", keywords);
                if (UtilValidate.isNotEmpty(keywordsString)) {
                    doc.put("keywords", keywordsString);
                }
            }
        }

        public void populateDocSort(Map<String, Object> doc) throws GeneralException {
            // FIXME?: Manual population of the alpha sort field required, because it's complex and not covered enough by schema
            addAlphaLocalizedContentStringMapToDoc(doc, "alphaTitleSort_", "alphaTitleSort_"+SolrLocaleUtil.I18N_GENERAL, "title_i18n_",
                    "title_i18n_"+SolrLocaleUtil.I18N_GENERAL, getTitleLocaleMap(), getLocales(), getDefaultLocale(), getLangCodeFn(), SolrLocaleUtil.I18N_GENERAL);
            for(GenericValue productCategoryMember : getProductCategoryMembers()) {
                BigDecimal sortPriority = productCategoryMember.getBigDecimal("sortPriority");
                if (sortPriority != null) {
                    String sortPrioFieldName = "categorySortPrio_" + SolrExprUtil.escapeFieldNamePart(productCategoryMember.getString("productCategoryId")) + "_d";
                    doc.put(sortPrioFieldName, sortPriority.doubleValue());
                }
            }
        }

        /** Optional method for custom client code, which may be used instead of overriding other methods. */
        public void populateDocCustom(Map<String, Object> doc) throws GeneralException {
        }

        /*
         * *****************************************************************
         * Product data getters
         * *****************************************************************
         */

        /** Returns moment this product's indexing started. */
        public Timestamp getMoment() {
            return moment;
        }

        public String getProductId() {
            return productId;
        }

        public GenericValue getProduct() throws GeneralException {
            GenericValue product = this.product;
            if (product == null) {
                product = getProductData().getProduct(getDctx(), getProductId(), isUseEntityCache());
                this.product = product;
            }
            return product;
        }

        public String getParentProductId() throws GeneralException {
            String parentProductId = this.parentProductId;
            if (parentProductId == null) {
                if (isVariant()) {
                    // IMPORTANT: same parent lookup logic as used by ProductContentWrapper
                    GenericValue assoc = getProductData().getParentProductAssoc(getDctx(), getProductId(), getMoment(), isUseEntityCache());
                    if (assoc != null) {
                        parentProductId = assoc.getString("productId");
                    } else {
                        // This does happen due to ALTERNATIVE_PACKAGE and maybe other reasons
                        //Debug.logError("Solr: getParentProductId: Product '" + getProductId() + "' is marked variant but has no recognized parent ProductAssoc", module);
                        parentProductId = "";
                    }
                } else {
                    parentProductId = "";
                }
                this.parentProductId = parentProductId;
            }
            return UtilValidate.isNotEmpty(parentProductId) ? parentProductId : null;
        }

        public List<GenericValue> getProductAssocFrom() throws GeneralException {
            List<GenericValue> productAssocFrom = this.productAssocFrom;
            if (productAssocFrom == null) {
                productAssocFrom = getProductData().getProductAssocFrom(getDctx(), getProductId(), getMoment(), isUseEntityCache());
                this.productAssocFrom = productAssocFrom;
            }
            return productAssocFrom;
        }

        public List<GenericValue> getProductAssocTo() throws GeneralException {
            List<GenericValue> productAssocTo = this.productAssocTo;
            if (productAssocTo == null) {
                productAssocTo = getProductData().getProductAssocTo(getDctx(), getProductId(), getMoment(), isUseEntityCache());
                this.productAssocTo = productAssocTo;
            }
            return productAssocTo;
        }

        public List<GenericValue> getProductAssocToVariant() throws GeneralException {
            List<GenericValue> productAssocToVariant = this.productAssocToVariant;
            if (productAssocToVariant == null) {
                if (isVariant()) {
                    productAssocToVariant = getProductData().getProductAssocToVariant(getDctx(), getProductId(), getMoment(), isUseEntityCache());
                }
                if (productAssocToVariant == null) {
                    productAssocToVariant = Collections.emptyList();
                }
                this.productAssocToVariant = productAssocToVariant;
            }
            return productAssocToVariant;
        }

        public List<GenericValue> getProductAssocFromVariant() throws GeneralException {
            List<GenericValue> productAssocFromVariant = this.productAssocFromVariant;
            if (productAssocFromVariant == null) {
                if (isVirtual()) {
                    productAssocFromVariant = EntityUtil.filterByAnd(getProductAssocFrom(), UtilMisc.toMap("productAssocTypeId", "PRODUCT_VARIANT"));
                }
                if (productAssocFromVariant == null) {
                    productAssocFromVariant = Collections.emptyList();
                }
                this.productAssocFromVariant = productAssocFromVariant;
            }
            return productAssocFromVariant;
        }

        public List<GenericValue> getProductCategoryMembers() throws GeneralException {
            List<GenericValue> productCategoryMembers = this.productCategoryMembers;
            if (productCategoryMembers == null) {
                productCategoryMembers = getProductData().getProductCategoryMembers(getDctx(), getProductId(), getMoment(), true, isUseEntityCache());
                this.productCategoryMembers = productCategoryMembers;
            }
            return productCategoryMembers;
        }

        public Collection<String> getOwnCategoryIds() throws GeneralException {
            Collection<String> ownCategoryIds = this.ownCategoryIds;
            if (ownCategoryIds == null) {
                ownCategoryIds = getProductData().getOwnCategoryIdsForProduct(getDctx(), getProductId(), getProductCategoryMembers());
                this.ownCategoryIds = UtilGenerics.cast(ownCategoryIds);
            }
            return ownCategoryIds;
        }

        public Collection<String> getAssocCategoryIds() throws GeneralException {
            Collection<String> assocCategoryIds = this.assocCategoryIds;
            if (assocCategoryIds == null) {
                assocCategoryIds = getProductData().getAssocCategoryIdsForProduct(getDctx(), getProductId(), getProductAssocToVariant(), getMoment(), true, isUseEntityCache());
                this.assocCategoryIds = UtilGenerics.cast(assocCategoryIds);
            }
            return assocCategoryIds;
        }

        public Collection<String> getCategoryIds() throws GeneralException { // FIXME: unused??
            Collection<String> categoryIds = this.categoryIds;
            if (categoryIds == null) {
                categoryIds = new LinkedHashSet<>(getOwnCategoryIds());
                categoryIds.addAll(getAssocCategoryIds());
                this.categoryIds = UtilGenerics.cast(categoryIds);
            }
            return categoryIds;
        }

        public Collection<String> getCatalogIds() throws GeneralException {
            Collection<String> catalogIds = this.catalogIds;
            if (catalogIds == null) {
                catalogIds = SolrCategoryUtil.getCatalogIdsFromCategoryTrails(new LinkedHashSet<>(), getDctx(), getProductData(), getCategoryTrails(), getMoment(), isUseEntityCache());
                determineRelatedCatalogIds(catalogIds);
                this.catalogIds = UtilGenerics.cast(catalogIds);
            }
            return catalogIds;
        }

        public String getCatalogId() throws GeneralException {
            Collection<String> catalogIds = getCatalogIds();
            return UtilValidate.isNotEmpty(catalogIds) ? catalogIds.iterator().next() : null;
        }

        protected void determineRelatedCatalogIds(Collection<String> catalogIds) throws GeneralException {
            if (catalogIds.isEmpty()) { // 2019-12: REMOVED: || productStores.isEmpty() -> if there's a catalog but not associated to a store, something is misconfigured
                // TODO: REVIEW: If we could not determine catalog directly, usually due to config, alternative package
                //  or other complex products, search product assoc categories to try to determine a catalog, so can get a store
                // NOTE: The found relatedCategoryIds are NOT added to the categoryIds in solr, must only be used to determine logical store/catalog
                Set<String> relatedCatalogIds = new LinkedHashSet<>();
                Set<String> relatedCategoryIds = new LinkedHashSet<>();
                Set<String> relatedTrails = new LinkedHashSet<>();
                // Self and virtuals are covered above
                Set<String> catCheckedProductIds = new LinkedHashSet<>(); // don't requery ProductCategoryMember for these already checked (productId and its virtual(s))
                catCheckedProductIds.add(getProductId());
                catCheckedProductIds.addAll(UtilMisc.getMapValuesForKey(getProductAssocToVariant(), "productId"));
                ProductWorker.getAnyRelatedCategoryIdsForProduct(relatedCategoryIds, new LinkedHashSet<>(), catCheckedProductIds, getDelegator(), getProductId(), getProduct(),
                        getProductAssocToVariant(), getProductAssocFrom(), getProductAssocTo(),false, getMoment(), true, isUseEntityCache()); // NOTE: firstFoundOnly==false
                SolrCategoryUtil.getCategoryTrails(relatedTrails, getDctx(), getProductData(), relatedCategoryIds, getMoment(), true, isUseEntityCache());
                SolrCategoryUtil.getCatalogIdsFromCategoryTrails(relatedCatalogIds, getDctx(), getProductData(), relatedTrails, getMoment(), isUseEntityCache());
                catalogIds.addAll(relatedCatalogIds);
                this.relatedCatalogIds = relatedCatalogIds;
                this.relatedCategoryIds = relatedCategoryIds;
                this.relatedTrails = relatedTrails;
            } else {
                relatedCatalogIds = Collections.emptySet();
                relatedCategoryIds = Collections.emptySet();
                relatedTrails = Collections.emptySet();
            }
        }

        /** WARN: May disappear in the future. */
        public Collection<String> getRelatedCategoryIds() throws GeneralException {
            getCatalogIds();
            return relatedCategoryIds;
        }

        /** WARN: May disappear in the future. */
        public Collection<String> getRelatedTrails() throws GeneralException {
            getCatalogIds();
            return relatedTrails;
        }

        /** WARN: May disappear in the future. */
        public Collection<String> getRelatedCatalogIds() throws GeneralException {
            getCatalogIds();
            return relatedCatalogIds;
        }

        public List<GenericValue> getProductStores() throws GeneralException {
            List<GenericValue> productStores = this.productStores;
            if (productStores == null) {
                Map<String, GenericValue> productStoreAndCatalogAssocMap = getProductStoreAndCatalogAssoc();
                productStores = new ArrayList<>(productStoreAndCatalogAssocMap.size());
                Set<String> storeIds = new HashSet<>();
                for (GenericValue storeAndAssoc : productStoreAndCatalogAssocMap.values()) {
                    String storeId = storeAndAssoc.getString("productStoreId");
                    if (!storeIds.contains(storeId)) {
                        GenericValue productStore = getProductData().getProductStore(getDctx(), storeId, isUseEntityCache());
                        productStores.add(productStore);
                        //productStores.add(storeAndAssoc);
                        storeIds.add(storeId);
                    }
                }
                this.productStores = productStores;
            }
            return productStores;
        }

        public Map<String, GenericValue> getProductStoreAndCatalogAssoc() throws GeneralException {
            Map<String, GenericValue> productStoreAndCatalogAssoc = this.productStoreAndCatalogAssoc;
            if (productStoreAndCatalogAssoc == null) {
                Collection<String> catalogs = getCatalogIds();
                if (!catalogs.isEmpty()) {
                    productStoreAndCatalogAssoc = new LinkedHashMap<>();
                    for (GenericValue psca : getProductData().getProductStoreAndCatalogAssocForCatalogIds(getDctx(),
                            catalogs, getMoment(), true, isUseEntityCache())) {
                        productStoreAndCatalogAssoc.put(psca.getString("productStoreId"), psca);
                    }
                } else {
                    productStoreAndCatalogAssoc = Collections.emptyMap();
                }
                this.productStoreAndCatalogAssoc = UtilGenerics.cast(productStoreAndCatalogAssoc);
            }
            return productStoreAndCatalogAssoc;
        }

        public Collection<String> getProductStoreIds() throws GeneralException {
            Collection<String> productStoreIds = this.productStoreIds;
            if (productStoreIds == null) {
                productStoreIds = new LinkedHashSet<>();
                for(GenericValue productStore : getProductStores()) {
                    productStoreIds.add(productStore.getString("productStoreId"));
                }
                this.productStoreIds = UtilGenerics.cast(productStoreIds);
            }
            return productStoreIds;
        }

        public GenericValue getProductStore() throws GeneralException {
            boolean productStoreChecked = this.productStoreChecked;
            if (!productStoreChecked) {
                productStore = getProductStore(getProductStores());
                productStoreChecked = true;
            }
            return productStore;
        }

        public String getProductStoreId() throws GeneralException {
            GenericValue productStore = getProductStore();
            return (productStore != null) ? productStore.getString("productStoreId") : null;
        }

        public GenericValue getProductStore(List<GenericValue> productStores) throws GeneralException {
            return ProductStoreWorker.getContentReferenceStoreOrFirst(productStores,
                    (SolrLocaleUtil.getConfiguredForceDefaultLocale(getDelegator()) == null || SolrProductUtil.getConfiguredForceDefaultCurrency(getDelegator()) == null)
                            ? ("product '" + getProductId() + "'") : null);
        }

        public GenericValue getProductStore(String productStoreId) throws GeneralException {
            return getProductStoreAndCatalogAssoc().get(productStoreId);
        }

        public Collection<String> getOwnCategoryTrails() throws GeneralException {
            Collection<String> ownCategoryTrails = this.ownCategoryTrails;
            if (ownCategoryTrails == null) {
                ownCategoryTrails = SolrCategoryUtil.getCategoryTrails(new LinkedHashSet<>(), getDctx(), getProductData(), getOwnCategoryIds(), getMoment(), true, isUseEntityCache());
                this.ownCategoryTrails = UtilGenerics.cast(ownCategoryTrails);
            }
            return ownCategoryTrails;
        }

        public Collection<String> getAssocCategoryTrails() throws GeneralException {
            Collection<String> assocCategoryTrails = this.assocCategoryTrails;
            if (assocCategoryTrails == null) {
                assocCategoryTrails = SolrCategoryUtil.getCategoryTrails(new LinkedHashSet<>(), getDctx(), getProductData(), getAssocCategoryIds(), getMoment(), true, isUseEntityCache());
                this.assocCategoryTrails = UtilGenerics.cast(assocCategoryTrails);
            }
            return assocCategoryTrails;
        }

        public Collection<String> getCategoryTrails() throws GeneralException {
            Collection<String> categoryTrails = this.categoryTrails;
            if (categoryTrails == null) {
                categoryTrails = new LinkedHashSet<>(getOwnCategoryTrails());
                categoryTrails.addAll(getAssocCategoryTrails());
                this.categoryTrails = UtilGenerics.cast(categoryTrails);
            }
            return categoryTrails;
        }

        public void putCategorySequenceNums(Map<String, Object> doc, String namePrefix, String nameSuffix) throws GeneralException {
            List<GenericValue> pcmList = getProductCategoryMembers();
            if (pcmList != null) {
                for(GenericValue pcm : pcmList) {
                    Long sequenceNum = pcm.getLong("sequenceNum");
                    if (sequenceNum != null) {
                        doc.put(namePrefix + SolrExprUtil.escapeFieldNamePart(pcm.getString("productCategoryId")) + nameSuffix, sequenceNum.intValue());
                    }
                }
            }
        }

        public String getProductTypeId() throws GeneralException {
            return getProduct().getString("productTypeId");
        }

        public boolean isVirtual() throws GeneralException {
            return "Y".equals(getProduct().getString("isVirtual"));
        }

        public boolean isVariant() throws GeneralException {
            return "Y".equals(getProduct().getString("isVariant"));
        }

        public boolean isDigital() throws GeneralException {
            Boolean digital = this.digital;
            if (digital == null) {
                digital = ProductWorker.isDigital(getProduct());
                this.digital = digital;
            }
            return digital;
        }

        public boolean isPhysical() throws GeneralException {
            Boolean physical = this.physical;
            if (physical == null) {
                physical = ProductWorker.isPhysical(getProduct());
                this.physical = physical;
            }
            return physical;
        }

        public boolean isRequireAmount() throws GeneralException {
            return Boolean.TRUE.equals(getProduct().getBoolean("requireAmount"));
        }

        public boolean isConfigurableProduct() throws GeneralException {
            String productTypeId = getProductTypeId();
            return "AGGREGATED".equals(productTypeId) || "AGGREGATED_SERVICE".equals(productTypeId);
        }

        public String getCurrencyUomId() throws GeneralException {
            String currencyUomId = this.currencyUomId;
            if (currencyUomId == null) {
                currencyUomId = getDefaultCurrency(getProductStore());
                this.currencyUomId = currencyUomId;
            }
            return currencyUomId;
        }

        public String getStoreCurrencyUomId(String productStoreId) throws GeneralException {
            return getStoreCurrencyUomId(getProductStore(productStoreId));
        }

        public String getStoreCurrencyUomId(GenericValue productStore) throws GeneralException {
            return getDefaultCurrency(productStore);
        }

        protected boolean isGetMinimumVariantPrice() {
            return true;
        }

        public Map<String, Object> getStdPriceMap() throws GeneralException {
            Map<String, Object> stdPriceMap = this.stdPriceMap;
            if (stdPriceMap == null && !isConfigurableProduct()) {
                String productStoreId = getProductStoreId();
                String prodCatalogId = getCatalogId();
                Map<String, Object> ovrdFields = addStoreStdPriceMapOvrdFields(new HashMap<>(getStdPriceMapOvrdFields()), productStoreId, prodCatalogId, true);
                if (ovrdFields.get("productStoreId") != null) { // compatibility
                    productStoreId = (String) ovrdFields.get("productStoreId"); // Support legacy client override here
                }
                this.stdPriceMapProductStoreId = productStoreId;
                GenericValue productStore = (productStoreId != null) ? getProductStore(productStoreId) : null;
                String currencyUomId = getStoreCurrencyUomId(productStore);
                this.stdPriceMapCurrencyUomId = currencyUomId;
                stdPriceMap = getProductData().getProductStandardPrices(getDctx(), getContext(), getUserLogin(), getProduct(),
                        productStore, prodCatalogId, currencyUomId, getBuilderLocale(), isUseEntityCache(), ovrdFields);
                if (!ServiceUtil.isSuccess(stdPriceMap)) {
                    Debug.logError("getProductStandardPrices: failed to get product prices for product '"
                            + getProduct().get("productId") + "': " + ServiceUtil.getErrorMessage(stdPriceMap), module);
                }
                this.stdPriceMap = stdPriceMap;
            }
            return stdPriceMap;
        }

        protected String getStdPriceMapProductStoreId() throws GeneralException { // compatibility
            getStdPriceMap();
            return stdPriceMapProductStoreId;
        }

        protected String getStdPriceMapCurrencyUomId() throws GeneralException { // compatibility
            getStdPriceMap();
            return stdPriceMapCurrencyUomId;
        }

        /**
         * @deprecated SCIPIO: 3.0.0: Only applies to default store
         */
        @Deprecated
        public Map<String, Object> getStdPriceMapOvrdFields() throws GeneralException {
            return Map.of();
        }

        protected Map<String, Object> addStoreStdPriceMapOvrdFields(Map<String, Object> context, String productStoreId, String prodCatalogId, boolean rootStore) throws GeneralException {
            context.put("getMinimumVariantPrice", isGetMinimumVariantPrice());
            return context;
        }

        public ProductConfigWrapper getCfgPriceWrapper() throws GeneralException {
            ProductConfigWrapper cfgPriceWrapper = this.cfgPriceWrapper;
            if (cfgPriceWrapper == null && isConfigurableProduct()) {
                String productStoreId = getProductStoreId();
                String prodCatalogId = getCatalogId();
                Map<String, Object> ovrdFields = addStoreCfgPriceWrapperOvrdFields(new HashMap<>(getCfgPriceWrapperOvrdFields()), productStoreId, prodCatalogId, true);
                if (ovrdFields.get("productStoreId") != null) { // NOTE: Deprecated/unnecessary most of the time
                    productStoreId = (String) ovrdFields.get("productStoreId");
                }
                if (ovrdFields.get("prodCatalogId") != null) { // NOTE: Deprecated/unnecessary most of the time
                    prodCatalogId = (String) ovrdFields.get("prodCatalogId");
                }
                this.cfgPriceWrapperProductStoreId = productStoreId;
                GenericValue productStore = (productStoreId != null) ? getProductStore(productStoreId) : null;
                String currencyUomId = getStoreCurrencyUomId(productStore);
                this.cfgPriceWrapperCurrencyUomId = currencyUomId;
                cfgPriceWrapper = getProductData().getConfigurableProductStartingPrices(getDctx(), getContext(), getUserLogin(), getProductId(),
                        productStoreId, prodCatalogId, currencyUomId, getBuilderLocale(), isUseEntityCache());
                this.cfgPriceWrapper = cfgPriceWrapper;
            }
            return cfgPriceWrapper;
        }

        protected String getCfgPriceWrapperProductStoreId() throws GeneralException { // compatibility
            getCfgPriceWrapper();
            return cfgPriceWrapperProductStoreId;
        }

        protected String getCfgPriceWrapperCurrencyUomId() throws GeneralException { // compatibility
            getCfgPriceWrapper();
            return cfgPriceWrapperCurrencyUomId;
        }

        /**
         * @deprecated SCIPIO: 3.0.0: Only applies to default store
         */
        @Deprecated
        public Map<String, Object> getCfgPriceWrapperOvrdFields() throws GeneralException {
            return Map.of();
        }

        protected Map<String, Object> addStoreCfgPriceWrapperOvrdFields(Map<String, Object> context, String productStoreId, String prodCatalogId, boolean rootStore) throws GeneralException {
            context.put("getMinimumVariantPrice", isGetMinimumVariantPrice()); // NOTE: currently ignored; for future use
            return context;
        }

        public Map<String, Map<String, Object>> getStorePriceFields() throws GeneralException {
            Map<String, Map<String, Object>> storePriceFields = this.storePriceFields;
            if (storePriceFields == null) {
                storePriceFields = new LinkedHashMap<>();
                Set<String> storeIds = new HashSet<>();
                // FIXME: This collects the first prodCatalogId for each store, meaning highest ProductStoreCatalog.sequenceNum
                //  Ideally this should do one for all prodCatalogId, but most parameters are by productStoreId
                for(GenericValue psc : getProductStoreAndCatalogAssoc().values()) {
                    String productStoreId = psc.getString("productStoreId");
                    if (!storeIds.contains(productStoreId)) {
                        storePriceFields.put(productStoreId, UtilMisc.toMap("productStoreId", productStoreId, "prodCatalogId", psc.get("prodCatalogId")));
                        storeIds.add(productStoreId);
                    }
                }
                this.storePriceFields = storePriceFields;
            }
            return storePriceFields;
        }

        public Map<String, Map<String, Object>> getStoreStdPriceMaps() throws GeneralException {
            Map<String, Map<String, Object>> storeStdPriceMaps = this.storeStdPriceMaps;
            if (storeStdPriceMaps == null && !isConfigurableProduct()) {
                storeStdPriceMaps = new LinkedHashMap<>();
                for(Map.Entry<String, Map<String, Object>> entry : getStorePriceFields().entrySet()) {
                    String productStoreId = entry.getKey();
                    String prodCatalogId = (String) entry.getValue().get("prodCatalogId");
                    GenericValue productStore = getProductStore(productStoreId);
                    Map<String, Object> ovrdFields = addStoreStdPriceMapOvrdFields(new HashMap<>(), productStoreId, prodCatalogId, false);
                    Map<String, Object> priceMap = getProductData().getProductStandardPrices(getDctx(), getContext(), getUserLogin(), getProduct(),
                            productStore, prodCatalogId, getStoreCurrencyUomId(productStore), getBuilderLocale(), isUseEntityCache(), ovrdFields);
                    if (!ServiceUtil.isSuccess(priceMap)) {
                        Debug.logError("getStoreStdPriceMaps: failed to get product prices for product ["
                                + getProduct().get("productId") + "] store [" + entry.getKey() + "]: " + ServiceUtil.getErrorMessage(priceMap), module);
                    } else {
                        storeStdPriceMaps.put(entry.getKey(), priceMap);
                    }
                }
                this.storeStdPriceMaps = storeStdPriceMaps;
            }
            return storeStdPriceMaps;
        }

        public Map<String, ProductConfigWrapper> getStoreCfgPriceWrappers() throws GeneralException {
            Map<String, ProductConfigWrapper> storeCfgPriceWrappers = this.storeCfgPriceWrappers;
            if (storeCfgPriceWrappers == null && isConfigurableProduct()) {
                storeCfgPriceWrappers = new LinkedHashMap<>();
                for(Map.Entry<String, Map<String, Object>> entry : getStorePriceFields().entrySet()) {
                    String productStoreId = entry.getKey();
                    String prodCatalogId = (String) entry.getValue().get("prodCatalogId");
                    //GenericValue productStore = getProductStore(productStoreId);
                    // ovrdFields unused here for now
                    //Map<String, Object> ovrdFields = addStoreCfgPriceWrapperOvrdFields(new HashMap<>(), productStoreId, prodCatalogId, false);
                    ProductConfigWrapper pcw = getProductData().getConfigurableProductStartingPrices(getDctx(), getContext(),
                            getUserLogin(), getProductId(), productStoreId, prodCatalogId, getStoreCurrencyUomId(productStoreId),
                            getBuilderLocale(), isUseEntityCache());
                    if (pcw == null) {
                        Debug.logError("getStoreCfgPriceWrappers: failed to get configurable prices for product ["
                                + getProduct().get("productId") + "] store [" + entry.getKey() + "]", module);
                    } else {
                        storeCfgPriceWrappers.put(entry.getKey(), pcw);
                    }
                }
                this.storeCfgPriceWrappers = storeCfgPriceWrappers;
            }
            return storeCfgPriceWrappers;
        }

        public Set<String> getFeatureSet() throws GeneralException {
            Set<String> featureSet = this.featureSet;
            if (featureSet == null) {
                Map<String, Object> featureSetResult = getDispatcher().runSync("getProductFeatureSet",
                        UtilMisc.toMap("productId", getProductId(), "emptyAction", "success", "useCache", isUseEntityCache()));
                featureSet = UtilGenerics.cast(featureSetResult.get("featureSet"));
                if (featureSet == null) {
                    featureSet = Collections.emptySet();
                }
                this.featureSet = featureSet;
            }
            return featureSet;
        }

        public boolean isUseVariantStockCalcForTotal() throws GeneralException {
            Boolean useVariantStockCalcForTotal = this.useVariantStockCalcForTotal;
            if (useVariantStockCalcForTotal == null) {
                useVariantStockCalcForTotal = ProductStoreWorker.isUseVariantStockCalc(getProductStore());
                this.useVariantStockCalcForTotal = useVariantStockCalcForTotal;
            }
            return useVariantStockCalcForTotal;
        }

        public Map<String, BigDecimal> getProductStoreInventories() throws GeneralException {
            Map<String, BigDecimal> productStoreInventories = this.productStoreInventories;
            if (productStoreInventories == null) {
                productStoreInventories = ProductWorker.getProductStockPerProductStore(getDelegator(), getDispatcher(), getProduct(),
                        getProductStores(), true, isUseVariantStockCalcForTotal(), getMoment(), isUseEntityCache());
                this.productStoreInventories = productStoreInventories;
            }
            return productStoreInventories;
        }

        public BigDecimal getProductStoreInventory(String productStoreId) throws GeneralException {
            return getProductStoreInventories().get(productStoreId);
        }

        public Timestamp getSalesDiscDate() throws GeneralException {
            return getProduct().getTimestamp("salesDiscontinuationDate");
        }

        public List<Locale> getLocales() throws GeneralException {
            List<Locale> locales = this.locales;
            if (locales == null) {
                locales = SolrLocaleUtil.getConfiguredLocales(getProductStore());
                this.locales = locales;
            }
            return locales;
        }

        public Locale getDefaultLocale() throws GeneralException {
            Locale defaultLocale = this.defaultLocale;
            if (defaultLocale == null) {
                defaultLocale = SolrLocaleUtil.getConfiguredDefaultLocale(getProductStore());
                this.defaultLocale = defaultLocale;
            }
            return defaultLocale;
        }

        public String getInternalName() throws GeneralException {
            return getProduct().getString("internalName");
        }

        public String getSmallImage() throws GeneralException {
            return getProduct().getString("smallImageUrl");
        }

        public String getMediumImage() throws GeneralException {
            return getProduct().getString("mediumImageUrl");
        }

        public String getLargeImage() throws GeneralException {
            return getProduct().getString("largeImageUrl");
        }

        /** Creates ProductContentWrapper for supported languages. */
        public List<ProductContentWrapper> getPcwList() throws GeneralException {
            List<ProductContentWrapper> pcwList = this.pcwList;
            if (pcwList == null) {
                pcwList = new ArrayList<>(getPcwMap().values());
                this.pcwList = pcwList;
            }
            return pcwList;
        }

        public Map<String, ProductContentWrapper> getPcwMap() throws GeneralException {
            Map<String, ProductContentWrapper> pcwMap = this.pcwMap;
            if (pcwMap == null) {
                pcwMap = getProductData().getProductContentWrappersForLocales(getDctx(), getProduct(), getLocales(), getDefaultLocale(), getLangCodeFn(), isUseEntityCache());
                this.pcwMap = pcwMap;
            }
            return pcwMap;
        }

        public List<GenericValue> getProductContent() throws GeneralException {
            List<GenericValue> productContent = this.productContent;
            if (productContent == null) {
                productContent = getProductData().getProductContent(getDctx(), getProductId(), getMoment(), isUseEntityCache());
                this.productContent = productContent;
            }
            return productContent;
        }

        public Map<String, String> getTitleLocaleMap() throws GeneralException {
            Map<String, String> titleLocaleMap = this.titleLocaleMap;
            if (titleLocaleMap == null) {
                titleLocaleMap = getProductData().getLocalizedProductContentStringMap(getDctx(), getProduct(), "PRODUCT_NAME", getLocales(), getDefaultLocale(), getLangCodeFn(), SolrLocaleUtil.I18N_GENERAL, getPcwList(), getMoment(), isUseEntityCache());
                this.titleLocaleMap = titleLocaleMap;
            }
            return titleLocaleMap;
        }

        public Map<String, String> getDescriptionLocaleMap() throws GeneralException {
            Map<String, String> descriptionLocaleMap = this.descriptionLocaleMap;
            if (descriptionLocaleMap == null) {
                descriptionLocaleMap = getProductData().getLocalizedProductContentStringMap(getDctx(), getProduct(), "DESCRIPTION", getLocales(), getDefaultLocale(), getLangCodeFn(), SolrLocaleUtil.I18N_GENERAL, getPcwList(), getMoment(), isUseEntityCache());
                this.descriptionLocaleMap = descriptionLocaleMap;
            }
            return descriptionLocaleMap;
        }

        public Map<String, String> getLongDescriptionLocaleMap() throws GeneralException {
            Map<String, String> longDescriptionLocaleMap = this.longDescriptionLocaleMap;
            if (longDescriptionLocaleMap == null) {
                longDescriptionLocaleMap = getProductData().getLocalizedProductContentStringMap(getDctx(), getProduct(), "LONG_DESCRIPTION", getLocales(), getDefaultLocale(), getLangCodeFn(), SolrLocaleUtil.I18N_GENERAL, getPcwList(), getMoment(), isUseEntityCache());
                this.longDescriptionLocaleMap = longDescriptionLocaleMap;
            }
            return longDescriptionLocaleMap;
        }

        /** Gets product keywords. NOTE: for variant products, we also include the keywords from the virtual/parent. */
        public Set<String> getKeywords() throws GeneralException {
            Set<String> keywords = this.keywords;
            if (keywords == null) {
                keywords = getProductData().getProductKeywords(new LinkedHashSet<>(), getDelegator(), isUseEntityCache(), getProductId(), getParentProductId());
                this.keywords = keywords;
            }
            return keywords;
        }
    }
}
