package com.ilscipio.scipio.solr;

import com.ilscipio.scipio.product.product.ProductDataCache;
import com.ilscipio.scipio.product.product.ProductDataReader;
import org.apache.solr.common.SolrInputDocument;
import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.UtilDateTime;
import org.ofbiz.base.util.UtilGenerics;
import org.ofbiz.base.util.UtilMisc;
import org.ofbiz.base.util.UtilProperties;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.entity.Delegator;
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
 * <p>Main method: {@link #makeProductMapDoc(GenericValue)}</p>
 */
public class SolrProductIndexer {
    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());

    protected static final Factory DEFAULT_FACTORY = new Factory();
    protected static final Factory CONFIGURED_FACTORY = readConfiguredFactory();
    protected static final boolean USE_INDEXER_CACHE = UtilProperties.getPropertyAsBoolean(SolrUtil.solrConfigName, "solr.index.rebuild.indexerCache.enable", true);
    protected static final int INDEXER_CACHE_MAX_PRODUCTS = UtilProperties.getPropertyAsInteger(SolrUtil.solrConfigName, "solr.index.rebuild.indexerCache.productCacheSize",
            UtilProperties.getPropertyAsInteger(SolrUtil.solrConfigName, "solr.index.rebuild.record.buffer.size", 1000));

    private DispatchContext dctx;
    private Map<String, Object> serviceContext;
    private GenericValue userLogin;
    private ProductDataReader productDataReader;
    private boolean useEntityCache;
    private long startTime = System.currentTimeMillis();

    protected SolrProductIndexer(DispatchContext dctx, Map<String, Object> serviceContext, ProductDataReader productDataReader) {
        this.dctx = dctx;
        this.serviceContext = serviceContext;
        this.userLogin = (GenericValue) serviceContext.get("userLogin");
        this.productDataReader = productDataReader;
        this.useEntityCache = isUseEntityCache(serviceContext);
    }

    protected SolrProductIndexer(DispatchContext dctx, Map<String, Object> serviceContext) {
        this(dctx, serviceContext, DEFAULT_FACTORY.getProductDataReader(dctx, serviceContext));
    }

    /*
     * *************************************************************
     * Factory
     * *************************************************************
     */

    public static SolrProductIndexer getInstance(DispatchContext dctx, Map<String, Object> serviceContext) {
        return getFactory(dctx).getIndexer(dctx, serviceContext);
    }

    public static Factory getFactory(DispatchContext dctx) {
        return CONFIGURED_FACTORY;
    }

    protected static Factory readConfiguredFactory() {
        String factoryClassName = UtilProperties.getPropertyValue(SolrUtil.solrConfigName, "solr.index.indexer.factoryClass", Factory.class.getName());
        try {
            Class<Factory> factoryClass = UtilGenerics.cast(SolrProductIndexer.class.getClassLoader().loadClass(factoryClassName));
            return factoryClass.newInstance();
        } catch (Exception e) {
            Debug.logError(e, "Error loading indexer cache from " + SolrUtil.solrConfigName + "#solr.index.indexer.factoryClass", module);
            return DEFAULT_FACTORY;
        }
    }

    public static class Factory {
        public SolrProductIndexer getIndexer(DispatchContext dctx, Map<String, Object> serviceContext) {
            return new SolrProductIndexer(dctx, serviceContext, getProductDataReader(dctx, serviceContext));
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
    public Map<String, Object> getServiceContext() {
        return serviceContext;
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

    /** Returns the solr core intended for use - should be derived from {@link #getServiceContext()} and often null. */
    public String getCore() {
        return (String) getServiceContext().get("core");
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

    protected BigDecimal scaleCurrency(BigDecimal amount) {
        return amount.setScale(2, RoundingMode.HALF_UP);
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
     * General document handling and building
     * *************************************************************
     */

    public String getDocId(Object doc) {
        if (doc instanceof SolrInputDocument) {
            return getDocId((SolrInputDocument) doc);
        } else {
            return getDocId(UtilGenerics.<Map<String, Object>>cast(doc));
        }
    }

    public String getDocId(Map<String, Object> doc) {
        return (String) doc.get("id");
    }

    public String getDocId(SolrInputDocument doc) {
        return (String) doc.getFieldValue("id");
    }

    public SolrInputDocument getSolrDoc(Object doc) {
        return (doc instanceof SolrInputDocument) ? (SolrInputDocument) doc : makeSolrDoc(UtilGenerics.<Map<String, Object>>cast(doc));
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
     * Generates a document in map form of product content that may be passed to the {@link SolrProductSearch#addListToSolrIndex} or {@link #makeSolrDoc(Map)} method/service.
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
     */
    public Map<String, Object> makeProductMapDoc(GenericValue product) throws Exception {
        ProductDocBuilder productDocBuilder = makeProductDocBuilder(product);
        if (SolrUtil.verboseOn()) {
            Debug.logInfo("Solr: makeProductMapDoc: Getting product content for product '" + productDocBuilder.getProductId() + "'", module);
        }
        return productDocBuilder.populateDoc(new HashMap<>());
    }

    /**
     * Generates a document in SolrInputDocument form of product content.
     * @see #makeProductMapDoc(GenericValue)
     */
    public SolrInputDocument makeProductSolrDoc(GenericValue product) throws Exception {
        return makeSolrDoc(makeProductMapDoc(product));
    }

    /** Creates a new product reader instance for doc map generation. Client code should override. */
    public ProductDocBuilder makeProductDocBuilder(GenericValue product) {
        return new ProductDocBuilder(product);
    }

    /**
     * Records and caches product info during call to {@link #makeProductMapDoc(GenericValue)}.
     * <p>Intended for client code to subclass and override any method as needed - many factoring points provided.</p>
     * <p></p>Written to minimize entity lookups.</p>
     * <p>NOTE: These use mutable pattern to prevent multiple reads automatically and without effort.
     * Client code is not obliged to follow these patterns for additional code added.</p>
     * <p>NOTE: Further caching is done underneath by {@link ProductDataCache} from {@link #getProductData()}.</p>
     */
    public class ProductDocBuilder {
        protected final Timestamp moment;
        protected final GenericValue product;
        protected final String productId;
        protected String parentProductId; // empty string means already looked up

        protected List<GenericValue> productAssocFrom;
        protected List<GenericValue> productAssocTo;
        protected List<GenericValue> productAssocToVariant;
        protected List<GenericValue> productAssocFromVariant;

        protected Set<String> ownCategoryIds;
        protected Set<String> assocCategoryIds;
        protected Set<String> categoryIds;

        protected Set<String> ownCategoryTrails;
        protected Set<String> assocCategoryTrails;
        protected Set<String> categoryTrails;

        protected Set<String> catalogIds;
        protected List<GenericValue> productStores;
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
        protected ProductConfigWrapper cfgPriceWrapper;

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

        protected ProductDocBuilder(GenericValue product) {
            this.product = product;
            this.productId = product.getString("productId");
            this.moment = UtilDateTime.nowTimestamp();
        }

        /*
         * *************************************************************
         * Product map document production - populateDoc
         * *************************************************************
         */

        /** Produces a solr document in the form of a map. Trivially converted to SolrInputDocument later using {@link #makeSolrDoc(Map)}. */
        public Map<String, Object> populateDoc(Map<String, Object> doc) throws Exception {
            populateDocMeta(doc);
            populateDocType(doc);
            populateDocPrice(doc);
            populateDocInventory(doc);
            populateDocContent(doc);
            populateDocSort(doc);
            populateDocCustom(doc);
            return doc;
        }

        public void populateDocMeta(Map<String, Object> doc) throws Exception {
            doc.put("id", getProductId());
            doc.put("productId", getProductId());
            doc.put("productStore", getProductStoreIds());
            doc.put("cat", getCategoryTrails());
            doc.put("ownCat_ss", getOwnCategoryTrails());
            doc.put("catalog", getCatalogIds());
            checkStoresAndCatalogs();
        }

        public void checkStoresAndCatalogs() throws Exception {
            List<GenericValue> productStores = getProductStores();
            Collection<String> catalogIds = getCatalogIds();
            if (productStores.isEmpty()) {
                if (catalogIds.isEmpty()) {
                    Debug.logInfo("Solr: Could not determine store for product '" + getProductId() + "'; no catalogs", module);
                } else {
                    Debug.logInfo("Solr: Could not determine store for product '" + getProductId() + "' from catalogs: " + catalogIds, module);
                }
            } else {
                if (SolrUtil.verboseOn()) {
                    if (UtilValidate.isNotEmpty(getRelatedCatalogIds())) {
                        Debug.logInfo("Solr: Determined store(s) for product '" + getProductId() + "' indirectly (" + getProductStoreIds() + ") from related catalogs: " + catalogIds, module);
                    } else {
                        Debug.logInfo("Solr: Determined store(s) for product '" + getProductId() + "' directly (" + getProductStoreIds() + ") from catalogs: " + catalogIds, module);
                    }
                }
            }
        }

        public void populateDocType(Map<String, Object> doc) throws Exception {
            populateDocTypeBasic(doc);
            populateDocTypeFeatures(doc);
        }

        public void populateDocTypeBasic(Map<String, Object> doc) throws Exception {
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
        }

        public void populateDocTypeFeatures(Map<String, Object> doc) throws Exception {
            Collection<String> featureSet = getFeatureSet();
            if (featureSet != null) {
                doc.put("features", featureSet);
            }
        }

        public void populateDocPrice(Map<String, Object> doc) throws Exception {
            if (isConfigurableProduct()) {
                populateDocPriceConfigurable(doc);
            } else {
                populateDocPriceStandard(doc);
            }
        }

        public void populateDocPriceStandard(Map<String, Object> doc) throws Exception {
            Map<String, Object> priceMap = getStdPriceMap();
            if (priceMap.get("listPrice") != null) {
                String listPrice = scaleCurrency((BigDecimal) priceMap.get("listPrice")).toString();
                doc.put("listPrice", listPrice);
            }
            if (priceMap.get("defaultPrice") != null) {
                String defaultPrice = scaleCurrency((BigDecimal) priceMap.get("defaultPrice")).toString();
                if (defaultPrice != null) {
                    doc.put("defaultPrice", defaultPrice);
                }
            }
        }

        public void populateDocPriceConfigurable(Map<String, Object> doc) throws Exception {
            ProductConfigWrapper pcw = getCfgPriceWrapper();
            BigDecimal listPrice = pcw.getTotalListPrice();
            // 2017-08-22: listPrice is NEVER null here - getTotalListPrice returns 0 if there was no list price - and
            // this creates 0$ list prices we can't validate in queries; this logic requires an extra check + ofbiz patch
            //if (listPrice != null) {
            if (listPrice != null && ((listPrice.compareTo(BigDecimal.ZERO) != 0) || pcw.hasOriginalListPrice())) {
                doc.put("listPrice", scaleCurrency(listPrice).toString());
            }
            BigDecimal defaultPrice = pcw.getTotalPrice();
            if (defaultPrice != null) {
                doc.put("defaultPrice", scaleCurrency(defaultPrice).toString());
            }
        }

        public void populateDocInventory(Map<String, Object> doc) throws Exception {
            populateDocInventoryStock(doc);
            populateDocInventoryStatus(doc);
        }

        public void populateDocInventoryStock(Map<String, Object> doc) throws Exception {
            // WARN: here the total (inStock) behavior for variants is determined by the first store found only!
            for (Map.Entry<String, BigDecimal> entry : getProductStoreInventories().entrySet()) {
                if ("_total_".equals(entry.getKey())) {
                    doc.put("inStock", entry.getValue().toBigInteger().intValue());
                } else {
                    String fieldName = "storeStock_" + SolrExprUtil.escapeFieldNamePart(entry.getKey()) + "_pi";
                    if (doc.containsKey(fieldName)) {
                        Debug.logError("Solr: DATA ERROR - DUPLICATE PRODUCT STORE storeStock_ VARIABLE DETECTED (" + fieldName
                                + ", for productStoreId '" + entry.getKey() + "') - productStoreId clash - Solr cannot index data for this store!"
                                + " This means that your system contains two ProductStores that have productStoreIds"
                                + " too similar so they cannot be uniquely represented in the Solr schema field names."
                                + " You will need to change the ID of one of the ProductStores and reindex using rebuildSolrIndex.", module);
                    } else {
                        doc.put(fieldName, entry.getValue().toBigInteger().intValue());
                    }
                }
            }
        }

        public void populateDocInventoryStatus(Map<String, Object> doc) throws Exception {
            Timestamp salesDiscDate = getSalesDiscDate();
            if (salesDiscDate != null) {
                doc.put("salesDiscDate_dt", salesDiscDate);
            }
        }

        public void populateDocContent(Map<String, Object> doc) throws Exception {
            populateDocContentInternal(doc);
            populateDocContentI18n(doc);
            populateDocContentKeywords(doc);
            populateDocContentImages(doc);
        }

        public void populateDocContentInternal(Map<String, Object> doc) throws Exception {
            String internalName = getInternalName();
            if (internalName != null) {
                doc.put("internalName", internalName);
            }
        }

        public void populateDocContentImages(Map<String, Object> doc) throws Exception {
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

        public void populateDocContentI18n(Map<String, Object> doc) throws Exception {
            addLocalizedContentStringMapToDoc(doc, "title_i18n_", "title_i18n_"+SolrLocaleUtil.I18N_GENERAL, getTitleLocaleMap(), getLangCodeFn(), SolrLocaleUtil.I18N_GENERAL);
            addLocalizedContentStringMapToDoc(doc, "description_i18n_", "description_i18n_"+SolrLocaleUtil.I18N_GENERAL, getDescriptionLocaleMap(), getLangCodeFn(), SolrLocaleUtil.I18N_GENERAL);
            addLocalizedContentStringMapToDoc(doc, "longdescription_i18n_", "longdescription_i18n_"+SolrLocaleUtil.I18N_GENERAL, getLongDescriptionLocaleMap(), getLangCodeFn(), SolrLocaleUtil.I18N_GENERAL);
        }

        public void populateDocContentKeywords(Map<String, Object> doc) throws Exception {
            Collection<String> keywords = getKeywords();
            if (UtilValidate.isNotEmpty(keywords)) {
                String keywordsString = String.join(" ", keywords);
                if (UtilValidate.isNotEmpty(keywordsString)) {
                    doc.put("keywords", keywordsString);
                }
            }
        }

        public void populateDocSort(Map<String, Object> doc) throws Exception {
            // FIXME?: Manual population of the alpha sort field required, because it's complex and not covered enough by schema
            addAlphaLocalizedContentStringMapToDoc(doc, "alphaTitleSort_", "alphaTitleSort_"+SolrLocaleUtil.I18N_GENERAL, "title_i18n_",
                    "title_i18n_"+SolrLocaleUtil.I18N_GENERAL, getTitleLocaleMap(), getLocales(), getDefaultLocale(), getLangCodeFn(), SolrLocaleUtil.I18N_GENERAL);
        }

        /** Optional method for custom client code, which may be used instead of overriding other methods. */
        public void populateDocCustom(Map<String, Object> doc) throws Exception {
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

        public GenericValue getProduct() {
            return product;
        }

        public String getProductId() {
            return productId;
        }

        public String getParentProductId() throws Exception {
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
            }
            return UtilValidate.isNotEmpty(parentProductId) ? parentProductId : null;
        }

        public List<GenericValue> getProductAssocFrom() throws Exception {
            if (productAssocFrom == null) {
                productAssocFrom = getProductData().getProductAssocFrom(getDctx(), getProductId(), getMoment(), isUseEntityCache());
            }
            return productAssocFrom;
        }

        public List<GenericValue> getProductAssocTo() throws Exception {
            if (productAssocTo == null) {
                productAssocTo = getProductData().getProductAssocTo(getDctx(), getProductId(), getMoment(), isUseEntityCache());
            }
            return productAssocTo;
        }

        public List<GenericValue> getProductAssocToVariant() throws Exception {
            if (productAssocToVariant == null) {
                if (isVariant()) {
                    productAssocToVariant = getProductData().getProductAssocToVariant(getDctx(), getProductId(), getMoment(), isUseEntityCache());
                }
                if (productAssocToVariant == null) {
                    productAssocToVariant = Collections.emptyList();
                }
            }
            return productAssocToVariant;
        }

        public List<GenericValue> getProductAssocFromVariant() throws Exception {
            if (productAssocFromVariant == null) {
                if (isVirtual()) {
                    productAssocFromVariant = EntityUtil.filterByAnd(getProductAssocFrom(), UtilMisc.toMap("productAssocTypeId", "PRODUCT_VARIANT"));
                }
                if (productAssocFromVariant == null) {
                    productAssocFromVariant = Collections.emptyList();
                }
            }
            return productAssocFromVariant;
        }

        public Collection<String> getOwnCategoryIds() throws Exception {
            if (ownCategoryIds == null) {
                ownCategoryIds = getProductData().getOwnCategoryIdsForProduct(getDctx(), getProductId(), getMoment(), true, isUseEntityCache());
            }
            return ownCategoryIds;
        }

        public Collection<String> getAssocCategoryIds() throws Exception {
            if (assocCategoryIds == null) {
                assocCategoryIds = getProductData().getAssocCategoryIdsForProduct(getDctx(), getProductId(), getProductAssocToVariant(), getMoment(), true, isUseEntityCache());
            }
            return assocCategoryIds;
        }

        public Collection<String> getCategoryIds() throws Exception { // FIXME: unused??
            if (categoryIds == null) {
                categoryIds = new LinkedHashSet<>(getOwnCategoryIds());
                categoryIds.addAll(getAssocCategoryIds());
            }
            return categoryIds;
        }

        public Collection<String> getCatalogIds() throws Exception {
            if (catalogIds == null) {
                catalogIds = SolrCategoryUtil.getCatalogIdsFromCategoryTrails(new LinkedHashSet<>(), getDctx(), getProductData(), getCategoryTrails(), getMoment(), isUseEntityCache());
                determineRelatedCatalogIds(catalogIds);
            }
            return catalogIds;
        }

        public String getCatalogId() throws Exception {
            Collection<String> catalogIds = getCatalogIds();
            return UtilValidate.isNotEmpty(catalogIds) ? catalogIds.iterator().next() : null;
        }

        protected void determineRelatedCatalogIds(Collection<String> catalogIds) throws Exception {
            if (catalogIds.isEmpty()) { // 2019-12: REMOVED: || productStores.isEmpty() -> if there's a catalog but not associated to a store, something is misconfigured
                // TODO: REVIEW: If we could not determine catalog directly, usually due to config, alternative package
                //  or other complex products, search product assoc categories to try to determine a catalog, so can get a store
                // NOTE: The found relatedCategoryIds are NOT added to the categoryIds in solr, must only be used to determine logical store/catalog
                relatedCatalogIds = new LinkedHashSet<>();
                relatedCategoryIds = new LinkedHashSet<>();
                relatedTrails = new LinkedHashSet<>();
                // Self and virtuals are covered above
                Set<String> catCheckedProductIds = new LinkedHashSet<>(); // don't requery ProductCategoryMember for these already checked (productId and its virtual(s))
                catCheckedProductIds.add(getProductId());
                catCheckedProductIds.addAll(UtilMisc.getMapValuesForKey(getProductAssocToVariant(), "productId"));
                ProductWorker.getAnyRelatedCategoryIdsForProduct(relatedCategoryIds, new LinkedHashSet<>(), catCheckedProductIds, getDelegator(), getProductId(), getProduct(),
                        getProductAssocToVariant(), getProductAssocFrom(), getProductAssocTo(),false, getMoment(), true, isUseEntityCache()); // NOTE: firstFoundOnly==false
                SolrCategoryUtil.getCategoryTrails(relatedTrails, getDctx(), getProductData(), relatedCategoryIds, getMoment(), true, isUseEntityCache());
                SolrCategoryUtil.getCatalogIdsFromCategoryTrails(relatedCatalogIds, getDctx(), getProductData(), relatedTrails, getMoment(), isUseEntityCache());
                catalogIds.addAll(relatedCatalogIds);
            } else {
                relatedCatalogIds = Collections.emptySet();
                relatedCategoryIds = Collections.emptySet();
                relatedTrails = Collections.emptySet();
            }
        }

        /** WARN: May disappear in the future. */
        public Collection<String> getRelatedCategoryIds() throws Exception {
            getCatalogIds();
            return relatedCategoryIds;
        }

        /** WARN: May disappear in the future. */
        public Collection<String> getRelatedTrails() throws Exception {
            getCatalogIds();
            return relatedTrails;
        }

        /** WARN: May disappear in the future. */
        public Collection<String> getRelatedCatalogIds() throws Exception {
            getCatalogIds();
            return relatedCatalogIds;
        }

        public List<GenericValue> getProductStores() throws Exception {
            if (productStores == null) {
                Collection<String> catalogs = getCatalogIds();
                if (!catalogs.isEmpty()) {
                    productStores = EntityUtil.orderBy(getProductData().getProductStoresForCatalogIds(getDctx(), catalogs, getMoment(), true, isUseEntityCache()),
                            UtilMisc.toList("defaultPriority"));
                } else {
                    productStores = Collections.emptyList();
                }
            }
            return productStores;
        }

        public Collection<String> getProductStoreIds() throws Exception {
            if (productStoreIds == null) {
                productStoreIds = new LinkedHashSet<>();
                for(GenericValue productStore : getProductStores()) {
                    productStoreIds.add(productStore.getString("productStoreId"));
                }
            }
            return productStoreIds;
        }

        public GenericValue getProductStore() throws Exception {
            if (!productStoreChecked) {
                productStore = getProductStore(getProductStores());
                productStoreChecked = true;
            }
            return productStore;
        }

        public String getProductStoreId() throws Exception {
            GenericValue productStore = getProductStore();
            return (productStore != null) ? productStore.getString("productStoreId") : null;
        }

        public GenericValue getProductStore(List<GenericValue> productStores) {
            return ProductStoreWorker.getContentReferenceStoreOrFirst(productStores,
                    (SolrLocaleUtil.getConfiguredForceDefaultLocale(getDelegator()) == null || SolrProductUtil.getConfiguredForceDefaultCurrency(getDelegator()) == null)
                            ? ("product '" + getProductId() + "'") : null);
        }

        public Collection<String> getOwnCategoryTrails() throws Exception {
            if (ownCategoryTrails == null) {
                ownCategoryTrails = SolrCategoryUtil.getCategoryTrails(new LinkedHashSet<>(), getDctx(), getProductData(), getOwnCategoryIds(), getMoment(), true, isUseEntityCache());
            }
            return ownCategoryTrails;
        }

        public Collection<String> getAssocCategoryTrails() throws Exception {
            if (assocCategoryTrails == null) {
                assocCategoryTrails = SolrCategoryUtil.getCategoryTrails(new LinkedHashSet<>(), getDctx(), getProductData(), getAssocCategoryIds(), getMoment(), true, isUseEntityCache());
            }
            return assocCategoryTrails;
        }

        public Collection<String> getCategoryTrails() throws Exception {
            if (categoryTrails == null) {
                categoryTrails = new LinkedHashSet<>(getOwnCategoryTrails());
                categoryTrails.addAll(getAssocCategoryTrails());
            }
            return categoryTrails;
        }

        public String getProductTypeId() throws Exception {
            return getProduct().getString("productTypeId");
        }

        public boolean isVirtual() throws Exception {
            return "Y".equals(getProduct().getString("isVirtual"));
        }

        public boolean isVariant() throws Exception {
            return "Y".equals(getProduct().getString("isVariant"));
        }

        public boolean isDigital() throws Exception {
            if (digital == null) {
                digital = ProductWorker.isDigital(getProduct());
            }
            return digital;
        }

        public boolean isPhysical() throws Exception {
            if (physical == null) {
                physical = ProductWorker.isPhysical(getProduct());
            }
            return physical;
        }

        public boolean isRequireAmount() throws Exception {
            return Boolean.TRUE.equals(getProduct().getBoolean("requireAmount"));
        }

        public boolean isConfigurableProduct() throws Exception {
            String productTypeId = getProductTypeId();
            return "AGGREGATED".equals(productTypeId) || "AGGREGATED_SERVICE".equals(productTypeId);
        }

        public String getCurrencyUomId() throws Exception {
            if (currencyUomId == null) {
                currencyUomId = getDefaultCurrency(getProductStore());
            }
            return currencyUomId;
        }

        public Map<String, Object> getStdPriceMap() throws Exception {
            if (stdPriceMap == null && !isConfigurableProduct()) {
                stdPriceMap = getProductData().getProductStandardPrices(getDctx(), getServiceContext(), getUserLogin(), getProduct(),
                        getProductStore(), getCurrencyUomId(), getDefaultLocale(), isUseEntityCache());
                if (!ServiceUtil.isSuccess(stdPriceMap)) {
                    Debug.logError("Solr: getProductStandardPrices: failed to get product prices for product '"
                            + getProduct().get("productId") + "': " + ServiceUtil.getErrorMessage(stdPriceMap), module);
                }
            }
            return stdPriceMap;
        }

        public ProductConfigWrapper getCfgPriceWrapper() throws Exception {
            if (cfgPriceWrapper == null && isConfigurableProduct()) {
                cfgPriceWrapper = getProductData().getConfigurableProductStartingPrices(getDctx(), getServiceContext(), getUserLogin(), getProduct(),
                        getProductStore(), getCurrencyUomId(), getDefaultLocale(), isUseEntityCache());
            }
            return cfgPriceWrapper;
        }

        public Set<String> getFeatureSet() throws Exception {
            if (featureSet == null) {
                Map<String, Object> featureSetResult = getDispatcher().runSync("getProductFeatureSet",
                        UtilMisc.toMap("productId", getProductId(), "emptyAction", "success", "useCache", isUseEntityCache()));
                featureSet = UtilGenerics.cast(featureSetResult.get("featureSet"));
                if (featureSet == null) {
                    featureSet = Collections.emptySet();
                }
            }
            return featureSet;
        }

        public boolean isUseVariantStockCalcForTotal() throws Exception {
            if (useVariantStockCalcForTotal == null) {
                useVariantStockCalcForTotal = ProductStoreWorker.isUseVariantStockCalc(getProductStore());
            }
            return useVariantStockCalcForTotal;
        }

        public Map<String, BigDecimal> getProductStoreInventories() throws Exception {
            if (productStoreInventories == null) {
                productStoreInventories = ProductWorker.getProductStockPerProductStore(getDelegator(), getDispatcher(), getProduct(),
                        getProductStores(), true, isUseVariantStockCalcForTotal(), getMoment(), isUseEntityCache());
            }
            return productStoreInventories;
        }

        public Timestamp getSalesDiscDate() throws Exception {
            return getProduct().getTimestamp("salesDiscontinuationDate");
        }

        public List<Locale> getLocales() throws Exception {
            if (locales == null) {
                locales = SolrLocaleUtil.getConfiguredLocales(getProductStore());
            }
            return locales;
        }

        public Locale getDefaultLocale() throws Exception {
            if (defaultLocale == null) {
                defaultLocale = SolrLocaleUtil.getConfiguredDefaultLocale(getProductStore());
            }
            return defaultLocale;
        }

        public String getInternalName() throws Exception {
            return getProduct().getString("internalName");
        }

        public String getSmallImage() throws Exception {
            return getProduct().getString("smallImageUrl");
        }

        public String getMediumImage() throws Exception {
            return getProduct().getString("mediumImageUrl");
        }

        public String getLargeImage() throws Exception {
            return getProduct().getString("largeImageUrl");
        }

        /** Creates ProductContentWrapper for supported languages. */
        public List<ProductContentWrapper> getPcwList() throws Exception {
            if (pcwList == null) {
                pcwList = new ArrayList<>(getPcwMap().values());
            }
            return pcwList;
        }

        public Map<String, ProductContentWrapper> getPcwMap() throws Exception {
            if (pcwMap == null) {
                pcwMap = getProductData().getProductContentWrappersForLocales(getDctx(), getProduct(), getLocales(), getDefaultLocale(), getLangCodeFn(), isUseEntityCache());
            }
            return pcwMap;
        }

        public List<GenericValue> getProductContent() throws Exception {
            if (productContent == null) {
                productContent = getProductData().getProductContent(getDctx(), getProductId(), getMoment(), isUseEntityCache());
            }
            return productContent;
        }

        public Map<String, String> getTitleLocaleMap() throws Exception {
            if (titleLocaleMap == null) {
                titleLocaleMap = getProductData().getLocalizedProductContentStringMap(getDctx(), getProduct(), "PRODUCT_NAME", getLocales(), getDefaultLocale(), getLangCodeFn(), SolrLocaleUtil.I18N_GENERAL, getPcwList(), getMoment(), isUseEntityCache());
            }
            return titleLocaleMap;
        }

        public Map<String, String> getDescriptionLocaleMap() throws Exception {
            if (descriptionLocaleMap == null) {
                descriptionLocaleMap = getProductData().getLocalizedProductContentStringMap(getDctx(), getProduct(), "DESCRIPTION", getLocales(), getDefaultLocale(), getLangCodeFn(), SolrLocaleUtil.I18N_GENERAL, getPcwList(), getMoment(), isUseEntityCache());
            }
            return descriptionLocaleMap;
        }

        public Map<String, String> getLongDescriptionLocaleMap() throws Exception {
            if (longDescriptionLocaleMap == null) {
                longDescriptionLocaleMap = getProductData().getLocalizedProductContentStringMap(getDctx(), getProduct(), "LONG_DESCRIPTION", getLocales(), getDefaultLocale(), getLangCodeFn(), SolrLocaleUtil.I18N_GENERAL, getPcwList(), getMoment(), isUseEntityCache());
            }
            return longDescriptionLocaleMap;
        }

        /** Gets product keywords. NOTE: for variant products, we also include the keywords from the virtual/parent. */
        public Set<String> getKeywords() throws Exception {
            if (keywords == null) {
                keywords = getProductData().getProductKeywords(new LinkedHashSet<>(), getDelegator(), isUseEntityCache(), getProductId(), getParentProductId());
            }
            return keywords;
        }
    }
}
