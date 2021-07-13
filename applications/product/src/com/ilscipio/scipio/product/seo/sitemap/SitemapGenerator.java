package com.ilscipio.scipio.product.seo.sitemap;

import java.io.File;
import java.io.IOException;
import java.net.MalformedURLException;
import java.net.URISyntaxException;
import java.net.URL;
import java.sql.Timestamp;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Date;
import java.util.EnumMap;
import java.util.HashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Set;

import com.ilscipio.scipio.product.category.CatalogAltUrlSanitizer;
import com.ilscipio.scipio.product.category.CatalogFilters;
import com.ilscipio.scipio.product.seo.SeoConfig;
import org.ofbiz.base.location.FlexibleLocation;
import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.GeneralException;
import org.ofbiz.base.util.UtilGenerics;
import org.ofbiz.base.util.UtilMisc;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.GenericValue;
import org.ofbiz.entity.condition.EntityCondition;
import org.ofbiz.entity.condition.EntityOperator;
import org.ofbiz.service.LocalDispatcher;
import org.ofbiz.service.ModelService;
import org.ofbiz.service.ServiceUtil;
import org.ofbiz.webapp.ExtWebappInfo;
import org.ofbiz.webapp.FullWebappInfo;
import org.ofbiz.webapp.OfbizUrlBuilder;
import org.xml.sax.SAXException;

import com.ilscipio.scipio.ce.util.PathUtil;
import com.ilscipio.scipio.ce.webapp.filter.urlrewrite.ScipioUrlRewriter;
import com.ilscipio.scipio.product.seo.SeoCatalogTraverser;
import com.ilscipio.scipio.product.seo.SeoCatalogUrlWorker;
import com.redfin.sitemapgenerator.SitemapIndexGenerator;
import com.redfin.sitemapgenerator.WebSitemapGenerator;
import com.redfin.sitemapgenerator.WebSitemapUrl;

/**
 * Builds sitemap and records stats.
 * <p>
 * NOT thread-safe.
 * <p>
 * TODO: missing multi-locale link support - unclear if library supports - may need to do one-locale-per-index
 * TODO: does not delete old files (minor issue - spiders will simply ignore them in theory)
 */
public class SitemapGenerator extends SeoCatalogTraverser {

    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());
    private static final SitemapGeneratorFactory DEFAULT_FACTORY = new SitemapGeneratorFactory();

    static final String logPrefix = "Seo: Sitemap: ";

    protected List<Locale> locales;
    protected String baseUrl;
    protected String sitemapWebappPathPrefix;
    protected String sitemapContextPath;
    protected String webappPathPrefix;
    protected String contextPath;
    protected SitemapConfig sitemapConfig;
    protected SeoCatalogUrlWorker urlWorker;
    protected OfbizUrlBuilder ofbizUrlBuilder;
    protected ScipioUrlRewriter urlRewriter;
    protected Map<String, Object> urlRewriterCtx;
    protected FullWebappInfo webappInfo;
    protected String fullSitemapDir;
    protected Map<ElemType, ElemHandler> elemHandlers = null;
    protected ElemHandler categoryElemHandler = null; // optimization
    protected ElemHandler productElemHandler = null; // optimization
    protected ElemHandler contentElemHandler = null; // optimization
    protected Map<String, ?> servCtxOpts;

    public enum ElemType { CATEGORY, PRODUCT, CONTENT }

    // DEV NOTE: If adding fields, beware of copy constructor below, needed for extension

    protected SitemapGenerator(Delegator delegator, LocalDispatcher dispatcher, List<Locale> locales, GenericValue webSite, GenericValue productStore, String baseUrl, String sitemapWebappPathPrefix, String sitemapContextPath,
                               String webappPathPrefix, String contextPath, SitemapConfig sitemapConfig, SeoCatalogUrlWorker urlWorker, OfbizUrlBuilder ofbizUrlBuilder, ScipioUrlRewriter urlRewriteConf, Map<String, Object> urlRewriterCtx,
                               Map<String, ?> servCtxOpts, SitemapTraversalConfig travConfig) throws GeneralException, IOException {
        super(delegator, dispatcher, travConfig);
        setWebSite(webSite);
        setProductStore(productStore);
        this.locales = locales;
        this.baseUrl = baseUrl;
        this.sitemapWebappPathPrefix = sitemapWebappPathPrefix;
        this.sitemapContextPath = sitemapContextPath;
        this.webappPathPrefix = webappPathPrefix;
        this.contextPath = contextPath;
        this.sitemapConfig = sitemapConfig;
        this.urlWorker = urlWorker;
        this.ofbizUrlBuilder = ofbizUrlBuilder;
        this.urlRewriter = urlRewriteConf;
        this.urlRewriterCtx = urlRewriterCtx;
        this.webappInfo = FullWebappInfo.fromWebapp(ExtWebappInfo.fromWebSiteId(webSiteId), delegator, null);
        this.fullSitemapDir = sitemapConfig.getSitemapDirUrlLocation(webappInfo.getWebappInfo().getLocation());
        this.servCtxOpts = servCtxOpts;
        getSitemapDirFile(); // test this for exception
        reset();
    }

    /**
     * Copy constructor.
     */
    protected SitemapGenerator(SitemapGenerator other) {
        super(other);
        this.locales = other.locales;
        this.baseUrl = other.baseUrl;
        this.sitemapWebappPathPrefix = other.sitemapWebappPathPrefix;
        this.sitemapContextPath = other.sitemapContextPath;
        this.webappPathPrefix = other.webappPathPrefix;
        this.contextPath = other.contextPath;
        this.sitemapConfig = other.sitemapConfig;
        this.urlWorker = other.urlWorker;
        this.ofbizUrlBuilder = other.ofbizUrlBuilder;
        this.urlRewriter = other.urlRewriter;
        this.urlRewriterCtx = other.urlRewriterCtx;
        this.webappInfo = other.webappInfo;
        this.fullSitemapDir = other.fullSitemapDir;
        this.elemHandlers = copyElemHandlers(other.elemHandlers);
        this.categoryElemHandler = other.categoryElemHandler;
        this.productElemHandler = other.productElemHandler;
        this.contentElemHandler = other.contentElemHandler;
        this.servCtxOpts = other.servCtxOpts;
    }

    public static SitemapGeneratorFactory getFactory(SitemapConfig sitemapConfig) {
        return (sitemapConfig.getGeneratorFactory() != null) ? sitemapConfig.getGeneratorFactory() : getDefaultFactory();
    }

    public static SitemapGeneratorFactory getDefaultFactory() {
        return DEFAULT_FACTORY;
    }

    public static SitemapGenerator getGeneratorForWebsite(Delegator delegator, LocalDispatcher dispatcher, String webSiteId, Map<String, ?> servCtxOpts, boolean useCache) throws GeneralException, IOException {
        // TODO: LOCALIZE WITH PROP MESSAGE EXCEPTIONS
        GenericValue webSite = delegator.findOne("WebSite", UtilMisc.toMap("webSiteId", webSiteId), useCache);
        if (webSite == null) throw new GeneralException("website not found: " + webSiteId);

        String productStoreId = webSite.getString("productStoreId");
        if (UtilValidate.isEmpty(productStoreId)) throw new GeneralException("website has no product store: " + webSiteId);

        GenericValue productStore = delegator.findOne("ProductStore", UtilMisc.toMap("productStoreId", productStoreId), useCache);
        if (productStore == null) throw new GeneralException("store not found: " + productStoreId);

        SitemapConfig sitemapConfig = SitemapConfig.getSitemapConfigForWebsite(delegator, dispatcher, webSiteId);
        return getFactory(sitemapConfig).createGeneratorForWebsite(delegator, dispatcher, webSite, productStore, sitemapConfig, servCtxOpts, useCache);
    }

    /**
     * Generator factory: this works by creating a new instance from an already-created one using copy construction.
     * The returned instance must super to the copy constructor {@link #SitemapGenerator(SitemapGenerator)}
     * (this is a kludge, but otherwise there are too many parameters).
     * <p>
     * How to extend:
     * For simple extension, it is sufficient to override {@link #createGenerator(SitemapGenerator)}.
     * If not enough, also override {@link #useCopyInstantiation} to return false and override one of the other methods as needed.
     */
    public static class SitemapGeneratorFactory {

        /**
         * Main factory method for client code.
         */
        public SitemapGenerator createGeneratorForWebsite(Delegator delegator, LocalDispatcher dispatcher, GenericValue webSite, GenericValue productStore, SitemapConfig sitemapConfig, Map<String, ?> servCtxOpts, boolean useCache) throws GeneralException, IOException {
            try {
                if (sitemapConfig == null) {
                    throw new IllegalArgumentException("No valid sitemap config for website '" + webSite.getString("webSiteId") + "'");
                }
                OfbizUrlBuilder ofbizUrlBuilder = OfbizUrlBuilder.fromWebSiteId(webSite.getString("webSiteId"), delegator);
                String sitemapWebappPathPrefix = sitemapConfig.getSitemapWebappPathPrefix();
                if (sitemapWebappPathPrefix == null) {
                    sitemapWebappPathPrefix = sitemapConfig.getDefaultWebappPathPrefix(ofbizUrlBuilder);
                }
                String sitemapContextPath = sitemapConfig.getSitemapContextPath();
                if (sitemapContextPath == null) {
                    sitemapContextPath = sitemapConfig.getDefaultContextPath(delegator);
                }
                String webappPathPrefix = sitemapConfig.getWebappPathPrefix();
                if (webappPathPrefix == null) {
                    webappPathPrefix = sitemapConfig.getDefaultWebappPathPrefix(ofbizUrlBuilder);
                }
                String contextPath = sitemapConfig.getContextPath();
                if (contextPath == null) {
                    contextPath = sitemapConfig.getDefaultContextPath(delegator);
                }
                String baseUrl = sitemapConfig.getBaseUrl();
                if (baseUrl == null) {
                    baseUrl = sitemapConfig.getDefaultBaseUrl(ofbizUrlBuilder, sitemapConfig.isBaseUrlSecure());
                }

                List<Locale> locales = sitemapConfig.getLocalesOrDefault(webSite, productStore);

                Map<String, Object> urlRewriterCtx = new HashMap<>();
                urlRewriterCtx.put("globalContext", new HashMap<String, Object>());
                urlRewriterCtx.put("delegator", delegator);
                urlRewriterCtx.put("dispatcher", dispatcher);
                //urlRewriterCtx.put("security", security); // FIXME: missing
                // FIXME: only single locale supported; we may need a rewriter per-locale in the future...
                urlRewriterCtx.put("locale", locales.get(0));
                urlRewriterCtx.put("webSiteId", webSite.getString("webSiteId"));
                ScipioUrlRewriter urlRewriterConf = null;
                if (sitemapConfig.getUrlConfPath() != null) {
                    urlRewriterConf = ScipioUrlRewriter.getForContext(
                            FullWebappInfo.fromWebapp(ExtWebappInfo.fromWebSiteId(webSite.getString("webSiteId")), delegator, null),
                            sitemapConfig.getUrlConfPath(), urlRewriterCtx);
                }
                SeoCatalogUrlWorker urlWorker = SeoCatalogUrlWorker.getInstance(delegator, webSite.getString("webSiteId"));

                SitemapTraversalConfig travConfig = createTraversalConfig(delegator, dispatcher, locales, webSite, productStore,
                        baseUrl, sitemapWebappPathPrefix, sitemapContextPath, webappPathPrefix, contextPath, sitemapConfig,
                        urlWorker, ofbizUrlBuilder, urlRewriterConf, urlRewriterCtx, servCtxOpts, useCache);

                SitemapGenerator generator = createGenerator(delegator, dispatcher, locales, webSite, productStore,
                        baseUrl, sitemapWebappPathPrefix, sitemapContextPath, webappPathPrefix, contextPath, sitemapConfig,
                        urlWorker, ofbizUrlBuilder, urlRewriterConf, urlRewriterCtx, servCtxOpts, travConfig);
                if (useCopyInstantiation() && !isDefaultFactory()) {
                    // kludge using copy construction because we simply have too many parameters and too much work for simple cases
                    generator = createGenerator(generator);
                }
                return generator;
            } catch (SAXException e) {
                throw new IOException(e);
            }
        }

        /**
         * Override this to false for non-copy-constructor extend.
         */
        protected boolean useCopyInstantiation() {
            return true;
        }

        /**
         * Copy-constructor factory, override this for simple extension.
         */
        protected SitemapGenerator createGenerator(SitemapGenerator baseInstance) {
            return new SitemapGenerator(baseInstance);
        }

        protected SitemapGenerator createGenerator(Delegator delegator, LocalDispatcher dispatcher, List<Locale> locales, GenericValue webSite, GenericValue productStore, String baseUrl, String sitemapWebappPathPrefix, String sitemapContextPath,
                                                   String webappPathPrefix, String contextPath, SitemapConfig sitemapConfig,
                                                   SeoCatalogUrlWorker urlWorker, OfbizUrlBuilder ofbizUrlBuilder, ScipioUrlRewriter urlRewriteConf, Map<String, Object> urlRewriterCtx, Map<String, ?> servCtxOpts, SitemapTraversalConfig travConfig) throws GeneralException, IOException {
            return new SitemapGenerator(delegator, dispatcher, locales, webSite, productStore,
                    baseUrl, sitemapWebappPathPrefix, sitemapContextPath, webappPathPrefix, contextPath, sitemapConfig,
                    SeoCatalogUrlWorker.getInstance(delegator, webSite.getString("webSiteId")),
                    ofbizUrlBuilder, urlRewriteConf,urlRewriterCtx, servCtxOpts, travConfig);
        }

        protected SitemapTraversalConfig createTraversalConfig(SitemapConfig sitemapConfig) {
            return new SitemapTraversalConfig(sitemapConfig);
        }

        protected SitemapTraversalConfig createTraversalConfig(Delegator delegator, LocalDispatcher dispatcher, List<Locale> locales, GenericValue webSite, GenericValue productStore, String baseUrl, String sitemapWebappPathPrefix, String sitemapContextPath,
                                                               String webappPathPrefix, String contextPath, SitemapConfig sitemapConfig,
                                                               SeoCatalogUrlWorker urlWorker, OfbizUrlBuilder ofbizUrlBuilder, ScipioUrlRewriter urlRewriteConf, Map<String, Object> urlRewriterCtx, Map<String, ?> servCtxOpts, boolean useCache) throws GeneralException, IOException {
            SitemapTraversalConfig travConfig = (SitemapTraversalConfig) createTraversalConfig(sitemapConfig)
                    .setDoContent(sitemapConfig.isDoContent())
                    .setUseCache(useCache);
            if (sitemapConfig.isUseDefaultCatalogFilters()) {
                travConfig.addFilters(sitemapConfig.getDefaultCatalogFilters());
            }
            if (sitemapConfig.isUseAutoCatalogFilters()) {
                if (!sitemapConfig.isIncludeVariant()) {
                    travConfig.addFilter(CatalogFilters.ExcludeVariantsProductFilter.getInstance());
                }
                if (UtilValidate.isNotEmpty(sitemapConfig.getExcludeSpecificCategoryIds())) {
                    travConfig.addFilter(new CatalogFilters.ExcludeSpecificCategoryFilter(sitemapConfig.getExcludeSpecificCategoryIds()));
                }
                if (UtilValidate.isNotEmpty(sitemapConfig.getExcludeSpecificProductIds())) {
                    travConfig.addFilter(new CatalogFilters.ExcludeSpecificProductFilter(sitemapConfig.getExcludeSpecificProductIds()));
                }
            }
            travConfig.addFilters(sitemapConfig.getCatalogFilters());
            return travConfig;
        }

        public boolean isDefaultFactory() {
            return SitemapGeneratorFactory.class.equals(this.getClass());
        }
    }

    public static class SitemapTraversalConfig extends SeoTraversalConfig {
        public SitemapTraversalConfig(SitemapConfig sitemapConfig) {
            setDoCategory(sitemapConfig.isDoCategory());
            setDoProduct(sitemapConfig.isDoProduct());
        }
    }

    @Override
    public SitemapTraversalConfig newTravConfig() {
        return new SitemapTraversalConfig(sitemapConfig);
    }

    @Override
    public SitemapTraversalConfig getTravConfig() {
        return (SitemapTraversalConfig) travConfig;
    }

    protected Map<String, ?> getServCtxOpts() {
        return servCtxOpts;
    }

    public class SitemapTraversalState extends SeoTraversalState {
        Map<Locale, List<String>> trailNames; // reset for every new ProdCatalogCategory

        public SitemapTraversalState(List<GenericValue> trailCategories, int physicalDepth, Map<Locale, List<String>> trailNames) {
            super(trailCategories, physicalDepth);
            this.trailNames = trailNames;
        }

        public SitemapTraversalState(SitemapTraversalState other, boolean deepCopy) {
            super(other, deepCopy);
            if (deepCopy) {
                if (trailNames != null) {
                    Map<Locale, List<String>> trailNames = new HashMap<>();
                    for (Map.Entry<Locale, List<String>> entry : other.trailNames.entrySet()) {
                        trailNames.put(entry.getKey(), new ArrayList<>(entry.getValue()));
                    }
                    this.trailNames = trailNames;
                }
            } else {
                this.trailNames = other.trailNames;
            }
        }

        @Override
        public SeoTraversalState copy(boolean deepCopy) {
            return new SitemapTraversalState(this, deepCopy);
        }

        public Map<Locale, List<String>> getTrailNames() {
            return trailNames;
        }
    }

    @Override
    protected TraversalState newTraversalState(List<GenericValue> trailCategories, int physicalDepth) {
        return new SitemapTraversalState(trailCategories, physicalDepth, newTrailNames());
    }

    public SitemapConfig getSitemapConfig() {
        return sitemapConfig;
    }

    public SeoCatalogUrlWorker getUrlWorker() { return urlWorker; }

    public SeoConfig getSeoConfig() { return getUrlWorker().getConfig(); }

    @Override
    public void reset() throws GeneralException {
        super.reset();
        resetElemHandlers();
    }

    // moved to TraversalState
//    /**
//     * NOTE: because the way this is edited in-place during iteration by push/pop,
//     * the only time this will still contain entries at the end is if an error happened.
//     * So we don't need to reset this often, only during reset() (e.g. there is no need
//     * to reset at every new ProdCatalogCategory, it arranges itself).
//     */
//    protected void resetTrailNames() {
//        this.trailNames = newTrailNames();
//    }

    protected Map<Locale, List<String>> newTrailNames() {
        Map<Locale, List<String>> trailNames = new HashMap<>();
        for(Locale locale : getLocales()) {
            List<String> trailList = newCategoryTrailList();
            trailNames.put(locale, trailList);
        }
        return trailNames;
    }

    protected Map<Locale, List<String>> getTrailNames(TraversalState state) {
        return ((SitemapTraversalState) state).trailNames;
    }

    protected List<GenericValue> getTrailEntities(TraversalState state) {
        return ((SitemapTraversalState) state).getTrailCategories();
    }

    protected String getSitemapWebappPathPrefix() {
        return sitemapWebappPathPrefix;
    }

    protected String getSitemapContextPath() {
        return sitemapContextPath;
    }

    protected String getWebappPathPrefix() {
        return webappPathPrefix;
    }

    protected String getContextPath() {
        return contextPath;
    }

    protected String getBaseUrl() {
        return (baseUrl != null ? baseUrl : "");
    }

    protected List<Locale> getLocales() {
        return locales;
    }

    protected Locale getDefaultLocale() {
        return locales.get(0);
    }

    /**
     * Gets cached conf.
     * Avoids reloading the urlrewrite.xml file for every single URL.
     */
    protected ScipioUrlRewriter getUrlRewriter() {
        return urlRewriter;
    }

    protected Map<String, Object> getUrlRewriterCtx() {
        return urlRewriterCtx;
    }

    public String getFullSitemapDir() {
        return fullSitemapDir;
    }

    protected File getSitemapDirFile() throws IOException {
        String fullSitemapDir = getFullSitemapDir();
        try {
            URL url = FlexibleLocation.resolveLocation(fullSitemapDir);
            if (url == null) throw new MalformedURLException("Cannot resolve location: " + getFullSitemapDir());
            return new File(url.toURI());
        } catch(Exception e) {
            throw new IOException("Error resolving sitemap directory: " + fullSitemapDir + ": " + e.getMessage(), e);
        }
    }

    protected WebSitemapGenerator getSitemapGenerator(String filePrefix) throws IOException {
        File myDir = getSitemapDirFile();
        myDir.mkdirs();
        return WebSitemapGenerator.builder(getBaseUrl(), myDir).fileNamePrefix(filePrefix).dateFormat(sitemapConfig.getDateFormat()).gzip(sitemapConfig.isGzip()).build();
    }

    /**
     * The main iteration call for product/category sitemap generation - wrapper
     * around {@link #traverseCategoriesDepthFirst(List)}, plus content.
     */
    public void buildSitemapForWebsite() throws GeneralException {
        if ("all-system".equals(getSitemapConfig().getCategoryTraversalMode()) || "all-system".equals(getSitemapConfig().getProductTraversalMode())) {
            setProdCatalogFromProductStore();
            if (isDoCategory()) {
                try(DoStateHandler dsh = doCategoryOnlySection()) {
                    if ("all-system".equals(getSitemapConfig().getCategoryTraversalMode())) {
                        traverseAllCategoriesInSystem();
                    } else {
                        traverseStoreCatalogsDepthFirst();
                    }
                }
            }
            if (isDoProduct()) {
                try(DoStateHandler dsh = doProductOnlySection()) {
                    if ("all-system".equals(getSitemapConfig().getProductTraversalMode())) {
                        traverseAllProductsInSystem();
                    } else {
                        traverseStoreCatalogsDepthFirst();
                    }
                }
            }
        } else {
            traverseStoreCatalogsDepthFirst();
        }
        buildSitemapForContent();
    }

    /**
     * Content link generation. Includes CMS if enabled ({@link #buildSitemapForCmsPage}).
     */
    public void buildSitemapForContent() throws GeneralException {
        if (!sitemapConfig.isDoContent()) return;
        buildSitemapForCmsPage();
    }

    /**
     * CMS link generation (but not other content).
     */
    public void buildSitemapForCmsPage() throws GeneralException {
        if (!sitemapConfig.isDoCmsPage()) return;

        // TODO: REVIEW: locale handling
        Locale contentLocale = getDefaultLocale();
        List<String> uriList = getCmsUriList(contentLocale);
        if (uriList == null || uriList.size() == 0) return;

        for(String uri : uriList) {
            buildSitemapCmsPageLink(uri, contentLocale);
        }
    }

    @Override
    public EntityCondition makeProductStoreCatalogCond(String productStoreId) {
        EntityCondition cond = EntityCondition.makeCondition("productStoreId", productStoreId);
        if (sitemapConfig.getProdCatalogIds() != null) {
            cond = EntityCondition.makeCondition(cond, EntityOperator.AND,
                    makeFieldPossibleValuesCond("prodCatalogId", sitemapConfig.getProdCatalogIds()));
        }
        return cond;
    }

    @Override
    public EntityCondition makeProdCatalogCategoryCond(String prodCatalogId) {
        EntityCondition cond = EntityCondition.makeCondition("prodCatalogId", prodCatalogId);
        if (sitemapConfig.getProdCatalogCategoryTypeIds() != null) {
            cond = EntityCondition.makeCondition(cond, EntityOperator.AND,
                    makeFieldPossibleValuesCond("prodCatalogCategoryTypeId", sitemapConfig.getProdCatalogCategoryTypeIds()));
        }
        return cond;
    }

    /**
     * If applicable, reorder by the prodCatalogIds in the config, so that order won't change randomly.
     */
    @Override
    public List<GenericValue> filterProductStoreCatalogList(List<GenericValue> productStoreCatalogList) {
        if (sitemapConfig.getProdCatalogIds() == null || sitemapConfig.getProdCatalogIds().size() <= 1) {
            return productStoreCatalogList;
        } else {
            return reorderByStringFieldValues(productStoreCatalogList, "prodCatalogId",
                    sitemapConfig.getProdCatalogIds(), true);
        }
    }

    /**
     * If applicable, reorder by the prodCatalogCategoryTypeIds in the config, so that order won't change randomly.
     */
    @Override
    public List<GenericValue> filterProdCatalogCategoryList(List<GenericValue> prodCatalogCategoryList) {
        if (sitemapConfig.getProdCatalogCategoryTypeIds() == null || sitemapConfig.getProdCatalogCategoryTypeIds().size() <= 1) {
            return prodCatalogCategoryList;
        } else {
            return reorderByStringFieldValues(prodCatalogCategoryList, "prodCatalogCategoryTypeId",
                    sitemapConfig.getProdCatalogCategoryTypeIds(), true);
        }
    }

    /**
     * Applies ProdCatalogCategory filters, post-query.
     * NOTE: use EntityConditions instead of this.
     */
    @Override
    public boolean isApplicableCategoryAssoc(GenericValue prodCatalogCategory) {
        // This is covered (faster) in the condition filters.
//        if (!prodCatalogCategory.getModelEntity().isField("prodCatalogId")) return true;
//        if (config.getProdCatalogIds() != null && !config.getProdCatalogIds().contains(prodCatalogCategory.getString("prodCatalogId"))) {
//            return false;
//        }
//        if (config.getProdCatalogCategoryTypeIds() != null && !config.getProdCatalogCategoryTypeIds().contains(prodCatalogCategory.getString("prodCatalogCategoryTypeId"))) {
//            return false;
//        }
        return true;
    }

    @Override
    public void pushCategory(GenericValue productCategory, TraversalState state) throws GeneralException {
        CatalogAltUrlSanitizer.SanitizeContext sanitizeCtx = getUrlWorker().getCatalogAltUrlSanitizer().makeSanitizeContext().setNameIndex(state.getPhysicalDepth());
        Map<Locale, List<String>> trailNames = getTrailNames(state);
        if (getSitemapConfig().isPreProcessTrail()) {
            for (Locale locale : locales) {
                // NOTE: this is non-last - cannot reuse the one determined in previous call
                SeoConfig.TrailFormat trailFormat = getSeoConfig().getCategoryUrlTrailFormat(); // FIXME?: this is flawed and may violate configuration; we're forced to ignore product-url-trail-format
                String trailName = getUrlWorker().getCategoryPathPart(getDelegator(), getDispatcher(), locale, productCategory, trailFormat, sanitizeCtx, isUseCache());
                trailNames.get(locale).add(trailName); // no need copy, just remove after
            }
        } else {
            // NOTE: it should not be necessary to do this, but to prevent bugs, fill the trailNames with IDs (still faster)
            for (Locale locale : locales) {
                trailNames.get(locale).add(productCategory.getString("productCategoryId"));
            }
        }
    }

    @Override
    public void popCategory(GenericValue productCategory, TraversalState state) throws GeneralException {
        Map<Locale, List<String>> trailNames = getTrailNames(state);
        for(Locale locale : locales) {
            List<String> trail = trailNames.get(locale);
            trail.remove(trail.size() - 1);
        }
    }

    @Override
    public void visitCategory(GenericValue productCategory, TraversalState state) throws GeneralException {
        buildSitemapCategoryLink(productCategory, getTrailNames(state), getTrailEntities(state));
    }

    @Override
    public void visitProduct(GenericValue product, TraversalState state) throws GeneralException {
        buildSitemapProductLink(product, getTrailNames(state), getTrailEntities(state));
    }

    protected void buildSitemapCategoryLink(GenericValue productCategory, Map<Locale, List<String>> trailNames, List<GenericValue> trailEntities) throws GeneralException {
        String productCategoryId = productCategory.getString("productCategoryId");
        try {

            // TODO: missing multi-locale link support - unclear if library supports

            Locale locale = getDefaultLocale();
            String url;

            if (getSitemapConfig().isPreProcessTrail()) {
                List<String> trail = trailNames.get(locale);
                CatalogAltUrlSanitizer.SanitizeContext sanitizeCtx = getUrlWorker().getCatalogAltUrlSanitizer().makeSanitizeContext().setTargetCategory(productCategory)
                        .setLast(true).setNameIndex(trail.size() - 1).setTotalNames(trail.size());
                url = getUrlWorker().makeCategoryUrlPath(getDelegator(), getDispatcher(), locale, productCategory, trail, getContextPath(), sanitizeCtx, isUseCache()).toString();
            } else {
                url = getUrlWorker().makeCategoryUrlCore(getDelegator(), getDispatcher(), locale, productCategory, null, null, trailEntities,
                        getWebappInfo(), isUseCache()).toString();
            }

            String processedUrl = postprocessElementLink(url);
            if (processedUrl == null || matchesUrlFilter(processedUrl)) {
                Debug.logInfo("Filtered category url: " + (processedUrl != null ? processedUrl : url), module);
                getStats().categoryFiltered++;
            } else {
                if (Debug.verboseOn()) {
                    Debug.logVerbose(getLogMsgPrefix() + "Adding category url: " + processedUrl, module);
                }
                WebSitemapUrl libUrl = buildSitemapLibUrl(processedUrl, null);
                getCategoryElemHandler().addUrl(libUrl);
            }
        } catch(Exception e) {
            getStats().categoryError++;
            Debug.logError(getLogErrorPrefix() + "Cannot build URL for category '" + productCategoryId + "': " + e.getMessage(), module);
        }
    }

    protected void buildSitemapProductLink(GenericValue product, Map<Locale, List<String>> trailNames, List<GenericValue> trailEntities) throws GeneralException {
        String productId = product.getString("productId");
        try {

            // TODO: missing multi-locale link support - unclear if library supports

            Locale locale = getDefaultLocale();
            String url;

            if (getSitemapConfig().isPreProcessTrail()) {
                List<String> trail = trailNames.get(locale);
                CatalogAltUrlSanitizer.SanitizeContext sanitizeCtx = getUrlWorker().getCatalogAltUrlSanitizer().makeSanitizeContext().setTargetProduct(product)
                        .setLast(true).setNameIndex(trail.size()).setTotalNames(trail.size() + 1);
                url = getUrlWorker().makeProductUrlPath(getDelegator(), getDispatcher(), locale, product, trail, getContextPath(), sanitizeCtx, isUseCache()).toString();
            } else {
                url = getUrlWorker().makeProductUrlCore(getDelegator(), getDispatcher(), locale, product, null, null, trailEntities,
                        getWebappInfo(), isUseCache()).toString();
            }

            String processedUrl = postprocessElementLink(url);
            if (processedUrl == null || matchesUrlFilter(processedUrl)) {
                Debug.logInfo("Filtered product url: " + (processedUrl != null ? processedUrl : url), module);
                getStats().productFiltered++;
            } else {
                if (Debug.verboseOn()) {
                    Debug.logVerbose(getLogMsgPrefix() + "Adding product url: " + processedUrl, module);
                }
                WebSitemapUrl libUrl = buildSitemapLibUrl(processedUrl, sitemapConfig.isUseProductLastModDate() ? product.getTimestamp("lastModifiedDate") : null);
                getProductElemHandler().addUrl(libUrl);
                // TODO?: is there need to do variants (not explicitly associated to category)?
                // usually don't want to advertise the variants unless attached to category for some reason?...
                //if (config.doChildProduct) {
                //}
            }
        } catch(Exception e) {
            getStats().productError++;
            Debug.logError(getLogErrorPrefix() + "Cannot build URL for product '" + productId + "': " + e.getMessage(), module);
        }
    }

    protected void buildSitemapCmsPageLink(String uri, Locale locale) {
        try {
            uri = PathUtil.concatPaths(getContextPath(), uri);
            String url = postprocessElementLink(uri);
            if (Debug.verboseOn()) {
                Debug.logVerbose(getLogMsgPrefix()+"Processing CMS page url: " + url, module);
            }
            WebSitemapUrl libUrl = buildSitemapLibUrl(url, null);
            getContentElemHandler().addUrl(libUrl);
        } catch(Exception e) {
            getStats().contentError++;
            Debug.logError(getLogErrorPrefix()+"Error processing cms page URI: " + uri + ": " + e.getMessage(), module);
        }
    }

    protected void resetElemHandlers() {
        this.elemHandlers = createElemHandlers();
    }

    protected Map<ElemType, ElemHandler> createElemHandlers() {
        Map<ElemType, ElemHandler> elemHandlers = new EnumMap<>(ElemType.class);
        if (sitemapConfig.isDoCategory()) {
            elemHandlers.put(ElemType.CATEGORY, new CategoryElemHandler());
        }
        if (sitemapConfig.isDoProduct()) {
            elemHandlers.put(ElemType.PRODUCT, new ProductElemHandler());
        }
        if (sitemapConfig.isDoContent()) {
            elemHandlers.put(ElemType.CONTENT, new ContentElemHandler());
        }
        return elemHandlers;
    }

    protected Map<ElemType, ElemHandler> copyElemHandlers(Map<ElemType, ElemHandler> otherHandlers) {
        Map<ElemType, ElemHandler> elemHandlers = new EnumMap<>(ElemType.class);
        for(Map.Entry<ElemType, ElemHandler> entry : otherHandlers.entrySet()) {
            // DEV NOTE: don't use copy() abstraction in order to avoid super "this" reference issues
            if (entry.getKey() == ElemType.CATEGORY) {
                elemHandlers.put(entry.getKey(), new CategoryElemHandler((CategoryElemHandler) entry.getValue()));
            } else if (entry.getKey() == ElemType.PRODUCT) {
                elemHandlers.put(entry.getKey(), new ProductElemHandler((ProductElemHandler) entry.getValue()));
            } else if (entry.getKey() == ElemType.CONTENT) {
                elemHandlers.put(entry.getKey(), new ContentElemHandler((ContentElemHandler) entry.getValue()));
            }
        }
        return elemHandlers;
    }

    protected ElemHandler getCategoryElemHandler() { return elemHandlers.get(ElemType.CATEGORY); }

    protected ElemHandler getProductElemHandler() { return elemHandlers.get(ElemType.PRODUCT); }

    protected ElemHandler getContentElemHandler() { return elemHandlers.get(ElemType.CONTENT); }

    protected abstract class ElemHandler {
        private WebSitemapGenerator wsg;
        private List<String> sitemapFiles;
        private long urlCount;
        private long sitemapFileIndex;
        private Set<String> seenUrls;

        protected ElemHandler() {
            this.wsg = null;
            this.sitemapFiles = new ArrayList<>();
            this.urlCount = 0;
            this.sitemapFileIndex = 0;
            this.seenUrls = new LinkedHashSet<>();
        }

        protected ElemHandler(ElemHandler other) {
            this.wsg = other.wsg;
            this.sitemapFiles = new ArrayList<>(other.sitemapFiles);
            this.urlCount = other.urlCount;
            this.sitemapFileIndex = other.sitemapFileIndex;
            this.seenUrls = new LinkedHashSet<>(other.seenUrls);
        }

        public WebSitemapGenerator getWsg() { return wsg; }
        public List<String> getSitemapFiles() { return sitemapFiles; }
        public long getUrlCount() { return urlCount; }
        public long getSitemapFileIndex() { return sitemapFileIndex; }

        public abstract String getTypeFilenamePrefix();

        public String getNumberedSitemapFilenamePrefix() {
            return getTypeFilenamePrefix() + sitemapFileIndex;
        }

        public String getSitemapFilename() {
            return getNumberedSitemapFilenamePrefix() + "." + sitemapConfig.getSitemapExtension();
        }

        public void addUrl(WebSitemapUrl url) throws IOException, URISyntaxException {
            if (sitemapConfig.getSizemapSize() != null && urlCount >= sitemapConfig.getSizemapSize()) {
                commitSitemapFile();
            }
            if (wsg == null) {
                beginSitemapFile();
            }
            String urlStr = url.getUrl().toString();
            if (!seenUrls.contains(urlStr)) {
                wsg.addUrl(url);
                seenUrls.add(urlStr);
                urlCount++;
                updateStatsCount();
            }
        }

        protected void beginSitemapFile() throws IOException, URISyntaxException {
            sitemapFileIndex++;
            urlCount = 0;
            wsg = getSitemapGenerator(getNumberedSitemapFilenamePrefix());
            Debug.logInfo(getLogMsgPrefix() + "Building: " + getSitemapFilename(), module);
        }

        protected void commitSitemapFile() {
            if (wsg == null) return;
            String fn = getSitemapFilename();
            Debug.logInfo(getLogMsgPrefix() + "Writing: " + fn + " (" + urlCount + " entries)", module);
            wsg.write();
            sitemapFiles.add(fn);
            wsg = null;
        }

        protected abstract ElemType getType();

        protected abstract void updateStatsCount();
    }

    protected class CategoryElemHandler extends ElemHandler {
        protected CategoryElemHandler() { super(); }
        protected CategoryElemHandler(CategoryElemHandler other) { super(other); }
        @Override protected ElemType getType() { return ElemType.CATEGORY; }
        @Override protected void updateStatsCount() { getStats().categorySuccess++; }
        @Override public String getTypeFilenamePrefix() { return sitemapConfig.getCategoryFilePrefix(); }
    }

    protected class ProductElemHandler extends ElemHandler {
        protected ProductElemHandler() { super(); }
        protected ProductElemHandler(ProductElemHandler other) { super(other); }
        @Override protected ElemType getType() { return ElemType.PRODUCT; }
        @Override protected void updateStatsCount() { getStats().productSuccess++; }
        @Override public String getTypeFilenamePrefix() { return sitemapConfig.getProductFilePrefix(); }
    }

    protected class ContentElemHandler extends ElemHandler {
        protected ContentElemHandler() { super(); }
        protected ContentElemHandler(ContentElemHandler other) { super(other); }
        @Override protected ElemType getType() { return ElemType.CONTENT; }
        @Override protected void updateStatsCount() { getStats().contentSuccess++; }
        @Override public String getTypeFilenamePrefix() { return sitemapConfig.getContentFilePrefix(); }
    }

    protected WebSitemapUrl buildSitemapLibUrl(String url, Timestamp lastModDate) throws MalformedURLException {
        WebSitemapUrl.Options opts = new WebSitemapUrl.Options(url);
        if (lastModDate != null) {
            opts.lastMod(new Date(lastModDate.getTime()));
        }
        return opts.build();
    }

    protected List<String> getAllSitemapFilenames() {
        List<String> sitemapFiles = new ArrayList<>();
        for(ElemType elemType : ElemType.values()) { // always same order
            sitemapFiles.addAll(elemHandlers.get(elemType).getSitemapFiles());
        }
        return sitemapFiles;
    }

    public void commitSitemaps() {
        for(ElemType elemType : ElemType.values()) { // always same order
            elemHandlers.get(elemType).commitSitemapFile();
        }
    }

    public void commitSitemapsAndIndex() throws IOException, URISyntaxException {
        commitSitemaps();
        generateSitemapIndex(getAllSitemapFilenames());
    }

    // old, unused
//    protected void writeSitemap(List<WebSitemapUrl> urlList, String filePrefix) throws IOException, URISyntaxException {
//        WebSitemapGenerator wsg = getSitemapGenerator(filePrefix);
//        for(WebSitemapUrl url : urlList){
//            wsg.addUrl(url);
//        }
//        wsg.write();
//    }

    public void generateSitemapIndex(List<String> sitemapFilenames) throws IOException, URISyntaxException {
        Debug.logInfo(getLogMsgPrefix()+"Writing index '" + sitemapConfig.getSitemapIndexFile(), module);
        if (sitemapFilenames.isEmpty()) {
            Debug.logWarning(getLogMsgPrefix()+"No sitemap files were produced - sitemap generator may throw error", module);
        }

        File myDir = getSitemapDirFile();
        myDir.mkdirs();

        File myFile = new File(myDir, sitemapConfig.getSitemapIndexFile());
        try {
            myFile.createNewFile();
        } catch (IOException e) {
            Debug.logInfo(getLogMsgPrefix()+"Index file '" + myFile.toString() + "' may already exist; replacing", module);
            // ignore if file already exists
        }

        SitemapIndexGenerator sig = new SitemapIndexGenerator(getBaseUrl(), myFile);
        for(String url : sitemapFilenames){
            sig.addUrl(getSitemapFileLink(url));
        }
        sig.write();

        Debug.logInfo(getLogMsgPrefix()+"Done writing index '" + sitemapConfig.getSitemapIndexFile() + "'", module);
    }

    public String getSitemapFileLink(String filename) {
        return postprocessSiteMapFileLink(SitemapConfig.concatPaths(getSitemapContextPath(), sitemapConfig.getSitemapDirPath(), filename));
    }

    public String getSitemapIndexFileLink() {
        return getSitemapFileLink(sitemapConfig.getSitemapIndexFile());
    }

    public FullWebappInfo getWebappInfo() {
        return webappInfo;
    }

    /**
     * Use urlrewritefilter rules to convert urls - emulates urlrewritefilter - just like the original url would be
     * WARN: emulation only - see UrlRewriteConf for issues.
     */
    protected String applyUrlRewriteRules(String url) {
        if (url == null) return "";
        if (getUrlRewriter() == null) return url;
        return getUrlRewriter().processOutboundUrl(url, getWebappInfo(), getUrlRewriterCtx());
    }

    protected SeoConfig.UrlFilter matchUrlFilter(String url) {
        for(SeoConfig.UrlFilter urlFilter : getSeoConfig().getUrlFilters()) {
            if (urlFilter.matches(url)) {
                return urlFilter;
            }
        }
        return null;
    }

    protected boolean matchesUrlFilter(String url) {
        return matchUrlFilter(url) != null;
    }

    /**
     * Applies URL rewrite rules and appends baseUrl, as applicable.
     */
    protected String postprocessLink(String webappPathPrefix, String url) {
        // 2018-07-27: we should apply the rules on the whole URL to properly emulate the @pageUrl, even if it's slower
        //return SitemapConfig.concatPaths(getBaseUrl(), webappPathPrefix, applyUrlRewriteRules(url));
        return applyUrlRewriteRules(SitemapConfig.concatPaths(getBaseUrl(), webappPathPrefix, url));
    }

    protected String postprocessElementLink(String url) {
        return postprocessLink(getWebappPathPrefix(), url);
    }

    protected String postprocessSiteMapFileLink(String url) {
        return postprocessLink(getSitemapWebappPathPrefix(), url);
    }

    @Override
    protected String getLogMsgPrefix() {
        return logPrefix+"Website '" + webSiteId + "': ";
    }

    @Override
    protected String getLogErrorPrefix() {
        return logPrefix+"Error generating sitemap for website '" + webSiteId + "': ";
    }

    protected List<String> getCmsUriList(Locale contentLocale) throws GeneralException {
        Map<String, Object> servCtx = getDispatcher().getDispatchContext()
                .makeValidContext("cmsGetWebsiteIndexableProcessMappingUris", ModelService.IN_PARAM, getServCtxOpts());
        servCtx.put("webSiteId", sitemapConfig.getWebSiteId());
        servCtx.put("useCache", isUseCache());
        // TODO: REVIEW: locale handling is loosely defined at the moment
        servCtx.put("contentLocale", contentLocale);
        Map<String, Object> servResult = getDispatcher().runSync("cmsGetWebsiteIndexableProcessMappingUris", servCtx);
        if (ServiceUtil.isSuccess(servResult)) {
            return UtilGenerics.checkList(servResult.get("uriList"));
        } else {
            throw new GeneralException("Could not get website cms URIs: " + ServiceUtil.getErrorMessage(servResult));
        }
    }

    // TODO: move (generic)
    static EntityCondition makeFieldPossibleValuesCond(String fieldName, Collection<?> values) {
        //if (values == null || values.isEmpty()) return null;
        List<EntityCondition> condList = new ArrayList<>(values.size());
        for(Object value : values) {
            condList.add(EntityCondition.makeCondition(fieldName, value));
        }
        return EntityCondition.makeCondition(condList, EntityOperator.OR);
    }

    // TODO: move (generic)
    static List<GenericValue> reorderByStringFieldValues(List<GenericValue> values, String fieldName, Collection<String> orderedFieldValues, boolean unknownLast) {
        if (values == null || values.isEmpty() || orderedFieldValues == null || orderedFieldValues.isEmpty()) return values;

        Map<String, List<GenericValue>> valuesByType = new HashMap<>();
        for(String typeId : orderedFieldValues) { // simplifies the code
            valuesByType.put(typeId, new ArrayList<>());
        }
        List<GenericValue> unknownTypes = new ArrayList<>();

        for(GenericValue cat : values) {
            String typeId = cat.getString(fieldName);

            List<GenericValue> subList = valuesByType.get(typeId);
            if (subList != null) {
                subList.add(cat);
            } else {
                unknownTypes.add(cat);
            }
        }

        List<GenericValue> result = new ArrayList<>();
        if (!unknownLast) result.addAll(unknownTypes);

        for(String typeId : orderedFieldValues) {
            result.addAll(valuesByType.get(typeId));
        }

        if (unknownLast) result.addAll(unknownTypes);
        return result;
    }
}
