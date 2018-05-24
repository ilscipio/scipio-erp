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
import java.util.List;
import java.util.Locale;
import java.util.Map;

import org.ofbiz.base.component.ComponentConfig.WebappInfo;
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
import org.ofbiz.webapp.WebAppUtil;
import org.xml.sax.SAXException;

import com.ilscipio.scipio.ce.util.PathUtil;
import com.ilscipio.scipio.ce.webapp.filter.UrlRewriteConf;
import com.ilscipio.scipio.product.seo.SeoCatalogTraverser;
import com.ilscipio.scipio.product.seo.SeoCatalogUrlWorker;
import com.ilscipio.scipio.product.seo.UrlGenStats;
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
    
    static final String logPrefix = "Seo: Sitemap: ";
    
    protected final List<Locale> locales;
    protected final String webSiteId;
    protected final String baseUrl;
    protected final String sitemapContextPath;
    protected final String contextPath;
    protected final SitemapConfig config;
    protected final SeoCatalogUrlWorker urlWorker;
    
    protected UrlRewriteConf urlRewriteConf;
    protected final WebappInfo webappInfo;
    
    protected final GenericValue webSite;
    protected final GenericValue productStore;
    
    protected final String fullSitemapDir;
    
    public enum ElemType { CATEGORY, PRODUCT, CONTENT }
    
    protected Map<ElemType, ElemHandler> elemHandlers = null;
    protected ElemHandler categoryElemHandler = null; // optimization
    protected ElemHandler productElemHandler = null; // optimization
    protected ElemHandler contentElemHandler = null; // optimization

    protected final Map<String, ?> servCtxOpts;

    protected SitemapGenerator(Delegator delegator, LocalDispatcher dispatcher, List<Locale> locales, String webSiteId, GenericValue webSite, GenericValue productStore, String baseUrl, String sitemapContextPath, String contextPath, SitemapConfig config,
            SeoCatalogUrlWorker urlWorker, UrlRewriteConf urlRewriteConf, SitemapTraversalConfig travConfig, Map<String, ?> servCtxOpts) throws GeneralException, IOException, URISyntaxException, SAXException {
        super(delegator, dispatcher, travConfig);
        this.locales = locales;
        this.webSiteId = webSiteId;
        this.webSite = webSite;
        this.productStore = productStore;
        this.baseUrl = baseUrl;
        this.sitemapContextPath = sitemapContextPath;
        this.contextPath = contextPath;
        this.config = config;
        this.urlWorker = urlWorker;
        this.urlRewriteConf = urlRewriteConf;
        this.webappInfo = WebAppUtil.getWebappInfoFromWebsiteId(webSiteId);
        this.fullSitemapDir = config.getSitemapDirUrlLocation(webappInfo.getLocation());
        this.servCtxOpts = servCtxOpts;
        getSitemapDirFile(); // test this for exception
        reset();
    }

    public static SitemapGenerator getWorkerForWebsite(Delegator delegator, LocalDispatcher dispatcher, String webSiteId, Map<String, ?> servCtxOpts, boolean useCache) throws GeneralException, IOException, URISyntaxException, SAXException, IllegalArgumentException {
        // TODO: LOCALIZE WITH PROP MESSAGE EXCEPTIONS
        
        GenericValue webSite = delegator.findOne("WebSite", UtilMisc.toMap("webSiteId", webSiteId), useCache);
        if (webSite == null) throw new GeneralException("website not found: " + webSiteId);
        String productStoreId = webSite.getString("productStoreId");
        if (UtilValidate.isEmpty(productStoreId)) throw new GeneralException("website has no product store: " + webSiteId);

        GenericValue productStore = delegator.findOne("ProductStore", UtilMisc.toMap("productStoreId", productStoreId), useCache);
        if (productStore == null) throw new GeneralException("store not found: " + productStoreId);

        SitemapConfig config = SitemapConfig.getSitemapConfigForWebsite(delegator, dispatcher, webSiteId);
        
        return getWorkerForWebsite(delegator, dispatcher, webSite, productStore, config, servCtxOpts, useCache);
    }
    
    public static SitemapGenerator getWorkerForWebsite(Delegator delegator, LocalDispatcher dispatcher, GenericValue webSite, GenericValue productStore, SitemapConfig config, Map<String, ?> servCtxOpts, boolean useCache) throws GeneralException, IOException, URISyntaxException, SAXException {
        String webSiteId = webSite.getString("webSiteId");
        if (config == null) throw new IOException("no valid sitemap config for website '" + webSiteId + "'");
        String sitemapContextPath = config.getSitemapContextPath();
        if (sitemapContextPath == null) {
            sitemapContextPath = config.getDefaultContextPath(delegator);
        }
        String contextPath = config.getContextPath();
        if (contextPath == null) {
            contextPath = config.getDefaultContextPath(delegator);
        }
        String baseUrl = config.getBaseUrl();
        if (baseUrl == null) {
            baseUrl = config.getDefaultBaseUrl(delegator, config.isBaseUrlSecure());
        }
        UrlRewriteConf urlRewriteConf = null;
        if (config.getUrlConfPath() != null) {
            urlRewriteConf = UrlRewriteConf.loadConf(config.getUrlConfPath());
        }
        
        SitemapTraversalConfig travConfig = (SitemapTraversalConfig) new SitemapTraversalConfig(config).setDoContent(config.isDoContent()).setUseCache(useCache);
        return new SitemapGenerator(delegator, dispatcher, 
                config.getLocalesOrDefault(webSite, productStore), 
                webSiteId, webSite, productStore,
                baseUrl, sitemapContextPath, contextPath, config,
                SeoCatalogUrlWorker.getInstance(delegator, webSiteId),
                urlRewriteConf,
                travConfig,
                servCtxOpts);
    }

    public static class SitemapTraversalConfig extends SeoTraversalConfig {
        public SitemapTraversalConfig(SitemapConfig sitemapConfig) {
            setDoCategory(sitemapConfig.isDoCategory());
            setDoProduct(sitemapConfig.isDoProduct());
        }
    }
    
    @Override
    public SitemapTraversalConfig newTravConfig() {
        return new SitemapTraversalConfig(config);
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
    
    public SitemapConfig getConfig() {
        return config;
    }
    
    @Override
    public void reset() throws GeneralException {
        super.reset();
        resetElemHandlers();
        //resetTrailNames(); // moved to TraversalState
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
    
    protected String getSitemapContextPath() {
        return sitemapContextPath;
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
    protected UrlRewriteConf getUrlRewriteConf() {
        return urlRewriteConf;
    }
    
    public String getFullSitemapDir() {
        return fullSitemapDir;
    }
    
    protected File getSitemapDirFile() throws IOException, URISyntaxException {
        String fullSitemapDir = getFullSitemapDir();
        try {
            URL url = FlexibleLocation.resolveLocation(fullSitemapDir);
            if (url == null) throw new MalformedURLException("Cannot resolve location: " + getFullSitemapDir());
            return new File(url.toURI());
        } catch(Exception e) {
            throw new IOException("Error resolving sitemap directory: " + fullSitemapDir + ": " + e.getMessage(), e);
        }
    }
    
    protected WebSitemapGenerator getSitemapGenerator(String filePrefix) throws IOException, URISyntaxException {
        File myDir = getSitemapDirFile();
        myDir.mkdirs();
        return WebSitemapGenerator.builder(getBaseUrl(), myDir).fileNamePrefix(filePrefix).dateFormat(config.getDateFormat()).gzip(config.isGzip()).build();
    }
    
    /**
     * The main iteration call for product/category sitemap generation - wrapper 
     * around {@link #traverseCategoriesDepthFirst(List)}, plus content.
     */
    public void buildSitemapDeepForWebsite() throws GeneralException {
        traverseProductStoreDfs(productStore);
        buildSitemapForContent();
    }
 
    /**
     * Content link generation.
     */
    public void buildSitemapForContent() throws GeneralException {
        if (!config.isDoContent()) return;
        buildSitemapForCmsPage();
    }
    
    /**
     * CMS link generation.
     */
    public void buildSitemapForCmsPage() throws GeneralException {
        if (!config.isDoCmsPage()) return;
        
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
        if (config.getProdCatalogIds() != null) {
            cond = EntityCondition.makeCondition(cond, EntityOperator.AND, 
                    makeFieldPossibleValuesCond("prodCatalogId", config.getProdCatalogIds()));
        }
        return cond;
    }
    
    @Override
    public EntityCondition makeProdCatalogCategoryCond(String prodCatalogId) {
        EntityCondition cond = EntityCondition.makeCondition("prodCatalogId", prodCatalogId);
        if (config.getProdCatalogCategoryTypeIds() != null) {
            cond = EntityCondition.makeCondition(cond, EntityOperator.AND, 
                    makeFieldPossibleValuesCond("prodCatalogCategoryTypeId", config.getProdCatalogCategoryTypeIds()));
        }
        return cond;
    }
    
    /**
     * If applicable, reorder by the prodCatalogIds in the config, so that order won't change randomly.
     */
    @Override
    public List<GenericValue> filterProductStoreCatalogList(List<GenericValue> productStoreCatalogList) {
        if (config.getProdCatalogIds() == null || config.getProdCatalogIds().size() <= 1) {
            return productStoreCatalogList;
        } else {
            return reorderByStringFieldValues(productStoreCatalogList, "prodCatalogId", 
                    config.getProdCatalogIds(), true);
        }
    }
    
    /**
     * If applicable, reorder by the prodCatalogCategoryTypeIds in the config, so that order won't change randomly.
     */
    @Override
    public List<GenericValue> filterProdCatalogCategoryList(List<GenericValue> prodCatalogCategoryList) {
        if (config.getProdCatalogCategoryTypeIds() == null || config.getProdCatalogCategoryTypeIds().size() <= 1) {
            return prodCatalogCategoryList;
        } else {
            return reorderByStringFieldValues(prodCatalogCategoryList, "prodCatalogCategoryTypeId", 
                    config.getProdCatalogCategoryTypeIds(), true);
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
        Map<Locale, List<String>> trailNames = getTrailNames(state);
        for(Locale locale : locales) {
            // NOTE: this is non-last - cannot reuse the one determined in previous call
            String trailName = urlWorker.getCategoryUrlTrailName(getDelegator(), getDispatcher(), locale, productCategory, false, isUseCache());       
            trailNames.get(locale).add(trailName); // no need copy, just remove after
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
        buildSitemapCategoryLink(productCategory, getTrailNames(state));
    }

    @Override
    public void visitProduct(GenericValue product, TraversalState state) throws GeneralException {
        buildSitemapProductLink(product, getTrailNames(state));
    }

    protected void buildSitemapCategoryLink(GenericValue productCategory, Map<Locale, List<String>> trailNames) throws GeneralException {
        String productCategoryId = productCategory.getString("productCategoryId");
        try {
            
            // TODO: missing multi-locale link support - unclear if library supports
            
            Locale locale = getDefaultLocale();
            List<String> trail = trailNames.get(locale);

            String url = urlWorker.makeCategoryUrlPath(getDelegator(), getDispatcher(), locale, productCategory, trail, getContextPath(), isUseCache()).toString();
            url = SitemapConfig.concatPaths(getBaseUrl(), applyUrlRewriteRules(url));
            
            if (Debug.verboseOn()) Debug.logVerbose(getLogMsgPrefix()+"Processing category url: " + url, module);
            
            WebSitemapUrl libUrl = buildSitemapLibUrl(url, null);
            getCategoryElemHandler().addUrl(libUrl);
            getStats().categorySuccess++;
        } catch(Exception e) {
            stats.categoryError++;
            Debug.logError(getLogErrorPrefix() + "Cannot build URL for category '" + productCategoryId + "': " + e.getMessage(), module);
        }
    }
    
    protected void buildSitemapProductLink(GenericValue product, Map<Locale, List<String>> trailNames) throws GeneralException {
        if (!config.isIncludeVariant() && "Y".equals(product.getString("isVariant"))) {
            stats.productSkipped++;
            return;
        }
        
        String productId = product.getString("productId");
        try {
            
            // TODO: missing multi-locale link support - unclear if library supports
            
            Locale locale = getDefaultLocale();
            List<String> trail = trailNames.get(locale);
            
            String url = urlWorker.makeProductUrlPath(getDelegator(), getDispatcher(), locale, product, trail, getContextPath(), isUseCache()).toString();
            url = postprocessUrl(url);

            if (Debug.verboseOn()) Debug.logVerbose(getLogMsgPrefix()+"Processing product url: " + url, module);

            WebSitemapUrl libUrl = buildSitemapLibUrl(url, config.isUseProductLastModDate() ? product.getTimestamp("lastModifiedDate") : null);
            getProductElemHandler().addUrl(libUrl);
            stats.productSuccess++;
            
            // TODO?: is there need to do variants (not explicitly associated to category)? 
            // usually don't want to advertise the variants unless attached to category for some reason?...
            //if (config.doChildProduct) {  
            //}
        } catch(Exception e) {
            stats.productError++;
            Debug.logError(getLogErrorPrefix() + "Cannot build URL for product '" + productId + "': " + e.getMessage(), module);
        } 
    }

    protected void buildSitemapCmsPageLink(String uri, Locale locale) {
        try {
            uri = PathUtil.concatPaths(getContextPath(), uri);
            String url = postprocessUrl(uri);
            
            if (Debug.verboseOn()) Debug.logVerbose(getLogMsgPrefix()+"Processing CMS page url: " + url, module);

            WebSitemapUrl libUrl = buildSitemapLibUrl(url, null);
            getContentElemHandler().addUrl(libUrl);
            
            stats.contentSuccess++;
        } catch(Exception e) {
            stats.contentError++;
            Debug.logError(getLogErrorPrefix()+"Error processing cms page URI: " + uri + ": " + e.getMessage(), module);
        }
    }

    protected void resetElemHandlers() {
        this.elemHandlers = createElemHandlers();
        this.categoryElemHandler = elemHandlers.get(ElemType.CATEGORY);
        this.productElemHandler = elemHandlers.get(ElemType.PRODUCT);
        this.contentElemHandler = elemHandlers.get(ElemType.CONTENT);
    }
    
    protected Map<ElemType, ElemHandler> createElemHandlers() {
        Map<ElemType, ElemHandler> elemHandlers = new EnumMap<>(ElemType.class);
        if (config.isDoCategory()) {
            elemHandlers.put(ElemType.CATEGORY, new CategoryElemHandler());
        }
        if (config.isDoProduct()) {
            elemHandlers.put(ElemType.PRODUCT, new ProductElemHandler());
        }
        if (config.isDoContent()) {
            elemHandlers.put(ElemType.CONTENT, new ContentElemHandler());
        }
        return elemHandlers;
    }
    
    protected ElemHandler getCategoryElemHandler() {
        return categoryElemHandler;
    }
    
    protected ElemHandler getProductElemHandler() {
        return productElemHandler;
    }
    
    protected ElemHandler getContentElemHandler() {
        return contentElemHandler;
    }
    
    protected abstract class ElemHandler {
        private WebSitemapGenerator wsg = null;
        private List<String> sitemapFiles = new ArrayList<>();
        private long urlCount = 0;
        private long sitemapFileIndex = 0;
        
        public WebSitemapGenerator getWsg() { return wsg; }
        public List<String> getSitemapFiles() { return sitemapFiles; }
        public long getUrlCount() { return urlCount; }
        public long getSitemapFileIndex() { return sitemapFileIndex; }

        public abstract String getTypeFilenamePrefix();
        
        public String getNumberedSitemapFilenamePrefix() {
            return getTypeFilenamePrefix() + sitemapFileIndex;
        }
        
        public String getSitemapFilename() {
            return getNumberedSitemapFilenamePrefix() + "." + config.getSitemapExtension();
        }
        
        public void addUrl(WebSitemapUrl url) throws IOException, URISyntaxException {
            if (config.getSizemapSize() != null && urlCount >= config.getSizemapSize()) {
                commitSitemapFile();
            }
            if (wsg == null) {
                beginSitemapFile();
            }
            wsg.addUrl(url);
            urlCount++;
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
    }

    protected class CategoryElemHandler extends ElemHandler {
        @Override protected ElemType getType() { return ElemType.CATEGORY; }
        @Override public String getTypeFilenamePrefix() { return config.getCategoryFilePrefix(); }
    }
    
    protected class ProductElemHandler extends ElemHandler {
        @Override protected ElemType getType() { return ElemType.PRODUCT; }
        @Override public String getTypeFilenamePrefix() { return config.getProductFilePrefix(); }
    }
    
    protected class ContentElemHandler extends ElemHandler {
        @Override protected ElemType getType() { return ElemType.CONTENT; }
        @Override public String getTypeFilenamePrefix() { return config.getContentFilePrefix(); }
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
        Debug.logInfo(getLogMsgPrefix()+"Writing index '" + config.getSitemapIndexFile(), module);
        if (sitemapFilenames.isEmpty()) {
            Debug.logWarning(getLogMsgPrefix()+"No sitemap files were produced - sitemap generator may throw error", module);
        }

        File myDir = getSitemapDirFile();
        myDir.mkdirs();
        
        File myFile = new File(myDir, config.getSitemapIndexFile());
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
        
        Debug.logInfo(getLogMsgPrefix()+"Done writing index '" + config.getSitemapIndexFile() + "'", module);
    }
    
    public String getSitemapFileLink(String filename) {
        return SitemapConfig.concatPaths(getBaseUrl(), getSitemapContextPath(), config.getSitemapDirPath(), filename);
    }
    
    public String getSitemapIndexFileLink() {
        return getSitemapFileLink(config.getSitemapIndexFile());
    }

    /**
     * Use urlrewritefilter rules to convert urls - emulates urlrewritefilter - just like the original url would be
     * WARN: emulation only - see UrlRewriteConf for issues.
     */
    protected String applyUrlRewriteRules(String url) {
        if (url == null) return "";
        if (getUrlRewriteConf() == null) return url;
        return getUrlRewriteConf().processOutboundUrl(url);
    }
    
    /**
     * Applies URL rewrite rules and appends baseUrl, as applicable.
     */
    protected String postprocessUrl(String url) {
        return SitemapConfig.concatPaths(getBaseUrl(), applyUrlRewriteRules(url));
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
        servCtx.put("webSiteId", config.getWebSiteId());
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
            valuesByType.put(typeId, new ArrayList<GenericValue>());
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
