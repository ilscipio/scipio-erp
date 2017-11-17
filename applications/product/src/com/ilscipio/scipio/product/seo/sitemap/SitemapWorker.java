package com.ilscipio.scipio.product.seo.sitemap;

import java.io.File;
import java.io.IOException;
import java.net.URISyntaxException;
import java.sql.Timestamp;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.Locale;

import org.ofbiz.base.location.FlexibleLocation;
import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.GeneralException;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.GenericValue;
import org.ofbiz.entity.util.EntityQuery;
import org.ofbiz.service.LocalDispatcher;

import com.ilscipio.scipio.ce.webapp.filter.UrlRewriteConf;
import com.ilscipio.scipio.product.seo.SeoCatalogUrlWorker;
import com.ilscipio.scipio.product.seo.SeoUrlUtil.UrlGenStats;
import com.redfin.sitemapgenerator.SitemapIndexGenerator;
import com.redfin.sitemapgenerator.WebSitemapGenerator;
import com.redfin.sitemapgenerator.WebSitemapUrl;

/**
 * Builds sitemap and records stats.
 * <p>
 * NOT thread-safe.
 */
public class SitemapWorker {

    public static final String module = SitemapWorker.class.getName();
    
    protected final Delegator delegator;
    protected final LocalDispatcher dispatcher;
    protected final Locale locale;
    protected final String webSiteId;
    protected final String contextPath;
    protected final SitemapConfig config;
    protected final SeoCatalogUrlWorker urlWorker;
    protected final boolean useCache;
    
    static final String logPrefix = "Seo: Sitemap: ";
    
    protected UrlRewriteConf urlRewriteConf;
    
    protected UrlGenStats stats = null;
    protected WebSitemapGenerator productWsg = null;
    protected WebSitemapGenerator categoryWsg = null;
    protected List<String> productSitemapFiles = null;
    protected List<String> categorySitemapFiles = null;
    protected long productUrlCount = 0;
    protected long categoryUrlCount = 0;
    protected long productSitemapFileIndex = 0;
    protected long categorySitemapFileIndex = 0;
    
    protected SitemapWorker(Delegator delegator, LocalDispatcher dispatcher, Locale locale, String webSiteId, String contextPath, SitemapConfig config,
            SeoCatalogUrlWorker urlWorker, UrlRewriteConf urlRewriteConf, boolean useCache) throws GeneralException, IOException, URISyntaxException {
        this.delegator = delegator;
        this.dispatcher = dispatcher;
        this.locale = locale;
        this.webSiteId = webSiteId;
        this.contextPath = contextPath;
        this.config = config;
        this.urlWorker = urlWorker;
        this.urlRewriteConf = urlRewriteConf;
        this.useCache = useCache;
        reset();
    }

    public static SitemapWorker getWorkerForWebsite(Delegator delegator, LocalDispatcher dispatcher, String webSiteId, Locale locale, boolean useCache) throws GeneralException, IOException, URISyntaxException {
        SitemapConfig config = SitemapConfig.getSitemapConfigForWebsite(delegator, dispatcher, webSiteId);
        String contextPath = config.getContextPath();
        UrlRewriteConf urlRewriteConf = null;
        if (config.getUrlConfPath() != null) {
            urlRewriteConf = UrlRewriteConf.loadConf(config.getUrlConfPath());
        }
        return new SitemapWorker(delegator, dispatcher, locale, webSiteId,
                contextPath, config,
                SeoCatalogUrlWorker.getInstance(delegator, webSiteId),
                urlRewriteConf,
                useCache);
    }

    public SitemapConfig getConfig() {
        return config;
    }
    
    public void reset() throws IOException, URISyntaxException {
        this.stats = new UrlGenStats(config.doProduct, config.doCategory, false);
        this.productWsg = null;
        this.categoryWsg = null;
        this.productSitemapFiles = new ArrayList<>();
        this.categorySitemapFiles = new ArrayList<>();
        this.productUrlCount = 0;
        this.categoryUrlCount = 0;
        this.productSitemapFileIndex = 0;
        this.categorySitemapFileIndex = 0;
    }
    
    public UrlGenStats getStats() {
        return stats;
    }
    
    protected String getContextPath() {
        return contextPath;
    }
    
    /**
     * Gets cached conf.
     * Avoids reloading the urlrewrite.xml file for every single URL.
     */
    protected UrlRewriteConf getUrlRewriteConf() {
        return urlRewriteConf;
    }
    
    protected WebSitemapGenerator getSitemapGenerator(String filePrefix) throws IOException, URISyntaxException {
        File myDir = new File(FlexibleLocation.resolveLocation(config.sitemapDir).toURI());
        myDir.mkdirs();
        return WebSitemapGenerator.builder(config.baseUrl, myDir).fileNamePrefix(filePrefix).dateFormat(config.dateFormat).gzip("gzip".equals(config.getCompress())).build();
    }
    
    public void buildSitemapDeep(List<GenericValue> productCategories) throws GeneralException, IOException, URISyntaxException {
        buildSitemapDeepInternal(productCategories, new ArrayList<String>());
    }
    
    protected void buildSitemapDeepInternal(List<GenericValue> categoryAssocList, List<String> trailNames) throws GeneralException, IOException {
        for (GenericValue categoryAssoc : categoryAssocList) {
            GenericValue category = null;
            if (categoryAssoc.getModelEntity().getEntityName().equals("ProductCategoryRollup")) {
                category = categoryAssoc.getRelatedOne("CurrentProductCategory", useCache);
            } else if (categoryAssoc.getModelEntity().getEntityName().equals("ProdCatalogCategory")) {
                category = categoryAssoc.getRelatedOne("ProductCategory", useCache);
            }
            if (category == null) {
                Debug.logError(getLogMsgPrefix()+"Schema error: Could not get related ProductCategory for: " + categoryAssoc, module);
                continue;
            }
            
            // self
            if (config.doCategory) {
                buildSitemapCategory(category, trailNames);
            }
            
            // NOTE: this is non-last - cannot reuse the one determined in previous call
            String trailName = urlWorker.getCategoryUrlTrailName(delegator, dispatcher, locale, category, false);       
            trailNames.add(trailName); // no need copy, just remove after
            try {
                // child cats (recursive)
                List<GenericValue> childProductCategoryRollups = EntityQuery.use(delegator).from("ProductCategoryRollup")
                        .where("parentProductCategoryId", category.getString("productCategoryId")).filterByDate().cache(useCache).queryList(); // not need: .orderBy("sequenceNum")
                if (UtilValidate.isNotEmpty(childProductCategoryRollups)) {
                    buildSitemapDeepInternal(childProductCategoryRollups, trailNames);
                }
    
                // products
                if (config.doProduct) {
                    List<GenericValue> productCategoryMembers = EntityQuery.use(delegator).from("ProductCategoryMember")
                            .where("productCategoryId", category.getString("productCategoryId")).filterByDate()
                            .cache(useCache).queryList(); // not need: .orderBy("sequenceNum")
                    if (UtilValidate.isNotEmpty(productCategoryMembers)) {
                        for (GenericValue productCategoryMember : productCategoryMembers) {
                            GenericValue product = productCategoryMember.getRelatedOne("Product", useCache);
                            buildSitemapProduct(product, trailNames);
                        }
                    }
                }
            } finally {
                trailNames.remove(trailNames.size() - 1);
            }
        }
    }
    
    protected void buildSitemapCategory(GenericValue productCategory, List<String> trailNames) throws GeneralException, IOException {
        String productCategoryId = productCategory.getString("productCategoryId");
        try {
            String url = urlWorker.makeCategoryUrlPath(delegator, dispatcher, locale, productCategory, trailNames, getContextPath()).toString();
            
            url = (config.baseUrl != null ? config.baseUrl : "") + applyUrlRewriteRules(url);
            
            if (Debug.verboseOn()) Debug.logVerbose(getLogMsgPrefix()+"Creating category url: " + url, module);
            
            WebSitemapUrl sitemapUrl = new WebSitemapUrl.Options(url).build();
            
            addCategoryUrl(sitemapUrl);
            stats.categorySuccess++;
        } catch(Exception e) {
            stats.categoryError++;
            Debug.logError(getLogErrorPrefix() + "Cannot build URL for category '" + productCategoryId + "': " + e.getMessage(), module);
        }
    }
    
    protected void buildSitemapProduct(GenericValue product, List<String> trailNames) throws GeneralException, IOException {
        String productId = product.getString("productId");
        try {
            String url = urlWorker.makeProductUrlPath(delegator, dispatcher, locale, product, trailNames, getContextPath()).toString();
            
            url = (config.baseUrl != null ? config.baseUrl : "") + applyUrlRewriteRules(url);
            
            if (Debug.verboseOn()) Debug.logVerbose(getLogMsgPrefix()+"Creating product url: " + url, module);
            
            Timestamp lastModifiedDateTs = product.getTimestamp("lastModifiedDate");
            Date lastModifiedDate = lastModifiedDateTs != null ? new Date(lastModifiedDateTs.getTime()) : null;
                    
            WebSitemapUrl.Options opts = new WebSitemapUrl.Options(url);
            if (lastModifiedDate != null) opts = opts.lastMod(lastModifiedDate);
            WebSitemapUrl productUrl = opts.build();
            
            addProductUrl(productUrl);
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
    
    protected void addProductUrl(WebSitemapUrl url) throws IOException, URISyntaxException {
        if (config.sizemapSize != null && productUrlCount > config.sizemapSize) {
            commitProductSitemap();
        }
        
        if (productWsg == null) {
            productWsg = getSitemapGenerator(config.productFilePrefix);
            productUrlCount = 0;
            productSitemapFileIndex++;
            Debug.logInfo(getLogMsgPrefix()+"Building product sitemap file number " + productSitemapFileIndex, module);
        }
        
        productWsg.addUrl(url);
        productUrlCount++;
    }
    
    protected void addCategoryUrl(WebSitemapUrl url) throws IOException, URISyntaxException {
        if (config.sizemapSize != null && categoryUrlCount > config.sizemapSize) {
            commitCategorySitemap();
        }
        
        if (categoryWsg == null) {
            categoryWsg = getSitemapGenerator(config.categoryFilePrefix);
            categoryUrlCount = 0;
            categorySitemapFileIndex++;
            Debug.logInfo(getLogMsgPrefix()+"Building category sitemap file number " + categorySitemapFileIndex, module);
        }
        
        categoryWsg.addUrl(url);
        categoryUrlCount++;
    }
    
    protected void commitProductSitemap() {
        Debug.logInfo(getLogMsgPrefix()+"Writing product sitemap file number " + productSitemapFileIndex + " (" + productUrlCount + " entries)", module);
        productWsg.write();
        productSitemapFiles.add(config.productFilePrefix + productSitemapFileIndex + "." + config.sitemapExtension);
        productWsg = null;
    }
    
    protected void commitCategorySitemap() {
        Debug.logInfo(getLogMsgPrefix()+"Writing category sitemap file number " + categorySitemapFileIndex + " (" + categoryUrlCount + " entries)", module);
        categoryWsg.write();
        categorySitemapFiles.add(config.categoryFilePrefix + categorySitemapFileIndex + "." + config.sitemapExtension);
        categoryWsg = null;
    }
    
    public void commitSitemaps() {
        if (productWsg != null) {
            commitProductSitemap();
        }
        if (categoryWsg != null) {
            commitCategorySitemap();
        }
    }
    
    public void commitSitemapsAndIndex() throws IOException, URISyntaxException {
        commitSitemaps();
        List<String> sitemapFiles = new ArrayList<>(productSitemapFiles.size() + categorySitemapFiles.size());
        sitemapFiles.addAll(productSitemapFiles);
        sitemapFiles.addAll(categorySitemapFiles);
        generateSitemapIndex(sitemapFiles);
    }
    
    protected void writeSitemap(List<WebSitemapUrl> urlList, String filePrefix) throws IOException, URISyntaxException {
        WebSitemapGenerator wsg = getSitemapGenerator(filePrefix);
        for(WebSitemapUrl url : urlList){
            wsg.addUrl(url);
        }
        wsg.write();
    }

    public void generateSitemapIndex(List<String> siteMapLists) throws IOException, URISyntaxException {
        Debug.logInfo(getLogMsgPrefix()+"Writing index '" + config.sitemapIndexFile, module);

        File myDir = new File(FlexibleLocation.resolveLocation(config.sitemapDir).toURI());
        myDir.mkdirs();
        
        File myFile = new File(myDir, config.sitemapIndexFile);
        try {
            myFile.createNewFile();
        } catch (IOException e) {
            Debug.logInfo(getLogMsgPrefix()+"Index file '" + myFile.toString() + "' may already exist; replacing", module);
            // ignore if file already exists
        }
        
        SitemapIndexGenerator sig = new SitemapIndexGenerator(config.baseUrl, myFile);
        for(String url : siteMapLists){
            sig.addUrl(concatPaths(config.baseUrl, config.sitemapSubdir, url));
        }
        sig.write();
        
        Debug.logInfo(getLogMsgPrefix()+"Done writing index '" + config.sitemapIndexFile + "'", module);
    }

    /**
     * Use urlrewritefilter rules to convert urls - emulates urlrewritefilter - just like the original url would be
     * TODO: REVIEW: 2017: urlrewritefilter internals may have changed...
     */
    public String applyUrlRewriteRules(String url) {
        if (url == null) return "";
        UrlRewriteConf urlRewriteConf = getUrlRewriteConf();
        if (urlRewriteConf == null) return url;
        return urlRewriteConf.processOutboundUrl(url);
    }
    
    protected String getLogMsgPrefix() {
        return logPrefix+"Website '" + webSiteId + "': ";
    }
    
    protected String getLogErrorPrefix() {
        return logPrefix+"Error generating sitemap for website '" + webSiteId + "': ";
    }
    
    protected String getLogErrorMsg(Throwable t) {
        return getLogErrorPrefix() + t.getMessage();
    }
    
    // TODO: use a util and delete this
    private static String concatPaths(String... parts) {
        StringBuilder sb = new StringBuilder();
        for(String part : parts) {
            if (part == null || part.isEmpty()) continue;
            else if (sb.length() == 0) sb.append(part);
            else {
                if (sb.charAt(sb.length() - 1) == '/') {
                    if (part.startsWith("/")) {
                        sb.append(part.substring(1));
                    } else {
                        sb.append(part);
                    }
                } else {
                    if (part.startsWith("/")) {
                        sb.append(part);
                    } else {
                        sb.append('/');
                        sb.append(part);
                    }
                }
            }
        }
        return sb.toString();
    }
}
