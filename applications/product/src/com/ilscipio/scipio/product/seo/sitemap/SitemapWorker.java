package com.ilscipio.scipio.product.seo.sitemap;

import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.net.MalformedURLException;
import java.net.URISyntaxException;
import java.net.URL;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.Locale;

import org.ofbiz.base.location.FlexibleLocation;
import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.GeneralException;
import org.ofbiz.base.util.UtilMisc;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.GenericEntityException;
import org.ofbiz.entity.GenericValue;
import org.ofbiz.entity.util.EntityQuery;
import org.ofbiz.product.product.ProductContentWrapper;
import org.ofbiz.service.LocalDispatcher;
import org.tuckey.web.filters.urlrewrite.Conf;
import org.tuckey.web.filters.urlrewrite.OutboundRule;

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
    
    @Deprecated
    private static final String URL_HYPHEN = "-";
    
    protected final Delegator delegator;
    protected final LocalDispatcher dispatcher;
    protected final String webSiteId;
    protected final SitemapConfig config;
    protected final Locale defaultLocale;
    protected final boolean useCache;
    
    private static final String logPrefix = "Seo: Sitemap: ";
    
    protected UrlGenStats stats = null;
    protected WebSitemapGenerator productWsg = null;
    protected WebSitemapGenerator categoryWsg = null;
    protected List<String> productSitemapFiles = null;
    protected List<String> categorySitemapFiles = null;
    protected long productUrlCount = 0;
    protected long categoryUrlCount = 0;
    protected long productSitemapFileIndex = 0;
    protected long categorySitemapFileIndex = 0;
    
    public SitemapWorker(Delegator delegator, LocalDispatcher dispatcher, String webSiteId, SitemapConfig config,
            Locale defaultLocale, boolean useCache) throws GeneralException, IOException, URISyntaxException {
        this.delegator = delegator;
        this.dispatcher = dispatcher;
        this.webSiteId = webSiteId;
        this.config = config;
        this.defaultLocale = defaultLocale;
        this.useCache = useCache;
        reset();
    }

    public static SitemapWorker getWorkerForWebsite(Delegator delegator, LocalDispatcher dispatcher, String webSiteId, Locale defaultLocale, boolean useCache) throws GeneralException, IOException, URISyntaxException {
        return new SitemapWorker(delegator, dispatcher, webSiteId, 
                SitemapConfig.getSitemapConfigForWebsite(delegator, dispatcher, webSiteId),
                defaultLocale,
                useCache);
    }

    public SitemapConfig getConfig() {
        return config;
    }
    
    public void reset() throws IOException, URISyntaxException {
        this.stats = new UrlGenStats(config.doProduct, config.doCategory);
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
    
    protected WebSitemapGenerator getSitemapGenerator(String filePrefix) throws IOException, URISyntaxException {
        File myDir = new File(FlexibleLocation.resolveLocation(config.sitemapDir).toURI());
        myDir.mkdirs();
        return WebSitemapGenerator.builder(config.baseUrl, myDir).fileNamePrefix(filePrefix).dateFormat(config.dateFormat).gzip(true).build();
    }
    
    public void buildSitemapDeep(List<GenericValue> productCategories) throws GeneralException, IOException, URISyntaxException {
        buildSitemapDeepInternal(productCategories);
    }
    
    protected void buildSitemapDeepInternal(List<GenericValue> productCategories) throws GeneralException, IOException {
        for (GenericValue productCategory : productCategories) {
            GenericValue category = null;
            if (productCategory.getModelEntity().getEntityName().equals("ProductCategoryRollup")) {
                category = productCategory.getRelatedOne("CurrentProductCategory", useCache);
            } else if (productCategory.getModelEntity().getEntityName().equals("ProdCatalogCategory")) {
                category = productCategory.getRelatedOne("ProductCategory", useCache);
            }
            if (category != null) {
                // self
                if (config.doCategory) {
                    buildSitemapCategory(category);
                }
                
                // child cats (recursive)
                List<GenericValue> childProductCategoryRollups = EntityQuery.use(delegator).from("ProductCategoryRollup")
                        .where("parentProductCategoryId", category.getString("productCategoryId")).filterByDate().cache(useCache).queryList(); // not need: .orderBy("sequenceNum")
                if (UtilValidate.isNotEmpty(childProductCategoryRollups)) {
                    buildSitemapDeepInternal(childProductCategoryRollups);
                }

                // products
                if (config.doProduct) {
                    List<GenericValue> productCategoryMembers = EntityQuery.use(delegator).from("ProductCategoryMember")
                            .where("productCategoryId", category.getString("productCategoryId")).filterByDate()
                            .cache(useCache).queryList(); // not need: .orderBy("sequenceNum")
                    if (UtilValidate.isNotEmpty(productCategoryMembers)) {
                        for (GenericValue productCategoryMember : productCategoryMembers) {
                            GenericValue product = productCategoryMember.getRelatedOne("Product", useCache);
                            buildSitemapProduct(product, false);
                        }
                    }
                }
            }
        }
    }
    
    protected void buildSitemapCategory(GenericValue productCategory) throws GeneralException, IOException {
        String productCategoryId = productCategory.getString("productCategoryId");
        try {
            
            // FIXME: TOTAL REWRITE NEEDED
            
            String categoryName = productCategory.getString("categoryName"); //CategoryContentWrapper.getProductCategoryContentAsText(category, "CATEGORY_NAME", defaultLocale, dispatcher);
            if(categoryName != null){
                String catName=categoryName.replaceAll(" ",URL_HYPHEN).replaceAll(",", "");
                StringBuilder urlBuilder = new StringBuilder("/");
                urlBuilder.append(catName);
                urlBuilder.append(URL_HYPHEN);
                urlBuilder.append(productCategoryId);
                String url = urlBuilder.toString();                     
                String rewriteUrl = applyUrlRewriteRules(url);
                if(Debug.verboseOn())Debug.logInfo("Created url " + rewriteUrl, module);
                WebSitemapUrl sitemapUrl = new WebSitemapUrl.Options(rewriteUrl).build();
                
                addCategoryUrl(sitemapUrl);
                
                stats.categorySuccess++;
            } else {
                Debug.logWarning(getMsgLogPrefix()+"Category '" + productCategoryId + "' has no name; skipping", module);
                stats.categorySkipped++;
            }
        } catch(Exception e) {
            stats.categoryError++;
            Debug.logError(e.getMessage(), getErrorLogPrefix() + "cannot build URL for category '" + productCategoryId + "': " + e.getMessage(), module);
        }
    }
    
    protected void buildSitemapProduct(GenericValue product, boolean doChildProducts) throws GeneralException, IOException {
        String productId = product.getString("productId");
        try {
            
            // FIXME: TOTAL REWRITE NEEDED
            
            ProductContentWrapper wrapper = new ProductContentWrapper(dispatcher, product, defaultLocale, "text/plain");
            String url = null; //CatalogUrlSeoTransform.makeProductUrl("/",
                    //null, product.getString("productId"),
                    //wrapper.get("PRODUCT_NAME").toString(), null, null);
            String rewriteUrl = applyUrlRewriteRules(url);
            if (Debug.verboseOn()) Debug.logVerbose(getMsgLogPrefix()+"Created url " + rewriteUrl, module);
            Date lastModifiedDate = new Date(product.getTimestamp("lastModifiedDate").getTime());
            WebSitemapUrl productUrl = new WebSitemapUrl.Options(rewriteUrl)
                    .lastMod(lastModifiedDate).build();
            
            addProductUrl(productUrl);
            
            stats.productSuccess++;
            
            // TODO?: is there need to do variants? 
            // usually don't want to advertise the variants unless attached to category for some reason?...
            //if (doChildProducts) {  
            //}
        } catch(Exception e) {
            stats.categoryError++;
            Debug.logError(e.getMessage(), getErrorLogPrefix() + "cannot build URL for product '" + productId + "': " + e.getMessage(), module);
        } 
    }
    
    protected void addProductUrl(WebSitemapUrl url) throws IOException, URISyntaxException {
        if (productUrlCount > config.sizemapSize) {
            commitProductSitemap();
            productWsg = null;
        }
        
        if (productWsg == null) {
            productWsg = getSitemapGenerator(config.productFilePrefix);
            productUrlCount = 0;
            productSitemapFileIndex++;
            Debug.logInfo(getMsgLogPrefix()+"Building product sitemap file number " + productSitemapFileIndex, module);
        }
        
        productWsg.addUrl(url);
        productUrlCount++;
    }
    
    protected void addCategoryUrl(WebSitemapUrl url) throws IOException, URISyntaxException {
        if (categoryUrlCount > config.sizemapSize) {
            commitCategorySitemap();
            categoryWsg = null;
        }
        
        if (categoryWsg == null) {
            categoryWsg = getSitemapGenerator(config.categoryFilePrefix);
            categoryUrlCount = 0;
            categorySitemapFileIndex++;
            Debug.logInfo(getMsgLogPrefix()+"Building category sitemap file number " + categorySitemapFileIndex, module);
        }
        
        categoryWsg.addUrl(url);
        categoryUrlCount++;
    }
    
    protected void commitProductSitemap() {
        Debug.logInfo(getMsgLogPrefix()+"Writing product sitemap file number " + productSitemapFileIndex + " (" + productUrlCount + " entries)", module);
        productWsg.write();
        productSitemapFiles.add(config.productFilePrefix + productSitemapFileIndex + "." + config.sitemapExtension);
    }
    
    protected void commitCategorySitemap() {
        Debug.logInfo(getMsgLogPrefix()+"Writing category sitemap file number " + productSitemapFileIndex + " (" + categoryUrlCount + " entries)", module);
        productWsg.write();
        categorySitemapFiles.add(config.categoryFilePrefix + categorySitemapFileIndex + "." + config.sitemapExtension);
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
        Debug.logInfo(getMsgLogPrefix()+"Writing index '" + config.sitemapIndexFile, module);

        File myDir = new File(FlexibleLocation.resolveLocation(config.sitemapDir).toURI());
        myDir.mkdirs();
        
        File myFile = new File(myDir, config.sitemapIndexFile);
        try {
            myFile.createNewFile();
        } catch (IOException e) {
            Debug.logInfo(getMsgLogPrefix()+"Index file '" + myFile.toString() + "' may already exist; replacing", module);
            // ignore if file already exists
        }
        
        SitemapIndexGenerator sig = new SitemapIndexGenerator(config.baseUrl, myFile);
        for(String url : siteMapLists){
            sig.addUrl(concatPaths(config.baseUrl, config.sitemapSubdir, url));
        }
        sig.write();
        
        Debug.logInfo(getMsgLogPrefix()+"Done writing index " + config.sitemapIndexFile, module);
    }

    /**
     * Use tuckey rules to convert urls - just like the original url would be
     * TODO: REVIEW: 2017: tuckey internals may have changed...
     */
    public String applyUrlRewriteRules(String originalUrl) {
        if (config.urlConfPath == null) return originalUrl;
        
        String rewriteUrl = originalUrl;

        // Taken over from Tuckey UrlRewriter
        if (Debug.verboseOn()) {
            Debug.logVerbose("processing outbound url for " + originalUrl, module);
        }

        if (originalUrl == null) {
            return "";
        }

        // load config
        URL confUrl;
        InputStream inputStream = null;
        try {
            confUrl = FlexibleLocation.resolveLocation(config.urlConfPath);

            inputStream = confUrl.openStream();
            // attempt to retrieve from location other than local WEB-INF

            if (inputStream == null) {
                Debug.logError(getErrorLogPrefix()+"Unable to find urlrewrite conf file at " + config.urlConfPath, module);
            } else {
                Conf conf = new Conf(null, inputStream, config.urlConfPath,
                        confUrl.toString(), false);

                @SuppressWarnings("unchecked")
                final List<OutboundRule> outboundRules = conf.getOutboundRules();
                for (int i = 0; i < outboundRules.size(); i++) {
                    final OutboundRule outboundRule = (OutboundRule) outboundRules
                            .get(i);

                    rewriteUrl = rewriteUrl.replaceAll(outboundRule.getFrom(),
                            outboundRule.getTo());

                    if (rewriteUrl != null) {
                        // means this rule has matched
                        if (Debug.verboseOn()) {
                            Debug.logVerbose(getMsgLogPrefix()+"\"" + outboundRule.getDisplayName() + "\" matched", module);
                        }
                        if (outboundRule.isLast()) {
                            if (Debug.verboseOn()) {
                                Debug.logVerbose(getMsgLogPrefix()+"rule is last", module);
                            }
                            // there can be no more matches on this request
                            break;
                        }
                    }
                }

            }
        } catch (MalformedURLException e) {
            Debug.logError(e, getErrorLogMsg(e), module);
        } catch (IOException e) {
            Debug.logError(e, getErrorLogMsg(e), module);
        } finally {
            try {
                if (inputStream != null) inputStream.close();
            } catch (IOException e) {
            }
        }

        return config.baseUrl + rewriteUrl;
    }

    /**
     * Returns canonical Url for a product (Example: https://www.ilscipio.com/product/my-product-PD1000)
     */
    protected String getCanonicalProductUrl(Delegator delegator, LocalDispatcher dispatcher, String productId) {
        String canonicalProductUrl = null;
        try {
            GenericValue product = delegator.findOne("Product", UtilMisc.toMap("productId", productId), true);
            if (product != null) {
                ProductContentWrapper wrapper = new ProductContentWrapper(dispatcher, product, defaultLocale, "text/html");
                String url = null; //CatalogUrlSeoTransform.makeProductUrl("/", null, product.getString("productId"), wrapper.get("PRODUCT_NAME").toString(), null, null);
                canonicalProductUrl = applyUrlRewriteRules(url);
            }
        }
        catch (GenericEntityException e) {
            Debug.logError(e, getErrorLogPrefix() + e.getMessage(), module);
        }
        return canonicalProductUrl;
    }

    /**
     * Returns canonical Url path for a product
     * (Example: /product/my-product-PD1000)
     */
    protected String getCanonicalProductUrlPath(Delegator delegator, LocalDispatcher dispatcher, String productId) {
        return getCanonicalProductUrlPath(delegator, dispatcher, productId).replace(config.baseUrl, "");
    }

    
    /**
     * Generate product sitemaps
     * @param siteMapId The id of the sitemap to be returned
     * @param productList GenericValue List of all products that are to be added
     * @return sitemap path
     * */
    @Deprecated
    String generateProductSitemap(int sitemapId, List<GenericValue> productList) {
        List<WebSitemapUrl> urlList = new ArrayList<WebSitemapUrl>();
        
        try {
            if (productList.size() > 0) {
                for (GenericValue product : productList) {
                    ProductContentWrapper wrapper = new ProductContentWrapper(dispatcher, product, defaultLocale, "text/plain");
                    String url = null; //CatalogUrlSeoTransform.makeProductUrl("/",
                            //null, product.getString("productId"),
                            //wrapper.get("PRODUCT_NAME").toString(), null, null);
                    String rewriteUrl = applyUrlRewriteRules(url);
                    if(Debug.verboseOn())Debug.logVerbose("Created url " + rewriteUrl, module);
                    Date lastModifiedDate = new Date(product.getTimestamp("lastModifiedDate").getTime());
                    WebSitemapUrl productUrl = new WebSitemapUrl.Options(rewriteUrl)
                            .lastMod(lastModifiedDate).build();
                    urlList.add(productUrl);
                }
                
                writeSitemap(urlList, config.productFilePrefix + sitemapId);
            }

        } catch (Exception e) {
            Debug.logError(e, getErrorLogMsg(e), module);
        }
        return config.productFilePrefix + sitemapId + "." + config.sitemapExtension;
    }
    
    /**
     * Generate Category sitemaps
     * @param siteMapId The id of the sitemap to be returned
     * @param categoryList GenericValue List of all categories that are to be added
     * */
    @Deprecated
    String generateCategorySitemap(int sitemapId, List<GenericValue> categoryList) {
        List<WebSitemapUrl> urlList = new ArrayList<WebSitemapUrl>();
        
        try {
            if (categoryList.size() > 0) {
                for (GenericValue category : categoryList) {
                    String currentCategoryId = category.getString("productCategoryId");
                    String categoryName = null; //CategoryContentWrapper.getProductCategoryContentAsText(category, "CATEGORY_NAME", defaultLocale, dispatcher);
                    if(categoryName != null){
                        String catName=categoryName.replaceAll(" ",URL_HYPHEN).replaceAll(",", "");
                        StringBuilder urlBuilder = new StringBuilder("/");
                        urlBuilder.append(catName);
                        urlBuilder.append(URL_HYPHEN);
                        urlBuilder.append(currentCategoryId);
                        String url = urlBuilder.toString();                     
                        String rewriteUrl = applyUrlRewriteRules(url);
                        if(Debug.verboseOn())Debug.logInfo("Created url " + rewriteUrl, module);
                        WebSitemapUrl sitemapUrl = new WebSitemapUrl.Options(rewriteUrl).build();
                        urlList.add(sitemapUrl);
                    }
                }
                
                writeSitemap(urlList, config.categoryFilePrefix + sitemapId);
            }

        } catch (Exception e) {
            Debug.logError(e, getErrorLogMsg(e), module);
        }
        return config.categoryFilePrefix + sitemapId + "." + config.sitemapExtension;
    }
    
    protected String getMsgLogPrefix() {
        return logPrefix+"Website '" + webSiteId + "': ";
    }
    
    protected String getErrorLogPrefix() {
        return logPrefix+"Error generating sitemap for website '" + webSiteId + "': ";
    }
    
    protected String getErrorLogMsg(Throwable t) {
        return getErrorLogPrefix() + t.getMessage();
    }
    
    // TODO: use a util
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
