package com.ilscipio.scipio.product.seo.sitemap;

import java.util.List;
import java.util.Locale;
import java.util.Map;

import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.UtilMisc;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.GenericValue;
import org.ofbiz.entity.util.EntityQuery;
import org.ofbiz.service.DispatchContext;
import org.ofbiz.service.LocalDispatcher;
import org.ofbiz.service.ModelService;
import org.ofbiz.service.ServiceUtil;

/**
 * TODO: LOCALIZE messages
 */
public abstract class SitemapServices {

    public static final String module = SitemapServices.class.getName();
    
    protected static final String logPrefix = "Seo: Sitemap: ";
    
    protected SitemapServices() {
    }
    
    public static Map<String, Object> generateWebsiteAlternativeUrlSitemapFiles(DispatchContext dctx, Map<String, ? extends Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        String webSiteId = (String) context.get("webSiteId");
        Locale locale = (Locale) context.get("locale");
        boolean useCache = Boolean.TRUE.equals(context.get("useCache"));

        try {
            // TODO: LOCALIZE
            GenericValue webSite = delegator.findOne("WebSite", UtilMisc.toMap("webSiteId", webSiteId), useCache);
            if (webSiteId == null) return ServiceUtil.returnError("web site not found: " + webSiteId);
            String productStoreId = webSite.getString("productStoreId");
            if (UtilValidate.isEmpty(productStoreId)) return ServiceUtil.returnError("web site has no product store: " + webSiteId);

            GenericValue productStore = delegator.findOne("ProductStore", UtilMisc.toMap("productStoreId", productStoreId), useCache);
            if (productStore == null) return ServiceUtil.returnError("store not found: " + productStoreId);
            
            String defaultLocaleString = productStore.getString("defaultLocaleString");
            Locale defaultLocale = UtilMisc.parseLocale(defaultLocaleString);
            
            SitemapWorker sitemapWorker = SitemapWorker.getWorkerForWebsite(delegator, dispatcher, webSiteId, defaultLocale, useCache);
            
            List<GenericValue> prodCatalogList = EntityQuery.use(delegator).from("ProductStoreCatalog").where("productStoreId", productStoreId)
                    .filterByDate().cache(useCache).queryList();
            
            for(GenericValue prodCatalog : prodCatalogList) {
                List<GenericValue> catalogCategories = EntityQuery.use(delegator).from("ProdCatalogCategory")
                        .where("prodCatalogId", prodCatalog.getString("prodCatalogId")).filterByDate().cache(useCache).queryList();
                sitemapWorker.buildSitemapDeep(catalogCategories);
            }
            
            sitemapWorker.commitSitemapsAndIndex();
            
            return sitemapWorker.getStats().toServiceResultSuccessFailure(locale, false);
            
            /* OLD CODE - TODO: REDO 
            // now let's fetch all products
            List<GenericValue> products = delegator.findList("Product", null, UtilMisc.toSet("productId", "lastModifiedDate"), null, null, false);
            int numDocs = products.size();

            Debug.logInfo("Found " + numDocs + " products. Generating product sitemaps....", module);

            List<String> siteMapLists = new ArrayList<String>();
            for (int i = 0; i < products.size(); i += sitemapSize) {
                List<GenericValue> subList = products.subList(i, i + Math.min(sitemapSize, products.size() - i));

                Debug.logInfo("Generating sitemap document for product-entries " + i
                        + " to " + (i + Math.min(sitemapSize, products.size() - i)) + ".", module);
                String entry = sitemapWorker.generateProductSitemap(i, subList);
                Debug.logInfo("Generated sitemap " + entry, module);
                siteMapLists.add(entry);
            }

            List<GenericValue> categories = delegator.findList("ProductCategoryRollup", null, UtilMisc.toSet("productCategoryId"), null, null, false);
            int numCategories = 0;
            if (categories != null) {
                numCategories = categories.size();
            }

            Debug.logInfo("Found " + numCategories + " categories. Generating product sitemaps....", module);
            for (int i = 0; i < categories.size(); i += sitemapSize) {
                List<GenericValue> subList = categories.subList(i, i + Math.min(sitemapSize, categories.size() - i));

                Debug.logInfo("Generating sitemap document for category-entries " + i + " to "
                        + (i + Math.min(sitemapSize, categories.size() - i)) + ".", module);
                String entry = sitemapWorker.generateCategorySitemap(i, subList);
                Debug.logInfo("Generated sitemap " + entry, module);
                siteMapLists.add(entry);
            }
            
            sitemapWorker.generateSitemapIndex(siteMapLists);
            */
        } catch (Exception e) {
            String message = "Error generating sitemap for web site '" + webSiteId + "':" + e.getMessage();
            Debug.logError(e, logPrefix + message, module);
            return ServiceUtil.returnError(message);
        }
    }
    
    public static Map<String, Object> generateAllAlternativeUrlSitemapFiles(DispatchContext dctx, Map<String, ? extends Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();

        int success = 0;
        int error = 0;

        for(Map.Entry<String, SitemapConfig> entry : SitemapConfig.getAllSitemapConfigs(delegator, dispatcher).entrySet()) {
            String webSiteId = entry.getKey();
            try {
                Map<String, Object> servCtx = dctx.makeValidContext("generateWebsiteAlternativeUrlSitemapFiles", ModelService.IN_PARAM, context);
                servCtx.put("webSiteId", webSiteId);
                Map<String, Object> servResult = dispatcher.runSync("generateWebsiteAlternativeUrlSitemapFiles", servCtx, -1 , true);
                if (ServiceUtil.isSuccess(servResult)) success++;
                else error++;
            } catch(Exception e) {
                String message = "Error generating sitemap for web site '" + webSiteId + "':" + e.getMessage();
                Debug.logError(e, logPrefix + message, module);
                error++;
            }
        }
        
        // TODO: LOCALIZE
        if (error > 0) {
            return ServiceUtil.returnFailure(success + " website sitemaps updated; " + error + " failed");
        } else {
            return ServiceUtil.returnSuccess(success + " website sitemaps updated");
        }
    }
    
}
