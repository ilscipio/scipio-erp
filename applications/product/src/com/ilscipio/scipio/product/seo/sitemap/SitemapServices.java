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

import com.ilscipio.scipio.product.seo.SeoUrlUtil.UrlGenStats;

/**
 * TODO: LOCALIZE messages
 */
public abstract class SitemapServices {

    public static final String module = SitemapServices.class.getName();
    
    private static final String logPrefix = SitemapWorker.logPrefix;
    
    protected SitemapServices() {
    }
    
    public static Map<String, Object> generateWebsiteAlternativeUrlSitemapFiles(DispatchContext dctx, Map<String, ? extends Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        String webSiteId = (String) context.get("webSiteId");
        Locale locale = (Locale) context.get("locale");
        boolean useCache = Boolean.TRUE.equals(context.get("useCache"));

        SitemapWorker sitemapWorker = null;
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
            
            sitemapWorker = SitemapWorker.getWorkerForWebsite(delegator, dispatcher, webSiteId, defaultLocale, useCache);
            
            List<GenericValue> prodCatalogList = EntityQuery.use(delegator).from("ProductStoreCatalog").where("productStoreId", productStoreId)
                    .filterByDate().cache(useCache).queryList();
            
            for(GenericValue prodCatalog : prodCatalogList) {
                List<GenericValue> catalogCategories = EntityQuery.use(delegator).from("ProdCatalogCategory")
                        .where("prodCatalogId", prodCatalog.getString("prodCatalogId")).filterByDate().cache(useCache).queryList();
                sitemapWorker.buildSitemapDeep(catalogCategories);
            }
            
            sitemapWorker.commitSitemapsAndIndex();
            
            UrlGenStats stats = sitemapWorker.getStats();
            String dirMsg = "";
            if (sitemapWorker != null && sitemapWorker.getConfig().getSitemapDir() != null) {
                dirMsg = " (" + sitemapWorker.getConfig().getSitemapDir() + ")";
            }
            // TODO: LOCALIZE
            String resultMsg = "Generated sitemaps for website '" + webSiteId + "': " + stats.toMsg(locale) + dirMsg;
            Debug.logInfo(logPrefix+resultMsg, module);
            return stats.toServiceResultSuccessFailure(resultMsg);
        } catch (Exception e) {
            String dirMsg = "";
            if (sitemapWorker != null) {
                if (sitemapWorker.getConfig().getSitemapDir() != null) {
                    dirMsg += " (" + sitemapWorker.getConfig().getSitemapDir() + ")";
                }
                if (sitemapWorker.getStats() != null) {
                    dirMsg += " (" + sitemapWorker.getStats().toMsg(locale) + ")";
                }
            }
            // TODO: LOCALIZE
            String message = "Error generating sitemaps for web site '" + webSiteId + "'" + dirMsg + ": " + e.getMessage();
            Debug.logError(e, logPrefix + message, module);
            return ServiceUtil.returnError(message);
        }
    }
    
    public static Map<String, Object> generateAllAlternativeUrlSitemapFiles(DispatchContext dctx, Map<String, ? extends Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();

        int success = 0;
        int fail = 0;

        for(Map.Entry<String, SitemapConfig> entry : SitemapConfig.getAllSitemapConfigs(delegator, dispatcher).entrySet()) {
            String webSiteId = entry.getKey();
            try {
                Map<String, Object> servCtx = dctx.makeValidContext("generateWebsiteAlternativeUrlSitemapFiles", ModelService.IN_PARAM, context);
                servCtx.put("webSiteId", webSiteId);
                Map<String, Object> servResult = dispatcher.runSync("generateWebsiteAlternativeUrlSitemapFiles", servCtx, -1 , true);
                if (ServiceUtil.isSuccess(servResult)) success++;
                else fail++;
            } catch(Exception e) {
                String message = "Error generating sitemap for web site '" + webSiteId + "': " + e.getMessage();
                Debug.logError(e, logPrefix + message, module);
                fail++;
            }
        }
        
        if (fail > 0) {
            // TODO: LOCALIZE
            return ServiceUtil.returnFailure(success + " website sitemaps updated; " + fail + " failed");
        } else {
            // TODO: LOCALIZE
            return ServiceUtil.returnSuccess(success + " website sitemaps updated");
        }
    }
    
}
