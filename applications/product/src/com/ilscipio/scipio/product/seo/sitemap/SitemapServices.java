package com.ilscipio.scipio.product.seo.sitemap;

import java.util.Locale;
import java.util.Map;

import org.ofbiz.base.util.Debug;
import org.ofbiz.entity.Delegator;
import org.ofbiz.service.DispatchContext;
import org.ofbiz.service.LocalDispatcher;
import org.ofbiz.service.ModelService;
import org.ofbiz.service.ServiceUtil;

import com.ilscipio.scipio.product.seo.UrlGenStats;

/**
 * TODO: LOCALIZE messages
 */
public abstract class SitemapServices {

    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());

    private static final String logPrefix = SitemapGenerator.logPrefix;

    protected SitemapServices() {
    }

    public static Map<String, Object> generateWebsiteAlternativeUrlSitemapFiles(DispatchContext dctx, Map<String, ? extends Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        String webSiteId = (String) context.get("webSiteId");
        Locale locale = (Locale) context.get("locale");
        boolean useCache = Boolean.TRUE.equals(context.get("useCache"));

        SitemapGenerator generator = null;
        try {
            // TODO: LOCALIZE

            generator = SitemapGenerator.getWorkerForWebsite(delegator, dispatcher, webSiteId, context, useCache);
            generator.buildSitemapDeepForWebsite();
            generator.commitSitemapsAndIndex();

            UrlGenStats stats = generator.getStats();
            String dirMsg = "";
            if (generator != null) {
                // just in case coding/other errors here, these don't need to cause service fail
                try {
                    dirMsg += " (directory: " + generator.getFullSitemapDir() + ")";
                } catch(Exception e) {
                    Debug.logError(e, logPrefix+"Error determining full sitemap file directory location: " + e.getMessage(), module);
                }
                try {
                    dirMsg += " (index URL: " + generator.getSitemapIndexFileLink() + ")";
                } catch(Exception e) {
                    Debug.logError(e, logPrefix+"Error determining sitemap index link: " + e.getMessage(), module);
                }
            }
            // TODO: LOCALIZE
            String resultMsg = "Generated sitemaps for website '" + webSiteId + "': " + stats.toMsg(locale) + dirMsg;
            Debug.logInfo(logPrefix+resultMsg, module);
            return stats.toServiceResultSuccessFailure(resultMsg);
        } catch (Exception e) {
            String dirMsg = "";
            if (generator != null) {
                dirMsg = " (" + generator.getFullSitemapDir() + " - "
                        + generator.getStats().toMsg(locale) + ")";
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
