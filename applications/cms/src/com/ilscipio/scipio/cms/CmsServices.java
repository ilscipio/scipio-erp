package com.ilscipio.scipio.cms;

import java.sql.Timestamp;
import java.util.*;

import javax.servlet.ServletRequest;

import org.apache.http.client.methods.HttpGet;
import org.apache.http.impl.client.CloseableHttpClient;
import org.apache.solr.common.SolrDocument;
import org.apache.solr.common.SolrDocumentList;
import org.ofbiz.base.util.*;
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.GenericDelegator;
import org.ofbiz.entity.GenericEntityException;
import org.ofbiz.entity.GenericValue;
import org.ofbiz.product.category.CatalogUrlFilter;
import org.ofbiz.product.store.ProductStoreWorker;
import org.ofbiz.service.DispatchContext;
import org.ofbiz.service.LocalDispatcher;
import org.ofbiz.service.ServiceUtil;
import org.ofbiz.webapp.ExtWebappInfo;
import org.ofbiz.webapp.FullWebappInfo;
import org.ofbiz.webapp.WebAppNotFoundException;
import org.ofbiz.webapp.website.WebSiteWorker;

import com.ilscipio.scipio.cms.content.CmsPage;
import com.ilscipio.scipio.cms.control.CmsWebSiteInfo;
import com.ilscipio.scipio.cms.template.CmsAssetTemplate;
import com.ilscipio.scipio.cms.template.CmsPageTemplate;
import com.ilscipio.scipio.cms.webapp.CmsWebappUtil;

/**
 * General CMS services.
 * <p>
 * DEV NOTE: More specific services should go under service files under
 * other packages. Service helper utils can go in CmsServicesCommon.
 */
public abstract class CmsServices {

    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());
    //private static final ServiceErrorFormatter errorFmt = CmsServiceUtil.getErrorFormatter();
    private static final ScipioHttpClient SCIPIO_HTTP_CLIENT = ScipioHttpClient.fromConfig(ScipioHttpClient.Config.fromProperties("cms", "cache.prewarm.connect."));
    private static final String BING_INDEX_NOW_URL = UtilProperties.getPropertyValue("cms", "bing.default.indexnowUrl", "https://www.bing.com/indexnow");

    protected CmsServices() {
    }

    public static Map<String, Object> getDescriptors(DispatchContext dctx, Map<String, ?> context) {
        Map<String, Object> result = ServiceUtil.returnSuccess();
        Delegator delegator = dctx.getDelegator();

        String webSiteId = WebSiteWorker.getWebSiteId((ServletRequest) context.get("request"));
        List<CmsPageTemplate> pageTemplates = CmsPageTemplate.getWorker().findAll(delegator, UtilMisc.toMap("webSiteId", webSiteId),
                UtilMisc.toList("templateName ASC"), false);
        result.put("pageTemplates", pageTemplates);
        List<CmsAssetTemplate> assetTemplates = CmsAssetTemplate.getWorker().findAll(delegator, UtilMisc.toMap("webSiteId", webSiteId),
                UtilMisc.toList("templateName ASC"), false);
        result.put("assetTemplates", assetTemplates);
        List<CmsPage> pages = CmsPage.getWorker().findAll(delegator, UtilMisc.toMap("webSiteId", webSiteId), UtilMisc.toList("pageName ASC"), false);
        result.put("pages", pages);

        return result;
    }

    public static Map<String, Object> getCmsWebSites(DispatchContext dctx, Map<String, ?> context) {
        Delegator delegator = dctx.getDelegator();
        Map<String, Object> result = ServiceUtil.returnSuccess();

        Set<String> webSiteIdSet = CmsWebSiteInfo.getAllCmsRegWebSitesInfo().keySet();
        List<GenericValue> webSiteList;
        try {
            webSiteList = CmsWebappUtil.getWebSiteList(delegator, webSiteIdSet);
        } catch (GenericEntityException e) {
            Debug.logError(e, module);
            return ServiceUtil.returnError(e.getMessage());
        }

        result.put("webSiteIdSet", webSiteIdSet);
        result.put("webSiteList", webSiteList);
        return result;
    }

    public static Map<String, Object> submitProductToBingIndex(DispatchContext dctx, Map<String, ? extends Object> context) {
        GenericDelegator delegator = (GenericDelegator) dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        String productId = (String) context.get("productId");
        String productStoreId = (String) context.get("productStoreId");
        List<String> productStoreIds = UtilMisc.toList(productStoreId);
        Map<String,Object> serviceResult = ServiceUtil.returnSuccess();
        String bingApiKey = UtilProperties.getPropertyValue("cms","bing.default.indexnow.apiKey");
        boolean bingEnabled = UtilProperties.getPropertyAsBoolean("cms","bing.default.indexnow.enabled",false);
        Locale locale = (Locale) context.get("locale");


        try{


            Map<String,Object> solrResult = dispatcher.runSync("solrKeywordSearch", UtilMisc.toMap("query","productId:"+ productId), -1, true);
            if(UtilValidate.isNotEmpty(solrResult.get("results"))){
                SolrDocumentList solrEntries = (SolrDocumentList) solrResult.get("results");

                //Use first entry
                SolrDocument firstEntry = solrEntries.get(0);
                if(UtilValidate.isEmpty(productStoreId)){
                    productStoreIds = (List) firstEntry.get("productStore");
                }
                boolean discontinued = UtilValidate.isEmpty(firstEntry.get("discontinued_b"))
                        || ((Boolean) firstEntry.get("discontinued_b"));
                if(!discontinued){
                    for(String pStoreId : productStoreIds){
                        //Override with productstore specific setting (if configured)
                        bingApiKey = UtilValidate.isNotEmpty(UtilProperties.getPropertyValue("cms","bing."+productStoreIds+".apiKey"))?
                                UtilProperties.getPropertyValue("cms","bing."+productStoreIds+".indexnow.apiKey") : bingApiKey;
                        bingEnabled = UtilValidate.isNotEmpty(UtilProperties.getPropertyValue("cms","bing."+productStoreIds+".enabled"))?
                                UtilProperties.getPropertyAsBoolean("cms","bing."+productStoreIds+".indexnow.ebabled",false) : bingEnabled;

                        if(bingEnabled){
                            String url = makeProductLink(productId,pStoreId,dctx,locale);
                            if(UtilValidate.isNotEmpty(url)){
                                String bingRequestUrl = BING_INDEX_NOW_URL + "?key="+bingApiKey.trim()+"&url="+url;
                                if(UtilValidate.isNotEmpty(url.trim())){
                                    CloseableHttpClient httpClient = SCIPIO_HTTP_CLIENT.getHttpClient();
                                    HttpGet httpget = new HttpGet(bingRequestUrl.trim());
                                    httpClient.execute(httpget);
                                }
                            }
                        }
                    }
                }
            }
        }catch (Exception e){
            serviceResult = ServiceUtil.returnError(e.getMessage());
        }



        return serviceResult;
    }

    private static String makeProductLink(String productId, String productStoreId, DispatchContext dctx, Locale locale) {
        String webSiteId = ProductStoreWorker.getStoreDefaultWebSiteId(dctx.getDelegator(), productStoreId, true);
        if(UtilValidate.isNotEmpty(webSiteId)){
            try {

                FullWebappInfo targetWebappInfo = FullWebappInfo.fromWebapp(ExtWebappInfo.fromWebSiteId(webSiteId), dctx.getDelegator());

                String path = CatalogUrlFilter.makeCatalogAltLink(UtilMisc.toMap("delegator", dctx.getDelegator(), "dispatcher", dctx.getDispatcher()),
                        dctx.getDelegator(), dctx.getDispatcher(),
                        locale,
                        null, productId, null, null, targetWebappInfo, true, true, null,
                        null, null, null, null);
                return path;
            } catch (Exception e) {
                Throwable cause = e;
                Debug.log("Could not make product link for product '" + productId + "' store '" + productStoreId +
                        "' website '" + webSiteId + "': " + e.toString() + " (further messages will be silenced)", module);
            }
        }
        return null;
    }

}
