package com.ilscipio.scipio.cms;

import java.util.List;
import java.util.Map;
import java.util.Set;

import javax.servlet.ServletRequest;

import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.UtilMisc;
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.GenericEntityException;
import org.ofbiz.entity.GenericValue;
import org.ofbiz.service.DispatchContext;
import org.ofbiz.service.ServiceUtil;
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
    private static final ServiceErrorFormatter errorFmt = CmsServiceUtil.getErrorFormatter();

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

}
