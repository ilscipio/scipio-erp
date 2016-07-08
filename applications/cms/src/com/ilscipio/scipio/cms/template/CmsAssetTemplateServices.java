package com.ilscipio.scipio.cms.template;

import java.util.Map;

import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.UtilMisc;
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.GenericValue;
import org.ofbiz.service.DispatchContext;
import org.ofbiz.service.ServiceUtil;

/**
 * CmsAssetTemplateServices - CMS Asset Template and Asset Template Version Services
 *
 */
public final class CmsAssetTemplateServices {
    public static final String module = CmsAssetTemplateServices.class.getName();

    /**
     * Sets an asset template version as live version. The live version is the content that
     * will be displayed to regular page visitors.
     * 
     * @param dctx
     *            The DispatchContext that this service is operating in
     * @param context
     *            Map containing the input parameters
     * @return Map with the result of the service, the output parameters
     */
    public static Map<String, Object> activateAssetTemplateVersion(DispatchContext dctx, Map<String, ?> context) {
        Map<String, Object> result = ServiceUtil.returnSuccess();
        try {
            Delegator delegator = dctx.getDelegator();
            
            String assetTemplateId = (String) context.get("assetTemplateId");
            String activeAssetTemplateId = (String) context.get("activeAssetTemplateId");
            String templateName = (String) context.get("templateName");
            
            GenericValue assetTemplate = delegator.findByPrimaryKey("CmsAssetTemplate", UtilMisc.toMap("assetTemplateId", assetTemplateId));
            GenericValue activeAssetTemplate = delegator.findByPrimaryKey("CmsAssetTemplate", UtilMisc.toMap("assetTemplateId", activeAssetTemplateId));
            
            if (assetTemplate != null && activeAssetTemplate != null) {
                assetTemplate.set("inactive", "N");
                activeAssetTemplate.set("inactive", "Y");
                delegator.store(assetTemplate);
                delegator.store(activeAssetTemplate);
            }
            else {
                result = ServiceUtil.returnFailure("Asset template version '" + templateName + "' not found"); // TODO: Localize
            }
        } 
        catch (Exception e) {
            Debug.logError(e, module);
            result = ServiceUtil.returnFailure(e.getMessage());
        }
        return result;
    }
    
    /**
     * Gets the active/default asset template version of the given asset template.
     * 
     * @param dctx
     *            The DispatchContext that this service is operating in
     * @param context
     *            Map containing the input parameters
     * @return Map with the result of the service, the output parameters
     */
    public static Map<String, Object> cmsGetActiveAssetTemplateVersion(DispatchContext dctx, Map<String, ?> context) {
        Map<String, Object> result = ServiceUtil.returnSuccess();
        try {

            String webSiteId = (String) context.get("webSiteId");
            String templateName = (String) context.get("templateName");
            
            CmsAssetTemplate assetTemplateModel = CmsAssetTemplate.findActiveVersion(templateName, webSiteId);
            
            if (assetTemplateModel != null) {
                result.put("assetTemplate", assetTemplateModel.getEntity());
            }
            else {
                result = ServiceUtil.returnFailure("Could not find active template version for webSiteId '" +
                        webSiteId + "' and templateName '" + templateName + "'"); // TODO: Localize
            }
        } 
        catch (Exception e) {
            Debug.logError(e, module);
            result = ServiceUtil.returnFailure(e.getMessage()); // FIXME: Shouldn't be failure here or anywhere else!
        }
        return result;
    }
    
    /**
     * Gets the active/default asset template version of the given asset template.
     * 
     * @param dctx
     *            The DispatchContext that this service is operating in
     * @param context
     *            Map containing the input parameters
     * @return Map with the result of the service, the output parameters
     */
    public static Map<String, Object> getRelatedActiveAssetTemplateVersion(DispatchContext dctx, Map<String, ?> context) {
        Map<String, Object> result = ServiceUtil.returnSuccess();
        try {
            String relatedAssetTemplateId = (String) context.get("relatedAssetTemplateId");

            CmsAssetTemplate assetTemplateModel = CmsAssetTemplate.findRelatedActiveVersion(relatedAssetTemplateId);
            
            if (assetTemplateModel != null) {
                result.put("assetTemplate", assetTemplateModel.getEntity());
            }
            else {
                result = ServiceUtil.returnFailure("Could not find related active template version for related assetTemplateId '" +
                        relatedAssetTemplateId + "'"); // TODO: Localize
            }
        } 
        catch (Exception e) {
            Debug.logError(e, module);
            result = ServiceUtil.returnFailure(e.getMessage());
        }
        return result;
    }    
    
    
}
