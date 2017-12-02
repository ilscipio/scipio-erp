package com.ilscipio.scipio.cms.template;

import java.util.Map;

import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.entity.Delegator;
import org.ofbiz.service.DispatchContext;
import org.ofbiz.service.ServiceUtil;

/**
 * CmsTemplateServices - Generic CMS Template Services (that don't
 * fall into the more specific classes)
 */
public abstract class CmsTemplateServices {
    
    public static final String module = CmsTemplateServices.class.getName();
    
    protected CmsTemplateServices() {
    }
    
    /**
     * Creates or updates an attribute for a given template (page or asset).
     */
    public static Map<String, Object> createUpdateAttribute(DispatchContext dctx, Map<String, ?> context) {
        Map<String, Object> result = ServiceUtil.returnSuccess();
        Delegator delegator = dctx.getDelegator();
        try {
            
            //String pageTemplateId = (String) context.get("pageTemplateId");
            //String assetTemplateId = (String) context.get("assetTemplateId");
            String attributeTemplateId = (String) context.get("attributeTemplateId");

            CmsAttributeTemplate attr = null;

            if (UtilValidate.isNotEmpty(attributeTemplateId)) {
                attr = CmsAttributeTemplate.getWorker().findByIdAlways(delegator, attributeTemplateId, false);
                attr.update(context);
            } else {
                attr = new CmsAttributeTemplate(delegator, context);
            }

            attr.store();
            result.put("attributeTemplateId", attr.getId());
        } catch (Exception e) {
            Debug.logError(e, "Cms: Error creating or updating attribute: " + e.getMessage(), module);
            return ServiceUtil.returnError(e.getMessage());
        }
        return result;
    }
    
}
