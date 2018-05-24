package com.ilscipio.scipio.cms.template;

import java.util.Map;

import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.entity.Delegator;
import org.ofbiz.service.DispatchContext;
import org.ofbiz.service.ServiceUtil;

import com.ilscipio.scipio.cms.CmsServiceUtil;
import com.ilscipio.scipio.cms.ServiceErrorFormatter;
import com.ilscipio.scipio.cms.ServiceErrorFormatter.FormattedError;

/**
 * CmsTemplateServices - Generic CMS Template Services (that don't
 * fall into the more specific classes)
 */
public abstract class CmsTemplateServices {
    
    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());
    private static final ServiceErrorFormatter errorFmt = CmsServiceUtil.getErrorFormatter();
    
    protected CmsTemplateServices() {
    }
    
    /**
     * Creates or updates an attribute for a given template (page or asset).
     */
    public static Map<String, Object> createUpdateAttribute(DispatchContext dctx, Map<String, ?> context) {
        Map<String, Object> result = ServiceUtil.returnSuccess();
        Delegator delegator = dctx.getDelegator();
        try {
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
            FormattedError err = errorFmt.format(e, "Error creating or updating attribute", context);
            Debug.logError(err.getEx(), err.getLogMsg(), module);
            return err.returnError();
        }
        return result;
    }
    
}
