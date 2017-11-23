package com.ilscipio.scipio.cms.template;

import java.util.Map;

import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.UtilGenerics;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.GenericValue;
import org.ofbiz.service.DispatchContext;
import org.ofbiz.service.LocalDispatcher;
import org.ofbiz.service.ServiceUtil;

import com.ilscipio.scipio.cms.CmsServiceUtil;

public abstract class CmsScriptTemplateServices {
    
    public static final String module = CmsScriptTemplateServices.class.getName();

    protected CmsScriptTemplateServices() {
    }

    public static Map<String, Object> createUpdateScriptTemplate(DispatchContext dctx, Map<String, ?> context) {
        Map<String, Object> result = ServiceUtil.returnSuccess();
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        try {
            GenericValue userLogin = CmsServiceUtil.getUserLoginOrSystem(dctx, context);
            //Debug.logInfo("createUpdateAsset triggered",module);
            String scriptTemplateId = (String) context.get("scriptTemplateId");
            
            // Create empty template
            Map<String, Object> fields = ServiceUtil.setServiceFields(dispatcher, "cmsCreateUpdateScriptTemplate", 
                    UtilGenerics.<String, Object> checkMap(context), userLogin, null, null);
            
            CmsScriptTemplate scriptTmp = null;
            if (UtilValidate.isNotEmpty(scriptTemplateId)) {
                scriptTmp = CmsScriptTemplate.getWorker().findByIdAlways(delegator, scriptTemplateId, false);
                
                fields.put("createdBy", (String) userLogin.get("userLoginId"));
                
                // NOTE: 2016-12: IMPORTANT: EVERY TIME THERE IS A BODY OR LOCATION UPDATE OPERATION,
                // and standalone is not explicit false, we SWITCH standalone from N to Y,
                // so we NEVER delete user's changes automatically.
                if (UtilValidate.isEmpty((String) fields.get("standalone"))) {
                    fields.put("standalone", "Y");
                }
                
                scriptTmp.update(fields);
            } else {
                fields.put("lastUpdatedBy", (String) userLogin.get("userLoginId"));
                scriptTmp = new CmsScriptTemplate(delegator, fields);
            }
            
            scriptTmp.store();
            result.put("scriptTemplateId", scriptTmp.getId());
        } catch (Exception e) {
            Debug.logError(e, module);
            return ServiceUtil.returnError(e.getMessage());
        }
        return result;
    }
    
    public static Map<String, Object> updateScriptTemplateInfo(DispatchContext dctx, Map<String, ?> context) {
        Map<String, Object> result = ServiceUtil.returnSuccess();
        Delegator delegator = dctx.getDelegator();
        try {
            String scriptTemplateId = (String) context.get("scriptTemplateId");
            CmsScriptTemplate scriptTmp = CmsScriptTemplate.getWorker().findByIdAlways(delegator, scriptTemplateId, false);
            scriptTmp.update(context);
            scriptTmp.store();
        } catch (Exception e) {
            Debug.logError(e, module);
            return ServiceUtil.returnError(e.getMessage());
        }
        return result;
    }
    
    public static Map<String, Object> getScriptTemplate(DispatchContext dctx, Map<String, ?> context) {
        Map<String, Object> result = ServiceUtil.returnSuccess();
        Delegator delegator = dctx.getDelegator();
        try {
            String scriptTemplateId = (String) context.get("scriptTemplateId");
            CmsScriptTemplate scriptTemplate = null;

            scriptTemplate = CmsScriptTemplate.getWorker().findByIdAlways(delegator, scriptTemplateId, false);

            // FIXME: why is scriptTemplate GenericValue here?
            result.put("scriptTemplateValue", scriptTemplate.getEntity());
            result.put("scriptTemplate", scriptTemplate);
        } catch (Exception e) {
            Debug.logError(e, module);
            return ServiceUtil.returnFailure(e.getMessage()); // FIXME: Shouldn't be failure here or anywhere else!
        }
        return result;
    }
    
    public static Map<String, Object> deleteScriptTemplate(DispatchContext dctx, Map<String, ?> context) {
        Map<String, Object> result = ServiceUtil.returnSuccess();
        Delegator delegator = dctx.getDelegator();
        try {
            String scriptTemplateId = (String) context.get("scriptTemplateId");
            CmsScriptTemplate template = CmsScriptTemplate.getWorker().findByIdAlways(delegator, scriptTemplateId, false);
            template.remove();
        } catch (Exception e) {
            Debug.logError(e, module);
            return ServiceUtil.returnError(e.getMessage());
        }
        return result;
    }
    
    public static Map<String, Object> deleteScriptTemplateIfOrphan(DispatchContext dctx, Map<String, ?> context) {
        Map<String, Object> result = ServiceUtil.returnSuccess();
        Delegator delegator = dctx.getDelegator();
        try {
            String scriptTemplateId = (String) context.get("scriptTemplateId");
            CmsScriptTemplate template = CmsScriptTemplate.getWorker().findByIdAlways(delegator, scriptTemplateId, false);
            template.removeIfOrphan();
        } catch (Exception e) {
            Debug.logError(e, module);
            return ServiceUtil.returnError(e.getMessage());
        }
        return result;
    }
    
}
