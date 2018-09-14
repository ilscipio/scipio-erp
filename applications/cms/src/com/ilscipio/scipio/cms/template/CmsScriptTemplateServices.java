package com.ilscipio.scipio.cms.template;

import java.util.HashMap;
import java.util.Map;

import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.UtilGenerics;
import org.ofbiz.base.util.UtilMisc;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.GenericValue;
import org.ofbiz.service.DispatchContext;
import org.ofbiz.service.LocalDispatcher;
import org.ofbiz.service.ServiceUtil;

import com.ilscipio.scipio.cms.CmsServiceUtil;
import com.ilscipio.scipio.cms.ServiceErrorFormatter;
import com.ilscipio.scipio.cms.ServiceErrorFormatter.FormattedError;

public abstract class CmsScriptTemplateServices {

    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());
    private static final ServiceErrorFormatter errorFmt =
            CmsServiceUtil.getErrorFormatter().specialize().setDefaultLogMsgGeneral("Script Template Error").build();

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

            CmsScriptTemplate scriptTmpl = null;
            if (UtilValidate.isNotEmpty(scriptTemplateId)) {
                scriptTmpl = CmsScriptTemplate.getWorker().findByIdAlways(delegator, scriptTemplateId, false);

                fields.put("createdBy", (String) userLogin.get("userLoginId"));

                // NOTE: 2016-12: IMPORTANT: EVERY TIME THERE IS A BODY OR LOCATION UPDATE OPERATION,
                // and standalone is not explicit false, we SWITCH standalone from N to Y,
                // so we NEVER delete user's changes automatically.
                if (UtilValidate.isEmpty((String) fields.get("standalone"))) {
                    fields.put("standalone", "Y");
                }

                scriptTmpl.update(fields);
            } else {
                fields.put("lastUpdatedBy", (String) userLogin.get("userLoginId"));
                scriptTmpl = new CmsScriptTemplate(delegator, fields);
            }

            scriptTmpl.store();
            result.put("scriptTemplateId", scriptTmpl.getId());
        } catch (Exception e) {
            FormattedError err = errorFmt.format(e, context);
            Debug.logError(err.getEx(), err.getLogMsg(), module);
            return err.returnError();
        }
        return result;
    }

    public static Map<String, Object> copyScriptTemplate(DispatchContext dctx, Map<String, ?> context) {
        Delegator delegator = dctx.getDelegator();
        Map<String, Object> copyArgs = new HashMap<>();
        GenericValue userLogin = (GenericValue) context.get("userLogin");
        if (userLogin != null) {
            copyArgs.put("copyCreatorId", userLogin.get("partyId"));
        }
        try {
            String srcScriptTemplateId = (String) context.get("srcScriptTemplateId");
            CmsScriptTemplate srcScriptTmpl = CmsScriptTemplate.getWorker().findByIdAlways(delegator, srcScriptTemplateId, false);
            CmsScriptTemplate scriptTmpl = srcScriptTmpl.copy(copyArgs);

            scriptTmpl.update(UtilMisc.toHashMapWithKeys(context, "templateName", "description"));

            scriptTmpl.store();
            Map<String, Object> result = ServiceUtil.returnSuccess();
            result.put("scriptTemplateId", scriptTmpl.getId());
            return result;
        } catch (Exception e) {
            FormattedError err = errorFmt.format(e, context);
            Debug.logError(err.getEx(), err.getLogMsg(), module);
            return err.returnError();
        }
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
            FormattedError err = errorFmt.format(e, context);
            Debug.logError(err.getEx(), err.getLogMsg(), module);
            return err.returnError();
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
            FormattedError err = errorFmt.format(e, context);
            Debug.logError(err.getEx(), err.getLogMsg(), module);
            return err.returnFailure();
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
            FormattedError err = errorFmt.format(e, context);
            Debug.logError(err.getEx(), err.getLogMsg(), module);
            return err.returnError();
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
            FormattedError err = errorFmt.format(e, context);
            Debug.logError(err.getEx(), err.getLogMsg(), module);
            return err.returnError();
        }
        return result;
    }

}
