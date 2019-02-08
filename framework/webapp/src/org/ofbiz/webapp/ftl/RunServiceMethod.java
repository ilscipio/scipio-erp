package org.ofbiz.webapp.ftl;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.base.util.template.FreeMarkerWorker;
import org.ofbiz.service.LocalDispatcher;
import org.ofbiz.service.ModelService;
import org.ofbiz.service.ServiceUtil;

import com.ilscipio.scipio.ce.webapp.ftl.context.ContextFtlUtil;
import com.ilscipio.scipio.ce.webapp.ftl.context.TransformUtil;
import com.ilscipio.scipio.ce.webapp.ftl.lang.LangFtlUtil;

import freemarker.core.Environment;
import freemarker.template.TemplateMethodModelEx;
import freemarker.template.TemplateModelException;

/**
 * SCIPIO: runService helper method.
 */
public class RunServiceMethod implements TemplateMethodModelEx {
    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());

    @Override
    public Object exec(@SuppressWarnings("rawtypes") List args) throws TemplateModelException {
        String serviceName = TransformUtil.getStringNonEscapingArg(args, "name", 0);
        if (UtilValidate.isEmpty(serviceName)) {
            throw new TemplateModelException("Missing service name");
        }
        Environment env = FreeMarkerWorker.getCurrentEnvironment();
        LocalDispatcher dispatcher = ContextFtlUtil.getDispatcher(env);

        @SuppressWarnings("unchecked")
        Map<String, Object> serviceCtx = (Map<String, Object>) LangFtlUtil.unwrap(TransformUtil.getModel(args, "ctx", 1));
        if (serviceCtx == null) {
            serviceCtx = new HashMap<>();
        } else {
            try {
                serviceCtx = dispatcher.getDispatchContext().makeValidContext(serviceName, ModelService.IN_PARAM, serviceCtx);
            } catch (Exception e) {
                String exMode = TransformUtil.getStringNonEscapingArg(args, "exMode", 3, "");
                if (!exMode.endsWith("-nolog")) {
                    Debug.logError(e, "Error validating service '" + serviceName + "' context", module);
                }
                return handleException(e, exMode, env);
            }
        }

        if (!Boolean.FALSE.equals(TransformUtil.getBooleanArg(args, "inclEnvCtx", -1))) {
            // NOTE: We ONLY use the request/session for default fields (userLogin, locale, timeZone)
            // as backward-compatibility IF their keys are not set in current context, as the renderer should have set them.
            ServiceUtil.checkSetServiceContextDefaults(serviceCtx, ModelService.COMMON_INTERNAL_IN_FIELDS,
                    ContextFtlUtil.getContext(env), ContextFtlUtil.getRequest(env));
        }

        boolean newTrans = Boolean.TRUE.equals(TransformUtil.getBooleanArg(args, "newTrans", 2));
        try {
            return dispatcher.runSync(serviceName, serviceCtx, newTrans);
        } catch (Exception e) {
            String exMode = TransformUtil.getStringNonEscapingArg(args, "exMode", 3, "");
            if (!exMode.endsWith("-nolog")) {
                Debug.logError(e, "Error invoking service '" + serviceName + "'", module);
            }
            return handleException(e, exMode, env);
        }
    }

    private static Map<String, Object> handleException(Exception e, String exMode, Environment env) throws TemplateModelException {
        int dashIndex = exMode.indexOf('-');
        if (dashIndex > 0) {
            exMode = exMode.substring(0, dashIndex);
        }
        if ("null".equals(exMode)) {
            return null;
        } else if ("empty".equals(exMode)) {
            return new HashMap<>(); // Use new map, just in case (not performance-sensitive): Collections.emptyMap();
        } else if ("throw".equals(exMode)) {
            throw new TemplateModelException(e);
        } else {
            Map<String, Object> result = "error".equals(exMode) ? ServiceUtil.returnError(e.getMessage()) : new HashMap<>();
            result.put("errorEx", e);
            result.put("errorMessageEx", e.getMessage());
            return result;
        }
    }
}
