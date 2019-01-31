package org.ofbiz.webapp.ftl;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.base.util.template.FreeMarkerWorker;
import org.ofbiz.service.LocalDispatcher;
import org.ofbiz.service.ModelService;

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
    public Object exec(@SuppressWarnings("rawtypes") List arguments) throws TemplateModelException {
        String serviceName = TransformUtil.getStringNonEscapingArg(arguments, "name", 0);
        if (UtilValidate.isEmpty(serviceName)) {
            throw new TemplateModelException("Missing service name");
        }
        Environment env = FreeMarkerWorker.getCurrentEnvironment();
        LocalDispatcher dispatcher = ContextFtlUtil.getDispatcher(env);

        @SuppressWarnings("unchecked")
        Map<String, Object> context = (Map<String, Object>) LangFtlUtil.unwrap(TransformUtil.getModel(arguments, "ctx", 1));
        if (context == null) {
            context = new HashMap<>();
        } else {
            try {
                context = dispatcher.getDispatchContext().makeValidContext(serviceName, ModelService.IN_PARAM, context);
            } catch (Exception e) {
                Debug.logError(e, "Error validating service '" + serviceName + "' context", module);
                if (isThrowExceptions(arguments)) {
                    throw new TemplateModelException(e);
                }
                return null;
            }
        }

        if (!Boolean.FALSE.equals(TransformUtil.getBooleanArg(arguments, "inclEnvCtx", -1))) {
            Map<String, Object> envCtx = ContextFtlUtil.getContext(env);
            if (!context.containsKey("userLogin") && envCtx.containsKey("userLogin")) {
                context.put("userLogin", envCtx.get("userLogin"));
            }
            if (!context.containsKey("locale") && envCtx.containsKey("locale")) {
                context.put("locale", envCtx.get("locale"));
            }
            if (!context.containsKey("timeZone") && envCtx.containsKey("timeZone")) {
                context.put("timeZone", envCtx.get("timeZone"));
            }
        }

        boolean newTrans = Boolean.TRUE.equals(TransformUtil.getBooleanArg(arguments, "newTrans", -1));
        try {
            return dispatcher.runSync(serviceName, context, newTrans);
        } catch (Exception e) {
            Debug.logError(e, "Error invoking service '" + serviceName + "'", module);
            if (isThrowExceptions(arguments)) {
                throw new TemplateModelException(e);
            }
            return null;
        }
    }

    private static boolean isThrowExceptions(List<?> arguments) throws TemplateModelException {
        return Boolean.TRUE.equals(TransformUtil.getBooleanArg(arguments, "throwEx", -1));
    }
}
