package com.ilscipio.cato.webapp.ftl;

import java.util.Map;

import javax.servlet.http.HttpServletRequest;

import org.ofbiz.base.util.UtilGenerics;
import org.ofbiz.base.util.template.FreeMarkerWorker;

import freemarker.core.Environment;
import freemarker.ext.beans.BeanModel;
import freemarker.template.TemplateModel;
import freemarker.template.TemplateModelException;
import freemarker.template.utility.DeepUnwrap;

/**
 * FTL transform utility methods.
 */
public final class FtlTransformUtil {

    private FtlTransformUtil() {
    }
    
    public static Environment getCurrentEnvironment() throws TemplateModelException {
        Environment env = FreeMarkerWorker.getCurrentEnvironment();
        if (env == null) {
            throw new TemplateModelException("Missing Freemarker environment");
        }
        return env;
    }
    
    public static HttpServletRequest getRequest(Environment env) throws TemplateModelException {
        BeanModel req = (BeanModel) env.getVariable("request");
        return (req != null) ? (HttpServletRequest) req.getWrappedObject() : null;
    }
    
    public static Map<String, Object> getGlobalContext(Environment env) throws TemplateModelException {
        Map<String, Object> res = null;
        Map<String, Object> context = FreeMarkerWorker.getWrappedObject("context", env);
        if (context != null) {
            res = UtilGenerics.checkMap(context.get("globalContext"));
        }
        return res;
    }
    
    public static Object unwrap(TemplateModel templateModel) throws TemplateModelException {
        // FIXME? or should this be based on FreeMarkerWorker.getWrappedObject(templateModel) instead??
        return DeepUnwrap.unwrap(templateModel);
    }
    
    public static Object getDefaultIfNull(Object obj, TemplateModel defaultVal) throws TemplateModelException {
        if ((obj == null || obj == TemplateModel.NOTHING) && defaultVal != null) {
            return defaultVal;
        }
        else {
            return obj;
        }
    }
    
}
