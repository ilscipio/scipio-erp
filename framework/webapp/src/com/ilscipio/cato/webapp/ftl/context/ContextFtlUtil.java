package com.ilscipio.cato.webapp.ftl.context;

import java.util.Map;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.ofbiz.base.util.UtilGenerics;
import org.ofbiz.base.util.template.FreeMarkerWorker;

import freemarker.core.Environment;
import freemarker.ext.beans.BeanModel;
import freemarker.template.TemplateModelException;

public abstract class ContextFtlUtil {

    protected ContextFtlUtil() {
    }
    
    public static HttpServletRequest getRequest(Environment env) throws TemplateModelException {
        BeanModel req = (BeanModel) env.getVariable("request");
        return (req != null) ? (HttpServletRequest) req.getWrappedObject() : null;
    }

    public static HttpServletResponse getResponse(Environment env) throws TemplateModelException {
        BeanModel req = (BeanModel) env.getVariable("response");
        return (req != null) ? (HttpServletResponse) req.getWrappedObject() : null;
    }

    /**
     * Fishes the unwrapped/raw screen context out of FTL environment.
     * <p>
     * <strong>WARNING/FIXME?</strong>: in current cato-patched macro and survey renderers, when this called from
     * macros/templates it will be null; only globalContext is present. note "context" is not
     * a real var but a special MapStack key (only on MapStack.get(); not part of MapStack.keySet() at time of writing).
     */
    public static Map<String, Object> getContext(Environment env) throws TemplateModelException {
        // this is what Ofbiz code currently does; should be a BeanModel wrapping the real context.
        return FreeMarkerWorker.getWrappedObject("context", env);
    }

    /**
     * Fishes the unwrapped/raw screen globalContext out of FTL environment.
     */
    public static Map<String, Object> getGlobalContext(Environment env) throws TemplateModelException {
        Map<String, Object> res = FreeMarkerWorker.getWrappedObject("globalContext", env);
        // I think globalContext is always present as a top-level #global or is supposed to be, 
        // at least in standard render and cato-patched other renders, and that it unwraps 
        // to real globalContext from BeanModel, but in case not, check context.globalContext.
        if (res == null) {
            Map<String, Object> context = getContext(env);
            if (context != null) {
                res = UtilGenerics.checkMap(context.get("globalContext"));
            }
        }
        return res;
    }

    /**
     * Get screen globalContext from given context, or from FTL env if (and only if) passed context is null. 
     */
    public static Map<String, Object> getGlobalContext(Map<String, Object> context, Environment env) throws TemplateModelException {
        Map<String, Object> res = null;
        if (context != null) {
            res = UtilGenerics.checkMap(context.get("globalContext"));
        }
        else if (env != null) {
            res = getGlobalContext(env);
        }
        return res;
    }

}
