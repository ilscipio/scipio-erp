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
    
    /**
     * Unwraps template model; if cannot, returns null.
     */
    public static Object unwrapOrNull(TemplateModel templateModel) throws TemplateModelException {
        if (templateModel != null) {
            Object res = DeepUnwrap.permissiveUnwrap(templateModel);
            if (res != templateModel) {
                return res;
            }
            else {
                return null;
            }
        }
        else {
            return null;
        }
    }
    
    /**
     * If TemplateModel, unwraps value, or if cannot, returns null;
     * if not TemplateModel, returns as-is.
     * <p>
     * Ensures no TemplateModels remain.
     */
    public static Object unwrapOrNull(Object value) throws TemplateModelException {
        if (value instanceof TemplateModel) {
            Object res = DeepUnwrap.permissiveUnwrap((TemplateModel) value);
            if (res != value) {
                return res;
            }
            else {
                return null;
            }
        }
        else {
            return value;
        }
    }
    
    /**
     * Unwraps template model; if cannot, returns as-is.
     */
    public static Object unwrapPermissive(TemplateModel templateModel) throws TemplateModelException {
        if (templateModel != null) {
            return DeepUnwrap.permissiveUnwrap(templateModel);
        }
        else {
            return null;
        }
    }
    
    /**
     * Unwraps value; if cannot, returns value, even if still TemplateModel.
     */
    public static Object unwrapPermissive(Object value) throws TemplateModelException {
        if (value instanceof TemplateModel) {
            return DeepUnwrap.permissiveUnwrap((TemplateModel) value);
        }
        else {
            return value;
        }
    }    
    
    /**
     * Unwraps template model; if cannot, throws exception. If null, returns null.
     */
    public static Object unwrap(TemplateModel templateModel) throws TemplateModelException {
        if (templateModel != null) {
            return DeepUnwrap.unwrap(templateModel); // will throw exception if improper type
        }
        else {
            return null;
        }
    }
    
    /**
     * If template model, unwraps, or if cannot, throws exception;
     * if not template model or null, returns value.
     */
    public static Object unwrap(Object value) throws TemplateModelException {
        if (value instanceof TemplateModel) {
            return DeepUnwrap.unwrap((TemplateModel) value);
        }
        else {
            return value;
        }
    }    
    
    /**
     * Unwraps template model; if cannot, throws exception.
     * <p>
     * Interpretation of null depends on the ObjectWrapper.
     */
    public static Object unwrapAlways(TemplateModel templateModel) throws TemplateModelException {
        // FIXME? should all these DeepUnwrap.unwrap calls be more like FreeMarkerWorker.getWrappedObject(templateModel) instead??
        return DeepUnwrap.unwrap(templateModel); // will throw exception if improper type
    }
    
    /**
     * Unwraps value if template model and unwrappable; else exception.
     * <p>
     * Interpretation of null depends on the ObjectWrapper.
     */
    public static Object unwrapAlways(Object value) throws TemplateModelException {
        if (value instanceof TemplateModel || value == null) {
            return DeepUnwrap.unwrap((TemplateModel) value);
        }
        else {
            throw new TemplateModelException("Cannot unwrap non-TemplateModel value (type " + value.getClass().getName() + ")");
        }
    }    
    
}
