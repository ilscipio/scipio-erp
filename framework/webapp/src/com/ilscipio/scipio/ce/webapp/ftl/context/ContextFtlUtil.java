package com.ilscipio.scipio.ce.webapp.ftl.context;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.UtilGenerics;
import org.ofbiz.base.util.UtilHttp;
import org.ofbiz.base.util.template.FreeMarkerWorker;

import com.ilscipio.scipio.ce.webapp.ftl.lang.LangFtlUtil;
import com.ilscipio.scipio.ce.webapp.ftl.lang.LangFtlUtil.TemplateValueTargetType;

import freemarker.core.Environment;
import freemarker.ext.util.WrapperTemplateModel;
import freemarker.template.ObjectWrapper;
import freemarker.template.SimpleHash;
import freemarker.template.SimpleSequence;
import freemarker.template.TemplateCollectionModel;
import freemarker.template.TemplateHashModelEx;
import freemarker.template.TemplateModel;
import freemarker.template.TemplateModelException;

/**
 * SCIPIO: Ofbiz and Freemarker context-handling Freemarker utils.
 * <p>
 * Manages request attributes, context vars, and FTL context.
 * <p>
 * <strong>WARN:</strong> All utility methods here (except special wrap methods)
 * using ObjectWrapper should take an ObjectWrapper from caller - let caller decide which - and never
 * call Environment.getObjectWrapper anymore.
 * 
 * @see com.ilscipio.scipio.ce.webapp.ftl.CommonFtlUtil
 */
public abstract class ContextFtlUtil {

    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());
    
    /**
     * Global unique names of scipio request variables container maps, or in other words,
     * the scipio request variables namespace name.
     * <p>
     * <em>NOTE</em>: 2015-10-30: All set/getRequestVar and push/read/popRequestStack variables are now stored in
     * a second map within request attributes and context to give them their own namespace and make them trackable.
     * <p>
     * <em>NOTE</em>: Each context must use a different variable name, because contexts may get merged by the
     *     renderer in some situations and leads to unexpected type conflicts.
     * <p>
     * <strong>WARN</strong>: These names are subject to change; never rely on them. Nor should ever need to
     *     access them directly outside of this code.
     *     
     * @see pushRequestStack
     * @see getRequestVar
     */
    public static final String REQUEST_VAR_MAP_NAME_REQATTRIBS = "scipioTmplReqVarsAttr";
    public static final String REQUEST_VAR_MAP_NAME_GLOBALCONTEXT = "scipioTmplReqVarsCtx";
    public static final String REQUEST_VAR_MAP_NAME_FTLGLOBALS = "scipioTmplReqVarsFtl";
    
    public static final int REQUEST_STACK_INITIAL_CAPACITY = 10;

    protected ContextFtlUtil() {
    }
    
    public static HttpServletRequest getRequest(Environment env) throws TemplateModelException {
        WrapperTemplateModel req = (WrapperTemplateModel) env.getVariable("request");
        return (req != null) ? (HttpServletRequest) req.getWrappedObject() : null;
    }

    public static HttpServletResponse getResponse(Environment env) throws TemplateModelException {
        WrapperTemplateModel req = (WrapperTemplateModel) env.getVariable("response");
        return (req != null) ? (HttpServletResponse) req.getWrappedObject() : null;
    }

    /**
     * Fishes the unwrapped/raw screen context out of FTL environment.
     * <p>
     * <strong>WARNING/FIXME?</strong>: in current scipio-patched macro and survey renderers, when this called from
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
        // at least in standard render and scipio-patched other renders, and that it unwraps 
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
     * Removes the whole request vars map.
     */
    public static void removeRequestVars(HttpServletRequest request, 
            Map<String, Object> context, Environment env) throws TemplateModelException {
        if (request != null) {
            request.removeAttribute(ContextFtlUtil.REQUEST_VAR_MAP_NAME_REQATTRIBS);
        }
        Map<String, Object> globalContext = getGlobalContext(context, env);
        if (globalContext != null) {
            globalContext.remove(ContextFtlUtil.REQUEST_VAR_MAP_NAME_GLOBALCONTEXT);
        }
        if (env != null) {
            env.setGlobalVariable(ContextFtlUtil.REQUEST_VAR_MAP_NAME_FTLGLOBALS, null);
        }
    }

    /**
     * Wipes all request vars and sets a new holder map.
     * <p>
     * Attempts to set the same map for as many contexts present as possible, for compatibility.
     * <p>
     * <em>NOTE:</em> in some cases this call is not necessary, but it's a good idea anyway
     * because it will set the same map for request and context (but not ftl - FIXME?),
     * which might prevent problems in odd rendering cases.
     * This should be called at beginning of rendering at a point where as many of the parameters 
     * are non-null as possible (but env will probably usually be null).
     */
    public static void resetRequestVars(HttpServletRequest request, 
            Map<String, Object> context, Environment env) throws TemplateModelException {
        RequestVarMapWrapper mapWrapper = new RequestVarMapWrapper();
        if (request != null) {
            request.setAttribute(ContextFtlUtil.REQUEST_VAR_MAP_NAME_REQATTRIBS, mapWrapper);
        }
        Map<String, Object> globalContext = getGlobalContext(context, env);
        if (globalContext != null) {
            globalContext.put(ContextFtlUtil.REQUEST_VAR_MAP_NAME_GLOBALCONTEXT, mapWrapper);
        }
        if (env != null) {
            // Here we "hide" our variables in freemarker globals
            env.setGlobalVariable(ContextFtlUtil.REQUEST_VAR_MAP_NAME_FTLGLOBALS, new RequestVarMapWrapperModel(mapWrapper.getRawMap()));
        }
    }
    
    public static void resetRequestVars(HttpServletRequest request, Map<String, Object> context) throws TemplateModelException {
        resetRequestVars(request, context, null);
    }

    private static Map<String, Object> getRequestVarMapFromReqAttribs(HttpServletRequest request) {
        RequestVarMapWrapper mapWrapper = (RequestVarMapWrapper) request.getAttribute(ContextFtlUtil.REQUEST_VAR_MAP_NAME_REQATTRIBS);
        if (mapWrapper == null) {
            // FIXME: should try to get underlying map from globalContext or FTL globals
            mapWrapper = new RequestVarMapWrapper();
            request.setAttribute(ContextFtlUtil.REQUEST_VAR_MAP_NAME_REQATTRIBS, mapWrapper);
        }
        return mapWrapper.getRawMap();
    }

    private static Map<String, Object> getRequestVarMapFromGlobalContext(Map<String, Object> parentMap) {
        RequestVarMapWrapper mapWrapper = (RequestVarMapWrapper) parentMap.get(ContextFtlUtil.REQUEST_VAR_MAP_NAME_GLOBALCONTEXT);
        if (mapWrapper == null) {
            // FIXME: should try to get underlying map from request or FTL globals
            mapWrapper = new RequestVarMapWrapper();
            parentMap.put(ContextFtlUtil.REQUEST_VAR_MAP_NAME_GLOBALCONTEXT, mapWrapper);
        }
        return mapWrapper.getRawMap();
    }

    private static Map<String, Object> getRequestVarMapFromFtlGlobals(Environment env) {
        RequestVarMapWrapperModel mapWrapper = null;
        try {
            mapWrapper = (RequestVarMapWrapperModel) env.getGlobalVariable(ContextFtlUtil.REQUEST_VAR_MAP_NAME_FTLGLOBALS);
        } catch (TemplateModelException e) {
            Debug.logError(e, "Scipio: Error getting request var map from FTL globals", module);
        }
        if (mapWrapper == null) {
            // FIXME: should try to get underlying map from request or globalContext
            mapWrapper = new RequestVarMapWrapperModel();
            env.setGlobalVariable(ContextFtlUtil.REQUEST_VAR_MAP_NAME_FTLGLOBALS, mapWrapper);
        }
        return mapWrapper.getRawMap();
    }
    
    
    /**
     * This silly wrapper is needed to prevent Ofbiz context's auto-escaping mechanism from wrapping our map and
     * creating auto-escaping issues.
     * <p>
     * It must NOT extend Map interface.
     */
    public static class RequestVarMapWrapper {
        private final Map<String, Object> map;

        public RequestVarMapWrapper(Map<String, Object> map) {
            this.map = map;
        }
        
        public RequestVarMapWrapper() {
            this.map = new HashMap<String, Object>();
        }
        
        public Map<String, Object> getRawMap() {
            return map;
        }
    }
    
    /**
     * This wrapper essentially hides the request var map values from Freemarker
     * so we don't have to deal with wrapping/unwrapping anymore. We simply
     * store this in FTL vars and use methods provided here to read/write it.
     */
    public static class RequestVarMapWrapperModel implements TemplateModel {
        private final Map<String, Object> map;

        public RequestVarMapWrapperModel(Map<String, Object> map) {
            this.map = map;
        }
        
        public RequestVarMapWrapperModel() {
            this.map = new HashMap<String, Object>();
        }
        
        public Map<String, Object> getRawMap() {
            return map;
        }
    }

    /**
     * Method for setting request-scope variables, with fallback to globals.
     * <p> 
     * Values set by this method should only be read using {@link #getRequestVar} (or a transform that calls it).
     * The values set in request and context may be stored wrapped as <code>TemplateModel</code> or raw object at 
     * implementation's discretion. Likewise in general values read back may be either wrapped or raw object and caller
     * has to check and handle (get/read methods do not convert result), which freemarker calls do anyway.
     * <p>
     * Name should be globally unique across request attribs, screen contexts and FTL context at same time.
     * <p>
     * Currently this sets request attributes above all. If request is missing, tries to set a var in screen globalContext
     * (from passed context; if context null, fished out of FTL env). If globalContext is missing, last resort is to set
     * an FTL #global var. i.e. tries to use longest-lived scope possible.
     * <p>
     * <em>WARN</em>: In general the values are NOT unwrapped by this method before being stored; they preserve their original types
     * wherever possible; caller handles unwrapping if desired. In theory we should unwrap EVERYTHING before
     * storage because the request var map may be used across different rendering contexts;
     * however this adds even more performance overhead. 
     * FIXME?: Maybe should force unwrapping to be safe (and prevent auto-escape wrapping as well)...
     * but will affect all callers.
     * <p>
     * <em>NOTE</em>: 2015-10-30: All set/getRequestVar and push/read/popRequestStack variables are now stored in
     * a second map within request attributes and context to give them their own namespace and make them trackable.
     * <p>
     * <em>NOTE</em>: decision to use request, globalContext or FTL globals is based
     * on whether these contexts are passed, so "statically". it's not based on whether var itself exists.
     * <p>
     * <em>DEV NOTE</em>: could also have set var in all contexts every time but then pushRequestStack
     * has to do the same for consistency and there it would affect performance.
     * setting in all contexts I think would only hide renderer bugs anyway.
     * <p>
     * <em>DEV NOTE</em>: could also set all these vars in their own separate map which is then stored
     * in req attribs/globals. help to avoid name clashes but don't see need yet.
     * 
     * @param name the multi-context unique global var name
     * @param value the value, either raw or <code>TemplateModel</code>
     * @param request the servlet request, or null if not available
     * @param context the screen context, or null if not available
     * @param env the Freemarker environment, or null if not available
     * @throws TemplateModelException
     * @see #getRequestVar
     */
    static void setRequestVar(String name, Object value, HttpServletRequest request, 
            Map<String, Object> context, Environment env) throws TemplateModelException {
        if (request != null) {
            getRequestVarMapFromReqAttribs(request).put(name, value);
            //Debug.logInfo("setRequestVar: request attrib (name: " + name + ")", module);
        }
        else {
            Map<String, Object> globalContext = getGlobalContext(context, env);
            if (globalContext != null) {
                getRequestVarMapFromGlobalContext(globalContext).put(name, value);
                //globalContext.put(name, value);
                //Debug.logInfo("setRequestVar: globalContext var (name: " + name + ")", module);
            }
            else if (env != null) {
                getRequestVarMapFromFtlGlobals(env).put(name, value);
                //Debug.logInfo("setRequestVar: ftl global var (name: " + name + ")", module);
            }
            else {
                throw new IllegalArgumentException("No request, context or ftl environment to set request scope var (name: " + name + ")");
            }
        }
    }

    public static void setRequestVar(String name, Object value, Environment env) throws TemplateModelException {
        HttpServletRequest request = getRequest(env);
        Map<String, Object> context = null;
        if (request == null) { // optimization: don't need to look this up if has request (true in most cases now)
            context = getContext(env);
        }
        setRequestVar(name, value, request, context, env);
    }

    public static void setRequestVar(String name, Object value, HttpServletRequest request, 
            Map<String, Object> context) throws TemplateModelException {
        setRequestVar(name, value, request, context, null);
    }


    /**
     * Method for getting request-scope variables, with fallback to globals.
     * <p>
     * Must and should only be used to read values set by {@link setRequestVar}.
     * <p>
     * Return value may or may not be a <code>TemplateModel</code>; caller must wrap or unwrap as needed.
     * Can use {@link com.ilscipio.scipio.ce.webapp.ftl.TransformFtlUtil} <code>unwrapXxx</code> methods.
     * 
     * @see setRequestVar
     */
    static Object getRequestVar(String name, HttpServletRequest request, 
            Map<String, Object> context, Environment env) throws TemplateModelException {
        Object res = null;
    
        if (request != null) {
            res = getRequestVarMapFromReqAttribs(request).get(name);
            //Debug.logInfo("getRequestVar: request attrib (name: " + name + ")", module);
        }
        else {
            Map<String, Object> globalContext = getGlobalContext(context, env);
            if (globalContext != null) {    
                res = getRequestVarMapFromGlobalContext(globalContext).get(name);
                //Debug.logInfo("getRequestVar: globalContext var (name: " + name + ")", module);
            }
            else if (env != null) {
                res = getRequestVarMapFromFtlGlobals(env).get(name);
                //Debug.logInfo("getRequestVar: ftl global var (name: " + name + ")", module);
            }
            else {
                throw new IllegalArgumentException("No request, context or ftl environment to get request scope var (name: " + name + ")");
            }
        }
        
        return res;
    }

    public static Object getRequestVar(String name, Environment env) throws TemplateModelException {
        HttpServletRequest request = getRequest(env);
        Map<String, Object> context = null;
        if (request == null) {
            context = getContext(env);
        }
        return getRequestVar(name, request, context, env);
    }

    public static Object getRequestVar(String name, HttpServletRequest request, Map<String, Object> context) throws TemplateModelException {
        return getRequestVar(name, request, context, null);
    }

    /**
     * Method providing support for a stack structure having request scope, with fallback to globals.
     * <p>
     * <strong>Do not access underlying structure directly.</strong>
     * 
     * @see setRequestVar
     */
    static void pushRequestStack(String name, Object value, boolean setLast, HttpServletRequest request, 
            Map<String, Object> context, Environment env) throws TemplateModelException {
        // NOTE: There is no value wrapping or unwrapping in these methods anymore; we just store them and all wrapping/unwrapping
        // is left to caller unless absolutely necessary (and in those cases, he specifies the objectWrapper for behavior).
        //if (value instanceof TemplateModel) {
        //    value = FtlTransformUtil.unwrapPermissive((TemplateModel) value);
        //}
        // if env.setGlobalVariable: 
        if (request != null) {
            updateStack(getRequestVarMapFromReqAttribs(request), name, value, setLast, "request attributes");
        }
        else {
            Map<String, Object> globalContext = getGlobalContext(context, env);
            if (globalContext != null) {   
                updateStack(getRequestVarMapFromGlobalContext(globalContext), name, value, setLast, "globalContext");
            }
            else if (env != null) {
                updateStack(getRequestVarMapFromFtlGlobals(env), name, value, setLast, "FTL globals");
            }
            else {
                throw new IllegalArgumentException("No request, context or ftl environment to push request scope stack (name: " + name + ")");
            }
        }
    }
    
    private static void updateStack(Map<String, Object> varMap, String name, Object value, boolean setLast, String desc) {
        List<Object> stack;
        Object stackObj = varMap.get(name);
        if (stackObj instanceof List) {
            stack = UtilGenerics.checkList(stackObj);
        }
        else {
            if (stackObj != null) {
                Debug.logWarning("Overriding " + desc + " var with new stack (name: " + name + ")", module);
            }
            stack = new ArrayList<Object>(ContextFtlUtil.REQUEST_STACK_INITIAL_CAPACITY);
        }
        
        if (setLast) {
            if (stack.isEmpty()) {
                stack.add(value);
            }
            else {
                stack.set(stack.size() - 1, value);
            }                    
        }
        else {
            stack.add(value);
        }
        
        varMap.put(name, stack);
        //Debug.logInfo("pushRequestStack: " + desc + " var (name: " + name + ")", module);
    }
    
    static void pushRequestStack(String name, Object value, boolean setLast, Environment env) throws TemplateModelException {
        HttpServletRequest request = getRequest(env);
        Map<String, Object> context = null;
        if (request == null) {
            context = getContext(env);
        }
        pushRequestStack(name, value, setLast, request, context, env);
    }

    static void pushRequestStack(String name, Object value, boolean setLast, HttpServletRequest request, Map<String, Object> context) throws TemplateModelException {
        pushRequestStack(name, value, setLast, request, context, null);
    }

    static void pushRequestStack(String name, Object value, HttpServletRequest request, 
            Map<String, Object> context, Environment env) throws TemplateModelException {
        pushRequestStack(name, value, false, request, context, env);
    }

    public static void pushRequestStack(String name, Object value, Environment env) throws TemplateModelException {
        HttpServletRequest request = getRequest(env);
        Map<String, Object> context = null;
        if (request == null) {
            context = getContext(env);
        }
        pushRequestStack(name, value, false, request, context, env);
    }

    public static void pushRequestStack(String name, Object value, HttpServletRequest request, Map<String, Object> context) throws TemplateModelException {
        pushRequestStack(name, value, false, request, context, null);
    }

    /**
     * Similar to pushRequestStack but replaces last elem and never fails.
     * <p>
     * <strong>Do not access underlying structure directly.</strong>
     * 
     * @see setRequestVar
     */
    static void setLastRequestStack(String name, Object value, HttpServletRequest request, 
            Map<String, Object> context, Environment env) throws TemplateModelException {
        pushRequestStack(name, value, true, request, context, env);
    }

    public static void setLastRequestStack(String name, Object value, Environment env) throws TemplateModelException {
        HttpServletRequest request = getRequest(env);
        Map<String, Object> context = null;
        if (request == null) {
            context = getContext(env);
        }
        pushRequestStack(name, value, true, request, context, env);
    }

    public static void setLastRequestStack(String name, Object value, HttpServletRequest request, Map<String, Object> context) throws TemplateModelException {
        pushRequestStack(name, value, true, request, context, null);
    }

    /**
     * Method providing support for a stack structure having request scope, with fallback to globals.
     * <p>
     * <strong>Do not access underlying structure directly.</strong>
     * <p>
     * Return value may or may not be a <code>TemplateModel</code>; caller must wrap or unwrap as needed.
     * 
     * @see setRequestVar
     */
    static Object readRequestStack(String name, HttpServletRequest request, 
            Map<String, Object> context, Environment env) throws TemplateModelException {
        return ContextFtlUtil.readRequestStack(name, false, request, context, env);
    }

    public static Object readRequestStack(String name, Environment env) throws TemplateModelException {
        HttpServletRequest request = getRequest(env);
        Map<String, Object> context = null;
        if (request == null) {
            context = getContext(env);
        }
        return ContextFtlUtil.readRequestStack(name, false, request, context, env);
    }

    public static Object readRequestStack(String name, HttpServletRequest request, Map<String, Object> context) throws TemplateModelException {
        return ContextFtlUtil.readRequestStack(name, false, request, context, null);
    }

    static Object readRequestStack(String name, boolean pop, HttpServletRequest request, 
            Map<String, Object> context, Environment env) throws TemplateModelException {
        Object res = null;
    
        if (request != null) {
            res = readStack(getRequestVarMapFromReqAttribs(request), name, pop, "request attributes");
        }
        else {
            Map<String, Object> globalContext = getGlobalContext(context, env);
            if (globalContext != null) {   
                res = readStack(getRequestVarMapFromGlobalContext(globalContext), name, pop, "globalContext");
            }
            else if (env != null) {
                res = readStack(getRequestVarMapFromFtlGlobals(env), name, pop, "FTL globals");
            }
            else {
                throw new IllegalArgumentException("No request, context or ftl environment to " + (pop ? "pop" : "read") + " request scope stack (name: " + name + ")");
            }
        }
        return res;
    }

    private static Object readStack(Map<String, Object> varMap, String name, boolean pop, String desc) {
        Object res = null;
        List<Object> stack = null;
        Object stackObj = varMap.get(name);
        if (stackObj instanceof List) {
            stack = UtilGenerics.checkList(stackObj);
        }
        if (stack != null && !stack.isEmpty()) {
            res = pop ? stack.remove(stack.size() - 1) : stack.get(stack.size() - 1);
            // don't need, just rely on references
            //if (pop) {
            //    request.setAttribute(name, stack);
            //}
        }
        else if (pop) {
            Debug.logError("Trying to pop empty " + desc + " stack (name: " + name + ")", module);
        }
        //Debug.logInfo((pop ? "pop" : "read") + "RequestStack: " + desc + " (name: " + name + ")", module);
        return res;
    }
    
    
    static Object readRequestStack(String name, boolean pop, Environment env) throws TemplateModelException {
        HttpServletRequest request = getRequest(env);
        Map<String, Object> context = null;
        if (request == null) {
            context = getContext(env);
        }
        return readRequestStack(name, pop, request, context, env);
    }

    static Object readRequestStack(String name, boolean pop, HttpServletRequest request, Map<String, Object> context) throws TemplateModelException {
        return readRequestStack(name, pop, request, context, null);
    }

    /**
     * Method providing support for a stack structure having request scope, with fallback to globals.
     * <p>
     * <strong>Do not access underlying structure directly.</strong>
     * <p>
     * Return value may or may not be a <code>TemplateModel</code>; caller must wrap or unwrap as needed.
     * 
     * @see setRequestVar
     */ 
    static Object popRequestStack(String name, HttpServletRequest request, 
            Map<String, Object> context, Environment env) throws TemplateModelException {
        return readRequestStack(name, true, request, context, env);
    }

    public static Object popRequestStack(String name, Environment env, ObjectWrapper objectWrapper) throws TemplateModelException {
        HttpServletRequest request = getRequest(env);
        Map<String, Object> context = null;
        if (request == null) {
            context = getContext(env);
        }
        return readRequestStack(name, true, request, context, env);
    }

    public static Object popRequestStack(String name, HttpServletRequest request, Map<String, Object> context) throws TemplateModelException {
        return readRequestStack(name, true, request, context, null);
    }

    /**
     * Gets the request stack as a list. The stack cannot be modified using this list.
     * It may be TemplateModel-wrapped or unwrapped as may be the individual values.
     *
     * @param name
     * @param request
     * @param context
     * @param env
     * @param copyTargetType target type for list copy. if null, does not copy (should be avoided in most cases!).
     * @return
     * @throws TemplateModelException
     */
    static Object getRequestStackAsList(String name, HttpServletRequest request, 
            Map<String, Object> context, Environment env, ObjectWrapper objectWrapper, TemplateValueTargetType copyTargetType) throws TemplateModelException {
        if (request != null) {
            return getStackAsList(getRequestVarMapFromReqAttribs(request), name, copyTargetType, objectWrapper, "request attributes");
        }
        else {
            Map<String, Object> globalContext = getGlobalContext(context, env);
            if (globalContext != null) {  
                return getStackAsList(getRequestVarMapFromGlobalContext(globalContext), name, copyTargetType, objectWrapper, "globalContext");
            }
            else if (env != null) {
                return getStackAsList(getRequestVarMapFromFtlGlobals(env), name, copyTargetType, objectWrapper, "FTL globals");
            }
            else {
                throw new IllegalArgumentException("No request, context or ftl environment to get request scope stack (name: " + name + ")");
            }
        }
    }

    private static Object getStackAsList(Map<String, Object> varMap, String name, TemplateValueTargetType copyTargetType, ObjectWrapper objectWrapper, String desc) throws TemplateModelException {
        List<Object> stack = null;
        Object stackObj = varMap.get(name);
        if (stackObj instanceof List) {
            stack = UtilGenerics.checkList(stackObj);
        }
        if (stack != null) {
            if (copyTargetType == null) {
                return Collections.unmodifiableList(stack);
            }
            else {
                return LangFtlUtil.copyList(stack, copyTargetType, objectWrapper);
            }
        }
        else {
            return null;
        }
    }
    
    
    /**
     * Returns copy of request stack as a SimpleSequence.
     */
    public static Object getRequestStackAsList(String name, Environment env, ObjectWrapper objectWrapper) throws TemplateModelException {
        return ContextFtlUtil.getRequestStackAsList(name, LangFtlUtil.TemplateValueTargetType.SIMPLEMODEL, env, objectWrapper);
    }

    /**
     * Returns copy of request stack in request copyTargetType.
     * <strong>WARN</strong>: if copyTargetType is null, no copy is made and unmodifiable list is returned.
     *      This list must be discarded by caller as soon as possible, before any more changes to the stack.
     */
    public static Object getRequestStackAsList(String name, LangFtlUtil.TemplateValueTargetType copyTargetType, Environment env, ObjectWrapper objectWrapper) throws TemplateModelException {
        HttpServletRequest request = getRequest(env);
        Map<String, Object> context = null;
        if (request == null) {
            context = getContext(env);
        }
        return getRequestStackAsList(name, request, context, env, objectWrapper, copyTargetType);
    }
    

    static Integer getRequestStackSize(String name, HttpServletRequest request, 
            Map<String, Object> context, Environment env) throws TemplateModelException {
        if (request != null) {
            return getStackSize(getRequestVarMapFromReqAttribs(request), name);
        }
        else {
            Map<String, Object> globalContext = getGlobalContext(context, env);
            if (globalContext != null) {  
                return getStackSize(getRequestVarMapFromGlobalContext(globalContext), name);
            }
            else if (env != null) {
                return getStackSize(getRequestVarMapFromFtlGlobals(env), name);
            }
            else {
                return null;
            }
        }
    }
    
    private static Integer getStackSize(Map<String, Object> varMap, String name) throws TemplateModelException {
        List<Object> stack = null;
        Object stackObj = varMap.get(name);
        if (stackObj instanceof List) {
            stack = UtilGenerics.checkList(stackObj);
        }
        if (stack != null) {
            return stack.size();
        }
        else {
            return null;
        }
    }
    
    public static Integer getRequestStackSize(String name, Environment env) throws TemplateModelException {
        HttpServletRequest request = getRequest(env);
        Map<String, Object> context = null;
        if (request == null) {
            context = getContext(env);
        }
        return getRequestStackSize(name, request, context, env);
    }

    /**
     * Returns copy of request stack as a raw List (but elements may still be TemplateModels).
     */
    public static Object getRequestStackAsList(String name, HttpServletRequest request, Map<String, Object> context) throws TemplateModelException {
        return getRequestStackAsList(name, request, context, null, null, LangFtlUtil.TemplateValueTargetType.RAW);
    }
    
    /**
     * Merges macro argument maps following the args/inlineArgs pattern.
     * Auto-converts maps to simple maps (but only those where this is logical; usually only
     * the args parameter).
     * <p>
     * NOTE: The objectWrapper is the one the resulting SimpleHash is initialized with, but usually it is
     * not used for any of the initial values merged into it by this call.
     * <p>
     * FIXME? suboptimal; must optimize; hard to do. complication is we have no access to the concatenation operator "+"
     * and its implementation is private in Freemarker.
     * 
     */
    public static TemplateHashModelEx mergeArgMaps(TemplateHashModelEx args, TemplateHashModelEx inlineArgs,
            TemplateHashModelEx defaultArgs, TemplateHashModelEx overrideArgs, boolean recordArgNames, Environment env, ObjectWrapper objectWrapper) throws TemplateModelException {
        SimpleHash res = new SimpleHash(objectWrapper);
        
        if (args != null) {
            args = (TemplateHashModelEx) LangFtlUtil.toSimpleMap(args, null, objectWrapper);
        }
        
        if (defaultArgs != null && !defaultArgs.isEmpty()) {
            LangFtlUtil.addToSimpleMap(res, defaultArgs);
        }
        if (args != null && !args.isEmpty()) {
            LangFtlUtil.addToSimpleMap(res, args);
        }
        if (inlineArgs != null && !inlineArgs.isEmpty()) {
            LangFtlUtil.addToSimpleMap(res, inlineArgs);
        }
        if (overrideArgs != null && !overrideArgs.isEmpty()) {
            LangFtlUtil.addToSimpleMap(res, overrideArgs);
        }
        
        if (recordArgNames) {
            // FIXME: this whole part definitely too inefficient, but freemarker wants it...
            // TODO: for allArgNames, use bean-wrapped set and modify in-place (screw immutability)
            // TODO: for localArgNames, would be better to have the "+" operator result...
            // Problem: we have no access to the concatenation operator "+" which in some cases is desirable
            TemplateCollectionModel defaultKeys = defaultArgs != null ? defaultArgs.keys() : null;
            TemplateCollectionModel overrideKeys = overrideArgs != null ? overrideArgs.keys() : null;
            
            SimpleSequence localArgNames = new SimpleSequence(objectWrapper);
            if (defaultKeys != null) {
                LangFtlUtil.addToSimpleList(localArgNames, defaultKeys);
            }
            if (overrideKeys != null) {
                LangFtlUtil.addToSimpleList(localArgNames, overrideKeys);
            }
            
            TemplateModel allArgNamesPrev = args != null ? args.get("allArgNames") : null;
            SimpleSequence allArgNames = new SimpleSequence(objectWrapper);
            if (allArgNamesPrev != null && allArgNamesPrev != TemplateModel.NOTHING) {
                LangFtlUtil.addToSimpleList(allArgNames, allArgNamesPrev);
            }
            LangFtlUtil.addToSimpleList(allArgNames, localArgNames);
            
            res.put("localArgNames", localArgNames);
            res.put("allArgNames", allArgNames);
        }
        return res;
    }

    /**
     * Attempts to get the current "user" or "screen" locale normally found in screen context
     * as the simple "locale" variable.
     * TODO: REVIEW: this is currently using getGlobalVariable as most likely the fastest that
     * will avoid problems from macros - unclear if more or less reliable than trying to read
     * out of "context" map (which not guaranteed present).
     */
    public static Locale getContextLocale(Environment env) throws TemplateModelException {
        WrapperTemplateModel model = (WrapperTemplateModel) env.getGlobalVariable("locale");
        if (model != null) return (Locale) ((WrapperTemplateModel) model).getWrappedObject();
        return null;
    }
    
    /**
     * Gets locale from the HttpServletRequest object in context using UtilHttp.
     */
    public static Locale getRequestLocale(Environment env) throws TemplateModelException {
        HttpServletRequest request = getRequest(env);
        if (request != null) return UtilHttp.getLocale(request);
        return null;
    }
}
