package com.ilscipio.cato.ce.webapp.ftl.context;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.UtilGenerics;
import org.ofbiz.base.util.template.FreeMarkerWorker;

import com.ilscipio.cato.ce.webapp.ftl.lang.LangFtlUtil;

import freemarker.core.Environment;
import freemarker.ext.beans.BeanModel;
import freemarker.template.ObjectWrapper;
import freemarker.template.SimpleHash;
import freemarker.template.SimpleSequence;
import freemarker.template.TemplateCollectionModel;
import freemarker.template.TemplateHashModelEx;
import freemarker.template.TemplateModel;
import freemarker.template.TemplateModelException;

/**
 * Cato: Ofbiz and Freemarker context-handling Freemarker utils.
 * <p>
 * Manages request attributes, context vars, and FTL context.
 *
 * @see com.ilscipio.cato.ce.webapp.ftl.CommonFtlUtil
 */
public abstract class ContextFtlUtil {

    public static final String module = ContextFtlUtil.class.getName();
    
    /**
     * Global unique names of cato request variables container maps, or in other words,
     * the cato request variables namespace name.
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
    public static final String REQUEST_VAR_MAP_NAME_REQATTRIBS = "catoTmplReqVarsAttr";
    public static final String REQUEST_VAR_MAP_NAME_GLOBALCONTEXT = "catoTmplReqVarsCtx";
    public static final String REQUEST_VAR_MAP_NAME_FTLGLOBALS = "catoTmplReqVarsFtl";
    
    public static final int REQUEST_STACK_INITIAL_CAPACITY = 10;

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
        Map<String, Object> requestVarMap = new HashMap<String, Object>();
        if (request != null) {
            request.setAttribute(ContextFtlUtil.REQUEST_VAR_MAP_NAME_REQATTRIBS, requestVarMap);
        }
        Map<String, Object> globalContext = getGlobalContext(context, env);
        if (globalContext != null) {
            globalContext.put(ContextFtlUtil.REQUEST_VAR_MAP_NAME_GLOBALCONTEXT, requestVarMap);
        }
        if (env != null) {
            // FIXME?: this doesn't share the map with the above. currently makes no real difference
            // because resetRequestVars usually called with env null, and fallback to ftl globals should be rare anyway.
            // possible could change SimpleHash into SimpleMapModel (around requestVarMap)...
            env.setGlobalVariable(ContextFtlUtil.REQUEST_VAR_MAP_NAME_FTLGLOBALS, new SimpleHash(env.getObjectWrapper()));
        }
    }

    @SuppressWarnings("unchecked")
    private static Map<String, Object> getRequestVarMapFromReqAttribs(HttpServletRequest request) {
        Map<String, Object> map = (Map<String, Object>) request.getAttribute(ContextFtlUtil.REQUEST_VAR_MAP_NAME_REQATTRIBS);
        if (map == null) {
            map = new HashMap<String, Object>();
            request.setAttribute(ContextFtlUtil.REQUEST_VAR_MAP_NAME_REQATTRIBS, map);
        }
        return map;
    }

    @SuppressWarnings("unchecked")
    private static Map<String, Object> getRequestVarMapFromGlobalContext(Map<String, Object> parentMap) {
        Map<String, Object> map = (Map<String, Object>) parentMap.get(ContextFtlUtil.REQUEST_VAR_MAP_NAME_GLOBALCONTEXT);
        if (map == null) {
            map = new HashMap<String, Object>();
            parentMap.put(ContextFtlUtil.REQUEST_VAR_MAP_NAME_GLOBALCONTEXT, map);
        }
        return map;
    }

    private static SimpleHash getRequestVarMapFromFtlGlobals(Environment env) {
        // WARN: we violate Freemarker immutability logic by changing SimpleHash after initial creation,
        // but it doesn't really matter since no template should ever read it.
        SimpleHash map = null;
        try {
            map = (SimpleHash) env.getGlobalVariable(ContextFtlUtil.REQUEST_VAR_MAP_NAME_FTLGLOBALS);
        } catch (TemplateModelException e) {
            Debug.logError(e, "Cato: Error getting request var map from FTL globals", module);
        }
        if (map == null) {
            map = new SimpleHash(env.getObjectWrapper());
            env.setGlobalVariable(ContextFtlUtil.REQUEST_VAR_MAP_NAME_FTLGLOBALS, map);
        }
        return map;
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
     * @param unwrap if true, always try to unwrap value before storing, where possible; if false,
     *               never try to unwrap; if null, implementation decides; generally should be set to null 
     * @param request the servlet request, or null if not available
     * @param context the screen context, or null if not available
     * @param env the Freemarker environment, or null if not available
     * @throws TemplateModelException
     * @see #getRequestVar
     */
    static void setRequestVar(String name, Object value, Boolean unwrap, HttpServletRequest request, 
            Map<String, Object> context, Environment env) throws TemplateModelException {
        if (request != null) {
            if (unwrap == Boolean.TRUE) {
                getRequestVarMapFromReqAttribs(request).put(name, LangFtlUtil.unwrapPermissive(value));
            }
            else {
                // by default, don't bother unwrapping anymore (no point since have containing map)
                getRequestVarMapFromReqAttribs(request).put(name, value);
            }
            //Debug.logInfo("setRequestVar: request attrib (name: " + name + ")", module);
        }
        else {
            Map<String, Object> globalContext = getGlobalContext(context, env);
            if (globalContext != null) {
                if (unwrap == Boolean.TRUE) {
                    getRequestVarMapFromGlobalContext(globalContext).put(name, LangFtlUtil.unwrapPermissive(value));
                }
                else {
                    // by default, don't bother unwrapping anymore (no point since have containing map)
                    getRequestVarMapFromGlobalContext(globalContext).put(name, value);
                }
                //globalContext.put(name, value);
                //Debug.logInfo("setRequestVar: globalContext var (name: " + name + ")", module);
            }
            else if (env != null) {
                getRequestVarMapFromFtlGlobals(env).put(name, (value instanceof TemplateModel) ? 
                        (TemplateModel) value : env.getObjectWrapper().wrap(value));
                //Debug.logInfo("setRequestVar: ftl global var (name: " + name + ")", module);
            }
            else {
                throw new IllegalArgumentException("No request, context or ftl environment to set request scope var (name: " + name + ")");
            }
        }
    }

    public static void setRequestVar(String name, Object value, Boolean unwrap, Environment env) throws TemplateModelException {
        HttpServletRequest request = getRequest(env);
        Map<String, Object> context = null;
        if (request == null) { // optimization: don't need to look this up if has request (true in most cases now)
            context = getContext(env);
        }
        setRequestVar(name, value, unwrap, request, context, env);
    }

    public static void setRequestVar(String name, Object value, Environment env) throws TemplateModelException {
        HttpServletRequest request = getRequest(env);
        Map<String, Object> context = null;
        if (request == null) { // optimization: don't need to look this up if has request (true in most cases now)
            context = getContext(env);
        }
        setRequestVar(name, value, null, request, context, env);
    }

    public static void setRequestVar(String name, Object value, Boolean unwrap, HttpServletRequest request, 
            Map<String, Object> context) throws TemplateModelException {
        setRequestVar(name, value, unwrap, request, context, null);
    }

    public static void setRequestVar(String name, Object value, HttpServletRequest request, 
            Map<String, Object> context) throws TemplateModelException {
        setRequestVar(name, value, null, request, context, null);
    }

    /**
     * Method for getting request-scope variables, with fallback to globals.
     * <p>
     * Must and should only be used to read values set by {@link setRequestVar}.
     * <p>
     * Return value may or may not be a <code>TemplateModel</code>; caller must wrap or unwrap as needed.
     * Can use {@link com.ilscipio.cato.ce.webapp.ftl.TransformFtlUtil} <code>unwrapXxx</code> methods.
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
        // WARNING: currently I don't see any need to wrap OR unwrap the value, no matter how inconsistent
        // it makes the list, so don't do it for performance reasons, but in future it could be needed.
        // caller should always check result from read/popRequestStack so that's where convert should happen;
        // minimizes deep conversions.
        // if non env.setGlobalVariable: 
        //if (value instanceof TemplateModel) {
        //    value = FtlTransformUtil.unwrapPermissive((TemplateModel) value);
        //}
        // if env.setGlobalVariable: 
        //[wrapping code]
    
        if (request != null) {
            Map<String, Object> requestVarMap = getRequestVarMapFromReqAttribs(request);
            
            List<Object> stack;
            Object stackObj = requestVarMap.get(name);
            if (stackObj instanceof List) {
                stack = UtilGenerics.checkList(stackObj);
            }
            else {
                if (stackObj != null) {
                    Debug.logWarning("Overriding request attribute with new stack (name: " + name + ")", module);
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
    
            requestVarMap.put(name, stack);
            //Debug.logInfo("pushRequestStack: request attrib (name: " + name + ")", module);
        }
        else {
            Map<String, Object> globalContext = getGlobalContext(context, env);
            if (globalContext != null) {   
                Map<String, Object> requestVarMap = getRequestVarMapFromGlobalContext(globalContext);
                
                List<Object> stack;
                Object stackObj = requestVarMap.get(name);
                if (stackObj instanceof List) {
                    stack = UtilGenerics.checkList(stackObj);
                }
                else {
                    if (stackObj != null) {
                        Debug.logWarning("Overriding globalContext var with new stack (name: " + name + ")", module);
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
    
                requestVarMap.put(name, stack);
                //Debug.logInfo("pushRequestStack: globalContext var (name: " + name + ")", module);
            }
            else if (env != null) {
                SimpleHash requestVarMap = getRequestVarMapFromFtlGlobals(env);
                
                SimpleSequence stack;
                Object stackObj = requestVarMap.get(name);
                if (stackObj instanceof SimpleSequence) {
                    stack = (SimpleSequence) stackObj;
                }
                else {
                    if (stackObj != null) {
                        Debug.logWarning("Overriding FTL globals var with new stack (name: " + name + ")", module);
                    }
                    stack = new SimpleSequence(ContextFtlUtil.REQUEST_STACK_INITIAL_CAPACITY, env.getObjectWrapper());
                }
                
                // WARN: stack.add() sort of violates freemarker language by modifying list in-place after initial construction,
                // but no one should ever be accessing this list directly anyway apart from these methods
                if (setLast) {
                    if (stack.size() <= 0) {
                        stack.add(value);
                    }
                    else {
                        // NOTE: unfortunately we are forced to create a new stack here... still faster than multiple push/pop calls
                        SimpleSequence newStack = new SimpleSequence(
                                (stack.size() >= ContextFtlUtil.REQUEST_STACK_INITIAL_CAPACITY) ? stack.size() + ContextFtlUtil.REQUEST_STACK_INITIAL_CAPACITY : ContextFtlUtil.REQUEST_STACK_INITIAL_CAPACITY, 
                                env.getObjectWrapper());
                        for(int i=0; i < (stack.size() - 1); i++) {
                            newStack.add(stack.get(i));
                        }
                        stack = newStack;
                        stack.add(value);
                    }                    
                }
                else {
                    stack.add(value);
                }
                
                requestVarMap.put(name, stack);
                //Debug.logInfo("pushRequestStack: ftl global var (name: " + name + ")", module);
            }
            else {
                throw new IllegalArgumentException("No request, context or ftl environment to push request scope stack (name: " + name + ")");
            }
        }
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
            List<Object> stack = null;
            Object stackObj = getRequestVarMapFromReqAttribs(request).get(name);
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
                Debug.logError("Trying to pop empty request attrib stack (name: " + name + ")", module);
            }
            //Debug.logInfo((pop ? "pop" : "read") + "RequestStack: request attrib (name: " + name + ")", module);
        }
        else {
            Map<String, Object> globalContext = getGlobalContext(context, env);
            if (globalContext != null) {   
                List<Object> stack = null;
                Object stackObj = getRequestVarMapFromGlobalContext(globalContext).get(name);
                if (stackObj instanceof List) {
                    stack = UtilGenerics.checkList(stackObj);
                }
                if (stack != null && !stack.isEmpty()) {
                    res = pop ? stack.remove(stack.size() - 1) : stack.get(stack.size() - 1);
                    //if (pop) {
                    //    globalContext.put(name, stack);
                    //}
                }
                else if (pop) {
                    Debug.logError("Trying to pop empty globalContext stack (name: " + name + ")", module);
                }
                //Debug.logInfo((pop ? "pop" : "read") + "RequestStack: globalContext var (name: " + name + ")", module);
            }
            else if (env != null) {
                SimpleHash requestVarMap = getRequestVarMapFromFtlGlobals(env);
                SimpleSequence stack = null;
                Object stackObj = requestVarMap.get(name);
                if (stackObj instanceof SimpleSequence) {
                    stack = (SimpleSequence) stackObj;
                }
                if (stack != null && stack.size() >= 1) {
                    res = stack.get(stack.size() - 1);
                    if (pop) {
                        if (stack.size() <= 1) {
                            env.setGlobalVariable(name, null);
                        }
                        else {
                            // unfortunately this part is poor performance, but it's the only slow op
                            // in all of this (apart from recursive wrapping/unwrapping), so not big deal
                            SimpleSequence newStack = new SimpleSequence(
                                    (stack.size() >= ContextFtlUtil.REQUEST_STACK_INITIAL_CAPACITY) ? stack.size() + ContextFtlUtil.REQUEST_STACK_INITIAL_CAPACITY : ContextFtlUtil.REQUEST_STACK_INITIAL_CAPACITY, 
                                    env.getObjectWrapper());
                            for(int i=0; i < (stack.size() - 1); i++) {
                                newStack.add(stack.get(i));
                            }
                            requestVarMap.put(name, newStack);
                        }
                    }
                }
                else if (pop) {
                    Debug.logError("Trying to pop empty FTL globals stack (name: " + name + ")", module);
                }
                //Debug.logInfo((pop ? "pop" : "read") + "RequestStack: ftl global var (name: " + name + ")", module);
            }
            else {
                throw new IllegalArgumentException("No request, context or ftl environment to " + (pop ? "pop" : "read") + " request scope stack (name: " + name + ")");
            }
        }
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

    public static Object popRequestStack(String name, Environment env) throws TemplateModelException {
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
            Map<String, Object> context, Environment env, LangFtlUtil.TemplateValueTargetType copyTargetType) throws TemplateModelException {
        if (request != null) {
            List<Object> stack = null;
            Object stackObj = getRequestVarMapFromReqAttribs(request).get(name);
            if (stackObj instanceof List) {
                stack = UtilGenerics.checkList(stackObj);
            }
            if (stack != null) {
                if (copyTargetType == null) {
                    return Collections.unmodifiableList(stack);
                }
                else {
                    return LangFtlUtil.copyList(stack, copyTargetType, env != null ? env.getObjectWrapper() : null);
                }
            }
            else {
                return null;
            }
        }
        else {
            Map<String, Object> globalContext = getGlobalContext(context, env);
            if (globalContext != null) {   
                List<Object> stack = null;
                Object stackObj = getRequestVarMapFromGlobalContext(globalContext).get(name);
                if (stackObj instanceof List) {
                    stack = UtilGenerics.checkList(stackObj);
                }
                if (stack != null) {
                    if (copyTargetType == null) {
                        return Collections.unmodifiableList(stack);
                    }
                    else {
                        return LangFtlUtil.copyList(stack, copyTargetType, env != null ? env.getObjectWrapper() : null);
                    }
                }
                else {
                    return null;
                }
            }
            else if (env != null) {
                SimpleHash requestVarMap = getRequestVarMapFromFtlGlobals(env);
                SimpleSequence stack = null;
                Object stackObj = requestVarMap.get(name);
                if (stackObj instanceof SimpleSequence) {
                    stack = (SimpleSequence) stackObj;
                }
                if (stack != null) {
                    if (copyTargetType == null) {
                        return stack; // WARN: can't make unmodifiable?!
                    }
                    else {
                        return LangFtlUtil.copyList(stack, copyTargetType, env != null ? env.getObjectWrapper() : null);
                    }
                }
                else {
                    return null;
                }
            }
            else {
                throw new IllegalArgumentException("No request, context or ftl environment to get request scope stack (name: " + name + ")");
            }
        }
    }

    /**
     * Returns copy of request stack as a SimpleSequence.
     */
    public static Object getRequestStackAsList(String name, Environment env) throws TemplateModelException {
        return ContextFtlUtil.getRequestStackAsList(name, LangFtlUtil.TemplateValueTargetType.SIMPLEMODEL, env);
    }

    /**
     * Returns copy of request stack in request copyTargetType.
     * <strong>WARN</strong>: if copyTargetType is null, no copy is made and unmodifiable list is returned.
     *      This list must be discarded by caller as soon as possible, before any more changes to the stack.
     */
    public static Object getRequestStackAsList(String name, LangFtlUtil.TemplateValueTargetType copyTargetType, Environment env) throws TemplateModelException {
        HttpServletRequest request = getRequest(env);
        Map<String, Object> context = null;
        if (request == null) {
            context = getContext(env);
        }
        return getRequestStackAsList(name, request, context, env, copyTargetType);
    }

    /**
     * Returns copy of request stack as a raw List (but elements may still be TemplateModels).
     */
    public static Object getRequestStackAsList(String name, HttpServletRequest request, Map<String, Object> context) throws TemplateModelException {
        return getRequestStackAsList(name, request, context, null, LangFtlUtil.TemplateValueTargetType.RAW);
    }
    
    /**
     * Merges macro argument maps following the args/inlineArgs pattern.
     * Auto-converts maps to simple maps (but only those where this is logical; usually only
     * the args parameter).
     * 
     * FIXME? suboptimal; must optimize;hard to do. complication is we have no access to the concatenation operator "+"
     * and its implementation is private in Freemarker.
     */
    public static TemplateHashModelEx mergeArgMaps(TemplateHashModelEx args, TemplateHashModelEx inlineArgs,
            TemplateHashModelEx defaultArgs, TemplateHashModelEx overrideArgs, boolean recordArgNames, Environment env) throws TemplateModelException {
        SimpleHash res = new SimpleHash(env.getObjectWrapper());
        ObjectWrapper objectWrapper = env.getObjectWrapper();
        
        if (args != null) {
            args = (TemplateHashModelEx) LangFtlUtil.toSimpleMap(objectWrapper, args);
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


}
