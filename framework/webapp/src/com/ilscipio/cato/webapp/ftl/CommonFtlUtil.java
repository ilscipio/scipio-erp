package com.ilscipio.cato.webapp.ftl;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import javax.servlet.http.HttpServletRequest;

import org.apache.commons.lang.StringUtils;
import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.UtilGenerics;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.base.util.cache.UtilCache;

import freemarker.core.Environment;
import freemarker.core.Macro;
import freemarker.ext.beans.BeansWrapper;
import freemarker.ext.beans.SimpleMapModel;
import freemarker.ext.util.WrapperTemplateModel;
import freemarker.template.ObjectWrapper;
import freemarker.template.SimpleHash;
import freemarker.template.SimpleSequence;
import freemarker.template.TemplateCollectionModel;
import freemarker.template.TemplateDirectiveModel;
import freemarker.template.TemplateHashModel;
import freemarker.template.TemplateHashModelEx;
import freemarker.template.TemplateModel;
import freemarker.template.TemplateModelException;
import freemarker.template.TemplateModelIterator;
import freemarker.template.TemplateScalarModel;
import freemarker.template.TemplateSequenceModel;
import freemarker.template.TemplateTransformModel;
import javolution.util.FastMap;

/**
 * Cato: Common generic Freemarker templating utility methods.
 * <p>
 * Java language analog of <code>component://common/webcommon/includes/cato/lib/utilities.ftl</code>.
 * <p>
 * <em>NOTE</em>: This is for common, general-purpose code only (generic utilities). Code that implements
 * template macro markup (e.g. <code>htmlTemplate.ftl</code>) logic belongs in separate class.
 */
public final class CommonFtlUtil {

    public static final String module = CommonFtlUtil.class.getName();
    
    /**
     * Global unique name of cato request variables container maps, or in other words,
     * the cato request variables namespace name.
     * <p>
     * <em>NOTE</em>: 2015-10-30: All set/getRequestVar and push/read/popRequestStack variables are now stored in
     * a second map within request attributes and context to give them their own namespace and make them trackable.
     */
    public static final String REQUEST_VAR_MAP_NAME = "catoTmplReqVars";
    
    public static final int REQUEST_STACK_INITIAL_CAPACITY = 10; 
    
    private static final UtilCache<String, Map<String, Object>> headingElemSpecFromStyleStrCache = 
            UtilCache.createUtilCache("com.ilscipio.cato.webapp.ftl.CommonFtlUtil.headingElemSpecFromStyleStrCache");
    
    private static final Set<String> emptyStrSet = new HashSet<String>();
    
    
    private CommonFtlUtil() {
    }
    
    /**
     * Used for TemplateModel <-> unwrapped/raw value conversions.
     */
    public enum TemplateValueTargetType {
        PRESERVE,
        RAW,
        MODEL,
        SIMPLEMODEL,
        COMPLEXMODEL
    }

    /**
     * Parses a complex style string meant to describe an element notably heading into hash of constituent values.
     * <p>
     * If cacheId is specified, the result is looked up from a global cache based on a key composed
     * of styleStr, containerStyleStr, and cacheId. cacheId must uniquely identify the remaining parameters.
     *
     * @param styleStr
     * @param containerStyleStr
     * @param allowedHeadingElemTypes string of "|"-separated names or collection
     * @param allowedElemTypes string of "|"-separated names or collection, or null or boolean to let caller figure out
     * @param allowedContainerElemTypes string of "|"-separated names or collection, or null or boolean to let caller figure out
     * @param cacheId global unique cache ID that should uniquely identify combination of params other than styleStr and containerStyleStr
     * @return
     */
    public static Map<String, Object> getHeadingElemSpecFromStyleStr(String styleStr, String containerStyleStr, 
            Object allowedHeadingElemTypes, Object allowedElemTypes, Object allowedContainerElemTypes,
            String cacheId) {
        Map<String, Object> res = null;
        String cacheKey = null;
        if (cacheId != null && !cacheId.isEmpty()) {
            cacheKey = styleStr + "::" + containerStyleStr + "::" + cacheId;
        }
  
        if (cacheKey != null) {
            res = headingElemSpecFromStyleStrCache.get(cacheKey);
        }
        
        if (res == null) {
            res = getHeadingElemSpecFromStyleStr(styleStr, containerStyleStr, allowedHeadingElemTypes, allowedElemTypes, allowedContainerElemTypes);
            
            if (cacheKey != null) {
                // note: probably no need to synchronize on cache; duplicate insertion is ok
                headingElemSpecFromStyleStrCache.put(cacheKey, res);
            }
        }
        
        return res;
    }

    public static Map<String, Object> getHeadingElemSpecFromStyleStr(String styleStr, String containerStyleStr, 
            Object allowedHeadingElemTypes, Object allowedElemTypes, Object allowedContainerElemTypes) {
        String allowedHeadingElemTypesStr = nameListArgToStr(allowedHeadingElemTypes);
        Set<String> allowedElemTypesSet = nameListArgToSet(allowedElemTypes);
        Set<String> allowedContainerElemTypesSet = nameListArgToSet(allowedContainerElemTypes);

        String headingLevel = "";
        String relHeadingLevel = "";
        boolean isHeadingElem = false;
        
        // TODO: clean up names. optimization not needed due to cache.
        
        String titleStyle = styleStr;
        String titleContainerStyle = containerStyleStr;
                
        Map<String, String> titleArgs = FastMap.newInstance();
        String titleArgsStr = titleStyle;
        
        String[] titleStyleParts = titleStyle.split(";");
        
        if (titleStyleParts.length > 3) {
            titleArgsStr = titleStyleParts[2];
        }
        else {
            titleArgsStr = titleStyleParts[titleStyleParts.length - 1];
        }
        
        if (UtilValidate.isNotEmpty(titleArgsStr) && titleArgsStr.contains("=")) { //heuristic detect params part
            titleArgs = splitStrParams(titleArgsStr, ",");
            if (titleStyleParts.length >= 3) {
                titleContainerStyle = titleStyleParts[0];
                titleStyle = titleStyleParts[1];
            }
            else if (titleStyleParts.length == 2) {
                titleStyle = titleStyleParts[0];
            }
            else {
                titleStyle = "";
            }
        }
        else if (titleStyleParts.length > 1) {
            titleContainerStyle = titleStyleParts[0];
            titleStyle = titleStyleParts[1];
        }
        
        String titleContainerElemType = "";
        String titleContainerClass = "";
        
        if (UtilValidate.isNotEmpty(titleContainerStyle)) {
            String[] titleContainerStyleParts = titleContainerStyle.split(":");
            if (titleContainerStyleParts.length <= 1) {
                titleContainerElemType = titleContainerStyle.toLowerCase();
                titleContainerClass = titleContainerStyle;
            }
            else {
                titleContainerElemType = titleContainerStyleParts[0].toLowerCase();
                titleContainerClass = titleContainerStyle.substring(titleContainerElemType.length() + 1);
            }
            
            // if omitted, leave to caller to figure out if titleContainerStyle elem or class
            if (allowedContainerElemTypesSet != null) {
                if (allowedContainerElemTypesSet.contains(titleContainerElemType)) {
                    if (titleContainerStyleParts.length <= 1) {
                        titleContainerClass = "";
                    }
                }
                else {
                    titleContainerElemType = "";
                    titleContainerClass = titleContainerStyle;
                }
            }
        }
        
        String titleElemType = "";
        String titleClass = "";
        
        if (UtilValidate.isNotEmpty(titleStyle)) {
            titleStyleParts = titleStyle.split(":");
            
            if (titleStyleParts.length <= 1) {
                titleElemType = titleStyle.toLowerCase();
                titleClass = titleStyle;
            }
            else {
                titleElemType = titleStyleParts[0].toLowerCase();
                titleClass = titleStyle.substring(titleElemType.length() + 1);
            }
            
            boolean elemTypeFound = false;
            
            if (UtilValidate.isNotEmpty(allowedHeadingElemTypesStr)) {
                Pattern pat = Pattern.compile("(" + allowedHeadingElemTypesStr + ")(\\+)?(\\d*)");
                Matcher m = pat.matcher(titleElemType);
                if (m.matches()) {
                    if (UtilValidate.isNotEmpty(m.group(2))) {
                        if (UtilValidate.isNotEmpty(m.group(3))) {
                            relHeadingLevel = m.group(3);
                        }
                    }
                    else {
                        if (UtilValidate.isNotEmpty(m.group(3))) {
                            // overrides headingLevel (so style from screen affects heading calc)
                            headingLevel = m.group(3);
                        }
                    }
                    if (titleStyleParts.length <= 1) {
                        titleClass = "";
                    }
                    titleElemType = m.group(1);
                    elemTypeFound = true;
                    isHeadingElem = true;
                }
            }
            
            
            // if not specified, let caller figure it out if titleStyle elem or class
            if (!elemTypeFound && allowedElemTypesSet != null) {
                if (allowedElemTypesSet.contains(titleElemType)) {
                    if (titleStyleParts.length <= 1) {
                        titleClass = "";
                    }
                    elemTypeFound = true;
                }
                else {
                    titleElemType = "";
                    // if invalid type found, use the full string as class, in case ":" char is important somehow
                    titleClass = titleStyle;
                }
            }
            
        }
        
        Map<String, Object> res = FastMap.newInstance();
        res.putAll(titleArgs);
        res.put("containerStyleStr", titleContainerStyle); 
        res.put("containerElemType", titleContainerElemType); 
        res.put("containerElemClass", titleContainerClass); 
        
        res.put("styleStr", titleStyle); 
        res.put("elemType", titleElemType); 
        res.put("elemClass", titleClass);
        //res.put("class", titleClass); // WARN: DO NOT USE, will conflict with getClass() method
        
        res.put("isHeadingElem", isHeadingElem); 
        if (!headingLevel.isEmpty() || !res.containsKey("level")) { // may already come from args
            res.put("level", headingLevel); 
        }
        if (!relHeadingLevel.isEmpty() || !res.containsKey("relLevel")) {
            res.put("relLevel", relHeadingLevel); 
        }
        
        res.put("argsStr", titleArgsStr); 
        
        return Collections.unmodifiableMap(res);
    }
    
    private static String nameListArgToStr(Object arg) {
        String res = null;
        if (arg instanceof String) {
            res = (String) arg;
        }
        else if (arg instanceof Collection) {
            Collection<String> argColl = UtilGenerics.checkCollection(arg);
            if (argColl.isEmpty()) {
                res = "";
            }
            else {
                res = StringUtils.join(argColl,"|");
            }
        }
        return res;
    }
    
    private static Set<String> nameListArgToSet(Object arg) {
        Set<String> res = null;
        if (arg instanceof Set) {
            res = UtilGenerics.checkSet(arg);
        }
        else if (arg instanceof String) {
            res = new HashSet<String>(Arrays.asList(((String) arg).split("\\|")));
        }
        else if (arg instanceof Collection) {
            Collection<String> argColl = UtilGenerics.checkCollection(arg);
            res = new HashSet<String>(argColl);
        }
        return res;
    }    
    
    /**
     * Extracts parameters from a string in the format and returns as a hash:
     * name1=val1DELIMname2=val2DELIMname3=val3
     * where DELIM is specified delimiter (& &amp; , ; etc.)
     */
    public static Map<String, String> splitStrParams(String paramStr, String paramDelim) {
        Map<String, String> res = FastMap.newInstance();
        for(String pair : paramStr.split(paramDelim)) {
            String[] parts = pair.split("=", 2);
            if (parts.length >= 2) {
                res.put(parts[0], parts[1]);
            }
        }
        return res;
    }
    
    
    /**
     * Makes a generic element attribute string for html, xml, etc. from attrib map.
     * <p>
     * If emptyValToken non-empty, values matching emptyValToken are treated as empty and 
     * included regardless of includeEmpty setting.
     */
    public static String makeElemAttribStr(Map<String, Object> attribs, boolean includeEmpty, String emptyValToken, Collection<String> exclude) {
        StringBuilder sb = new StringBuilder();
        
        if (emptyValToken == null) {
            emptyValToken = "";
        }
        
        if (exclude == null || exclude.isEmpty()) {
            exclude = emptyStrSet;
        }
        else if (!(exclude instanceof Set)) {
            exclude = new HashSet<String>(exclude); // faster
        }
        
        for(Map.Entry<String, Object> pair : attribs.entrySet()) {
            String name = pair.getKey();
            if (!exclude.contains(name)) {
                Object val = pair.getValue();
                String valStr = (val != null) ? val.toString() : "";
                
                if (includeEmpty || !valStr.isEmpty()) {
                    sb.append(" ");
                    sb.append(name);
                    sb.append("=\"");
                    if (!valStr.equals(emptyValToken)) {
                        sb.append(valStr);
                    }
                    sb.append("\"");
                }
            } 
        }
        
        return sb.toString();
    }
    
    @SuppressWarnings("unchecked")
    private static Object getCatoRequestVarsEntry(Object mapObj, String varName) {
        if (mapObj == null) {
            return null;
        }
        else {
            return ((Map<String, Object>) mapObj).get(varName);
        }
    }
    
    /**
     * Removes the whole request vars map.
     */
    public static void removeRequestVars(HttpServletRequest request, 
            Map<String, Object> context, Environment env) throws TemplateModelException {
        if (request != null) {
            request.removeAttribute(REQUEST_VAR_MAP_NAME);
        }
        Map<String, Object> globalContext = FtlTransformUtil.getGlobalContext(context, env);
        if (globalContext != null) {
            globalContext.remove(REQUEST_VAR_MAP_NAME);
        }
        if (env != null) {
            env.setGlobalVariable(REQUEST_VAR_MAP_NAME, null);
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
            request.setAttribute(REQUEST_VAR_MAP_NAME, requestVarMap);
        }
        Map<String, Object> globalContext = FtlTransformUtil.getGlobalContext(context, env);
        if (globalContext != null) {
            globalContext.put(REQUEST_VAR_MAP_NAME, requestVarMap);
        }
        if (env != null) {
            // FIXME?: this doesn't share the map with the above. currently makes no real difference
            // because resetRequestVars usually called with env null, and fallback to ftl globals should be rare anyway.
            // possible could change SimpleHash into SimpleMapModel (around requestVarMap)...
            env.setGlobalVariable(REQUEST_VAR_MAP_NAME, new SimpleHash(env.getObjectWrapper()));
        }
    }    
    
    @SuppressWarnings("unchecked")
    private static Map<String, Object> getRequestVarMapFromAttribs(HttpServletRequest request) {
        Map<String, Object> map = (Map<String, Object>) request.getAttribute(REQUEST_VAR_MAP_NAME);
        if (map == null) {
            map = new HashMap<String, Object>();
            request.setAttribute(REQUEST_VAR_MAP_NAME, map);
        }
        return map;
    }
    
    @SuppressWarnings("unchecked")
    private static Map<String, Object> getRequestVarMapFromMap(Map<String, Object> parentMap) {
        Map<String, Object> map = (Map<String, Object>) parentMap.get(REQUEST_VAR_MAP_NAME);
        if (map == null) {
            map = new HashMap<String, Object>();
            parentMap.put(REQUEST_VAR_MAP_NAME, map);
        }
        return map;
    }
    
    @SuppressWarnings("unchecked")
    private static SimpleHash getRequestVarMapFromFtlGlobals(Environment env) {
        // WARN: we violate Freemarker immutability logic by changing SimpleHash after initial creation,
        // but it doesn't really matter since no template should ever read it.
        SimpleHash map = null;
        try {
            map = (SimpleHash) env.getGlobalVariable(REQUEST_VAR_MAP_NAME);
        } catch (TemplateModelException e) {
            Debug.logError(e, "Cato: Error getting request var map from FTL globals", module);
        }
        if (map == null) {
            map = new SimpleHash(env.getObjectWrapper());
            env.setGlobalVariable(REQUEST_VAR_MAP_NAME, map);
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
    public static void setRequestVar(String name, Object value, Boolean unwrap, HttpServletRequest request, 
            Map<String, Object> context, Environment env) throws TemplateModelException {
        if (request != null) {
            if (unwrap == Boolean.TRUE) {
                getRequestVarMapFromAttribs(request).put(name, FtlTransformUtil.unwrapPermissive(value));
            }
            else {
                // by default, don't bother unwrapping anymore (no point since have containing map)
                getRequestVarMapFromAttribs(request).put(name, value);
            }
            //Debug.logInfo("setRequestVar: request attrib (name: " + name + ")", module);
        }
        else {
            Map<String, Object> globalContext = FtlTransformUtil.getGlobalContext(context, env);
            if (globalContext != null) {
                if (unwrap == Boolean.TRUE) {
                    getRequestVarMapFromMap(globalContext).put(name, FtlTransformUtil.unwrapPermissive(value));
                }
                else {
                    // by default, don't bother unwrapping anymore (no point since have containing map)
                    getRequestVarMapFromMap(globalContext).put(name, value);
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
        HttpServletRequest request = FtlTransformUtil.getRequest(env);
        Map<String, Object> context = null;
        if (request == null) { // optimization: don't need to look this up if has request (true in most cases now)
            context = FtlTransformUtil.getContext(env);
        }
        setRequestVar(name, value, unwrap, request, context, env);
    }
    
    public static void setRequestVar(String name, Object value, Environment env) throws TemplateModelException {
        HttpServletRequest request = FtlTransformUtil.getRequest(env);
        Map<String, Object> context = null;
        if (request == null) { // optimization: don't need to look this up if has request (true in most cases now)
            context = FtlTransformUtil.getContext(env);
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
     * Must and should only be used to read values set by {@link #setRequestVar}.
     * <p>
     * Return value may or may not be a <code>TemplateModel</code>; caller must wrap or unwrap as needed.
     * Can use {@link com.ilscipio.cato.webapp.ftl.FtlTransformUtil} <code>unwrapXxx</code> methods.
     * 
     * @see #setRequestVar
     */
    public static Object getRequestVar(String name, HttpServletRequest request, 
            Map<String, Object> context, Environment env) throws TemplateModelException {
        Object res = null;

        if (request != null) {
            res = getRequestVarMapFromAttribs(request).get(name);
            //Debug.logInfo("getRequestVar: request attrib (name: " + name + ")", module);
        }
        else {
            Map<String, Object> globalContext = FtlTransformUtil.getGlobalContext(context, env);
            if (globalContext != null) {    
                res = getRequestVarMapFromMap(globalContext).get(name);
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
        HttpServletRequest request = FtlTransformUtil.getRequest(env);
        Map<String, Object> context = null;
        if (request == null) {
            context = FtlTransformUtil.getContext(env);
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
     * @see #setRequestVar
     */
    public static void pushRequestStack(String name, Object value, HttpServletRequest request, 
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
            Map<String, Object> requestVarMap = getRequestVarMapFromAttribs(request);
            
            List<Object> stack;
            Object stackObj = requestVarMap.get(name);
            if (stackObj instanceof List) {
                stack = UtilGenerics.checkList(stackObj);
            }
            else {
                if (stackObj != null) {
                    Debug.logWarning("Overriding request attribute with new stack (name: " + name + ")", module);
                }
                stack = new ArrayList<Object>(REQUEST_STACK_INITIAL_CAPACITY);
            }
            
            stack.add(value);

            requestVarMap.put(name, stack);
            //Debug.logInfo("pushRequestStack: request attrib (name: " + name + ")", module);
        }
        else {
            Map<String, Object> globalContext = FtlTransformUtil.getGlobalContext(context, env);
            if (globalContext != null) {   
                Map<String, Object> requestVarMap = getRequestVarMapFromMap(globalContext);
                
                List<Object> stack;
                Object stackObj = requestVarMap.get(name);
                if (stackObj instanceof List) {
                    stack = UtilGenerics.checkList(stackObj);
                }
                else {
                    if (stackObj != null) {
                        Debug.logWarning("Overriding globalContext var with new stack (name: " + name + ")", module);
                    }
                    stack = new ArrayList<Object>(REQUEST_STACK_INITIAL_CAPACITY);
                }
                
                stack.add(value);
                
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
                    stack = new SimpleSequence(REQUEST_STACK_INITIAL_CAPACITY, env.getObjectWrapper());
                }
                
                // WARN: this sort of violates freemarker language by modifying list in-place after initial construction,
                // but no one should ever be accessing this list directly anyway apart from these methods
                stack.add(value);
                
                requestVarMap.put(name, stack);
                //Debug.logInfo("pushRequestStack: ftl global var (name: " + name + ")", module);
            }
            else {
                throw new IllegalArgumentException("No request, context or ftl environment to push request scope stack (name: " + name + ")");
            }
        }
    }
    
    public static void pushRequestStack(String name, Object value, Environment env) throws TemplateModelException {
        HttpServletRequest request = FtlTransformUtil.getRequest(env);
        Map<String, Object> context = null;
        if (request == null) {
            context = FtlTransformUtil.getContext(env);
        }
        pushRequestStack(name, value, request, context, env);
    }
    
    public static void pushRequestStack(String name, Object value, HttpServletRequest request, Map<String, Object> context) throws TemplateModelException {
        pushRequestStack(name, value, request, context, null);
    }
    
    /**
     * Method providing support for a stack structure having request scope, with fallback to globals.
     * <p>
     * <strong>Do not access underlying structure directly.</strong>
     * <p>
     * Return value may or may not be a <code>TemplateModel</code>; caller must wrap or unwrap as needed.
     * 
     * @see #setRequestVar
     */
    public static Object readRequestStack(String name, HttpServletRequest request, 
            Map<String, Object> context, Environment env) throws TemplateModelException {
        return readRequestStack(name, false, request, context, env);
    }
    
    public static Object readRequestStack(String name, Environment env) throws TemplateModelException {
        HttpServletRequest request = FtlTransformUtil.getRequest(env);
        Map<String, Object> context = null;
        if (request == null) {
            context = FtlTransformUtil.getContext(env);
        }
        return readRequestStack(name, false, request, context, env);
    }
    
    public static Object readRequestStack(String name, HttpServletRequest request, Map<String, Object> context) throws TemplateModelException {
        return readRequestStack(name, false, request, context, null);
    }
    
    static Object readRequestStack(String name, boolean pop, HttpServletRequest request, 
            Map<String, Object> context, Environment env) throws TemplateModelException {
        Object res = null;

        if (request != null) {
            List<Object> stack = null;
            Object stackObj = getRequestVarMapFromAttribs(request).get(name);
            if (stackObj instanceof List) {
                stack = UtilGenerics.checkList(stackObj);
            }
            if (stack != null && !stack.isEmpty()) {
                res = pop ? stack.remove(stack.size() - 1) : stack.get(stack.size() - 1);
                // don't need, just rely on references
                //if (pop) {
                //    request.setAttribute(name, stack); // for correctness
                //}
            }
            else if (pop) {
                Debug.logError("Trying to pop empty request attrib stack (name: " + name + ")", module);
            }
            //Debug.logInfo((pop ? "pop" : "read") + "RequestStack: request attrib (name: " + name + ")", module);
        }
        else {
            Map<String, Object> globalContext = FtlTransformUtil.getGlobalContext(context, env);
            if (globalContext != null) {   
                List<Object> stack = null;
                Object stackObj = getRequestVarMapFromMap(globalContext).get(name);
                if (stackObj instanceof List) {
                    stack = UtilGenerics.checkList(stackObj);
                }
                if (stack != null && !stack.isEmpty()) {
                    res = pop ? stack.remove(stack.size() - 1) : stack.get(stack.size() - 1);
                    //if (pop) {
                    //    globalContext.put(name, stack); // for correctness
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
                            SimpleSequence newStack = new SimpleSequence(REQUEST_STACK_INITIAL_CAPACITY, env.getObjectWrapper());
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
        HttpServletRequest request = FtlTransformUtil.getRequest(env);
        Map<String, Object> context = null;
        if (request == null) {
            context = FtlTransformUtil.getContext(env);
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
     * @see #setRequestVar
     */ 
    public static Object popRequestStack(String name, HttpServletRequest request, 
            Map<String, Object> context, Environment env) throws TemplateModelException {
        return readRequestStack(name, true, request, context, env);
    }
    
    public static Object popRequestStack(String name, Environment env) throws TemplateModelException {
        HttpServletRequest request = FtlTransformUtil.getRequest(env);
        Map<String, Object> context = null;
        if (request == null) {
            context = FtlTransformUtil.getContext(env);
        }
        return readRequestStack(name, true, request, context, env);
    }
    
    public static Object popRequestStack(String name, HttpServletRequest request, Map<String, Object> context) throws TemplateModelException {
        return readRequestStack(name, true, request, context, null);
    }
    
    public static <K, V> Map<K, V> concatMaps(Map<? extends K, ? extends V> first, Map<? extends K, ? extends V> second) {
        Map<K, V> res = new LinkedHashMap<K, V>();
        if (first != null) {
            res.putAll(first);
        }
        if (second != null) {
            res.putAll(second);
        }
        return res;
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
    public static Object getRequestStackAsList(String name, HttpServletRequest request, 
            Map<String, Object> context, Environment env, TemplateValueTargetType copyTargetType) throws TemplateModelException {
        if (request != null) {
            List<Object> stack = null;
            Object stackObj = getRequestVarMapFromAttribs(request).get(name);
            if (stackObj instanceof List) {
                stack = UtilGenerics.checkList(stackObj);
            }
            if (stack != null) {
                if (copyTargetType == null) {
                    return Collections.unmodifiableList(stack);
                }
                else {
                    return copyList(stack, copyTargetType, env != null ? env.getObjectWrapper() : null);
                }
            }
            else {
                return null;
            }
        }
        else {
            Map<String, Object> globalContext = FtlTransformUtil.getGlobalContext(context, env);
            if (globalContext != null) {   
                List<Object> stack = null;
                Object stackObj = getRequestVarMapFromMap(globalContext).get(name);
                if (stackObj instanceof List) {
                    stack = UtilGenerics.checkList(stackObj);
                }
                if (stack != null) {
                    if (copyTargetType == null) {
                        return Collections.unmodifiableList(stack);
                    }
                    else {
                        return copyList(stack, copyTargetType, env != null ? env.getObjectWrapper() : null);
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
                        return copyList(stack, copyTargetType, env != null ? env.getObjectWrapper() : null);
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
        HttpServletRequest request = FtlTransformUtil.getRequest(env);
        Map<String, Object> context = null;
        if (request == null) {
            context = FtlTransformUtil.getContext(env);
        }
        return getRequestStackAsList(name, request, context, env, TemplateValueTargetType.SIMPLEMODEL);
    }
    
    /**
     * Returns copy of request stack as a List (elements may still be TemplateModels).
     */
    public static Object getRequestStackAsList(String name, HttpServletRequest request, Map<String, Object> context) throws TemplateModelException {
        return getRequestStackAsList(name, request, context, null, TemplateValueTargetType.RAW);
    }
    
    /**
     * Checks if the given model matches the logical FTL object type.
     * 
     * @see com.ilscipio.cato.webapp.ftl.OfbizFtlObjectType
     */
    public static boolean isObjectType(String ftlTypeName, TemplateModel object) {
        return OfbizFtlObjectType.isObjectTypeSafe(ftlTypeName, object);
    }
    
    public static Object getMapKeys(TemplateModel object) throws TemplateModelException {
        if (OfbizFtlObjectType.COMPLEXMAP.isObjectType(object)) {
            // would be safer to let the wrapper do it, but we know it's just a BeanModel in Ofbiz so we can optimize.
            Map<Object, Object> wrappedObject = UtilGenerics.cast(((WrapperTemplateModel) object).getWrappedObject());
            return wrappedObject.keySet();
        }
        else if (object instanceof TemplateHashModelEx) {
            return ((TemplateHashModelEx) object).keys();
        }
        else {
            throw new TemplateModelException("object is not a map or does not support key iteration");
        }
    }
    
    /**
     * Shallow-copies map or list. Note: won't preserve order for maps.
     * 
     * @param object
     * @param toSimpleType if true, converts to simple FTL type instead of beans, where possible
     * @return
     * @throws TemplateModelException
     */
    public static Object copyObject(TemplateModel object, TemplateValueTargetType targetType, ObjectWrapper objectWrapper) throws TemplateModelException {
        if (targetType == null) {
            targetType = TemplateValueTargetType.PRESERVE;
        }
        if (OfbizFtlObjectType.COMPLEXMAP.isObjectType(object)) {
            // would be safer to let the wrapper do it, but we know it's just a BeanModel in Ofbiz so we can optimize.
            Map<Object, Object> wrappedObject = UtilGenerics.cast(((WrapperTemplateModel) object).getWrappedObject());
            if (targetType == TemplateValueTargetType.SIMPLEMODEL) {
                return new SimpleHash(wrappedObject, objectWrapper);
            }
            else {
                return new HashMap<Object, Object>(wrappedObject);
            }
        }
        else if (object instanceof TemplateHashModelEx && OfbizFtlObjectType.MAP.isObjectType(object)) {
            TemplateHashModelEx hashModel = (TemplateHashModelEx) object;
            SimpleHash res = new SimpleHash(objectWrapper);
            TemplateCollectionModel modelColl = hashModel.keys();
            TemplateModelIterator modelIt = modelColl.iterator();
            while(modelIt.hasNext()) {
                String key = ((TemplateScalarModel) modelIt.next()).getAsString();
                res.put(key, hashModel.get(key));
            }
            return res;
        }
        else if (object instanceof TemplateCollectionModel || object instanceof TemplateSequenceModel) {
            return copyList(object, targetType, objectWrapper);
        }
        else {
            throw new TemplateModelException("object is not cloneable");
        }
    }
    
    /**
     * Copies a list to a target model/raw list type. In general does not wrap/unwrap individual values.
     */
    public static Object copyList(Object object, TemplateValueTargetType targetType, ObjectWrapper objectWrapper) throws TemplateModelException {
        if (targetType == null) {
            targetType = TemplateValueTargetType.PRESERVE;
        }
        if (object instanceof Iterable) {
            return copyList(UtilGenerics.<Iterable<Object>>cast(object), targetType, objectWrapper);
        }
        else if (object instanceof TemplateModel) {
            return copyList((TemplateModel) object, targetType, objectWrapper);
        }
        throw new TemplateModelException("Cannot copy list of type " + object.getClass().toString() + 
            " to target type: " + targetType.toString());
    }
    
    public static Object copyList(Iterable<Object> object, TemplateValueTargetType targetType, ObjectWrapper objectWrapper) throws TemplateModelException {
        if (targetType == null) {
            targetType = TemplateValueTargetType.PRESERVE;
        }
        if (object instanceof Collection) {
            Collection<Object> collection = UtilGenerics.<Collection<Object>>cast(object);
            if (targetType == TemplateValueTargetType.PRESERVE || targetType == TemplateValueTargetType.RAW) {
                return new ArrayList<Object>(collection);
            }
            else if (targetType == TemplateValueTargetType.MODEL || targetType == TemplateValueTargetType.SIMPLEMODEL) {
                return new SimpleSequence(collection, objectWrapper);
            }
            else if (targetType == TemplateValueTargetType.COMPLEXMODEL) {
                return objectWrapper.wrap(new ArrayList<Object>(collection));
            }
        }
        else {
            Iterable<Object> iterable = UtilGenerics.<Iterable<Object>>cast(object);
            if (targetType == TemplateValueTargetType.PRESERVE || targetType == TemplateValueTargetType.RAW) {
                List<Object> res = new ArrayList<Object>();
                for(Object val : iterable) {
                    res.add(val);
                }
                return res;
            }
            else if (targetType == TemplateValueTargetType.MODEL || targetType == TemplateValueTargetType.SIMPLEMODEL) {
                SimpleSequence res = new SimpleSequence(objectWrapper);
                for(Object val : iterable) {
                    res.add(val);
                }
                return res;
            }
            else if (targetType == TemplateValueTargetType.COMPLEXMODEL) {
                List<Object> res = new ArrayList<Object>();
                for(Object val : iterable) {
                    res.add(val);
                }
                return objectWrapper.wrap(res);
            } 
        }
        throw new TemplateModelException("Cannot copy list of type " + object.getClass().toString() + 
                " to target type: " + targetType.toString());
    }
    
    public static Object copyList(TemplateModel object, TemplateValueTargetType targetType, ObjectWrapper objectWrapper) throws TemplateModelException {
        if (targetType == null) {
            targetType = TemplateValueTargetType.PRESERVE;
        }
        if (object instanceof TemplateCollectionModel) { // TODO: isObjectType
            TemplateCollectionModel collectionModel = (TemplateCollectionModel) object;
            if (targetType == TemplateValueTargetType.RAW) {
                List<Object> res = new ArrayList<Object>();
                TemplateModelIterator it = collectionModel.iterator();
                while(it.hasNext()) {
                    res.add(it.next());
                }
                return res;
            }
            else if (targetType == TemplateValueTargetType.MODEL || targetType == TemplateValueTargetType.SIMPLEMODEL || targetType == TemplateValueTargetType.PRESERVE) {
                return new SimpleSequence(collectionModel);
            }
            else if (targetType == TemplateValueTargetType.COMPLEXMODEL) {
                List<Object> res = new ArrayList<Object>();
                TemplateModelIterator it = collectionModel.iterator();
                while(it.hasNext()) {
                    res.add(it.next());
                }
                return objectWrapper.wrap(res);
            } 
        }
        else if (object instanceof TemplateSequenceModel) { // TODO: isObjectType
            TemplateSequenceModel seqModel = (TemplateSequenceModel) object;
            if (targetType == TemplateValueTargetType.RAW) {
                List<Object> res = new ArrayList<Object>();
                for(int i=0; i < seqModel.size(); i++) {
                    res.add(seqModel.get(i));
                }
                return res;
            }
            else if (targetType == TemplateValueTargetType.MODEL || targetType == TemplateValueTargetType.SIMPLEMODEL || targetType == TemplateValueTargetType.PRESERVE) {
                SimpleSequence res = new SimpleSequence(seqModel.size(), objectWrapper);
                for(int i=0; i < seqModel.size(); i++) {
                    res.add(seqModel.get(i));
                }
                return res;
            }
            else if (targetType == TemplateValueTargetType.COMPLEXMODEL) {
                List<Object> res = new ArrayList<Object>();
                for(int i=0; i < seqModel.size(); i++) {
                    res.add(seqModel.get(i));
                }
                return objectWrapper.wrap(res);
            } 
        }
        else if (object instanceof WrapperTemplateModel) {
            Object wrappedObj = ((WrapperTemplateModel) object).getWrappedObject();
            if (wrappedObj instanceof Collection) {
                Collection<Object> collection = UtilGenerics.<Collection<Object>>cast(object);
                if (targetType == TemplateValueTargetType.RAW) {
                    return new ArrayList<Object>(collection);
                }
                else if (targetType == TemplateValueTargetType.MODEL || targetType == TemplateValueTargetType.SIMPLEMODEL) {
                    return new SimpleSequence(collection, objectWrapper);
                }
                else if (targetType == TemplateValueTargetType.PRESERVE || targetType == TemplateValueTargetType.COMPLEXMODEL) {
                    return objectWrapper.wrap(new ArrayList<Object>(collection));
                }
            }
            else if (wrappedObj instanceof Iterable) {
                Iterable<Object> iterable = UtilGenerics.<Iterable<Object>>cast(object);
                if (targetType == TemplateValueTargetType.RAW) {
                    List<Object> res = new ArrayList<Object>();
                    for(Object val : iterable) {
                        res.add(val);
                    }
                    return res;
                }
                else if (targetType == TemplateValueTargetType.MODEL || targetType == TemplateValueTargetType.SIMPLEMODEL) {
                    SimpleSequence res = new SimpleSequence(objectWrapper);
                    for(Object val : iterable) {
                        res.add(val);
                    }
                    return res;
                }
                else if (targetType == TemplateValueTargetType.PRESERVE || targetType == TemplateValueTargetType.COMPLEXMODEL) {
                    List<Object> res = new ArrayList<Object>();
                    for(Object val : iterable) {
                        res.add(val);
                    }
                    return objectWrapper.wrap(res);
                }
            }
        }
        throw new TemplateModelException("Cannot copy list of type " + object.getClass().toString() + 
                " to target type: " + targetType.toString());
    }
    
    
    public static TemplateHashModel toSimpleMap(ObjectWrapper objectWrapper, TemplateModel object) throws TemplateModelException {
        if (OfbizFtlObjectType.COMPLEXMAP.isObjectType(object)) {
            // would be safer to let the wrapper do it, but we know it's just a BeanModel in Ofbiz so we can optimize.
            Map<Object, Object> wrappedObject = UtilGenerics.cast(((WrapperTemplateModel) object).getWrappedObject());
            return new SimpleMapModel(wrappedObject, (BeansWrapper) objectWrapper);
        }
        else if (object instanceof TemplateHashModel) {
            return (TemplateHashModel) object;
        }
        else {
            throw new TemplateModelException("object is not a recognized map type");
        }
    }
    
    /**
     * Same as Freemarker's ?is_directive.
     */
    public static boolean isDirective(Object object) {
        return (object instanceof TemplateTransformModel || object instanceof Macro || object instanceof TemplateDirectiveModel);
    }
    
    public static Map<String, Object> extractPrefixedStyleNamesWithInt(String styleStr, Map<String, String> prefixMap) {
        Map<String, Object> res = new HashMap<String, Object>();        
        Set<String> prefixes = prefixMap.keySet();
        Matcher m = Pattern.compile("(^|\\s)(" + StringUtils.join(prefixes, "|") + ")(\\d+)(\\s|$)").matcher(styleStr);
        while (m.find()) {
            String prefix = m.group(2);
            Integer val = Integer.parseInt(m.group(3));
            res.put(prefixMap.get(prefix), val);
        }
        return res;
    }
    
}
