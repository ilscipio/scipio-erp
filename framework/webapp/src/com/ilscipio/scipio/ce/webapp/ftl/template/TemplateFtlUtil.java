package com.ilscipio.scipio.ce.webapp.ftl.template;

import java.io.IOException;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.apache.commons.lang.StringEscapeUtils;
import org.apache.commons.lang.StringUtils;
import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.UtilGenerics;
import org.ofbiz.base.util.UtilHttp;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.base.util.cache.UtilCache;
import org.ofbiz.base.util.template.FreeMarkerWorker;
import org.ofbiz.webapp.control.RequestHandler;

import com.ilscipio.scipio.ce.webapp.ftl.CommonFtlUtil;
import com.ilscipio.scipio.ce.webapp.ftl.context.ContextFtlUtil;
import com.ilscipio.scipio.ce.webapp.ftl.lang.LangFtlUtil;

import freemarker.core.Environment;
import freemarker.ext.beans.BooleanModel;
import freemarker.template.SimpleScalar;
import freemarker.template.Template;
import freemarker.template.TemplateBooleanModel;
import freemarker.template.TemplateException;
import freemarker.template.TemplateModel;
import freemarker.template.TemplateModelException;
import freemarker.template.TemplateScalarModel;
import javolution.util.FastMap;

/**
 * SCIPIO: Theme- and styling-framework-agnostic templating utilities.
 * <p>
 * Any theme- or styling-framework-specific code should be placed in a separate package.
 * 
 * @see com.ilscipio.scipio.ce.webapp.ftl.CommonFtlUtil
 */
public abstract class TemplateFtlUtil {

    public static final String module = TemplateFtlUtil.class.getName();
    
    private static final UtilCache<String, Map<String, Object>> headingElemSpecFromStyleStrCache = 
            UtilCache.createUtilCache("com.ilscipio.scipio.ce.webapp.ftl.template.TemplateFtlUtil.headingElemSpecFromStyleStrCache");

    private static volatile Template escapePart2ArgFunctionCall = null;
    private static volatile Template escapePart3ArgFunctionCall = null;
    private static volatile Template escapeFullUrl2ArgFunctionCall = null;
    private static volatile Template escapeFullUrl3ArgFunctionCall = null;
    
    /**
     * Keep this private, implied unmodifiable.
     */
    private static final Set<String> emptyStrSet = new HashSet<String>();

    protected TemplateFtlUtil() {
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
            res = TemplateFtlUtil.getHeadingElemSpecFromStyleStr(styleStr, containerStyleStr, allowedHeadingElemTypes, allowedElemTypes, allowedContainerElemTypes);
            
            if (cacheKey != null) {
                // note: probably no need to synchronize on cache; duplicate insertion is ok
                headingElemSpecFromStyleStrCache.put(cacheKey, res);
            }
        }
        
        return res;
    }

    /**
     * Parses a complex style string meant to describe an element notably heading into hash of constituent values.
     * <p>
     * Core implementation; never caches.
     */
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
            titleArgs = TemplateFtlUtil.splitStrParams(titleArgsStr, ",");
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
     * noValToken is similar but prevents values altogether.
     * <p>
     * NOTE (2016-08-30):  emptyValToken and noValToken should be avoided in favor of {@link com.ilscipio.scipio.ce.webapp.ftl.template.AttribSpecialValue}.
     * @throws IOException 
     * @throws TemplateException 
     */
    public static String makeElemAttribStr(Map<String, Object> attribs, boolean includeEmpty, String emptyValToken, String noValToken,
            Collection<String> exclude, String attribNamePrefix, boolean alwaysAddPrefix,
            String attribNamePrefixStrip, Map<String, String> attribNameSubstitutes, boolean camelCaseToDashLowerNames, String escapeLang) throws TemplateModelException {
        StringBuilder sb = new StringBuilder();
        
        Environment env = null;
        if (escapeLang != null && !escapeLang.isEmpty() && !"none".equals(escapeLang)) {
            env = FreeMarkerWorker.getCurrentEnvironment();
        } else {
            escapeLang = null;
        }
        
        if (emptyValToken == null) {
            emptyValToken = "";
        }
        
        if (noValToken != null && noValToken.isEmpty()) {
            noValToken = null;
        }
        
        if (exclude == null || exclude.isEmpty()) {
            exclude = emptyStrSet;
        }
        else if (!(exclude instanceof Set)) {
            exclude = new HashSet<String>(exclude); // faster
        }
        
        if (attribNameSubstitutes != null && attribNameSubstitutes.isEmpty()) {
            attribNameSubstitutes = null;
        }
        if (attribNamePrefix != null && attribNamePrefix.isEmpty()) {
            attribNamePrefix = null;
        }
        if (attribNamePrefixStrip != null && attribNamePrefixStrip.isEmpty()) {
            attribNamePrefixStrip = null;
        }
        
        for(Map.Entry<String, Object> pair : attribs.entrySet()) {
            String name = pair.getKey();
            
            if (attribNamePrefix != null) {
                if (alwaysAddPrefix || !name.startsWith(attribNamePrefix)) {
                    name = attribNamePrefix + name;
                }
            }
            if (attribNamePrefixStrip != null) {
                if (name.startsWith(attribNamePrefixStrip)) {
                    name = name.substring(attribNamePrefixStrip.length());
                }
            }
            if (attribNameSubstitutes != null) {
                String newName = attribNameSubstitutes.get(name);
                if (newName != null) {
                    name = newName;
                }
            }
    
            if (!exclude.contains(name)) {
                Object val = pair.getValue();
                String valStr = (val != null) ? val.toString() : "";
                if (valStr == null) {
                    valStr = ""; // just in case
                }
                
                if (AttribSpecialValue.isSpecialValue(val) || includeEmpty || !valStr.isEmpty()) {
                    sb.append(" ");
                    if (camelCaseToDashLowerNames) {
                        name = LangFtlUtil.camelCaseToDashLowerName(name);
                    }
                    sb.append(name);
                    if (!AttribSpecialValue.isNoneValue(val) && (noValToken == null || !noValToken.equals(valStr))) {
                        sb.append("=\"");
                        if (!valStr.equals(emptyValToken)) {
                            if (escapeLang != null && env != null && !RawScript.isRawScript(val)) {
                                valStr = execEscapePartFunction(new SimpleScalar(valStr), new SimpleScalar(escapeLang), env).getAsString();
                            }
                            sb.append(valStr);
                        }
                        sb.append("\"");
                    }
                }
            } 
        }
        
        return sb.toString();
    }

    public static Map<String, Object> extractPrefixedStyleNamesWithInt(String styleStr, Map<String, String> prefixMap) {
        Map<String, Object> res = new HashMap<String, Object>();        
        Set<String> prefixes = prefixMap.keySet();
        Matcher m = Pattern.compile("(^|\\s)(" + StringUtils.join(prefixes, "|") + ")(\\d+)(?=\\s|$)").matcher(styleStr);
        while (m.find()) {
            String prefix = m.group(2);
            Integer val = Integer.parseInt(m.group(3));
            res.put(prefixMap.get(prefix), val);
        }
        return res;
    }

    /**
     * Compiles a progress success action.
     * <p>
     * Currently the link handling is similar to org.ofbiz.widget.WidgetWorker.buildHyperlinkUrl.
     */
    public static String compileProgressSuccessAction(String progressSuccessAction, HttpServletRequest request, 
            HttpServletResponse response) {
        if (progressSuccessAction.startsWith("redirect;")) {
            String newAction = "none";
            String[] parts = progressSuccessAction.split(";", 3);
            if (parts.length == 3) {
                Boolean fullPath = null;
                Boolean secure = null;
                Boolean encode = null;
                String[] options = parts[1].split("\\s*,\\s*");
                for(String pair : options) {
                    if (pair.length() > 0) {
                        String[] elems = pair.split("\\s*=\\s*", 2);
                        if (elems.length == 2) {
                            Boolean val = null;
                            if ("true".equals(elems[1])) {
                                val = true;
                            }
                            else if ("false".equals(elems[1])) {
                                val = false;
                            }
                            if (val != null) {
                                if ("fullPath".equalsIgnoreCase(elems[0])) {
                                    fullPath = val;
                                }
                                else if ("secure".equalsIgnoreCase(elems[0])) {
                                    secure = val;
                                }
                                else if ("encode".equalsIgnoreCase(elems[0])) {
                                    encode = val;
                                }
                                else {
                                    Debug.logError("Scipio: progress success action value has invalid option name: [" + pair + "] in " + progressSuccessAction, module);
                                }
                            }
                            else {
                                Debug.logError("Scipio: progress success action value has invalid option value: [" + pair + "] in " + progressSuccessAction, module);
                            }
                        }
                        else {
                            Debug.logError("Scipio: progress success action value has invalid option: [" + pair + "] in " + progressSuccessAction, module);
                        }
                    }
                }
                String target = parts[2].replaceAll("&", "&amp;");
                StringBuilder sb = new StringBuilder();
                sb.append("redirect;");
                // we don't have access to WidgetWorker, but don't need anyway
                //try {
                //    WidgetWorker.buildHyperlinkUrl(sb, target, "intra-app", null, null, fullPath, secure, encode, request, response, context);
                //} catch (IOException e) {
                //    Debug.logError("Scipio: progress success action value has invalid format in " + progressSuccessAction, module);
                //}
                String localRequestName = StringEscapeUtils.unescapeHtml(target);
                localRequestName = UtilHttp.encodeAmpersands(localRequestName);
                if (request != null && response != null) {
                    // FIXME: this does not support inter-webapp links or any new parameters
                    sb.append(RequestHandler.makeUrl(request, response, localRequestName, fullPath, secure, encode));
                }
                else {
                    sb.append(localRequestName);
                }
                newAction = sb.toString();
            }
            else {
                Debug.logError("Scipio: progress success action value has invalid format in " + progressSuccessAction, module);
            }
            progressSuccessAction = newAction;
        }
        return progressSuccessAction;
    }

    /**
     * Compiles a progress success action.
     * <p>
     * Context-dependent; uses thread environment.
     */
    public static String compileProgressSuccessAction(String progressSuccessAction) throws TemplateModelException {
        Environment env = CommonFtlUtil.getCurrentEnvironment();
        return compileProgressSuccessAction(progressSuccessAction, ContextFtlUtil.getRequest(env), ContextFtlUtil.getResponse(env));
    }
    
    public static String getPlainClassArgNames(String style) {
        if (style.startsWith("+") || style.startsWith("=")) {
            return style.substring(1);
        }
        else {
            return style;
        }
    }
    
    public static String getClassArgPrefix(String style) {
        if (style.startsWith("+") || style.startsWith("=")) {
            return style.substring(0, 1);
        }
        else {
            return "";
        }
    }
    
    public static TemplateScalarModel execEscapePartFunction(TemplateModel arg1, TemplateModel arg2, Environment env) throws TemplateModelException {
        if (escapePart2ArgFunctionCall == null) {
            // NOTE: no real need for synchronize here
            escapePart2ArgFunctionCall = LangFtlUtil.getFunctionCall("escapePart", 2, env);
        }
        return (TemplateScalarModel) LangFtlUtil.execFunction(escapePart2ArgFunctionCall, new TemplateModel[] { arg1, arg2}, env);
    }

    public static TemplateScalarModel execEscapePartFunction(TemplateModel arg1, TemplateModel arg2, TemplateModel arg3, Environment env) throws TemplateModelException {
        if (escapePart3ArgFunctionCall == null) {
            // NOTE: no real need for synchronize here
            escapePart3ArgFunctionCall = LangFtlUtil.getFunctionCall("escapePart", 3, env);
        }
        return (TemplateScalarModel) LangFtlUtil.execFunction(escapePart3ArgFunctionCall, new TemplateModel[] { arg1, arg2, arg3}, env);
    }
    
    public static String escapePart(String value, String lang, Environment env) throws TemplateModelException {
        if (value == null || value.isEmpty() || lang == null || lang.isEmpty()) {
            return value;
        }
        return execEscapePartFunction(new SimpleScalar(value), new SimpleScalar(lang), env).getAsString();
    }
    
    public static String escapePart(String value, String lang, Boolean strict, Environment env) throws TemplateModelException {
        if (value == null || value.isEmpty() || lang == null || lang.isEmpty()) {
            return value;
        }
        if (strict != null) {
            return execEscapePartFunction(new SimpleScalar(value), new SimpleScalar(lang), 
                    strict ? TemplateBooleanModel.TRUE : TemplateBooleanModel.FALSE, env).getAsString();
        } else {
            return execEscapePartFunction(new SimpleScalar(value), new SimpleScalar(lang), env).getAsString();
        }
    }
    
    public static TemplateScalarModel execEscapeFullUrlFunction(TemplateModel arg1, TemplateModel arg2, Environment env) throws TemplateModelException {
        if (escapeFullUrl2ArgFunctionCall == null) {
            // NOTE: no real need for synchronize here
            escapeFullUrl2ArgFunctionCall = LangFtlUtil.getFunctionCall("escapeFullUrl", 2, env);
        }
        return (TemplateScalarModel) LangFtlUtil.execFunction(escapeFullUrl2ArgFunctionCall, new TemplateModel[] { arg1, arg2}, env);
    }

    public static TemplateScalarModel execEscapeFullUrlFunction(TemplateModel arg1, TemplateModel arg2, TemplateModel arg3, Environment env) throws TemplateModelException {
        if (escapeFullUrl3ArgFunctionCall == null) {
            // NOTE: no real need for synchronize here
            escapeFullUrl3ArgFunctionCall = LangFtlUtil.getFunctionCall("escapeFullUrl", 3, env);
        }
        return (TemplateScalarModel) LangFtlUtil.execFunction(escapeFullUrl3ArgFunctionCall, new TemplateModel[] { arg1, arg2, arg3}, env);
    }
    
    public static String escapeFullUrl(String value, String lang, Environment env) throws TemplateModelException {
        if (value == null || value.isEmpty() || lang == null || lang.isEmpty()) {
            return value;
        }
        return execEscapeFullUrlFunction(new SimpleScalar(value), new SimpleScalar(lang), env).getAsString();
    }
    
    public static String escapeFullUrl(String value, String lang, Boolean strict, Environment env) throws TemplateModelException {
        if (value == null || value.isEmpty() || lang == null || lang.isEmpty()) {
            return value;
        }
        if (strict != null) {
            return execEscapeFullUrlFunction(new SimpleScalar(value), new SimpleScalar(lang), 
                    strict ? TemplateBooleanModel.TRUE : TemplateBooleanModel.FALSE, env).getAsString();
        } else {
            return execEscapeFullUrlFunction(new SimpleScalar(value), new SimpleScalar(lang), env).getAsString();
        }
    }

}
