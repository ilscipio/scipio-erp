package org.ofbiz.webapp.renderer;

import java.io.IOException;
import java.io.StringWriter;
import java.util.HashMap;
import java.util.LinkedHashSet;
import java.util.Map;
import java.util.Set;

import javax.servlet.http.HttpServletRequest;

import org.apache.commons.lang.RandomStringUtils;
import org.apache.commons.lang.StringUtils;
import org.ofbiz.base.util.Debug;
import org.ofbiz.webapp.renderer.RenderTargetExpr.MultiRenderTargetExpr;
import org.ofbiz.webapp.renderer.RenderWriter.SwitchRenderWriter;

/**
 * Targeted rendering util.
 * <p>
 * FIXME?: The main RenderTargetExpr class is currently under widget package, so we can't
 * access it from here.
 */
public abstract class RenderTargetUtil {

    public static final String module = RenderTargetUtil.class.getName();
    
    public static final String RENDERTARGETEXPR_PARAMNAME = "scpRenderTargetExpr";
    public static final String ERRORRENDERTARGETEXPR_PARAMNAME = "scpErrorRenderTargetExpr";
    public static final String LOGINRENDERTARGETEXPR_PARAMNAME = "scpLoginRenderTargetExpr";

    public static final String RENDERTARGETSTATE_ATTR = "scpRenderTargetState";
    public static final String RENDERTARGETON_ATTR = "scpRenderTargetOn";

    public static final boolean DEBUG = false || Debug.verboseOn();
    
    /**
     * for an expr in the format:
     * "+multi:somename:$My-Section,
     */
    public static final String RENDERTARGETEXPR_RAW_MULTI_PREFIX = "+multi:";
    public static final String RENDERTARGETEXPR_RAW_EMPTY = "";
    
    public static final String RENDERTARGETEXPR_MULTI_DEFAULT = "_default_";

    /**
     * SPECIAL DELIMITER attribute name for non-optimized multi targeting support.
     * It consists of: 
     * prefix + random chars + name + name-terminator + open-/close-flag
     * The random chars are to prevent clashes with page content; this could theoretically
     * still happen but the odds are statistically extremely small. Nevertheless this remains
     * the most reliable way to extract sections using the current ofbiz code.
     */
    public static final String MULTITARGET_DELIM_PREFIX_ATTR = "scpRenderTargetDelim";
    public static final String MULTITARGET_DELIM_PREFIX = "!%#@%#@!@#";
    public static final int MULTITARGET_DELIM_RANDOM_SIZE = 8;
    public static final String MULTITARGET_DELIM_NAMESUFFIX = ";";
    public static final char MULTITARGET_DELIM_OPEN_CHAR = '^';
    public static final String MULTITARGET_DELIM_OPEN = String.valueOf(MULTITARGET_DELIM_OPEN_CHAR);
    public static final char MULTITARGET_DELIM_CLOSE_CHAR = '$';
    public static final String MULTITARGET_DELIM_CLOSE = String.valueOf(MULTITARGET_DELIM_CLOSE_CHAR);

    protected RenderTargetUtil() {
    }

    public static Object getRawRenderTargetExpr(HttpServletRequest request, String paramName) {
        Object scpRenderTargetExpr = request.getAttribute(paramName);
        if (scpRenderTargetExpr != null) return scpRenderTargetExpr;
        else {
            String strExpr = request.getParameter(paramName);
            if (strExpr != null) {
                if (strExpr.startsWith(RENDERTARGETEXPR_RAW_MULTI_PREFIX)) {
                    Map<String, Object> exprMap = splitMultiExprToMap(strExpr.substring(RENDERTARGETEXPR_RAW_MULTI_PREFIX.length()));
                    return exprMap.isEmpty() ? RENDERTARGETEXPR_RAW_EMPTY : exprMap;
                } else {
                    return strExpr;
                }
            }
        }
        return null;
    }
    
    public static Map<String, Object> splitMultiExprToMap(String expr) {
        Map<String, Object> exprMap = new HashMap<>();
        String[] pairs = StringUtils.split(expr, ",");
        for(String pair : pairs) {
            String[] parts = StringUtils.split(pair, ":", 2);
            if (parts.length != 2) throw new IllegalArgumentException("targeted rendering multi expression is invalid: " + expr);
            String name = parts[0].trim();
            String strExpr = parts[1].trim();
            if (name.isEmpty() || strExpr.isEmpty()) throw new IllegalArgumentException("targeted rendering multi expression is invalid: " + expr);
            exprMap.put(name, strExpr);
        }
        return exprMap;
    }
    
    public static Object getRawRenderTargetExpr(HttpServletRequest request) {
        return getRawRenderTargetExpr(request, RENDERTARGETEXPR_PARAMNAME);
    }
    
    public static void setRawRenderTargetExpr(HttpServletRequest request, String paramName, Object expr) {
        request.setAttribute(paramName, expr);
    }
    
    public static void setRawRenderTargetExpr(HttpServletRequest request, Object expr) {
        setRawRenderTargetExpr(request, RENDERTARGETEXPR_PARAMNAME, expr);
    }
    
    /**
     * This caches the result of the first get so next fetch doesn't re-preprocess it.
     */
    public static Object getSetRawRenderTargetExpr(HttpServletRequest request) {
        Object expr = getRawRenderTargetExpr(request);
        setRawRenderTargetExpr(request, expr != null ? expr : RENDERTARGETEXPR_RAW_EMPTY);
        return expr;
    }

    /**
     * FIXME?: this is an approximation because we don't have the state yet from where this is called.
     */
    public static boolean isRenderTargetExprOn(HttpServletRequest request, Object expr) {
        return (expr != null) && !(expr instanceof String && ((String) expr).isEmpty());
    }
    
    public static boolean isRenderTargetExprMulti(Object expr) {
        return (expr instanceof MultiRenderTargetExpr || expr instanceof Map);
    }
    
    public static String generateMultiTargetDelimiterPrefix(HttpServletRequest request) {
        return MULTITARGET_DELIM_PREFIX + RandomStringUtils.randomAlphanumeric(MULTITARGET_DELIM_RANDOM_SIZE);
    }
    
    public static String generateSetMultiTargetDelimiterPrefix(HttpServletRequest request) {
        String prefix = generateMultiTargetDelimiterPrefix(request);
        request.setAttribute(MULTITARGET_DELIM_PREFIX_ATTR, prefix);
        return prefix;
    }
    
    public static String getOrGenerateMultiTargetDelimiterPrefix(HttpServletRequest request) {
        String prefix = (String) request.getAttribute(MULTITARGET_DELIM_PREFIX_ATTR);
        if (prefix == null || prefix.isEmpty()) {
            prefix = generateMultiTargetDelimiterPrefix(request);
        }
        return prefix;
    }
    
    public static String getOrGenerateSetMultiTargetDelimiterPrefix(HttpServletRequest request) {
        String prefix = (String) request.getAttribute(MULTITARGET_DELIM_PREFIX_ATTR);
        if (prefix == null || prefix.isEmpty()) {
            prefix = generateSetMultiTargetDelimiterPrefix(request);
        }
        return prefix;
    }
    
    public static void makeMultiTargetDelimOpen(Appendable out, String name, String prefix) throws IOException {
        out.append(prefix);
        out.append(name);
        out.append(MULTITARGET_DELIM_NAMESUFFIX);
        out.append(MULTITARGET_DELIM_OPEN);
    }
    
    public static void makeMultiTargetDelimClose(Appendable out, String name, String prefix) throws IOException {
        out.append(prefix);
        out.append(name);
        out.append(MULTITARGET_DELIM_NAMESUFFIX);
        out.append(MULTITARGET_DELIM_CLOSE);
    }
    
    public static Map<String, Object> extractMultiTargetOutputs(Appendable out, String prefix) {
        if (out instanceof SwitchRenderWriter) {
            // TODO: NOT IMPLEMENTED: fast mode not yet implemented because it is more likely to fail.
            //Map<String, Object> map = new HashMap<>();
            throw new UnsupportedOperationException("SwitchRenderWriter fast extract not yet implemented");
        } else if (out instanceof StringWriter) {
            return extractMultiTargetOutputsTextual(((StringWriter) out).getBuffer(), prefix);
        } else {
            throw new UnsupportedOperationException();
        }
    }
    
    /**
     * DIRTY TEXT EXTRACTION - slow and memory hog, but only method guaranteed success
     */
    public static Map<String, Object> extractMultiTargetOutputsTextual(StringBuffer sb, String prefix) {
        Map<String, Object> map = new HashMap<>();
        int prefixLength = prefix.length();
        
        //if (DEBUG) {
        //    Debug.logInfo("FULL OUTPUT: " + sb.toString(), module);
        //}

        Set<String> nameStack = new LinkedHashSet<>(); // NOTE: this may have more than one at a time due to nesting
        int delimIndex;
        int lastPostDelimIndex = 0;
        int searchIndex = 0;
        while(searchIndex < sb.length() && ((delimIndex = sb.indexOf(prefix, searchIndex)) >= 0)) {
            final int nameIndex = delimIndex + prefixLength;
            int nameSuffixIndex = sb.indexOf(MULTITARGET_DELIM_NAMESUFFIX, nameIndex);
            if (nameSuffixIndex < 0) {
                Debug.logWarning("invalid targeted rendering output: delimiter is missing name (this is probably an error, but not guaranteed)", module);
                searchIndex = nameIndex;
                continue;
            }
            String name = sb.substring(nameIndex, nameSuffixIndex);
            if (name.isEmpty()) {
                Debug.logWarning("invalid targeted rendering output: delimiter name empty (this is probably an error, but not guaranteed)", module);
                searchIndex = nameIndex;
                continue;
            }
            int lastCharIndex = nameSuffixIndex + MULTITARGET_DELIM_NAMESUFFIX.length();
            if (lastCharIndex >= sb.length()) {
                Debug.logWarning("invalid targeted rendering output: unexpected end of buffer (missing delimiter open/close symbol) (this is probably an error, but not guaranteed)", module);
                searchIndex = nameSuffixIndex;
                continue;
            }
            char openCloseChar = sb.charAt(lastCharIndex);
            int postDelimIndex = lastCharIndex + 1;
            String prevTextPart = sb.substring(lastPostDelimIndex, delimIndex);
            if (openCloseChar == MULTITARGET_DELIM_OPEN_CHAR) {
                for(String prevName : nameStack) {
                    ((StringBuilder) map.get(prevName)).append(prevTextPart);
                }
                map.put(name, new StringBuilder());
                nameStack.add(name);
            } else if (openCloseChar == MULTITARGET_DELIM_CLOSE_CHAR) {
                for(String prevName : nameStack) {
                    ((StringBuilder) map.get(prevName)).append(prevTextPart);
                }
                nameStack.remove(name);
//                if (DEBUG) {
//                    Debug.logInfo("TEXT PART: " + prevTextPart, module);
//                }
            } else {
                Debug.logWarning("invalid targeted rendering output: unrecognized open/close symbol (this is probably an error, but not guaranteed)", module);
                searchIndex = lastCharIndex;
                continue;
            }
            searchIndex = postDelimIndex;
            lastPostDelimIndex = postDelimIndex;
        }
        if (lastPostDelimIndex < sb.length()) {
            String prevTextPart = sb.substring(lastPostDelimIndex);
            for(String prevName : nameStack) {
                ((StringBuilder) map.get(prevName)).append(prevTextPart);
            }
        }
        // TODO: check if this works
//        for(Map.Entry<String, Object> entry : map.entrySet()) {
//            entry.setValue(entry.getValue().toString());
//        }
        return map;
    }
    
}
