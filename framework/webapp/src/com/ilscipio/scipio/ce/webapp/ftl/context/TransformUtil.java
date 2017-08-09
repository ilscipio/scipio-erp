package com.ilscipio.scipio.ce.webapp.ftl.context;

import java.util.Map;

import org.ofbiz.base.util.UtilGenerics;
import org.ofbiz.base.util.template.FreeMarkerWorker;

import com.ilscipio.scipio.ce.webapp.ftl.lang.LangFtlUtil;
import com.ilscipio.scipio.ce.webapp.ftl.template.TemplateFtlUtil;

import freemarker.core.Environment;
import freemarker.template.TemplateBooleanModel;
import freemarker.template.TemplateDateModel;
import freemarker.template.TemplateHashModel;
import freemarker.template.TemplateModel;
import freemarker.template.TemplateModelException;
import freemarker.template.TemplateNumberModel;
import freemarker.template.TemplateScalarModel;

/**
 * SCIPIO: Utilities intended explicitly for implementation Freemarker transforms,
 * or that implement the transform interface as seen by templates.
 * <p>
 * The utils here provide the common behavior for transforms such that they all should behave
 * predictably to users.
 * <p>
 * Functions which are more generic in nature should go in the other util classes such
 * as {@link ContextFtlUtil}.
 */
public abstract class TransformUtil {

    public static final String module = TransformUtil.class.getName();
    
    protected TransformUtil() {
    }

    /**
     * Abstracted method to retrieve a "context/global" var (loosely-defined) from the Freemarker environment.
     * At minimum, always include Freemarker globals and data model, encompassing Ofbiz context and globalContext.
     * In addition - SUBJECT TO CHANGE - may read from current or main namespace.
     * <p>
     * NOTE: 2016-10-13: Currently this only reads from globals and data model, ignoring main
     * and current namespaces. So it will only respond to changes made using #global directive and not #assign.
     * TODO: REVIEW: We will start with this more restrictive/safer behavior first and see in future if main
     * or current namespace should be considered. This should be made to match the FTL macro implementations.
     * Some of the more common variable names (such as "locale") can cause problematic conflicts.
     * 
     * @see freemarker.core.Environment#getGlobalVariable(String)
     * @see com.ilscipio.scipio.ce.webapp.ftl.lang.LangFtlUtil#getMainNsOrGlobalVar(String, Environment)
     * @see com.ilscipio.scipio.ce.webapp.ftl.lang.LangFtlUtil#getCurrentNsOrGlobalVar(String, Environment)
     */
    public static TemplateModel getFtlContextGlobalVar(String name, Environment env) throws TemplateModelException {
        //return LangFtlUtil.getMainNsOrGlobalVar(name, env);
        return env.getGlobalVariable(name);
    }
    
    /**
     * Gets boolean arg.
     * <p>
     * Will automatically interpret string true/false as boolean.
     */
    public static Boolean getBooleanArg(TemplateModel obj, Boolean defaultValue) throws TemplateModelException {
        if (obj instanceof TemplateBooleanModel) {
            return ((TemplateBooleanModel) obj).getAsBoolean();
        }
        else if (obj instanceof TemplateScalarModel) {
            TemplateScalarModel s = (TemplateScalarModel) obj;
            String val = s.getAsString();
            // SCIPIO: empty check is desirable and makes it so caller can request default by specifying ""
            if (!val.isEmpty()) {
                return "true".equalsIgnoreCase(s.getAsString());
            }
        } else if (obj != null) {
            throw new TemplateModelException("Expected boolean model or string model representation of boolean, but got a " +
                    obj.getClass() + " instead");
        }
        return defaultValue;
    }
    
    public static Boolean getBooleanArg(TemplateModel obj) throws TemplateModelException {
        return getBooleanArg(obj, null);
    }
    
    // map 
    
    public static Boolean getBooleanArg(Map<?, ?> args, String key, Boolean defaultValue) throws TemplateModelException {
        return getBooleanArg(getModel(args, key), defaultValue);
    }
    
    public static Boolean getBooleanArg(Map<?, ?> args, String key) throws TemplateModelException {
        return getBooleanArg(getModel(args, key), null);

    }
    
    /**
     * Gets string arg.
     * <p>
     * If number or date passed, will be coerced to string. Other types such as maps or lists
     * will throw TemplateModelException.
     */
    public static String getStringArg(TemplateModel obj, String defaultValue, boolean useDefaultWhenEmpty, boolean nonEscaping) throws TemplateModelException {
        String result = null;
        if (obj instanceof TemplateScalarModel) {
            TemplateScalarModel s = (TemplateScalarModel) obj;
            result = LangFtlUtil.getAsString(s, nonEscaping);
        } else if (obj == null) {
            return defaultValue;
        } else if (obj instanceof TemplateNumberModel || obj instanceof TemplateDateModel) {
            // TODO: optimize this call
            result = LangFtlUtil.execStringBuiltIn(obj, FreeMarkerWorker.getCurrentEnvironment()).getAsString();
        } else {
            throw new TemplateModelException("Expected string model or something coercible to string, but got a " +
                    obj.getClass() + " instead");
        }
        if (useDefaultWhenEmpty && result.isEmpty()) {
            return defaultValue;
        }
        return result;
    }

    /**
     * Gets string arg.
     * <p>
     * Only returns the default if the string is null, but not if empty.
     */
    public static String getStringArg(TemplateModel obj, String defaultValue) throws TemplateModelException {
        return getStringArg(obj, defaultValue, false, false);
    }
    
    public static String getStringArg(TemplateModel obj, boolean nonEscaping) throws TemplateModelException {
        return getStringArg(obj, null, false, nonEscaping);
    }
    
    public static String getStringArg(TemplateModel obj) throws TemplateModelException {
        return getStringArg(obj, null, false, false);
    }

    /**
     * Gets string arg, bypassing screen auto-escaping.
     * <p>
     * Only returns the default if the string is null, but not if empty.
     */
    public static String getStringNonEscapingArg(TemplateModel obj, String defaultValue) throws TemplateModelException {
        return getStringArg(obj, defaultValue, false, true);
    }
    
    public static String getStringNonEscapingArg(TemplateModel obj) throws TemplateModelException {
        return getStringArg(obj, null, false, true);
    }
    
    public static String getStringArg(Map<?, ?> args, String key, String defaultValue, boolean useDefaultWhenEmpty, boolean nonEscaping) throws TemplateModelException {
        return getStringArg(getModel(args, key), defaultValue, useDefaultWhenEmpty, nonEscaping);
    }
    
    public static String getStringArg(Map<?, ?> args, String key, String defaultValue) throws TemplateModelException {
        return getStringArg(getModel(args, key), defaultValue, false, false);
    }
    
    public static String getStringArg(Map<?, ?> args, String key, boolean nonEscaping) throws TemplateModelException {
        return getStringArg(getModel(args, key), null, false, nonEscaping);
    }
    
    public static String getStringArg(Map<?, ?> args, String key) throws TemplateModelException {
        return getStringArg(getModel(args, key), null, false, false);
    }
    
    public static String getStringNonEscapingArg(Map<?, ?> args, String key, String defaultValue) throws TemplateModelException {
        return getStringArg(getModel(args, key), defaultValue, false, true);
    }
    
    public static String getStringNonEscapingArg(Map<?, ?> args, String key) throws TemplateModelException {
        return getStringArg(getModel(args, key), null, false, true);
    }
    
    public static Object getBooleanOrStringArg(TemplateModel obj, Object defaultValue, boolean useDefaultWhenEmpty, boolean nonEscaping) throws TemplateModelException {
        Object result = null;
        if (obj instanceof TemplateBooleanModel) {
            return ((TemplateBooleanModel) obj).getAsBoolean();
        } else if (obj instanceof TemplateScalarModel) {
            TemplateScalarModel s = (TemplateScalarModel) obj;
            result = LangFtlUtil.getAsString(s, nonEscaping);
        } else if (obj != null) {
            result = obj.toString();
        } else {
            return defaultValue;
        }
        if (useDefaultWhenEmpty && (result instanceof String) && ((String) result).isEmpty()) {
            return defaultValue;
        }
        return result;
    }
    
    public static Object getBooleanOrStringArg(TemplateModel obj) throws TemplateModelException {
        return getBooleanOrStringArg(obj, null, false, false);
    }
    
    public static Object getBooleanOrStringNonEscapingArg(TemplateModel obj) throws TemplateModelException {
        return getBooleanOrStringArg(obj, null, false, true);
    }
    
    public static Object getBooleanOrStringArg(Map<?, ?> args, String key, Object defaultValue, boolean useDefaultWhenEmpty, boolean nonEscaping) throws TemplateModelException {
        return getBooleanOrStringArg(getModel(args, key), defaultValue, useDefaultWhenEmpty, nonEscaping);
    }
    
    public static Object getBooleanOrStringArg(Map<?, ?> args, String key) throws TemplateModelException {
        return getBooleanOrStringArg(getModel(args, key), null, false, false);
    }
    
    public static Object getBooleanOrStringNonEscapingArg(Map<?, ?> args, String key) throws TemplateModelException {
        return getBooleanOrStringArg(getModel(args, key), null, false, true);

    }

    /**
     * Gets integer arg.
     * <p>
     * If string passed, will be parsed as integer. Other types such as maps or lists
     * will throw TemplateModelException.
     */
    public static Integer getIntegerArg(TemplateModel obj, Integer defaultValue) throws TemplateModelException, NumberFormatException {
        if (obj instanceof TemplateNumberModel) {
            return ((TemplateNumberModel) obj).getAsNumber().intValue();
        } else if (obj instanceof TemplateScalarModel) {
            TemplateScalarModel s = (TemplateScalarModel) obj;
            String strResult = LangFtlUtil.getAsString(s, true);
            if (strResult.isEmpty()) {
                return defaultValue;
            } else {
                return Integer.parseInt(strResult);
            }
        } else if (obj == null) {
            return defaultValue;
        } else {
            throw new TemplateModelException("Expected integer model or string representing of integer, but got a " +
                    obj.getClass() + " instead");
        }
    }
    
    public static Integer getIntegerArg(TemplateModel obj) throws TemplateModelException {
        return getIntegerArg(obj, null);
    }
    
    public static Integer getIntegerArg(Map<?, ?> args, String key, Integer defaultValue) throws TemplateModelException {
        return getIntegerArg(getModel(args, key), defaultValue);
    }
    
    public static Integer getIntegerArg(Map<?, ?> args, String key) throws TemplateModelException {
        return getIntegerArg(getModel(args, key), null);
    }
    
    /**
     * Gets a deep-unwrapped map.
     * FIXME: nonEscaping bool is currently not handled... it may bypass escaping in some cases but not others...
     */
    public static <K,V> Map<K, V> getMapArg(TemplateModel obj, Map<K, V> defaultValue, boolean useDefaultWhenEmpty, boolean nonEscaping) throws TemplateModelException {
        if (!nonEscaping) {
            throw new UnsupportedOperationException("getMapArg currently only supports escaping-bypassing (nonEscaping true)");
        }
        Map<K, V> result = null;
        if (obj instanceof TemplateHashModel) {
            result = UtilGenerics.checkMap(LangFtlUtil.unwrapAlways(obj));
        } else if (obj == null) {
            return defaultValue;
        } else {
            throw new TemplateModelException("Expected hash/map model or something coercible to map, but got a " +
                    obj.getClass() + " instead");
        }
        if (useDefaultWhenEmpty && result.isEmpty()) {
            return defaultValue;
        }
        return result;
    }
    
    /**
     * Gets a deep-unwrapped map.
     * FIXME: nonEscaping bool is currently not handled... it may bypass escaping in some cases but not others...
     */
    public static <K,V> Map<K, V> getMapArg(Map<?, ?> args, String key, Map<K, V> defaultValue, boolean useDefaultWhenEmpty, boolean nonEscaping) throws TemplateModelException {
        return getMapArg(getModel(args, key), defaultValue, useDefaultWhenEmpty, nonEscaping);
    }
    
    public static TemplateModel getModel(Map<?, ?> args, String key) {
        return (TemplateModel) args.get(key);
    }

    /**
     * Escapes a URL built by a transform such as ofbizUrl IF a language is specified.
     * <p>
     * WARN/FIXME?: 2016-10-19: THE STRICT BOOLEAN IS CURRENTLY IGNORED HERE BECAUSE THERE ARE TOO
     * MANY ESCAPED AMPERSANDS THROUGHOUT ALL OF OFBIZ AND TEMPLATES.
     */
    public static String escapeGeneratedUrl(String value, String lang, boolean strict, Environment env) throws TemplateModelException {
        //return TemplateFtlUtil.escapeFullUrl(value, lang, strict, env); // TODO/FIXME?
        return TemplateFtlUtil.escapeFullUrl(value, lang, null, env);
    }
}
