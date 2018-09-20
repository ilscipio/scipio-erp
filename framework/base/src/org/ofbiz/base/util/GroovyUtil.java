/*
 * Licensed to the Apache Software Foundation (ASF) under one
 * or more contributor license agreements.  See the NOTICE file
 * distributed with this work for additional information
 * regarding copyright ownership.  The ASF licenses this file
 * to you under the Apache License, Version 2.0 (the
 * "License"); you may not use this file except in compliance
 * with the License.  You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 * KIND, either express or implied.  See the License for the
 * specific language governing permissions and limitations
 * under the License.
 */
package org.ofbiz.base.util;

import java.io.IOException;
import java.io.InputStream;
import java.net.URL;
import java.util.HashMap;
import java.util.Map;

import javax.script.ScriptContext;

import org.codehaus.groovy.control.CompilationFailedException;
import org.codehaus.groovy.control.CompilerConfiguration;
import org.codehaus.groovy.runtime.InvokerHelper;
import org.ofbiz.base.location.FlexibleLocation;
import org.ofbiz.base.util.cache.UtilCache;

import groovy.lang.Binding;
import groovy.lang.GroovyClassLoader;
import groovy.lang.GroovyShell;
import groovy.lang.Script;

/**
 * Groovy Utilities.
 *
 */
public class GroovyUtil {

    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());

    private static final UtilCache<String, Class<?>> parsedScripts = UtilCache.createUtilCache("script.GroovyLocationParsedCache", 0, 0, false);
    /**
     * SCIPIO: A new cache needed for the {@link #eval} methods (in stock they simply don't cache, which is expensive).
     * Added 2018-09-19.
     */
    private static final UtilCache<String, Class<?>> parsedInlineScripts = UtilCache.createUtilCache("script.GroovyInlineParsedCache", 0, 0, false); // SCIPIO

    /**
     * Static Groovy script class loader.
     * <p>
     * SCIPIO: NOTE: This is mainly the class loader and config for the STANDARD lang variant configuration;
     * the other variants may eventually require their own having different loaders and/or configs.
     * <p>
     * WARN: This classloader does not use the current thread context classloader as parent.
     * <p>
     * WARN: Due to its design, the GroovyClassLoader can produce a thread lock even when loading
     * non-caching scripts. Therefore, it should only be shared if there is a script cache in front
     * of it.
     */
    private static final GroovyClassLoader groovyScriptClassLoader;
    private static final CompilerConfiguration groovyScriptCompilerConfig; // SCIPIO
    static {
        GroovyClassLoader groovyClassLoader = null;
        String scriptBaseClass = UtilProperties.getPropertyValue("groovy", "scriptBaseClass");
        CompilerConfiguration conf;
        if (!scriptBaseClass.isEmpty()) {
            conf = new CompilerConfiguration();
            conf.setScriptBaseClass(scriptBaseClass);
            groovyClassLoader = new GroovyClassLoader(GroovyUtil.class.getClassLoader(), conf);
        } else {
            conf = CompilerConfiguration.DEFAULT;
        }
        groovyScriptClassLoader = groovyClassLoader;
        groovyScriptCompilerConfig = conf; // SCIPIO
        /*
         *  With the implementation of @BaseScript annotations (introduced with Groovy 2.3.0) something was broken
         *  in the CompilerConfiguration.setScriptBaseClass method and an error is thrown when our scripts are executed;
         *  the workaround is to execute at startup a script containing the @BaseScript annotation.
         */
        try {
            GroovyUtil.runScriptAtLocation("component://base/config/GroovyInit.groovy", null, null);
        } catch (Exception e) {
            Debug.logWarning("The following error occurred during the initialization of Groovy: " + e.getMessage(), module);
        }
    }

    private GroovyUtil() {}

    /**
     * SCIPIO: Evaluate a Groovy condition, expression or script block (versatile).
     * <p>
     * Based on the original stock method {@link #eval(String, Map)}, which was
     * far too rigid (in fact, the original overload should probably not be used in Scipio).
     * <p>
     * 2018-09-19: Original overload modified to accept a specific Groovy language
     * variant; the default is {@link GroovyLangVariant#STANDARD}.
     * Also modified for: convertOperators, bindingToContext and useCache options.
     *
     * @param expression The expression to evaluate
     * @param context The context to use in evaluation (re-written)
     * @param langVariant The language variant to use (default: STANDARD)
     * @param convertOperators whether to apply StringUtil.convertOperatorSubstitutions on the expression or not
     * @param bindingToContext if true, dump binding variables into context at end of method; if false, don't
     * @param useCache use compiled script cache or not
     * @see <a href="StringUtil.html#convertOperatorSubstitutions(java.lang.String)">StringUtil.convertOperatorSubstitutions(java.lang.String)</a>
     * @return Object The result of the evaluation
     * @throws CompilationFailedException
     */
    @SuppressWarnings("unchecked")
    public static Object eval(String expression, Map<String, Object> context,
            GroovyLangVariant langVariant, boolean convertOperators, boolean bindingToContext, boolean useCache) throws CompilationFailedException {
        Object o;
        if (expression == null || expression.equals("")) {
            Debug.logError("Groovy Evaluation error. Empty expression", module);
            return null;
        }
        if (Debug.verboseOn()) {
            Debug.logVerbose("Evaluating -- " + expression, module);
            Debug.logVerbose("Using Context -- " + context, module);
        }
        try {
            if (langVariant == null) {
                langVariant = GroovyLangVariant.STANDARD;
            }
            Binding binding = getBinding(context, langVariant);
            if (useCache) {
                // SCIPIO: NOTE: see ScriptUtil for cacheKey format. Note we make a special case (+o)
                // to separate cases with and without operator substitutions.
                String cacheKey;
                if (convertOperators) {
                    cacheKey = langVariant.getName() + "+o://" + expression;
                } else {
                    cacheKey = langVariant.getName() + "://" + expression;
                }
                Class<?> parsedScript = parsedInlineScripts.get(cacheKey);
                if (parsedScript == null) {
                    if (convertOperators) {
                        String convertedOpExpr = StringUtil.convertOperatorSubstitutions(expression);
                        parsedScript = langVariant.getCommonGroovyClassLoader().parseClass(convertedOpExpr);
                        parsedScript = parsedInlineScripts.putIfAbsentAndGet(cacheKey, parsedScript);
                        // SCIPIO: extra caching: we additionally cache the script after operator substitutions
                        // in the non-op cache to help prevent duplicate scripts in memory
                        String nonOpCacheKey = langVariant.getName() + "://" + convertedOpExpr;
                        parsedScript = parsedInlineScripts.putIfAbsentAndGet(nonOpCacheKey, parsedScript);
                    } else {
                        parsedScript = langVariant.getCommonGroovyClassLoader().parseClass(expression);
                        parsedScript = parsedInlineScripts.putIfAbsentAndGet(cacheKey, parsedScript);
                    }
                }
                o = InvokerHelper.createScript(parsedScript, binding).run();
            } else {
                // SCIPIO: 2018-09-12: Must use our custom config (for access to predefined helpers)
                //GroovyShell shell = new GroovyShell(getBinding(context));
                // NOTE: Here we use getNewGroovyClassLoader to create a new GroovyClassLoader;
                // this is because GroovyClassLoader is designed to synchronize on an internal variable
                // even when it is not using its own cache, for some reason.
                // Creating a new one is not ideal, but avoids a thread lock.
                // TODO: REVIEW: the use of GroovyShell is inconsistent with the caching code and code elsewhere...
                GroovyClassLoader gcl = null;
                try {
                    gcl = langVariant.createGroovyClassLoader();
                    GroovyShell shell = new GroovyShell(gcl,
                            binding, langVariant.getGroovyCompilerConfiguration());
                    if (convertOperators) {
                        expression = StringUtil.convertOperatorSubstitutions(expression);
                    }
                    o = shell.evaluate(expression);
                } finally {
                    if (gcl != null) {
                        try {
                            gcl.close();
                        } catch(IOException e) {
                            Debug.logError(e, "Error closing GroovyClassLoader.", module);
                        }
                    }
                }
            }
            if (Debug.verboseOn()) {
                Debug.logVerbose("Evaluated to -- " + o, module);
            }
            if (bindingToContext) { // SCIPIO: now optional
                // read back the context info
                context.putAll(binding.getVariables());
            }
        } catch (CompilationFailedException e) {
            Debug.logError(e, "Groovy Evaluation error.", module);
            throw e;
        }
        return o;
    }

    /**
     * Evaluate a Groovy condition or expression (SCIPIO: legacy stock method)
     * @deprecated <strong>SCIPIO: WARNING:</strong> This stock method was non-caching (script recompiled at every call).
     * This method should only have been used for performance-insensitive applications 
     * and cases where arbitrary scripts are expected to be passed (such as admin webapp's TemplateTest).
     * However, in addition, this method dumps the binding variables back into context, which is unusual
     * and unwanted behavior for *.groovy scripts in Scipio, so the method has been deprecated for Scipio
     * to avoid unexpected side effects (context corruption).
     * <p>
     * SCIPIO: 2018-09-19: Uses {@link GroovyLangVariant#STANDARD},
     * converts operator substitutions,
     * dumps binding variables into context after call, NON-caching.
     *
     * @param expression The expression to evaluate
     * @param context The context to use in evaluation (re-written)
     * @see <a href="StringUtil.html#convertOperatorSubstitutions(java.lang.String)">StringUtil.convertOperatorSubstitutions(java.lang.String)</a>
     * @return Object The result of the evaluation
     * @throws CompilationFailedException
     */
    @Deprecated
    public static Object eval(String expression, Map<String, Object> context) throws CompilationFailedException {
        return eval(expression, context, GroovyLangVariant.STANDARD, true, true, false);
    }

    /**
     * SCIPIO: Evaluates a groovy script block using given language variant, does NOT convert operation substitutions
     * and does NOT dump the binding's variables into context after run.
     */
    public static Object evalBlock(String expression, Map<String, Object> context, GroovyLangVariant langVariant, boolean useCache) throws CompilationFailedException {
        return eval(expression, context, langVariant, false, false, useCache);
    }

    /**
     * SCIPIO: Evaluates a groovy script block using standard *.groovy file and minilang script block
     * behavior: uses {@link GroovyLangVariant#STANDARD}, does NOT convert operation substitutions
     * and does NOT dump the binding's variables into context after run.
     */
    public static Object evalBlock(String expression, Map<String, Object> context, boolean useCache) throws CompilationFailedException {
        return eval(expression, context, GroovyLangVariant.STANDARD, false, false, useCache);
    }

    /**
     * SCIPIO: Evaluates a legacy simple non-flexible condition expression (that in old ofbiz was previously
     * handled by Beanshell), often as a second interpreter fallback executed after the string
     * was already run through a <code>FlexibleStringExpander</code>.
     * <p>
     * This is predominantly intended for form-widget use-when expressions, where the flexible expressions
     * did not already evaluate to true or false..
     * <p>
     * <strong>WARN:</strong> useCache can be true ONLY if the input expression is non-variable; in other words if
     * it must not contain any flexible expressions! Otherwise cache may explode!
     */
    public static Object evalConditionExpr(String expression, Map<String, Object> context, boolean useCache) throws CompilationFailedException {
        // SCIPIO: Optimization: true/false
        if ("true".equals(expression)) {
            return true;
        } else if ("false".equals(expression)) {
            return false;
        }
        return eval(expression, context, GroovyLangVariant.SIMPLE, true, false, useCache);
    }

    /** Returns a <code>Binding</code> instance initialized with the
     * variables contained in <code>context</code>. If <code>context</code>
     * is <code>null</code>, an empty <code>Binding</code> is returned.
     * <p>The <code>context Map</code> is added to the <code>Binding</code>
     * as a variable called "context" so that variables can be passed
     * back to the caller. Any variables that are created in the script
     * are lost when the script ends unless they are copied to the
     * "context" <code>Map</code>.</p>
     * <p>
     * SCIPIO: 2018-09-19: Modified to accept a specific Groovy language
     * variant; the logical default is {@link GroovyLangVariant#STANDARD}.
     *
     * @param context A <code>Map</code> containing initial variables
     * @return A <code>Binding</code> instance
     */
    public static Binding getBinding(Map<String, Object> context, GroovyLangVariant langVariant) {
        // SCIPIO: 2018-09-19: Impl. MOVED to GroovyLangVariant#createBinding to allow specialization
        return langVariant.createBinding(context);
    }

    /** Returns a <code>Binding</code> instance initialized with the
     * variables contained in <code>context</code>. If <code>context</code>
     * is <code>null</code>, an empty <code>Binding</code> is returned.
     * <p>The <code>context Map</code> is added to the <code>Binding</code>
     * as a variable called "context" so that variables can be passed
     * back to the caller. Any variables that are created in the script
     * are lost when the script ends unless they are copied to the
     * "context" <code>Map</code>.</p>
     * <p>
     * SCIPIO: NOTE: 2018-09-19: This uses {@link GroovyLangVariant#STANDARD}'s binding;
     * use {@link #getBinding(Map, GroovyLangVariant)} to specify another.
     *
     * @param context A <code>Map</code> containing initial variables
     * @return A <code>Binding</code> instance
     */
    public static Binding getBinding(Map<String, Object> context) {
        return getBinding(context, GroovyLangVariant.STANDARD);
    }
    
    /**
     * SCIPIO: Stock code factored out from {@link #getBinding(Map)}, for use
     * by {@link GroovyLangVariant#createBinding(Map)}.
     * <p>
     * Factored out 2018-09-19.
     */
    private static Map<String, Object> prepareBindingVarsCommon(Map<String, Object> context) {
        Map<String, Object> vars = new HashMap<>();
        if (context != null) {
            vars.putAll(context);
            vars.put("context", context);
            if (vars.get(ScriptUtil.SCRIPT_HELPER_KEY) == null) {
                ScriptContext scriptContext = ScriptUtil.createScriptContext(context);
                ScriptHelper scriptHelper = (ScriptHelper)scriptContext.getAttribute(ScriptUtil.SCRIPT_HELPER_KEY);
                if (scriptHelper != null) {
                    vars.put(ScriptUtil.SCRIPT_HELPER_KEY, scriptHelper);
                }
            }
        }
        return vars;
    }

    public static Class<?> getScriptClassFromLocation(String location) throws GeneralException {
        try {
            Class<?> scriptClass = parsedScripts.get(location);
            if (scriptClass == null) {
                URL scriptUrl = FlexibleLocation.resolveLocation(location);
                if (scriptUrl == null) {
                    throw new GeneralException("Script not found at location [" + location + "]");
                }
                if (groovyScriptClassLoader != null) {
                    scriptClass = parseClass(scriptUrl.openStream(), location, groovyScriptClassLoader);
                } else {
                    scriptClass = parseClass(scriptUrl.openStream(), location);
                }
                Class<?> scriptClassCached = parsedScripts.putIfAbsent(location, scriptClass);
                if (scriptClassCached == null) { // putIfAbsent returns null if the class is added to the cache
                    if (Debug.verboseOn()) {
                        Debug.logVerbose("Cached Groovy script at: " + location, module);
                    }
                } else {
                    // the newly parsed script is discarded and the one found in the cache (that has been created by a concurrent thread in the meantime) is used
                    scriptClass = scriptClassCached;
                }
            }
            return scriptClass;
        } catch (RuntimeException e) {
            throw e;
        } catch (Exception e) {
            throw new GeneralException("Error loading Groovy script at [" + location + "]: ", e);
        }
    }

    /**
     * @throws IOException
     * @deprecated SCIPIO: 2017-01-30: ambiguous; method specifying an explicit ClassLoader should be used instead, to ensure library loading consistency.
     */
    @Deprecated
    public static Class<?> loadClass(String path) throws ClassNotFoundException, IOException {
        GroovyClassLoader groovyClassLoader = new GroovyClassLoader();
        Class<?> classLoader = groovyClassLoader.loadClass(path);
        groovyClassLoader.close();
        return classLoader;
    }

    public static Class<?> loadClass(String path, GroovyClassLoader groovyClassLoader) throws ClassNotFoundException, IOException { // SCIPIO
        return groovyClassLoader.loadClass(path);
    }

    /**
     * @deprecated SCIPIO: 2017-01-30: ambiguous; method specifying an explicit ClassLoader should be used instead, to ensure library loading consistency.
     */
    @Deprecated
    public static Class<?> parseClass(InputStream in, String location) throws IOException {
        GroovyClassLoader groovyClassLoader = new GroovyClassLoader();
        Class<?> classLoader = groovyClassLoader.parseClass(UtilIO.readString(in), location);
        groovyClassLoader.close();
        return classLoader;
    }

    public static Class<?> parseClass(InputStream in, String location, GroovyClassLoader groovyClassLoader) throws IOException {
        return groovyClassLoader.parseClass(UtilIO.readString(in), location);
    }

    /**
     * @throws IOException
     * @deprecated SCIPIO: 2017-01-30: ambiguous; method specifying an explicit ClassLoader should be used instead, to ensure library loading consistency.
     */
    @Deprecated
    public static Class<?> parseClass(String text) throws IOException {
        GroovyClassLoader groovyClassLoader = new GroovyClassLoader();
        Class<?> classLoader = groovyClassLoader.parseClass(text);
        groovyClassLoader.close();
        return classLoader;
    }

    public static Class<?> parseClass(String text, GroovyClassLoader groovyClassLoader) throws IOException { // SCIPIO: added 2017-01-27
        return groovyClassLoader.parseClass(text);
    }

    public static Object runScriptAtLocation(String location, String methodName, Map<String, Object> context) throws GeneralException {
        Script script = InvokerHelper.createScript(getScriptClassFromLocation(location), getBinding(context));
        Object result = null;
        if (UtilValidate.isEmpty(methodName)) {
            result = script.run();
        } else {
            result = script.invokeMethod(methodName, new Object[] { context });
        }
        return result;
    }

    /**
     * SCIPIO: Special case of {@link #runScriptAtLocation} where no methodName is specified
     */
    public static Object runScriptAtLocation(String location, Map<String, Object> context) throws GeneralException {
        return runScriptAtLocation(location,"",context);
    }

    /**
     * SCIPIO: Special case of {@link #runScriptAtLocation} that uses an empty context and returns the new
     * context (instead of script result).
     */
    public static Map<String, Object> runScriptAtLocationNewEmptyContext(String location, String methodName) throws GeneralException {
        Map<String, Object> context = new HashMap<String, Object>();
        runScriptAtLocation(location, methodName, context);
        return context;
    }

    /**
     * SCIPIO: Returns a {@link groovy.lang.Script} instance for the script groovy class at the given location,
     * initialized with the given {@link groovy.lang.Binding}.
     * <p>
     * Similar to calling (in Groovy code):
     * <pre>
     * {@code
     * def inst = GroovyUtil.getScriptClassFromLocation(location).newInstance();
     * inst.setBinding(binding);
     * }
     * </pre>
     * <p>
     * NOTE: Although this can handle groovy classes that do not extend <code>groovy.lang.Script</code>,
     * in such cases you may want to call {@link #getScriptClassFromLocation}
     * together with <code>.newInstance()</code> instead of this.
     * <p>
     * Added 2017-01-26.
     * @see #getScriptClassFromLocation
     */
    public static Script getScriptFromLocation(String location, Binding binding) throws GeneralException {
        return InvokerHelper.createScript(getScriptClassFromLocation(location), binding);
    }

    /**
     * SCIPIO: Returns a {@link groovy.lang.Script} instance for the script groovy class at the given location,
     * initialized with binding built from the given context.
     * <p>
     * Added 2017-01-26.
     * @see #getScriptFromLocation(String, Binding)
     */
    public static Script getScriptFromLocation(String location, Map<String, Object> context) throws GeneralException {
        return InvokerHelper.createScript(getScriptClassFromLocation(location), getBinding(context));
    }

    /**
     * SCIPIO: Returns the static <code>GroovyClassLoader</code> instance, configured
     * for Ofbiz script base class and other settings.
     * @deprecated 2018-09-19: ambiguous and static: use {@link GroovyLangVariant#getCommonGroovyClassLoader()}
     * or {@link GroovyLangVariant#createGroovyClassLoader()} instead.
     */
    @Deprecated
    public static GroovyClassLoader getGroovyScriptClassLoader() {
        return groovyScriptClassLoader;
    }

    /**
     * SCIPIO: Creates a new GroovyClassLoader.
     * <p>
     * This required in the absence of a script cache, because a design issue
     * about the GroovyClassLoader causes it to make a thread lock even when
     * not actually using its file cache.
     * <p>
     * TODO: REVIEW: Currently this does NOT use the current thread context class loader;
     * it may cause unpredictable behavior due to caching and other reasons.
     */
    private static GroovyClassLoader getNewGroovyScriptClassLoader(CompilerConfiguration conf) {
        return new GroovyClassLoader(GroovyUtil.class.getClassLoader(), conf);
    }

    /**
     * SCIPIO: Groovy language variants supported by GroovyUtil, and their configuration/behavior.
     * These modify the behavior of the language such as the <code>Binding</code>.
     * <p>
     * The general default is {@link #STANDARD}.
     * <p>
     * DEV NOTE: I decided not to make these enums to make future patching easier, but
     * in essence these take the place of an enum.
     * <p>
     * TODO?: Could register variants via properties... no need yet.
     * <p>
     * Added 2018-09-19.
     */
    public static abstract class GroovyLangVariant {

        /**
         * Standard groovy language most often used in Scipio, i.e. normally used in *.groovy files
         * and minilang script element blocks.
         * <p>
         * Uses base script class defined in groovy.properties.
         * <p>
         * Missing variables throw <code>MissingPropertyException</code>.
         */
        public static final GroovyLangVariant STANDARD = new StandardVariantConfig();

        /**
         * Simplified groovy variant, for short simple expressions
         * such as form widget field use-when attributes.
         * <p>
         * Uses base script class defined in groovy.properties.
         * <p>
         * Missing variables are treated as null instead of 
         * throwing <code>MissingPropertyException</code>.
         */
        public static final GroovyLangVariant SIMPLE = new SimpleVariantConfig();

        /**
         * Groovy variant that explicitly tries to emulate old Beanshell scripts for compatibility.
         * <p>
         * Uses base script class defined in groovy.properties (NOTE: subject to change if issues found).
         * <p>
         * Currently (2018-09-13), this does the same as {@link #SIMPLE}, but this
         * is subject to change.
         */
        public static final GroovyLangVariant BSH = new BshVariantConfig();

        private static final Map<String, GroovyLangVariant> nameMap;
        static {
            Map<String, GroovyLangVariant> m = new HashMap<>();
            m.put(STANDARD.getName(), STANDARD);
            m.put(SIMPLE.getName(), SIMPLE);
            m.put(BSH.getName(), BSH);
            nameMap = m;
        }

        public static GroovyLangVariant fromName(String variantName) throws IllegalArgumentException {
            GroovyLangVariant variant = fromNameOrNull(variantName);
            if (variant == null) {
                throw new IllegalArgumentException("Unrecognized Scipio Groovy language variant name: " + variant);
            }
            return variant;
        }

        public static GroovyLangVariant fromNameOrNull(String variantName) {
            return nameMap.get(variantName);
        }

        public static GroovyLangVariant fromNameOrDefault(String variantName, GroovyLangVariant defaultValue) {
            GroovyLangVariant variant = fromNameOrNull(variantName);
            return (variant != null) ? variant : defaultValue;
        }

        public static GroovyLangVariant fromName(Object variantObj) throws IllegalArgumentException {
            GroovyLangVariant variant = fromNameOrNull(variantObj);
            if (variant == null) {
                throw new IllegalArgumentException("Unrecognized Scipio Groovy language variant instance or name: " + variant);
            }
            return variant;
        }

        public static GroovyLangVariant fromNameOrNull(Object variantObj) {
            if (variantObj instanceof GroovyLangVariant) return (GroovyLangVariant) variantObj;
            if (variantObj instanceof String) return fromNameOrNull((String) variantObj);
            return null;
        }

        public static GroovyLangVariant fromNameOrDefault(Object variantObj, GroovyLangVariant defaultValue) {
            if (variantObj instanceof GroovyLangVariant) return (GroovyLangVariant) variantObj;
            if (variantObj instanceof String) return fromNameOrDefault((String) variantObj, defaultValue);
            return defaultValue;
        }

        // Subclasses may/should override these

        public abstract String getName();

        public CompilerConfiguration getGroovyCompilerConfiguration() {
            return GroovyUtil.groovyScriptCompilerConfig;
        }

        /**
         * Returns the common or shared groovy classloader IF supported, otherwise
         * a new one.
         */
        public GroovyClassLoader getCommonGroovyClassLoader() {
            return GroovyUtil.groovyScriptClassLoader;
        }

        /**
         * Creates a new groovy classloader (always new).
         */
        public GroovyClassLoader createGroovyClassLoader() {
            return getNewGroovyScriptClassLoader(getGroovyCompilerConfiguration());
        }

        /**
         * Creates the binding.
         * <p>
         * 2018-09-19: Implementation MOVED here from
         * {@link GroovyUtil#getBinding(Map, GroovyLangVariant)} so it can be
         * overridden.
         */
        public Binding createBinding(Map<String, Object> context) {
            return this.createBareBinding(prepareBindingVarsCommon(context));
        }

        /**
         * Create a new {@link groovy.lang.Binding} simply wrapping around the context
         * of the needed type.
         */
        public Binding createBareBinding(Map<String, Object> context) {
            return new Binding(context);
        }

        public static class StandardVariantConfig extends GroovyLangVariant {
            protected StandardVariantConfig() {}

            @Override
            public String getName() {
                return "std";
            }
        }

        public static class SimpleVariantConfig extends GroovyLangVariant {
            protected SimpleVariantConfig() {}

            @Override
            public String getName() {
                return "simple";
            }

            @Override
            public Binding createBareBinding(Map<String, Object> context) {
                return new MissingVarsNullBinding(context); // Simplified binding
            }
        }

        public static class BshVariantConfig extends GroovyLangVariant {
            protected BshVariantConfig() {}

            @Override
            public String getName() {
                return "bsh";
            }

            @Override
            public Binding createBareBinding(Map<String, Object> context) {
                return new MissingVarsNullBinding(context); // Simplified binding
            }
        }
    }

    /**
     * SCIPIO: A custom Groovy Binding that returns null when variable are missing instead
     * of throwing <code>MissingPropertyException</code>. Suitable for small expressions.
     * <p>
     * In other words, this can be used to emulate old Beanshell behavior.
     * <p>
     * DEV NOTE: DO NOT MODIFY this class if it's insufficient (and not broken);
     * make more subclasses for specific behaviors if needed.
     * This one is intended for reuse.
     * <p>
     * Added 2018-09-19.
     */
    public static class MissingVarsNullBinding extends Binding {
        public MissingVarsNullBinding(Map<String, Object> variables) {
            super(variables);
        }

        @Override
        public Object getVariable(String name) {
            try {
                return super.getVariable(name);
            } catch(groovy.lang.MissingPropertyException e) {
                return null;
            }
        }
    }
}
