/*******************************************************************************
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
 *******************************************************************************/
package org.ofbiz.base.util;

import java.util.Formatter;
import java.util.HashMap;
import java.util.Map;

import org.apache.logging.log4j.*;

/**
 * Configurable Debug logging wrapper class
 *
 */
public final class Debug {

    private static final String noModuleModule = "NoModule";  // set to null for previous behavior
    private static final Object[] emptyParams = new Object[0];

    public static final int ALWAYS = 0;
    public static final int VERBOSE = 1;
    public static final int TIMING = 2;
    public static final int INFO = 3;
    public static final int IMPORTANT = 4;
    public static final int WARNING = 5;
    public static final int ERROR = 6;
    public static final int FATAL = 7;

    private static final String[] levelProps = {"", "print.verbose", "print.timing", "print.info", "print.important", "print.warning", "print.error", "print.fatal"};
    private static final Level[] levelObjs = {Level.OFF, Level.DEBUG, Level.TRACE, Level.INFO, Level.INFO, Level.WARN, Level.ERROR, Level.FATAL};

    private static final Map<String, Integer> levelStringMap = new HashMap<String, Integer>();

    private static final boolean levelOnCache[] = new boolean[8]; // this field is not thread safe

    private static final Logger root = LogManager.getRootLogger();

    static {
        levelStringMap.put("verbose", Debug.VERBOSE);
        levelStringMap.put("timing", Debug.TIMING);
        levelStringMap.put("info", Debug.INFO);
        levelStringMap.put("important", Debug.IMPORTANT);
        levelStringMap.put("warning", Debug.WARNING);
        levelStringMap.put("error", Debug.ERROR);
        levelStringMap.put("fatal", Debug.FATAL);
        levelStringMap.put("always", Debug.ALWAYS);

        // initialize levelOnCache
        for (int i = 0; i < levelOnCache.length; i++) {
            levelOnCache[i] = (i == Debug.ALWAYS || UtilProperties.propertyValueEqualsIgnoreCase("debug.properties", levelProps[i], "true"));
        }
    }

    public static Logger getLogger(String module) {
        if (UtilValidate.isNotEmpty(module)) {
            // SCIPIO: 2016-11-11: this is a workaround for .groovy module names not showing
            // FIXME?: This should maybe solved another way as this might impact performance?
            if (module != null && module.endsWith(".groovy")) {
                // 2018-05-22: some of the groovy names are very long, and this looks a little poor, so simply omit the groovy part
                //module = module.substring(0, module.length() - 7) + "Groovy";
                module = module.substring(0, module.length() - 7);
            }
            return LogManager.getLogger(module);
        } else {
            return root;
        }
    }

    /** Gets an Integer representing the level number from a String representing the level name; will return null if not found */
    public static Integer getLevelFromString(String levelName) {
        if (levelName == null) return null;
        return levelStringMap.get(levelName.toLowerCase());
    }

    public static void log(int level, Throwable t, String msg, String module) {
        log(level, t, msg, module, "org.ofbiz.base.util.Debug", emptyParams);
    }

    public static void log(int level, Throwable t, String msg, String module, Object... params) {
        log(level, t, msg, module, "org.ofbiz.base.util.Debug", params);
    }

    public static void log(int level, Throwable t, String msg, String module, String callingClass) {
        log(level, t, msg, module, callingClass, new Object[0]);
    }

    public static void log(int level, Throwable t, String msg, String module, String callingClass, Object... params) {
        if (isOn(level)) {
            if (msg != null && params.length > 0) {
                StringBuilder sb = new StringBuilder();
                Formatter formatter = new Formatter(sb);
                formatter.format(msg, params);
                msg = sb.toString();
            }

            // log
            Logger logger = getLogger(module);
            logger.log(levelObjs[level], msg, t);
        }
    }

    public static boolean isOn(int level) {
        return levelOnCache[level];
    }

    // leaving these here
    public static void log(String msg) {
        log(Debug.ALWAYS, null, msg, noModuleModule, emptyParams);
    }

    public static void log(String msg, Object... params) {
        log(Debug.ALWAYS, null, msg, noModuleModule, params);
    }

    public static void log(Throwable t) {
        log(Debug.ALWAYS, t, null, noModuleModule, emptyParams);
    }

    public static void log(String msg, String module) {
        log(Debug.ALWAYS, null, msg, module, emptyParams);
    }

    public static void log(String msg, String module, Object... params) {
        log(Debug.ALWAYS, null, msg, module, params);
    }

    public static void log(Throwable t, String module) {
        log(Debug.ALWAYS, t, null, module, emptyParams);
    }

    public static void log(Throwable t, String msg, String module) {
        log(Debug.ALWAYS, t, msg, module, emptyParams);
    }

    public static void log(Throwable t, String msg, String module, Object... params) {
        log(Debug.ALWAYS, t, msg, module, params);
    }

    public static boolean verboseOn() {
        return isOn(Debug.VERBOSE);
    }

    public static void logVerbose(String msg, String module) {
        log(Debug.VERBOSE, null, msg, module, emptyParams);
    }

    public static void logVerbose(String msg, String module, Object... params) {
        log(Debug.VERBOSE, null, msg, module, params);
    }

    public static void logVerbose(Throwable t, String module) {
        log(Debug.VERBOSE, t, null, module, emptyParams);
    }

    public static void logVerbose(Throwable t, String msg, String module) {
        log(Debug.VERBOSE, t, msg, module, emptyParams);
    }

    public static void logVerbose(Throwable t, String msg, String module, Object... params) {
        log(Debug.VERBOSE, t, msg, module, params);
    }

    public static boolean timingOn() {
        return isOn(Debug.TIMING);
    }

    public static void logTiming(String msg, String module) {
        log(Debug.TIMING, null, msg, module, emptyParams);
    }

    public static void logTiming(String msg, String module, Object... params) {
        log(Debug.TIMING, null, msg, module, params);
    }

    public static void logTiming(Throwable t, String module) {
        log(Debug.TIMING, t, null, module, emptyParams);
    }

    public static void logTiming(Throwable t, String msg, String module) {
        log(Debug.TIMING, t, msg, module, emptyParams);
    }

    public static void logTiming(Throwable t, String msg, String module, Object... params) {
        log(Debug.TIMING, t, msg, module, params);
    }

    public static boolean infoOn() {
        return isOn(Debug.INFO);
    }

    public static void logInfo(String msg, String module) {
        log(Debug.INFO, null, msg, module, emptyParams);
    }

    public static void logInfo(String msg, String module, Object... params) {
        log(Debug.INFO, null, msg, module, params);
    }

    public static void logInfo(Throwable t, String module) {
        log(Debug.INFO, t, null, module, emptyParams);
    }

    public static void logInfo(Throwable t, String msg, String module) {
        log(Debug.INFO, t, msg, module, emptyParams);
    }

    public static void logInfo(Throwable t, String msg, String module, Object... params) {
        log(Debug.INFO, t, msg, module, params);
    }

    public static boolean importantOn() {
        return isOn(Debug.IMPORTANT);
    }

    public static void logImportant(String msg, String module) {
        log(Debug.IMPORTANT, null, msg, module, emptyParams);
    }

    public static void logImportant(String msg, String module, Object... params) {
        log(Debug.IMPORTANT, null, msg, module, params);
    }

    public static void logImportant(Throwable t, String module) {
        log(Debug.IMPORTANT, t, null, module, emptyParams);
    }

    public static void logImportant(Throwable t, String msg, String module) {
        log(Debug.IMPORTANT, t, msg, module, emptyParams);
    }

    public static void logImportant(Throwable t, String msg, String module, Object... params) {
        log(Debug.IMPORTANT, t, msg, module, params);
    }

    public static boolean warningOn() {
        return isOn(Debug.WARNING);
    }

    public static void logWarning(String msg, String module) {
        log(Debug.WARNING, null, msg, module, emptyParams);
    }

    public static void logWarning(String msg, String module, Object... params) {
        log(Debug.WARNING, null, msg, module, params);
    }

    public static void logWarning(Throwable t, String module) {
        log(Debug.WARNING, t, null, module, emptyParams);
    }

    public static void logWarning(Throwable t, String msg, String module) {
        log(Debug.WARNING, t, msg, module, emptyParams);
    }

    public static void logWarning(Throwable t, String msg, String module, Object... params) {
        log(Debug.WARNING, t, msg, module, params);
    }

    public static boolean errorOn() {
        return isOn(Debug.ERROR);
    }

    public static void logError(String msg, String module) {
        log(Debug.ERROR, null, msg, module, emptyParams);
    }

    public static void logError(String msg, String module, Object... params) {
        log(Debug.ERROR, null, msg, module, params);
    }

    public static void logError(Throwable t, String module) {
        log(Debug.ERROR, t, null, module, emptyParams);
    }

    public static void logError(Throwable t, String msg, String module) {
        log(Debug.ERROR, t, msg, module, emptyParams);
    }

    public static void logError(Throwable t, String msg, String module, Object... params) {
        log(Debug.ERROR, t, msg, module, params);
    }

    public static boolean fatalOn() {
        return isOn(Debug.FATAL);
    }

    public static void logFatal(String msg, String module) {
        log(Debug.FATAL, null, msg, module, emptyParams);
    }

    public static void logFatal(String msg, String module, Object... params) {
        log(Debug.FATAL, null, msg, module, params);
    }

    public static void logFatal(Throwable t, String module) {
        log(Debug.FATAL, t, null, module, emptyParams);
    }

    public static void logFatal(Throwable t, String msg, String module) {
        log(Debug.FATAL, t, msg, module, emptyParams);
    }

    public static void logFatal(Throwable t, String msg, String module, Object... params) {
        log(Debug.FATAL, t, msg, module, params);
    }

    public static void set(int level, boolean on) {
        levelOnCache[level] = on;
    }

    public static boolean get(int level) {
        return levelOnCache[level];
    }
    
    /**
     * SCIPIO: Optimized object-based wrapper around the Debug logging methods.
     * <p>
     * 2018-05-24: This should now be used in all Java classes instead of the old module String:
     * <pre>{@code
     *     private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());
     * }</pre>
     * This should be used in Groovy scripts:
     * <pre>{@code
     *     final module = Debug.getOfbizLogger(getClass());
     * }</pre>
     * <p>
     * In method bodies, the same <code>Debug.logXxx</code> invocation format as before can be preserved since new overloads
     * are added which accept a OfbizLogger object. 
     * <p>
     * <strong>NOTE:</strong> 2018-05-24: Although the logXxx methods on this logger can be used, 
     * for consistency reasons it is recommended to use the Debug.logXxx overloads which accept a OfbizLogger
     * as parameter, for the foreseeable future. There is no real performance difference.
     * <p>
     * When used in this fashion, this wrapper object provides optimized caching of the log4j 
     * {@link org.apache.logging.log4j.Logger} instance for much faster <code>Debug.logXxx</code> invocations,
     * compared to the legacy invocations that take a module String, which incur some string and lookup overhead.
     * <p>
     * Added 2018-05-24.
     */
    public static class OfbizLogger {
        private static final OfbizLogger rootOfbizLogger = OfbizLogger.getInstance(Debug.root); 
        
        private final Logger log4jLogger;

        private OfbizLogger(Logger log4jLogger) {
            this.log4jLogger = log4jLogger;
        }

        /**
         * Returns a new OfbizLogger wrapper logger around the given log4j 
         * {@link org.apache.logging.log4j.Logger} instance.
         */
        public static OfbizLogger getInstance(Logger log4jLogger) {
            return new OfbizLogger(log4jLogger);
        }

        /**
         * Returns a new OfbizLogger wrapper logger for the given Ofbiz
         * Java class.
         * <p>
         * For Java, the class can be gotten by invoking
         * <code>java.lang.invoke.MethodHandles.lookup().lookupClass()</code>
         * in the calling class.
         */
        public static OfbizLogger getInstance(Class<?> cls) {
            return (cls != null) ? new OfbizLogger(LogManager.getLogger(cls)) : rootOfbizLogger;
        }

        /**
         * Returns a new OfbizLogger wrapper logger for the given Ofbiz
         * Java or Groovy class module name.
         * <p>
         * For Java, the module name can be gotten by invoking
         * <code>java.lang.invoke.MethodHandles.lookup().lookupClass()</code>
         * in the calling class.
         * <p>
         * For Groovy, the module name can simply be specified as "MyScriptName.groovy".
         */
        public static OfbizLogger getInstance(String module) {
            return UtilValidate.isNotEmpty(module) ? new OfbizLogger(Debug.getLogger(module)) : rootOfbizLogger;
        }

        /**
         * Returns the root OfbizLogger logger wrapper.
         */
        public static OfbizLogger getInstance() {
            return rootOfbizLogger;
        }

        public Logger getLogger() {
            return log4jLogger;
        }

        /**
         * Returns the logger name, in other words the module string.
         * For Java classes, this is the fully-qualified class name.
         */
        public String getName() {
            return log4jLogger.getName();
        }

        @Override
        public String toString() {
            // Return the logger name, for backward compat, for anywhere module string
            // was used in string concatenations
            return log4jLogger.getName();
        }

        @Override
        public boolean equals(Object obj) {
            // For backward compat, workaround for module.equals(...) invocations
            if (obj == null) return false;
            return toString().equals(obj.toString());
        }

        @Override
        public int hashCode() {
            return log4jLogger.getName().hashCode();
        }

        public void log(int level, Throwable t, String msg) {
            log(level, t, msg, "org.ofbiz.base.util.Debug$OfbizLogger", emptyParams);
        }

        public void log(int level, Throwable t, String msg, Object... params) {
            log(level, t, msg, "org.ofbiz.base.util.Debug$OfbizLogger", params);
        }

        public void log(int level, Throwable t, String msg, String callingClass) {
            log(level, t, msg, callingClass, new Object[0]);
        }

        public void log(int level, Throwable t, String msg, String callingClass, Object... params) {
            if (isOn(level)) {
                if (msg != null && params.length > 0) {
                    StringBuilder sb = new StringBuilder();
                    Formatter formatter = new Formatter(sb);
                    formatter.format(msg, params);
                    msg = sb.toString();
                }

                // log
                log4jLogger.log(levelObjs[level], msg, t);
            }
        }

        public boolean isOn(int level) {
            return levelOnCache[level];
        }

        public void log(String msg) {
            log(Debug.ALWAYS, null, msg, emptyParams);
        }

        public void log(String msg, Object... params) {
            log(Debug.ALWAYS, null, msg, params);
        }

        public void log(Throwable t) {
            log(Debug.ALWAYS, t, null, emptyParams);
        }

        public void log(Throwable t, String msg) {
            log(Debug.ALWAYS, t, msg, emptyParams);
        }

        public void log(Throwable t, String msg, Object... params) {
            log(Debug.ALWAYS, t, msg, params);
        }

        public boolean verboseOn() {
            return isOn(Debug.VERBOSE);
        }

        public void logVerbose(String msg) {
            log(Debug.VERBOSE, null, msg, emptyParams);
        }

        public void logVerbose(String msg, Object... params) {
            log(Debug.VERBOSE, null, msg, params);
        }

        public void logVerbose(Throwable t) {
            log(Debug.VERBOSE, t, null, emptyParams);
        }

        public void logVerbose(Throwable t, String msg) {
            log(Debug.VERBOSE, t, msg, emptyParams);
        }

        public void logVerbose(Throwable t, String msg, Object... params) {
            log(Debug.VERBOSE, t, msg, params);
        }

        public boolean timingOn() {
            return isOn(Debug.TIMING);
        }

        public void logTiming(String msg) {
            log(Debug.TIMING, null, msg, emptyParams);
        }

        public void logTiming(String msg, Object... params) {
            log(Debug.TIMING, null, msg, params);
        }

        public void logTiming(Throwable t) {
            log(Debug.TIMING, t, null, emptyParams);
        }

        public void logTiming(Throwable t, String msg) {
            log(Debug.TIMING, t, msg, emptyParams);
        }

        public void logTiming(Throwable t, String msg, Object... params) {
            log(Debug.TIMING, t, msg, params);
        }

        public boolean infoOn() {
            return isOn(Debug.INFO);
        }

        public void logInfo(String msg) {
            log(Debug.INFO, null, msg, emptyParams);
        }

        public void logInfo(String msg, Object... params) {
            log(Debug.INFO, null, msg, params);
        }

        public void logInfo(Throwable t) {
            log(Debug.INFO, t, null, emptyParams);
        }

        public void logInfo(Throwable t, String msg) {
            log(Debug.INFO, t, msg, emptyParams);
        }

        public void logInfo(Throwable t, String msg, Object... params) {
            log(Debug.INFO, t, msg, params);
        }

        public boolean importantOn() {
            return isOn(Debug.IMPORTANT);
        }

        public void logImportant(String msg) {
            log(Debug.IMPORTANT, null, msg, emptyParams);
        }

        public void logImportant(String msg, Object... params) {
            log(Debug.IMPORTANT, null, msg, params);
        }

        public void logImportant(Throwable t) {
            log(Debug.IMPORTANT, t, null, emptyParams);
        }

        public void logImportant(Throwable t, String msg) {
            log(Debug.IMPORTANT, t, msg, emptyParams);
        }

        public void logImportant(Throwable t, String msg, Object... params) {
            log(Debug.IMPORTANT, t, msg, params);
        }

        public boolean warningOn() {
            return isOn(Debug.WARNING);
        }

        public void logWarning(String msg) {
            log(Debug.WARNING, null, msg, emptyParams);
        }

        public void logWarning(String msg, Object... params) {
            log(Debug.WARNING, null, msg, params);
        }

        public void logWarning(Throwable t) {
            log(Debug.WARNING, t, null, emptyParams);
        }

        public void logWarning(Throwable t, String msg) {
            log(Debug.WARNING, t, msg, emptyParams);
        }

        public void logWarning(Throwable t, String msg, Object... params) {
            log(Debug.WARNING, t, msg, params);
        }

        public boolean errorOn() {
            return isOn(Debug.ERROR);
        }

        public void logError(String msg) {
            log(Debug.ERROR, null, msg, emptyParams);
        }

        public void logError(String msg, Object... params) {
            log(Debug.ERROR, null, msg, params);
        }

        public void logError(Throwable t) {
            log(Debug.ERROR, t, null, emptyParams);
        }

        public void logError(Throwable t, String msg) {
            log(Debug.ERROR, t, msg, emptyParams);
        }

        public void logError(Throwable t, String msg, Object... params) {
            log(Debug.ERROR, t, msg, params);
        }

        public boolean fatalOn() {
            return isOn(Debug.FATAL);
        }

        public void logFatal(String msg) {
            log(Debug.FATAL, null, msg, emptyParams);
        }

        public void logFatal(String msg, Object... params) {
            log(Debug.FATAL, null, msg, params);
        }

        public void logFatal(Throwable t) {
            log(Debug.FATAL, t, null, emptyParams);
        }

        public void logFatal(Throwable t, String msg) {
            log(Debug.FATAL, t, msg, emptyParams);
        }

        public void logFatal(Throwable t, String msg, Object... params) {
            log(Debug.FATAL, t, msg, params);
        }
    }

    /**
     * Returns a new OfbizLogger wrapper logger around the given log4j 
     * {@link org.apache.logging.log4j.Logger} instance.
     */
    public static OfbizLogger getOfbizLogger(Logger log4jLogger) {
        return OfbizLogger.getInstance(log4jLogger);
    }

    /**
     * Returns a new OfbizLogger wrapper logger for the given Ofbiz
     * Java class.
     * <p>
     * For Java, the class can be gotten by invoking
     * <code>java.lang.invoke.MethodHandles.lookup().lookupClass()</code>
     * in the calling class.
     */
    public static OfbizLogger getOfbizLogger(Class<?> cls) {
        return OfbizLogger.getInstance(cls);
    }

    /**
     * Returns a new OfbizLogger wrapper logger for the given Ofbiz
     * Java or Groovy class module name.
     * <p>
     * For Java, the module name can be gotten by invoking
     * <code>java.lang.invoke.MethodHandles.lookup().lookupClass()</code>
     * in the calling class.
     * <p>
     * For Groovy, the module name can simply be specified as "MyScriptName.groovy".
     */
    public static OfbizLogger getOfbizLogger(String module) {
        return OfbizLogger.getInstance(module);
    }

    /**
     * Returns the root OfbizLogger logger wrapper.
     */
    public static OfbizLogger getOfbizLogger() {
        return OfbizLogger.getInstance();
    }

    /*
     * *********************************************************************
     * SCIPIO: OfbizLogger overloads
     * *********************************************************************
     * These provide compatibility with the legacy ofbiz Debug log line invocation interface,
     * when using the OfbizLogger instance from a class (instead of the legacy module String).
     * NOTE: 2018-05-24: For the foreseeable future, it is recommended to use these 
     * instead of the OfbizLogger methods, for consistency reason. These do not add any overhead.
     */

    public static void log(int level, Throwable t, String msg, OfbizLogger logger) {
        log(level, t, msg, logger, "org.ofbiz.base.util.Debug", emptyParams);
    }

    public static void log(int level, Throwable t, String msg, OfbizLogger logger, Object... params) {
        log(level, t, msg, logger, "org.ofbiz.base.util.Debug", params);
    }

    public static void log(int level, Throwable t, String msg, OfbizLogger logger, String callingClass) {
        log(level, t, msg, logger, callingClass, new Object[0]);
    }

    public static void log(int level, Throwable t, String msg, OfbizLogger logger, String callingClass, Object... params) {
        if (isOn(level)) {
            if (msg != null && params.length > 0) {
                StringBuilder sb = new StringBuilder();
                Formatter formatter = new Formatter(sb);
                formatter.format(msg, params);
                msg = sb.toString();
            }

            // log
            logger.log4jLogger.log(levelObjs[level], msg, t);
        }
    }

    public static void log(String msg, OfbizLogger logger, Object... params) {
        log(Debug.ALWAYS, null, msg, logger, params);
    }

    public static void log(Throwable t, OfbizLogger logger) {
        log(Debug.ALWAYS, t, null, logger, emptyParams);
    }

    public static void log(Throwable t, String msg, OfbizLogger logger) {
        log(Debug.ALWAYS, t, msg, logger, emptyParams);
    }

    public static void log(Throwable t, String msg, OfbizLogger logger, Object... params) {
        log(Debug.ALWAYS, t, msg, logger, params);
    }

    public static void logVerbose(String msg, OfbizLogger logger) {
        log(Debug.VERBOSE, null, msg, logger, emptyParams);
    }

    public static void logVerbose(String msg, OfbizLogger logger, Object... params) {
        log(Debug.VERBOSE, null, msg, logger, params);
    }

    public static void logVerbose(Throwable t, OfbizLogger logger) {
        log(Debug.VERBOSE, t, null, logger, emptyParams);
    }

    public static void logVerbose(Throwable t, String msg, OfbizLogger logger) {
        log(Debug.VERBOSE, t, msg, logger, emptyParams);
    }

    public static void logVerbose(Throwable t, String msg, OfbizLogger logger, Object... params) {
        log(Debug.VERBOSE, t, msg, logger, params);
    }

    public static void logTiming(String msg, OfbizLogger logger) {
        log(Debug.TIMING, null, msg, logger, emptyParams);
    }

    public static void logTiming(String msg, OfbizLogger logger, Object... params) {
        log(Debug.TIMING, null, msg, logger, params);
    }

    public static void logTiming(Throwable t, OfbizLogger logger) {
        log(Debug.TIMING, t, null, logger, emptyParams);
    }

    public static void logTiming(Throwable t, String msg, OfbizLogger logger) {
        log(Debug.TIMING, t, msg, logger, emptyParams);
    }

    public static void logTiming(Throwable t, String msg, OfbizLogger logger, Object... params) {
        log(Debug.TIMING, t, msg, logger, params);
    }

    public static void logInfo(String msg, OfbizLogger logger) {
        log(Debug.INFO, null, msg, logger, emptyParams);
    }

    public static void logInfo(String msg, OfbizLogger logger, Object... params) {
        log(Debug.INFO, null, msg, logger, params);
    }

    public static void logInfo(Throwable t, OfbizLogger logger) {
        log(Debug.INFO, t, null, logger, emptyParams);
    }

    public static void logInfo(Throwable t, String msg, OfbizLogger logger) {
        log(Debug.INFO, t, msg, logger, emptyParams);
    }

    public static void logInfo(Throwable t, String msg, OfbizLogger logger, Object... params) {
        log(Debug.INFO, t, msg, logger, params);
    }

    public static void logImportant(String msg, OfbizLogger logger) {
        log(Debug.IMPORTANT, null, msg, logger, emptyParams);
    }

    public static void logImportant(String msg, OfbizLogger logger, Object... params) {
        log(Debug.IMPORTANT, null, msg, logger, params);
    }

    public static void logImportant(Throwable t, OfbizLogger logger) {
        log(Debug.IMPORTANT, t, null, logger, emptyParams);
    }

    public static void logImportant(Throwable t, String msg, OfbizLogger logger) {
        log(Debug.IMPORTANT, t, msg, logger, emptyParams);
    }

    public static void logImportant(Throwable t, String msg, OfbizLogger logger, Object... params) {
        log(Debug.IMPORTANT, t, msg, logger, params);
    }

    public static void logWarning(String msg, OfbizLogger logger) {
        log(Debug.WARNING, null, msg, logger, emptyParams);
    }

    public static void logWarning(String msg, OfbizLogger logger, Object... params) {
        log(Debug.WARNING, null, msg, logger, params);
    }

    public static void logWarning(Throwable t, OfbizLogger logger) {
        log(Debug.WARNING, t, null, logger, emptyParams);
    }

    public static void logWarning(Throwable t, String msg, OfbizLogger logger) {
        log(Debug.WARNING, t, msg, logger, emptyParams);
    }

    public static void logWarning(Throwable t, String msg, OfbizLogger logger, Object... params) {
        log(Debug.WARNING, t, msg, logger, params);
    }

    public static void logError(String msg, OfbizLogger logger) {
        log(Debug.ERROR, null, msg, logger, emptyParams);
    }

    public static void logError(String msg, OfbizLogger logger, Object... params) {
        log(Debug.ERROR, null, msg, logger, params);
    }

    public static void logError(Throwable t, OfbizLogger logger) {
        log(Debug.ERROR, t, null, logger, emptyParams);
    }

    public static void logError(Throwable t, String msg, OfbizLogger logger) {
        log(Debug.ERROR, t, msg, logger, emptyParams);
    }

    public static void logError(Throwable t, String msg, OfbizLogger logger, Object... params) {
        log(Debug.ERROR, t, msg, logger, params);
    }

    public static void logFatal(String msg, OfbizLogger logger) {
        log(Debug.FATAL, null, msg, logger, emptyParams);
    }

    public static void logFatal(String msg, OfbizLogger logger, Object... params) {
        log(Debug.FATAL, null, msg, logger, params);
    }

    public static void logFatal(Throwable t, OfbizLogger logger) {
        log(Debug.FATAL, t, null, logger, emptyParams);
    }

    public static void logFatal(Throwable t, String msg, OfbizLogger logger) {
        log(Debug.FATAL, t, msg, logger, emptyParams);
    }

    public static void logFatal(Throwable t, String msg, OfbizLogger logger, Object... params) {
        log(Debug.FATAL, t, msg, logger, params);
    }
}
