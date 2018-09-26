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

import java.io.BufferedInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.Serializable;
import java.math.BigDecimal;
import java.math.BigInteger;
import java.net.URL;
import java.text.MessageFormat;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Enumeration;
import java.util.HashMap;
import java.util.HashSet;
import java.util.InvalidPropertiesFormatException;
import java.util.Iterator;
import java.util.LinkedHashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.MissingResourceException;
import java.util.Properties;
import java.util.ResourceBundle;
import java.util.Set;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.apache.commons.lang.StringUtils;
import org.ofbiz.base.location.FlexibleLocation;
import org.ofbiz.base.util.cache.UtilCache;
import org.ofbiz.base.util.collections.ResourceBundleMapWrapper;
import org.ofbiz.base.util.string.FlexibleStringExpander;
import org.w3c.dom.Document;
import org.w3c.dom.Element;

/** Generic Property Accessor with Cache - Utilities for working with properties files.
 * <p>UtilProperties divides properties files into two classes: non-locale-specific -
 * which are used for application parameters, configuration settings, etc; and
 * locale-specific - which are used for UI labels, system messages, etc. Each class
 * of properties files is kept in its own cache.</p>
 * <p>The locale-specific class of properties files can be in any one of three
 * formats: the standard text-based key=value format (*.properties file), the Java
 * XML properties format, and the OFBiz-specific XML file format
 * (see the <a href="#xmlToProperties(java.io.InputStream,%20java.util.Locale,%20java.util.Properties)">xmlToProperties</a>
 * method).</p>
 */
@SuppressWarnings("serial")
public final class UtilProperties implements Serializable {

    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());

    private UtilProperties() {}

    /**
     * A cache for storing Properties instances. Each Properties instance is keyed by its URL.
     */
    private static final UtilCache<String, Properties> urlCache = UtilCache.createUtilCache("properties.UtilPropertiesUrlCache");

    /**
     * SCIPIO: A lightweight cache for storing Properties instances. Each Properties instance is keyed by its resource name.
     * Mainly intended for *.properties files (not localized resource bundles). This is a lightweight second-layer cache
     * around the instances already stored in {@link #urlCache}, so as to not duplicate the Properties instances.
     * Improves access for the many individual property lookups.
     * Added 2018-07-18.
     */
    private static final UtilCache<String, Properties> propResourceCache = UtilCache.createUtilCache("properties.UtilPropertiesPropResourceCache");

    /**
     * SCIPIO: A read-only empty properties instance.
     */
    private static final Properties emptyProperties = new ExtendedProperties();

    // SCIPIO: 2018-07-18: HashSet is not thread-safe! Use an immutable collection copy pattern instead.
    // NOTE: Here even omitting volatile because this does not appear to be critical or one-time information (mainly performance?).
    //private static final Set<String> propertiesNotFound = new HashSet<String>();
    private static Set<String> propertiesNotFound = Collections.emptySet();

    /** Compares the specified property to the compareString, returns true if they are the same, false otherwise
     * @param resource The name of the resource - if the properties file is 'webevent.properties', the resource name is 'webevent'
     * @param name The name of the property in the properties file
     * @param compareString The String to compare the property value to
     * @return True if the strings are the same, false otherwise
     */
    public static boolean propertyValueEquals(String resource, String name, String compareString) {
        String value = getPropertyValue(resource, name);

        return value.trim().equals(compareString);
    }

    /** Compares Ignoring Case the specified property to the compareString, returns true if they are the same, false otherwise
     * @param resource The name of the resource - if the properties file is 'webevent.properties', the resource name is 'webevent'
     * @param name The name of the property in the properties file
     * @param compareString The String to compare the property value to
     * @return True if the strings are the same, false otherwise
     */
    public static boolean propertyValueEqualsIgnoreCase(String resource, String name, String compareString) {
        String value = getPropertyValue(resource, name);

        return value.trim().equalsIgnoreCase(compareString);
    }

    /** Returns the value of the specified property name from the specified resource/properties file.
     * If the specified property name or properties file is not found, the defaultValue is returned.
     * @param resource The name of the resource - if the properties file is 'webevent.properties', the resource name is 'webevent'
     * @param name The name of the property in the properties file
     * @param defaultValue The value to return if the property is not found
     * @return The value of the property in the properties file, or if not found then the defaultValue
     */
    public static String getPropertyValue(String resource, String name, String defaultValue) {
        String value = getPropertyValue(resource, name);

        if (UtilValidate.isEmpty(value)) {
            return defaultValue;
        }
        return value;
    }

    /** SCIPIO: Returns the value of the specified property name from the specified resource/properties file.
     * If the specified property name or properties file is not found, the defaultValue is returned.
     * Added 2018-07-12.
     * @param resource The name of the resource - if the properties file is 'webevent.properties', the resource name is 'webevent'
     * @param name The name of the property in the properties file
     * @param defaultValue The value to return if the property is not found
     * @return The value of the property in the properties file, or if not found then the defaultValue
     */
    public static String getPropertyValue(Properties properties, String name, String defaultValue) {
        String value = getPropertyValue(properties, name);

        if (UtilValidate.isEmpty(value)) {
            return defaultValue;
        }
        return value;
    }

    /**
     * getPropertyNumber, as double.
     * <p>
     * SCIPIO: <strong>WARN:</strong> This method is inconsistent; you should use {@link #getPropertyAsDouble(String, String, double)} instead.
     */
    public static double getPropertyNumber(String resource, String name, double defaultValue) {
        String str = getPropertyValue(resource, name);
        if (UtilValidate.isEmpty(str)) { // SCIPIO: 2018-09-26: don't try/warn if empty
            return defaultValue;
        }
        try {
            return Double.parseDouble(str);
        } catch (NumberFormatException nfe) {
            Debug.logWarning("Error converting String \"" + str + "\" to double; using defaultNumber: " + defaultValue + ".", module); // SCIPIO: 2018-09-26: don't swallow
            return defaultValue;
        }
    }

    /**
     * getPropertyNumber, as double, with default value 0.00000.
     * <p>
     * SCIPIO: <strong>WARN:</strong> This method is inconsistent; you should use {@link #getPropertyAsDouble(String, String, double)} instead.
     */
    public static double getPropertyNumber(String resource, String name) {
        return getPropertyNumber(resource, name, 0.00000);
    }

    /**
     * Returns the Number as a Number-Object of the specified property name from the specified resource/properties file.
     * If the specified property name or properties file is not found, the defaultObject is returned.
     * @param resource The name of the resource - if the properties file is 'webevent.properties', the resource name is 'webevent'
     * @param name The name of the property in the properties file
     * @param defaultNumber Optional: The Number to return if the property is not found.
     * @param type A String of the the Object the Number is converted to (like "Integer").
     * @return A Number-Object of the property as the defined type; or if not found the defaultObject
     */
    private static Number getPropertyNumber(String resource, String name, Number defaultNumber, String type) {
        String str = getPropertyValue(resource, name);
        if (UtilValidate.isEmpty(str)) {
            // SCIPIO: 2017-07-15: should not be a warning nor error
            //Debug.logWarning("Error converting String \"" + str + "\" to " + type + "; using defaultNumber " + defaultNumber + ".", module);
            Debug.logInfo("Property [" + resource + "/" + name + "] empty; using defaultNumber " + defaultNumber + ".", module);
            return defaultNumber;
        }
        try {
            return (Number)(ObjectType.simpleTypeConvert(str, type, null, null));
        } catch (Exception e) { // SCIPIO: 2018-09-26: use Exception here, because there may be unexpected RuntimeExceptions thrown here
            Debug.logWarning("Error converting String \"" + str + "\" to " + type + "; using defaultNumber " + defaultNumber + ".", module);
        }
        return defaultNumber;
    }

    /**
     * Returns a Boolean-Object of the specified property name from the specified resource/properties file.
     * If the specified property name or properties file is not found, the defaultValue is returned.
     * @param resource The name of the resource - if the properties file is 'webevent.properties', the resource name is 'webevent'
     * @param name The name of the property in the properties file
     * @param defaultValue Optional: The Value to return if the property is not found or not the correct format.
     * @return A Boolean-Object of the property; or if not found the defaultValue
     */
    public static Boolean getPropertyAsBoolean(String resource, String name, boolean defaultValue) {
        String str = getPropertyValue(resource, name);
        if ("true".equalsIgnoreCase(str)) {
            return Boolean.TRUE;
        } else if ("false".equalsIgnoreCase(str)) {
            return Boolean.FALSE;
        } else {
            return defaultValue;
        }
    }

    /**
     * Returns a Boolean-Object of the specified property name from the specified resource/properties file. [SCIPIO: 2017-08-29: boxed-type overload]
     * If the specified property name or properties file is not found, the defaultValue is returned.
     * @param resource The name of the resource - if the properties file is 'webevent.properties', the resource name is 'webevent'
     * @param name The name of the property in the properties file
     * @param defaultValue Optional: The Value to return if the property is not found or not the correct format. [SCIPIO: 2017-08-29: now boxed type]
     * @return A Boolean-Object of the property; or if not found the defaultValue
     */
    public static Boolean getPropertyAsBoolean(String resource, String name, Boolean defaultValue) {
        String str = getPropertyValue(resource, name);
        if ("true".equalsIgnoreCase(str)) {
            return Boolean.TRUE;
        } else if ("false".equalsIgnoreCase(str)) {
            return Boolean.FALSE;
        } else {
            return defaultValue;
        }
    }

    /**
     * Returns an Integer-Object of the specified property name from the specified resource/properties file.
     * If the specified property name or properties file is not found, the defaultNumber is returned.
     * @param resource The name of the resource - if the properties file is 'webevent.properties', the resource name is 'webevent'
     * @param name The name of the property in the properties file
     * @param defaultNumber Optional: The Value to return if the property is not found.
     * @return An Integer-Object of the property; or if not found the defaultNumber
     */
    public static Integer getPropertyAsInteger(String resource, String name, int defaultNumber) {
        return (Integer)getPropertyNumber(resource, name, defaultNumber, "Integer");
    }

    /**
     * Returns an Integer-Object of the specified property name from the specified resource/properties file. [SCIPIO: 2017-08-29: boxed-type overload]
     * If the specified property name or properties file is not found, the defaultNumber is returned.
     * @param resource The name of the resource - if the properties file is 'webevent.properties', the resource name is 'webevent'
     * @param name The name of the property in the properties file
     * @param defaultNumber Optional: The Value to return if the property is not found. [SCIPIO: 2017-08-29: now boxed type]
     * @return An Integer-Object of the property; or if not found the defaultNumber
     */
    public static Integer getPropertyAsInteger(String resource, String name, Integer defaultNumber) {
        return (Integer)getPropertyNumber(resource, name, defaultNumber, "Integer");
    }

    /**
     * SCIPIO: Returns an Integer-Object of the specified property name from the specified resource/properties file. [SCIPIO: 2017-08-29: boxed-type overload]
     * If the specified property name or properties file is not found, the defaultNumber is returned.
     * If value is not between the given minValue and maxValue (where null means unbounded), the defaultValue is returned.
     * Added 2017-07-12.
     * @param resource The name of the resource - if the properties file is 'webevent.properties', the resource name is 'webevent'
     * @param name The name of the property in the properties file
     * @param defaultNumber Optional: The Value to return if the property is not found. [SCIPIO: 2017-08-29: now boxed type]
     * @return An Integer-Object of the property; or if not found the defaultNumber
     */
    public static Integer getPropertyAsIntegerInRange(String resource, String name, Integer minValue, Integer maxValue, Integer defaultNumber) {
        return UtilNumber.getInRange((Integer)getPropertyNumber(resource, name, defaultNumber, "Integer"), minValue, maxValue, defaultNumber);
    }

    /**
     * Returns a Long-Object of the specified property name from the specified resource/properties file.
     * If the specified property name or properties file is not found, the defaultNumber is returned.
     * @param resource The name of the resource - if the properties file is 'webevent.properties', the resource name is 'webevent'
     * @param name The name of the property in the properties file
     * @param defaultNumber Optional: The Value to return if the property is not found.
     * @return A Long-Object of the property; or if not found the defaultNumber
     */
    public static Long getPropertyAsLong(String resource, String name, long defaultNumber) {
        return (Long)getPropertyNumber(resource, name, defaultNumber, "Long");
    }

    /**
     * SCIPIO: Returns a Long-Object of the specified property name from the specified resource/properties file. [SCIPIO: 2017-08-29: boxed-type overload]
     * If the specified property name or properties file is not found, the defaultNumber is returned.
     * If value is not between the given minValue and maxValue (where null means unbounded), the defaultValue is returned.
     * Added 2017-07-12.
     * @param resource The name of the resource - if the properties file is 'webevent.properties', the resource name is 'webevent'
     * @param name The name of the property in the properties file
     * @param defaultNumber Optional: The Value to return if the property is not found. [SCIPIO: 2017-08-29: now boxed type]
     * @return A Long-Object of the property; or if not found the defaultNumber
     */
    public static Long getPropertyAsLongInRange(String resource, String name, Long minValue, Long maxValue, Long defaultNumber) {
        return UtilNumber.getInRange((Long)getPropertyNumber(resource, name, defaultNumber, "Long"), minValue, maxValue, defaultNumber);
    }

    /**
     * Returns a Long-Object of the specified property name from the specified resource/properties file. [SCIPIO: 2017-08-29: boxed-type overload]
     * If the specified property name or properties file is not found, the defaultNumber is returned.
     * @param resource The name of the resource - if the properties file is 'webevent.properties', the resource name is 'webevent'
     * @param name The name of the property in the properties file
     * @param defaultNumber Optional: The Value to return if the property is not found. [SCIPIO: 2017-08-29: now boxed type]
     * @return A Long-Object of the property; or if not found the defaultNumber
     */
    public static Long getPropertyAsLong(String resource, String name, Long defaultNumber) {
        return (Long)getPropertyNumber(resource, name, defaultNumber, "Long");
    }

    /**
     * Returns a Float-Object of the specified property name from the specified resource/properties file.
     * If the specified property name or properties file is not found, the defaultNumber is returned.
     * @param resource The name of the resource - if the properties file is 'webevent.properties', the resource name is 'webevent'
     * @param name The name of the property in the properties file
     * @param defaultNumber Optional: The Value to return if the property is not found.
     * @return A Long-Object of the property; or if not found the defaultNumber
     */
    public static Float getPropertyAsFloat(String resource, String name, float defaultNumber) {
        return (Float)getPropertyNumber(resource, name, defaultNumber, "Float");
    }

    /**
     * Returns a Float-Object of the specified property name from the specified resource/properties file. [SCIPIO: 2017-08-29: boxed-type overload]
     * If the specified property name or properties file is not found, the defaultNumber is returned.
     * @param resource The name of the resource - if the properties file is 'webevent.properties', the resource name is 'webevent'
     * @param name The name of the property in the properties file
     * @param defaultNumber Optional: The Value to return if the property is not found. [SCIPIO: 2017-08-29: now boxed type]
     * @return A Long-Object of the property; or if not found the defaultNumber
     */
    public static Float getPropertyAsFloat(String resource, String name, Float defaultNumber) {
        return (Float)getPropertyNumber(resource, name, defaultNumber, "Float");
    }

    /**
     * Returns a Double-Object of the specified property name from the specified resource/properties file.
     * If the specified property name or properties file is not found, the defaultNumber is returned.
     * @param resource The name of the resource - if the properties file is 'webevent.properties', the resource name is 'webevent'
     * @param name The name of the property in the properties file
     * @param defaultNumber Optional: The Value to return if the property is not found.
     * @return A Double-Object of the property; or if not found the defaultNumber
     */
    public static Double getPropertyAsDouble(String resource, String name, double defaultNumber) {
        return (Double)getPropertyNumber(resource, name, defaultNumber, "Double");
    }

    /**
     * Returns a Double-Object of the specified property name from the specified resource/properties file. [SCIPIO: 2017-08-29: boxed-type overload]
     * If the specified property name or properties file is not found, the defaultNumber is returned.
     * @param resource The name of the resource - if the properties file is 'webevent.properties', the resource name is 'webevent'
     * @param name The name of the property in the properties file
     * @param defaultNumber Optional: The Value to return if the property is not found. [SCIPIO: 2017-08-29: now boxed type]
     * @return A Double-Object of the property; or if not found the defaultNumber
     */
    public static Double getPropertyAsDouble(String resource, String name, Double defaultNumber) {
        return (Double)getPropertyNumber(resource, name, defaultNumber, "Double");
    }

    /**
     * Returns a BigInteger-Object of the specified property name from the specified resource/properties file.
     * If the specified property name or properties file is not found, the defaultNumber is returned.
     * @param resource The name of the resource - if the properties file is 'webevent.properties', the resource name is 'webevent'
     * @param name The name of the property in the properties file
     * @param defaultNumber Optional: The Value to return if the property is not found.
     * @return A BigInteger-Object of the property; or if not found the defaultNumber
     */
    public static BigInteger getPropertyAsBigInteger(String resource, String name, BigInteger defaultNumber) {
        String strValue = getPropertyValue(resource, name);
        if (UtilValidate.isEmpty(strValue)) { // SCIPIO: 2018-09-26: don't warn if empty
            return defaultNumber;
        }
        BigInteger result = defaultNumber;
        try {
            result = new BigInteger(strValue);
        } catch (NumberFormatException nfe) {
            Debug.logWarning("Couldn't convert String \"" + strValue + "\" to BigInteger; using defaultNumber " + defaultNumber.toString() + ".", module);
        }
        return result;
    }

    /**
     * Returns a BigDecimal-Object of the specified property name from the specified resource/properties file.
     * If the specified property name or properties file is not found, the defaultNumber is returned.
     * @param resource The name of the resource - if the properties file is 'webevent.properties', the resource name is 'webevent'
     * @param name The name of the property in the properties file
     * @param defaultNumber Optional: The Value to return if the property is not found.
     * @return A BigDecimal-Object of the property; or if not found the defaultNumber
     */
    public static BigDecimal getPropertyAsBigDecimal(String resource, String name, BigDecimal defaultNumber) {
        String strValue = getPropertyValue(resource, name);
        if (UtilValidate.isEmpty(strValue)) { // SCIPIO: 2018-09-26: don't warn if empty
            return defaultNumber;
        }
        BigDecimal result = defaultNumber;
        try {
            result = new BigDecimal(strValue);
        } catch (NumberFormatException nfe) {
            Debug.logWarning("Couldn't convert String \"" + strValue + "\" to BigDecimal; using defaultNumber " + defaultNumber.toString() + ".", module);
        }
        return result;
    }

    /** Returns the value of the specified property name from the specified resource/properties file
     * @param resource The name of the resource - can be a file, class, or URL
     * @param name The name of the property in the properties file
     * @return The value of the property in the properties file
     */
    public static String getPropertyValue(String resource, String name) {
        if (UtilValidate.isEmpty(resource)) {
            return "";
        }
        if (UtilValidate.isEmpty(name)) {
            return "";
        }

        Properties properties = getProperties(resource);
        if (properties == null) {
            return "";
        }

        String value = null;

        try {
            value = properties.getProperty(name);
        } catch (Exception e) {
            Debug.logInfo(e, module);
        }
        return value == null ? "" : value.trim();
    }

    /** SCIPIO: Returns the value of the specified property name from the specified resource/properties file
     * Added 2018-07-12.
     * @param resource The name of the resource - can be a file, class, or URL
     * @param name The name of the property in the properties file
     * @return The value of the property in the properties file
     */
    public static String getPropertyValue(Properties properties, String name) {
        if (UtilValidate.isEmpty(name)) {
            return "";
        }
        if (properties == null) {
            return "";
        }

        String value = null;

        try {
            value = properties.getProperty(name);
        } catch (Exception e) {
            Debug.logInfo(e, module);
        }
        return value == null ? "" : value.trim();
    }

    /** SCIPIO: Returns the value of the specified property name from the specified resource/properties file,
     * or null if it is absent or empty.
     * Added 2018-04-27.
     * @param resource The name of the resource - can be a file, class, or URL
     * @param name The name of the property in the properties file
     * @return The value of the property in the properties file
     */
    public static String getPropertyValueOrNull(String resource, String name) {
        String value = getPropertyValue(resource, name);
        return value.isEmpty() ? null : value;
    }

    /** SCIPIO: Returns the value of the specified property name from the specified resource/properties file,
     * or null if it is absent or empty.
     * Added 2018-07-12.
     * @param resource The name of the resource - can be a file, class, or URL
     * @param name The name of the property in the properties file
     * @return The value of the property in the properties file
     */
    public static String getPropertyValueOrNull(Properties properties, String name) {
        String value = getPropertyValue(properties, name);
        return value.isEmpty() ? null : value;
    }

    /**
     * Returns a new <code>Properties</code> instance created from <code>fileName</code>.
     * <p>This method is intended for low-level framework classes that need to read
     * properties files before OFBiz has been fully initialized.</p>
     *
     * @param fileName The full name of the properties file ("foo.properties")
     * @return A new <code>Properties</code> instance created from <code>fileName</code>, or
     * <code>null</code> if the file was not found
     * @throws IllegalArgumentException if <code>fileName</code> is empty
     * @throws IllegalStateException if there was a problem reading the file
     */
    public static Properties createProperties(String fileName) {
        Assert.notEmpty("fileName", fileName);
        InputStream inStream = null;
        try {
            URL url = Thread.currentThread().getContextClassLoader().getResource(fileName);
            if (url == null) {
                return null;
            }
            inStream = url.openStream();
            Properties properties = new Properties();
            properties.load(inStream);
            return properties;
        } catch (Exception e) {
            throw new IllegalStateException("Exception thrown while reading " + fileName + ": " + e);
        } finally {
            if (inStream != null) {
                try {
                    inStream.close();
                } catch (IOException e) {
                    Debug.logError(e, "Exception thrown while closing InputStream", module);
                }
            }
        }
    }

    /** Returns the specified resource/properties file
     * <p>
     * SCIPIO: MERGED PROPERTIES (2018-07-18):
     * This method now supports merged properties
     * by combining several resource names in the resource string.
     * Format: "+resource1+resource2+resource3"
     * In other words, a starting "+" indicates merged properties mode,
     * and the rest of the string is resource names separated by "+".
     * This is equivalent to calling {@code getMergedProperties("resource1", "resource2", "resource3")}.
     * The merged properties are cached and the result must not be modified.
     *
     * @param resource The name of the resource - can be a file, class, or URL
     * @return The properties file
     */
    public static Properties getProperties(String resource) {
        if (UtilValidate.isEmpty(resource)) {
            return null;
        }
        // SCIPIO: 2018-07-18: Now uses an extra lightweight cache around the URL cache.
        // The two-layer caching ensures reuse of the Properties instances.
        //URL url = resolvePropertiesUrl(resource, null);
        //return getProperties(url);
        Properties properties = propResourceCache.get(resource);
        if (properties == null) {
            if (resource.charAt(0) == '+') {
                // SCIPIO: 2018-07-18: MERGED PROPERTIES
                String[] mergeResources = StringUtils.split(resource.substring(1), '+');
                properties = getMergedPropertiesFromUrlCache(mergeResources);
            } else {
                URL url = resolvePropertiesUrl(resource, null);
                properties = getProperties(url);
            }
            if (properties != null) {
                propResourceCache.put(resource, properties);
            }
        }
        return properties;
    }

    /**
     * SCIPIO: Returns an immutable empty Properties instance, which can
     * be used to avoid null checks in code constructs. WARN: Must not be modified!
     * Added 2018-07-18.
     */
    public static Properties getEmptyProperties() {
        return emptyProperties;
    }

    /**
     * SCIPIO: Returns a Properties instance composed of the given resources merged together.
     * The entries in the last resource override the previous ones.
     * <p>
     * NOTE: Like {@link #getProperties(String)}, the resulting properties are cached and
     * should not be modified.
     * <p>
     * If one or more of the resources are missing, they are skipped. If all of them
     * are missing, returns null.
     * <p>
     * Added 2018-07-18.
     */
    public static Properties getMergedProperties(String... resources) {
        String propResourceCacheKey = "+" + StringUtils.join(resources, "+");
        Properties properties = propResourceCache.get(propResourceCacheKey);
        if (properties == null) {
            properties = getMergedPropertiesFromUrlCache(resources);
            if (properties != null) {
                propResourceCache.put(propResourceCacheKey, properties);
            }
        }
        return properties;
    }

    /**
     * SCIPIO: Returns a Properties instance composed of the given resources merged together.
     * The entries in the last resource override the previous ones.
     * <p>
     * NOTE: Like {@link #getProperties(String)}, the resulting properties are cached and
     * should not be modified.
     * <p>
     * If one or more of the resources are missing, they are skipped. If all of them
     * are missing, returns null.
     * <p>
     * Added 2018-07-18.
     */
    public static Properties getMergedProperties(Collection<String> resources) {
        return getMergedProperties(resources.toArray(new String[resources.size()]));
    }

    private static Properties getMergedPropertiesFromUrlCache(String[] resources) {
        if (resources.length == 0) {
            throw new IllegalArgumentException("No resources specified for merged properties");
        }

        // Make cache key for urlCache, which is "+url1+url2+url3"
        StringBuilder urlCacheKeySb = new StringBuilder();
        URL[] urlList = new URL[resources.length];
        for(int i = 0; i < resources.length; i++) {
            URL url = resolvePropertiesUrl(resources[i], null);
            urlList[i] = url;
            if (url != null) {
                urlCacheKeySb.append('+');
                urlCacheKeySb.append(url.toString());
            }
        }
        String urlCacheKey = urlCacheKeySb.toString();

        Properties mergedProperties = (urlCacheKey.length() > 0) ? urlCache.get(urlCacheKey) : null;
        if (mergedProperties == null) {
            // DEV NOTE: This log message is only printed if the string resources map
            // to a different set of URLs after filtering for normalization and missing ones,
            // so less than you might expect; clear the cache using admin UI for testing properly.
            StringBuilder log = new StringBuilder("Merged properties: Resources: [");
            for(int i = 0; i < resources.length; i++) {
                log.append("[");
                log.append(resources[i]);
                URL url = urlList[i];
                if (url != null) {
                    Properties properties = getProperties(url);
                    if (properties != null) {
                        if (mergedProperties == null) { // for now: require at least one valid Properties
                            mergedProperties = new ExtendedProperties();
                        }
                        mergedProperties.putAll(properties);
                        log.append("->merged] + ");
                    } else {
                        log.append("->not merged (file not loaded)] + ");
                    }
                } else {
                    log.append("->not merged (url not resolved)] + ");
                }
            }
            if (log.length() > 0) log.setLength(log.length() - " + ".length());
            log.append("]");
            if (mergedProperties != null) {
                urlCache.put(urlCacheKey, mergedProperties);
            } else {
                log.append(" (no resources could be loaded)");
            }
            if (Debug.verboseOn()) {
                log.append("; resolved URLs: [");
                log.append(urlCacheKey);
                log.append("]");
            }
            Debug.logInfo(log.toString(), module);
        }
        return mergedProperties;
    }

    /** Returns the specified resource/properties file
     * @param url The URL to the resource
     * @return The properties file
     */
    public static Properties getProperties(URL url) {
        if (url == null) {
            return null;
        }
        String cacheKey = url.toString();
        Properties properties = urlCache.get(cacheKey);
        if (properties == null) {
            try {
                properties = new ExtendedProperties(url, null);
                urlCache.put(cacheKey, properties);
            } catch (Exception e) {
                Debug.logInfo(e, module);
            }
        }
        if (properties == null) {
            Debug.logInfo("[UtilProperties.getProperties] could not find resource: " + url, module);
            return null;
        }
        return properties;
    }


    // ========= URL Based Methods ==========

    /** Compares the specified property to the compareString, returns true if they are the same, false otherwise
     * @param url URL object specifying the location of the resource
     * @param name The name of the property in the properties file
     * @param compareString The String to compare the property value to
     * @return True if the strings are the same, false otherwise
     */
    public static boolean propertyValueEquals(URL url, String name, String compareString) {
        String value = getPropertyValue(url, name);

        if (value == null) {
            return false;
        }
        return value.trim().equals(compareString);
    }

    /** Compares Ignoring Case the specified property to the compareString, returns true if they are the same, false otherwise
     * @param url URL object specifying the location of the resource
     * @param name The name of the property in the properties file
     * @param compareString The String to compare the property value to
     * @return True if the strings are the same, false otherwise
     */
    public static boolean propertyValueEqualsIgnoreCase(URL url, String name, String compareString) {
        String value = getPropertyValue(url, name);

        if (value == null) {
            return false;
        }
        return value.trim().equalsIgnoreCase(compareString);
    }

    /** Returns the value of the specified property name from the specified resource/properties file.
     * If the specified property name or properties file is not found, the defaultValue is returned.
     * @param url URL object specifying the location of the resource
     * @param name The name of the property in the properties file
     * @param defaultValue The value to return if the property is not found
     * @return The value of the property in the properties file, or if not found then the defaultValue
     */
    public static String getPropertyValue(URL url, String name, String defaultValue) {
        String value = getPropertyValue(url, name);

        if (UtilValidate.isEmpty(value)) {
            return defaultValue;
        }
        return value;
    }

    public static double getPropertyNumber(URL url, String name, double defaultValue) {
        String str = getPropertyValue(url, name);
        if (str == null) {
            return defaultValue;
        }

        try {
            return Double.parseDouble(str);
        } catch (NumberFormatException nfe) {
            return defaultValue;
        }
    }

    public static double getPropertyNumber(URL url, String name) {
        return getPropertyNumber(url, name, 0.00000);
    }

    /** Returns the value of the specified property name from the specified resource/properties file
     * @param url URL object specifying the location of the resource
     * @param name The name of the property in the properties file
     * @return The value of the property in the properties file
     */
    public static String getPropertyValue(URL url, String name) {
        if (url == null) {
            return "";
        }
        if (UtilValidate.isEmpty(name)) {
            return "";
        }
        Properties properties = getProperties(url);

        if (properties == null) {
            return null;
        }

        String value = null;

        try {
            value = properties.getProperty(name);
        } catch (Exception e) {
            Debug.logInfo(e, module);
        }
        return value == null ? "" : value.trim();
    }

    /** Returns the value of a split property name from the specified resource/properties file
     * Rather than specifying the property name the value of a name.X property is specified which
     * will correspond to a value.X property whose value will be returned. X is a number from 1 to
     * whatever and all values are checked until a name.X for a certain X is not found.
     * @param url URL object specifying the location of the resource
     * @param name The name of the split property in the properties file
     * @return The value of the split property from the properties file
     */
    public static String getSplitPropertyValue(URL url, String name) {
        if (url == null) {
            return "";
        }
        if (UtilValidate.isEmpty(name)) {
            return "";
        }

        Properties properties = getProperties(url);

        if (properties == null) {
            return null;
        }

        String value = null;

        try {
            int curIdx = 1;
            String curName = null;

            while ((curName = properties.getProperty("name." + curIdx)) != null) {
                if (name.equals(curName)) {
                    value = properties.getProperty("value." + curIdx);
                    break;
                }
                curIdx++;
            }
        } catch (Exception e) {
            Debug.logInfo(e, module);
        }
        return value == null ? "" : value.trim();
    }

    /** Sets the specified value of the specified property name to the specified resource/properties file
     * @param resource The name of the resource - must be a file
     * @param name The name of the property in the properties file
     * @param value The value of the property in the properties file */
     public static void setPropertyValue(String resource, String name, String value) {
         if (UtilValidate.isEmpty(resource)) {
            return;
        }
         if (UtilValidate.isEmpty(name)) {
            return;
        }

         Properties properties = getProperties(resource);
         if (properties == null) {
             return;
         }

        try (
                FileOutputStream propFile = new FileOutputStream(resource);) {
             properties.setProperty(name, value);
             if ("XuiLabels".equals(name)) {
                 properties.store(propFile,
                     "##############################################################################\n"
                     +"# Licensed to the Apache Software Foundation (ASF) under one                   \n"
                     +"# or more contributor license agreements.  See the NOTICE file                 \n"
                     +"# distributed with this work for additional information                        \n"
                     +"# regarding copyright ownership.  The ASF licenses this file                   \n"
                     +"# to you under the Apache License, Version 2.0 (the                            \n"
                     +"# \"License\"); you may not use this file except in compliance                 \n"
                     +"# with the License.  You may obtain a copy of the License at                   \n"
                     +"#                                                                              \n"
                     +"# http://www.apache.org/licenses/LICENSE-2.0                                   \n"
                     +"#                                                                              \n"
                     +"# Unless required by applicable law or agreed to in writing,                   \n"
                     +"# software distributed under the License is distributed on an                  \n"
                     +"# \"AS IS\" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY                     \n"
                     +"# KIND, either express or implied.  See the License for the                    \n"
                     +"# specific language governing permissions and limitations                      \n"
                     +"# under the License.                                                           \n"
                     +"###############################################################################\n"
                     +"#                                                                              \n"
                     +"# Dynamically modified by OFBiz Framework (org.ofbiz.base.util : UtilProperties.setPropertyValue)\n"
                     +"#                                                                              \n"
                     +"# By default the screen is 1024x768 wide. If you want to use another screen size,\n"
                     +"# you must create a new directory under specialpurpose/pos/screens, like the 800x600.\n"
                     +"# You must also set the 3 related parameters (StartClass, ClientWidth, ClientHeight) accordingly.\n"
                     +"#");
             } else {
                 properties.store(propFile,
                     "##############################################################################\n"
                     +"# Licensed to the Apache Software Foundation (ASF) under one                   \n"
                     +"# or more contributor license agreements.  See the NOTICE file                 \n"
                     +"# distributed with this work for additional information                        \n"
                     +"# regarding copyright ownership.  The ASF licenses this file                   \n"
                     +"# to you under the Apache License, Version 2.0 (the                            \n"
                     +"# \"License\"); you may not use this file except in compliance                 \n"
                     +"# with the License.  You may obtain a copy of the License at                   \n"
                     +"#                                                                              \n"
                     +"# http://www.apache.org/licenses/LICENSE-2.0                                   \n"
                     +"#                                                                              \n"
                     +"# Unless required by applicable law or agreed to in writing,                   \n"
                     +"# software distributed under the License is distributed on an                  \n"
                     +"# \"AS IS\" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY                     \n"
                     +"# KIND, either express or implied.  See the License for the                    \n"
                     +"# specific language governing permissions and limitations                      \n"
                     +"# under the License.                                                           \n"
                     +"###############################################################################\n"
                     +"#                                                                              \n"
                     +"# Dynamically modified by OFBiz Framework (org.ofbiz.base.util : UtilProperties.setPropertyValue)\n"
                     +"# The comments have been removed, you may still find them on the OFBiz repository... \n"
                     +"#");
             }

             //propFile.close(); // SCIPIO: 2018-08-30: covered by try-with-resources
         } catch (FileNotFoundException e) {
             Debug.logInfo(e, "Unable to located the resource file.", module);
         } catch (IOException e) {
             Debug.logError(e, module);
         }
     }

     /** Sets the specified value of the specified property name to the specified resource/properties in memory, does not persist it
      * @param resource The name of the resource
      * @param name The name of the property in the resource
      * @param value The value of the property to set in memory */
      public static void setPropertyValueInMemory(String resource, String name, String value) {
          if (UtilValidate.isEmpty(resource)) {
            return;
        }
          if (UtilValidate.isEmpty(name)) {
            return;
        }

          Properties properties = getProperties(resource);
          if (properties == null) {
              return;
          }
          properties.setProperty(name, value);
      }

    // ========= Locale & Resource Based Methods ==========

    /** Returns the value of the specified property name from the specified
     *  resource/properties file corresponding to the given locale.
     * @param resource The name of the resource - can be a file, class, or URL
     * @param name The name of the property in the properties file
     * @param locale The locale that the given resource will correspond to
     * @return The value of the property in the properties file
     */
    public static String getMessage(String resource, String name, Locale locale) {
        // SCIPIO: This whole method can delegate to NoTrim version.
        String value = getMessageNoTrim(resource, name, locale);
        return value == null ? name : value.trim();
    }

    /** Returns the value of the specified property name from the specified
     *  resource/properties file corresponding to the given locale.
     * <p>
     * SCIPIO: Version that guarantees there be no trim() operation.
     *
     * @param resource The name of the resource - can be a file, class, or URL
     * @param name The name of the property in the properties file
     * @param locale The locale that the given resource will correspond to
     * @return The value of the property in the properties file
     */
    public static String getMessageNoTrim(String resource, String name, Locale locale) {
        if (UtilValidate.isEmpty(resource)) {
            return "";
        }
        if (UtilValidate.isEmpty(name)) {
            return "";
        }

        ResourceBundle bundle = getResourceBundle(resource, locale);

        if (bundle == null) {
            return name;
        }

        String value = null;
        if (bundle.containsKey(name)) {
            value = bundle.getString(name);
        } else {
            Debug.logInfo(name + " misses in " + resource + " for locale " + locale, module);
            return name;
        }
        return value == null ? name : value; // SCIPIO: TODO: REVIEW: some redundancy in this statement?...
    }

    /** Returns the value of the specified property name from the specified resource/properties file corresponding
     * to the given locale and replacing argument place holders with the given arguments using the MessageFormat class
     * @param resource The name of the resource - can be a file, class, or URL
     * @param name The name of the property in the properties file
     * @param arguments An array of Objects to insert into the message argument place holders
     * @param locale The locale that the given resource will correspond to
     * @return The value of the property in the properties file
     */
    public static String getMessage(String resource, String name, Object[] arguments, Locale locale) {
        String value = getMessage(resource, name, locale);

        if (UtilValidate.isEmpty(value)) {
            return "";
        }
        if (arguments != null && arguments.length > 0) {
            value = MessageFormat.format(value, arguments);
        }
        return value;
    }

    /** Returns the value of the specified property name from the specified resource/properties file corresponding
     * to the given locale and replacing argument place holders with the given arguments using the MessageFormat class
     * @param resource The name of the resource - can be a file, class, or URL
     * @param name The name of the property in the properties file
     * @param arguments A List of Objects to insert into the message argument place holders
     * @param locale The locale that the given resource will correspond to
     * @return The value of the property in the properties file
     */
    public static <E> String getMessage(String resource, String name, List<E> arguments, Locale locale) {
        String value = getMessage(resource, name, locale);

        if (UtilValidate.isEmpty(value)) {
            return "";
        }
        if (UtilValidate.isNotEmpty(arguments)) {
            value = MessageFormat.format(value, arguments.toArray());
        }
        return value;
    }

    /** Returns the value of the specified property name from the specified resource/properties file corresponding
     * to the given locale and replacing argument place holders with the given arguments using the MessageFormat class
     * <p>
     * SCIPIO: Version that guarantees there to be no trim() operation.
     *
     * @param resource The name of the resource - can be a file, class, or URL
     * @param name The name of the property in the properties file
     * @param arguments A List of Objects to insert into the message argument place holders
     * @param locale The locale that the given resource will correspond to
     * @return The value of the property in the properties file
     */
    public static <E> String getMessageNoTrim(String resource, String name, List<E> arguments, Locale locale) {
        String value = getMessageNoTrim(resource, name, locale);

        if (UtilValidate.isEmpty(value)) {
            return "";
        }
        if (UtilValidate.isNotEmpty(arguments)) {
            value = MessageFormat.format(value, arguments.toArray());
        }
        return value;
    }

    public static String getMessageList(String resource, String name, Locale locale, Object... arguments) {
        return getMessage(resource, name, arguments, locale);
    }

    /** Returns the value of the specified property name from the specified resource/properties file corresponding
     * to the given locale and replacing argument place holders with the given arguments using the FlexibleStringExpander class
     * @param resource The name of the resource - can be a file, class, or URL
     * @param name The name of the property in the properties file
     * @param context A Map of Objects to insert into the message place holders using the ${} syntax of the FlexibleStringExpander
     * @param locale The locale that the given resource will correspond to
     * @return The value of the property in the properties file
     */
    public static String getMessage(String resource, String name, Map<String, ? extends Object> context, Locale locale) {
        String value = getMessage(resource, name, locale);

        if (UtilValidate.isEmpty(value)) {
            return "";
        }
        if (UtilValidate.isNotEmpty(context)) {
            value = FlexibleStringExpander.expandString(value, context, locale);
        }
        return value;
    }

    /** Returns the value of the specified property name from the specified resource/properties file corresponding
     * to the given locale and replacing argument place holders with the given arguments using the FlexibleStringExpander class
     * <p>
     * SCIPIO: Version that guarantees there to be no trim() operation.
     *
     * @param resource The name of the resource - can be a file, class, or URL
     * @param name The name of the property in the properties file
     * @param context A Map of Objects to insert into the message place holders using the ${} syntax of the FlexibleStringExpander
     * @param locale The locale that the given resource will correspond to
     * @return The value of the property in the properties file
     */
    public static String getMessageNoTrim(String resource, String name, Map<String, ? extends Object> context, Locale locale) {
        String value = getMessageNoTrim(resource, name, locale);

        if (UtilValidate.isEmpty(value)) {
            return "";
        }
        if (UtilValidate.isNotEmpty(context)) {
            value = FlexibleStringExpander.expandString(value, context, locale);
        }
        return value;
    }

    public static String getMessageMap(String resource, String name, Locale locale, Object... context) {
        return getMessage(resource, name, UtilGenerics.toMap(String.class, context), locale);
    }

    private static Set<String> resourceNotFoundMessagesShown = new HashSet<>();
    /** Returns the specified resource/properties file as a ResourceBundle
     * @param resource The name of the resource - can be a file, class, or URL
     * @param locale The locale that the given resource will correspond to
     * @return The ResourceBundle
     */
    public static ResourceBundle getResourceBundle(String resource, Locale locale) {
        if (UtilValidate.isEmpty(resource)) {
            throw new IllegalArgumentException("resource cannot be null or empty");
        }
        if (locale == null) {
            throw new IllegalArgumentException("locale cannot be null");
        }
        ResourceBundle bundle = null;
        try {
            bundle = UtilResourceBundle.getBundle(resource, locale, (ClassLoader) null);
        } catch (MissingResourceException e) {
            String resourceCacheKey = createResourceName(resource, locale, false);
            if (!resourceNotFoundMessagesShown.contains(resourceCacheKey)) {
                resourceNotFoundMessagesShown.add(resourceCacheKey);
                Debug.logInfo("[UtilProperties.getPropertyValue] could not find resource: " + resource + " for locale " + locale, module);
            }
            throw new IllegalArgumentException("Could not find resource bundle [" + resource + "] in the locale [" + locale + "]");
        }
        return bundle;
    }

    /** Returns the specified resource/properties file as a Map with the original
     *  ResourceBundle in the Map under the key _RESOURCE_BUNDLE_
     * @param resource The name of the resource - can be a file, class, or URL
     * @param locale The locale that the given resource will correspond to
     * @return Map containing all entries in The ResourceBundle
     */
    public static ResourceBundleMapWrapper getResourceBundleMap(String resource, Locale locale) {
        return new ResourceBundleMapWrapper(getResourceBundle(resource, locale));
    }

    /** Returns the specified resource/properties file as a Map with the original
     *  ResourceBundle in the Map under the key _RESOURCE_BUNDLE_
     * @param resource The name of the resource - can be a file, class, or URL
     * @param locale The locale that the given resource will correspond to
     * @param context The screen rendering context
     * @return Map containing all entries in The ResourceBundle
     */
    public static ResourceBundleMapWrapper getResourceBundleMap(String resource, Locale locale, Map<String, Object> context) {
        return new ResourceBundleMapWrapper(getResourceBundle(resource, locale), context);
    }

    /** Returns the specified resource/properties file.<p>Note that this method
     * will return a Properties instance for the specified locale <em>only</em> -
     * if you need <a href="http://www.w3.org/International/">I18n</a> properties, then use
     * <a href="#getResourceBundle(java.lang.String,%20java.util.Locale)">
     * getResourceBundle(String resource, Locale locale)</a>. This method is
     * intended to be used primarily by the UtilProperties class.</p>
     * @param resource The name of the resource - can be a file, class, or URL
     * @param locale The desired locale
     * @return The Properties instance, or null if no matching properties are found
     */
    public static Properties getProperties(String resource, Locale locale) {
        if (UtilValidate.isEmpty(resource)) {
            throw new IllegalArgumentException("resource cannot be null or empty");
        }
        if (locale == null) {
            throw new IllegalArgumentException("locale cannot be null");
        }
        Properties properties = null;
        URL url = resolvePropertiesUrl(resource, locale);
        if (url != null) {
            try {
                properties = new ExtendedProperties(url, locale);
            } catch (Exception e) {
                if (UtilValidate.isNotEmpty(e.getMessage())) {
                    Debug.logInfo(e.getMessage(), module);
                } else {
                    Debug.logInfo("Exception thrown: " + e.getClass().getName(), module);
                }
                properties = null;
            }
        }
        if (UtilValidate.isNotEmpty(properties)) {
            if (Debug.verboseOn()) {
                Debug.logVerbose("Loaded " + properties.size() + " properties for: " + resource + " (" + locale + ")", module);
            }
        }
        return properties;
    }

    // ========= Classes and Methods for expanded Properties file support ========== //

    // Private lazy-initializer class
    private static class FallbackLocaleHolder {
        private static final Locale fallbackLocale = getFallbackLocale();

        private static Locale getFallbackLocale() {
            Locale fallbackLocale = null;
            String locale = getPropertyValue("general", "locale.properties.fallback");
            if (UtilValidate.isNotEmpty(locale)) {
                fallbackLocale = UtilMisc.parseLocale(locale);
            }
            if (fallbackLocale == null) {
                fallbackLocale = Locale.ENGLISH;
            }
            return fallbackLocale;
        }
    }

    /** Returns the configured fallback locale. UtilProperties uses this locale
     * to resolve locale-specific XML properties.<p>The fallback locale can be
     * configured using the <code>locale.properties.fallback</code> property in
     * <code>general.properties</code>.
     * @return The configured fallback locale
     */
    public static Locale getFallbackLocale() {
        return FallbackLocaleHolder.fallbackLocale;
    }

    /** Converts a Locale instance to a candidate Locale list. The list
     * is ordered most-specific to least-specific. Example:
     * <code>localeToCandidateList(Locale.US)</code> would return
     * a list containing <code>en_US</code> and <code>en</code>.
     * @return A list of candidate locales.
     */
    public static List<Locale> localeToCandidateList(Locale locale) {
        List<Locale> localeList = new LinkedList<>();
        localeList.add(locale);
        String localeString = locale.toString();
        int pos = localeString.lastIndexOf("_", localeString.length());
        while (pos != -1) {
            localeString = localeString.substring(0, pos);
            localeList.add(new Locale(localeString));
            pos = localeString.lastIndexOf("_", localeString.length());
        }
        return localeList;
    }

    // Private lazy-initializer class
    private static class CandidateLocalesHolder {
        private static Set<Locale> defaultCandidateLocales = getDefaultCandidateLocales();

        private static Set<Locale> getDefaultCandidateLocales() {
            Set<Locale> defaultCandidateLocales = new LinkedHashSet<>();
            defaultCandidateLocales.addAll(localeToCandidateList(Locale.getDefault()));
            defaultCandidateLocales.addAll(localeToCandidateList(getFallbackLocale()));
            defaultCandidateLocales.add(Locale.ROOT);
            return Collections.unmodifiableSet(defaultCandidateLocales);
        }
    }

    /** Returns the default candidate Locale list. The list is populated
     * with the JVM's default locale, the OFBiz fallback locale, and
     * the <code>LOCALE_ROOT</code> (empty) locale - in that order.
     * @return A list of default candidate locales.
     */
    public static Set<Locale> getDefaultCandidateLocales() {
        return CandidateLocalesHolder.defaultCandidateLocales;
    }

    /** Returns a list of candidate locales based on a supplied locale.
     * The returned list consists of the supplied locale and the
     * <a href="#getDefaultCandidateLocales()">default candidate locales</a>
     * - in that order.
     * @param locale The desired locale
     * @return A list of candidate locales
     */
    public static List<Locale> getCandidateLocales(Locale locale) {
        // Java 6 conformance
        if (Locale.ROOT.equals(locale)) {
            return UtilMisc.toList(locale);
        }
        Set<Locale> localeSet = new LinkedHashSet<>();
        localeSet.addAll(localeToCandidateList(locale));
        localeSet.addAll(getDefaultCandidateLocales());
        List<Locale> localeList = new ArrayList<>(localeSet);
        return localeList;
    }

    /** Create a localized resource name based on a resource name and
     * a locale.
     * @param resource The desired resource
     * @param locale The desired locale
     * @param removeExtension Remove file extension from resource String
     * @return Localized resource name
     */
    public static String createResourceName(String resource, Locale locale, boolean removeExtension) {
        String resourceName = resource;
        if (removeExtension) {
            if (resourceName.endsWith(".xml")) {
                resourceName = resourceName.replace(".xml", "");
            } else if (resourceName.endsWith(".properties")) {
                resourceName = resourceName.replace(".properties", "");
            }
        }
        if (locale != null) {
            if (UtilValidate.isNotEmpty(locale.toString())) {
                resourceName = resourceName + "_" + locale;
            }
        }
        return resourceName;
    }

    public static boolean isPropertiesResourceNotFound(String resource, Locale locale, boolean removeExtension) {
        return propertiesNotFound.contains(createResourceName(resource, locale, removeExtension));
    }

    /**
     * Resolve a properties file URL.
     * <p>This method uses the following strategy:</p>
     * <ul>
     *   <li>Locate the XML file specified in <code>resource (MyProps.xml)</code></li>
     *   <li>Locate the file that starts with the name specified in
     *     <code>resource</code> and ends with the locale's string and
     *     <code>.xml (MyProps_en.xml)</code>
     *   </li>
     *   <li>Locate the file that starts with the name specified in
     *     <code>resource</code> and ends with the locale's string and
     *     <code>.properties (MyProps_en.properties)</code>
     *   </li>
     *   <li>Locate the file that starts with the name specified in
     *     <code>resource and ends with the locale's string (MyProps_en)</code>
     *   </li>
     * </ul>
     *
     * The <code>component://</code> protocol is supported in the
     * <code>resource</code> parameter.
     *
     * @param resource The resource to resolve
     * @param locale The desired locale
     * @return A URL instance or null if not found.
     */
    public static URL resolvePropertiesUrl(String resource, Locale locale) {
        if (UtilValidate.isEmpty(resource)) {
            throw new IllegalArgumentException("resource cannot be null or empty");
        }
        String resourceName = createResourceName(resource, locale, false);
        if (propertiesNotFound.contains(resourceName)) {
            return null;
        }
        boolean containsProtocol = resource.contains(":");
        ClassLoader loader = Thread.currentThread().getContextClassLoader();
        URL url = null;
        try {
            // Check for complete URL first
            if (resource.endsWith(".xml") || resource.endsWith(".properties")) {
                if (containsProtocol) {
                    url = FlexibleLocation.resolveLocation(resource, loader);
                } else {
                    url = UtilURL.fromResource(resource, loader);
                }
                if (url != null) {
                    return url;
                }
            }
            // Check for *.properties file
            if (containsProtocol) {
                url = FlexibleLocation.resolveLocation(resourceName + ".properties", loader);
            } else {
                url = UtilURL.fromResource(resourceName + ".properties", loader);
            }
            if (url != null) {
                return url;
            }
            // Check for Java XML properties file
            if (containsProtocol) {
                url = FlexibleLocation.resolveLocation(resourceName + ".xml", loader);
            } else {
                url = UtilURL.fromResource(resourceName + ".xml", loader);
            }
            if (url != null) {
                return url;
            }
            // Check for Custom XML properties file
            if (containsProtocol) {
                url = FlexibleLocation.resolveLocation(resource + ".xml", loader);
            } else {
                url = UtilURL.fromResource(resource + ".xml", loader);
            }
            if (url != null) {
                return url;
            }
            if (containsProtocol) {
                url = FlexibleLocation.resolveLocation(resource, loader);
            } else {
                url = UtilURL.fromResource(resource, loader);
            }
            if (url != null) {
                return url;
            }
        } catch (Exception e) {
            Debug.logInfo("Properties resolver: invalid URL - " + e.getMessage(), module);
        }
        if (propertiesNotFound.size() <= 300) {
            // Sanity check - list could get quite large
            // SCIPIO: 2018-07-18: HashSet is not thread-safe, so can't do this.
            // However, also want to avoid locking globally on this, so use an immutable collection copy.
            // Even omitting volatile since this does not appear critical to record.
            //propertiesNotFound.add(resourceName);
            Set<String> newPropertiesNotFound = new HashSet<>(propertiesNotFound);
            newPropertiesNotFound.add(resourceName);
            propertiesNotFound = Collections.unmodifiableSet(newPropertiesNotFound);
        }
        return null;
    }

    /**
     * Convert XML property file to Properties instance. This method will convert
     * both the Java XML properties file format and the OFBiz custom XML
     * properties file format.
     *
     * <p>The format of the custom XML properties file is:</p>
     * <pre>
     * {@code
     * <resource>
     *     <property key="key">
     *     <value xml:lang="locale 1">Some value</value>
     *     <value xml:lang="locale 2">Some value</value>
     *     ...
     *     </property>
     *     ...
     * </resource>
     * }
     * </pre>
     * where <em>"locale 1", "locale 2"</em> are valid xml:lang values..
     *
     * @param in XML file InputStream
     * @param locale The desired locale
     * @param properties Optional Properties object to populate
     * @return Properties instance or null if not found
     */
    public static Properties xmlToProperties(InputStream in, Locale locale, Properties properties) throws IOException, InvalidPropertiesFormatException {
        if (in == null) {
            throw new IllegalArgumentException("InputStream cannot be null");
        }
        Document doc = null;
        try {
            doc = UtilXml.readXmlDocument(in, true, "XML Properties file");
            in.close();
        } catch (Exception e) {
            Debug.logWarning(e, "XML file for locale " + locale + " could not be loaded.", module);
            in.close();
            return null;
        }
        Element resourceElement = doc.getDocumentElement();
        List<? extends Element> propertyList = UtilXml.childElementList(resourceElement, "property");
        if (UtilValidate.isNotEmpty(propertyList)) {
            // Custom XML properties file format
            if (locale == null) {
                throw new IllegalArgumentException("locale cannot be null");
            }
            String localeString = locale.toString();
            String correctedLocaleString = localeString.replace('_','-');
            for (Element property : propertyList) {
                // Support old way of specifying xml:lang value.
                // Old way: en_AU, new way: en-AU
                Element value = UtilXml.firstChildElement(property, "value", "xml:lang", correctedLocaleString);
                if( value == null ) {
                    value = UtilXml.firstChildElement(property, "value", "xml:lang", localeString);
                }
                if (value != null) {
                    if (properties == null) {
                        properties = new Properties();
                    }
                    String valueString = UtilXml.elementValue(value);
                    if (valueString != null) {
                        properties.put(property.getAttribute("key"), valueString);
                    }
                }
            }
            return properties;
        }
        propertyList = UtilXml.childElementList(resourceElement, "entry");
        if (UtilValidate.isEmpty(propertyList)) {
            throw new InvalidPropertiesFormatException("XML properties file invalid or empty");
        }
        // Java XML properties file format
        for (Element property : propertyList) {
            String value = UtilXml.elementValue(property);
            if (value != null) {
                if (properties == null) {
                    properties = new Properties();
                }
                properties.put(property.getAttribute("key"), value);
            }
        }
        return properties;
    }

    /**
     * SCIPIO: Returns all property names in the given Properties that start with given prefix
     * and end with given suffix, with option to forbid dots in between.
     * Added 2017-07-10.
     */
    public static Set<String> getPropertyNamesWithPrefixSuffix(Properties properties, String prefix, String suffix, boolean allowDots, boolean returnPrefix, boolean returnSuffix) {
        Set<String> names = new HashSet<>();
        int prefixLength = (prefix == null) ? 0 : prefix.length();
        int suffixLength = (suffix == null ? 0 : suffix.length());
        for(String name : properties.stringPropertyNames()) {
            if ((prefix == null || name.startsWith(prefix)) && (suffix == null || name.endsWith(suffix))) {
                String middle = name.substring(prefixLength, name.length() - suffixLength);
                if (allowDots || !middle.contains(".")) {
                    names.add((returnPrefix ? prefix : "") + middle + (returnSuffix ? suffix : ""));
                }
            }
        }
        return names;
    }

    /**
     * SCIPIO: Puts all property name/value pairs in the given Properties that start with given prefix
     * and end with given suffix, with option to forbid dots in between, to the given out map.
     * Added 2017-07-10.
     */
    public static void putPropertiesWithPrefixSuffix(Map<String, ? super String> out, Properties properties, String prefix, String suffix, boolean allowDots, boolean returnPrefix, boolean returnSuffix) {
        int prefixLength = (prefix == null) ? 0 : prefix.length();
        int suffixLength = (suffix == null) ? 0 : suffix.length();
        for(String name : properties.stringPropertyNames()) {
            if ((prefix == null || name.startsWith(prefix)) && (suffix == null || name.endsWith(suffix))) {
                String middle = name.substring(prefixLength, name.length() - suffixLength);
                if (allowDots || !middle.contains(".")) {
                    String value = properties.getProperty(name);
                    if (value != null) value = value.trim();
                    out.put((returnPrefix ? prefix : "") + middle + (returnSuffix ? suffix : ""), value);
                }
            }
        }
    }

    /**
     * SCIPIO: Puts all property name/value pairs in the given Properties that start with given prefix
     * with option to forbid dots in names, to the given out map.
     * Added 2017-07-10.
     */
    public static void putPropertiesWithPrefix(Map<String, ? super String> out, Properties properties, String prefix, boolean allowDots, boolean returnPrefix) {
        int prefixLength = (prefix == null) ? 0 : prefix.length();
        for(String name : properties.stringPropertyNames()) {
            if ((prefix == null || name.startsWith(prefix))) {
                String middle = name.substring(prefixLength, name.length());
                if (allowDots || !middle.contains(".")) {
                    String value = properties.getProperty(name);
                    if (value != null) value = value.trim();
                    out.put((returnPrefix ? prefix : "") + middle, value);
                }
            }
        }
    }

    /**
     * SCIPIO: Puts all property name/value pairs in the given Properties that start with given prefix,
     * stripping the prefix and allowing dots in names, to the given out map.
     * Added 2017-07-10.
     */
    public static void putPropertiesWithPrefix(Map<String, ? super String> out, Properties properties, String prefix) {
        putPropertiesWithPrefix(out, properties, prefix, true, false);
    }

    /**
     * SCIPIO: Gets all property name/value pairs in the given Properties that start with given prefix
     * and end with given suffix, with option to forbid dots in between, in an unordered map.
     * Added 2018-04-27.
     */
    public static Map<String, String> getPropertiesWithPrefixSuffix(Properties properties, String prefix, String suffix, boolean allowDots, boolean returnPrefix, boolean returnSuffix) {
        Map<String, String> out = new HashMap<>();
        putPropertiesWithPrefixSuffix(out, properties, prefix, suffix, allowDots, returnPrefix, returnSuffix);
        return out;
    }

    /**
     * SCIPIO: Gets all property name/value pairs in the given Properties that start with given prefix
     * with option to forbid dots in name, in an unordered map.
     * Added 2018-04-27.
     */
    public static Map<String, String> getPropertiesWithPrefix(Properties properties, String prefix, boolean allowDots, boolean returnPrefix) {
        Map<String, String> out = new HashMap<>();
        putPropertiesWithPrefix(out, properties, prefix, allowDots, returnPrefix);
        return out;
    }

    /**
     * SCIPIO: Gets all property name/value pairs in the given Properties that start with given prefix,
     * stripping the prefix and allowing dots in names, in an unordered map.
     * Added 2018-04-27.
     */
    public static Map<String, String> getPropertiesWithPrefix(Properties properties, String prefix) {
        Map<String, String> out = new HashMap<>();
        putPropertiesWithPrefix(out, properties, prefix, true, false);
        return out;
    }

    /**
     * SCIPIO: Extracts properties having the given prefix and keyed by an ID as the next name part between dots.
     * Added 2017-11.
     */
    public static <T> void extractPropertiesWithPrefixAndId(Map<String, Map<String, T>> out, Properties properties, String prefix) {
        extractPropertiesWithPrefixAndId(out, (Map<?, ?>) properties, prefix);
    }

    /**
     * SCIPIO: Extracts properties having the given prefix and keyed by an ID as the next name part between dots.
     * Added 2017-11.
     */
    @SuppressWarnings("unchecked")
    public static <T> void extractPropertiesWithPrefixAndId(Map<String, Map<String, T>> out, Map<?, ?> properties, String prefix) {
        if (prefix == null) prefix = "";
        for(Map.Entry<?, ?> entry : properties.entrySet()) {
            String name = (String) entry.getKey();
            if (name.startsWith(prefix)) {
                String rest = name.substring(prefix.length());
                int nextDotIndex = rest.indexOf('.');
                if (nextDotIndex > 0 && nextDotIndex < (rest.length() - 1)) {
                    String id = rest.substring(0, nextDotIndex);
                    String subName = rest.substring(nextDotIndex + 1);
                    String value = (String) entry.getValue();
                    Map<String, T> subMap = out.get(id);
                    if (subMap == null) {
                        subMap = new HashMap<>();
                        out.put(id, subMap);
                    }
                    subMap.put(subName, (T) value);
                }
            }
        }
    }

    /**
     * SCIPIO: Puts all property name/value pairs in the given Properties that match the given regexp,
     * with option to return names from the first numbered regexp group.
     * Added 2018-08-23.
     */
    public static void putPropertiesMatching(Map<String, ? super String> out, Properties properties, Pattern nameRegexp, boolean returnFirstGroup) {
        for(String name : properties.stringPropertyNames()) {
            Matcher m = nameRegexp.matcher(name);
            if (m.matches()) {
                String value = properties.getProperty(name);
                if (value != null) value = value.trim();
                out.put(returnFirstGroup ? m.group(1) : name, value);
            }
        }
    }

    /**
     * SCIPIO: Puts all property name/value pairs in the given Properties that match the given regexp.
     * The names are unchanged.
     * Added 2018-08-23.
     */
    public static void putPropertiesMatching(Map<String, ? super String> out, Properties properties, Pattern nameRegexp) {
        putPropertiesMatching(out, properties, nameRegexp, false);
    }

    /**
     * SCIPIO: Gets all property name/value pairs in the given Properties that match the given regexp,
     * with option to return names from the first numbered regexp group.
     * Added 2018-08-23.
     */
    public static Map<String, String> getPropertiesMatching(Properties properties, Pattern nameRegexp, boolean returnFirstGroup) {
        Map<String, String> out = new HashMap<>();
        putPropertiesMatching(out, properties, nameRegexp, returnFirstGroup);
        return out;
    }

    /**
     * SCIPIO: Gets all property name/value pairs in the given Properties that match the given regexp.
     * The names are unchanged.
     * Added 2018-08-23.
     */
    public static Map<String, String>  getPropertiesMatching(Properties properties, Pattern nameRegexp) {
        Map<String, String> out = new HashMap<>();
        putPropertiesMatching(out, properties, nameRegexp, false);
        return out;
    }

    /**
     * SCIPIO: Cleans the given string value, following {@link #getPropertyValue} logic.
     * Added 2018-04-27.
     */
    public static String cleanValue(String value) {
        return value == null ? "" : value.trim();
    }

    /**
     * SCIPIO: Returns the value or null.
     * NOTE: This assumes the string is already trimmed.
     * Added 2018-04-27.
     */
    public static String valueOrNull(String value) {
        return (value == null || value.isEmpty()) ? null : value;
    }

    /**
     * SCIPIO: Converts the given string value to a number type, following {@link #getPropertyNumber} logic.
     * NOTE: This assumes the string is already trimmed.
     * Added 2018-04-27.
     */
    @SuppressWarnings("unchecked")
    public static <N extends Number> N asNumber(Class<N> type, String value, N defaultNumber) {
        if (UtilValidate.isEmpty(value)) {
            return defaultNumber;
        } else {
            try {
                return (N)(ObjectType.simpleTypeConvert(value, type.getSimpleName(), null, null));
            } catch (Exception e) { // SCIPIO: 2018-09-26: use Exception here, because there may be unexpected RuntimeExceptions thrown here
                Debug.logWarning("Error converting String \"" + value + "\" to " + type + "; using defaultNumber " + defaultNumber + ".", module);
            }
            return defaultNumber;
        }
    }

    /**
     * SCIPIO: Converts the given string value to a number type, following {@link #getPropertyNumber} logic.
     * NOTE: This assumes the string is already trimmed.
     * Added 2018-04-27.
     */
    public static <N extends Number> N asNumber(Class<N> type, String value) {
        return asNumber(type, value, null);
    }

    /**
     * SCIPIO: Converts the given value to a number type, following {@link #getPropertyNumber} logic.
     * NOTE: If string, this assumes the string is already trimmed.
     * Added 2018-04-27.
     */
    @SuppressWarnings("unchecked")
    public static <N extends Number> N asNumber(Class<N> type, Object value, N defaultNumber) {
        if (value == null) {
            return defaultNumber;
        } else if (type.isAssignableFrom(value.getClass())) {
            return (N) value;
        } else {
            return asNumber(type, (String) value, defaultNumber);
        }
    }

    /**
     * SCIPIO: Converts the given value to a number type, following {@link #getPropertyNumber} logic.
     * NOTE: If string, this assumes the string is already trimmed.
     * Added 2018-04-27.
     */
    @SuppressWarnings("unchecked")
    public static <N extends Number> N asNumber(Class<N> type, Object value) {
        if (value == null || type.isAssignableFrom(value.getClass())) {
            return (N) value;
        } else {
            return asNumber(type, (String) value, null);
        }
    }

    /**
     * SCIPIO: Converts the given string value to a Boolean type, following {@link #getPropertyAsBoolean} logic.
     * NOTE: This assumes the string is already trimmed.
     * Added 2018-04-27.
     */
    public static Boolean asBoolean(String value, Boolean defaultValue) {
        if ("true".equalsIgnoreCase(value)) {
            return Boolean.TRUE;
        } else if ("false".equalsIgnoreCase(value)) {
            return Boolean.FALSE;
        } else {
            return defaultValue;
        }
    }

    /**
     * SCIPIO: Converts the given string value to a Boolean type, following {@link #getPropertyAsBoolean} logic.
     * NOTE: This assumes the string is already trimmed.
     * Added 2018-04-27.
     */
    public static Boolean asBoolean(String value) {
        return asBoolean(value, null);
    }

    /**
     * SCIPIO: Converts the given value to a Boolean type, following {@link #getPropertyAsBoolean} logic.
     * NOTE: If string, this assumes the string is already trimmed.
     * Added 2018-04-27.
     */
    public static Boolean asBoolean(Object value, Boolean defaultValue) {
        if (value == null) {
            return defaultValue;
        } else if (value instanceof Boolean) {
            return (Boolean) value;
        } else {
            return asBoolean((String) value, defaultValue);
        }
    }

    /**
     * SCIPIO: Converts the given value to a Boolean type, following {@link #getPropertyAsBoolean} logic.
     * NOTE: If string, this assumes the string is already trimmed.
     * Added 2018-04-27.
     */
    public static Boolean asBoolean(Object value) {
        if (value == null || value instanceof Boolean) {
            return (Boolean) value;
        } else {
            return asBoolean((String) value);
        }
    }

    /**
     * SCIPIO: Converts the given string value to a Integer type, following {@link #getPropertyAsInteger} logic.
     * NOTE: This assumes the string is already trimmed.
     * Added 2018-04-27.
     */
    public static Integer asInteger(String value, Integer defaultValue) {
        return asNumber(Integer.class, value, defaultValue);
    }

    /**
     * SCIPIO: Converts the given string value to a Integer type, following {@link #getPropertyAsInteger} logic.
     * NOTE: This assumes the string is already trimmed.
     * Added 2018-04-27.
     */
    public static Integer asInteger(String value) {
        return asInteger(value, null);
    }

    /**
     * SCIPIO: Converts the given value to a Integer type, following {@link #getPropertyAsInteger} logic.
     * NOTE: If string, this assumes the string is already trimmed.
     * Added 2018-04-27.
     */
    public static Integer asInteger(Object value, Integer defaultValue) {
        if (value == null) {
            return defaultValue;
        }  else if (value instanceof Integer) {
            return (Integer) value;
        } else {
            return asInteger((String) value, defaultValue);
        }
    }

    /**
     * SCIPIO: Converts the given value to a Integer type, following {@link #getPropertyAsInteger} logic.
     * NOTE: If string, this assumes the string is already trimmed.
     * Added 2018-04-27.
     */
    public static Integer asInteger(Object value) {
        if (value == null || value instanceof Integer) {
            return (Integer) value;
        } else {
            return asInteger((String) value, null);
        }
    }

    /**
     * SCIPIO: Converts the given string value to a Long type, following {@link #getPropertyAsLong} logic.
     * NOTE: This assumes the string is already trimmed.
     * Added 2018-04-27.
     */
    public static Long asLong(String value, Long defaultValue) {
        return asNumber(Long.class, value, defaultValue);
    }

    /**
     * SCIPIO: Converts the given string value to a Long type, following {@link #getPropertyAsLong} logic.
     * NOTE: This assumes the string is already trimmed.
     * Added 2018-04-27.
     */
    public static Long asLong(String value) {
        return asLong(value, null);
    }

    /**
     * SCIPIO: Converts the given value to a Long type, following {@link #getPropertyAsLong} logic.
     * NOTE: If string, this assumes the string is already trimmed.
     * Added 2018-04-27.
     */
    public static Long asLong(Object value, Long defaultValue) {
        if (value == null) {
            return defaultValue;
        } else if (value instanceof Long) {
            return (Long) value;
        } else {
            return asLong((String) value, defaultValue);
        }
    }

    /**
     * SCIPIO: Converts the given value to a Long type, following {@link #getPropertyAsLong} logic.
     * NOTE: If string, this assumes the string is already trimmed.
     * Added 2018-04-27.
     */
    public static Long asLong(Object value) {
        if (value == null || value instanceof Long) {
            return (Long) value;
        } else {
            return asLong((String) value, null);
        }
    }

    /**
     * SCIPIO: Converts the given string value to a Float type, following {@link #getPropertyAsFloat} logic.
     * NOTE: This assumes the string is already trimmed.
     * Added 2018-04-27.
     */
    public static Float asFloat(String value, Float defaultValue) {
        return asNumber(Float.class, value, defaultValue);
    }

    /**
     * SCIPIO: Converts the given string value to a Float type, following {@link #getPropertyAsFloat} logic.
     * NOTE: This assumes the string is already trimmed.
     * Added 2018-04-27.
     */
    public static Float asFloat(String value) {
        return asFloat(value, null);
    }

    /**
     * SCIPIO: Converts the given value to a Float type, following {@link #getPropertyAsFloat} logic.
     * NOTE: If string, this assumes the string is already trimmed.
     * Added 2018-04-27.
     */
    public static Float asFloat(Object value, Float defaultValue) {
        if (value == null) {
            return defaultValue;
        } else if (value instanceof Float) {
            return (Float) value;
        } else {
            return asFloat((String) value, defaultValue);
        }
    }

    /**
     * SCIPIO: Converts the given value to a Float type, following {@link #getPropertyAsFloat} logic.
     * NOTE: If string, this assumes the string is already trimmed.
     * Added 2018-04-27.
     */
    public static Float asFloat(Object value) {
        if (value == null || value instanceof Float) {
            return (Float) value;
        } else {
            return asFloat((String) value, null);
        }
    }

    /**
     * SCIPIO: Converts the given string value to a Double type, following {@link #getPropertyAsDouble} logic.
     * NOTE: This assumes the string is already trimmed.
     * Added 2018-04-27.
     */
    public static Double asDouble(String value, Double defaultValue) {
        return asNumber(Double.class, value, defaultValue);
    }

    /**
     * SCIPIO: Converts the given string value to a Double type, following {@link #getPropertyAsDouble} logic.
     * NOTE: This assumes the string is already trimmed.
     * Added 2018-04-27.
     */
    public static Double asDouble(String value) {
        return asDouble(value, null);
    }

    /**
     * SCIPIO: Converts the given value to a Double type, following {@link #getPropertyAsDouble} logic.
     * NOTE: If string, this assumes the string is already trimmed.
     * Added 2018-04-27.
     */
    public static Double asDouble(Object value, Double defaultValue) {
        if (value == null) {
            return defaultValue;
        } else if (value instanceof Double) {
            return (Double) value;
        } else {
            return asDouble((String) value, defaultValue);
        }
    }

    /**
     * SCIPIO: Converts the given value to a Double type, following {@link #getPropertyAsDouble} logic.
     * NOTE: If string, this assumes the string is already trimmed.
     * Added 2018-04-27.
     */
    public static Double asDouble(Object value) {
        if (value == null || value instanceof Double) {
            return (Double) value;
        } else {
            return asDouble((String) value, null);
        }
    }

    /**
     * SCIPIO: Converts the given string value to a BigInteger type, following {@link #getPropertyAsBigInteger} logic.
     * NOTE: This assumes the string is already trimmed.
     * Added 2018-04-27.
     */
    public static BigInteger asBigInteger(String value, BigInteger defaultValue) {
        if (UtilValidate.isEmpty(value)) { // SCIPIO: 2018-09-26: don't warn if empty
            return defaultValue;
        }
        BigInteger result = defaultValue;
        try {
            result = new BigInteger(value);
        } catch (NumberFormatException nfe) {
            Debug.logWarning("Couldn't convert String \"" + value + "\" to BigInteger; using defaultNumber " + defaultValue.toString() + ".", module);
        }
        return result;
    }

    /**
     * SCIPIO: Converts the given string value to a BigInteger type, following {@link #getPropertyAsBigInteger} logic.
     * NOTE: This assumes the string is already trimmed.
     * Added 2018-04-27.
     */
    public static BigInteger asBigInteger(String value) {
        return asBigInteger(value, null);
    }

    /**
     * SCIPIO: Converts the given value to a BigInteger type, following {@link #getPropertyAsBigInteger} logic.
     * NOTE: If string, this assumes the string is already trimmed.
     * Added 2018-04-27.
     */
    public static BigInteger asBigInteger(Object value, BigInteger defaultValue) {
        if (value == null) {
            return defaultValue;
        } else if (value instanceof BigInteger) {
            return (BigInteger) value;
        } else {
            return asBigInteger((String) value, defaultValue);
        }
    }

    /**
     * SCIPIO: Converts the given value to a BigInteger type, following {@link #getPropertyAsBigInteger} logic.
     * NOTE: If string, this assumes the string is already trimmed.
     * Added 2018-04-27.
     */
    public static BigInteger asBigInteger(Object value) {
        if (value == null || value instanceof BigInteger) {
            return (BigInteger) value;
        } else {
            return asBigInteger((String) value, null);
        }
    }

    /**
     * SCIPIO: Converts the given string value to a BigDecimal type, following {@link #getPropertyAsBigDecimal} logic.
     * NOTE: This assumes the string is already trimmed.
     * Added 2018-04-27.
     */
    public static BigDecimal asBigDecimal(String value, BigDecimal defaultValue) {
        if (UtilValidate.isEmpty(value)) { // SCIPIO: 2018-09-26: don't warn if empty
            return defaultValue;
        }
        BigDecimal result = defaultValue;
        try {
            result = new BigDecimal(value);
        } catch (NumberFormatException nfe) {
            Debug.logWarning("Couldn't convert String \"" + value + "\" to BigDecimal; using defaultNumber " + defaultValue.toString() + ".", module);
        }
        return result;
    }

    /**
     * SCIPIO: Converts the given string value to a BigDecimal type, following {@link #getPropertyAsBigDecimal} logic.
     * NOTE: This assumes the string is already trimmed.
     * Added 2018-04-27.
     */
    public static BigDecimal asBigDecimal(String value) {
        return asBigDecimal(value, null);
    }

    /**
     * SCIPIO: Converts the given value to a BigDecimal type, following {@link #getPropertyAsBigDecimal} logic.
     * NOTE: If string, this assumes the string is already trimmed.
     * Added 2018-04-27.
     */
    public static BigDecimal asBigDecimal(Object value, BigDecimal defaultValue) {
        if (value == null) {
            return defaultValue;
        } else if (value instanceof BigDecimal) {
            return (BigDecimal) value;
        } else {
            return asBigDecimal((String) value, defaultValue);
        }
    }

    /**
     * SCIPIO: Converts the given value to a BigDecimal type, following {@link #getPropertyAsBigDecimal} logic.
     * NOTE: If string, this assumes the string is already trimmed.
     * Added 2018-04-27.
     */
    public static BigDecimal asBigDecimal(Object value) {
        if (value == null || value instanceof BigDecimal) {
            return (BigDecimal) value;
        } else {
            return asBigDecimal((String) value, null);
        }
    }

    /** Custom ResourceBundle class. This class extends ResourceBundle
     * to add custom bundle caching code and support for the OFBiz custom XML
     * properties file format.
     */
    public static class UtilResourceBundle extends ResourceBundle {
        private static final UtilCache<String, UtilResourceBundle> bundleCache = UtilCache.createUtilCache("properties.UtilPropertiesBundleCache");
        protected Properties properties = null;
        protected Locale locale = null;
        protected int hashCode = hashCode();

        protected UtilResourceBundle() {}

        public UtilResourceBundle(Properties properties, Locale locale, UtilResourceBundle parent) {
            this.properties = properties;
            this.locale = locale;
            setParent(parent);
            String hashString = properties.toString();
            if (parent != null) {
                hashString += parent.properties;
            }
            this.hashCode = hashString.hashCode();
        }

        public static ResourceBundle getBundle(String resource, Locale locale, ClassLoader loader) throws MissingResourceException {
            String resourceName = createResourceName(resource, locale, true);
            UtilResourceBundle bundle = bundleCache.get(resourceName);
            if (bundle == null) {
                double startTime = System.currentTimeMillis();
                List<Locale> candidateLocales = getCandidateLocales(locale);
                UtilResourceBundle parentBundle = null;
                int numProperties = 0;
                while (candidateLocales.size() > 0) {
                    Locale candidateLocale = candidateLocales.remove(candidateLocales.size() - 1);
                    // ResourceBundles are connected together as a singly-linked list
                    String lookupName = createResourceName(resource, candidateLocale, true);
                    UtilResourceBundle lookupBundle = bundleCache.get(lookupName);
                    if (lookupBundle == null) {
                        Properties newProps = getProperties(resource, candidateLocale);
                        if (UtilValidate.isNotEmpty(newProps)) {
                            // The last bundle we found becomes the parent of the new bundle
                            parentBundle = bundle;
                            bundle = new UtilResourceBundle(newProps, candidateLocale, parentBundle);
                            bundleCache.putIfAbsent(lookupName, bundle);
                            numProperties = newProps.size();
                        }
                    } else {
                        parentBundle = bundle;
                        bundle = lookupBundle;
                    }
                }
                if (bundle == null) {
                    throw new MissingResourceException("Resource " + resource + ", locale " + locale + " not found", null, null);
                } else if (!bundle.getLocale().equals(locale)) {
                    // Create a "dummy" bundle for the requested locale
                    bundle = new UtilResourceBundle(bundle.properties, locale, parentBundle);
                }
                double totalTime = System.currentTimeMillis() - startTime;
                if (Debug.infoOn()) {
                    Debug.logInfo("ResourceBundle " + resource + " (" + locale + ") created in " + totalTime / 1000.0 + "s with "
                            + numProperties + " properties", module);
                }
                bundleCache.putIfAbsent(resourceName, bundle);
            }
            return bundle;
        }

        @Override
        public int hashCode() {
            return this.hashCode;
        }

        @Override
        public boolean equals(Object obj) {
            return obj == null ? false : obj.hashCode() == this.hashCode;
        }

        @Override
        public Locale getLocale() {
            return this.locale;
        }

        @Override
        protected Object handleGetObject(String key) {
            return properties.get(key);
        }

        @Override
        public Enumeration<String> getKeys() {
            return new Enumeration<String>() {
                Iterator<String> i = UtilGenerics.cast(properties.keySet().iterator());
                public boolean hasMoreElements() {
                    return (i.hasNext());
                }
                public String nextElement() {
                    return i.next();
                }
            };
        }

    }

    /** Custom Properties class. Extended from Properties to add support
     * for the OFBiz custom XML file format.
     */
    public static class ExtendedProperties extends Properties {
        public ExtendedProperties() {
            super();
        }
        public ExtendedProperties(Properties defaults) {
            super(defaults);
        }
        public ExtendedProperties(URL url, Locale locale) throws IOException, InvalidPropertiesFormatException {
            InputStream in = null;
            try {
                in = new BufferedInputStream(url.openStream());
                if (url.getFile().endsWith(".xml")) {
                    xmlToProperties(in, locale, this);
                } else {
                    load(in);
                }
            } finally {
                if (in != null) {
                    in.close();
                }
            }
        }
        @Override
        public void loadFromXML(InputStream in) throws IOException, InvalidPropertiesFormatException {
            try {
                xmlToProperties(in, null, this);
            } finally {
                in.close();
            }
        }
    }
}
