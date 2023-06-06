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
package org.ofbiz.entity.util;

import java.io.IOException;
import java.io.InputStream;
import java.io.Serializable;
import java.math.BigDecimal;
import java.math.BigInteger;
import java.net.URL;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.InvalidPropertiesFormatException;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Optional;
import java.util.Properties;
import java.util.ResourceBundle;
import java.util.Set;
import java.util.TreeMap;

import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.UtilGenerics;
import org.ofbiz.base.util.UtilMisc;
import org.ofbiz.base.util.UtilProperties;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.base.util.UtilXml;
import org.ofbiz.base.util.collections.ResourceBundleMapWrapper;
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.DelegatorFactory;
import org.ofbiz.entity.GenericEntityException;
import org.ofbiz.entity.GenericValue;
import org.ofbiz.entity.condition.EntityCondition;
import org.ofbiz.entity.condition.EntityOperator;
import org.w3c.dom.Element;

import javax.servlet.ServletContext;

/**
 * Entity-based property lookups based on SystemProperty entity.
 *
 * <p>SCIPIO: 3.0.0: Added {@link #getWebappPropertyValue}.</p>
 */
@SuppressWarnings("serial")
public final class EntityUtilProperties implements Serializable {

    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());

    private EntityUtilProperties () {}

    /**
     * SCIPIO: Returns the value for the given SystemProperty, or null if missing.
     * If the SystemProperty exists, the result even if no value is an empty string;
     * if it does not exist, the result is null.
     * Added 2018-07-27.
     */
    public static String getEntityPropertyValueOrNull(String resource, String name, Delegator delegator) {
        return getEntityPropertyValue(resource, name, delegator); // SCIPIO: Optional
    }

    /**
     * SCIPIO: Returns the value for the given SystemProperty, empty string if set and significant, or null if missing or treated as such.
     * If the SystemProperty exists, the result even if no value is an empty string;
     * if it does not exist, the result is null.
     * @deprecated Use {@link #getEntityPropertyValueOrNull}, which does the same thing.
     * Added 2018-07-27.
     */
    @Deprecated
    public static String getSystemPropertyValueOrNull(String resource, String name, Delegator delegator) {
        return getEntityPropertyValueOrNull(resource, name, delegator);
    }

    /**
     * Gets the given SystemPropertyValue, empty string if set and significant, or null if missing or treated as such.
     *
     * <p>SCIPIO: 3.0.0: Got rid of Optional and use null vs empty string instead.</p>
     * <p>SCIPIO: 2018-08-17: Modified to use null vs "" instead of inappropriate Map.</p>
     */
    private static String getEntityPropertyValue(String resource, String name, Delegator delegator) { // SCIPIO: Renamed from: getSystemPropertyValue
        String result = null;
        if (UtilValidate.isEmpty(resource) || UtilValidate.isEmpty(name)) {
            return null;
        }

        if (delegator == null) { // SCIPIO: 2019-01: Although should rarely happen, there is no reason to crash here
            Debug.logWarning("Missing delegator when querying for entity property [" + resource + "#" + name
                    + "]; treating as not set in database", module);
            return null;
        }

        // SCIPIO: Bad, only replace at end of string
        //resource = resource.replace(".properties", "");
        if (resource.endsWith(".properties")) {
            resource = resource.substring(0, resource.length() - ".properties".length());
        }
        try {
            // SCIPIO: Support for resource name aliases
            EntityCondition resourceAliasCond = ResourceNameAliases.resourceNameAliasConditionMap.get(resource);
            GenericValue systemProperty;
            if (resourceAliasCond != null) {
                systemProperty = EntityQuery.use(delegator)
                        .from("SystemProperty")
                        .where(EntityCondition.makeCondition(resourceAliasCond,
                                EntityOperator.AND,
                                EntityCondition.makeCondition("systemPropertyId", name)))
                        .cache()
                        .queryFirst();
            } else {
                systemProperty = EntityQuery.use(delegator)
                        .from("SystemProperty")
                        .where("systemResourceId", resource, "systemPropertyId", name)
                        .cache()
                        .queryOne();
            }
            if (systemProperty != null) {
                //property exists in database

                // SCIPIO: 2018-07-27: new useEmpty explicit flag
                // NOTE: The default for useEmpty in Scipio is N, while the logical ofbiz 16+ default
                // of this method is Y, so we effectively invert the logic.
                //results.put("isExistInDb", "Y");
                //results.put("value", (systemProperty.getString("systemPropertyValue") != null) ? systemProperty.getString("systemPropertyValue") : "");

                GenericValue systemPropertyEnc = checkEncryptedSystemProperty(delegator, systemProperty);
                GenericValue effProperty = (systemPropertyEnc != null) ? systemPropertyEnc : systemProperty;

                if (effProperty.containsKey("systemPropertyValue")) {
                    String value = effProperty.getStringOrEmpty("systemPropertyValue");
                    if (!value.isEmpty() || Boolean.TRUE.equals(systemProperty.getBoolean("useEmpty"))) {
                        result = value;
                    }
                }
            }
        } catch (GenericEntityException e) {
            Debug.logError("Could not get a system property for " + name + " : " + e.getMessage(), module);
        }
        return result;
    }

    private static GenericValue checkEncryptedSystemProperty(Delegator delegator, GenericValue systemProperty) throws GenericEntityException {
        Object encrypt = systemProperty.get("encrypt");
        boolean encryptCache = "true".equals(encrypt);
        boolean encryptNoCache = !encryptCache && "true-nocache".equals(encrypt);
        if (encryptCache || encryptNoCache) {
            GenericValue systemPropertyEnc = EntityQuery.use(delegator)
                    .from("SystemPropertyEnc")
                    .where("systemResourceId", systemProperty.get("systemResourceId"), "systemPropertyId", systemProperty.get("systemPropertyId"))
                    .cache(encryptCache)
                    .queryOne();
            if (systemPropertyEnc == null) {
                Debug.logError("Misconfigured encrypted system property SystemPropertyEnc systemResourceId [" + systemProperty.get("systemResourceId") +
                        "] systemPropertyId [" + systemProperty.get("systemPropertyId") + "]; needs both SystemProperty and SystemPropertyEnc records (for fast caching)", module);
                return null;
            }
            return systemPropertyEnc;
        }
        return null;
    }

    public static boolean propertyValueEquals(String resource, String name, String compareString) {
        return UtilProperties.propertyValueEquals(resource, name, compareString);
    }

    public static boolean propertyValueEqualsIgnoreCase(String resource, String name, String compareString, Delegator delegator) {
        String s = getEntityPropertyValue(resource, name, delegator); // SCIPIO: Optional
        if (s != null) {
            compareString = (compareString == null) ? "" : compareString;
            return s.equalsIgnoreCase(compareString);
        } else {
            return UtilProperties.propertyValueEqualsIgnoreCase(resource, name, compareString);
        }
    }

    public static String getPropertyValue(String resource, String name, String defaultValue, Delegator delegator) {
        String s = getEntityPropertyValue(resource, name, delegator); // SCIPIO: Optional
        if (s != null) {
            return (UtilValidate.isEmpty(s)) ? defaultValue : s;
        } else {
            return UtilProperties.getPropertyValue(resource, name, defaultValue);
        }
    }

    /**
     * Returns the given servlet attribute or init parameter named resource.name, or if not defined/null, the given entity property value.
     *
     * <p>NOTE: If the name already starts with resource., it is not appended (offsets inconsistency in property settings).</p>
     *
     * <p>NOTE: It is recommended to cache the result of this lookup in a class (by delegator name + webapp context key) since it heavier.</p>
     *
     * <p>SCIPIO: 3.0.0: Added.</p>
     */
    public static String getWebappPropertyValue(String resource, String name, String defaultValue, Delegator delegator, ServletContext servletContext) {
        if (servletContext != null) {
            if (resource.endsWith(".properties")) {
                resource = resource.substring(0, resource.length() - ".properties".length());
            }
            String attrName = (name.startsWith(resource + ".")) ? name : resource + "." + name;
            Object attrValue = servletContext.getAttribute(attrName);
            if (attrValue != null) {
                return attrValue.toString();
            }
            attrValue = servletContext.getInitParameter(attrName);
            if (attrValue != null) {
                return attrValue.toString();
            }
        }
        return getPropertyValue(resource, name, defaultValue, delegator);
    }

    /**
     * Returns the given servlet attribute or init parameter named resource.name, or if not defined/null, the given entity property value.
     *
     * <p>SCIPIO: 3.0.0: Added.</p>
     */
    public static String getWebappPropertyValue(String resource, String name, Delegator delegator, ServletContext servletContext) {
        return getWebappPropertyValue(resource, name, null, delegator, servletContext);
    }

    public static String getPropertyValueFromDelegatorName(String resource, String name, String defaultValue, String delegatorName) {
        Delegator delegator = DelegatorFactory.getDelegator(delegatorName);
        if (delegator == null) { // This should not happen, but in case...
            Debug.logError("Could not get a delegator. Using the 'default' delegator", module);
            // this will be the common case for now as the delegator isn't available where we want to do this
            // we'll cheat a little here and assume the default delegator
            delegator = DelegatorFactory.getDelegator("default");
            Debug.logError("Could not get a delegator. Using the 'default' delegator", module);
            if (delegator == null) {
                Debug.logError("Could not get a system property for " + name + ". Reason: the delegator is null", module);
            }
        }
        String s = getEntityPropertyValue(resource, name, delegator); // SCIPIO: Optional
        if (s != null) {
            return (UtilValidate.isEmpty(s)) ? defaultValue : s;
        } else {
            return UtilProperties.getPropertyValue(resource, name, defaultValue);
        }
    }


    /**
     * getPropertyNumber, as double.
     * <p>
     * SCIPIO: <strong>WARN:</strong> This method is inconsistent; you should use {@link #getPropertyAsDouble(String, String, Double, Delegator)} instead.
     */
    public static double getPropertyNumber(String resource, String name, double defaultValue, Delegator delegator) { // SCIPIO: added 2018-09-26
        String str = getPropertyValue(resource, name, delegator);
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
     * getPropertyNumber, as double.
     * <p>
     * SCIPIO: <strong>WARN:</strong> This method is inconsistent; you should use {@link #getPropertyAsDouble(String, String, Double)} instead.
     */
    @Deprecated
    public static double getPropertyNumber(String resource, String name, double defaultValue) {
        return UtilProperties.getPropertyNumber(resource, name, defaultValue);
    }

    /**
     * getPropertyNumber, as double, with default value 0.00000.
     * <p>
     * SCIPIO: <strong>WARN:</strong> This method is inconsistent; you should use {@link #getPropertyAsDouble(String, String, Double)} instead.
     */
    @Deprecated
    public static double getPropertyNumber(String resource, String name) {
        return UtilProperties.getPropertyNumber(resource, name);
    }

    public static Boolean getPropertyAsBoolean(String resource, String name, Boolean defaultValue, Delegator delegator) { // SCIPIO: added 2018-09-26
        return UtilProperties.asBoolean(getPropertyValue(resource, name, delegator), defaultValue);
    }

    public static Boolean getPropertyAsBoolean(String resource, String name, Boolean defaultValue) {
        return UtilProperties.getPropertyAsBoolean(resource, name, defaultValue);
    }

    public static Integer getPropertyAsInteger(String resource, String name, Integer defaultNumber, Delegator delegator) { // SCIPIO: added 2018-09-26
        return UtilProperties.asInteger(getPropertyValue(resource, name, delegator), defaultNumber);
    }

    public static Integer getPropertyAsInteger(String resource, String name, Integer defaultNumber) {
        return UtilProperties.getPropertyAsInteger(resource, name, defaultNumber);
    }

    public static Long getPropertyAsLong(String resource, String name, Long defaultNumber, Delegator delegator) { // SCIPIO: added 2018-09-26
        return UtilProperties.asLong(getPropertyValue(resource, name, delegator), defaultNumber);
    }

    public static Long getPropertyAsLong(String resource, String name, Long defaultNumber) {
        return UtilProperties.getPropertyAsLong(resource, name, defaultNumber);
    }

    public static Float getPropertyAsFloat(String resource, String name, Float defaultNumber, Delegator delegator) { // SCIPIO: added 2018-09-26
        return UtilProperties.asFloat(getPropertyValue(resource, name, delegator), defaultNumber);
    }

    public static Float getPropertyAsFloat(String resource, String name, Float defaultNumber) {
        return UtilProperties.getPropertyAsFloat(resource, name, defaultNumber);
    }

    public static Double getPropertyAsDouble(String resource, String name, Double defaultNumber, Delegator delegator) { // SCIPIO: added 2018-09-26
        return UtilProperties.asDouble(getPropertyValue(resource, name, delegator), defaultNumber);
    }

    public static Double getPropertyAsDouble(String resource, String name, Double defaultNumber) {
        return UtilProperties.getPropertyAsDouble(resource, name, defaultNumber);
    }

    public static BigInteger getPropertyAsBigInteger(String resource, String name, BigInteger defaultNumber, Delegator delegator) { // SCIPIO: added 2018-09-26
        return UtilProperties.asBigInteger(getPropertyValue(resource, name, delegator), defaultNumber);
    }

    public static BigInteger getPropertyAsBigInteger(String resource, String name, BigInteger defaultNumber) {
        return UtilProperties.getPropertyAsBigInteger(resource, name, defaultNumber);
    }

    public static BigDecimal getPropertyAsBigDecimal(String resource, String name, BigDecimal defaultNumber, Delegator delegator) { // SCIPIO: added 2018-09-26
        return UtilProperties.asBigDecimal(getPropertyValue(resource, name, delegator), defaultNumber);
    }

    public static BigDecimal getPropertyAsBigDecimal(String resource, String name, BigDecimal defaultNumber) {
        return UtilProperties.getPropertyAsBigDecimal(resource, name, defaultNumber);
    }

    public static String getPropertyValue(String resource, String name, Delegator delegator) {
        String s = getEntityPropertyValue(resource, name, delegator); // SCIPIO: Optional
        if (s != null) {
            return s;
        } else {
            return UtilProperties.getPropertyValue(resource, name);
        }
    }

    public static String getPropertyValueFromDelegatorName(String resource, String name, String delegatorName) {
        Delegator delegator = DelegatorFactory.getDelegator(delegatorName);
        if (delegator == null) { // This should not happen, but in case...
            Debug.logError("Could not get a delegator. Using the 'default' delegator", module);
            // this will be the common case for now as the delegator isn't available where we want to do this
            // we'll cheat a little here and assume the default delegator
            delegator = DelegatorFactory.getDelegator("default");
            Debug.logError("Could not get a delegator. Using the 'default' delegator", module);
            if (delegator == null) {
                Debug.logError("Could not get a system property for " + name + ". Reason: the delegator is null", module);
            }
        }
        String s = getEntityPropertyValue(resource, name, delegator); // SCIPIO: Optional
        if (s != null) {
            return s;
        } else {
            return UtilProperties.getPropertyValue(resource, name);
        }
    }

    public static Properties getProperties(String resource) {
        return UtilProperties.getProperties(resource);
    }

    public static Properties getProperties(URL url) {
        return UtilProperties.getProperties(url);
    }

    public static Properties getProperties(Delegator delegator, String resourceName) {
        return getProperties(delegator, resourceName, false);
    }

    public static Properties getProperties(Delegator delegator, String resourceName, boolean includeEncrypted) {
        Properties properties = UtilProperties.getProperties(resourceName);
        List<GenericValue> gvList;
        try {
            gvList = EntityQuery.use(delegator)
                    .from("SystemProperty")
                    .where("systemResourceId", resourceName)
                    .queryList();
            if (UtilValidate.isNotEmpty(gvList)) {
                for (Iterator<GenericValue> i = gvList.iterator(); i.hasNext();) {
                    GenericValue gv = i.next();
                    GenericValue systemPropertyEnc = includeEncrypted ? checkEncryptedSystemProperty(delegator, gv) : null;
                    String value = (systemPropertyEnc != null) ? systemPropertyEnc.getString("systemPropertyValue") : gv.getString("systemPropertyValue");
                    if (UtilValidate.isNotEmpty(value)) {
                        properties.setProperty(gv.getString("systemPropertyId"), value);
                    }
                }
            }
        } catch (GenericEntityException e) {
            Debug.logError(e.getMessage(), module);
        }
        return properties;
    }

    public static boolean propertyValueEquals(URL url, String name, String compareString) {
        return UtilProperties.propertyValueEquals(url, name, compareString);
    }

    public static boolean propertyValueEqualsIgnoreCase(URL url, String name, String compareString) {
        return UtilProperties.propertyValueEqualsIgnoreCase(url, name, compareString);
    }

    public static String getPropertyValue(URL url, String name, String defaultValue) {
        return UtilProperties.getPropertyValue(url, name, defaultValue);
    }

    public static double getPropertyNumber(URL url, String name, double defaultValue) {
        return UtilProperties.getPropertyNumber(url, name, defaultValue);
    }

    public static double getPropertyNumber(URL url, String name) {
        return UtilProperties.getPropertyNumber(url, name);
    }

    public static String getPropertyValue(URL url, String name) {
        return UtilProperties.getPropertyValue(url, name);
    }

    public static String getSplitPropertyValue(URL url, String name) {
        return UtilProperties.getSplitPropertyValue(url, name);
    }

    public static void setPropertyValue(String resource, String name, String value) {
        UtilProperties.setPropertyValue(resource, name, value);
    }

    public static void setPropertyValueInMemory(String resource, String name, String value) {
        UtilProperties.setPropertyValueInMemory(resource, name, value);
    }

    public static String setPropertyValue(Delegator delegator, String resourceName, String name, String value) {
        return setPropertyValue(delegator, resourceName, name, value, null);
    }

    public static String setPropertyValue(Delegator delegator, String resourceName, String name, String value, Boolean useEmpty) {
        return setPropertyValue(delegator, resourceName, name, value, useEmpty, null);
    }

    public static String setPropertyValue(Delegator delegator, String resourceName, String name, String value, Boolean useEmpty, String encrypt) {
        if (encrypt != null) {
            throw new UnsupportedOperationException(); // TODO: secondary record creation
        }
        GenericValue gv = null;
        String prevValue = null;
        try {
            gv = EntityQuery.use(delegator)
                    .from("SystemProperty")
                    .where("systemResourceId", resourceName, "systemPropertyId", name)
                    .queryOne();
            if (gv != null) {
                prevValue = gv.getString("systemPropertyValue");
                gv.set("systemPropertyValue", value);
                if (useEmpty != null) {
                    gv.set("useEmpty", useEmpty ? "Y" : "N");
                }
                gv.store();
            } else {
                gv = delegator.makeValue("SystemProperty", UtilMisc.toMap("systemResourceId", resourceName, "systemPropertyId", name, "systemPropertyValue", value, "description", null));
                if (useEmpty != null) {
                    gv.set("useEmpty", useEmpty ? "Y" : "N");
                }
                gv = gv.create();
            }
        } catch (GenericEntityException e) {
            Debug.logError(String.format("tenantId=%s, exception=%s, message=%s", delegator.getDelegatorTenantId(), e.getClass().getName(), e.getMessage()), module);
        }
        return prevValue;
    }

    public static String getMessage(String resource, String name, Locale locale, Delegator delegator) {
        String s = getEntityPropertyValue(resource, name, delegator); // SCIPIO: Optional
        if (s != null) {
            return s;
        } else {
            return UtilProperties.getMessage(resource, name, locale);
        }
    }

    public static String getMessage(String resource, String name, Object[] arguments, Locale locale) {
        return UtilProperties.getMessage(resource, name, arguments, locale);
    }

    public static <E> String getMessage(String resource, String name, List<E> arguments, Locale locale) {
        return UtilProperties.getMessage(resource, name, arguments, locale);
    }

    public static String getMessageList(String resource, String name, Locale locale, Object... arguments) {
        return UtilProperties.getMessageList(resource, name, locale, arguments);
    }

    public static String getMessage(String resource, String name, Map<String, ? extends Object> context, Locale locale) {
        return UtilProperties.getMessage(resource, name, context, locale);
    }

    public static String getMessageMap(String resource, String name, Locale locale, Object... context) {
        return UtilProperties.getMessageMap(resource, name, locale, context);
    }

    public static ResourceBundle getResourceBundle(String resource, Locale locale) {
        return UtilProperties.getResourceBundle(resource, locale);
    }

    public static ResourceBundleMapWrapper getResourceBundleMap(String resource, Locale locale) {
        return UtilProperties.getResourceBundleMap(resource, locale);
    }

    public static ResourceBundleMapWrapper getResourceBundleMap(String resource, Locale locale, Map<String, Object> context) {
        return UtilProperties.getResourceBundleMap(resource, locale, context);
    }

    public static ResourceBundle getGlobalResourceBundle(Locale locale) { // SCIPIO
        return UtilProperties.getGlobalResourceBundle(locale);
    }

    public static ResourceBundleMapWrapper getGlobalResourceBundleMap(Locale locale, Map<String, Object> context) { // SCIPIO
        return UtilProperties.getGlobalResourceBundleMap(locale, context);
    }

    public static Properties getProperties(String resource, Locale locale) {
        return UtilProperties.getProperties(resource, locale);
    }

    @Deprecated
    public static Locale getFallbackLocale() {
        return UtilProperties.getFallbackLocale();
    }

    public static List<Locale> localeToCandidateList(Locale locale) {
        return UtilProperties.localeToCandidateList(locale);
    }

    public static Set<Locale> getDefaultCandidateLocales() {
        return UtilProperties.getDefaultCandidateLocales();
    }

    @Deprecated
    public static List<Locale> getCandidateLocales(Locale locale) {
        return UtilProperties.getCandidateLocales(locale);
    }

    public static String createResourceName(String resource, Locale locale, boolean removeExtension) {
        return UtilProperties.createResourceName(resource, locale, removeExtension);
    }

    public static boolean isPropertiesResourceNotFound(String resource, Locale locale, boolean removeExtension) {
        return UtilProperties.isPropertiesResourceNotFound(resource, locale, removeExtension);
    }

    public static URL resolvePropertiesUrl(String resource, Locale locale) {
        return UtilProperties.resolvePropertiesUrl(resource, locale);
    }

    public static Properties xmlToProperties(InputStream in, Locale locale, Properties properties) throws IOException, InvalidPropertiesFormatException {
        return UtilProperties.xmlToProperties(in, locale, properties);
    }

    /** Reads properties from LocalizedProperty entity (SCIPIO). */
    public static Properties entityResourceToProperties(String resource, Locale locale, Properties properties, Delegator delegator, boolean useCache) {
        if (delegator == null) { // FIXME: default delegator fallback
            delegator = DelegatorFactory.getDefaultDelegator();
        }
        if (locale == null) {
            throw new IllegalArgumentException("locale cannot be null");
        }
        resource = UtilProperties.normResourceName(resource);
        String localeString = locale.toString();
        String correctedLocaleString = localeString.replace('_','-');

        List<GenericValue> propertyList = delegator.from("LocalizedProperty").where(
                EntityCondition.makeCondition("resourceId", resource),
                EntityCondition.makeCondition(EntityCondition.makeCondition("lang", localeString), EntityOperator.OR,
                        EntityCondition.makeCondition("lang", correctedLocaleString))).orderBy("propertyId").cache(useCache).queryListSafe();
        if (UtilValidate.isNotEmpty(propertyList)) {
            for (GenericValue property : propertyList) {
                Object value = property.get("value");
                boolean useEmpty = property.getBoolean("useEmpty", false);
                if (useEmpty || (value instanceof String && !((String) value).isEmpty())) {
                    properties.put(property.get("propertyId"), property.get("value"));
                }
            }
        }
        return properties;
    }

    /** Reads properties from LocalizedProperty entity (SCIPIO). */
    public static Properties entityResourceToProperties(String resourceName, Locale locale, Properties properties, Object delegator, boolean useCache) {
        return entityResourceToProperties(resourceName, locale, properties, (Delegator) delegator, useCache);
    }

    /** Reads properties from LocalizedProperty entity (SCIPIO). */
    public static Map<String, Map<String, String>> entityResourceToLocalePropertyMap(String resource, boolean sort, Delegator delegator, boolean useCache, Map<String, Map<String, String>> out) {
        if (delegator == null) { // FIXME: default delegator fallback
            delegator = DelegatorFactory.getDefaultDelegator();
        }
        resource = UtilProperties.normResourceName(resource);
        List<GenericValue> propertyList = delegator.from("LocalizedProperty").where("resourceId", resource).orderBy("propertyId").cache(useCache).queryListSafe();
        if (UtilValidate.isNotEmpty(propertyList)) {
            for (GenericValue property : propertyList) {
                String value = property.getString("value");
                boolean useEmpty = property.getBoolean("useEmpty", false);
                if (useEmpty || UtilValidate.isNotEmpty(value)) {
                    String propertyId = property.getString("propertyId");
                    String lang = property.getString("lang");
                    Map<String, String> propMap = UtilGenerics.cast(out.get(propertyId));
                    boolean newMap = false;
                    if (propMap == null) {
                        propMap = sort ? new TreeMap<>() : new LinkedHashMap<>();
                        newMap = true;
                    }
                    propMap.put(lang, value);
                    if (newMap) {
                        out.put(propertyId, propMap);
                    }
                }
            }
        }
        return out;
    }

    /** Reads properties from LocalizedProperty entity (SCIPIO). */
    public static Map<String, Map<String, String>> entityResourceToLocalePropertyMap(String resource, boolean sort, Object delegator, boolean useCache, Map<String, Map<String, String>> out) throws IOException, InvalidPropertiesFormatException {
        return entityResourceToLocalePropertyMap(resource, sort, (Delegator) delegator, useCache, out);
    }

    /**
     * SCIPIO: Resource name alias support core handling.
     * <p>
     * Added 2018-10-02.
     */
    private static class ResourceNameAliases {
        static final Map<String, EntityCondition> resourceNameAliasConditionMap = readResourceNameAliasConditionMap();

        /**
         * SCIPIO: Pre-builds lookup conditions for resource name aliases
         * (this is the best can optimize this without adding an extra cache layer).
         */
        static Map<String, EntityCondition> readResourceNameAliasConditionMap() {
            Map<String, EntityCondition> condMap = new HashMap<>();
            for(Map.Entry<String, List<String>> entry : UtilProperties.getResourceNameAliasAndReverseAliasMap().entrySet()) {
                List<String> aliases = entry.getValue();
                List<EntityCondition> condList = new ArrayList<>(aliases.size() + 1);
                condList.add(EntityCondition.makeCondition("systemResourceId", entry.getKey()));
                for(String alias : aliases) {
                    condList.add(EntityCondition.makeCondition("systemResourceId", alias));
                }
                condMap.put(entry.getKey(), EntityCondition.makeCondition(condList, EntityOperator.OR));
            }
            return condMap;
        }
    }
}
