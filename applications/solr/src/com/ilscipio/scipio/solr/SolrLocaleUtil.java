package com.ilscipio.scipio.solr;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Locale;
import java.util.Set;

import org.apache.commons.lang.StringUtils;
import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.UtilMisc;
import org.ofbiz.base.util.UtilProperties;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.GenericValue;

public abstract class SolrLocaleUtil {

    public static final String module = SolrLocaleUtil.class.getName();
    
    protected SolrLocaleUtil() {
    }

    static final List<Locale> configuredLocales;
    static {
        String locStr = UtilProperties.getPropertyValue(SolrUtil.solrConfigName, "solr.content.locales");
        if (locStr != null) locStr = locStr.trim();
        if (UtilValidate.isEmpty(locStr)) {
            locStr = UtilProperties.getPropertyValue("general", "locales.available");
        }
        if (UtilValidate.isEmpty(locStr)) {
            locStr = "en";
        }
        List<Locale> locList = parseCompatibleLocalesValid(locStr);
        if (locList.size() == 0) {
            locList.add(Locale.ENGLISH);
        }
        Debug.logInfo("Solr: Configured content locales: " + joinLocales(locList, ","), module);
        configuredLocales = Collections.unmodifiableList(locList);
    }
    
    static final Locale configuredDefaultLocale;
    static {
        Locale locale = null;
        try {
            String locStr = UtilProperties.getPropertyValue(SolrUtil.solrConfigName, "solr.content.locales.default");
            if (UtilValidate.isNotEmpty(locStr)) {
                locale = UtilMisc.parseLocale(locStr);
            }
        } catch(Exception e) {
            Debug.logError("Solr: Error reading default locale: " + e.getMessage(), module);
        }
        if (locale == null) locale = Locale.getDefault();
        locale = SolrLocaleUtil.getCompatibleLocale(locale);
        if (!configuredLocales.contains(locale)) {
            Locale firstLocale = configuredLocales != null ? configuredLocales.get(0) : null;
            Debug.logWarning("Solr: Configured content locale default/fallback (" + locale 
                    + ") is not present in solr locales list (you may need extra configuration)! Using first in list as default instead: " + firstLocale, module);
            locale = firstLocale;
        } else {
            Debug.logInfo("Solr: Configured content locale default/fallback: " + locale.toString(), module);
        }
        configuredDefaultLocale = locale;
    }
    
    // not currently useful
    //public static final boolean SOLR_CONTENT_LOCALES_REQUIREALL = UtilProperties.getPropertyAsBoolean(solrConfigName, "solr.content.locales.requireAll", false);

    /**
     * Gets content locales. FIXME: currently ignores product store!
     */
    public static List<Locale> getConfiguredLocales(Delegator delegator, String productStoreId) {
        return configuredLocales; 
    }
    
    /**
     * Gets content locales.
     */
    public static List<Locale> getConfiguredLocales(Delegator delegator) {
        return configuredLocales; 
    }

    /**
     * Gets default content locale. FIXME: currently ignores product store!
     */
    public static Locale getConfiguredDefaultLocale(Delegator delegator, String productStoreId) {
        return configuredDefaultLocale; 
    }
    
    /**
     * Gets default content locale.
     */
    public static Locale getConfiguredDefaultLocale(Delegator delegator) {
        return configuredDefaultLocale; 
    }

    /**
     * Tries to return a field language code for the solr schema for the locale.
     * For "en_US", returns the "en" part.
     * TODO: REVIEW: sketchy
     */
    public static String getLangCode(Locale locale) {
        if (locale == null) return null;
        return locale.getLanguage();
    }

    public static String getLangCodeValid(Locale locale) {
        String res = getLangCode(locale);
        if (configuredLocales.contains(Locale.forLanguageTag(res))) return res;
        else return null;
    }

    public static String getLangCodeValidOrDefault(Locale locale) {
        String res = getLangCodeValid(locale);
        return res != null ? res : getLangCode(configuredDefaultLocale);
    }

    /**
     * Tries to return a field language locale for the solr schema for the locale.
     * For "en_US", returns "en" locale.
     * DOES NOT VALIDATE that is part of the configured locales.
     * TODO: REVIEW: sketchy
     */
    public static Locale getCompatibleLocale(Locale locale) {
        return (locale == null) ? null : Locale.forLanguageTag(getLangCode(locale));
    }

    public static Locale getCompatibleLocaleValid(Locale locale) {
        Locale res = getCompatibleLocale(locale);
        if (configuredLocales.contains(res)) return res;
        else return null;
    }
    
    public static Locale getCompatibleLocaleValidOrDefault(Locale locale) {
        Locale res = getCompatibleLocaleValid(locale);
        return res != null ? res : configuredDefaultLocale; 
    }

    public static Locale getCompatibleLocaleValidOrDefault(Locale locale, Locale fallbackLocale) {
        Locale res = getCompatibleLocaleValid(locale);
        if (res != null) return res;
        res = getCompatibleLocaleValid(fallbackLocale);
        return res != null ? res : configuredDefaultLocale; 
    }

    public static Locale getCompatibleProductStoreLocaleValid(GenericValue productStore) {
        return getCompatibleLocaleValid(productStore != null ? UtilMisc.parseLocale(productStore.getString("defaultLocaleString")) : null);
    }
    
    public static Locale getCompatibleProductStoreLocaleValidOrDefault(GenericValue productStore) {
        Locale res = getCompatibleProductStoreLocaleValid(productStore);
        return res != null ? res : configuredDefaultLocale; 
    }
    
    public static Locale getCompatibleLocaleValidOrProductStoreDefault(Locale locale, GenericValue productStore) {
        Locale res = getCompatibleLocaleValid(locale);
        if (res != null) return res;
        res = getCompatibleProductStoreLocaleValid(productStore);
        return res != null ? res : configuredDefaultLocale; 
    }

    public static List<Locale> parseCompatibleLocalesValid(String locStr, boolean allowSpecial) {
        List<Locale> locList = new ArrayList<>();
        if (UtilValidate.isNotEmpty(locStr)) {
            try {
                for(String tag : locStr.split("\\s*,\\s*")) {
                    Locale locale;
                    if (allowSpecial && "general".equals(tag)) {
                        locale = new Locale("general"); // FAKE locale
                    } else {
                        locale = UtilMisc.parseLocale(tag);
                    }
                    if (locale == null) throw new IllegalArgumentException("invalid locale: " + tag);
                    locList.add(locale);
                }
            } catch(Exception e) {
                Debug.logError(e, "Solr: Could not parse content locales: " + locStr + ": " + e.getMessage(), module);
            }
        }
        return locList;
    }
    
    public static List<Locale> parseCompatibleLocalesValid(String locStr) {
        return parseCompatibleLocalesValid(locStr, false);
    }
    
    public static List<Locale> parseCompatibleLocalesValidSpecial(String locStr) {
        return parseCompatibleLocalesValid(locStr, true);
    }

    public static String joinLocales(Collection<Locale> locales, String delim) {
        StringBuilder sb = new StringBuilder();
        for(Locale locale : locales) {
            sb.append(delim);
            sb.append(locale.toString());
        }
        return (sb.length() > 0) ? sb.substring(delim.length()) : "";
    }
    
    public static Set<String> determineI18nQueryFieldsForUserLocale(Locale userLocale, GenericValue productStore, 
            Collection<Locale> forceLocales, String fieldPrefix, String userLocalePower, String forceLocalePower) {
        Set<String> fields = new LinkedHashSet<>();
        
        Locale locale = getCompatibleLocaleValid(userLocale);
        Locale storeLocale = SolrLocaleUtil.getCompatibleProductStoreLocaleValid(productStore);
        
        if (locale == null) locale = storeLocale; // just in case; usually doesn't happen
        
        if (locale == null) {
            // NOTE: this shouldn't really happen unless store is misconfigured
            fields.add(fieldPrefix + "general");
        } else {
            if (userLocalePower == null) userLocalePower = "";
            
            // add user (or store) locale
            fields.add(fieldPrefix + locale.toString() + userLocalePower);
    
            // if user locale is same as store locale, then also search general, because it should be in default store language
            if (storeLocale != null && locale.toString().equals(storeLocale.toString())) {
                fields.add(fieldPrefix + "general" + userLocalePower);
            }
        }
        addAllI18nQueryFields(fields, forceLocales, fieldPrefix, forceLocalePower);
        return fields;
    }
    
    public static Set<String> determineI18nQueryFieldsForUserAndStoreLocale(Locale userLocale, GenericValue productStore, 
            Collection<Locale> forceLocales, String fieldPrefix, String userLocalePower, String storeLocalePower, String forceLocalePower) {
        Set<String> fields = new LinkedHashSet<>();
 
        Locale locale = getCompatibleLocaleValid(userLocale);
        Locale storeLocale = SolrLocaleUtil.getCompatibleProductStoreLocaleValid(productStore);
        
        if (locale == null) locale = storeLocale; // just in case; usually doesn't happen

        if (locale == null) {
            // NOTE: this shouldn't really happen unless store is misconfigured
            fields.add(fieldPrefix + "general");
        } else {
            if (userLocalePower == null) userLocalePower = "";
            
            if (storeLocale == null || locale.toString().equals(storeLocale.toString())) {
                // NOTE: am adding userLocalePower here always in case the result set is combined with other stuff
                
                // add user locale
                fields.add(fieldPrefix + locale.toString() + userLocalePower);
                
                // add general
                fields.add(fieldPrefix + "general" + userLocalePower);
            } else {
                if (storeLocalePower == null) storeLocalePower = "";
                
                // add user locale, with power 
                fields.add(fieldPrefix + locale.toString() + userLocalePower);
                
                // add store locale
                fields.add(fieldPrefix + storeLocale.toString() + storeLocalePower);
                
                // add general
                fields.add(fieldPrefix + "general" + storeLocalePower);
            }
        }
        
        addAllI18nQueryFields(fields, forceLocales, fieldPrefix, forceLocalePower);
        return fields;
    }


    public static void addAllI18nQueryFields(Set<String> out, Collection<Locale> locales, String fieldPrefix, String fieldSuffix, boolean checkCompatible) {
        if (locales == null) return;
        if (fieldPrefix == null) fieldPrefix = "";
        if (fieldSuffix == null) fieldSuffix = "";
        for(Locale locale : locales) {
            if (checkCompatible) locale = getCompatibleLocaleValid(locale);
            if (locale != null) out.add(fieldPrefix + locale.toString());
        }
    }
    
    /**
     * WARN: assumes locales already compatible
     */
    public static void addAllI18nQueryFields(Set<String> out, Collection<Locale> locales, String fieldPrefix, String fieldSuffix) {
        addAllI18nQueryFields(out, locales, fieldPrefix, fieldSuffix, false);
    }
}
