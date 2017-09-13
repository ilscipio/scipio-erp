package com.ilscipio.solr;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Locale;

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

    static final String solrContentLocalesStr;
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
        List<Locale> locList = new ArrayList<>();
        StringBuilder normLocStr = new StringBuilder();
        try {
            for(String tag : locStr.split("\\s*,\\s*")) {
                Locale locale = UtilMisc.parseLocale(tag);
                if (locale == null) throw new IllegalArgumentException("invalid locale: " + tag);
                if (locList.contains(locale)) {
                    Debug.logWarning("Solr: Configured locale list contains duplicate locales: " + locStr, module);
                    continue;
                }
                locList.add(locale);
                if (normLocStr.length() > 0) normLocStr.append(",");
                normLocStr.append(locale.toString());
            }
            Debug.logInfo("Solr: Configured content locales: " + locStr, module);
        } catch(Exception e) {
            Debug.logError(e, "Solr: Could not parse content locales: " + locStr + ": " + e.getMessage(), module);
            locStr = "en";
            locList = UtilMisc.toList(Locale.ENGLISH);
        }
        solrContentLocalesStr = normLocStr.toString();
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
    public static String getConfiguredLocalesString(Delegator delegator, String productStoreId) {
        return solrContentLocalesStr; 
    }
    
    /**
     * Gets content locales.
     */
    public static String getConfiguredLocalesString(Delegator delegator) {
        return solrContentLocalesStr; 
    }

    /**
     * Gets content locales. FIXME: currently ignores product store!
     */
    public static List<Locale> getConfiguredLocales(Delegator delegator, String productStoreId) {
        return configuredLocales; 
    }
    
    /**
     * Gets content locales. FIXME: currently ignores product store!
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

}
