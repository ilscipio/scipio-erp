package com.ilscipio.scipio.solr;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Locale;
import java.util.Set;

import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.UtilMisc;
import org.ofbiz.base.util.UtilProperties;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.GenericValue;

/**
 * Utilities to manipulate Locales and to make them work with the solr schema locales.
 * <p>
 * NOTE: Most methods deal with content locales of titles, descriptions, etc. rather than
 * system locale.
 */
public abstract class SolrLocaleUtil {

    public static final String module = SolrLocaleUtil.class.getName();
    
    /**
     * Name used as a special value in place of locale string to designate the default or "general"
     * field for content values not explicitly given a locale (in the original entities).
     * In other words the "*_i18n_general" fields in the solr schema.
     */
    public static final String I18N_GENERAL = "general";
    
    private static final String spellcheckI18nDictBaseName = UtilProperties.getPropertyValue(SolrUtil.solrConfigName, "solr.spellcheck.localDictBaseName", "default");

    protected SolrLocaleUtil() {
    }

    static final List<Locale> configuredLocales;
    static { // NOTE: MUST do this one first
        String locStr = UtilProperties.getPropertyValue(SolrUtil.solrConfigName, "solr.content.locales");
        if (UtilValidate.isEmpty(locStr)) {
            locStr = UtilProperties.getPropertyValue("general", "locales.available");
        }
        List<Locale> locList = parseCompatibleLocalesValid(locStr);
        if (locList.size() == 0) {
            Debug.logWarning("Solr: No content locales configured in solrconfig.properties or general.properties"
                    + " - will be english-only!", module);
            locList.add(getCompatibleLocale(Locale.ENGLISH)); // fallback only
        }
        Debug.logInfo("Solr: Configured content locales: " + joinLocales(locList, ","), module);
        configuredLocales = Collections.unmodifiableList(locList);
    }
    
    static final Locale configuredFallbackDefaultLocale;
    static {
        Locale locale = getCompatiblePropertyLocaleValid(SolrUtil.solrConfigName, "solr.content.locales.default.fallback", null);
        if (locale == null) {
            locale = getCompatiblePropertyLocaleValid("general", "locale.properties.fallback", null);
        }
        if (locale == null) {
            locale = configuredLocales.get(0);
            Debug.logWarning("Solr: No default content locale configured in solrconfig.properties or general.properties"
                    + " - using first from solr.content.locales list: " + locale, module);
             // do not use Locale.getDefault because frequently doesn't reflect stored content descriptions
        }
        Debug.logInfo("Solr: Configured content locale default/fallback: " + locale, module);
        configuredFallbackDefaultLocale = locale;
    }
    
    static final Locale configuredForceDefaultLocale;
    static {
        Locale locale = getCompatiblePropertyLocaleValid(SolrUtil.solrConfigName, "solr.content.locales.default.force", null);
        Debug.logInfo("Solr: Configured content locale force-default/fallback: " + (locale != null ? locale : "(none)"), module);
        configuredForceDefaultLocale = locale;
    }
    
    // not currently useful
    //public static final boolean SOLR_CONTENT_LOCALES_REQUIREALL = UtilProperties.getPropertyAsBoolean(solrConfigName, "solr.content.locales.requireAll", false);

    /**
     * Gets content locales.
     * NOTE: Currently (2017-09-14) this never uses product store configuration for this, but you should
     * pass the productStore anyway for future use.
     */
    public static List<Locale> getConfiguredLocales(Delegator delegator, GenericValue productStore) {
        return configuredLocales; 
    }

    /**
     * Gets content locales.
     * NOTE: Currently (2017-09-14) this never uses product store configuration for this, but you should
     * pass the productStore anyway for future use.
     */
    public static List<Locale> getConfiguredLocales(GenericValue productStore) {
        return getConfiguredLocales(null, productStore); 
    }
    
    /**
     * Gets default content locale for the store.
     * This is the locale that the inline titles and descriptions that do have an explicit locale
     * should be written in (such as Product.description entity field or Content records with no localeString).
     * Uses forced default first, then ProductStore.defaultLocaleString, then fallback default, in that order.
     */
    public static Locale getConfiguredDefaultLocale(Delegator delegator, GenericValue productStore) {
        if (configuredForceDefaultLocale != null) return configuredForceDefaultLocale;
        if (productStore != null) {
            Locale storeLocale = getCompatibleProductStoreLocaleStrictValid(productStore);
            if (storeLocale != null) return storeLocale;
        }
        return configuredFallbackDefaultLocale; 
    }
    
    /**
     * Gets default content locale for the store.
     * This is the locale that the inline titles and descriptions that do have an explicit locale
     * should be written in (such as Product.description entity field or Content records with no localeString).
     * Uses forced default first, then ProductStore.defaultLocaleString, then fallback default, in that order.
     */
    public static Locale getConfiguredDefaultLocale(GenericValue productStore) {
        return getConfiguredDefaultLocale(null, productStore);
    }
    
    /**
     * Gets the force default content locale (only).
     * WARN: Avoid using in client code.
     */
    public static Locale getConfiguredForceDefaultLocale(Delegator delegator, GenericValue productStore) {
        return configuredForceDefaultLocale; 
    }
    
    /**
     * Gets the force default content locale (only).
     * WARN: Avoid using in client code.
     */
    public static Locale getConfiguredForceDefaultLocale(GenericValue productStore) {
        return getConfiguredForceDefaultLocale(null, productStore); 
    }
    
    /**
     * Gets the fallback default content locale (only).
     * WARN: Avoid using in client code.
     */
    public static Locale getConfiguredFallbackDefaultLocale(Delegator delegator, GenericValue productStore) {
        return configuredFallbackDefaultLocale; 
    }
    
    /**
     * Gets the fallback default content locale (only).
     * WARN: Avoid using in client code.
     */
    public static Locale getConfiguredFallbackDefaultLocale(GenericValue productStore) {
        return getConfiguredFallbackDefaultLocale(null, productStore); 
    }

    /**
     * Tries to return a field language code for the solr schema for the locale.
     * For "en_XX", returns the "en" part.
     */
    public static String getLangCode(Locale locale) {
        assert(locale != null);
        // TODO?: REVIEW: is getLanguage appropriate? uses old codes; have to check against solr...
        return locale.getLanguage();
    }

    public static boolean isSameLangCode(Locale first, Locale second) {
        return first != null && second != null && getLangCode(first).equals(getLangCode(second));
    }

    /**
     * Tries to return a field language locale for the solr schema for the locale.
     * For "en_US", returns "en" locale.
     * DOES NOT VALIDATE that is part of the configured locales.
     */
    public static Locale getCompatibleLocale(Locale locale) {
        return (locale == null) ? null : new Locale(getLangCode(locale));
    }
    
    public static Locale getCompatibleLocale(String localeString) {
        // TODO: REVIEW: UtilMisc.parseLocale may not work with the 3-letter codes???
        // may need extra parse??
        Locale locale = UtilMisc.parseLocale(localeString);
        if (locale == null) return null;
        else return new Locale(getLangCode(locale));
    }

    /**
     * Returns a solr-schema-compatible Locale that is also valid for the given store, or null.
     * It does NOT return the store default locale.
     */
    public static Locale getCompatibleLocaleValid(Locale locale, GenericValue productStore) {
        Locale res = getCompatibleLocale(locale);
        if (getConfiguredLocales(productStore).contains(res)) return res;
        else return null;
    }
    
    public static Locale getCompatibleLocaleValid(String localeString, GenericValue productStore) {
        Locale res = getCompatibleLocale(localeString);
        if (getConfiguredLocales(productStore).contains(res)) return res;
        else return null;
    }
    
    /**
     * Returns a solr-schema-compatible Locale that is also valid for the given store, or if not valid,
     * returns the store's configured default (convenience method).
     * @see #getConfiguredDefaultLocale(GenericValue)
     */
    public static Locale getCompatibleLocaleValidOrDefault(Locale locale, GenericValue productStore) {
        Locale res = getCompatibleLocaleValid(locale, productStore);
        return res != null ? res : getConfiguredDefaultLocale(productStore); 
    }

    public static Locale getCompatibleLocaleValidOrDefault(Locale locale, Locale fallbackLocale, GenericValue productStore) {
        Locale res = getCompatibleLocaleValid(locale, productStore);
        if (res != null) return res;
        res = getCompatibleLocaleValid(fallbackLocale, productStore);
        return res != null ? res : getConfiguredDefaultLocale(productStore); 
    }

    /**
     * Returns the product store locale or forced default, or null if store has none or not valid for solr schema.
     * Unlike {@link #getConfiguredDefaultLocale(GenericValue)}, does not return the fallback-default.
     */
    public static Locale getCompatibleProductStoreLocaleValid(GenericValue productStore) {
        if (getConfiguredForceDefaultLocale(productStore) != null) return getConfiguredForceDefaultLocale(productStore);
        return getCompatibleProductStoreLocaleStrictValid(productStore);
    }
    
    /**
     * Returns the product store locale itself, or null if store has none or not valid for solr schema.
     * Bypasses the force and fallback defaults.
     * WARN: Avoid using this in client code! Special cases only.
     */
    public static Locale getCompatibleProductStoreLocaleStrictValid(GenericValue productStore) {
        if (productStore == null) return null;
        return getCompatibleLocaleValid(productStore.getString("defaultLocaleString"), productStore);
    }
    
    /**
     * @deprecated use {@link #getConfiguredDefaultLocale(GenericValue)}.
     */
    @Deprecated
    public static Locale getCompatibleProductStoreLocaleValidOrDefault(GenericValue productStore) {
        Locale res = getCompatibleProductStoreLocaleValid(productStore);
        return res != null ? res : getConfiguredDefaultLocale(productStore); 
    }
    
    /**
     * Returns the given locale if valid, or the default locale otherwise (convenience method).
     * @see #getConfiguredDefaultLocale(GenericValue)
     */
    public static Locale getCompatibleLocaleValidOrProductStoreDefault(Locale locale, GenericValue productStore) {
        Locale res = getCompatibleLocaleValid(locale, productStore);
        if (res != null) return res;
        return getConfiguredDefaultLocale(productStore);
    }
    
    public static Locale getCompatiblePropertyLocaleValid(String resource, String propName, GenericValue productStore) {
        Locale locale = null;
        try {
            String locStr = UtilProperties.getPropertyValue(resource, propName);
            if (UtilValidate.isNotEmpty(locStr)) {
                locale = UtilMisc.parseLocale(locStr);
                locale = SolrLocaleUtil.getCompatibleLocaleValid(locale, productStore);
                if (locale == null) {
                    Debug.logWarning("Solr: Content locale property [" + resource + "/" + propName
                            + "] designates a locale name (" + locStr + ") not supported in solr schema"
                            + ", store or solr config (see solrconfig.properties); ignoring", module);
                }
            }
        } catch(Exception e) {
            Debug.logError("Solr: Error reading content locale property [" + resource + "/" + propName
                    + "]: " + e.getMessage(), module);
        }
        return locale;
    }

    public static List<Locale> parseCompatibleLocalesValid(String locStr, boolean allowSpecial) {
        List<Locale> locList = new ArrayList<>();
        if (UtilValidate.isNotEmpty(locStr)) {
            try {
                for(String tag : locStr.split("\\s*,\\s*")) {
                    Locale locale;
                    if (allowSpecial && SolrLocaleUtil.I18N_GENERAL.equals(tag)) {
                        locale = new Locale(SolrLocaleUtil.I18N_GENERAL); // FAKE locale
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
    
    /**
     * Joins locales.
     * WARN: Assumes locales are already compatible/valid.
     */
    public static String joinLocales(Collection<Locale> locales, String delim) {
        StringBuilder sb = new StringBuilder();
        for(Locale locale : locales) {
            sb.append(delim);
            sb.append(getLangCode(locale));
        }
        return (sb.length() > 0) ? sb.substring(delim.length()) : "";
    }
    
    public static Set<String> determineI18nQueryFieldsForUserLocale(Locale userLocale, GenericValue productStore, boolean useStoreLocale,
            Collection<Locale> forceLocales, String fieldPrefix, String userLocalePower, String storeLocalePower, String forceLocalePower) {
        Set<String> fields = new LinkedHashSet<>();
 
        Locale locale = getCompatibleLocaleValid(userLocale, productStore);
        Locale storeLocale = SolrLocaleUtil.getCompatibleProductStoreLocaleValid(productStore);
        
        if (locale == null) locale = storeLocale; // just in case; usually doesn't happen

        if (locale == null) {
            // NOTE: this shouldn't happen unless store is misconfigured
            fields.add(fieldPrefix + SolrLocaleUtil.I18N_GENERAL);
        } else {
            // NOTE: 2017-09-14: no need to include the "*_i18n_general" field here anymore because
            // its content will have been merged into the proper locale already (if applicable)
            
            if (userLocalePower == null) userLocalePower = "";
            
            // add user locale
            fields.add(fieldPrefix + getLangCode(locale) + userLocalePower);
            
            if (useStoreLocale && storeLocale != null && !isSameLangCode(locale, storeLocale)) {
                if (storeLocalePower == null) storeLocalePower = "";
 
                // add store locale
                fields.add(fieldPrefix + getLangCode(storeLocale) + storeLocalePower);
            }
        }
        
        addAllI18nValuePrefixSuffix(fields, forceLocales, fieldPrefix, forceLocalePower);
        return fields;
    }
    
    /**
     * WARN: assumes locales already compatible
     */
    public static void addAllI18nValuePrefixSuffix(Set<String> out, Collection<Locale> locales, String fieldPrefix, String fieldSuffix) {
        if (locales == null) return;
        if (fieldPrefix == null) fieldPrefix = "";
        if (fieldSuffix == null) fieldSuffix = "";
        for(Locale locale : locales) {
            if (locale != null) out.add(fieldPrefix + getLangCode(locale));
        }
    }
    
    public static String getSpellcheckI18nDictBaseName(GenericValue productStore) {
        return spellcheckI18nDictBaseName;
    }
    
    /**
     * Determines the dictionary names should use for user locale.
     * Similar logic to {@link #determineI18nQueryFieldsForUserLocale} but implemented differently
     * due to spellchecker limitations.
     */
    public static Set<String> determineSpellcheckI18nDictNames(Locale userLocale, GenericValue productStore, boolean useStoreLocale, String customDictBaseName) {
        if (UtilValidate.isEmpty(customDictBaseName)) customDictBaseName = getSpellcheckI18nDictBaseName(productStore);
        
        Set<String> fields = new LinkedHashSet<>();
        
        Locale locale = getCompatibleLocaleValid(userLocale, productStore);
        Locale storeLocale = SolrLocaleUtil.getCompatibleProductStoreLocaleValid(productStore);
        
        if (locale == null) locale = storeLocale; // just in case; usually doesn't happen

        if (locale == null) {
            // NOTE: this shouldn't happen unless store is misconfigured
            fields.add(customDictBaseName + SolrLocaleUtil.I18N_GENERAL);
        } else {
            // add user locale
            fields.add(customDictBaseName + (useStoreLocale ? "_dlang_" : "_lang_") + getLangCode(locale));
        }
        return fields;
    }
    
}
