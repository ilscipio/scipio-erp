package com.ilscipio.scipio.product.category;

import java.util.HashMap;
import java.util.Locale;
import java.util.Map;

import org.ofbiz.base.util.UtilMisc;

public abstract class CatalogAltUrlSanitizer {

    /**
     * Clean and convert product name to alt url for storage as ALTERNATIVE_URL.
     */
    public abstract String convertProductNameToAltUrl(String name, Locale locale);
    
    /**
     * Clean and convert category name to alt url for storage as ALTERNATIVE_URL.
     */
    public abstract String convertCategoryNameToAltUrl(String name, Locale locale);

    // TODO?: Future?
//    /**
//     * Clean and convert category name to alt url for storage as ALTERNATIVE_URL.
//     */
//    public abstract String convertContentNameToAltUrl(String name);

    
    /**
     * Clean and convert a general name to alt url for storage as ALTERNATIVE_URL.
     * This mostly exists for reuse reasons.
     */
    public abstract String convertGeneralNameToAltUrl(String name, Locale locale);
    
    /**
     * Cleans alt url coming out of DB IF not already applied before going into db.
     * This is a TRADEOFF with storage-time cleaning.
     * <p>
     * WARN: the locale here might not perfectly match the altUrl due to performance reasons!!
     * More reliable in other methods. See FIXME in SeoCatalogUrlWorker.
     */
    public abstract String sanitizeAltUrlFromDb(String altUrl, Locale locale);
    
    
    /**
     * Returns new map with keys converted.
     */
    public Map<String, String> convertProductNamesToAltUrls(Map<String, String> map) {
        Map<String, String> localeSeoNameMap = new HashMap<>();
        for(Map.Entry<String, String> entry : map.entrySet()) {
            localeSeoNameMap.put(entry.getKey(), convertProductNameToAltUrl(entry.getValue(), parseLocale(entry.getKey())));
        }
        return localeSeoNameMap;
    }
    
    /**
     * Returns new map with keys converted.
     */
    public Map<String, String> convertCategoryNamesToAltUrls(Map<String, String> map) {
        Map<String, String> localeSeoNameMap = new HashMap<>();
        for(Map.Entry<String, String> entry : map.entrySet()) {
            localeSeoNameMap.put(entry.getKey(), convertCategoryNameToAltUrl(entry.getValue(), parseLocale(entry.getKey())));
        }
        return localeSeoNameMap;
    }
    
    /**
     * TODO: REVIEW: this doesn't belong here but it's the only way to make the parsing consistent.
     */
    public Locale parseLocale(String localeString) {
        return UtilMisc.parseLocale(localeString);
    }
}
