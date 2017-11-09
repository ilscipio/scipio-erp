package com.ilscipio.scipio.product.category;

import java.util.HashMap;
import java.util.Locale;
import java.util.Map;

import org.ofbiz.base.util.UtilMisc;

public abstract class CatalogAltUrlSanitizer {

    public enum ObjectType {
        PRODUCT,
        CATEGORY;
        // TODO?: FUTURE: CONTENT
    }
    
    /**
     * Clean and convert a name to alt url for storage as ALTERNATIVE_URL.
     */
    public abstract String convertNameToDbAltUrl(String name, Locale locale, ObjectType entityType);
    
    /**
     * Clean and convert an id to alt url for storage as ALTERNATIVE_URL - may be called as fallback when
     * no name found (depending on config), or may be left to live.
     */
    public abstract String convertIdToDbAltUrl(String id, Locale locale, ObjectType entityType);
    
    /**
     * Clean and convert a name to alt url for storage as ALTERNATIVE_URL - returns new map with keys converted.
     */
    public Map<String, String> convertNamesToDbAltUrls(Map<String, String> map, ObjectType entityType) {
        Map<String, String> newMap = new HashMap<>();
        for(Map.Entry<String, String> entry : map.entrySet()) {
            newMap.put(entry.getKey(), convertNameToDbAltUrl(entry.getValue(), parseLocale(entry.getKey()), entityType));
        }
        return newMap;
    }
    
    /**
     * Cleans alt url coming out of DB IF not already applied before going into db.
     * This is a TRADEOFF with storage-time cleaning.
     * <p>
     * WARN: 2017-11-08: FOR SEO URLs, THIS MUST DO NOTHING AND RETURN altUrl AS-IS! 
     * This will allow better DB queries. See implementation.
     * <p>
     * WARN: the locale here might not perfectly match the altUrl due to performance reasons!!
     * More reliable in other methods. See FIXME in SeoCatalogUrlWorker.
     */
    public abstract String sanitizeAltUrlFromDb(String altUrl, Locale locale, ObjectType entityType);

    /**
     * Converts an ID (productId, etc.) directly to a live alt URL part.
     * WARN: In most cases this method should be left to do nothing! The DB IDs should
     * only contain simple characters to begin with.
     */
    public abstract String convertIdToLiveAltUrl(String id, Locale locale, ObjectType entityType);

    /**
     * TODO: REVIEW: this doesn't belong here but it's the only way to make the parsing consistent.
     */
    public Locale parseLocale(String localeString) {
        return UtilMisc.parseLocale(localeString);
    }
}
