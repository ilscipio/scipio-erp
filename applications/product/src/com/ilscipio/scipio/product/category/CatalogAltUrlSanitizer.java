package com.ilscipio.scipio.product.category;

import java.io.Serializable;
import java.util.HashMap;
import java.util.Locale;
import java.util.Map;

import org.ofbiz.base.util.UtilMisc;

public abstract class CatalogAltUrlSanitizer {

    /**
     * Clean and convert a name to alt url for storage as ALTERNATIVE_URL.
     */
    public abstract String convertNameToDbAltUrl(String name, Locale locale, CatalogUrlType entityType, SanitizeContext ctxInfo);

    /**
     * Clean and convert an id to alt url for storage as ALTERNATIVE_URL - may be called as fallback when
     * no name found (depending on config), or may be left to live.
     */
    public abstract String convertIdToDbAltUrl(String id, Locale locale, CatalogUrlType entityType, SanitizeContext ctxInfo);

    /**
     * Clean and convert a name to alt url for storage as ALTERNATIVE_URL - returns new map with keys converted.
     * NOTE: last, nameIndex and totalNames may be null.
     */
    public Map<String, String> convertNamesToDbAltUrls(Map<String, String> map, CatalogUrlType entityType, SanitizeContext ctxInfo) {
        Map<String, String> newMap = new HashMap<>();
        for(Map.Entry<String, String> entry : map.entrySet()) {
            newMap.put(entry.getKey(), convertNameToDbAltUrl(entry.getValue(), parseLocale(entry.getKey()), entityType, ctxInfo));
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
     * NOTE: last, nameIndex and totalNames may be null.
     */
    public abstract String sanitizeAltUrlFromDb(String altUrl, Locale locale, CatalogUrlType entityType, SanitizeContext ctxInfo);

    /**
     * Converts an ID (productId, etc.) directly to a live alt URL part.
     * WARN: In most cases this method should be left to do nothing! The DB IDs should
     * only contain simple characters to begin with.
     * NOTE: last, nameIndex and totalNames may be null.
     */
    public abstract String convertIdToLiveAltUrl(String id, Locale locale, CatalogUrlType entityType, SanitizeContext ctxInfo);

    /**
     * TODO: REVIEW: this doesn't belong here but it's the only way to make the parsing consistent.
     */
    public Locale parseLocale(String localeString) {
        return UtilMisc.parseLocale(localeString);
    }


    public static abstract class Factory<T extends CatalogAltUrlSanitizer> implements Serializable {
        public abstract T getInstance();
        public abstract T getInstance(Map<String, Object> options);
    }

    /**
     * Class to pass arguments to methods above, needed to prevent compatibility breakage.
     * NOTE: Unless otherwise specified, all fields may be null.
     */
    public static class SanitizeContext implements Serializable {
        private Boolean last;
        private Integer nameIndex;
        private Integer totalNames;

        public SanitizeContext(Boolean last, Integer nameIndex, Integer totalNames) {
            this.last = last;
            this.nameIndex = nameIndex;
            this.totalNames = totalNames;
        }

        public SanitizeContext() {
        }

        public static SanitizeContext undefined() {
            return ReadOnlySanitizeContext.UNDEFINED;
        }

        public static SanitizeContext lastElem(boolean last) {
            return last ? ReadOnlySanitizeContext.LAST : ReadOnlySanitizeContext.NON_LAST;
        }

        public static SanitizeContext lastElem() {
            return ReadOnlySanitizeContext.LAST;
        }

        public static SanitizeContext nonLastElem() {
            return ReadOnlySanitizeContext.NON_LAST;
        }

        public Boolean getLast() {
            return last;
        }

        public boolean isLast() {
            return Boolean.TRUE.equals(last);
        }

        public boolean isNonLast() {
            return Boolean.FALSE.equals(last);
        }

        public void setLast(Boolean last) {
            this.last = last;
        }

        public Integer getNameIndex() {
            return nameIndex;
        }

        public void setNameIndex(Integer nameIndex) {
            this.nameIndex = nameIndex;
        }

        public Integer getTotalNames() {
            return totalNames;
        }

        public void setTotalNames(Integer totalNames) {
            this.totalNames = totalNames;
        }

        public static class ReadOnlySanitizeContext extends SanitizeContext {
            private static final SanitizeContext UNDEFINED = new ReadOnlySanitizeContext(); // FIXME: should be unmodifiable
            private static final SanitizeContext LAST = new ReadOnlySanitizeContext(true, null, null); // FIXME: should be unmodifiable
            private static final SanitizeContext NON_LAST = new ReadOnlySanitizeContext(false, null, null); // FIXME: should be unmodifiable

            public ReadOnlySanitizeContext(Boolean last, Integer nameIndex, Integer totalNames) {
                super(last, nameIndex, totalNames);
            }

            public ReadOnlySanitizeContext() {
            }

            @Override
            public void setLast(Boolean last) {
                throw new UnsupportedOperationException();
            }

            @Override
            public void setNameIndex(Integer nameIndex) {
                throw new UnsupportedOperationException();
            }

            @Override
            public void setTotalNames(Integer totalNames) {
                throw new UnsupportedOperationException();
            }
        }
    }
}
